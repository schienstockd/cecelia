"""
Bayesian (btrack) cell tracking — port of the old cecelia `bayesian_tracking_utils.py`.

btrack (Ulicna et al. 2021, Front. Comput. Sci.; https://github.com/quantumjot/btrack)
links per-cell centroids across time into tracks. This module reads centroids from a
segmentation's label-props H5AD, runs btrack, and writes the lineage columns back into the
same H5AD `obs`, following our AnnData convention (docs/DATAMODEL.md):

  - `obs.index`      = integer cell label (preserved; we align on it)
  - `X` / `var`      = feature matrix (preserved untouched)
  - `obsm['spatial']` + `uns['spatial_cols']` = centroids (`centroid_x`/`_y`/`_z`, present axes)
  - `obsm['temporal']` + `uns['temporal_cols']` = time (`centroid_t`)

Track lineage is identity, not a measurement, so it goes into `obs` (NOT `X`):
`track_id, track_parent, track_root, track_state, track_generation, cell_id`. Cells not in
a track get `NaN`. No track *measures* (speed/angle) are computed here — gating on track
properties is a later phase (docs/POPULATION.md).

Membership for the gated case is resolved by Julia (the sole gate evaluator) and handed to
us as an explicit list of label IDs — we never evaluate gates here.

Coordinates are tracked in **pixel space** (no physical-unit scaling); `maxSearchRadius`
is therefore in pixels, matching the UI label. Anisotropic-Z scaling can be added later if
needed by passing physical pixel sizes from Julia.
"""
import os

import numpy as np
import pandas as pd
import anndata as ad

import btrack
from btrack import config as btrack_config
from btrack import utils as btrack_utils

import cecelia.utils.label_props_utils as label_props_utils

# btrack Track.to_dict() fields we keep (per-timepoint rows; ID == btrack track number)
_TRACK_COLS = ("ID", "parent", "root", "state", "generation", "t", "label_id")


class BayesianTrackingUtils:
    def __init__(self, params: dict, logger):
        self.log = logger
        self.task_dir   = params["taskDir"]
        self.value_name = params.get("valueName", "default")
        # Vendored btrack base config — path supplied by the caller (the tracking task runner ships
        # it beside itself), so this IO helper never reaches into package-data. Loaded from disk so
        # headless/production runs never hit btrack's network download.
        self.btrack_config_path = params["btrackConfig"]
        # explicit gated-population label IDs (None = track the whole segmentation)
        self.label_ids  = params.get("labelIds", None)

        self.max_search_radius   = params["maxSearchRadius"]
        self.max_lost            = params["maxLost"]
        self.track_branching     = bool(params["trackBranching"])
        self.min_timepoints      = params["minTimepoints"]
        self.accuracy            = params["accuracy"]
        self.prob_to_assign      = params["probToAssign"]
        self.noise_inital        = params["noiseInital"]
        self.noise_processing    = params["noiseProcessing"]
        self.noise_measurements  = params["noiseMeasurements"]
        self.lambda_link         = params["lambdaLink"]
        self.lambda_branch       = params["lambdaBranch"]
        self.lambda_time         = params["lambdaTime"]
        self.lambda_dist         = params["lambdaDist"]
        self.theta_time          = params["thetaTime"]
        self.theta_dist          = params["thetaDist"]
        self.dist_thresh         = params["distThresh"]
        self.time_thresh         = params["timeThresh"]
        self.segmentation_miss_rate = params["segmentationMissRate"]

        self.props_path = os.path.join(self.task_dir, "labelProps", f"{self.value_name}.h5ad")

    # ── main ──────────────────────────────────────────────────────────────────────
    def track_objects(self):
        self.log.progress(0, 4)
        self.log.log(f">> Read centroids: {self.props_path}")
        # read centroids through the LabelPropsView chain — the one H5AD access idiom
        # (docs/DATAMODEL.md), mirror of the Julia LabelProps reader.
        view = label_props_utils.LabelPropsView(self.props_path)
        if self.label_ids is not None:
            view.filter_by_label(self.label_ids)
        centroid_df = self._centroids_from_view(view)
        if self.label_ids is not None:
            self.log.log(f">> Restricted to {len(centroid_df)} cells from gated population")
        self.log.progress(1, 4)

        self.log.log(">> Start tracking objects")
        track_df = self._track(centroid_df)
        self.log.progress(2, 4)

        # drop unassigned, sort, then filter short tracks
        track_df = track_df.dropna(axis=0).sort_values("label_id")
        n_before = track_df["track_id"].nunique()
        track_df = track_df[
            track_df.groupby("track_id")["track_id"].transform("size") >= self.min_timepoints
        ]
        n_after = track_df["track_id"].nunique()
        self.log.log(f"> {n_before} tracks -> {n_after} after min {self.min_timepoints} timepoints")

        # cell_id: 1-based index within each track, ordered by time
        track_df["cell_id"] = track_df.groupby("track_id")["t"].rank(method="first").astype(int)
        self.log.progress(3, 4)

        self._write_back(track_df)
        self.log.progress(4, 4)
        self.log.log(">> done")

    # ── centroids → btrack input frame (x, y, z, t, label_id) ──────────────────────
    def _centroids_from_view(self, view) -> pd.DataFrame:
        temporal_cols = view.temporal_columns()      # ['centroid_t'] or [] (guarded → explicit)
        if not temporal_cols:
            raise SystemExit(
                "[ERROR] No temporal axis in label props — tracking needs a timecourse "
                f"segmentation (value_name='{self.value_name}')")

        df = view.view_centroid_cols().as_df()    # centroid_x/_y[/_z] + centroid_t + label (label-filtered)
        labels = df["label"].to_numpy(dtype=np.int64)
        t = df[temporal_cols[0]].to_numpy(dtype=np.float64)

        # select each axis BY NAME (never positionally) — z is absent for 2D; btrack still wants a z
        # column, so fill zeros. (btrack's own frame schema is t,x,y,z,label_id.)
        x = df["centroid_x"].to_numpy(dtype=np.float64)
        y = df["centroid_y"].to_numpy(dtype=np.float64)
        z = (df["centroid_z"].to_numpy(dtype=np.float64)
             if "centroid_z" in df.columns else np.zeros_like(labels, dtype=np.float64))

        return pd.DataFrame({"t": t, "x": x, "y": y, "z": z, "label_id": labels})

    # ── run btrack ──────────────────────────────────────────────────────────────────
    def _track(self, centroid_df: pd.DataFrame) -> pd.DataFrame:
        objects = btrack_utils.objects_from_dict(
            {k: np.asarray(centroid_df[k].values) for k in centroid_df.columns})

        cfg = btrack_config.load_config(self.btrack_config_path)

        # probability NOT to assign a track (reversed prob_to_assign), as in old cecelia:
        # assumes 0.0001 highest / 0.1 lowest.
        prob_not_assign = (0.1 - (1 / 10000)) * (1 - self.prob_to_assign)

        cfg.motion_model.max_lost = self.max_lost
        cfg.motion_model.prob_not_assign = prob_not_assign
        cfg.motion_model.accuracy = self.accuracy * 10  # highest that worked without crashing
        cfg.motion_model.P = cfg.motion_model.P * self.noise_inital
        cfg.motion_model.G = cfg.motion_model.G * self.noise_processing
        cfg.motion_model.R = cfg.motion_model.R * self.noise_measurements

        cfg.hypothesis_model.lambda_link = self.lambda_link
        cfg.hypothesis_model.lambda_branch = self.lambda_branch
        cfg.hypothesis_model.lambda_time = self.lambda_time
        cfg.hypothesis_model.lambda_dist = self.lambda_dist
        cfg.hypothesis_model.theta_time = self.theta_time
        cfg.hypothesis_model.theta_dist = self.theta_dist
        cfg.hypothesis_model.dist_thresh = self.dist_thresh
        cfg.hypothesis_model.time_thresh = self.time_thresh
        cfg.hypothesis_model.segmentation_miss_rate = self.segmentation_miss_rate

        if self.track_branching and "P_branch" not in cfg.hypothesis_model.hypotheses:
            cfg.hypothesis_model.hypotheses = list(cfg.hypothesis_model.hypotheses) + ["P_branch"]

        with btrack.BayesianTracker() as tracker:
            tracker.configure(cfg)
            tracker.max_search_radius = self.max_search_radius
            tracker.append(objects)
            tracker.track(step_size=100)
            # run the optimizer for up to 10 minutes before timing out
            tracker.optimize(options={"tm_lim": 60_000 * 10})
            tracks = tracker.tracks

        frames = [pd.DataFrame(x.to_dict(), columns=_TRACK_COLS) for x in tracks]
        track_df = pd.concat(frames, ignore_index=True) if frames else \
            pd.DataFrame(columns=_TRACK_COLS)
        return track_df.rename(columns={"ID": "track_id"})

    # ── write lineage back into obs (our AnnData convention) ────────────────────────
    def _write_back(self, track_df: pd.DataFrame):
        # one row per tracked cell, keyed by label; write through the LabelPropsView chain
        # (add_obs aligns by label, cells without a track get NaN). Same idiom as Julia's
        # LabelProps add_obs/save! — the single H5AD write path (docs/DATAMODEL.md).
        lineage = track_df[
            ["label_id", "track_id", "parent", "root", "state", "generation", "cell_id"]
        ].rename(columns={
            "label_id":   "label",
            "parent":     "track_parent",
            "root":       "track_root",
            "state":      "track_state",
            "generation": "track_generation",
        })

        view = label_props_utils.LabelPropsView(self.props_path)

        # invalidate stale track measures: new tracks make any cached live.cell.* / live.track.*
        # columns (written by tracking.track_measures against the *previous* tracking) wrong.
        # The tracking task owns this invalidation (docs/TODO.md #00028, porting spec Step 5).
        stale = [c for c in view.adata.obs.columns
                 if c.startswith("live.cell.") or c.startswith("live.track.")]
        if stale:
            self.log.log(f">> Invalidate {len(stale)} stale track-measure columns")

        n_tracked = int(track_df["track_id"].notna().sum())
        self.log.log(f">> Save {n_tracked} tracked cells -> {self.props_path}")
        view.drop_obs(stale).add_obs(lineage).save()


def write_track_props(params: dict, log):
    """
    Create the companion per-track h5ad `{value_name}__tracks.h5ad` — ONE row per track.

    Track measures (computed in Julia via the celltrackR port) land in `X`/`var` so they are
    gateable one-point-per-track; lineage goes in `obs`; the obs index is the `track_id`. This
    is the mirror of `measure_utils._to_anndata`: building a NEW .h5ad from scratch is the
    producing task's job and uses `anndata` directly (docs/DATAMODEL.md). The Julia
    `tracking.track_measures` handler hands us the table via the params JSON.

    Params:
      outPath       - absolute path of the track h5ad to write
      trackIds      - list[int], obs index (one per track)
      measureNames  - list[str], var names (the live.track.* measures)
      X             - list[list[float|None]] of shape (n_tracks, n_measures); None → NaN
      lineage       - dict[str, list[float|None]], per-track obs columns aligned to trackIds
    """
    out_path      = params["outPath"]
    track_ids     = params.get("trackIds", []) or []
    measure_names = params.get("measureNames", []) or []
    x_rows        = params.get("X", []) or []
    lineage       = params.get("lineage", {}) or {}

    n_tracks, n_meas = len(track_ids), len(measure_names)
    if n_tracks and x_rows:
        # None → np.nan for a float matrix (JSON has no NaN, so Julia sent nulls)
        X = np.asarray(x_rows, dtype=np.float32).reshape(n_tracks, n_meas)
    else:
        X = np.empty((n_tracks, n_meas), dtype=np.float32)

    obs = pd.DataFrame(index=[str(int(t)) for t in track_ids])
    for col, vals in lineage.items():
        obs[col] = np.asarray(vals, dtype=np.float64)

    adata = ad.AnnData(
        X   = X,
        obs = obs,
        var = pd.DataFrame(index=list(measure_names)),
    )
    adata.uns["cecelia_table"] = "tracks"   # marks this h5ad as a per-track table

    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    adata.write_h5ad(out_path)
    log.log(f">> wrote {n_tracks} tracks ({n_meas} measures) -> {out_path}")
    return out_path
