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

Coordinates are scaled to **µm** before btrack sees them (`physicalSizes` from Julia's
`img_physical_sizes`, skimage order `[sz, sy, sx]`), so every distance param —
`maxSearchRadius`, `distThresh`, `thetaDist` — is in µm. This matches `track_measures`,
which already reports µm/min from the same accessor, and handles anisotropic Z: at
0.33 µm XY vs 2 µm Z, pixel-space tracking under-counted a one-plane hop 6-fold.
"""
import os

import numpy as np
import pandas as pd
import anndata as ad

import btrack
from btrack import config as btrack_config
from btrack import utils as btrack_utils

import cecelia.utils.label_props_utils as label_props_utils
from cecelia.utils.atomic_io import write_h5ad_atomic

# btrack Track.to_dict() fields we keep (per-timepoint rows; ID == btrack track number)
_TRACK_COLS = ("ID", "parent", "root", "state", "generation", "t", "label_id")

# `track_source` sentinel written when tracking runs on the whole segmentation, not on a gated pop.
# Bypasses the pop→pop conflict/attribution rules (whole-seg is the documented "prime everything"
# mode, docs/todo/MULTI_POP_TRACKING_ORPHANS_PLAN.md decision 1). Kept as a module-level const so the
# conflict detector, `_write_back`, and the Julia writer's mirror (`WHOLE_SEG_TRACK_SOURCE` in
# `app/src/gating/population_manager.jl`) all agree on the string.
WHOLE_SEG_TRACK_SOURCE = "whole_seg"


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
        # Stable per-pop provenance stamped on every row this run writes: the gated pop's UID from
        # the gating sidecar, or `"whole_seg"` for the un-gated whole-segmentation run. `_write_back`
        # uses it to delete PRIOR rows written by this same source before laying down the new ones
        # — so re-tracking a pop replaces its own tracks, without touching cells tracked under
        # another pop. See docs/todo/MULTI_POP_TRACKING_PLAN.md Decision 1. Missing → "whole_seg"
        # so an older Julia handler that hasn't been updated still lands somewhere sensible.
        self.track_source = str(params.get("trackSource", WHOLE_SEG_TRACK_SOURCE))
        # Override the P1 conflict detector — write over labels that are currently owned by a
        # different pop's `track_source`. Never set implicitly. Used by the intentional pop→pop
        # refinement idiom (see MULTI_POP_TRACKING_ORPHANS_PLAN.md decision 2); the whole-seg
        # non-symmetry doesn't need it because whole_seg is already treated as bypass.
        self.force_track_source = bool(params.get("trackSourceForce", False))
        # Live pop UIDs the Julia handler saw when it launched this run — used to sweep rows stamped
        # with a `track_source` from a pop that no longer exists (MULTI_POP_TRACKING_ORPHANS_PLAN.md
        # decision 3). `None` disables the sweep, for a legacy Julia handler that doesn't emit the
        # param — behaviour matches the shipped code. An empty list runs the sweep normally: every
        # pop-authored row is now an orphan, `whole_seg` rows survive (sentinel).
        live = params.get("liveTrackSources", None)
        self.live_track_sources = None if live is None else set(str(s) for s in live)

        self.max_search_radius   = params["maxSearchRadius"]
        # [sz, sy, sx] in µm — skimage axis order, matching the centroid columns, the same shape
        # `img_physical_sizes` hands every other spatial task (cellNeighbours, the mesh tasks,
        # track_measures). Absent/empty for an uncalibrated image.
        self.physical_sizes      = params.get("physicalSizes") or None
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

        # ── pixels → µm ─────────────────────────────────────────────────────────────
        # ONE shared conversion (`scale_centroids`, the mirror of Julia's `scale_centroids!`) rather than
        # a local multiply, so the linking, the µm/min measures and the spatial tasks all convert the
        # same way. It scales the CENTROID COLUMNS, never `centroid_t` — see below on why time is left in
        # frames.
        #
        # Scaling the COORDINATES, not each spatial param, for two reasons.
        #
        # 1. It makes every distance param physical at once — `maxSearchRadius`, `distThresh`,
        #    `thetaDist` — and any added later, with one conversion rather than one per param.
        #    `track_measures` already reports µm/min from the same `img_physical_sizes`, so the
        #    linking and the measures computed on its own output now share a coordinate system;
        #    they did not, which is how "radius 8" and "a 8 µm jump" could both be true.
        # 2. It fixes Z ANISOTROPY, which no per-param conversion can. On this data a voxel is
        #    ~0.33 µm in XY and 2 µm in Z, so in pixel space btrack scored a one-plane hop as
        #    0.33 µm of motion when it is 2 µm — a 6x under-count, and exactly the direction that
        #    links cells at different depths. In µm the axes are commensurate by construction.
        #
        scaled = label_props_utils.scale_centroids(
            df, self.physical_sizes if self.physical_sizes else (1.0, 1.0, 1.0))

        # select each axis BY NAME (never positionally) — z is absent for 2D; btrack still wants a z
        # column, so fill zeros. (btrack's own frame schema is t,x,y,z,label_id.)
        x = scaled["centroid_x"].to_numpy(dtype=np.float64)
        y = scaled["centroid_y"].to_numpy(dtype=np.float64)
        z = (scaled["centroid_z"].to_numpy(dtype=np.float64)
             if "centroid_z" in scaled.columns else np.zeros_like(labels, dtype=np.float64))

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
    #
    # PROVENANCE-AWARE MERGE. Sequential runs of `bayesian_tracking` on the SAME segmentation used to
    # overwrite `track_id`: `add_obs` builds a full-length column filled with NaN for labels not in
    # the current run, so tracking `/qc/CD169-/fragments` after `/qc/CD169-/cells` NaN-ed out the
    # cells rows and left only fragments' tracks. The new contract, per
    # docs/todo/MULTI_POP_TRACKING_PLAN.md Decision 1:
    #
    #   1. DELETE prior rows written by THIS run's source (`track_source == self.track_source`) —
    #      re-running one pop removes its own previous tracks as a unit, wherever those cells now
    #      sit (a re-gated pop's edge doesn't leak old ids).
    #   2. COMPACT surviving `track_id`s to `1..N` and remap `track_parent`/`track_root` through the
    #      same permutation — every id space is renumbered together, so a chain of tracking runs
    #      doesn't grow ids without bound.
    #   3. WRITE new run's rows with `track_id` starting at `N+1` and stamp `track_source` on them.
    #
    # Non-symmetry with whole-seg: `whole_seg`-sourced rows aren't matched by a per-pop delete step,
    # so `track whole_seg → track pop A` refines A's cells but leaves the rest of the segmentation's
    # tracks intact. That's the intended "prime everything, refine one pop" mode.
    def _write_back(self, track_df: pd.DataFrame):
        view = label_props_utils.LabelPropsView(self.props_path)
        obs = view.adata.obs
        labels_all = obs.index.astype(np.int64).to_numpy()
        n_labels   = len(labels_all)

        # ── 1. read the existing lineage into aligned float arrays ──────────────────
        def _read_float(col: str) -> np.ndarray:
            return (obs[col].to_numpy(dtype=np.float64, copy=True)
                    if col in obs.columns else np.full(n_labels, np.nan))
        cur_id     = _read_float("track_id")
        cur_parent = _read_float("track_parent")
        cur_root   = _read_float("track_root")
        cur_state  = _read_float("track_state")
        cur_gen    = _read_float("track_generation")
        cur_cellid = _read_float("cell_id")
        # `track_source` is a categorical/string column; read as strings ("" for missing) so the
        # delete-mask comparison below is a plain equality (never NaN-vs-string).
        if "track_source" in obs.columns:
            src_series = obs["track_source"]
            src_obj = src_series.astype(object).where(src_series.notna(), None).to_numpy(copy=True)
        else:
            src_obj = np.array([None] * n_labels, dtype=object)

        # ── 2. delete prior rows this same source authored ─────────────────────────
        del_mask = np.array([s == self.track_source for s in src_obj])
        if del_mask.any():
            for arr in (cur_id, cur_parent, cur_root, cur_state, cur_gen, cur_cellid):
                arr[del_mask] = np.nan
            for i in np.where(del_mask)[0]:
                src_obj[i] = None
            self.log.log(f">> Delete {int(del_mask.sum())} rows previously written by "
                         f"track_source='{self.track_source}'")

        # Row lookup by label id, shared between the conflict check below and the WRITE step. Both
        # want "which row is label L on" — before the guard, we didn't need it here.
        rowof = {int(l): i for i, l in enumerate(labels_all)}

        # ── 2a. orphan sweep (MULTI_POP_TRACKING_ORPHANS_PLAN P3) ─────────────────
        # A row still stamped by a `track_source` that is NOT in the live pop-UID set (nor the
        # whole_seg sentinel) belongs to a pop the user has already deleted. Nothing in the
        # tracking pipeline would ever fire that source's DELETE again, so without this sweep those
        # rows persist forever and their `track_id` bleeds into whichever live pop later includes
        # the same label. Sweep NaNs those rows and clears their src marker, mirroring the DELETE
        # step's contract; COMPACT then re-numbers around them.
        # `None` disables the sweep (a legacy Julia handler that never emits the param → shipped
        # code path unchanged). An empty live-set is a valid config — every pop-authored row is
        # then an orphan; `whole_seg` rows always survive.
        if self.live_track_sources is not None:
            live = self.live_track_sources
            sweep_idx = [i for i, s in enumerate(src_obj)
                         if isinstance(s, str) and s and s != WHOLE_SEG_TRACK_SOURCE
                            and s not in live]
            if sweep_idx:
                sweep_idx_arr = np.array(sweep_idx, dtype=np.int64)
                for arr in (cur_id, cur_parent, cur_root, cur_state, cur_gen, cur_cellid):
                    arr[sweep_idx_arr] = np.nan
                for i in sweep_idx:
                    src_obj[i] = None
                self.log.log(f">> Sweep {len(sweep_idx)} orphan rows "
                             f"(track_source from deleted pop(s))")

        # ── 2b. conflict detector (MULTI_POP_TRACKING_ORPHANS_PLAN P1) ─────────────
        # After DELETE, any row still stamped with a track_source other than {None, whole_seg,
        # self.track_source} is currently owned by a DIFFERENT live pop. Writing over it would
        # silently transfer lineage ownership and leave the previous owner's DELETE step unable to
        # find it on a future re-run. Fail by default and name the labels; the caller passes
        # `trackSourceForce=true` to override (the intentional pop→pop refinement idiom — whole-seg
        # → pop is already bypass because whole_seg is treated as "prime everything, refine one pop"
        # and doesn't count as a conflict).
        conflicts = []
        for lbl in track_df["label_id"].to_numpy(dtype=np.int64):
            r = rowof.get(int(lbl))
            if r is None:
                continue
            s = src_obj[r]
            if s is None or s == WHOLE_SEG_TRACK_SOURCE or s == self.track_source:
                continue
            conflicts.append((int(lbl), str(s)))
        if conflicts:
            sample = conflicts[:5]
            tail = f" (and {len(conflicts) - 5} more)" if len(conflicts) > 5 else ""
            pairs = ", ".join(f"label={l} source='{s}'" for l, s in sample)
            msg = (f"track_source='{self.track_source}': {len(conflicts)} labels already owned by "
                   f"other pop(s). First: {pairs}{tail}. "
                   f"Re-track the owning pops first, adjust pop definitions to remove overlap, "
                   f"or pass trackSourceForce=true.")
            if self.force_track_source:
                self.log.log(f">> [WARN] {msg} — proceeding under trackSourceForce=true")
            else:
                raise ValueError(msg)

        # ── 3. compact surviving track_ids to 1..N (with parent/root remapped) ─────
        survivor_mask = ~np.isnan(cur_id)
        n_surviving = 0
        if survivor_mask.any():
            unique_old = np.unique(cur_id[survivor_mask].astype(np.int64))
            n_surviving = len(unique_old)
            id_map = {int(old): new for new, old in enumerate(unique_old, start=1)}

            def _remap(arr: np.ndarray) -> np.ndarray:
                out = np.full_like(arr, np.nan)
                nz = ~np.isnan(arr)
                for i in np.where(nz)[0]:
                    m = id_map.get(int(arr[i]))
                    # A parent/root that pointed at a deleted track has no image in id_map → NaN.
                    if m is not None:
                        out[i] = m
                return out
            cur_id     = _remap(cur_id)
            cur_parent = _remap(cur_parent)
            cur_root   = _remap(cur_root)

        # ── 4. write the run's rows: new track_ids from n_surviving + 1 ────────────
        # Renumber the whole run atomically so internal parent/root references stay intact after the
        # shift.
        run_uniq = np.unique(track_df["track_id"].to_numpy(dtype=np.int64))
        offset   = n_surviving
        run_map  = {int(old): offset + i + 1 for i, old in enumerate(run_uniq)}

        def _offset_series(s: pd.Series) -> np.ndarray:
            vals = s.to_numpy()
            out  = np.full(len(vals), np.nan)
            for i, v in enumerate(vals):
                # NaN check tolerates both numeric NaN and pandas' None (btrack root == track_id
                # by construction, so `root`/`parent` are never NaN here — the guard is defensive).
                try:
                    iv = int(v)
                except (TypeError, ValueError):
                    continue
                m = run_map.get(iv)
                if m is not None:
                    out[i] = m
            return out

        run_new_id     = _offset_series(track_df["track_id"])
        run_new_parent = _offset_series(track_df["parent"])
        run_new_root   = _offset_series(track_df["root"])
        run_state      = track_df["state"].to_numpy(dtype=np.float64)
        run_gen        = track_df["generation"].to_numpy(dtype=np.float64)
        run_cellid     = track_df["cell_id"].to_numpy(dtype=np.float64)
        run_labels     = track_df["label_id"].to_numpy(dtype=np.int64)

        # `rowof` is built once above (before the conflict check) and reused here.
        for i in range(len(track_df)):
            r = rowof.get(int(run_labels[i]))
            if r is None:
                continue
            cur_id[r]     = run_new_id[i]
            cur_parent[r] = run_new_parent[i]
            cur_root[r]   = run_new_root[i]
            cur_state[r]  = run_state[i]
            cur_gen[r]    = run_gen[i]
            cur_cellid[r] = run_cellid[i]
            src_obj[r]    = self.track_source

        # ── 5. invalidate stale track measures + write ─────────────────────────────
        # Same policy as the pre-provenance path: any cached `live.cell.*` / `live.track.*` was
        # computed against the previous tracking output and is now wrong.
        stale = [c for c in obs.columns
                 if c.startswith("live.cell.") or c.startswith("live.track.")]
        if stale:
            self.log.log(f">> Invalidate {len(stale)} stale track-measure columns")

        n_written = int(np.sum(~np.isnan(cur_id)))
        self.log.log(f">> Save {n_written} tracked cells -> {self.props_path}")
        view.drop_obs(stale).add_obs({
            "track_id":         cur_id,
            "track_parent":     cur_parent,
            "track_root":       cur_root,
            "track_state":      cur_state,
            "track_generation": cur_gen,
            "cell_id":          cur_cellid,
        }).add_categorical_obs("track_source", labels_all, src_obj).save()


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
    write_h5ad_atomic(adata, out_path)
    log.log(f">> wrote {n_tracks} tracks ({n_meas} measures) -> {out_path}")
    return out_path
