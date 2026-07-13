"""
Napari WebSocket bridge.

Julia sends JSON commands; this script routes them to a NapariState instance
running on the Qt main thread. State (im_scale, axes, task_dir) persists
between calls, mirroring the Python NapariUtils class from cecelia R.
"""
import asyncio
import datetime
import json
import os
import pickle
import queue
import sys
import threading
import urllib.request
from functools import lru_cache

import dask.array as da
import napari
import numpy as np
import zarr
from qtpy.QtCore import QTimer

HOST = "localhost"
PORT = 7655

# name of the Shapes layer used for spatial cell selection (linked brushing → flow plots)
SELECTION_LAYER = "Cell selection"

# qualitative palette for colouring labels/tracks by a CATEGORICAL obs column (e.g. HMM state).
# Okabe–Ito (colourblind-safe), as RGBA floats in 0..1 — matches the web canvas 'okabe-ito' palette.
_CATEGORICAL_RGBA = [
    (0.902, 0.624, 0.000, 1.0), (0.337, 0.706, 0.914, 1.0), (0.000, 0.620, 0.451, 1.0),
    (0.941, 0.894, 0.259, 1.0), (0.000, 0.447, 0.698, 1.0), (0.835, 0.369, 0.000, 1.0),
    (0.800, 0.475, 0.655, 1.0), (0.580, 0.580, 0.580, 1.0),
]

# The shared label-props reader (cecelia.utils.label_props_utils) resolves via the editable
# `cecelia` install in the pixi env (python/pyproject.toml + the pixi `cecelia` dep) — no sys.path
# manipulation needed. Launched via `pixi run napari`.


# ── State class (mirrors NapariUtils) ─────────────────────────────────────────

class NapariState:
    """Maintains viewer + image state between WebSocket commands."""

    def __init__(self, viewer: napari.Viewer):
        self._viewer = viewer
        # set when open_image is called
        self._im_data = None          # list[dask array], one per multiscale level
        self._im_scale = None         # scale without channel axis, e.g. [z, y, x] µm
        self._im_units = None         # unit tuple matching _im_scale axes, e.g. ('µm','µm','µm')
        self._axes = None             # ['t','c','z','y','x']
        self._channel_axis = None     # int index into im_data shape
        # set by set_task_dir
        self._task_dir = None
        # population / cell-selection state
        self._sel_ctx = None          # {project_uid, image_uid, value_name, api_url}
        self._centroid_cache = {}     # value_name → (labels, C, axes) for the open image
        self._tracks_cache = {}       # value_name → (tracks, track_ids) for the Tracks layer
        self._pop_sigs = {}           # pop layer name → signature, to skip unchanged updates
        self._track_sigs = {}         # track layer name → signature, to skip unchanged updates
        self._labels_orig_cmap = {}   # labels layer name → original colormap, to restore on reset
        self._colcol_cache = {}       # (value_name, column) → (labels, vals, is_cat) obs column read
        self._ts_handler = None       # timestamp slider callback, disconnected before reconnecting

        # ── layer-props autosave (debounced, atomic) ────────────────────────────
        # Save brightness/contrast/colormap + the T/Z slider position the moment the user changes
        # them (coalesced ~500ms), so the view survives navigation AND a crash/hard-kill — the file
        # is only ever written atomically. Off unless the app enables it per open (configure_autosave).
        self._autosave_path = None     # target .pkl for the currently open image, or None
        self._autosave_enabled = False
        self._autosave_loading = False # True while applying loaded props → suppress the write-back
        self._autosave_conns = []      # [(emitter, cb)] connected for the current image; dropped on reconnect
        self._autosave_timer = QTimer()
        self._autosave_timer.setSingleShot(True)
        self._autosave_timer.setInterval(500)   # debounce window: one write ~500ms after the last change
        self._autosave_timer.timeout.connect(self._autosave_flush)

    # ── Viewer lifecycle ───────────────────────────────────────────────────────

    def clear(self):
        self._viewer.layers.clear()

    # ── Image ─────────────────────────────────────────────────────────────────

    def open_image(self, path: str, channel_names=None, channel_colormaps=None,
                   show_3d: bool = False, as_dask: bool = True, visible=True):
        self._viewer.layers.clear()
        self._centroid_cache = {}     # stale once a new image's geometry loads
        self._tracks_cache = {}
        self._pop_sigs = {}
        self._track_sigs = {}
        self._labels_orig_cmap = {}
        self._colcol_cache = {}
        self._sel_ctx = None

        self._im_data = _open_zarr_multiscale(path, as_dask=as_dask)

        # read axes and scale from .zattrs (series-level, set by bioformats2raw)
        self._axes = _read_axes(path)
        full_scale = _read_scale(path)   # one value per axis, e.g. [t, c, z, y, x]

        # channel axis index and scale without channel dimension
        self._channel_axis = None
        self._im_scale = None
        self._im_units = None
        if self._axes:
            low = [a.lower() for a in self._axes]
            if "c" in low:
                self._channel_axis = low.index("c")
            if full_scale is not None:
                self._im_scale = [s for i, s in enumerate(full_scale)
                                  if i != self._channel_axis]
        if self._im_scale is not None:
            unit = _read_unit_from_ome_xml(path)
            self._im_units = tuple(unit for _ in self._im_scale)

        # Delegate the actual layer creation to the SHARED generic helper (cecelia.utils.napari_utils,
        # which coastal mirrors): per-channel colormaps, additive blending, contrast-from-sample and the
        # list-name guard live there so both projects render identically. The bridge keeps all the
        # disk/scale/units logic above. See docs/todo/CECELIA_NAPARI_UPSTREAM_PLAN.md.
        from cecelia.utils import napari_utils
        napari_utils.add_image(
            self._viewer, self._im_data,
            scale=self._im_scale, units=self._im_units,
            channel_axis=self._channel_axis, channel_names=channel_names,
            colormaps=channel_colormaps, contrast=True, visible=visible,
        )

        # label the viewer's sliders/axes with the dimension names (t/z/y/x) instead of the
        # default -1/-2/… indices. The viewer dims exclude the channel axis (split into layers).
        axis_labels = self._display_axes()
        if axis_labels and len(axis_labels) == self._viewer.dims.ndim:
            self._viewer.dims.axis_labels = axis_labels

        self._viewer.scale_bar.unit = None
        self._viewer.scale_bar.visible = True
        self._viewer.scale_bar.ticks = False

        # timecourse: show an elapsed-time stamp that follows the t slider (ports old `add_timestamp`)
        self._setup_timestamp(path)

        # 3D view is a per-set preference applied "where possible": only switch to volumetric display
        # when the image actually has a z-axis with depth. A 2D image (no z, or z==1) stays 2D, so
        # clicking through a mixed 2D/3D set with the toggle on shows each image correctly rather than
        # forcing a flat plane into a rotatable 3D view.
        if show_3d and (self._z_axis_len() or 0) > 1:
            self._viewer.dims.ndisplay = 3
            self._viewer.reset_view()

    def _z_axis_len(self):
        """Length of the image's `z` axis, or None if there is no `z` axis / no data loaded.
        Reads from the full (channel-inclusive) data shape, since `self._axes` includes `c`."""
        if not self._axes or not self._im_data:
            return None
        low = [a.lower() for a in self._axes]
        if "z" not in low:
            return None
        try:
            return int(self._im_data[0].shape[low.index("z")])
        except Exception:
            return None

    def _time_axis_len(self):
        """Length of the image's `t` axis, or None if there is no `t` axis / no data loaded.
        Reads from the full (channel-inclusive) data shape, since `self._axes` includes `c`."""
        if not self._axes or not self._im_data:
            return None
        low = [a.lower() for a in self._axes]
        if "t" not in low:
            return None
        try:
            return int(self._im_data[0].shape[low.index("t")])
        except Exception:
            return None

    def _setup_timestamp(self, path: str):
        """For timecourse data (a `t` axis), show an elapsed-time text overlay (top-left) that updates
        as the t slider moves — `t_index × frame_interval`, formatted H:MM:SS. The frame interval is
        read from OME-XML `pixels.time_increment` (seconds); if absent it falls back to the frame
        index ("t = N"). Ports the old R `napari_utils.add_timestamp`. Default ON for timecourse."""
        axes = self._display_axes()
        ov = self._viewer.text_overlay
        # disconnect the previous image's handler so they don't accumulate across opens (the viewer
        # persists; only the layers are cleared)
        if self._ts_handler is not None:
            try:
                self._viewer.dims.events.current_step.disconnect(self._ts_handler)
            except Exception:
                pass
            self._ts_handler = None
        if "t" not in axes:
            ov.visible = False
            return
        # bioformats2raw writes a full TCZYX series, so a single-timepoint image still carries a
        # singleton `t` axis. Without this guard that showed a misleading "t = 0" overlay on images
        # that have no real timecourse — treat a length-1 time axis as "no timecourse".
        t_len = self._time_axis_len()
        if t_len is not None and t_len <= 1:
            ov.visible = False
            return
        t_idx = axes.index("t")
        interval = _read_time_increment(path)        # seconds per frame, or None
        def _update(event=None):
            try:
                step = self._viewer.dims.current_step
                t = step[t_idx] if t_idx < len(step) else 0
                ov.text = (str(datetime.timedelta(seconds=int(round(t * interval))))
                           if interval and interval > 0 else f"t = {t}")
            except Exception:
                pass
        ov.visible = True
        ov.font_size = 12
        ov.color = "white"
        try:
            ov.position = "top_left"
        except Exception:
            pass
        _update()
        self._viewer.dims.events.current_step.connect(_update)
        self._ts_handler = _update   # remembered so the next open_image disconnects it first

    # ── Labels ────────────────────────────────────────────────────────────────

    def show_labels(self, value_name: str = "default",
                    label_files: list = None,
                    show_labels: bool = True, show_points: bool = False):
        if self._task_dir is None:
            raise RuntimeError("call set_task_dir before show_labels")
        if label_files is None:
            label_files = [f"{value_name}.zarr"]

        import os
        for label_filename in label_files:
            # name by the value_name (drop the ".zarr") → "(C) Labels", not "(C.zarr) Labels"
            stem = label_filename[:-5] if label_filename.endswith(".zarr") else label_filename
            layer_name = f"({stem}) Labels"
            if show_labels:
                labels_path = os.path.join(self._task_dir, "labels", label_filename)
                if not os.path.exists(labels_path):
                    # genuinely-optional set (e.g. single-model run has no _nuc.zarr) — skip,
                    # but make it visible rather than silent so a wrong path is debuggable.
                    print(f"[show_labels] skip: not on disk: {labels_path}", flush=True)
                    continue

                store = zarr.open(labels_path, mode="r")
                n_levels = len(self._im_data) if self._im_data else 1
                arrays, lvl = [], 0
                while lvl < n_levels and str(lvl) in store:
                    arrays.append(da.from_zarr(store[str(lvl)]))
                    lvl += 1
                if not arrays:
                    arrays = [da.from_zarr(store)]
                # a present-but-unreadable label set is a real error — surface it, don't no-op.
                if not arrays:
                    raise RuntimeError(f"no label arrays loaded from {labels_path}")

                _remove_layer(self._viewer, layer_name)
                from cecelia.utils import napari_utils
                layer = napari_utils.add_labels(
                    self._viewer, arrays if len(arrays) > 1 else arrays[0],
                    name=layer_name, scale=self._im_scale, units=self._im_units, opacity=0.7,
                )
                print(f"[show_labels] added {layer_name}: shape={layer.data.shape} "
                      f"scale={self._im_scale}", flush=True)
            else:
                _remove_layer(self._viewer, layer_name)

    def _labels_color_dict(self, labels, vals, is_cat, percentile: float = 99.5):
        """Per-label RGBA dict for a DirectLabelColormap. Categorical → the Okabe–Ito palette per
        level; continuous → viridis normalised to the [100-p, p] percentile range (like the old R
        `show_channel_intensity`). NaN, background (0) and unmapped (None) → transparent."""
        import matplotlib.pyplot as plt
        color_dict = {}
        finite = vals[~np.isnan(vals)]
        if is_cat and len(finite):
            levels = sorted({int(round(v)) for v in finite})
            lvl_colour = {lvl: _CATEGORICAL_RGBA[i % len(_CATEGORICAL_RGBA)]
                          for i, lvl in enumerate(levels)}
            for lab, v in zip(labels, vals):
                color_dict[int(lab)] = (0., 0., 0., 0.) if np.isnan(v) else lvl_colour[int(round(v))]
        elif len(finite):
            vmin = float(np.percentile(finite, 100 - percentile))
            vmax = float(np.percentile(finite, percentile))
            rng = (vmax - vmin) or 1.0
            for lab, v in zip(labels, vals):
                if np.isnan(v):
                    color_dict[int(lab)] = (0., 0., 0., 0.)
                else:
                    t = min(1.0, max(0.0, (v - vmin) / rng))
                    color_dict[int(lab)] = tuple(float(c) for c in plt.cm.viridis(t))
        color_dict[0] = (0., 0., 0., 0.)       # background label
        color_dict[None] = (0., 0., 0., 0.)    # any label not in the dict
        return color_dict

    def colour_labels(self, value_name: str = "default", column: str = "", percentile: float = 99.5):
        """Recolour `value_name`'s Labels layer by an obs column via a DirectLabelColormap
        (continuous → viridis, categorical → palette per level). `column=""` restores the layer's
        original colormap. Ports the old `show_channel_intensity` (per-label color_dict), updated from
        the deprecated `layer.color` dict to napari's `DirectLabelColormap`."""
        if self._task_dir is None:
            raise RuntimeError("call set_task_dir before colour_labels")
        # target this value_name's Labels layer(s); fall back to every Labels layer if none matched
        targets = [l for l in self._viewer.layers
                   if getattr(l, "name", "").endswith(") Labels") and f"({value_name})" in l.name]
        if not targets:
            targets = [l for l in self._viewer.layers if getattr(l, "name", "").endswith(") Labels")]
        if not targets:
            return
        if not column:                                   # reset to the remembered default colormap
            for l in targets:
                orig = self._labels_orig_cmap.pop(l.name, None)
                if orig is not None:
                    l.colormap = orig
            return
        lab, vals, is_cat = self._read_label_column(value_name, column)
        cmap = napari.utils.DirectLabelColormap(
            color_dict=self._labels_color_dict(lab, vals, is_cat, percentile))
        for l in targets:
            self._labels_orig_cmap.setdefault(l.name, l.colormap)   # remember the original once
            l.colormap = cmap

    # ── Populations (linked brushing with the flow plots) ─────────────────────

    def _display_axes(self):
        """Non-channel image axes, in display order (e.g. ['t','z','y','x'])."""
        if not self._axes:
            return []
        return [a.lower() for a in self._axes if a.lower() != "c"]

    def _centroid_matrix(self, value_name: str):
        """Return (labels, C, axes): per-cell centroid coordinates as an (n, n_display_dim)
        array in display-axis order, read once from the H5AD and cached. Maps the H5AD's
        `centroid-i` (skimage z,y,x order) + temporal `t` onto the image's display axes."""
        if value_name in self._centroid_cache:
            return self._centroid_cache[value_name]
        from cecelia.utils.label_props_utils import LabelPropsView
        path = os.path.join(self._task_dir, "labelProps", f"{value_name}.h5ad")
        view = LabelPropsView(path)
        centroid_cols = view.centroid_columns()
        temporal_cols = view.temporal_columns()
        df = view.only_centroid_cols().as_df()
        view.close()

        labels = df["label"].to_numpy().astype(int)
        display_axes = self._display_axes()
        spatial_axes = [a for a in display_axes if a in ("z", "y", "x")]   # z,y,x order
        temporal_axes = [a for a in display_axes if a == "t"]
        axis_to_col = {}
        for ax, col in zip(spatial_axes, centroid_cols):
            axis_to_col[ax] = col
        for ax, col in zip(temporal_axes, temporal_cols):
            axis_to_col[ax] = col
        axes = [a for a in display_axes if a in axis_to_col]
        C = (df[[axis_to_col[a] for a in axes]].to_numpy(dtype=float)
             if axes else np.empty((len(df), 0)))
        res = (labels, C, axes)
        self._centroid_cache[value_name] = res
        return res

    @staticmethod
    def pop_layer_name(pop_type: str, value_name: str, name: str) -> str:
        # prefixed by pop_type + the SEGMENTATION value_name so several segmentations coexist as
        # separate layers (e.g. "(flow) (T) /qc" AND "(flow) (B) /qc"). Mirrors track_layer_name.
        return f"({pop_type}) ({value_name}) {name}"

    @staticmethod
    def _pop_layer_vn(pop_type: str, layer_name: str):
        # extract the segmentation value_name from a pop layer name "(pop_type) (VN) path" (None if
        # it doesn't match) — used to scope reconciliation to specific segmentations.
        prefix = f"({pop_type}) ("
        if not layer_name.startswith(prefix):
            return None
        rest = layer_name[len(prefix):]
        end = rest.find(")")
        return rest[:end] if end >= 0 else None

    def show_populations(self, pops, value_name: str = "default",
                         points_size: int = 6, pop_type: str = "flow",
                         value_names=None, scoped: bool = False):
        """Reconcile the population Points layers **per pop** — update existing layers in place,
        add new ones, remove only the gone (deleted/renamed) ones, and **skip layers that didn't
        change** (same membership + colour + size + visibility). This avoids the old full flush
        (remove + re-add every layer on every gating change), which was prohibitively slow on
        CODEX images (many populations × many cells): a single gate edit now touches only the
        one population (+ descendants) that actually changed. Membership (label IDs) comes from
        Julia; centroids are read locally from the H5AD.

        `scoped` + `value_names`: a SCOPED push (a live gate edit, which recomputed only the edited
        segmentation) prunes stale layers ONLY within `value_names`, leaving other segmentations'
        layers intact. A full push (scoped False) prunes globally, so a vanished pop/segmentation is
        still cleaned up on the next open / master toggle."""
        if self._task_dir is None:
            raise RuntimeError("call set_task_dir before show_populations")

        # name layers by SEGMENTATION value_name + full population path (root/A/B/C → "(flow) (T)
        # /A/B/C"), not the leaf name — so pops from several segmentations coexist. Each pop carries
        # its own `value_name` (default to the call's for older senders); centroids are read per
        # value_name (cached). Mirrors show_tracks, which is already multi-segmentation.
        desired = {}
        for p in pops:
            vn = p.get("value_name", value_name)
            desired[self.pop_layer_name(pop_type, vn, p.get("path") or p["name"])] = (vn, p)

        # remove layers whose population is gone (deleted / renamed) — per layer, not a flush.
        # A scoped push only prunes within the pushed segmentations (a live edit that recomputed just
        # those); a full push prunes across all segmentations of this pop_type.
        scope = set(value_names) if (scoped and value_names) else None
        for name in [l.name for l in self._viewer.layers
                     if l.name.startswith(f"({pop_type}") and l.name not in desired
                     and (scope is None or self._pop_layer_vn(pop_type, l.name) in scope)]:
            _remove_layer(self._viewer, name)
            self._pop_sigs.pop(name, None)
        if not desired:
            return

        # per-value_name centroid matrices, cached across pops sharing a segmentation
        mats = {}
        def _mat(vn):
            if vn not in mats:
                labels, C, _ = self._centroid_matrix(vn)
                mats[vn] = (labels, C, {int(l): i for i, l in enumerate(labels)}) if len(C) else None
            return mats[vn]

        for name, (vn, pop) in desired.items():
            m = _mat(vn)
            if m is None:
                continue
            labels, C, label_to_row = m
            ids     = [int(l) for l in pop.get("label_ids", [])]
            colour  = pop.get("colour", "#ffffff")
            visible = pop.get("show", True)
            # signature of everything that affects the rendered layer; unchanged → skip
            sig = (hash(tuple(ids)), colour, points_size, visible)
            layer = self._viewer.layers[name] if name in self._viewer.layers else None
            if layer is not None and self._pop_sigs.get(name) == sig:
                continue

            rows = [label_to_row[i] for i in ids if i in label_to_row]
            pts  = C[rows, :] if rows else np.empty((0, C.shape[1]))
            props = {"label_id": [int(labels[r]) for r in rows]}
            if layer is not None:
                # mutate in place — no destroy/recreate (napari only redraws this layer)
                layer.data = pts
                layer.properties = props
                layer.face_color = colour
                layer.size = points_size
                layer.visible = visible
            else:
                self._viewer.add_points(
                    pts, name=name, face_color=colour, border_color="black",
                    size=points_size, scale=self._im_scale, units=self._im_units,
                    visible=visible, properties=props, blending="translucent_no_depth",
                )
            self._pop_sigs[name] = sig

    # ── Tracks (napari native Tracks layer) ──────────────────────────────────

    def _tracks_matrix(self, value_name: str):
        """Return (tracks, track_ids, vertex_labels) for napari's Tracks layer. `tracks` is
        (n_vertices, 1+D) with columns `[track_id, t, (z,) y, x]` — the per-cell centroid matrix
        (display-axis order, which starts with t — see `_display_axes`) prefixed with the cell's
        `track_id`. `vertex_labels` is the cell label per vertex (same order), so any obs column can be
        mapped onto the vertices for `color_by`. Only cells with `track_id > 0` are kept; rows are
        sorted by (track_id, t) so napari links each track's vertices in time order. Ports R
        `napari_utils.show_tracks`'s tracks-array construction. Cached per value_name."""
        if value_name in self._tracks_cache:
            return self._tracks_cache[value_name]
        labels, C, axes = self._centroid_matrix(value_name)
        if len(C) == 0 or "t" not in axes:        # tracks need a time axis
            res = (np.empty((0, C.shape[1] + 1)), np.empty(0, dtype=int), np.empty(0, dtype=int))
            self._tracks_cache[value_name] = res
            return res
        # read track_id (cell obs), aligned to the centroid-matrix labels
        from cecelia.utils.label_props_utils import LabelPropsView
        path = os.path.join(self._task_dir, "labelProps", f"{value_name}.h5ad")
        view = LabelPropsView(path)
        tdf = view.view_cols(["track_id"]).as_df()   # label + track_id
        view.close()
        tid_by_label = {int(l): t for l, t in zip(tdf["label"], tdf["track_id"])}
        tids = np.array([tid_by_label.get(int(l), np.nan) for l in labels], dtype=float)
        keep = ~np.isnan(tids) & (np.nan_to_num(tids) > 0)
        tids_i = tids[keep].astype(int)
        vlabels = labels[keep]
        tracks = np.column_stack([tids_i, C[keep, :]])
        t_col = 1 + axes.index("t")               # +1 for the prepended track_id column
        order = np.lexsort((tracks[:, t_col], tracks[:, 0]))   # sort by track_id, then t
        tracks = tracks[order]
        tids_i = tids_i[order]
        vlabels = vlabels[order]
        res = (tracks, tids_i, vlabels)
        self._tracks_cache[value_name] = res
        return res

    def _read_label_column(self, value_name: str, column: str):
        """Read an obs column aligned to cell labels → (labels:int[], values:float[], is_categorical).
        Non-numeric columns are factorised to integer codes (and treated as categorical); a numeric
        column with few integer-like levels is also categorical (e.g. an HMM state). NaN stays NaN.
        Cached per (value_name, column). Mirrors the old `show_channel_intensity` value read."""
        key = (value_name, column)
        if key in self._colcol_cache:
            return self._colcol_cache[key]
        import pandas as pd
        from cecelia.utils.label_props_utils import LabelPropsView
        path = os.path.join(self._task_dir, "labelProps", f"{value_name}.h5ad")
        view = LabelPropsView(path)
        df = view.view_cols([column]).as_df()        # label + column
        view.close()
        labels = df["label"].to_numpy().astype(int)
        raw = df[column]
        vals = pd.to_numeric(raw, errors="coerce").to_numpy(dtype=float)
        if np.count_nonzero(~np.isnan(vals)) == 0:   # non-numeric → factorise (categorical)
            codes, _ = pd.factorize(raw.astype(str))
            vals = np.where(codes < 0, np.nan, codes.astype(float))
            is_cat = True
        else:
            uniq = np.unique(vals[~np.isnan(vals)])
            is_cat = len(uniq) <= 12 and np.allclose(uniq, np.round(uniq))
        res = (labels, vals, is_cat)
        self._colcol_cache[key] = res
        return res

    def _categorical_track_colormap(self, present_values):
        """A step napari Colormap over the present categorical values so a level gets the **same**
        Okabe–Ito colour the labels use (consistent colour scheme across the Tracks and Labels layers
        for the same column). Real levels (sorted) → `_CATEGORICAL_RGBA[i]`; missing (-1) → grey.
        Returns None if there's nothing to map (caller falls back to a named colormap)."""
        import napari
        pv = sorted({float(v) for v in present_values})
        if not pv:
            return None
        reals = [v for v in pv if v >= 0]
        cmap_of = {v: _CATEGORICAL_RGBA[i % len(_CATEGORICAL_RGBA)] for i, v in enumerate(reals)}
        for v in pv:
            if v < 0:
                cmap_of[v] = (0.6, 0.6, 0.6, 1.0)        # missing → grey (≈ labels' transparent)
        if len(pv) == 1:
            c = cmap_of[pv[0]]
            return napari.utils.Colormap(colors=[c, c], controls=[0.0, 1.0], interpolation="zero")
        lo, hi = pv[0], pv[-1]; span = (hi - lo) or 1.0
        pos = [(v - lo) / span for v in pv]
        colors = [cmap_of[v] for v in pv]                # one colour per value (step / 'zero' interp)
        controls = [0.0] + [(a + b) / 2 for a, b in zip(pos, pos[1:])] + [1.0]
        return napari.utils.Colormap(colors=colors, controls=controls, interpolation="zero")

    @staticmethod
    def track_layer_name(value_name: str, path: str, pop_type: str = "track") -> str:
        # prefixed by pop_type + the SEGMENTATION value_name (e.g. "(track) (C) Tracks /_tracked"),
        # so tracks from several segmentations (A/B/C) AND pop types (track gates vs trackclust
        # cluster pops) are distinguishable and never collide in the layer list.
        return f"({pop_type}) ({value_name}) Tracks {path}"

    def show_tracks(self, pops, value_name: str = "default",
                    tail_width: int = 4, tail_length: int = 30, pop_type: str = "track",
                    color_by: str = ""):
        """Render track populations as napari Tracks layers — one layer per pop, named by the pop's
        **segmentation** (`value_name`) so several segmentations show side by side. Each pop carries
        its own `value_name` + `track_ids`; the per-segmentation track vertices are read locally
        (`_tracks_matrix`, cached per value_name) and bin-masked to the pop's track_ids. Reconciles
        like `show_populations`: layers absent from `pops` are removed, unchanged ones skipped. By
        default coloured by `track_id` (turbo); when `color_by` names an obs column (e.g. an HMM
        state) each vertex is shaded by that column — categorical → turbo, continuous → viridis
        (ports old R `show_tracks` color_by). NaN → -1 (R's fillna(-1)).

        NOTE (future phase): the old R `show_tracks(split_tracks=…)` rendered ONE layer per cluster
        value (each a flat colour, independently toggle-able) — for the upcoming **Leiden
        track-clustering** phase (cluster whole tracks → one layer each), NOT the per-timepoint
        colour-by here. Add a `split_by`/per-value layer path here when that lands."""
        if self._task_dir is None:
            raise RuntimeError("call set_task_dir before show_tracks")

        # each pop is keyed by its own segmentation; default to the call's value_name if absent
        desired = {}
        for p in pops:
            vn = p.get("value_name", value_name)
            pt = p.get("pop_type", "track")
            desired[self.track_layer_name(vn, p.get("path") or p["name"], pt)] = (vn, p)
        # remove any stale Tracks layer not in `desired` (across ALL segmentations) — " Tracks "
        # uniquely identifies track layers (points are "(pt) /path", labels "(vn) Labels")
        for name in [l.name for l in self._viewer.layers
                     if " Tracks " in l.name and l.name not in desired]:
            _remove_layer(self._viewer, name)
            self._track_sigs.pop(name, None)
        if not desired:
            return

        cby = color_by if color_by and color_by != "track_id" else ""
        # build the tracks matrix + colour-by values ONCE per segmentation (cached across pops)
        per_vn = {}
        for vn, _ in desired.values():
            if vn in per_vn:
                continue
            tracks, all_tids, all_vlabels = self._tracks_matrix(vn)
            col_vals, col_cmap, col_cmaps_dict = None, "turbo", None
            if cby and len(tracks):
                try:
                    lab, vals, is_cat = self._read_label_column(vn, cby)
                    vbl = {int(l): v for l, v in zip(lab, vals)}
                    col_vals = np.nan_to_num(
                        np.array([vbl.get(int(l), np.nan) for l in all_vlabels], dtype=float), nan=-1.0)
                    if is_cat:
                        # categorical → Okabe–Ito step colormap (matches the labels' colours); fall back
                        # to turbo if it can't be built
                        cm = self._categorical_track_colormap(col_vals)
                        if cm is not None:
                            col_cmaps_dict, col_cmap = {cby: cm}, None
                    else:
                        col_cmap = "viridis"             # continuous → viridis (matches labels)
                except Exception as e:               # column missing for this segmentation → default colouring
                    print(f"[show_tracks] colour_by '{cby}' unavailable for {vn}: {e}", flush=True)
            per_vn[vn] = (tracks, all_tids, col_vals, col_cmap, col_cmaps_dict)

        for name, (vn, pop) in desired.items():
            tracks, all_tids, col_vals, col_cmap, col_cmaps_dict = per_vn[vn]
            if len(tracks) == 0:
                continue
            ids     = set(int(t) for t in pop.get("track_ids", []))
            visible = pop.get("show", True)
            use_cby = cby if col_vals is not None else ""
            sig = (vn, hash(tuple(sorted(ids))), tail_width, tail_length, visible, use_cby)
            existing = name in self._viewer.layers
            if existing and self._track_sigs.get(name) == sig:
                continue
            mask = np.array([t in ids for t in all_tids]) if ids else np.zeros(len(all_tids), bool)
            sub = tracks[mask, :]
            existing and _remove_layer(self._viewer, name)   # Tracks graph can't be mutated in place
            if len(sub) > 0:
                props = {"track_id": sub[:, 0].astype(int).tolist()}
                if use_cby:
                    props[use_cby] = col_vals[mask].tolist()
                # Delegate to the shared helper (passes scale AND units so napari keeps unit-aware
                # rendering across layers). Categorical colour-by → the per-level Okabe–Ito
                # `colormaps_dict` (consistent with labels); otherwise a named colormap (viridis
                # continuous / turbo by track_id). See docs/todo/CECELIA_NAPARI_UPSTREAM_PLAN.md.
                from cecelia.utils import napari_utils
                napari_utils.add_tracks(
                    self._viewer, sub, name=name,
                    scale=self._im_scale, units=self._im_units, properties=props,
                    color_by=(use_cby or "track_id"), tail_width=tail_width, tail_length=tail_length,
                    colormap=(col_cmap or "turbo"),
                    colormaps_dict=(col_cmaps_dict if use_cby else None),
                )
            self._track_sigs[name] = sig

    # ── Spatial cell selection → POST back to Julia (linked brushing) ─────────

    def start_cell_selection(self, project_uid: str, image_uid: str,
                             value_name: str, api_url: str,
                             z_mode: str = "stack", z_window: int = 0):
        """Add a Shapes layer; when the user draws on it, resolve which cell centroids fall
        inside and POST the label IDs to {api_url}/api/napari/event.

        `z_mode="slice"` restricts the selection to cells whose z-centroid is within `z_window`
        slices of the **currently displayed** z (read live when the polygon is closed); `"stack"`
        (default) ignores z and selects across the whole stack. No-op on images without a z axis.

        On a timelapse (t axis) the selection is ALWAYS restricted to the currently displayed
        timepoint — a drawn region means "these cells, at this frame", not every frame's cells in
        that XY tube (which would over-select by the frame count)."""
        if self._task_dir is None:
            raise RuntimeError("call set_task_dir before start_cell_selection")
        self._sel_ctx = {"project_uid": project_uid, "image_uid": image_uid,
                         "value_name": value_name, "api_url": api_url,
                         "z_mode": z_mode, "z_window": int(z_window)}
        # napari only allows drawing/editing Shapes in 2-D display — drop out of 3-D render mode
        # so the user can actually draw the selection polygon on a 3-D image.
        if self._viewer.dims.ndisplay != 2:
            self._viewer.dims.ndisplay = 2
        _remove_layer(self._viewer, SELECTION_LAYER)   # also drops old event handlers
        # match the image layer's scale + units so the polygon aligns with the cells and napari
        # doesn't warn "Inconsistent units across layers". An EMPTY Shapes layer defaults to
        # ndim=2, so we must pass ndim explicitly or a length-N scale raises a broadcast error
        # (and then no layer gets added at all).
        shp_kwargs = dict(name=SELECTION_LAYER, edge_color="cyan",
                          face_color="transparent", edge_width=2)
        if self._im_scale is not None:
            shp_kwargs.update(scale=self._im_scale, units=self._im_units,
                              ndim=len(self._im_scale))
        layer = self._viewer.add_shapes(**shp_kwargs)
        layer.mode = "add_polygon"
        # automatic commit: when the user closes a polygon, events.data fires and we push the
        # cells inside it to the flow plots (no key press / no polling needed).
        layer.events.data.connect(self._on_selection_changed)
        self._viewer.layers.selection.active = layer

    def _on_selection_changed(self, event=None):
        if self._sel_ctx is None:
            return
        try:
            layer = self._viewer.layers[SELECTION_LAYER]
        except KeyError:
            return
        shapes = [np.asarray(s) for s in layer.data]
        usable = [s for s in shapes if s.shape[0] >= 3]   # closed polygons only
        # Mid-draw (a polygon with <3 vertices) fires events.data repeatedly — don't spam the
        # API with empty selections while the user is still clicking. Only act when there's a
        # closed polygon, or when the user explicitly cleared all shapes (→ clear the selection).
        if not usable:
            if not shapes:
                self._post_selection([])
            return

        labels, C, axes = self._centroid_matrix(self._sel_ctx["value_name"])
        if len(C) == 0:
            return

        from matplotlib.path import Path
        # Test in the currently displayed dims only (the polygon lives in that plane); other
        # dims (z/t) are ignored, so "these XY cells" selects across slices. The centroid matrix
        # C is indexed by the viewer's displayed-dim indices. The polygon vertices, however, are
        # only 2-D when drawn in a 2-D slice of an N-D image (napari gives them just the in-plane
        # coords) — so index them by their OWN columns, not the viewer dim indices (which would
        # overflow a 2-column shape, e.g. "index 2 out of bounds for axis 1 with size 2").
        disp = [d for d in self._viewer.dims.displayed if d < C.shape[1]]
        if len(disp) < 2:
            return
        pts = C[:, disp]
        inside = np.zeros(len(C), dtype=bool)
        for shp in usable:
            if shp.shape[1] == len(disp):
                poly = shp                       # already in-plane (displayed) coords
            elif shp.shape[1] > max(disp):
                poly = shp[:, disp]              # full N-D vertices → pick the displayed dims
            else:
                continue                         # can't align this shape safely
            inside |= Path(poly).contains_points(pts)

        # z-slice scope: optionally keep only cells near the currently displayed z. The polygon is
        # always 2-D (in-plane), so by default a selection spans the whole z-stack; "slice" mode
        # restricts it to cells whose z-centroid is within ±z_window slices of the live z. The z
        # value is read here (not at start) so scrolling to a different slice before closing the
        # polygon selects on that slice. No-op when there's no z axis (2-D image).
        if self._sel_ctx.get("z_mode") == "slice" and "z" in axes:
            display_axes = self._display_axes()
            try:
                z_now = self._viewer.dims.current_step[display_axes.index("z")]
            except (ValueError, IndexError):
                z_now = None
            if z_now is not None:
                win = int(self._sel_ctx.get("z_window", 0))
                inside &= np.abs(np.round(C[:, axes.index("z")]) - z_now) <= win

        # timelapse scope: a region drawn on the image means "these cells, at the frame you're
        # looking at". The polygon test is in-plane only and ignores t, so WITHOUT this every
        # detection in the XY tube across ALL timepoints is selected (e.g. 64× on a 64-frame movie)
        # — the "way too many cells" symptom. Always restrict to the currently displayed timepoint
        # (read live, like z above, so scrolling to another frame before closing selects on it).
        if "t" in axes:
            display_axes = self._display_axes()
            try:
                t_now = self._viewer.dims.current_step[display_axes.index("t")]
            except (ValueError, IndexError):
                t_now = None
            if t_now is not None:
                inside &= np.round(C[:, axes.index("t")]).astype(int) == int(t_now)

        self._post_selection([int(x) for x in labels[inside]])

    def _post_selection(self, label_ids):
        ctx = self._sel_ctx
        if ctx is None:
            return
        body = json.dumps({
            "type": "cellSelection",
            "projectUid": ctx["project_uid"], "imageUid": ctx["image_uid"],
            "valueName": ctx["value_name"], "labels": label_ids,
        }).encode()
        url = ctx["api_url"].rstrip("/") + "/api/napari/event"

        def _do():
            try:
                req = urllib.request.Request(
                    url, data=body, method="POST",
                    headers={"Content-Type": "application/json"})
                urllib.request.urlopen(req, timeout=10).read()
            except Exception as e:
                print(f"[napari] cell-selection POST failed: {e}", flush=True)

        threading.Thread(target=_do, daemon=True).start()

    def update_selection_scope(self, z_mode=None, z_window=None):
        """Change the z scope of the *active* cell selection and re-evaluate the current polygon
        immediately — so toggling slice/stack or the ± window updates the flow plots without the
        user redrawing. No-op if no selection is active."""
        if self._sel_ctx is None:
            return
        if z_mode is not None:
            self._sel_ctx["z_mode"] = z_mode
        if z_window is not None:
            self._sel_ctx["z_window"] = int(z_window)
        self._on_selection_changed()   # re-run point-in-polygon (+ z filter) on the drawn shape

    # ── Layer management ──────────────────────────────────────────────────────

    def show_layer(self, name: str):
        if name in self._viewer.layers:
            self._viewer.layers[name].visible = True

    def hide_layer(self, name: str):
        if name in self._viewer.layers:
            self._viewer.layers[name].visible = False

    def remove_layer(self, name: str):
        _remove_layer(self._viewer, name)

    # ── Camera ────────────────────────────────────────────────────────────────

    def centre(self, pos, tp=None, zoom=None):
        self._viewer.camera.center = pos
        if tp is not None:
            step = list(self._viewer.dims.current_step)
            step[0] = tp
            self._viewer.dims.current_step = step
        if zoom is not None:
            self._viewer.camera.zoom = zoom

    # ── Persistence ───────────────────────────────────────────────────────────

    # ── Live autosave (debounced) ───────────────────────────────────────────────

    def configure_autosave(self, path: str, enabled: bool):
        """Point live autosave at `path` for the currently open image and (re)wire the change events.
        Called by the app AFTER each open (layers are recreated per open, so we must reconnect to the
        fresh layers), and again when the user toggles the setting while an image is open."""
        self._autosave_path = path
        self._autosave_enabled = bool(enabled)
        self._reconnect_autosave()

    def _reconnect_autosave(self):
        # drop connections to the previous image's (now-destroyed) layers
        for emitter, cb in self._autosave_conns:
            try:
                emitter.disconnect(cb)
            except Exception:
                pass
        self._autosave_conns = []
        self._autosave_timer.stop()
        if not self._autosave_enabled:
            return
        cb = self._schedule_autosave
        # per Image-layer display props …
        for layer in self._viewer.layers:
            if type(layer).__name__ == "Image":
                for ev in (layer.events.contrast_limits, layer.events.gamma,
                           layer.events.colormap, layer.events.opacity,
                           layer.events.blending, layer.events.visible):
                    ev.connect(cb)
                    self._autosave_conns.append((ev, cb))
        # … and the viewer's T/Z slider position
        ev = self._viewer.dims.events.current_step
        ev.connect(cb)
        self._autosave_conns.append((ev, cb))

    def _schedule_autosave(self, event=None):
        # ignore changes we cause ourselves while applying loaded props
        if not self._autosave_enabled or self._autosave_loading or not self._autosave_path:
            return
        self._autosave_timer.start()   # single-shot restart → coalesces a burst into one write

    def _autosave_flush(self):
        if not self._autosave_enabled or not self._autosave_path:
            return
        try:
            self.save_layer_props(self._autosave_path)
        except Exception:
            pass

    # ── Persistence (also used for the on-switch save/load) ──────────────────────

    def save_layer_props(self, filepath: str):
        props = {"Image": []}
        _keys = [
            "opacity", "blending", "visible", "gamma",
            "contrast_limits", "colormap",
        ]
        for layer in self._viewer.layers:
            if type(layer).__name__ == "Image":
                props["Image"].append({
                    k: getattr(layer, k).name if k == "colormap" else getattr(layer, k)
                    for k in _keys
                })
        # viewer dims position (the T/Z slider) so the image reopens on the same frame/slice
        try:
            props["dims"] = {"current_step": list(self._viewer.dims.current_step)}
        except Exception:
            pass
        # atomic write (tmp + os.replace) so a crash/kill never leaves a half-written props file —
        # the image always reopens in a valid remembered state.
        tmp = filepath + ".tmp"
        with open(tmp, "wb") as f:
            pickle.dump(props, f, pickle.HIGHEST_PROTOCOL)
            f.flush()
            os.fsync(f.fileno())
        os.replace(tmp, filepath)

    def load_layer_props(self, filepath: str):
        with open(filepath, "rb") as f:
            data = pickle.load(f)
        self._autosave_loading = True   # applying these must not trigger a write-back
        try:
            entries = list(reversed(data.get("Image", [])))
            for layer in self._viewer.layers:
                if type(layer).__name__ == "Image" and entries:
                    for k, v in entries.pop().items():
                        setattr(layer, k, v)
            # restore the T/Z slider, clamped to this image's dims (a different segmentation/shape
            # may have fewer steps) — preserve current_step length, only override saved axes.
            dims = data.get("dims") or {}
            saved = dims.get("current_step")
            if saved is not None:
                try:
                    cur = list(self._viewer.dims.current_step)
                    nsteps = self._viewer.dims.nsteps
                    for i in range(len(cur)):
                        if i < len(saved) and i < len(nsteps):
                            cur[i] = max(0, min(int(saved[i]), int(nsteps[i]) - 1))
                    self._viewer.dims.current_step = tuple(cur)
                except Exception:
                    pass
        finally:
            self._autosave_loading = False

    # ── Screenshot ────────────────────────────────────────────────────────────

    def save_screenshot(self, path: str, canvas_only: bool = True):
        self._viewer.window.screenshot(path, canvas_only=canvas_only, flash=False)

    # ── Task dir (needed for labels / props) ──────────────────────────────────

    def set_task_dir(self, path: str):
        self._task_dir = path


# ── OME-ZARR helpers ──────────────────────────────────────────────────────────

def _series_base(path: str) -> str:
    """Return the path containing resolution-level directories.
    bioformats2raw: multiscales lives in path/0  → return path/0
    flat OME-ZARR:  multiscales lives in path    → return path
    Uses zarr API so it works for both zarr v2 (.zattrs) and zarr v3 (zarr.json).
    """
    import os
    series = os.path.join(path, "0")
    if not os.path.isdir(series):
        return path
    try:
        g = zarr.open_group(series, mode='r')
        if g.attrs.get("multiscales"):
            return series
    except Exception:
        pass
    return path


# contrast-from-sample moved to the shared cecelia.utils.napari_utils.set_contrast_from_sample
# (open_image delegates via napari_utils.add_image(contrast=True)); coastal mirrors it.


def _open_zarr_multiscale(path: str, as_dask: bool = True) -> list:
    """Return list of arrays, one per resolution level.
    as_dask=True (default): lazy dask arrays — chunked, computed on demand.
    as_dask=False: raw zarr arrays — direct chunk access, no dask overhead.
    """
    import os
    base = _series_base(path)
    store = zarr.open_group(base, mode='r')
    arrays, level = [], 0
    while str(level) in store:
        arr = store[str(level)]
        if as_dask:
            arr = da.from_array(arr, chunks=arr.chunks)
        arrays.append(arr)
        level += 1
    if not arrays:
        root = zarr.open_group(path, mode='r')
        arr = root[next(iter(root))] if len(root) else zarr.open(path, mode='r')
        if as_dask:
            arr = da.from_array(arr, chunks=arr.chunks)
        arrays.append(arr)
    return arrays


def _read_multiscales_meta(path: str) -> dict:
    """Return the first multiscales entry, checking series dir first.
    Uses zarr API to handle both zarr v2 (.zattrs) and zarr v3 (zarr.json).
    """
    import os
    for candidate in [os.path.join(path, "0"), path]:
        if not os.path.isdir(candidate):
            continue
        try:
            g = zarr.open_group(candidate, mode='r')
            ms = g.attrs.get("multiscales")
            if ms:
                return ms[0] if isinstance(ms, list) else {}
        except Exception:
            continue
    return {}


def _read_axes(path: str):
    ms = _read_multiscales_meta(path)
    axes = ms.get("axes", [])
    return [ax["name"] for ax in axes] if axes else None


@lru_cache(maxsize=64)
def _parse_ome_xml(xml_path: str, _mtime: float):
    """Parse one OME-XML file. Cached on (path, mtime) — a long-lived bridge parses each store's
    metadata at most once (mtime in the key invalidates it if the file is rewritten). `ome_types`
    is imported lazily here so its pydantic model-build cost is paid on first use, not at import."""
    from ome_types import from_xml
    with open(xml_path) as f:
        return from_xml(f.read())


def _load_ome_xml(path: str):
    """Locate and parse a store's OME-XML (OME/METADATA.ome.xml or METADATA.ome.xml), or None.
    Shared by the readers below so a single open parses the file once instead of two or three times."""
    import os
    for candidate in (
        os.path.join(path, "OME", "METADATA.ome.xml"),
        os.path.join(path, "METADATA.ome.xml"),
    ):
        if os.path.isfile(candidate):
            try:
                return _parse_ome_xml(candidate, os.path.getmtime(candidate))
            except Exception:
                return None
    return None


def _read_unit_from_ome_xml(path: str) -> str:
    """Read physical pixel unit from OME-XML metadata. Returns 'µm' as default."""
    omexml = _load_ome_xml(path)
    if omexml is not None:
        try:
            unit = omexml.images[0].pixels.physical_size_x_unit
            if unit is not None:
                return unit.value  # e.g. 'µm', 'nm', 'mm'
        except Exception:
            pass
    return 'µm'


def _read_scale_from_ome_xml(path: str, axes):
    """Read physical pixel scale from OME-XML metadata, returning values in axis order."""
    omexml = _load_ome_xml(path)
    if omexml is None:
        return None
    try:
        pixels = omexml.images[0].pixels
        sizes = {
            'x': pixels.physical_size_x,
            'y': pixels.physical_size_y,
            'z': pixels.physical_size_z,
        }
        return [float(sizes.get(ax.lower()) or 1.0) for ax in axes]
    except Exception:
        return None


def _read_scale(path: str):
    """Return scale list (one value per axis, physical units) or None."""
    ms = _read_multiscales_meta(path)
    for dataset in ms.get("datasets", [])[:1]:
        for t in dataset.get("coordinateTransformations", []):
            if t.get("type") == "scale":
                return t["scale"]
    # Fallback: read from OME-XML when zarr metadata has no coordinateTransformations.
    axes = _read_axes(path)
    if axes:
        return _read_scale_from_ome_xml(path, axes)
    return None


def _read_time_increment(path: str):
    """Seconds between timepoints from OME-XML `pixels.time_increment` (with its unit), or None.
    Used for the timecourse timestamp overlay. Converts ms/min/h to seconds where the unit says so."""
    omexml = _load_ome_xml(path)
    if omexml is None:
        return None
    try:
        pixels = omexml.images[0].pixels
        inc = pixels.time_increment
        if inc is None:
            return None
        secs = float(inc)
        unit = getattr(getattr(pixels, "time_increment_unit", None), "value", None)
        return {"ms": secs / 1000.0, "min": secs * 60.0, "h": secs * 3600.0}.get(unit, secs)
    except Exception:
        return None


def _remove_layer(viewer: napari.Viewer, name: str):
    if name in viewer.layers:
        viewer.layers.remove(name)


# ── WebSocket command dispatcher ──────────────────────────────────────────────

def execute_command(state: NapariState, cmd: dict) -> dict:
    t = cmd.get("type")
    try:
        if t == "ping":
            return {"type": "pong"}

        elif t == "gl_info":
            return {"type": "gl_info", **_gl_info()}

        elif t == "open_image":
            state.open_image(
                path=cmd["path"],
                channel_names=cmd.get("channel_names"),
                channel_colormaps=cmd.get("channel_colormaps"),
                show_3d=cmd.get("show_3d", False),
                as_dask=cmd.get("as_dask", True),
                visible=cmd.get("visible", True),
            )

        elif t == "show_labels":
            state.show_labels(
                value_name=cmd.get("value_name", "default"),
                label_files=cmd.get("label_files", None),
                show_labels=cmd.get("show_labels", True),
                show_points=cmd.get("show_points", False),
            )

        elif t == "colour_labels":
            state.colour_labels(
                value_name=cmd.get("value_name", "default"),
                column=cmd.get("column", ""),
                percentile=cmd.get("percentile", 99.5),
            )

        elif t == "set_task_dir":
            state.set_task_dir(cmd["path"])

        elif t == "show_populations":
            state.show_populations(
                pops=cmd.get("pops", []),
                value_name=cmd.get("value_name", "default"),
                points_size=cmd.get("points_size", 6),
                pop_type=cmd.get("pop_type", "flow"),
                value_names=cmd.get("value_names"),
                scoped=cmd.get("scoped", False),
            )

        elif t == "show_tracks":
            state.show_tracks(
                pops=cmd.get("pops", []),
                value_name=cmd.get("value_name", "default"),
                tail_width=cmd.get("tail_width", 4),
                tail_length=cmd.get("tail_length", 30),
                pop_type=cmd.get("pop_type", "track"),
                color_by=cmd.get("color_by", ""),
            )

        elif t == "start_cell_selection":
            state.start_cell_selection(
                project_uid=cmd["project_uid"],
                image_uid=cmd["image_uid"],
                value_name=cmd.get("value_name", "default"),
                api_url=cmd.get("api_url", "http://localhost:8080"),
                z_mode=cmd.get("z_mode", "stack"),
                z_window=cmd.get("z_window", 0),
            )

        elif t == "update_selection_scope":
            state.update_selection_scope(
                z_mode=cmd.get("z_mode"),
                z_window=cmd.get("z_window"),
            )

        elif t == "show_layer":
            state.show_layer(cmd["name"])

        elif t == "hide_layer":
            state.hide_layer(cmd["name"])

        elif t == "remove_layer":
            state.remove_layer(cmd["name"])

        elif t == "centre":
            state.centre(cmd["pos"], tp=cmd.get("tp"), zoom=cmd.get("zoom"))

        elif t == "save_layer_props":
            state.save_layer_props(cmd["path"])

        elif t == "load_layer_props":
            state.load_layer_props(cmd["path"])

        elif t == "configure_autosave":
            state.configure_autosave(cmd.get("path"), bool(cmd.get("enabled", False)))

        elif t == "save_screenshot":
            state.save_screenshot(cmd["path"], canvas_only=cmd.get("canvas_only", True))

        elif t == "clear":
            state.clear()

        else:
            return {"type": "error", "msg": f"unknown command: {t}"}

        return {"type": "ok", "cmd": t}

    except Exception as e:
        return {"type": "error", "cmd": t, "msg": str(e)}


# ── Qt / asyncio glue ─────────────────────────────────────────────────────────

command_queue: queue.Queue = queue.Queue()
_state: NapariState | None = None


def drain_queue():
    while not command_queue.empty():
        cmd, resp_q = command_queue.get()
        result = execute_command(_state, cmd)
        resp_q.put(result)


async def handle(websocket):
    async for message in websocket:
        cmd = json.loads(message)
        resp_q: queue.Queue = queue.Queue()
        command_queue.put((cmd, resp_q))
        # hand off the blocking wait to a worker thread so asyncio stays free
        result = await asyncio.get_event_loop().run_in_executor(
            None, resp_q.get
        )
        await websocket.send(json.dumps(result))


async def ws_server():
    import websockets
    async with websockets.serve(handle, HOST, PORT):
        print(f"napari bridge ready on ws://{HOST}:{PORT}", flush=True)
        await asyncio.Future()


def run_ws_server():
    asyncio.run(ws_server())


def _port_available(host: str, port: int) -> bool:
    """True if we can bind the WS port. If not, another bridge already owns it — we must
    NOT start, or we'd open a second napari window with no working WS server (the WS thread
    would silently die on the bind error, leaving a zombie window)."""
    import socket
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        s.bind((host, port))
        return True
    except OSError:
        return False
    finally:
        s.close()


def _gl_info() -> dict:
    """Query the OpenGL renderer/vendor/version the process is using, so discrete-GPU offload is
    self-evident. GPU selection (the __NV_PRIME_*/__GLX_*/DRI_PRIME env the Julia side sets when 'Use
    discrete GPU' is on) is process-wide, so a throwaway offscreen context reports the same GPU
    napari's canvas renders on — and this avoids reaching into napari's private canvas internals.
    Returns a dict; on failure it carries an 'error' key rather than raising (never fatal). Must run
    on the Qt main thread (GL context creation) — the WS command dispatcher already does."""
    try:
        from qtpy.QtGui import QOffscreenSurface, QOpenGLContext
        surface = QOffscreenSurface()
        surface.create()
        ctx = QOpenGLContext()
        if not ctx.create() or not ctx.makeCurrent(surface):
            return {"renderer": "unavailable", "vendor": "", "version": "",
                    "error": "could not create a GL context"}
        try:
            from OpenGL.GL import (GL_RENDERER, GL_VENDOR, GL_VERSION, glGetString)
            info = {"renderer": glGetString(GL_RENDERER).decode(errors="replace"),
                    "vendor":   glGetString(GL_VENDOR).decode(errors="replace"),
                    "version":  glGetString(GL_VERSION).decode(errors="replace")}
        finally:
            ctx.doneCurrent()
        return info
    except Exception as e:
        return {"renderer": "unavailable", "vendor": "", "version": "", "error": str(e)}


def _log_gl_renderer():
    """Also print the renderer to stdout (belt-and-braces for anyone watching the raw terminal; the
    Julia side additionally logs it as @info via the gl_info command, which is where the app console
    surfaces it)."""
    info = _gl_info()
    if info.get("error"):
        print(f"[napari] GL renderer: {info['renderer']} ({info['error']})", flush=True)
    else:
        print(f"[napari] GL renderer: {info['renderer']}  |  vendor: {info['vendor']}"
              f"  |  {info['version']}", flush=True)


def main():
    global _state

    if not _port_available(HOST, PORT):
        print(f"napari bridge: port {PORT} is already in use — another bridge is running. "
              f"Exiting instead of opening a zombie window.", flush=True)
        sys.exit(1)

    threading.Thread(target=run_ws_server, daemon=True).start()

    viewer = napari.Viewer()
    _state = NapariState(viewer)

    timer = QTimer()
    timer.timeout.connect(drain_queue)
    timer.start(100)

    _log_gl_renderer()
    print("napari viewer started", flush=True)
    napari.run()


if __name__ == "__main__":
    main()
