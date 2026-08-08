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
import queue
import sys
import threading
import time
import urllib.request

import dask.array as da
import napari
import numpy as np
from qtpy.QtCore import QTimer

# Shared cecelia readers/helpers — the bridge delegates to these instead of hand-rolling its own.
#   napari_utils : generic viewer.add_* / colormap / view-state / clip-plane / movie builders
#                  (shared with coastal); only needs numpy, so top-level.
#   zarr_utils   : the ONE way to open an OME-ZARR + read its NGFF axes/scale (open_as_zarr,
#                  read_axes, read_scale); light deps, so top-level.
# ome_xml_utils (read_pixel_unit / read_time_increment) is imported LAZILY where used, so ome-types'
# pydantic build cost is paid on first image open rather than at bridge startup.
from cecelia.utils import block_transfer, movie_io, napari_utils, zarr_utils

HOST = "localhost"
PORT = 7655

# Incoming-message cap. `websockets` defaults to 1 MiB, which the task preview crosses: it ships a
# whole label block as one JSON message, and a full-frame uint32 mask of a large image compresses to
# roughly 840 KB → ~1.1 MB once base64'd (measured ratios in `cecelia.utils.block_transfer`). Hitting
# the default is not a graceful degradation — the server rejects the frame and closes the connection,
# so the preview would fail on big images only. Set explicitly rather than left implicit.
WS_MAX_SIZE = 64 * 1024 * 1024

# bridge process start time — reported in the ping reply so the backend/Settings panel can show the
# bridge's uptime and spot a STALE bridge (it survives a backend restart; see docs/NAPARI.md restart rules)
_STARTED_AT = time.time()

#: Command-surface version, reported by `ping` and checked before the backend ADOPTS this process.
#: Mirrored by `NAPARI_PROTOCOL` in app/src/napari.jl; a test asserts the two agree.
#:
#: Uptime was already reported here "to spot a stale bridge", but that left the judgement to a human
#: reading a number. A bridge outlives the backend by design (adopted after a crash or a Ctrl-C), so it
#: can be running code from before a branch switch while the backend sends it commands from after —
#: which is not a graceful degradation: it surfaced once as `unexpected keyword argument 'mask'` and
#: once as a bare "Preview failed", neither naming the real cause.
#:
#: Bump whenever the command surface changes shape: a new/renamed command, a changed argument, or a
#: changed reply. 1: the surface as of the protocol's introduction. 2: the movie recorders take
#: `size_x`/`size_y` (and `scale` is gone), and `ping` reports the canvas size. 3: `stitch_movies`
#: (side-by-side version comparison), the recorders take `frame_offset`/`frame_total` so a multi-pass
#: job drives one progress bar, and they take `show_timestamp`/`show_scale_bar`.
PROTOCOL = 3

# name of the Shapes layer used for spatial cell selection (linked brushing → flow plots)
SELECTION_LAYER = "Cell selection"

# qualitative palette for colouring labels/tracks by a CATEGORICAL obs column (e.g. HMM state).
# Okabe–Ito (colourblind-safe), as RGBA floats in 0..1 — matches the web canvas 'okabe-ito' palette.
_CATEGORICAL_RGBA = [
    (0.902, 0.624, 0.000, 1.0), (0.337, 0.706, 0.914, 1.0), (0.000, 0.620, 0.451, 1.0),
    (0.941, 0.894, 0.259, 1.0), (0.000, 0.447, 0.698, 1.0), (0.835, 0.369, 0.000, 1.0),
    (0.800, 0.475, 0.655, 1.0), (0.580, 0.580, 0.580, 1.0),
]


# Colour hex↔RGBA conversion lives in the shared toolkit (napari_utils.hex_to_rgba / rgba_to_hex)
# so every napari colour path parses hex ONE way; the bridge just calls it.

# The shared helpers (cecelia.utils.napari_utils generic layer/colour builders, and
# cecelia.utils.label_props_utils for cell data) resolve via the editable `cecelia` install in the
# pixi env (python/pyproject.toml + the pixi `cecelia` dep) — no sys.path manipulation needed.
# napari_utils is imported at module top (it only needs numpy); the heavier cecelia data readers
# (label_props_utils → anndata) stay lazily imported inside the methods that use them. Launched via
# `pixi run napari`.


# ── State class (mirrors NapariUtils) ─────────────────────────────────────────

class NapariState:
    """Maintains viewer + image state between WebSocket commands."""

    def __init__(self, viewer: napari.Viewer):
        self._viewer = viewer
        # set when open_image is called
        self._im_data = None          # list[dask array], one per multiscale level
        self._im_scale = None         # scale without channel axis, e.g. [z, y, x] µm
        self._preview_layers = set()   # layer names the task preview added (see _remove_preview_layers)
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
        self._autosave_path = None     # target .json props file for the currently open image, or None
        self._autosave_enabled = False
        self._autosave_loading = False # True while applying loaded props → suppress the write-back
        self._autosave_conns = []      # [(emitter, cb)] connected for the current image; dropped on reconnect
        self._autosave_timer = QTimer()
        self._autosave_timer.setSingleShot(True)
        self._autosave_timer.setInterval(500)   # debounce window: one write ~500ms after the last change
        self._autosave_timer.timeout.connect(self._autosave_flush)

        # Keep label layers RENDERABLE in 3D — see _sync_label_levels. Connected here, on the viewer's
        # own event, rather than at the places we set `ndisplay` ourselves: the user can also flip 2D/3D
        # from napari's own button, and that has to behave the same as the movie panel's z control.
        self._viewer.dims.events.ndisplay.connect(lambda _e=None: self._sync_label_levels())

    # ── Viewer lifecycle ───────────────────────────────────────────────────────

    # napari renders a MULTISCALE layer at its COARSEST level in 3D — automatic level selection is a
    # 2D-viewport calculation, so there is nothing to compute once the whole volume is on screen:
    #
    #     elif slice_input.ndisplay == 3:
    #         data_level = len(data) - 1        # napari/layers/_scalar_field/scalar_field.py
    #
    # For an intensity image that is fine: a coarse image still looks like the image. For LABELS it is
    # not, because our pyramids are built by strided subsampling (`create_slices_multiscales`), not by
    # a mode filter — level n keeps every 2^n-th voxel per axis, so at the coarsest level a segmentation
    # of ordinary-sized cells is almost entirely background. Switching the movie's z control to 3D
    # therefore made the masks vanish.
    #
    # So pin label layers to full resolution while the viewer is in 3D, and hand the level back to
    # napari in 2D, where the automatic choice is what keeps panning a large image fast.
    #
    # LEVEL 0, not "the finest level under some memory budget" — full resolution costs memory on a big
    # volume and that is accepted: pixelated masks are not an acceptable 3D view (Dominik, 2026-08-08).
    # Don't reintroduce a coarser choice as an optimisation. `locked_data_level`
    # is public API as of napari 0.7.1 (the pinned version); the guard keeps an older napari on today's
    # behaviour instead of crashing.
    def _sync_label_levels(self):
        in_3d = self._viewer.dims.ndisplay == 3
        for layer in self._viewer.layers:
            if not isinstance(layer, napari.layers.Labels) or not getattr(layer, "multiscale", False):
                continue
            if not hasattr(layer, "locked_data_level"):
                print("[labels] napari has no locked_data_level; 3D will render the coarsest level",
                      flush=True)
                return
            try:
                layer.locked_data_level = 0 if in_3d else None
            except Exception as e:
                # never let a display nicety take the viewer down
                print(f"[labels] could not pin {layer.name} to full resolution: {e}", flush=True)

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
        # `layers.clear()` just removed the preview layer, but the view listener is attached to
        # dims/camera and would survive it — left on, it keeps posting "the view moved" for a preview
        # that no longer exists, and the frontend would re-request against the newly opened image.
        self._detach_view_listener()

        # open the store + read its geometry through the shared cecelia readers (the same code the
        # analysis pipeline uses) — one implementation of "open an OME-ZARR, read its axes/scale".
        # ome_xml_utils is imported lazily so ome-types' pydantic build cost is paid on first open,
        # not at bridge startup.
        from cecelia.utils import ome_xml_utils
        self._im_data, _ = zarr_utils.open_as_zarr(path, as_dask=as_dask)

        # read axes and scale from the NGFF metadata (series-level, set by bioformats2raw)
        self._axes = zarr_utils.read_axes(path)
        full_scale = zarr_utils.read_scale(path)   # one value per axis, e.g. [t, c, z, y, x]

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
            # Per-axis units, not one unit repeated. The time axis is seconds, the spatial axes
            # micrometres; labelling them all with the pixel unit renders a correct frame
            # interval as "10 um". Prefer the NGFF `axes[].unit` written at import and fall back
            # to the OME pixel unit for stores written before that existed.
            unit = ome_xml_utils.read_pixel_unit(path)
            ngff_units = zarr_utils.read_axis_units(path) or {}
            self._im_units = tuple(
                ngff_units.get(ax.lower()) or unit
                for i, ax in enumerate(self._axes) if i != self._channel_axis
            )

        # Delegate the actual layer creation to the SHARED generic helper (cecelia.utils.napari_utils,
        # which coastal mirrors): per-channel colormaps, additive blending, contrast-from-sample and the
        # list-name guard live there so both projects render identically. The bridge keeps all the
        # disk/scale/units logic above. See docs/todo/CECELIA_NAPARI_UPSTREAM_PLAN.md.
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

    def set_z_view(self, show_3d: bool = False, z=None):
        """Show the WHOLE z stack as a 3D render, or a single z SLICE in 2D.

        Both layer kinds follow the viewer's `ndisplay`, which is why this is one switch rather than a
        per-layer setting: a Labels layer cannot be projected at all (`Labels.projection_mode` accepts
        only `'none'` — napari raises `ValueError` for `'max'`), so "the whole stack" for a MASK can
        only mean the volumetric render. Flattening the channels with a thick slice would therefore
        show a projected image against a single-plane mask, which is worse than either.

        A 2D image is left alone: forcing a flat plane into a rotatable 3D view helps nobody, which is
        the same guard `open_image` applies to its `show_3d` flag. `z=None` in 2D keeps whatever slice
        is showing. Returns the state actually reached, so the caller can tell it was refused.
        """
        z_len = self._z_axis_len() or 0
        if show_3d and z_len > 1:
            self._viewer.dims.ndisplay = 3
            self._viewer.reset_view()
            return {"ndisplay": 3, "z": None}

        self._viewer.dims.ndisplay = 2
        if z is not None and z_len > 1:
            axes = self._display_axes()
            if "z" in axes:
                idx = axes.index("z")
                step = list(self._viewer.dims.current_step)
                nsteps = self._viewer.dims.nsteps
                if idx < len(step) and idx < len(nsteps):
                    # clamp: the caller's z came from a config that may outlive the image it was
                    # written against (a cropped version has fewer planes)
                    step[idx] = max(0, min(int(z), int(nsteps[idx]) - 1))
                    self._viewer.dims.current_step = tuple(step)
        cur = self._viewer.dims.current_step
        axes = self._display_axes()
        at = int(cur[axes.index("z")]) if "z" in axes and axes.index("z") < len(cur) else None
        return {"ndisplay": 2, "z": at}

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

    def _data_extent_um(self):
        """Physical (µm) size of the full data extent along x and y, from data shape × per-axis scale.
        `export_figure` tight-fits the capture to the data extent, so this IS the captured frame's
        physical width/height — the frontend draws a correct vector scale bar from it (Phase E2). Returns
        `{"x": w, "y": h, "unit": …}` or None if scale/axes unknown. `_im_scale`/display axes exclude the
        channel axis and share order, so the same index selects a shape value and its scale."""
        if not self._axes or not self._im_data or self._im_scale is None:
            return None
        try:
            low   = [a.lower() for a in self._axes]
            disp  = [a for a in low if a != "c"]                              # matches _im_scale order
            shape = self._im_data[0].shape
            shape_noc = [s for i, s in enumerate(shape) if i != self._channel_axis]
            out = {}
            for ax in ("x", "y"):
                if ax in disp:
                    j = disp.index(ax)
                    if j < len(shape_noc) and j < len(self._im_scale):
                        out[ax] = float(shape_noc[j]) * float(self._im_scale[j])
            if "x" not in out or "y" not in out:
                return None
            out["unit"] = self._im_units[0] if self._im_units else None
            return out
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
        # NGFF time-axis scale first, OME-XML fallback — one resolver, so the overlay works
        # for stores that carry only one of the two (see zarr_utils.read_time_increment).
        interval = zarr_utils.read_time_increment(path)      # seconds per frame, or None
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

    def _invalidate_colcol_cache(self, value_name: str):
        """Drop cached colour-by reads (`_colcol_cache`) for `value_name`'s cell AND branch
        sidecars. `_colcol_cache` is keyed by `(value_name, column)` with no mtime check, so
        re-running a task (segmentation, branching, measure) against the SAME value_name while
        the image stays open in napari would otherwise keep colouring the freshly (re)loaded
        layer with values read from the OLD h5ad, before the re-run overwrote it — e.g. a branch
        layer re-shown after a `preDilationSize` change rendering with the previous run's
        branch-type distribution. Must be called whenever a Labels/Branches layer is (re)loaded."""
        branch_key = "__branch::" + value_name
        self._colcol_cache = {k: v for k, v in self._colcol_cache.items()
                              if k[0] not in (value_name, branch_key)}

    def _label_store_path(self, subdir: str, label_filename: str) -> str:
        """Absolute path of one label store. `subdir` is the store family — "labels" for cell
        segmentations, "branchLabels" for skeletons (mirrors `img_labels_dir`/`img_branch_labels_dir`
        on the Julia side)."""
        return os.path.join(self._task_dir, subdir, label_filename)

    def _show_label_stores(self, subdir: str, suffix: str, value_name: str,
                           label_files: list, show: bool, cache: bool,
                           levels: int = None, contour: int = 0, after_add=None):
        """Add or remove one family's label layers — the ONE implementation behind `show_labels`,
        `show_branch_labels` and the live preview.

        Those three differ only in the store subdirectory, the layer-name suffix, and what happens
        after a layer is added; opening the store, aligning it to the viewer's axes and adding it is
        identical, and had already drifted into two near-copies before a third was needed here.

        `levels` caps the pyramid depth (default: the image's level count, so label levels line up
        with the image's). `after_add(value_name, layer)` runs per added layer. A store that isn't on
        disk is skipped and logged — that's a legitimate case (a single-model run writes no `_nuc`
        set) — but a store that IS present and unreadable raises, because that's a real fault.
        """
        if self._task_dir is None:
            raise RuntimeError(f"call set_task_dir before showing {subdir}")
        if label_files is None:
            label_files = [f"{value_name}.zarr"]
        self._invalidate_colcol_cache(value_name)

        for label_filename in label_files:
            # name by the value_name → "(C) Labels", never "(C.zarr) Labels" or the staging path
            stem = _label_layer_stem(label_filename)
            layer_name = f"({stem}) {suffix}"
            if not show:
                _remove_layer(self._viewer, layer_name)
                continue

            labels_path = self._label_store_path(subdir, label_filename)
            if not os.path.exists(labels_path):
                print(f"[{subdir}] skip: not on disk: {labels_path}", flush=True)
                continue

            # labels are a flat multiscales store (segmentation_utils writes axes + numeric
            # `datasets`), same layout as an image — open via the shared reader.
            n_levels = levels if levels is not None else \
                (len(self._im_data) if self._im_data else None)
            arrays, _ = zarr_utils.open_zarr(labels_path, multiscales=n_levels, as_dask=True)
            # a present-but-unreadable label set is a real error — surface it, don't no-op.
            if not arrays:
                raise RuntimeError(f"no label arrays loaded from {labels_path}")

            # A stem holds at most ONE layer of its family at a time, so adding evicts every sibling
            # suffix — that is what swaps a live preview for the finished set (and back) instead of
            # leaving two layers of the same labels stacked on each other.
            for other in _LABEL_SUFFIXES.get(subdir, (suffix,)):
                _remove_layer(self._viewer, f"({stem}) {other}")
            layer = napari_utils.add_labels(
                self._viewer, arrays if len(arrays) > 1 else arrays[0],
                name=layer_name, scale=self._im_scale, units=self._im_units, opacity=0.7,
                cache=cache, contour=contour,
                # align the layer's dims to the viewer's BY NAME (see expand_to_axes) — a store
                # with fewer axes than the image would otherwise have them read from the right —
                # and stretch a projected store across the axis it collapsed (lazy; no bytes).
                # This is also why a Z-FLATTENED skeleton of a timelapse renders as a CURTAIN
                # through z rather than a tower: it belongs to the whole volume, not to z=0
                # (ports the old R create_branching, which wrote the MIP onto every z plane).
                axes=zarr_utils.read_axes(labels_path), image_axes=self._display_axes(),
                image_shape=self._display_shape(),
            )
            print(f"[{subdir}] added {layer_name}: shape={layer.data.shape} "
                  f"scale={self._im_scale} cache={cache} levels={len(arrays)}", flush=True)
            if after_add is not None:
                after_add(value_name, layer)

        # A layer ADDED while the viewer is already in 3D never sees an `ndisplay` event, so pin it here
        # too — otherwise turning a mask on during a 3D session shows nothing. Cheap and idempotent.
        self._sync_label_levels()

    def show_labels(self, value_name: str = "default",
                    label_files: list = None,
                    show_labels: bool = True, show_points: bool = False,
                    cache: bool = False, preview: bool = False, contour: int = 0):
        """Add or remove the cell-segmentation labels layers for a value_name.

        `show_points` is INERT and always has been — centroid points are their own command
        (`show_populations`, which knows about pop types and colours). Kept only so the existing wire
        payload from `show_labels!` stays valid; don't build anything on it.

        `contour` draws each label as an outline of that many pixels rather than a filled region
        (0 = filled, napari's default) — an outline keeps the channel signal under the mask readable.

        `preview=True` shows a store that is still being WRITTEN by a running segmentation, in its own
        `({vn}) Labels (live)` layer. Two things differ, and both are forced here rather than trusted
        to the caller:

        * **Level 0 only.** A label store declares its whole pyramid in `.zattrs` when it is created
          but only holds level 0 until the writer finalises it (`_finalize_label_pyramid`), so asking
          for the image's level count would raise `KeyError: '1'`. The preview therefore renders at
          full resolution at every zoom — the honest cost of watching an unfinished store.
        * **Caching off.** The whole point is to see bytes that changed since the last look, and
          napari's cachey would serve the old ones (see `napari_utils.add_labels` on why dask task
          names make that cache dangerous for re-run labels specifically).

        A preview also reads the run's STAGING store, not the final path — during a re-run the final
        path still holds the PREVIOUS segmentation until the run completes, so pointing the preview
        there would quietly show the old labels while the new ones are computed. Callers normally
        pass `label_files` from the task's `live_outputs` declaration, which already names the
        staging stores; the fallback below matches it.
        """
        if preview and label_files is None:
            label_files = [f"{value_name}.zarr{zarr_utils.STAGING_SUFFIX}"]
        self._show_label_stores(
            "labels", "Labels (live)" if preview else "Labels",
            value_name, label_files, show_labels,
            cache=False if preview else cache,
            levels=1 if preview else None,
            contour=contour,
        )

    def refresh_labels(self, value_name: str = "default", label_files: list = None):
        """Re-read an in-progress label store into its EXISTING live-preview layer.

        Reassigns `layer.data` from a freshly opened view of the same store, which is what actually
        forces the re-read: `layer.refresh()` alone re-slices whatever the layer already holds. Shape
        is stable by construction — the store is allocated at its full final shape before the first
        frame is written — so this is always a like-for-like swap, and cheap enough to call on every
        progress tick. A value_name with no preview layer is a silent no-op (the user turned it off,
        or the run finished and the real layer took over).

        Reads the run's STAGING store (see `show_labels`), which the finishing run RENAMES onto the
        final path — so the store can disappear between this refresh deciding to read it and actually
        reading it. That race is expected and benign: the run has just finished, and the task-finished
        handler is about to replace this layer with the real one. Never let it raise.
        """
        if self._task_dir is None:
            raise RuntimeError("call set_task_dir before refresh_labels")
        if label_files is None:
            label_files = [f"{value_name}.zarr{zarr_utils.STAGING_SUFFIX}"]

        for label_filename in label_files:
            layer_name = f"({_label_layer_stem(label_filename)}) Labels (live)"
            if layer_name not in self._viewer.layers:
                continue
            labels_path = self._label_store_path("labels", label_filename)
            if not os.path.exists(labels_path):
                continue
            try:
                arrays, _ = zarr_utils.open_zarr(labels_path, multiscales=1, as_dask=True)
            except Exception as e:
                # promoted out from under us mid-refresh — see the docstring
                print(f"[refresh_labels] skip {layer_name}: {e}", flush=True)
                continue
            if not arrays:
                continue
            self._viewer.layers[layer_name].data = arrays[0]
            print(f"[refresh_labels] {layer_name}", flush=True)

    # ── View-change notification, for re-previewing what the user is now looking at ────────────
    #
    # A preview of one region goes stale the moment the view moves, and a stale mask reads as broken.
    # The viewer is the only thing that knows the view changed, so it says so: the same POST-back
    # channel the cell selection uses (`/api/napari/event`), which the backend relays over WS.
    #
    # Coalesced HERE as well as in the frontend, and both are load-bearing: a single pan emits camera
    # events continuously, so without a bridge-side timer this would post hundreds of HTTP requests per
    # drag — the frontend's debounce would collapse them into one preview, but only after the flood had
    # already been sent. The frontend window is the one tuned for GPU cost; this one just stops the
    # firehose.
    #
    # And the coalesced post is DEDUPED against the region it last reported, which is what keeps this
    # from being a feedback loop: the events we listen to are proxies (camera zoom/centre, slider,
    # ndisplay) for the only thing that matters — which pixels a preview would run on. Anything that
    # fires one of them without moving that box (a canvas refresh, a window resize, our own layer
    # swap) would otherwise trigger a preview, whose layer swap can fire them again, forever. Posting
    # only on a real region change makes a self-sustaining loop impossible rather than unlikely.
    _VIEW_EVENT_COALESCE_S = 0.15

    def _attach_view_listener(self, api_url: str):
        if getattr(self, "_view_listener_url", None) == api_url:
            return                                  # already listening for this endpoint
        self._detach_view_listener()
        self._view_listener_url = api_url
        self._view_timer = None
        self._view_lock = threading.Lock()
        # the region we last reported; None = nothing reported yet, so the first change always posts
        self._last_posted_region = None

        def on_view_change(_event=None):
            with self._view_lock:
                if self._view_timer is not None:
                    self._view_timer.cancel()
                self._view_timer = threading.Timer(
                    self._VIEW_EVENT_COALESCE_S, self._post_view_changed)
                self._view_timer.daemon = True
                self._view_timer.start()

        self._on_view_change = on_view_change
        self._viewer.dims.events.current_step.connect(on_view_change)
        self._viewer.camera.events.zoom.connect(on_view_change)
        self._viewer.camera.events.center.connect(on_view_change)
        self._viewer.dims.events.ndisplay.connect(on_view_change)

    def _detach_view_listener(self):
        cb = getattr(self, "_on_view_change", None)
        if cb is not None:
            for emitter in (self._viewer.dims.events.current_step,
                            self._viewer.camera.events.zoom,
                            self._viewer.camera.events.center,
                            self._viewer.dims.events.ndisplay):
                try:
                    emitter.disconnect(cb)
                except Exception:
                    pass                            # already gone; detaching must never raise
        timer = getattr(self, "_view_timer", None)
        if timer is not None:
            timer.cancel()
        self._on_view_change = None
        self._view_timer = None
        self._view_listener_url = None
        self._last_posted_region = None

    def _post_view_changed(self):
        url = getattr(self, "_view_listener_url", None)
        if not url:
            return
        # Did the previewable region actually move? An event that leaves it identical would produce a
        # byte-identical preview, so reporting it is pure cost — and the loop fuel described above.
        # Quiet, because this runs per coalesced event; the region is printed when one is really used.
        try:
            region = self.preview_region(verbose=False)
        except Exception:
            return                              # nothing previewable (no image layer) → nothing to say
        if region == getattr(self, "_last_posted_region", None):
            return
        body = json.dumps({"type": "viewChanged"}).encode()
        try:
            req = urllib.request.Request(
                url.rstrip("/") + "/api/napari/event", data=body, method="POST",
                headers={"Content-Type": "application/json"})
            urllib.request.urlopen(req, timeout=10).read()
        except Exception as e:
            print(f"[preview] view-changed POST failed: {e}", flush=True)
            return          # NOT recorded as posted: the frontend never heard, so let the next
                            # event for this same region try again rather than dedupe the retry away
        self._last_posted_region = region

    def show_task_preview(self, value_name: str = "default", layers: list = None,
                          region: dict = None, show: bool = True, api_url: str = None):
        """Show one region's task preview as IN-MEMORY layers.

        The counterpart to `_show_label_stores`, and deliberately not routed through it: there is no
        store. The worker computes one plane and returns the blocks themselves
        (`cecelia.utils.block_transfer`), which land here as full-extent lazy arrays with that one block
        filled in — so each layer aligns with the image by shape alone, no `translate`, and nothing is
        written to the user's project.

        A reply carries a LIST, because one task can preview several things:

        * ``kind="labels"`` — a segmentation mask. Takes the `Preview` slot in the `labels` family, so
          it evicts (and is evicted by) `({vn}) Labels` and `({vn}) Labels (live)`: all three are the
          same value_name's segmentation, and stacking them would show one mask over another.
        * ``kind="image"`` — a corrected channel, e.g. from AF correction. Added as an Image layer named
          after the channel it corrects (`(default) nuc-GFP AF`) so it sits beside the original and can
          be toggled against it — comparing corrected to raw IS the judgement being made, so they have
          to coexist rather than evict each other.

        `show=False` (or a preview that produced nothing) removes every layer this has added.
        """
        stem = value_name
        if not show or not layers:
            self._remove_preview_layers(stem)
            # stop reporting view changes: nothing is chasing the view any more
            self._detach_view_listener()
            return

        # Carry the user's display settings across the re-preview. Replacing the layers is what resets
        # them: moving the T or Z slider fires `viewChanged` → re-preview → remove + re-add, so a
        # contrast window the user had just dialled in was wiped every time they scrolled — while
        # scrolling through t/z is exactly how you judge a correction. Keyed by layer NAME, so a
        # parameter change that outputs different channels simply finds nothing to restore.
        kept = {name: napari_utils.capture_layer_props(self._viewer.layers[name])
                for name in sorted(getattr(self, "_preview_layers", set()))
                if name in self._viewer.layers}

        # replace the previous preview wholesale: a parameter change can alter which channels a task
        # even outputs, so leaving stale layers behind would show a mix of two parameter sets
        self._remove_preview_layers(stem)
        added = []
        for spec in layers:
            kind = str(spec.get("kind", "labels"))
            name = str(spec.get("name", _PREVIEW_SUFFIX))
            axes = list(spec.get("axes") or [])
            shape = spec.get("shape")
            if not shape or not axes:
                raise ValueError("preview layer needs 'shape' and 'axes'")
            block = block_transfer.decode_block(spec["block"])
            data = block_transfer.place_block_lazy(block, shape, axes, region or {})
            layer_name = f"({stem}) {name}"

            if kind == "labels":
                self._invalidate_colcol_cache(value_name)
                for other in _LABEL_SUFFIXES["labels"]:
                    _remove_layer(self._viewer, f"({stem}) {other}")
                layer = napari_utils.add_labels(
                    self._viewer, data, name=layer_name,
                    scale=self._im_scale, units=self._im_units, opacity=0.7,
                    # cache off for the same reason as a live store preview: consecutive previews of the
                    # same region produce same-shaped dask arrays, and a served-from-cache plane would
                    # show the PREVIOUS parameters' result — the one thing this feature exists to avoid.
                    cache=False,
                    axes=axes, image_axes=self._display_axes(),
                    image_shape=self._display_shape(),
                )
            elif kind == "image":
                layer = napari_utils.add_image(
                    self._viewer, data, name=layer_name,
                    scale=self._im_scale, units=self._im_units,
                    axes=axes, image_axes=self._display_axes(),
                    image_shape=self._display_shape(),
                    cache=False,
                )
                # Render the corrected channel in its ORIGINAL's colour. Comparing the two is the
                # judgement being made, and napari's default grey makes a magenta channel's correction
                # look like a different measurement rather than the same one. Contrast limits are
                # deliberately NOT copied — the corrected values live on a different scale (a ratio
                # rescaled to the dtype), so the original's window would usually show black.
                self._mirror_source_colormap(layer, spec.get("source"))
            else:
                raise ValueError(f"unknown preview layer kind {kind!r}")

            # Last, so the user's own adjustment outranks the defaults above — including a colormap they
            # changed by hand, which should not be reverted to the source channel's on every scroll.
            # Guarded inside `apply_layer_props`: a restored contrast window can fall outside the new
            # block's range, and skipping it is better than failing the preview.
            napari_utils.apply_layer_props(layer, kept.get(layer_name))

            self._preview_layers.add(layer_name)
            added.append(layer)
            print(f"[preview] added {layer_name} ({kind}): block={tuple(block.shape)} "
                  f"extent={tuple(int(x) for x in shape)} region={region}", flush=True)

        # only now that a preview is actually on screen does a view change mean anything
        if api_url:
            self._attach_view_listener(api_url)
        return added

    def _mirror_source_colormap(self, layer, source: str = None):
        """Give a derived layer the colormap of the layer it came from.

        Best-effort on purpose. The source channel may not be a layer at all — a preview can name a
        channel the user has closed, and an older worker (protocol < 3) sends no `source` — and in
        every one of those cases the right answer is napari's default, not a failed preview. So this
        never raises: the preview is the point, the colour is a courtesy.
        """
        if not source:
            return
        src = next((l for l in self._viewer.layers if l.name == source), None)
        if src is None:
            return
        try:
            layer.colormap = src.colormap
        except Exception as e:                       # a Labels source, an exotic colormap — not fatal
            print(f"[preview] could not mirror colormap from {source!r}: {e}", flush=True)

    def _remove_preview_layers(self, stem: str):
        """Remove every layer a previous preview added for this value_name.

        Tracked by name rather than rediscovered by suffix, because an image preview's name comes from
        the CHANNEL it corrects — there is no fixed suffix to scan for, and guessing one would either
        miss layers (leaving a stale parameter set on screen) or delete a user's own.
        """
        for name in sorted(getattr(self, "_preview_layers", set())):
            _remove_layer(self._viewer, name)
        self._preview_layers = set()
        # the labels slot is also removable by its fixed name, for a preview added before a restart
        _remove_layer(self._viewer, f"({stem}) {_PREVIEW_SUFFIX}")

    def show_branch_labels(self, value_name: str = "default",
                           label_files: list = None, show_labels: bool = True,
                           cache: bool = False):
        """Add or remove the skeleton labels layer written by `segment.branching`. Parallel to
        `show_labels` but the store lives in `branchLabels/`, not `labels/`, and the layer is
        namespaced `({vn}) Branches` so it doesn't collide with regular Labels. The generic labels
        picker never lists branch labels (docs/todo/BRANCHING_PLAN.md Decision 6)."""
        def _default_colour_by(vn, _layer):
            # Default colour-by branch-type (ports the old R `show_branching` behaviour).
            # Routed through `_classify_column` (see `_read_branch_column`), so branch-type
            # ∈ {0,1,2,3} is correctly detected as CATEGORICAL and gets 4 distinct Okabe-Ito
            # colours (one per skan type), not a continuous viridis ramp. Bridge-side so every
            # show hits the same default. Best-effort — a missing column / broken sidecar
            # shouldn't fail the layer add; the user still sees the layer, just in raw label colours.
            try:
                self.colour_branch_labels(value_name=vn, column="branch-type", percentile=100.0)
            except Exception as e:
                print(f"[show_branch_labels] default colour-by branch-type skipped: {e}",
                      flush=True)

        self._show_label_stores("branchLabels", "Branches", value_name, label_files,
                                show_labels, cache=cache, after_add=_default_colour_by)

    def _labels_color_dict(self, labels, vals, is_cat, percentile: float = 99.5, overrides=None):
        """Per-label RGBA dict for a DirectLabelColormap, plus a `{value(str) -> '#hex'}` legend for
        the categorical case. Categorical → a user population's colour where one covers the level
        (`overrides`), else the Okabe–Ito palette per level; continuous → viridis over the [100-p, p]
        percentile range. NaN, background (0) and unmapped (None) → transparent. Returns
        (color_dict, legend)."""
        import matplotlib.pyplot as plt
        overrides = overrides or {}
        color_dict = {}
        legend = {}
        finite = vals[~np.isnan(vals)]
        if is_cat and len(finite):
            levels = sorted({int(round(v)) for v in finite})
            lvl_colour = {}
            for i, lvl in enumerate(levels):
                ov = napari_utils.hex_to_rgba(overrides.get(str(lvl)))         # user pop colour, or…
                rgba = ov if ov is not None else _CATEGORICAL_RGBA[i % len(_CATEGORICAL_RGBA)]  # …default
                lvl_colour[lvl] = rgba
                legend[str(lvl)] = napari_utils.rgba_to_hex(rgba)
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
        return color_dict, legend

    def colour_labels(self, value_name: str = "default", column: str = "", percentile: float = 99.5,
                      overrides=None):
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
            return {}
        if not column:                                   # reset to the remembered default colormap
            for l in targets:
                orig = self._labels_orig_cmap.pop(l.name, None)
                if orig is not None:
                    l.colormap = orig
            return {}
        lab, vals, is_cat = self._read_label_column(value_name, column)
        color_dict, legend = self._labels_color_dict(lab, vals, is_cat, percentile, overrides)
        cmap = napari.utils.DirectLabelColormap(color_dict=color_dict)
        for l in targets:
            self._labels_orig_cmap.setdefault(l.name, l.colormap)   # remember the original once
            l.colormap = cmap
        return legend                                    # {value(str) -> '#hex'} for the UI legend

    def _read_branch_column(self, value_name: str, column: str):
        """Read a per-branch obs column from `labelProps/{value_name}__branch.h5ad`. Returns
        `(labels, values, is_categorical)` in the same shape as `_read_label_column`. Ports the
        old `show_branching` (napari_utils.py, .branch labels + DirectLabelColormap by
        `branch-{property}`) to the current bridge; the h5ad is read through the canonical
        LabelPropsView, never raw HDF5 (see CLAUDE.md → H5AD access). Categorical/continuous is
        decided by the SAME `_classify_column` cell columns use — `branch-type` (skan's 4 codes)
        and `skeleton-id` (connected-component id) are both small integer level sets and must come
        out categorical here exactly like `hmm.state` does for cells, not hardcoded continuous."""
        key = ("__branch::" + value_name, column)
        if key in self._colcol_cache:
            return self._colcol_cache[key]
        import pandas as pd
        from cecelia.utils.label_props_utils import LabelPropsView
        path = os.path.join(self._task_dir, "labelProps", f"{value_name}__branch.h5ad")
        view = LabelPropsView(path)
        df = view.view_cols([column]).as_df()
        view.close()
        labels = df["label"].to_numpy().astype(int)
        if column not in df.columns:
            res = (labels, np.full(len(labels), np.nan), False)
            self._colcol_cache[key] = res
            return res
        raw = pd.Series(np.asarray(df[column])).reset_index(drop=True)
        vals, is_cat = self._classify_column(column, raw)
        res = (labels, vals, is_cat)
        self._colcol_cache[key] = res
        return res

    def colour_branch_labels(self, value_name: str = "default", column: str = "",
                             percentile: float = 99.5, overrides=None):
        """Recolour `value_name`'s Branches layer by a per-branch obs column via a
        DirectLabelColormap (continuous → viridis, categorical → palette per level). `column=""`
        restores the layer's original colormap. Reads `labelProps/{vn}__branch.h5ad` via
        `_read_branch_column`."""
        if self._task_dir is None:
            raise RuntimeError("call set_task_dir before colour_branch_labels")
        targets = [l for l in self._viewer.layers
                   if getattr(l, "name", "").endswith(") Branches") and f"({value_name})" in l.name]
        if not targets:
            targets = [l for l in self._viewer.layers if getattr(l, "name", "").endswith(") Branches")]
        if not targets:
            return {}
        if not column:
            for l in targets:
                orig = self._labels_orig_cmap.pop(l.name, None)
                if orig is not None:
                    l.colormap = orig
            return {}
        lab, vals, is_cat = self._read_branch_column(value_name, column)
        color_dict, legend = self._labels_color_dict(lab, vals, is_cat, percentile, overrides)
        cmap = napari.utils.DirectLabelColormap(color_dict=color_dict)
        for l in targets:
            self._labels_orig_cmap.setdefault(l.name, l.colormap)
            l.colormap = cmap
        return legend

    # ── Populations (linked brushing with the flow plots) ─────────────────────

    def preview_region(self, verbose: bool = True):
        """What the viewer is currently looking at, as the task-preview region contract.

        Returns `{"xy": {"X": [lo, hi], "Y": [lo, hi]}, "z": int, "t": int, "ndisplay": int}` in
        **level-0 pixels**. The worker turns that into the region it computes
        (`slice_utils.preview_region_bounds`) — this only reports, it doesn't decide.

        TWO sources, and the split is load-bearing:
        * **X/Y from `layer.corner_pixels`** — the visible box, in the layer's own DATA coordinates, so
          there is no world/scale conversion to get wrong.
        * **z/t from `viewer.dims.current_step`** — the slider position.

        It is tempting to take all four from `corner_pixels` (one source, no second coordinate system)
        and this code did. It is **wrong**: napari leaves `corner_pixels` at `[0, 0]` for a dimension it
        is not displaying, so z and t both read as 0 no matter where the sliders are. Live, at
        `current_step = [44, 9, 275, 263]`, every preview segmented `t=0, z=0` — drift padding, all
        channels black, "0 cells" on every parameter. The unit test that "confirmed" the collapse
        assumption built a fresh layer, whose step is also 0, so it compared 0 to 0 and could not fail.

        Two conversions that are easy to get silently wrong, so they are explicit here:
        * corner_pixels is at `data_level`, not level 0 → multiply by that level's downsample factor.
        * corner_pixels bounds are INCLUSIVE; the region contract is half-open → +1 before scaling,
          or the preview quietly loses its last row and column.

        `verbose=False` for the view-change dedup, which calls this per coalesced camera event and
        would otherwise flood the log with regions nothing previewed.
        """
        layer = next((l for l in self._viewer.layers if isinstance(l, napari.layers.Image)), None)
        if layer is None:
            raise RuntimeError("no image layer open — nothing to preview")

        axes = self._display_axes()                     # e.g. ['t','z','y','x'], channel dropped
        factors = (np.asarray(layer.downsample_factors)[layer.data_level]
                   if getattr(layer, "multiscale", False) else np.ones(len(axes)))
        # the level scaling + inclusive→half-open conversion live in napari_utils, where they are
        # unit-tested without needing a viewer (see the docstring on why z/t come from the slider)
        out = napari_utils.preview_region_from_corners(
            layer.corner_pixels, factors, axes,
            ndisplay=self._viewer.dims.ndisplay,
            current_step=self._viewer.dims.current_step)
        if verbose:
            print(f"[preview_region] level={getattr(layer, 'data_level', 0)} {out}", flush=True)
        return out

    def _display_axes(self):
        """Non-channel image axes, in display order (e.g. ['t','z','y','x'])."""
        if not self._axes:
            return []
        return [a.lower() for a in self._axes if a.lower() != "c"]

    def _display_shape(self):
        """Extent of each display axis, aligned with `_display_axes` (channel axis dropped).

        Used to STRETCH a projected label store across the axis it collapsed: a skeleton computed on the
        Z-MIP belongs to the whole volume, so it should render on every plane rather than only the first
        (`napari_utils.expand_to_axes`, `viewer_shape`). Level 0's shape, because that is the level the
        layer's own Y/X are measured against."""
        if not self._axes or not self._im_data:
            return None
        shape = list(self._im_data[0].shape)
        if len(shape) != len(self._axes):
            return None                       # metadata doesn't describe the store — don't guess
        return [n for ax, n in zip(self._axes, shape) if ax.lower() != "c"]

    def _centroid_matrix(self, value_name: str):
        """Return (labels, C, axes): per-cell centroid coordinates as an (n, n_display_dim)
        array in display-axis order, read once from the H5AD and cached. Maps the H5AD's explicit
        `centroid_{x,y,z}` + `centroid_t` columns onto the image's display axes BY NAME."""
        if value_name in self._centroid_cache:
            return self._centroid_cache[value_name]
        from cecelia.utils.label_props_utils import LabelPropsView
        path = os.path.join(self._task_dir, "labelProps", f"{value_name}.h5ad")
        view = LabelPropsView(path)
        centroid_cols = view.centroid_columns()       # explicit centroid_x/_y/_z (present axes)
        temporal_cols = view.temporal_columns()        # ['centroid_t'] or []
        df = view.only_centroid_cols().as_df()
        view.close()

        labels = df["label"].to_numpy().astype(int)
        display_axes = self._display_axes()
        # map each display axis to its centroid column BY NAME (never positionally) — 2D-safe
        axis_to_col = {}
        for ax in display_axes:
            if ax in ("z", "y", "x") and f"centroid_{ax}" in centroid_cols:
                axis_to_col[ax] = f"centroid_{ax}"
            elif ax == "t" and temporal_cols:
                axis_to_col[ax] = temporal_cols[0]
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

    @staticmethod
    def _classify_column(column: str, raw) -> tuple:
        """(values:float[], is_categorical) for a raw obs Series. Non-numeric values factorise to
        integer codes (categorical); a numeric column with few integer-like levels is also
        categorical (e.g. an HMM state, or skan's `branch-type`/`skeleton-id`). NaN stays NaN.
        The ONE classifier — every colour-by read (cell columns, branch columns) must go through
        this, not a bespoke variant, so a column means the same thing everywhere it's coloured."""
        import pandas as pd
        vals = pd.to_numeric(raw, errors="coerce").to_numpy(dtype=float)
        if np.count_nonzero(~np.isnan(vals)) == 0:   # non-numeric → factorise (categorical)
            # factorise only the non-null values so NaN stays NaN (astype(str) would turn NaN into a
            # spurious "nan" category — common now that broadcast track columns leave untracked cells NaN)
            mask = raw.notna().to_numpy()
            codes = np.full(len(raw), -1)
            if mask.any():
                c, _ = pd.factorize(raw[mask].astype(str))
                codes[mask] = c
            vals = np.where(codes < 0, np.nan, codes.astype(float))
            is_cat = True
        else:
            uniq = np.unique(vals[~np.isnan(vals)])
            is_cat = napari_utils.is_categorical_column(column, uniq)   # clusters.* name-rule + ≤20 cap
        return vals, is_cat

    def _read_label_column(self, value_name: str, column: str):
        """Read an obs column aligned to cell labels → (labels:int[], values:float[], is_categorical).
        Cached per (value_name, column). Mirrors the old `show_channel_intensity` value read.

        A column absent from the CELL table but present in the TRACK table (`{value_name}__tracks.h5ad`,
        e.g. `clusters.*` from clustTracks) is read there — keyed by track_id — and broadcast to each
        cell via its `track_id`, so colour-by shades cells + track vertices by the cell's TRACK
        cluster/population (ports R `split_tracks`: colour each track by its cluster so you can see
        which population a track is from). Untracked cells (no/zero track_id) → NaN → grey."""
        key = (value_name, column)
        if key in self._colcol_cache:
            return self._colcol_cache[key]
        import pandas as pd
        from cecelia.utils.label_props_utils import LabelPropsView
        path = os.path.join(self._task_dir, "labelProps", f"{value_name}.h5ad")
        view = LabelPropsView(path)
        df = view.view_cols([column, "track_id"]).as_df()   # label + column (+ track_id) if present
        view.close()
        labels = df["label"].to_numpy().astype(int)
        if column in df.columns:                     # cell-level column → per-cell value directly
            raw = df[column]
        else:                                        # track-level column → broadcast via track_id
            raw = self._read_track_level_column(value_name, column, df)
            if raw is None:                          # absent from both tables → nothing to colour
                res = (labels, np.full(len(labels), np.nan), False)
                self._colcol_cache[key] = res
                return res
        raw = pd.Series(np.asarray(raw)).reset_index(drop=True)
        vals, is_cat = self._classify_column(column, raw)
        res = (labels, vals, is_cat)
        self._colcol_cache[key] = res
        return res

    def _read_track_level_column(self, value_name: str, column: str, cell_df):
        """Read a TRACK-level obs column from `{value_name}__tracks.h5ad` (keyed by track_id) and
        broadcast it to cells via `cell_df["track_id"]`. Returns a per-cell numpy array aligned to
        `cell_df` rows, or None if there's no track table / track_id / matching column."""
        from cecelia.utils.label_props_utils import LabelPropsView
        tpath = os.path.join(self._task_dir, "labelProps", f"{value_name}__tracks.h5ad")
        if "track_id" not in cell_df.columns or not os.path.isfile(tpath):
            return None
        tview = LabelPropsView(tpath)
        tdf = tview.view_cols([column]).as_df()      # label (= track_id) + column
        tview.close()
        if column not in tdf.columns:
            return None
        return napari_utils.broadcast_track_to_cells(
            cell_df["track_id"].to_numpy(),
            tdf["label"].to_numpy().astype(int), tdf[column].to_numpy())

    def _categorical_track_colormap(self, present_values, overrides=None):
        """A step napari Colormap over the present categorical values so a level gets the **same**
        colour the labels use (consistent scheme across Tracks and Labels for one column). Real levels
        (sorted) take a user population's colour where one covers them (`overrides` = {value(str) ->
        '#hex'}), else `_CATEGORICAL_RGBA[i]`; missing (-1) → grey. Returns `(Colormap, legend)` where
        legend is `{value(str) -> '#hex'}`; `(None, {})` if there's nothing to map."""
        overrides = overrides or {}
        pv = sorted({float(v) for v in present_values})
        if not pv:
            return None, {}
        reals = [v for v in pv if v >= 0]
        cmap_of, legend = {}, {}
        for i, v in enumerate(reals):
            key = str(int(v)) if float(v).is_integer() else str(v)     # match Julia _val_key
            ov = napari_utils.hex_to_rgba(overrides.get(key))
            rgba = ov if ov is not None else _CATEGORICAL_RGBA[i % len(_CATEGORICAL_RGBA)]
            cmap_of[v] = rgba
            legend[key] = napari_utils.rgba_to_hex(rgba)
        for v in pv:
            if v < 0:
                cmap_of[v] = (0.6, 0.6, 0.6, 1.0)        # missing → grey (≈ labels' transparent)
        if len(pv) == 1:
            c = cmap_of[pv[0]]
            return napari.utils.Colormap(colors=[c, c], controls=[0.0, 1.0], interpolation="zero"), legend
        lo, hi = pv[0], pv[-1]; span = (hi - lo) or 1.0
        pos = [(v - lo) / span for v in pv]
        colors = [cmap_of[v] for v in pv]                # one colour per value (step / 'zero' interp)
        controls = [0.0] + [(a + b) / 2 for a, b in zip(pos, pos[1:])] + [1.0]
        return napari.utils.Colormap(colors=colors, controls=controls, interpolation="zero"), legend

    @staticmethod
    def track_layer_name(value_name: str, path: str, pop_type: str = "track") -> str:
        # prefixed by pop_type + the SEGMENTATION value_name (e.g. "(track) (C) Tracks /_tracked"),
        # so tracks from several segmentations (A/B/C) AND pop types (track gates vs trackclust
        # cluster pops) are distinguishable and never collide in the layer list.
        return f"({pop_type}) ({value_name}) Tracks {path}"

    def show_tracks(self, pops, value_name: str = "default",
                    tail_width: int = 4, tail_length: int = 30, pop_type: str = "track",
                    color_by: str = "", overrides=None):
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
            return {}

        cby = color_by if color_by and color_by != "track_id" else ""
        legend = {}                                       # {value(str) -> '#hex'} for the colour-by legend
        # fingerprint the colour overrides so a colour-ONLY change (recolour a category, or a pop colour
        # edit) re-renders — otherwise the layer signature is unchanged and the layer is skipped, and the
        # new colour never shows (the categorical colormap is built from `overrides`, but per-layer state
        # like track_ids/visibility didn't change).
        ov_sig = hash(tuple(sorted((overrides or {}).items())))
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
                        # categorical → step colormap (user pop colour where one covers a level, else
                        # Okabe–Ito; matches the labels' colours); fall back to turbo if unbuildable
                        cm, leg = self._categorical_track_colormap(col_vals, overrides)
                        if cm is not None:
                            col_cmaps_dict, col_cmap = {cby: cm}, None
                            legend.update(leg)
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
            # A NAMED track pop (gated `track` / `trackclust`, defined in the pop manager) always renders
            # in ITS OWN colour — like point pops (face_color) — even when a colour-by column is active:
            # colour-by must NOT override a population's defined colour. Colour-by applies ONLY to the
            # whole-segmentation "_tracked" overlay (all tracks, no per-pop colour). So a Leiden track
            # cluster shows the colour you gave it; the plain _tracked layer can be shaded by a measure.
            is_whole = str(pop.get("path", "")).endswith("_tracked")
            use_cby  = cby if (col_vals is not None and is_whole) else ""
            # Not colour-by → solid pop colour (turbo only for an un-coloured _tracked with no colour-by).
            pop_colour = pop.get("colour") or "#9ca3af"
            sig = (vn, hash(tuple(sorted(ids))), tail_width, tail_length, visible, use_cby, ov_sig, pop_colour, is_whole)
            existing = name in self._viewer.layers
            if existing and self._track_sigs.get(name) == sig:
                continue
            mask = np.array([t in ids for t in all_tids]) if ids else np.zeros(len(all_tids), bool)
            sub = tracks[mask, :]
            # A Tracks graph can't be mutated in place, so a recolour / re-push removes + re-adds the
            # layer — which would reset display props the user tuned by hand in napari (tail length,
            # opacity, blending, …). Snapshot them off the existing layer and restore after re-adding so
            # only the intended change (colour) actually changes. `tail_length` overrides our default.
            prev = None
            if existing:
                ex = self._viewer.layers[name]
                prev = {a: getattr(ex, a, None) for a in ("tail_length", "tail_width", "opacity", "blending")}
                _remove_layer(self._viewer, name)
            if len(sub) > 0:
                props = {"track_id": sub[:, 0].astype(int).tolist()}
                # Three colouring modes (napari Tracks colour ONLY via color_by + a colormap):
                #   1. colour-by column (whole _tracked only) → categorical Okabe–Ito colormaps_dict /
                #      continuous viridis, keyed by the column;
                #   2. plain _tracked, no colour-by → napari's per-track turbo (distinguishes tracks);
                #   3. a NAMED pop (gated track / trackclust) → its FLAT pop colour.
                # For (3) we must colour by a CONSTANT helper property, NOT track_id: napari keeps its
                # built-in turbo for `track_id` and ignores a custom colormap attached to it — that's why
                # the pop colour never showed. A constant property mapped through a solid two-stop
                # colormap gives one flat colour = the pop's.
                if use_cby:
                    props[use_cby] = col_vals[mask].tolist()
                    layer_color_by, layer_cmap, layer_cmaps_dict = use_cby, (col_cmap or "turbo"), col_cmaps_dict
                elif is_whole:
                    layer_color_by, layer_cmap, layer_cmaps_dict = "track_id", "turbo", None
                else:
                    # NAMED pop → flat pop colour, the old-R way (show_tracks split_tracks): colour a
                    # NONZERO-constant helper property through a black→colour colormap. Colouring by
                    # track_id keeps napari's turbo; a helper property + this colormap makes every track
                    # render in the pop colour (value 1 → the colour end; 0=black is never hit).
                    try:
                        props["cc_pop"] = [1.0] * sub.shape[0]
                        layer_color_by, layer_cmap = "cc_pop", None
                        layer_cmaps_dict = {"cc_pop": napari_utils.solid_track_colormap(pop_colour)}
                    except Exception:
                        layer_color_by, layer_cmap, layer_cmaps_dict = "track_id", "turbo", None
                # Delegate to the shared helper (passes scale AND units so napari keeps unit-aware
                # rendering across layers). See docs/todo/CECELIA_NAPARI_UPSTREAM_PLAN.md.
                layer = napari_utils.add_tracks(
                    self._viewer, sub, name=name,
                    scale=self._im_scale, units=self._im_units, properties=props,
                    color_by=layer_color_by, tail_width=tail_width, tail_length=tail_length,
                    colormap=layer_cmap, colormaps_dict=layer_cmaps_dict,
                )
                if prev is not None:                     # carry the user's manual layer tweaks over
                    for attr, val in prev.items():
                        if val is not None:
                            try: setattr(layer, attr, val)
                            except Exception: pass
            self._track_sigs[name] = sig
        return legend

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

    # Layer props are stored as JSON — the single canonical format, read by the Julia in-app crop render
    # too (docs/todo/CROP_PANEL_PLAN.md Phase 0). Every field is JSON-native, so pickle bought nothing.
    @staticmethod
    def _jsonable(k, v):
        if k == "contrast_limits":
            return [float(x) for x in v]          # numpy scalars → plain floats
        if k in ("opacity", "gamma"):
            return float(v)
        if k == "visible":
            return bool(v)
        return v                                   # blending (str); colormap handled by the caller

    # Max stops kept for `colormap_lut`. napari's additive channel primaries (red/green/…/bop blue) are
    # 2-entry ramps and stay exact at any cap; the 256-entry perceptual maps (viridis/turbo/…) resample
    # to 64 with a worst-case error of 2/255 — invisible in a preview thumbnail. (The one outlier is
    # `gist_earth`, a stepped terrain map, at 24/255; it is not a channel colormap.)
    _LUT_MAX_STOPS = 64

    @classmethod
    def _colormap_lut(cls, colormap):
        """napari colormap → black→colour stops the Julia preview renderer can interpolate.

        The renderer cannot resolve a colormap by NAME without duplicating napari's palette here, and
        that duplication silently broke: `bop blue` was missing from its table and rendered as WHITE.
        napari owns its colormaps, so it exports the actual colours and the renderer just interpolates.
        Covers the perceptual maps and the white→colour `I *` set too, which no name table could
        approximate. See `api/src/image_render.jl`.
        """
        cols = np.asarray(colormap.colors, dtype=float)[:, :3]
        n = len(cols)
        if n > cls._LUT_MAX_STOPS:                 # resample uniformly; 2-stop ramps are never touched
            src = np.linspace(0.0, 1.0, n)
            tgt = np.linspace(0.0, 1.0, cls._LUT_MAX_STOPS)
            cols = np.stack([np.interp(tgt, src, cols[:, k]) for k in range(3)], axis=1)
        return [[round(float(v), 4) for v in stop] for stop in cols]

    def save_layer_props(self, filepath: str):
        props = {"Image": []}
        _keys = [
            "opacity", "blending", "visible", "gamma",
            "contrast_limits", "colormap",
        ]
        for layer in self._viewer.layers:
            if type(layer).__name__ == "Image":
                entry = {
                    k: (layer.colormap.name if k == "colormap" else self._jsonable(k, getattr(layer, k)))
                    for k in _keys
                }
                # The name is kept for the viewer's own restore (`colormap` is settable by name); the LUT
                # is what the Julia renderer reads. An exotic colormap must not fail the whole save.
                try:
                    entry["colormap_lut"] = self._colormap_lut(layer.colormap)
                except Exception as e:
                    print(f"[props] could not export LUT for {layer.colormap.name!r}: {e}", flush=True)
                props["Image"].append(entry)
        # viewer dims position (the T/Z slider) so the image reopens on the same frame/slice
        try:
            props["dims"] = {"current_step": [int(x) for x in self._viewer.dims.current_step]}
        except Exception:
            pass
        # atomic write (tmp + os.replace) so a crash/kill never leaves a half-written props file —
        # the image always reopens in a valid remembered state.
        tmp = filepath + ".tmp"
        with open(tmp, "w", encoding="utf-8") as f:
            json.dump(props, f)
            f.flush()
            os.fsync(f.fileno())
        os.replace(tmp, filepath)

    def load_layer_props(self, filepath: str):
        if os.path.exists(filepath):
            with open(filepath, encoding="utf-8") as f:
                data = json.load(f)
        else:
            # One-time migration of a pre-JSON pickle (same dict shape) → rewrite as JSON, then use it.
            # `pickle` is imported lazily here ONLY for this legacy path; nothing is ever written as pickle
            # again, so it can be removed once no `.pkl` props remain in the wild.
            legacy = filepath[:-5] + ".pkl" if filepath.endswith(".json") else None
            if not (legacy and os.path.exists(legacy)):
                return
            import pickle
            with open(legacy, "rb") as f:
                data = pickle.load(f)
            try:
                tmp = filepath + ".tmp"
                with open(tmp, "w", encoding="utf-8") as f:
                    json.dump(data, f)
                os.replace(tmp, filepath)
            except Exception:
                pass
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

    # ── View snapshot (the "view state" atom) ───────────────────────────────────

    def capture_view_state(self):
        """A durable, JSON-safe snapshot of the current view (camera + dims + per-layer display props),
        for zoom-to-source / animation. Delegates to the shared helper so the schema lives in one place
        (cecelia.utils.napari_utils; coastal can reuse it). See docs/todo/ANIMATION_PLAN.md."""
        return napari_utils.capture_view_state(self._viewer)

    def apply_view_state(self, snapshot):
        """Re-apply a snapshot from `capture_view_state` to the current viewer (missing layers /
        unsettable attrs are skipped). Delegates to the shared helper."""
        return napari_utils.apply_view_state(self._viewer, snapshot)

    # ── Screenshot ────────────────────────────────────────────────────────────

    def save_screenshot(self, path: str, canvas_only: bool = True, scale=1, fit_data: bool = True,
                        clean: bool = False):
        """Capture the canvas to `path`.

        `fit_data=True` (default) uses napari's `export_figure`: it tightly re-fits the view to the
        DATA extent, so the figure has NO black margins and comes out at `scale`× the native data
        resolution — i.e. the capture looks like the viewer (image filling the frame), not a tiny image
        floating in a big black canvas. (napari's plain `screenshot(scale=…)` only enlarges the canvas
        at a fixed zoom, which ADDS margins — the opposite of what we want here.) `export_figure`
        restores the previous camera afterwards, so the view snapshot captured alongside is unaffected.

        `fit_data=False` captures the current canvas as-shown (a plain canvas screenshot). Falls back to
        that if there are no layers to fit.

        `clean=True` (Phase E1) hides napari's baked scale bar + timestamp overlay for the shot and
        restores them after — a clean still for publication (add a vector scale bar / timestamp in
        Illustrator, or Cecelia's own; see ANIMATION_PLAN.md Decision 7). The hide/restore itself is
        `napari_utils.overlays_hidden`, shared with the movie recorders (which expose the two
        independently)."""
        with napari_utils.overlays_hidden(self._viewer, scale_bar=clean, timestamp=clean):
            if fit_data and len(self._viewer.layers) > 0:
                self._viewer.window.export_figure(path=path, scale=float(scale or 1), flash=False)
            else:
                self._viewer.window.screenshot(path, canvas_only=canvas_only, flash=False)

    def record_timelapse(self, path: str, fps: int = 15, canvas_only: bool = True,
                         size_x=None, size_y=None, t_start: int = 0, t_end=None, title_card=None,
                         task_id=None, api_url=None, frame_offset=0, frame_total=0,
                         show_timestamp: bool = True, show_scale_bar: bool = True):
        """Record the open image's T-sweep to `path` (mp4). Resolves the T slider index from the image
        axes and delegates to the shared `napari_utils.record_timelapse`. Returns the frame count, the
        path and the size actually written. Raises if the image has no time axis. Phase F1 of the
        batch-movie work (docs/todo/ANIMATION_PLAN.md); F1.2/F1.3 add per-image config + batch; H adds
        `title_card`.

        `size_x`/`size_y` are the requested output WIDTH/HEIGHT in pixels (blank = the canvas size).
        THIS is the one place the axis order flips: the UI and the routes speak X/Y, napari speaks
        (height, width) — see `_movie_size`.

        `frame_offset`/`frame_total` are for a multi-pass job (one pass per image version of a
        side-by-side comparison) — see `_record_hooks`.

        `show_timestamp`/`show_scale_bar` (both default True — what every movie was) hide napari's
        baked overlays for the duration of the render and restore them after, so the window is
        unchanged once it finishes. The two are separate because a figure often wants the elapsed time
        burnt in and the scale bar added as vector art later."""
        axes = self._display_axes()                       # non-channel axes, dims.current_step order
        if "t" not in axes:
            raise RuntimeError("this image has no time axis to record")
        n_t = self._time_axis_len()
        if not n_t or n_t <= 1:
            raise RuntimeError("this image has a single timepoint — nothing to sweep")
        size = _movie_size(size_x, size_y)
        on_progress, should_cancel = _record_hooks(task_id, api_url, frame_offset, frame_total)
        try:
            with napari_utils.overlays_hidden(self._viewer, scale_bar=not show_scale_bar,
                                              timestamp=not show_timestamp):
                frames = napari_utils.record_timelapse(
                    self._viewer, path, t_axis_index=axes.index("t"), n_timepoints=n_t,
                    fps=fps, canvas_only=canvas_only, size=size, t_start=t_start, t_end=t_end,
                    title_card=title_card, on_progress=on_progress, should_cancel=should_cancel)
        except napari_utils.RecordCancelled as e:
            # A reply, not an error: the caller asked for this, and the previous movie (if any) is
            # still on disk untouched — the staged file was removed.
            return {"cancelled": True, "frames": e.frames, "path": path, "n_timepoints": n_t}
        finally:
            _clear_record_cancel(task_id)
        return {"frames": frames, "path": path, "n_timepoints": n_t,
                **self._recorded_size(path)}

    def record_keyframes(self, path: str, keyframes, fps: int = 15, canvas_only: bool = True,
                         size_x=None, size_y=None, title_card=None, task_id=None, api_url=None,
                         show_timestamp: bool = True, show_scale_bar: bool = True):
        """Render an interpolated keyframe animation to `path` (mp4): each keyframe's saved view state is
        applied + captured with `steps` tween frames from the previous one (camera/contrast/colour/T
        interpolation). The "connect animation steps" render — see docs/todo/ANIMATION_PLAN.md (F2);
        H4 adds `title_card`. Delegates to the shared `napari_utils.record_keyframes`. Needs ≥2 keyframes.
        `size_x`/`size_y` as for `record_timelapse` (blank = the canvas size)."""
        on_progress, should_cancel = _record_hooks(task_id, api_url)
        try:
            with napari_utils.overlays_hidden(self._viewer, scale_bar=not show_scale_bar,
                                              timestamp=not show_timestamp):
                frames = napari_utils.record_keyframes(self._viewer, path, keyframes, fps=fps,
                                                       canvas_only=canvas_only,
                                                       size=_movie_size(size_x, size_y),
                                                       title_card=title_card, on_progress=on_progress,
                                                       should_cancel=should_cancel)
        except napari_utils.RecordCancelled as e:
            return {"cancelled": True, "frames": e.frames, "path": path, "keyframes": len(keyframes)}
        finally:
            _clear_record_cancel(task_id)
        return {"frames": frames, "path": path, "keyframes": len(keyframes),
                **self._recorded_size(path)}

    def stitch_movies(self, path: str, sources, labels=None, layout: str = "row", fps: int = 15,
                      title_card=None, task_id=None, api_url=None, frame_offset=0, frame_total=0):
        """Compose already-recorded movies into one side-by-side file at `path` (the tail of a version
        comparison — docs/todo/MOVIE_COMPARE_PLAN.md D1). `sources` are the per-version recordings, in
        column order; `labels` captions them.

        It needs no viewer, and `movie_io.stitch_movies` does the work — but it lives here, as a bridge
        command, for two reasons: this process already owns writing into `movies/` (staging, cancel
        registry, progress channel), and the title card is prepended HERE, from the live viewer, so the
        card's channel legend is read the same way a single recording reads it (D6). Running it as a
        second Python process would duplicate all of that.

        Returns the frame count + the size written, or `{"cancelled": True, ...}` — the same reply
        shape as the recorders, so the Julia side treats a cancelled stitch exactly like a cancelled
        record."""
        on_progress, should_cancel = _record_hooks(task_id, api_url, frame_offset, frame_total)
        try:
            frames = movie_io.stitch_movies(sources, path, fps=fps, labels=labels, layout=layout,
                                            on_progress=on_progress, should_cancel=should_cancel)
        except napari_utils.RecordCancelled as e:
            # Nothing was promoted onto `path`, so any previous movie there is still intact — same
            # contract as a cancelled record.
            return {"cancelled": True, "frames": e.frames, "path": path}
        finally:
            _clear_record_cancel(task_id)
        # The card goes on the COMPOSED file, once, through the shared prepend the recorders use — the
        # per-version passes are recorded without one.
        napari_utils._maybe_prepend_title(self._viewer, path, title_card)
        return {"frames": frames, "path": path, "columns": len(sources),
                **self._recorded_size(path)}

    def _recorded_size(self, path):
        """`{"sizeX": w, "sizeY": h}` read back off the finished movie, or `{}`.

        Reported rather than echoed: a clamp, an odd-axis fix or a HiDPI rounding all move the real size
        away from what was asked for, and the UI should show what LANDED. Best-effort — never fails a
        recording that already succeeded."""
        try:
            import imageio.v2 as imageio
            with imageio.get_reader(str(path)) as r:
                h, w = r.get_data(0).shape[:2]
            return {"sizeX": int(w), "sizeY": int(h)}
        except Exception as e:
            print(f"[WARN] could not read back the movie size: {e}", flush=True)
            return {}

    # ── Task dir (needed for labels / props) ──────────────────────────────────

    def set_task_dir(self, path: str):
        self._task_dir = path


# OME-ZARR store opening + NGFF/OME-XML geometry reading now live in the shared cecelia readers
# (cecelia.utils.zarr_utils: open_as_zarr/open_zarr, series_base, read_axes, read_scale;
# cecelia.utils.ome_xml_utils: read_pixel_unit, read_time_increment). The bridge used to carry
# its own copies — they were consolidated so images are opened ONE way. See docs/NAPARI.md.


# Every layer-name suffix each label-store family can occupy, keyed by its on-disk subdirectory. A
# given store gets AT MOST ONE of these at a time, so `_show_label_stores` evicts all of a family's
# suffixes before adding one — that's what makes a finished segmentation replace its own live preview
# (and a preview of a re-run replace the finished layer, whose store the re-run has just deleted).
_PREVIEW_SUFFIX = "Preview"

_LABEL_SUFFIXES = {
    # `Preview` has no store behind it (`show_task_preview` builds it in memory) but shares the family
    # so the three mutually evict: a finished run's labels replace a preview of the same value_name,
    # and a new preview replaces them.
    "labels":       ("Labels", "Labels (live)", _PREVIEW_SUFFIX),
    "branchLabels": ("Branches",),
}


def _label_layer_stem(label_filename: str) -> str:
    """Layer-name stem for a label store filename — the value_name the store belongs to.

    The layer name must NOT be the bare filename, because a store still being written by a running
    task lives at a STAGING path (`X.zarr.partial`, see `zarr_utils.staged_store`). Naming the layer
    after that file would break two things at once: `colour_labels` targets a layer by its `({vn})`
    prefix, and adding the finished `({vn}) Labels` layer evicts the preview only when both share a
    stem. Multi-type runs keep their own layers (`X_nuc.zarr` → `X_nuc`) — the stem is per FILE, just
    not per on-disk name.
    """
    name = label_filename
    for suffix in (zarr_utils.STAGING_SUFFIX, zarr_utils.SUPERSEDED_SUFFIX):
        if name.endswith(suffix):
            name = name[:-len(suffix)]
            break
    return name[:-5] if name.endswith(".zarr") else name


def _movie_size(size_x, size_y):
    """Requested movie X/Y → napari's `(height, width)`, or None for "the canvas size"."""
    return movie_io.size_from_xy(size_x, size_y)


# ── Recording: cancel + progress ──────────────────────────────────────────────
#
# A record is ONE command, so it occupies the Qt-thread command loop for its whole run (minutes at 4K).
# Batch movies get a progress bar and a Cancel because Julia loops over IMAGES and each image is its own
# bridge call — the events happen BETWEEN calls. A single record has no "between", so both have to come
# from inside the frame loop.
#
# Cancel comes in OUT OF BAND: `handle()` answers `record_cancel` on the asyncio WS thread and never
# queues it, so it lands while the Qt thread is still rendering (measured: flag set at +1.5 s of a
# blocked 6 s command). The loop polls the flag per frame. Queueing it instead would deliver the cancel
# *after* the recording it was meant to stop.
#
# Cancels are keyed by TASK ID, not a single global flag: a stale "cancel" must not kill the next
# recording the user starts.
_record_cancel_lock = threading.Lock()
_record_cancelled: set = set()


def request_record_cancel(task_id):
    """Flag a recording task as cancelled. Called from the WS thread, read from the Qt thread."""
    if not task_id:
        return
    with _record_cancel_lock:
        _record_cancelled.add(str(task_id))


def _record_cancel_requested(task_id):
    with _record_cancel_lock:
        return bool(task_id) and str(task_id) in _record_cancelled


def _clear_record_cancel(task_id):
    with _record_cancel_lock:
        _record_cancelled.discard(str(task_id))


# Minimum gap between progress posts. A frame can render in milliseconds at canvas size, and a POST per
# frame would cost more than the render; a progress bar needs a couple of updates a second, not 60.
_PROGRESS_MIN_INTERVAL_S = 0.4


def _record_hooks(task_id, api_url, frame_offset=0, frame_total=0):
    """`(on_progress, should_cancel)` for a recording, or `(None, None)` when it isn't task-driven.

    Progress goes back over the SAME bridge→backend channel the view listener uses
    (`POST /api/napari/event`), which Julia relays onto the task rail as `task:progress`.

    `frame_offset`/`frame_total` place THIS call's frames inside a longer job, so a side-by-side
    comparison — several recordings plus a stitch, one per version — advances a single bar instead of
    restarting it per pass (which reads as a stuck or broken render). Both default to 0, i.e. "this
    call is the whole job", which is every existing caller. See docs/todo/MOVIE_COMPARE_PLAN.md."""
    if not task_id:
        return None, None

    state = {"last": 0.0}
    offset = max(0, int(frame_offset or 0))

    def on_progress(frame, total):
        done = offset + int(frame)
        # A job total is an ESTIMATE made before the passes ran (frames per version × versions, plus
        # the stitch), so clamp rather than ever posting frame > total, which renders as a bar past
        # its own end.
        overall = max(done, int(frame_total or 0) or (offset + int(total)))
        now = time.monotonic()
        if done < overall and (now - state["last"]) < _PROGRESS_MIN_INTERVAL_S:
            return                                 # …but never skip the final frame
        state["last"] = now
        if not api_url:
            return
        body = json.dumps({"type": "recordProgress", "taskId": str(task_id),
                           "frame": done, "total": overall}).encode()
        try:
            req = urllib.request.Request(
                api_url.rstrip("/") + "/api/napari/event", data=body, method="POST",
                headers={"Content-Type": "application/json"})
            urllib.request.urlopen(req, timeout=2).read()
        except Exception as e:
            # Progress is advisory: a backend that went away must not take the recording down with it.
            print(f"[record] progress POST failed: {e}", flush=True)

    return on_progress, lambda: _record_cancel_requested(task_id)


def _remove_layer(viewer: napari.Viewer, name: str):
    if name in viewer.layers:
        viewer.layers.remove(name)


# ── WebSocket command dispatcher ──────────────────────────────────────────────

def execute_command(state: NapariState, cmd: dict) -> dict:
    t = cmd.get("type")
    try:
        if t == "ping":
            # the canvas size rides along on the existing health poll rather than earning its own
            # command: it is what a movie comes out at when no size is asked for, so the movie controls
            # show it as their placeholder. None when there is no window yet.
            hw = napari_utils.canvas_size(state._viewer) if state._viewer is not None else None
            return {"type": "pong", "started_at": _STARTED_AT, "protocol": PROTOCOL,
                    "canvas_size_y": hw[0] if hw else None,
                    "canvas_size_x": hw[1] if hw else None}

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

        elif t == "set_z_view":
            return state.set_z_view(show_3d=cmd.get("show_3d", False), z=cmd.get("z", None))

        elif t == "show_labels":
            state.show_labels(
                value_name=cmd.get("value_name", "default"),
                label_files=cmd.get("label_files", None),
                show_labels=cmd.get("show_labels", True),
                show_points=cmd.get("show_points", False),
                cache=bool(cmd.get("cache", False)),
                preview=bool(cmd.get("preview", False)),
                # `contour` (mask outline width) was on the wire from the day the control shipped —
                # `show_labels!` sends it, the Julia handler reads it, `show_labels` accepts it — and
                # THIS LINE dropped it, so every add ran with the default 0 and masks came back FILLED.
                # The outline only ever reached napari through `apply_view_state` (the live slider), so
                # it looked right until anything rebuilt the layer: a mask toggle, the post-open overlay
                # restore, or a movie's per-cell re-open. That is why recordings came out filled.
                # Same failure as the `show_task_preview` mismatch below; the test named for it now
                # covers this command too, in BOTH directions.
                contour=int(cmd.get("contour", 0) or 0),
            )

        elif t == "refresh_labels":
            state.refresh_labels(
                value_name=cmd.get("value_name", "default"),
                label_files=cmd.get("label_files", None),
            )

        elif t == "show_task_preview":
            # `layers`, not the old `mask`/`label_shape`/`label_axes` triple: a reply carries a LIST of
            # layers, each with its own kind (see `show_task_preview`). This line was missed when the
            # method was rewritten, and because it is the ONLY caller in production, every preview died
            # here with a TypeError before rendering anything — reported as a bare "Preview failed".
            # The tests called the method directly and so never crossed this boundary; one now goes
            # through `_dispatch` for exactly that reason.
            state.show_task_preview(
                value_name=cmd.get("value_name", "default"),
                layers=cmd.get("layers"),
                region=cmd.get("region"),
                show=bool(cmd.get("show", True)),
                api_url=cmd.get("api_url"),
            )

        elif t == "colour_labels":
            legend = state.colour_labels(
                value_name=cmd.get("value_name", "default"),
                column=cmd.get("column", ""),
                percentile=cmd.get("percentile", 99.5),
                overrides=cmd.get("colour_overrides"),
            )
            return {"type": "ok", "cmd": t, "legend": legend or {}}

        elif t == "show_branch_labels":
            state.show_branch_labels(
                value_name=cmd.get("value_name", "default"),
                label_files=cmd.get("label_files", None),
                show_labels=cmd.get("show_labels", True),
                cache=bool(cmd.get("cache", False)),
            )

        elif t == "colour_branch_labels":
            legend = state.colour_branch_labels(
                value_name=cmd.get("value_name", "default"),
                column=cmd.get("column", ""),
                percentile=cmd.get("percentile", 99.5),
                overrides=cmd.get("colour_overrides"),
            )
            return {"type": "ok", "cmd": t, "legend": legend or {}}

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
            legend = state.show_tracks(
                pops=cmd.get("pops", []),
                value_name=cmd.get("value_name", "default"),
                tail_width=cmd.get("tail_width", 4),
                tail_length=cmd.get("tail_length", 30),
                pop_type=cmd.get("pop_type", "track"),
                color_by=cmd.get("color_by", ""),
                overrides=cmd.get("colour_overrides"),
            )
            return {"type": "ok", "cmd": t, "legend": legend or {}}

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

        elif t == "record_timelapse":
            res = state.record_timelapse(cmd["path"], fps=cmd.get("fps", 15),
                                         canvas_only=cmd.get("canvas_only", True),
                                         size_x=cmd.get("size_x"), size_y=cmd.get("size_y"),
                                         t_start=cmd.get("t_start", 0), t_end=cmd.get("t_end"),
                                         title_card=cmd.get("title_card"),
                                         task_id=cmd.get("task_id"), api_url=cmd.get("api_url"),
                                         frame_offset=cmd.get("frame_offset", 0),
                                         frame_total=cmd.get("frame_total", 0),
                                         show_timestamp=cmd.get("show_timestamp", True),
                                         show_scale_bar=cmd.get("show_scale_bar", True))
            return {"type": "ok", "cmd": t, **res}

        elif t == "record_keyframes":
            res = state.record_keyframes(cmd["path"], cmd.get("keyframes", []),
                                         fps=cmd.get("fps", 15), canvas_only=cmd.get("canvas_only", True),
                                         size_x=cmd.get("size_x"), size_y=cmd.get("size_y"),
                                         title_card=cmd.get("title_card"),
                                         task_id=cmd.get("task_id"), api_url=cmd.get("api_url"),
                                         show_timestamp=cmd.get("show_timestamp", True),
                                         show_scale_bar=cmd.get("show_scale_bar", True))
            return {"type": "ok", "cmd": t, **res}

        elif t == "stitch_movies":
            res = state.stitch_movies(cmd["path"], cmd.get("sources", []),
                                      labels=cmd.get("labels"), layout=cmd.get("layout", "row"),
                                      fps=cmd.get("fps", 15), title_card=cmd.get("title_card"),
                                      task_id=cmd.get("task_id"), api_url=cmd.get("api_url"),
                                      frame_offset=cmd.get("frame_offset", 0),
                                      frame_total=cmd.get("frame_total", 0))
            return {"type": "ok", "cmd": t, **res}

        elif t == "save_screenshot":
            state.save_screenshot(cmd["path"], canvas_only=cmd.get("canvas_only", True),
                                  scale=cmd.get("scale", 1), fit_data=cmd.get("fit_data", True),
                                  clean=cmd.get("clean", False))
            # fold the view snapshot into the reply so a screenshot and its provenance are captured
            # atomically (same view) — one round trip, no camera-moved-between-calls skew. `extent_um` =
            # the captured frame's physical size (for a vector scale bar on the still, Phase E2).
            return {"type": "ok", "cmd": t, "view_state": state.capture_view_state(),
                    "extent_um": state._data_extent_um()}

        elif t == "capture_view_state":
            return {"type": "ok", "cmd": t, "view_state": state.capture_view_state()}

        elif t == "preview_region":
            return {"type": "ok", "cmd": t, "region": state.preview_region()}

        elif t == "apply_view_state":
            return {"type": "ok", "cmd": t,
                    "applied": state.apply_view_state(cmd.get("view_state") or {})}

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
        if cmd.get("type") == "record_cancel":
            # NOT queued. The command loop is busy rendering the very recording this cancels, so going
            # through the queue would deliver it after the render finished — see `request_record_cancel`.
            request_record_cancel(cmd.get("task_id"))
            await websocket.send(json.dumps({"type": "ok", "cmd": "record_cancel"}))
            continue
        resp_q: queue.Queue = queue.Queue()
        command_queue.put((cmd, resp_q))
        # hand off the blocking wait to a worker thread so asyncio stays free
        result = await asyncio.get_event_loop().run_in_executor(
            None, resp_q.get
        )
        await websocket.send(json.dumps(result))


async def ws_server():
    import websockets
    async with websockets.serve(handle, HOST, PORT, max_size=WS_MAX_SIZE):
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
