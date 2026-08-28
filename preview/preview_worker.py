"""Resident preview worker — runs a task's real compute over one visible region, on demand.

A runtime process, like `napari/napari_bridge.py` and `mcp/` (not part of the `cecelia` IO library).
It exists for one measured reason: the fixed cost of a Python process that can segment is **17.7 s**
(11.7 s `import cecelia.utils` + 5.7 s `import cellpose` + 0.2 s model construction; cellpose 4's
cpsam is a 1.2 GB ViT-L, so construction is ~2 s rather than 0.2 s), which is fatal
per preview and irrelevant once. Staying resident pays it at toggle-on. Model construction is cheap
enough that warm *models* are a minor bonus, not the point — see
docs/todo/TASK_PREVIEW_PLAN.md (Decision 8).

What it does NOT do:

* **No second cellpose implementation.** It calls `CellposeUtils.predict_slice`, the same method the
  full run uses, so a preview cannot drift from the thing it is previewing.
* **No 3D.** One z-plane, always. A visible z-stack is many times the cost of a plane and that is
  not a preview. In 3D display mode it previews the current plane and reports `fallback2d` so the
  caller can say so. (Under cellpose 3 there was also no shortcut, because cyto* rescaled to a
  canonical diameter and cost tracked CELLS rather than pixels. **Cellpose 4 is the opposite** —
  cpsam runs fixed 256 px tiles, so cost tracks PIXELS and is linear in region area: measured
  0.28 s / 1.03 s / 4.14 s for 256² / 512² / 1024² on an RTX 2000 Ada, ~1.5 GiB peak VRAM. A
  downsampled preview would now be cheaper, which makes it a real option rather than a dead end —
  but it would no longer be "the same compute as the run", so it is not taken here.)
* **Scratch labels store on disk, no image blocks over the wire.** The mask is written to
  `{task_dir}/labels/{value_name}__preview.ome.zarr` and the reply carries `path`/`valueName`
  rather than an inline block. The browser then fetches it through the same
  `/api/viewer/slab?labels=<vn>&preview=1` route as a finished mask — one reader, one geometry,
  one texture path. This overrides an earlier napari-era design that RETURNED the block inline
  (`cecelia.utils.block_transfer`) to avoid staging in the project tree; that reasoning stopped
  applying when the browser viewer became the sole preview surface, and the block-over-WS path
  needed its own decode + texture upload rather than riding the labels slab.

  Debris is bounded: the previous preview's store is wiped at the start of each request, a
  `cleanup` message clears every `*__preview.ome.zarr` under a task_dir, and an `atexit` handler
  sweeps the last-known task dirs when the worker itself exits.

  AF preview follows the same pattern with per-channel image stores at
  `{task_dir}/{value_name}__preview_af_ch{N}.ome.zarr` and the reply carries `previewImages`; the
  browser swaps each corrected channel's slab URL onto the scratch store
  (`/api/viewer/slab?preview_af=1&sourceChannel=N`). Flow planes stay off disk — PNG bytes,
  viewer-agnostic — because they are canvas plots, not layers.

Protocol: one JSON message per connection, same shape as the napari bridge.
    {"type": "ping"}     -> {"type": "ok", "protocol": PROTOCOL}
    {"type": "preview", ...} -> {"type": "ok",
                                 "layers"       : [{kind, name, valueName, path, shape, axes}, …]?,
                                 "previewImages": [{sourceChannel, name, valueName, path,
                                                     shape, axes}, …]?,
                                 "region": …, "fallback2d": bool, plus per-task fields}
    {"type": "cleanup", "taskDir": "..."} -> {"type": "ok"}   removes every scratch preview store
                                                             (labels AND AF images) keyed on
                                                             {task_dir}

`PROTOCOL` exists because a running worker is ADOPTED, not relaunched, when the backend restarts — that
is deliberate (a warm worker survives a Revise restart, which is most of its value) but it means stale
worker code otherwise outlives every restart. It presented as a bare "Preview failed": a worker from
before the AF backend existed ignored `funName`, fell through to the segmentation path, and raised
"no models in preview params". Bump this whenever the reply shape or the backend set changes.
"""
import asyncio
import atexit
import base64
import glob
import itertools
import json
import os
import shutil
import traceback

import numpy as np
import zarr

import cecelia.utils.correction_utils as correction_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.slice_utils as slice_utils
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.dim_utils import DimUtils

#: Task dirs we have staged preview stores into this session, for `atexit` cleanup and for the
#: `cleanup` message. Best-effort: adds are cheap and a torn-down worker cannot rely on `finally`.
_PREVIEW_TASK_DIRS: set = set()

#: Suffix used for every preview labels store. Keyed off the value_name so a preview for `smoothed`
#: doesn't tread on a preview for `default`, and so `_sweep_preview_labels` can wipe every preview
#: in a task dir with one glob whatever the value_name was.
_PREVIEW_LABEL_SUFFIX = '__preview.ome.zarr'

#: Suffix stem for AF preview image stores. Per-channel — every corrected channel writes its own store,
#: keyed off the value_name AND the source channel index (`{vn}__preview_af_ch{N}.ome.zarr`) so the
#: browser can swap ONE channel's slab URL onto the scratch store while the others keep reading the
#: source. Same glob idiom as the labels sweep — `_sweep_preview_af` wipes them all.
_PREVIEW_AF_STEM = '__preview_af_ch'
_PREVIEW_AF_SUFFIX = '.ome.zarr'

# `cellpose_utils` is imported LAZILY, by the cellpose backend only — see `_cellpose_imports`. It pulls
# in cellpose and torch (3.1 s of the worker's import cost, measured warm), and AF correction needs
# neither: `af_correct_frame` is numpy. The worker was built for segmentation and grew a second backend,
# so the import stayed at module level and every backend paid for every other backend's dependencies.

# Timepoints read for the whole-image normalisation statistic. Measured on `EaMaVq` (201 frames,
# single-level): exact = 30.6 s, 20 frames = 3.3 s, and the resulting mask counts were IDENTICAL at
# every budget down to 2 frames (the window itself moves ~3% at 20, ~35% at 2). 20 keeps the statistic
# stable and the wait short. Runs stay EXACT — see `_compute_norm_params(max_frames=…)`.
NORM_FRAMES = 20

#: `(z, xy)` stride for the correction's global values. A full pass over a 181-frame movie costs tens of
#: seconds; at (2, 4) the same derivation gives byte-identical background levels on real data. Safe
#: because every value is an interior histogram threshold and the channel has a background population to
#: find it in — the one assumption, spelled out on `correction_utils.af_weight_stats`. Unlike the true
#: max of the percentile window this all replaced, which is biased low by construction and so could
#: never be made cheap. Runs stay exact: they pass no stride.
AF_PREVIEW_STRIDE = (2, 4)


def _preview_timepoints(dim_utils, max_frames=NORM_FRAMES):
    """At most `max_frames` timepoints, evenly strided — or None for "read them all".

    The AF analogue of `SegmentationUtils._subsample_time`: same ceil-stride policy and the same
    `NORM_FRAMES` budget, expressed as the index list `af_weight_stats` takes rather than a sliced
    array. Two spellings because the two APIs differ, one policy.

    The AF path had NO time budget while the cellpose path had had one all along, which is most of why
    a first AF preview felt broken and a first cellpose preview did not. Measured on
    `zolIMa/2h06xA` (181 x 4 x 31 x 1024 x 1024), on top of `AF_PREVIEW_STRIDE`:

        all 181 frames  26.6 s      backgrounds {1: 24, 2: 20, 3: 31}
        20 frames        2.9 s      IDENTICAL
        10 frames        1.4 s      IDENTICAL
        5 frames         0.7 s      IDENTICAL

    Byte-identical for the same reason the spatial stride is: every value is an interior histogram
    threshold, and subsampling does not move one as long as the channel still has a background
    population to find it in. Runs stay EXACT — they pass no budget, as they pass no stride.
    """
    if not dim_utils.is_timeseries():
        return None
    total = int(dim_utils.dim_val('T'))
    if not max_frames or max_frames < 1 or total <= max_frames:
        return None
    stride = -(-total // max_frames)                  # ceil → at most max_frames frames
    return list(range(0, total, stride))


_CELLPOSE = None


def _cellpose_imports():
    """The segmentation stack, imported on first use and remembered.

    Deferred because it costs 3.1 s of cellpose + torch (measured warm) that an AF-only session never
    needs — and a resident worker pays its imports where the user is waiting, at toggle-on. Both callers
    are already cellpose-only paths, so no AF request can reach this.

    Cheap to keep resident once loaded, which is the whole premise of this process; the point is only
    that it is not loaded for a task that has no use for it.
    """
    global _CELLPOSE
    if _CELLPOSE is None:
        from cecelia.utils.cellpose_utils import CellposeUtils
        from cecelia.utils.segmentation_utils import count_labels
        _CELLPOSE = (CellposeUtils, count_labels)
    return _CELLPOSE

#: Reply-shape + backend-set version. The backend refuses to adopt a worker that doesn't match and
#: relaunches instead — see `_ensure_preview!`. 1: single `mask` field, cellpose only.
#: 2: `layers` list with per-layer kind, cellpose + AF correction.
#: 3: layers carry `source`, the viewer layer they derive from, so the bridge can mirror its colormap.
#:    Bumped even though the bridge falls back gracefully: an adopted protocol-2 worker would omit it
#:    and quietly render every corrected channel grey, which reads as "the fix didn't work".
#: 4: AF correction is the power weight, not a ratio. `derived` carries `background` /
#:    `competingBackgrounds` / `saturatedFrac` / `exponent` where it carried `ceiling` / `background` /
#:    `afBackground`. This bump is the load-bearing kind: an adopted protocol-3 worker still has the old
#:    `af_correct_frame`, so it would keep serving RATIO previews — hollowed-out overlapping cells and
#:    all — against a backend that believes it is showing the new method. Silently, and the preview's
#:    entire purpose is to agree with the run.
#: 5: two fixes, one of which has no shape at all. (a) `af_channel_indices` had moved to `script_utils`
#:    and the call here still used the old name, so every AF preview died on `AttributeError` — a fix
#:    INSIDE the worker is exactly the case adoption gets wrong and nothing else can catch, so it earns
#:    a bump on its own; see `PREVIEW_PROTOCOL` in `app/src/preview.jl` for the behavioural rule.
#:    (b) the REQUEST now carries `channelNames` from `ccid.json`, the only authoritative copy — an
#:    adopted protocol-4 worker ignores the field and falls back to the store's stale OME-XML, which
#:    renders every corrected channel grey.
#:
#: 6: `segment.coastal` is a backend. Squarely the "answers differently" case the rule is about: an
#:    adopted protocol-5 worker has no coastal entry, so it raises "no preview backend for
#:    'segment.coastal'" — and the user would see a preview toggle that never works, on a fresh
#:    checkout, with nothing in the log pointing at an old process.
#:
#: 7: `opticalFlow.inspect` is a backend. It answers with `planes` (PNGs) instead of `layers`, so an
#:    adopted protocol-6 worker returns "no preview backend for 'opticalFlow.inspect'" and the flow
#:    panel is permanently empty with nothing in the log naming the stale process.
#:
#: NOT bumped for the AF cold-start work (per-channel background cache, `_preview_timepoints`, lazy
#: cellpose import), and that restraint is the rule working rather than an oversight. The rule is
#: "bump when an adopted older peer would ANSWER differently" — an old worker here answers the same
#: backgrounds (measured byte-identical) and the same reply shape, just slower. Bumping anyway would
#: cost every user a fresh 18 s of imports to fix nothing, which is precisely the adoption this
#: version exists to allow. A version that moves on every commit is a version nobody can reason about.
#: 8: the flow planes are COLOUR-mapped (`params.colormap`, viridis by default). Bumped by the same
#:    rule the AF work was not: an adopted protocol-7 worker ignores the parameter and answers grey
#:    PNGs, which reads as "the colormap setting does nothing" with no error anywhere.
#: 9: `opticalFlow.inspect` is pre-training only — no model, no probability plane. An adopted
#:    protocol-8 worker would still emit `probability` for a request that no longer asks for one.
#: 10: two new backends — `opticalFlow.probability` (the model's prob map, post-training) and
#:    `segment.coastalMeasure` (the composite the Segment page actually runs). Backend-set changes
#:    need the bump for the reason the header gives: an adopted older worker ignores what it has
#:    never heard of and fails with "no preview backend", which reads as a broken button rather than
#:    a stale process.
#:
#: NOT bumped for the flow-inspect region crop or the websocket frame cap below. The crop lives
#: entirely in the REQUEST (`api/src/optical_flow_api.jl` sends narrower bounds); a worker of any
#: version answers whatever region it is handed, so an adopted protocol-10 worker answers identically.
#: The cap is on messages the worker RECEIVES, and requests are a few hundred bytes.
#: 12: the bleedthrough estimator is chosen per combination by `exclusive` (different cell types →
#:     the TLS slope; possibly co-labelled → the envelope floor). Load-bearing again: an adopted
#:     protocol-11 worker always uses the envelope, which on a mutually-exclusive pair derives ~5x too
#:     small a coefficient — so it would preview a correction that visibly leaves the overspill in,
#:     against a backend that believes it is showing the new one.
#: 11: AF correction unmixes BLEEDTHROUGH before the dominance weight
#:     (`correction_utils.af_bleedthrough_alphas`). The load-bearing kind of bump, the same case as 4:
#:     an adopted protocol-10 worker still has the old one-mechanism `af_correct_frame`, so it would
#:     keep serving previews in which a leaked channel erases the target it leaked into — silently,
#:     against a backend that believes it is showing the new method, for a preview whose entire purpose
#:     is to agree with the run. Its `derived` readout would also lack `bleedthrough`.
#: 13: reply shape changed for LABELS previews — layers carry `valueName`/`path` instead of an
#:     inline `block`. The API returns a preview slab URL and the browser fetches the mask through
#:     `/api/viewer/slab?labels=<vn>&preview=1`; an adopted protocol-12 worker would still send the
#:     block and the browser would see nothing. AF is now `NotImplementedError` here (deferred to
#:     P7.1) — an old worker still segments channels the client cannot render.
#: 14: AF preview writes ONE image store per corrected channel and the reply carries
#:     `previewImages` (`sourceChannel`, `valueName`, `path`, `axes`, `shape`) instead of the
#:     `NotImplementedError`. The browser fetches each corrected channel through
#:     `/api/viewer/slab?preview_af=1&sourceChannel=N` in place of the source channel — same reader,
#:     same texture upload as a normal image slab. An adopted protocol-13 worker would still 501 on
#:     AF, so the browser AF toggle would keep looking broken against a backend that believes it works.
PROTOCOL = 14

#: Named in the error a channel NAME raises, so the message points at the Julia function that should
#: have resolved it — see `script_utils.channel_indices`.
_AF_TRANSLATOR = 'af_combinations_for_python (af_correct.jl)'
_CELLPOSE_TRANSLATOR = 'cellpose_models_for_python (cellpose.jl)'

HOST = "127.0.0.1"
PORT = int(os.environ.get("CECELIA_PREVIEW_PORT", "7656"))

# Frame cap, set explicitly on both resident-Python legs rather than left implicit — same number and
# same reason as `WS_MAX_SIZE` in napari_bridge.py and `WS_MAX_FRAME_SIZE` in app/src/utils.jl, which
# is where the measurement and the failure mode are written down. Only inbound requests pass through
# this one (they are tiny); it is here so the two workers cannot disagree about the number.
WS_MAX_SIZE = 64 * 1024 * 1024

_AXES = ("X", "Y", "Z", "T")


class PreviewState:
    """Everything worth keeping between previews. Opening the image and reading its OME-XML is the
    other per-invocation cost a resident process removes."""

    def __init__(self):
        self._images = {}        # im_path → (dask levels, dim_utils)
        self._images_zarr = {}   # im_path → plain zarr levels (see image_zarr)
        self._model_cache = {}   # shared with each CellposeUtils instance (see `segmenter`)
        self._norm = {}          # (im_path, channels, normalise) → cellpose norm params
        self._af = {}            # (im_path, channel, method) → per-channel AF stats
        self._af_alpha = {}      # (im_path, src, dst, method) → bleedthrough coefficient (0.0 = none)

    def image(self, im_path):
        """The image as DASK levels. Kept lazy because cellpose's whole-image normalisation
        (`_compute_norm_params` → `channel_histograms`) streams an entire channel: with a plain zarr
        handle that materialises the channel in RAM, which is the OOM the streaming rework removed."""
        if im_path not in self._images:
            levels, _ = zarr_utils.open_as_zarr(im_path, as_dask=True)
            dim_utils = DimUtils(ome_xml_utils.parse_meta(im_path), use_channel_axis=True)
            dim_utils.calc_image_dimensions(levels[0].shape)
            self._images[im_path] = (levels, dim_utils)
        return self._images[im_path]

    def image_zarr(self, im_path):
        """The same image as plain ZARR levels, for a reader that works one frame at a time.

        Not the same handle as `image` on purpose, and not a blanket flip of it. AF's derivation reads
        `fortify(arr[slice])` per frame — bounded either way — and measured on a real store the dask
        handle costs **9.3×** on exactly that access pattern (`af_correct_image` 278.7 s → 30.1 s),
        because each slice rebuilds and executes a graph. Cellpose's normalisation needs the opposite
        (see `image`), so both handles exist. Opening a second one is metadata-only.
        """
        if im_path not in self._images_zarr:
            levels, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
            self._images_zarr[im_path] = levels
        return self._images_zarr[im_path]

    def segmenter(self, params, dim_utils):
        """A fresh `CellposeUtils` per preview — params change every time, and that is the point —
        but carrying the loaded-model dict across, so switching a threshold doesn't reload a model."""
        CellposeUtils, _ = _cellpose_imports()
        seg = CellposeUtils(params, dim_utils)
        seg._model_cache = self._model_cache
        return seg

    def norm_params(self, seg, levels, im_path, model_params):
        """Whole-image percentile ranges, cached — **the** thing that makes a second preview fast.

        Measured on `EaMaVq` (201 × 20 × 544 × 548): `_compute_norm_params` takes **24 s**, because
        scale-to-whole is global by definition and it streams a histogram over the entire level. That
        dwarfs the ~0.5 s of inference it feeds, so paying it per preview would make the tuning loop
        pointless.

        It only depends on the image, the channels read, and the `normalise` percentile — none of
        which are what you tune. Diameter, thresholds, filters and `stitchThreshold` all reuse it, so
        the first preview on an image is slow and every subsequent one is not. Changing a channel or
        the percentile correctly misses the cache."""
        if not seg.normalise_to_whole:
            return None
        key = (im_path,
               tuple(sorted(script_utils.channel_indices(
                   model_params.get("cellChannels"), "cellChannels", _CELLPOSE_TRANSLATOR))),
               tuple(sorted(script_utils.channel_indices(
                   model_params.get("nucChannels"), "nucChannels", _CELLPOSE_TRANSLATOR))),
               model_params.get("normalise"))
        if key not in self._norm:
            self._norm[key] = seg._compute_norm_params(
                levels, model_params, max_frames=NORM_FRAMES)
        return self._norm[key]

    def af_stats(self, im_path, levels, dim_utils, channel_idx, competing_channels, method,
                 exclusive=True):
        """The correction's global values — one background level per participating channel — cached.

        The AF analogue of `norm_params`, and the same bargain: deriving these costs a pass over the
        movie, against **5-7 ms** to correct one visible plane with them. So the first preview of an
        image is slow and every one after it is instant.

        CACHED PER CHANNEL, not per combination. A background level depends on the image, the channel
        and the method — plus the two preview-only budgets below, which are module CONSTANTS and so are
        deliberately absent from the key. Make either one a parameter and it has to join the key, or a
        preview will keep serving values derived at the old budget. Keying the
        whole `AfWeightStats` by `(target, competitors)` therefore re-derived the SAME numbers once per
        combination: a three-channel AF setup asks for {1,2,3} three times over (target 1 vs 2,3; target
        2 vs 1,3; target 3 vs 1,2), and paid a full pass each time. Measured on `zolIMa/2h06xA`, one
        pass 26.9 s → **80.7 s** for the preview the user actually configured.

        So the cache holds one entry per channel and the stats are assembled from it; only genuinely
        missing channels are derived, in a single pass over their union. The second and third
        combinations of that setup now cost nothing.

        `AF_PREVIEW_STRIDE` and `_preview_timepoints` are the two concessions to the cold start, and both
        are safe because every value here is an interior threshold over a histogram, which subsampling
        does not move. (It was safe before for a more delicate reason — the ceiling was a
        COUNT-thresholded max rather than a true max — and that reason is gone along with the ceiling.)
        """
        competing = script_utils.channel_indices(
            competing_channels, f'competingChannels for channel {channel_idx}', _AF_TRANSLATOR)
        channels = script_utils.channel_indices(
            [channel_idx], 'the target channel', _AF_TRANSLATOR) + competing

        uniq = list(dict.fromkeys(channels))
        wanted_pairs = [(s, d) for s in uniq for d in uniq if s != d]
        missing_ch = [ch for ch in uniq if (im_path, ch, method) not in self._af]
        # keyed by the ESTIMATOR too: `exclusive` picks between the TLS slope and the envelope floor,
        # which differ ~5x on real data, so a cached value from the other mode is a different number
        def _mode(dst):
            return bool(exclusive) if dst == int(channel_idx) else True
        missing_pairs = [p for p in wanted_pairs
                         if (im_path, p[0], p[1], method, _mode(p[1])) not in self._af_alpha]
        # A bleedthrough coefficient belongs to an ordered PAIR, so it gets its own cache rather than
        # being squeezed into the per-channel one — and it is derived pair-by-pair
        # (`af_bleedthrough_alphas` loops over pairs independently), so deriving it over a SUBSET of the
        # channels gives the same number as over all of them. That is what keeps the two caches
        # composable: a third combination naming an already-seen pair pays nothing.
        needed = list(dict.fromkeys(missing_ch + [c for p in missing_pairs for c in p]))
        if needed:
            # one pass over the union of what is not yet known, never per combination
            derived = correction_utils.af_weight_stats(
                self.image_zarr(im_path)[0], dim_utils, needed,
                background_method=method, spatial_stride=AF_PREVIEW_STRIDE,
                timepoints=_preview_timepoints(dim_utils),
                exclusive={int(channel_idx): bool(exclusive)})
            for ch in needed:
                # nbins and exponent are image-wide rather than per-channel, but they are two ints and
                # carrying them here keeps this to ONE cache — a second one keyed by image would have to
                # be invalidated in step with this, for nothing
                self._af[(im_path, ch, method)] = (
                    derived.backgrounds[ch], derived.saturated[ch], derived.nbins, derived.exponent)
            for s in needed:
                for d in needed:
                    # ABSENT means "no leak detected", and that has to be cached as 0.0 — leaving the key
                    # out would make every subsequent preview re-derive the whole pass to learn the same
                    # nothing, which is the cold start this cache exists to pay once.
                    #
                    # Keyed by the mode THIS PAIR was derived under, which is only the caller's flag for
                    # pairs INTO the requested target; every other pair fell back to the default. Keying
                    # them all by the caller's flag would file a default-derived value under the other
                    # mode, and a later request for that target would read a coefficient from the
                    # estimator it did not ask for.
                    if s == d:
                        continue
                    mode = bool(exclusive) if d == int(channel_idx) else True
                    self._af_alpha.setdefault(
                        (im_path, s, d, method, mode), float(derived.alphas.get((s, d), 0.0)))

        entries = [self._af[(im_path, ch, method)] for ch in channels]
        return correction_utils.AfWeightStats(
            backgrounds={ch: entries[i][0] for i, ch in enumerate(channels)},
            alphas={p: self._af_alpha[(im_path, p[0], p[1], method, _mode(p[1]))]
                    for p in wanted_pairs
                    if self._af_alpha.get((im_path, p[0], p[1], method, _mode(p[1])), 0.0) > 0.0},
            saturated={ch: entries[i][1] for i, ch in enumerate(channels)},
            nbins=entries[0][2], exponent=entries[0][3])


STATE = PreviewState()


def _axis_lengths(dim_utils):
    out = {}
    for ax in _AXES:
        if dim_utils.dim_idx(ax) is not None:
            out[ax] = int(dim_utils.dim_val(ax))
    return out


def _axis_indices(dim_utils, exclude=()):
    """Axis letter → array axis index. `exclude` drops an axis and shifts the ones after it, which is
    how the label store's axes relate to the image's (labels have no channel axis)."""
    order = [ax for ax in dim_utils.im_dim_order if ax not in exclude]
    return {ax: (order.index(ax) if ax in order else None) for ax in (*_AXES, "C")}


def _as_cyx(cropped, dim_utils):
    """The cropped block → `[C, Y, X]`, which is what `predict_slice` takes for a 2D tile.

    T and Z are size 1 by construction (one timepoint, one plane), so dropping them is a reshape, not
    a projection — nothing is averaged or lost."""
    order = list(dim_utils.im_dim_order)
    kept = [ax for ax in order if ax not in ("T", "Z")]
    arr = cropped.reshape([s for ax, s in zip(order, cropped.shape) if ax not in ("T", "Z")])
    if "C" in kept:
        return np.moveaxis(arr, kept.index("C"), 0)
    return arr[np.newaxis, ...]


def _real_image_edges(bounds, axis_len):
    """Which of the crop's Y/X faces are the IMAGE edge, not just where the user stopped looking.

    `SegmentationUtils.post_process` needs this to avoid two crop artefacts that both show FEWER cells
    than the run produces: clearing every cell at the crop edge, and size-filtering a cell on its
    clipped pixel count. See that method's docstring.
    """
    out = {}
    for ax in ("Y", "X"):
        if ax not in bounds:
            continue
        lo, hi = int(bounds[ax][0]), int(bounds[ax][1])
        full = int(axis_len.get(ax, hi))
        out[ax] = (lo <= 0, hi >= full)
    return out


def _region_signal(im_path, bounds, tile):
    """Does this region contain image data at all? `(has_signal, why)`.

    "0 cells" is ambiguous and the ambiguity is expensive: on a drift-corrected stack, aiming at a
    padded plane returns 0 cells and looks EXACTLY like a diameter that is too large, so the user
    retunes parameters against a region that could never produce a mask (see docs/TODO.md → A third of a
    drift-corrected stack can be empty, which this
    was measured on).

    Two checks, authoritative first:

    * **The valid box** (`zarr_utils.read_valid_box`, #435) — the producer recorded which part of the
      store is data rather than padding, per timepoint. This is a fact, not an inference, and it is the
      case that actually bites. `None` means the store never padded, which is most of them.
    * **All-zero pixels** — the fallback for a store with no box (an unpadded region can still be
      genuinely black: a channel with no signal here, or outside the specimen).
    """
    try:
        t_lo = int(bounds["T"][0]) if "T" in bounds else None
        box = zarr_utils.read_valid_box(im_path, timepoint=t_lo)
    except Exception:
        box = None                      # no box, unreadable, or an older store — fall through
    if box:
        for ax, (lo, hi) in box.items():
            if ax not in bounds:
                continue
            r_lo, r_hi = int(bounds[ax][0]), int(bounds[ax][1])
            if r_hi <= int(lo) or r_lo >= int(hi):        # region lies wholly outside the data
                return False, "padding"
    if not np.any(tile):
        return False, "blank"
    return True, ""


def _run_tile_seams(bounds, axis_len, block_size):
    """Tile-write boundaries the RUN would place strictly inside this region, per axis.

    The preview hands the whole visible region to `predict_slice` as ONE tile. A run does not: it tiles
    at `blockSize` and re-stitches labels split across each seam (`_create_xy_tiles` →
    `_stitch_tile_seams`). So where a seam crosses the previewed region, the run's mask there is the
    product of two inferences plus an IoU re-join, and the preview's is a single inference — the counts
    and boundaries near it legitimately differ.

    The test is POSITIONAL, not "is the region bigger than blockSize": the grid is anchored at the image
    origin (`y = 0, block_size, 2*block_size, …`) and only the *write* bounds land on it (reads are
    padded by `overlap`). A 600 px region sitting inside one 1024 px tile has no seam; a 300 px one
    straddling y=512 has one.
    """
    seams = {}
    if not block_size or block_size < 1:
        return seams
    for ax in ("Y", "X"):
        if ax not in bounds:
            continue
        lo, hi = int(bounds[ax][0]), int(bounds[ax][1])
        full = int(axis_len.get(ax, hi))
        n = sum(1 for b in range(block_size, full, block_size) if lo < b < hi)
        if n:
            seams[ax] = n
    return seams


class PreviewContext:
    """Everything a preview backend needs, computed once by `preview` before it dispatches.

    The split exists so each task's compute is the ONLY thing that varies: opening the image, turning
    the viewer's region into bounds, and cropping are identical whether you are previewing a
    segmentation or a correction, and previously they were welded to cellpose.
    """

    __slots__ = ('im_path', 'task_dir', 'value_name', 'params', 'levels', 'dim_utils',
                 'axis_len', 'bounds', 'fallback2d', 'given_names')

    def __init__(self, im_path, task_dir, value_name, params, levels, dim_utils,
                 axis_len, bounds, fallback2d, given_names=None):
        self.im_path, self.task_dir, self.value_name = im_path, task_dir, value_name
        self.params, self.levels, self.dim_utils = params, levels, dim_utils
        self.axis_len, self.bounds, self.fallback2d = axis_len, bounds, fallback2d
        self.given_names = list(given_names or ())

    def crop(self):
        """The visible region of the image, all channels, as `[C, Y, X]`."""
        sl = slice_utils.crop_slice_tuple(
            self.levels[0].ndim, _axis_indices(self.dim_utils), self.bounds)
        return _as_cyx(zarr_utils.fortify(self.levels[0][sl]), self.dim_utils)

    def crop_at_t(self, t):
        """The same region at timepoint `t`, as `[C, Y, X]` — read one frame at a time.

        Reads through the PLAIN ZARR handle, not `self.levels`. This is the access pattern
        `PreviewState.image_zarr` exists for and its docstring already measures on the AF path: a
        dask slice rebuilds and executes a graph, so paying that per frame across a temporal window
        is the whole cost. Measured on `zolIMa/VJy1Nx` driftCorrected, the 17-frame window a
        `temporalScales` of 8 needs, 512 px crop:

            17 dask slices     1.276 s
            17 zarr slices     0.106 s      (12x)

        That was 41% of the 3.1 s a flow panel took to answer a nudge of the t slider. `self.levels`
        stays dask because `norm_params` needs it lazy — a whole-channel histogram through a plain
        handle is the OOM that put it there — so this is a second handle, not a flip of the first,
        exactly as `af_stats` uses one.

        `crop()` is deliberately left alone: it is ONE slice, so it pays the graph once.
        """
        levels = STATE.image_zarr(self.im_path)
        sl = slice_utils.crop_slice_tuple(
            levels[0].ndim, _axis_indices(self.dim_utils), {**self.bounds, 'T': (t, t + 1)})
        return _as_cyx(zarr_utils.fortify(levels[0][sl]), self.dim_utils)

    def block_geometry(self):
        """`(axes, full_shape, block_shape)` for a channel-less block covering this region — what the
        receiver needs to place it in a full-extent layer with no translate."""
        axes = [ax for ax in self.dim_utils.im_dim_order if ax != 'C']
        full = [self.axis_len[ax] if ax in self.axis_len else 1 for ax in axes]
        sl = slice_utils.crop_slice_tuple(
            len(axes), _axis_indices(self.dim_utils, exclude=('C',)), self.bounds)
        shape = tuple(len(range(*x.indices(d))) for x, d in zip(sl, full))
        return axes, [int(x) for x in full], shape

    def channel_names(self):
        """Display name per channel index, for naming a layer after the channel it corrects.

        The request's `channelNames` WINS. `ccid.json` is authoritative for channel names — they are
        user-editable — so only Julia knows them, and the store's OME-XML is a different copy that is
        routinely out of date: on a real image it still read `CH1..CH4` while the viewer showed
        `SHG`/`nuc-GFP`/`mem-TOM`/`CD169-Kat`, which named the corrected layer `CH3 AF` and pointed its
        `source` at a layer that does not exist — so the colormap mirror found nothing and every
        corrected channel came out grey.

        OME-XML remains the fallback for a caller that sends no names (a REPL or test driving the worker
        directly). It is a fallback, not a second source of truth.
        """
        if self.given_names:
            return list(self.given_names)
        try:
            px = ome_xml_utils.parse_meta(self.im_path).images[0].pixels
            return [c.name or f'ch{i}' for i, c in enumerate(px.channels)]
        except Exception:
            return []


def _layer_disk(kind, name, value_name, path, axes, full_shape, source=None):
    """One layer for the browser viewer to fetch. `kind` decides Labels vs Image on the receiving end.

    The mask is on DISK — the reply carries its `path` and `valueName`, not a decoded block. The
    browser fetches it through `/api/viewer/slab?labels=<vn>&preview=1` (the same reader as the real
    labels slab), so there is no second decode path and the preview and the run share one texture
    upload.

    `source` names the viewer layer this one is DERIVED from — kept for the AF path, which does not
    yet write to disk (P7.1).
    """
    out = {'kind': kind, 'name': name, 'valueName': str(value_name),
           'path': str(path),
           'shape': [int(x) for x in full_shape], 'axes': list(axes)}
    if source:
        out['source'] = str(source)
    return out


def _region_slice(axes, bounds):
    """A tuple of `slice` objects placing the region within a full-shape array. Axes with no bound
    stay whole (`slice(None)`)."""
    out = []
    for ax in axes:
        b = bounds.get(ax) if bounds else None
        if b is None:
            out.append(slice(None))
        else:
            out.append(slice(int(b[0]), int(b[1])))
    return tuple(out)


def _sweep_preview_labels(task_dir):
    """Best-effort delete of every `*__preview.ome.zarr` (and its staging siblings) under
    `{task_dir}/labels/`. Called from the `cleanup` message, from the start of every request, and
    from atexit."""
    if not task_dir:
        return
    labels_dir = os.path.join(task_dir, 'labels')
    if not os.path.isdir(labels_dir):
        return
    for pattern in (f'*{_PREVIEW_LABEL_SUFFIX}',
                    f'*{_PREVIEW_LABEL_SUFFIX}.partial',
                    f'*{_PREVIEW_LABEL_SUFFIX}.superseded'):
        for path in glob.glob(os.path.join(labels_dir, pattern)):
            try:
                shutil.rmtree(path, ignore_errors=True)
            except OSError:
                pass


def _sweep_preview_af(task_dir):
    """Best-effort delete of every `*__preview_af_ch*.ome.zarr` (and its staging siblings) directly
    under `{task_dir}/`. The AF stores sit at the image meta root, not under `labels/`, because they
    are image data — one per corrected channel — and the slab route resolves them from
    `dirname(image_zarr_path)` which IS the task dir."""
    if not task_dir or not os.path.isdir(task_dir):
        return
    for pattern in (f'*{_PREVIEW_AF_STEM}*{_PREVIEW_AF_SUFFIX}',
                    f'*{_PREVIEW_AF_STEM}*{_PREVIEW_AF_SUFFIX}.partial',
                    f'*{_PREVIEW_AF_STEM}*{_PREVIEW_AF_SUFFIX}.superseded'):
        for path in glob.glob(os.path.join(task_dir, pattern)):
            try:
                shutil.rmtree(path, ignore_errors=True)
            except OSError:
                pass


def _atexit_sweep():
    for td in list(_PREVIEW_TASK_DIRS):
        _sweep_preview_labels(td)
        _sweep_preview_af(td)


atexit.register(_atexit_sweep)


def _stage_labels_store(block, axes, full_shape, bounds, task_dir, value_name, im_path=None):
    """Write ONE level of labels into `{task_dir}/labels/{value_name}__preview.ome.zarr`.

    The block is region-sized; the store is FULL-image-sized with the block placed at `bounds` and
    the rest left as the array's fill value (0 — unwritten label chunks read as background, which is
    exactly what a preview outside the region should show). This is the geometry the real labels slab
    reads at, so `/api/viewer/slab?labels=<vn>&preview=1` rides the same reader with no offset
    fiddling on either side (`try_serve_slab` just retargets the path).

    The staging + promote lifecycle comes from `zarr_utils.staged_store`, and any leftover preview
    store at the same path is wiped up-front — a killed request from a previous parameter change
    would otherwise sit as `.partial` debris until this worker exits.

    Returns the promoted absolute path.
    """
    _PREVIEW_TASK_DIRS.add(task_dir)
    labels_dir = os.path.join(task_dir, 'labels')
    os.makedirs(labels_dir, exist_ok=True)

    final_path = os.path.join(labels_dir, f'{value_name}{_PREVIEW_LABEL_SUFFIX}')

    # Start-of-request cleanup: an earlier preview may have promoted at this exact path.
    if os.path.exists(final_path):
        shutil.rmtree(final_path, ignore_errors=True)

    # Inherit zarr format from the source image; labels codec is separate from that format decision
    # (see `_open_label_store` in segmentation_utils — one rule for both label writers).
    enc = zarr_utils.store_encoding_of(im_path) if im_path else {'zarr_format': 2, 'separator': None}
    fmt = enc.get('zarr_format', 2)
    separator = enc.get('separator')

    full = tuple(int(x) for x in full_shape)
    axes_up = [str(a).upper() for a in axes]

    with zarr_utils.staged_store(final_path) as staging:
        g = zarr.open_group(staging, mode='w', zarr_format=fmt)
        ms_meta = zarr_utils.multiscales_metadata(axes_up, 1)
        zarr_utils.write_multiscales_attrs(g, ms_meta, fmt)

        # Chunk per-plane on Y/X so writing the region touches only the chunks that overlap it —
        # the rest are unwritten and read back as 0 through the labels store's fill_value.
        chunks = tuple(min(full[i], 512) if ax in ('Y', 'X') else 1
                       for i, ax in enumerate(axes_up))
        level0 = g.create_array(
            '0', shape=full, chunks=chunks, dtype=block.dtype, fill_value=0,
            **zarr_utils._codec_kwargs('labels', fmt, separator=separator))
        level0[_region_slice(axes_up, bounds)] = np.ascontiguousarray(block)

    return final_path


def _stage_af_image_store(block, axes, full_shape, bounds, task_dir, value_name,
                          channel_index, im_path=None):
    """Write ONE level of a corrected image channel into
    `{task_dir}/{value_name}__preview_af_ch{N}.ome.zarr`.

    Same geometry story as `_stage_labels_store`: region-sized block placed at `bounds` inside a
    full-image store, unwritten chunks read back as 0. The slab route (`preview_af=1&sourceChannel=N`)
    swaps this in for the source channel's slab, so the geometry MUST match the image or the browser's
    `X-Slab-Shape` assertion will 500 the render.

    The store sits at the image meta root (not under `labels/`), so `dirname(image_zarr_path)` on the
    Julia side resolves directly to it. One store per corrected channel — sibling stores for channels
    the user is A/B toggling against stay separate, so the browser can swap channel-by-channel.

    Codec kind is `'image'`, matching the source image (label codec here would be wrong: the corrected
    output is intensity, not label ids). Returns the promoted absolute path.
    """
    _PREVIEW_TASK_DIRS.add(task_dir)
    os.makedirs(task_dir, exist_ok=True)

    final_path = os.path.join(
        task_dir, f'{value_name}{_PREVIEW_AF_STEM}{int(channel_index)}{_PREVIEW_AF_SUFFIX}')

    # Start-of-request cleanup: an earlier preview may have promoted at this exact path.
    if os.path.exists(final_path):
        shutil.rmtree(final_path, ignore_errors=True)

    enc = zarr_utils.store_encoding_of(im_path) if im_path else {'zarr_format': 2, 'separator': None}
    fmt = enc.get('zarr_format', 2)
    separator = enc.get('separator')

    full = tuple(int(x) for x in full_shape)
    axes_up = [str(a).upper() for a in axes]

    with zarr_utils.staged_store(final_path) as staging:
        g = zarr.open_group(staging, mode='w', zarr_format=fmt)
        ms_meta = zarr_utils.multiscales_metadata(axes_up, 1)
        zarr_utils.write_multiscales_attrs(g, ms_meta, fmt)

        chunks = tuple(min(full[i], 512) if ax in ('Y', 'X') else 1
                       for i, ax in enumerate(axes_up))
        level0 = g.create_array(
            '0', shape=full, chunks=chunks, dtype=block.dtype, fill_value=0,
            **zarr_utils._codec_kwargs('image', fmt, separator=separator))
        level0[_region_slice(axes_up, bounds)] = np.ascontiguousarray(block)

    return final_path


def _preview_cellpose(ctx):
    """Segment the visible region with the task's own `predict_slice` + `post_process`."""
    models = ctx.params.get('models') or {}
    if not models:
        raise ValueError('no models in preview params')

    # `SegmentationUtils.__init__` requires taskDir/outputValueName — it is built to own its output
    # store. A preview never writes one (the block is returned instead), so these only satisfy the
    # constructor's contract; nothing in the preview path resolves them to a real path.
    seg = STATE.segmenter(
        {**ctx.params, 'taskDir': ctx.task_dir, 'outputValueName': ctx.value_name}, ctx.dim_utils)
    tile = ctx.crop()
    axes, full_shape, block_shape = ctx.block_geometry()

    _, count_labels = _cellpose_imports()
    merged, passes = None, []
    for key in _base_groups(seg, models):
        model_params = models[key]
        # Whole-image intensity statistics, applied to the crop: percentiles over the visible
        # region alone would normalise differently from the run, so the preview would show a
        # result the run cannot reproduce. Cached across previews — see `PreviewState.norm_params`.
        norm_params = STATE.norm_params(seg, ctx.levels, ctx.im_path, model_params)
        masks = seg.predict_slice(tile, model_params, norm_params)
        # The label modifications the RUN applies after inference — erosion, expansion, the size
        # filter, border clearing. `la_t=None, T=1` is the whole-array branch the run uses per frame;
        # `is_3d=False` because a preview is one z-plane (so `clearDepth` can never apply — the 2D
        # warning covers that). `real_border` keeps it honest about being a CROP: see `post_process`.
        masks = seg.post_process(masks, ['Y', 'X'], None, 1, False,
                                 real_border=_real_image_edges(ctx.bounds, ctx.axis_len))
        merged, passes = _merge_pass(seg, merged, masks, key, passes, count_labels)

    if merged is None:
        raise ValueError('no base model in preview params')
    counts = {'base': count_labels(merged)}
    block = np.reshape(np.asarray(merged, dtype=seg.LABEL_DTYPE), block_shape)

    has_signal, why = _region_signal(ctx.im_path, ctx.bounds, tile)
    preview_path = _stage_labels_store(
        block, axes, full_shape, ctx.bounds, ctx.task_dir, ctx.value_name, im_path=ctx.im_path)
    return {
        'counts': counts,
        'passes': passes,
        'hasSignal': has_signal,
        'noSignalWhy': why,
        # tile seams the RUN would place inside this region, which the preview does not reproduce
        'runSeams': _run_tile_seams(ctx.bounds, ctx.axis_len, seg.block_size),
        'blockSize': int(seg.block_size),
        'layers': [_layer_disk('labels', 'Preview', ctx.value_name, preview_path, axes, full_shape)],
    }


def _base_groups(seg, models):
    """The `base` model groups a preview segments, in the order the RUN applies them.

    `SegmentationUtils.model_order`, not `sorted()` — the ordering rule is numeric, so past nine
    groups a plain sort puts `'10'` before `'2'` and previews the passes in an order no run uses.
    Only `base`: a preview judges the primary label type, and a `nuc` pass is matched INTO the base
    by the run rather than shown beside it.
    """
    return [k for k in seg.model_order(models)
            if str(models[k].get('matchAs', 'base')) == 'base']


def _merge_pass(seg, merged, masks, key, passes, count_labels):
    """Stack one pass onto the preview exactly as `predict_from_zarr` stacks it into the store.

    `offset_pass` + `fill_unlabelled` are the run's own two primitives, so this cannot drift from
    what a run produces — which is the whole point, and what the previous version got wrong by
    overwriting its output block per group.

    `passes` accumulates `{group, from, to, objects}`: the same id ranges the run stamps onto the
    store via `write_label_passes`, plus a count, so the reply can say what the SECOND pass actually
    contributed. On a two-pass config that number is the one being judged — it is the objects pass 1
    missed, and a preview showing only a merged total cannot tell you it is zero.
    """
    masks = np.asarray(masks, dtype=seg.LABEL_DTYPE)
    # The running id counter comes from the RANGES, not from `merged.max()`. A pass whose output is
    # entirely covered by an earlier one leaves no trace in the array, so `merged.max()` would fall
    # back to the previous pass's top and the NEXT pass would reuse ids this one already owns —
    # overlapping ranges, and every later `objects` count wrong. `predict_from_zarr` keeps
    # `max_labels` as a separate monotonic counter for exactly this reason.
    top = passes[-1]['to'] if passes else 0
    first_id = top + 1
    masks, top = seg.offset_pass(masks, top)
    merged = masks if merged is None else seg.fill_unlabelled(merged, masks)
    if top >= first_id:
        # Counted AFTER the merge: a pass-2 object entirely covered by pass 1 contributes nothing to
        # the picture, so counting its own output would report objects that are not on screen.
        kept = merged[(merged >= first_id) & (merged <= top)]
        passes.append({'group': str(key), 'from': first_id, 'to': top,
                       'objects': int(np.unique(kept).size)})
    return merged, passes


_WINDOW_ID = itertools.count(1)


def _next_window_id():
    """A window id that is never reused in this process — what `TemporalWindow.id` requires."""
    return next(_WINDOW_ID)


def _coastal_imports():
    """Deferred like `_cellpose_imports`, and for the same reason: torch + coastal cost a session
    that never previews a flow model nothing."""
    from cecelia.utils.coastal_utils import CoastalUtils
    from cecelia.utils.segmentation_utils import count_labels, TemporalWindow
    return CoastalUtils, count_labels, TemporalWindow


def _temporal_window(ctx, radius):
    """`(context, centre)` — the visible region across a temporal window, as `[W, C, Y, X]`.

    `centre` indexes the requested timepoint WITHIN the window, which is not `radius` at the ends of
    the movie: the window is clamped there, never reflected, because repeating a frame invents motion
    that was not imaged. Every caller needs that index, so it is returned rather than re-derived.

    Shared by the coastal segmentation preview and both flow canvas plots because all three claim to
    show what a RUN is fed over the window `predict_from_zarr` would have built. Two copies of the
    clamping were already two chances for one of those claims to stop being true; they were also two
    copies of the read, and only one of them would have been made fast.
    """
    t_now = int(ctx.bounds.get('T', (0, 1))[0])
    n_t = int(ctx.axis_len.get('T', 1))
    lo = max(0, t_now - radius)
    hi = min(n_t - 1, t_now + radius)
    # Each frame is reduced to [C, Y, X] by the same helper the single-frame path uses, so the window
    # is exactly "the tile, through time".
    return np.stack([ctx.crop_at_t(t) for t in range(lo, hi + 1)]), t_now - lo


def _preview_coastal(ctx):
    """Segment the visible region with the flow model — same `predict_slice` the run calls.

    The one difference from the cellpose backend is the shape of the input. Coastal's prediction for a
    timepoint reads frames AROUND it, so the region alone is not enough: the worker hands over one
    plane at one timepoint, and this rebuilds the temporal window the run's `predict_from_zarr` would
    have built, over the SAME region and by the same rules — clamped at the movie's ends, never
    reflected, and the model's own radius rather than a preview-specific one. Get that wrong and the
    preview shows something the run cannot reproduce, which is worse than no preview.
    """
    models = ctx.params.get('models') or {}
    if not models:
        raise ValueError('no models in preview params')

    CoastalUtils, count_labels, TemporalWindow = _coastal_imports()
    seg = CoastalUtils(
        {**ctx.params, 'taskDir': ctx.task_dir, 'outputValueName': ctx.value_name}, ctx.dim_utils)

    axes, full_shape, block_shape = ctx.block_geometry()
    context, centre = _temporal_window(ctx, seg.TEMPORAL_RADIUS)
    tile = context[centre]

    # `predict_slice` takes ONE `TemporalWindow` (segmentation_utils) — it used to take `context=` and
    # `context_index=`, and it grew to six such kwargs before they were collected into the object. This
    # caller was not updated, so every coastal preview raised `unexpected keyword argument 'context'`.
    # Built here rather than inside `_temporal_window` because the other caller of that helper wants
    # the raw pair for `_project_window`, not a window object.
    #
    #   start:    the movie index of `frames[0]`. `centre` is `t_now - lo`, so this recovers `lo` —
    #             which is not `t_now - radius` at the start of a movie, where the window is clamped.
    #   tile:     full-image Y/X, so work can be carried between timepoints of the SAME region and
    #             not across a region that moved sideways.
    #   channels: None — `crop_at_t` reads every channel, unlike the run, which narrows.
    #   id:       monotonic per process. The per-window caches key on it and must never see a value
    #             twice, so it cannot be derived from the timepoint (the user nudges t back and forth
    #             over a region that has changed).
    y0, y1 = ctx.bounds.get('Y', (0, int(ctx.axis_len.get('Y', 0))))
    x0, x1 = ctx.bounds.get('X', (0, int(ctx.axis_len.get('X', 0))))
    window = TemporalWindow(
        frames=context, index=centre,
        start=int(ctx.bounds.get('T', (0, 1))[0]) - centre,
        tile=(int(y0), int(y1), int(x0), int(x1)),
        channels=None, id=_next_window_id())

    merged, passes = None, []
    for key in _base_groups(seg, models):
        model_params = models[key]
        norm_params = STATE.norm_params(seg, ctx.levels, ctx.im_path, model_params)
        masks = seg.predict_slice(tile, model_params, norm_params, window)
        masks = seg.post_process(masks, ['Y', 'X'], None, 1, False,
                                 real_border=_real_image_edges(ctx.bounds, ctx.axis_len))
        merged, passes = _merge_pass(seg, merged, masks, key, passes, count_labels)

    if merged is None:
        raise ValueError('no base model in preview params')
    counts = {'base': count_labels(merged)}
    block = np.reshape(np.asarray(merged, dtype=seg.LABEL_DTYPE), block_shape)

    has_signal, why = _region_signal(ctx.im_path, ctx.bounds, tile)
    preview_path = _stage_labels_store(
        block, axes, full_shape, ctx.bounds, ctx.task_dir, ctx.value_name, im_path=ctx.im_path)
    return {
        'counts': counts,
        'passes': passes,
        'hasSignal': has_signal,
        'noSignalWhy': why,
        'runSeams': _run_tile_seams(ctx.bounds, ctx.axis_len, seg.block_size),
        'blockSize': int(seg.block_size),
        'layers': [_layer_disk('labels', 'Preview', ctx.value_name, preview_path, axes, full_shape)],
    }


def _preview_flow_inspect(ctx):
    """"What goes INTO a model" — every flow metric plane for one timepoint, as PNGs.

    A PRE-TRAINING view, and no model appears in it at all. The question is *which of these look
    like cells*, asked before anything is trained; the metrics are a property of the movie, the
    channels and the temporal scales, and a checkpoint has nothing to say about them. An earlier
    version took an optional model and added its probability map, which quietly turned a "what
    should I train on" panel into a "what did I train" one.

    Nor instances: those are SEGMENTATION output and the Segment page previews them through the
    normal preview path — a second instance renderer here would be the same picture computed a
    different way.

    A BACKEND rather than a new message type, so the region maths, the image handle, the norm-param
    cache and the reply envelope are all the ones `preview` already has — and the window/projection
    is built by the same `CoastalUtils` the real run uses, so these are the planes a run is actually
    fed. Returns `planes`, not `layers`: canvas plots, nothing near napari.
    """
    seg, mp, frame, metrics, scales = _flow_frame_and_metrics(ctx)
    planes = [('input (projected)', frame)] + [(k, metrics[k]) for k in sorted(metrics)]
    return {**_planes_reply(ctx, planes),
            'metricKeys': sorted(metrics),
            'temporalScales': list(scales)}


def _flow_frame_and_metrics(ctx):
    """`(seg, model_params, frame, metrics, scales)` for the timepoint in view.

    Shared by the metric sheet and the probability map because it must be: both claim to show what a
    RUN is fed, and two copies of the window/projection/metric build would be two chances to drift
    from `CoastalUtils` — the exact silent-misalignment failure the metric-set contract exists to
    prevent. One path, used by both, and it is the run's own.
    """
    models = ctx.params.get('models') or {}
    if not models:
        raise ValueError('no models in preview params')

    CoastalUtils, _, _ = _coastal_imports()
    seg = CoastalUtils(
        {**ctx.params, 'taskDir': ctx.task_dir, 'outputValueName': ctx.value_name}, ctx.dim_utils)
    mp = models[sorted(models.keys())[0]]
    # `_temporal_for`, not `temporal_config`: the run's resolution, including the Temporal scale mode
    # and the plane rename it needs. Reading the manifest directly showed the TRAINED offsets whatever
    # the form said, so under "Match durations" the preview and the run disagreed about the very
    # channels this view exists to display.
    scales, cumulative, dropped, rename = seg._temporal_for(mp)

    context, centre = _temporal_window(ctx, seg.TEMPORAL_RADIUS)

    window = seg._project_window(context, mp, STATE.norm_params(seg, ctx.levels, ctx.im_path, mp))
    # `_plane_features` rather than `_flow_metrics` + a local drop: it is the run's own composition of
    # the two, so the rename cannot be forgotten here while the run applies it.
    frame, metrics = seg._plane_features(window, centre, scales, cumulative, dropped, rename)
    return seg, mp, frame, metrics, scales


def _planes_reply(ctx, planes):
    """`{planes: [{name, png}]}` — the canvas-plot envelope. No layers, nothing near napari."""
    from cecelia.utils.plane_render import DEFAULT_COLORMAP, plane_png
    cmap = str(ctx.params.get('colormap') or DEFAULT_COLORMAP)
    return {'planes': [{'name': n,
                        'png': base64.b64encode(plane_png(a, colormap=cmap)).decode('ascii')}
                       for n, a in planes]}


def _preview_flow_probability(ctx):
    """"How good is this model" — the projected input beside the model's probability map.

    The POST-training counterpart to `_preview_flow_inspect`, and deliberately a separate view: that
    one is asked before a model exists and must not take one (a model picker there turned "what
    should I train on" into "what did I train"). This one is meaningless without a checkpoint.

    Two planes only. Not instances — those are segmentation output and the Segment page previews them
    through the normal preview path; the question here is whether the model learned to tell cell from
    background at all, which is exactly what the probability map shows and what instances hide behind
    a threshold and a growing step.

    `predict_frame` returns `(prob_map, instances, props)` and the run throws the first away
    (`CoastalUtils._predict_plane`), so this is the one place it is looked at.
    """
    seg, mp, frame, metrics, _scales = _flow_frame_and_metrics(ctx)
    # `_get_inference` is the run's own accessor and it CACHES per model — so scrubbing t or z
    # re-runs the network but does not rebuild it, which is what makes this fast enough to be a
    # canvas plot rather than a job.
    prob, _instances, _props = seg._get_inference(mp).predict_frame(frame, metrics)
    return {**_planes_reply(ctx, [('input (projected)', frame),
                                 ('probability', np.asarray(prob, dtype=np.float32))]),
            'metricKeys': sorted(metrics)}


def _preview_af(ctx):
    """AF-correct the visible region, one scratch image store per corrected channel.

    Same shape as the pre-P7 (napari-era) path — `af_weight_stats` is derived over the WHOLE image
    (tens of seconds on a real movie, cached; do NOT derive over the crop, that is the whole reason
    for the split) and `af_correct_frame` is per-voxel arithmetic on the visible tile. The change is
    delivery: instead of an inline block per corrected channel that napari stamps as a new Image
    layer, each channel's corrected pixels go to
    `{task_dir}/{value_name}__preview_af_ch{N}.ome.zarr` and the reply carries a `previewImages`
    entry per corrected channel `{sourceChannel, valueName, path, shape, axes}`. The browser flips
    that channel's slab URL onto the scratch store (`/api/viewer/slab?preview_af=1&sourceChannel=N`),
    keeping A/B by toggling the preview on/off rather than side-by-side layers (a two-texture compare
    mode would be its own design).

    Codec is `'image'` — the corrected output is intensity, not label ids — and dtype is the source
    image's, so `af_correct_frame` casts in place and the slab route sees the same bytes-per-voxel it
    would have for the original channel.
    """
    combos = {int(k): v for k, v in (ctx.params.get('afCombinations') or {}).items()}
    if not combos:
        raise ValueError('no channel combinations in preview params')
    method = str(ctx.params.get('backgroundMethod', 'triangle'))

    tile = ctx.crop()                       # [C, Y, X]
    axes, full_shape, block_shape = ctx.block_geometry()
    names = ctx.channel_names()
    out_dtype = ctx.levels[0].dtype

    preview_images, stats_out = [], {}
    for ch in sorted(combos):
        # `channel_indices` here does two things: coerces string names → ints, and points a stale
        # backend at the Julia translator it should have gone through. A miss in the combo list means
        # the caller sent something we can't compute against — skip cleanly, don't raise.
        competing = script_utils.channel_indices(
            combos[ch].get('competingChannels'), f'competingChannels for channel {ch}',
            _AF_TRANSLATOR)
        if not competing:
            continue
        stats = STATE.af_stats(ctx.im_path, ctx.levels, ctx.dim_utils, ch, competing, method,
                               exclusive=bool(combos[ch].get('exclusive', True)))
        slabs = {c: tile[c] for c in [ch] + competing}
        corrected = correction_utils.af_correct_frame(slabs, ch, stats, out_dtype)
        # Reshape to the channel-less block shape (T/Z restored as length-1) — the store is
        # channel-less (one file per channel), so no channel axis lands in `axes`/`full_shape` either.
        block = np.reshape(corrected, block_shape)
        preview_path = _stage_af_image_store(
            block, axes, full_shape, ctx.bounds, ctx.task_dir, ctx.value_name,
            channel_index=ch, im_path=ctx.im_path)
        label = names[ch] if ch < len(names) else f'ch{ch}'
        preview_images.append({
            'sourceChannel': int(ch),
            'name': f'{label} AF',
            'valueName': str(ctx.value_name),
            'path': str(preview_path),
            'shape': [int(x) for x in full_shape],
            'axes': list(axes),
        })
        # `af_derived_values` is the same helper the run's QC reports through, so the readout on
        # the preview toggle and the banked metric cannot disagree about a name or a value.
        stats_out[str(ch)] = correction_utils.af_derived_values(stats, ch)

    if not preview_images:
        raise ValueError('no combination names a competing channel')

    has_signal, why = _region_signal(ctx.im_path, ctx.bounds, tile)
    return {
        'hasSignal': has_signal,
        'noSignalWhy': why,
        'derived': stats_out,
        'previewImages': preview_images,
    }


#: fun_name → the compute that previews it. A task absent here is not previewable, which the Julia
#: side already declares via `task_previewable` — this is the other half of the same statement.
# A COMPOSITE needs its own entry, mapped to the backend of the step a preview actually runs. The
# module pages run the composites (`segment.cellposeMeasure`, `segment.coastalMeasure`), not the bare
# segmenters, so a composite missing here is not a corner case — it is the preview button being dead
# on the page people use. This has now shipped broken twice for exactly that reason, so the pairing is
# asserted by `test_preview_backends_cover_composites.py` rather than left to whoever adds the next
# composite noticing that this file exists.
_BACKENDS = {
    'segment.cellpose': _preview_cellpose,
    'segment.cellposeMeasure': _preview_cellpose,
    'segment.coastal': _preview_coastal,
    'segment.coastalMeasure': _preview_coastal,
    'opticalFlow.inspect': _preview_flow_inspect,
    'opticalFlow.probability': _preview_flow_probability,
    'cleanupImages.afCorrect': _preview_af,
    'cleanupImages.afDriftCorrect': _preview_af,
}


def preview(msg):
    im_path = msg['imPath']
    task_dir = msg['taskDir']
    value_name = str(msg.get('outputValueName', 'preview'))
    region = msg.get('region') or {}
    params = dict(msg.get('params') or {})
    fun_name = str(msg.get('funName', 'segment.cellpose'))

    backend = _BACKENDS.get(fun_name)
    if backend is None:
        raise ValueError(f'no preview backend for {fun_name!r}; '
                         f'known: {sorted(_BACKENDS)}')

    levels, dim_utils = STATE.image(im_path)
    axis_len = _axis_lengths(dim_utils)
    bounds, fallback2d = slice_utils.preview_region_bounds(
        region.get('xy') or {}, region.get('z'), region.get('t'),
        axis_len, ndisplay=int(region.get('ndisplay', 2)))
    if 'X' not in bounds or 'Y' not in bounds:
        raise ValueError(f'empty preview region: {region.get("xy")!r}')

    ctx = PreviewContext(im_path, task_dir, value_name, params, levels, dim_utils,
                         axis_len, bounds, fallback2d,
                         given_names=msg.get('channelNames'))
    out = backend(ctx)
    out.setdefault('hasSignal', True)
    out.setdefault('noSignalWhy', '')
    out['region'] = {ax: list(v) for ax, v in bounds.items()}
    out['fallback2d'] = fallback2d
    out['valueName'] = value_name
    out['funName'] = fun_name
    return out


def execute_command(msg):
    kind = msg.get("type", "")
    if kind == "ping":
        return {"type": "ok", "protocol": PROTOCOL, "backends": sorted(_BACKENDS)}
    if kind == "preview":
        return {"type": "ok", **preview(msg)}
    if kind == "cleanup":
        # Toggle-off / stop path — the API calls this before killing the worker so a subsequent
        # session doesn't inherit a stale scratch store on the slab route. Both label previews AND
        # AF image previews get swept; both are keyed off `{task_dir}`.
        task_dir = str(msg.get("taskDir") or "")
        _sweep_preview_labels(task_dir)
        _sweep_preview_af(task_dir)
        _PREVIEW_TASK_DIRS.discard(task_dir)
        return {"type": "ok"}
    raise ValueError(f"unknown command: {kind!r}")


async def handle(ws):
    async for raw in ws:
        try:
            reply = execute_command(json.loads(raw))
        except Exception as e:                       # never let one bad request kill the worker
            traceback.print_exc()
            reply = {"type": "error", "msg": f"{type(e).__name__}: {e}"}
        await ws.send(json.dumps(reply))


async def main():
    import websockets
    async with websockets.serve(handle, HOST, PORT, max_size=WS_MAX_SIZE):
        print(f"preview worker ready on ws://{HOST}:{PORT}", flush=True)
        await asyncio.Future()


if __name__ == "__main__":
    asyncio.run(main())
