"""Resident preview worker — runs a task's real compute over one visible region, on demand.

A runtime process, like `napari/napari_bridge.py` and `mcp/` (not part of the `cecelia` IO library).
It exists for one measured reason: the fixed cost of a Python process that can segment is **17.7 s**
(11.7 s `import cecelia.utils` + 5.7 s `import cellpose` + 0.2 s model construction), which is fatal
per preview and irrelevant once. Staying resident pays it at toggle-on. Model construction is cheap
enough that warm *models* are a minor bonus, not the point — see
docs/todo/TASK_PREVIEW_PLAN.md (Decision 8).

What it does NOT do:

* **No second cellpose implementation.** It calls `CellposeUtils.predict_slice`, the same method the
  full run uses, so a preview cannot drift from the thing it is previewing.
* **No 3D.** One z-plane, always. A visible z-stack costs ~90 s with no shortcut available
  (downsampling doesn't help — cellpose rescales to a canonical diameter, so cost tracks CELLS, not
  pixels), and that is not a preview. In 3D display mode it previews the current plane and reports
  `fallback2d` so the caller can say so.
* **Nothing on disk.** The mask block is RETURNED (`cecelia.utils.block_transfer`), not written. An
  earlier design wrote a never-promoted scratch store and let the bridge open it; that put throwaway
  bytes in the user's project tree, needed its own staging + sweep lifecycle, and left debris to
  accumulate whenever a preview didn't finish cleanly. A preview is a picture, not data.

Protocol: one JSON message per connection, same shape as the napari bridge.
    {"type": "ping"}     -> {"type": "ok", "protocol": PROTOCOL}
    {"type": "preview", ...} -> {"type": "ok", "layers": [{kind, name, block, shape, axes}, …],
                                 "region": …, "fallback2d": bool, plus per-task fields}

`PROTOCOL` exists because a running worker is ADOPTED, not relaunched, when the backend restarts — that
is deliberate (a warm worker survives a Revise restart, which is most of its value) but it means stale
worker code otherwise outlives every restart. It presented as a bare "Preview failed": a worker from
before the AF backend existed ignored `funName`, fell through to the segmentation path, and raised
"no models in preview params". Bump this whenever the reply shape or the backend set changes.
"""
import asyncio
import base64
import json
import os
import traceback

import numpy as np

import cecelia.utils.correction_utils as correction_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.slice_utils as slice_utils
import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.block_transfer import encode_block
import cecelia.utils.script_utils as script_utils
from cecelia.utils.dim_utils import DimUtils

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
PROTOCOL = 12

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


def _layer(kind, name, block, axes, full_shape, source=None):
    """One layer for the viewer to build. `kind` decides Labels vs Image on the receiving end.

    `source` names the viewer layer this one is DERIVED from — the channel being corrected. The bridge
    mirrors that layer's colormap so the corrected channel renders in the same colour as its original,
    which is the whole point of putting them side by side: a grey copy of a magenta channel is hard to
    compare against, and the comparison IS the judgement. Sent explicitly rather than parsed back out
    of `name` ("CH1 AF" → "CH1"), because a channel name containing the suffix would break that guess.
    """
    out = {'kind': kind, 'name': name, 'block': encode_block(block),
           'shape': [int(x) for x in full_shape], 'axes': list(axes)}
    if source:
        out['source'] = str(source)
    return out


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

    counts, block = {}, None
    for key in sorted(models.keys()):
        model_params = models[key]
        match_as = str(model_params.get('matchAs', 'base'))
        if match_as != 'base':
            continue            # one type per preview: it is the primary you are judging
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
        block = np.reshape(np.asarray(masks, dtype=seg.LABEL_DTYPE), block_shape)
        _, count_labels = _cellpose_imports()
        counts[match_as] = count_labels(masks)

    if block is None:
        raise ValueError('no base model in preview params')

    has_signal, why = _region_signal(ctx.im_path, ctx.bounds, tile)
    return {
        'counts': counts,
        'hasSignal': has_signal,
        'noSignalWhy': why,
        # tile seams the RUN would place inside this region, which the preview does not reproduce
        'runSeams': _run_tile_seams(ctx.bounds, ctx.axis_len, seg.block_size),
        'blockSize': int(seg.block_size),
        'layers': [_layer('labels', 'Preview', block, axes, full_shape)],
    }


def _coastal_imports():
    """Deferred like `_cellpose_imports`, and for the same reason: torch + coastal cost a session
    that never previews a flow model nothing."""
    from cecelia.utils.coastal_utils import CoastalUtils
    from cecelia.utils.segmentation_utils import count_labels
    return CoastalUtils, count_labels


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

    CoastalUtils, count_labels = _coastal_imports()
    seg = CoastalUtils(
        {**ctx.params, 'taskDir': ctx.task_dir, 'outputValueName': ctx.value_name}, ctx.dim_utils)

    axes, full_shape, block_shape = ctx.block_geometry()
    context, centre = _temporal_window(ctx, seg.TEMPORAL_RADIUS)
    tile = context[centre]

    counts, block = {}, None
    for key in sorted(models.keys()):
        model_params = models[key]
        match_as = str(model_params.get('matchAs', 'base'))
        if match_as != 'base':
            continue            # one type per preview: it is the primary you are judging
        norm_params = STATE.norm_params(seg, ctx.levels, ctx.im_path, model_params)
        masks = seg.predict_slice(tile, model_params, norm_params,
                                  context=context, context_index=centre)
        masks = seg.post_process(masks, ['Y', 'X'], None, 1, False,
                                 real_border=_real_image_edges(ctx.bounds, ctx.axis_len))
        block = np.reshape(np.asarray(masks, dtype=seg.LABEL_DTYPE), block_shape)
        counts[match_as] = count_labels(masks)

    if block is None:
        raise ValueError('no base model in preview params')

    has_signal, why = _region_signal(ctx.im_path, ctx.bounds, tile)
    return {
        'counts': counts,
        'hasSignal': has_signal,
        'noSignalWhy': why,
        'runSeams': _run_tile_seams(ctx.bounds, ctx.axis_len, seg.block_size),
        'blockSize': int(seg.block_size),
        'layers': [_layer('labels', 'Preview', block, axes, full_shape)],
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
    from cecelia.utils.coastal_utils import temporal_config

    models = ctx.params.get('models') or {}
    if not models:
        raise ValueError('no models in preview params')

    CoastalUtils, _ = _coastal_imports()
    seg = CoastalUtils(
        {**ctx.params, 'taskDir': ctx.task_dir, 'outputValueName': ctx.value_name}, ctx.dim_utils)
    mp = models[sorted(models.keys())[0]]
    scales, cumulative, dropped = temporal_config(seg._manifest(mp))

    context, centre = _temporal_window(ctx, seg.TEMPORAL_RADIUS)

    window = seg._project_window(context, mp, STATE.norm_params(seg, ctx.levels, ctx.im_path, mp))
    frame, metrics = seg._flow_metrics(window, centre, scales, cumulative)
    if dropped:
        # The model's OWN dropped set, from its manifest. Keeping a plane the model was not trained
        # on would show a sheet that does not match the channels it is fed.
        metrics = {k: v for k, v in metrics.items() if k not in dropped}
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
    """AF-correct the visible region, one Image layer per corrected channel.

    The whole reason this is possible in a fraction of a second: `af_weight_stats` derives one
    background level per participating channel over the WHOLE image — tens of seconds on a 181-frame
    movie — and they are cached here, while `af_correct_frame` is pure per-voxel arithmetic on the crop.
    Those globals are exactly what must NOT come from the visible region: derive them from a crop and
    the preview subtracts a different background than the run, which is the one thing it exists to
    rule out.

    Outputs an IMAGE, not labels, so the reply carries `kind: 'image'` per layer and the receiver adds
    them beside the originals for A/B comparison.
    """
    combos = {int(k): v for k, v in (ctx.params.get('afCombinations') or {}).items()}
    if not combos:
        raise ValueError('no channel combinations in preview params')
    method = str(ctx.params.get('backgroundMethod', 'triangle'))

    tile = ctx.crop()                       # [C, Y, X]
    axes, full_shape, block_shape = ctx.block_geometry()
    names = ctx.channel_names()
    out_dtype = ctx.levels[0].dtype
    layers, stats_out = [], {}

    for ch in sorted(combos):
        # same coercion + diagnosis the run uses: a channel NAME here means the Julia translator did not
        # run, which is a stale-backend symptom rather than a bad parameter
        competing = script_utils.channel_indices(
            combos[ch].get('competingChannels'), f'competingChannels for channel {ch}',
            _AF_TRANSLATOR)
        if not competing:
            continue
        stats = STATE.af_stats(ctx.im_path, ctx.levels, ctx.dim_utils, ch, competing, method,
                               exclusive=bool(combos[ch].get('exclusive', True)))
        # every participating channel stays separate — each contributes its own term to the weight's
        # denominator, so there is nothing to collapse into a single reference image
        slabs = {c: tile[c] for c in [ch] + competing}
        corrected = correction_utils.af_correct_frame(slabs, ch, stats, out_dtype)
        # to the channel-less block shape (T/Z restored as length-1) so the receiver can place it at
        # `region` without knowing how the crop was flattened — same contract as the labels path
        block = np.reshape(corrected, block_shape)
        label = names[ch] if ch < len(names) else f'ch{ch}'
        layers.append(_layer('image', f'{label} AF', block, axes, full_shape, source=label))
        # same helper the run's QC reports through, so the readout and the banked metric cannot
        # disagree about a name or a value
        stats_out[str(ch)] = correction_utils.af_derived_values(stats, ch)

    if not layers:
        raise ValueError('no combination names a competing channel')

    has_signal, why = _region_signal(ctx.im_path, ctx.bounds, tile)
    return {'hasSignal': has_signal, 'noSignalWhy': why,
            'derived': stats_out, 'layers': layers}


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
