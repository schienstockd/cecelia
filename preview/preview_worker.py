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
from cecelia.utils.cellpose_utils import CellposeUtils
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.segmentation_utils import count_labels

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
PROTOCOL = 4

#: Named in the error a channel NAME raises, so the message points at the Julia function that should
#: have resolved it — see `script_utils.channel_indices`.
_AF_TRANSLATOR = 'af_combinations_for_python (af_correct.jl)'
_CELLPOSE_TRANSLATOR = 'cellpose_models_for_python (cellpose.jl)'

HOST = "127.0.0.1"
PORT = int(os.environ.get("CECELIA_PREVIEW_PORT", "7656"))
_AXES = ("X", "Y", "Z", "T")


class PreviewState:
    """Everything worth keeping between previews. Opening the image and reading its OME-XML is the
    other per-invocation cost a resident process removes."""

    def __init__(self):
        self._images = {}        # im_path → (dask levels, dim_utils)
        self._images_zarr = {}   # im_path → plain zarr levels (see image_zarr)
        self._model_cache = {}   # shared with each CellposeUtils instance (see `segmenter`)
        self._norm = {}          # (im_path, channels, normalise) → cellpose norm params
        self._af = {}            # (im_path, channel, af channels, method) → AF stats

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

    def af_stats(self, im_path, levels, dim_utils, channel_idx, competing_channels, method):
        """The correction's global values — one background level per participating channel — cached.

        The AF analogue of `norm_params`, and the same bargain: deriving these costs a full pass over
        the movie (tens of seconds), against **5-7 ms** to correct one visible plane with them. So the
        first preview of an image is slow and every one after it is instant.

        The key is what the values actually depend on: the image, the participating channels, and the
        background method. Nothing else — so switching the method correctly misses the cache, and moving
        the view correctly hits it.

        `AF_PREVIEW_STRIDE` is the one concession to the cold start, and it is safe because every value
        here is now an interior threshold over a histogram, which subsampling does not move. (It was
        safe before for a more delicate reason — the ceiling was a COUNT-thresholded max rather than a
        true max — and that reason is gone along with the ceiling.)
        """
        competing = script_utils.channel_indices(
            competing_channels, f'competingChannels for channel {channel_idx}', _AF_TRANSLATOR)
        channels = script_utils.channel_indices(
            [channel_idx], 'the target channel', _AF_TRANSLATOR) + competing
        key = (im_path, int(channel_idx), tuple(sorted(competing)), method)
        if key not in self._af:
            self._af[key] = correction_utils.af_weight_stats(
                self.image_zarr(im_path)[0], dim_utils, channels,
                background_method=method, spatial_stride=AF_PREVIEW_STRIDE)
        return self._af[key]


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
    retunes parameters against a region that could never produce a mask (see TODO #00090, which this
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
                 'axis_len', 'bounds', 'fallback2d')

    def __init__(self, im_path, task_dir, value_name, params, levels, dim_utils,
                 axis_len, bounds, fallback2d):
        self.im_path, self.task_dir, self.value_name = im_path, task_dir, value_name
        self.params, self.levels, self.dim_utils = params, levels, dim_utils
        self.axis_len, self.bounds, self.fallback2d = axis_len, bounds, fallback2d

    def crop(self):
        """The visible region of the image, all channels, as `[C, Y, X]`."""
        sl = slice_utils.crop_slice_tuple(
            self.levels[0].ndim, _axis_indices(self.dim_utils), self.bounds)
        return _as_cyx(zarr_utils.fortify(self.levels[0][sl]), self.dim_utils)

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
        """Channel names from the OME-XML, so a layer can be named after the channel it corrects."""
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
        stats = STATE.af_stats(ctx.im_path, ctx.levels, ctx.dim_utils, ch, competing, method)
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
_BACKENDS = {
    'segment.cellpose': _preview_cellpose,
    'segment.cellposeMeasure': _preview_cellpose,
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
                         axis_len, bounds, fallback2d)
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
    async with websockets.serve(handle, HOST, PORT):
        print(f"preview worker ready on ws://{HOST}:{PORT}", flush=True)
        await asyncio.Future()


if __name__ == "__main__":
    asyncio.run(main())
