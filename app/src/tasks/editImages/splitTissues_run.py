"""
Split-into-tissues task runner — two modes, called in turn by `splitTissues.jl`.

``mode = "detect"``: find the tissues on a low-resolution level (`tissue_split_utils.detect_tissues`,
the derivation the task preview runs too) and write the plan — the tissues in reading order with their
full-resolution boxes — to ``planPath`` (JSON), and the detection labels beside it (``.npy``), so the
write pass cuts exactly what was detected.

``mode = "write"``: for each ``outputs`` entry ``{label, imOutPath}``, write the tissue's box as a NEW
OME-ZARR with every pixel outside the tissue's mask set to 0. Streamed per timepoint, channel and
band of rows, so a 60-channel slide never sits in RAM; the pyramid is built from the written level 0.

Parameter contract (JSON written by Julia):
  mode            - "detect" | "write"
  imPath          - absolute path to the source .ome.zarr
  planPath        - the plan JSON (written by detect, read by write); labels at `<planPath>.labels.npy`
  channelIndices  - detect: 0-based channels that mark tissue; empty = all
  thresholdAdjust, splitNeck, mergeGapUm, minAreaMm2, marginUm - detect: `SplitParams`
  outputs         - write: [{label, imOutPath}]
"""
import copy
import json

import numpy as np

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.script_utils as script_utils
import cecelia.utils.tissue_split_utils as tissue_split_utils
from cecelia.utils.atomic_io import atomic_path, write_json_atomic
from cecelia.utils.dim_utils import DimUtils

#: Rows per write band — a whole number of 512-px chunks; bounds RAM to band × box width per channel.
BAND_ROWS = 2048


def _open(im_path):
    levels, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
    dim_utils = DimUtils(ome_xml_utils.parse_meta(im_path), use_channel_axis=True)
    dim_utils.calc_image_dimensions(levels[0].shape, axes=zarr_utils.read_axes(im_path))
    return levels, dim_utils


def _px_um_x(im_path):
    scale = zarr_utils.read_scale_um(im_path)
    axes = [str(a).upper() for a in (zarr_utils.read_axes(im_path) or [])]
    if not scale or 'X' not in axes or not float(scale[axes.index('X')]) > 0:
        raise ValueError('pixel size not set — set it in the image metadata so areas and gaps mean µm')
    return float(scale[axes.index('X')])


def detect(params, log):
    im_path = params['imPath']
    levels, dim_utils = _open(im_path)
    chs = params.get('channelIndices') or None
    det = tissue_split_utils.detect_tissues(
        levels, dim_utils, _px_um_x(im_path), tissue_split_utils.SplitParams.from_dict(params),
        channels=[int(c) for c in chs] if chs else None)
    log.log(f'>> detection level {det.level} ({det.px_um:.1f} µm/px): {len(det.tissues)} tissue(s)')
    labels_path = params['planPath'] + '.labels.npy'
    with atomic_path(labels_path) as tmp:
        with open(tmp, 'wb') as f:
            np.save(f, det.labels, allow_pickle=False)
    write_json_atomic(params['planPath'], {
        'level': det.level, 'factorYX': list(det.factor_yx), 'fullYX': list(det.full_yx),
        'pxUm': det.px_um, 'largestShare': det.largest_share(),
        'tissues': [t.as_dict() for t in det.tissues]})


def _write_one(levels, dim_utils, im_path, labels, factor, tissue, out_path, log, tick):
    level_in = levels[0]
    axis = {ax: dim_utils.dim_idx(ax) for ax in ('X', 'Y', 'Z', 'T', 'C')}
    y0, y1, x0, x1 = tissue['y0'], tissue['y1'], tissue['x0'], tissue['x1']
    out_shape = list(level_in.shape)
    out_shape[axis['Y']], out_shape[axis['X']] = y1 - y0, x1 - x0
    out_dims = copy.deepcopy(dim_utils)
    out_dims.im_dim[axis['Y']], out_dims.im_dim[axis['X']] = y1 - y0, x1 - x0

    with zarr_utils.staged_store(out_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, tuple(out_shape), level_in.dtype, out_dims, nscales=len(levels),
            reference_zarr=im_path)
        n_t = level_in.shape[axis['T']] if axis['T'] is not None else 1
        n_c = level_in.shape[axis['C']] if axis['C'] is not None else 1
        xs = np.arange(x0, x1)
        for b0 in range(0, y1 - y0, BAND_ROWS):
            b1 = min(y1 - y0, b0 + BAND_ROWS)
            keep = tissue_split_utils.labels_block(
                labels, factor, np.arange(y0 + b0, y0 + b1), xs) == tissue['label']
            mask_shape = [1] * len(out_shape)
            mask_shape[axis['Y']], mask_shape[axis['X']] = keep.shape
            keep = keep.reshape(mask_shape)
            for t in range(n_t):
                for c in range(n_c):
                    src = [slice(None)] * len(out_shape)
                    dst = [slice(None)] * len(out_shape)
                    src[axis['Y']], src[axis['X']] = slice(y0 + b0, y0 + b1), slice(x0, x1)
                    dst[axis['Y']] = slice(b0, b1)
                    for ax, i in (('T', t), ('C', c)):
                        if axis[ax] is not None:
                            src[axis[ax]] = dst[axis[ax]] = slice(i, i + 1)
                    block = np.asarray(level_in[tuple(src)])
                    level0[tuple(dst)] = np.where(keep, block, 0).astype(block.dtype, copy=False)
            tick()
        zarr_utils.write_multiscale_pyramid(group, level0, out_dims, len(levels), list(pchunks))
        ome_xml_utils.save_meta_in_zarr(staging, im_path, changed_shape=tuple(out_shape),
                                        dim_utils=out_dims)
        zarr_utils.write_calibration(staging, out_dims)
        # VALID-BOX-EXEMPT: like a crop, the tissue image moves the coordinates; no box = all valid.
    log.log(f'>> tissue {tissue["label"]} → {out_path}')


def write(params, log):
    im_path = params['imPath']
    levels, dim_utils = _open(im_path)
    with open(params['planPath'], encoding='utf-8') as f:
        plan = json.load(f)
    labels = np.load(params['planPath'] + '.labels.npy', allow_pickle=False)
    factor = tuple(plan['factorYX'])
    by_label = {int(t['label']): t for t in plan['tissues']}
    outputs = params['outputs']
    bands = [-(-(by_label[int(o['label'])]['y1'] - by_label[int(o['label'])]['y0']) // BAND_ROWS)
             for o in outputs]
    total, done = sum(bands), 0

    def tick():
        nonlocal done
        done += 1
        log.progress(done, total)

    log.progress(0, total)
    for o in outputs:
        _write_one(levels, dim_utils, im_path, labels, factor, by_label[int(o['label'])],
                   o['imOutPath'], log, tick)


def run(params):
    log = script_utils.get_logfile_utils(params)
    mode = params.get('mode')
    if mode == 'detect':
        detect(params, log)
    elif mode == 'write':
        write(params, log)
    else:
        raise ValueError(f'unknown mode {mode!r}; expected "detect" or "write"')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
