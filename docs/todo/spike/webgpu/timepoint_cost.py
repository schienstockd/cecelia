"""What does ONE timepoint actually cost to get from disk into a renderable array?

This exists because G1 turned up a number that contradicts CLOUD_MIGRATION_ASSESSMENT section 3b.
That section measured local NVMe at 0.008 ms/chunk buffered and concluded "1 timepoint (4 ch, 1620
chunks) = 0.013 s". Reading one real timepoint through zarr took **1.04 s** - ~80x that. Both can be
true: 0.008 ms/chunk is the cost of getting 20 KB of COMPRESSED bytes off the filesystem, and it says
nothing about decompressing them. So the split has to be measured, not attributed.

It matters to all three renderer options, not just the browser:
  A (napari remoted) pays it server-side, today, on every t-slider step.
  B (browser) pays the same decode in WASM, plus HTTP per chunk.
  C (Julia raycast) pays it server-side in Julia's blosc.

Stages timed separately, on the real store:
  1. raw   - read the chunk FILES as bytes, no decode (this is what 3b measured)
  2. decode- blosc-decompress those same bytes to arrays, nothing else
  3. zarr  - the real path: zarr fancy-index a whole timepoint into numpy
  4. f16   - uint16 -> float16 conversion, which G0 showed a filterable WebGPU texture needs
"""
import argparse, json, os, sys, time

import numpy as np


def stats(v):
    v = sorted(v)
    return {'n': len(v), 'median_ms': round(1000 * v[len(v) // 2], 2),
            'min_ms': round(1000 * v[0], 2), 'max_ms': round(1000 * v[-1], 2)}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--project', default='zolIMa')
    ap.add_argument('--uid', default='VJy1Nx')
    ap.add_argument('--version', default='ccidSmoothed.ome.zarr')
    ap.add_argument('--projects-dir', default='~/cecelia-feijoa/projects')
    ap.add_argument('--channels', type=int, default=4)
    ap.add_argument('--reps', type=int, default=5)
    ap.add_argument('--out', default='')
    a = ap.parse_args()

    repo = os.environ.get('CCIA_REPO') or os.getcwd()
    sys.path.insert(0, os.path.join(repo, 'python'))
    from cecelia.utils import zarr_utils
    import numcodecs

    zpath = os.path.join(os.path.expanduser(a.projects_dir), a.project, '0', a.uid, a.version)
    arrs, _ = zarr_utils.open_as_zarr(zpath, as_dask=False)
    arr = arrs[0]
    nt, nc_all, nz, ny, nx = arr.shape
    nc = min(nc_all, a.channels)

    lvl = os.path.join(zpath, '0')
    meta = json.load(open(os.path.join(lvl, '.zarray'), encoding='utf-8'))
    cy = int(np.ceil(ny / meta['chunks'][3]))
    cx = int(np.ceil(nx / meta['chunks'][4]))
    codec = numcodecs.get_codec(meta['compressor'])

    R = {'zarr': zpath, 'shape': list(arr.shape), 'channels_used': nc,
         'chunks': meta['chunks'], 'compressor': meta['compressor'],
         'chunks_per_timepoint': nz * cy * cx * nc,
         'uncompressed_MB_per_timepoint': round(nc * nz * ny * nx * 2 / 1e6, 1)}

    def keys(t):
        return [os.path.join(lvl, '%d.%d.%d.%d.%d' % (t, c, z, iy, ix))
                for c in range(nc) for z in range(nz)
                for iy in range(cy) for ix in range(cx)]

    # 1. raw bytes off the filesystem
    raw, nbytes = [], 0
    for t in range(1, 1 + a.reps):
        ks = keys(t)
        t0 = time.perf_counter()
        blobs = [open(k, 'rb').read() for k in ks if os.path.exists(k)]
        raw.append(time.perf_counter() - t0)
        nbytes = sum(len(b) for b in blobs)
    R['stage1_raw_bytes'] = stats(raw)
    R['compressed_MB_per_timepoint'] = round(nbytes / 1e6, 2)
    R['n_chunk_files_found'] = len(blobs)
    R['us_per_chunk_raw'] = round(1e6 * sorted(raw)[len(raw) // 2] / max(1, len(blobs)), 1)

    # 2. decode only, on bytes already in RAM
    dec = []
    for _ in range(a.reps):
        t0 = time.perf_counter()
        for b in blobs:
            codec.decode(b)
        dec.append(time.perf_counter() - t0)
    R['stage2_decode_only'] = stats(dec)
    R['us_per_chunk_decode'] = round(1e6 * sorted(dec)[len(dec) // 2] / max(1, len(blobs)), 1)

    # 3. the real path
    zr = []
    for t in range(1, 1 + a.reps):
        t0 = time.perf_counter()
        vol = np.asarray(arr[t, :nc])
        zr.append(time.perf_counter() - t0)
    R['stage3_zarr_timepoint'] = stats(zr)

    # 4. uint16 -> float16 (what a filterable WebGPU 3D texture needs)
    conv = []
    for _ in range(a.reps):
        t0 = time.perf_counter()
        vol.astype(np.float16)
        conv.append(time.perf_counter() - t0)
    R['stage4_uint16_to_f16'] = stats(conv)

    # 5. manual assembly: read + decode + place into a preallocated array. If this is close to
    # stage1+stage2 then stage3's excess is zarr's own per-chunk indexing overhead, not work.
    shp = (nc, nz, ny, nx)
    cyz, cxz = meta['chunks'][3], meta['chunks'][4]
    man = []
    for t in range(1, 1 + a.reps):
        t0 = time.perf_counter()
        out = np.empty(shp, dtype=arr.dtype)
        for c in range(nc):
            for z in range(nz):
                for iy in range(cy):
                    for ix in range(cx):
                        k = os.path.join(lvl, '%d.%d.%d.%d.%d' % (t, c, z, iy, ix))
                        y0, x0 = iy * cyz, ix * cxz
                        y1, x1 = min(y0 + cyz, ny), min(x0 + cxz, nx)
                        if not os.path.exists(k):
                            out[c, z, y0:y1, x0:x1] = meta['fill_value'] or 0
                            continue
                        buf = np.frombuffer(codec.decode(open(k, 'rb').read()),
                                            dtype=arr.dtype).reshape(cyz, cxz)
                        out[c, z, y0:y1, x0:x1] = buf[:y1 - y0, :x1 - x0]
        man.append(time.perf_counter() - t0)
    R['stage5_manual_assembly'] = stats(man)

    print('RESULT ' + json.dumps(R), flush=True)
    if a.out:
        json.dump(R, open(a.out, 'w', encoding='utf-8'), indent=1)


if __name__ == '__main__':
    main()
