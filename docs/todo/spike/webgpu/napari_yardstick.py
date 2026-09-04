"""G1 - Viewer's real interactive frame cost on REAL data, in a VISIBLE window.

Why a visible window: three headless designs were tried and all three produced invalid numbers
(see docs/archive/napari-webgpu-audit.md -> G1). The root cause is that with `show=False` there is no
real paint, so every timing measures Qt plumbing - the same failure
`docs/todo/CLOUD_MIGRATION_ASSESSMENT.md` section 2 recorded and declined to publish. A shown
QWidget can be painted SYNCHRONOUSLY via `repaint()`, which is the one primitive that makes this
measurable. Dominik approved a brief window, 2026-08-24.

Timing: set camera -> `native.repaint()` (synchronous paint) -> `glFinish()` (GPU drained). No
framebuffer readback anywhere in the timed region.

VALIDITY GATES - if any fails the numbers are void and must not be quoted:
  1. frames non-blank and varying (proves the scene actually drew)
  2. 3D MIP must cost materially more than the same scene with every layer hidden
  3. `--control` run with NO PRIME env lands on llvmpipe and must be MUCH slower. Hardware and
     software landing within ~5% is the exact tell that voided the previous numbers.

Run:
  CCIA_REPO=<repo> DISPLAY=:1 __NV_PRIME_RENDER_OFFLOAD=1 __GLX_VENDOR_LIBRARY_NAME=nvidia \
    pixi run python napari_yardstick.py --uid VJy1Nx --out g1_nvidia.json
  # control (no PRIME env -> llvmpipe):
  CCIA_REPO=<repo> DISPLAY=:1 pixi run python napari_yardstick.py --uid VJy1Nx --out g1_llvmpipe.json
"""
import argparse, json, os, sys, time

import numpy as np


def log(m):
    print('[stage] ' + m, flush=True)


def stats(v):
    v = sorted(v)
    return {'n': len(v), 'ms_median': round(v[len(v) // 2], 2),
            'ms_min': round(v[0], 2), 'ms_max': round(v[-1], 2),
            'fps_median': round(1000.0 / v[len(v) // 2], 1) if v[len(v) // 2] > 0 else None}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--project', default='zolIMa')
    ap.add_argument('--uid', default='VJy1Nx')
    ap.add_argument('--version', default='ccidSmoothed.ome.zarr')
    ap.add_argument('--projects-dir', default='~/cecelia-feijoa/projects')
    ap.add_argument('--channels', type=int, default=4)
    ap.add_argument('--size', default='1920x1080')
    ap.add_argument('--reps', type=int, default=12)
    ap.add_argument('--out', default='')
    a = ap.parse_args()

    log('start')
    repo = os.environ.get('CCIA_REPO') or os.getcwd()
    sys.path.insert(0, os.path.join(repo, 'python'))
    from cecelia.utils import zarr_utils
    from qtpy.QtWidgets import QApplication
    QApplication.instance() or QApplication([])
    import napari
    from vispy.gloo import gl

    zpath = os.path.join(os.path.expanduser(a.projects_dir), a.project, '0', a.uid, a.version)
    arrs, _ = zarr_utils.open_as_zarr(zpath, as_dask=False)
    arr = arrs[0]
    W, H = (int(x) for x in a.size.split('x'))
    nt, nc_all, nz, ny, nx = arr.shape
    nc = min(nc_all, a.channels)
    R = {'zarr': zpath, 'shape': list(arr.shape), 'channels_used': nc,
         'requested_size': [W, H], 'prime_env': bool(os.environ.get('__NV_PRIME_RENDER_OFFLOAD'))}

    log('reading t0')
    vol = np.asarray(arr[0, :nc])
    R['resident_MB'] = round(vol.nbytes / 1e6, 1)
    R['voxels_per_channel'] = int(nz * ny * nx)

    log('viewer (a window will appear)')
    v = napari.Viewer(show=True)
    v.window._qt_window.resize(W, H)
    scale = (2.0, 0.3315, 0.3315)                      # z,y,x um - real, from .zattrs
    cmaps = ['red', 'green', 'blue', 'magenta']
    layers = [v.add_image(vol[c], name='ch%d' % c, scale=scale, rendering='mip',
                          colormap=cmaps[c % 4], blending='additive') for c in range(nc)]
    canvas = v.window._qt_viewer.canvas
    native = getattr(canvas, 'native', canvas)
    QApplication.processEvents()
    time.sleep(1.0)                                    # let the WM map and size the window
    QApplication.processEvents()
    R['canvas_px'] = [native.width(), native.height()]

    def frame():
        """One synchronous frame. repaint() paints NOW; glFinish drains the GPU."""
        native.repaint()
        gl.glFinish()

    def sweep(prep, reps, grab_std=False):
        prep(0)
        frame()
        ms, stds = [], []
        for i in range(reps):
            prep(i + 1)
            t0 = time.perf_counter()
            frame()
            ms.append(1000 * (time.perf_counter() - t0))
            if grab_std:
                im = np.asarray(v.screenshot(canvas_only=True, flash=False))
                stds.append(round(float(im[..., :3].std()), 2))
        out = stats(ms)
        if grab_std:
            out['stds'] = stds
            out['nonblank'] = all(s > 0.5 for s in stds)
            out['varies'] = len(set(stds)) >= 3
        return out

    def rot(i):
        v.camera.angles = (0, (i * 15) % 360, 0)

    v.dims.ndisplay = 3
    v.reset_view()
    frame()

    log('measure 3d rotate')
    R['rotate_3d'] = sweep(rot, a.reps, grab_std=True)

    log('measure hidden baseline')
    for l in layers:
        l.visible = False
    R['baseline_hidden'] = sweep(rot, a.reps)
    for l in layers:
        l.visible = True

    log('measure 3d, single channel')
    for l in layers[1:]:
        l.visible = False
    R['rotate_3d_1ch'] = sweep(rot, a.reps)
    for l in layers:
        l.visible = True

    log('measure 2d')
    v.dims.ndisplay = 2
    v.reset_view()
    frame()
    R['pan_2d'] = sweep(lambda i: setattr(v.camera, 'zoom', v.camera.zoom * 1.02), a.reps)

    b = R['baseline_hidden']['ms_median']
    for k in ('rotate_3d', 'rotate_3d_1ch', 'pan_2d'):
        net = R[k]['ms_median'] - b
        R[k]['net_ms'] = round(net, 2)
        R[k]['net_fps'] = round(1000.0 / net, 1) if net > 0.05 else None

    log('measure t-scrub (3d, includes IO)')
    v.dims.ndisplay = 3
    v.reset_view()
    frame()
    scrub = []
    for t in range(1, 6):
        t0 = time.perf_counter()
        sub = np.asarray(arr[t, :nc])
        for c in range(nc):
            layers[c].data = sub[c]
        frame()
        scrub.append(1000 * (time.perf_counter() - t0))
    R['scrub_3d_with_io'] = stats(scrub)

    # LAST: _gl_info() builds its own QOffscreenSurface + GL context and segfaults this process if
    # called before the legacy viewer's canvas exists (reproduced 3x).
    sys.path.insert(0, os.path.join(repo, 'napari'))
    import napari_bridge
    R['gl'] = napari_bridge._gl_info()
    R['hardware_gl'] = 'NVIDIA' in str(R['gl'])
    R['valid_gates'] = {
        'nonblank': R['rotate_3d'].get('nonblank'),
        'varies': R['rotate_3d'].get('varies'),
        'net_positive': R['rotate_3d']['net_ms'] > 0,
    }
    R['valid'] = all(bool(x) for x in R['valid_gates'].values())

    print('RESULT ' + json.dumps(R), flush=True)
    if a.out:
        json.dump(R, open(a.out, 'w', encoding='utf-8'), indent=1)
    v.close()


if __name__ == '__main__':
    main()
