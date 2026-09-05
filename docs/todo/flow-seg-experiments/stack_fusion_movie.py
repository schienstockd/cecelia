"""Movie of Z-neighbour fusion methods on a small temporal crop of c91ICQ.

For each (t) in [t0..t1), each (input in {raw, stackAligned}), apply every method
along Z with a 3-plane window, then render one mp4 with methods side-by-side.

Only one XY z-slice is shown per panel — the fusion has already done its work
per plane. Motion is visible frame-to-frame across T.

Usage:
    pixi run python docs/todo/flow-seg-experiments/stack_fusion_movie.py \
        --t0 40 --t1 70 --c 0 --z 3
"""

from __future__ import annotations
import argparse
import subprocess
import tempfile
import time as _time
from pathlib import Path

import cv2
import matplotlib.pyplot as plt
import numpy as np

from cecelia.utils.zarr_utils import open_as_zarr
from coastal.smooth import temporal_gated


PROJECT_DIR = Path('/home/dominik/cecelia-feijoa/projects/d5vw7z/0/c91ICQ')
OUT_DIR = Path.home() / 'Downloads' / 'TMP'

INPUTS = {
    'raw': PROJECT_DIR / 'ccidImage.ome.zarr',
    'stackAligned': PROJECT_DIR / 'ccidStackAligned.ome.zarr',
}

FARNEBACK_KW = dict(
    pyr_scale=0.5, levels=3, winsize=11, iterations=3,
    poly_n=5, poly_sigma=1.2, flags=0,
)
FB_CONSISTENCY_PX = 1.0


def load_tc_range(path: Path, t0: int, t1: int, c: int) -> np.ndarray:
    """Return (T, Z, Y, X) float32."""
    levels = open_as_zarr(str(path), as_dask=False)
    arr = levels[0]
    if isinstance(arr, list):
        arr = arr[0]
    return np.asarray(arr[t0:t1, c]).astype(np.float32)


def _to_u8(plane: np.ndarray) -> np.ndarray:
    lo, hi = float(plane.min()), float(plane.max())
    if hi <= lo:
        return np.zeros_like(plane, dtype=np.uint8)
    return np.clip(((plane - lo) / (hi - lo) * 255.0), 0, 255).astype(np.uint8)


def _flow(prev: np.ndarray, nxt: np.ndarray) -> np.ndarray:
    return cv2.calcOpticalFlowFarneback(
        _to_u8(prev), _to_u8(nxt), None, **FARNEBACK_KW,
    )


def _warp_to_ref(neighbor: np.ndarray, flow_ref_to_nb: np.ndarray) -> np.ndarray:
    h, w = neighbor.shape
    yy, xx = np.mgrid[0:h, 0:w].astype(np.float32)
    return cv2.remap(
        neighbor, xx + flow_ref_to_nb[..., 0], yy + flow_ref_to_nb[..., 1],
        interpolation=cv2.INTER_LINEAR, borderMode=cv2.BORDER_REPLICATE,
    )


def _fb_confidence(flow_fwd: np.ndarray, flow_bwd: np.ndarray) -> np.ndarray:
    h, w = flow_fwd.shape[:2]
    yy, xx = np.mgrid[0:h, 0:w].astype(np.float32)
    px = xx + flow_fwd[..., 0]
    py = yy + flow_fwd[..., 1]
    bwd_x = cv2.remap(flow_bwd[..., 0], px, py, cv2.INTER_LINEAR,
                      borderMode=cv2.BORDER_REPLICATE)
    bwd_y = cv2.remap(flow_bwd[..., 1], px, py, cv2.INTER_LINEAR,
                      borderMode=cv2.BORDER_REPLICATE)
    return np.sqrt((flow_fwd[..., 0] + bwd_x) ** 2 +
                   (flow_fwd[..., 1] + bwd_y) ** 2)


def method_raw(stack): return stack.copy()


def method_mean_z3(stack):
    Z = stack.shape[0]; out = np.empty_like(stack)
    for z in range(Z):
        z0, z1 = max(0, z - 1), min(Z - 1, z + 1)
        out[z] = stack[z0:z1 + 1].mean(axis=0)
    return out


def method_median_z3(stack):
    Z = stack.shape[0]; out = np.empty_like(stack)
    for z in range(Z):
        z0, z1 = max(0, z - 1), min(Z - 1, z + 1)
        out[z] = np.median(stack[z0:z1 + 1], axis=0)
    return out


def method_gated_z3(stack):
    return temporal_gated(
        stack, frames=3, time_axis=0, search=1, patch=5, sigma=None, k=1.0,
    ).astype(np.float32)


def method_farneback_z3(stack):
    Z = stack.shape[0]; out = np.empty_like(stack)
    for z in range(Z):
        ref = stack[z]
        accum = ref.copy()
        weight = np.ones_like(ref, dtype=np.float32)
        for dz in (-1, 1):
            zn = z + dz
            if zn < 0 or zn >= Z:
                continue
            neighbor = stack[zn]
            flow_fwd = _flow(ref, neighbor)
            flow_bwd = _flow(neighbor, ref)
            err = _fb_confidence(flow_fwd, flow_bwd)
            trust = (err <= FB_CONSISTENCY_PX).astype(np.float32)
            accum += _warp_to_ref(neighbor, flow_fwd) * trust
            weight += trust
        out[z] = accum / weight
    return out


METHODS = {
    'raw':        method_raw,
    'mean_z3':    method_mean_z3,
    'median_z3':  method_median_z3,
    'gated_z3':   method_gated_z3,
    'farneback':  method_farneback_z3,
}


def render_movie(t0: int, t1: int, c: int, z: int, fps: int, out_name: str) -> None:
    print(f'loading T=[{t0}, {t1}) c={c} from both inputs …')
    stacks = {name: load_tc_range(path, t0, t1, c) for name, path in INPUTS.items()}
    T = stacks['raw'].shape[0]
    print(f'  shape per input: {stacks["raw"].shape}')

    print('applying methods to every (t) …')
    results: dict[str, dict[str, np.ndarray]] = {}
    timings: dict[str, dict[str, float]] = {}
    for input_name, tstack in stacks.items():
        results[input_name] = {}
        timings[input_name] = {}
        for m_name, fn in METHODS.items():
            t_start = _time.perf_counter()
            out = np.stack([fn(tstack[t]) for t in range(T)], axis=0)  # (T, Z, Y, X)
            timings[input_name][m_name] = _time.perf_counter() - t_start
            results[input_name][m_name] = out
            print(f'  {input_name:12s} {m_name:10s} '
                  f'{timings[input_name][m_name]:6.2f}s')

    # Common intensity range across ALL frames, methods, inputs, at the chosen z.
    all_slices = np.stack([
        results[i_name][m_name][:, z]
        for i_name in INPUTS for m_name in METHODS
    ])  # (n_panels, T, Y, X)
    vmin = float(np.percentile(all_slices, 1))
    vmax = float(np.percentile(all_slices, 99.5))
    print(f'intensity: vmin={vmin:.0f} vmax={vmax:.0f}')

    n_inputs = len(INPUTS); n_methods = len(METHODS)
    fig, axes = plt.subplots(
        n_inputs, n_methods,
        figsize=(2.8 * n_methods, 2.8 * n_inputs),
        gridspec_kw=dict(wspace=0.03, hspace=0.12),
    )
    axes = np.atleast_2d(axes)

    ims: dict[tuple, plt.AxesImage] = {}
    for i, input_name in enumerate(INPUTS):
        for j, m_name in enumerate(METHODS):
            ax = axes[i, j]
            im = ax.imshow(
                results[input_name][m_name][0, z],
                cmap='gray', vmin=vmin, vmax=vmax, interpolation='nearest',
            )
            ims[(i, j)] = im
            ax.set_xticks([]); ax.set_yticks([])
            secs = timings[input_name][m_name]
            title = f'{m_name}  ({secs:.1f}s/{T}f)' if i == 0 else f'{secs:.1f}s/{T}f'
            ax.set_title(title, fontsize=8)
            if j == 0:
                ax.set_ylabel(input_name, fontsize=9)

    title_txt = fig.suptitle(
        f'c91ICQ  T=[{t0}, {t1})  c={c}  z={z}  '
        f'|  frame 000/{T}',
        fontsize=11,
    )

    frames_dir = Path(tempfile.mkdtemp(prefix='stackfuse_frames_'))
    print(f'writing frames -> {frames_dir}')
    for t in range(T):
        for i, input_name in enumerate(INPUTS):
            for j, m_name in enumerate(METHODS):
                ims[(i, j)].set_data(results[input_name][m_name][t, z])
        title_txt.set_text(
            f'c91ICQ  T=[{t0}, {t1})  c={c}  z={z}  '
            f'|  frame {t:03d}/{T}  (actual t={t0 + t})'
        )
        fig.savefig(frames_dir / f'f_{t:04d}.png', dpi=110, bbox_inches='tight')
    plt.close(fig)

    out_path = OUT_DIR / out_name
    print(f'encoding -> {out_path}')
    subprocess.run(
        ['ffmpeg', '-y', '-framerate', str(fps),
         '-i', str(frames_dir / 'f_%04d.png'),
         '-c:v', 'libx264', '-pix_fmt', 'yuv420p',
         '-vf', 'pad=ceil(iw/2)*2:ceil(ih/2)*2',
         str(out_path)],
        check=True, capture_output=True,
    )
    for p in frames_dir.glob('*.png'):
        p.unlink()
    frames_dir.rmdir()
    print(f'wrote {out_path}')


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--t0', type=int, default=45)
    ap.add_argument('--t1', type=int, default=75)
    ap.add_argument('--c',  type=int, default=0)
    ap.add_argument('--z',  type=int, default=3)
    ap.add_argument('--fps', type=int, default=8)
    ap.add_argument('--out', type=str, default='stack_fusion_movie_c91ICQ.mp4')
    args = ap.parse_args()
    render_movie(args.t0, args.t1, args.c, args.z, args.fps, args.out)


if __name__ == '__main__':
    main()
