"""Movie of TEMPORAL fusion methods on a small temporal crop of c91ICQ.

Companion to stack_fusion_movie.py. Same 5 methods (raw, mean, median, coastal
temporal_gated, farneback warp+fuse) but fused along T instead of Z. Per-z
processing (each z-plane treated as a (T, Y, X) volume).

Why: on c91ICQ the intra-stack Z-fusion showed marginal gains — Z=6 planes at
wide physical spacing don't share enough signal for fusion to help. The visible
per-frame noise looks like a T-axis problem (shot noise per acquisition), which
is where coastal's temporal_gated was measured to win.

Usage:
    pixi run python docs/todo/flow-seg-experiments/temporal_fusion_movie.py \
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
    pyr_scale=0.5, levels=3, winsize=15, iterations=3,
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


def _to_u8(plane):
    lo, hi = float(plane.min()), float(plane.max())
    if hi <= lo:
        return np.zeros_like(plane, dtype=np.uint8)
    return np.clip(((plane - lo) / (hi - lo) * 255.0), 0, 255).astype(np.uint8)


def _flow(prev, nxt):
    return cv2.calcOpticalFlowFarneback(
        _to_u8(prev), _to_u8(nxt), None, **FARNEBACK_KW,
    )


def _warp_to_ref(neighbor, flow_ref_to_nb):
    h, w = neighbor.shape
    yy, xx = np.mgrid[0:h, 0:w].astype(np.float32)
    return cv2.remap(
        neighbor, xx + flow_ref_to_nb[..., 0], yy + flow_ref_to_nb[..., 1],
        interpolation=cv2.INTER_LINEAR, borderMode=cv2.BORDER_REPLICATE,
    )


def _fb_confidence(flow_fwd, flow_bwd):
    h, w = flow_fwd.shape[:2]
    yy, xx = np.mgrid[0:h, 0:w].astype(np.float32)
    px = xx + flow_fwd[..., 0]; py = yy + flow_fwd[..., 1]
    bwd_x = cv2.remap(flow_bwd[..., 0], px, py, cv2.INTER_LINEAR,
                      borderMode=cv2.BORDER_REPLICATE)
    bwd_y = cv2.remap(flow_bwd[..., 1], px, py, cv2.INTER_LINEAR,
                      borderMode=cv2.BORDER_REPLICATE)
    return np.sqrt((flow_fwd[..., 0] + bwd_x) ** 2 +
                   (flow_fwd[..., 1] + bwd_y) ** 2)


# All methods take a (T, Y, X) single-plane volume and return same shape.

def m_raw(vol): return vol.copy()


def m_mean_t3(vol):
    T = vol.shape[0]; out = np.empty_like(vol)
    for t in range(T):
        t0, t1 = max(0, t - 1), min(T - 1, t + 1)
        out[t] = vol[t0:t1 + 1].mean(axis=0)
    return out


def m_median_t3(vol):
    T = vol.shape[0]; out = np.empty_like(vol)
    for t in range(T):
        t0, t1 = max(0, t - 1), min(T - 1, t + 1)
        out[t] = np.median(vol[t0:t1 + 1], axis=0)
    return out


def m_gated_t3(vol):
    return temporal_gated(
        vol, frames=3, time_axis=0, search=1, patch=5, sigma=None, k=1.0,
    ).astype(np.float32)


def m_farneback_t3(vol):
    T = vol.shape[0]; out = np.empty_like(vol)
    for t in range(T):
        ref = vol[t]
        accum = ref.copy()
        weight = np.ones_like(ref, dtype=np.float32)
        for dt in (-1, 1):
            tn = t + dt
            if tn < 0 or tn >= T:
                continue
            neighbor = vol[tn]
            flow_fwd = _flow(ref, neighbor)
            flow_bwd = _flow(neighbor, ref)
            err = _fb_confidence(flow_fwd, flow_bwd)
            trust = (err <= FB_CONSISTENCY_PX).astype(np.float32)
            accum += _warp_to_ref(neighbor, flow_fwd) * trust
            weight += trust
        out[t] = accum / weight
    return out


_STRIPFIX_MAX_ABS_SHIFT_LOG: dict[int, float] = {}


# Drift trajectory for c91ICQ — loaded once, indexed by absolute frame t.
# shifts[t] = drift delta from frame t to frame t+1, order [dz, dy, dx].
_DRIFT_PATH = Path('/home/dominik/cecelia-feijoa/projects/d5vw7z/1/c91ICQ'
                   '/tasks/drift_shifts.json')


def _load_drift_shifts() -> np.ndarray:
    import json
    with open(_DRIFT_PATH) as f:
        d = json.load(f)
    # (T-1, 3) [dz, dy, dx] per transition
    return np.asarray(d['shifts'], dtype=np.float32)


_DRIFT_SHIFTS = _load_drift_shifts()  # cached at module import


def m_driftrow_absolute(vol, t_offset: int):
    """Per-row intra-frame drift correction using the driftCorrect trajectory.

    Physical model: row y of frame t is acquired at fractional time (t + y/H).
    Between frames t and t+1 the sample drifts by delta_t. Row y therefore
    captured the sample at (y/H) of the way through delta_t. Undoing that
    intra-frame residual around the frame centroid: shift row y by
        -(y/H - 0.5) * delta_t
    (Centered form — leaves the frame's overall centroid unchanged.)

    Uses the drift trajectory pre-computed by driftCorrect (no pattern matching,
    no template). Assumes acquisition ~= frame period (worst-case model).

    Args:
      vol: (T, Y, X) at a single (c, z), aligned in the crop's own [t0, t1)
      t_offset: absolute t of vol[0] — needed to index _DRIFT_SHIFTS
    """
    T, H, W = vol.shape
    out = np.empty_like(vol)
    max_shifts = []
    for t in range(T):
        t_abs = t_offset + t
        idx = min(t_abs, len(_DRIFT_SHIFTS) - 1)
        # [dz, dy, dx] → we only warp XY within the plane
        _, dy_delta, dx_delta = _DRIFT_SHIFTS[idx]

        # Per-row shift: r_frac = y/H - 0.5, warp = -r_frac * delta.
        # cv2.remap samples input at (map_x, map_y) for output pixel (y, x).
        # To shift a row DOWN/RIGHT by (+dy, +dx): sample from (y - dy, x - dx).
        yy = np.arange(H, dtype=np.float32)
        r_frac = (yy / max(H - 1, 1)) - 0.5
        row_dy = -r_frac * dy_delta   # sub-frame Y drift to remove
        row_dx = -r_frac * dx_delta

        xx = np.arange(W, dtype=np.float32)[None, :]
        map_x = xx - row_dx[:, None]
        map_y = yy[:, None] - row_dy[:, None] + np.zeros((1, W), dtype=np.float32)
        out[t] = cv2.remap(
            vol[t], map_x, map_y,
            interpolation=cv2.INTER_LINEAR, borderMode=cv2.BORDER_REPLICATE,
        )
        max_shifts.append(float(max(abs(dy_delta), abs(dx_delta))))
    print(f'    driftrow max-abs-delta per frame '
          f'(min={min(max_shifts):.2f} max={max(max_shifts):.2f} '
          f'mean={np.mean(max_shifts):.2f})')
    return out


# Global holding the current t_offset — set by render_movie before calling driftrow.
_T_OFFSET = 0


def m_driftrow(vol):
    return m_driftrow_absolute(vol, _T_OFFSET)


def m_stripfix_t3(vol):
    """Piecewise-rigid horizontal-strip registration against a per-frame template.

    Template = mean_t3(frame) — smoother than gated (which bails to identity)
    so it actually differs from the input, letting phase correlation detect
    within-frame shifts.
    """
    T = vol.shape[0]
    template_vol = np.empty_like(vol)
    for t in range(T):
        t0, t1 = max(0, t - 1), min(T - 1, t + 1)
        template_vol[t] = vol[t0:t1 + 1].mean(axis=0)

    out = np.empty_like(vol)
    _STRIPFIX_MAX_ABS_SHIFT_LOG.clear()
    for t in range(T):
        out[t], max_shift = _strip_register(
            vol[t], template_vol[t], n_strips=32, max_shift_px=8.0,
        )
        _STRIPFIX_MAX_ABS_SHIFT_LOG[t] = max_shift
    print(f'    stripfix max-abs-shift per frame '
          f'(min={min(_STRIPFIX_MAX_ABS_SHIFT_LOG.values()):.2f} '
          f'max={max(_STRIPFIX_MAX_ABS_SHIFT_LOG.values()):.2f} '
          f'mean={np.mean(list(_STRIPFIX_MAX_ABS_SHIFT_LOG.values())):.2f})')
    return out


def _strip_register(frame, template, n_strips=32, max_shift_px=8.0):
    H, W = frame.shape
    strip_h = max(H // n_strips, 4)
    hann = cv2.createHanningWindow((W, strip_h), cv2.CV_32F)

    centers, shifts = [], []
    f32 = frame.astype(np.float32)
    t32 = template.astype(np.float32)
    for s in range(n_strips):
        y0 = int(round(s * (H - strip_h) / max(n_strips - 1, 1)))
        y1 = y0 + strip_h
        (dx, dy), _ = cv2.phaseCorrelate(f32[y0:y1], t32[y0:y1], hann)
        if abs(dx) > max_shift_px or abs(dy) > max_shift_px:
            dx = dy = 0.0
        centers.append(0.5 * (y0 + y1))
        shifts.append((dy, dx))

    centers = np.array(centers, dtype=np.float32)
    dy_c = np.array([s[0] for s in shifts], dtype=np.float32)
    dx_c = np.array([s[1] for s in shifts], dtype=np.float32)
    max_abs = float(max(np.abs(dy_c).max(), np.abs(dx_c).max()))

    yy = np.arange(H, dtype=np.float32)
    dy_row = np.interp(yy, centers, dy_c).astype(np.float32)
    dx_row = np.interp(yy, centers, dx_c).astype(np.float32)

    map_x = (np.arange(W, dtype=np.float32)[None, :] + dx_row[:, None])
    map_y = (yy[:, None] + dy_row[:, None]) + np.zeros((1, W), dtype=np.float32)
    warped = cv2.remap(
        frame, map_x, map_y,
        interpolation=cv2.INTER_LINEAR, borderMode=cv2.BORDER_REPLICATE,
    )
    return warped, max_abs


def m_flowreg(vol):
    """Dense farneback REGISTRATION (not fusion): warp frame t to align with t-1.

    Preserves per-frame signal (no averaging) — dense flow just moves each pixel
    to its position in the previous frame. Kills intra-frame non-rigid deformation
    (the morphing stripes) since those show up as displacement fields farneback
    catches.

    Rolling pairwise (vol[t-1] as ref, not out[t-1]) so mis-registrations don't
    compound frame-to-frame. First frame stays as-is.
    """
    T, H, W = vol.shape
    out = np.empty_like(vol)
    out[0] = vol[0]
    yy, xx = np.mgrid[0:H, 0:W].astype(np.float32)
    for t in range(1, T):
        ref = vol[t - 1]
        mov = vol[t]
        flow = _flow(ref, mov)   # ref → mov: mov[y+dy, x+dx] ≈ ref[y, x]
        out[t] = cv2.remap(
            mov, xx + flow[..., 0], yy + flow[..., 1],
            interpolation=cv2.INTER_LINEAR, borderMode=cv2.BORDER_REPLICATE,
        )
    return out


ALL_METHODS = {
    'raw':        m_raw,
    'mean_t3':    m_mean_t3,
    'median_t3':  m_median_t3,
    'gated_t3':   m_gated_t3,
    'farneback':  m_farneback_t3,
    'stripfix':   m_stripfix_t3,
    'driftrow':   m_driftrow,
    'flowreg':    m_flowreg,
}
DEFAULT_METHODS = 'raw,gated_t3,farneback,flowreg'


def apply_along_t_at_z(tstack: np.ndarray, z: int, fn) -> np.ndarray:
    """tstack is (T, Z, Y, X); return (T, Y, X) fused along T at plane z."""
    return fn(tstack[:, z])


def _to_unit(arr, lo, hi):
    return np.clip((arr - lo) / max(hi - lo, 1e-9), 0.0, 1.0)


def _composite(green, magenta):
    """RGB overlay: G channel from `green`, R+B from `magenta`. Both already in [0,1]."""
    return np.stack([magenta, green, magenta], axis=-1)


def render_movie(t0, t1, c, z, fps, out_name, methods, overlay):
    global _T_OFFSET
    _T_OFFSET = t0
    print(f'loading T=[{t0}, {t1}) c={c} from both inputs …')
    stacks = {name: load_tc_range(path, t0, t1, c) for name, path in INPUTS.items()}
    T = stacks['raw'].shape[0]
    print(f'  shape per input: {stacks["raw"].shape}')

    print(f'applying T-axis methods at z={z} …')
    results: dict[str, dict[str, np.ndarray]] = {}
    timings: dict[str, dict[str, float]] = {}
    for input_name, tstack in stacks.items():
        results[input_name] = {}
        timings[input_name] = {}
        for m_name, fn in methods.items():
            t_start = _time.perf_counter()
            results[input_name][m_name] = apply_along_t_at_z(tstack, z, fn)
            timings[input_name][m_name] = _time.perf_counter() - t_start
            print(f'  {input_name:12s} {m_name:10s} '
                  f'{timings[input_name][m_name]:6.2f}s')

    all_slices = np.stack([
        results[i_name][m_name]
        for i_name in INPUTS for m_name in methods
    ])
    vmin = float(np.percentile(all_slices, 1))
    vmax = float(np.percentile(all_slices, 99.5))
    print(f'intensity: vmin={vmin:.0f} vmax={vmax:.0f}')

    # Green reference for overlay = the raw ccidImage at plane z.
    ref_green = stacks['raw'][:, z]
    g_vmin, g_vmax = vmin, vmax  # same scaling so grey = perfect match

    n_inputs = len(INPUTS); n_methods = len(methods)
    fig, axes = plt.subplots(
        n_inputs, n_methods,
        figsize=(2.8 * n_methods, 2.8 * n_inputs),
        gridspec_kw=dict(wspace=0.03, hspace=0.12),
    )
    axes = np.atleast_2d(axes)

    ims = {}
    for i, input_name in enumerate(INPUTS):
        for j, m_name in enumerate(methods):
            ax = axes[i, j]
            if overlay:
                g0 = _to_unit(ref_green[0], g_vmin, g_vmax)
                m0 = _to_unit(results[input_name][m_name][0], vmin, vmax)
                im = ax.imshow(_composite(g0, m0), interpolation='nearest')
            else:
                im = ax.imshow(
                    results[input_name][m_name][0],
                    cmap='gray', vmin=vmin, vmax=vmax, interpolation='nearest',
                )
            ims[(i, j)] = im
            ax.set_xticks([]); ax.set_yticks([])
            secs = timings[input_name][m_name]
            title = f'{m_name}  ({secs:.1f}s/{T}f)' if i == 0 else f'{secs:.1f}s/{T}f'
            ax.set_title(title, fontsize=8)
            if j == 0:
                ax.set_ylabel(input_name, fontsize=9)

    header = 'T-AXIS fusion' + ('  [overlay: raw=green, method=magenta]' if overlay else '')
    title_txt = fig.suptitle(
        f'c91ICQ  {header}  T=[{t0}, {t1})  c={c}  z={z}  '
        f'|  frame 000/{T}',
        fontsize=11,
    )

    frames_dir = Path(tempfile.mkdtemp(prefix='tfuse_frames_'))
    print(f'writing frames -> {frames_dir}')
    for t in range(T):
        for i, input_name in enumerate(INPUTS):
            for j, m_name in enumerate(methods):
                if overlay:
                    g = _to_unit(ref_green[t], g_vmin, g_vmax)
                    m = _to_unit(results[input_name][m_name][t], vmin, vmax)
                    ims[(i, j)].set_data(_composite(g, m))
                else:
                    ims[(i, j)].set_data(results[input_name][m_name][t])
        title_txt.set_text(
            f'c91ICQ  {header}  T=[{t0}, {t1})  c={c}  z={z}  '
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
    ap.add_argument('--out', type=str, default='temporal_fusion_movie_c91ICQ.mp4')
    ap.add_argument('--methods', type=str, default=DEFAULT_METHODS,
                    help=f'csv subset of {list(ALL_METHODS)}')
    ap.add_argument('--overlay', action='store_true',
                    help='render each panel as RGB overlay: raw ccidImage green, method magenta')
    args = ap.parse_args()
    names = [n.strip() for n in args.methods.split(',') if n.strip()]
    unknown = [n for n in names if n not in ALL_METHODS]
    if unknown:
        raise SystemExit(f'unknown methods: {unknown}. options: {list(ALL_METHODS)}')
    methods = {n: ALL_METHODS[n] for n in names}
    render_movie(args.t0, args.t1, args.c, args.z, args.fps, args.out,
                 methods, args.overlay)


if __name__ == '__main__':
    main()
