"""Compare Z-neighbour fusion methods on c91ICQ (drift-3d follow-up P1).

Methods per (t, c) Z-stack, window size 3 (z-1, z, z+1):
    raw       — identity (baseline)
    mean_z3
    median_z3
    gated_z3  — coastal.smooth.temporal_gated with time_axis=Z
    farneback — dense flow warp+fuse with forward-backward consistency gate

Two inputs: raw (ccidImage) and stackAligned (ccidStackAligned). Renders a
grid (methods x inputs) at a chosen (t, c, z) plus an XZ slice at a chosen y
row, saved to ~/Downloads/TMP.

Not wired into any task; iterate here then decide direction.
"""

from __future__ import annotations
import argparse
import os
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

# Farneback params — small winsize for microscopy small non-rigid deformations.
FARNEBACK_KW = dict(
    pyr_scale=0.5, levels=3, winsize=11, iterations=3,
    poly_n=5, poly_sigma=1.2, flags=0,
)
FB_CONSISTENCY_PX = 1.0  # round-trip error above this -> fall back to ref only


def load_tc(path: Path, t: int, c: int) -> np.ndarray:
    """Return (Z, Y, X) float32 for one (t, c) at level 0."""
    levels = open_as_zarr(str(path), as_dask=False)
    arr = levels[0]
    if isinstance(arr, list):
        arr = arr[0]
    return np.asarray(arr[t, c]).astype(np.float32)


def _to_u8(plane: np.ndarray) -> np.ndarray:
    """Per-plane min-max to uint8 for cv2 flow input."""
    lo, hi = float(plane.min()), float(plane.max())
    if hi <= lo:
        return np.zeros_like(plane, dtype=np.uint8)
    return np.clip(((plane - lo) / (hi - lo) * 255.0), 0, 255).astype(np.uint8)


def _flow(prev: np.ndarray, nxt: np.ndarray) -> np.ndarray:
    """Farneback flow such that nxt[y+dy, x+dx] ~= prev[y, x]."""
    return cv2.calcOpticalFlowFarneback(
        _to_u8(prev), _to_u8(nxt), None, **FARNEBACK_KW,
    )


def _warp_to_ref(neighbor: np.ndarray, flow_ref_to_nb: np.ndarray) -> np.ndarray:
    """Sample `neighbor` at ref-grid positions displaced by flow."""
    h, w = neighbor.shape
    yy, xx = np.mgrid[0:h, 0:w].astype(np.float32)
    map_x = xx + flow_ref_to_nb[..., 0]
    map_y = yy + flow_ref_to_nb[..., 1]
    return cv2.remap(
        neighbor, map_x, map_y,
        interpolation=cv2.INTER_LINEAR,
        borderMode=cv2.BORDER_REPLICATE,
    )


def _fb_confidence(flow_fwd: np.ndarray, flow_bwd: np.ndarray) -> np.ndarray:
    """Round-trip pixel-position error in ref frame. Smaller = more consistent."""
    h, w = flow_fwd.shape[:2]
    yy, xx = np.mgrid[0:h, 0:w].astype(np.float32)
    # ref pixel p goes to p2 in neighbor:
    px = xx + flow_fwd[..., 0]
    py = yy + flow_fwd[..., 1]
    # sample bwd flow at p2 (bwd should send p2 back to p):
    bwd_x = cv2.remap(flow_bwd[..., 0], px, py, cv2.INTER_LINEAR,
                      borderMode=cv2.BORDER_REPLICATE)
    bwd_y = cv2.remap(flow_bwd[..., 1], px, py, cv2.INTER_LINEAR,
                      borderMode=cv2.BORDER_REPLICATE)
    # round-trip end position minus start:
    err_x = flow_fwd[..., 0] + bwd_x
    err_y = flow_fwd[..., 1] + bwd_y
    return np.sqrt(err_x * err_x + err_y * err_y)


def method_raw(stack: np.ndarray) -> np.ndarray:
    return stack.copy()


def method_mean_z3(stack: np.ndarray) -> np.ndarray:
    Z = stack.shape[0]
    out = np.empty_like(stack)
    for z in range(Z):
        z0, z1 = max(0, z - 1), min(Z - 1, z + 1)
        out[z] = stack[z0:z1 + 1].mean(axis=0)
    return out


def method_median_z3(stack: np.ndarray) -> np.ndarray:
    Z = stack.shape[0]
    out = np.empty_like(stack)
    for z in range(Z):
        z0, z1 = max(0, z - 1), min(Z - 1, z + 1)
        out[z] = np.median(stack[z0:z1 + 1], axis=0)
    return out


def method_gated_z3(stack: np.ndarray) -> np.ndarray:
    # coastal temporal_gated with time_axis=Z. window==frames.
    return temporal_gated(
        stack, frames=3, time_axis=0,
        search=1, patch=5, sigma=None, k=1.0,
    ).astype(np.float32)


def method_farneback_z3(stack: np.ndarray) -> np.ndarray:
    Z = stack.shape[0]
    out = np.empty_like(stack)
    for z in range(Z):
        ref = stack[z]
        accum = ref.copy()
        weight = np.ones_like(ref, dtype=np.float32)
        for dz in (-1, 1):
            zn = z + dz
            if zn < 0 or zn >= Z:
                continue
            neighbor = stack[zn]
            flow_fwd = _flow(ref, neighbor)      # ref -> neighbor
            flow_bwd = _flow(neighbor, ref)      # neighbor -> ref
            err = _fb_confidence(flow_fwd, flow_bwd)
            trust = (err <= FB_CONSISTENCY_PX).astype(np.float32)
            warped = _warp_to_ref(neighbor, flow_fwd)
            accum += warped * trust
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


def sharpness_vol(vol: np.ndarray) -> float:
    """Mean variance-of-Laplacian across Z — one scalar per volume."""
    vals = []
    for z in range(vol.shape[0]):
        lap = cv2.Laplacian(vol[z].astype(np.float32), cv2.CV_32F, ksize=3)
        vals.append(float(lap.var()))
    return float(np.mean(vals))


def _panel_range(*vols):
    stacked = np.stack([v.ravel() for v in vols])
    return float(np.percentile(stacked, 1)), float(np.percentile(stacked, 99.5))


def render(t: int, c: int, z: int, y_row: int, out_name: str) -> None:
    stacks = {name: load_tc(path, t, c) for name, path in INPUTS.items()}
    results: dict[str, dict[str, np.ndarray]] = {}
    sharps: dict[str, dict[str, float]] = {}
    for input_name, stack in stacks.items():
        results[input_name] = {}
        sharps[input_name] = {}
        for m_name, fn in METHODS.items():
            fused = fn(stack)
            results[input_name][m_name] = fused
            sharps[input_name][m_name] = sharpness_vol(fused)

    all_vols = [v for d in results.values() for v in d.values()]
    vmin, vmax = _panel_range(*all_vols)

    n_inputs = len(INPUTS)
    n_methods = len(METHODS)
    fig, axes = plt.subplots(
        n_inputs * 2, n_methods,
        figsize=(3.2 * n_methods, 3.2 * n_inputs * 2),
        gridspec_kw=dict(wspace=0.05, hspace=0.15),
    )
    axes = np.atleast_2d(axes)

    for i, input_name in enumerate(INPUTS):
        for j, m_name in enumerate(METHODS):
            fused = results[input_name][m_name]

            # row 2*i: XY at chosen z
            ax_xy = axes[2 * i, j]
            ax_xy.imshow(fused[z], cmap='gray', vmin=vmin, vmax=vmax,
                         interpolation='nearest')
            ax_xy.set_xticks([]); ax_xy.set_yticks([])
            sh = sharps[input_name][m_name]
            title = m_name if i == 0 else ''
            ax_xy.set_title(f'{title}\nvarLap={sh:.0f}', fontsize=8)
            if j == 0:
                ax_xy.set_ylabel(f'{input_name}\nXY z={z}', fontsize=9)

            # row 2*i+1: XZ slice at chosen y_row (Z, X)
            ax_xz = axes[2 * i + 1, j]
            xz = fused[:, y_row, :]  # (Z, X)
            ax_xz.imshow(xz, cmap='gray', vmin=vmin, vmax=vmax,
                         aspect='auto', interpolation='nearest')
            ax_xz.set_xticks([]); ax_xz.set_yticks([])
            if j == 0:
                ax_xz.set_ylabel(f'{input_name}\nXZ y={y_row}', fontsize=9)

    fig.suptitle(
        f'c91ICQ  t={t} c={c}  |  Z-neighbour fusion methods (n=3 window)\n'
        f'FB gate: round-trip err <= {FB_CONSISTENCY_PX}px  |  '
        f'winsize={FARNEBACK_KW["winsize"]}',
        fontsize=11,
    )
    out_path = OUT_DIR / out_name
    fig.savefig(out_path, dpi=130, bbox_inches='tight')
    plt.close(fig)
    print(f'wrote {out_path}')

    # Also dump per-method sharpness table.
    print(f'\nsharpness (mean var-of-Laplacian across Z):')
    print(f'{"":22s}' + ''.join(f'{m:>12s}' for m in METHODS))
    for input_name in INPUTS:
        row = f'{input_name:22s}'
        for m_name in METHODS:
            row += f'{sharps[input_name][m_name]:12.1f}'
        print(row)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--t', type=int, default=60)
    ap.add_argument('--c', type=int, default=0)
    ap.add_argument('--z', type=int, default=3)
    ap.add_argument('--y', type=int, default=256)
    ap.add_argument('--out', type=str, default='stack_fusion_grid_c91ICQ.png')
    args = ap.parse_args()
    render(args.t, args.c, args.z, args.y, args.out)


if __name__ == '__main__':
    main()
