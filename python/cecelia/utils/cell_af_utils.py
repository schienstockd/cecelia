"""Per-cell autofluorescence correction for cyclic multiplex panels (PhenoCycler, IBEX, CODEX).

In a cyclic run every fluorophore channel is re-imaged once per cycle, so a cell's autofluorescence in
that fluorophore's band shows up in EVERY marker read through it. On a 60-marker PhenoCycler panel this
was the largest structure in the cell table: markers sharing a dye correlated 0.41 (Spearman) vs 0.24
for markers on different dyes, and exclusive pairs read as co-expressed (CD4–MUC2 0.72) — even though
the scanner had already subtracted its blank cycles.

The correction, per marker j in a dye group g (≥ `MIN_GROUP` markers), in INTENSITY units:

    background_i = lower quartile over the OTHER markers of g of x_ik / s_k   (s_k = the marker's typical
                                                                    level; the minimum when only two
                                                                    others: a cell positive for one of
                                                                    them must not raise its estimate)
    x'_ij        = x_ij − β_j · (background_i − p2(background))      (β_j fitted on the marker's negatives,
                                                                    p2 = the cleanest cells, so nothing
                                                                    is ever brightened)

Autofluorescence ADDS photons, so the correction subtracts in linear units: a positive cell keeps its
specific signal whatever its autofluorescence. (Subtracting in a log-like space instead divides at the
bright end — positives on high-AF cell types came out at half the value of the same positives on clean
cells.) β is fitted iteratively on the half of the cells closest to the fit (smallest residual), so the
marker's positives, up to half the table, never flatten the slope. The estimate is per CELL on purpose:
smoothing it over a neighbourhood (or subtracting a spatial background) removed far less of the shared
signal on the same data, because the autofluorescence follows cell types, not a field across the tissue.

Validated on HuBMAP intestine PhenoCycler data (60 markers, 152,689 cells); the record is in
docs/DATAMODEL.md → *Autofluorescence-corrected intensities*.
"""

import numpy as np

MIN_GROUP = 3            # fewer markers on a dye than this → no leave-one-out estimate; left as measured
BG_QUANTILE = 0.25       # background = this order statistic of the other markers (the minimum for two)
FIT_KEEP = 50            # β is fitted on the cells below this percentile of the residual (its negatives)
FIT_ROUNDS = 3           # refits; each re-selects the negatives from the previous fit's residuals
BASELINE_PERCENTILE = 2  # a cell's AF is measured above this percentile of the panel's (the cleanest cells)
QC_SAMPLE = 20000        # cells sampled for the before/after correlation metric


def _scale(x):
    """A marker's typical level, to put the markers of one dye on a common footing. The median, unless
    the marker is mostly zero (scanner-subtracted data), then a tenth of its 90th percentile."""
    return max(float(np.median(x)), float(np.percentile(x, 90)) / 10.0, 1e-9)


def _lower_stat(others):
    """Per row, the BG_QUANTILE order statistic of `others` (cells × markers) — robust to the few markers a
    cell is truly positive for."""
    k = int(np.floor((others.shape[1] - 1) * BG_QUANTILE))
    return np.partition(others, k, axis=1)[:, k]


def _fit_beta(bg, x):
    """Slope of x on bg over the marker's negatives: start below the median of x, then refit on the cells
    nearest the line. ≥ 0 — autofluorescence only ever adds signal."""
    neg = x <= np.percentile(x, FIT_KEEP)
    beta = 0.0
    for _ in range(FIT_ROUNDS):
        if neg.sum() <= 10 or np.ptp(bg[neg]) <= 0:
            break
        b, a = np.polyfit(bg[neg], x[neg], 1)
        beta = max(float(b), 0.0)
        r = x - (a + beta * bg)
        neg = r <= np.percentile(r, FIT_KEEP)
    return beta


def dye_groups(groups, exclude=()):
    """{group key: [channel index, ...]} for the groups that can be corrected (≥ MIN_GROUP members).
    `groups[i]` is channel i's dye key, or None for no group; `exclude` channels are left out."""
    out = {}
    ex = set(int(i) for i in exclude)
    for i, g in enumerate(groups):
        if g is None or g == "" or i in ex:
            continue
        out.setdefault(str(g), []).append(i)
    return {g: idx for g, idx in out.items() if len(idx) >= MIN_GROUP}


def correct_cell_af(X, groups, exclude=()):
    """Correct per-cell autofluorescence. `X` is (cells, channels) intensities; returns
    (corrected, info) where `corrected` has one column per corrected channel, in `info["channels"]`
    order (0-based channel indices), in the input's intensity units (≥ 0). Channels outside a
    correctable group are not returned."""
    X = np.clip(np.asarray(X, dtype=np.float64), 0, None)
    grp = dye_groups(groups, exclude)
    chans = sorted(i for idx in grp.values() for i in idx)
    info = {"channels": chans, "groups": grp, "betas": {}}
    if not chans or X.shape[0] == 0:
        return np.zeros((X.shape[0], 0)), info
    out = np.empty((X.shape[0], len(chans)))
    col = {ch: k for k, ch in enumerate(chans)}
    for idx in grp.values():
        U = X[:, idx] / np.array([_scale(X[:, j]) for j in idx])
        for k, j in enumerate(idx):
            bg = _lower_stat(np.delete(U, k, axis=1))
            beta = _fit_beta(bg, X[:, j])
            info["betas"][j] = beta
            # AF above the LEAST autofluorescent cells: a per-marker constant, so every cell only ever
            # loses signal and a dim cell is never brightened.
            out[:, col[j]] = np.clip(X[:, j] - beta * (bg - np.percentile(bg, BASELINE_PERCENTILE)), 0, None)
    return out, info


def same_dye_correlation(X, groups, exclude=(), sample=QC_SAMPLE, seed=0):
    """(same-dye, other-dye) mean Spearman correlation between markers — the QC readout: shared
    autofluorescence shows as markers on one dye correlating more than markers on different dyes.
    None when there is nothing to compare."""
    X = np.asarray(X, dtype=np.float64)
    grp = dye_groups(groups, exclude)
    chans = sorted(i for idx in grp.values() for i in idx)
    if len(chans) < 2 or X.shape[0] < 3:
        return None
    rng = np.random.default_rng(seed)
    rows = rng.choice(X.shape[0], min(sample, X.shape[0]), replace=False)
    S = X[np.ix_(rows, chans)]
    R = np.argsort(np.argsort(S, axis=0), axis=0).astype(np.float64)
    C = np.corrcoef(R.T)
    key = {ch: g for g, idx in grp.items() for ch in idx}
    same, other = [], []
    for a in range(len(chans)):
        for b in range(a + 1, len(chans)):
            r = C[a, b]
            if not np.isfinite(r):
                continue
            (same if key[chans[a]] == key[chans[b]] else other).append(r)
    if not same:
        return None
    return float(np.mean(same)), (float(np.mean(other)) if other else None)
