"""
Shared mesh helpers for the spatial MESH route (surface-based contact + aggregates), the counterpart
to the fast points (centroid) route — mostly used for live images where cell surfaces matter
(docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 11). Meshes are built ON THE FLY from label submasks
(marching cubes) — never persisted (the legacy saveMeshes was dropped). trimesh is a bundled dep
(THIRD_PARTY.md); this mirrors the mesh building already in measure_utils._extended_3d_measures.
"""
import numpy as np


def require_volume(n_spatial, task):
    """Raise unless the label store has 3 spatial axes (z, y, x). Marching cubes builds no mesh from a
    2D frame (every cell fails and is skipped), so a 2D image would read each whole label frame into
    RAM — the OOM on a tile scan — only to return nothing. Called by the mesh runners BEFORE any read."""
    if n_spatial != 3:
        raise ValueError(
            f"{task}: the mesh route needs a 3D (z, y, x) label volume, but this image has "
            f"{n_spatial} spatial axes — use the points route for a 2D image")


def build_label_meshes(vol, labels, phys, min_voxels=8, origin=None):
    """Build a physical-scaled trimesh per label via marching cubes on its submask.

    `vol`: 3D label volume (z, y, x, int); `labels`: label ids to build; `phys`: [sz, sy, sx] µm/voxel.
    Returns `{label: trimesh.Trimesh}` in the full-volume physical frame (vertices offset by the
    submask origin, then scaled). Cells with < `min_voxels` voxels or that fail marching cubes are
    skipped. Uses `scipy.ndimage.find_objects` so each cell is meshed from its own bounding box, not
    the whole volume. `origin` (z0, y0, x0) is where `vol` sits in a larger volume, for a crop."""
    import trimesh
    from scipy import ndimage

    phys = np.asarray(phys, dtype=float)
    wanted = {int(l) for l in labels}
    objs = ndimage.find_objects(vol)          # objs[label-1] = tuple of slices, or None
    out = {}
    for i, sl in enumerate(objs):
        lb = i + 1
        if sl is None or lb not in wanted:
            continue
        submask = vol[sl] == lb
        if submask.sum() < min_voxels:
            continue
        try:
            m = trimesh.voxel.ops.matrix_to_marching_cubes(submask)
            if not m.is_watertight:
                m.fill_holes()
        except Exception:
            continue
        start = np.array([s.start for s in sl], dtype=float)       # (z0, y0, x0)
        if origin is not None:
            start = start + np.asarray(origin, dtype=float)
        m.vertices = (m.vertices + start) * phys[: m.vertices.shape[1]]
        out[lb] = m
    return out


def _radius(m):
    return 0.5 * float(np.linalg.norm(m.extents))          # half the bbox diagonal (bounding radius)


def _surface_distance(ma, mb):
    """Min surface-to-surface distance (µm) as the nearest VERTEX pair of the two meshes. Marching-cubes
    vertices sit on every voxel face, so this is within half a voxel of the true surface gap (up to
    ~1 µm along a 2 µm z step) and ~0 when the meshes touch. The same definition `nearest_surface`
    answers in bulk; a triangle-exact search cost minutes per frame on a dense image."""
    from scipy.spatial import cKDTree

    d, _ = cKDTree(mb.vertices).query(ma.vertices, k=1)
    return float(d.min())


def nearest_surface(a_meshes, b_meshes):
    """For each A mesh, the nearest B mesh surface distance (µm, `_surface_distance`'s vertex-pair
    definition) + that B's label. ONE KD-tree over every B vertex, queried with each A's vertices, so a
    frame is a bulk lookup rather than a mesh-pair search. A with no B vertex within
    `_SURFACE_SEARCH_PAD` µm reads `(inf, None)`. Returns `{a_label: (min_dist_µm, b_label|None)}`."""
    from scipy.spatial import cKDTree

    if not a_meshes or not b_meshes:
        return {lb: (float("inf"), None) for lb in a_meshes}
    b_labels = list(b_meshes.keys())
    verts = np.concatenate([b_meshes[l].vertices for l in b_labels])
    owner = np.repeat(np.arange(len(b_labels)), [len(b_meshes[l].vertices) for l in b_labels])
    tree = cKDTree(verts)
    out = {}
    for lb, m in a_meshes.items():
        d, i = tree.query(m.vertices, k=1, distance_upper_bound=_SURFACE_SEARCH_PAD)
        j = int(np.argmin(d))
        out[lb] = (float(d[j]), b_labels[owner[i[j]]]) if np.isfinite(d[j]) else (float("inf"), None)
    return out


def nearest_surface_per_target(a_meshes, b_meshes, targets):
    """`nearest_surface` once per target population: `targets` is `[(name, {b_label, ...}), ...]`,
    each searched only among its own B meshes (one contact column set per target, #1734). B is
    meshed once by the caller and split here. Returns `{name: {a_label: (min_dist_µm, b_label|None)}}`."""
    return {name: nearest_surface(a_meshes, {bl: m for bl, m in b_meshes.items() if bl in labs})
            for name, labs in targets}


# how far to look for the nearest surface (µm): a near-but-not-touching neighbour is still found for
# the min_distance readout, not just contacts; beyond it reads as none.
_SURFACE_SEARCH_PAD = 50.0


def mesh_proximity_edges(meshes, max_dist):
    """Undirected edges between meshes whose SURFACE distance ≤ max_dist. Candidate pairs pre-filtered
    by centroid KDTree (Decision 11), so O(edges) not O(N²). Returns list of (labelA, labelB)."""
    from scipy.spatial import cKDTree

    labels = list(meshes.keys())
    if len(labels) < 2:
        return []
    cent = np.array([meshes[l].centroid for l in labels])
    rad = {l: _radius(meshes[l]) for l in labels}
    max_rad = max(rad.values())
    tree = cKDTree(cent)
    edges = []
    for i, li in enumerate(labels):
        reach = max_dist + rad[li] + max_rad
        for j in tree.query_ball_point(cent[i], reach):
            if j <= i:
                continue
            if _surface_distance(meshes[li], meshes[labels[j]]) <= max_dist:
                edges.append((li, labels[j]))
    return edges


def mesh_aggregates(meshes, max_dist, min_cells):
    """Aggregate ids by mesh-proximity connected components: cells whose meshes are within `max_dist`
    (surface) are linked; a component with ≥ `min_cells` meshes is an aggregate (legacy
    cellClustersMeshes). Returns `{label: aggregate_id}` (0 = not aggregated). Union-find, no extra deps."""
    labels = list(meshes.keys())
    parent = {l: l for l in labels}

    def find(x):
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x

    for a, b in mesh_proximity_edges(meshes, max_dist):
        parent[find(a)] = find(b)

    comps = {}
    for l in labels:
        comps.setdefault(find(l), []).append(l)
    out = {l: 0 for l in labels}
    aid = 0
    for members in comps.values():
        if len(members) >= min_cells:
            aid += 1
            for m in members:
                out[m] = aid
    return out


# ── Enclosure: how much of an A cell is wrapped by a B cell, and how many B cells an A cell wraps ───
# Measured on the LABEL voxels the meshes come from: the share of a thin shell around the object that
# lies on the host. An engulfed fragment sits in a vacuole that is open in some planes, or spills past
# it, so "inside a sealed hole" almost never holds on real segmentations; the share of its surroundings
# that is host does, and stays a measure (0–1) rather than a verdict.

ENCLOSURE_SHELL = 2          # voxels: the shell grown round the object, in 3D
ENCLOSED_FRAC = 0.5          # a host wraps an object when this much of its shell is that host


def _shells(vol, labels, shell):
    """`{label: (slices, ring mask)}` per listed object in `vol`: the `shell`-voxel ring round it (3D
    dilation minus the object), inside its bounding box grown by `shell`."""
    from scipy import ndimage

    wanted = {int(l) for l in labels}
    out = {}
    for i, sl in enumerate(ndimage.find_objects(vol)):
        if sl is None or i + 1 not in wanted:
            continue
        box = tuple(slice(max(0, s.start - shell), min(n, s.stop + shell)) for s, n in zip(sl, vol.shape))
        obj = vol[box] == i + 1
        ring = ndimage.binary_dilation(obj, iterations=shell) & ~obj
        if ring.any():
            out[i + 1] = (box, ring)
    return out


def enclosure_per_target(a_vol, a_labels, b_vol, targets, shell=ENCLOSURE_SHELL, frac=ENCLOSED_FRAC):
    """Per target: how enclosed each A cell is by that target's B cells, and how many of them each A wraps.

    `targets` is `[(name, {b_label, ...}), ...]` as for `nearest_surface_per_target`. `enclosed[a]` =
    the share (0–1) of A's `shell`-voxel ring lying on the target's B labels in `b_vol`; `holds[a]` =
    how many of the target's B cells have ≥ `frac` of THEIR ring on A. A and B may be the same label
    image (one segmentation gated twice). Returns `{name: (enclosed {a: float}, holds {a: int})}` over
    the A labels present in `a_vol`."""
    b_all = set().union(*(labs for _, labs in targets)) if targets else set()
    a_rings = _shells(a_vol, a_labels, shell)
    present = {int(l) for l in np.unique(a_vol)} & {int(l) for l in a_labels}
    # which A label wraps each B (≥ frac of B's ring on one A)
    wrapped_by = {}
    for b, (box, ring) in _shells(b_vol, b_all, shell).items():
        under = a_vol[box][ring].astype(np.int64)
        labs, cnt = np.unique(under[under > 0], return_counts=True)
        if len(labs):
            k = int(np.argmax(cnt))
            if cnt[k] >= frac * ring.sum() and int(labs[k]) in present:
                wrapped_by[b] = int(labs[k])
    out = {}
    for name, labs in targets:
        ids = np.fromiter((int(l) for l in labs), dtype=np.int64)
        enclosed = {a: 0.0 for a in present}
        for a, (box, ring) in a_rings.items():
            enclosed[a] = float(np.isin(b_vol[box][ring], ids).mean())
        holds = {a: 0 for a in present}
        for b, a in wrapped_by.items():
            if b in labs:
                holds[a] += 1
        out[name] = (enclosed, holds)
    return out
