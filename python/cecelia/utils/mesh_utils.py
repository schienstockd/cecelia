"""
Shared mesh helpers for the spatial MESH route (surface-based contact + aggregates), the counterpart
to the fast points (centroid) route — mostly used for live images where cell surfaces matter
(docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 11). Meshes are built ON THE FLY from label submasks
(marching cubes) — never persisted (the legacy saveMeshes was dropped). trimesh is a bundled dep
(THIRD_PARTY.md); this mirrors the mesh building already in measure_utils._extended_3d_measures.
"""
import numpy as np


def build_label_meshes(vol, labels, phys, min_voxels=8):
    """Build a physical-scaled trimesh per label via marching cubes on its submask.

    `vol`: 3D label volume (z, y, x, int); `labels`: label ids to build; `phys`: [sz, sy, sx] µm/voxel.
    Returns `{label: trimesh.Trimesh}` in the full-volume physical frame (vertices offset by the
    submask origin, then scaled). Cells with < `min_voxels` voxels or that fail marching cubes are
    skipped. Uses `scipy.ndimage.find_objects` so each cell is meshed from its own bounding box, not
    the whole volume."""
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
        origin = np.array([s.start for s in sl], dtype=float)      # (z0, y0, x0)
        m.vertices = (m.vertices + origin) * phys[: m.vertices.shape[1]]
        out[lb] = m
    return out


def _radius(m):
    return 0.5 * float(np.linalg.norm(m.extents))          # half the bbox diagonal (bounding radius)


def _surface_distance(ma, mb):
    """Approx min surface-to-surface distance (µm): min of A-vertices→B-surface and B-vertices→A-surface
    (vertex-sampled, unsigned). ~0 when the meshes touch/overlap. Avoids fcl."""
    import trimesh
    _, da, _ = trimesh.proximity.closest_point(mb, ma.vertices)
    _, db, _ = trimesh.proximity.closest_point(ma, mb.vertices)
    return float(min(da.min(), db.min()))


def nearest_surface(a_meshes, b_meshes):
    """For each A mesh, the nearest B mesh surface distance + B label. Pre-filters candidate B by
    CENTROID distance (KDTree, Decision 11) — a B can only be within surface range if its centroid is
    within `max_dist + r_A + r_B` — then computes the exact-ish surface distance only for those
    candidates. O(edges), not O(N²), and no fcl. Returns `{a_label: (min_dist_µm, b_label|None)}`."""
    from scipy.spatial import cKDTree

    if not a_meshes or not b_meshes:
        return {lb: (float("inf"), None) for lb in a_meshes}
    b_labels = list(b_meshes.keys())
    b_cent = np.array([b_meshes[l].centroid for l in b_labels])
    max_b_rad = max(_radius(b_meshes[l]) for l in b_labels)
    tree = cKDTree(b_cent)
    out = {}
    for lb, m in a_meshes.items():
        reach = _radius(m) + max_b_rad          # widest centroid gap that could still touch
        cand = tree.query_ball_point(m.centroid, reach + _SURFACE_SEARCH_PAD)
        best = (float("inf"), None)
        for ci in cand:
            d = _surface_distance(m, b_meshes[b_labels[ci]])
            if d < best[0]:
                best = (d, b_labels[ci])
        out[lb] = best
    return out


# how far past the touching-reach to still consider a candidate (µm), so a near-but-not-touching
# nearest neighbour is still found for the min_distance readout, not just contacts.
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
