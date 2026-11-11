// A segmentation's mask WITH its derived files (halo, nuc), shared VERBATIM by every pass that draws
// labels (mip.wgsl, brick.wgsl, brick_multi.wgsl, tile.wgsl). Pure: no uniform or texture access of
// its own. `${LAB_AT}` is the includer's raw fetch, `fn(vi: vec3<i32>) -> u32`, as for label_edge.wgsl.
//
// A derived file carries its base's cell ids, so the server packs it into the base mask as two flag
// bits (`compose_label_files!`, docs/todo/MASK_FILE_LAYERS_PLAN.md D2): OUT where the file reaches past
// the cell (a halo ring), IN where it overlaps it (a nucleus). With no file requested neither bit is
// ever set and every function here reduces to the plain mask.
//
// The CELL (`raw & LABEL_ID_MASK`) is what colour, population and pick read — labColour / labInPick
// strip the flags themselves. The BASE region (not OUT) is drawn as the viewer always drew it, filled
// or contoured; the FILE region (either flag) is always an outline, so it annotates the base mask
// instead of hiding it.

fn labCell(raw: u32) -> u32 { return raw & ${LABEL_ID_MASK}u; }
fn labBaseOf(raw: u32) -> u32 {
  if ((raw & ${LABEL_FILE_OUT_BIT}u) != 0u) { return 0u; }
  return labCell(raw);
}
fn labFileOf(raw: u32) -> u32 {
  if ((raw & (${LABEL_FILE_OUT_BIT}u | ${LABEL_FILE_IN_BIT}u)) == 0u) { return 0u; }
  return labCell(raw);
}
fn labBaseAt(vi: vec3<i32>) -> u32 { return labBaseOf(${LAB_AT}(vi)); }
fn labFileAt(vi: vec3<i32>) -> u32 { return labFileOf(${LAB_AT}(vi)); }

#include "label_edge.wgsl" LAB_EDGE=labEdge LAB_AT=labBaseAt
#include "label_edge.wgsl" LAB_EDGE=labFileEdge LAB_AT=labFileAt

// Whether the voxel `raw` at `vi` draws: its base region at the mask's fill / contour width `w`, its
// file region as an outline at least one voxel wide.
fn labDrawn(vi: vec3<i32>, raw: u32, w: i32) -> bool {
  let b = labBaseOf(raw);
  let f = labFileOf(raw);
  return (b != 0u && labEdge(vi, b, w)) || (f != 0u && labFileEdge(vi, f, max(w, 1)));
}

// Derived files are a PER-PLANE annotation: a pass draws them only with one sample per ray (`plane`,
// the 2D view). A raycast through several planes would meet a 3D halo's cap — a dome, a different
// footprint per plane — and stack one rim per plane over the cell; there the cells draw exactly as
// without the files (`labPlaneRaw`), which also keeps the march from stopping on a cap.
fn labPlaneRaw(raw: u32, plane: bool) -> u32 { return select(labBaseOf(raw), raw, plane); }

// A voxel of a derived file that lies PAST its cell (`OUT`: a halo ring) — kept apart from the cell
// the march finds, and outlined where the plane meets the file's edge (`labOutline`).
fn labIsOut(raw: u32) -> bool { return (raw & ${LABEL_FILE_OUT_BIT}u) != 0u; }
fn labOutline(vi: vec3<i32>, raw: u32, w: i32) -> bool {
  let f = labFileOf(raw);
  return f != 0u && labFileEdge(vi, f, max(w, 1));
}

// The pick / focus outline follows the cell's own (base) boundary.
fn labPickEdge(vi: vec3<i32>, raw: u32, w: i32) -> bool {
  let b = labBaseOf(raw);
  return b != 0u && labEdge(vi, b, w);
}
