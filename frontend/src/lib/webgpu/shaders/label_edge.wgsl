// The viewer's 'contour' test, shared VERBATIM by every pass that draws labels (mip.wgsl, brick.wgsl,
// brick_multi.wgsl, tile.wgsl). Pure: no uniform or texture access of its own.
//
// Whether the label `id` at `vi` is on its OUTLINE, w voxels thick — which is what lets the channel
// signal under the mask stay readable while the boundary stays exact. Filled at w = 0, the viewer's
// default. In-plane only (x/y): the outline of a 3D object through its z neighbours is a surface, not a
// contour, and would fill the region back in.
//
// `${LAB_AT}` is the includer's own fetch, `fn(vi: vec3<i32>) -> u32`, defined before the include:
// `#include "label_edge.wgsl" LAB_AT=labAt`. What a neighbour off the loaded data reads as is that
// fetch's decision, not this loop's — background in a raycast (a cell at the slab edge outlines along
// it), the same id in a tile (no outline along every tile seam).
fn labEdge(vi: vec3<i32>, id: u32, w: i32) -> bool {
  if (w <= 0) { return true; }
  for (var k = 1; k <= w; k = k + 1) {
    if (${LAB_AT}(vi + vec3<i32>(k, 0, 0)) != id || ${LAB_AT}(vi - vec3<i32>(k, 0, 0)) != id ||
        ${LAB_AT}(vi + vec3<i32>(0, k, 0)) != id || ${LAB_AT}(vi - vec3<i32>(0, k, 0)) != id) { return true; }
  }
  return false;
}
