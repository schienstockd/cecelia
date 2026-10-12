// The overlay pass: population points as camera-facing quads, drawn over the finished MIP.
//
// ONE INSTANCE PER POINT, six vertices generated in the shader — no vertex buffer for the quad, and no
// geometry uploaded per frame. The instance data is built once per (image, populations) and ordered by
// timepoint, so drawing a frame is `draw(6, count, 0, first)` over a contiguous range.
//
// IT SHARES THE RAYCAST'S UNIFORM BUFFER, and therefore its camera, deliberately: `project()` is the
// exact inverse of the ray construction, so a point lands on the voxel it was measured from at any yaw,
// pitch or zoom. A second camera copy for the overlays would be one number away from marking the wrong
// cell, and it would drift silently — the overlay would still look plausible.
//
// SIZE IS IN IMAGE PIXELS in the viewer (screen px in a movie) — `overlayPx`, overlay_quads.wgsl — so a
// marker keeps its size relative to the cell it marks as you zoom. Under perspective it is converted at
// the point's own depth, so a far cell's marker shrinks with the cell.
//
// The plane filter collapses the quad to zero area rather than being a CPU filter, because the 2D view
// changes plane from a slider: rebuilding and re-uploading the buffer per z step is exactly the cost
// the sorted-by-timepoint layout exists to avoid.

#include "mip_common.wgsl"
#include "overlay_quads.wgsl"

struct POut {
  @builtin(position) pos: vec4<f32>,
  @location(0) rgb: vec3<f32>,
  @location(1) local: vec2<f32>,          // -1..1 across the quad, for the round mask
  @location(2) @interpolate(flat) px: vec2<f32>,   // fill size, border — screen px (`overlayPx`)
};

@vertex fn vs(
  @builtin(vertex_index) vi: u32,
  @location(0) centre: vec3<f32>,         // absolute image µm
  @location(1) rgb: vec3<f32>,
  @location(2) plane: f32,
) -> POut {
  var o: POut;
  o.rgb = rgb;
  let corner = markerCorner(vi);
  o.local = corner;
  // Outside the planes actually LOADED → clipped. A range, not one plane, because the 3D view can be
  // cropped to part of the stack; the 2D view passes lo == hi.
  if (offPlanes(plane, p.ov.y, p.ov.w)) {
    o.pos = clippedPos();
    return o;
  }
  let aspect = p.vp.y / max(p.vp.z, 1.0);
  let c = camera();
  let q = project(centre - boxCentre(), c, aspect);
  let ndc = q.xy;
  let sizePx = overlayPx(p.ov.x, p.trail.z, pxPerUm(q.z));
  o.px = vec2(sizePx, overlayBorderPx(p.lab.w, sizePx, p.trail.z));
  o.pos = markerPos(ndc, corner, o.px.x, o.px.y, p.vp.yz);
  return o;
}

@fragment fn fs(in: POut) -> @location(0) vec4<f32> {
  let c = markerColour(in.local, in.rgb, in.px.x, in.px.y);
  if (c.a <= 0.001) { discard; }
  return c;
}
