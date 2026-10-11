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
// SIZE IS IN SCREEN PIXELS, not µm. A cell marker is annotation: it has to stay legible when you zoom
// out and must not swallow the cell when you zoom in — which is what the viewer's `points_size` does, and
// what a µm-sized quad would get backwards.
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
  let ndc = project(centre - boxCentre(), c, aspect).xy;
  o.pos = markerPos(ndc, corner, p.ov.x, p.lab.w, p.vp.yz);
  return o;
}

@fragment fn fs(in: POut) -> @location(0) vec4<f32> {
  let c = markerColour(in.local, in.rgb, p.ov.x, p.lab.w);
  if (c.a <= 0.001) { discard; }
  return c;
}
