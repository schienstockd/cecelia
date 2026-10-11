// Overlay points, alpha-blended over the raycast in the SAME pass. Two triangles per instance,
// scaled in SCREEN pixels via `p.ov.x` so a marker stays legible when zoomed out and doesn't
// swallow the cell when zoomed in. Plane filter uses `p.ov.y`/`p.ov.w` (the loaded z range);
// negative `p.ov.y` disables the filter. Mirrors `mip_points.wgsl` — same camera,
// same project(), so a marker sits on the cell rather than beside it.

#include "brick_common.wgsl"
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
