// Overlay points, alpha-blended over the raycast in the SAME pass. Two triangles per instance,
// sized by `p.ov.x` in image pixels (screen px in a movie) via `overlayPx`, so a marker keeps its
// size relative to the cell as you zoom. Plane filter uses `p.ov.y`/`p.ov.w` (the loaded z range);
// negative `p.ov.y` disables the filter. Mirrors `mip_points.wgsl` — same camera,
// same project(), so a marker sits on the cell rather than beside it.

#include "brick_common.wgsl"
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
