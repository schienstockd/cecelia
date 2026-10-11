// Overlay points over the tile view, alpha-blended in the SAME pass after the tiles. Mirrors
// `mip_points.wgsl` — same instance layout, same screen-pixel sizing and border — with the tile
// camera's `overlayNdc` in place of the orbit `project()`.

#include "tile_common.wgsl"
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
  // Off the plane on screen (± the z tolerance) → clipped.
  if (offPlanes(plane, p.ov.y, p.ov.w)) {
    o.pos = clippedPos();
    return o;
  }
  let ndc = overlayNdc(centre);
  o.pos = markerPos(ndc, corner, p.ov.x, p.ovx.x, p.vp.yz);
  return o;
}

@fragment fn fs(in: POut) -> @location(0) vec4<f32> {
  let c = markerColour(in.local, in.rgb, p.ov.x, p.ovx.x);
  if (c.a <= 0.001) { discard; }
  return c;
}
