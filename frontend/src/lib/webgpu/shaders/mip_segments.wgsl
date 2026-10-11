// Track tails: one screen-space quad per segment, drawn over the MIP with the points.
//
// QUADS RATHER THAN `line-list`, because WebGPU draws 1px lines only and a 1px tail over a noisy MIP is
// close to invisible — the viewer's `tail_width` defaults to 4. Each endpoint is projected independently
// and the quad is widened perpendicular to the SCREEN-space direction, so the width is in pixels and
// stays constant under perspective while the geometry stays correct.
//
// Shares the same uniform buffer, and therefore the same camera, as the raycast and the points — see
// `mip_points.wgsl` for why that matters more than it looks.

#include "mip_common.wgsl"
#include "overlay_quads.wgsl"

struct SOut { @builtin(position) pos: vec4<f32>, @location(0) rgb: vec3<f32>, @location(1) alpha: f32 };

@vertex fn vs(
  @builtin(vertex_index) vi: u32,
  @location(0) a: vec3<f32>,
  @location(1) b: vec3<f32>,
  @location(2) rgb: vec3<f32>,
  @location(3) plane: f32,
  @location(4) tEnd: f32,
) -> SOut {
  var o: SOut;
  o.rgb = rgb;
  o.alpha = tailAlpha(tEnd, p.trail.x, p.trail.y);
  // Ribbons carry their OWN plane bounds, so a viewer can widen the tail's z-reach independently of
  // the points' — a track's plane is where it ends, but the tail is a continuous path.
  if (offPlanes(plane, p.pan.z, p.pan.w)) {
    o.pos = clippedPos();
    return o;
  }
  let aspect = p.vp.y / max(p.vp.z, 1.0);
  let c = camera();
  let pa = project(a - boxCentre(), c, aspect).xy;
  let pb = project(b - boxCentre(), c, aspect).xy;
  o.pos = ribbonPos(pa, pb, vi, p.ov.z, p.vp.yz);
  return o;
}

@fragment fn fs(in: SOut) -> @location(0) vec4<f32> {
  return vec4(in.rgb, in.alpha);
}
