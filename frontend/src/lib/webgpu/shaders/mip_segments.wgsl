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

struct SOut { @builtin(position) pos: vec4<f32>, @location(0) rgb: vec3<f32> };

@vertex fn vs(
  @builtin(vertex_index) vi: u32,
  @location(0) a: vec3<f32>,
  @location(1) b: vec3<f32>,
  @location(2) rgb: vec3<f32>,
  @location(3) plane: f32,
) -> SOut {
  var o: SOut;
  o.rgb = rgb;
  // Ribbons carry their OWN plane bounds (pan.z / pan.w) so a viewer can widen the tail's z-reach
  // independently of the points' Z reach — a track's plane is where it ends, but the tail is a
  // continuous path and often reads best with more slack. Negative pan.z = no filter, same
  // convention as the points path.
  if (p.pan.z >= 0.0 && (plane < p.pan.z - 0.5 || plane > p.pan.w + 0.5)) {
    o.pos = vec4(0.0, 0.0, 2.0, 1.0);          // outside the widened track window: clipped away
    return o;
  }
  let aspect = p.vp.y / max(p.vp.z, 1.0);
  let c = camera();
  let pa = project(a - boxCentre(), c, aspect).xy;
  let pb = project(b - boxCentre(), c, aspect).xy;

  // In PIXELS, so the perpendicular is square on screen rather than stretched by the viewport aspect.
  let sa = vec2(pa.x * p.vp.y, pa.y * p.vp.z) * 0.5;
  let sb = vec2(pb.x * p.vp.y, pb.y * p.vp.z) * 0.5;
  var dir = sb - sa;
  let len = length(dir);
  // A zero-length segment has no direction to be perpendicular to; pick one rather than emit NaN,
  // which propagates to the whole quad and shows up as a stray triangle across the frame.
  dir = select(vec2(1.0, 0.0), dir / max(len, 1e-6), len > 1e-6);
  let nrm = vec2(-dir.y, dir.x) * (p.ov.z * 0.5);

  var corner = array<vec2<f32>, 6>(
    vec2(0.0, -1.0), vec2(1.0, -1.0), vec2(0.0, 1.0),
    vec2(0.0,  1.0), vec2(1.0, -1.0), vec2(1.0, 1.0));
  let k = corner[vi];
  let sp2 = mix(sa, sb, k.x) + nrm * k.y;
  o.pos = vec4(sp2.x * 2.0 / max(p.vp.y, 1.0), sp2.y * 2.0 / max(p.vp.z, 1.0), 0.0, 1.0);
  return o;
}

@fragment fn fs(in: SOut) -> @location(0) vec4<f32> {
  return vec4(in.rgb, 0.85);
}
