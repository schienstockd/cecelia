// Track tails. One screen-space quad per segment, widened perpendicular to the SCREEN-space
// direction; the width is image pixels (screen px in a movie) via `overlayPx`. Own plane bounds
// (`p.pan.z`/`p.pan.w`) so ribbons can be widened independently of the points' z reach. Mirrors
// `mip_segments.wgsl`.

#include "brick_common.wgsl"
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
  let pa = project(a - boxCentre(), c, aspect);
  let pb = project(b - boxCentre(), c, aspect);
  o.pos = ribbonPos(pa.xy, pb.xy, vi, overlayPx(p.ov.z, p.trail.z, pxPerUm(0.5 * (pa.z + pb.z))), p.vp.yz);
  return o;
}

@fragment fn fs(in: SOut) -> @location(0) vec4<f32> {
  return vec4(in.rgb, in.alpha);
}
