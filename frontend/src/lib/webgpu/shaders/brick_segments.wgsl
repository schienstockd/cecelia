// Track tails. One screen-space quad per segment, widened perpendicular to the SCREEN-space
// direction so the width is in pixels and stays constant under perspective. Own plane bounds
// (`p.pan.z`/`p.pan.w`) so ribbons can be widened independently of the points' z reach. Mirrors
// `mip_segments.wgsl`.

#include "brick_common.wgsl"

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
  if (p.pan.z >= 0.0 && (plane < p.pan.z - 0.5 || plane > p.pan.w + 0.5)) {
    o.pos = vec4(0.0, 0.0, 2.0, 1.0);
    return o;
  }
  let aspect = p.vp.y / max(p.vp.z, 1.0);
  let c = camera();
  let pa = project(a - boxCentre(), c, aspect).xy;
  let pb = project(b - boxCentre(), c, aspect).xy;

  let sa = vec2(pa.x * p.vp.y, pa.y * p.vp.z) * 0.5;
  let sb = vec2(pb.x * p.vp.y, pb.y * p.vp.z) * 0.5;
  var dir = sb - sa;
  let len = length(dir);
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
