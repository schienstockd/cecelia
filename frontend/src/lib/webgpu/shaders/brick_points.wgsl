// Overlay points, alpha-blended over the raycast in the SAME pass. Two triangles per instance,
// scaled in SCREEN pixels via `p.ov.x` so a marker stays legible when zoomed out and doesn't
// swallow the cell when zoomed in. Plane filter uses `p.ov.y`/`p.ov.w` (the loaded z range);
// negative `p.ov.y` disables the filter. Mirrors `mip_points.wgsl` — same camera,
// same project(), so a marker sits on the cell rather than beside it.

#include "brick_common.wgsl"

struct POut {
  @builtin(position) pos: vec4<f32>,
  @location(0) rgb: vec3<f32>,
  @location(1) local: vec2<f32>,
};

@vertex fn vs(
  @builtin(vertex_index) vi: u32,
  @location(0) centre: vec3<f32>,
  @location(1) rgb: vec3<f32>,
  @location(2) plane: f32,
) -> POut {
  var o: POut;
  o.rgb = rgb;
  var q = array<vec2<f32>, 6>(
    vec2(-1.0, -1.0), vec2(1.0, -1.0), vec2(-1.0, 1.0),
    vec2(-1.0,  1.0), vec2(1.0, -1.0), vec2( 1.0, 1.0));
  let corner = q[vi];
  o.local = corner;

  // Outside the planes actually LOADED → a degenerate quad clipped behind the far plane. Negative
  // ov.y disables the filter (3D volume view over the whole stack).
  if (p.ov.y >= 0.0 && (plane < p.ov.y - 0.5 || plane > p.ov.w + 0.5)) {
    o.pos = vec4(0.0, 0.0, 2.0, 1.0);
    return o;
  }

  let aspect = p.vp.y / max(p.vp.z, 1.0);
  let c = camera();
  let ndc = project(centre - boxCentre(), c, aspect);
  // Quad grown by the black-outline width so the border sits OUTSIDE the fill. Mirrors mip_points.wgsl
  // — same uniform slot (p.lab.w), same encoding.
  let px = p.ov.x + max(p.lab.w, 0.0);
  o.pos = vec4(ndc.x + corner.x * (2.0 * px / max(p.vp.y, 1.0)),
               ndc.y + corner.y * (2.0 * px / max(p.vp.z, 1.0)),
               0.0, 1.0);
  return o;
}

@fragment fn fs(in: POut) -> @location(0) vec4<f32> {
  let r = length(in.local);
  let a = 1.0 - smoothstep(0.75, 1.0, r);
  if (a <= 0.001) { discard; }
  let border = max(p.lab.w, 0.0);
  if (border > 0.0) {
    let inner = p.ov.x / max(p.ov.x + border, 0.0001);
    if (r > inner) { return vec4(0.0, 0.0, 0.0, a); }
  }
  return vec4(in.rgb, a);
}
