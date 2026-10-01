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
  // Two triangles, corners in -1..1. Written out rather than computed from bit tricks: this is read
  // far more often than it is executed.
  var q = array<vec2<f32>, 6>(
    vec2(-1.0, -1.0), vec2(1.0, -1.0), vec2(-1.0, 1.0),
    vec2(-1.0,  1.0), vec2(1.0, -1.0), vec2( 1.0, 1.0));
  let corner = q[vi];
  o.local = corner;

  // Outside the planes actually LOADED → a degenerate quad. A range, not one plane, because the 3D view
  // can be cropped to part of the stack: filtering on a single plane there would draw the whole stack's
  // cells against a box that only holds eight of its planes. The 2D view passes lo == hi. Negative lo
  // means no filter at all.
  if (p.ov.y >= 0.0 && (plane < p.ov.y - 0.5 || plane > p.ov.w + 0.5)) {
    o.pos = vec4(0.0, 0.0, 2.0, 1.0);     // behind the far plane: clipped, no fragments
    return o;
  }

  let aspect = p.vp.y / max(p.vp.z, 1.0);
  let c = camera();
  let ndc = project(centre - boxCentre(), c, aspect);
  // Pixels → NDC. The quad is square ON SCREEN, so the x offset divides by the canvas WIDTH and the y
  // by the height; using one for both stretches the marker with the window's aspect.
  // The quad is grown by the black-outline width so the border sits OUTSIDE the fill — a fill radius
  // of p.ov.x px and an outer radius of (p.ov.x + p.lab.w) px. Zero border keeps the old quad size.
  let px = p.ov.x + max(p.lab.w, 0.0);
  o.pos = vec4(ndc.x + corner.x * (2.0 * px / max(p.vp.y, 1.0)),
               ndc.y + corner.y * (2.0 * px / max(p.vp.z, 1.0)),
               0.0, 1.0);
  return o;
}

@fragment fn fs(in: POut) -> @location(0) vec4<f32> {
  // A round marker, and antialiased: a hard-edged square of colour over a noisy MIP reads as an
  // artefact rather than as an annotation.
  let r = length(in.local);
  let a = 1.0 - smoothstep(0.75, 1.0, r);
  if (a <= 0.001) { discard; }
  // Fill/border seam. local runs -1..1 across a quad of outer radius (size + border) px, so the
  // fill ends at ratio size/(size+border). Sharp on purpose: a soft ring against a coloured fill
  // reads as blur, not as an outline.
  let border = max(p.lab.w, 0.0);
  if (border > 0.0) {
    let inner = p.ov.x / max(p.ov.x + border, 0.0001);
    if (r > inner) { return vec4(0.0, 0.0, 0.0, a); }
  }
  return vec4(in.rgb, a);
}
