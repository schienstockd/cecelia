// The screen-space quad maths every overlay pass shares — points and track tails, for the MIP, brick
// and tile renderers alike. CAMERA-AGNOSTIC: each pass projects with its own camera and hands NDC in,
// so this holds no uniform access and one copy serves three uniform blocks.
//
// SIZES ARE SCREEN PIXELS. A cell marker is annotation: it has to stay legible when you zoom out and
// must not swallow the cell when you zoom in — a µm-sized quad gets that backwards. Tails are quads
// rather than `line-list` because WebGPU draws 1px lines only.

// Where an instance outside its plane window goes: behind the far plane, so it is clipped and emits
// no fragments. Collapsing the quad rather than CPU-filtering keeps a z step free of re-uploads.
fn clippedPos() -> vec4<f32> { return vec4(0.0, 0.0, 2.0, 1.0); }

// Outside planes [lo, hi], ± half a plane. Negative `lo` means no filter (3D over the whole stack).
fn offPlanes(plane: f32, lo: f32, hi: f32) -> bool {
  return lo >= 0.0 && (plane < lo - 0.5 || plane > hi + 0.5);
}

// Corner `vi` of a marker quad — two triangles, corners in -1..1. Written out rather than computed
// from bit tricks: this is read far more often than it is executed.
fn markerCorner(vi: u32) -> vec2<f32> {
  var q = array<vec2<f32>, 6>(
    vec2(-1.0, -1.0), vec2(1.0, -1.0), vec2(-1.0, 1.0),
    vec2(-1.0,  1.0), vec2(1.0, -1.0), vec2( 1.0, 1.0));
  return q[vi];
}

// A marker corner in clip space. The quad is square ON SCREEN — x divides by the canvas width, y by
// its height — and grown by the border so the outline sits OUTSIDE a fill of `sizePx`.
fn markerPos(ndc: vec2<f32>, corner: vec2<f32>, sizePx: f32, borderPx: f32, canvas: vec2<f32>) -> vec4<f32> {
  let px = sizePx + max(borderPx, 0.0);
  return vec4(ndc.x + corner.x * (2.0 * px / max(canvas.x, 1.0)),
              ndc.y + corner.y * (2.0 * px / max(canvas.y, 1.0)),
              0.0, 1.0);
}

// A round, antialiased marker: a hard-edged square over a noisy image reads as an artefact. Alpha 0
// outside the disc — the caller discards. The fill/border seam is sharp on purpose: a soft ring
// against a coloured fill reads as blur, not as an outline.
fn markerColour(local: vec2<f32>, rgb: vec3<f32>, sizePx: f32, borderPx: f32) -> vec4<f32> {
  let r = length(local);
  let a = 1.0 - smoothstep(0.75, 1.0, r);
  if (a <= 0.001) { return vec4(0.0); }
  let border = max(borderPx, 0.0);
  if (border > 0.0) {
    let inner = sizePx / max(sizePx + border, 0.0001);
    if (r > inner) { return vec4(0.0, 0.0, 0.0, a); }
  }
  return vec4(rgb, a);
}

// Corner `vi` of a tail quad from NDC `pa` to `pb`, `widthPx` wide. Widened perpendicular to the
// SCREEN-space direction, in pixels, so the width stays constant under perspective.
fn ribbonPos(pa: vec2<f32>, pb: vec2<f32>, vi: u32, widthPx: f32, canvas: vec2<f32>) -> vec4<f32> {
  let sa = vec2(pa.x * canvas.x, pa.y * canvas.y) * 0.5;
  let sb = vec2(pb.x * canvas.x, pb.y * canvas.y) * 0.5;
  var dir = sb - sa;
  let len = length(dir);
  // A zero-length segment has no direction to be perpendicular to; pick one rather than emit NaN,
  // which propagates to the whole quad and shows up as a stray triangle across the frame.
  dir = select(vec2(1.0, 0.0), dir / max(len, 1e-6), len > 1e-6);
  let nrm = vec2(-dir.y, dir.x) * (widthPx * 0.5);
  var corner = array<vec2<f32>, 6>(
    vec2(0.0, -1.0), vec2(1.0, -1.0), vec2(0.0, 1.0),
    vec2(0.0,  1.0), vec2(1.0, -1.0), vec2(1.0, 1.0));
  let k = corner[vi];
  let sp2 = mix(sa, sb, k.x) + nrm * k.y;
  return vec4(sp2.x * 2.0 / max(canvas.x, 1.0), sp2.y * 2.0 / max(canvas.y, 1.0), 0.0, 1.0);
}
