// The channel lens: a circle on the 2D view (docs/UI.md → *Channel lens*). One copy, `#include`d by
// every pass that draws the 2D view — the tile, flat and brick composites, their point and track
// passes, and the movie renderer — so they cannot disagree about where it is or what it draws.
//
// `p.lens`  x/y = centre, z = radius — framebuffer px, top-left origin (what `@builtin(position)`
//           reads in a fragment); z = 0 = no lens. w = rim width (px).
// `p.ch[c]` z = channel drawn outside the lens, w = picked for the lens (the aperture).
//
// Two modes, decided here from the picks so no host can disagree:
//   - channels picked: the lens draws THEM, the rest of the view draws the visible ones.
//   - none picked: a spotlight — the lens shows the view as it is, everything outside it dims
//     (to `LENS_DIM`, the ROI spotlight's level), points and tracks included.

/** Whether any channel is picked for the lens. */
fn lensPicked() -> bool {
  let n = min(i32(p.vp.x), ${MAX_CHANNELS});
  for (var c = 0; c < n; c = c + 1) {
    if (p.ch[c].w > 0.5) { return true; }
  }
  return false;
}

/** Whether the lens is on and `frag` is inside it. */
fn lensOver(frag: vec2<f32>) -> bool {
  return p.lens.z > 0.0 && distance(frag, p.lens.xy) <= p.lens.z;
}

/** Whether `frag` draws the lens's own channel set — inside it, with channels picked. */
fn lensIn(frag: vec2<f32>) -> bool {
  return lensOver(frag) && lensPicked();
}

/** Whether channel `c` is drawn at a fragment that draws the lens set (`inLens`) or not. */
fn chOn(c: i32, inLens: bool) -> bool {
  return select(p.ch[c].z, p.ch[c].w, inLens) > 0.5;
}

/** The brightness factor at `frag`: outside a spotlight lens, `LENS_DIM` as seen on screen — the
 *  target is sRGB, so the linear factor is its 2.2 power (0.35 seen ≈ 0.10 linear); else 1. */
fn lensShade(frag: vec2<f32>) -> f32 {
  if (p.lens.z <= 0.0 || lensOver(frag) || lensPicked()) { return 1.0; }
  return pow(${LENS_DIM}, 2.2);
}

/** The finished colour with the spotlight dim and the lens rim — a light ring, so the edge reads on
 *  any image. */
fn lensRim(rgb: vec3<f32>, frag: vec2<f32>) -> vec3<f32> {
  if (p.lens.z <= 0.0) { return rgb; }
  let half = p.lens.w * 0.5;
  let d = abs(distance(frag, p.lens.xy) - p.lens.z);
  return mix(rgb * lensShade(frag), vec3(1.0), (1.0 - smoothstep(half, half + 1.0, d)) * 0.8);
}
