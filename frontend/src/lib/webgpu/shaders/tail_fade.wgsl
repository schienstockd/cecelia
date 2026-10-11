// Track-tail fade: a segment's opacity from how long ago it arrived, so a tail reads as a trail in
// time — the newest hop solid, older hops fading out — as napari and Imaris draw them. Without it
// every hop of a tail is the same weight and a path has no direction: a cell that went A→B and one
// that went B→A draw identically.
//
// `p.trail` = (head, length, _, _): `head` is the newest END timepoint the tail shows (the frame on
// screen + 1 — see `tailRange` in `utils/viewerOverlays.ts` for why), `length` the tail in frames.
// A segment ending at `head` is age 0 and draws at the peak; the oldest one shown (age length - 1)
// draws at 1/length of the way up from the floor. The floor keeps the far end of a long tail
// visible rather than letting it vanish into a noisy MIP. `length <= 0` = no fade (peak).
//
// Included by `mip_segments.wgsl` and `brick_segments.wgsl` AFTER their common file, which declares
// `p` with a `trail` field in both blocks (`uniforms.json`).

fn tailAlpha(tEnd: f32) -> f32 {
  if (p.trail.y <= 0.0) { return f32(${TAIL_ALPHA_PEAK}); }
  let f = clamp(1.0 - (p.trail.x - tEnd) / p.trail.y, 0.0, 1.0);
  return mix(f32(${TAIL_ALPHA_FLOOR}), f32(${TAIL_ALPHA_PEAK}), f);
}
