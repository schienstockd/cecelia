// The tile view's uniform block and projection, shared VERBATIM by the tile pass and its two overlay
// passes (points, track tails). Same discipline as `mip_common.wgsl`: one copy `#include`d into every
// pass, so a marker sits on the cell the tile pass drew rather than beside it.
//
// The per-frame uniform block (`uniforms.json` → "tile"). Small — everything a tile draw needs about
// camera, channels and overlays — and sized so a `minBindingSize` check catches a layout drift without
// a probe frame.
//
// `cam`   x = camera pan X (µm, screen right), y = camera pan Y (µm, screen up),
//         z = camera dist (µm; half-height at unit-µm-per-µm ortho = `dist * VIEW_HALF_ANGLE`),
//         w = aspect (canvas w / h).
// `vp`    x = channel count, y = canvas W (px), z = canvas H (px), w = unused.
// `ext`   x = image extent X µm, y = image extent Y µm, z/w = unused.
// `slot`  x = channels per slot (nC — how far to step in z per channel), y/z/w = unused.
// `lab`   x = mask opacity (0 = off), y = contour width (px, 0 = filled), z = palette rows,
//         w = POINT border width (px, 0 = no outline). Same lanes as the MIP shader's `lab`, which is
//         what lets `label_colour.wgsl` read it.
// `ov`    points: x = size (px), y = first plane shown, z = tail width (px), w = last plane shown.
//         Negative y = no plane filter. Same lanes as the MIP block's `ov`.
// `ovx`   x/y = track ribbon first/last plane (negative x = no filter), z/w = unused.
// `ch[]`  per-channel window: x = lo, y = hi, z = visible (0/1), w = unused.

struct P {
  cam:  vec4<f32>,
  vp:   vec4<f32>,
  ext:  vec4<f32>,
  slot: vec4<f32>,
  lab:  vec4<f32>,
  ov:   vec4<f32>,
  ovx:  vec4<f32>,
  ch:   array<vec4<f32>, ${MAX_CHANNELS}>,
};
@group(0) @binding(0) var<uniform> p: P;

// World µm (image plane centred on the origin, screen up = -y world) → NDC. Orthographic, same
// convention as the MIP shader's project():
//   halfH = dist * VIEW_HALF_ANGLE, halfW = halfH * aspect
//   ndc.x = (wx - panX) / halfW,    ndc.y = -(wy + panY) / halfH
fn tileNdc(w: vec2<f32>) -> vec2<f32> {
  let halfH = p.cam.z * ${VIEW_HALF_ANGLE};
  let halfW = halfH * max(p.cam.w, 1e-6);
  return vec2((w.x - p.cam.x) / max(halfW, 1e-6), -(w.y + p.cam.y) / max(halfH, 1e-6));
}

// Absolute image µm (what the overlay buffers carry) → the centred world the tiles are drawn in.
fn overlayNdc(abs: vec3<f32>) -> vec2<f32> {
  return tileNdc(abs.xy - p.ext.xy * 0.5);
}
