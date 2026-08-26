// The tile pass's WGSL. Same LUT/contrast machinery as the MIP shader, different geometry: one
// instanced quad per resident tile, sampled from a shared 3D atlas texture.
//
// WHY A 3D ATLAS RATHER THAN N `texture_2d` OR A `texture_2d_array`. Same shape as the volume
// renderer's stacked-channel-in-z texture, and for the same reason: ONE binding, so a bind group does
// not need rebuilding when the visible tile set changes, and the shader loops channels with a `zpc`
// offset rather than through a per-channel binding — `maxSampledTexturesPerShaderStage` is 16 on
// Chrome/Dawn and would refuse a bind group at nC=17. Layer index is `slot * nC + c`, so
// `maxTextureDimension3D` (2048 baseline / 16384 on modern discrete) bounds `slots × nC` — comfortable
// for the ~32-slot cache the pan/zoom viewer needs.
//
// WHY `textureLoad` NOT A SAMPLER. `r16uint` is non-filterable in WebGPU, same reason as the volume:
// converting u16 → r16float on the CPU cost 973 ms on real data (audit's G4). MIP takes a maximum which
// wants no interpolation; the tile view's edges show a couple of pixel-blocky texels at deep zoom,
// which is a fair trade for the wire cost.
//
// THE COORDINATE MODEL. Image plane is centred at the origin in µm, exactly as the volume renderer
// draws it — half-extent = `ext.xy * 0.5`, screen up is -y in world, orthographic projection with
// half-height = `dist * VIEW_HALF_ANGLE`. Pan is µm across the screen's axes, matching `panDrag`. So
// the tile's WORLD rect (µm at the top-left corner of the tile, µm width, µm height) projects to NDC
// with the same expressions the MIP shader inverts for its overlay `project()` — the two views cannot
// drift apart as long as both use `VIEW_HALF_ANGLE` and the same pan convention.

import { MAX_CHANNELS, LUT_STOPS, VIEW_HALF_ANGLE } from '../../utils/volumeViewer'

/**
 * The per-frame uniform block. Small — everything a tile draw needs about camera and channels — and
 * sized so a `minBindingSize` check catches a layout drift without a probe frame.
 *
 * `cam`   x = camera pan X (µm, screen right), y = camera pan Y (µm, screen up),
 *         z = camera dist (µm; half-height at unit-µm-per-µm ortho = `dist * VIEW_HALF_ANGLE`),
 *         w = aspect (canvas w / h).
 * `vp`    x = channel count, y = canvas W (px), z = canvas H (px), w = unused.
 * `ext`   x = image extent X µm, y = image extent Y µm, z/w = unused.
 * `slot`  x = channels per slot (nC — how far to step in z per channel), y/z/w = unused.
 * `ch[]`  per-channel window: x = lo, y = hi, z = visible (0/1), w = unused.
 */
export const TILE_UNIFORM_BYTES = 4 * 16 + MAX_CHANNELS * 16

export const TILE_WGSL = `
struct P {
  cam:  vec4<f32>,
  vp:   vec4<f32>,
  ext:  vec4<f32>,
  slot: vec4<f32>,
  ch:   array<vec4<f32>, ${MAX_CHANNELS}>,
};
@group(0) @binding(0) var<uniform> p: P;
@group(0) @binding(1) var atlas: texture_3d<u32>;
@group(0) @binding(2) var lut: texture_2d<f32>;

struct Inst {
  // World rect in µm (top-left corner + width/height). Screen up is -y in world, so a tile with the
  // SMALLEST worldY sits at the TOP of the screen — matches the image row 0 convention.
  @location(0) worldXY: vec2<f32>,
  @location(1) worldWH: vec2<f32>,
  // How much of the slot actually carries pixels: an edge tile is smaller than the chunk. sampled.xy
  // (px) tells the fragment how far to walk into the atlas layer. NOT the same as worldWH in pixels
  // because a coarser LEVEL tile is smaller on disk per µm but drawn full-size for its level's µm rect.
  @location(2) sampledPx: vec2<f32>,
  // Which slot in the atlas holds this tile. Channels are stacked, so channel c reads layer
  // slot * nC + c at the tile's (px, py).
  @location(3) slotIdx: f32,
};

struct VOut {
  @builtin(position) pos: vec4<f32>,
  @location(0) uv: vec2<f32>,            // 0..1 across the tile — the local sample coord
  @location(1) @interpolate(flat) slot: i32,
  @location(2) @interpolate(flat) sampledPx: vec2<f32>,
};

// Two triangles, corners in [0, 1] with (0, 0) at the top-left of the tile (uv y grows DOWNWARD in
// image coords). Written out for readability — this is read far more often than executed.
@vertex fn vs(@builtin(vertex_index) vi: u32, inst: Inst) -> VOut {
  var q = array<vec2<f32>, 6>(
    vec2(0.0, 0.0), vec2(1.0, 0.0), vec2(0.0, 1.0),
    vec2(0.0, 1.0), vec2(1.0, 0.0), vec2(1.0, 1.0));
  let corner = q[vi];

  // World µm position of this vertex.
  let wx = inst.worldXY.x + corner.x * inst.worldWH.x;
  let wy = inst.worldXY.y + corner.y * inst.worldWH.y;

  // Orthographic project — same convention as the MIP shader's overlay project().
  //   ro.xy = (panX, -panY)          (screen up = -y world)
  //   halfH = dist * VIEW_HALF_ANGLE
  //   halfW = halfH * aspect
  //   ndc.x = (wx - panX) / halfW
  //   ndc.y = -(wy + panY) / halfH
  let halfH = p.cam.z * ${VIEW_HALF_ANGLE};
  let halfW = halfH * max(p.cam.w, 1e-6);
  let ndcX = (wx - p.cam.x) / max(halfW, 1e-6);
  let ndcY = -(wy + p.cam.y) / max(halfH, 1e-6);

  var o: VOut;
  o.pos = vec4(ndcX, ndcY, 0.0, 1.0);
  o.uv = corner;
  o.slot = i32(inst.slotIdx);
  o.sampledPx = inst.sampledPx;
  return o;
}

// Channel c's ramp at normalised intensity n: exact addressing of row c, lerp between the two stops.
// Same expression as the MIP shader — a second copy would drift; kept identical.
fn ramp(c: i32, n: f32) -> vec3<f32> {
  let pn = clamp(n, 0.0, 1.0) * (${LUT_STOPS}.0 - 1.0);
  let i = i32(floor(pn));
  let j = min(i + 1, ${LUT_STOPS} - 1);
  let f = pn - floor(pn);
  let a = textureLoad(lut, vec2<i32>(i, c), 0).rgb;
  let b = textureLoad(lut, vec2<i32>(j, c), 0).rgb;
  return mix(a, b, f);
}

@fragment fn fs(in: VOut) -> @location(0) vec4<f32> {
  // Local pixel coordinate in the tile's atlas layer. sampledPx is what was actually written; the
  // rest of the layer may hold stale bytes from a previous tenant, so an edge tile MUST clamp its read
  // rather than march past its own extent. Floor to nearest — non-filterable format, so this is what
  // the driver would do anyway; written out because the clamp needs the integer form.
  let px = i32(floor(in.uv.x * in.sampledPx.x));
  let py = i32(floor(in.uv.y * in.sampledPx.y));
  let sx = clamp(px, 0, i32(in.sampledPx.x) - 1);
  let sy = clamp(py, 0, i32(in.sampledPx.y) - 1);

  let nch = min(i32(p.vp.x), ${MAX_CHANNELS});
  let zpc = i32(p.slot.x);                        // channels per slot — how far to step in z per c
  let base = in.slot * zpc;

  var acc = vec3(0.0);
  for (var c = 0; c < nch; c = c + 1) {
    if (p.ch[c].z < 0.5) { continue; }
    let v = f32(textureLoad(atlas, vec3<i32>(sx, sy, base + c), 0).r);
    let win = clamp((v - p.ch[c].x) / max(p.ch[c].y - p.ch[c].x, 1.0), 0.0, 1.0);
    acc = acc + ramp(c, win);
  }
  return vec4(min(acc, vec3(1.0)), 1.0);
}
`
