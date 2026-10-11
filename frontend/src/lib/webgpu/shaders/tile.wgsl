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
//
// The uniform block and the projection live in `tile_common.wgsl`, shared with the overlay passes.

#include "tile_common.wgsl"
@group(0) @binding(1) var atlas: texture_3d<u32>;
@group(0) @binding(2) var lut: texture_2d<f32>;
// The segmentation mask, one 'r32uint' layer per slot — layer `slot` holds the mask for the tile in
// image slot `slot`, so a tile and its mask can never be drawn from different places. Its own texture
// rather than more image layers: ids are u32 (a whole-slide segmentation passes 65535 cells) while the
// image is u8/u16, and a mask is refetched on its own when it changes (a task preview re-run) without
// touching the image bytes.
@group(0) @binding(3) var labAtlas: texture_3d<u32>;
@group(0) @binding(4) var pal: texture_2d<f32>;

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
  // 1 when the slot's mask layer holds the mask on screen, 0 when it is absent or stale — a tile drawn
  // before its mask lands shows the image alone rather than another tile's mask.
  @location(4) labOn: f32,
};

struct VOut {
  @builtin(position) pos: vec4<f32>,
  @location(0) uv: vec2<f32>,            // 0..1 across the tile — the local sample coord
  @location(1) @interpolate(flat) slot: i32,
  @location(2) @interpolate(flat) sampledPx: vec2<f32>,
  @location(3) @interpolate(flat) labOn: f32,
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

  var o: VOut;
  o.pos = vec4(tileNdc(vec2(wx, wy)), 0.0, 1.0);
  o.uv = corner;
  o.slot = i32(inst.slotIdx);
  o.sampledPx = inst.sampledPx;
  o.labOn = inst.labOn;
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

// The tile `labAt` reads from, set by `fs` before it calls `labEdge`: the shared contour loop hands its
// fetch a voxel only, and the tile's extent and the id under test are per-tile, not per-voxel.
var<private> labTileSampled: vec2<f32>;
var<private> labTileId: u32;

// The mask id at (x, y) in layer `vi.z` — the tile's slot. Outside the tile reads as the id under test:
// the neighbour lives in another slot, and reading it as background would outline every cell along
// every tile seam.
fn labAt(vi: vec3<i32>) -> u32 {
  if (vi.x < 0 || vi.y < 0 || vi.x >= i32(labTileSampled.x) || vi.y >= i32(labTileSampled.y)) { return labTileId; }
  return textureLoad(labAtlas, vi, 0).r;
}

// The contour width is in this level's pixels.
#include "label_edge.wgsl" LAB_AT=labAt

#include "label_colour.wgsl"

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
  // The mask goes OVER the composite at its opacity — same compositing as the MIP shader.
  if (p.lab.x > 0.0 && in.labOn > 0.5) {
    let id = textureLoad(labAtlas, vec3<i32>(sx, sy, in.slot), 0).r;
    labTileSampled = in.sampledPx;
    labTileId = id;
    if (id != 0u && labShown(id) && labEdge(vec3<i32>(sx, sy, in.slot), id, i32(p.lab.y))) {
      acc = mix(min(acc, vec3(1.0)), labColour(id).rgb, p.lab.x);
    }
  }
  return vec4(min(acc, vec3(1.0)), 1.0);
}
