// ── Brick-atlas raycast shader (P5b) ───────────────────────────────────────────────
//
// One full-screen triangle, one fragment per pixel, marches a ray through the box the same way
// the flat renderer does. The DIFFERENCE is where a sample comes from: instead of one 3D texture
// covering the whole volume, each sample looks its brick up in the page table, misses through
// unmapped bricks (transparent — the tick loop will populate them), and reads the resident
// bricks out of the atlas 3D texture.
//
// P5b scope: max-projection per channel + a fixed per-channel colour (rgb wheel). No LUT, no
// labels, no overlays — those follow the same uniform block shape as `mipShader.ts` so P5d/P6
// can graft them in without a second uniform buffer. Same VIEW_HALF_ANGLE and same camera basis
// so the toggling between flat and brick renderers doesn't jump the framing.
//
// SEE `mipShader.ts` for the vertical-flip note (`up = cross(right, fwd)`, not the other way
// round) — the same discipline applies here so the two renderers put row 0 at the top identically.

import { VIEW_HALF_ANGLE, MAX_CHANNELS, LUT_STOPS } from '../../utils/volumeViewer'

/**
 * Sentinel written into the page table for an unmapped brick. Matches `pageTable.ts`'s
 * "not resident" convention on the JS side — a scheduler that resets an entry writes this. WGSL
 * cannot express `0xFFFFFFFFu` as a `const` from a template literal cleanly so it's inlined at
 * the two use sites.
 */
export const EMPTY_SLOT = 0xFFFFFFFF

/**
 * Uniform buffer size in bytes. Ten leading vec4s (camera + geometry + prev-level) + one
 * vec4 per channel slot. `EXT.w` used to carry a global normalisation ceiling; per-channel
 * contrast windows now live in `p.ch[c]` (lo, hi, visible, unused), same shape the flat
 * renderer's `mipShader.ts` uses.
 */
export const BRICK_UNIFORM_BYTES = 10 * 16 + MAX_CHANNELS * 16

/**
 * Field offsets INTO the uniform buffer, in f32 slots (× 4 = bytes). Written out because getting
 * one off-by-one shifts everything downstream — same discipline as `CH0` in `volumeRenderer.ts`.
 */
export const BU = {
  CAM: 0,        // yaw, pitch, dist, steps
  VP: 4,         // nch, canvasW, canvasH, ortho
  EXT: 8,        // extX, extY, extZ, unused
  DIMS: 12,      // nX, nY, nZ (voxels at CURRENT level), unused
  BRICK: 16,     // brickX, brickY, brickZ, channelsPerBrick
  ATLAS: 20,     // atlasW, atlasH, atlasD (voxels), slotsX
  GRID: 24,      // nBx, nBy, nBz (bricks per axis, current level), slotsY
  PAN: 28,       // panX, panY, unused, unused
  PREV_GRID: 32, // prevNBx, prevNBy, prevNBz, prevValid (0.0 = no fallback)
  PREV_DIMS: 36, // prevNX, prevNY, prevNZ (voxels at PREVIOUS level), unused
  /** Per-channel `(lo, hi, visible, unused)`. `visible < 0.5` means "skip this channel". */
  CH0: 40,
}

export const BRICK_WGSL = `
struct BU {
  cam:      vec4<f32>,  // yaw, pitch, dist, steps
  vp:       vec4<f32>,  // nch, canvasW, canvasH, ortho
  ext:      vec4<f32>,  // extX, extY, extZ, _
  dims:     vec4<f32>,  // nX, nY, nZ (current level), _
  brick:    vec4<f32>,  // brickX, brickY, brickZ, channelsPerBrick
  atlas:    vec4<f32>,  // atlasW, atlasH, atlasD, slotsX
  grid:     vec4<f32>,  // nBx, nBy, nBz (current level), slotsY
  pan:      vec4<f32>,  // panX, panY, _, _
  prevGrid: vec4<f32>,  // prevNBx, prevNBy, prevNBz, prevValid (0.0 = no fallback)
  prevDims: vec4<f32>,  // prevNX, prevNY, prevNZ (previous level), _
  ch:       array<vec4<f32>, ${MAX_CHANNELS}>,  // per-channel (lo, hi, visible, unused)
};
@group(0) @binding(0) var<uniform> p: BU;
// Current-level page table: (bz * nBy + by) * nBx + bx → slot index or 0xFFFFFFFF (not resident).
@group(0) @binding(1) var<storage, read> pt: array<u32>;
@group(0) @binding(2) var atlas: texture_3d<u32>;
// Previous-level page table: same shape, indexed by the OLDER level's grid dimensions
// (p.prevGrid.xyz). On a level switch, the current pt is copied here so old-level bricks stay
// visible until the new level's replacements land — Kiln's zoom-threshold trick, same shape as
// the 2D tile renderer's progressive-refinement pattern. Ignored when p.prevGrid.w < 0.5.
@group(0) @binding(3) var<storage, read> prevPt: array<u32>;
// LUT: MAX_CHANNELS rows × LUT_STOPS pixels wide. Row c is channel c's ramp resampled. Read
// via textureLoad + explicit lerp between two stops (a sampler would interpolate across the
// channel rows too — same reason mipShader.ts avoids sampling).
@group(0) @binding(4) var lut: texture_2d<f32>;

struct Cam { fwd: vec3<f32>, right: vec3<f32>, up: vec3<f32>, ro: vec3<f32> };
fn camera() -> Cam {
  let cy = cos(p.cam.x); let sy = sin(p.cam.x);
  let cp = cos(p.cam.y); let sp = sin(p.cam.y);
  var c: Cam;
  c.fwd = vec3(cp * sy, sp, cp * cy);
  c.right = normalize(cross(vec3(0.0, 1.0, 0.0), c.fwd));
  // Same vertical-flip discipline as mipShader.ts — cross(right, fwd), not cross(fwd, right).
  c.up = cross(c.right, c.fwd);
  c.ro = c.fwd * p.cam.z + c.right * p.pan.x + c.up * p.pan.y;
  return c;
}

struct VOut { @builtin(position) pos: vec4<f32>, @location(0) uv: vec2<f32> };
@vertex fn vs(@builtin(vertex_index) i: u32) -> VOut {
  var xy = array<vec2<f32>, 3>(vec2(-1.0, -1.0), vec2(3.0, -1.0), vec2(-1.0, 3.0));
  var o: VOut;
  o.pos = vec4(xy[i], 0.0, 1.0);
  o.uv = xy[i];
  return o;
}

fn hitBox(ro: vec3<f32>, rd: vec3<f32>, h: vec3<f32>) -> vec2<f32> {
  let inv = 1.0 / rd;
  let a = min((-h - ro) * inv, (h - ro) * inv);
  let b = max((-h - ro) * inv, (h - ro) * inv);
  return vec2(max(max(a.x, a.y), a.z), min(min(b.x, b.y), b.z));
}

/**
 * Sample the atlas for voxel index (vi, ch) at L0. Returns 0 for an out-of-box read (the ray
 * marches past the sides deliberately, so this is common), and 0 for an unmapped brick — an
 * unmapped brick is the "not loaded yet" state, and rendering it as zero means the visible
 * region grows in as the fetch loop catches up rather than flashing chunks of colour.
 */
fn atlasSample(vi: vec3<i32>, ch: i32) -> u32 {
  let nx = i32(p.dims.x); let ny = i32(p.dims.y); let nz = i32(p.dims.z);
  if (vi.x < 0 || vi.y < 0 || vi.z < 0 || vi.x >= nx || vi.y >= ny || vi.z >= nz) { return 0u; }
  let bxSize = i32(p.brick.x); let bySize = i32(p.brick.y); let bzSize = i32(p.brick.z);
  let bx = vi.x / bxSize;
  let by = vi.y / bySize;
  let bz = vi.z / bzSize;
  let nBx = i32(p.grid.x); let nBy = i32(p.grid.y); let nBz = i32(p.grid.z);
  var slot: u32 = 0xFFFFFFFFu;
  var lx = vi.x - bx * bxSize;
  var ly = vi.y - by * bySize;
  var lz = vi.z - bz * bzSize;
  if (bx < nBx && by < nBy && bz < nBz) {
    slot = pt[(bz * nBy + by) * nBx + bx];
  }
  // Prev-level fallback: no current-level brick here, but a coarser (or finer) level's brick
  // covers the same world position. Convert the vi index across levels using the ratio of
  // voxel counts, look up the prev grid, use those coords if it lands.
  if (slot == 0xFFFFFFFFu && p.prevGrid.w > 0.5) {
    let pnx = i32(p.prevDims.x); let pny = i32(p.prevDims.y); let pnz = i32(p.prevDims.z);
    let vpx = i32(f32(vi.x) * p.prevDims.x / p.dims.x);
    let vpy = i32(f32(vi.y) * p.prevDims.y / p.dims.y);
    let vpz = i32(f32(vi.z) * p.prevDims.z / p.dims.z);
    if (vpx >= 0 && vpy >= 0 && vpz >= 0 && vpx < pnx && vpy < pny && vpz < pnz) {
      let pbx = vpx / bxSize;
      let pby = vpy / bySize;
      let pbz = vpz / bzSize;
      let pnBx = i32(p.prevGrid.x); let pnBy = i32(p.prevGrid.y); let pnBz = i32(p.prevGrid.z);
      if (pbx < pnBx && pby < pnBy && pbz < pnBz) {
        let ps = prevPt[(pbz * pnBy + pby) * pnBx + pbx];
        if (ps != 0xFFFFFFFFu) {
          slot = ps;
          lx = vpx - pbx * bxSize;
          ly = vpy - pby * bySize;
          lz = vpz - pbz * bzSize;
        }
      }
    }
  }
  if (slot == 0xFFFFFFFFu) { return 0u; }
  let slotsX = i32(p.atlas.w);
  let slotsY = i32(p.grid.w);
  let s = i32(slot);
  let sx = s % slotsX;
  let sy = (s / slotsX) % slotsY;
  let sz = s / (slotsX * slotsY);
  let originX = sx * bxSize;
  let originY = sy * bySize;
  let nC = i32(p.brick.w);
  let originZBase = sz * bzSize * nC;
  return textureLoad(atlas,
    vec3<i32>(originX + lx, originY + ly, originZBase + ch * bzSize + lz), 0).r;
}

// Channel c's ramp at normalised intensity n. Same discipline as mipShader.ts's ramp: lerp
// between the two LUT stops n falls between, row c addressed exactly, so no filtering can bleed
// across into row c+1. Exact for ANY row count, no MAX_CHANNELS assumption.
fn ramp(c: i32, n: f32) -> vec3<f32> {
  let q = clamp(n, 0.0, 1.0) * (${LUT_STOPS}.0 - 1.0);
  let i = i32(floor(q));
  let j = min(i + 1, ${LUT_STOPS} - 1);
  let f = q - floor(q);
  let a = textureLoad(lut, vec2<i32>(i, c), 0).rgb;
  let b = textureLoad(lut, vec2<i32>(j, c), 0).rgb;
  return mix(a, b, f);
}

@fragment fn fs(in: VOut) -> @location(0) vec4<f32> {
  let h = p.ext.xyz * 0.5;
  let c = camera();
  let aspect = p.vp.y / max(p.vp.z, 1.0);

  var org = c.ro;
  var rd = -c.fwd;
  if (p.vp.w > 0.5) {
    let hh = p.cam.z * ${VIEW_HALF_ANGLE};
    org = c.ro + c.right * (in.uv.x * hh * aspect) + c.up * (in.uv.y * hh);
  } else {
    rd = normalize(-c.fwd + c.right * (in.uv.x * ${VIEW_HALF_ANGLE} * aspect)
                         + c.up * (in.uv.y * ${VIEW_HALF_ANGLE}));
  }

  let t = hitBox(org, rd, h);
  let t0 = max(t.x, 0.0);
  if (t.y <= t0) { return vec4(0.0, 0.0, 0.0, 1.0); }

  let n = i32(p.cam.w);
  let dt = (t.y - t0) / f32(n);
  let nch = min(i32(p.vp.x), ${MAX_CHANNELS});

  var acc = vec3(0.0);
  var mx = array<f32, ${MAX_CHANNELS}>();
  for (var s = 0; s < n; s = s + 1) {
    let wp = org + rd * (t0 + (f32(s) + 0.5) * dt);
    let uvw = (wp + h) / p.ext.xyz;
    let vi = vec3<i32>(uvw * p.dims.xyz);
    for (var ci = 0; ci < nch; ci = ci + 1) {
      let v = f32(atlasSample(vi, ci));
      mx[ci] = max(mx[ci], v);
    }
  }
  for (var ci = 0; ci < nch; ci = ci + 1) {
    // Skip channels flagged invisible — same convention as mipShader.ts's per-channel visible bit.
    if (p.ch[ci].z < 0.5) { continue; }
    let lo = p.ch[ci].x;
    let hi = p.ch[ci].y;
    let win = clamp((mx[ci] - lo) / max(hi - lo, 1.0), 0.0, 1.0);
    acc = acc + ramp(ci, win);
  }
  return vec4(min(acc, vec3(1.0)), 1.0);
}
`
