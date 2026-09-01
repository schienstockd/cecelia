// ── Brick-atlas raycast shader (P5b) ───────────────────────────────────────────────
//
// One full-screen triangle, one fragment per pixel, marches a ray through the box the same way
// the flat renderer does. The DIFFERENCE is where a sample comes from: instead of one 3D texture
// covering the whole volume, each sample looks its brick up in the page table, misses through
// unmapped bricks (transparent — the tick loop will populate them), and reads the resident
// bricks out of the atlas 3D texture.
//
// Overlays (points + track tails) share this uniform buffer and live in the SAME render pass as
// the raycast — a second camera would draw a marker next to its cell rather than on it and still
// look plausible. See `mipShader.ts` for the vertical-flip note (`up = cross(right, fwd)`, not
// the other way round) — the same discipline applies here so the two renderers put row 0 at the
// top identically.

import { VIEW_HALF_ANGLE, MAX_CHANNELS, LUT_STOPS } from '../../utils/volumeViewer'

/**
 * Sentinel written into the page table for an unmapped brick. Matches `pageTable.ts`'s
 * "not resident" convention on the JS side — a scheduler that resets an entry writes this. WGSL
 * cannot express `0xFFFFFFFFu` as a `const` from a template literal cleanly so it's inlined at
 * the two use sites.
 */
export const EMPTY_SLOT = 0xFFFFFFFF

/**
 * Uniform buffer size in bytes. Twelve leading vec4s (camera + geometry + overlays + labels +
 * prev-level) + one vec4 per channel slot. `EXT.w` used to carry a global normalisation ceiling;
 * per-channel contrast windows now live in `p.ch[c]` (lo, hi, visible, unused), same shape the
 * flat renderer's `mipShader.ts` uses.
 */
export const BRICK_UNIFORM_BYTES = 12 * 16 + MAX_CHANNELS * 16

/**
 * Field offsets INTO the uniform buffer, in f32 slots (× 4 = bytes). Written out because getting
 * one off-by-one shifts everything downstream — same discipline as `CH0` in `volumeRenderer.ts`.
 */
export const BU = {
  CAM: 0,        // yaw, pitch, dist, steps
  VP: 4,         // nch, canvasW, canvasH, ortho
  EXT: 8,        // extX, extY, extZ, zOriginUm (0 in an uncropped volume)
  DIMS: 12,      // nX, nY, nZ (voxels at CURRENT level), unused
  BRICK: 16,     // brickX, brickY, brickZ, channelsPerBrick
  ATLAS: 20,     // atlasW, atlasH, atlasD (voxels), slotsX
  GRID: 24,      // nBx, nBy, nBz (bricks per axis, current level), slotsY
  PAN: 28,       // panX, panY, ribbon planeLo, ribbon planeHi
  OV: 32,        // point size (px), first plane shown, tail width (px), last plane shown
  LAB: 36,       // opacity (0 = off), contour width (px, 0 = filled), palette rows, unused
  PREV_GRID: 40, // prevNBx, prevNBy, prevNBz, prevValid (0.0 = no fallback)
  PREV_DIMS: 44, // prevNX, prevNY, prevNZ (voxels at PREVIOUS level), unused
  /** Per-channel `(lo, hi, visible, unused)`. `visible < 0.5` means "skip this channel". */
  CH0: 48,
}

/**
 * The uniform struct + camera basis + projection, shared VERBATIM by the raycast and the overlay
 * passes. Same discipline as `SHARED_WGSL` in `mipShader.ts`: one copy interpolated into every
 * pass so a marker drawn by `project()` sits on the cell drawn by the raycast rather than beside
 * it. Vertical-flip lives in `up = cross(right, fwd)` — see the note there.
 */
const BRICK_SHARED_WGSL = `
struct BU {
  cam:      vec4<f32>,  // yaw, pitch, dist, steps
  vp:       vec4<f32>,  // nch, canvasW, canvasH, ortho
  ext:      vec4<f32>,  // extX, extY, extZ, zOriginUm
  dims:     vec4<f32>,  // nX, nY, nZ (current level), _
  brick:    vec4<f32>,  // brickX, brickY, brickZ, channelsPerBrick
  atlas:    vec4<f32>,  // atlasW, atlasH, atlasD, slotsX
  grid:     vec4<f32>,  // nBx, nBy, nBz (current level), slotsY
  pan:      vec4<f32>,  // panX, panY, ribbon planeLo, ribbon planeHi
  ov:       vec4<f32>,  // point size px, first plane, tail width px, last plane
  lab:      vec4<f32>,  // opacity (0 = off), contour px (0 = filled), palette rows, POINT border px (0 = no outline)
  prevGrid: vec4<f32>,  // prevNBx, prevNBy, prevNBz, prevValid (0.0 = no fallback)
  prevDims: vec4<f32>,  // prevNX, prevNY, prevNZ (previous level), _
  ch:       array<vec4<f32>, ${MAX_CHANNELS}>,  // per-channel (lo, hi, visible, unused)
};
@group(0) @binding(0) var<uniform> p: BU;

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

// World µm → clip space, the exact inverse of the ray construction. Returns w along the view
// axis so a caller can size a point under perspective. Matches mipShader.ts's project() so a
// user toggling between renderers gets the same on-screen coordinates.
fn project(world: vec3<f32>, c: Cam, aspect: f32) -> vec3<f32> {
  let d = world - c.ro;
  let sx = dot(d, c.right);
  let sy = dot(d, c.up);
  if (p.vp.w > 0.5) {
    let hh = p.cam.z * ${VIEW_HALF_ANGLE};
    return vec3(sx / (hh * aspect), sy / hh, 1.0);
  }
  let w = max(dot(d, -c.fwd), 1e-4);
  return vec3(sx / (w * ${VIEW_HALF_ANGLE} * aspect), sy / (w * ${VIEW_HALF_ANGLE}), w);
}

// Centre of the LOADED box in absolute image um. Overlays are absolute; the raycast is centred on
// the origin. ext.w is the z origin -- 0 for an uncropped volume, non-zero when the brick
// renderer loads a subrange (plane mode / cropped 3D).
fn boxCentre() -> vec3<f32> {
  return vec3(p.ext.x * 0.5, p.ext.y * 0.5, p.ext.w + p.ext.z * 0.5);
}
`

export const BRICK_WGSL = `
${BRICK_SHARED_WGSL}
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
// Label atlas: r32uint, same slot layout as the image atlas but ONE plane per slot in Z (labels
// have no channels). Same page-table entries — a resident brick has BOTH intensity and labels
// (or a placeholder for the label atlas when labels are off). A 1x1x1 placeholder is bound when
// no segmentation is picked, and the shader skips label sampling entirely at p.lab.x == 0.
@group(0) @binding(5) var labAtlas: texture_3d<u32>;
// Label palette: LABEL_PALETTE_N x 1 rgba8. id % rows -- consecutive ids get consecutive rows,
// so touching cells always come out maximally far apart in hue.
@group(0) @binding(6) var pal: texture_2d<f32>;

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

/**
 * Sample the LABEL atlas for voxel vi. Shares the page-table lookup with atlasSample: labels
 * bricks land in the SAME slot as their intensity twin, so one lookup gates both. Returns 0 for
 * an out-of-box read AND for an unmapped brick -- an unresident brick has no label either.
 *
 * Unlike atlasSample, the label atlas has NO per-channel Z stride -- one plane per brick along Z.
 * The prev-level fallback is skipped here: sampling a coarser-level label at a finer position
 * looks correct until two neighbouring cells straddle the coarser voxel and get swapped ids.
 */
fn labAtlasSample(vi: vec3<i32>) -> u32 {
  let nx = i32(p.dims.x); let ny = i32(p.dims.y); let nz = i32(p.dims.z);
  if (vi.x < 0 || vi.y < 0 || vi.z < 0 || vi.x >= nx || vi.y >= ny || vi.z >= nz) { return 0u; }
  let bxSize = i32(p.brick.x); let bySize = i32(p.brick.y); let bzSize = i32(p.brick.z);
  let bx = vi.x / bxSize;
  let by = vi.y / bySize;
  let bz = vi.z / bzSize;
  let nBx = i32(p.grid.x); let nBy = i32(p.grid.y); let nBz = i32(p.grid.z);
  if (bx >= nBx || by >= nBy || bz >= nBz) { return 0u; }
  let slot = pt[(bz * nBy + by) * nBx + bx];
  if (slot == 0xFFFFFFFFu) { return 0u; }
  let slotsX = i32(p.atlas.w);
  let slotsY = i32(p.grid.w);
  let s = i32(slot);
  let sx = s % slotsX;
  let sy = (s / slotsX) % slotsY;
  let sz = s / (slotsX * slotsY);
  let lx = vi.x - bx * bxSize;
  let ly = vi.y - by * bySize;
  let lz = vi.z - bz * bzSize;
  return textureLoad(labAtlas,
    vec3<i32>(sx * bxSize + lx, sy * bySize + ly, sz * bzSize + lz), 0).r;
}

// napari's contour: the label's OUTLINE, w voxels thick, in-plane only (x/y). Mirrors
// mipShader.ts's labEdge — filled at w = 0 (napari's default), which draws the region rather
// than the boundary.
fn labEdge(vi: vec3<i32>, id: u32, w: i32) -> bool {
  if (w <= 0) { return true; }
  for (var k = 1; k <= w; k = k + 1) {
    if (labAtlasSample(vi + vec3<i32>(k, 0, 0)) != id ||
        labAtlasSample(vi - vec3<i32>(k, 0, 0)) != id ||
        labAtlasSample(vi + vec3<i32>(0, k, 0)) != id ||
        labAtlasSample(vi - vec3<i32>(0, k, 0)) != id) { return true; }
  }
  return false;
}

// id % rows on the one-row palette. Id 0 never reaches here so every row is available.
fn labColour(id: u32) -> vec3<f32> {
  let rows = max(i32(p.lab.z), 1);
  return textureLoad(pal, vec2<i32>(i32(id % u32(rows)), 0), 0).rgb;
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
  // The NEAREST label along the ray -- front-to-back, first non-zero id wins. Mirrors
  // mipShader.ts: a max over label ids is meaningless because id ordering is not brightness, and
  // in 2D (steps == 1) the single sample IS the plane's mask.
  var labId: u32 = 0u;
  var labVi = vec3<i32>(0, 0, 0);
  for (var s = 0; s < n; s = s + 1) {
    let wp = org + rd * (t0 + (f32(s) + 0.5) * dt);
    let uvw = (wp + h) / p.ext.xyz;
    let vi = vec3<i32>(uvw * p.dims.xyz);
    if (p.lab.x > 0.0 && labId == 0u) {
      let id = labAtlasSample(vi);
      if (id != 0u) { labId = id; labVi = vi; }
    }
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
  // Label composite: mix the id's palette colour on top of the raycast result at p.lab.x. The
  // ray already found the front-most id; labEdge decides whether THIS voxel is on the contour.
  // No cascade to the outer channels -- napari draws the mask on top of the signal.
  if (labId != 0u && labEdge(labVi, labId, i32(p.lab.y))) {
    acc = mix(min(acc, vec3(1.0)), labColour(labId), p.lab.x);
  }
  return vec4(min(acc, vec3(1.0)), 1.0);
}
`

/**
 * Overlay points, alpha-blended over the raycast in the SAME pass. Two triangles per instance,
 * scaled in SCREEN pixels via `p.ov.x` so a marker stays legible when zoomed out and doesn't
 * swallow the cell when zoomed in. Plane filter uses `p.ov.y`/`p.ov.w` (the loaded z range);
 * negative `p.ov.y` disables the filter. Mirrors `POINTS_WGSL` in `mipShader.ts` — same camera,
 * same project(), so a marker sits on the cell rather than beside it.
 */
export const BRICK_POINTS_WGSL = `
${BRICK_SHARED_WGSL}

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
  // Quad grown by the black-outline width so the border sits OUTSIDE the fill. Mirrors POINTS_WGSL
  // in mipShader.ts — same uniform slot (p.lab.w), same encoding.
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
`

/**
 * Track tails. One screen-space quad per segment, widened perpendicular to the SCREEN-space
 * direction so the width is in pixels and stays constant under perspective. Own plane bounds
 * (`p.pan.z`/`p.pan.w`) so ribbons can be widened independently of the points' z reach. Mirrors
 * `SEGMENTS_WGSL` in `mipShader.ts`.
 */
export const BRICK_SEGMENTS_WGSL = `
${BRICK_SHARED_WGSL}

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
`
