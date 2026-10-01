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
// look plausible. See `mip_common.wgsl` for the vertical-flip note (`up = cross(right, fwd)`, not
// the other way round) — the same discipline applies here so the two renderers put row 0 at the
// top identically.
//
// The single-atlas variant (N = 1). `brick_multi.wgsl` is the N = 2..4 twin; `brickShader.ts`
// fills its generated atlas declarations and switch arms.

#include "brick_common.wgsl"
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
// channel rows too — same reason mip.wgsl avoids sampling).
@group(0) @binding(4) var lut: texture_2d<f32>;
// Label atlas: r32uint, same slot layout as the image atlas but ONE plane per slot in Z (labels
// have no channels). Same page-table entries — a resident brick has BOTH intensity and labels
// (or a placeholder for the label atlas when labels are off). A 1x1x1 placeholder is bound when
// no segmentation is picked, and the shader skips label sampling entirely at p.lab.x == 0.
@group(0) @binding(5) var labAtlas: texture_3d<u32>;
// Label palette: LABEL_PALETTE_N x 1 rgba8. id % rows -- consecutive ids get consecutive rows,
// so touching cells always come out maximally far apart in hue.
@group(0) @binding(6) var pal: texture_2d<f32>;
// Pick highlight — bitset + focus id + contour width in one storage buffer. All the correction-cockpit
// "what am I editing right now" work rides through this binding; BU stays untouched, which is what
// lets the flat and brick renderers share the same snippet (utils/viewerLabels.ts) — only the
// binding NUMBER differs.
#include "pick.wgsl" PICK_BINDING=BRICK_PICK_BINDING

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

// the viewer's contour: the label's OUTLINE, w voxels thick, in-plane only (x/y). Mirrors
// mip.wgsl's labEdge — filled at w = 0 (the viewer's default), which draws the region rather
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

// Channel c's ramp at normalised intensity n. Same discipline as mip.wgsl's ramp: lerp
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
  // mip.wgsl: a max over label ids is meaningless because id ordering is not brightness, and
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
    // Skip channels flagged invisible — same convention as mip.wgsl's per-channel visible bit.
    if (p.ch[ci].z < 0.5) { continue; }
    let lo = p.ch[ci].x;
    let hi = p.ch[ci].y;
    let win = clamp((mx[ci] - lo) / max(hi - lo, 1.0), 0.0, 1.0);
    acc = acc + ramp(ci, win);
  }
  // Label composite: mix the id's palette colour on top of the raycast result at p.lab.x. The
  // ray already found the front-most id; labEdge decides whether THIS voxel is on the contour.
  // No cascade to the outer channels -- viewer draws the mask on top of the signal.
  if (labId != 0u && p.lab.x > 0.0 && labEdge(labVi, labId, i32(p.lab.y))) {
    acc = mix(min(acc, vec3(1.0)), labColour(labId), p.lab.x);
  }
  // Pick highlight sits ON TOP of everything above — the whole point of "what am I editing right now"
  // is that it is legible without the user having to hunt. Focus wins over pick (both may be true).
  // See mip.wgsl for the twin.
  if (labId != 0u) {
    let pw = labPickContourPx();
    if (pw > 0 && labEdge(labVi, labId, pw)) {
      if (labIsFocus(labId)) { acc = vec3(0.2, 1.0, 1.0); }
      else if (labInPick(labId)) { acc = vec3(1.0, 1.0, 1.0); }
    }
  }
  return vec4(min(acc, vec3(1.0)), 1.0);
}
