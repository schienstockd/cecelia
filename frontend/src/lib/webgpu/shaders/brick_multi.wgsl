#include "brick_common.wgsl"
// Current-level page table — slots span all N atlases (Decision 2 of the parent plan).
// atlasSample decodes global slot → (atlasIndex, localSlot) below.
@group(0) @binding(${PT_BINDING}) var<storage, read> pt: array<u32>;
${ATLAS_DECLS}
// Previous-level page table — same convention as pt, indexed by the OLDER level's grid.
@group(0) @binding(${PREV_PT_BINDING}) var<storage, read> prevPt: array<u32>;
@group(0) @binding(${LUT_BINDING}) var lut: texture_2d<f32>;
// Label atlas grew to N (S2) — each per-atlas slot grid has its own r32uint texture. The
// P2 orphan-brick gate in kickLabelFetch is dropped in the same change; label bricks now
// route to labAtlas[atlasIndex] alongside their intensity twin.
${LAB_ATLAS_DECLS}
@group(0) @binding(${PAL_BINDING}) var pal: texture_2d<f32>;
#include "pick.wgsl"

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

// Dispatch to the right atlas via switch(atlasIndex). Per S0 (2026-09-17, PR #941) this
// compiles and executes on Chromium/Dawn Vulkan; the fallback shape (if/else if) is
// documented in the plan should any future backend reject it.
fn sampleAtlas(atlasIndex: u32, coord: vec3<i32>) -> u32 {
  switch atlasIndex {
${ATLAS_SWITCH}
  }
}

/**
 * Multi-atlas version of atlasSample: same page-table + prev-level fallback as N=1, but the
 * final read decodes the GLOBAL slot into (atlasIndex, localSlot) and dispatches through
 * sampleAtlas. Slot geometry is derived from the atlas dims and brick shape — no new uniform
 * field required, so BU stays untouched between N=1 and N>=2 variants.
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
  let nC = i32(p.brick.w);
  // slotsZ derived from the per-atlas physical Z size: atlasD = brickZ * nC * slotsZ.
  // Homogeneous layouts (parent plan Decision 3) mean every atlas has the same slotsZ.
  let slotsZ = max(i32(p.atlas.z) / max(bzSize * nC, 1), 1);
  let perAtlas = u32(slotsX * slotsY * slotsZ);
  let atlasIdx = slot / perAtlas;
  let localSlot = i32(slot % perAtlas);
  let sx = localSlot % slotsX;
  let sy = (localSlot / slotsX) % slotsY;
  let sz = localSlot / (slotsX * slotsY);
  let originX = sx * bxSize;
  let originY = sy * bySize;
  let originZBase = sz * bzSize * nC;
  return sampleAtlas(atlasIdx,
    vec3<i32>(originX + lx, originY + ly, originZBase + ch * bzSize + lz));
}

// Label atlas: N textures (S2), one per intensity atlas, same slot grid but no channel
// stacking (labels are single-channel, r32uint). Decodes (atlasIndex, localSlot) from the
// shared page-table slot and dispatches through sampleLabAtlas — same shape as the intensity
// path. The old P2 orphan-brick guard is retired in the same change.
fn sampleLabAtlas(atlasIndex: u32, coord: vec3<i32>) -> u32 {
  switch atlasIndex {
${LAB_ATLAS_SWITCH}
  }
}

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
  let nC = i32(p.brick.w);
  let slotsZ = max(i32(p.atlas.z) / max(bzSize * nC, 1), 1);
  let perAtlas = u32(slotsX * slotsY * slotsZ);
  let atlasIdx = slot / perAtlas;
  let localSlot = i32(slot % perAtlas);
  let sx = localSlot % slotsX;
  let sy = (localSlot / slotsX) % slotsY;
  let sz = localSlot / (slotsX * slotsY);
  let lx = vi.x - bx * bxSize;
  let ly = vi.y - by * bySize;
  let lz = vi.z - bz * bzSize;
  return sampleLabAtlas(atlasIdx,
    vec3<i32>(sx * bxSize + lx, sy * bySize + ly, sz * bzSize + lz));
}

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

fn labColour(id: u32) -> vec3<f32> {
  let rows = max(i32(p.lab.z), 1);
  return textureLoad(pal, vec2<i32>(i32(id % u32(rows)), 0), 0).rgb;
}

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
    if (p.ch[ci].z < 0.5) { continue; }
    let lo = p.ch[ci].x;
    let hi = p.ch[ci].y;
    let win = clamp((mx[ci] - lo) / max(hi - lo, 1.0), 0.0, 1.0);
    acc = acc + ramp(ci, win);
  }
  if (labId != 0u && p.lab.x > 0.0 && labEdge(labVi, labId, i32(p.lab.y))) {
    acc = mix(min(acc, vec3(1.0)), labColour(labId), p.lab.x);
  }
  if (labId != 0u) {
    let pw = labPickContourPx();
    if (pw > 0 && labEdge(labVi, labId, pw)) {
      if (labIsFocus(labId)) { acc = vec3(0.2, 1.0, 1.0); }
      else if (labInPick(labId)) { acc = vec3(1.0, 1.0, 1.0); }
    }
  }
  return vec4(min(acc, vec3(1.0)), 1.0);
}
