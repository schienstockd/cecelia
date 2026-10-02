// The MIP raycast shader. One pass, one full-screen triangle, no geometry: every fragment marches a
// ray through the volume and keeps each channel's maximum, then colours it through that channel's LUT
// and adds the channels together — the same additive composite `image_render.jl` does on the CPU.
//
// Measured on an RTX 2000 Ada with real data at 1566x1003, 4 channels, 256 steps: 5.3 ms/frame,
// against the viewer's 36.0 ms for the same view (docs/archive/napari-webgpu-audit.md → G2).
//
// IT ALSO DRAWS THE 2D VIEW, and that is deliberate rather than lazy. A single z plane is a volume one
// plane deep seen face-on: `steps = 1` samples the box midpoint, which IS that plane, exactly. So there
// is no second renderer, no second contrast path and no second palette — the duplication this codebase
// keeps warning about. What the 2D view does need is ORTHOGRAPHIC projection: under perspective a flat
// plane is foreshortened towards the edges, which is wrong for a view people measure on. The two share
// one framing convention (half-height = 0.45 x dist) so toggling between them does not jump.
//
// WHY `textureLoad` AND NOT A SAMPLER on the volume. The volume is `r16uint`, which WebGPU classes as
// non-filterable, so it cannot be sampled with interpolation at all. That is deliberate: MIP takes a
// maximum, which needs no interpolation, and converting the fetched slab to `r16float` on the CPU
// costs 973 ms — more than the entire read and decode. If smooth sampling is ever wanted it happens on
// the GPU, not on the wire (WEB_VIEWER_PLAN.md decision 2).
//
// The LUT is a separate `rgba8unorm` 2D texture, one row per channel, and it is read with
// `textureLoad` + an explicit lerp between two stops rather than through a sampler. A sampler is the
// obvious choice and it is the wrong one: WebGPU filtering has no per-axis control, so `linear` would
// interpolate across the CHANNEL rows as well as along the ramp. It happens to be exact while
// MAX_CHANNELS is a power of two — `(c + 0.5) / 8` round-trips in f32 — so the bug would appear the day
// someone changes that constant, as a faint bleed of the next channel's colour. Doing the lerp here is
// exact for any row count, drops a binding, and stops the LUT needing to be filterable at all.
//
// Channel colours must never be derived from a colormap NAME on this side — the server resolves them,
// because a name table here is a second copy of the viewer's palette and the first copy being incomplete
// rendered a channel white.
//
// Used by `volumeRenderer.ts` (browser) and the movie host (`python/cecelia/utils/wgsl_utils.py`).

#include "mip_common.wgsl"
@group(0) @binding(1) var vol: texture_3d<u32>;
@group(0) @binding(2) var lut: texture_2d<f32>;
// The segmentation mask for the SAME timepoint and the same planes — uploaded in the same slot as the
// image, so a mask can never be one frame behind the pixels it outlines. 'r32uint': real label stores
// are UInt32 and an id is an identity, not a quantity, so there is nothing to interpolate.
@group(0) @binding(3) var lab: texture_3d<u32>;
// Label colours, one row: 'id % rows'. Consecutive ids get consecutive rows, and the rows are
// golden-angle hues, so cells labelled next to each other come out maximally far apart in hue — which
// is the property that matters when two touching cells must be told apart.
@group(0) @binding(4) var pal: texture_2d<f32>;
// Pick highlight — bitset + focus id + contour width in one storage buffer. All the correction-cockpit
// "what am I editing right now" work rides through this binding; the shader's uniform struct stays
// untouched, which is what lets the flat and brick renderers share the same snippet
// (utils/viewerLabels.ts) — only the binding NUMBER differs.
#include "pick.wgsl" PICK_BINDING=MIP_PICK_BINDING

struct VOut { @builtin(position) pos: vec4<f32>, @location(0) uv: vec2<f32> };

// One oversized triangle covers the viewport with three vertices and no vertex buffer.
@vertex fn vs(@builtin(vertex_index) i: u32) -> VOut {
  var xy = array<vec2<f32>, 3>(vec2(-1.0, -1.0), vec2(3.0, -1.0), vec2(-1.0, 3.0));
  var o: VOut;
  o.pos = vec4(xy[i], 0.0, 1.0);
  o.uv = xy[i];
  return o;
}

// Slab method: entry/exit distance along the ray for an axis-aligned box of half-extent h.
fn hitBox(ro: vec3<f32>, rd: vec3<f32>, h: vec3<f32>) -> vec2<f32> {
  let inv = 1.0 / rd;
  let a = min((-h - ro) * inv, (h - ro) * inv);
  let b = max((-h - ro) * inv, (h - ro) * inv);
  return vec2(max(max(a.x, a.y), a.z), min(min(b.x, b.y), b.z));
}

// Channel c's ramp at normalised intensity n: lerp between the two stops n falls between, on row c.
// Row c is addressed exactly, so no filtering can reach row c+1 (see the header).
fn ramp(c: i32, n: f32) -> vec3<f32> {
  let p = clamp(n, 0.0, 1.0) * (${LUT_STOPS}.0 - 1.0);
  let i = i32(floor(p));
  let j = min(i + 1, ${LUT_STOPS} - 1);
  let f = p - floor(p);
  let a = textureLoad(lut, vec2<i32>(i, c), 0).rgb;
  let b = textureLoad(lut, vec2<i32>(j, c), 0).rgb;
  return mix(a, b, f);
}

// The label id at a voxel, 0 outside the loaded box. Out of range reads as BACKGROUND deliberately: a
// cell touching the edge of the slab then draws its outline along that edge rather than losing it.
fn labAt(vi: vec3<i32>) -> u32 {
  if (vi.x < 0 || vi.y < 0 || vi.z < 0 ||
      vi.x >= i32(p.dims.x) || vi.y >= i32(p.dims.y) || vi.z >= i32(p.dims.z)) { return 0u; }
  return textureLoad(lab, vi, 0).r;
}

// the viewer's 'contour': the label's OUTLINE, w voxels thick, instead of a filled region — which is what
// lets the channel signal under the mask stay readable while the boundary stays exact. Filled at w = 0,
// which is the viewer's default and this one. In-plane only (x/y): the outline of a 3D object through its
// z neighbours is a surface, not a contour, and would fill the region back in.
fn labEdge(vi: vec3<i32>, id: u32, w: i32) -> bool {
  if (w <= 0) { return true; }
  for (var k = 1; k <= w; k = k + 1) {
    if (labAt(vi + vec3<i32>(k, 0, 0)) != id || labAt(vi - vec3<i32>(k, 0, 0)) != id ||
        labAt(vi + vec3<i32>(0, k, 0)) != id || labAt(vi - vec3<i32>(0, k, 0)) != id) { return true; }
  }
  return false;
}

// 'id % rows' on the one-row palette. Id 0 never reaches here, so every row is available to real cells.
// A NEGATIVE row count switches to a colour TABLE: `pal` is W = -rows texels wide and as many rows
// as it needs, id at (id % W, id / W), and alpha 0 = this label is not drawn. That is how a movie
// draws a population-filtered, population-coloured mask with this pass; the viewer always sends the
// palette's row count, which takes the branch it always took.
fn labColour(id: u32) -> vec4<f32> {
  let rows = i32(p.lab.z);
  if (rows < 0) {
    let w = u32(-rows);
    let row = id / w;
    if (row >= textureDimensions(pal).y) { return vec4(0.0); }
    return textureLoad(pal, vec2<i32>(i32(id % w), i32(row)), 0);
  }
  return vec4(textureLoad(pal, vec2<i32>(i32(id % u32(max(rows, 1))), 0), 0).rgb, 1.0);
}

// Whether a label is drawn at all: always on the palette, the table's alpha otherwise. A hidden label
// is skipped by the march, so the nearest DRAWN one along the ray is what shows.
fn labShown(id: u32) -> bool {
  return p.lab.z >= 0.0 || labColour(id).a > 0.0;
}

@fragment fn fs(in: VOut) -> @location(0) vec4<f32> {
  let h = p.ext.xyz * 0.5;
  let c = camera();
  let fwd = c.fwd; let ro = c.ro; let right = c.right; let up = c.up;
  let aspect = p.vp.y / max(p.vp.z, 1.0);

  // Orthographic moves the ray ORIGIN across the image plane and holds the direction constant;
  // perspective holds the origin and fans the direction. Same half-height either way, so the two frame
  // the volume identically at the centre and the toggle does not jump.
  var org = ro;
  var rd = -fwd;
  if (p.vp.w > 0.5) {
    let hh = p.cam.z * ${VIEW_HALF_ANGLE};
    org = ro + right * (in.uv.x * hh * aspect) + up * (in.uv.y * hh);
  } else {
    rd = normalize(-fwd + right * (in.uv.x * ${VIEW_HALF_ANGLE} * aspect)
                       + up * (in.uv.y * ${VIEW_HALF_ANGLE}));
  }

  let t = hitBox(org, rd, h);
  let t0 = max(t.x, 0.0);
  if (t.y <= t0) { return vec4(0.0, 0.0, 0.0, 1.0); }

  let n = i32(p.cam.w);
  let dt = (t.y - t0) / f32(n);
  let zpc = i32(p.dims.w);
  let nch = min(i32(p.vp.x), ${MAX_CHANNELS});

  var mx = array<f32, ${MAX_CHANNELS}>();
  // The NEAREST label along the ray, not the maximum. A maximum over label ids is meaningless — the
  // largest id is not a visible feature, it is whichever cell happened to be labelled last — and viewer
  // cannot project a Labels layer at all ('projection_mode' accepts only 'none'), so there is no
  // behaviour to copy either. The ray marches front to back, so the first non-zero id IS the nearest
  // surface, which is the one thing a person can actually point at. In 2D ('steps == 1') the single
  // sample is the plane, so the same line gives the exact per-plane mask with no second path.
  var labId: u32 = 0u;
  var labVi = vec3<i32>(0, 0, 0);
  for (var s = 0; s < n; s = s + 1) {
    let wp = org + rd * (t0 + (f32(s) + 0.5) * dt);
    let uvw = (wp + h) / p.ext.xyz;
    let vi = vec3<i32>(uvw * p.dims.xyz);
    if (vi.x < 0 || vi.y < 0 || vi.z < 0 ||
        vi.x >= i32(p.dims.x) || vi.y >= i32(p.dims.y) || vi.z >= i32(p.dims.z)) { continue; }
    if (p.lab.x > 0.0 && labId == 0u) {
      let id = textureLoad(lab, vi, 0).r;
      if (id != 0u && labShown(id)) { labId = id; labVi = vi; }
    }
    for (var c = 0; c < nch; c = c + 1) {
      // Channels are stacked along z in ONE texture, so a channel is a z offset of zpc planes.
      let v = f32(textureLoad(vol, vec3<i32>(vi.x, vi.y, vi.z + c * zpc), 0).r);
      mx[c] = max(mx[c], v);
    }
  }

  var acc = vec3(0.0);
  for (var c = 0; c < nch; c = c + 1) {
    if (p.ch[c].z < 0.5) { continue; }
    let win = clamp((mx[c] - p.ch[c].x) / max(p.ch[c].y - p.ch[c].x, 1.0), 0.0, 1.0);
    acc = acc + ramp(c, win);
  }
  // The mask goes OVER the composite at its opacity, the way viewer layers a Labels layer over the
  // image — not added to it. Adding would brighten the signal it is meant to annotate, and two masks
  // over one bright cell would saturate to white.
  if (labId != 0u && p.lab.x > 0.0 && labEdge(labVi, labId, i32(p.lab.y))) {
    acc = mix(min(acc, vec3(1.0)), labColour(labId).rgb, p.lab.x);
  }
  // Pick highlight sits ON TOP of everything above — the whole point of "what am I editing right now"
  // is that it is legible without the user having to hunt. Focus wins over pick (both may be true).
  // Solid colour override at the outline pixels; interior falls through untouched so the cell body
  // still shows the signal + any normal label draw.
  if (labId != 0u) {
    let pw = labPickContourPx();
    if (pw > 0 && labEdge(labVi, labId, pw)) {
      if (labIsFocus(labId)) { acc = vec3(0.2, 1.0, 1.0); }
      else if (labInPick(labId)) { acc = vec3(1.0, 1.0, 1.0); }
    }
  }
  return vec4(min(acc, vec3(1.0)), 1.0);
}
