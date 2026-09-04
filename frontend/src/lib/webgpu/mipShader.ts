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

import { MAX_CHANNELS, LUT_STOPS, VIEW_HALF_ANGLE } from '../../utils/volumeViewer'

/**
 * The uniform layout and the camera, shared verbatim by all three passes.
 *
 * ONE COPY, interpolated, because there were three and that is exactly how a convention drifts. The
 * raycast builds rays from this basis and the overlay passes invert it; if the two ever disagree by a
 * sign, a marker sits beside the cell it marks and still looks plausible.
 *
 * THE VERTICAL FLIP IS HERE, in `up`. `cross(right, fwd)` rather than `cross(fwd, right)` — i.e. screen
 * up is -y in world. Image row 0 must appear at the TOP, as it does in viewer and in every image
 * viewer, and the naive basis puts it at the bottom: WebGPU's NDC y points up while a framebuffer's
 * rows count down from the top, so a right-handed basis and a raster image disagree by exactly this
 * sign. Derived rather than guessed (screen top mapped to the LAST texture row), and asserted by
 * `docs/todo/spike/webgpu/shader_check.mjs` — the orientation check there is the one-click proof.
 */
const SHARED_WGSL = `
struct P {
  cam:  vec4<f32>,                       // yaw, pitch, dist, steps
  vp:   vec4<f32>,                       // channel count, canvas w, canvas h, orthographic
  ext:  vec4<f32>,                       // physical extent x, y, z; w = z origin of the loaded slab (µm)
  dims: vec4<f32>,                       // nx, ny, nz, z-planes per channel
  ov:   vec4<f32>,                       // overlays: point size (px), first plane shown, tail width, last plane shown
  lab:  vec4<f32>,                       // labels: opacity (0 = off), contour width (px, 0 = filled), palette rows, POINT border width (px, 0 = no outline)
  pan:  vec4<f32>,                       // pan xy (screen µm), then z/w = track ribbon planeLo/planeHi
  ch:   array<vec4<f32>, ${MAX_CHANNELS}>, // per channel: lo, hi, visible, unused
};
@group(0) @binding(0) var<uniform> p: P;

struct Cam { fwd: vec3<f32>, right: vec3<f32>, up: vec3<f32>, ro: vec3<f32> };
fn camera() -> Cam {
  let cy = cos(p.cam.x); let sy = sin(p.cam.x);
  let cp = cos(p.cam.y); let sp = sin(p.cam.y);
  var c: Cam;
  c.fwd = vec3(cp * sy, sp, cp * cy);
  c.right = normalize(cross(vec3(0.0, 1.0, 0.0), c.fwd));
  // screen up is -y in world: see the note above. Written as the cross product in the other order
  // rather than as a negation, so there is no minus sign for someone to 'tidy away'.
  c.up = cross(c.right, c.fwd);
  // PAN moves the eye across the screen's own axes. Here rather than at the ray, because project()
  // inverts this same origin — so the overlays pan with the pixels and cannot drift apart. Moving the
  // eye rather than the box is also what keeps the pan correct once the view is rotated: 'right' is
  // wherever right currently is, which at yaw 90 degrees runs along world z.
  c.ro = c.fwd * p.cam.z + c.right * p.pan.x + c.up * p.pan.y;
  return c;
}

// World µm → clip space, the exact inverse of the ray construction. 'w' (distance along the view axis)
// is returned so a caller can size a point under perspective.
fn project(world: vec3<f32>, c: Cam, aspect: f32) -> vec3<f32> {
  let d = world - c.ro;
  let sx = dot(d, c.right);
  let sy = dot(d, c.up);
  if (p.vp.w > 0.5) {                            // orthographic: constant half-height
    let hh = p.cam.z * ${VIEW_HALF_ANGLE};
    return vec3(sx / (hh * aspect), sy / hh, 1.0);
  }
  let w = max(dot(d, -c.fwd), 1e-4);             // perspective: half-height grows with distance
  return vec3(sx / (w * ${VIEW_HALF_ANGLE} * aspect), sy / (w * ${VIEW_HALF_ANGLE}), w);
}

// The centre of the LOADED box in absolute image µm. The volume is drawn centred on the origin, so an
// overlay coordinate — which is absolute — has to be shifted by this. 'ext.w' carries the z origin
// because a cropped 3D view starts partway up the stack.
fn boxCentre() -> vec3<f32> {
  return vec3(p.ext.x * 0.5, p.ext.y * 0.5, p.ext.w + p.ext.z * 0.5);
}
`

export const MIP_WGSL = `
${SHARED_WGSL}
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
fn labColour(id: u32) -> vec3<f32> {
  let rows = max(i32(p.lab.z), 1);
  return textureLoad(pal, vec2<i32>(i32(id % u32(rows)), 0), 0).rgb;
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
      if (id != 0u) { labId = id; labVi = vi; }
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
  if (labId != 0u && labEdge(labVi, labId, i32(p.lab.y))) {
    acc = mix(min(acc, vec3(1.0)), labColour(labId), p.lab.x);
  }
  return vec4(min(acc, vec3(1.0)), 1.0);
}
`

/**
 * The overlay pass: population points as camera-facing quads, drawn over the finished MIP.
 *
 * ONE INSTANCE PER POINT, six vertices generated in the shader — no vertex buffer for the quad, and no
 * geometry uploaded per frame. The instance data is built once per (image, populations) and ordered by
 * timepoint, so drawing a frame is `draw(6, count, 0, first)` over a contiguous range.
 *
 * IT SHARES THE RAYCAST'S UNIFORM BUFFER, and therefore its camera, deliberately: `project()` is the
 * exact inverse of the ray construction, so a point lands on the voxel it was measured from at any yaw,
 * pitch or zoom. A second camera copy for the overlays would be one number away from marking the wrong
 * cell, and it would drift silently — the overlay would still look plausible.
 *
 * SIZE IS IN SCREEN PIXELS, not µm. A cell marker is annotation: it has to stay legible when you zoom
 * out and must not swallow the cell when you zoom in — which is what the viewer's `points_size` does, and
 * what a µm-sized quad would get backwards.
 *
 * The plane filter collapses the quad to zero area rather than being a CPU filter, because the 2D view
 * changes plane from a slider: rebuilding and re-uploading the buffer per z step is exactly the cost
 * the sorted-by-timepoint layout exists to avoid.
 */
export const POINTS_WGSL = `
${SHARED_WGSL}

struct POut {
  @builtin(position) pos: vec4<f32>,
  @location(0) rgb: vec3<f32>,
  @location(1) local: vec2<f32>,          // -1..1 across the quad, for the round mask
};

@vertex fn vs(
  @builtin(vertex_index) vi: u32,
  @location(0) centre: vec3<f32>,         // absolute image µm
  @location(1) rgb: vec3<f32>,
  @location(2) plane: f32,
) -> POut {
  var o: POut;
  o.rgb = rgb;
  // Two triangles, corners in -1..1. Written out rather than computed from bit tricks: this is read
  // far more often than it is executed.
  var q = array<vec2<f32>, 6>(
    vec2(-1.0, -1.0), vec2(1.0, -1.0), vec2(-1.0, 1.0),
    vec2(-1.0,  1.0), vec2(1.0, -1.0), vec2( 1.0, 1.0));
  let corner = q[vi];
  o.local = corner;

  // Outside the planes actually LOADED → a degenerate quad. A range, not one plane, because the 3D view
  // can be cropped to part of the stack: filtering on a single plane there would draw the whole stack's
  // cells against a box that only holds eight of its planes. The 2D view passes lo == hi. Negative lo
  // means no filter at all.
  if (p.ov.y >= 0.0 && (plane < p.ov.y - 0.5 || plane > p.ov.w + 0.5)) {
    o.pos = vec4(0.0, 0.0, 2.0, 1.0);     // behind the far plane: clipped, no fragments
    return o;
  }

  let aspect = p.vp.y / max(p.vp.z, 1.0);
  let c = camera();
  let ndc = project(centre - boxCentre(), c, aspect);
  // Pixels → NDC. The quad is square ON SCREEN, so the x offset divides by the canvas WIDTH and the y
  // by the height; using one for both stretches the marker with the window's aspect.
  // The quad is grown by the black-outline width so the border sits OUTSIDE the fill — a fill radius
  // of p.ov.x px and an outer radius of (p.ov.x + p.lab.w) px. Zero border keeps the old quad size.
  let px = p.ov.x + max(p.lab.w, 0.0);
  o.pos = vec4(ndc.x + corner.x * (2.0 * px / max(p.vp.y, 1.0)),
               ndc.y + corner.y * (2.0 * px / max(p.vp.z, 1.0)),
               0.0, 1.0);
  return o;
}

@fragment fn fs(in: POut) -> @location(0) vec4<f32> {
  // A round marker, and antialiased: a hard-edged square of colour over a noisy MIP reads as an
  // artefact rather than as an annotation.
  let r = length(in.local);
  let a = 1.0 - smoothstep(0.75, 1.0, r);
  if (a <= 0.001) { discard; }
  // Fill/border seam. local runs -1..1 across a quad of outer radius (size + border) px, so the
  // fill ends at ratio size/(size+border). Sharp on purpose: a soft ring against a coloured fill
  // reads as blur, not as an outline.
  let border = max(p.lab.w, 0.0);
  if (border > 0.0) {
    let inner = p.ov.x / max(p.ov.x + border, 0.0001);
    if (r > inner) { return vec4(0.0, 0.0, 0.0, a); }
  }
  return vec4(in.rgb, a);
}
`

/**
 * Track tails: one screen-space quad per segment, drawn over the MIP with the points.
 *
 * QUADS RATHER THAN `line-list`, because WebGPU draws 1px lines only and a 1px tail over a noisy MIP is
 * close to invisible — the viewer's `tail_width` defaults to 4. Each endpoint is projected independently
 * and the quad is widened perpendicular to the SCREEN-space direction, so the width is in pixels and
 * stays constant under perspective while the geometry stays correct.
 *
 * Shares the same uniform buffer, and therefore the same camera, as the raycast and the points — see
 * `POINTS_WGSL` for why that matters more than it looks.
 */
export const SEGMENTS_WGSL = `
${SHARED_WGSL}

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
  // Ribbons carry their OWN plane bounds (pan.z / pan.w) so a viewer can widen the tail's z-reach
  // independently of the points' Z reach — a track's plane is where it ends, but the tail is a
  // continuous path and often reads best with more slack. Negative pan.z = no filter, same
  // convention as the points path.
  if (p.pan.z >= 0.0 && (plane < p.pan.z - 0.5 || plane > p.pan.w + 0.5)) {
    o.pos = vec4(0.0, 0.0, 2.0, 1.0);          // outside the widened track window: clipped away
    return o;
  }
  let aspect = p.vp.y / max(p.vp.z, 1.0);
  let c = camera();
  let pa = project(a - boxCentre(), c, aspect).xy;
  let pb = project(b - boxCentre(), c, aspect).xy;

  // In PIXELS, so the perpendicular is square on screen rather than stretched by the viewport aspect.
  let sa = vec2(pa.x * p.vp.y, pa.y * p.vp.z) * 0.5;
  let sb = vec2(pb.x * p.vp.y, pb.y * p.vp.z) * 0.5;
  var dir = sb - sa;
  let len = length(dir);
  // A zero-length segment has no direction to be perpendicular to; pick one rather than emit NaN,
  // which propagates to the whole quad and shows up as a stray triangle across the frame.
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
