// The MIP raycast shader. One pass, one full-screen triangle, no geometry: every fragment marches a
// ray through the volume and keeps each channel's maximum, then colours it through that channel's LUT
// and adds the channels together — the same additive composite `image_render.jl` does on the CPU.
//
// Measured on an RTX 2000 Ada with real data at 1566x1003, 4 channels, 256 steps: 5.3 ms/frame,
// against napari's 36.0 ms for the same view (docs/todo/NAPARI_WEBGPU_AUDIT.md → G2).
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
// because a name table here is a second copy of napari's palette and the first copy being incomplete
// rendered a channel white.

import { MAX_CHANNELS, LUT_STOPS, VIEW_HALF_ANGLE } from '../../utils/volumeViewer'

export const MIP_WGSL = `
struct P {
  cam:  vec4<f32>,                       // yaw, pitch, dist, steps
  vp:   vec4<f32>,                       // channel count, canvas w, canvas h, orthographic
  ext:  vec4<f32>,                       // physical extent x, y, z
  dims: vec4<f32>,                       // nx, ny, nz, z-planes per channel
  ch:   array<vec4<f32>, ${MAX_CHANNELS}>, // per channel: lo, hi, visible, unused
};

@group(0) @binding(0) var<uniform> p: P;
@group(0) @binding(1) var vol: texture_3d<u32>;
@group(0) @binding(2) var lut: texture_2d<f32>;

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

@fragment fn fs(in: VOut) -> @location(0) vec4<f32> {
  let h = p.ext.xyz * 0.5;
  let cy = cos(p.cam.x); let sy = sin(p.cam.x);
  let cp = cos(p.cam.y); let sp = sin(p.cam.y);
  let fwd = vec3(cp * sy, sp, cp * cy);
  let ro = fwd * p.cam.z;
  let right = normalize(cross(vec3(0.0, 1.0, 0.0), fwd));
  let up = cross(fwd, right);
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
  for (var s = 0; s < n; s = s + 1) {
    let wp = org + rd * (t0 + (f32(s) + 0.5) * dt);
    let uvw = (wp + h) / p.ext.xyz;
    let vi = vec3<i32>(uvw * p.dims.xyz);
    if (vi.x < 0 || vi.y < 0 || vi.z < 0 ||
        vi.x >= i32(p.dims.x) || vi.y >= i32(p.dims.y) || vi.z >= i32(p.dims.z)) { continue; }
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
  return vec4(min(acc, vec3(1.0)), 1.0);
}
`
