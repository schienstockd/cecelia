// The uniform struct + camera basis + projection, shared VERBATIM by the raycast and the overlay
// passes. Same discipline as `mip_common.wgsl`: one copy `#include`d into every
// pass so a marker drawn by `project()` sits on the cell drawn by the raycast rather than beside
// it. Vertical-flip lives in `up = cross(right, fwd)` — see the note there.

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
  // Same vertical-flip discipline as mip_common.wgsl — cross(right, fwd), not cross(fwd, right).
  c.up = cross(c.right, c.fwd);
  c.ro = c.fwd * p.cam.z + c.right * p.pan.x + c.up * p.pan.y;
  return c;
}

// World µm → clip space, the exact inverse of the ray construction. Returns w along the view
// axis so a caller can size a point under perspective. Matches mip_common.wgsl's project() so a
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
