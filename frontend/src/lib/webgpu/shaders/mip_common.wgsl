// The uniform layout and the camera, shared verbatim by all three passes.
//
// ONE COPY, `#include`d, because there were three and that is exactly how a convention drifts. The
// raycast builds rays from this basis and the overlay passes invert it; if the two ever disagree by a
// sign, a marker sits beside the cell it marks and still looks plausible.
//
// THE VERTICAL FLIP IS HERE, in `up`. `cross(right, fwd)` rather than `cross(fwd, right)` — i.e. screen
// up is -y in world. Image row 0 must appear at the TOP, as it does in viewer and in every image
// viewer, and the naive basis puts it at the bottom: WebGPU's NDC y points up while a framebuffer's
// rows count down from the top, so a right-handed basis and a raster image disagree by exactly this
// sign. Derived rather than guessed (screen top mapped to the LAST texture row), and asserted by
// `docs/todo/spike/webgpu/shader_check.mjs` — the orientation check there is the one-click proof.

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
