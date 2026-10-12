// Track tails over the tile view: one screen-space quad per segment, drawn before the points so a
// marker sits on top of the path leading to it. Mirrors `mip_segments.wgsl` with the tile camera.

#include "tile_common.wgsl"
#include "overlay_quads.wgsl"

struct SOut { @builtin(position) pos: vec4<f32>, @location(0) rgb: vec3<f32>, @location(1) alpha: f32 };

@vertex fn vs(
  @builtin(vertex_index) vi: u32,
  @location(0) a: vec3<f32>,
  @location(1) b: vec3<f32>,
  @location(2) rgb: vec3<f32>,
  @location(3) plane: f32,
  @location(4) tEnd: f32,
) -> SOut {
  var o: SOut;
  o.rgb = rgb;
  o.alpha = tailAlpha(tEnd, p.ovx.z, p.ovx.w);
  // Ribbons carry their OWN plane bounds, so a viewer can widen the tail's z-reach independently of
  // the points' — a track's plane is where it ends, but the tail is a continuous path.
  if (offPlanes(plane, p.ovx.x, p.ovx.y)) {
    o.pos = clippedPos();
    return o;
  }
  let pa = overlayNdc(a);
  let pb = overlayNdc(b);
  o.pos = ribbonPos(pa, pb, vi, overlayPx(p.ov.z, p.ext.z, pxPerUm()), p.vp.yz);
  return o;
}

@fragment fn fs(in: SOut) -> @location(0) vec4<f32> {
  return vec4(in.rgb, in.alpha);
}
