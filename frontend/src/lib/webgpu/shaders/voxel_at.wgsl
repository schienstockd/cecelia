// The voxel a ray sample reads, shared VERBATIM by every raycast (mip.wgsl, brick.wgsl,
// brick_multi.wgsl) through its common file, which declares the uniform block `p` (`P` / `BU`) with
// the `dims` and `zwin` fields this reads. One copy, because the held/shown mapping disagreeing
// between the flat and brick renderers would draw different planes for the same window.
//
// `uvw` is the sample's coordinate in the box, and the box is the SHOWN planes, which can be a
// sub-range of the planes the texture HOLDS: a ±n window at a stack end shows fewer planes than the
// 2n+1 it keeps loaded, so the texture keeps one shape while the window moves (`heldPlanes`).
// `zwin.x` is the first shown plane inside the held ones and `zwin.y` how many are shown, both in
// current-level voxels; `zwin.y == 0` means all of them (the default, and every other host).
fn voxelAt(uvw: vec3<f32>) -> vec3<i32> {
  let shown = select(p.dims.z, p.zwin.y, p.zwin.y > 0.0);
  return vec3<i32>(vec3(uvw.x * p.dims.x, uvw.y * p.dims.y, uvw.z * shown + p.zwin.x));
}
