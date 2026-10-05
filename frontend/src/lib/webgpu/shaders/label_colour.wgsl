// A label's colour and whether it is drawn, shared VERBATIM by every raycast that composites labels
// (mip.wgsl, brick.wgsl, brick_multi.wgsl). The includer supplies `p.lab` and the `pal` texture.
//
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
