// The tile pass: one instanced quad per resident tile, sampled from a shared 3D atlas texture. Same
// LUT/contrast machinery as the MIP shader, different geometry.
//
// The WGSL and the reasoning behind it (why a 3D atlas, why `textureLoad`, the coordinate model)
// live in `shaders/tile.wgsl`; this module only expands it.

import { expandWgsl, uniformLayout } from './shaderSource'

/** The per-frame uniform block (`uniforms.json` → "tile"). */
export const TILE_LAYOUT = uniformLayout('tile')

/** Bytes in the tile uniform block — sized so a `minBindingSize` check catches a layout drift
 *  without a probe frame. */
export const TILE_UNIFORM_BYTES = TILE_LAYOUT.bytes

export const TILE_WGSL = expandWgsl('tile.wgsl')
