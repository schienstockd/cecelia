// The MIP raycast and its two overlay passes (points, track tails) — the flat renderer's shaders.
//
// The WGSL lives in `shaders/mip*.wgsl`, which is where the reasoning is written down (why
// `textureLoad`, why the LUT is lerped by hand, why one shader also draws the 2D view, the vertical
// flip). This module only expands it. The same files are what the movie renderer runs
// (SHARED_RENDERER_PLAN.md), so an edit there changes the viewer and the movies together.

import { expandWgsl, SHADER_CONSTANTS, uniformLayout } from './shaderSource'

/** Storage-buffer binding number for the pick data on the flat renderer (`constants.json`). The
 *  shader takes it from the same constant, so a mismatch cannot happen silently. */
export const MIP_PICK_BINDING = SHADER_CONSTANTS.MIP_PICK_BINDING

/** The uniform block all three passes share (`uniforms.json` → "mip", struct `P`). */
export const MIP_LAYOUT = uniformLayout('mip')

/** The raycast (`shaders/mip.wgsl`). */
export const MIP_WGSL = expandWgsl('mip.wgsl')

/** Population points as camera-facing quads over the MIP (`shaders/mip_points.wgsl`). */
export const POINTS_WGSL = expandWgsl('mip_points.wgsl')

/** Track tails as screen-space quads over the MIP (`shaders/mip_segments.wgsl`). */
export const SEGMENTS_WGSL = expandWgsl('mip_segments.wgsl')
