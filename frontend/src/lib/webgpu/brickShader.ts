// ── Brick-atlas raycast shader (P5b) ───────────────────────────────────────────────
//
// One full-screen triangle, one fragment per pixel, marches a ray through the box the same way
// the flat renderer does. The DIFFERENCE is where a sample comes from: instead of one 3D texture
// covering the whole volume, each sample looks its brick up in the page table, misses through
// unmapped bricks (transparent — the tick loop will populate them), and reads the resident
// bricks out of the atlas 3D texture.
//
// Overlays (points + track tails) share this uniform buffer and live in the SAME render pass as
// the raycast — a second camera would draw a marker next to its cell rather than on it and still
// look plausible. See `mipShader.ts` for the vertical-flip note (`up = cross(right, fwd)`, not
// the other way round) — the same discipline applies here so the two renderers put row 0 at the
// top identically.
//
// The WGSL itself lives in `shaders/brick*.wgsl` (SHARED_RENDERER_PLAN.md Phase 1); this module
// expands it and owns the multi-atlas binding arithmetic.

import { expandWgsl, SHADER_CONSTANTS, uniformLayout } from './shaderSource'

/** Storage-buffer binding number for the pick data on the brick renderer AT N=1. See
 *  `MIP_PICK_BINDING` in `mipShader.ts` — same feature, same snippet, different slot because the
 *  brick renderer's label + palette bindings already occupy 5 and 6.
 *
 *  Multi-atlas shift (S1 of `docs/todo/WEBGPU_MULTI_ATLAS_SHADER_VARIANTS_PLAN.md`): at N>=2
 *  every binding after the atlas slot shifts up by `N-1`. Callers must use
 *  `makeBrickShader(n).bindings.pick` to pick up the right number per variant. This constant
 *  stays for backward-compat (the N=1 path uses it verbatim) and for the pick buffer's uniform
 *  contract with `viewerLabels.ts`. */
export const BRICK_PICK_BINDING = SHADER_CONSTANTS.BRICK_PICK_BINDING

/** Max atlases the renderer compiles a variant for. Mirrors `MAX_ATLASES` in
 *  `frontend/src/utils/brickAtlas.ts` — kept as an independent literal here because the shader
 *  factory's bounds check must not depend on the utils module (which imports device types). */
export const BRICK_MAX_ATLASES = 4

/**
 * Sentinel written into the page table for an unmapped brick. Matches `pageTable.ts`'s
 * "not resident" convention on the JS side — a scheduler that resets an entry writes this. WGSL
 * cannot express `0xFFFFFFFFu` as a `const` from a template literal cleanly so it's inlined at
 * the two use sites.
 */
export const EMPTY_SLOT = 0xFFFFFFFF

/** The brick uniform block (`shaders/uniforms.json` → "brick", struct `BU` in `brick_common.wgsl`). */
const BRICK_LAYOUT = uniformLayout('brick')

/** Uniform buffer size in bytes: twelve leading vec4s + one vec4 per channel slot. */
export const BRICK_UNIFORM_BYTES = BRICK_LAYOUT.bytes

/**
 * Field offsets INTO the uniform buffer, in f32 slots (× 4 = bytes), read from the layout so the
 * renderer and the shader's struct cannot disagree. Lanes per field are named in `uniforms.json`.
 */
export const BU = {
  CAM: BRICK_LAYOUT.base.cam,
  VP: BRICK_LAYOUT.base.vp,
  EXT: BRICK_LAYOUT.base.ext,
  DIMS: BRICK_LAYOUT.base.dims,
  BRICK: BRICK_LAYOUT.base.brick,
  ATLAS: BRICK_LAYOUT.base.atlas,
  GRID: BRICK_LAYOUT.base.grid,
  PAN: BRICK_LAYOUT.base.pan,
  OV: BRICK_LAYOUT.base.ov,
  LAB: BRICK_LAYOUT.base.lab,
  PREV_GRID: BRICK_LAYOUT.base.prevGrid,
  PREV_DIMS: BRICK_LAYOUT.base.prevDims,
  /** Per-channel `(lo, hi, visible, unused)`. `visible < 0.5` means "skip this channel". */
  CH0: BRICK_LAYOUT.base.ch,
}



/** The N = 1 raycast (`shaders/brick.wgsl`). */
export const BRICK_WGSL = expandWgsl('brick.wgsl')

/** Overlay points over the brick raycast (`shaders/brick_points.wgsl`). */
export const BRICK_POINTS_WGSL = expandWgsl('brick_points.wgsl')

/** Track tails over the brick raycast (`shaders/brick_segments.wgsl`). */
export const BRICK_SEGMENTS_WGSL = expandWgsl('brick_segments.wgsl')

// ── Multi-atlas shader variants (S1 of WEBGPU_MULTI_ATLAS_SHADER_VARIANTS_PLAN.md) ────
//
// One WGSL variant per possible atlas count. Chromium/Dawn does not ship the `binding_array`
// runtime (§A of `docs/todo/spike/webgpu/diagnostic.html` returned `bindGroupAccepts=false`),
// so multi-atlas rendering rides through N static bindings + a `switch(atlasIndex)` over
// compile-time-literal `textureLoad`s. §I of the same diagnostic proved this shape parses,
// pipelines, and returns correct values on real Dawn — the workstream unblocked on
// PR #941 (2026-09-17).
//
// N=1 keeps the existing `BRICK_WGSL` literal verbatim (Decision 2 — "byte-identical" — so
// the common case pays nothing). N∈{2,3,4} builds a shifted variant here.

/** Per-variant binding numbers. Callers building bind-group layouts / bind groups read from
 *  this instead of the top-level `BRICK_PICK_BINDING` etc. constants — those only describe
 *  the N=1 shape. See the module header for the shift rule.
 *
 *  Both `atlas` and `labAtlas` are length `nAtlases` — S2 grew the label atlas to N alongside
 *  the intensity atlas. At N=1 the layout is exactly today's numbering (atlas[0]=2, labAtlas[0]=5,
 *  pal=6, pick=7). At N>=2 both atlas arrays occupy N contiguous slots and everything past pal/pick
 *  shifts by 2*(N-1). */
export interface BrickShaderBindings {
  uniform: number
  pt: number
  atlas: readonly number[]
  prevPt: number
  lut: number
  labAtlas: readonly number[]
  pal: number
  pick: number
}

export interface BrickShaderVariant {
  nAtlases: number
  code: string
  bindings: BrickShaderBindings
}

const N1_BINDINGS: BrickShaderBindings = {
  uniform: 0, pt: 1, atlas: [2], prevPt: 3, lut: 4, labAtlas: [5], pal: 6, pick: BRICK_PICK_BINDING,
}

function bindingsForN(n: number): BrickShaderBindings {
  // Layout formula, applied uniformly at every N. N=1 collapses to today's numbering, which is
  // why `BRICK_WGSL` still parses against `N1_BINDINGS` verbatim (Decision 2 byte-identity).
  const atlas = Array.from({ length: n }, (_, i) => 2 + i)
  const prevPt = 2 + n
  const lut = 3 + n
  const labAtlas = Array.from({ length: n }, (_, i) => 4 + n + i)
  const pal = 4 + 2 * n
  const pick = 5 + 2 * n
  return { uniform: 0, pt: 1, atlas, prevPt, lut, labAtlas, pal, pick }
}

function makeMultiAtlasBrickWgsl(nAtlases: number): string {
  const b = bindingsForN(nAtlases)
  // Atlas bindings (2..2+N-1). Each texture_3d<u32> is a distinct compile-time-literal binding
  // — the constraint that makes Decision 3 work without `binding_array`.
  const atlasDecls = b.atlas
    .map((bi, i) => `@group(0) @binding(${bi}) var atlas${i}: texture_3d<u32>;`)
    .join('\n')
  // switch(atlasIndex) fan-out — one arm per binding, last is default: to satisfy WGSL's
  // exhaustiveness for u32 switches without a fallthrough. Verified on Chromium/Dawn Vulkan by
  // S0's diagnostic — see WEBGPU_MULTI_ATLAS_SHADER_VARIANTS_PLAN.md Decision 3.
  const atlasSwitch = b.atlas
    .map((_, i) => {
      const head = i === b.atlas.length - 1 ? 'default' : `case ${i}u`
      return `    ${head}: { return textureLoad(atlas${i}, coord, 0).r; }`
    })
    .join('\n')
  // Label atlas gets the same treatment (S2) — one texture per intensity atlas, sized to the
  // same per-atlas slot grid. Bindings live past prevPt/lut per the shift formula above.
  const labAtlasDecls = b.labAtlas
    .map((bi, i) => `@group(0) @binding(${bi}) var labAtlas${i}: texture_3d<u32>;`)
    .join('\n')
  const labAtlasSwitch = b.labAtlas
    .map((_, i) => {
      const head = i === b.labAtlas.length - 1 ? 'default' : `case ${i}u`
      return `    ${head}: { return textureLoad(labAtlas${i}, coord, 0).r; }`
    })
    .join('\n')
  return expandWgsl('brick_multi.wgsl', {
    ATLAS_DECLS: atlasDecls, ATLAS_SWITCH: atlasSwitch,
    LAB_ATLAS_DECLS: labAtlasDecls, LAB_ATLAS_SWITCH: labAtlasSwitch,
    PT_BINDING: b.pt, PREV_PT_BINDING: b.prevPt, LUT_BINDING: b.lut, PAL_BINDING: b.pal,
    PICK_BINDING: b.pick,
  })
}

/**
 * Build the raycast shader + its bind numbers for a given atlas count. N=1 returns the
 * existing single-atlas literal verbatim (Decision 2 — the common path pays nothing). N∈{2,3,4}
 * generates a variant with N `texture_3d<u32>` bindings + a `switch(atlasIndex)` decode.
 *
 * See `docs/todo/WEBGPU_MULTI_ATLAS_SHADER_VARIANTS_PLAN.md` for the shape and the S0 diagnostic
 * that verified it on real Dawn.
 */
export function makeBrickShader(opts: { nAtlases: number }): BrickShaderVariant {
  const n = opts.nAtlases | 0
  if (n < 1 || n > BRICK_MAX_ATLASES) {
    throw new Error(`makeBrickShader: nAtlases must be 1..${BRICK_MAX_ATLASES}, got ${opts.nAtlases}`)
  }
  if (n === 1) return { nAtlases: 1, code: BRICK_WGSL, bindings: N1_BINDINGS }
  return { nAtlases: n, code: makeMultiAtlasBrickWgsl(n), bindings: bindingsForN(n) }
}

/** N=1 bind numbers — exposed for callers that want to read the layout without asking the
 *  factory for a code string. Matches `BRICK_PICK_BINDING` etc. */
export const BRICK_N1_BINDINGS = N1_BINDINGS
