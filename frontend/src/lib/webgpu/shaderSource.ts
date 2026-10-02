// The viewer's WGSL, read from `shaders/*.wgsl` — the ONE copy both renderers run.
//
// The browser imports the files at build time (Vite `?raw`); the movie renderer reads the same files
// from disk (`python/cecelia/utils/wgsl_utils.py`). Both expand them with the same two rules, so the
// text a GPU compiles is identical on either side (SHARED_RENDERER_PLAN.md Decision 1):
//
//   - `#include "x.wgsl" [NAME=VALUE ...]` on a line of its own → x.wgsl, expanded. VALUE is a
//     variable name (resolved in the includer's scope) or a literal. The included file sees the
//     includer's variables plus these.
//   - `${NAME}` → the variable NAME: a key of `shaders/constants.json`, or one the caller passes.
//     An unknown name throws — a silently empty `${MAX_CHANNELS}` is a compile error at best and a
//     wrong array size at worst.
//
// Numbers print the JS way (`String(n)`: 32, 0.45). The Python twin formats an integral float
// without its `.0` so the two agree; `shaderSource.test.ts` and `test_wgsl_utils.py` check both
// against the same golden.
//
// The uniform blocks are data too (`shaders/uniforms.json`, Decision 3): the renderers write slots
// by NAME through `uniformLayout`, and the movie host packs the same names, so neither side can
// shift a field by hand.

import SHADER_CONSTANTS_JSON from './shaders/constants.json'
import UNIFORMS_JSON from './shaders/uniforms.json'
import { buildUniformLayout, expandSource, packLayout, type BlockSpec, type UniformLayout, type WgslVars } from './wgslExpand'

export { expandSource, type UniformLayout, type WgslVars }

const FILES = import.meta.glob('./shaders/*.wgsl', { query: '?raw', import: 'default', eager: true }) as
  Record<string, string>

export const SHADER_CONSTANTS: Readonly<Record<string, number>> = SHADER_CONSTANTS_JSON
const UNIFORMS = UNIFORMS_JSON as Record<string, BlockSpec>

/** The raw text of `shaders/<file>` — before includes and substitution. */
export function wgslFile(file: string): string {
  const src = FILES[`./shaders/${file}`]
  if (src === undefined) throw new Error(`wgsl: no shaders/${file}`)
  return src
}

/** `shaders/<file>` with its includes inlined and every `${NAME}` substituted. */
export function expandWgsl(file: string, vars: WgslVars = {}): string {
  return expandSource(file, { ...SHADER_CONSTANTS, ...vars }, wgslFile)
}

/** The uniform block `name` ('mip' | 'tile' | 'brick') from `shaders/uniforms.json`. */
export function uniformLayout(name: string): UniformLayout {
  const spec = UNIFORMS[name]
  if (!spec) throw new Error(`wgsl: no uniform block '${name}'`)
  return buildUniformLayout(name, spec, SHADER_CONSTANTS)
}

/** `packLayout` over a named block — see `wgslExpand.ts`. */
export function packUniforms(name: string, values: Record<string, number>): Float32Array {
  return packLayout(uniformLayout(name), values)
}
