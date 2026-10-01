// The WGSL expansion rules and the uniform-layout arithmetic — pure, no Vite, no JSON imports.
//
// Split out of `shaderSource.ts` so plain Node can run it too (Node strips the types):
// `docs/todo/spike/webgpu/shader_check.mjs` imports this file rather than keeping its own copy. The
// rules are documented in `shaderSource.ts`; the Python twin is `python/cecelia/utils/wgsl_utils.py`.

export type WgslVars = Record<string, string | number>

const INCLUDE = /^[ \t]*#include[ \t]+"([\w.-]+)"((?:[ \t]+\w+=[\w.-]+)*)[ \t]*$/gm
const TOKEN = /\$\{([A-Za-z_]\w*)\}/g

/** The expansion rules over any file reader — `expandWgsl` with the shader directory, the golden
 *  test with in-memory files. `vars` is the whole scope: no constants are added here. */
export function expandSource(file: string, vars: WgslVars, read: (file: string) => string,
                             stack: string[] = []): string {
  if (stack.includes(file)) throw new Error(`wgsl: include cycle ${[...stack, file].join(' → ')}`)
  const withIncludes = read(file).replace(INCLUDE, (_m, inc: string, args: string) => {
    const scope: WgslVars = { ...vars }
    for (const kv of args.trim().split(/\s+/).filter(Boolean)) {
      const [k, v] = kv.split('=')
      scope[k] = v in vars ? vars[v] : v
    }
    // The included text keeps its own trailing newline; drop it so the include line's own newline
    // is the one that ends the block.
    return expandSource(inc, scope, read, [...stack, file]).replace(/\n$/, '')
  })
  return withIncludes.replace(TOKEN, (_m, name: string) => {
    if (!(name in vars)) throw new Error(`wgsl: ${file} uses \${${name}} but it is not defined`)
    return String(vars[name])
  })
}

export interface FieldSpec { name: string; lanes: string[]; count?: string }
export interface BlockSpec { struct: string; file: string; fields: FieldSpec[] }
export interface UniformLayout {
  /** The WGSL struct this block is, and the file that declares it. */
  struct: string
  file: string
  fields: readonly FieldSpec[]
  /** f32 slot of every lane: `at.cam.dist`. For an array field, the slot of element 0 — add
   *  `index * 4` for element `index`. */
  at: Readonly<Record<string, Readonly<Record<string, number>>>>
  /** f32 slot where each field starts: `base.ch`. */
  base: Readonly<Record<string, number>>
  /** Elements per field: 1, or the array length. */
  counts: Readonly<Record<string, number>>
  floats: number
  bytes: number
}

/** A uniform block's slots from its `uniforms.json` entry. Every field is a `vec4<f32>` (or an array
 *  of them), so a field is 4 slots and there is no padding to compute. */
export function buildUniformLayout(name: string, spec: BlockSpec,
                                   constants: Readonly<Record<string, number>>): UniformLayout {
  const at: Record<string, Record<string, number>> = {}
  const base: Record<string, number> = {}
  const counts: Record<string, number> = {}
  let slot = 0
  for (const f of spec.fields) {
    if (f.lanes.length !== 4) throw new Error(`wgsl: ${name}.${f.name} must name 4 lanes`)
    base[f.name] = slot
    counts[f.name] = fieldCount(f, constants)
    at[f.name] = Object.fromEntries(f.lanes.map((l, i) => [l, slot + i]))
    slot += 4 * counts[f.name]
  }
  return { struct: spec.struct, file: spec.file, fields: spec.fields, at, base, counts, floats: slot, bytes: slot * 4 }
}

function fieldCount(f: FieldSpec, constants: Readonly<Record<string, number>>): number {
  if (f.count === undefined) return 1
  const n = constants[f.count] ?? Number(f.count)
  if (!Number.isInteger(n) || n < 1) throw new Error(`wgsl: field ${f.name} has bad count '${f.count}'`)
  return n
}

/**
 * Pack a uniform block from lane names: `{ 'cam.dist': 120, 'ch[2].hi': 4000 }`. Lanes not given
 * are 0. The renderers write slots in place through `uniformLayout(...).at` instead; this exists so
 * the browser and the movie host can be checked against one golden (`shaders/golden.json`).
 */
export function packLayout(L: UniformLayout, values: Record<string, number>): Float32Array {
  const u = new Float32Array(L.floats)
  for (const [key, v] of Object.entries(values)) u[laneSlot(L, key)] = v
  return u
}

function laneSlot(L: UniformLayout, key: string): number {
  const m = /^(\w+)(?:\[(\d+)\])?\.(\w+)$/.exec(key)
  if (!m) throw new Error(`wgsl: bad lane '${key}'`)
  const [, field, idx, lane] = m
  const f = L.fields.find(x => x.name === field)
  const slot = L.at[field]?.[lane]
  if (!f || slot === undefined) throw new Error(`wgsl: no lane '${key}' in ${L.struct}`)
  const i = idx === undefined ? 0 : Number(idx)
  if (i >= L.counts[field] || (idx !== undefined && f.count === undefined)) {
    throw new Error(`wgsl: lane '${key}' is out of range`)
  }
  return slot + i * 4
}
