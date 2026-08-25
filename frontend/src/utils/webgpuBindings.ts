// Which shader stage uses which binding — parsed from the WGSL, so the bind group LAYOUT can be
// checked against the shaders that share it.
//
// This exists because of a bug that renders as nothing at all until it renders as everything. The
// overlay passes read the shared uniform in their VERTEX stage (a point is projected before there is a
// fragment to shade), while the layout declared binding 0 as `FRAGMENT` only. WebGPU treats that as a
// pipeline-creation validation error: `createRenderPipeline` returns an INVALID pipeline, and setting
// an invalid pipeline invalidates the whole render pass — which is the pass the volume draws in. So the
// viewer renders perfectly until the moment a population is switched on, and then goes black, with the
// only diagnostic in the browser console. Nothing else in the codebase could have caught it: the
// pipelines are built at runtime against a real device, and the WGSL itself is entirely correct.
//
// The parse is transitive on purpose. `camera()` is the function that touches the uniform, and no entry
// point mentions `p` directly — a scan of entry-point bodies alone would conclude nothing uses binding
// 0 at all and pass a layout that cannot work.

export type Stage = 'vertex' | 'fragment'

/** `binding number → the variable's name`, from `@group(0) @binding(n) var[<...>] name`. */
export function bindingDecls(wgsl: string): Map<number, string> {
  const out = new Map<number, string>()
  const re = /@group\(0\)\s*@binding\((\d+)\)\s*var(?:<[^>]*>)?\s*([A-Za-z_]\w*)/g
  for (const m of wgsl.matchAll(re)) out.set(Number(m[1]), m[2])
  return out
}

/** Every top-level `fn` body, by name, with the stage attribute if it is an entry point. */
export function functions(wgsl: string): { name: string; stage: Stage | null; body: string }[] {
  const out: { name: string; stage: Stage | null; body: string }[] = []
  const re = /(@vertex\s+|@fragment\s+)?fn\s+([A-Za-z_]\w*)\s*\(/g
  for (const m of wgsl.matchAll(re)) {
    const open = wgsl.indexOf('{', m.index! + m[0].length)
    if (open < 0) continue
    let depth = 0, end = open
    for (let i = open; i < wgsl.length; i++) {
      if (wgsl[i] === '{') depth++
      else if (wgsl[i] === '}') { depth--; if (depth === 0) { end = i; break } }
    }
    const tag = (m[1] ?? '').trim()
    out.push({
      name: m[2],
      stage: tag === '@vertex' ? 'vertex' : tag === '@fragment' ? 'fragment' : null,
      body: wgsl.slice(open, end + 1),
    })
  }
  return out
}

const mentions = (body: string, name: string) =>
  new RegExp('(^|[^\\w.])' + name + '(?![\\w])').test(body)

/**
 * `binding number → the stages that reach it`, following calls. A helper that touches a binding lends
 * that binding to every entry point that can call it, however indirectly.
 */
export function bindingStages(wgsl: string): Map<number, Set<Stage>> {
  const decls = bindingDecls(wgsl)
  const fns = functions(wgsl)

  /** Bindings a function touches directly, plus the functions it calls. */
  const direct = new Map<string, Set<number>>()
  const calls = new Map<string, Set<string>>()
  for (const f of fns) {
    const d = new Set<number>()
    for (const [n, v] of decls) if (mentions(f.body, v)) d.add(n)
    direct.set(f.name, d)
    const c = new Set<string>()
    for (const g of fns) if (g.name !== f.name && new RegExp('\\b' + g.name + '\\s*\\(').test(f.body)) c.add(g.name)
    calls.set(f.name, c)
  }
  // Fixpoint rather than one pass: `fs` → `ramp` is one hop, but a three-deep chain is not, and the
  // prelude is shared so the depth is not fixed.
  for (let changed = true; changed;) {
    changed = false
    for (const f of fns) {
      const mine = direct.get(f.name)!
      for (const g of calls.get(f.name)!) {
        for (const b of direct.get(g) ?? []) if (!mine.has(b)) { mine.add(b); changed = true }
      }
    }
  }

  const out = new Map<number, Set<Stage>>()
  for (const f of fns) {
    if (!f.stage) continue
    for (const b of direct.get(f.name)!) {
      if (!out.has(b)) out.set(b, new Set())
      out.get(b)!.add(f.stage)
    }
  }
  return out
}

/** `binding number → the stages the layout makes it visible to`, from a `createBindGroupLayout` call
 *  in TypeScript source. Whitespace-tolerant: the entries wrap across lines. */
export function layoutVisibility(ts: string): Map<number, Set<Stage>> {
  const out = new Map<number, Set<Stage>>()
  const re = /\{\s*binding:\s*(\d+),\s*visibility:\s*([^,]+?),/g
  for (const m of ts.matchAll(re)) {
    const s = new Set<Stage>()
    if (m[2].includes('GPUShaderStage.VERTEX')) s.add('vertex')
    if (m[2].includes('GPUShaderStage.FRAGMENT')) s.add('fragment')
    out.set(Number(m[1]), s)
  }
  return out
}

/** Bindings the shaders need in a stage the layout does not grant, as readable strings. Empty is the
 *  only acceptable answer — every entry here is a pipeline that cannot be created. */
export function visibilityGaps(wgsls: string[], layoutTs: string): string[] {
  const vis = layoutVisibility(layoutTs)
  const need = new Map<number, Set<Stage>>()
  for (const w of wgsls) {
    for (const [b, stages] of bindingStages(w)) {
      if (!need.has(b)) need.set(b, new Set())
      for (const s of stages) need.get(b)!.add(s)
    }
  }
  const gaps: string[] = []
  for (const [b, stages] of [...need].sort((a, c) => a[0] - c[0])) {
    const have = vis.get(b)
    if (!have) { gaps.push(`binding ${b} is used but absent from the layout`); continue }
    for (const s of stages) if (!have.has(s)) gaps.push(`binding ${b} is used in the ${s} stage but not visible to it`)
  }
  return gaps
}
