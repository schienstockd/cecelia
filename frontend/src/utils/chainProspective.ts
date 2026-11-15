// What a chain WILL have made by the time a node runs, for the node's pickers — populations a gate
// step will draw and populations a tracking step will derive. The images may not have them yet: a
// chain built from one exemplar's history is run on the whole set, and only the exemplar is gated.
// The value-name and model halves of the same idea live in ChainModule (`extraValueNames`) and
// `chainModelOptions.ts`.

import type { ParamDef, ParamValues } from '../tasks/types'
import type { PopGroupDef, PopOption } from './popGroups'
import { isTrackPopType } from './measureGroups'
import { popRefList } from '../tasks/paramValues'

/** A population an upstream node will make: `kind` is the picker family that lists it. */
export interface ProspectivePop { valueName: string; path: string; kind: 'live' | 'track' }

const MANUAL_GATE_FN = 'gate.manual_gate'
const WHOLE = new Set(['', 'NONE', 'root', '/'])

function walkParams(ps: ParamDef[] | undefined, fn: (p: ParamDef) => void) {
  for (const p of ps ?? []) { fn(p); walkParams(p.params, fn) }
}

/**
 * The populations `nodes` (an ancestor set) will make. A manual CELL-gate step names the gates it
 * waits for (`requiredPops`, on its `valueName`); a param that declares `producesPop` derives
 * `<label set><pop>/<marker>` — tracking `/qc/OTI` on `Tcells` → `Tcells` `/qc/OTI/_tracked`.
 */
export function prospectivePops(nodes: readonly { fn: string; params?: ParamValues }[],
                                defOf: (fn: string) => { params?: ParamDef[] } | undefined): ProspectivePop[] {
  const out: ProspectivePop[] = []
  for (const n of nodes) {
    const params = n.params ?? {}
    const vn = String(params.valueName ?? 'default')
    if (n.fn === MANUAL_GATE_FN) {
      // only cell gates: a track gate step's pops are not cell populations, and a `live` read of one
      // comes back empty (docs/POPULATION.md) — offering them to tracking would wire an empty run
      if (isTrackPopType(String(params.popType ?? 'flow'))) continue
      for (const p of String(params.requiredPops ?? '').split(',').map(s => s.trim()).filter(Boolean))
        out.push({ valueName: vn, path: p.startsWith('/') ? p : `/${p}`, kind: 'live' })
      continue
    }
    walkParams(defOf(n.fn)?.params, p => {
      if (!p.producesPop) return
      // one value (a run saved by the single picker: "NONE", "/qc/OTI") or the multi picker's list of
      // segmentation-prefixed refs ("Tcells/qc/OTI", all-cells "Tcells/"); nothing picked = whole
      const refs = popRefList(params[p.key] ?? p.default ?? '', vn).filter(v => v !== '')
      for (const ref of refs.length ? refs : [`${vn}/`]) {
        const slash = ref.indexOf('/')
        const [rvn, path] = slash < 0 ? [vn, ref] : [ref.slice(0, slash), ref.slice(slash)]
        const base = WHOLE.has(path) ? '' : path.replace(/\/+$/, '')
        out.push({ valueName: rvn, path: `${base}/${p.producesPop}`, kind: 'track' })
      }
    })
  }
  const seen = new Set<string>()
  return out.filter(p => { const k = `${p.kind}\u0000${p.valueName}${p.path}`; return !seen.has(k) && !!seen.add(k) })
}

type Opt = { label: string; value: string }

/**
 * A single population dropdown (tracking's `popsToTrack`): what is on the first image, plus the gates
 * an upstream step will draw on this label set, plus the saved value if neither has it — so a saved
 * `/qc/OTI` shows as itself instead of a blank select when the first ticked image is not gated yet.
 */
export function singlePopOptions(onImage: readonly Opt[], extra: readonly ProspectivePop[],
                                 valueName: string, current: unknown): Opt[] {
  const out = [...onImage]
  const has = (v: string) => out.some(o => o.value === v)
  for (const p of extra)
    if (p.kind === 'live' && p.valueName === valueName && !has(p.path))
      out.push({ label: `${p.path} · after gating`, value: p.path })
  if (typeof current === 'string' && current && !has(current))
    out.push({ label: `${current} · not on this image`, value: current })
  return out
}

/**
 * The across-segmentations chip list (HMM's `pops`): the image's groups, plus a "From this chain"
 * group of what upstream steps will make that the picker accepts (`accepts` has the kind; none
 * declared = gated cells), plus a "Saved" group for selected values in neither.
 */
export function prospectivePopGroups(groups: readonly PopGroupDef[], extra: readonly ProspectivePop[],
                                     accepts: readonly string[] | undefined, current: unknown): PopGroupDef[] {
  const out = [...groups]
  const has = new Set(groups.flatMap(g => g.opts.map(o => o.value)))
  const ok = (k: ProspectivePop['kind']) => accepts?.length ? accepts.includes(k) : k === 'live'
  const chain: PopOption[] = []
  for (const p of extra) {
    const v = `${p.valueName}${p.path}`
    if (ok(p.kind) && !has.has(v)) { chain.push({ label: v, value: v }); has.add(v) }
  }
  if (chain.length) out.push({ title: 'From this chain', opts: chain })
  const saved = (Array.isArray(current) ? current : []).filter((v): v is string => typeof v === 'string' && !has.has(v))
  if (saved.length) out.push({ title: 'Saved, not on this image', opts: saved.map(v => ({ label: v, value: v })) })
  return out
}
