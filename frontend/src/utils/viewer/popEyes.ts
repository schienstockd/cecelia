/**
 * The viewer's per-population eyes — which gated pops the pop-out draws as points (the Populations
 * rows) and as cell-track ribbons (the Tracks rows, #1766). One eye per layer, same rules for both.
 *
 * The eye is USER intent, separate from the Population Manager's `show` (#1730): `show` only SEEDS
 * the eye of a pop the viewer has never stored a state for. Stored per (image × segmentation) as
 * `{ "<popType>::<path>": visible }` (`settings.getPopEyes`), so the choice survives a reopen and
 * — the reason it exists — a refetch: the manager pings the viewer on every gate write, and a set
 * rebuilt from `show` on each fetch reset every toggle the user had just made.
 *
 * Pure; `ViewerWindow.vue` owns the refs and the store writes.
 */
import { makePopPathRemap, type PopIdent } from '../popRenameRemap'
import { soloVisibility } from './channelSolo'

/** Which eye: a pop's points or its cell-track ribbon. Each is its own stored bag. */
export type EyeLayer = 'points' | 'ribbons'
/** Where `settings.getPopEyes` keeps each layer's bag. */
export const POP_EYES_KEYS: Readonly<Record<EyeLayer, string>> =
  { points: 'cc.viewerPopEyes', ribbons: 'cc.viewerRibbonEyes' }

/** One segmentation's stored eyes: `"<popType>::<path>"` → visible. */
export type PopEyes = Record<string, boolean>

/** What the reconcile needs of a fetched pop. */
export interface EyePop { popType: string; path: string; uid?: string; show: boolean }

export const eyeKey = (popType: string, path: string) => `${popType}::${path}`

/**
 * One segmentation's eyes after a fetch. Per pop type that was fetched:
 *  - a stored eye follows its pop across a RENAME (uid match) and is dropped on a DELETE
 *    (`makePopPathRemap`; first fetch falls back to path presence);
 *  - a pop with no stored eye (new, or never seen) is seeded from the manager's `show` — or OFF with
 *    `seedOff` (one-at-a-time mode: a pop gated meanwhile must not break the one-pop view).
 * Eyes of pop types NOT in `fetchedTypes` are kept untouched — the panel switched that type off,
 * which is not the user forgetting what they had shown in it.
 */
export function reconcilePopEyes(
  stored: Readonly<PopEyes>, prev: readonly EyePop[], next: readonly EyePop[],
  fetchedTypes: readonly string[], seedOff = false,
): PopEyes {
  const fetched = new Set(fetchedTypes)
  const out: PopEyes = {}
  const ident = (p: EyePop): PopIdent => ({ key: p.path, uid: p.uid ?? '' })
  const remaps = new Map([...fetched].map(pt => [pt, makePopPathRemap(
    prev.filter(p => p.popType === pt).map(ident), next.filter(p => p.popType === pt).map(ident))]))
  for (const [k, v] of Object.entries(stored)) {
    const i = k.indexOf('::')
    const popType = i < 0 ? '' : k.slice(0, i), path = i < 0 ? k : k.slice(i + 2)
    const remap = remaps.get(popType)
    if (!remap) { out[k] = v; continue }
    const to = remap(path)
    if (to !== null) out[eyeKey(popType, to)] = v
  }
  for (const p of next) {
    const k = eyeKey(p.popType, p.path)
    if (!(k in out)) out[k] = seedOff ? false : p.show
  }
  return out
}

/**
 * One segmentation's RIBBON eyes after a fetch — `reconcilePopEyes`, so a gate edit keeps the Tracks
 * rows as the user left them (#1766). The one addition: while nothing is stored yet, a path in
 * `legacyHidden` (a ribbon hide kept per path before the ribbons had eyes — `getTrackPopHidden`)
 * seeds off, so an upgrade does not bring back ribbons the user had hidden.
 */
export function reconcileRibbonEyes(
  stored: Readonly<PopEyes>, legacyHidden: ReadonlySet<string>,
  prev: readonly EyePop[], next: readonly EyePop[], fetchedTypes: readonly string[],
): PopEyes {
  const legacy = Object.keys(stored).length ? new Set<string>() : legacyHidden
  const seeded = next.map(p => legacy.has(p.path) ? { ...p, show: false } : p)
  return reconcilePopEyes(stored, prev, seeded, fetchedTypes)
}

/** One pop's eye flipped from what it reads now (`popEyeOn`). Keyed by segmentation; not mutated. */
export function flipPopEye(eyes: ReadonlyMap<string, Readonly<PopEyes>>, vn: string,
                           popType: string, path: string, show: boolean): Map<string, PopEyes> {
  const out = new Map([...eyes].map(([v, e]) => [v, { ...e }]))
  out.set(vn, { ...out.get(vn) ?? {}, [eyeKey(popType, path)]: !popEyeOn(eyes, vn, popType, path, show) })
  return out
}

/** Same contents — so a reconcile that changed nothing does not rewrite storage. */
export function samePopEyes(a: Readonly<PopEyes>, b: Readonly<PopEyes>): boolean {
  const ka = Object.keys(a)
  return ka.length === Object.keys(b).length && ka.every(k => k in b && a[k] === b[k])
}

/** A pop listed in the section, addressed across segmentations. */
export interface ListedPop { vn: string; popType: string; path: string }

/** Every listed pop on (`visible`) or off. Pops not listed keep their eye. Keyed by segmentation. */
export function setAllPopEyes(
  eyes: ReadonlyMap<string, Readonly<PopEyes>>, listed: readonly ListedPop[], visible: boolean,
): Map<string, PopEyes> {
  const out = new Map([...eyes].map(([vn, e]) => [vn, { ...e }]))
  for (const p of listed) {
    if (!out.has(p.vn)) out.set(p.vn, {})
    out.get(p.vn)![eyeKey(p.popType, p.path)] = visible
  }
  return out
}

/** Only listed pop `keep` on, every other listed pop off — `soloVisibility` over the listed order,
 *  so an out-of-range `keep` (nothing was on) falls back to the first pop, as for channels. */
export function soloPopEyes(
  eyes: ReadonlyMap<string, Readonly<PopEyes>>, listed: readonly ListedPop[], keep: number,
): Map<string, PopEyes> {
  const vis = soloVisibility(listed.length, keep)
  const out = new Map([...eyes].map(([vn, e]) => [vn, { ...e }]))
  listed.forEach((p, i) => {
    if (!out.has(p.vn)) out.set(p.vn, {})
    out.get(p.vn)![eyeKey(p.popType, p.path)] = vis[i]
  })
  return out
}

/** Whether a pop's eye is on. A pop with no stored eye (not reconciled yet) reads its manager `show`. */
export function popEyeOn(eyes: ReadonlyMap<string, Readonly<PopEyes>>, vn: string,
                         popType: string, path: string, show: boolean): boolean {
  return eyes.get(vn)?.[eyeKey(popType, path)] ?? show
}

/** The paths whose eye is off, sorted — the ribbon eyes' projection onto `settings.setTrackPopHidden`,
 *  the per-path shape the movie recorder (`viewerLook`) and the Julia renderer take. */
export function offPaths(eyes: Readonly<PopEyes>): string[] {
  const out = new Set<string>()
  for (const [k, v] of Object.entries(eyes)) {
    if (v) continue
    const i = k.indexOf('::')
    out.add(i < 0 ? k : k.slice(i + 2))
  }
  return [...out].sort()
}
