// KiwiRef — one typed pointer to an app object, used as a Kiwi prompt chip, as the evidence on a
// claim in Kiwi's reply, and as the target Kiwi points at (docs/todo/KIWI_ASSISTANT_PLAN.md → *The
// reference type*). The SHAPE lives in `lib/kiwiRef.schema.json`, which the Julia resolver
// (`api/src/kiwi_refs.jl`) reads too; the union below is its TS mirror, and `kiwiRef.test.ts` fails if
// the two drift (kinds, required fields, optional fields). Refs are project-relative — the projectUid
// travels with the request. Pure; no DOM.

import schema from '../lib/kiwiRef.schema.json'
import type { CaptureAddress } from './captureAddress'

export type KiwiRef =
  | { kind: 'project' }
  | { kind: 'set'; setUid: string }
  | { kind: 'image'; imageUid: string }
  | { kind: 'population'; imageUid: string; valueName: string; popPath: string }
  | { kind: 'cells'; imageUid: string; valueName: string; labelIds: number[] }
  | { kind: 'tracks'; imageUid: string; valueName: string; trackIds: number[] }
  | { kind: 'viewer'; imageUid: string; t?: number; z?: number }
  | { kind: 'plot'; plotId: string; u?: number; v?: number }
  | { kind: 'tile'; imageUid: string; valueName: string; cellId: string; t?: number; z?: number }
  | { kind: 'capture'; captureId: string }
  | { kind: 'task'; funName: string }
  | { kind: 'ui'; anchor: string }
  | { kind: 'blackboard'; entryId: string; version?: number }

export type KiwiRefKind = KiwiRef['kind']

/** Every kind, in schema order — read from the schema, not restated. */
export const KIWI_REF_KINDS: string[] = schema.oneOf.map((b) => b.$ref.split('/').pop() as string)

/** How far the resolver could check a ref (`POST /api/kiwi/refs/resolve`). The UI must not render
 *  these alike (plan Decision 10): `exists` = on disk; `live` = an open panel/landscape, true now and
 *  gone when it closes; `format` = shape only (UI anchors); `shape` = the ref itself is malformed. */
export type KiwiRefCheck = 'exists' | 'live' | 'format' | 'shape'

export interface KiwiRefResult {
  ok: boolean
  check: KiwiRefCheck
  label: string
  error: string
  /** what the object holds, in a line — only some kinds say (a plot: its series, grouping, images) */
  detail?: string
  /** a plot's page — where to reopen it once its panel has closed */
  route?: string
}

/** The refs a share-in capture points at: its image position (`viewer`) and, for a UI capture, its
 *  anchor (`ui`). A plot capture's `plotSpec.specId` names the plot TYPE, not the live panel, so it
 *  is not a `plot` ref — that comes from the plot registry's plotId when the chip is made. */
export function refsFromCaptureAddress(addr: CaptureAddress): KiwiRef[] {
  const out: KiwiRef[] = []
  if (addr.imageUid) {
    const v: KiwiRef = { kind: 'viewer', imageUid: addr.imageUid }
    // a slab capture carries t as [from, to]; a viewer ref is one position, so take the start
    const t = Array.isArray(addr.t) ? addr.t[0] : addr.t
    if (typeof t === 'number' && t >= 0) v.t = t
    if (typeof addr.z === 'number' && addr.z >= 0) v.z = addr.z
    out.push(v)
  }
  if (addr.domAnchor) out.push({ kind: 'ui', anchor: addr.domAnchor })
  return out
}
