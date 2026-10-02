// Per-profile localStorage — a drop-in for `localStorage.getItem/setItem/removeItem` on any key that
// describes how a PERSON works (a dismissed hint, a remembered filter, a half-typed Kiwi question).
//
// localStorage stays the synchronous read path — every call site reads at setup time and can't await —
// but it is now a MIRROR of the active profile's bag (`<config_dir>/user-profiles/<name>/settings.toml`,
// keys prefixed `ls:`). Writes go to both; reads come from the mirror.
//
// The mirror records whose it is (`cc.profileMirrorOf`). `adoptProfileBag` runs once per boot when the
// settings store hydrates:
//   • same owner (or none yet — a browser from before this mirror existed) → the bag wins over the
//     mirror for keys it has; mirror-only keys are bootstrapped up into the bag.
//   • different owner → another person used this browser last. Their mirror is cleared, the bag is
//     written down, and the caller reloads so every store and component re-reads from the new person's
//     state — including defaults for anything the new profile has never set.
//
// The keys that belong to the mirror are learned, not listed: any key read or written through this
// module is recorded in `cc.profileMirrorKeys`. Cross-window channels (`cc.openProject`, `cc.uiLog`, …)
// and per-machine renderer knobs never come through here, so they are never cleared.
//
// Classification of which keys belong here: docs/audit/persistence-audit.md.

import { debouncedSave } from './debouncedSave'
import { patchProfileSettings, type ProfileSettingsValue } from './profileSettingsApi'

export const BAG_PREFIX = 'ls:'
export const OWNER_KEY = 'cc.profileMirrorOf'
export const INDEX_KEY = 'cc.profileMirrorKeys'
const PATCH_WAIT_MS = 400

/** The localStorage surface this module needs — injectable so the pure core is testable. */
export interface KV {
  getItem(k: string): string | null
  setItem(k: string, v: string): void
  removeItem(k: string): void
}

export interface AdoptPlan {
  /** True when the mirror belonged to someone else and was replaced — the caller must reload. */
  reload: boolean
  /** Mirror keys present locally but absent from the bag — PATCH these up. */
  bootstrap: Record<string, string>
}

/**
 * Reconcile the localStorage mirror with the active profile's bag. Pure apart from `kv`.
 *
 * `extraMirrorKeys` are mirror keys owned by another module that keeps its own key list (the settings
 * store's `cc.<PROFILE_KEY>` mirror). They are cleared on an owner change but never bootstrapped here —
 * their owner does that.
 */
export function adoptProfileBag(kv: KV, profile: string, bag: Record<string, ProfileSettingsValue>,
                                extraMirrorKeys: readonly string[] = []): AdoptPlan {
  const owner = kv.getItem(OWNER_KEY)
  const index = readIndex(kv)
  const bagLs: Record<string, string> = {}
  for (const [k, v] of Object.entries(bag)) {
    if (k.startsWith(BAG_PREFIX) && typeof v === 'string') bagLs[k.slice(BAG_PREFIX.length)] = v
  }

  if (owner !== null && owner !== profile) {
    for (const k of index) kv.removeItem(k)
    for (const k of extraMirrorKeys) kv.removeItem(k)
    for (const [k, v] of Object.entries(bagLs)) kv.setItem(k, v)
    writeIndex(kv, new Set(Object.keys(bagLs)))
    kv.setItem(OWNER_KEY, profile)
    return { reload: true, bootstrap: {} }
  }

  const bootstrap: Record<string, string> = {}
  for (const k of index) {
    if (k in bagLs) continue
    const v = kv.getItem(k)
    if (v !== null) bootstrap[k] = v
  }
  for (const [k, v] of Object.entries(bagLs)) { kv.setItem(k, v); index.add(k) }
  writeIndex(kv, index)
  kv.setItem(OWNER_KEY, profile)
  return { reload: false, bootstrap }
}

/** Point the mirror at a profile's new name, so a rename isn't mistaken for a change of person. */
export function renameMirrorOwner(kv: KV, from: string, to: string): void {
  if (kv.getItem(OWNER_KEY) === from) kv.setItem(OWNER_KEY, to)
}

function readIndex(kv: KV): Set<string> {
  try {
    const v = JSON.parse(kv.getItem(INDEX_KEY) ?? '[]')
    return new Set(Array.isArray(v) ? v.filter((x): x is string => typeof x === 'string') : [])
  } catch { return new Set() }
}
function writeIndex(kv: KV, s: Set<string>): void { kv.setItem(INDEX_KEY, JSON.stringify([...s])) }

// ── Live instance over window.localStorage ──────────────────────────────────────────────────────

const _ls: KV = {
  getItem: k => { try { return localStorage.getItem(k) } catch { return null } },
  setItem: (k, v) => { try { localStorage.setItem(k, v) } catch { /* private mode — session only */ } },
  removeItem: k => { try { localStorage.removeItem(k) } catch { /* private mode */ } },
}

let _known: Set<string> | null = null
function _register(k: string) {
  _known ??= readIndex(_ls)
  if (_known.has(k)) return
  _known.add(k)
  writeIndex(_ls, _known)
}

const _pending: Record<string, ProfileSettingsValue> = {}
const _save = debouncedSave(async () => {
  const patch = { ..._pending }
  for (const k of Object.keys(_pending)) delete _pending[k]
  if (Object.keys(patch).length) await patchProfileSettings(patch)
}, { wait: PATCH_WAIT_MS })

function _queue(k: string, v: string | null) {
  _pending[BAG_PREFIX + k] = v
  _save.schedule()
}

if (typeof window !== 'undefined') window.addEventListener('beforeunload', () => { void _save.flush() })

/** localStorage-shaped, profile-backed. Use for per-person UI state; see the header for what not to. */
export const profileStorage: KV = {
  getItem(k) { _register(k); return _ls.getItem(k) },
  setItem(k, v) { _register(k); _ls.setItem(k, v); _queue(k, v) },
  removeItem(k) { _register(k); _ls.removeItem(k); _queue(k, null) },
}

/** Boot-time reconcile against the live localStorage; queues the bootstrap PATCH. */
export function adoptLiveProfileBag(profile: string, bag: Record<string, ProfileSettingsValue>,
                                    extraMirrorKeys: readonly string[]): boolean {
  const plan = adoptProfileBag(_ls, profile, bag, extraMirrorKeys)
  _known = null
  if (plan.reload) return true
  for (const [k, v] of Object.entries(plan.bootstrap)) _queue(k, v)
  return false
}

export function renameLiveMirrorOwner(from: string, to: string): void { renameMirrorOwner(_ls, from, to) }
