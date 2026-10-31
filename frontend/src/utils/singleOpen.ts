import { computed, ref, type Ref, type WritableComputedRef } from 'vue'
import type { KV } from './profileStorage'

/**
 * The one-open-at-a-time accordion engine: a registry of named groups, one state per name.
 *
 * A group is keyed by its STORAGE KEY, and that string is also the keyword a `CollapsibleSection`
 * takes (`accordion="cc.vw.section"`) — so every section, and every script, that names the same key
 * in this app instance shares ONE `open` ref. There is never a second persisted ref for a key, which
 * is what would race (two refs each writing their own idea of "open" to the same storage entry).
 *
 * State:
 * - `open` — the open section's id; `''` is a real state (everything collapsed, persisted like any
 *   other choice — a no-op click on the open header would leave no way to see everything at once).
 * - Before the user has chosen anything (nothing stored), `open` reads the group's DEFAULT. The
 *   default is not written to storage, so a later change of default reaches users who never chose.
 * - The first non-empty default offered wins (`group(key, 'ask')` from a script, or a section marked
 *   `default-open`); declare it in one place.
 *
 * Pure apart from the injected `kv`, so it is unit-tested (`singleOpen.test.ts`); the app's instance
 * over `profileStorage` is `composables/useSingleOpenSection.ts`.
 */
export interface SingleOpenGroup {
  /** The open section's id ('' = all collapsed). Writable: assigning it is `set`. */
  open: WritableComputedRef<string>
  /** True once the user (or code) has chosen — false while `open` is still the default. */
  chosen: Readonly<Ref<boolean>>
  isOpen(id: string): boolean
  /** Opens `id`; closes it if it is already open. */
  toggle(id: string): void
  set(id: string): void
  /** Offer `id` as the default. Ignored if one is already declared. */
  offerDefault(id: string): void
}

/** The storage surface the registry needs — `profileStorage`'s own injectable `KV`, read/write only. */
export type SingleOpenKV = Pick<KV, 'getItem' | 'setItem'>

export interface SingleOpenOptions {
  /** Default true. `false` = state lives only for this app instance (e.g. a dialog whose open group
   *  follows the current page) — the caller usually `set`s it on mount. */
  persist?: boolean
}

export type SingleOpenRegistry =
  (key: string, defaultOpen?: string, opts?: SingleOpenOptions) => SingleOpenGroup

/** `kv` is read lazily (on a group's first use), so `null` — no storage, e.g. SSR/tests — is fine. */
export function createSingleOpenRegistry(kv: () => SingleOpenKV | null): SingleOpenRegistry {
  const groups = new Map<string, SingleOpenGroup>()

  return (key, defaultOpen = '', opts = {}) => {
    let g = groups.get(key)
    if (!g) {
      const persist = opts.persist !== false
      const store = persist ? kv() : null
      const picked = ref<string | null>(store ? store.getItem(key) : null)
      const fallback = ref('')
      const open = computed<string>({
        get: () => picked.value ?? fallback.value,
        set: (v: string) => {
          picked.value = v
          if (store) { try { store.setItem(key, v) } catch { /* storage full / blocked: keep in memory */ } }
        },
      })
      const set = (id: string) => { open.value = id }
      g = {
        open,
        chosen: computed(() => picked.value !== null),
        isOpen: (id: string) => open.value === id,
        toggle: (id: string) => set(open.value === id ? '' : id),
        set,
        offerDefault: (id: string) => { if (id && !fallback.value) fallback.value = id },
      }
      groups.set(key, g)
    }
    g.offerDefault(defaultOpen)
    return g
  }
}
