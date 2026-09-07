import { ref, watch, type Ref } from 'vue'

/**
 * Accordion state — ONE section open at a time, persisted to localStorage.
 *
 * Two callers today: the viewer window's control accordion (Overlays / Channels / Segmentation /
 * Populations / …) and the app sidebar's nav-group accordion (Data / Populations / Explore / …).
 * Same rule, one implementation — a bespoke second copy is the divergent-re-implementation bug
 * (CLAUDE.md → "one way to do each thing; the second way is the bug").
 *
 * Behaviour:
 * - `open` starts from localStorage if present, else `defaultOpen`.
 * - `toggle(key)` opens `key` if a different section (or nothing) is open; closes it if it is
 *   already open. `''` is a real state (everything collapsed), because clicking the currently-open
 *   header being a no-op is worse — the user has no way to see everything at once.
 * - Persists on every change.
 */
export interface SingleOpenSection {
  open: Ref<string>
  isOpen(key: string): boolean
  toggle(key: string): void
  set(key: string): void
}

export function useSingleOpenSection(storageKey: string, defaultOpen: string = ''): SingleOpenSection {
  const stored = typeof window !== 'undefined' ? localStorage.getItem(storageKey) : null
  const open = ref<string>(stored ?? defaultOpen)
  watch(open, v => {
    if (typeof window !== 'undefined') localStorage.setItem(storageKey, v)
  })

  return {
    open,
    isOpen: (key: string) => open.value === key,
    toggle: (key: string) => { open.value = open.value === key ? '' : key },
    set:    (key: string) => { open.value = key },
  }
}
