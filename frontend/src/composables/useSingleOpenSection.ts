import { profileStorage } from '../utils/profileStorage'
import {
  createSingleOpenRegistry, type SingleOpenGroup, type SingleOpenOptions,
} from '../utils/singleOpen'

/**
 * Accordion state — ONE section open at a time, persisted per profile (profileStorage).
 *
 * Usually you don't call this: give each `CollapsibleSection` the same `accordion` keyword (the
 * storage key) and a `section` id, and they share the group. Call it when a SCRIPT needs the state
 * too — the viewer window reads `open` for its own layout, the sidebar (plain headings, not
 * CollapsibleSections) opens the current page's group on navigation, the guides dialog opens the
 * group the map points at. Same key ⇒ the same state object as the sections', never a second copy
 * (`utils/singleOpen.ts`: one registry per app instance, keyed by name).
 *
 * A profile switch reloads the page (`profileStorage.adoptLiveProfileBag`), so the registry never
 * holds another person's choice.
 */
export type SingleOpenSection = SingleOpenGroup

const registry = createSingleOpenRegistry(() => (typeof window !== 'undefined' ? profileStorage : null))

export function useSingleOpenSection(storageKey: string, defaultOpen: string = '',
                                     opts?: SingleOpenOptions): SingleOpenSection {
  return registry(storageKey, defaultOpen, opts)
}
