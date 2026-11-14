// Shared open/close state for the feature-maturity dialog, mirroring `lib/iconLegendOpen.ts` — one
// dialog mounted in App.vue, any number of callers.
//
// Its own dialog, not a section of Guides: it is where every maturity dot leads (a page's in the
// sidebar, a task's in TaskRunner — `MaturityIndicator.vue`), and "how far can I trust this?" is not a
// "what do I do next?" question. Also in Help ▾ → Feature maturity.

import { ref } from 'vue'

export const isMaturityOpen = ref(false)

export function openMaturity() { isMaturityOpen.value = true }
export function closeMaturity() { isMaturityOpen.value = false }

/** The row to open and scroll to when the dialog mounts — a page's or task's maturity link
 *  (`MaturityMap.vue` consumes it once). Row ids are `utils/maturity.ts`' (`guide:…`, `task:…`). */
export const maturityFocus = ref<string | null>(null)

export function openMaturityRow(rowId: string) {
  maturityFocus.value = rowId
  openMaturity()
}
