// Shared open/close state for the Call for Datasets modal, mirroring `lib/colorLegendOpen.ts` —
// one dialog mounted in App.vue, any number of callers.
//
// A second field carries the ASK to scroll to on open: `?ask=<id>` deep links (from a vis-aid
// column or a task-param placeholder) open the modal AT the relevant card rather than at the top
// of the list — otherwise a reader clicks a "Request this" chip and then has to scan a scrolling
// dialog to find the card they came for.

import { ref } from 'vue'

export const isCallForDatasetsOpen = ref(false)
/** The ask id to scroll to on open, or null for "top of the list". */
export const callForDatasetsFocusId = ref<string | null>(null)

export function openCallForDatasets(focusId: string | null = null) {
  callForDatasetsFocusId.value = focusId
  isCallForDatasetsOpen.value = true
}
export function closeCallForDatasets() {
  isCallForDatasetsOpen.value = false
  callForDatasetsFocusId.value = null
}
