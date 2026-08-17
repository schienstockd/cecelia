// Shared open/close state for the icon glossary, mirroring `lib/guideOpen.ts` / `lib/whatsNew.ts` —
// one dialog mounted in App.vue, any number of callers.
//
// The glossary sits beside the compass in the header rather than inside Settings: "what does this
// symbol mean?" is asked WHILE looking at the symbol, and a Settings page is the wrong distance away.

import { ref } from 'vue'

export const isIconLegendOpen = ref(false)

export function openIconLegend() { isIconLegendOpen.value = true }
export function closeIconLegend() { isIconLegendOpen.value = false }
