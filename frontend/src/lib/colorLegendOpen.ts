// Shared open/close state for the colour glossary, mirroring `lib/iconLegendOpen.ts` — one dialog
// mounted in App.vue, any number of callers.
//
// Sits beside the icon key in the header, not inside Settings: "what does this colour mean?" is
// asked WHILE looking at the swatch, and a Settings page is the wrong distance away.

import { ref } from 'vue'

export const isColorLegendOpen = ref(false)

export function openColorLegend() { isColorLegendOpen.value = true }
export function closeColorLegend() { isColorLegendOpen.value = false }
