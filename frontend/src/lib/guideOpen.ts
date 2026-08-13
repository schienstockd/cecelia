// Shared open/close state for the guide PICKER, mirroring `lib/whatsNew.ts` — one dialog mounted in
// App.vue, any number of callers.
//
// Deliberately separate from `stores/guide.ts`: that store is the RUNTIME (which guide is running,
// which step, what it's waiting for) and lives for the app's lifetime; this is one boolean for a modal.
// Putting the flag in the store would make every consumer of "is the picker open" instantiate the
// runtime, and `GuideBubble` would import the dialog's state to close it.
//
// Entry points: the compass button in AppHeader, the "Show me" button on a What's New tip card
// (`WhatNewCard`), and the bubble's "More guides" when a guide finishes.

import { ref } from 'vue'

export const isGuidesOpen = ref(false)

export function openGuides() { isGuidesOpen.value = true }
export function closeGuides() { isGuidesOpen.value = false }
