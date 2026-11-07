// Shared open/close state for the AI opt-in dialog (docs/todo/AI_TOGGLE_PLAN.md P3). Same pattern as
// `lib/preferencesOpen.ts`: `AiOptInDialog` is mounted ONCE in App.vue and driven by this ref. Every
// "Turn on…" — Preferences → Kiwi, Kiwi's AI-off panel — calls `openAiOptIn()`, so consent is asked
// in one place.
import { ref } from 'vue'

export const isAiOptInOpen = ref(false)
export function openAiOptIn() { isAiOptInOpen.value = true }
export function closeAiOptIn() { isAiOptInOpen.value = false }
