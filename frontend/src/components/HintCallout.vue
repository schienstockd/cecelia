<script setup lang="ts">
// One-line, first-use-only guidance. Dismissed permanently on click, per hint id, in localStorage
// (`cc.hint.<id>`). Not a tour, not a modal — an inline callout that disappears once acknowledged.
// See docs/todo/ONBOARDING_PLAN.md (P4). Reuses the settings-store localStorage idiom.
import { ref } from 'vue'

const props = defineProps<{ hintKey: string; text: string }>()

const _key = `cc.hint.${props.hintKey}`
const dismissed = ref(localStorage.getItem(_key) === '1')

function dismiss() {
  dismissed.value = true
  try { localStorage.setItem(_key, '1') } catch { /* private mode — just hide for this session */ }
}
</script>

<template>
  <div v-if="!dismissed" class="hint-callout" role="note">
    <i class="pi pi-info-circle hint-icon" />
    <span class="hint-text">{{ text }}</span>
    <button class="hint-x cc-btn cc-btn-bare cc-btn-icon cc-btn-dense" @click="dismiss" v-tooltip.left="'Dismiss'" aria-label="Dismiss hint">
      <i class="pi pi-times" />
    </button>
  </div>
</template>

<style scoped>
.hint-callout {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.4rem 0.75rem;
  background: color-mix(in srgb, var(--cc-accent) 12%, var(--cc-surface-1));
  border-bottom: 1px solid var(--cc-border);
  border-left: 2px solid var(--cc-accent);
  font-size: var(--cc-fs-md);
  color: var(--cc-text);
  flex-shrink: 0;
}
.hint-icon { color: var(--cc-accent); font-size: var(--cc-fs-md); flex-shrink: 0; }
.hint-text { flex: 1; }
/* .hint-x → cc-btn cc-btn-bare cc-btn-icon */
.hint-x:hover { background: var(--cc-surface-2); color: var(--cc-text); }
</style>
