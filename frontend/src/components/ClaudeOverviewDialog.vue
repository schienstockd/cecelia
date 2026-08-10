<!--
  "What can Claude do here?" — a brief, visual how-to for the two Claude entry points (Ask / Chat)
  and what the assistant can see / suggest / create / can't do. Opened on demand from the lab-log
  toolbar's info button; static reference, no live data. Content lives in lib/claudeOverview.ts
  (testable, one place to edit). Built on the shared BaseModal shell (docs/UI.md → "Modals & dialogs").
-->
<script setup lang="ts">
import BaseModal from './BaseModal.vue'
import { computed } from 'vue'
import { CLAUDE_ENTRY_POINTS, claudeCapabilities, CLAUDE_EXAMPLES } from '../lib/claudeOverview'
import { useSettingsStore } from '../stores/settings'

defineEmits<{ (e: 'close'): void }>()

// A connector the user hid in Settings is dropped from the capability list too — the dialog must not
// advertise what the prompt no longer offers (lib/claudeOverview.ts → claudeCapabilities).
const settings = useSettingsStore()
const capabilities = computed(() => claudeCapabilities(settings.hiddenMcpAccounts))

// Terminal set-up is NOT repeated here. It lives in the lab-log toolbar — one button, in the place
// people act from — and duplicating it in this dialog cost a whole band of chrome to say what the
// toolbar already shows. This dialog is reference only: no live state, no controls.
</script>

<template>
  <BaseModal title="What Claude can do here" icon="pi-sparkles" width="620px" @close="$emit('close')">
    <!-- two entry points, side by side: the how-to -->
    <div class="co-entries">
      <div v-for="e in CLAUDE_ENTRY_POINTS" :key="e.name" class="co-entry cc-card cc-card-2">
        <div class="co-entry-head"><i :class="['pi', e.icon]" /> {{ e.name }}</div>
        <p class="co-entry-what cc-muted cc-fs-md">{{ e.what }}</p>
        <ol class="co-steps">
          <li v-for="(s, i) in e.steps" :key="i">{{ s }}</li>
        </ol>
      </div>
    </div>

    <!-- capability rows: sees / suggests / creates / can't -->
    <div class="co-rows">
      <div v-for="g in capabilities" :key="g.key" class="co-cell" :class="'tone-' + g.tone">
        <div class="co-cell-head"><i :class="['pi', g.icon]" /> {{ g.title }}</div>
        <ul>
          <li v-for="(it, i) in g.items" :key="i">{{ it }}</li>
        </ul>
      </div>
    </div>

    <!-- example prompts to try -->
    <div class="co-examples">
      <span class="co-examples-label cc-muted">Try asking</span>
      <div class="co-chips cc-row cc-row-tight">
        <span v-for="(ex, i) in CLAUDE_EXAMPLES" :key="i" class="co-chip">{{ ex }}</span>
      </div>
    </div>
  </BaseModal>
</template>

<style scoped>
.co-entries { display: grid; grid-template-columns: 1fr 1fr; gap: 14px; margin-bottom: 18px; }
/* a raised card ON the dialog's surface-1 body — the surface-2 variant of .cc-card */
.co-entry { padding: 14px 16px; }
.co-entry-head { font-weight: 600; color: var(--cc-text); display: flex; align-items: center; gap: 6px; }
.co-entry-head .pi { color: var(--cc-accent); }
.co-entry-what { margin: 8px 0 10px; line-height: 1.4; }
.co-steps { margin: 0; padding-left: 18px; color: var(--cc-text); font-size: var(--cc-fs-md); line-height: 1.55; }

/* One ROW per capability: the heading titles the box, the items flow beneath it in two columns. The
   2×2 grid ran ragged once the lists grew to different lengths, and giving the heading its own
   column just traded that for a band of empty space — so it sits on top and the full width goes to
   the list. */
.co-rows { display: grid; grid-template-columns: 1fr; gap: 10px; }
.co-cell { border: 1px solid var(--cc-border); border-radius: var(--cc-radius-lg); padding: 12px 14px; }
.co-cell-head { font-weight: 600; display: flex; align-items: center; gap: 6px; margin-bottom: 6px; }
.co-cell ul {
  margin: 0; padding-left: 18px; font-size: var(--cc-fs-md); line-height: 1.55; color: var(--cc-text);
  columns: 2; column-gap: 26px;
}
.co-cell li { break-inside: avoid; }
.co-cell.tone-good .co-cell-head { color: #56d364; }
.co-cell.tone-good .co-cell-head .pi { color: #56d364; }
.co-cell.tone-neutral .co-cell-head .pi { color: var(--cc-accent); }
.co-cell.tone-muted { opacity: 0.85; }
.co-cell.tone-muted .co-cell-head { color: var(--cc-text-dim); }

.co-examples { margin-top: 18px; }
.co-examples-label { display: block; margin-bottom: 6px; }

.co-chip {
  font-size: var(--cc-fs-sm); color: var(--cc-text); background: var(--cc-surface-2);
  border: 1px solid var(--cc-border); border-radius: var(--cc-radius-pill); padding: 3px 10px;
}
</style>
