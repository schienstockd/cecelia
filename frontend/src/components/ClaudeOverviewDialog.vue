<!--
  "What can Claude do here?" — a brief, visual how-to for the two Claude entry points (Ask / Chat)
  and what the assistant can see / suggest / create / can't do. Opened on demand from the lab-log
  toolbar's info button; static reference, no live data. Content lives in lib/claudeOverview.ts
  (testable, one place to edit). Built on the shared BaseModal shell (docs/UI.md → "Modals & dialogs").
-->
<script setup lang="ts">
import BaseModal from './BaseModal.vue'
import {
  CLAUDE_ENTRY_POINTS, CLAUDE_CAPABILITIES, CLAUDE_EXAMPLES,
} from '../lib/claudeOverview'

defineEmits<{ (e: 'close'): void }>()
</script>

<template>
  <BaseModal title="What Claude can do here" icon="pi-sparkles" width="620px" @close="$emit('close')">
    <!-- two entry points, side by side: the how-to -->
    <div class="co-entries">
      <div v-for="e in CLAUDE_ENTRY_POINTS" :key="e.name" class="co-entry">
        <div class="co-entry-head"><i :class="['pi', e.icon]" /> {{ e.name }}</div>
        <p class="co-entry-what">{{ e.what }}</p>
        <ol class="co-steps">
          <li v-for="(s, i) in e.steps" :key="i">{{ s }}</li>
        </ol>
      </div>
    </div>

    <!-- capability grid: sees / suggests / creates / can't -->
    <div class="co-grid">
      <div v-for="g in CLAUDE_CAPABILITIES" :key="g.key" class="co-cell" :class="'tone-' + g.tone">
        <div class="co-cell-head"><i :class="['pi', g.icon]" /> {{ g.title }}</div>
        <ul>
          <li v-for="(it, i) in g.items" :key="i">{{ it }}</li>
        </ul>
      </div>
    </div>

    <!-- example prompts to try -->
    <div class="co-examples">
      <span class="co-examples-label">Try asking</span>
      <div class="co-chips">
        <span v-for="(ex, i) in CLAUDE_EXAMPLES" :key="i" class="co-chip">{{ ex }}</span>
      </div>
    </div>
  </BaseModal>
</template>

<style scoped>
.co-entries { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-bottom: 14px; }
.co-entry {
  border: 1px solid var(--cc-border); border-radius: 8px; padding: 10px 12px;
  background: var(--cc-surface-2);
}
.co-entry-head { font-weight: 600; color: var(--cc-text); display: flex; align-items: center; gap: 6px; }
.co-entry-head .pi { color: var(--cc-accent); }
.co-entry-what { margin: 6px 0 8px; color: var(--cc-text-dim); font-size: 0.85rem; line-height: 1.35; }
.co-steps { margin: 0; padding-left: 18px; color: var(--cc-text); font-size: 0.82rem; line-height: 1.5; }

.co-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; }
.co-cell { border: 1px solid var(--cc-border); border-radius: 8px; padding: 8px 12px; }
.co-cell-head { font-weight: 600; display: flex; align-items: center; gap: 6px; margin-bottom: 4px; }
.co-cell ul { margin: 0; padding-left: 18px; font-size: 0.8rem; line-height: 1.5; color: var(--cc-text); }
.co-cell.tone-good .co-cell-head { color: #56d364; }
.co-cell.tone-good .co-cell-head .pi { color: #56d364; }
.co-cell.tone-neutral .co-cell-head .pi { color: var(--cc-accent); }
.co-cell.tone-muted { opacity: 0.85; }
.co-cell.tone-muted .co-cell-head { color: var(--cc-text-dim); }

.co-examples { margin-top: 14px; }
.co-examples-label { display: block; font-size: 0.78rem; color: var(--cc-text-dim); margin-bottom: 6px; }
.co-chips { display: flex; flex-wrap: wrap; gap: 6px; }
.co-chip {
  font-size: 0.78rem; color: var(--cc-text); background: var(--cc-surface-2);
  border: 1px solid var(--cc-border); border-radius: 999px; padding: 3px 10px;
}
</style>
