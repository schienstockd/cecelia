<!--
  "What Kiwi does here" — how Claude fits the work (the four-step loop) and what each way in can do
  (Kiwi vs a terminal session). Opened on demand from the Kiwi header's `?` button; static reference,
  no live data. Content lives in lib/claudeOverview.ts — the SAME loop the "Assist with Kiwi" guide
  and user-docs/README.md use, so the three never drift apart. The documentation-helper framing (Kiwi
  is a record, not a verdict; see docs/archive/kiwi-purpose-and-framing.md) is the loop's "Keep it"
  step and the provisional line. Built on the shared BaseModal shell (docs/UI.md → "Modals & dialogs").
-->
<script setup lang="ts">
import BaseModal from './BaseModal.vue'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'
import { computed } from 'vue'
import { CLAUDE_LOOP, claudeAccess, CLAUDE_PROVISIONAL } from '../lib/claudeOverview'
import { useSettingsStore } from '../stores/settings'

defineEmits<{ (e: 'close'): void }>()

// A connector the user hid in Settings is dropped from the table too — the dialog must not advertise
// what the prompt no longer offers (lib/claudeOverview.ts → claudeAccess).
const settings = useSettingsStore()
const accessRows = computed(() => claudeAccess(settings.hiddenMcpAccounts).map(r => ({ ...r })))
const ACCESS_COLUMNS: SelectionColumn[] = [
  { key: 'text', label: '' },
  { key: 'kiwi', label: 'Kiwi', width: 72 },
  { key: 'terminal', label: 'Terminal', width: 72 },
]

// Terminal set-up is NOT repeated here. It lives on Kiwi's Terminal row — one button, in the place
// people act from — and duplicating it in this dialog cost a whole band of chrome to say what the
// row already shows. This dialog is reference only: no live state, no controls.
</script>

<template>
  <BaseModal title="What Kiwi does here" icon="pi-sparkles" width="620px" @close="$emit('close')">
    <!-- the loop: four numbered steps -->
    <ol class="co-loop">
      <li v-for="(s, i) in CLAUDE_LOOP" :key="i" class="co-step cc-card cc-card-2">
        <span class="co-num">{{ i + 1 }}</span>
        <div class="co-step-title">{{ s.title }}</div>
        <div class="co-step-text cc-muted cc-fs-sm">{{ s.text }}</div>
      </li>
    </ol>

    <!-- Kiwi vs terminal: icons carry the meaning, not colour alone -->
    <SelectionTable selection-mode="none" density="compact" :columns="ACCESS_COLUMNS"
                    :rows="accessRows" id-key="text">
      <template #cell-kiwi="{ row }">
        <i :class="['pi', row.kiwi ? 'pi-check co-yes' : 'pi-minus co-no']" :aria-label="row.kiwi ? 'yes' : 'no'" />
      </template>
      <template #cell-terminal="{ row }">
        <i :class="['pi', row.terminal ? 'pi-check co-yes' : 'pi-minus co-no']" :aria-label="row.terminal ? 'yes' : 'no'" />
      </template>
    </SelectionTable>
    <p class="co-note cc-muted cc-fs-sm">{{ CLAUDE_PROVISIONAL }}</p>
  </BaseModal>
</template>

<style scoped>
.co-loop { list-style: none; margin: 0 0 18px; padding: 0; display: grid; grid-template-columns: repeat(4, 1fr); gap: 10px; }
/* a raised card ON the dialog's surface-1 body — the surface-2 variant of .cc-card */
.co-step { padding: 12px; display: flex; flex-direction: column; gap: 4px; }
.co-num {
  width: 20px; height: 20px; border-radius: 50%; display: inline-flex; align-items: center; justify-content: center;
  font-size: var(--cc-fs-xs); font-weight: 600; color: var(--cc-text); background: var(--cc-surface-1);
  border: 1px solid var(--cc-border);
}
.co-step-title { font-weight: 600; color: var(--cc-text); }
.co-step-text { line-height: 1.4; }

.co-yes { color: var(--cc-accent); }
.co-no { color: var(--cc-text-dim); }
.co-note { margin: 10px 0 0; }
</style>
