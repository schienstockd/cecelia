<!--
  "Kiwi and Claude Code" — the two modes side by side, either first: when to use each, what you can
  do with it, how to start, what it won't do (ClaudeWaysIn.vue). Then "What each can use": one section
  per group, one open at a time (useSingleOpenSection), a Kiwi / Claude Code tick per line
  (lib/claudeTools.ts, pinned by its test to the real tool allow-lists, so a new tool can't go
  missing). Opened from the Kiwi header's `?`; static reference, no live data. Every reply is a claim
  to check, not a verdict: the provisional line. Built on the shared BaseModal shell.
-->
<script setup lang="ts">
import BaseModal from './BaseModal.vue'
import ClaudeWaysIn from './ClaudeWaysIn.vue'
import CollapsibleSection from './CollapsibleSection.vue'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'
import { CLAUDE_PROVISIONAL } from '../lib/claudeOverview'
import { capabilityGroups } from '../lib/claudeTools'
import { useSingleOpenSection } from '../composables/useSingleOpenSection'

defineEmits<{ (e: 'close'): void }>()

const GROUPS = capabilityGroups().map(g => ({ ...g, rows: g.items.map(c => ({ ...c })) }))
const COLUMNS: SelectionColumn[] = [
  { key: 'label', label: '' },
  { key: 'kiwi', label: 'Kiwi', width: 64 },
  { key: 'terminal', label: 'Claude Code', width: 96 },
]
// An accordion like every other in the app: one group open at a time, remembered, none to start with.
const { isOpen, toggle } = useSingleOpenSection('cc.claudeOverview.openGroup')

// Terminal set-up is NOT repeated here. It lives on Kiwi's Terminal row — one button, in the place
// people act from — and duplicating it in this dialog cost a whole band of chrome to say what the
// row already shows. This dialog is reference only: no live state, no controls.
</script>

<template>
  <BaseModal title="Kiwi and Claude Code" icon="pi-question-circle" width="720px" @close="$emit('close')">
    <ClaudeWaysIn />

    <div class="co-uses">
      <div class="cc-eyebrow cc-fs-2xs co-uses-title">What each can use</div>
      <CollapsibleSection v-for="g in GROUPS" :key="g.key" :label="g.title"
                          :open="isOpen(g.key)" @update:open="() => toggle(g.key)"
                          :tip="`What Kiwi and Claude Code can use: ${g.title.toLowerCase()}`">
        <!-- icons carry the meaning, not colour alone -->
        <SelectionTable selection-mode="none" density="compact" :columns="COLUMNS" :rows="g.rows" id-key="label">
          <template #cell-kiwi="{ row }">
            <i :class="['pi', row.kiwi ? 'pi-check co-yes' : 'pi-minus co-no']" :aria-label="row.kiwi ? 'yes' : 'no'" />
          </template>
          <template #cell-terminal="{ row }">
            <i :class="['pi', row.terminal ? 'pi-check co-yes' : 'pi-minus co-no']" :aria-label="row.terminal ? 'yes' : 'no'" />
          </template>
        </SelectionTable>
      </CollapsibleSection>
    </div>
    <p class="co-note cc-muted">{{ CLAUDE_PROVISIONAL }}</p>
  </BaseModal>
</template>

<style scoped>
.co-uses { margin-top: 14px; }
.co-uses-title { margin-bottom: 4px; }
.co-yes { color: var(--cc-accent); }
.co-no { color: var(--cc-text-dim); }
.co-note { margin: 10px 0 0; }
</style>
