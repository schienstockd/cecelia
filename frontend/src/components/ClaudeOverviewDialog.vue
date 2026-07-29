<!--
  "What can Claude do here?" — a brief, visual how-to for the two Claude entry points (Ask / Chat)
  and what the assistant can see / suggest / create / can't do. Opened on demand from the lab-log
  toolbar's info button; static reference, no live data. Content lives in lib/claudeOverview.ts
  (testable, one place to edit). Built on the shared BaseModal shell (docs/UI.md → "Modals & dialogs").
-->
<script setup lang="ts">
import { computed } from 'vue'
import BaseModal from './BaseModal.vue'
import {
  CLAUDE_ENTRY_POINTS, CLAUDE_CAPABILITIES, CLAUDE_EXAMPLES,
  CLAUDE_TERMINAL, claudeChatCommand,
} from '../lib/claudeOverview'
import { useObserverStore } from '../stores/observer'
import { useCopyFlash } from '../composables/useCopyFlash'
import { terminalCta } from '../utils/observerSetup'

defineEmits<{ (e: 'close'): void }>()

// Terminal setup is ONE CLICK: the backend registers the observer MCP in the user's Claude Code
// config, so plain `claude` picks it up. Nothing to copy — a mistyped path is the failure mode we're
// designing out. The `--mcp-config` line only appears if that fails, and only with the real path.
// The primary entry point for this is the lab-log toolbar (it replaces Chat to Claude until set up);
// the same control is repeated here because this dialog is where people come to understand the flow.
const observer = useObserverStore()
const fallbackCommand = computed(() => claudeChatCommand(observer.mcpConfigPath))
const ctaMode = computed(() => terminalCta(observer.available, observer.terminalState))

// copy-to-clipboard for the fallback line — shared helper (docs/UI.md → UX-primitive catalog)
const { isCopied: copied, copy } = useCopyFlash()
const copyCommand = () => copy(fallbackCommand.value)
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

    <!-- terminal hand-off: one click, Cecelia registers the MCP for the user's own claude -->
    <div class="co-terminal">
      <p class="co-terminal-note cc-muted cc-fs-md">{{ CLAUDE_TERMINAL.note }}</p>
      <div class="co-terminal-row">
        <button v-if="ctaMode !== 'chat'" class="cc-btn cc-btn-primary" :disabled="observer.registering"
                @click="observer.registerMcp()">
          <i class="pi pi-download" />
          {{ observer.registering ? CLAUDE_TERMINAL.busy
             : ctaMode === 'resync' ? CLAUDE_TERMINAL.resync : CLAUDE_TERMINAL.action }}
        </button>
        <span v-else class="co-terminal-done cc-fs-sm">
          <i class="pi pi-check" /> {{ CLAUDE_TERMINAL.done }}
        </span>
      </div>
      <p v-if="ctaMode === 'resync'" class="co-terminal-err cc-fs-sm">{{ CLAUDE_TERMINAL.staleWhy }}</p>
      <!-- failure fallback: the real resolved command, never a placeholder -->
      <template v-if="observer.registerError">
        <p class="co-terminal-err cc-fs-sm">{{ observer.registerError }}</p>
        <template v-if="fallbackCommand">
          <p class="co-terminal-note cc-muted cc-fs-sm">{{ CLAUDE_TERMINAL.failedPrefix }}</p>
          <div class="co-cmd">
            <code class="co-cmd-text">{{ fallbackCommand }}</code>
            <button class="cc-btn cc-btn-bare cc-btn-icon" @click="copyCommand"
              v-tooltip.left="copied() ? 'Copied!' : 'Copy command'">
              <i :class="copied() ? 'pi pi-check' : 'pi pi-copy'" />
            </button>
          </div>
        </template>
      </template>
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
      <span class="co-examples-label cc-muted">Try asking</span>
      <div class="co-chips">
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

.co-terminal { margin-bottom: 18px; }
.co-terminal-note { margin: 0 0 6px; line-height: 1.4; }
.co-terminal-row { display: flex; align-items: center; gap: 10px; }
.co-terminal-done { color: var(--cc-sev-ok); display: inline-flex; align-items: center; gap: 4px; }
.co-terminal-err { margin: 8px 0 4px; color: var(--cc-sev-warn); }
.co-cmd {
  display: flex; align-items: center; gap: 8px;
  background: var(--cc-surface-2); border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-md); padding: 6px 6px 6px 10px;
}
.co-cmd-text {
  flex: 1; font-family: var(--cc-mono); font-size: var(--cc-fs-sm);
  color: var(--cc-text); overflow-x: auto; white-space: nowrap;
}

.co-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 14px; }
.co-cell { border: 1px solid var(--cc-border); border-radius: var(--cc-radius-lg); padding: 12px 14px; }
.co-cell-head { font-weight: 600; display: flex; align-items: center; gap: 6px; margin-bottom: 6px; }
.co-cell ul { margin: 0; padding-left: 18px; font-size: var(--cc-fs-md); line-height: 1.55; color: var(--cc-text); }
.co-cell.tone-good .co-cell-head { color: #56d364; }
.co-cell.tone-good .co-cell-head .pi { color: #56d364; }
.co-cell.tone-neutral .co-cell-head .pi { color: var(--cc-accent); }
.co-cell.tone-muted { opacity: 0.85; }
.co-cell.tone-muted .co-cell-head { color: var(--cc-text-dim); }

.co-examples { margin-top: 18px; }
.co-examples-label { display: block; margin-bottom: 6px; }
.co-chips { display: flex; flex-wrap: wrap; gap: 6px; }
.co-chip {
  font-size: var(--cc-fs-sm); color: var(--cc-text); background: var(--cc-surface-2);
  border: 1px solid var(--cc-border); border-radius: var(--cc-radius-pill); padding: 3px 10px;
}
</style>
