<!--
  The four-step Claude loop (lib/claudeOverview.ts → CLAUDE_LOOP) as numbered cards. One renderer for
  every place that shows it — Kiwi's `?` dialog (ClaudeOverviewDialog) and the Guides map (GuideMap) —
  so the loop looks the same wherever a user meets it.
-->
<script setup lang="ts">
import { CLAUDE_LOOP } from '../lib/claudeOverview'
</script>

<template>
  <ol class="cl-loop">
    <li v-for="(s, i) in CLAUDE_LOOP" :key="i" class="cl-step cc-card cc-card-2">
      <span class="cl-num">{{ i + 1 }}</span>
      <div class="cl-step-title">{{ s.title }}</div>
      <div class="cl-step-text cc-muted cc-fs-sm">{{ s.text }}</div>
    </li>
  </ol>
</template>

<style scoped>
.cl-loop { list-style: none; margin: 0; padding: 0; display: grid; grid-template-columns: repeat(4, 1fr); gap: 10px; }
/* a raised card ON a dialog's surface-1 body — the surface-2 variant of .cc-card */
.cl-step { padding: 12px; display: flex; flex-direction: column; gap: 4px; }
.cl-num {
  width: 20px; height: 20px; border-radius: 50%; display: inline-flex; align-items: center; justify-content: center;
  font-size: var(--cc-fs-xs); font-weight: 600; color: var(--cc-text); background: var(--cc-surface-1);
  border: 1px solid var(--cc-border);
}
.cl-step-title { font-weight: 600; color: var(--cc-text); }
.cl-step-text { line-height: 1.4; }
</style>
