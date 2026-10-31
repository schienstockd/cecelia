<!--
  The two modes — Kiwi and Claude Code — side by side, not as a sequence (lib/claudeOverview.ts →
  KIWI_MODE / TERMINAL_MODE / CLAUDE_BRIDGE). Each card leads with a small schematic, so a glance says
  which one you need: Kiwi = a reply pointing at a cell in the app; Claude Code = a terminal that
  builds a chain, a board and a notebook. Then what you'd ask it / what it makes, how to start, and
  its limit. Static and theme-coloured (tokens, no fixed colours) — the feijoa sketches are animated
  720×360 explainers on a paper palette, wrong at 72 px in a dark card. One renderer for every place
  that shows it — Kiwi's `?` dialog (ClaudeOverviewDialog) and the Guides map (GuideMap).
-->
<script setup lang="ts">
import { KIWI_MODE, TERMINAL_MODE, CLAUDE_BRIDGE, type Mode } from '../lib/claudeOverview'

const MODES: { mode: Mode; fig: 'kiwi' | 'terminal'; quoted: boolean }[] = [
  { mode: KIWI_MODE, fig: 'kiwi', quoted: true },          // questions you'd ask
  { mode: TERMINAL_MODE, fig: 'terminal', quoted: false }, // things it makes
]
</script>

<template>
  <div class="cw">
    <div class="cw-modes">
      <section v-for="{ mode: m, fig, quoted } in MODES" :key="m.title" class="cw-mode cc-card cc-card-2">
        <div class="cw-head">
          <svg v-if="fig === 'kiwi'" class="cw-fig" viewBox="0 0 96 60" fill="none" aria-hidden="true">
            <!-- the app, a few cells, one marked -->
            <rect class="cw-frame" x="2" y="8" width="62" height="48" rx="4" />
            <line class="cw-line" x1="2" y1="16" x2="64" y2="16" />
            <circle class="cw-dim" cx="16" cy="28" r="4" />
            <circle class="cw-dim" cx="20" cy="44" r="3.5" />
            <circle class="cw-dim" cx="48" cy="45" r="3.5" />
            <circle class="cw-ink" cx="36" cy="34" r="5" />
            <circle class="cw-acc cw-dash" cx="36" cy="34" r="9" />
            <!-- Kiwi's reply, pointing back at the cell -->
            <path class="cw-acc cw-bubble" d="M62 4 h28 a4 4 0 0 1 4 4 v12 a4 4 0 0 1 -4 4 h-18 l-6 5 v-5 h-4 a4 4 0 0 1 -4 -4 v-12 a4 4 0 0 1 4 -4z" />
            <line class="cw-ink" x1="66" y1="11" x2="88" y2="11" />
            <line class="cw-dim" x1="66" y1="17" x2="80" y2="17" />
            <path class="cw-acc cw-dash" d="M64 29 L45 33" />
          </svg>
          <svg v-else class="cw-fig" viewBox="0 0 96 60" fill="none" aria-hidden="true">
            <!-- the terminal -->
            <rect class="cw-frame cw-term" x="2" y="12" width="44" height="36" rx="4" />
            <path class="cw-acc" d="M9 24 l5 4 l-5 4" />
            <line class="cw-ink" x1="18" y1="33" x2="26" y2="33" />
            <line class="cw-dim" x1="9" y1="40" x2="36" y2="40" />
            <!-- what it builds: a chain, a board, a notebook -->
            <path class="cw-dim" d="M48 30 h8 M56 12 v36 M56 12 h4 M56 30 h4 M56 48 h4" />
            <circle class="cw-acc" cx="66" cy="12" r="3" /><circle class="cw-acc" cx="78" cy="12" r="3" />
            <circle class="cw-acc" cx="90" cy="12" r="3" />
            <path class="cw-acc" d="M69 12 h6 M81 12 h6" />
            <rect class="cw-ink" x="63" y="24" width="12" height="12" rx="1.5" />
            <rect class="cw-ink" x="78" y="24" width="12" height="12" rx="1.5" />
            <path class="cw-dim" d="M65 33 l3 -3 l3 2 l3 -4 M80 33 v-3 M83 33 v-6 M86 33 v-4" />
            <rect class="cw-ink" x="63" y="42" width="27" height="14" rx="1.5" />
            <path class="cw-dim" d="M67 47 h19 M67 51 h13" />
          </svg>
          <div>
            <div class="cw-title">{{ m.title }}</div>
            <div class="cc-muted cw-line1">{{ m.use }}</div>
          </div>
        </div>
        <div class="cc-eyebrow cc-fs-2xs">{{ m.canLabel }}</div>
        <ul :class="['cw-can', { 'cw-quoted': quoted }]">
          <li v-for="c in m.can" :key="c" class="cc-fs-sm cw-line1">{{ c }}</li>
        </ul>
        <div class="cw-foot cc-fs-sm">
          <span class="cw-line1">{{ m.start }}</span>
          <span class="cw-never cw-line1">{{ m.never }}</span>
        </div>
      </section>
    </div>
    <p class="cw-bridge cc-muted">{{ CLAUDE_BRIDGE }}</p>
  </div>
</template>

<style scoped>
.cw-modes { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; }
/* a raised card ON a dialog's surface-1 body — the surface-2 variant of .cc-card */
.cw-mode { padding: 12px; display: flex; flex-direction: column; gap: 8px; }
.cw-head { display: flex; gap: 10px; align-items: center; }
.cw-head > div { min-width: 0; }
/* one line each — the copy is capped to fit (claudeOverview.test.ts); this only guards a wider font */
.cw-line1 { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.cw-fig { flex: none; width: 72px; height: 45px; stroke-linecap: round; stroke-linejoin: round; stroke-width: 1.3; }
.cw-title { font-weight: 600; color: var(--cc-text); }
.cw-can { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 3px; flex: 1; color: var(--cc-text); }
.cw-quoted li::before { content: '“'; color: var(--cc-text-dim); }
.cw-quoted li::after { content: '”'; color: var(--cc-text-dim); }
.cw-can:not(.cw-quoted) li::before { content: '•\00a0\00a0'; color: var(--cc-accent); }
.cw-foot { display: flex; flex-direction: column; gap: 3px; padding-top: 7px; border-top: 1px solid var(--cc-border); color: var(--cc-text); }
.cw-never { color: var(--cc-text-dim); }
.cw-bridge { margin: 8px 0 0; }

/* schematic strokes — theme tokens only, so it follows light/dark */
.cw-frame { stroke: var(--cc-border); stroke-width: 1.5; fill: var(--cc-surface-1); }
.cw-term { fill: var(--cc-bg); }
.cw-line { stroke: var(--cc-border); stroke-width: 1.5; }
.cw-ink { stroke: var(--cc-text); }
.cw-dim { stroke: var(--cc-text-dim); }
.cw-acc { stroke: var(--cc-accent); }
.cw-dash { stroke-dasharray: 2.5 2.5; }
.cw-bubble { fill: var(--cc-surface-2); }
</style>
