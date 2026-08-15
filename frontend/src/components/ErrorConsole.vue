<script setup lang="ts">
import { ref, computed, watch, nextTick } from 'vue'
import { useLogStore, type LogLevel } from '../stores/log'
import { isVisible, formatEntry, logGroup, LOG_GROUPS, type LogGroup } from '../utils/logFilter'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'

// `fill`: render just the open panel filling its container (no docked collapse bar / toggle). Used by
// the standalone console window (ConsoleView) so the docked bar and the window are the SAME component.
const props = defineProps<{ fill?: boolean }>()

const log = useLogStore()

type Filter = LogLevel | 'all'
const filter = ref<Filter>('all')
const scrollEl = ref<HTMLElement | null>(null)
const expandedId = ref<number | null>(null)
const copied = ref(false)

// Two facets now — LEVEL (as before) and GROUP (one chip per runtime component). The filter itself is
// pure and lives in utils/logFilter.ts: an error is never hidden by a group chip, which is what makes
// turning the chatty children off safe. See that file for the rest of the rule.
const visible = computed(() =>
  log.entries.filter(e => isVisible(e, {
    groups: log.groups as LogGroup[], level: filter.value, query: log.query,
  }))
)

// Autoscroll follows only while you are AT the bottom. Before the child processes joined the stream
// the console was quiet enough that an unconditional jump-to-bottom was invisible; with napari and a
// running task in it, scrolling up to read something and being yanked back a moment later makes the
// panel unusable. Scrolling up pauses, scrolling back to the bottom resumes — no button to remember.
const AT_BOTTOM_PX = 24
function onScroll() {
  const el = scrollEl.value
  if (!el) return
  log.follow = el.scrollHeight - el.scrollTop - el.clientHeight < AT_BOTTOM_PX
}

watch(() => log.entries.length, async () => {
  if (!log.follow) return
  await nextTick()
  if (scrollEl.value) scrollEl.value.scrollTop = scrollEl.value.scrollHeight
})

function jumpToEnd() {
  log.follow = true
  nextTick(() => { if (scrollEl.value) scrollEl.value.scrollTop = scrollEl.value.scrollHeight })
}

function fmt(d: Date) {
  return d.toTimeString().slice(0, 8)
}

function toggleExpand(id: number) {
  expandedId.value = expandedId.value === id ? null : id
}

// Copy what is ON SCREEN, details included — the thing you actually want to paste into an issue or a
// message. Copies the filtered view rather than everything, so narrowing to one component or one
// search first is how you choose what goes in.
async function copyVisible() {
  try {
    await navigator.clipboard.writeText(visible.value.map(formatEntry).join('\n'))
    copied.value = true
    setTimeout(() => { copied.value = false }, 1200)
  } catch {
    log.warn('Could not copy to the clipboard', { source: 'frontend' })
  }
}

const groupOptions = computed<ChipOption[]>(() =>
  LOG_GROUPS.map(g => ({
    value: g.value,
    label: g.label,
    badge: log.groupCounts[g.value] ?? 0,
    tip: g.tip,
  }))
)

// Pop the console out into its own browser window: a hash-history route → the popup boots the same
// SPA, sees #/console, and renders this component full-window (App.vue bare mode) with its own WS.
function openConsoleWindow() {
  const url = location.origin + location.pathname + '#/console'
  window.open(url, 'cecelia-console', 'width=980,height=600')
}

const filterCounts = computed(() => ({
  all:   log.entries.length,
  info:  log.entries.filter(e => e.level === 'info').length,
  warn:  log.entries.filter(e => e.level === 'warn').length,
  error: log.entries.filter(e => e.level === 'error').length,
}))

// per-level active colour (all → default accent); mirrors the old .filter-tab.active.<lvl> text colour
const LEVEL_ACCENT: Partial<Record<Filter, string>> = {
  error: '#fca5a5',
  warn:  '#fcd34d',
  info:  '#93c5fd',
}

const filterOptions = computed<ChipOption[]>(() =>
  (['all', 'info', 'warn', 'error'] as Filter[]).map(lvl => ({
    value: lvl,
    label: lvl,
    badge: filterCounts.value[lvl],
    accent: LEVEL_ACCENT[lvl],
    // No per-option `tip`: the chip's own label IS the level, so "Show info messages" restated it —
    // and a second tooltip on the row renders over the chips. The group tooltip is the one that says
    // something (`docs/UI.md` → chip rows carry one tooltip, not both).
  }))
)
</script>

<template>
  <!-- collapsed bar (never in fill/window mode).
       `data-guide="console.bar"` is on BOTH this and the open panel below. The two are mutually
       exclusive `v-if`s, so exactly one is ever in the DOM and the guide's `querySelector` cannot
       pick the wrong one — whereas anchoring only the collapsed bar would leave the tour pointing at
       nothing for anyone who already had the console open. -->
  <div v-if="!fill && !log.consoleOpen" class="console-bar" data-guide="console.bar" @click="log.openConsole()">
    <span class="bar-toggle" v-tooltip.top="'Open error console'">
      <i class="pi pi-angle-up" />
      Console
    </span>

    <span v-if="log.lastEntry" class="bar-last cc-muted" :class="log.lastEntry.level">
      <span class="lvl-dot" />
      {{ log.lastEntry.message }}
    </span>

    <span v-if="log.unreadErrors > 0" class="unread-badge"
      v-tooltip.top="`${log.unreadErrors} unread error(s)`">
      {{ log.unreadErrors }}
    </span>

    <button
      class="icon-btn cc-btn cc-btn-bare cc-btn-icon cc-btn-lg bar-window-btn"
      @click.stop="openConsoleWindow"
      v-tooltip.top="'Open the console in a separate window'"
    >
      <i class="pi pi-external-link" />
    </button>
  </div>

  <!-- open panel (always shown in fill/window mode) -->
  <div v-if="fill || log.consoleOpen" class="console-panel" :class="{ fill }">
    <div class="console-toolbar" data-guide="console.bar">
      <button
        v-if="!fill"
        class="bar-toggle"
        @click="log.closeConsole()"
        v-tooltip.top="'Collapse console'"
      >
        <i class="pi pi-angle-down" /> Console
      </button>
      <span v-else class="bar-toggle"><i class="pi pi-desktop" /> Console</span>

      <ChipSelect
        class="filter-chips"
        variant="segmented"
        v-tooltip.top="'Show only messages of this severity'"
        :options="filterOptions"
        :model-value="filter"
        @update:model-value="v => filter = v as Filter"
        aria-label="Log level filter"
      />

      <input
        v-model="log.query"
        class="log-search cc-input-xs"
        type="search"
        placeholder="Search"
        v-tooltip.top="'Filter by message, source or stack trace'"
      />

      <button
        class="icon-btn cc-btn cc-btn-bare cc-btn-icon cc-btn-lg"
        @click="copyVisible"
        v-tooltip.top="'Copy the visible messages'"
        :disabled="visible.length === 0"
      >
        <i :class="copied ? 'pi pi-check' : 'pi pi-copy'" />
      </button>

      <button
        class="icon-btn cc-btn cc-btn-bare cc-btn-icon cc-btn-lg"
        @click="log.clear()"
        v-tooltip.top="'Clear all console messages'"
        :disabled="log.entries.length === 0"
      >
        <i class="pi pi-trash" />
      </button>

      <button
        v-if="!fill"
        class="icon-btn cc-btn cc-btn-bare cc-btn-icon cc-btn-lg"
        @click="openConsoleWindow"
        v-tooltip.top="'Open the console in a separate window'"
      >
        <i class="pi pi-external-link" />
      </button>
    </div>

    <!-- Component chips. One per thing that can talk, so "what is napari saying" is a click rather
         than a scroll — the children are off by default (they narrate) and their errors show anyway. -->
    <div class="console-sources">
      <ChipSelect
        multiple
        :options="groupOptions"
        :model-value="log.groups"
        @update:model-value="v => log.setGroups(v as LogGroup[])"
        aria-label="Log source filter"
      />
    </div>

    <div class="console-body" ref="scrollEl" @scroll.passive="onScroll">
      <div
        v-for="entry in visible"
        :key="entry.id"
        class="log-entry"
        :class="[entry.level, { expanded: expandedId === entry.id }]"
        @click="entry.detail ? toggleExpand(entry.id) : undefined"
        :style="entry.detail ? 'cursor: pointer' : undefined"
      >
        <span class="ts">{{ fmt(entry.timestamp) }}</span>
        <span class="lvl">{{ entry.level }}</span>
        <span v-if="entry.source" class="src" :class="'grp-' + logGroup(entry.source)">{{ entry.source }}</span>
        <span class="msg">{{ entry.message }}</span>
        <span v-if="entry.detail" class="expand-icon">
          <i :class="expandedId === entry.id ? 'pi pi-chevron-up' : 'pi pi-chevron-down'" />
        </span>

        <pre v-if="expandedId === entry.id && entry.detail" class="detail">{{ entry.detail }}</pre>
      </div>

      <div v-if="visible.length === 0" class="empty cc-empty">
        No {{ filter === 'all' ? '' : filter + ' ' }}messages
      </div>
    </div>

    <!-- Only while scrolled away from the end — the affordance for the autoscroll you just paused. -->
    <button v-if="!log.follow" class="follow-btn cc-btn cc-btn-bare" @click="jumpToEnd"
            v-tooltip.top="'Scroll to the newest message and resume following'">
      <i class="pi pi-angle-double-down" /> Newest
    </button>
  </div>
</template>

<style scoped>
/* ── shared ── */
.bar-toggle {
  display: flex;
  align-items: center;
  gap: 0.3rem;
  font-size: var(--cc-fs-sm);
  font-weight: 600;
  color: var(--cc-text-dim);
  background: none;
  border: none;
  cursor: pointer;
  padding: 0 0.25rem;
  white-space: nowrap;
}
.bar-toggle:hover { color: var(--cc-text); }

/* ── collapsed bar ── */
.console-bar {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  height: var(--cc-console-bar-h);
  padding: 0 0.75rem;
  background: var(--cc-surface-1);
  border-top: 1px solid var(--cc-border);
  cursor: pointer;
  flex-shrink: 0;
}
.console-bar:hover { background: var(--cc-surface-2); }

.bar-last { display: flex; align-items: center; gap: 0.4rem; flex: 1; overflow: hidden; white-space: nowrap; text-overflow: ellipsis; }
.bar-last.error { color: #fca5a5; }
.bar-last.warn  { color: #fcd34d; }
.bar-last.info  { color: var(--cc-text-dim); }

.lvl-dot {
  width: 6px; height: 6px;
  border-radius: var(--cc-radius-pill);
  flex-shrink: 0;
}
.bar-last.error .lvl-dot { background: var(--cc-sev-fail); }
.bar-last.warn  .lvl-dot { background: var(--cc-sev-warn); }
.bar-last.info  .lvl-dot { background: #3b82f6; }

/* pop-out button on the collapsed bar — pin to the right edge, don't shrink */
.bar-window-btn { margin-left: auto; flex-shrink: 0; }

.unread-badge {
  background: #7f1d1d;
  color: #fca5a5;
  font-size: var(--cc-fs-xs);
  font-weight: 700;
  padding: 0.1rem 0.45rem;
  border-radius: var(--cc-radius-pill);
  min-width: 1.4em;
  text-align: center;
}

/* ── open panel ── */
.console-panel {
  display: flex;
  flex-direction: column;
  height: var(--cc-console-open-h);
  border-top: 1px solid var(--cc-border);
  background: var(--cc-console-bg);
  flex-shrink: 0;
  position: relative;          /* anchors the floating "Newest" button */
}
/* window/standalone mode: fill the whole container instead of the docked open-height */
.console-panel.fill { height: 100%; border-top: none; }

.console-toolbar {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.3rem 0.75rem;
  border-bottom: 1px solid var(--cc-border);
  background: var(--cc-surface-1);
}

/* the search box takes the slack, so the trailing icon buttons stay pinned right */
.filter-chips { flex-shrink: 0; }

/* + cc-input-xs — box/size/focus come from the global input base and its density step; only the
   layout and the console's monospace are this component's own (docs/UI.md → form controls). */
.log-search { flex: 1; min-width: 6rem; font-family: var(--cc-mono); }

/* ── source chips ── */
.console-sources {
  padding: 0.25rem 0.75rem;
  border-bottom: 1px solid var(--cc-border);
  background: var(--cc-surface-1);
}

/* ── follow / jump-to-newest ── */
.follow-btn {
  position: absolute;
  right: 1rem;
  bottom: 0.6rem;
  display: flex;
  align-items: center;
  gap: 0.3rem;
  font-size: var(--cc-fs-xs);
  padding: 0.2rem 0.6rem;
  border-radius: var(--cc-radius-pill);
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
  color: var(--cc-text-dim);
  box-shadow: 0 2px 6px #0006;
}
.follow-btn:hover { color: var(--cc-text); background: var(--cc-surface-1); }

/* .icon-btn → cc-btn cc-btn-bare cc-btn-icon cc-btn-lg */
.icon-btn:hover:not(:disabled) { background: var(--cc-surface-2); color: var(--cc-text); }
.icon-btn:disabled { opacity: 0.35; cursor: not-allowed; }

/* ── log body ── */
.console-body {
  flex: 1;
  overflow-y: auto;
  font-family: var(--cc-mono);
  font-size: var(--cc-fs-sm);
  padding: 0.25rem 0;
}

.log-entry {
  display: flex;
  flex-wrap: wrap;
  align-items: baseline;
  gap: 0.5rem;
  padding: 0.2rem 0.75rem;
  line-height: 1.5;
  border-bottom: 1px solid transparent;
}
.log-entry:hover { background: var(--cc-surface-2); }
.log-entry.error { background: #7f1d1d18; }
.log-entry.error:hover { background: #7f1d1d30; }
.log-entry.warn  { background: #78350f10; }

.ts  { color: #4b5563; flex-shrink: 0; }
/* the source tag stays the fine-grained one (19 of them); the tint groups it to its chip, so scanning
   for "which component said this" is colour, not reading */
.src { color: #6b7280; flex-shrink: 0; font-style: italic; }
.src.grp-backend   { color: #9ca3af; }
.src.grp-napari    { color: #7dd3fc; }
.src.grp-preview   { color: var(--cc-accent-soft); }
.src.grp-runner    { color: #86efac; }
.src.grp-notebooks { color: #fdba74; }
.src.grp-tasks     { color: #67e8f9; }

.lvl {
  font-weight: 700;
  text-transform: uppercase;
  font-size: var(--cc-fs-2xs);
  flex-shrink: 0;
  padding: 0.05rem 0.35rem;
  border-radius: var(--cc-radius-xs);
}
.error .lvl { background: #7f1d1d; color: #fca5a5; }
.warn  .lvl { background: #78350f; color: #fcd34d; }
.info  .lvl { background: #1e3a5f; color: var(--cc-active); }

.msg { color: var(--cc-text); flex: 1; }
.error .msg { color: #fca5a5; }
.warn  .msg { color: #fcd34d; }

.expand-icon { color: var(--cc-text-dim); font-size: var(--cc-fs-2xs); flex-shrink: 0; }

.detail {
  width: 100%;
  margin: 0.35rem 0 0.2rem;
  padding: 0.5rem 0.75rem;
  background: var(--cc-surface-1);
  border-radius: var(--cc-radius-sm);
  border-left: 2px solid var(--cc-border);
  color: var(--cc-text-dim);
  font-size: var(--cc-fs-xs);
  white-space: pre-wrap;
  word-break: break-all;
  overflow-x: auto;
}

/* + cc-empty — padding is this console's own geometry, the rest is the scenario */
.empty { padding: 1.5rem; }
</style>
