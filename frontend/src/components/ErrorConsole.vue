<script setup lang="ts">
import { ref, computed, watch, nextTick } from 'vue'
import { useLogStore, type LogLevel } from '../stores/log'

// `fill`: render just the open panel filling its container (no docked collapse bar / toggle). Used by
// the standalone console window (ConsoleView) so the docked bar and the window are the SAME component.
const props = defineProps<{ fill?: boolean }>()

const log = useLogStore()

type Filter = LogLevel | 'all'
const filter = ref<Filter>('all')
const scrollEl = ref<HTMLElement | null>(null)
const expandedId = ref<number | null>(null)

const visible = computed(() =>
  filter.value === 'all'
    ? log.entries
    : log.entries.filter(e => e.level === filter.value)
)

watch(() => log.entries.length, async () => {
  await nextTick()
  if (scrollEl.value) scrollEl.value.scrollTop = scrollEl.value.scrollHeight
})

function fmt(d: Date) {
  return d.toTimeString().slice(0, 8)
}

function toggleExpand(id: number) {
  expandedId.value = expandedId.value === id ? null : id
}

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
</script>

<template>
  <!-- collapsed bar (never in fill/window mode) -->
  <div v-if="!fill && !log.consoleOpen" class="console-bar" @click="log.openConsole()">
    <span class="bar-toggle" v-tooltip.top="'Open error console'">
      <i class="pi pi-angle-up" />
      Console
    </span>

    <span v-if="log.lastEntry" class="bar-last" :class="log.lastEntry.level">
      <span class="lvl-dot" />
      {{ log.lastEntry.message }}
    </span>

    <span v-if="log.unreadErrors > 0" class="unread-badge"
      v-tooltip.top="`${log.unreadErrors} unread error(s)`">
      {{ log.unreadErrors }}
    </span>

    <button
      class="icon-btn bar-window-btn"
      @click.stop="openConsoleWindow"
      v-tooltip.top="'Open the console in a separate window'"
    >
      <i class="pi pi-external-link" />
    </button>
  </div>

  <!-- open panel (always shown in fill/window mode) -->
  <div v-if="fill || log.consoleOpen" class="console-panel" :class="{ fill }">
    <div class="console-toolbar">
      <button
        v-if="!fill"
        class="bar-toggle"
        @click="log.closeConsole()"
        v-tooltip.top="'Collapse console'"
      >
        <i class="pi pi-angle-down" /> Console
      </button>
      <span v-else class="bar-toggle"><i class="pi pi-desktop" /> Console</span>

      <div class="filter-tabs">
        <button
          v-for="lvl in (['all', 'info', 'warn', 'error'] as Filter[])"
          :key="lvl"
          class="filter-tab"
          :class="{ active: filter === lvl, [lvl]: true }"
          @click="filter = lvl"
          v-tooltip.top="`Show ${lvl === 'all' ? 'all' : lvl} messages`"
        >
          {{ lvl }} <span class="cnt">{{ filterCounts[lvl] }}</span>
        </button>
      </div>

      <button
        class="icon-btn"
        @click="log.clear()"
        v-tooltip.top="'Clear all console messages'"
        :disabled="log.entries.length === 0"
      >
        <i class="pi pi-trash" />
      </button>

      <button
        v-if="!fill"
        class="icon-btn"
        @click="openConsoleWindow"
        v-tooltip.top="'Open the console in a separate window'"
      >
        <i class="pi pi-external-link" />
      </button>
    </div>

    <div class="console-body" ref="scrollEl">
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
        <span v-if="entry.source" class="src">{{ entry.source }}</span>
        <span class="msg">{{ entry.message }}</span>
        <span v-if="entry.detail" class="expand-icon">
          <i :class="expandedId === entry.id ? 'pi pi-chevron-up' : 'pi pi-chevron-down'" />
        </span>

        <pre v-if="expandedId === entry.id && entry.detail" class="detail">{{ entry.detail }}</pre>
      </div>

      <div v-if="visible.length === 0" class="empty">
        No {{ filter === 'all' ? '' : filter + ' ' }}messages
      </div>
    </div>
  </div>
</template>

<style scoped>
/* ── shared ── */
.bar-toggle {
  display: flex;
  align-items: center;
  gap: 0.3rem;
  font-size: 0.75rem;
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

.bar-last {
  display: flex;
  align-items: center;
  gap: 0.4rem;
  font-size: 0.75rem;
  flex: 1;
  overflow: hidden;
  white-space: nowrap;
  text-overflow: ellipsis;
  color: var(--cc-text-dim);
}
.bar-last.error { color: #fca5a5; }
.bar-last.warn  { color: #fcd34d; }
.bar-last.info  { color: var(--cc-text-dim); }

.lvl-dot {
  width: 6px; height: 6px;
  border-radius: 50%;
  flex-shrink: 0;
}
.bar-last.error .lvl-dot { background: #ef4444; }
.bar-last.warn  .lvl-dot { background: #f59e0b; }
.bar-last.info  .lvl-dot { background: #3b82f6; }

/* pop-out button on the collapsed bar — pin to the right edge, don't shrink */
.bar-window-btn { margin-left: auto; flex-shrink: 0; }

.unread-badge {
  background: #7f1d1d;
  color: #fca5a5;
  font-size: 0.7rem;
  font-weight: 700;
  padding: 0.1rem 0.45rem;
  border-radius: 999px;
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

.filter-tabs {
  display: flex;
  gap: 0.2rem;
  flex: 1;
}
.filter-tab {
  font-size: 0.72rem;
  padding: 0.15rem 0.55rem;
  border-radius: 0.3rem;
  border: 1px solid transparent;
  background: none;
  color: var(--cc-text-dim);
  cursor: pointer;
  font-weight: 500;
}
.filter-tab:hover { background: var(--cc-surface-2); }
.filter-tab.active { border-color: var(--cc-border); color: var(--cc-text); }
.filter-tab.active.error { border-color: #7f1d1d; color: #fca5a5; }
.filter-tab.active.warn  { border-color: #78350f; color: #fcd34d; }
.filter-tab.active.info  { border-color: #1e3a5f; color: #93c5fd; }
.cnt {
  font-size: 0.65rem;
  background: var(--cc-surface-2);
  padding: 0.05rem 0.3rem;
  border-radius: 999px;
}

.icon-btn {
  background: none;
  border: none;
  cursor: pointer;
  color: var(--cc-text-dim);
  padding: 0.2rem 0.4rem;
  border-radius: 0.25rem;
  font-size: 0.8rem;
}
.icon-btn:hover:not(:disabled) { background: var(--cc-surface-2); color: var(--cc-text); }
.icon-btn:disabled { opacity: 0.35; cursor: not-allowed; }

/* ── log body ── */
.console-body {
  flex: 1;
  overflow-y: auto;
  font-family: var(--cc-mono);
  font-size: 0.75rem;
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
.src { color: #6b7280; flex-shrink: 0; font-style: italic; }

.lvl {
  font-weight: 700;
  text-transform: uppercase;
  font-size: 0.65rem;
  flex-shrink: 0;
  padding: 0.05rem 0.35rem;
  border-radius: 0.2rem;
}
.error .lvl { background: #7f1d1d; color: #fca5a5; }
.warn  .lvl { background: #78350f; color: #fcd34d; }
.info  .lvl { background: #1e3a5f; color: #93c5fd; }

.msg { color: var(--cc-text); flex: 1; }
.error .msg { color: #fca5a5; }
.warn  .msg { color: #fcd34d; }

.expand-icon { color: var(--cc-text-dim); font-size: 0.65rem; flex-shrink: 0; }

.detail {
  width: 100%;
  margin: 0.35rem 0 0.2rem;
  padding: 0.5rem 0.75rem;
  background: var(--cc-surface-1);
  border-radius: 0.3rem;
  border-left: 2px solid var(--cc-border);
  color: var(--cc-text-dim);
  font-size: 0.7rem;
  white-space: pre-wrap;
  word-break: break-all;
  overflow-x: auto;
}

.empty {
  padding: 1.5rem;
  text-align: center;
  color: var(--cc-text-dim);
  font-size: 0.8rem;
}
</style>
