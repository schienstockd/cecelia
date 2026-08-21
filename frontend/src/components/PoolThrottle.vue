<!--
  PoolThrottle — live scheduler throttle + occupancy readout: one slider per resource pool
  (docs/SCHEDULER.md). Lower a pool to be gentle (e.g. Disk I/O to 1 for slow-share imports), raise
  it when the machine is free. Changes apply immediately (resize_pool!) and persist to custom.toml.
  Under each slider a "running / limit" line + mini bar shows how many tasks are executing in that
  pool right now (polled from /api/pools while open — there is no pool:* WS event). Compact 2×2 grid:
  cpu/gpu on the first row, io/network on the second. Lives in a popover off the Task Manager.

  Below the pools, the THREAD budget: how wide one task may go, as opposed to how many run at once.
  Same control because it is the same question — how hard may this machine work — and both are
  properties of the box rather than of any one task (`cpu_utils.py` on why it is not a task param).
  It has an "auto" state the pools do not: absent config means a number derived from the core count,
  which is not the same as the identical number written down. Applies to the NEXT task started.
-->
<script setup lang="ts">
import { ref, computed, onMounted, onUnmounted } from 'vue'
import { threadReadout, threadTip, clampWorkers, type ThreadBudget } from '../utils/threadBudget'

// `compact` is for a host that is NOT watching a run: Settings. It drops the live occupancy readouts
// and the hint lines, which are what you want beside a running task and noise in a settings pane, and
// lets the sliders fill their column instead of sitting in a 260px card. A PROP rather than a second
// component — the sliders, the endpoints and the auto/derived rule must not exist twice.
const props = withDefaults(defineProps<{ compact?: boolean }>(), { compact: false })

interface PoolInfo { name: string; limit: number; running?: number; queued?: number }
const POOL_ORDER = ['cpu', 'gpu', 'io', 'network']
const POOL_META: Record<string, { label: string; max: number; tip: string }> = {
  cpu:     { label: 'CPU',      max: 32, tip: 'General compute — most tasks (tracking, measures, clustering, corrections).' },
  gpu:     { label: 'GPU',      max: 8,  tip: 'The GPU — cellpose segmentation/correction. 1 runs one GPU job at a time; raise for batch segmentation.' },
  io:      { label: 'Disk I/O', max: 16, tip: 'Local disk — image import/convert and crop. Drop to 1 when importing over a slow network share so copies don\'t pile up.' },
  network: { label: 'Network',  max: 8,  tip: 'Remote/SMB reads — reserved for HPC/remote tasks. No tasks use it yet.' },
}
const pools     = ref<Record<string, number>>({})                                // name → limit
const occupancy = ref<Record<string, { running: number; queued: number }>>({})   // name → live counts
const poolBusy  = ref<string | null>(null)
const orderedPools = computed(() => POOL_ORDER.filter(n => n in pools.value))

const runningOf = (name: string) => occupancy.value[name]?.running ?? 0
const queuedOf  = (name: string) => occupancy.value[name]?.queued ?? 0
// Occupancy-bar fill: running slots as a fraction of the limit (capped at 100%).
function fillPct(name: string): string {
  const limit = pools.value[name] || 1
  return `${Math.min(100, Math.round((runningOf(name) / limit) * 100))}%`
}

async function fetchPools(): Promise<PoolInfo[] | null> {
  try {
    const res = await fetch('/api/pools')
    if (res.ok) return await res.json() as PoolInfo[]
  } catch { /* backend may not be ready */ }
  return null
}
function applyOccupancy(list: PoolInfo[]) {
  occupancy.value = Object.fromEntries(list.map(p => [p.name, { running: p.running ?? 0, queued: p.queued ?? 0 }]))
}
// Full load (mount): seed both the slider limits and the occupancy.
async function loadPools() {
  const list = await fetchPools()
  if (list) { pools.value = Object.fromEntries(list.map(p => [p.name, p.limit])); applyOccupancy(list) }
}
// Poll: refresh only the live occupancy — do NOT touch `pools` (limits), so a periodic tick can't
// snap a slider back while the user is mid-drag.
async function refreshOccupancy() {
  const list = await fetchPools()
  if (list) applyOccupancy(list)
}

async function setPool(name: string, limit: number) {
  poolBusy.value = name
  try {
    const res = await fetch('/api/pools/set', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ name, limit }),
    })
    if (res.ok) { const d = await res.json() as PoolInfo; pools.value[d.name] = d.limit }
  } catch { /* ignore */ }
  finally { poolBusy.value = null }
}

// ── thread budget ───────────────────────────────────────────────────────────
const threads     = ref<ThreadBudget | null>(null)
const threadsBusy = ref(false)
const threadText  = computed(() => threadReadout(threads.value))
const threadHint  = computed(() => threadTip(threads.value))

async function loadThreads() {
  try {
    const res = await fetch('/api/tasks/threads')
    if (res.ok) threads.value = await res.json() as ThreadBudget
  } catch { /* backend may not be ready */ }
}

// `workers = 0` clears the setting and returns to the derived default — a distinct action, not the
// bottom of the slider's range, which is why "Auto" is its own button.
async function setThreads(workers: number) {
  const b = threads.value
  if (!b) return
  threadsBusy.value = true
  try {
    const res = await fetch('/api/tasks/threads/set', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ workers }),
    })
    if (res.ok) {
      const d = await res.json() as { workers: number; derived: boolean }
      threads.value = { ...b, workers: d.workers, derived: d.derived }
    }
  } catch { /* ignore */ }
  finally { threadsBusy.value = false }
}

let timer: number | undefined
onMounted(() => { loadPools(); loadThreads(); timer = window.setInterval(refreshOccupancy, 1500) })
onUnmounted(() => { if (timer) clearInterval(timer) })
</script>

<template>
  <div class="pt-root" :class="props.compact ? 'compact cc-row cc-row-loose' : ''">
    <div v-if="!props.compact" class="pt-head">Concurrent tasks</div>
    <div class="pt-grid">
      <div v-for="name in orderedPools" :key="name" class="pt-cell"
           v-tooltip.bottom="POOL_META[name]?.tip">
        <div class="pt-cell-head">
          <span class="pt-label">{{ POOL_META[name]?.label ?? name }}</span>
          <span class="pt-val">{{ pools[name] }}</span>
        </div>
        <input type="range" class="pt-slider" min="1" :max="POOL_META[name]?.max ?? 32"
               :value="pools[name]" :disabled="poolBusy === name"
               @input="pools[name] = +($event.target as HTMLInputElement).value"
               @change="setPool(name, +($event.target as HTMLInputElement).value)" />
        <!-- live occupancy: how many tasks are running now vs the limit, + any queued for this pool -->
        <div v-if="!props.compact" class="pt-occ cc-readout cc-fs-2xs"
             :class="{ busy: runningOf(name) > 0 || queuedOf(name) > 0 }">
          <span><span class="pt-occ-n">{{ runningOf(name) }}</span><span class="pt-occ-sep">/</span>{{ pools[name] }} running</span>
          <span v-if="queuedOf(name) > 0" class="pt-occ-q">+{{ queuedOf(name) }} queued</span>
        </div>
        <div v-if="!props.compact" class="pt-bar">
          <div class="pt-bar-fill" :style="{ width: fillPct(name) }" />
        </div>
      </div>
    </div>
    <p v-if="!props.compact" class="pt-hint cc-muted cc-fs-xs">
      Lower to throttle, raise to run more at once. Saved automatically.
    </p>

    <!-- threads per task: the other axis — how WIDE one task may go, not how many run -->
    <div v-if="threads" class="pt-threads">
      <div v-if="!props.compact" class="pt-head">Threads per task</div>
      <div class="pt-cell" :class="{ threads: props.compact }">
        <div class="pt-cell-head">
          <span class="pt-label">{{ props.compact ? 'Threads' : 'Worker threads' }}</span>
          <button v-if="props.compact && !threads.derived"
                  class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" :disabled="threadsBusy"
                  v-tooltip.left="'Back to the number derived from this machine'"
                  @click="setThreads(0)"><i class="pi pi-undo" /></button>
          <span class="pt-val" :class="{ auto: threads.derived }">{{ threadText }}</span>
        </div>
        <input type="range" class="pt-slider" min="1" :max="threads.max"
               v-tooltip.bottom="threadHint"
               :value="threads.workers" :disabled="threadsBusy"
               @input="threads.workers = clampWorkers(+($event.target as HTMLInputElement).value, threads.max)"
               @change="setThreads(clampWorkers(+($event.target as HTMLInputElement).value, threads.max))" />
      </div>
      <button v-if="!props.compact && !threads.derived"
              class="cc-btn cc-btn-bare cc-btn-dense cc-fs-2xs pt-auto"
              :disabled="threadsBusy"
              v-tooltip.bottom="'Back to the number derived from this machine'"
              @click="setThreads(0)">Reset to auto</button>
      <p v-if="!props.compact" class="pt-hint cc-muted cc-fs-xs">Applies to the next task started.</p>
    </div>
  </div>
</template>

<style scoped>
.pt-root { width: 260px; padding: 0.6rem 0.7rem; }
/* Inline in a settings column, on ONE row of controls (two if the column is narrow) rather than a
   2x2 card plus a separate threads block. `display: contents` dissolves the two group wrappers so
   their cells become direct flex items here — the grouping only exists for the popover, where the
   rule between "how many" and "how wide" is worth the space. */
.pt-root.compact { width: 100%; padding: 0; }
/* the flex row itself is `.cc-row cc-row-loose` on the element — one wrap unit per control, which is
   what `.cc-row` is for; only the per-cell sizing is local.
   `.cc-row` CENTRES its items, which is right for controls of equal height and wrong here: the
   threads cell is taller than the four pool cells, so centring floated it against them. Top-aligned
   instead, so every label starts on the same line whatever the cell contains. */
.pt-root.compact { align-items: flex-start; }
.pt-root.compact .pt-grid,
.pt-root.compact .pt-threads { display: contents; }
.pt-root.compact .pt-threads { border-top: none; }
.pt-root.compact .pt-cell { flex: 1 1 4.5rem; min-width: 4.5rem; gap: 0.05rem; }
.pt-root.compact .pt-cell { flex-direction: column; align-items: stretch; }
/* Every cell's label row is the SAME height, whether or not it holds the reset button — otherwise the
   button's own box makes the threads cell taller and its slider sits lower than the four beside it
   (Dominik spotted it, and correctly guessed the icon). The button is stripped to its glyph for the
   same reason. */
/* CENTRE, not baseline (the default here): an icon-only button has no text baseline to share, so on
   `align-items: baseline` it dragged the whole label row down by its own box. */
.pt-root.compact .pt-cell-head { gap: 0.25rem; min-height: 1.2rem; align-items: center; }
.pt-root.compact .pt-cell-head .cc-btn {
  padding: 0; height: 1.2rem; min-height: 0; line-height: 1; flex: 0 0 auto;
}
.pt-root.compact .pt-label { flex: 1; font-size: var(--cc-fs-2xs); }
.pt-root.compact .pt-val { font-size: var(--cc-fs-2xs); }
/* the threads cell earns a bit more room: its label is longer and its value can read "auto · 8" */
.pt-root.compact .pt-cell.threads { flex: 1.6 1 7rem; }
.pt-head { font-size: var(--cc-fs-sm); font-weight: 600; color: var(--cc-text); margin-bottom: 0.5rem; }
.pt-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 0.5rem 0.9rem; }
.pt-cell { display: flex; flex-direction: column; gap: 0.15rem; }
.pt-cell-head { display: flex; justify-content: space-between; align-items: baseline; }
.pt-label { font-size: var(--cc-fs-sm); color: var(--cc-text); }
.pt-val { font-size: var(--cc-fs-sm); font-variant-numeric: tabular-nums; color: var(--cc-accent, var(--cc-text)); }
.pt-slider { width: 100%; accent-color: var(--cc-accent); cursor: pointer; }
.pt-slider:disabled { opacity: 0.5; cursor: default; }

/* live occupancy readout under each slider — dim when idle, brightens when the pool is busy */
.pt-occ { display: flex; justify-content: space-between; align-items: baseline; gap: 0.3rem; }
.pt-occ.busy { color: var(--cc-text); }
.pt-occ.busy .pt-occ-n { color: var(--cc-accent); font-weight: 600; }
.pt-occ-sep { opacity: 0.5; margin: 0 1px; }
.pt-occ-q { color: var(--cc-accent); }
.pt-bar { height: 3px; border-radius: var(--cc-radius-xs); background: var(--cc-surface-2); overflow: hidden; margin-top: 2px; }
.pt-bar-fill { height: 100%; background: var(--cc-accent); transition: width 0.3s; }

.pt-hint { margin: 0.55rem 0 0; }

/* thread budget — separated from the pools by a rule, because it rations a different thing */
.pt-threads { margin-top: 0.7rem; padding-top: 0.6rem; border-top: 1px solid var(--cc-border); }
.pt-val.auto { color: var(--cc-text-dim); }
.pt-auto { margin-top: 0.35rem; color: var(--cc-accent); }
</style>
