<!--
  PoolThrottle — live scheduler throttle + occupancy readout: one slider per resource pool
  (docs/SCHEDULER.md). Lower a pool to be gentle (e.g. Disk I/O to 1 for slow-share imports), raise
  it when the machine is free. Changes apply immediately (resize_pool!) and persist to custom.toml.
  Under each slider a "running / limit" line + mini bar shows how many tasks are executing in that
  pool right now (polled from /api/pools while open — there is no pool:* WS event). Compact 2×2 grid:
  cpu/gpu on the first row, io/network on the second. Lives in a popover off the Task Manager.
-->
<script setup lang="ts">
import { ref, computed, onMounted, onUnmounted } from 'vue'

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

let timer: number | undefined
onMounted(() => { loadPools(); timer = window.setInterval(refreshOccupancy, 1500) })
onUnmounted(() => { if (timer) clearInterval(timer) })
</script>

<template>
  <div class="pt-root">
    <div class="pt-head">Concurrent tasks</div>
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
        <div class="pt-occ" :class="{ busy: runningOf(name) > 0 || queuedOf(name) > 0 }">
          <span><span class="pt-occ-n">{{ runningOf(name) }}</span><span class="pt-occ-sep">/</span>{{ pools[name] }} running</span>
          <span v-if="queuedOf(name) > 0" class="pt-occ-q">+{{ queuedOf(name) }} queued</span>
        </div>
        <div class="pt-bar"><div class="pt-bar-fill" :style="{ width: fillPct(name) }" /></div>
      </div>
    </div>
    <p class="pt-hint">Lower to throttle, raise to run more at once. Saved automatically.</p>
  </div>
</template>

<style scoped>
.pt-root { width: 260px; padding: 0.6rem 0.7rem; }
.pt-head { font-size: 0.78rem; font-weight: 600; color: var(--cc-text); margin-bottom: 0.5rem; }
.pt-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 0.5rem 0.9rem; }
.pt-cell { display: flex; flex-direction: column; gap: 0.15rem; }
.pt-cell-head { display: flex; justify-content: space-between; align-items: baseline; }
.pt-label { font-size: 0.76rem; color: var(--cc-text); }
.pt-val { font-size: 0.78rem; font-variant-numeric: tabular-nums; color: var(--cc-accent, var(--cc-text)); }
.pt-slider { width: 100%; accent-color: var(--cc-accent); cursor: pointer; }
.pt-slider:disabled { opacity: 0.5; cursor: default; }

/* live occupancy readout under each slider — dim when idle, brightens when the pool is busy */
.pt-occ {
  display: flex; justify-content: space-between; align-items: baseline; gap: 0.3rem;
  font-size: 0.64rem; color: var(--cc-text-dim); font-variant-numeric: tabular-nums;
}
.pt-occ.busy { color: var(--cc-text); }
.pt-occ.busy .pt-occ-n { color: var(--cc-accent); font-weight: 600; }
.pt-occ-sep { opacity: 0.5; margin: 0 1px; }
.pt-occ-q { color: var(--cc-accent); }
.pt-bar { height: 3px; border-radius: 2px; background: var(--cc-surface-2); overflow: hidden; margin-top: 2px; }
.pt-bar-fill { height: 100%; background: var(--cc-accent); transition: width 0.3s; }

.pt-hint { font-size: 0.68rem; color: var(--cc-text-dim); margin: 0.55rem 0 0; }
</style>
