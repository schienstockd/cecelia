<!--
  PoolThrottle — live scheduler throttle: one slider per resource pool (docs/SCHEDULER.md).
  Lower a pool to be gentle (e.g. Disk I/O to 1 for slow-share imports), raise it when the machine
  is free. Changes apply immediately (resize_pool!) and persist to custom.toml. Compact 2×2 grid:
  cpu/gpu on the first row, io/network on the second. Lives in a popover off the Task Manager.
-->
<script setup lang="ts">
import { ref, computed, onMounted } from 'vue'

interface PoolInfo { name: string; limit: number }
const POOL_ORDER = ['cpu', 'gpu', 'io', 'network']
const POOL_META: Record<string, { label: string; max: number; tip: string }> = {
  cpu:     { label: 'CPU',      max: 32, tip: 'General compute — most tasks (tracking, measures, clustering, corrections).' },
  gpu:     { label: 'GPU',      max: 8,  tip: 'The GPU — cellpose segmentation/correction. 1 runs one GPU job at a time; raise for batch segmentation.' },
  io:      { label: 'Disk I/O', max: 16, tip: 'Local disk — image import/convert and crop. Drop to 1 when importing over a slow network share so copies don\'t pile up.' },
  network: { label: 'Network',  max: 8,  tip: 'Remote/SMB reads — reserved for HPC/remote tasks. No tasks use it yet.' },
}
const pools    = ref<Record<string, number>>({})
const poolBusy = ref<string | null>(null)
const orderedPools = computed(() => POOL_ORDER.filter(n => n in pools.value))

async function loadPools() {
  try {
    const res = await fetch('/api/pools')
    if (res.ok) {
      const list = await res.json() as PoolInfo[]
      pools.value = Object.fromEntries(list.map(p => [p.name, p.limit]))
    }
  } catch { /* backend may not be ready */ }
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

onMounted(loadPools)
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
.pt-hint { font-size: 0.68rem; color: var(--cc-text-dim); margin: 0.55rem 0 0; }
</style>
