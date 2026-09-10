<!--
  CopyFunParamsModal — pick a previous (image, valueName) run of the current task, so the module form
  can be loaded with the exact params that produced it. Source list comes from
  `GET /api/tasks/funparams/sources` (routes.jl → api_task_fun_params_sources), which sweeps this set
  for `meta.funParamsByName[fun]` keys and matching `run_log` entries.

  Pick-only: the modal returns the chosen row; the caller does the actual param fetch + hand-off
  through `paramHandoff` so the same reconciliation runs as any other offer (flow-model vault, drafts).
-->
<script setup lang="ts">
import { ref, watch } from 'vue'
import BaseModal from './BaseModal.vue'

export interface CopyFunParamsRow {
  imageUid: string
  imageName: string
  valueName: string
  at?: string
}

const props = defineProps<{
  projectUid: string
  setUid: string
  fun: string
  funLabel: string
  currentImageUid?: string   // greyed out (a self-copy is a no-op)
}>()

const emit = defineEmits<{
  (e: 'pick', row: CopyFunParamsRow): void
  (e: 'close'): void
}>()

const rows    = ref<CopyFunParamsRow[]>([])
const loading = ref(false)
const err     = ref('')

async function load() {
  loading.value = true; err.value = ''; rows.value = []
  try {
    const qs = new URLSearchParams({ projectUid: props.projectUid, setUid: props.setUid, fun: props.fun })
    const r = await fetch(`/api/tasks/funparams/sources?${qs.toString()}`)
    if (!r.ok) throw new Error(((await r.json().catch(() => ({}))) as { error?: string }).error ?? `HTTP ${r.status}`)
    rows.value = (await r.json()) as CopyFunParamsRow[]
  } catch (e) {
    err.value = e instanceof Error ? e.message : String(e)
  } finally { loading.value = false }
}

watch(() => [props.projectUid, props.setUid, props.fun], load, { immediate: true })

function pick(row: CopyFunParamsRow) {
  if (row.imageUid === props.currentImageUid) return   // self-copy is a no-op
  emit('pick', row); emit('close')
}

// e.g. "2026-09-05T14:22:01" → "05 Sep 14:22" (loose, best-effort)
function fmtAt(at?: string): string {
  if (!at) return ''
  const m = at.match(/^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2})/)
  if (!m) return at
  const months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
  return `${m[3]} ${months[Number(m[2]) - 1] ?? m[2]} ${m[4]}:${m[5]}`
}
</script>

<template>
  <BaseModal width="520px" @close="emit('close')">
    <template #title>
      <i class="pi pi-copy" /> Copy settings — {{ funLabel }}
    </template>
    <div class="cfp-panel">
      <div v-if="err" class="cfp-err">{{ err }}</div>
      <div v-else-if="loading" class="cfp-hint cc-muted cc-fs-xs">
        <i class="pi pi-spin pi-spinner" /> Reading previous runs…
      </div>
      <div v-else-if="rows.length === 0" class="cfp-hint cc-muted cc-fs-xs">
        No previous runs found in this set.
      </div>
      <ul v-else class="cfp-list">
        <li v-for="row in rows"
            :key="`${row.imageUid}::${row.valueName}`"
            class="cfp-row"
            :class="{ 'cfp-row-self': row.imageUid === currentImageUid }"
            v-tooltip.left="row.imageUid === currentImageUid
              ? 'This is the image you\'re running on'
              : 'Load these settings into the form'"
            @click="pick(row)">
          <span class="cfp-name">{{ row.valueName }}</span>
          <span class="cfp-image cc-muted cc-fs-xs">{{ row.imageName }}</span>
          <span v-if="fmtAt(row.at)" class="cfp-at cc-muted cc-fs-2xs">{{ fmtAt(row.at) }}</span>
        </li>
      </ul>
    </div>
  </BaseModal>
</template>

<style scoped>
.cfp-panel { display: flex; flex-direction: column; gap: 0.5rem; min-height: 4rem; }
.cfp-err { color: var(--cc-danger); font-size: var(--cc-fs-xs); }
.cfp-hint { padding: 0.5rem 0; text-align: center; }
.cfp-list { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.25rem; }
.cfp-row {
  display: grid; grid-template-columns: 1fr auto auto; align-items: baseline; gap: 0.6rem;
  padding: 0.4rem 0.55rem; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-sm);
  cursor: pointer; background: var(--cc-surface-1);
}
.cfp-row:hover { background: var(--cc-surface-2); }
.cfp-row-self { opacity: 0.5; cursor: default; }
.cfp-row-self:hover { background: var(--cc-surface-1); }
.cfp-name { font-weight: 500; color: var(--cc-text); }
.cfp-image, .cfp-at { white-space: nowrap; }
</style>
