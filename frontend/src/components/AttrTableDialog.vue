<script setup lang="ts">
// Attributes from a table: paste, drop or choose a CSV/TSV, match its rows to the set's images by a key
// column (name, original path or ID), preview what it would write, then write it through the ordinary
// attr routes — or, from the design dialog (`mode: 'use'`), hand the values back for the design's
// preview, written when the design is confirmed. Logic in utils/attrTable.ts; docs/UI.md → *Metadata*.
import { computed, ref } from 'vue'
import BaseModal from './BaseModal.vue'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'
import InlineNote from './InlineNote.vue'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'
import {
  detectKey, KEY_KIND_LABEL, parseTable, planImport, planSummary, planWrites,
  type ImportPlan, type KeyKind, type TableImage, type TableKey,
} from '../utils/attrTable'
import { writeImportPlan } from '../utils/imageAttrApi'
import { useProjectStore } from '../stores/project'
import { useLogStore } from '../stores/log'

const props = withDefaults(defineProps<{
  projectUid: string
  setUid: string
  images: TableImage[]
  /** `apply` writes; `use` returns the plan (the design dialog writes it on confirm) */
  mode?: 'apply' | 'use'
}>(), { mode: 'apply' })
const emit = defineEmits<{ (e: 'close'): void; (e: 'applied'): void; (e: 'use', plan: ImportPlan): void }>()

const project = useProjectStore()
const log = useLogStore()

// ── Input ─────────────────────────────────────────────────────────────────────────────────────────
const text = ref('')
const fileInput = ref<HTMLInputElement | null>(null)
async function loadFile(f: File | null | undefined) { if (f) { text.value = await f.text(); keyOverride.value = null } }
function onDrop(e: DragEvent) { void loadFile(e.dataTransfer?.files?.[0]) }

const table = computed(() => text.value.trim() ? parseTable(text.value) : null)

// ── Key: detected, until the user picks another ───────────────────────────────────────────────────
const keyOverride = ref<TableKey | null>(null)
const detected = computed(() => table.value ? detectKey(table.value, props.images) : null)
const key = computed<TableKey>(() => keyOverride.value ?? detected.value ?? { column: 0, kind: 'name' })
const KIND_OPTIONS: ChipOption[] = (['name', 'path', 'uid'] as KeyKind[]).map(k => ({
  value: k, label: KEY_KIND_LABEL[k],
  tip: { name: 'Image name, with or without extension', path: 'Original file path, whole or its end', uid: 'Image ID' }[k],
}))
const keyColumn = computed({
  get: () => String(key.value.column),
  set: (v: string) => { keyOverride.value = { ...key.value, column: Number(v) } },
})
const keyKind = computed({
  get: () => key.value.kind,
  set: (v: string | string[] | null) => { keyOverride.value = { ...key.value, kind: (v as KeyKind) ?? 'name' } },
})

// ── The plan and its preview ──────────────────────────────────────────────────────────────────────
const plan = computed(() => table.value ? planImport(table.value, props.images, key.value) : null)

const columns = computed<SelectionColumn[]>(() => [
  { key: 'name', label: 'Image', ellipsis: true, sortable: true },
  ...(plan.value?.attrs ?? []).map((a, i) => ({
    key: `c${i}`, label: plan.value!.newAttrs.includes(a) ? `${a} (new)` : a, ellipsis: true,
  })),
])
const rows = computed(() => (plan.value?.matched ?? []).map(r => ({
  uid: r.uid, name: r.name, cells: r.cells,
  ...Object.fromEntries(r.cells.map((c, i) => [`c${i}`, c.value])),
})))

type Cell = ImportPlan['matched'][number]['cells'][number]
const cellOf = (row: Record<string, unknown>, ci: number) => (row.cells as Cell[])[ci]
function cellClass(row: Record<string, unknown>, ci: number) {
  const c = cellOf(row, ci).change
  return c === 'blank' || c === 'same' ? 'cc-muted' : c === 'overwrite' || c === 'case' ? 'at-over' : ''
}
function cellTip(row: Record<string, unknown>, ci: number) {
  const c = cellOf(row, ci)
  return { blank: 'Blank: left as it is', same: 'Already this value', set: 'New value', case: `Was ${c.old}`, overwrite: `Was ${c.old}` }[c.change]
}

const list = (xs: string[], max = 12) => xs.slice(0, max).join(', ') + (xs.length > max ? `, +${xs.length - max}` : '')
const notes = computed(() => {
  const p = plan.value
  if (!p) return []
  const out: { short: string; detail: string; warn?: boolean }[] = []
  const overwrites = p.counts.overwrite + p.counts.case
  if (overwrites) out.push({ short: `${overwrites} value(s) overwritten`, warn: true,
    detail: list(p.matched.flatMap(r => r.cells.filter(c => c.change === 'overwrite' || c.change === 'case')
      .map(c => `${r.name} ${c.attr}: ${c.old} → ${c.value}`)), 8) })
  if (p.variants.length) out.push({ short: 'Values differ only by case or spacing', warn: true,
    detail: p.variants.map(v => `${v.attr}: ${v.values.join(' / ')}`).join('; ') })
  if (p.nameClashes.length) out.push({ short: 'Column names differ only by case', warn: true,
    detail: p.nameClashes.map(c => `${c.attr} vs ${c.existing}`).join('; ') })
  if (p.unmatched.length) out.push({ short: `${p.unmatched.length} row(s) match no image`,
    detail: list(p.unmatched.map(m => `row ${m.row}: ${m.key || '(blank)'}`)) })
  if (p.ambiguous.length) out.push({ short: `${p.ambiguous.length} row(s) match several images`, warn: true,
    detail: list(p.ambiguous.map(m => `row ${m.row}: ${m.key} (${m.count})`)) })
  if (p.duplicates.length) out.push({ short: `${p.duplicates.length} repeated row(s) skipped`,
    detail: list(p.duplicates.map(m => `row ${m.row}: ${m.name}`)) })
  if (p.imagesWithoutRow.length) out.push({ short: `${p.imagesWithoutRow.length} image(s) have no row`,
    detail: list(p.imagesWithoutRow.map(i => i.name)) })
  if (p.newAttrs.length) out.push({ short: `New: ${list(p.newAttrs, 6)}`, detail: 'Created on every image of the set' })
  const skipped = p.ignored.filter(i => i.why !== 'key')
  if (skipped.length) out.push({ short: `${skipped.length} column(s) not used`,
    detail: list(skipped.map(i => `${i.header || '(blank)'}: ${{ blank: 'no header', imageTable: 'image table column', duplicate: 'repeated', key: '' }[i.why]}`)) })
  return out
})

// ── Apply ─────────────────────────────────────────────────────────────────────────────────────────
const busy = ref(false)
async function apply() {
  const p = plan.value
  if (!p || !planWrites(p)) return
  if (props.mode === 'use') { emit('use', p); emit('close'); return }
  busy.value = true
  try {
    const n = await writeImportPlan(props.projectUid, props.images.map(i => i.uid), p, {
      addAttrKey: a => project.addAttrKey(props.setUid, a),
      setAttrValues: (a, v) => project.setAttrValues(a, v),
    })
    log.info(`Wrote ${n} value(s) from a table to ${p.matched.length} image(s).`, { source: 'metadata' })
    emit('applied')
    emit('close')
  } catch (e) {
    log.error(`Table not applied: ${(e as Error).message}`, { source: 'metadata' })
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <BaseModal title="Attributes from a table" icon="pi-table" width="820px" @close="emit('close')">
    <div class="at-body">
      <div class="at-input" @dragover.prevent @drop.prevent="onDrop">
        <textarea v-model="text" rows="5" class="at-text" placeholder="Paste a table, or drop a CSV / TSV file…"
                  v-tooltip.bottom="'One row per image; a header row first'" @input="keyOverride = null" />
        <div class="cc-row">
          <button class="cc-btn cc-btn-ghost" v-tooltip.bottom="'Load a CSV or TSV file'" @click="fileInput?.click()">
            <i class="pi pi-upload" /> Choose file
          </button>
          <input ref="fileInput" type="file" accept=".csv,.tsv,.txt,text/csv,text/tab-separated-values" hidden
                 @change="e => loadFile((e.target as HTMLInputElement).files?.[0])" />
        </div>
      </div>

      <template v-if="table && plan">
        <div class="cc-row">
          <span class="cc-muted">Match by</span>
          <select v-model="keyColumn" v-tooltip.bottom="'The column that names each image'">
            <option v-for="(h, i) in table.header" :key="i" :value="String(i)">{{ h || `Column ${i + 1}` }}</option>
          </select>
          <ChipSelect v-model="keyKind" :options="KIND_OPTIONS" variant="segmented" aria-label="Key kind" />
          <span class="cc-readout cc-fs-xs">{{ planSummary(plan) }}</span>
        </div>

        <div v-if="!plan.matched.length" class="cc-empty">No row matches an image — try another column</div>
        <div v-else class="at-table">
          <SelectionTable :columns="columns" :rows="rows" id-key="uid" selection-mode="none" density="compact">
            <template v-for="(_, ci) in plan.attrs" :key="ci" #[`cell-c${ci}`]="{ row }">
              <span :class="cellClass(row, ci)" v-tooltip.bottom="cellTip(row, ci)">{{ cellOf(row, ci).change === 'blank' ? '—' : cellOf(row, ci).value }}</span>
            </template>
          </SelectionTable>
        </div>

        <div class="at-notes">
          <InlineNote v-for="n in notes" :key="n.short" :short="n.short" :detail="n.detail"
                      :severity="n.warn ? 'warn' : undefined" />
        </div>
      </template>
    </div>

    <template #footer>
      <button class="cc-btn cc-btn-ghost" :disabled="busy" @click="emit('close')">Cancel</button>
      <span class="at-spacer" />
      <button class="cc-btn cc-btn-primary" :disabled="busy || !plan || !planWrites(plan)" @click="apply">
        {{ mode === 'use' ? 'Use in design' : 'Apply' }}
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
.at-body { display: flex; flex-direction: column; gap: 10px; }
.at-input { display: flex; flex-direction: column; gap: 6px; }
.at-text { width: 100%; box-sizing: border-box; resize: vertical; font-family: var(--cc-mono); }
.at-table { max-height: 260px; overflow: auto; }
.at-over { background: var(--cc-tint-amber); color: var(--cc-ink-amber); border-radius: var(--cc-radius-xs); padding: 0 3px; }
.at-notes { display: flex; flex-direction: column; gap: 4px; }
.at-spacer { flex: 1; }
</style>
