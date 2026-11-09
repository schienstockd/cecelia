<!--
  What made this board: the provenance record an exported board figure embeds, shown readable — for the
  board on screen, or for a figure file (PDF/SVG) the user picks (docs/ANALYSIS.md → *What made this
  board*). One layout for both: what it shows, the steps with their non-default parameters, and what
  changed since the record's time — the board's frozen-parameters time, or the figure's export time.
  Steps from `recordRows`; gates from `POST /api/analysis/gate-edits` (the gate history). Read-only;
  a figure file is parsed in the browser, nothing is uploaded.
-->
<script setup lang="ts">
import { ref, computed, onMounted } from 'vue'
import BaseModal from '../BaseModal.vue'
import SelectionTable, { type SelectionColumn } from '../SelectionTable.vue'
import InlineNote from '../InlineNote.vue'
import { postJson } from '../../utils/blackboardApi'
import { downloadText } from '../../plots/export'
import { paramLines, paramSummary } from '../../utils/paramTree'
import type { HistoryParamsRow } from '../../utils/chainFromHistory'
import {
  boardProvenanceUrl, buildProvenanceRecord, readFigureRecord, recordRows, recordRowsCsv, recordGateFiles, rowParamTree,
  popGateStatus, type ProvenanceRecord, type ProvenanceBoardIn, type BoardProvenanceResponse, type GateEdit,
  type RowStatus,
} from '../../utils/boardProvenance'

const props = defineProps<{
  projectUid: string
  /** the record of the board on screen (what an export would embed now), or null when it can't be read */
  loadCurrent: () => Promise<ProvenanceBoardIn | null>
}>()
const emit = defineEmits<{ (e: 'close'): void }>()

const record = ref<ProvenanceRecord | null>(null)
const fileName = ref('')                                   // '' = the board on screen
const since = ref('')                                      // ISO; '' = nothing to compare against
// per board: today's table (a file), null = the board on screen (compared with its frozen rows)
const nowRuns = ref<(HistoryParamsRow[] | null)[]>([])
const nowGateFiles = ref<Record<string, string[]>[]>([])  // per board: population key → today's gate files
const gates = ref(new Map<string, GateEdit>())
const note = ref('')
const loading = ref(false)
const comparable = ref(true)                               // false: a file from another project

async function loadGateEdits(rec: ProvenanceRecord) {
  gates.value = new Map()
  const files = recordGateFiles(rec)
  if (!since.value || !files.length) return
  // no answer (network, non-2xx) leaves every gate unflagged
  const r = await postJson('/api/analysis/gate-edits', { projectUid: props.projectUid, since: since.value, files })
  if (Array.isArray(r?.files)) gates.value = new Map((r.files as GateEdit[]).map(g => [g.file, g]))
}

async function showCurrent() {
  loading.value = true; note.value = ''; fileName.value = ''; comparable.value = true
  try {
    const b = await props.loadCurrent()
    record.value = b ? buildProvenanceRecord([b], new Date().toISOString()) : null
    if (!b || !record.value) { note.value = 'Nothing on this board yet.'; return }
    nowRuns.value = [null]
    nowGateFiles.value = [{}]
    since.value = record.value.boards[0].parameters.takenAt ?? ''
    await loadGateEdits(record.value)
  } finally { loading.value = false }
}

const fileInput = ref<HTMLInputElement | null>(null)
async function onFile(e: Event) {
  const input = e.target as HTMLInputElement
  const f = input.files?.[0]
  input.value = ''
  if (!f) return
  loading.value = true; note.value = ''
  try {
    const rec = await readFigureRecord(f.name, await f.arrayBuffer())
    fileName.value = f.name
    record.value = rec
    if (!rec) { note.value = 'No Cecelia record in this file.'; return }
    since.value = rec.exportedAt
    comparable.value = rec.project.uid === props.projectUid
    if (!comparable.value) {
      gates.value = new Map()
      note.value = `From project “${rec.project.name}” — open it to see what changed since.`
      return
    }
    // today's table and gate files for the same images and populations. Cluster runs aren't in the
    // record, so a step only a cluster run reaches comes back `absent`, never wrongly `same`.
    const uids = rec.images.map(i => i.uid)
    const today = await Promise.all(rec.boards.map(async b => {
      try {
        const res = await fetch(boardProvenanceUrl(props.projectUid, uids, { pops: b.populations.map(p => p.key), clusterRuns: [] }))
        return res.ok ? (await res.json()) as BoardProvenanceResponse : null
      } catch { return null }
    }))
    nowRuns.value = today.map(t => t?.runs ?? [])
    nowGateFiles.value = today.map(t => Object.fromEntries((t?.populations ?? []).map(p => [p.key, p.gateFiles])))
    await loadGateEdits(rec)
  } finally { loading.value = false }
}

onMounted(showCurrent)

const fmt = (iso?: string) => (iso ? iso.replace('T', ' ').slice(0, 16) : '')
// the flags, in words; the tooltip says what each means
const STATUS: Record<RowStatus, { short: string; detail: string } | null> = {
  same: null,
  changed: { short: 'changed', detail: 'Ran again, or added, after the board was frozen — the plots show the newer run' },
  reran: { short: 're-ran', detail: 'This step ran again after the figure was exported' },
  absent: { short: 'not found', detail: 'No such step for these images and populations now' },
}
const GATE: Record<string, { short: string; detail: string }> = {
  edited: { short: 'gate changed', detail: 'A gate of this population was edited, or added to an image, since' },
  missing: { short: 'gate gone', detail: 'The gate file this population was defined by no longer exists' },
  unknown: { short: 'gate ?', detail: 'Can’t tell whether this gate changed: no gate history from then, or the check failed' },
  invalid: { short: 'gate ?', detail: 'The record names a gate file this project can’t read' },
}

const columns: SelectionColumn[] = [
  { key: 'step', label: 'Step' }, { key: 'params', label: 'Parameters', ellipsis: true }, { key: 'ran', label: 'Ran' },
  { key: 'flag', label: '' },
]
// a row opens to its parameters as an indented tree (`paramTree`); the cell shows only a summary.
// Accordion: one step open at a time, across every board in the dialog.
const openId = ref<string | null>(null)
const toggle = (id: string) => { openId.value = openId.value === id ? null : id }
const boards = computed(() => (record.value?.boards ?? []).map((b, i) => {
  const rows = recordRows(b.runs, comparable.value ? (nowRuns.value[i] ?? null) : null)
  const flags = comparable.value && !!since.value
  return {
    title: b.title,
    rows,
    pops: b.populations.map(p => {
      const s = flags ? popGateStatus(p.gateFiles, gates.value, nowGateFiles.value[i]?.[p.key] ?? []) : null
      return { key: p.key, label: [p.valueName, p.pop].filter(Boolean).join(' '), flag: s ? GATE[s] ?? null : null }
    }),
    table: rows.map((r, k) => {
      const tree = rowParamTree(r)
      return {
        id: `${i}:${k}`, step: r.output ? `${r.step} · ${r.output}` : r.step, ran: r.lastRun.slice(0, 10),
        params: paramSummary(tree), lines: paramLines(tree), flag: flags ? STATUS[r.status] : null,
      }
    }),
  }
}))
const anyFlag = computed(() => boards.value.some(b => b.table.some(r => r.flag) || b.pops.some(p => p.flag)))

const summary = computed(() => {
  const r = record.value
  if (!r) return ''
  const when = fileName.value ? `exported ${fmt(r.exportedAt)}` : since.value ? `frozen ${fmt(since.value)}` : 'not frozen yet'
  return [`${r.images.length} image${r.images.length === 1 ? '' : 's'}`, when, r.author?.profile, `Cecelia ${r.ceceliaVersion}`]
    .filter(Boolean).join(' · ')
})

function downloadCsv() {
  const label = (s: RowStatus) => (comparable.value && since.value ? STATUS[s]?.short ?? '' : '')
  const csv = recordRowsCsv(boards.value, label)
  const base = (fileName.value.replace(/\.[^.]+$/, '') || record.value?.boards[0]?.title || 'board').replace(/[^\w.-]+/g, '_')
  downloadText(`${base}_record.csv`, csv, 'text/csv')
}
</script>

<template>
  <BaseModal :title="fileName || 'What made this board'" icon="pi pi-history" width="760px" @close="emit('close')">
    <div v-if="loading" class="cc-empty"><i class="pi pi-spin pi-spinner" /></div>
    <template v-else>
      <div v-if="note" class="cc-empty-inline cc-muted">{{ note }}</div>
      <div v-if="record" class="rec-head">
        <div class="cc-muted cc-fs-xs" v-tooltip.bottom="record.images.map(i => i.name).join(', ')">{{ summary }}</div>
        <div v-if="anyFlag" class="cc-muted cc-fs-xs">
          Flags mark what changed since the {{ fileName ? 'export' : 'board was frozen' }}; hover one for details.
        </div>
      </div>
      <section v-for="(b, bi) in boards" :key="bi" class="rec-board">
        <h4 v-if="boards.length > 1" class="cc-eyebrow">{{ b.title }}</h4>
        <div v-if="b.pops.length" class="cc-row cc-fs-xs">
          <span class="cc-muted">Populations</span>
          <span v-for="p in b.pops" :key="p.key" class="cc-row-group">
            {{ p.label }}<InlineNote v-if="p.flag" :short="p.flag.short" :detail="p.flag.detail" severity="warn" />
          </span>
        </div>
        <SelectionTable :columns="columns" :rows="b.table" id-key="id" selection-mode="none" density="compact"
                        :is-expanded="r => openId === r.id" @row-click="r => r.lines.length && toggle(r.id)">
          <template #cell-params="{ row }">
            <span v-if="row.lines.length" class="rec-params">
              <i :class="['pi', openId === row.id ? 'pi-chevron-down' : 'pi-chevron-right', 'cc-muted']" />{{ row.params }}
            </span>
            <span v-else class="cc-muted">defaults</span>
          </template>
          <template #row-detail="{ row }">
            <div class="rec-tree cc-fs-xs">
              <div v-for="(l, n) in row.lines" :key="n" :style="{ paddingLeft: `${l.depth * 14}px` }">
                <span :class="l.value === undefined ? 'cc-muted' : ''">{{ l.key }}</span><template v-if="l.value !== undefined">
                <span class="cc-muted"> = </span>{{ l.value }}</template>
              </div>
            </div>
          </template>
          <template #cell-flag="{ row }">
            <InlineNote v-if="row.flag" :short="row.flag.short" :detail="row.flag.detail" severity="warn" />
          </template>
        </SelectionTable>
      </section>
    </template>
    <template #footer>
      <button v-if="fileName" class="cc-btn cc-btn-ghost" type="button" @click="showCurrent">This board</button>
      <button class="cc-btn cc-btn-ghost" type="button" v-tooltip.top="'Read the record inside an exported PDF or SVG'"
              @click="fileInput?.click()">Read a figure file…</button>
      <input ref="fileInput" type="file" accept=".pdf,.svg" hidden @change="onFile" />
      <span class="rec-spacer" />
      <button class="cc-btn" type="button" :disabled="!boards.length" v-tooltip.top="'The steps as a CSV'" @click="downloadCsv">
        <i class="pi pi-download" /> CSV
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
.rec-head { display: flex; flex-direction: column; gap: 2px; margin-bottom: 10px; }
.rec-board { display: flex; flex-direction: column; gap: 6px; margin-bottom: 14px; }
.rec-spacer { flex: 1; }
.rec-params { cursor: pointer; }
.rec-params .pi { font-size: 0.65em; margin-right: 4px; }
.rec-tree { padding: 4px 0 6px 18px; font-family: var(--cc-mono); line-height: 1.5; }
</style>
