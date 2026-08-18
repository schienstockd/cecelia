<!--
  The tracking-correction worklist — one entry in the interactive-view registry.

  This INVERTS the old R version. There you found the wrong track yourself, across hundreds of them,
  and then said how to fix it; the app helped with neither. Here the backend ranks what looks wrong
  and pre-picks the fix (`GET /api/tracking/issues`), each row carries a ready-to-submit op, and the
  user only judges it — which is the part a human is actually needed for.

  **Why each row draws its geometry.** The question a candidate poses cannot be answered by its
  numbers. Two track ends 2 µm apart are ONE cell if the first was heading toward where the second
  starts, and TWO cells if it was heading away — same distance, opposite answer. So the row shows the
  paths (`plots/trackPaths.ts`, Observable Plot) with the decision point marked, purely so an obvious
  non-issue can be rejected without leaving the list. napari remains where you look at the image;
  "Show" flies it to the candidate's own coordinate and frame.

  **Nothing is written until Apply.** Judging a row queues its op; the queue is submitted as ONE
  `tracking.correct_measures` run (Decision 3b). That is old R's `-mod` staging, without the file: it
  wrote a whole second `.h5ad` per edit because reticulate had no other channel.

  Registry-hosted so it gets the panel chrome, zoom and export for free — but page-scoped ONLY. It
  MUTATES, and the Analysis board is read-only (docs/ANALYSIS.md).
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, nextTick, useTemplateRef } from 'vue'
import SelectionTable from '../SelectionTable.vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import PlotSpinner from './PlotSpinner.vue'
import ConfirmButton from '../ConfirmButton.vue'
import { useDataRefresh } from '../../composables/useDataRefresh'
import { useLogStore } from '../../stores/log'
import { useTaskStore } from '../../stores/tasks'
import { useProjectStore } from '../../stores/project'
import { useWsStore } from '../../stores/ws'
import {
  visibleIssues, worklistSummary, opLabel, opDescription, issueKey, undoLast, worklistCsvRows,
  KIND_LABEL, type TrackIssue, type TrackOp, type IssuesResponse,
} from '../../lib/trackCorrection'
import { rowsToCsv, downloadBlob, downloadDataUrl, elementToImageURL } from '../../plots/export'
import {
  pathPoints, pathDomain, focusPoint, gapGeometry, gapHint, type TrackPathMap,
} from '../../plots/trackPaths'
import { resolveTrackValueName } from '../../plots/trackDiagnostics'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  // `valueName` is the tracked label set; `kinds` filters the worklist; `pending` is the uncommitted
  // op queue and `skipped` the rows judged not-a-problem. All persisted, so navigating away and back
  // does not lose a half-reviewed movie.
  state: { imageUid?: string; valueName?: string; kinds?: string[]
           pending?: TrackOp[]; skipped?: string[] }
}>()

const log = useLogStore()
const tasks = useTaskStore()
const project = useProjectStore()
const ws = useWsStore()

const imageUid = computed(() => (props.state.imageUid && props.imageUids.includes(props.state.imageUid))
  ? props.state.imageUid : (props.imageUids[0] ?? ''))
// NEVER default to 'default' (or to the ACTIVE segmentation): both are routinely untracked, and this
// panel then reports "nothing to review" for an image that has candidates — see resolveTrackValueName.
const valueNames = ref<string[]>([])
const trackedNames = ref<string[]>([])
const activeName = ref('')            // the segmentation the rest of the app is pointed at
const valueName = computed({
  get: () => resolveTrackValueName(props.state.valueName, trackedNames.value, valueNames.value,
                                   activeName.value),
  set: v => (props.state.valueName = v),
})
const kinds = computed({ get: () => props.state.kinds ?? [], set: v => (props.state.kinds = v) })
const pending = computed({ get: () => props.state.pending ?? [], set: v => (props.state.pending = v) })
const skipped = computed({ get: () => props.state.skipped ?? [], set: v => (props.state.skipped = v) })

const data = ref<IssuesResponse | null>(null)
const loading = ref(false)
const error = ref('')

const paths = computed<TrackPathMap>(() => (data.value?.paths ?? {}) as TrackPathMap)
const rows = computed(() => visibleIssues(data.value?.issues ?? [], {
  kinds: kinds.value, pending: pending.value, skipped: skipped.value,
}))
const summary = computed(() => worklistSummary(data.value, pending.value.length))

const kindOptions = computed<ChipOption[]>(() =>
  Object.entries(data.value?.counts ?? {})
    .map(([k, n]) => ({ value: k, label: `${KIND_LABEL[k] ?? k} ${n}` })))

/** Which segmentations this image has, and which of them are tracked. */
async function loadValueNames() {
  if (!props.projectUid || !imageUid.value) { valueNames.value = []; trackedNames.value = []; return }
  try {
    const r = await fetch(`/api/gating/channels?projectUid=${props.projectUid}` +
                          `&imageUid=${imageUid.value}&popType=track`)
    if (!r.ok) return
    const d = await r.json() as { valueNames?: string[]; trackedValueNames?: string[]
                                  valueName?: string }
    valueNames.value = d.valueNames ?? []
    trackedNames.value = d.trackedValueNames ?? []
    activeName.value = d.valueName ?? ''    
  } catch { /* the issues request reports its own failure */ }
}

async function load() {
  if (!props.projectUid || !imageUid.value) { data.value = null; return }
  loading.value = true; error.value = ''
  try {
    const q = `projectUid=${props.projectUid}&imageUid=${imageUid.value}` +
              `&valueName=${encodeURIComponent(valueName.value)}`
    const r = await fetch(`/api/tracking/issues?${q}`)
    const d = await r.json()
    if (!r.ok) throw new Error(d?.error || `HTTP ${r.status}`)
    data.value = d as IssuesResponse
  } catch (e) {
    error.value = String((e as Error)?.message ?? e)
    data.value = null
  } finally {
    loading.value = false
  }
}
onMounted(async () => { await loadValueNames(); await load() })
watch([() => props.projectUid, imageUid], async () => { await loadValueNames(); await load() })
watch(valueName, load)
// applying corrections re-runs the detector's own input, and `commit` promises the user this list
// comes back updated — that promise is this line (subject to the global autoRefreshOnTask setting)
useDataRefresh(() => (imageUid.value ? [imageUid.value] : []), load)

// ── Judging a row ─────────────────────────────────────────────────────────────
// Queue, don't write. The op goes on the stack exactly as the detector emitted it — nothing here
// translates a suggestion into an edit, which is the property that makes the worklist trustworthy.
function queue(i: TrackIssue) {
  pending.value = [...pending.value, i.op]
}
function skip(i: TrackIssue) {
  skipped.value = [...skipped.value, issueKey(i)]
}
function undo() {
  pending.value = undoLast(pending.value)
}

/** Fly napari to the candidate's own coordinate and frame — the row and the viewer agree on "here". */
async function show(i: TrackIssue) {
  try {
    await fetch('/api/napari/centre', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ pos: i.centroid, tp: Math.round(i.atT) }),
    })
  } catch (e) {
    log.warn(`Could not move the viewer: ${e}`, { source: 'correct' })
  }
}

// ── Committing ────────────────────────────────────────────────────────────────
// ONE task run for the whole queue, through the composite that also recomputes the track measures —
// so a correction can never leave `live.*` describing the previous assignment.
const FUN = 'tracking.correct_measures'
function commit() {
  if (!pending.value.length || !imageUid.value) return
  const params = { valueName: valueName.value, trackOps: JSON.stringify(pending.value) }
  const img = project.imageByUid(imageUid.value)
  const task = tasks.add({
    module: 'tracking', label: 'Correct tracks', imageUid: imageUid.value,
    imageName: img?.name || imageUid.value,
    status: 'queued' as const, taskName: 'trackCorrectMeasures', funName: FUN,
    params, projectUid: props.projectUid,
  })
  ws.send({ type: 'task:run', taskId: task.id, funName: FUN, params,
            imageUid: imageUid.value, projectUid: props.projectUid, setUid: props.setUid,
            poolName: 'cpu' })
  log.info(`Applying ${pending.value.length} correction(s) — the worklist refreshes when it finishes.`,
           { source: 'correct' })
  pending.value = []
  skipped.value = []
}

// ── Row geometry ──────────────────────────────────────────────────────────────
// Drawn with Observable Plot, loaded lazily like the other plot views so the module is not in the
// initial bundle for a page nobody opened.
const PlotMod = ref<typeof import('@observablehq/plot') | null>(null)
onMounted(async () => { PlotMod.value = await import('@observablehq/plot') })

// The `#cell-` slot exposes `{ row, value }` and NO index, so a positional ref array cannot line up
// with the rows — and quietly drawing every thumbnail one row off is exactly the kind of wrong that
// looks fine. Each container carries its issue key instead, and the draw pass matches on that.
const tcRoot = useTemplateRef<HTMLElement>('tcRoot')

/** The heading hint for a gap — the discriminator the row's numbers cannot express. */
function hintFor(i: TrackIssue): string {
  if (i.kind !== 'gap' || i.trackIds.length < 2) return ''
  const g = gapGeometry(paths.value, i.trackIds[0], i.trackIds[1])
  return g ? gapHint(g.cosine) : ''
}

function drawThumbs() {
  const P = PlotMod.value
  if (!P) return
  const root = tcRoot.value
  if (!root) return
  const byKey = new Map(rows.value.map(i => [issueKey(i), i]))
  root.querySelectorAll<HTMLElement>('.tc-thumb').forEach(el => {
    const i = byKey.get(el.dataset.issue ?? '')
    if (!i) return
    el.replaceChildren()
    const pts = pathPoints(paths.value, i.trackIds)
    if (!pts.length) return
    const dom = pathDomain(pts)
    const focus = focusPoint(pts, i.atT, i.trackIds[0])
    const marks = [
      P.line(pts, { x: 'x', y: 'y', z: 'track', stroke: 'track', strokeWidth: 1.5 }),
      P.dot(pts.filter(p => p.i === 0), { x: 'x', y: 'y', fill: 'track', r: 2 }),
    ]
    // the decision point — where the join would close, or where the split would cut
    if (focus) marks.push(P.dot([focus], { x: 'x', y: 'y', stroke: 'currentColor', r: 4, strokeWidth: 1.5 }))
    el.append(P.plot({
      width: 132, height: 132, margin: 4,
      x: { domain: dom?.x, axis: null }, y: { domain: dom?.y, axis: null },
      color: { legend: false },
      style: { background: 'transparent' },
      marks,
    }))
  })
}
watch([rows, PlotMod], () => nextTick(drawThumbs), { deep: false })

const columns = [
  { key: 'thumb',  label: '',        width: 140 },
  { key: 'reason', label: 'What looks wrong' },
]

// ── export (the generic panel contract — plots/export.ts) ──
// CSV is the point of exporting a worklist: a correction is a change to the data that no figure
// shows, so the record of what was found and what was decided has to be able to leave the app.
// The PNG is the review itself, thumbnails included (foreignObject capture, like the HMM panels).
// No SVG: the picture here is a TABLE, and a vector table is a screenshot with extra steps.
const exportFormats = ['csv', 'png']
const stem = computed(() => `track_worklist_${valueName.value}`.replace(/[^\w.-]+/g, '_'))
function exportAs(kind: string) {
  if (kind === 'csv') {
    const rows = worklistCsvRows(data.value?.issues ?? [], pending.value, skipped.value)
    if (rows.length) downloadBlob(`${stem.value}.csv`, new Blob([rowsToCsv(rows)], { type: 'text/csv' }))
  } else if (kind === 'png') {
    elementToImageURL(tcRoot.value, 'png', '#1f2226')
      .then(url => url && downloadDataUrl(`${stem.value}.png`, url))
  }
}
defineExpose({ exportFormats, exportAs })
</script>

<template>
  <div ref="tcRoot" class="tc">
    <div class="cc-row tc-head">
      <span class="cc-muted cc-fs-sm">
        <i class="pi pi-flag" /> {{ summary }}
      </span>
      <ChipSelect v-if="kindOptions.length" v-model="kinds" :options="kindOptions" multiple
                  v-tooltip="'Show only these kinds'" />
      <span class="tc-spacer" />
      <select v-if="valueNames.length > 1" v-model="valueName"
              v-tooltip="'Which tracked segmentation'" aria-label="Segmentation">
        <option v-for="vn in valueNames" :key="vn" :value="vn"
                :disabled="trackedNames.length > 0 && !trackedNames.includes(vn)">{{ vn }}</option>
      </select>
      <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip="'Re-scan the tracks'" @click="load">
        <i class="pi pi-refresh" />
      </button>
    </div>

    <PlotSpinner v-if="loading" />
    <p v-else-if="error" class="cc-muted cc-fs-sm">{{ error }}</p>
    <p v-else-if="!rows.length" class="cc-muted cc-fs-sm">{{ summary || 'Nothing to review.' }}</p>

    <SelectionTable
      v-else
      :rows="rows"
      :columns="columns"
      selection-mode="none"
      density="compact"
      row-key="reason"
    >
      <template #cell-thumb="{ row }">
        <div class="tc-thumb" :data-issue="issueKey(row as TrackIssue)" />
      </template>
      <template #cell-reason="{ row }">
        <div class="tc-reason">
          <span class="cc-module-tag">{{ KIND_LABEL[(row as TrackIssue).kind] }}</span>
          {{ (row as TrackIssue).reason }}
          <span v-if="hintFor(row as TrackIssue)" class="cc-muted cc-fs-2xs tc-hint">
            <i class="pi pi-directions" /> {{ hintFor(row as TrackIssue) }}
          </span>
        </div>
      </template>
      <template #actions="{ row }">
        <div class="cc-btn-group">
          <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
                  v-tooltip="'Show it in napari'" @click="show(row as TrackIssue)">
            <i class="pi pi-map-marker" />
          </button>
          <button class="cc-btn cc-btn-primary cc-btn-micro"
                  v-tooltip="opDescription((row as TrackIssue).op)" @click="queue(row as TrackIssue)">
            {{ opLabel((row as TrackIssue).op) }}
          </button>
          <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
                  v-tooltip="'Not a problem — hide it'" @click="skip(row as TrackIssue)">
            <i class="pi pi-times" />
          </button>
        </div>
      </template>
    </SelectionTable>

    <div v-if="pending.length" class="cc-row tc-foot">
      <span class="cc-muted cc-fs-sm">{{ pending.length }} queued</span>
      <button class="cc-btn cc-btn-ghost cc-btn-dense" v-tooltip="'Take back the last one'" @click="undo">
        <i class="pi pi-undo" /> Undo
      </button>
      <ConfirmButton class="cc-btn cc-btn-primary cc-btn-dense"
                     :label="`Apply ${pending.length}`"
                     confirm-label="Apply — this rewrites the tracks"
                     @confirm="commit">
        <i class="pi pi-save" /> Apply {{ pending.length }}
      </ConfirmButton>
    </div>
  </div>
</template>

<style scoped>
.tc { display: flex; flex-direction: column; gap: 0.5rem; height: 100%; min-height: 0; }
/* `.cc-row` carries the flex/align/wrap/gap; only the bits it does not own live here. */
.tc-foot { margin-top: auto; }
.tc-spacer { flex: 1; }
.tc-thumb { width: 132px; height: 132px; }
.tc-reason { display: flex; flex-direction: column; gap: 0.15rem; }
.tc-hint { display: inline-flex; align-items: center; gap: 0.25rem; }
</style>
