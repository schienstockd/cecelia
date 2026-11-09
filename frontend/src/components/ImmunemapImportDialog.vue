<script setup lang="ts">
// Preview + register step for Import from Immunemap. Paste an experiment link (or id) → scan
// (read-only, one request per video) → a table of the experiment's videos with their condition,
// size and calibration, and a group × attribute table that splits each group name into the factors
// to compare by (time prefilled, the rest typed) → register the ticked ones into the current set. Nothing is downloaded here:
// the `importImages.immunemap` task does that, run from the task panel afterwards — the same split as
// LegacyMigrateDialog. Design: docs/todo/IMMUNEMAP_IMPORT_PLAN.md.
import { ref, computed, watch } from 'vue'
import BaseModal from './BaseModal.vue'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'
import {
  addAttrColumn, badAttrColumns, defaultSelection, formatBytes, groupAttrsToRegister,
  mixedPixelSizes, removeAttrColumn, renameAttrColumn, selectionSummary, shapeText,
  suggestGroupAttrs, videoKey, videosToRegister, type GroupAttrs, type ImmunemapManifest,
} from '../utils/immunemapImport'

const props = defineProps<{ projectUid: string; setUid: string }>()
const emit = defineEmits<{ (e: 'imported', images: unknown[]): void; (e: 'close'): void }>()

const experiment = ref('')
const scanning = ref(false)
const importing = ref(false)
const error = ref('')
const manifest = ref<ImmunemapManifest | null>(null)
const selected = ref<string[]>([])
const done = ref(0)
const existed = ref(0)
const attrs = ref<GroupAttrs>({ columns: [], values: {} })
const badCols = computed(() => badAttrColumns(attrs.value.columns))
// The group table: one row per group, one column per attribute. Column keys are positional (`a0`…)
// so a rename changes only the label, not the slot the cell inputs render through.
const groupColumns = computed<SelectionColumn[]>(() => [
  { key: 'name', label: 'Group' },
  ...attrs.value.columns.map((c, i) => ({ key: `a${i}`, label: c })),
])
const groupRows = computed(() => (manifest.value?.groups ?? []).map(g => ({ ...g, key: String(g.id) })))

// Header names are edited as drafts (a v-model per column) and committed on change — the
// `useFieldDraft` pattern for a list, so a re-render mid-typing can't revert the field.
const colDrafts = ref<string[]>([])
watch(() => [...attrs.value.columns], c => { colDrafts.value = [...c] }, { immediate: true })
function onRename(i: number) {
  if (!renameAttrColumn(attrs.value, i, colDrafts.value[i].trim())) colDrafts.value[i] = attrs.value.columns[i]
}

const rows = computed(() => (manifest.value?.videos ?? []).map(v => ({ ...v, key: videoKey(v) })))
const summary = computed(() => selectionSummary(manifest.value?.videos ?? [], selected.value))
const mixed = computed(() => mixedPixelSizes(manifest.value?.videos ?? [], selected.value))

const COLUMNS: SelectionColumn[] = [
  { key: 'name',          label: 'Video',     sortable: true },
  { key: 'condition',     label: 'Condition', sortable: true },
  { key: 'shape',         label: 'Shape' },
  { key: 'sizeBytes',     label: 'Size',      sortable: true },
  { key: 'nCuratedTracks', label: 'Tracks',   sortable: true },
]

async function scan() {
  error.value = ''
  manifest.value = null
  const ref_ = experiment.value.trim()
  if (!ref_) { error.value = 'Paste an Immunemap experiment link or id.'; return }
  scanning.value = true
  try {
    const res = await fetch('/api/import/scan-immunemap', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ experiment: ref_ }),
    })
    const body = await res.json().catch(() => ({})) as ImmunemapManifest
    if (!res.ok || body.error) throw new Error(body.error ?? `HTTP ${res.status}`)
    manifest.value = body
    selected.value = defaultSelection(body.videos)
    attrs.value = suggestGroupAttrs(body.groups)
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  } finally {
    scanning.value = false
  }
}

async function confirmImport() {
  const m = manifest.value
  if (!m || summary.value.n === 0 || badCols.value.length) return
  importing.value = true
  error.value = ''
  try {
    const res = await fetch('/api/import/register-immunemap', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        projectUid: props.projectUid, setUid: props.setUid,
        experiment: m.experiment, videos: videosToRegister(m.videos, selected.value),
        groupAttrs: groupAttrsToRegister(attrs.value),
      }),
    })
    type Registered = { uid?: string; name: string; status: string }
    const body = await res.json().catch(() => ({})) as { images?: Registered[]; error?: string }
    if (!res.ok || body.error) throw new Error(body.error ?? `HTTP ${res.status}`)
    const added = (body.images ?? []).filter(i => i.status === 'pending' && i.uid)
    done.value = added.length
    existed.value = (body.images ?? []).filter(i => i.status === 'exists').length
    emit('imported', added)
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  } finally {
    importing.value = false
  }
}
</script>

<template>
  <BaseModal title="Import from Immunemap" width="900px" @close="$emit('close')">
    <div v-if="done || existed" class="im">
      <p class="im-doneline"><i class="pi pi-check-circle" />
        Added <strong>{{ done }}</strong> video{{ done === 1 ? '' : 's' }}<template v-if="existed">,
        {{ existed }} already in the set</template>.
      </p>
      <p class="cc-muted cc-fs-lg im-lead">
        Select them and run <strong>Import from Immunemap</strong> to download and convert.
      </p>
    </div>

    <div v-else class="im">
      <div class="cc-row">
        <input v-model="experiment" class="im-input"
               v-tooltip.bottom="'Experiment page link from app.immunemap.org, or its id'"
               placeholder="https://app.immunemap.org/experiment-public-view?id=…"
               @keyup.enter="scan" />
        <button class="cc-btn cc-btn-primary" :disabled="scanning" @click="scan">
          <i :class="scanning ? 'pi pi-spin pi-spinner' : 'pi pi-search'" /> Scan
        </button>
      </div>

      <p v-if="error" class="cc-muted-error im-p"><i class="pi pi-exclamation-triangle" /> {{ error }}</p>

      <template v-if="manifest">
        <div class="im-exp">
          <a :href="manifest.experiment.url" target="_blank" rel="noopener">{{ manifest.experiment.name }}</a>
          <span v-for="d in manifest.experiment.dois" :key="d" class="cc-muted"> · doi:{{ d }}</span>
        </div>
        <p class="cc-muted cc-fs-sm im-cite">Cite this DOI and Immunemap; check the dataset’s terms</p>

        <div class="im-groups">
          <div class="cc-row im-attrs">
            <span v-tooltip.bottom="'Each becomes an image attribute; the group name stays as condition'">
              Attributes</span>
            <span v-for="(c, ci) in attrs.columns" :key="c" class="cc-row im-attr">
              <input v-model="colDrafts[ci]" class="cc-input-xs" :class="{ bad: badCols.includes(c) }"
                     v-tooltip.bottom="'Attribute name'" @change="onRename(ci)" />
              <button class="cc-btn-icon cc-btn-dense" v-tooltip.bottom="'Remove attribute'"
                      @click="removeAttrColumn(attrs, ci)"><i class="pi pi-times" /></button>
            </span>
            <button class="cc-btn cc-btn-ghost" v-tooltip.bottom="'Add an attribute column'"
                    @click="addAttrColumn(attrs)"><i class="pi pi-plus" /></button>
          </div>
          <SelectionTable selection-mode="none" :columns="groupColumns" :rows="groupRows" id-key="key">
            <template #cell-name="{ row: g }">{{ g.name }} <span class="cc-muted">· {{ g.nVideos }}</span></template>
            <template v-for="(c, ci) in attrs.columns" :key="c" #[`cell-a${ci}`]="{ row: g }">
              <input v-model="attrs.values[g.key][c]" class="cc-input-xs im-cell"
                     v-tooltip.bottom="`${c} for this group`" />
            </template>
          </SelectionTable>
          <p v-if="badCols.length" class="cc-muted-warn im-p">Attribute names must be set and not “condition”</p>
        </div>

        <div class="im-tablewrap">
          <SelectionTable class="im-table" selection-mode="multi" :columns="COLUMNS" :rows="rows"
                          v-model:selected="selected" id-key="key"
                          :row-class="r => ({ off: !r.importable || !selected.includes(r.key) })"
                          :row-tooltip="r => r.importable ? 'Include this video' : String(r.reason)">
            <template #cell-name="{ row: v }">
              <div>{{ v.name }}</div>
              <div v-if="!v.importable" class="cc-muted-warn">{{ v.reason }}</div>
            </template>
            <template #cell-shape="{ row: v }"><span class="cc-muted">{{ shapeText(v) }}</span></template>
            <template #cell-sizeBytes="{ row: v }"><span class="cc-muted">{{ formatBytes(v.sizeBytes) }}</span></template>
          </SelectionTable>
        </div>

        <p v-if="mixed.length" class="cc-muted-warn im-p">
          <i class="pi pi-info-circle" /> Mixed pixel sizes: {{ mixed.join(', ') }} µm
        </p>
      </template>
    </div>

    <template #footer>
      <button v-if="done || existed" class="cc-btn cc-btn-primary" @click="$emit('close')">Done</button>
      <template v-else>
        <button class="cc-btn" @click="$emit('close')">Cancel</button>
        <button v-if="manifest" class="cc-btn cc-btn-primary"
                :disabled="importing || summary.n === 0 || badCols.length > 0" @click="confirmImport">
          <i :class="importing ? 'pi pi-spin pi-spinner' : 'pi pi-plus'" />
          Add {{ summary.n }} video{{ summary.n === 1 ? '' : 's' }} ({{ formatBytes(summary.bytes) }})
        </button>
      </template>
    </template>
  </BaseModal>
</template>

<style scoped>
.im { display: flex; flex-direction: column; gap: 0.6rem; color: var(--cc-text); }
.im-lead { margin: 0; }
.im-doneline { margin: 0; font-size: 1rem; }
.im-doneline .pi-check-circle { color: var(--cc-sev-ok); margin-right: 0.35rem; }
.im-input { flex: 1; background: var(--cc-surface-1); }
.im-p { margin: 0; }
.im-exp { font-size: var(--cc-fs-lg); }
.im-cite { margin: 0; }
.im-groups { display: flex; flex-direction: column; gap: 0.3rem; }
.im-attrs { flex-wrap: wrap; gap: 0.4rem; }
.im-attr { gap: 0.1rem; }
.im-attr input { width: 7rem; }
.im-attr input.bad { border-color: var(--cc-sev-warn); }
.im-cell { width: 8rem; }
.im-tablewrap { max-height: 46vh; overflow: auto; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-md); }
.im-table { width: 100%; }
.im-table :deep(.off) { opacity: 0.45; }
</style>
