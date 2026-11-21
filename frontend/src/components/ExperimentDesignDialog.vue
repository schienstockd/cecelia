<script setup lang="ts">
// The experiment design dialog: opened after files are registered into a set with no design, before
// they are imported, so the names and original folders can suggest one. Three steps — the question,
// the layout (roles + the rules that give each attribute its values), and the grid with what it can
// support. Nothing is blocked: the user confirms, keeps the set exploratory, or cancels. Logic in
// utils/experimentDesign.ts; plan docs/todo/EXPERIMENT_DESIGN_PLAN.md (Decision 5, Target UX).
import { computed, onBeforeUnmount, ref, watch } from 'vue'
import BaseModal from './BaseModal.vue'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'
import InlineNote from './InlineNote.vue'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'
import SuggestInput from './SuggestInput.vue'
import {
  applyRules, designReservations, emptyDesign, gridPivot, runOf, sameNameGroups, suggestDesign,
  type DesignExpect, type DesignGrid, type DesignImage, type DesignMode, type DesignRule, type ExperimentDesign,
} from '../utils/experimentDesign'
import { previewDesign, saveDesign } from '../utils/experimentDesignApi'
import { createImageAttr, deleteImageAttr, setImageAttr } from '../utils/imageAttrApi'
import { buildFieldRegex } from '../utils/regexBuilder'
import { debouncedLatest } from '../utils/debouncedLatest'
import { useProjectStore } from '../stores/project'
import { useLogStore } from '../stores/log'

const props = defineProps<{
  projectUid: string
  setUid: string
  setName: string
  /** the images to design over: the set's, newly registered ones included */
  images: DesignImage[]
  /** how many of them were just added (the title) */
  nAdded: number
}>()
const emit = defineEmits<{ (e: 'close'): void; (e: 'saved'): void }>()

const project = useProjectStore()
const log = useLogStore()

// ── Draft ──────────────────────────────────────────────────────────────────────────────────────────
const suggestion = suggestDesign(props.images)
const step = ref<1 | 2 | 3>(1)
const draft = ref<ExperimentDesign>({
  ...emptyDesign(), group: suggestion.group, unit: suggestion.unit, parents: suggestion.parents,
})
const rules = ref<DesignRule[]>(suggestion.rules.map(r => ({ ...r })))

const EXPECT_OPTIONS: ChipOption[] = [
  { value: 'unsure', label: 'Not sure' },
  { value: 'same', label: 'No difference' },
  { value: 'higher', label: 'Higher in…' },
  { value: 'lower', label: 'Lower in…' },
]
const MODE_OPTIONS: ChipOption[] = [
  { value: 'designed', label: 'Designed comparison', tip: 'Groups and replicates; plots and checks follow them' },
  { value: 'exploratory', label: 'Exploratory', tip: 'A first look; figures and claims are labelled exploratory' },
]
const expect = computed({
  get: () => draft.value.expect,
  set: (v: string | string[] | null) => { draft.value.expect = (v as DesignExpect) ?? 'unsure' },
})
const mode = computed({
  get: () => draft.value.mode,
  set: (v: string | string[] | null) => { draft.value.mode = (v as DesignMode) ?? 'designed' },
})

// ── Values the rules give, and the attributes there are to choose from ────────────────────────────
const ruleValues = computed(() => applyRules(props.images, rules.value))
const valueOf = (img: DesignImage, attr: string) => ruleValues.value[img.uid]?.[attr] ?? img.attr?.[attr] ?? ''
const renamed = computed(() => new Set(rules.value.flatMap(r => (r.source === 'attr' ? [r.from] : []))))
const attrNames = computed(() => [...new Set([
  ...props.images.flatMap(i => Object.keys(i.attr ?? {})).filter(a => !renamed.value.has(a)),
  ...rules.value.map(r => r.attr.trim()).filter(Boolean),
])].sort())
const attrOptions = computed<ChipOption[]>(() => attrNames.value.map(a => ({ value: a })))
const groupValues = computed(() =>
  [...new Set(props.images.map(i => draft.value.group.map(g => valueOf(i, g)).join(' ')).filter(Boolean))].sort())

function setRole(role: 'group' | 'unit' | 'parents', v: string | string[] | null) {
  const picked = Array.isArray(v) ? v : v ? [v] : []
  draft.value[role] = picked
  // an attribute plays one role: picking it here takes it from the others
  for (const other of (['group', 'unit', 'parents'] as const).filter(r => r !== role))
    draft.value[other] = draft.value[other].filter(a => !picked.includes(a))
}

// ── Rules ─────────────────────────────────────────────────────────────────────────────────────────
type RuleSource = DesignRule['source']
const SOURCE_LABEL: Record<RuleSource, string> = { name: 'File name', path: 'Original path', attr: 'Rename attribute' }
function setRuleSource(i: number, source: RuleSource) {
  const r = rules.value[i]
  rules.value[i] = source === 'attr'
    ? { attr: r.attr, source, from: attrNames.value[0] ?? '' }
    : { attr: r.attr, source, regex: r.source === 'attr' ? '' : r.regex }
}
function addRule() {
  rules.value.push({ attr: '', source: 'path', regex: buildFieldRegex('/\\', 'secondLast', false) })
}
const existingAttrs = computed(() => [...new Set(props.images.flatMap(i => Object.keys(i.attr ?? {})))].sort())

// ── The images → values table ─────────────────────────────────────────────────────────────────────
const tableAttrs = computed(() => [...new Set([...draft.value.group, ...draft.value.parents, ...draft.value.unit])])
const columns = computed<SelectionColumn[]>(() => [
  { key: 'name', label: 'File name', ellipsis: true, sortable: true },
  { key: 'run', label: 'Run', sortable: true },
  ...tableAttrs.value.map(a => ({ key: `a:${a}`, label: a, sortable: true })),
])
const rows = computed(() => props.images.map(img => ({
  uid: img.uid, name: img.name, run: runOf(img.name),
  ...Object.fromEntries(tableAttrs.value.map(a => [`a:${a}`, valueOf(img, a)])),
})))

// ── The grid, from the preview route ──────────────────────────────────────────────────────────────
const grid = ref<DesignGrid | null>(null)
const previewError = ref('')
const preview = debouncedLatest<void>(async (_, isCurrent) => {
  const reply = await previewDesign(props.projectUid, props.setUid, draft.value, ruleValues.value)
  if (!isCurrent()) return
  grid.value = reply.grid
  previewError.value = ''
}, { wait: 250, onError: e => { previewError.value = (e as Error).message } })
watch([draft, rules], () => preview.schedule(), { deep: true, immediate: true })
onBeforeUnmount(() => preview.cancel())

const sameNames = computed(() => sameNameGroups(props.images))
const reservations = computed(() => grid.value ? designReservations(grid.value, draft.value, sameNames.value) : [])
const pivot = computed(() => grid.value ? gridPivot(grid.value, draft.value) : null)
const groupLabel = computed(() => draft.value.group.join(' × ') || 'Group')
const gridColumns = computed<SelectionColumn[]>(() => [
  { key: 'group', label: groupLabel.value },
  ...(pivot.value?.cols ?? []).map((c, i) => ({ key: `c${i}`, label: c })),
])
const gridRows = computed(() => pivot.value?.rows ?? [])
const parentLabel = computed(() => draft.value.parents.join(' × '))
const unitNoun = computed(() => (draft.value.unit.join(' × ') || 'images').toLowerCase())

// ── Save ──────────────────────────────────────────────────────────────────────────────────────────
const saving = ref(false)

// Write each rule's attribute on these images (create the column first when it is new); a rename then
// drops the attribute it copied.
async function writeAttributes() {
  const uids = props.images.map(i => i.uid)
  for (const r of rules.value) {
    const name = r.attr.trim()
    if (!name) continue
    if (!existingAttrs.value.includes(name)) {
      await createImageAttr(props.projectUid, uids, name)
      project.addAttrKey(props.setUid, name)
    }
    const values = Object.fromEntries(uids.map(u => [u, ruleValues.value[u]?.[name] ?? '']))
    project.setAttrValues(name, await setImageAttr(props.projectUid, name, values))
    if (r.source === 'attr' && r.from && r.from !== name) {
      await deleteImageAttr(props.projectUid, uids, r.from)
      project.removeAttrKey(props.setUid, r.from)
    }
  }
}

async function save(how: 'confirmed' | 'exploratory') {
  saving.value = true
  try {
    const design: ExperimentDesign = { ...draft.value, status: 'confirmed', mode: how === 'exploratory' ? 'exploratory' : 'designed' }
    if (design.mode === 'exploratory' && step.value === 1) {
      // declared exploratory up front: the question only, no roles to confirm
      design.group = []; design.unit = []; design.parents = []
    } else {
      await writeAttributes()
    }
    if (!['higher', 'lower'].includes(design.expect)) design.expectIn = ''
    await saveDesign(props.projectUid, props.setUid, design, how === 'exploratory' ? 'exploratory' : 'confirmed')
    log.info(`Design ${how === 'exploratory' ? 'kept as exploratory' : 'confirmed'} for "${props.setName}".`, { source: 'experimentDesign' })
    emit('saved')
    emit('close')
  } catch (e) {
    log.error(`Design not saved: ${(e as Error).message}`, { source: 'experimentDesign' })
  } finally {
    saving.value = false
  }
}

const title = computed(() => `${props.nAdded} file${props.nAdded === 1 ? '' : 's'} added · ${props.setName}`)
const STEPS = ['Question', 'Layout', 'Confirm']
const nextLabel = computed(() => (STEPS[step.value] ?? '').toLowerCase())
</script>

<template>
  <BaseModal :title="title" icon="pi-bullseye" width="880px" @close="emit('close')">
    <template #toolbar>
      <div class="xd-steps">
        <span v-for="(s, i) in STEPS" :key="s" :class="['xd-step', { 'cc-muted': step < i + 1, on: step === i + 1, done: step > i + 1 }]">
          <i v-if="step > i + 1" class="pi pi-check" /> {{ i + 1 }} {{ s }}
        </span>
      </div>
    </template>

    <!-- 1 · the question -->
    <div v-if="step === 1" class="xd-body">
      <label class="xd-field">
        <span class="cc-eyebrow cc-fs-2xs">What do you want to find out?</span>
        <textarea v-model="draft.question" rows="2" placeholder="One sentence" v-tooltip.bottom="'The question this set answers'" />
      </label>
      <div class="xd-field">
        <span class="cc-eyebrow cc-fs-2xs">What do you expect?</span>
        <div class="cc-row">
          <ChipSelect v-model="expect" :options="EXPECT_OPTIONS" variant="segmented" aria-label="Expectation"
                      v-tooltip.bottom="'What you expect to see'" />
          <select v-if="draft.expect === 'higher' || draft.expect === 'lower'" v-model="draft.expectIn"
                  v-tooltip.bottom="'The group you expect it in'">
            <option value="" disabled>{{ groupLabel }}…</option>
            <option v-for="v in groupValues" :key="v" :value="v">{{ v }}</option>
          </select>
        </div>
      </div>
      <div class="xd-field">
        <span class="cc-eyebrow cc-fs-2xs">How will this set be used?</span>
        <ChipSelect v-model="mode" :options="MODE_OPTIONS" variant="segmented" aria-label="Design mode" />
      </div>
    </div>

    <!-- 2 · the layout -->
    <div v-else-if="step === 2" class="xd-body">
      <div class="xd-roles">
        <div class="xd-role">
          <span class="cc-eyebrow cc-fs-2xs" v-tooltip.top="'What is compared'">Group</span>
          <ChipSelect :model-value="draft.group" :options="attrOptions" multiple
                      @update:model-value="v => setRole('group', v)" />
        </div>
        <div class="xd-role">
          <span class="cc-eyebrow cc-fs-2xs" v-tooltip.top="'One replicate; none = each image'">Unit</span>
          <ChipSelect :model-value="draft.unit" :options="attrOptions" multiple
                      @update:model-value="v => setRole('unit', v)" />
        </div>
        <div class="xd-role">
          <span class="cc-eyebrow cc-fs-2xs" v-tooltip.top="'The units sit inside these, outermost first'">Nested in</span>
          <ChipSelect :model-value="draft.parents" :options="attrOptions" multiple reorderable
                      @update:model-value="v => setRole('parents', v)" />
        </div>
      </div>

      <div class="xd-table">
        <SelectionTable :columns="columns" :rows="rows" id-key="uid" selection-mode="none" density="compact" />
      </div>

      <div class="cc-panel-section">
        <div class="cc-panel-heading">
          <span class="cc-eyebrow cc-fs-2xs">Rules</span>
          <span class="cc-panel-heading-actions">
            <button class="cc-btn cc-btn-ghost" v-tooltip.bottom="'Add an attribute from the name or path'" @click="addRule">
              <i class="pi pi-plus" /> Add rule
            </button>
          </span>
        </div>
        <div v-for="(r, i) in rules" :key="i" class="xd-rule">
          <span class="xd-attr"><SuggestInput v-model="r.attr" :options="existingAttrs" placeholder="Attribute" tip="Attribute name" /></span>
          <span>←</span>
          <select :value="r.source" v-tooltip.bottom="'Where the value comes from'" @change="setRuleSource(i, ($event.target as HTMLSelectElement).value as RuleSource)">
            <option v-for="(label, s) in SOURCE_LABEL" :key="s" :value="s">{{ label }}</option>
          </select>
          <select v-if="r.source === 'attr'" v-model="r.from" v-tooltip.bottom="'Attribute to rename'">
            <option v-for="a in existingAttrs" :key="a" :value="a">{{ a }}</option>
          </select>
          <input v-else v-model="r.regex" class="xd-regex" placeholder="Regex" v-tooltip.bottom="'First group, else the whole match'" />
          <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.bottom="'Remove rule'" @click="rules.splice(i, 1)">
            <i class="pi pi-times" />
          </button>
        </div>
      </div>
    </div>

    <!-- 3 · the grid and what it can support -->
    <div v-else class="xd-body">
      <div v-if="previewError" class="cc-empty">{{ previewError }}</div>
      <template v-else-if="pivot && grid">
        <span class="cc-eyebrow cc-fs-2xs">{{ groupLabel }}{{ parentLabel ? ` × ${parentLabel}` : '' }} ({{ unitNoun }})</span>
        <SelectionTable :columns="gridColumns" :rows="gridRows" id-key="group" selection-mode="none" density="compact">
          <template #cell-group="{ row }">
            {{ row.group }}<div class="cc-muted cc-fs-2xs">{{ row.summary }}</div>
          </template>
          <template v-for="(_, ci) in pivot.cols" :key="ci" #[`cell-c${ci}`]="{ row }">
            <span v-if="!row.cells[ci].length" class="cc-muted">—</span>
            <span v-for="c in row.cells[ci]" :key="c.label" class="xd-par">{{ c.label || '·' }} <span class="cc-muted">({{ c.n }})</span></span>
          </template>
        </SelectionTable>
        <div class="xd-notes">
          <InlineNote v-for="n in reservations" :key="n.short" :short="n.short" :detail="n.long"
                      :severity="n.level === 'warn' ? 'warn' : undefined" />
        </div>
      </template>
    </div>

    <template #footer>
      <button class="cc-btn cc-btn-ghost" :disabled="saving" @click="step === 1 ? emit('close') : step--">
        {{ step === 1 ? 'Cancel' : 'Back' }}
      </button>
      <span class="xd-spacer" />
      <template v-if="step === 1 && draft.mode === 'exploratory'">
        <button class="cc-btn cc-btn-primary" :disabled="saving" @click="save('exploratory')">Keep as exploratory</button>
      </template>
      <template v-else-if="step < 3">
        <button class="cc-btn cc-btn-primary" @click="step++">Next: {{ nextLabel }}</button>
      </template>
      <template v-else>
        <button class="cc-btn" :disabled="saving" @click="save('exploratory')">Keep as exploratory</button>
        <button class="cc-btn cc-btn-primary" :disabled="saving || !grid" @click="save('confirmed')">Confirm design</button>
      </template>
    </template>
  </BaseModal>
</template>

<style scoped>
.xd-steps { display: flex; gap: 16px; padding: 6px 16px; border-bottom: 1px solid var(--cc-border); }
.xd-step.on { font-weight: 600; }
.xd-step.done { color: var(--cc-sev-ok); }
.xd-body { display: flex; flex-direction: column; gap: 14px; }
.xd-field { display: flex; flex-direction: column; gap: 6px; }
.xd-field textarea { width: 100%; resize: vertical; }
.xd-roles { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 12px; }
.xd-role { display: flex; flex-direction: column; gap: 6px; min-width: 0; }
.xd-table { max-height: 260px; overflow: auto; }
.xd-rule { display: flex; gap: 6px; align-items: center; margin-top: 6px; }
.xd-attr { width: 120px; }
.xd-regex { flex: 1; min-width: 0; font-family: var(--cc-mono); }
.xd-par { margin-right: 10px; white-space: nowrap; }
.xd-notes { display: flex; flex-direction: column; gap: 6px; }
.xd-spacer { flex: 1; }
</style>
