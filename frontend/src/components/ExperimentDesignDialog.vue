<script setup lang="ts">
// The experiment design dialog, opened from the Design check in the Checks cluster (DesignCheckButton) once a
// set has its images and metadata: set a design up, edit it, or promote an exploratory set. The names and
// original folders suggest one. Three steps — the question,
// the layout (roles + the rules that give each attribute its values), and the grid with what it can
// support. Nothing is blocked: the user confirms, keeps the set exploratory, or cancels. Logic in
// utils/experimentDesign.ts; plan docs/todo/EXPERIMENT_DESIGN_PLAN.md (Decision 5, Target UX).
import { computed, onBeforeUnmount, ref, watch } from 'vue'
import BaseModal from './BaseModal.vue'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'
import InlineNote from './InlineNote.vue'
import SearchSelect from './SearchSelect.vue'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'
import SuggestInput from './SuggestInput.vue'
import DesignQuestionFields from './DesignQuestionFields.vue'
import DesignSketch from './DesignSketch.vue'
import {
  applyRules, attrRole, designAttrRows, designChange, designSketch, setAttrRole, type AttrRole, designDraft, designReservations, gridPivot, runOf, sameNameGroups, suggestDesign,
  type DesignGrid, type DesignImage, type DesignMode, type DesignRule, type ExperimentDesign,
} from '../utils/experimentDesign'
import { previewDesign, saveDesign } from '../utils/experimentDesignApi'
import { createImageAttr, renameImageAttr, setImageAttr } from '../utils/imageAttrApi'
import { buildRegex, defaultSpec } from '../utils/regexBuilder'
import ExtractRuleEditor from './ExtractRuleEditor.vue'
import { debouncedLatest } from '../utils/debouncedLatest'
import { useProjectStore } from '../stores/project'
import { useLogStore } from '../stores/log'

const props = defineProps<{
  projectUid: string
  setUid: string
  setName: string
  /** the images to design over: the set's, newly registered ones included */
  images: DesignImage[]
  /** the set's current design, when it has one */
  existing?: ExperimentDesign | null
  /** open on this step (the Design check opens a confirmed design at its grid) */
  startStep?: 1 | 2 | 3
}>()
const emit = defineEmits<{ (e: 'close'): void; (e: 'saved'): void }>()

const project = useProjectStore()
const log = useLogStore()

// ── Draft ──────────────────────────────────────────────────────────────────────────────────────────
const suggestion = suggestDesign(props.images)
const step = ref<1 | 2 | 3>(props.startStep ?? 1)
const draft = ref<ExperimentDesign>(designDraft(props.existing ?? null, suggestion))
const rules = ref<DesignRule[]>(suggestion.rules.map(r => ({ ...r })))

const MODE_OPTIONS: ChipOption[] = [
  { value: 'designed', label: 'Designed comparison', tip: 'Groups and replicates; plots and checks follow them' },
  { value: 'exploratory', label: 'Exploratory', tip: 'A first look; figures and claims are labelled exploratory' },
]
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
// step 2: a row per attribute in the design (or added/touched here); the rest wait in the "+ Attribute" picker
const attrValues = computed(() => Object.fromEntries(attrNames.value.map(a =>
  [a, new Set(props.images.map(i => valueOf(i, a)).filter(Boolean)).size])))
const added = ref<string[]>([])
const attrRows = computed(() => designAttrRows(attrNames.value, draft.value, attrValues.value, added.value))
function setRole(a: string, role: AttrRole) {
  if (!added.value.includes(a)) added.value.push(a)
  draft.value = setAttrRole(draft.value, a, role)
}
const groupValues = computed(() =>
  [...new Set(props.images.map(i => draft.value.group.map(g => valueOf(i, g)).join(' ')).filter(Boolean))].sort())

// ── The three questions ───────────────────────────────────────────────────────────────────────────
// What is compared (group); what n counts (the replicate) and what names it uniquely (`named within`:
// mouse numbers restart each day, so a mouse is named within its date); what sits inside a replicate
// and is never the n (unit; none = each image). Stored as group / parents = [...within, replicate] / unit.
const replicate = computed(() => draft.value.parents.at(-1) ?? '')
// each attribute's role, as segmented chips; "names the n" needs an n that isn't this attribute
const roleOptions = (a: string): ChipOption[] => [
  { value: 'group', label: 'Compared', tip: 'The groups you compare' },
  { value: 'replicate', label: 'The n', tip: 'One replicate: what n counts' },
  { value: 'namedWithin', label: 'Names the n', tip: 'Part of what tells two replicates apart — Mouse 1 on two days is two mice',
    disabled: !replicate.value || replicate.value === a },
  { value: 'inside', label: 'Inside n', tip: 'Several per replicate, never the n' },
  { value: '', label: '—', tip: 'Not part of the design' },
]

// ── Rules ─────────────────────────────────────────────────────────────────────────────────────────
const existingAttrs = computed(() => [...new Set(props.images.flatMap(i => Object.keys(i.attr ?? {})))].sort())
function addRule() {
  const spec = defaultSpec('field')
  rules.value.push({ attr: '', source: 'name', regex: buildRegex(spec), spec })
}
function addRename() {
  const from = existingAttrs.value.find(a => !renamed.value.has(a)) ?? ''
  rules.value.push({ attr: '', source: 'attr', from })
}
// A rename carries the attribute's role to the new name: the name standing in the roles for each
// rename rule, replaced as the rule changes (the old name while the new one is blank).
const standIn = new WeakMap<object, string>()
function replaceInRoles(from: string, to: string) {
  if (!from || from === to) return
  const re = (xs: string[]) => xs.map(a => (a === from ? to : a))
  draft.value.group = re(draft.value.group); draft.value.unit = re(draft.value.unit); draft.value.parents = re(draft.value.parents)
  added.value = re(added.value)
}
watch(rules, rs => {
  for (const r of rs) {
    if (r.source !== 'attr') continue
    const now = r.attr.trim() || r.from
    const was = standIn.get(r) ?? r.from
    if (now !== was) replaceInRoles(was, now)
    standIn.set(r, now)
  }
}, { deep: true, immediate: true })
function removeRule(i: number) {
  const r = rules.value[i]
  if (r.source === 'attr') replaceInRoles(standIn.get(r) ?? r.attr.trim(), r.from)   // the role goes back
  rules.value.splice(i, 1)
}

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
// the rule values, with a renamed attribute blanked: after the rename it is gone, so it must not
// read as a second attribute beside its new name
const previewAttrs = computed(() => {
  const out: Record<string, Record<string, string>> = {}
  for (const [uid, a] of Object.entries(ruleValues.value)) {
    out[uid] = { ...a }
    for (const r of rules.value) if (r.source === 'attr' && r.attr.trim() && r.from !== r.attr.trim()) out[uid][r.from] = ''
  }
  return out
})
const grid = ref<DesignGrid | null>(null)
const previewError = ref('')
const preview = debouncedLatest<void>(async (_, isCurrent) => {
  const reply = await previewDesign(props.projectUid, props.setUid, draft.value, previewAttrs.value)
  if (!isCurrent()) return
  grid.value = reply.grid
  previewError.value = ''
}, { wait: 250, onError: e => { previewError.value = (e as Error).message } })
watch([draft, rules], () => preview.schedule(), { deep: true, immediate: true })
onBeforeUnmount(() => preview.cancel())

const sameNames = computed(() => sameNameGroups(props.images))
const reservations = computed(() => grid.value ? designReservations(grid.value, draft.value, sameNames.value) : [])
const pivot = computed(() => grid.value ? gridPivot(grid.value, draft.value) : null)
const sketch = computed(() => grid.value ? designSketch(grid.value, draft.value) : null)
const groupLabel = computed(() => draft.value.group.join(' × ') || 'Group')
const gridColumns = computed<SelectionColumn[]>(() => [
  { key: 'group', label: groupLabel.value },
  ...(pivot.value?.colLabels ?? []).map((c, i) => ({ key: `c${i}`, label: c })),
])
const gridRows = computed(() => pivot.value?.rows ?? [])
// the attribute a hovered reservation is about, so the grid can light what shows it
const pointAt = ref('')

// ── Save ──────────────────────────────────────────────────────────────────────────────────────────
const saving = ref(false)

// Write each rule's attribute on these images (create the column first when it is new); a rename goes
// through the rename route, which moves the values and takes the old name off every image.
async function writeAttributes() {
  const uids = props.images.map(i => i.uid)
  for (const r of rules.value) {
    const name = r.attr.trim()
    if (!name) continue
    if (r.source === 'attr') {
      if (!r.from || r.from === name) continue
      const stored = await renameImageAttr(props.projectUid, uids, r.from, name, props.setUid)
      project.addAttrKey(props.setUid, name)
      project.setAttrValues(name, stored)
      project.removeAttrKey(props.setUid, r.from)
      continue
    }
    if (!existingAttrs.value.includes(name)) {
      await createImageAttr(props.projectUid, uids, name)
      project.addAttrKey(props.setUid, name)
    }
    const values = Object.fromEntries(uids.map(u => [u, ruleValues.value[u]?.[name] ?? '']))
    project.setAttrValues(name, await setImageAttr(props.projectUid, name, values))
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
    await saveDesign(props.projectUid, props.setUid, design, designChange(props.existing ?? null, design))
    log.info(`Design ${how === 'exploratory' ? 'kept as exploratory' : 'confirmed'} for "${props.setName}".`, { source: 'experimentDesign' })
    emit('saved')
    emit('close')
  } catch (e) {
    log.error(`Design not saved: ${(e as Error).message}`, { source: 'experimentDesign' })
  } finally {
    saving.value = false
  }
}

const title = computed(() => `${props.existing?.mode === 'designed' ? 'Design' : 'Set up a design'} · ${props.setName}`)
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
      <DesignQuestionFields v-model:question="draft.question" v-model:expect="draft.expect"
                            v-model:expect-in="draft.expectIn" :group-values="groupValues" :group-label="groupLabel" />
      <div class="xd-field">
        <span class="cc-eyebrow cc-fs-2xs">How will this set be used?</span>
        <ChipSelect v-model="mode" :options="MODE_OPTIONS" variant="segmented" aria-label="Design mode" />
      </div>
    </div>

    <!-- 2 · the layout -->
    <div v-else-if="step === 2" class="xd-body">
      <!-- one row per attribute with its role, and a sketch of what the roles mean (live, from these images) -->
      <div class="xd-layout">
        <div class="xd-attrs">
          <div v-for="a in attrRows.rows" :key="a" class="xd-arow">
            <span class="xd-aname" :title="a">{{ a }}</span>
            <ChipSelect :model-value="attrRole(draft, a)" :options="roleOptions(a)" variant="segmented"
                        :aria-label="`Role of ${a}`" @update:model-value="v => setRole(a, (v ?? '') as AttrRole)" />
          </div>
          <SearchSelect v-if="attrRows.picker.length" class="xd-add" model-value="" :groups="attrRows.picker"
                        placeholder="+ Attribute" aria-label="Add an attribute to the design"
                        v-tooltip.bottom="'Give another attribute a role'" @update:model-value="a => added.push(a)" />
        </div>
        <DesignSketch class="xd-sketch" :sketch="sketch" />
      </div>

      <div class="xd-table">
        <SelectionTable :columns="columns" :rows="rows" id-key="uid" selection-mode="none" density="compact" />
      </div>

      <div class="cc-panel-section">
        <div class="cc-panel-heading">
          <span class="cc-eyebrow cc-fs-2xs">Rules</span>
          <span class="cc-panel-heading-actions">
            <button class="cc-btn cc-btn-ghost" v-tooltip.bottom="'Read an attribute off the name or path'" @click="addRule">
              <i class="pi pi-plus" /> From the file
            </button>
            <button class="cc-btn cc-btn-ghost" :disabled="!existingAttrs.length"
                    v-tooltip.bottom="'Give an attribute a new name'" @click="addRename">
              <i class="pi pi-pencil" /> Rename
            </button>
          </span>
        </div>
        <div v-for="(r, i) in rules" :key="i" class="xd-rule">
          <span class="xd-attr"><SuggestInput v-model="r.attr" :options="existingAttrs" placeholder="Attribute" tip="Attribute name" /></span>
          <span class="cc-muted">←</span>
          <template v-if="r.source === 'attr'">
            <span class="cc-muted">renamed from</span>
            <select v-model="r.from" v-tooltip.bottom="'Attribute to rename'">
              <option v-for="a in existingAttrs" :key="a" :value="a">{{ a }}</option>
            </select>
            <span class="xd-spacer" />
          </template>
          <ExtractRuleEditor v-else class="xd-extract" compact :regex="r.regex" :source="r.source" :spec="r.spec"
                             :sample="images[0]"
                             @update:regex="v => { r.regex = v }"
                             @update:source="v => { r.source = v }"
                             @update:spec="v => { r.spec = v }" />
          <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.bottom="'Remove rule'" @click="removeRule(i)">
            <i class="pi pi-times" />
          </button>
        </div>
      </div>
    </div>

    <!-- 3 · the grid and what it can support -->
    <div v-else class="xd-body">
      <div v-if="previewError" class="cc-empty">{{ previewError }}</div>
      <template v-else-if="pivot && grid">
        <span class="cc-muted">{{ pivot.caption }}</span>
        <SelectionTable :columns="gridColumns" :rows="gridRows" id-key="group" selection-mode="none" density="compact">
          <template #cell-group="{ row }">
            {{ row.group }}<div class="cc-muted cc-fs-2xs">{{ row.summary }}</div>
          </template>
          <template v-for="(_, ci) in pivot.cols" :key="ci" #[`cell-c${ci}`]="{ row }">
            <!-- an empty cell is the evidence for a confound with the columns: lit while its note is hovered -->
            <span v-if="!row.cells[ci].length" :class="['cc-muted', 'xd-empty', { lit: pointAt && pointAt === pivot.colAttr }]">—</span>
            <div v-for="c in row.cells[ci]" :key="c.label" class="xd-par">
              {{ c.label }}<span v-if="c.label" class="cc-muted"> · </span><span class="cc-muted">{{ c.count }}</span>
            </div>
          </template>
        </SelectionTable>
        <div class="xd-notes">
          <div v-for="n in reservations" :key="n.short" @mouseenter="pointAt = n.about ?? ''" @mouseleave="pointAt = ''">
            <InlineNote :short="n.short" :detail="n.long" :severity="n.level === 'warn' ? 'warn' : undefined" />
          </div>
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
.xd-layout { display: grid; grid-template-columns: minmax(0, auto) minmax(180px, 1fr); gap: 12px; align-items: start; }
.xd-attrs { display: flex; flex-direction: column; gap: 4px; min-width: 0; }
.xd-arow { display: flex; align-items: center; gap: 8px; }
.xd-add { align-self: flex-start; }
.xd-aname { width: 5.5rem; flex: none; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font-size: var(--cc-fs-sm); }
.xd-table { max-height: 170px; overflow: auto; }
@media (max-width: 640px) { .xd-layout { grid-template-columns: 1fr; } }
.xd-rule { display: flex; gap: 6px; align-items: flex-start; margin-top: 6px; }
.xd-attr { width: 120px; }
.xd-extract { flex: 1; min-width: 0; }
.xd-par { white-space: nowrap; }
.xd-empty { display: inline-block; padding: 0 6px; border-radius: var(--cc-radius-sm); }
.xd-empty.lit { background: var(--cc-tint-amber); color: var(--cc-ink-amber); }
.xd-notes { display: flex; flex-direction: column; gap: 6px; }
.xd-spacer { flex: 1; }
</style>
