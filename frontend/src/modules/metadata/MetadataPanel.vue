<script setup lang="ts">
import { ref, computed } from 'vue'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useLogStore } from '../../stores/log'
import { metadataWarning } from '../../lib/imageMetadataWarnings'
import { extractWith, regexSampleFor, type BuilderSpec, type RegexSource } from '../../utils/regexBuilder'
import ExtractRuleEditor from '../../components/ExtractRuleEditor.vue'
import { parseChannelNameList, channelNamesAsText, referenceCandidates,
         splitByChannelCount, skippedChannelCountMsg } from '../../utils/channelNames'
import PhysicalSizeDialog from '../../components/PhysicalSizeDialog.vue'
import ConfirmDeleteButton from '../../components/ConfirmDeleteButton.vue'
import SuggestInput from '../../components/SuggestInput.vue'
import { createImageAttr, deleteImageAttr, renameImageAttr, setImageAttr } from '../../utils/imageAttrApi'
import { designSaves } from '../../utils/experimentDesignApi'

// resizable sidebar width (persisted) — same behaviour as the TaskRunner functions panel
// This panel does NOT own its width — `CollapsiblePanel` (its host, via ModuleLayout's `#right`) does.
// It had its own `usePanelResize` + drag handle, which meant two widths and two stacked handles on one
// edge: dragging the host widened the host while this stayed pinned at its stored 280px, so the
// content shifted instead of reflowing. Same fix as TaskRunner.

const props = defineProps<{
  setUid: string | undefined
  selectedUids: string[]
}>()

const project     = useProjectStore()
const projectMeta = useProjectMetaStore()
const log         = useLogStore()

// ── Computed helpers ───────────────────────────────────────────────────────────

const setImages = computed(() =>
  project.sets.find(s => s.uid === props.setUid)?.images ?? []
)

// If nothing is selected, operate on all images in the set
const targetUids = computed(() =>
  props.selectedUids.length > 0
    ? props.selectedUids
    : setImages.value.map(i => i.uid)
)

// Union of all attr keys across all images in the set
const attrNames = computed(() => {
  const keys = new Set<string>()
  for (const img of setImages.value)
    for (const k of Object.keys(img.attr ?? {})) keys.add(k)
  return [...keys].sort()
})


// Values already in use for the SELECTED attribute, across the set.
//
// Assigning one is overwhelmingly a repeat — "MERTK" onto four images — and until now there was
// nothing to repeat FROM: the field was bare text. That matters more than convenience, because an
// attribute value is a grouping axis, and a typo does not error. `MERTk` silently becomes its own
// cohort group in every cross-image comparison, which surfaces much later as a group of one.
const valuesInUse = computed(() => {
  const a = selectedAttr.value
  if (!a) return []
  const vals = new Set<string>()
  for (const img of setImages.value) {
    const v = img.attr?.[a]
    if (typeof v === 'string' && v.trim() !== '') vals.add(v)
  }
  return [...vals].sort()
})

// ── Attr create/delete ─────────────────────────────────────────────────────────

const newAttrName   = ref('')
const selectedAttr  = ref('')

async function createAttr() {
  const name = newAttrName.value.trim()
  if (!name) return
  if (attrNames.value.includes(name)) {
    log.warn(`Attribute "${name}" already exists.`, { source: 'metadata' }); return
  }
  const projectUid = projectMeta.current?.uid
  if (!projectUid || !props.setUid) return

  try { await createImageAttr(projectUid, setImages.value.map(i => i.uid), name) }
  catch (e) { log.error((e as Error).message, { source: 'metadata' }); return }

  project.addAttrKey(props.setUid, name)
  selectedAttr.value = name
  newAttrName.value = ''
  log.info(`Created attribute "${name}".`, { source: 'metadata' })
}

async function deleteAttr() {
  const name = selectedAttr.value
  if (!name) return
  const projectUid = projectMeta.current?.uid
  if (!projectUid || !props.setUid) return

  try { await deleteImageAttr(projectUid, setImages.value.map(i => i.uid), name) }
  catch (e) { log.error((e as Error).message, { source: 'metadata' }); return }

  project.removeAttrKey(props.setUid, name)
  selectedAttr.value = ''
  log.info(`Deleted attribute "${name}".`, { source: 'metadata' })
}

// ── Rename ───────────────────────────────────────────────────────────────────────
// The value moves to the new name on every image; the set's experiment design follows (the route).
const renaming = ref(false)
const renameTo = ref('')
function toggleRename() {
  renaming.value = !renaming.value
  renameTo.value = renaming.value ? selectedAttr.value : ''
}
async function renameAttr() {
  const from = selectedAttr.value
  const to = renameTo.value.trim()
  const projectUid = projectMeta.current?.uid
  if (!from || !to || to === from || !projectUid || !props.setUid) return
  let stored: Record<string, string>
  try { stored = await renameImageAttr(projectUid, setImages.value.map(i => i.uid), from, to, props.setUid) }
  catch (e) { log.error((e as Error).message, { source: 'metadata' }); return }
  project.addAttrKey(props.setUid, to)
  project.setAttrValues(to, stored)
  project.removeAttrKey(props.setUid, from)
  selectedAttr.value = to
  renaming.value = false
  designSaves.value++   // the set's design may have followed the rename
  log.info(`Renamed attribute "${from}" to "${to}".`, { source: 'metadata' })
}

// ── Assign single value ────────────────────────────────────────────────────────

const singleValue = ref('')

async function assignSingleValue() {
  if (!selectedAttr.value || !singleValue.value) return
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return

  const values: Record<string, string> = {}
  for (const uid of targetUids.value) values[uid] = singleValue.value

  let stored: Record<string, string>
  try { stored = await setImageAttr(projectUid, selectedAttr.value, values) }
  catch (e) { log.error((e as Error).message, { source: 'metadata' }); return }

  project.setAttrValues(selectedAttr.value, stored)
  log.info(`Assigned "${singleValue.value}" to ${targetUids.value.length} image(s).`, { source: 'metadata' })
}

// ── Extract via regex ──────────────────────────────────────────────────────────

const regexpValue  = ref('')
const regexpSource = ref<RegexSource>('name')

// The regex is built (or typed) in the shared editor (components/ExtractRuleEditor.vue), which keeps
// the builder's settings in `regexSpec` and writes the pattern into `regexpValue`.
const regexSpec = ref<BuilderSpec | null>(null)
// the image the editor previews on: the first one Apply would target
const regexSampleImg = computed(() => {
  const uid = targetUids.value[0] ?? setImages.value[0]?.uid
  return setImages.value.find(i => i.uid === uid) ?? null
})

async function assignRegexp() {
  if (!selectedAttr.value || !regexpValue.value) return
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return

  try { new RegExp(regexpValue.value) }
  catch { log.warn('Invalid regular expression.', { source: 'metadata' }); return }

  const values: Record<string, string> = {}
  for (const uid of targetUids.value) {
    const img  = setImages.value.find(i => i.uid === uid)
    if (!img) continue
    // first capture group wins (the builder always makes one), else the whole match
    values[uid] = extractWith(regexpValue.value, regexSampleFor(img, regexpSource.value))
  }

  let stored: Record<string, string>
  try { stored = await setImageAttr(projectUid, selectedAttr.value, values) }
  catch (e) { log.error((e as Error).message, { source: 'metadata' }); return }

  project.setAttrValues(selectedAttr.value, stored)
  log.info(`Applied regex to ${Object.keys(values).length} image(s).`, { source: 'metadata' })
}

// ── Group sequences ────────────────────────────────────────────────────────────

async function assignGroupSequences() {
  const projectUid = projectMeta.current?.uid
  if (!projectUid || !props.setUid) return
  const allAttrs = attrNames.value.filter(n => n !== 'GroupSeq')
  if (allAttrs.length === 0) {
    log.warn('Create at least one attribute before assigning group sequences.', { source: 'metadata' })
    return
  }

  // Group images by all attr values, assign 1-based sequence within each group
  type GroupKey = string
  const groups = new Map<GroupKey, string[]>()
  for (const img of setImages.value) {
    const key = allAttrs.map(a => img.attr?.[a] ?? '').join('|')
    const g = groups.get(key) ?? []
    g.push(img.uid)
    groups.set(key, g)
  }
  const values: Record<string, string> = {}
  for (const uids of groups.values())
    uids.forEach((uid, i) => { values[uid] = String(i + 1) })

  // Ensure GroupSeq key exists
  try { await createImageAttr(projectUid, setImages.value.map(i => i.uid), 'GroupSeq') }
  catch (e) { log.error((e as Error).message, { source: 'metadata' }); return }
  project.addAttrKey(props.setUid, 'GroupSeq')

  let stored: Record<string, string>
  try { stored = await setImageAttr(projectUid, 'GroupSeq', values) }
  catch (e) { log.error((e as Error).message, { source: 'metadata' }); return }

  project.setAttrValues('GroupSeq', stored)
  log.info('Assigned group sequences.', { source: 'metadata' })
}

// ── Channel names ──────────────────────────────────────────────────────────────

const channelNameList = ref('')
const channelRefUid   = ref('')

const channelNames = computed(() => parseChannelNameList(channelNameList.value))
// Only images that already carry names can serve as a reference (utils/channelNames).
const channelRefs = computed(() => referenceCandidates(setImages.value))

// Picking a reference FILLS the list field rather than assigning straight away — same contract as the
// regex builder above: the control writes the real value into the visible input, so what gets written
// is on screen and editable before Assign. One field, one apply path.
function copyChannelsFromReference() {
  const src = setImages.value.find(i => i.uid === channelRefUid.value)
  if (src) channelNameList.value = channelNamesAsText(src)
}

// Targets whose KNOWN channel count contradicts the list are left out — see splitByChannelCount for
// why writing a name for a channel that isn't there is worse than skipping the image. Surfaced before
// Assign (the hint below) as well as after (the log line), so a mismatch isn't a surprise.
const channelSplit = computed(() =>
  splitByChannelCount(setImages.value.filter(i => targetUids.value.includes(i.uid)),
                      channelNames.value.length))

async function assignChannelNames() {
  const names = channelNames.value
  if (!names.length) return
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  const { apply, skipped } = channelSplit.value
  const uids = apply.map(i => i.uid)
  if (!uids.length) {
    log.warn(skippedChannelCountMsg(skipped, names.length), { source: 'metadata' }); return
  }

  const res = await fetch('/api/images/channelnames', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ projectUid, imageUids: uids, channelNames: names }),
  })
  const body = await res.json().catch(() => ({})) as { error?: string; values?: Record<string, string> }
  if (!res.ok) { log.error(body.error ?? `HTTP ${res.status}`, { source: 'metadata' }); return }

  for (const uid of uids)
    project.updateImageMeta(uid, { channelNames: names })

  log.info(`Set ${names.length} channel name(s) on ${uids.length} image(s).`, { source: 'metadata' })
  if (skipped.length) log.warn(skippedChannelCountMsg(skipped, names.length), { source: 'metadata' })
}


const attrDisabled = computed(() => !selectedAttr.value)

// ── Physical size & timing — opens the shared modal (PhysicalSizeDialog.vue) rather than
// cramming a full editor into this narrow sidebar; see feedback after the first version shipped.

const showPhysDialog = ref(false)
const physFocusUid = computed(() => props.selectedUids[0] ?? setImages.value[0]?.uid ?? null)
const flaggedCount = computed(() => setImages.value.filter(i => metadataWarning(i)).length)
</script>

<template>
  <aside class="metadata-panel-wrap">
    <div class="metadata-panel cc-side-panel">

    <!-- ── Physical size & timing ───────────────────────────────── -->
    <section class="cc-panel-section md-section">
      <h3 class="cc-panel-heading cc-eyebrow cc-fs-2xs">Physical size &amp; timing</h3>
      <!-- ONE tip on the button; the count folds into it. A second tip on the badge inside fired on
           top of the button's own (docs/UI.md → nested tooltips). -->
      <button class="cc-btn cc-btn-ghost" data-guide="metadata.physEditor" :disabled="!physFocusUid" @click="showPhysDialog = true"
        v-tooltip.bottom="flaggedCount
          ? `Voxel size and frame interval — ${flaggedCount} image(s) flagged`
          : 'View or fix voxel size and frame interval for the selected image(s)'">
        <i class="pi pi-gauge" /> Open editor
        <span v-if="flaggedCount" class="warn-count">{{ flaggedCount }}</span>
      </button>
    </section>
    <PhysicalSizeDialog v-if="showPhysDialog && setUid && physFocusUid"
      :set-uid="setUid" :focus-uid="physFocusUid" :selected-uids="selectedUids"
      @close="showPhysDialog = false" />

    <!-- ── Attribute management ─────────────────────────────────── -->
    <section class="cc-panel-section md-section" data-guide="metadata.attributes">
      <h3 class="cc-panel-heading cc-eyebrow cc-fs-2xs">Attributes</h3>

      <!-- create -->
      <div class="field-row">
        <input class="field-input flex1" v-model="newAttrName" placeholder="New attribute name…"
          @keydown.enter="createAttr"
          v-tooltip.bottom="'Name for the new metadata column — e.g. condition, timepoint'" />
        <button class="cc-btn cc-btn-primary" @click="createAttr" :disabled="!newAttrName.trim()"
          v-tooltip.bottom="'Add this attribute key to all images in the set'">
          Create
        </button>
      </div>

      <!-- select + delete -->
      <div class="field-row">
        <select class="field-input flex1" v-model="selectedAttr"
          :disabled="attrNames.length === 0"
          v-tooltip.bottom="'Select which attribute to assign values to'">
          <option value="">— select attribute —</option>
          <option v-for="n in attrNames" :key="n" :value="n">{{ n }}</option>
        </select>
        <button class="cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on': renaming }" :disabled="!selectedAttr"
          aria-label="Rename attribute" v-tooltip.bottom="'Rename this attribute on every image'" @click="toggleRename">
          <i class="pi pi-pencil" />
        </button>
        <ConfirmDeleteButton :disabled="!selectedAttr"
          title="Delete this attribute from all images."
          :armed-title="`Click again to permanently delete '${selectedAttr}' from every image.`"
          @confirm="deleteAttr" />
      </div>
      <div v-if="renaming && selectedAttr" class="field-row">
        <input class="field-input flex1" v-model="renameTo" placeholder="New name…" autofocus
          @keydown.enter="renameAttr" @keydown.escape="renaming = false"
          v-tooltip.bottom="'Press Enter to rename, Escape to cancel'" />
        <button class="cc-btn cc-btn-primary" :disabled="!renameTo.trim() || renameTo.trim() === selectedAttr"
          v-tooltip.bottom="'Rename the attribute; its values stay'" @click="renameAttr">
          Rename
        </button>
      </div>
    </section>

    <!-- ── Assign single value ─────────────────────────────────── -->
    <section class="cc-panel-section md-section" :class="{ disabled: attrDisabled }">
      <h3 class="cc-panel-heading cc-eyebrow cc-fs-2xs">Assign value</h3>
      <div class="field-row">
        <SuggestInput v-model="singleValue" :options="valuesInUse" mark-existing
          input-class="field-input flex1" placeholder="Value…"
          :disabled="attrDisabled"
          :tip="'Assign this value to the selected attribute for all target images'"
          @keydown.enter="assignSingleValue" />
        <button class="cc-btn cc-btn-ghost" :disabled="attrDisabled || !singleValue"
          @click="assignSingleValue"
          v-tooltip.bottom="'Apply to selected images, or all images if none are selected'">
          Assign
        </button>
      </div>
    </section>

    <!-- ── Regex ──────────────────────────────────────────────── -->
    <section class="cc-panel-section md-section" :class="{ disabled: attrDisabled }">
      <h3 class="cc-panel-heading cc-eyebrow cc-fs-2xs">Extract via regex</h3>
      <ExtractRuleEditor v-model:regex="regexpValue" v-model:source="regexpSource" v-model:spec="regexSpec"
                         :sample="regexSampleImg" :disabled="attrDisabled">
        <template #action>
          <button class="cc-btn cc-btn-ghost" :disabled="attrDisabled || !regexpValue"
            @click="assignRegexp"
            v-tooltip.bottom="'Extract a value from each image filename or path'">
            Apply
          </button>
        </template>
      </ExtractRuleEditor>
    </section>

    <!-- ── Group sequences ────────────────────────────────────── -->
    <section class="cc-panel-section md-section">
      <h3 class="cc-panel-heading cc-eyebrow cc-fs-2xs">Group sequences</h3>
      <p class="section-hint cc-muted">Numbers images within each attribute group (<strong>GroupSeq</strong>).</p>
      <button class="cc-btn cc-btn-ghost" @click="assignGroupSequences"
        :disabled="attrNames.length === 0"
        v-tooltip.bottom="'Assign GroupSeq from the current attribute values'">
        Assign
      </button>
    </section>

    <!-- ── Channel names ──────────────────────────────────────── -->
    <section class="cc-panel-section md-section" data-guide="metadata.channels">
      <h3 class="cc-panel-heading cc-eyebrow cc-fs-2xs">Channel names</h3>

      <div class="field-col">
        <!-- Copy from a reference image: fills the field below, so the names are visible and editable
             before Assign writes them (same contract as the regex builder). -->
        <div class="field-row">
          <select class="field-input flex1" v-model="channelRefUid" @change="copyChannelsFromReference"
            :disabled="channelRefs.length === 0"
            v-tooltip.bottom="'Copy channel names from this image'">
            <option value="">— from reference image —</option>
            <option v-for="i in channelRefs" :key="i.uid" :value="i.uid">
              {{ i.name }} ({{ i.channelNames?.length }})
            </option>
          </select>
        </div>

        <textarea class="field-textarea" v-model="channelNameList" rows="4"
          placeholder="One channel name per line…"
          v-tooltip.bottom="'Assign these channel names to the selected images (or all if none selected)'" />
        <div class="field-row">
          <button class="cc-btn cc-btn-ghost" :disabled="!channelNames.length"
            @click="assignChannelNames"
            v-tooltip.bottom="'Set channel names on the target images'">
            Assign channels
          </button>
          <span v-if="channelNames.length" class="cc-readout">{{ channelSplit.apply.length }} image(s)</span>
        </div>
        <p v-if="channelNames.length && channelSplit.skipped.length" class="section-hint cc-muted-warn">
          {{ channelSplit.skipped.length }} target(s) don't have {{ channelNames.length }} channels — skipped.
        </p>
      </div>

      <p class="section-hint cc-muted">Or edit channel names directly in the image table.</p>
    </section>

    </div>
  </aside>
</template>

<style scoped>
/* outer wrapper fills the host panel, which owns the width; inner scrolls under it */
.metadata-panel-wrap {
  position: relative;
  flex: 1;
  min-width: 0;
  display: flex;
  min-height: 0;
}
.metadata-panel { overflow-y: auto; }   /* + .cc-side-panel */

/* + .cc-panel-section (padding, divider); the stack inside is ours, so the heading's own margin goes */
.md-section { display: flex; flex-direction: column; gap: var(--cc-space-sm); }
.md-section > .cc-panel-heading { margin: 0; }
.md-section.disabled { opacity: 0.45; pointer-events: none; }

.section-hint { margin: 0; line-height: 1.5; }

.field-row {
  display: flex;
  gap: 0.4rem;
  align-items: center;
}
.field-col {
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
}
/* `:deep` too: the Assign value field is SuggestInput's <input>, under its scope, so a bare class never reached it */
.flex1, :deep(.flex1) { flex: 1; min-width: 0; }

/* visual styling from the global form base (style.css) */
.field-input:disabled, :deep(.field-input:disabled) { opacity: 0.4; cursor: not-allowed; }

.field-textarea {
  border-radius: var(--cc-radius-sm);
  padding: 0.35rem 0.5rem;
  resize: vertical;
  font-family: var(--cc-mono);
  line-height: 1.5;
  width: 100%;
  box-sizing: border-box;
}
.field-textarea:focus { outline: 1px solid var(--cc-accent); }

.warn-count {
  display: inline-flex; align-items: center; justify-content: center;
  min-width: 1.1rem; height: 1.1rem; padding: 0 0.3rem;
  border-radius: var(--cc-radius-pill); font-size: var(--cc-fs-2xs); font-weight: 700;
  background: var(--cc-tint-amber); color: var(--cc-ink-amber);
}

/* buttons use the global .cc-btn utilities (style.css) */
</style>
