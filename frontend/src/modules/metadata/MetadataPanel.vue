<script setup lang="ts">
import { ref, computed } from 'vue'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useLogStore } from '../../stores/log'
import { metadataWarning } from '../../lib/imageMetadataWarnings'
import { buildFieldRegex, buildLookaroundRegex, extractWith,
         type FieldPos, type CtxClass, type ExtractKind } from '../../utils/regexBuilder'
import { usePanelResize } from '../../composables/usePanelResize'
import PhysicalSizeDialog from '../../components/PhysicalSizeDialog.vue'
import ConfirmButton from '../../components/ConfirmButton.vue'

// resizable sidebar width (persisted) — same behaviour as the TaskRunner functions panel
const { width: panelWidth, onResizeStart } =
  usePanelResize({ min: 260, max: 520, default: 280, storageKey: 'cc-metadata-width' })

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

  const res = await fetch('/api/images/attr/create', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ projectUid, imageUids: setImages.value.map(i => i.uid), attrName: name }),
  })
  const body = await res.json().catch(() => ({})) as { error?: string }
  if (!res.ok) { log.error(body.error ?? `HTTP ${res.status}`, { source: 'metadata' }); return }

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

  const res = await fetch('/api/images/attr/delete', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ projectUid, imageUids: setImages.value.map(i => i.uid), attrName: name }),
  })
  const body = await res.json().catch(() => ({})) as { error?: string }
  if (!res.ok) { log.error(body.error ?? `HTTP ${res.status}`, { source: 'metadata' }); return }

  project.removeAttrKey(props.setUid, name)
  selectedAttr.value = ''
  log.info(`Deleted attribute "${name}".`, { source: 'metadata' })
}

// ── Assign single value ────────────────────────────────────────────────────────

const singleValue = ref('')

async function assignSingleValue() {
  if (!selectedAttr.value || !singleValue.value) return
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return

  const values: Record<string, string> = {}
  for (const uid of targetUids.value) values[uid] = singleValue.value

  const res = await fetch('/api/images/attr/set', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ projectUid, attrName: selectedAttr.value, values }),
  })
  const body = await res.json().catch(() => ({})) as { error?: string }
  if (!res.ok) { log.error(body.error ?? `HTTP ${res.status}`, { source: 'metadata' }); return }

  project.setAttrValues(selectedAttr.value, values)
  log.info(`Assigned "${singleValue.value}" to ${targetUids.value.length} image(s).`, { source: 'metadata' })
}

// ── Extract via regex ──────────────────────────────────────────────────────────

const regexpValue  = ref('')
const regexpSource = ref<'name' | 'filepath'>('name')

// ── Regex builder (split by a separator, take the Nth/last field) ────────────────
// Covers the trivial "just pull the treatment/mouse/etc. out of the filename" cases people usually
// have. It fills the visible regex field, so the user sees the real pattern and can tweak it.
const builderOpen      = ref(false)
const builderMode      = ref<'field' | 'around'>('field')
// field mode: split by a separator, take a field
const builderSep       = ref<'-' | '_' | '.' | 'space' | 'custom'>('-')
const builderCustomSep = ref('')
const builderPos       = ref<FieldPos>('last')
const builderStripExt  = ref(true)
// look-around mode: extract a token bounded by context. Each side is literal text + a class that
// varies, so "M" + a number → (?<=M\d+) anchors M1b/M2a/M4f without hardcoding the mouse number.
const beforeText       = ref('')
const beforeCls        = ref<CtxClass>('none')
const extractKind      = ref<ExtractKind>('digits')
const extractText      = ref('')
const afterText        = ref('')
const afterCls         = ref<CtxClass>('none')
const effectiveSep = computed(() =>
  builderSep.value === 'space'  ? ' ' :
  builderSep.value === 'custom' ? builderCustomSep.value : builderSep.value)
// Live preview of the SINGLE regex field against the first image Apply would target. The builder
// writes straight into `regexpValue` (below), so there's one input and one preview — never two.
const regexSample = computed(() => {
  const uid = targetUids.value[0] ?? setImages.value[0]?.uid
  const img = setImages.value.find(i => i.uid === uid)
  if (!img) return ''
  return regexpSource.value === 'filepath' ? (img.filepath ?? img.name) : img.name
})
const regexPreview = computed(() => extractWith(regexpValue.value, regexSample.value))
// Adjusting any builder control rebuilds the pattern into the regex field (user interaction only —
// wired via @change — so it never clobbers a hand-typed regex unless a builder control is touched).
function applyBuilder() {
  const r = builderMode.value === 'around'
    ? buildLookaroundRegex(
        { text: beforeText.value, cls: beforeCls.value },
        { kind: extractKind.value, text: extractText.value },
        { text: afterText.value,  cls: afterCls.value })
    : buildFieldRegex(effectiveSep.value, builderPos.value, builderStripExt.value)
  if (r) regexpValue.value = r
}

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
    const src  = regexpSource.value === 'filepath' ? (img.filepath ?? img.name) : img.name
    // first capture group wins (the builder always makes one), else the whole match
    values[uid] = extractWith(regexpValue.value, src)
  }

  const res = await fetch('/api/images/attr/set', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ projectUid, attrName: selectedAttr.value, values }),
  })
  const body = await res.json().catch(() => ({})) as { error?: string }
  if (!res.ok) { log.error(body.error ?? `HTTP ${res.status}`, { source: 'metadata' }); return }

  project.setAttrValues(selectedAttr.value, values)
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
  const createRes = await fetch('/api/images/attr/create', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ projectUid, imageUids: setImages.value.map(i => i.uid), attrName: 'GroupSeq' }),
  })
  if (!createRes.ok) { log.error(`HTTP ${createRes.status}`, { source: 'metadata' }); return }
  project.addAttrKey(props.setUid, 'GroupSeq')

  const setRes = await fetch('/api/images/attr/set', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ projectUid, attrName: 'GroupSeq', values }),
  })
  const body = await setRes.json().catch(() => ({})) as { error?: string }
  if (!setRes.ok) { log.error(body.error ?? `HTTP ${setRes.status}`, { source: 'metadata' }); return }

  project.setAttrValues('GroupSeq', values)
  log.info('Assigned group sequences.', { source: 'metadata' })
}

// ── Channel names ──────────────────────────────────────────────────────────────

const channelNameList = ref('')

async function assignChannelNames() {
  const names = channelNameList.value.split('\n').map(s => s.trim()).filter(Boolean)
  if (!names.length) return
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return

  const res = await fetch('/api/images/channelnames', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ projectUid, imageUids: targetUids.value, channelNames: names }),
  })
  const body = await res.json().catch(() => ({})) as { error?: string }
  if (!res.ok) { log.error(body.error ?? `HTTP ${res.status}`, { source: 'metadata' }); return }

  for (const uid of targetUids.value)
    project.updateImageMeta(uid, { channelNames: names })

  log.info(`Set ${names.length} channel name(s) on ${targetUids.value.length} image(s).`, { source: 'metadata' })
}


const attrDisabled = computed(() => !selectedAttr.value)

// ── Physical size & timing — opens the shared modal (PhysicalSizeDialog.vue) rather than
// cramming a full editor into this narrow sidebar; see feedback after the first version shipped.

const showPhysDialog = ref(false)
const physFocusUid = computed(() => props.selectedUids[0] ?? setImages.value[0]?.uid ?? null)
const flaggedCount = computed(() => setImages.value.filter(i => metadataWarning(i)).length)
</script>

<template>
  <aside class="metadata-panel-wrap" :style="{ width: panelWidth + 'px' }">
    <!-- drag handle on left edge (shared usePanelResize) -->
    <div class="resize-handle" @mousedown="onResizeStart" v-tooltip.left="'Drag to resize the panel'" />
    <div class="metadata-panel">

    <!-- ── Physical size & timing ───────────────────────────────── -->
    <section class="panel-section">
      <div class="section-title">Physical size &amp; timing</div>
      <button class="btn-ghost btn-sm" :disabled="!physFocusUid" @click="showPhysDialog = true"
        v-tooltip.bottom="'View or fix voxel size and frame interval for the selected image(s).'">
        <i class="pi pi-ruler" /> Open editor
        <span v-if="flaggedCount" class="warn-count" v-tooltip.bottom="`${flaggedCount} image(s) in this set are flagged`">{{ flaggedCount }}</span>
      </button>
    </section>
    <PhysicalSizeDialog v-if="showPhysDialog && setUid && physFocusUid"
      :set-uid="setUid" :focus-uid="physFocusUid" :selected-uids="selectedUids"
      @close="showPhysDialog = false" />

    <!-- ── Attribute management ─────────────────────────────────── -->
    <section class="panel-section">
      <div class="section-title">Attributes</div>

      <!-- create -->
      <div class="field-row">
        <input class="field-input flex1" v-model="newAttrName" placeholder="New attribute name…"
          @keydown.enter="createAttr"
          v-tooltip.bottom="'Name for the new metadata column — e.g. condition, timepoint.'" />
        <button class="btn-primary btn-sm" @click="createAttr" :disabled="!newAttrName.trim()"
          v-tooltip.bottom="'Add this attribute key to all images in the set.'">
          Create
        </button>
      </div>

      <!-- select + delete -->
      <div class="field-row">
        <select class="field-input flex1" v-model="selectedAttr"
          :disabled="attrNames.length === 0"
          v-tooltip.bottom="'Choose which attribute to assign values to.'">
          <option value="">— select attribute —</option>
          <option v-for="n in attrNames" :key="n" :value="n">{{ n }}</option>
        </select>
        <ConfirmButton @confirm="deleteAttr" v-slot="{ armed, arm, confirm, cancel }">
          <button v-if="!armed" class="btn-danger btn-sm" :disabled="!selectedAttr" @click="arm"
            v-tooltip.bottom="'Delete this attribute from all images.'"><i class="pi pi-trash" /></button>
          <template v-else>
            <button class="btn-danger btn-sm" @click="confirm"
              v-tooltip.bottom="`Permanently delete '${selectedAttr}' from every image.`">Confirm</button>
            <button class="btn-ghost btn-sm" @click="cancel">Cancel</button>
          </template>
        </ConfirmButton>
      </div>
    </section>

    <!-- ── Assign single value ─────────────────────────────────── -->
    <section class="panel-section" :class="{ disabled: attrDisabled }">
      <div class="section-title">Assign value</div>
      <div class="field-row">
        <input class="field-input flex1" v-model="singleValue" placeholder="Value…"
          :disabled="attrDisabled"
          @keydown.enter="assignSingleValue"
          v-tooltip.bottom="'Assign this value to the selected attribute for all target images.'" />
        <button class="btn-ghost btn-sm" :disabled="attrDisabled || !singleValue"
          @click="assignSingleValue"
          v-tooltip.bottom="'Apply to selected images, or all images if none are selected.'">
          Assign
        </button>
      </div>
    </section>

    <!-- ── Regex ──────────────────────────────────────────────── -->
    <section class="panel-section" :class="{ disabled: attrDisabled }">
      <div class="section-title">Extract via regex</div>
      <div class="field-row">
        <input class="field-input flex1" v-model="regexpValue" placeholder="Regular expression…"
          :disabled="attrDisabled"
          @keydown.enter="assignRegexp"
          v-tooltip.bottom="'Regex over the name; a (group) if present, else the whole match. e.g. M(\\d+)→4. New to regex? Use the builder.'" />
        <button class="btn-ghost btn-sm" :disabled="attrDisabled || !regexpValue"
          @click="assignRegexp"
          v-tooltip.bottom="'Extract a value from each image filename or path.'">
          Apply
        </button>
      </div>

      <!-- one live preview for the single regex field (whether typed by hand or built below) -->
      <div class="regex-preview" v-if="regexSample && regexpValue">
        <span class="preview-src" v-tooltip.bottom="regexSample">{{ regexSample }}</span>
        <span class="preview-arrow">→</span>
        <span class="preview-val">{{ regexPreview || '(no match)' }}</span>
      </div>

      <div class="radio-row">
        <label class="radio-label">
          <input type="radio" v-model="regexpSource" value="name" :disabled="attrDisabled" />
          Filename
        </label>
        <label class="radio-label">
          <input type="radio" v-model="regexpSource" value="filepath" :disabled="attrDisabled" />
          Original path
        </label>
      </div>

      <!-- Builder: split by a separator, take a field — writes into the SAME regex field above, so
           there's only ever one input + one preview. For the common "no clue about regex" case. -->
      <button class="builder-toggle" @click="builderOpen = !builderOpen" :disabled="attrDisabled"
        v-tooltip.bottom="'Build the pattern above without writing regex — pick a separator and field.'">
        <i :class="['pi', builderOpen ? 'pi-chevron-down' : 'pi-chevron-right']" /> Builder
      </button>
      <div v-if="builderOpen" class="builder" :class="{ disabled: attrDisabled }">
        <div class="field-row">
          <span class="builder-label">Mode</span>
          <select class="field-input" v-model="builderMode" @change="applyBuilder" :disabled="attrDisabled">
            <option value="field">Split into fields</option>
            <option value="around">Around a marker</option>
          </select>
        </div>

        <!-- Field mode: split by a separator, take a field -->
        <template v-if="builderMode === 'field'">
          <div class="field-row">
            <span class="builder-label">Split by</span>
            <select class="field-input" v-model="builderSep" @change="applyBuilder" :disabled="attrDisabled">
              <option value="-">- dash</option>
              <option value="_">_ underscore</option>
              <option value=".">. dot</option>
              <option value="space">␣ space</option>
              <option value="custom">custom…</option>
            </select>
            <input v-if="builderSep === 'custom'" type="text" class="field-input builder-custom"
              v-model="builderCustomSep" @input="applyBuilder" maxlength="4" placeholder="sep" :disabled="attrDisabled" />
          </div>
          <div class="field-row">
            <span class="builder-label">Take</span>
            <select class="field-input" v-model="builderPos" @change="applyBuilder" :disabled="attrDisabled">
              <option value="first">1st field</option>
              <option value="second">2nd field</option>
              <option value="third">3rd field</option>
              <option value="last">last field</option>
            </select>
            <label class="radio-label" v-tooltip.bottom="'Drop a trailing .extension from the captured value.'">
              <input type="checkbox" v-model="builderStripExt" @change="applyBuilder" :disabled="attrDisabled" /> no ext
            </label>
          </div>
        </template>

        <!-- Look-around mode: extract a token bounded by context — e.g. "M1a" → digits after "M" → 1 -->
        <template v-else>
          <div class="field-row">
            <span class="builder-label">Preceded by</span>
            <input type="text" class="field-input flex1"
              v-model="beforeText" @input="applyBuilder" placeholder="text, e.g. M" :disabled="attrDisabled" />
            <select class="field-input" v-model="beforeCls" @change="applyBuilder" :disabled="attrDisabled"
              v-tooltip.bottom="'…then a varying part (a number, letters, …).'">
              <option value="none">—</option>
              <option value="digits">+ number</option>
              <option value="letters">+ letters</option>
              <option value="lower">+ lowercase</option>
              <option value="upper">+ uppercase</option>
            </select>
          </div>
          <div class="field-row">
            <span class="builder-label">Extract</span>
            <select class="field-input" v-model="extractKind" @change="applyBuilder" :disabled="attrDisabled">
              <option value="digits">a number</option>
              <option value="letters">letters</option>
              <option value="lower">lowercase</option>
              <option value="upper">uppercase</option>
              <option value="word">word</option>
              <option value="custom">custom…</option>
            </select>
            <input v-if="extractKind === 'custom'" type="text" class="field-input flex1"
              v-model="extractText" @input="applyBuilder" placeholder="e.g. [a-d]" :disabled="attrDisabled" />
          </div>
          <div class="field-row">
            <span class="builder-label">Followed by</span>
            <input type="text" class="field-input flex1"
              v-model="afterText" @input="applyBuilder" placeholder="text, e.g. -" :disabled="attrDisabled" />
            <select class="field-input" v-model="afterCls" @change="applyBuilder" :disabled="attrDisabled"
              v-tooltip.bottom="'…then a varying part (a number, letters, …).'">
              <option value="none">—</option>
              <option value="digits">+ number</option>
              <option value="letters">+ letters</option>
              <option value="lower">+ lowercase</option>
              <option value="upper">+ uppercase</option>
            </select>
          </div>
        </template>

        <p v-if="builderMode === 'around'" class="builder-hint">
          Each side: fixed text + a part that varies (<em>+ number</em>, <em>+ letters</em>).
        </p>
        <p class="builder-hint">Adjusts the pattern above — watch the preview, then <strong>Apply</strong>.</p>
      </div>
    </section>

    <!-- ── Group sequences ────────────────────────────────────── -->
    <section class="panel-section">
      <div class="section-title">Group sequences</div>
      <p class="section-hint">Numbers images within each attribute group (<strong>GroupSeq</strong>).</p>
      <button class="btn-ghost btn-sm" @click="assignGroupSequences"
        :disabled="attrNames.length === 0"
        v-tooltip.bottom="'Assign GroupSeq from the current attribute values.'">
        Assign
      </button>
    </section>

    <!-- ── Channel names ──────────────────────────────────────── -->
    <section class="panel-section">
      <div class="section-title">Channel names</div>

      <div class="field-col">
        <textarea class="field-textarea" v-model="channelNameList" rows="4"
          placeholder="One channel name per line…"
          v-tooltip.bottom="'Assign these channel names to the selected images (or all if none selected).'" />
        <div class="field-row">
          <button class="btn-ghost btn-sm" :disabled="!channelNameList.trim()"
            @click="assignChannelNames"
            v-tooltip.bottom="'Set channel names on the target images.'">
            Assign channels
          </button>
        </div>
      </div>

      <p class="section-hint">Or edit channel names directly in the image table.</p>
    </section>

    </div>
  </aside>
</template>

<style scoped>
/* outer wrapper carries the (resizable) width + the fixed drag handle; inner scrolls under it */
.metadata-panel-wrap {
  position: relative;
  flex-shrink: 0;
  display: flex;
  min-height: 0;
}
.resize-handle {
  position: absolute; left: 0; top: 0; bottom: 0; width: 5px;
  cursor: col-resize; z-index: 10;
}
.resize-handle:hover, .resize-handle:active { background: var(--cc-accent); opacity: 0.35; }

.metadata-panel {
  width: 100%;
  border-left: 1px solid var(--cc-border);
  display: flex;
  flex-direction: column;
  overflow-y: auto;
  background: var(--cc-bg);
}

.panel-section {
  padding: 0.85rem 1rem;
  border-bottom: 1px solid var(--cc-border);
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}
.panel-section.disabled { opacity: 0.45; pointer-events: none; }

.section-title {
  font-size: 0.7rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.07em;
  color: var(--cc-text-dim);
}
.section-hint {
  font-size: 0.75rem;
  color: var(--cc-text-dim);
  margin: 0;
  line-height: 1.5;
}

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
.flex1 { flex: 1; min-width: 0; }

/* visual styling from the global form base (style.css) */
.field-input:disabled { opacity: 0.4; cursor: not-allowed; }

.field-textarea {
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
  border-radius: 0.3rem;
  color: var(--cc-text);
  font-size: 0.8rem;
  padding: 0.35rem 0.5rem;
  resize: vertical;
  font-family: var(--cc-mono);
  line-height: 1.5;
  width: 100%;
  box-sizing: border-box;
}
.field-textarea:focus { outline: 1px solid var(--cc-accent); }

.radio-row {
  display: flex;
  gap: 0.75rem;
}
.radio-label {
  display: flex;
  align-items: center;
  gap: 0.3rem;
  font-size: 0.78rem;
  color: var(--cc-text-dim);
  cursor: pointer;
}

/* ── Regex builder ─────────────────────────────────────────────────────────── */
.builder-toggle {
  display: inline-flex; align-items: center; gap: 0.3rem;
  background: none; border: none; cursor: pointer;
  color: var(--cc-text-dim); font-size: 0.75rem; padding: 0.1rem 0;
  margin-top: 0.15rem;
}
.builder-toggle:hover { color: var(--cc-text); }
.builder-toggle:disabled { opacity: 0.4; cursor: not-allowed; }
.builder-toggle .pi { font-size: 0.6rem; }

.builder {
  display: flex; flex-direction: column; gap: 0.4rem;
  margin-top: 0.35rem; padding: 0.5rem;
  border: 1px solid var(--cc-border); border-radius: 0.3rem;
  background: var(--cc-surface-1);
}
.builder.disabled { opacity: 0.5; pointer-events: none; }
/* fixed label width + flexible controls → every select/input lines up down the builder */
.builder-label { font-size: 0.75rem; color: var(--cc-text-dim); flex: 0 0 5rem; white-space: nowrap; }
.builder .field-input { flex: 1 1 0; min-width: 0; }
.builder .builder-custom { flex: 0 0 3.5rem; }

.builder-hint { font-size: 0.72rem; color: var(--cc-text-dim); margin: 0; line-height: 1.4; }

/* single preview for the regex field (typed or built) */
.regex-preview { display: flex; align-items: center; gap: 0.3rem; font-size: 0.72rem; min-width: 0; }
.preview-src {
  color: var(--cc-text-dim); overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
  flex: 1; min-width: 0;
}
.preview-arrow { color: var(--cc-text-dim); flex-shrink: 0; }
.preview-val {
  color: var(--cc-accent); font-weight: 600; font-family: var(--cc-mono);
  flex-shrink: 0; max-width: 45%; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
}

.warn-count {
  display: inline-flex; align-items: center; justify-content: center;
  min-width: 1.1rem; height: 1.1rem; padding: 0 0.3rem;
  border-radius: 999px; font-size: 0.65rem; font-weight: 700;
  background: #7c2d1244; color: #fcd34d;
}

.btn-sm {
  display: flex; align-items: center; gap: 0.3rem;
  font-size: 0.78rem; font-weight: 500; padding: 0.3rem 0.65rem;
  border-radius: 0.35rem; border: 1px solid transparent;
  cursor: pointer; white-space: nowrap; transition: background 0.1s;
}
.btn-primary { background: var(--cc-accent); color: #fff; }
.btn-primary:hover:not(:disabled) { filter: brightness(1.1); }
.btn-primary:disabled { opacity: 0.4; cursor: not-allowed; }
.btn-ghost { background: var(--cc-surface-2); border-color: var(--cc-border); color: var(--cc-text-dim); }
.btn-ghost:hover:not(:disabled) { color: var(--cc-text); border-color: #484f58; }
.btn-ghost:disabled { opacity: 0.4; cursor: not-allowed; }
.btn-danger { background: #7f1d1d44; border-color: #7f1d1d; color: #fca5a5; }
.btn-danger:hover:not(:disabled) { background: #7f1d1d88; }
.btn-danger:disabled { opacity: 0.4; cursor: not-allowed; }
</style>
