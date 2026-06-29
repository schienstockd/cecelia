<script setup lang="ts">
import { ref, computed } from 'vue'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useLogStore } from '../../stores/log'

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

const channelNames = computed(() => {
  // Channel names from the first selected (or first) image
  const uid = props.selectedUids[0] ?? setImages.value[0]?.uid
  const img = setImages.value.find(i => i.uid === uid)
  return img?.channelNames ?? []
})

// ── Attr create/delete ─────────────────────────────────────────────────────────

const newAttrName   = ref('')
const selectedAttr  = ref('')
const confirmDelete = ref(false)

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
  confirmDelete.value = false

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

async function assignRegexp() {
  if (!selectedAttr.value || !regexpValue.value) return
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return

  let re: RegExp
  try { re = new RegExp(regexpValue.value) }
  catch { log.warn('Invalid regular expression.', { source: 'metadata' }); return }

  const values: Record<string, string> = {}
  for (const uid of targetUids.value) {
    const img  = setImages.value.find(i => i.uid === uid)
    if (!img) continue
    const src  = regexpSource.value === 'filepath' ? (img.filepath ?? img.name) : img.name
    const match = src.match(re)
    values[uid] = match ? match[0] : ''
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
const copyingChannels = ref(false)

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

async function copyChannelNamesToAll() {
  const uid = props.selectedUids[0] ?? setImages.value[0]?.uid
  const img = setImages.value.find(i => i.uid === uid)
  if (!img?.channelNames?.length) {
    log.warn('Select an image with channel names first.', { source: 'metadata' }); return
  }
  const names = img.channelNames
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  copyingChannels.value = true

  try {
    const allUids = setImages.value.map(i => i.uid)
    const res = await fetch('/api/images/channelnames', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, imageUids: allUids, channelNames: names }),
    })
    const body = await res.json().catch(() => ({})) as { error?: string }
    if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)

    for (const iuid of allUids)
      project.updateImageMeta(iuid, { channelNames: names })

    log.info(`Copied ${names.length} channel name(s) to all ${allUids.length} images.`, { source: 'metadata' })
  } catch (e) {
    log.error(e instanceof Error ? e.message : String(e), { source: 'metadata' })
  } finally {
    copyingChannels.value = false
  }
}

const attrDisabled = computed(() => !selectedAttr.value)
</script>

<template>
  <aside class="metadata-panel">

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
        <template v-if="!confirmDelete">
          <button class="btn-danger btn-sm" :disabled="!selectedAttr"
            @click="confirmDelete = true"
            v-tooltip.bottom="'Delete this attribute from all images.'">
            <i class="pi pi-trash" />
          </button>
        </template>
        <template v-else>
          <button class="btn-danger btn-sm" @click="deleteAttr"
            v-tooltip.bottom="`Permanently delete '${selectedAttr}' from every image.`">Confirm</button>
          <button class="btn-ghost btn-sm" @click="confirmDelete = false">Cancel</button>
        </template>
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
          v-tooltip.bottom="'JavaScript regex applied to the image name or filepath.'" />
        <button class="btn-ghost btn-sm" :disabled="attrDisabled || !regexpValue"
          @click="assignRegexp"
          v-tooltip.bottom="'Extract a value from each image filename or path.'">
          Apply
        </button>
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
    </section>

    <!-- ── Group sequences ────────────────────────────────────── -->
    <section class="panel-section">
      <div class="section-title">Group sequences</div>
      <p class="section-hint">
        Groups images by all current attr values and assigns a sequential index
        (<strong>GroupSeq</strong>) within each group.
      </p>
      <button class="btn-ghost btn-sm" @click="assignGroupSequences"
        :disabled="attrNames.length === 0"
        v-tooltip.bottom="'Compute GroupSeq based on the current attribute values.'">
        Assign group sequences
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

      <div class="channel-names-preview" v-if="channelNames.length">
        <span class="preview-label">First image channels:</span>
        <span v-for="n in channelNames" :key="n" class="channel-chip">{{ n }}</span>
      </div>

      <button class="btn-ghost btn-sm mt-2"
        :disabled="!channelNames.length || copyingChannels"
        @click="copyChannelNamesToAll"
        v-tooltip.bottom="'Copy channel names from the first selected image to all images in the set.'">
        <i v-if="copyingChannels" class="pi pi-spin pi-cog" />
        Copy from first to all
      </button>
    </section>

  </aside>
</template>

<style scoped>
.metadata-panel {
  width: 280px;
  min-width: 260px;
  max-width: 320px;
  border-left: 1px solid var(--cc-border);
  display: flex;
  flex-direction: column;
  overflow-y: auto;
  background: var(--cc-bg);
  flex-shrink: 0;
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

.channel-names-preview {
  display: flex;
  flex-wrap: wrap;
  gap: 0.3rem;
  align-items: center;
  margin-top: 0.25rem;
}
.preview-label {
  font-size: 0.7rem;
  color: var(--cc-text-dim);
  width: 100%;
}
.channel-chip {
  font-size: 0.68rem;
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
  border-radius: 999px;
  padding: 0.1rem 0.45rem;
  color: var(--cc-text);
  font-family: var(--cc-mono);
}

.mt-2 { margin-top: 0.5rem; }

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
