<!--
  Physical size & timing editor — a focused modal (not squeezed into the metadata sidebar) for
  viewing/fixing an image's voxel size and frame interval. Opens from the ImageTable warning icon
  (docs/UI.md) or the Metadata module page. Shell copied from ProjectPanel.vue's overlay/modal
  pattern (the one hand-rolled modal convention in this codebase — no PrimeVue Dialog).
  Explanatory text lives in tooltips, not inline paragraphs — see feedback from testing.
-->
<script setup lang="ts">
import { ref, computed, watch } from 'vue'
import BaseModal from './BaseModal.vue'
import { useProjectStore, type CciaImage } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useLogStore } from '../stores/log'
import { metadataWarning, flaggedFields, downstreamArtifactsNote, type PhysField as WarnField } from '../lib/imageMetadataWarnings'

const props = defineProps<{
  setUid: string
  focusUid: string          // the image this dialog was opened for — seeds the form
  selectedUids: string[]    // checkbox selection at time of opening (may be empty or just [focusUid])
}>()
const emit = defineEmits<{ (e: 'close'): void }>()

const project     = useProjectStore()
const projectMeta = useProjectMetaStore()
const log         = useLogStore()

const setImages = computed(() => project.sets.find(s => s.uid === props.setUid)?.images ?? [])
const focusedImg = computed(() => setImages.value.find(i => i.uid === props.focusUid) ?? null)
const focusedWarning = computed(() => focusedImg.value ? metadataWarning(focusedImg.value) : null)
// Any target that already has processed versions/segmentations built from its current calibration.
const downstreamNote = computed(() => {
  for (const uid of targetUids.value) {
    const note = downstreamArtifactsNote(setImages.value.find(i => i.uid === uid) ?? ({} as CciaImage))
    if (note) return note
  }
  return null
})

// Always includes — and leads with — the focused image (the one whose icon was actually
// clicked), regardless of whether it happens to be checked: opening an image's own editor must
// always be able to write back to that image. Any other checked images ride along as additional
// Apply targets; "Copy to selected" / "Fill flagged" are the explicit wide-reaching actions.
const targetUids = computed(() => [...new Set([props.focusUid, ...props.selectedUids])])

// Union of flags across the WHOLE target set, not just the focused image — drives the toggle
// chips (both their default state and their highlight): select a batch flagged for Z + Δt only,
// open a clean reference, and only Z/Δt light up — X/Y were never in question for anyone involved.
const targetFlags = computed<Set<WarnField>>(() => {
  const flags = new Set<WarnField>()
  for (const uid of targetUids.value) {
    const img = setImages.value.find(i => i.uid === uid)
    if (img) for (const f of flaggedFields(img)) flags.add(f)
  }
  return flags
})

type PhysField = 'physicalSizeX' | 'physicalSizeY' | 'physicalSizeZ' | 'timeIncrement'
function fieldValues(field: PhysField): (number | null | undefined)[] {
  return targetUids.value.map(uid => setImages.value.find(i => i.uid === uid)?.[field])
}
// Real files from "the same acquisition settings" still differ by tiny rounding amounts (e.g.
// 0.5964274525755702 vs 0.5960001239680258, ~0.07% apart — independent per-file resolution-tag
// rounding), so exact equality flagged every real dataset as "mixed". A relative tolerance treats
// that noise as equal while still catching genuinely different values (e.g. Z = 3.0 vs 0.6).
function valuesEqual(a: number | null, b: number | null): boolean {
  if (a === null || b === null) return a === b
  if (a === b) return true
  return Math.abs(a - b) / Math.max(Math.abs(a), Math.abs(b), 1e-12) < 1e-3
}
function isMixed(field: PhysField): boolean {
  const vals = fieldValues(field)
  const first = vals[0] ?? null
  return !vals.every(v => valuesEqual(v ?? null, first))
}

const physX     = ref<number | null>(null)
const physY     = ref<number | null>(null)
const physZ     = ref<number | null>(null)
const physUnit  = ref('micrometer')
const timeInc   = ref<number | null>(null)
const timeUnit  = ref('second')

// Which fields Apply / Copy to selected / Fill flagged actually write — e.g. only Z was wrong,
// leave X/Y alone even though the form shows their (correct) values too.
const includeX = ref(true)
const includeY = ref(true)
const includeZ = ref(true)
const includeT = ref(true)

function reseed() {
  // Always show the FOCUSED image's own real value — never blank it out just because other
  // selected images disagree (that made the value you're trying to copy invisible/unusable).
  // "Mixed" is now a passive highlight on the field (see isMixed in the template), not something
  // that hides the number.
  const seed = (field: PhysField) => focusedImg.value?.[field] ?? null
  physX.value    = seed('physicalSizeX')
  physY.value    = seed('physicalSizeY')
  physZ.value    = seed('physicalSizeZ')
  timeInc.value  = seed('timeIncrement')
  physUnit.value = focusedImg.value?.physicalSizeUnit ?? 'micrometer'
  timeUnit.value = focusedImg.value?.timeIncrementUnit ?? 'second'

  // Default to only fields flagged somewhere in the target set (see targetFlags) — e.g. select a
  // batch flagged for Z + Δt only, open a clean reference image: only Z/Δt default on, so
  // Copy/Fill flagged fixes exactly what's broken without also overwriting X/Y that were fine.
  includeX.value = targetFlags.value.has('x')
  includeY.value = targetFlags.value.has('y')
  includeZ.value = targetFlags.value.has('z')
  includeT.value = targetFlags.value.has('t')
}
watch(targetUids, reseed, { immediate: true })

// A PhysicalSizeZ written here is either a human confirming/entering it, or copied from a
// trusted reference — either way it's no longer "auto-corrected, unverified", so clear that
// stale marker (api_images_meta_set treats a null value as "delete this key").
function withZFlagCleared(fields: Record<string, unknown>): Record<string, unknown> {
  return 'PhysicalSizeZ' in fields ? { ...fields, PhysicalSizeZ_raw: null } : fields
}
function zFlagPatch(fields: Record<string, unknown>): Record<string, unknown> {
  return 'PhysicalSizeZ' in fields ? { physicalSizeZCorrected: false } : {}
}

function currentFields(): Record<string, unknown> {
  const fields: Record<string, unknown> = {}
  if (includeX.value && physX.value != null)   fields.PhysicalSizeX     = physX.value
  if (includeY.value && physY.value != null)   fields.PhysicalSizeY     = physY.value
  if (includeZ.value && physZ.value != null)   fields.PhysicalSizeZ     = physZ.value
  if ((includeX.value || includeY.value || includeZ.value) && physUnit.value)
    fields.PhysicalSizeUnit = physUnit.value
  if (includeT.value && timeInc.value != null) fields.TimeIncrement     = timeInc.value
  if (includeT.value && timeUnit.value)        fields.TimeIncrementUnit = timeUnit.value
  return fields
}
function referenceFields(img: CciaImage): Record<string, unknown> {
  const fields: Record<string, unknown> = {}
  if (includeX.value && img.physicalSizeX != null)     fields.PhysicalSizeX     = img.physicalSizeX
  if (includeY.value && img.physicalSizeY != null)     fields.PhysicalSizeY     = img.physicalSizeY
  if (includeZ.value && img.physicalSizeZ != null)     fields.PhysicalSizeZ     = img.physicalSizeZ
  if ((includeX.value || includeY.value || includeZ.value) && img.physicalSizeUnit)
    fields.PhysicalSizeUnit = img.physicalSizeUnit
  if (includeT.value && img.timeIncrement != null)     fields.TimeIncrement     = img.timeIncrement
  if (includeT.value && img.timeIncrementUnit)         fields.TimeIncrementUnit = img.timeIncrementUnit
  return fields
}
// Only copies keys actually present in `fields` (i.e. actually sent to the backend) into the
// local store patch. `Object.assign` in updateImageMeta would otherwise happily write
// `physicalSizeX: undefined` for a toggled-off field, wiping out that image's real stored value
// client-side even though the backend was never asked to touch it (this is exactly what produced
// the bogus "mixed" X/Y highlight after copying only Z/Δt — X/Y silently went to undefined).
function patchFrom(fields: Record<string, unknown>): Record<string, unknown> {
  const patch: Record<string, unknown> = { ...zFlagPatch(fields) }
  if ('PhysicalSizeX' in fields)     patch.physicalSizeX     = fields.PhysicalSizeX
  if ('PhysicalSizeY' in fields)     patch.physicalSizeY     = fields.PhysicalSizeY
  if ('PhysicalSizeZ' in fields)     patch.physicalSizeZ     = fields.PhysicalSizeZ
  if ('PhysicalSizeUnit' in fields)  patch.physicalSizeUnit  = fields.PhysicalSizeUnit
  if ('TimeIncrement' in fields)     patch.timeIncrement     = fields.TimeIncrement
  if ('TimeIncrementUnit' in fields) patch.timeIncrementUnit = fields.TimeIncrementUnit
  return patch
}

async function pushFieldsTo(fields: Record<string, unknown>, uids: string[]): Promise<boolean> {
  const projectUid = projectMeta.current?.uid
  if (!projectUid || !uids.length) return false
  const withCleared = withZFlagCleared(fields)
  const values: Record<string, Record<string, unknown>> = {}
  for (const u of uids) values[u] = withCleared
  const res = await fetch('/api/images/meta/set', {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ projectUid, values }),
  })
  const body = await res.json().catch(() => ({})) as { error?: string }
  if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
  return true
}

const saving = ref(false)
async function apply() {
  const fields = currentFields()
  if (!Object.keys(fields).length) return
  saving.value = true
  try {
    await pushFieldsTo(fields, targetUids.value)
    const patch = patchFrom(withZFlagCleared(fields))
    for (const uid of targetUids.value) project.updateImageMeta(uid, patch)
    log.info(`Updated physical size/timing for ${targetUids.value.length} image(s).`, { source: 'metadata' })
    emit('close')
  } catch (e) {
    log.error(e instanceof Error ? e.message : String(e), { source: 'metadata' })
  } finally {
    saving.value = false
  }
}

const copying = ref(false)
async function copyToSelected() {
  const img = focusedImg.value
  if (!img) return
  const fields = referenceFields(img)
  if (!Object.keys(fields).length) { log.warn('No values to copy yet.', { source: 'metadata' }); return }
  const uids = targetUids.value.filter(u => u !== img.uid)
  if (!uids.length) { log.warn('Select at least one other image first.', { source: 'metadata' }); return }
  copying.value = true
  try {
    await pushFieldsTo(fields, uids)
    const patch = patchFrom(withZFlagCleared(fields))
    for (const u of uids) project.updateImageMeta(u, patch)
    log.info(`Copied to ${uids.length} selected image(s).`, { source: 'metadata' })
  } catch (e) {
    log.error(e instanceof Error ? e.message : String(e), { source: 'metadata' })
  } finally {
    copying.value = false
  }
}

const propagating = ref(false)
async function fillFlagged() {
  const img = focusedImg.value
  if (!img) return
  const fields = referenceFields(img)
  if (!Object.keys(fields).length) { log.warn('No values to copy yet.', { source: 'metadata' }); return }
  const targets = props.selectedUids
    .filter(uid => uid !== img.uid)
    .map(uid => setImages.value.find(i => i.uid === uid))
    .filter((i): i is CciaImage => !!i && metadataWarning(i) !== null)
  if (!targets.length) { log.warn('No other selected images are flagged.', { source: 'metadata' }); return }
  propagating.value = true
  try {
    await pushFieldsTo(fields, targets.map(t => t.uid))
    const patch = patchFrom(withZFlagCleared(fields))
    for (const t of targets) project.updateImageMeta(t.uid, patch)
    log.info(`Filled ${targets.length} flagged image(s) from "${img.name}".`, { source: 'metadata' })
  } catch (e) {
    log.error(e instanceof Error ? e.message : String(e), { source: 'metadata' })
  } finally {
    propagating.value = false
  }
}
</script>

<template>
  <BaseModal width="480px" @close="emit('close')">
    <template #title>
      <i class="pi pi-ruler" /> Physical size &amp; timing
      <i class="pi pi-info-circle info-dot"
         v-tooltip.bottom="'Read from the file at import when possible. A flagged value means we couldn\'t find it, or it looked unusual — check Fiji (Image ▸ Properties) or your acquisition settings.'" />
    </template>

      <div class="pp-form">
        <p v-if="focusedImg" class="focus-name">{{ focusedImg.name }}</p>

        <p v-if="focusedWarning" class="warn-line" v-tooltip.bottom="focusedWarning.long">
          <i class="pi pi-exclamation-triangle" /> {{ focusedWarning.short }}
        </p>

        <p v-if="downstreamNote" class="rerun-line" v-tooltip.bottom="downstreamNote.long">
          <i class="pi pi-history" /> {{ downstreamNote.short }}
        </p>

        <div class="toggle-row" v-tooltip.bottom="'Which fields Apply / Copy / Fill flagged write — untick what\'s already correct.'">
          <label class="toggle-chip" :class="{ on: includeX, warn: targetFlags.has('x') }"><input type="checkbox" v-model="includeX" />X</label>
          <label class="toggle-chip" :class="{ on: includeY, warn: targetFlags.has('y') }"><input type="checkbox" v-model="includeY" />Y</label>
          <label class="toggle-chip" :class="{ on: includeZ, warn: targetFlags.has('z') }"><input type="checkbox" v-model="includeZ" />Z</label>
          <label class="toggle-chip" :class="{ on: includeT, warn: targetFlags.has('t') }"><input type="checkbox" v-model="includeT" />Δt</label>
        </div>

        <div class="dims-row" :class="{ excluded: !includeX && !includeY && !includeZ }">
          <label class="dim-label" v-tooltip.bottom="'Pixel size in X'">X</label>
          <input class="field-input" :class="{ warn: targetFlags.has('x'), mixed: isMixed('physicalSizeX') }"
            type="number" step="any" v-model.number="physX" :disabled="!includeX" placeholder="—"
            v-tooltip.bottom="isMixed('physicalSizeX') ? 'Other selected images have a different X — this is just ' + focusedImg?.name + '\'s.' : null" />
          <label class="dim-label" v-tooltip.bottom="'Pixel size in Y'">Y</label>
          <input class="field-input" :class="{ warn: targetFlags.has('y'), mixed: isMixed('physicalSizeY') }"
            type="number" step="any" v-model.number="physY" :disabled="!includeY" placeholder="—"
            v-tooltip.bottom="isMixed('physicalSizeY') ? 'Other selected images have a different Y — this is just ' + focusedImg?.name + '\'s.' : null" />
          <label class="dim-label" v-tooltip.bottom="'Voxel depth — the Z step'">Z</label>
          <input class="field-input" :class="{ warn: targetFlags.has('z'), mixed: isMixed('physicalSizeZ') }"
            type="number" step="any" v-model.number="physZ" :disabled="!includeZ" placeholder="—"
            v-tooltip.bottom="isMixed('physicalSizeZ') ? 'Other selected images have a different Z — this is just ' + focusedImg?.name + '\'s.' : null" />
          <select class="field-input unit-input" v-model="physUnit" v-tooltip.bottom="'Unit for X/Y/Z'">
            <option value="micrometer">µm</option>
            <option value="nanometer">nm</option>
            <option value="millimeter">mm</option>
          </select>
        </div>

        <div class="dims-row" :class="{ excluded: !includeT }">
          <label class="dim-label" v-tooltip.bottom="'Time between frames'">Δt</label>
          <input class="field-input" :class="{ warn: targetFlags.has('t'), mixed: isMixed('timeIncrement') }"
            type="number" step="any" v-model.number="timeInc" :disabled="!includeT" placeholder="—"
            v-tooltip.bottom="isMixed('timeIncrement') ? 'Other selected images have a different Δt — this is just ' + focusedImg?.name + '\'s.' : null" />
          <select class="field-input unit-input" v-model="timeUnit" v-tooltip.bottom="'Unit for the frame interval'">
            <option value="second">s</option>
            <option value="minute">min</option>
          </select>
        </div>
      </div>

    <template #footer>
      <button class="btn-ghost btn-sm" :disabled="propagating" @click="fillFlagged"
        v-tooltip.top="'Fill this image\'s values into the OTHER selected images that are flagged — same-session acquisitions usually match exactly.'">
        <i v-if="propagating" class="pi pi-spin pi-cog" /><i v-else class="pi pi-share-alt" />
        Fill flagged
      </button>
      <button class="btn-ghost btn-sm" :disabled="copying" @click="copyToSelected"
        v-tooltip.top="'Copy this image\'s values to the other selected images.'">
        <i v-if="copying" class="pi pi-spin pi-cog" /><i v-else class="pi pi-copy" />
        Copy to selected
      </button>
      <span class="footer-spacer" />
      <button class="btn-ghost btn-sm" @click="emit('close')">Cancel</button>
      <button class="btn-primary btn-sm" :disabled="saving" @click="apply"
        v-tooltip.top="`Apply to ${targetUids.length} image(s). Doesn't retroactively recompute derived measures (e.g. track speed).`">
        <i v-if="saving" class="pi pi-spin pi-cog" /><i v-else class="pi pi-check" />
        Apply
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
/* Shell (overlay/box/header/footer) lives in BaseModal; only dialog-specific styles remain here. */
.info-dot { font-size: 0.72rem; color: var(--cc-text-dim); cursor: default; }
.pp-form { padding: 1rem 1.25rem; display: flex; flex-direction: column; gap: 0.75rem; }

.focus-name { margin: 0; font-size: 0.82rem; font-weight: 600; color: var(--cc-text); }

.warn-line {
  display: flex; align-items: center; gap: 0.4rem; margin: 0;
  font-size: 0.78rem; color: #fcd34d;
  background: #7c2d1244; border: 1px solid #92400e55;
  border-radius: 0.3rem; padding: 0.35rem 0.55rem; cursor: help;
}

/* Informational (not an error): downstream artifacts need a re-run after a calibration change. */
.rerun-line {
  display: flex; align-items: center; gap: 0.4rem; margin: 0;
  font-size: 0.78rem; color: var(--cc-text-dim);
  background: var(--cc-surface-2); border: 1px solid var(--cc-border);
  border-radius: 0.3rem; padding: 0.35rem 0.55rem; cursor: help;
}

.toggle-row { display: flex; gap: 0.35rem; }
.toggle-chip {
  display: flex; align-items: center; gap: 0.25rem;
  font-size: 0.72rem; font-weight: 600;
  padding: 0.15rem 0.5rem; border-radius: 999px;
  border: 1px solid var(--cc-border); background: var(--cc-surface-2);
  color: var(--cc-text-dim); cursor: pointer;
}
.toggle-chip input { display: none; }
.toggle-chip.on { border-color: var(--cc-accent); color: var(--cc-accent); background: #a78bfa14; }
.toggle-chip.warn { border-color: #f59e0b88; }
.toggle-chip.warn.on { border-color: #f59e0b; color: #fbbf24; background: #7c2d1233; }

.dims-row { display: flex; align-items: center; gap: 0.4rem; transition: opacity 0.1s; }
.dims-row.excluded { opacity: 0.4; }
.dim-label {
  flex-shrink: 0; width: 1.2rem; text-align: right;
  font-size: 0.75rem; font-weight: 600; color: var(--cc-text-dim);
}
.field-input { flex: 1; min-width: 0; }
.field-input.warn { border-color: #f59e0b !important; background: #7c2d1222; }
.field-input.mixed { border: 1px dashed #60a5fa !important; background: #1e3a5f55; cursor: help; }
.unit-input { flex: 0 0 4.2rem; }

.footer-spacer { flex: 1; }

.btn-sm {
  display: flex; align-items: center; gap: 0.3rem;
  font-size: 0.78rem; font-weight: 500;
  padding: 0.35rem 0.75rem;
  border-radius: 0.35rem; border: 1px solid transparent;
  cursor: pointer; white-space: nowrap;
}
.btn-ghost { background: var(--cc-surface-2); border-color: var(--cc-border); color: var(--cc-text-dim); }
.btn-ghost:hover:not(:disabled) { color: var(--cc-text); }
.btn-ghost:disabled { opacity: 0.4; cursor: not-allowed; }
.btn-primary { background: var(--cc-accent); color: #fff; }
.btn-primary:hover:not(:disabled) { filter: brightness(1.1); }
.btn-primary:disabled { opacity: 0.4; cursor: not-allowed; }
</style>
