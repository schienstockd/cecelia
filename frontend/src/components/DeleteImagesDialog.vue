<!--
  DeleteImagesDialog — the ONE structured delete for images, opened from the Delete button in the Import
  page's action bar. Four scopes over the current selection, picked explicitly because they answer
  different questions and must not be silently combinable (docs/todo/IMAGE_DELETE_PLAN.md Decision 2):

    images    the whole image — pixels AND analysis (only the source file outside the project survives)
    versions  specific image versions, several at once, with the version that stays active
    labels    specific label sets (segmentations) and their measurements
    analysis  everything derived, keeping the image itself — "re-run this from clean"

  It COLLECTS a plan and emits it; `ImageFileActions.vue` executes it, so the per-image loop, the k/N
  readout and the completion toast stay in one place (docs/UI.md → File operations). The destructive
  confirm lives on this modal's own footer button, so nothing is ever one click from deletion.

  Replaces three older surfaces: the `importImages.remove` task's UI entry (the task itself stays,
  unlisted — it is a valid chain node), the napari ViewerPanel's per-label delete, and the plain
  two-click Delete this button grew out of.
-->
<script setup lang="ts">
import { ref, computed } from 'vue'
import BaseModal from './BaseModal.vue'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'
import ConfirmButton from './ConfirmButton.vue'
import {
  versionCounts, labelCounts, resolveNewActive, unimportsImage, survivorCounts,
  activeMismatches, partialNames, DEFAULT_VALUE_NAME, type NameCount,
} from '../utils/imageDelete'
import type { CciaImage } from '../stores/project'

export type DeletePlan =
  | { scope: 'images' }
  | { scope: 'versions'; valueNames: string[]; newActive: string }
  | { scope: 'labels'; valueNames: string[] }
  | { scope: 'analysis' }

const props = defineProps<{ images: CciaImage[] }>()
const emit  = defineEmits<{ (e: 'close'): void; (e: 'confirm', plan: DeletePlan): void }>()

type Scope = DeletePlan['scope']
const scope = ref<Scope>('images')

const n = computed(() => props.images.length)
const what = computed(() => n.value === 1
  ? (props.images[0].name || props.images[0].uid)
  : `${n.value} images`)

// ── Scopes ────────────────────────────────────────────────────────────────────
// A scope with nothing to act on is offered but disabled, with the reason as its tip — hiding it
// would leave the user wondering where the option went.
// UNION across the selection, each with how many images carry it: a name only some images have is
// still offered, and skipped for the images that lack it. The count is what makes that visible.
const versions = computed(() => versionCounts(props.images))
const labels   = computed(() => labelCounts(props.images))
const versionNames = computed(() => versions.value.map(v => v.name))
const labelNames   = computed(() => labels.value.map(l => l.name))
// "2/3" only when the name is not on every selected image — no badge when it applies to all
const nameChip = (c: NameCount): ChipOption => ({
  value: c.name,
  label: c.name,
  badge: c.count < n.value ? `${c.count}/${n.value}` : undefined,
  tip: c.count < n.value ? `${c.name} — on ${c.count} of ${n.value} images` : c.name,
})

const scopeOptions = computed<ChipOption[]>(() => [
  { value: 'images',   label: 'Whole images', icon: 'pi pi-trash',
    tip: 'Delete the images and everything derived from them' },
  { value: 'versions', label: 'Versions', icon: 'pi pi-layer-group',
    badge: versionNames.value.length, disabled: versionNames.value.length === 0,
    tip: versionNames.value.length ? 'Delete specific image versions' : 'No image version is registered yet' },
  { value: 'labels',   label: 'Label sets', icon: 'pi pi-th-large',
    badge: labelNames.value.length, disabled: labelNames.value.length === 0,
    tip: labelNames.value.length ? 'Delete specific segmentations and their measurements' : 'No label set is registered yet' },
  { value: 'analysis', label: 'All analysis', icon: 'pi pi-eraser',
    tip: 'Delete everything derived, keeping the images themselves' },
])

// ── Versions scope ────────────────────────────────────────────────────────────
// Pre-selects every NON-ACTIVE version (Decision 10): once a corrected version exists, the raw import
// and the intermediates are what you no longer need. Nothing is deleted without the confirm, so a
// pre-selection is a suggestion. Single-image selections know their own active version; a multi-image
// selection falls back to `default` as the anchor.
const activeOf = (img: CciaImage) => img.activeValueName || DEFAULT_VALUE_NAME
const commonActive = computed(() => {
  const first = props.images[0] ? activeOf(props.images[0]) : DEFAULT_VALUE_NAME
  return props.images.every(i => activeOf(i) === first) ? first : DEFAULT_VALUE_NAME
})
const pickedVersions = ref<string[]>([])
const newActive      = ref('')
function seedVersions() {
  pickedVersions.value = versionNames.value.filter(v => v !== commonActive.value)
  newActive.value = resolveNewActive(versionNames.value, pickedVersions.value, commonActive.value)
}
// re-resolve the active picker whenever the version selection changes, so it can never name a doomed one
function onVersionsChange(v: string[]) {
  pickedVersions.value = v
  newActive.value = resolveNewActive(versionNames.value, v, newActive.value || commonActive.value)
}
const versionOptions = computed<ChipOption[]>(() => versions.value.map(nameChip))
// The "keep active" candidates, badged like the version chips so it's visible which survive everywhere
const survivors = computed(() => survivorCounts(props.images, pickedVersions.value))
const survivorOptions = computed<ChipOption[]>(() => survivors.value.map(nameChip))
// BLOCKING: an image that keeps a version but not the CHOSEN one. Falling back per image would look
// like it worked while leaving that image on something the user didn't pick, so the confirm is greyed
// until the choice is one those images can take (or the selection narrows).
const activeConflicts = computed(() =>
  activeMismatches(props.images, pickedVersions.value, newActive.value))
// NON-BLOCKING: names that simply aren't on every selected image and get skipped there
const partialVersions = computed(() => partialNames(versions.value, n.value))
const partialLabels   = computed(() => partialNames(labels.value, n.value))
// Un-import is per IMAGE, not per selection: with a union list, taking `default` can strip one image
// of everything while another still has its corrected version. Count the images it happens to, so the
// warning is true rather than all-or-nothing.
const unimportCount = computed(() => props.images.filter(img =>
  unimportsImage(Object.keys(img.filepaths ?? {}), pickedVersions.value)).length)
const anySurvivor = computed(() => unimportCount.value < n.value)

const pickedLabels = ref<string[]>([])

// ── Confirm ───────────────────────────────────────────────────────────────────
const canConfirm = computed(() =>
  scope.value === 'images'   ? n.value > 0
  : scope.value === 'analysis' ? n.value > 0
  // versions: the active-version conflict blocks; a skipped name does not
  : scope.value === 'versions' ? pickedVersions.value.length > 0 && activeConflicts.value === 0
  : pickedLabels.value.length > 0)

const confirmLabel = computed(() => {
  if (scope.value === 'images')   return n.value === 1 ? 'Delete image' : `Delete ${n.value} images`
  if (scope.value === 'analysis') return n.value === 1 ? 'Delete analysis' : `Delete analysis of ${n.value} images`
  if (scope.value === 'versions') return `Delete ${pickedVersions.value.length} version(s)`
  return `Delete ${pickedLabels.value.length} label set(s)`
})

function submit() {
  if (!canConfirm.value) return
  const plan: DeletePlan =
    scope.value === 'versions' ? { scope: 'versions', valueNames: [...pickedVersions.value], newActive: newActive.value }
    : scope.value === 'labels' ? { scope: 'labels', valueNames: [...pickedLabels.value] }
    : { scope: scope.value }
  emit('confirm', plan)
  emit('close')
}

function onScopeChange(v: string) {
  scope.value = v as Scope
  if (v === 'versions') seedVersions()
  if (v === 'labels')   pickedLabels.value = []
}
</script>

<template>
  <BaseModal width="540px" @close="$emit('close')">
    <template #title><i class="pi pi-trash" /> Delete — {{ what }}</template>

    <div class="del-row">
      <span class="del-lbl cc-muted" v-tooltip.right="'What to delete from the selected images'">Delete</span>
      <ChipSelect class="del-chips" variant="segmented" :options="scopeOptions"
        :model-value="scope" @update:model-value="v => onScopeChange(v as string)"
        v-tooltip.right="'Select what to delete — the scopes are deliberately separate'" />
    </div>

    <!-- Whole images -->
    <p v-if="scope === 'images'" class="del-note cc-muted">
      Deletes the image stores and everything derived — segmentations, measurements, populations,
      gating. The original microscope files are not touched.
    </p>

    <!-- Versions -->
    <template v-else-if="scope === 'versions'">
      <div class="del-row">
        <span class="del-lbl cc-muted" v-tooltip.right="'Image versions to delete from disk'">Versions</span>
        <ChipSelect class="del-chips" multiple :options="versionOptions"
          :model-value="pickedVersions" @update:model-value="v => onVersionsChange(v as string[])"
          v-tooltip.right="'A version only some images have is skipped for the rest'" />
      </div>
      <div v-if="anySurvivor" class="del-row">
        <span class="del-lbl cc-muted"
          v-tooltip.right="'Stays active where it survives; each image falls back to its own'">Keep active</span>
        <ChipSelect class="del-chips" :options="survivorOptions" :model-value="newActive"
          @update:model-value="v => newActive = v as string"
          v-tooltip.right="'Which surviving version becomes the active one'" />
      </div>
      <!-- BLOCKS: an image keeps a version, but not the one chosen to stay active -->
      <p v-if="activeConflicts > 0" class="del-note cc-muted-warn">
        {{ activeConflicts }} of {{ n }} image(s) do not have "{{ newActive }}" to keep active. Select a
        version they all share, or delete from fewer images.
      </p>
      <p v-if="partialVersions.length" class="del-note cc-muted">
        Skipped where absent: {{ partialVersions.join(', ') }}.
      </p>
      <p v-if="unimportCount > 0" class="del-note cc-muted-warn">
        {{ unimportCount }} of {{ n }} image(s) lose every version and become un-imported — their
        analysis stays, but nothing can be viewed or re-run until they are imported again.
      </p>
      <p v-if="activeConflicts === 0 && unimportCount === 0" class="del-note cc-muted">
        Frees the deleted versions on disk. Analysis is not touched — Settings → Storage reports the bytes.
      </p>
    </template>

    <!-- Label sets -->
    <template v-else-if="scope === 'labels'">
      <div class="del-row">
        <span class="del-lbl cc-muted" v-tooltip.right="'Segmentations to delete, with their measurements'">Label sets</span>
        <ChipSelect class="del-chips" multiple :options="labels.map(nameChip)"
          :model-value="pickedLabels" @update:model-value="v => pickedLabels = v as string[]"
          v-tooltip.right="'A set only some images have is skipped for the rest'" />
      </div>
      <!-- Warns but never blocks: a set only some images have is simply skipped for the rest -->
      <p v-if="partialLabels.length" class="del-note cc-muted-warn">
        Not on every image, skipped where absent: {{ partialLabels.join(', ') }}.
      </p>
      <p class="del-note cc-muted">
        Deletes each set's labels, measurements, tracks and skeleton output. Gating strategies are kept —
        re-run the segmentation under the same name and they apply again.
      </p>
    </template>

    <!-- All analysis -->
    <p v-else class="del-note cc-muted">
      Deletes segmentations, measurements, populations, clustering and spatial output. The images and
      their versions stay, so the pipeline can be re-run from clean. Gating strategies and the run
      history are kept.
    </p>

    <template #footer>
      <button class="cc-btn cc-btn-ghost" @click="$emit('close')">Cancel</button>
      <ConfirmButton @confirm="submit" v-slot="{ armed, arm, confirm, cancel }">
        <button v-if="!armed" class="cc-btn cc-btn-danger" :disabled="!canConfirm" @click="arm"
          v-tooltip.top="'Deletion cannot be undone'">
          <i class="pi pi-trash" /> {{ confirmLabel }}
        </button>
        <template v-else>
          <button class="cc-btn cc-btn-danger" @click="confirm">
            <i class="pi pi-exclamation-triangle" /> Click to confirm — {{ confirmLabel }}
          </button>
          <button class="cc-btn cc-btn-ghost" @click="cancel">Cancel</button>
        </template>
      </ConfirmButton>
    </template>
  </BaseModal>
</template>

<style scoped>
.del-row { display: flex; align-items: flex-start; gap: 0.6rem; margin-bottom: 0.6rem; }
.del-lbl { width: 5.5rem; flex-shrink: 0; padding-top: 0.2rem; }
.del-chips { flex: 1 1 auto; min-width: 0; }
/* colour + size come from .cc-muted / .cc-muted-warn; only spacing is local */
.del-note { margin: 0.2rem 0 0; }
</style>
