<!--
  ImageFileActions — the standard file operations on the image SELECTION (copy / move / delete), sitting
  in the Import page's action bar next to "Add images".

  These are file-manager operations, not analysis: they belong on the page where images are curated, and
  they apply to every checked image at once. They used to be per-row — a Move item and a Copy item in the
  ⋯ menu plus a delete ✕ at the far end of each row — which meant crowding that menu and doing one image
  at a time. Mount this ONLY from the Import page (`ImportModule.vue`); other module pages deliberately
  have no way to delete or re-file an image.

  Props: `setUid` (the source set) + `uids` (the current selection). Emits `done` after any operation so
  the host can clear the selection (the rows are gone or have moved).
-->
<script setup lang="ts">
import { ref, computed } from 'vue'
import { useToast } from 'primevue/usetoast'
import BaseModal from './BaseModal.vue'
import ConfirmDeleteButton from './ConfirmDeleteButton.vue'
import CopyDialog from './CopyDialog.vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useLogStore } from '../stores/log'
import { isImported } from '../utils/inclusion'
import { resolveSetDestination, destinationParams } from '../utils/setDestination'

const props = defineProps<{ setUid: string; uids: string[] }>()
const emit  = defineEmits<{ (e: 'done'): void }>()

const project     = useProjectStore()
const projectMeta = useProjectMetaStore()
const log         = useLogStore()
const toast       = useToast()

// Move and Delete are plain per-image HTTP loops, NOT task-rail jobs (Copy is — it dispatches
// editImages.copyImage, so it gets the console + progress bar + toast for free). They therefore have
// to report for themselves: `busy` drives a k/N readout beside the buttons while the loop runs, and a
// toast lands at the end. Deleting a multi-GB zarr is seconds per image, so "nothing is happening"
// would otherwise be indistinguishable from a hang — the rows vanishing one by one was the only cue.
const busy = ref<{ verb: string; done: number; total: number } | null>(null)
const busyText = computed(() =>
  busy.value ? `${busy.value.verb} ${busy.value.done}/${busy.value.total}…` : '')

// The selected images, resolved against the source set (a uid whose row has gone is simply dropped).
const images = computed(() => {
  const all = project.sets.find(s => s.uid === props.setUid)?.images ?? []
  const keep = new Set(props.uids)
  return all.filter(i => keep.has(i.uid))
})
const n = computed(() => images.value.length)
const converting = computed(() => images.value.some(i => i.status === 'converting'))
const allImported = computed(() => n.value > 0 && images.value.every(isImported))

// ── Copy ──────────────────────────────────────────────────────────────────────
// Duplicates data on disk (one editImages.copyImage task per image) — the dialog owns the version +
// destination pickers and the dispatch.
const showCopy = ref(false)

// ── Move ──────────────────────────────────────────────────────────────────────
// Manifest-only on the backend — no image data moves on disk (see /api/images/move). The picker offers
// every OTHER set plus "New set…"; the destination itself is resolved by the shared helper.
const showMove      = ref(false)
const moveTargetUid = ref('')
const moveNewName   = ref('')
const moving        = ref(false)
const otherSets     = computed(() => project.sets.filter(s => s.uid !== props.setUid))

function openMove() {
  moveTargetUid.value = otherSets.value[0]?.uid ?? ''   // default to the first other set, else new-set mode
  moveNewName.value   = ''
  showMove.value      = true
}

async function doMove() {
  if (moving.value) return
  const dest = resolveSetDestination(project.sets, moveTargetUid.value, moveNewName.value)
  if (!dest.ok) { log.warn(dest.error, { source: 'import' }); return }
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  moving.value = true
  // The route moves ONE image, so this walks the selection. In new-set mode only the first call may
  // create the set; from then on we address it by uid, so a name typo can't fan out into two sets.
  // Snapshot the targets first — each moveImage() shrinks the source set, so `images` is recomputed
  // under us mid-loop.
  const targets = [...images.value]
  let params = destinationParams(dest)
  let moved = 0
  let toName = dest.toSetUid
    ? (project.sets.find(s => s.uid === dest.toSetUid)?.name ?? '')
    : dest.newSetName!
  busy.value = { verb: 'Moving', done: 0, total: targets.length }
  try {
    for (const img of targets) {
      const res = await fetch('/api/images/move', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectUid, imageUid: img.uid, fromSetUid: props.setUid, ...params }),
      })
      const body = await res.json().catch(() => ({})) as
        { toSetUid?: string; toSetName?: string; error?: string }
      if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
      toName = body.toSetName ?? toName
      project.ensureSet(body.toSetUid!, toName)     // no-op if it already existed
      project.moveImage(props.setUid, body.toSetUid!, img.uid)
      params = { toSetUid: body.toSetUid! }
      moved++
      busy.value = { verb: 'Moving', done: moved, total: targets.length }
    }
    log.info(`Moved ${moved} image(s) to "${toName}".`, { source: 'import' })
    toast.add({ severity: 'success', summary: 'Moved', life: 2500,
                detail: `${moved} image(s) → ${toName}` })
  } catch (e) {
    log.error(`Failed to move image (${moved} moved): ${e instanceof Error ? e.message : String(e)}`,
      { source: 'import' })
    toast.add({ severity: 'error', summary: 'Move failed', life: 4000,
                detail: `${moved} of ${targets.length} moved — see the log` })
  } finally {
    busy.value = null
    moving.value = false
    showMove.value = false
    emit('done')
  }
}

// ── Delete ────────────────────────────────────────────────────────────────────
// DELETE, not remove: `delete_image!` (app/src/model/set.jl) rm -r's both {proj}/0/{uid} (the converted
// OME-ZARR) and {proj}/1/{uid} (labels, labelProps, gating sidecars). Only the original microscope file,
// which lives outside the project, survives. The copy has to say so — the old per-row ✕ called it
// "Remove … the original file is not deleted", which is true and reads as though nothing is lost.
const deleting = ref(false)

async function doDelete() {
  if (deleting.value) return
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  deleting.value = true
  const targets = [...images.value]     // snapshot: deleteImage() shrinks the set as we go
  let done = 0
  busy.value = { verb: 'Deleting', done: 0, total: targets.length }
  try {
    for (const img of targets) {
      const res = await fetch('/api/images/delete', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectUid, setUid: props.setUid, imageUid: img.uid }),
      })
      const body = await res.json().catch(() => ({})) as { error?: string }
      if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
      project.deleteImage(props.setUid, img.uid)
      done++
      busy.value = { verb: 'Deleting', done, total: targets.length }
    }
    log.info(`Deleted ${done} image(s) and their analysis.`, { source: 'import' })
    toast.add({ severity: 'success', summary: 'Deleted', life: 2500,
                detail: `${done} image(s) and their analysis` })
  } catch (e) {
    log.error(`Failed to delete image (${done} deleted): ${e instanceof Error ? e.message : String(e)}`,
      { source: 'import' })
    toast.add({ severity: 'error', summary: 'Delete failed', life: 4000,
                detail: `${done} of ${targets.length} deleted — see the log` })
  } finally {
    busy.value = null
    deleting.value = false
    emit('done')
  }
}
</script>

<template>
  <span class="file-actions">
    <button class="cc-btn" :disabled="!allImported || !!busy"
      @click="showCopy = true"
      v-tooltip.bottom="n === 0 ? 'Select images to copy'
        : allImported ? 'Copy the selected images into a new or existing set'
        : 'Import the selected images first'">
      <i class="pi pi-copy" /> Copy
    </button>

    <button class="cc-btn" :disabled="n === 0 || !!busy"
      @click="openMove"
      v-tooltip.bottom="n === 0 ? 'Select images to move'
        : 'Move the selected images to another set (no data is copied)'">
      <i class="pi pi-arrows-h" /> Move
    </button>

    <ConfirmDeleteButton :disabled="n === 0 || converting || !!busy"
      :title="converting ? 'Wait for the import to finish'
        : 'Delete the selected images and their analysis (source files are kept)'"
      armed-title="Click again to delete — cannot be undone"
      @confirm="doDelete">
      Delete
    </ConfirmDeleteButton>

    <!-- k/N while a move/delete loop runs; Copy reports through the task rail instead -->
    <span v-if="busy" class="cc-readout cc-fs-xs busy-readout"
      v-tooltip.bottom="'Working through the selection one image at a time'">
      <i class="pi pi-spin pi-spinner" /> {{ busyText }}
    </span>
  </span>

  <!-- no `done` here: a copy leaves the source rows in place, so the selection stays valid -->
  <CopyDialog v-if="showCopy && n > 0" :images="images" :set-uid="setUid" @close="showCopy = false" />

  <BaseModal v-if="showMove" width="420px" @close="showMove = false">
    <template #title>
      <i class="pi pi-arrows-h" /> Move {{ n }} image(s)
    </template>

    <div class="move-row">
      <span class="move-lbl cc-muted"
        v-tooltip.right="'Destination set — only set membership changes, no data is copied'">To set</span>
      <select v-model="moveTargetUid" class="move-select" v-tooltip.right="'Set the images are moved to'">
        <option v-for="s in otherSets" :key="s.uid" :value="s.uid">{{ s.name }}</option>
        <option value="">＋ New set…</option>
      </select>
    </div>
    <div v-if="!moveTargetUid" class="move-row">
      <span class="move-lbl cc-muted" />
      <input class="move-name-input" v-model="moveNewName" placeholder="New set name…"
        v-tooltip.right="'Name for the new set'" @keydown.enter="doMove" autofocus />
    </div>

    <template #footer>
      <button class="cc-btn cc-btn-ghost" @click="showMove = false">Cancel</button>
      <button class="cc-btn cc-btn-primary" :disabled="moving" @click="doMove">
        <i v-if="moving" class="pi pi-spin pi-cog" /><i v-else class="pi pi-arrows-h" /> Move
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
/* laid out as if the buttons were direct children of the action bar */
.file-actions { display: contents; }

.busy-readout { display: inline-flex; align-items: center; gap: 0.35rem; }   /* + .cc-readout (colour/size) */

.move-row { display: flex; align-items: center; gap: 0.6rem; margin-bottom: 0.55rem; }
.move-lbl { width: 4rem; flex-shrink: 0; }
.move-select, .move-name-input { flex: 1 1 auto; }
</style>
