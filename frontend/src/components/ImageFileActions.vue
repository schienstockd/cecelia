<!--
  ImageFileActions — the standard file operations on the image SELECTION (copy / move / delete), sitting
  in the Import page's action bar next to "Add images".

  These are file-manager operations, not analysis: they belong on the page where images are curated, and
  they apply to every checked image at once. They used to be per-row — a Move item and a Copy item in the
  ⋯ menu plus a delete ✕ at the far end of each row — which meant crowding that menu and doing one image
  at a time. Mount this ONLY from the Manage images page (`ManageImagesModule.vue`); other module pages deliberately
  have no way to delete or re-file an image.

  Props: `setUid` (the source set) + `uids` (the current selection). Emits `done` after any operation so
  the host can clear the selection (the rows are gone or have moved).
-->
<script setup lang="ts">
import { ref, computed } from 'vue'
import { useToast } from 'primevue/usetoast'
import BaseModal from './BaseModal.vue'
import CopyDialog from './CopyDialog.vue'
import DeleteImagesDialog, { type DeletePlan } from './DeleteImagesDialog.vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useLogStore } from '../stores/log'
import { isImported } from '../utils/inclusion'
import { orderDefaultLast, resolveNewActive, DEFAULT_VALUE_NAME } from '../utils/imageDelete'
import { resolveSetDestination, destinationParams } from '../utils/setDestination'
import type { CciaImage } from '../stores/project'

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
  if (!dest.ok) { log.warn(dest.error, { source: 'manageImages' }); return }
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
    log.info(`Moved ${moved} image(s) to "${toName}".`, { source: 'manageImages' })
    toast.add({ severity: 'success', summary: 'Moved', life: 2500,
                detail: `${moved} image(s) → ${toName}` })
  } catch (e) {
    log.error(`Failed to move image (${moved} moved): ${e instanceof Error ? e.message : String(e)}`,
      { source: 'manageImages' })
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
// The button opens `DeleteImagesDialog`, which collects a PLAN; this executes it. Four scopes, four
// routes, ONE loop carrying the k/N readout + toast — so progress reporting is written once
// (docs/UI.md → File operations) and the modal only decides what should happen.
//
// Whole-image delete is a real delete: `delete_image!` (app/src/model/set.jl) rm -r's both
// {proj}/0/{uid} (every image store) and {proj}/1/{uid} (labels, labelProps, gating, …). Only the
// original microscope file, which lives outside the project, survives.
const showDelete = ref(false)

async function post(url: string, body: unknown): Promise<Record<string, any>> {
  const res = await fetch(url, {
    method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body),
  })
  const parsed = await res.json().catch(() => ({})) as { error?: string }
  if (!res.ok) throw new Error(parsed.error ?? `HTTP ${res.status}`)
  return parsed as Record<string, any>
}

// One loop for the three per-image scopes; `step` is whatever that scope does to one image.
async function runPerImage(verb: string, summary: string,
                           step: (img: CciaImage, projectUid: string) => Promise<void>) {
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  const targets = [...images.value]   // snapshot: deleting an image mutates the set as we go
  let done = 0
  busy.value = { verb, done: 0, total: targets.length }
  try {
    for (const img of targets) {
      await step(img, projectUid)
      done++
      busy.value = { verb, done, total: targets.length }
    }
    log.info(`${summary} for ${done} image(s).`, { source: 'manageImages' })
    toast.add({ severity: 'success', summary: 'Deleted', life: 2500,
                detail: `${summary} — ${done} image(s)` })
  } catch (e) {
    log.error(`${verb} failed after ${done} image(s): ${e instanceof Error ? e.message : String(e)}`,
      { source: 'manageImages' })
    toast.add({ severity: 'error', summary: 'Delete failed', life: 4000,
                detail: `${done} of ${targets.length} done — see the log` })
  } finally {
    busy.value = null
    emit('done')
  }
}

async function runPlan(plan: DeletePlan) {
  if (busy.value) return

  if (plan.scope === 'images') {
    await runPerImage('Deleting', 'Deleted images and their analysis', async (img, projectUid) => {
      await post('/api/images/delete', { projectUid, setUid: props.setUid, imageUid: img.uid })
      project.deleteImage(props.setUid, img.uid)
    })
    return
  }

  if (plan.scope === 'versions') {
    // `default` LAST so the safe-primary un-import lands at the end of this image's loop rather than
    // mid-way (docs/todo/IMAGE_DELETE_PLAN.md Decision 11).
    const ordered = orderDefaultLast(plan.valueNames)
    await runPerImage('Deleting versions', 'Deleted image versions', async (img, projectUid) => {
      const own = Object.keys(img.filepaths ?? {})
      // The modal BLOCKS the case where a surviving image lacks the chosen active version, so this
      // resolve is a guard rather than a policy: it still runs because the store can have moved since
      // the modal opened, and because an image that loses every version has no active to set at all.
      // Never send a name this image doesn't have — `_active` would point at something unregistered.
      const newDefault = resolveNewActive(own, plan.valueNames, plan.newActive,
                                          img.activeValueName ?? '') || DEFAULT_VALUE_NAME
      for (const valueName of ordered) {
        if (!(img.filepaths ?? {})[valueName]) continue     // not on this image → skip it
        const body = await post('/api/images/version/remove',
          { projectUid, imageUid: img.uid, valueName, newDefault })
        if (body.image) project.updateImageMeta(img.uid, body.image as Partial<CciaImage>)
      }
    })
    return
  }

  if (plan.scope === 'labels') {
    await runPerImage('Deleting label sets', 'Deleted label sets', async (img, projectUid) => {
      for (const valueName of plan.valueNames) {
        if (!(img.labels ?? {})[valueName]) continue
        const body = await post('/api/images/labels/delete', { projectUid, imageUid: img.uid, valueName })
        if (body.image) project.updateImageMeta(img.uid, body.image as Partial<CciaImage>)
        else            project.removeLabelSet(img.uid, valueName)
      }
    })
    return
  }

  // analysis: ONE bulk request (the route takes imageUids), so there is no per-image step to count
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  const targets = [...images.value]
  busy.value = { verb: 'Deleting analysis', done: 0, total: targets.length }
  try {
    const body = await post('/api/images/analysis/reset',
      { projectUid, imageUids: targets.map(i => i.uid) })
    for (const [uid, image] of Object.entries(body.images ?? {})) {
      project.updateImageMeta(uid, image as Partial<CciaImage>)
    }
    busy.value = { verb: 'Deleting analysis', done: targets.length, total: targets.length }
    log.info(`Deleted the analysis of ${targets.length} image(s).`, { source: 'manageImages' })
    toast.add({ severity: 'success', summary: 'Deleted', life: 2500,
                detail: `Analysis of ${targets.length} image(s)` })
  } catch (e) {
    log.error(`Failed to delete analysis: ${e instanceof Error ? e.message : String(e)}`,
      { source: 'manageImages' })
    toast.add({ severity: 'error', summary: 'Delete failed', life: 4000, detail: 'See the log' })
  } finally {
    busy.value = null
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

    <!-- opens the structured modal; the confirm lives on its footer, so this is never one click from
         a deletion (docs/todo/IMAGE_DELETE_PLAN.md Decision 1) -->
    <button class="cc-btn cc-btn-danger-ghost" :disabled="n === 0 || converting || !!busy"
      @click="showDelete = true"
      v-tooltip.bottom="n === 0 ? 'Select images to delete'
        : converting ? 'Wait for the import to finish'
        : 'Delete images, versions, label sets or analysis'">
      <i class="pi pi-trash" /> Delete
    </button>

    <!-- k/N while a move/delete loop runs; Copy reports through the task rail instead -->
    <span v-if="busy" class="cc-readout cc-fs-xs busy-readout"
      v-tooltip.bottom="'Working through the selection one image at a time'">
      <i class="pi pi-spin pi-spinner" /> {{ busyText }}
    </span>
  </span>

  <DeleteImagesDialog v-if="showDelete && n > 0" :images="images"
    @confirm="runPlan" @close="showDelete = false" />

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
        <i v-if="moving" class="pi pi-spin pi-spinner" /><i v-else class="pi pi-arrows-h" /> Move
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
