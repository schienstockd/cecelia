<!--
  Animation controls — the side panel of /animation: what to capture, how the mp4 comes out, and Render.
  The timeline it acts on is the module page's canvas (AnimationTimeline.vue).

  Same two-half arrangement as the batch-movies panel: the controls as the top half, this module's task
  list as the bottom, sharing `PaneExpandBar`. Every option is inline — the render options used to sit
  behind a gear popover (`MovieOptionsButton`, still the viewer recorder's), which made sense on a page
  with no side panel and is redundant now there is one (Dominik, 2026-08-10).
-->
<script setup lang="ts">
import { computed, ref, nextTick, watch } from 'vue'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useProjectStore } from '../../stores/project'
import { useSettingsStore } from '../../stores/settings'
import { useAnimationStore, type AnimSnapshot } from '../../stores/animation'
import { useTaskStore } from '../../stores/tasks'
import { useWsStore } from '../../stores/ws'
import { useLogStore } from '../../stores/log'
import { buildTitleCard, unionViewSnapshot, applyViewState, type TitleCardPayload } from '../../utils/napariOverlays'
import { framesFor, activeAnimationUid } from '../../utils/animationTimeline'
import { movieSizeParams } from '../../utils/movieSize'
import { keyframeRestore, restoreNote, restoreTargetSet, type MovieRegistryEntry } from '../../utils/movieRestore'
import { useMovieRestore } from '../../composables/useMovieRestore'
import { useNapariStatus } from '../../composables/useNapariStatus'
import { useNapariOpen } from '../../composables/useNapariOpen'
import { usePaneExpand } from '../../composables/usePaneExpand'
import CcToggle from '../../components/CcToggle.vue'
import MovieOutputControls from '../../components/MovieOutputControls.vue'
import TitleCardControls from '../../components/TitleCardControls.vue'
import RestoreNotice from '../../components/RestoreNotice.vue'
import PaneExpandBar from '../../components/PaneExpandBar.vue'
import TaskList from '../../tasks/TaskList.vue'
import { useMovieSuffixes } from '../../composables/useMovieSuffixes'

const props = defineProps<{ selectedUids: string[]; setUid?: string }>()

const projectMeta = useProjectMetaStore()

// Suffixes already used in this project, offered in the recorder's "name" field. Lazily fetched and
// cached across the three recorder panels — see composables/useMovieSuffixes.ts.
const { suffixes: movieSuffixes, ensure: ensureMovieSuffixes } = useMovieSuffixes()
watch(() => projectMeta.current?.uid ?? '', (uid: string) => { void ensureMovieSuffixes(uid) }, { immediate: true })
const project = useProjectStore()
const settings = useSettingsStore()
const anim = useAnimationStore()
const tasks = useTaskStore()
const ws = useWsStore()
const log = useLogStore()
// the canvas size napari would record at, for the size fields' placeholder (shared poll)
const { canvasSizeX, canvasSizeY } = useNapariStatus()
const { openInNapari } = useNapariOpen()

const projectUid = computed(() => projectMeta.current?.uid ?? '')
// The image this page is working on: the table's selection, falling back to whatever napari has open.
const activeUid = computed(() => activeAnimationUid(props.selectedUids, project.napariImageUid))
const activeImage = computed(() => (activeUid.value ? project.imageByUid(activeUid.value) : null))
const imageName = (uid: string) => project.imageByUid(uid)?.name ?? uid
const frames = computed(() => framesFor(anim.snapshots, activeUid.value))
const selected = computed(() => frames.value.find(f => f.id === anim.selectedId) ?? null)

// Capture screenshots the LIVE viewer, so the page's image has to be the one napari is showing. The
// table's eye opens it; this button is the same call, within reach of the controls that need it.
const isOpen = computed(() => !!activeUid.value && activeUid.value === project.napariImageUid)
const opening = ref(false)
async function openActive() {
  const setUid = props.setUid ?? (activeUid.value ? project.setUidOfImage(activeUid.value) : null)
  if (!activeUid.value || !setUid || opening.value) return
  opening.value = true
  try { await openInNapari(activeUid.value, setUid) } finally { opening.value = false }
}

const capturing = ref(false)
const updating = ref(false)
const rendering = ref(false)

/** One screenshot of the live viewer → the sidecar PNG id + the view state behind it. */
async function screenshot(what: string) {
  const res = await fetch('/api/napari/screenshot', {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ projectUid: projectUid.value }),
  })
  if (!res.ok) {
    log.error(`${what} failed: ${(await res.json().catch(() => ({}))).error ?? res.status}`, { source: 'napari' })
    return null
  }
  return (await res.json()) as { assetId?: string; viewState?: Record<string, unknown>; imageUid?: string }
}

// capture the CURRENT napari view as a new keyframe (a base "look")
async function capture() {
  if (!isOpen.value || !projectUid.value || capturing.value) return
  capturing.value = true
  try {
    const j = await screenshot('Capture')
    if (!j) return
    const uid = j.imageUid ?? activeUid.value
    anim.add({ id: crypto.randomUUID(), assetId: j.assetId, snapshot: j.viewState,
               original: JSON.parse(JSON.stringify(j.viewState ?? {})),   // reset target
               imageUid: uid, imageName: imageName(uid), duration: 1 })
  } catch (e) {
    log.error(`Capture failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'napari' })
  } finally { capturing.value = false }
}

// add a keyframe by duplicating the last one (a copy to vary via the rows — no re-capture needed)
function addKeyframe() {
  const last = frames.value[frames.value.length - 1]
  if (!last) { capture(); return }   // nothing yet → capture a base
  const copy = JSON.parse(JSON.stringify(last.snapshot ?? {}))
  anim.add({
    id: crypto.randomUUID(), assetId: last.assetId, imageUid: last.imageUid, imageName: last.imageName,
    duration: last.duration ?? 1,
    snapshot: copy,
    original: JSON.parse(JSON.stringify(copy)),   // baseline = what it starts as; reset returns here
  })
}

// Update the selected keyframe FROM the current napari view — re-screenshot and replace its snapshot +
// thumbnail (and reset its baseline). This is how you "change" a snapshot: sync it, tweak in napari, save.
async function updateSelected() {
  const sel = selected.value
  if (!sel || !isOpen.value || !projectUid.value || updating.value) return
  updating.value = true
  try {
    const j = await screenshot('Update')
    if (!j) return
    const oldAsset = sel.assetId
    sel.snapshot = j.viewState
    sel.original = JSON.parse(JSON.stringify(j.viewState ?? {}))   // new baseline (no longer "edited")
    sel.assetId = j.assetId
    if (oldAsset && oldAsset !== j.assetId && !anim.snapshots.some(o => o.assetId === oldAsset)) {
      fetch('/api/board-assets/delete', {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectUid: projectUid.value, assetId: oldAsset }),
      }).catch(() => {})
    }
  } catch (e) {
    log.error(`Update failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'napari' })
  } finally { updating.value = false }
}

// toggling Sync on immediately mirrors the selected keyframe into napari
function onToggleSync(on: boolean) {
  settings.animationSyncNapari = on
  if (on && selected.value?.snapshot) applyViewState(selected.value.snapshot)
}

const canRender = computed(() => !!activeUid.value && frames.value.length >= 2 && !rendering.value)
async function render() {
  if (!canRender.value) return
  const uid = activeUid.value
  rendering.value = true
  log.info('Rendering animation… (this can take a moment)', { source: 'napari' })
  try {
    const keyframes = frames.value.map(f => ({
      viewState: f.snapshot,
      steps: Math.max(1, Math.round((f.duration ?? 1) * anim.fps)),
    }))
    // What the EDITOR needs back and the recorder does not (Phase 6, utils/movieRestore.ts): the
    // thumbnail, the title, and the duration in seconds rather than the whole frames it renders at. A
    // parallel array in the same order — banked with the movie, ignored by the recorder — so restoring
    // does not mean storing every view state twice.
    const keyframeMeta = frames.value.map(f => ({
      assetId: f.assetId, title: f.title, duration: f.duration ?? 1,
    }))
    // Title card (Phase H4): describe everything shown "at some point" across the animation — build from
    // a UNION of all keyframes' views (channels + overlays merged), via the SHARED buildTitleCard. It
    // includes the Channels section itself (from the union), since the recorder can't reconstruct the
    // union from one live view.
    let titleCard: TitleCardPayload | undefined
    if (anim.titleCard.enabled && frames.value.length) {
      const setUid   = project.setUidOfImage(uid) ?? ''
      const colourBy = setUid ? settings.getColourBy(setUid) : ''
      const overrides = (setUid && colourBy) ? settings.getColourOverrides(setUid, colourBy) : {}
      const union = unionViewSnapshot(frames.value.map(f => f.snapshot as { layers?: Record<string, unknown> } | undefined))
      titleCard = await buildTitleCard(projectUid.value, uid, union, activeImage.value,
        { note: anim.titleCard.note, durationSec: anim.titleCard.durationSec, colourBy, colourOverrides: overrides, includeChannels: true })
    }
    // Over the task rail (`movie:record` with keyframes), like the viewer's Record and the batch: the
    // render shows up in the task list with a progress bar and a Cancel instead of blocking here.
    const t = tasks.add({
      module: 'animation', label: `Render ${activeImage.value?.name ?? 'animation'}`,
      imageUid: uid, imageName: activeImage.value?.name ?? '', status: 'queued',
      taskName: 'movie.animation', funName: 'movie.animation', params: {}, projectUid: projectUid.value,
    })
    ws.send({
      type: 'movie:record', taskId: t.id, projectUid: projectUid.value, imageUid: uid,
      keyframes, keyframeMeta, fps: anim.fps, suffix: anim.suffix, titleCard,
      apiUrl: window.location.origin,
      ...movieSizeParams(anim.sizeX, anim.sizeY),
    })
  } catch (e) {
    log.error(`Render failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'napari' })
  } finally { rendering.value = false }
}

// ── Editing a rendered animation's timeline (Phase 6, docs/todo/MOVIE_MANAGEMENT_PLAN.md) ──────
// Arriving from the Movies page with `?fromMovie=…`. Only ONE image's keyframes are replaced — a
// timeline is per-image, so replacing the whole store would take out the timelines of images this
// movie has nothing to do with.
//
// Written to the store's refs directly rather than through `load()`: that one exists for hydrating from
// the project-open response and deliberately suppresses the autosave, which is right there and wrong
// here. A restored timeline IS the working one, so it has to persist like any other edit.
const { notice: restoreNotice, undo: undoRestore, dismiss: dismissRestore } = useMovieRestore({
  kind: 'keyframes',
  projectUid: () => projectUid.value,
  onError: m => log.error(m, { source: 'movies' }),
  apply: (entry: MovieRegistryEntry) => {
    const r = keyframeRestore(entry.config)
    if (!r) return null
    // An animation from before the image was banked attaches to whatever the page is on — there is no
    // third option, and refusing would make an old movie permanently uneditable.
    const uid = r.imageUid || activeUid.value || ''
    if (!uid) return null

    const prevFrames = anim.snapshots
    const prevOut = { fps: anim.fps, sizeX: anim.sizeX, sizeY: anim.sizeY,
                      suffix: anim.suffix, titleCard: { ...anim.titleCard } }
    // Fresh ids and a fresh baseline: these are new editable keyframes, not the originals, and reusing
    // an id would collide with a keyframe of the same animation that is still in the store.
    const restored: AnimSnapshot[] = r.frames.map(f => ({
      id: crypto.randomUUID(),
      assetId: f.assetId,
      snapshot: JSON.parse(JSON.stringify(f.viewState)),
      original: JSON.parse(JSON.stringify(f.viewState)),
      imageUid: uid, imageName: imageName(uid),
      title: f.title, duration: f.duration,
    }))
    anim.snapshots = [...anim.snapshots.filter(s => s.imageUid !== uid), ...restored]
    if (r.output.fps !== undefined) anim.fps = r.output.fps
    if (r.output.sizeX !== undefined) anim.sizeX = r.output.sizeX
    if (r.output.sizeY !== undefined) anim.sizeY = r.output.sizeY
    if (r.output.suffix !== undefined) anim.suffix = r.output.suffix ?? ''
    if (r.output.titleCard !== undefined) anim.titleCard = r.output.titleCard

    // The page shows ONE image's keyframes, so restoring one whose image isn't selected looks like
    // nothing happened. So SELECT it — and switch to its set when that is what it takes.
    //
    // The set comes from the IMAGE (`setUidOfImage`), never from whatever set happens to be active.
    // Checking the active set instead is what made this report "switch to its set to see it" and leave
    // the user to do it (Dominik, 2026-08-10) — the same failure whether the image really is in another
    // set or the page simply had none active yet. A restore is one click; it repairs what it can rather
    // than handing back an instruction.
    //
    // Selection written on the NEXT tick: arriving here is a navigation, and `ImageTable` seeds its
    // checkboxes from this same store slot `onMounted` — on a first visit that mount can land after this
    // callback and commit the old selection straight back over ours (the batch page hit exactly this).
    // No fallback: one image, and if its set is gone there is nowhere to land — say so instead.
    const targetSet = restoreTargetSet([project.setUidOfImage(uid)], '')
    const prevSel = targetSet ? project.getImageSelection('animation', targetSet) : []
    const prevSetUid = project.activeSetUid
    if (targetSet) {
      if (project.activeSetUid !== targetSet) project.activeSetUid = targetSet
      nextTick(() => project.setImageSelection('animation', targetSet, [uid]))
    }

    const dropped = [...r.dropped]
    if (!r.imageUid) dropped.push('which image it was recorded on — attached to the current one')
    else if (!targetSet) dropped.push(`${imageName(uid)} — that image is no longer in this project`)

    return {
      undo: () => {
        anim.snapshots = prevFrames
        anim.fps = prevOut.fps
        anim.sizeX = prevOut.sizeX
        anim.sizeY = prevOut.sizeY
        anim.suffix = prevOut.suffix
        anim.titleCard = prevOut.titleCard
        if (targetSet) {
          project.setImageSelection('animation', targetSet, prevSel)
          project.activeSetUid = prevSetUid
        }
      },
      note: restoreNote([], dropped),
    }
  },
})

// Which half is expanded — the shared two-half panel primitive, its own key so this panel remembers
// its arrangement separately from the module pages' task runner.
const { pane, toggle: togglePane } = usePaneExpand('cc-animation-pane')
</script>

<template>
  <div class="ap" :class="'pane-' + pane">
    <RestoreNotice v-if="restoreNotice" class="ap-restored" :source="restoreNotice.movie"
                   :note="restoreNotice.note" @undo="undoRestore" @dismiss="dismissRestore" />

    <p v-if="!activeUid" class="ap-hint cc-muted">Select an image (left) to build its timeline.</p>

    <template v-else>
      <PaneExpandBar :pane="pane" top-label="animation controls" bottom-label="task list"
                     top-icon="pi-cog" bottom-icon="pi-bars" @toggle="togglePane" />
      <!-- Keyframes — every capture reads the LIVE viewer, so this states what it is reading -->
      <section class="ap-sec">
        <h4>Keyframes <span class="ap-sub cc-muted">{{ frames.length }}</span></h4>
        <div class="ap-img cc-row cc-row-tight">
          <span class="ap-name" :title="activeImage?.name">{{ activeImage?.name ?? activeUid }}</span>
          <button v-if="!isOpen" class="cc-btn cc-btn-ghost cc-btn-micro" data-guide="animation.open"
                  :disabled="opening" @click="openActive"
                  v-tooltip.left="'Open this image in napari'">
            <i :class="['pi', opening ? 'pi-spin pi-spinner' : 'pi-eye']" /> Open
          </button>
        </div>
        <div class="ap-btns">
          <button class="cc-btn cc-btn-ghost" data-guide="animation.capture"
                  :disabled="capturing || !isOpen" @click="capture"
                  v-tooltip.left="isOpen ? 'Capture the current napari view as a keyframe (a base look)'
                                         : 'Open this image in napari first'">
            <i :class="['pi', capturing ? 'pi-spin pi-spinner' : 'pi-camera']" /> Capture view
          </button>
          <button class="cc-btn cc-btn-ghost" data-guide="animation.addKeyframe"
                  :disabled="!frames.length" @click="addKeyframe"
                  v-tooltip.left="'Duplicate the last keyframe to vary it via the rows'">
            <i class="pi pi-plus" /> Add keyframe
          </button>
          <button class="cc-btn cc-btn-ghost" :disabled="!selected || updating || !isOpen" @click="updateSelected"
                  v-tooltip.left="'Replace the selected keyframe with the current napari view (re-capture)'">
            <i :class="['pi', updating ? 'pi-spin pi-spinner' : 'pi-refresh']" /> Update selected
          </button>
        </div>
        <CcToggle class="ap-sync" label="Sync napari"
                  :model-value="settings.animationSyncNapari" @update:model-value="onToggleSync($event)"
                  v-tooltip.bottom="'Show the selected keyframe in napari when you click it'" />
      </section>

      <!-- Movie — the same controls as the viewer recorder and the batch page -->
      <section class="ap-sec">
        <h4>Movie</h4>
        <MovieOutputControls :suffix-options="movieSuffixes" v-model:fps="anim.fps" v-model:sizeX="anim.sizeX" v-model:sizeY="anim.sizeY"
                             v-model:suffix="anim.suffix" :canvas-x="canvasSizeX" :canvas-y="canvasSizeY" />
        <TitleCardControls v-model="anim.titleCard" />
      </section>

      <div class="ap-actions cc-row">
        <button class="cc-btn cc-btn-primary" data-guide="animation.render" :disabled="!canRender" @click="render"
                v-tooltip.top="canRender ? 'Render the timeline to an mp4' : 'Needs 2 or more keyframes'">
          <i :class="['pi', rendering ? 'pi-spin pi-spinner' : 'pi-play']" /> Render
        </button>
      </div>

      <!-- The TASKS half — wrapped so the `pane-<mode>` CSS below can hide it without reaching into
           TaskList's own root. Same containment as the batch panel: it owns its overflow, with a floor
           so a long config above cannot squeeze it to a sliver. -->
      <div class="ap-tasks">
        <TaskList module="animation" />
      </div>
    </template>
  </div>
</template>

<style scoped>
.ap { display: flex; flex-direction: column; gap: 7px; flex: 1; min-width: 0; padding: 2px; }
.ap-hint { margin: 2px 0; }
.ap-restored { margin-bottom: 0.5rem; }
/* Which half is showing, declared once per half — one rule covers the group AND a section added later,
   which a per-element guard would miss. */
.ap.pane-bottom > .ap-sec,
.ap.pane-bottom > .ap-actions { display: none; }
.ap.pane-top    > .ap-tasks   { display: none; }
.ap-tasks { display: flex; flex-direction: column; flex: 1 1 auto; min-width: 0; min-height: 14rem; overflow: auto; }
.ap.pane-bottom > .ap-tasks { min-height: 0; }

.ap-sec { border: 1px solid var(--cc-border); border-radius: var(--cc-radius-md); padding: 6px 8px; background: var(--cc-surface-1); }
.ap-sec h4 { display: flex; align-items: baseline; margin: 0 0 4px; font-size: var(--cc-fs-md); font-weight: 700; }
.ap-sub { margin-left: 6px; }
.ap-img { margin-bottom: 5px; }
.ap-name { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
  font-size: var(--cc-fs-sm); font-weight: 600; }
.ap-btns { display: flex; flex-direction: column; gap: 4px; }
.ap-btns .cc-btn { justify-content: flex-start; }
.ap-sync { margin-top: 6px; }
.ap-actions { margin-top: 2px; }
</style>
