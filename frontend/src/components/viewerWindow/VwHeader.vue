<!--
  The viewer window's header (docs/todo/GUI_LAYOUT_PLAN.md, V1; mock frame 4): the way back to the main
  window, WHAT is on screen (image name, image version, the labels shown and whether they are a
  preview or a live run), Help and Ask Kiwi. The provenance chip is also the version picker
  (docs/todo/VIEWER_ON_THE_IMAGE_PLAN.md, S3), marking which version tasks run against. The window has no app shell (a bare route), so this is
  its only chrome; the controls stay in the side panel.

  Back: raises the main window, named by the page it is on (lib/popout.ts → focusMainWindow — Chrome
  ignores `opener.focus()`, so the main window is targeted by its name).
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import TeleportPopover from '../TeleportPopover.vue'
import QuickMovie from '../QuickMovie.vue'
import type { CciaImage } from '../../stores/project'
import { useObserverStore } from '../../stores/observer'
import HelpMenu from '../HelpMenu.vue'
import AddToKiwiButton from '../kiwi/AddToKiwiButton.vue'
import type { KiwiRef } from '../../utils/kiwiRef'
import { NAV_GROUPS, navItemFor } from '../../lib/navGroups'
import { focusMainWindow } from '../../lib/popout'

const props = defineProps<{
  imageName: string
  /** the image version on screen ('' = the server's active one) */
  valueName: string
  /** the labels shown ('' = none) and where they come from */
  labels: string
  labelsState: '' | 'preview' | 'live'
  /** every registered version of the image, and the one tasks read */
  versions: string[]
  activeVersion: string
  projectUid: string
  imageUid: string
  setUid: string
  kiwiRef: () => KiwiRef | null
  kiwiTip: string
}>()
const emit = defineEmits<{ shortcuts: []; saveImage: []; draw: []; version: [valueName: string] }>()
const observer = useObserverStore()

const versionOpen = ref(false)
const versionBtn = ref<HTMLElement | null>(null)
function pickVersion(vn: string) { versionOpen.value = false; if (vn !== props.valueName) emit('version', vn) }
const notActive = computed(() => !!props.activeVersion && !!props.valueName && props.valueName !== props.activeVersion)

const shareOpen = ref(false)
const shareBtn = ref<HTMLElement | null>(null)
function share(ev: 'saveImage' | 'draw') {
  shareOpen.value = false
  if (ev === 'saveImage') emit('saveImage'); else emit('draw')
}

// Record movie… — the quick movie form, here rather than on the Movies page. The window has no project
// loaded, so the image object the form needs is fetched when it opens.
const movieOpen = ref(false)
const movieImg = ref<CciaImage | null>(null)
async function openMovie() {
  shareOpen.value = false
  movieOpen.value = true
  try {
    const q = `projectUid=${encodeURIComponent(props.projectUid)}&imageUid=${encodeURIComponent(props.imageUid)}`
    const res = await fetch(`/api/images/meta?${q}`)
    movieImg.value = res.ok ? ((await res.json()) as { image: CciaImage }).image : null
  } catch { movieImg.value = null }
}

// The opener's page, read when the pointer reaches the button — same origin, so its hash is readable;
// a closed or cross-origin opener just reads as the main window.
const openerPage = ref('')
function readOpener() {
  try {
    const o = window.opener as Window | null
    const path = o && !o.closed ? o.location.hash.replace(/^#/, '').split('?')[0] : ''
    openerPage.value = path ? (navItemFor(NAV_GROUPS, path)?.label ?? '') : ''
  } catch { openerPage.value = '' }
}
readOpener()
const back = focusMainWindow

const prov = computed(() => [
  props.valueName ? `Image: ${props.valueName}` : '',
  props.labels ? `Labels: ${props.labels}${props.labelsState ? ` · ${props.labelsState}` : ''}` : '',
].filter(Boolean).join(' · '))
</script>

<template>
  <header class="vwh">
    <button type="button" class="cc-btn cc-btn-ghost cc-btn-dense vwh-back" @pointerenter="readOpener" @click="back"
            v-tooltip.bottom="'Back to the main window'">
      <i class="pi pi-arrow-left" /> {{ openerPage || 'Main window' }}
    </button>
    <span class="vwh-name" :title="imageName">{{ imageName }}</span>
    <button v-if="prov && versions.length > 1" ref="versionBtn" type="button" class="vwh-prov cc-fs-xs"
            :class="notActive ? 'cc-muted-warn' : 'cc-muted'" aria-haspopup="menu" @click="versionOpen = !versionOpen"
            v-tooltip.bottom="notActive ? `Not the active version — tasks read ${activeVersion}` : 'Select the image version'">
      {{ prov }} <i class="pi pi-chevron-down cc-fs-3xs" />
    </button>
    <span v-else-if="prov" class="vwh-prov cc-muted cc-fs-xs"
          v-tooltip.bottom="'Labels are picked under On the image'">{{ prov }}</span>
    <TeleportPopover v-model="versionOpen" :anchor="versionBtn" placement="bottom-start" flush>
      <div class="cc-actions-menu" role="menu">
        <button v-for="vn in versions" :key="vn" class="cc-actions-item" role="menuitem"
                :class="{ 'cc-btn-on': vn === valueName }" @click="pickVersion(vn)">
          {{ vn }} <span v-if="vn === activeVersion" class="cc-muted cc-fs-xs">active — tasks read this</span>
        </button>
      </div>
    </TeleportPopover>
    <span class="vwh-grow" />
    <button ref="shareBtn" type="button" class="cc-btn cc-btn-ghost cc-btn-dense" :class="{ 'cc-btn-on': shareOpen }"
            aria-haspopup="menu" @click="shareOpen = !shareOpen"
            v-tooltip.bottom="'Save this view, or draw on it and send it to Kiwi'">
      Share <i class="pi pi-chevron-down cc-fs-2xs" />
    </button>
    <TeleportPopover v-model="shareOpen" :anchor="shareBtn" placement="bottom-end" flush>
      <div class="cc-actions-menu" role="menu">
        <button class="cc-actions-item" role="menuitem" @click="share('saveImage')">
          Save image (PNG) <span class="cc-muted cc-fs-xs">with the scale bar and overlays</span>
        </button>
        <button class="cc-actions-item" role="menuitem" data-guide="viewer.recordMovie" @click="openMovie">
          Record movie… <span class="cc-muted cc-fs-xs">or save a figure</span>
        </button>
        <button v-if="observer.ai.enabled" class="cc-actions-item" role="menuitem" @click="share('draw')">
          Draw and note <span class="cc-muted cc-fs-xs">mark the view for Kiwi</span>
        </button>
      </div>
    </TeleportPopover>
    <TeleportPopover v-model="movieOpen" :anchor="shareBtn" placement="bottom-end">
      <div class="vwh-movie">
        <div class="cc-eyebrow cc-fs-2xs">Record movie</div>
        <QuickMovie v-if="movieImg" :img="movieImg" :project-uid="projectUid" :set-uid="setUid" :shown-version="valueName" />
        <span v-else class="cc-muted cc-fs-xs">Loading…</span>
      </div>
    </TeleportPopover>
    <HelpMenu windowed>
      <template #learn="{ close }">
        <button class="cc-actions-item" role="menuitem" @click="close(); emit('shortcuts')">
          <i class="pi pi-question-circle" /> Mouse and keyboard shortcuts
        </button>
      </template>
    </HelpMenu>
    <AddToKiwiButton :kiwi-ref="kiwiRef" :tip="kiwiTip" :project-uid="projectUid" label="Ask Kiwi" />
  </header>
</template>

<style scoped>
.vwh {
  display: flex; align-items: center; gap: 0.75rem; flex-shrink: 0;
  height: var(--cc-header-h); padding: 0 0.75rem;
  background: var(--cc-surface-1); border-bottom: 1px solid var(--cc-border);
}
.vwh-name { font-weight: 600; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 28rem; }
.vwh-prov {
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis; padding: 0.1rem 0.5rem;
  border: 1px solid var(--cc-border); border-radius: var(--cc-radius-pill);
}
/* the quick movie form's width; its options open in their own popover */
.vwh-movie { width: 20rem; display: flex; flex-direction: column; gap: 0.4rem; }
button.vwh-prov { background: none; cursor: pointer; font: inherit; }
.vwh-grow { flex: 1; }
</style>
