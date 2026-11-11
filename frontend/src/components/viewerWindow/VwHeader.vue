<!--
  The viewer window's header (docs/todo/GUI_LAYOUT_PLAN.md, V1; mock frame 4): the way back to the main
  window, WHAT is on screen (image name, image version, the labels shown and whether they are a
  preview or a live run), Help and Ask Kiwi. The window has no app shell (a bare route), so this is
  its only chrome; the controls stay in the side panel.

  Back: raises the main window, named by the page it is on (lib/popout.ts → focusMainWindow — Chrome
  ignores `opener.focus()`, so the main window is targeted by its name).
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import TeleportPopover from '../TeleportPopover.vue'
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
  projectUid: string
  kiwiRef: () => KiwiRef | null
  kiwiTip: string
}>()
const emit = defineEmits<{ shortcuts: []; saveImage: []; draw: [] }>()
const observer = useObserverStore()

const shareOpen = ref(false)
const shareBtn = ref<HTMLElement | null>(null)
function share(ev: 'saveImage' | 'draw') {
  shareOpen.value = false
  if (ev === 'saveImage') emit('saveImage'); else emit('draw')
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
    <span v-if="prov" class="vwh-prov cc-muted cc-fs-xs"
          v-tooltip.bottom="'The image version and labels are picked in the main window’s Viewer panel'">{{ prov }}</span>
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
        <button v-if="observer.ai.enabled" class="cc-actions-item" role="menuitem" @click="share('draw')">
          Draw and note <span class="cc-muted cc-fs-xs">mark the view for Kiwi</span>
        </button>
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
.vwh-grow { flex: 1; }
</style>
