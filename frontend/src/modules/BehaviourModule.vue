<!--
  Behaviour analysis module page. Pick one OR MORE images (multi-select), then:
   • run behaviour tasks (HMM states/transitions, the hmm composite) in the right-hand TaskRunner —
     these are SET-SCOPE (fitted jointly across all selected images);
   • inspect results in the summary-plot canvas below the table (HMM state frequencies, track speed,
     …). Per-module canvas: only `behaviourAnalysis` plot specs are offered.

  Linked-brushing host (LINKED_BRUSHING_PLAN.md P4/P5): the page owns the "Clear selection"
  affordance and Escape shortcut for the shared `linkedSelection` bag; it also clears the bag on
  navigation away so a stale selection can't leak into another module page. Individual
  SummaryPanels write the bag (P3 chip strip) and mirror to the shipped `TrackHighlight` so the
  viewer / TrackScheme / cell cards react; the page-level clear here reverses both.
-->
<script setup lang="ts">
import { computed, onMounted, onBeforeUnmount } from 'vue'
import ModuleLayout from '../components/ModuleLayout.vue'
import SummaryCanvas from '../components/canvas/SummaryCanvas.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import { useTaskDefs } from '../composables/useTaskDefs'
import { useLinkedSelectionStore } from '../stores/linkedSelection'
import { useViewerStore } from '../stores/viewer'

const { defs: behaviourDefs, reload: reloadDefs } = useTaskDefs('behaviour')

const linkedSel = useLinkedSelectionStore()
const viewer = useViewerStore()

// A single, page-scoped clear: drops the shared bag AND the mirrored TrackHighlight in one
// action so the user's "get me back to no selection" gesture doesn't leave a stray highlight in
// the viewer. Individual panels handle their own local pressed-chip visual via the store's
// isEmpty watcher.
function clearSelection() {
  if (linkedSel.isEmpty && !viewer.trackHighlight) return
  linkedSel.clear()
  viewer.setTrackHighlight(null)
}

// Escape from anywhere on the page — global listener, added on mount, removed on unmount so a
// dropped page doesn't intercept keystrokes for another module. Ignore Escape while the user is
// typing in an input/textarea/contenteditable so a form's own dismiss semantics still work.
function onKeydown(e: KeyboardEvent) {
  if (e.key !== 'Escape') return
  if (linkedSel.isEmpty) return
  const t = e.target as HTMLElement | null
  if (t && (t.tagName === 'INPUT' || t.tagName === 'TEXTAREA' || t.isContentEditable)) return
  clearSelection()
}
onMounted(() => { window.addEventListener('keydown', onKeydown) })
onBeforeUnmount(() => {
  window.removeEventListener('keydown', onKeydown)
  // Decision 8: selection is per-page; navigating away clears it. Same for the mirror.
  clearSelection()
})

const badgeCount = computed(() => linkedSel.bag?.ids.length ?? 0)
</script>

<template>
  <ModuleLayout module="behaviourAnalysis" :show-attrs="true" :show-filter="true">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="behaviourDefs"
        :on-reload-defs="reloadDefs"
        module="behaviour"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
    <template #plots="{ selectedUids }">
      <div class="sp-plots-slot">
        <button v-if="!linkedSel.isEmpty" type="button" class="cc-btn cc-btn-dense sp-clear-selection"
                @click="clearSelection"
                v-tooltip.left="'Clear track selection (Esc)'">
          Clear selection · {{ badgeCount }}
        </button>
        <SummaryCanvas :image-uids="selectedUids" module="behaviourAnalysis" />
      </div>
    </template>
  </ModuleLayout>
</template>

<style scoped>
/* LINKED_BRUSHING_PLAN.md P5 — page-level Clear affordance. Only rendered when the bag is
   non-empty (v-if in the template), so idle state adds no chrome. Wrapper is `position: relative`
   so the Clear button anchors to the plots slot, not the viewport. */
.sp-plots-slot { position: relative; }
.sp-clear-selection { position: absolute; top: 8px; right: 8px; z-index: 30;
  background: var(--cc-kiwi-tint); border-color: var(--cc-kiwi-strong);
  color: var(--cc-kiwi-soft); }
.sp-clear-selection:hover { background: var(--cc-kiwi-soft); color: var(--cc-text); }
</style>
