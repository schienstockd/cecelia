<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import GatingPlots from './gate/GatingPlots.vue'
import { useTaskDefs } from '../composables/useTaskDefs'
import { useSettingsStore } from '../stores/settings'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { correctionLaunch } from '../utils/correctionSelections'
import { openViewerWindow } from '../utils/viewerWindow'

const { defs: trackingDefs, reload: reloadDefs } = useTaskDefs('tracking')
const settings = useSettingsStore()
const project = useProjectStore()
const projectMeta = useProjectMetaStore()

// The correction cockpit, reachable from the page whose results it fixes
function openCorrection(selectedUids: string[]) {
  const go = correctionLaunch('tracking', selectedUids, project.openImageUid)
  settings.correctionCockpitMode = go.mode
  settings.correctionCockpitOpen = true
  if (go.openInViewer) openViewerWindow({ projectUid: projectMeta.current?.uid ?? '', imageUid: go.openInViewer })
}
</script>

<template>
  <ModuleLayout module="tracking" :show-attrs="true" :show-filter="true" plots-label="Track gating">
    <template #actions="{ selectedUids }">
      <button class="cc-btn cc-btn-ghost cc-btn-dense" data-guide="tracking.correct"
              v-tooltip.bottom="'Fix tracks by hand: join, split or remove them in the viewer'" @click="openCorrection(selectedUids)">
        <i class="pi pi-wrench" /> Correct tracks
      </button>
    </template>
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="trackingDefs"
        :on-reload-defs="reloadDefs"
        module="tracking"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
    <!-- Track-property gating: the SAME gating canvas as the Gate page, in track mode (one point
         per track; gate on motility / per-track aggregates). Active when exactly one image is
         selected. Reuses GatingPlots via popType — no track-specific component. -->
    <template #plots="{ selectedUids, orderedUids, selectUids }">
      <GatingPlots :image-uid="selectedUids.length === 1 ? selectedUids[0] : null" pop-type="track"
                   :ordered-uids="orderedUids" :select-uids="selectUids" />
    </template>
  </ModuleLayout>
</template>
