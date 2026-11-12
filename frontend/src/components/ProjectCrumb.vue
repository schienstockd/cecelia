<!--
  The header's project crumb: "Project › set ▾" (docs/todo/GUI_LAYOUT_PLAN.md, mock frame 1). Moved
  from the sidebar's project block: the project name opens the project panel (switch, open, create),
  the active set is named after it, and an active sidebar layout (view profile) shows as a small chip.
  With no project it offers "Open a project…"; a spinner shows while one loads. Switching SETS stays in the set row (SetBar).
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useProjectStore } from '../stores/project'
import { useViewProfilesStore } from '../stores/viewProfiles'
import ProjectPanel from './ProjectPanel.vue'

const projectMeta = useProjectMetaStore()
const project = useProjectStore()
const viewProfiles = useViewProfilesStore()
const showPanel = ref(false)
const setName = computed(() => project.activeSet()?.name ?? '')
</script>

<template>
  <span class="crumb" data-guide="header.project">
    <template v-if="projectMeta.current">
      <button type="button" class="crumb-btn cc-btn cc-btn-bare cc-btn-dense" @click="showPanel = true"
              v-tooltip.bottom="'Switch project or create a new one'">
        <span class="crumb-proj">{{ projectMeta.current.name }}</span>
        <template v-if="setName">
          <span class="crumb-sep" aria-hidden="true">›</span>
          <span class="crumb-set cc-muted">{{ setName }}</span>
        </template>
        <i class="pi pi-chevron-down crumb-chev cc-muted cc-fs-3xs" aria-hidden="true" />
      </button>
      <!-- only while a profile curates the menu: a badge for the default "All pages" is noise -->
      <span v-if="viewProfiles.active" class="crumb-layout"
            v-tooltip.bottom="'Sidebar layout — change in Settings → Interface'">
        <i class="pi pi-eye" />{{ viewProfiles.active.label }}
      </span>
    </template>
    <button v-else type="button" class="cc-btn cc-btn-ghost cc-btn-dense" @click="showPanel = true"
            v-tooltip.bottom="'Open or create a project to get started'">
      <i class="pi pi-folder-open" /> Open a project…
    </button>
    <!-- a project takes a moment to load, whichever control opened it -->
    <i v-if="projectMeta.loading" class="pi pi-spin pi-spinner cc-muted cc-fs-xs" role="status"
       aria-label="Loading the project" v-tooltip.bottom="'Loading the project…'" />
  </span>
  <ProjectPanel v-if="showPanel" @close="showPanel = false" />
</template>

<style scoped>
.crumb { display: inline-flex; align-items: center; gap: 0.3rem; min-width: 0; white-space: nowrap; }
.crumb-btn { gap: 0.3rem; min-width: 0; }
.crumb-proj { font-weight: 600; color: var(--cc-text); max-width: 16rem; overflow: hidden; text-overflow: ellipsis; }
.crumb-sep { color: var(--cc-text-dim); }
.crumb-set { max-width: 12rem; overflow: hidden; text-overflow: ellipsis; }
.crumb-layout {
  display: inline-flex; align-items: center; gap: 0.25rem; margin-left: 0.3rem;
  color: var(--cc-text-dim); font-size: var(--cc-fs-2xs); padding: 0.05rem 0.3rem; border-radius: var(--cc-radius-xs);
  background: var(--cc-surface-2);
}
</style>
