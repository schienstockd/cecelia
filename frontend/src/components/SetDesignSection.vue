<script setup lang="ts">
/*
  The active set's experiment design, as a section of the Metadata page (plan Decision 5). Amber (the
  icon and a warn line) while the set has no design or an unconfirmed one. Opens ExperimentDesignDialog over the
  set's images: set one up, edit it, or promote an exploratory set. Logic: utils/experimentDesign.ts
  (`designSummary`). Plan: docs/todo/EXPERIMENT_DESIGN_PLAN.md (Decision 5).
*/
import { computed, ref } from 'vue'
import ExperimentDesignDialog from './ExperimentDesignDialog.vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { designSummary, type DesignImage } from '../utils/experimentDesign'
import { useSetDesign } from '../composables/useSetDesign'

const project = useProjectStore()
const projectMeta = useProjectMetaStore()

const set = computed(() => project.activeSet())
const { design, loaded } = useSetDesign(computed(() => projectMeta.current?.uid), computed(() => set.value?.uid))
const open = ref(false)
// the images the dialog designs over, frozen when it opens so an edit elsewhere can't reshuffle it
const images = ref<DesignImage[]>([])

const summary = computed(() => designSummary(design.value))
const needsAction = computed(() => loaded.value && (summary.value.state === 'none' || summary.value.state === 'inferred'))
const label = computed(() => summary.value.state === 'none' ? 'Set up' : summary.value.state === 'exploratory' ? 'Make a design' : 'Edit')

function openDialog() {
  if (!set.value) return
  images.value = set.value.images.map(i => ({ uid: i.uid, name: i.name, oriPath: i.oriPath, attr: { ...(i.attr ?? {}) } }))
  open.value = true
}
</script>

<template>
  <section v-if="set && set.images.length" class="cc-panel-section md-section">
    <h3 class="cc-panel-heading cc-eyebrow cc-fs-2xs">Experiment design</h3>
    <p class="sd-hint" :class="needsAction ? 'cc-muted-warn' : 'cc-muted'">{{ summary.tip }}</p>
    <button class="cc-btn cc-btn-ghost" :disabled="!loaded" @click="openDialog"
            v-tooltip.bottom="'What is compared, and what counts as n'">
      <i class="pi pi-bullseye" :class="{ 'sd-warn': needsAction }" />
      {{ label }}
    </button>
    <ExperimentDesignDialog
      v-if="open && projectMeta.current"
      :project-uid="projectMeta.current.uid"
      :set-uid="set.uid"
      :set-name="set.name"
      :images="images"
      :existing="design"
      @close="open = false"
    />
  </section>
</template>

<style scoped>
.sd-hint { margin: 0; line-height: 1.5; }
.sd-warn { color: var(--cc-sev-warn); }
</style>
