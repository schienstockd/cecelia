<script setup lang="ts">
// The set's experiment design in the action bar's Checks cluster (a `CheckButton`): amber while there is
// no design or an unconfirmed one, and when images sit outside a confirmed one — added after it was set
// up, or missing one of its attributes. Opens ExperimentDesignDialog at the confirm step. Refetches when
// the set's images or their attributes change. View: lib/checks.ts `designCheckView`.
// docs/todo/EXPERIMENT_DESIGN_PLAN.md (T7).
import { computed, ref, watch } from 'vue'
import CheckButton from './CheckButton.vue'
import ExperimentDesignDialog from './ExperimentDesignDialog.vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSetDesign } from '../composables/useSetDesign'
import { designCheckView } from '../lib/checks'
import type { DesignImage } from '../utils/experimentDesign'

const props = defineProps<{ setUid: string }>()
const project = useProjectStore()
const projectMeta = useProjectMetaStore()
const set = computed(() => project.sets.find(s => s.uid === props.setUid))
const { design, grid, loaded, reload } = useSetDesign(computed(() => projectMeta.current?.uid), computed(() => props.setUid))
// an image added, removed or re-annotated changes the grid
watch(() => JSON.stringify(set.value?.images.map(i => [i.uid, i.attr ?? {}]) ?? []), () => reload())
const view = computed(() => designCheckView(design.value, grid.value, loaded.value, set.value?.images.length ?? 0))

const open = ref(false)
const images = ref<DesignImage[]>([])
function openDialog() {
  if (!set.value) return
  images.value = set.value.images.map(i => ({ uid: i.uid, name: i.name, oriPath: i.oriPath, attr: { ...(i.attr ?? {}) } }))
  open.value = true
}
</script>

<template>
  <CheckButton data-guide="checks.design" label="Design" :state="view.state" :count="view.count" :tip="view.tip"
               :disabled="!projectMeta.current || !set?.images.length" @click="openDialog" />
  <ExperimentDesignDialog v-if="open && projectMeta.current && set" :project-uid="projectMeta.current.uid" :set-uid="set.uid"
                          :set-name="set.name" :images="images" :existing="design"
                          :start-step="design?.status === 'confirmed' ? 3 : 1" @close="open = false" />
</template>
