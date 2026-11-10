<script setup lang="ts">
// The set's acquisition check in the action bar's Checks cluster (a `CheckButton`): how many things to
// look at, and the set modal (AcqCheckDialog) on click. Refetches when a task finishes on the set (a re-import or a
// Refresh metadata changes what the check reads). docs/todo/ACQUISITION_TEST_CHECK_PLAN.md.
import { computed, ref, watch } from 'vue'
import AcqCheckDialog from './AcqCheckDialog.vue'
import CheckButton from './CheckButton.vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useDataRefresh } from '../composables/useDataRefresh'
import { fetchAcqCheck, type AcqCheckResult } from '../utils/acqCheckApi'
import { acqCheckView } from '../lib/checks'

const props = defineProps<{ setUid: string }>()
const project = useProjectStore()
const projectMeta = useProjectMetaStore()
const result = ref<AcqCheckResult | null>(null)
const open = ref(false)
const setName = computed(() => project.sets.find(s => s.uid === props.setUid)?.name)

async function load() {
  const uid = projectMeta.current?.uid
  result.value = uid && props.setUid ? await fetchAcqCheck(uid, props.setUid) : null
}
watch([() => props.setUid, () => projectMeta.current?.uid], load, { immediate: true })
useDataRefresh(() => project.sets.find(s => s.uid === props.setUid)?.images.map(i => i.uid) ?? [], load)
const view = computed(() => acqCheckView(result.value))
</script>

<template>
  <CheckButton label="Acquisition" :state="view.state" :count="view.count" :tip="view.tip"
               :disabled="!projectMeta.current" @click="open = true" />
  <AcqCheckDialog v-if="open && projectMeta.current" :project-uid="projectMeta.current.uid" :set-uid="setUid"
                  :set-name="setName" @changed="r => result = r" @close="open = false" />
</template>
