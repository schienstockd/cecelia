<!--
  A task form's vault model picker (`select` + `optionsFrom` naming a vault) — headed by where each
  model was trained and scoped like the Model vault manager, with the same View toggles beside it.

  The options are still the server's (`_OPTION_SOURCES`, which `validate_params` checks against);
  only their layout and visibility are decided here, from the vault listing's origin fields. Hiding is
  a view: an out-of-scope value stays valid, and stays listed while it is the selected one.
-->
<script setup lang="ts">
import { computed, watch } from 'vue'
import VaultViewButton from './VaultViewButton.vue'
import { useVaultRows } from '../composables/useVaultModel'
import { useSettingsStore } from '../stores/settings'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useAppControlStore } from '../stores/appControl'
import { vaultPickerLayout, type PickerOption } from '../utils/vaultScope'
import type { VaultKind } from '../utils/modelVaultKinds'

const props = defineProps<{ kind: VaultKind; options: PickerOption[]; tip?: string }>()
const model = defineModel<string>({ default: '' })

const settings    = useSettingsStore()
const projectMeta = useProjectMetaStore()
const appControl  = useAppControlStore()
const { rowFor, ensure } = useVaultRows(() => props.kind)

watch(() => props.options.map(o => o.value), ensure, { immediate: true })

const layout = computed(() => vaultPickerLayout(props.options, rowFor, {
  profile: appControl.activeProfileName, projectUid: projectMeta.current?.uid,
  otherUsers: settings.vaultOtherUsers, otherProjects: settings.vaultOtherProjects,
}, model.value))
</script>

<template>
  <div class="vault-select">
    <select class="select-input" :value="model" v-tooltip.bottom="tip"
            @change="model = ($event.target as HTMLSelectElement).value">
      <option v-for="o in layout.loose" :key="o.value" :value="o.value">{{ o.label }}</option>
      <optgroup v-for="g in layout.groups" :key="g.title" :label="g.title">
        <option v-for="o in g.options" :key="o.value" :value="o.value">{{ o.label }}</option>
      </optgroup>
    </select>
    <VaultViewButton :hidden="layout.hidden" />
  </div>
</template>

<style scoped>
.vault-select { display: flex; align-items: center; gap: 0.3rem; }
.select-input { flex: 1; min-width: 0; }
</style>
