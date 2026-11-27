<!--
  A training task's model NAME box (`valueNameInput` + `optionsFrom` naming a vault) — suggests the
  names scoped like the Model vault manager, with the same View toggles beside it (VaultModelSelect is
  the picker twin). Only the suggestions narrow: the vault is global, so the field still marks a name
  as existing when an out-of-scope model holds it — training onto it needs `overwrite`.
-->
<script setup lang="ts">
import { computed, watch } from 'vue'
import SuggestInput from './SuggestInput.vue'
import VaultViewButton from './VaultViewButton.vue'
import { useVaultRows, useVaultScope } from '../composables/useVaultModel'
import { vaultNamesInScope } from '../utils/vaultScope'
import type { VaultKind } from '../utils/modelVaultKinds'

const props = defineProps<{ kind: VaultKind; options: string[]; placeholder?: string; tip?: string }>()
const model = defineModel<string>({ default: '' })

const scope = useVaultScope()
const { rowFor, ensure } = useVaultRows(() => props.kind)
watch(() => props.options, ensure, { immediate: true })

const names = computed(() => vaultNamesInScope(props.options, rowFor, scope.value))
</script>

<template>
  <div class="vault-name">
    <SuggestInput v-model="model" :options="names.shown" :existing-options="options"
                  :placeholder="placeholder" :tip="tip" mark-existing />
    <VaultViewButton :hidden="names.hidden" />
  </div>
</template>

<style scoped>
.vault-name { display: flex; align-items: center; gap: 0.3rem; }
.vault-name :deep(input) { flex: 1; min-width: 0; }
</style>
