<!--
  The vault's View popover — Other users / Other projects — for every place that lists vault models:
  the Model vault manager and a task form's model picker. Both bind the SAME settings, so widening the
  view in one widens it in the other; a picker with its own copy would be a second preference for one
  question. Lit when off its default, so a narrowed list never looks complete. Rule: utils/vaultScope.ts.
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import CcToggle from './CcToggle.vue'
import TeleportPopover from './TeleportPopover.vue'
import { useSettingsStore, VAULT_VIEW_DEFAULTS } from '../stores/settings'

const props = defineProps<{ hidden?: number }>()

const settings = useSettingsStore()
const open = ref(false)
const btn = ref<HTMLElement | null>(null)
const changed = computed(() =>
  (Object.keys(VAULT_VIEW_DEFAULTS) as (keyof typeof VAULT_VIEW_DEFAULTS)[])
    .some(k => settings[k] !== VAULT_VIEW_DEFAULTS[k]))
const tip = computed(() => props.hidden
  ? `View: other users, other projects (${props.hidden} hidden)`
  : 'View: other users, other projects')
</script>

<template>
  <button ref="btn" type="button" class="cc-btn cc-btn-bare cc-btn-icon"
          :class="{ 'cc-btn-on cc-btn-on-solid': open, 'vault-view-set': changed }"
          @click="open = !open" v-tooltip.left="tip">
    <i class="pi pi-eye" />
  </button>
  <TeleportPopover v-model="open" :anchor="btn" placement="bottom-end">
    <div class="vault-view">
      <CcToggle v-model="settings.vaultOtherUsers" label="Other users"
                v-tooltip.bottom="'Include models other users trained'" />
      <CcToggle v-model="settings.vaultOtherProjects" label="Other projects"
                v-tooltip.bottom="'Include models trained in other projects'" />
    </div>
  </TeleportPopover>
</template>

<style scoped>
.vault-view { display: flex; flex-direction: column; gap: 0.45rem; padding: 0.5rem 0.65rem; }
.vault-view-set { color: var(--cc-accent); }   /* a view preference is off its default */
</style>
