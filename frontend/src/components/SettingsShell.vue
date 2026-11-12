<!--
  SettingsShell — the two-pane settings modal: category list left, the selected category's content
  right (Firefox / Thunderbird / GNOME Settings pattern). Shared by Preferences (the active profile's
  choices) and Settings (project + machine). Built on BaseModal.

  Each category's content is a named slot matching its id (`<template #storage>…`); only the active
  one renders, so a pane's fetches run when it is opened. A category may carry a `data-guide` id for
  its nav button — the anchor a guide points at to say "open Storage".

  If the active id is not in `categories` (a pane hidden for this user, or not yet loaded) the first
  category shows, so the modal never opens on a blank pane.

  Slots: one per category id; `#after` for dialogs the panes open (rendered outside the two columns).
-->
<script setup lang="ts" generic="Id extends string">
import { computed } from 'vue'
import BaseModal from './BaseModal.vue'

export interface SettingsCategory<Id extends string = string> {
  id: Id
  label: string
  icon: string
  guide?: string        // data-guide id on the nav button
  tip?: string          // tooltip on the nav button
}

const props = withDefaults(defineProps<{
  title: string
  icon: string
  categories: SettingsCategory<Id>[]
  ariaLabel?: string
  width?: string
  height?: string
}>(), { ariaLabel: 'Categories', width: '720px', height: '560px' })

const active = defineModel<Id>({ required: true })
defineEmits<{ (e: 'close'): void }>()

const shown = computed<Id | undefined>(() =>
  props.categories.some(c => c.id === active.value) ? active.value : props.categories[0]?.id)
</script>

<template>
  <BaseModal :title="title" :icon="icon" :width="width" :height="height" @close="$emit('close')">
    <div class="ss-cols">
      <nav class="ss-nav" :aria-label="ariaLabel">
        <button v-for="c in categories" :key="c.id"
                class="ss-nav-btn"
                :class="{ 'ss-nav-btn-active': shown === c.id }"
                :data-guide="c.guide"
                v-tooltip.right="c.tip"
                @click="active = c.id">
          <i :class="['pi', c.icon]" />
          <span>{{ c.label }}</span>
        </button>
      </nav>

      <section class="ss-pane">
        <slot v-if="shown" :name="shown" />
      </section>
    </div>
    <slot name="after" />
  </BaseModal>
</template>

<style scoped>
.ss-cols { display: grid; grid-template-columns: 12rem 1fr; gap: 1rem; height: 100%; min-height: 0; }
.ss-nav { display: flex; flex-direction: column; gap: 0.2rem; overflow-y: auto;
          border-right: 1px solid var(--cc-border); padding-right: 0.6rem; }
.ss-nav-btn {
  display: flex; align-items: center; gap: 0.5rem;
  padding: 0.45rem 0.7rem;
  background: transparent; border: 1px solid transparent;
  border-radius: var(--cc-radius-sm);
  color: var(--cc-text-dim);
  cursor: pointer;
  text-align: left;
  font-size: var(--cc-fs-md);
}
.ss-nav-btn:hover { background: var(--cc-surface-2); color: var(--cc-text); }
.ss-nav-btn-active { background: var(--cc-surface-2); color: var(--cc-text);
                     border-color: var(--cc-border); }
.ss-pane {
  min-width: 0;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  gap: 0.55rem;
  padding-right: 0.25rem;
}
</style>
