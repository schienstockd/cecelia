<script setup lang="ts">
// The ONE on/off toggle switch for the app. Use it for an immediate boolean OPTION — a setting that
// applies the moment you flip it (autoplay, loop, show-legend, title card, dark theme, …). It is a
// styled <input type="checkbox"> under the hood, so it works with v-model, keyboard focus and forms.
//
// When NOT to use it: a multi-SELECT list or a value staged as part of a form (image / channel /
// feature pickers, "select all", per-row selection). Those stay native <input type="checkbox"> — a
// list of sliding switches reads worse and misuses the on/off affordance. See docs/UI.md → Toggles.
//
// Label: pass text via the `label` prop or the default slot (slot wins). Both sit to the RIGHT of the
// switch; the whole control is one clickable <label>. Add a tooltip at the call site with v-tooltip.
const model = defineModel<boolean>({ required: true })
defineProps<{ label?: string; disabled?: boolean }>()
</script>

<template>
  <label class="cc-toggle" :class="{ disabled }">
    <input type="checkbox" class="cc-toggle-input" v-model="model" :disabled="disabled" />
    <span class="cc-toggle-track"><span class="cc-toggle-thumb" /></span>
    <span v-if="$slots.default || label" class="cc-toggle-label"><slot>{{ label }}</slot></span>
  </label>
</template>

<style scoped>
.cc-toggle { display: inline-flex; align-items: center; gap: 0.5rem; cursor: pointer; }
.cc-toggle.disabled { opacity: 0.5; cursor: not-allowed; }
.cc-toggle-input { display: none; }
.cc-toggle-track {
  width: 32px; height: 17px;
  border-radius: 999px;
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
  position: relative;
  flex-shrink: 0;
  transition: background 0.15s, border-color 0.15s;
}
.cc-toggle-input:checked ~ .cc-toggle-track {
  background: var(--cc-accent);
  border-color: var(--cc-accent);
}
.cc-toggle-thumb {
  position: absolute;
  width: 11px; height: 11px;
  border-radius: 50%;
  background: #fff;
  top: 2px; left: 2px;
  transition: left 0.15s;
}
.cc-toggle-input:checked ~ .cc-toggle-track .cc-toggle-thumb { left: 17px; }
.cc-toggle-label { font-size: 0.8rem; color: var(--cc-text); }
</style>
