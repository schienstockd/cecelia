<!--
  Collapsible section used inside the image panel and anywhere a labeled toggle is needed.
  Props
  ─────
    label       string   Heading text shown in the toggle bar.
    defaultOpen bool     Whether the section starts open (default: true).
    maxHeight   string   CSS max-height for the body div (default: '320px').
-->
<script setup lang="ts">
import { ref } from 'vue'

const props = withDefaults(defineProps<{
  label:        string
  defaultOpen?: boolean
  maxHeight?:   string
}>(), {
  defaultOpen: true,
  maxHeight:   '320px',
})

const open = ref(props.defaultOpen)
</script>

<template>
  <div class="collapsible-section">
    <button class="cs-toggle" @click="open = !open"
      v-tooltip.right="open ? `Collapse ${label}` : `Expand ${label}`">
      <i :class="['pi', open ? 'pi-chevron-up' : 'pi-chevron-down']" />
      <span class="cs-label">{{ label }}</span>
    </button>
    <div v-show="open" class="cs-body" :style="{ maxHeight }">
      <slot />
    </div>
  </div>
</template>

<style scoped>
.collapsible-section {
  border-top: 1px solid var(--cc-border);
  display: flex;
  flex-direction: column;
  flex-shrink: 0;
}

.cs-toggle {
  display: flex;
  align-items: center;
  gap: 0.4rem;
  width: 100%;
  padding: 0.35rem 1rem;
  background: var(--cc-surface-1);
  border: none;
  cursor: pointer;
  color: var(--cc-text-dim);
  font-size: 0.7rem;
  text-align: left;
  transition: background 0.1s, color 0.1s;
  flex-shrink: 0;
}
.cs-toggle:hover { background: var(--cc-surface-2); color: var(--cc-text); }
.cs-toggle .pi  { font-size: 0.65rem; }

.cs-label {
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  font-size: 0.65rem;
}

.cs-body {
  overflow-y: auto;
  background: var(--cc-bg);
}
</style>
