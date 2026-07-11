<!--
  Collapsible section used inside the image panel and anywhere a labeled toggle is needed.
  Props
  ─────
    label       string   Heading text shown in the toggle bar.
    defaultOpen bool     Whether the section starts open (default: true).
    maxHeight   string   CSS max-height for the body div (default: '320px').
    storageKey  string   When set, the open/closed state is remembered in localStorage under this
                         key (so it survives navigation — see the "persist every option" rule).
-->
<script setup lang="ts">
import { ref, watch } from 'vue'

const props = withDefaults(defineProps<{
  label:        string
  defaultOpen?: boolean
  maxHeight?:   string
  storageKey?:  string
}>(), {
  defaultOpen: true,
  maxHeight:   '320px',
})

const stored = props.storageKey ? localStorage.getItem(props.storageKey) : null
const open = ref(stored === null ? props.defaultOpen : stored === '1')
watch(open, v => {
  if (props.storageKey) { try { localStorage.setItem(props.storageKey, v ? '1' : '0') } catch { /* ignore */ } }
})
</script>

<template>
  <div class="collapsible-section">
    <button class="cs-toggle" @click="open = !open"
      v-tooltip.right="open ? `Collapse ${label}` : `Expand ${label}`">
      <i :class="['pi', open ? 'pi-chevron-up' : 'pi-chevron-down']" />
      <span class="cs-label">{{ label }}</span>
    </button>
    <!-- with a real max-height the body scrolls itself; with max-height:none it must NOT be a scroll
         container (overflow-y:auto would still make it the sticky scrollport, so a `position:sticky`
         descendant — e.g. the board's pop-manager rail — sticks to a box that never scrolls and never
         activates). Let the outer page scroll handle it. -->
    <div v-show="open" class="cs-body" :style="{ maxHeight, overflowY: maxHeight === 'none' ? 'visible' : 'auto' }">
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
