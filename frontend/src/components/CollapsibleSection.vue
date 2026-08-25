<!--
  Collapsible section used inside the image panel and anywhere a labeled toggle is needed.
  Props
  ─────
    label       string   Heading text shown in the toggle bar.
    defaultOpen bool     Whether the section starts open (default: true).
    maxHeight   string   CSS max-height for the body div (default: '320px').
    storageKey  string   When set, the open/closed state is remembered in localStorage under this
                         key (so it survives navigation — see the "persist every option" rule).
    open        bool     CONTROLLED mode: pass it (with @update:open, or v-model:open) and the parent
                         owns which sections are open. Omit it and the section manages itself, exactly
                         as before. This is what an ACCORDION needs — "only one open at a time" is a
                         fact about a GROUP of sections, and no section can know it alone.
-->
<script setup lang="ts">
import { ref, watch, computed } from 'vue'

const props = withDefaults(defineProps<{
  label:        string
  // one short line on what the section is for — a collapsed section is a control like any other, and
  // "Sensitivity" alone does not say what it is sensitive TO (docs/UI.md → every control needs a tooltip)
  tip?:         string
  defaultOpen?: boolean
  maxHeight?:   string
  storageKey?:  string
  open?:        boolean
}>(), {
  defaultOpen: true,
  maxHeight:   '320px',
})
const emit = defineEmits<{ 'update:open': [boolean] }>()

const stored = props.storageKey ? localStorage.getItem(props.storageKey) : null
const inner = ref(stored === null ? props.defaultOpen : stored === '1')
watch(inner, v => {
  if (props.storageKey) { try { localStorage.setItem(props.storageKey, v ? '1' : '0') } catch { /* ignore */ } }
})
// `undefined` is what distinguishes the two modes, so a parent that passes `:open="false"` is still
// controlling it — not falling back to the internal state, which would read as a section that will not
// stay shut.
const open = computed({
  get: () => (props.open === undefined ? inner.value : props.open),
  set: (v: boolean) => { props.open === undefined ? (inner.value = v) : emit('update:open', v) },
})
</script>

<template>
  <div class="collapsible-section">
    <button class="cs-toggle cc-section-toggle" @click="open = !open"
      v-tooltip.right="tip || (open ? `Collapse ${label}` : `Expand ${label}`)">
      <i :class="['pi', open ? 'pi-chevron-up' : 'pi-chevron-down']" />
      <span class="cs-label cc-eyebrow">{{ label }}</span>
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

/* the row itself is .cc-section-toggle; what's left here is this component's own chrome — the
   panel-rail bar (a surface + generous padding) that the bare inline adopters deliberately omit */
.cs-toggle {
  padding: 0.35rem 1rem;
  background: var(--cc-surface-1);
  font-size: var(--cc-fs-xs);
}
.cs-toggle:hover { background: var(--cc-surface-2); }

/* the heading IS the eyebrow scenario — colour/weight/tracking/case/size all come from .cc-eyebrow,
   which this component previously hand-rolled (and at a size the scale had no step for) */
.cs-label { color: inherit; }

.cs-body {
  overflow-y: auto;
  background: var(--cc-bg);
}
</style>
