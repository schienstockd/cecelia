<!--
  Collapsible section used inside the image panel and anywhere a labeled toggle is needed.
  Props
  ─────
    label       string   Heading text shown in the toggle bar.
    defaultOpen bool     Whether the section starts open (default: true). In an accordion, marks THIS section as
                         the group's default (the one open before the user has chosen) — say it once per group.
    maxHeight   string   CSS max-height for the body div (default: '320px').
    storageKey  string   When set, the open/closed state is remembered per profile (profileStorage) under this
                         key (so it survives navigation — see the "persist every option" rule).
    accordion   string   ONE-OPEN ACCORDION keyword: every section given the same keyword is one group, and
                         opening one closes the others. The keyword IS the group's profileStorage key (so
                         it must be unique, like any storage key). Pair it with `section`. A script that
                         needs the same state (read which is open, open one programmatically) calls
                         `useSingleOpenSection(<keyword>)` and gets the SAME state object
                         (`utils/singleOpen.ts` — one registry per app instance, never two refs per key).
                         Takes precedence over `open` and `storageKey`.
    section     string   This section's id within its accordion (what is persisted; default: `label`).
    open        bool|null CONTROLLED mode, outside an accordion: pass it (with @update:open, or v-model:open)
                         and the parent owns open/closed. Omit it — it is `null` — and the section manages
                         itself. For one-open-at-a-time use `accordion`, not this.
                         `null` rather than `undefined` is load-bearing — see the note by the computed.
    fill        bool     ACCORDION FILL: while open, the section takes the remaining height of its flex-column
                         parent and its body scrolls inside it (closed, it is header-only). The parent must be
                         a height-bounded flex column. Overrides `maxHeight`. Kiwi's cockpit and the viewer
                         side panel use it — one open section, using all the room the others are not — and
                         so does ModuleLayout's maximised plots section (no accordion, same fill).
-->
<script setup lang="ts">
import { ref, watch, computed, onBeforeUnmount } from 'vue'
import { profileStorage } from '../utils/profileStorage'
import { onSectionOpen } from '../utils/sectionOpen'
import { useSingleOpenSection } from '../composables/useSingleOpenSection'

const props = withDefaults(defineProps<{
  label:        string
  // one short line on what the section is for — a collapsed section is a control like any other, and
  // "Sensitivity" alone does not say what it is sensitive TO (docs/UI.md → every control needs a tooltip)
  tip?:         string
  // null = not said (reads as true outside an accordion); in an accordion only an explicit `true` counts
  defaultOpen?: boolean | null
  maxHeight?:   string
  storageKey?:  string
  open?:        boolean | null
  accordion?:   string
  section?:     string
  fill?:        boolean
}>(), {
  defaultOpen: null,
  maxHeight:   '320px',
  open:        null,
})
const emit = defineEmits<{ 'update:open': [boolean] }>()

const stored = props.storageKey ? profileStorage.getItem(props.storageKey) : null
const inner = ref(stored === null ? props.defaultOpen !== false : stored === '1')
watch(inner, v => {
  if (props.storageKey) { try { profileStorage.setItem(props.storageKey, v ? '1' : '0') } catch { /* ignore */ } }
})
// opened / closed from outside (`utils/sectionOpen.ts` — e.g. a page folding its image table so a
// pointed-at plot is the first thing in view)
if (props.storageKey) onBeforeUnmount(onSectionOpen(props.storageKey, v => { inner.value = v }))
// `null` MARKS UNCONTROLLED, and it has to be null rather than undefined.
//
// An optional prop typed `boolean` is Boolean-CAST by Vue: absent means `false`, not `undefined`. So a
// check for `undefined` is never true, every one of the app's own uncontrolled sections silently
// becomes controlled by a parent that is not listening, and they are all stuck shut — which is how
// this shipped with the image table showing "1 / 1 image" and no rows. A type
// union with `null` plus an explicit default suppresses the cast, so absent really is absent.
//
// Two modes rather than one because a parent that passes `:open="false"` must still be CONTROLLING it;
// falling back to internal state there would read as a section that will not stay shut.
// Accordion mode: the group is fixed at setup (a section does not move between accordions).
const group = props.accordion ? useSingleOpenSection(props.accordion) : null
const sectionId = () => props.section ?? props.label
if (group && props.defaultOpen === true) group.offerDefault(sectionId())

const open = computed({
  get: () => (group ? group.isOpen(sectionId())
    : props.open === null ? inner.value : props.open),
  set: (v: boolean) => {
    if (group) { if (v) group.set(sectionId()); else if (group.isOpen(sectionId())) group.set('') }
    else if (props.open === null) inner.value = v
    else emit('update:open', v)
  },
})
</script>

<template>
  <div :class="['collapsible-section', { 'cs-fill': fill && open }]">
    <button class="cs-toggle cc-section-toggle" @click="open = !open"
      v-tooltip.right="tip || (open ? `Collapse ${label}` : `Expand ${label}`)">
      <i :class="['pi', open ? 'pi-chevron-up' : 'pi-chevron-down']" />
      <span class="cs-label cc-eyebrow">{{ label }}</span>
    </button>
    <!-- with a real max-height the body scrolls itself; with max-height:none it must NOT be a scroll
         container (overflow-y:auto would still make it the sticky scrollport, so a `position:sticky`
         descendant — e.g. the board's pop-manager rail — sticks to a box that never scrolls and never
         activates). Let the outer page scroll handle it. -->
    <div v-show="open" class="cs-body"
         :style="fill ? undefined : { maxHeight, overflowY: maxHeight === 'none' ? 'visible' : 'auto' }">
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

/* Two classes so it outranks a parent's scoped `> * { flex-grow: 0 }` reset without `!important`.
   `min-height: 0` lets the body scroll instead of pushing the parent taller; a parent that itself
   scrolls can raise it so a short panel does not squeeze the open section down to its header. */
.collapsible-section.cs-fill { flex: 1 1 0; min-height: 0; overflow: hidden; }
.collapsible-section.cs-fill > .cs-body { flex: 1 1 0; min-height: 0; overflow-y: auto; }

.cs-body {
  overflow-y: auto;
  background: var(--cc-bg);
  /* Breathing room so the first row's control (e.g. a CcToggle taller than its label) is not
     flush-clipped by the section's top edge. */
  padding: 0.35rem 0;
}
</style>
