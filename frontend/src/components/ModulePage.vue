<script setup lang="ts">
// The shell for a STANDALONE module page — one that isn't built on `ModuleLayout` (the image-table
// family). Fixes the page frame so Notebooks / Animation / Movies read as one app: page-level controls
// on one row, content below.
//
// Extracted because the three had each grown their own: three h1 sizes (1.1 / 1.15 / 1.4rem), two
// paddings, two subtitle widths, and `.nb-header` / `.anim-head` / `.mov-head` doing the same
// flex-space-between under different names.
//
// NO TITLE, and NO SUBTITLE SLOT — both deliberate, and the same argument twice. The three pages
// carried a paragraph each explaining the feature to a first-time reader; those went first. The `<h1>`
// followed (Dominik, 2026-08-10): the sidebar already says which page you are on and highlights it, so
// a heading repeating that word is chrome the daily user reads past forever. Explanations live in
// `docs/`, and what the page IS, its controls say.
//
// Per-page extras (e.g. Notebooks' reading max-width) go on the call site as a plain class: Vue puts
// the parent's scope ID on a child component's root, so a scoped rule in the page still applies here.
withDefaults(defineProps<{
  /**
   * How the page handles its own height — the one axis the three pages genuinely differed on:
   *  - `flow`   content flows, an ancestor scrolls (a document-shaped page)
   *  - `scroll` the page is viewport-height and scrolls itself
   *  - `fill`   viewport-height flex column; a CHILD owns the scrolling (a player/canvas pane)
   */
  layout?: 'flow' | 'scroll' | 'fill'
}>(), { layout: 'flow' })
</script>

<template>
  <div class="mp" :class="`mp-${layout}`">
    <!-- no controls, no header: an empty bar would still take its margin -->
    <header v-if="$slots.controls" class="mp-head">
      <div class="mp-ctl cc-row cc-row-loose"><slot name="controls" /></div>
    </header>
    <slot />
  </div>
</template>

<style scoped>
.mp { padding: 1rem 1.25rem; }
.mp-scroll { height: 100%; overflow: auto; }
/* `min-height: 0` is what lets a flex child actually shrink and scroll rather than growing past the
   viewport — without it the content area pushes the page taller instead of scrolling. */
.mp-fill { display: flex; flex-direction: column; height: 100%; min-height: 0; }

.mp-head {
  display: flex; align-items: flex-start;
  gap: 1rem; flex-wrap: wrap; margin-bottom: 0.8rem;
}
</style>
