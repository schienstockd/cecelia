<script setup lang="ts">
// The shell for a STANDALONE module page — one that isn't built on `ModuleLayout` (the image-table
// family). Fixes the page frame so Notebooks / Animation / Movies read as one app: title on the left,
// page-level controls on the right, content below.
//
// Extracted because the three had each grown their own: three h1 sizes (1.1 / 1.15 / 1.4rem), two
// paddings, two subtitle widths, and `.nb-header` / `.anim-head` / `.mov-head` doing the same
// flex-space-between under different names. The h1 sizes escaped the tokenisation sweep because
// `findRawValues` exempts anything over 15px as display type, so this was the one text scale with no
// canonical answer.
//
// NO SUBTITLE SLOT, deliberately. The three pages carried a paragraph each explaining the feature to a
// first-time reader — permanent noise on a screen its owner uses daily, and the clearest tell that a
// page was AI-written. The page title and its controls say what the page is; the explanation lives in
// `docs/`.
//
// Per-page extras (e.g. Notebooks' reading max-width) go on the call site as a plain class: Vue puts
// the parent's scope ID on a child component's root, so a scoped rule in the page still applies here.
withDefaults(defineProps<{
  title: string
  /** PrimeIcons class for a glyph before the title, e.g. `pi pi-book`. */
  icon?: string
  /**
   * How the page handles its own height — the one axis the three pages genuinely differed on:
   *  - `flow`   content flows, an ancestor scrolls (a document-shaped page)
   *  - `scroll` the page is viewport-height and scrolls itself
   *  - `fill`   viewport-height flex column; a CHILD owns the scrolling (a player/canvas pane)
   */
  layout?: 'flow' | 'scroll' | 'fill'
}>(), { icon: '', layout: 'flow' })
</script>

<template>
  <div class="mp" :class="`mp-${layout}`">
    <header class="mp-head">
      <h1 class="mp-title"><i v-if="icon" :class="icon" />{{ title }}</h1>
      <div v-if="$slots.controls" class="mp-ctl cc-row cc-row-loose"><slot name="controls" /></div>
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
  display: flex; align-items: flex-start; justify-content: space-between;
  gap: 1rem; flex-wrap: wrap; margin-bottom: 0.8rem;
}
.mp-title { margin: 0; font-size: 1.15rem; display: flex; align-items: center; gap: 0.5rem; }

</style>
