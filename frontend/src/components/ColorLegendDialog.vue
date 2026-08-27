<script setup lang="ts">
/*
  The colour glossary — every colour this app assigns a MEANING to, grouped by family. Sibling to
  IconLegendDialog.vue: one explains what a glyph means, this explains what a swatch means.

  Renders `lib/colorLegend.ts`, which is also the authors' reference and is pinned by
  `colorLegend.test.ts` (a new colour token, palette or track mode fails the suite until it has a
  meaning). So this dialog cannot drift from the app: it has no list of its own.

  Opened from the header palette beside the key — "what does this colour mean?" is asked while
  looking at the colour, so it is one click from anywhere rather than buried in Settings.
*/
import { computed, ref } from 'vue'
import BaseModal from './BaseModal.vue'
import { COLOR_LEGEND } from '../lib/colorLegend'
import { closeColorLegend } from '../lib/colorLegendOpen'

const query = ref('')

// Filter on the meaning as well as the token id: someone looking for "warning" does not know the
// token is `--cc-sev-warn`, which is the whole reason they opened this.
const families = computed(() => {
  const q = query.value.trim().toLowerCase()
  if (!q) return COLOR_LEGEND
  return COLOR_LEGEND
    .map(f => ({ ...f, entries: f.entries.filter(e =>
      e.token.toLowerCase().includes(q) || e.means.toLowerCase().includes(q)) }))
    .filter(f => f.entries.length)
})

const total = computed(() => COLOR_LEGEND.reduce((n, f) => n + f.entries.length, 0))
const shown = computed(() => families.value.reduce((n, f) => n + f.entries.length, 0))
</script>

<template>
  <BaseModal title="What the colours mean" icon="pi-palette" width="720px" height="80vh"
             @close="closeColorLegend">
    <template #toolbar>
      <div class="cl-toolbar cc-row">
        <span class="cl-search">
          <i class="pi pi-search" />
          <input v-model="query" class="cl-input cc-input-xs" type="text"
                 placeholder="warning, failed, track…"
                 v-tooltip.bottom="'Search by meaning or token name'" />
        </span>
        <span class="cc-muted cc-fs-xs">{{ shown === total ? `${total} colours` : `${shown} of ${total}` }}</span>
      </div>
    </template>

    <div class="cl-body">
      <section v-for="f in families" :key="f.title" class="cl-family">
        <h3 class="cl-title cc-eyebrow cc-fs-2xs">{{ f.title }}</h3>
        <p v-if="f.note" class="cl-note cc-muted cc-fs-xs">{{ f.note }}</p>
        <div class="cl-grid">
          <div v-for="e in f.entries" :key="e.token" class="cl-row">
            <!-- Swatch: a single tinted square for a token, a strip of dots for a palette, a linear
                 gradient bar for the heat ramp, and an empty placeholder for a behavioural token
                 (track-mode:solid — the colour depends on the source, not on a fixed hex). -->
            <span class="cl-swatch">
              <span v-if="e.swatch.kind === 'var'" class="cl-chip"
                    :style="{ background: `var(${e.swatch.cssVar})` }" />
              <span v-else-if="e.swatch.kind === 'palette'" class="cl-strip cc-row-tight">
                <span v-for="(hex, i) in e.swatch.hexes" :key="i" class="cl-dot"
                      :style="{ background: hex }" />
              </span>
              <span v-else-if="e.swatch.kind === 'gradient'" class="cl-chip cl-gradient"
                    :style="{ background: `linear-gradient(90deg, ${e.swatch.hexes.join(', ')})` }" />
              <span v-else class="cl-chip cl-none" />
            </span>
            <span class="cl-means">{{ e.means }}</span>
            <span class="cc-uid cl-name">{{ e.token }}</span>
          </div>
        </div>
      </section>

      <p v-if="!families.length" class="cc-muted cc-fs-xs cl-empty">No colour matches that.</p>
    </div>
  </BaseModal>
</template>

<style scoped>
/* Toolbar strip matches IconLegendDialog.vue exactly — the two dialogs sit beside each other in the
   header and asking one to be visually different is a "which of these looks right" trap. */
.cl-toolbar {
  width: 100%;
  justify-content: space-between;
  padding: 0.6rem 1rem;
  border-bottom: 1px solid var(--cc-border);
  flex-shrink: 0;
}
.cl-search { display: inline-flex; align-items: center; gap: 0.35rem; flex: 1; min-width: 0; }
.cl-input  { flex: 1; min-width: 0; max-width: 22rem; }

.cl-body   { display: flex; flex-direction: column; gap: 1rem; }
.cl-family { display: flex; flex-direction: column; gap: 0.25rem; }
.cl-note   { margin: 0 0 0.2rem; }

/* One row per colour. The swatch column has to fit the widest palette (12 dots) without spilling
   into the means text — that spill was the first thing this dialog got wrong. Column width is a
   compromise: too wide leaves single-chip rows with a wide gutter; too narrow makes the palette
   strip overlap. 6.5rem + a wrapping strip fits 12 dots on two lines and a single chip on one. */
.cl-grid   { display: grid; grid-template-columns: repeat(auto-fill, minmax(23rem, 1fr)); gap: 0.15rem 0.75rem; }
.cl-row    {
  display: grid;
  grid-template-columns: 6.5rem 1fr auto;
  align-items: center;
  gap: 0.6rem;
  padding: 0.25rem 0;
  min-width: 0;
}
/* min-width:0 + overflow:hidden are the belt and braces — a palette or gradient inside must be
   allowed to shrink under the grid track, and can never paint outside its column. */
.cl-swatch { display: flex; align-items: center; justify-content: flex-start; min-width: 0; overflow: hidden; }
.cl-chip {
  display: inline-block;
  width: 3rem;
  height: 1rem;
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-xs);
}
/* The palette strip is a row of small dots — one per colour. Layout comes from `.cc-row-tight`
   (composed on the element); only the wrap-width and dot chrome are our business here.
   Wraps to a second line at 12 colours (cecelia); a 7-8 colour palette stays on one line. */
.cl-strip  { max-width: 6rem; }
.cl-dot    { width: 0.55rem; height: 0.55rem; border-radius: var(--cc-radius-pill); border: 1px solid var(--cc-border); }
/* Empty chip for a behavioural token (track-mode:solid — the colour is per-source, not a hex). */
.cl-none   { background: repeating-linear-gradient(45deg, var(--cc-surface-2), var(--cc-surface-2) 4px, var(--cc-surface-1) 4px, var(--cc-surface-1) 8px); }
.cl-gradient { border: 1px solid var(--cc-border); }

.cl-means  { min-width: 0; }
/* The token id is for whoever is writing code, so it is the quietest thing in the row. */
.cl-name   { grid-column: 3; }
.cl-empty  { margin: 0; }
</style>
