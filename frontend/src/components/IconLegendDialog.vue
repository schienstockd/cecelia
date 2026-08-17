<script setup lang="ts">
/*
  The icon glossary — every glyph in the app and what it means, grouped by family.

  Renders `lib/iconLegend.ts`, which is also the authors' reference and is pinned by
  `iconLegend.test.ts` (a new icon fails the suite until it has a meaning). So this dialog cannot drift
  from the app: it has no list of its own.

  Opened from the header key beside Guides — "what does this symbol mean?" is asked while looking at the
  symbol, so it is one click from anywhere rather than buried in Settings.
*/
import { computed, ref } from 'vue'
import BaseModal from './BaseModal.vue'
import { ICON_LEGEND } from '../lib/iconLegend'
import { closeIconLegend } from '../lib/iconLegendOpen'

const query = ref('')

// Filter on the meaning as well as the glyph name: someone looking for "delete" does not know it is
// called `pi-trash`, which is the whole reason they opened this.
const families = computed(() => {
  const q = query.value.trim().toLowerCase()
  if (!q) return ICON_LEGEND
  return ICON_LEGEND
    .map(f => ({ ...f, icons: f.icons.filter(i =>
      i.icon.includes(q) || i.means.toLowerCase().includes(q)) }))
    .filter(f => f.icons.length)
})

const total = computed(() => ICON_LEGEND.reduce((n, f) => n + f.icons.length, 0))
const shown = computed(() => families.value.reduce((n, f) => n + f.icons.length, 0))
</script>

<template>
  <BaseModal title="What the icons mean" icon="pi-key" width="720px" height="80vh"
             @close="closeIconLegend">
    <template #toolbar>
      <div class="il-toolbar cc-row">
        <span class="il-search">
          <i class="pi pi-search" />
          <input v-model="query" class="il-input cc-input-xs" type="text" placeholder="delete, hidden, warning…"
                 v-tooltip.bottom="'Search by meaning or icon name'" />
        </span>
        <span class="cc-muted cc-fs-xs">{{ shown === total ? `${total} icons` : `${shown} of ${total}` }}</span>
      </div>
    </template>

    <div class="il-body">
      <section v-for="f in families" :key="f.title" class="il-family">
        <h3 class="il-title cc-eyebrow cc-fs-2xs">{{ f.title }}</h3>
        <p v-if="f.note" class="il-note cc-muted cc-fs-xs">{{ f.note }}</p>
        <div class="il-grid">
          <div v-for="i in f.icons" :key="i.icon" class="il-row">
            <i :class="['pi', i.icon, 'il-glyph']" />
            <span class="il-means">{{ i.means }}</span>
            <span class="cc-uid il-name">{{ i.icon }}</span>
          </div>
        </div>
      </section>

      <p v-if="!families.length" class="cc-muted cc-fs-xs il-empty">No icon matches that.</p>
    </div>
  </BaseModal>
</template>

<style scoped>
/* BaseModal renders `<slot name="toolbar" />` BARE — no padding, no border — so every consumer supplies
   its own strip. These are PackagesDialog's values (`.pk-toolbar`), the dialog this one sits beside in
   the header and the closest thing to it in shape (search + a count). Not extracted into BaseModal:
   only two dialogs have a padded strip and they disagree on the vertical padding (0.6 vs FileBrowser's
   0.4), which is the n=2 trap in docs/todo/UX_PRIMITIVES_PLAN.md. */
.il-toolbar {
  width: 100%;
  justify-content: space-between;
  padding: 0.6rem 1rem;
  border-bottom: 1px solid var(--cc-border);
  flex-shrink: 0;
}
.il-search { display: inline-flex; align-items: center; gap: 0.35rem; flex: 1; min-width: 0; }
.il-input { flex: 1; min-width: 0; max-width: 22rem; }

.il-body { display: flex; flex-direction: column; gap: 1rem; }
.il-family { display: flex; flex-direction: column; gap: 0.25rem; }
.il-note { margin: 0 0 0.2rem; }

/* Two columns where there is room: the list is long, and a single column makes the reader scroll past
   families they are not looking for. */
.il-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(19rem, 1fr)); gap: 0.1rem 0.75rem; }
.il-row {
  display: grid;
  grid-template-columns: 1.25rem 1fr auto;
  align-items: baseline;
  gap: 0.4rem;
  padding: 0.15rem 0;
  min-width: 0;
}
.il-glyph { color: var(--cc-text); text-align: center; }
.il-means { min-width: 0; }
/* The glyph NAME is for whoever is writing code, so it is the quietest thing in the row. */
.il-name { grid-column: 3; }
.il-empty { margin: 0; }
</style>
