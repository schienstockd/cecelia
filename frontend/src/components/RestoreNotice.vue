<script setup lang="ts">
// "This page was just filled in from somewhere — here is how to put it back." One transient line above
// a page's controls, with an Undo. Used by the Animation and Batch pages when a movie's saved config is
// opened for editing (docs/todo/MOVIE_MANAGEMENT_PLAN.md Phase 6).
//
// NOT `HintCallout`: that is a first-use hint, dismissed permanently per id, and never says anything
// specific to what just happened. This is the opposite scenario — it appears because of one action, it
// names that action, and it carries the way out of it.
defineProps<{
  /** What the page was filled in from — named, because the undo is only trustworthy if it is. */
  source: string
  /** What could not be restored (`restoreNote`), or '' when everything came back. */
  note?: string
}>()
defineEmits<{ undo: []; dismiss: [] }>()
</script>

<template>
  <div class="restore-notice" role="status">
    <i class="pi pi-history restore-icon" />
    <span class="restore-text">
      Loaded from <strong>{{ source }}</strong><template v-if="note"> · <span class="cc-muted">{{ note }}</span></template>
    </span>
    <button class="cc-btn cc-btn-ghost cc-btn-micro" @click="$emit('undo')"
            v-tooltip.bottom="'Put back what this page had before'">
      <i class="pi pi-undo" /> Undo
    </button>
    <button class="restore-x cc-btn cc-btn-bare cc-btn-icon cc-btn-dense" @click="$emit('dismiss')"
            v-tooltip.left="'Keep it'" aria-label="Dismiss">
      <i class="pi pi-times" />
    </button>
  </div>
</template>

<style scoped>
.restore-notice {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.35rem 0.6rem;
  background: color-mix(in srgb, var(--cc-accent) 10%, var(--cc-surface-1));
  border: 1px solid var(--cc-border);
  border-left: 2px solid var(--cc-accent);
  border-radius: var(--cc-radius-sm);
  font-size: var(--cc-fs-md);
  color: var(--cc-text);
  flex-shrink: 0;
}
.restore-icon { color: var(--cc-accent); font-size: var(--cc-fs-md); flex-shrink: 0; }
.restore-text { flex: 1; min-width: 0; }
.restore-x:hover { background: var(--cc-surface-2); color: var(--cc-text); }
</style>
