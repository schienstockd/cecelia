<script setup lang="ts">
/*
  The right panel's "Next" footer (docs/todo/GUI_CLARITY_PLAN.md, Decisions 13, 15, 16): one button
  naming the page that comes after this one in the active set's recipe — its own, or the one its axes
  suggest. It only navigates; it never runs or selects anything. Logic: utils/setRecipe.ts.
*/
import { computed } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { useProjectStore } from '../stores/project'
import { resolveSetRecipe, nextStepFor, nextSourceLine } from '../utils/setRecipe'
import { useStoredSectionOpen } from '../utils/sectionOpen'

// `footer`: the same block as the right panel's, floating in the page's bottom-right corner — a page
// with no right panel (Gate, Analysis) keeps Next where task pages have it
// (docs/todo/GUI_LAYOUT_PLAN.md, M5; mock frame 2).
defineProps<{ footer?: boolean }>()
const route = useRoute()
const router = useRouter()
const project = useProjectStore()

const recipe = computed(() => resolveSetRecipe(project.activeSet()))
const next = computed(() => recipe.value ? nextStepFor(recipe.value.recipe, route.path) : null)
// Folded = just the button: once the user knows the way, the eyebrow and the recipe line are only in
// the way. One key per profile, so folding it on one page folds it on every page.
const open = useStoredSectionOpen('cc.nextStep.open', true)
</script>

<template>
  <div v-if="recipe && next" class="next-step" :class="{ footer, folded: !open }" data-guide="layout.nextStep">
    <div v-if="open" class="next-head">
      <span class="cc-eyebrow">Next</span>
      <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-dense next-fold" aria-label="Fold to the button"
              :aria-expanded="true" v-tooltip.top="'Fold to the button'" @click="open = false">
        <i class="pi pi-chevron-down" />
      </button>
    </div>
    <span v-if="open" class="next-src cc-muted cc-fs-xs">{{ nextSourceLine(recipe) }}</span>
    <div class="next-row">
      <button class="cc-btn cc-btn-next" @click="router.push(next.route)"
              v-tooltip.top="open ? next.why : `Next · ${nextSourceLine(recipe)}`">
        {{ next.label }} <i class="pi pi-arrow-right" />
      </button>
      <button v-if="!open" class="cc-btn cc-btn-bare cc-btn-icon cc-btn-dense next-fold" aria-label="Show why"
              :aria-expanded="false" v-tooltip.top="'Show why'" @click="open = true">
        <i class="pi pi-chevron-up" />
      </button>
    </div>
  </div>
</template>

<style scoped>
.next-step {
  display: flex;
  flex-direction: column;
  gap: var(--cc-space-2xs);
  padding: var(--cc-space-md);
  border-top: 1px solid var(--cc-border);
  background: var(--cc-surface-1);
}
.next-head, .next-row { display: flex; align-items: center; gap: var(--cc-space-2xs); }
.next-head { justify-content: space-between; }
.next-step .cc-btn-next { justify-content: center; flex: 1; }
.next-step.folded { padding: var(--cc-space-xs) var(--cc-space-md); }
.next-step.footer {
  position: absolute; right: var(--cc-space-lg); bottom: var(--cc-space-lg); z-index: 5;
  width: 280px; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-md);
  box-shadow: 0 6px 18px var(--cc-shadow);   /* the popover shadow (TeleportPopover) */
}
/* folded in the corner: shrink to the button, so it covers as little of the page as possible */
.next-step.footer.folded { width: auto; padding: var(--cc-space-xs); }
.next-step.footer.folded .cc-btn-next { flex: none; }
</style>
