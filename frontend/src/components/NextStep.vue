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

// `footer`: the same block as the right panel's, floating in the page's bottom-right corner — a page
// with no right panel (Gate, Analysis) keeps Next where task pages have it
// (docs/todo/GUI_LAYOUT_PLAN.md, M5; mock frame 2).
defineProps<{ footer?: boolean }>()
const route = useRoute()
const router = useRouter()
const project = useProjectStore()

const recipe = computed(() => resolveSetRecipe(project.activeSet()))
const next = computed(() => recipe.value ? nextStepFor(recipe.value.recipe, route.path) : null)
</script>

<template>
  <div v-if="recipe && next" class="next-step" :class="{ footer }" data-guide="layout.nextStep">
    <span class="cc-eyebrow">Next</span>
    <span class="next-src cc-muted cc-fs-xs">{{ nextSourceLine(recipe) }}</span>
    <button class="cc-btn cc-btn-next" @click="router.push(next.route)" v-tooltip.top="next.why">
      {{ next.label }} <i class="pi pi-arrow-right" />
    </button>
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
.next-step .cc-btn-next { justify-content: center; width: 100%; }
.next-step.footer {
  position: absolute; right: var(--cc-space-lg); bottom: var(--cc-space-lg); z-index: 5;
  width: 280px; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-md);
  box-shadow: 0 6px 18px rgba(0, 0, 0, 0.35);   /* the popover shadow (TeleportPopover) */
}
</style>
