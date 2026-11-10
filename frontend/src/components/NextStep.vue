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

// `inline`: just the button, for a page with no right panel (the action bar hosts it); the source
// line moves into the tooltip.
defineProps<{ inline?: boolean }>()
const route = useRoute()
const router = useRouter()
const project = useProjectStore()

const recipe = computed(() => resolveSetRecipe(project.activeSet()))
const next = computed(() => recipe.value ? nextStepFor(recipe.value.recipe, route.path) : null)
</script>

<template>
  <button v-if="recipe && next && inline" class="cc-btn cc-btn-next next-inline" data-guide="layout.nextStep"
    @click="router.push(next.route)" v-tooltip.bottom="`${nextSourceLine(recipe)} — ${next.why}`">
    Next: {{ next.label }} <i class="pi pi-arrow-right" />
  </button>
  <div v-else-if="recipe && next" class="next-step" data-guide="layout.nextStep">
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
.next-inline { flex: none; }
</style>
