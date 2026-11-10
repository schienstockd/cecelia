<script setup lang="ts">
/*
  The active set's recipe, in the set bar (docs/todo/GUI_CLARITY_PLAN.md, Decisions 13 and 16). A set
  with no recipe shows what its axes suggest and a "Use" button; nothing is applied silently. Stored
  per set via `POST /api/sets/recipe`. Logic: utils/setRecipe.ts.
*/
import { useSetRecipe } from '../composables/useSetRecipe'
import { WRITTEN_RECIPES } from '../utils/setRecipe'

const { set, own, suggestion, busy, save } = useSetRecipe()
</script>

<template>
  <div v-if="set" class="set-recipe cc-row" data-guide="set.recipe">
    <!-- a native select, like the set picker beside it: one line in a bar, one pick -->
    <select class="set-recipe-select" :value="own?.id ?? ''" :disabled="busy" aria-label="Recipe"
      @change="save(($event.target as HTMLSelectElement).value || null)"
      v-tooltip.bottom="'Lights the recipe\'s pages and names each next step'">
      <option value="">{{ suggestion ? `Suggested: ${suggestion.recipe.title}` : 'No recipe' }}</option>
      <option v-for="r in WRITTEN_RECIPES" :key="r.id" :value="r.id">{{ r.title }}</option>
    </select>
    <button v-if="suggestion" class="cc-btn cc-btn-ghost" :disabled="busy"
      @click="save(suggestion.recipe.id)"
      v-tooltip.bottom="`Suggested from the images (${suggestion.reason})`">
      Use
    </button>
  </div>
</template>

<style scoped>
.set-recipe { gap: var(--cc-space-xs); }
.set-recipe-select { max-width: 14rem; }
</style>
