<script setup lang="ts">
/*
  The active set's recipe, in the module set row (docs/todo/GUI_CLARITY_PLAN.md, Decisions 13 and 16).
  Compact: the recipe's own icon and a caret; the name is on the tooltip and in the menu, so the row
  keeps its width (the full title on every page was too wide). A set with no recipe shows the
  suggested one's icon, dimmed and dotted, and the menu offers it; nothing is applied silently.
  Stored per set via `POST /api/sets/recipe`. Logic: utils/setRecipe.ts. Welcome asks the question
  in full.
*/
import { computed, ref } from 'vue'
import TeleportPopover from './TeleportPopover.vue'
import { useSetRecipe } from '../composables/useSetRecipe'
import { WRITTEN_RECIPES } from '../utils/setRecipe'

const { set, own, suggestion, busy, save } = useSetRecipe()
const open = ref(false)
const anchor = ref<HTMLElement | null>(null)

const shown = computed(() => own.value ?? suggestion.value?.recipe ?? null)
const tip = computed(() => own.value ? `Recipe: ${own.value.title} — click to change`
  : suggestion.value ? `No recipe yet — suggested: ${suggestion.value.recipe.title} (${suggestion.value.reason})`
  : 'No recipe — select one to light its pages')

async function pick(id: string | null) { open.value = false; await save(id) }
</script>

<template>
  <template v-if="set">
    <button ref="anchor" class="cc-btn cc-btn-ghost cc-btn-dense set-recipe" :class="{ 'set-recipe-unset': !own }"
      data-guide="set.recipe" :disabled="busy" aria-label="Recipe" aria-haspopup="menu"
      @click="open = !open" v-tooltip.bottom="tip">
      <i v-if="shown" :class="['pi', shown.icon]" />
      <template v-else>Recipe</template>
      <span v-if="!own && suggestion" class="cc-dot set-recipe-dot" aria-hidden="true" />
      <i class="pi pi-chevron-down set-recipe-caret" />
    </button>
    <TeleportPopover v-model="open" :anchor="anchor" placement="bottom-start" flush>
      <div class="cc-actions-menu" role="menu">
        <div class="cc-actions-head">Recipe for “{{ set.name }}”</div>
        <button v-for="r in WRITTEN_RECIPES" :key="r.id" class="cc-actions-item" role="menuitemradio"
          :aria-checked="own?.id === r.id" @click="pick(r.id)">
          <i :class="['pi', r.icon]" />
          <span class="set-recipe-name">{{ r.title }}</span>
          <span v-if="suggestion?.recipe.id === r.id" class="cc-muted cc-fs-xs">suggested</span>
          <i v-if="own?.id === r.id" class="pi pi-check" />
        </button>
        <button v-if="own" class="cc-actions-item" role="menuitem" @click="pick(null)">
          <i class="pi pi-times" /> No recipe
        </button>
      </div>
    </TeleportPopover>
  </template>
</template>

<style scoped>
.set-recipe { position: relative; gap: var(--cc-space-3xs); }
.set-recipe-unset > .pi:first-child { opacity: 0.55; }
.set-recipe-dot { position: absolute; top: 1px; right: 1px; }
.set-recipe-caret { font-size: var(--cc-fs-2xs); color: var(--cc-text-dim); }
.set-recipe-name { flex: 1; }
</style>
