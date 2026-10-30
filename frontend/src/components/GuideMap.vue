<!--
  The map at the front of the Guides dialog: where do I start, and where is everything. A map, not a
  manual — names, icons, one hover line each; no paragraphs (docs/todo/WORKFLOW_RECIPES_PLAN.md → *The map*).

    Recipes      pick your kind of data; its pages light up and its steps list below. None picked =
                 the day-one pages lit.
    The groups   one card per sidebar group, a button per page: click goes there. A card's heading
                 opens that group's guides in the accordion below (the map IS the accordion's index).
    Pairs        drawn once: the page that defines a population ↔ the page that summarises it.
    Claude       one toggle: the loop (ClaudeLoop.vue — the same cards as Kiwi's ?)
                 and what Claude adds at each recipe step. Off by default — Cecelia works without it.

  All joins live in `utils/guideMap.ts` (tested); this file only renders them.
-->
<script setup lang="ts">
import { computed } from 'vue'
import { useRouter } from 'vue-router'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'
import CcToggle from './CcToggle.vue'
import { useGuideStore } from '../stores/guide'
import { useSettingsStore } from '../stores/settings'
import { useViewProfilesStore } from '../stores/viewProfiles'
import { RECIPES, isWanted, guideById, isGuideStep, type WrittenRecipe, type RecipeStep } from '../lib/guides'
import type { GuideDef } from '../lib/guides/types'
import ClaudeLoop from './ClaudeLoop.vue'
import { recipeRequestUrl } from '../lib/links'
import { closeGuides } from '../lib/guideOpen'
import { buildMap, mapPairs, chipTip, mapCaption } from '../utils/guideMap'
import { availablePaths, applyProfile } from '../utils/viewProfiles'
import { NAV_GROUPS } from '../lib/navGroups'

const recipeId = defineModel<string>('recipe', { default: '' })
const emit = defineEmits<{ (e: 'open-group', heading: string): void }>()

const guide = useGuideStore()
const settings = useSettingsStore()
const layouts = useViewProfilesStore()
const router = useRouter()

const written = RECIPES.filter(r => !isWanted(r)) as WrittenRecipe[]
const recipeOptions: ChipOption[] = written.map(r => ({ value: r.id, label: r.title, icon: r.icon, tip: r.whenThisIsYou }))
const recipe = computed(() => written.find(r => r.id === recipeId.value) ?? null)

const groups = computed(() =>
  buildMap(recipe.value, availablePaths(applyProfile(NAV_GROUPS, layouts.activeItems))))
const pairs = mapPairs()

function go(route: string) {
  void router.push(route)
  closeGuides()
}

// ── the picked recipe's steps ──
interface Row { n: number; s: RecipeStep; def: GuideDef | null; title: string }
const rows = computed<Row[]>(() => (recipe.value?.steps ?? []).map((s, i) => {
  const def = isGuideStep(s) ? (guideById(s.guide) ?? null) : null
  return { n: i + 1, s, def, title: def?.title ?? (isGuideStep(s) ? s.guide : s.label) }
}))
const isDone = (g: GuideDef | null) => !!g && guide.completed.has(g.id)
function startStep(r: Row) {
  if (r.def) { guide.start(r.def.id); closeGuides() }
  else if (!isGuideStep(r.s)) go(r.s.page)
}
const assistTip = (s: RecipeStep) =>
  s.assist ? `${s.assist.where === 'kiwi' ? 'Kiwi or a terminal session' : 'A terminal session'}: ${s.assist.text}` : ''
</script>

<template>
  <section class="gm">
    <div class="gm-top cc-row">
      <span class="cc-eyebrow cc-fs-2xs">Recipes</span>
      <ChipSelect v-model="recipeId" :options="recipeOptions" allow-empty aria-label="Recipe" />
      <span class="gm-grow" />
      <CcToggle v-model="settings.guidesShowClaude" label="Claude"
                v-tooltip.bottom="'Show where Claude helps'" />
    </div>

    <!-- the picked recipe: its steps, each with the fork it states -->
    <ol v-if="recipe" class="gm-steps">
      <li v-for="r in rows" :key="r.n" class="gm-step">
        <span class="gm-n cc-readout cc-fs-2xs">{{ r.n }}</span>
        <div class="gm-main">
          <div class="gm-head">
            <span class="gm-title">{{ r.title }}</span>
            <span v-if="r.s.optional" class="cc-muted cc-fs-2xs">optional</span>
            <span v-if="!r.def" class="cc-muted cc-fs-2xs">no guide</span>
            <span v-if="isDone(r.def)" class="gm-done"><i class="pi pi-check" /></span>
            <i v-if="settings.guidesShowClaude && r.s.assist" class="pi pi-sparkles gm-assist"
               :aria-label="assistTip(r.s)" v-tooltip.top="assistTip(r.s)" />
          </div>
          <p class="gm-why cc-muted cc-fs-xs">{{ r.s.why }}</p>
        </div>
        <button class="cc-btn cc-btn-primary cc-btn-dense cc-fs-xs" @click="startStep(r)"
                v-tooltip.left="r.def ? `Start: ${r.def.title}` : `Open ${r.title}`">
          {{ r.def ? (isDone(r.def) ? 'Again' : 'Start') : 'Open' }}
        </button>
      </li>
    </ol>

    <!-- the groups: a card each, a button per page -->
    <div class="gm-grid">
      <div v-for="g in groups" :key="g.heading" class="gm-card cc-card cc-card-2">
        <button class="gm-card-head cc-btn cc-btn-bare cc-fs-xs" @click="emit('open-group', g.heading)"
                v-tooltip.top="`${g.blurb} — show its guides`">
          {{ g.heading }}
        </button>
        <button v-for="c in g.chips" :key="c.route"
                :class="['gm-page cc-btn cc-btn-bare cc-fs-xs', { lit: c.lit, hidden: c.hidden }]"
                v-tooltip.right="chipTip(c)" @click="go(c.route)">
          <i :class="['pi', c.icon]" /> {{ c.label }}
        </button>
      </div>
    </div>

    <p class="gm-caption cc-muted cc-fs-2xs">{{ mapCaption(recipe) }}</p>

    <p class="gm-pairs cc-row cc-row-loose cc-muted cc-fs-2xs">
      <span v-for="([a, b], i) in pairs" :key="i" class="gm-pair">{{ a }} <i class="pi pi-arrow-right" /> {{ b }}</span>
    </p>

    <!-- Claude: the loop, in the same words as Kiwi's ? and its guide -->
    <ClaudeLoop v-if="settings.guidesShowClaude" />

    <p class="gm-ask cc-muted cc-fs-2xs">
      Not your kind of data?
      <a :href="recipeRequestUrl('')" target="_blank" rel="noopener"
         v-tooltip.top="'Tell us what you image, and send an example'">
        Ask for a recipe <i class="pi pi-external-link" />
      </a>
    </p>
  </section>
</template>

<style scoped>
.gm { display: flex; flex-direction: column; gap: 10px; margin-bottom: 14px; }
.gm-top { gap: 10px; flex-wrap: wrap; }
.gm-grow { flex: 1; }

.gm-steps { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 6px; }
.gm-step { display: grid; grid-template-columns: 1.4rem 1fr auto; align-items: start; gap: 8px; }
.gm-n { text-align: right; }
.gm-head { display: flex; align-items: center; gap: 6px; }
.gm-title { color: var(--cc-text); font-weight: 600; }
.gm-why { margin: 2px 0 0; }
.gm-done { color: var(--cc-accent); }
.gm-assist { color: var(--cc-accent); }

.gm-grid { display: grid; grid-template-columns: repeat(5, 1fr); gap: 8px; }
.gm-card { padding: 8px; display: flex; flex-direction: column; gap: 2px; }
.gm-card-head { font-weight: 600; justify-content: flex-start; padding: 2px 4px; }
.gm-page { justify-content: flex-start; gap: 6px; padding: 2px 4px; opacity: 0.55; text-align: left; }
/* lit = on the path. Not colour alone: lit pages are full-strength AND bold */
.gm-page.lit { opacity: 1; font-weight: 600; }
.gm-page.hidden { text-decoration: line-through; }

.gm-caption { margin: 0; }
.gm-pairs { margin: 0; flex-wrap: wrap; }
.gm-pair .pi { font-size: 0.7em; }

.gm-ask { margin: 0; }
</style>
