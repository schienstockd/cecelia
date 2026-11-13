<!--
  The map at the front of the Guides dialog: where do I start, and where is everything. A map, not a
  manual — names, icons, one hover line each; no paragraphs (docs/todo/WORKFLOW_RECIPES_PLAN.md → *The map*).

    Recipes      pick your kind of data; its pages light up and its steps list below. None picked =
                 nothing lit: there is no default path, it depends on the data and the question.
    The groups   one card per sidebar group, a button per page: click goes there. A card's heading
                 opens that group's guides in the accordion below (the map IS the accordion's index).
    Maturity     a dot per tier on each page button, read through the legend (utils/guideMap.ts → chipDot), and the
                 MaturityMap section below: every feature, expandable to its evidence and next steps.
    Claude       one toggle: ✦ on the pages where Claude makes something (mapData.ts → claude), the two
                 ways in (ClaudeWaysIn.vue — the same cards as Kiwi's ?)
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
import { useObserverStore } from '../stores/observer'
import { useViewProfilesStore } from '../stores/viewProfiles'
import { RECIPES, isWanted, guideById, isGuideStep, type WrittenRecipe, type RecipeStep } from '../lib/guides'
import type { GuideDef } from '../lib/guides/types'
import ClaudeWaysIn from './ClaudeWaysIn.vue'
import MaturityMap from './MaturityMap.vue'
import { tierTip } from '../lib/guides/tiers'
import { recipeRequestUrl } from '../lib/links'
import { closeGuides } from '../lib/guideOpen'
import { buildMap, chipTip, chipDot, tierLabel, TIER_LEGEND, mapCaption } from '../utils/guideMap'
import { availablePaths, applyProfile } from '../utils/viewProfiles'
import { NAV_GROUPS } from '../lib/navGroups'

const recipeId = defineModel<string>('recipe', { default: '' })
const emit = defineEmits<{ (e: 'open-group', heading: string): void }>()

const guide = useGuideStore()
const settings = useSettingsStore()
// "AI help" counts only while AI is on — off, the map shows no AI at all and hides its toggle.
const observer = useObserverStore()
const showAi = computed(() => settings.guidesShowClaude && observer.ai.enabled)
const layouts = useViewProfilesStore()
const router = useRouter()

const written = RECIPES.filter(r => !isWanted(r)) as WrittenRecipe[]
const recipeOptions: ChipOption[] = written.map(r => ({ value: r.id, label: r.title, icon: r.icon, tip: r.whenThisIsYou }))
const recipe = computed(() => written.find(r => r.id === recipeId.value) ?? null)

const groups = computed(() =>
  buildMap(recipe.value, availablePaths(applyProfile(NAV_GROUPS, layouts.activeItems))))

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
  s.assist ? `${s.assist.where === 'kiwi' ? 'Kiwi or Claude Code' : 'Claude Code'}: ${s.assist.text}` : ''
</script>

<template>
  <section class="gm">
    <div class="gm-top cc-row">
      <span class="cc-eyebrow cc-fs-2xs">Recipes</span>
      <ChipSelect v-model="recipeId" :options="recipeOptions" allow-empty aria-label="Recipe" />
      <span class="gm-grow" />
      <CcToggle v-if="observer.ai.enabled" v-model="settings.guidesShowClaude" label="AI help"
                v-tooltip.bottom="'Show where Kiwi or Claude Code helps — optional, needs a Claude plan'" />
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
            <i v-if="showAi && r.s.assist" class="pi pi-sparkles gm-assist"
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
                :class="['gm-page cc-btn cc-btn-bare cc-fs-xs', { lit: c.lit, dim: c.dim, hidden: c.hidden }]"
                v-tooltip.right="chipTip(c, showAi)" @click="go(c.route)">
          <i :class="['pi', c.icon]" />
          <span class="gm-label">{{ c.label }}</span>
          <span class="gm-marks">
            <i v-if="showAi && c.claude" class="pi pi-sparkles gm-assist" aria-label="Claude makes something here" />
            <span v-if="chipDot(c.tier)" :class="['cc-dot', chipDot(c.tier)]" :aria-label="tierLabel(c.tier)" />
          </span>
        </button>
      </div>
    </div>

    <div class="gm-legend cc-row cc-row-loose cc-muted cc-fs-2xs" aria-label="Maturity legend">
      <span v-for="l in TIER_LEGEND" :key="l.tier" class="gm-legend-item cc-row cc-row-tight" v-tooltip.top="tierTip(l.tier)">
        <span :class="['cc-dot', l.dot]" /> {{ l.label }}
      </span>
    </div>
    <p class="gm-caption cc-muted cc-fs-2xs">{{ mapCaption(recipe) }}</p>

    <!-- how far to trust each feature: one expandable row per feature, the unguided ones marked "no guide" -->
    <MaturityMap />

    <!-- Claude: the two ways in, in the same words as Kiwi's ? and its guide -->
    <ClaudeWaysIn v-if="showAi" />

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
/* the ✦ wears the same quiet ink as the Maturity rows' bug button — markers, not controls */
.gm-assist { color: var(--cc-text-dim); }

.gm-grid { display: grid; grid-template-columns: repeat(5, 1fr); gap: 8px; }
.gm-card { padding: 8px; display: flex; flex-direction: column; gap: 2px; }
.gm-card-head { font-weight: 600; justify-content: flex-start; padding: 2px 4px; }
/* one line, never wraps: a wrapped row shifts every row below it out of line with the next card.
   A label too long for the card truncates (the tooltip carries it in full). */
.gm-page { justify-content: flex-start; gap: 4px; padding: 2px 4px; text-align: left; min-width: 0; }
.gm-label { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; }
/* the markers sit at the right edge, so the dots read as one column per card. The dot is a quiet
   maturity marker; the full tier is in the Maturity rows below, and in the button's tooltip */
.gm-marks { display: inline-flex; align-items: center; gap: 3px; flex-shrink: 0; }
.gm-marks:empty { display: none; }
/* lit = on the path. Not colour alone: lit pages are full-strength AND bold */
.gm-page.lit { font-weight: 600; }
.gm-page.dim { opacity: 0.55; }
.gm-page.hidden { text-decoration: line-through; }

.gm-caption { margin: 0; }
.gm-legend { flex-wrap: wrap; }


.gm-ask { margin: 0; }
</style>
