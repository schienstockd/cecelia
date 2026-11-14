<!--
  The Guides dialog's first view: one choice and one button (docs/todo/WORKFLOW_RECIPES_PLAN.md →
  *The dialog's first view*). Which of the three it shows is `utils/setRecipe.ts` → `guideStartView`;
  which step is next and which are ticked is `recipeProgress`, the same rule as Welcome's Continue,
  so the two never name different steps for one set.

    import   nothing imported yet: Import images, and Start
    choose   "What are you working with?": the written recipes in Welcome's table, the set's suggestion
             named on its row, never applied. Selecting saves it as the set's recipe (the same write as Welcome and the set row's
             picker); with no set active it is walked in place and saved nowhere
    recipe   the next step as a card (title, why-line, Start guide / Open), a tick line for what has run
             on the set, and every step behind "All steps"

  Ticks are "a successful run on this set" (`ranPagesOf`), not "finished the guide": a step done without
  its guide still counts. Every guide by sidebar group is one click away behind "Browse all guides"
  (`@browse`). The AI help toggle (`settings.guidesShowClaude`, shown only while AI is on) adds each step's
  `assist` line — what Kiwi or Claude Code adds there.
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import { useRouter } from 'vue-router'
import CollapsibleSection from './CollapsibleSection.vue'
import CcToggle from './CcToggle.vue'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'
import { useGuideStore } from '../stores/guide'
import { useProjectStore } from '../stores/project'
import { useSettingsStore } from '../stores/settings'
import { useObserverStore } from '../stores/observer'
import { useSetRecipe } from '../composables/useSetRecipe'
import { guideById, isGuideStep } from '../lib/guides'
import type { GuideDef } from '../lib/guides/types'
import { closeGuides } from '../lib/guideOpen'
import { tierTip } from '../lib/guides/tiers'
import { chipDot, tierLabel } from '../utils/guideMap'
import { recipeRequestUrl } from '../lib/links'
import { guideStartView, recipeProgress, ranPagesOf, recipeChoiceRows, RECIPE_CHOICE_COLUMNS, type StepProgress } from '../utils/setRecipe'

const emit = defineEmits<{ (e: 'browse'): void }>()

const guide = useGuideStore()
const project = useProjectStore()
const settings = useSettingsStore()
const observer = useObserverStore()
const router = useRouter()
const { set, save, busy } = useSetRecipe()

// Dialog-local on purpose: the dialog is re-decided on every open from the set, and a choice that matters is saved on the set, not here.
const picked = ref<string | null>(null)
const changing = ref(false)

const projectImages = computed(() => project.sets.reduce((n, s) => n + s.images.length, 0))
const view = computed(() => guideStartView({
  projectImages: projectImages.value, set: set.value, picked: picked.value, changing: changing.value,
}))

const progress = computed(() => view.value.kind === 'recipe'
  ? recipeProgress(view.value.recipe, set.value ? ranPagesOf(set.value.images) : new Set())
  : null)
const required = computed(() => progress.value?.rows.filter(r => r.n !== null) ?? [])
const optional = computed(() => progress.value?.rows.filter(r => r.n === null) ?? [])
const ranRows = computed(() => required.value.filter(r => r.ran && r !== progress.value?.next))

const importGuide = guideById('import-images') ?? null
const showAi = computed(() => settings.guidesShowClaude && observer.ai.enabled)

const defOf = (r: StepProgress): GuideDef | null => isGuideStep(r.step) ? (guideById(r.step.guide) ?? null) : null
const titleOf = (r: StepProgress) => defOf(r)?.title ?? (isGuideStep(r.step) ? r.step.guide : r.step.label)
// The maturity dot, as on Browse's guide rows (`chipDot`): every tier that has one, so an unmarked step
// never reads as "fine"; the hover says what the tier asks of you.
const tierOf = (r: StepProgress) => defOf(r)?.maturity.tier ?? null
const misses = (g: GuideDef | null) => (g ? guide.prereqState(g).filter(p => !p.met) : [])
const assistLine = (r: StepProgress) => r.step.assist
  ? `${r.step.assist.where === 'kiwi' ? 'Kiwi or Claude Code' : 'Claude Code'}: ${r.step.assist.text}` : ''

function startGuide(g: GuideDef) { guide.start(g.id); closeGuides() }
function startRow(r: StepProgress) {
  const g = defOf(r)
  if (g) startGuide(g)
  else if (r.route) { void router.push(r.route); closeGuides() }
}

// The chooser: Welcome's rows and columns (`recipeChoiceRows`), the suggestion named, never pre-selected.
const recipeRows = computed(() => recipeChoiceRows(view.value.kind === 'choose' ? view.value.suggestion : null))
const RECIPE_COLUMNS: SelectionColumn[] = RECIPE_CHOICE_COLUMNS

async function choose(id: string) {
  if (view.value.kind === 'choose' && view.value.saves) await save(id)
  else picked.value = id
  changing.value = false
}
</script>

<template>
  <section class="gn">
    <!-- import: nothing in the project yet -->
    <template v-if="view.kind === 'import'">
      <div class="gn-next cc-card cc-card-2">
        <span class="cc-eyebrow cc-fs-2xs">Start here</span>
        <span class="gn-title">{{ importGuide?.title ?? 'Import images' }}</span>
        <p class="gn-why cc-muted cc-fs-xs">{{ importGuide?.summary }}</p>
        <div>
          <button v-if="importGuide" class="cc-btn cc-btn-primary" @click="startGuide(importGuide)"
                  v-tooltip.top="`Start: ${importGuide.title}`">Start guide</button>
        </div>
      </div>
    </template>

    <!-- choose: which recipe -->
    <template v-else-if="view.kind === 'choose'">
      <h3 class="gn-q">What are you working with?</h3>
      <!-- the same table and rows as Welcome's recipe question (WelcomeModule.vue), so the two read alike -->
      <SelectionTable :columns="RECIPE_COLUMNS" :rows="recipeRows" id-key="id" fit="fill"
                      model-value="" :disabled="busy" @update:model-value="id => choose(String(id))" />
      <p class="gn-ask cc-muted cc-fs-xs">
        None of these?
        <a :href="recipeRequestUrl('')" target="_blank" rel="noopener"
           v-tooltip.top="'Tell us what you image, and send an example'">Ask for a recipe <i class="pi pi-external-link" /></a>
      </p>
    </template>

    <!-- recipe: the next step, then the rest on request -->
    <template v-else-if="progress">
      <div class="gn-recipe cc-row">
        <span class="gn-recipe-title cc-row cc-row-tight"><i :class="['pi', view.recipe.icon]" /> {{ view.recipe.title }}</span>
        <span class="cc-muted cc-fs-xs">{{ view.saved ? 'this set\'s recipe' : 'not saved: no set open' }}</span>
        <span class="gn-grow" />
        <button class="cc-btn cc-btn-bare cc-btn-dense cc-fs-xs" @click="changing = true"
                v-tooltip.top="'Select a different recipe'">Change</button>
      </div>

      <div v-if="progress.next" class="gn-next cc-card cc-card-2">
        <span class="cc-eyebrow cc-fs-2xs">
          {{ progress.next.ran ? 'Last step' : 'Next' }} · step {{ progress.next.n }} of {{ progress.required }}
        </span>
        <span class="gn-title cc-row cc-row-tight">
          {{ titleOf(progress.next) }}
          <span v-if="chipDot(tierOf(progress.next))" :class="['cc-dot', chipDot(tierOf(progress.next))]"
                :aria-label="tierLabel(tierOf(progress.next))" v-tooltip.top="tierTip(tierOf(progress.next)!)" />
        </span>
        <p class="gn-why cc-muted cc-fs-xs">{{ progress.next.step.why }}</p>
        <p v-if="showAi && progress.next.step.assist" class="gn-why cc-muted cc-fs-xs">
          <i class="pi pi-sparkles" /> {{ assistLine(progress.next) }}
        </p>
        <p v-if="misses(defOf(progress.next)).length" class="gn-miss cc-fs-2xs">
          <i class="pi pi-exclamation-circle" /> needs {{ misses(defOf(progress.next)).map(p => p.label).join(', ') }}
        </p>
        <div>
          <button class="cc-btn cc-btn-primary" @click="startRow(progress.next)"
                  v-tooltip.top="defOf(progress.next) ? `Start: ${titleOf(progress.next)}` : `Open ${titleOf(progress.next)}`">
            {{ defOf(progress.next) ? 'Start guide' : 'Open' }}
          </button>
        </div>
      </div>

      <p v-if="ranRows.length" class="gn-ticks cc-row cc-row-loose cc-muted cc-fs-xs">
        <span v-for="r in ranRows" :key="r.n!" class="gn-tick"><i class="pi pi-check" /> {{ titleOf(r) }}</span>
      </p>

      <CollapsibleSection :label="`All steps · ${progress.required}${optional.length ? ` + ${optional.length} optional` : ''}`"
                          :default-open="false" max-height="none" tip="Every step of this recipe">
        <ol class="gn-steps">
          <li v-for="r in required" :key="r.n!" :class="['gn-step', { cur: r === progress.next }]">
            <span class="gn-n cc-readout cc-fs-2xs">{{ r.n }}</span>
            <span class="gn-step-main">
              <span class="cc-row cc-row-tight">
                {{ titleOf(r) }}
                <span v-if="chipDot(tierOf(r))" :class="['cc-dot', chipDot(tierOf(r))]"
                      :aria-label="tierLabel(tierOf(r))" v-tooltip.top="tierTip(tierOf(r)!)" />
                <span v-if="!defOf(r)" class="cc-muted cc-fs-2xs">no guide</span>
              </span>
              <span v-if="r === progress.next" class="gn-why cc-muted cc-fs-xs">{{ r.step.why }}</span>
            </span>
            <i v-if="r.ran" class="pi pi-check gn-done" aria-label="Has run on this set" />
            <button v-else :class="['cc-btn cc-btn-dense cc-fs-xs', r === progress.next ? 'cc-btn-primary' : 'cc-btn-ghost']"
                    @click="startRow(r)">{{ defOf(r) ? 'Start' : 'Open' }}</button>
          </li>
        </ol>
        <template v-if="optional.length">
          <span class="gn-sub cc-eyebrow cc-fs-2xs">Only if you need it</span>
          <ol class="gn-steps">
            <li v-for="(r, i) in optional" :key="i" class="gn-step">
              <span class="gn-n cc-muted cc-fs-2xs">·</span>
              <span class="gn-step-main">
                <span class="cc-row cc-row-tight">
                  {{ titleOf(r) }}
                  <span v-if="chipDot(tierOf(r))" :class="['cc-dot', chipDot(tierOf(r))]"
                      :aria-label="tierLabel(tierOf(r))" v-tooltip.top="tierTip(tierOf(r)!)" />
                  <span v-if="!defOf(r)" class="cc-muted cc-fs-2xs">no guide</span>
                </span>
                <!-- an optional step's why IS its condition ("only if…"), so it always shows -->
                <span class="gn-why cc-muted cc-fs-xs">{{ r.step.why }}</span>
              </span>
              <i v-if="r.ran" class="pi pi-check gn-done" aria-label="Has run on this set" />
              <button v-else class="cc-btn cc-btn-ghost cc-btn-dense cc-fs-xs" @click="startRow(r)">
                {{ defOf(r) ? 'Start' : 'Open' }}
              </button>
            </li>
          </ol>
        </template>
      </CollapsibleSection>
    </template>

    <div class="gn-foot cc-row">
      <button class="cc-btn cc-btn-bare cc-btn-dense cc-fs-xs gn-browse" @click="emit('browse')"
              v-tooltip.top="'Every guide, by page'">Browse all guides</button>
      <span class="gn-grow" />
      <CcToggle v-if="observer.ai.enabled" v-model="settings.guidesShowClaude" label="AI help"
                v-tooltip.top="'Show where Kiwi or Claude Code helps at each step'" />
    </div>
  </section>
</template>

<style scoped>
.gn { display: flex; flex-direction: column; gap: 10px; }
.gn-grow { flex: 1; }
.gn-recipe { gap: 8px; }
.gn-recipe-title { font-weight: 600; }

/* the one card in the first view (`.cc-card .cc-card-2`): the accent border is what makes it the place to look */
.gn-next { display: flex; flex-direction: column; gap: 3px; padding: 12px 14px; border-color: var(--cc-accent); }
.gn-title { font-size: var(--cc-fs-lg); font-weight: 600; }
.gn-why { margin: 0; line-height: 1.4; }
.gn-next .gn-why:last-of-type { margin-bottom: 8px; }
.gn-miss { margin: 0 0 6px; color: var(--cc-sev-warn); }

.gn-ticks { margin: 0; flex-wrap: wrap; }
.gn-tick .pi, .gn-done { color: var(--cc-sev-ok); }

.gn-q { margin: 0; font-size: var(--cc-fs-lg); font-weight: 600; }
.gn-ask { margin: 0; }

.gn-steps { list-style: none; margin: 0; padding: 0; }
.gn-step {
  display: grid; grid-template-columns: 1.4rem 1fr auto; align-items: center; gap: 8px;
  padding: 5px 0; border-top: 1px solid var(--cc-border);
}
.gn-step.cur { background: var(--cc-surface-2); }
.gn-n { text-align: right; }
.gn-step-main { display: flex; flex-direction: column; min-width: 0; }
.gn-sub { display: block; margin: 10px 0 2px; }

.gn-foot { border-top: 1px solid var(--cc-border); padding-top: 8px; }
.gn-browse { color: var(--cc-accent-soft); }
</style>
