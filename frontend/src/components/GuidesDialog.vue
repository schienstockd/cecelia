<!--
  The guide picker: every guide, grouped like the sidebar, each with its prerequisites checked live.
  Opened from the compass button in AppHeader (and from a What's New tip card's "Show me").
  Design: docs/todo/GUIDE_SYSTEM_PLAN.md.

  Built on `BaseModal` like every other dialog (docs/UI.md → Modals & dialogs). Patterned on
  `ClaudeOverviewDialog.vue`: static content from a lib module, no store of its own beyond the shared
  open flag in `lib/guideOpen.ts`.

  LAYOUT — divided rows inside each group heading, not eleven boxed cards (Dominik, 2026-08-12). Three
  rules, all of them fixing something the first version got wrong:

    1. The action column is a FIXED width and holds exactly one button. It used to be a stretch column
       that also held the "X first" button, so Start took the width of its widest sibling — narrow on
       most rows, wide on the ones with a fix — and the right edge zigzagged down the dialog.
    2. Readiness and step count share ONE right-aligned meta slot, so they line up across every row
       instead of starting wherever the title happened to end.
    3. Only MISSING prerequisites get a line, with the fix as an inline link on that line. Listing the
       met ones as chips too was the main source of wrapping, which made every row a different height
       for no information — "needs an open project ✓" tells you nothing you wanted to know.

  Prerequisites are shown, never enforced (plan D6). Start is always enabled — a miss is a warning plus
  a pointer at the guide that fixes it, because the user may know something we can't see (they are
  about to open a project; the data is on a drive we haven't listed yet).
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import BaseModal from './BaseModal.vue'
import { useGuideStore } from '../stores/guide'
import { guidesByGroup, guideById, RECIPES, isWanted } from '../lib/guides'
import type { WrittenRecipe } from '../lib/guides'
import { closeGuides } from '../lib/guideOpen'
import { recipeRequestUrl } from '../lib/links'
import type { GuideDef } from '../lib/guides/types'

const guide = useGuideStore()
const groups = computed(() => guidesByGroup())

// ── recipes: the "which pipeline is mine" axis over the same guides (D1/D9) ───────────────────────
const written = computed(() => RECIPES.filter(r => !isWanted(r)) as WrittenRecipe[])
const wanted = computed(() => RECIPES.filter(isWanted))

// One open at a time. Expanding all of them would push the guide groups below the fold, and the
// question a recipe answers ("is this me?") is answered by the closed row — the steps are the detail.
const openRecipe = ref<string | null>(null)
const toggleRecipe = (id: string) => { openRecipe.value = openRecipe.value === id ? null : id }

// Resolved against the catalogue, dropping anything that doesn't resolve: `guides.test.ts` makes a
// dangling id impossible, so this is only about never rendering an empty row if one slips through.
function stepsOf(r: WrittenRecipe) {
  return r.steps
    .map(s => ({ ...s, def: guideById(s.guide) }))
    .filter((s): s is typeof s & { def: GuideDef } => !!s.def)
}

// How much of the recipe you could run right now. Per-step prereqs already answer this — including the
// derived "your view profile hides this page" miss — so there is nothing new to check here.
function readyCount(r: WrittenRecipe) {
  return stepsOf(r).filter(s => guide.prereqsMet(s.def)).length
}

function start(g: GuideDef) {
  guide.start(g.id)
  closeGuides()
}

const isDone = (g: GuideDef) => guide.completed.has(g.id)
const misses = (g: GuideDef) => guide.prereqState(g).filter(p => !p.met)

// The one-line readiness readout. Counting the misses beats naming them here — the names go on their
// own line directly below, where there is room for them.
function readiness(g: GuideDef) {
  const n = misses(g).length
  return n === 0 ? 'Ready' : `${n} missing`
}

// A missing prerequisite that names a fix gets a "start that one instead" link. First miss only:
// three redirects for a guide you can't run yet is noise.
function firstFixable(g: GuideDef) {
  const miss = misses(g).find(p => p.fixGuide)
  if (!miss?.fixGuide) return null
  const target = guideById(miss.fixGuide)
  return target && target.id !== g.id ? target : null
}
</script>

<template>
  <BaseModal width="660px" @close="closeGuides()">
    <!-- own #title so the compass carries the green mark, matching the header button it was opened
         from. BaseModal's plain `icon` prop would inherit the title's text colour. -->
    <template #title><i class="pi pi-compass gd-mark" /> Guides</template>
    <p class="gd-intro cc-muted cc-fs-md">
      Bubbles appear beside the control to use, on your own data, and step forward as you go.
    </p>

    <!-- RECIPES first: which pipeline is mine, before where in a pipeline am I. The three we have not
         written are named too, with a link that asks for what would let us write them (plan D9) — an
         absent row teaches nothing, a named one at least says we know the case exists. -->
    <section class="gd-group">
      <h3 class="gd-group-head cc-eyebrow cc-fs-2xs">What are you trying to do?</h3>

      <div v-for="r in written" :key="r.id">
        <button class="gd-recipe cc-section-toggle" @click="toggleRecipe(r.id)"
                v-tooltip.top="'Show the steps for this kind of data'">
          <i :class="['pi', openRecipe === r.id ? 'pi-chevron-down' : 'pi-chevron-right']" />
          <i :class="['pi', r.icon, 'gd-icon']" />
          <span class="gd-title">{{ r.title }}</span>
          <span class="gd-summary cc-muted cc-fs-xs">{{ r.whenThisIsYou }}</span>
          <span class="gd-meta cc-readout cc-fs-2xs">
            {{ readyCount(r) }}/{{ r.steps.length }} ready
          </span>
        </button>

        <ol v-if="openRecipe === r.id" class="gd-steps">
          <li v-for="(s, i) in stepsOf(r)" :key="s.guide" class="gd-row gd-step">
            <span class="gd-step-n cc-readout cc-fs-2xs">{{ i + 1 }}</span>

            <div class="gd-main">
              <div class="gd-head">
                <span class="gd-title">{{ s.def.title }}</span>
                <span v-if="s.optional" class="gd-opt cc-muted cc-fs-2xs">optional</span>
                <span v-if="isDone(s.def)" class="gd-done"><i class="pi pi-check" /></span>
              </div>
              <!-- the `why`: the fork this step exists to state, which no single guide can say -->
              <p class="gd-summary cc-muted cc-fs-xs">{{ s.why }}</p>
            </div>

            <div class="gd-act">
              <button class="cc-btn cc-btn-primary cc-btn-dense cc-fs-xs" @click="start(s.def)"
                      v-tooltip.left="`Start: ${s.def.title}`">
                {{ isDone(s.def) ? 'Again' : 'Start' }}
              </button>
            </div>
          </li>
        </ol>
      </div>

      <!-- said ONCE, not per row: three links each carrying their own explanation is what made the
           first version of this dialog wrap differently on every row -->
      <p class="gd-ask cc-muted cc-fs-2xs">Not written yet — tell us what you image, and send an example:</p>
      <p class="gd-wanted cc-row cc-row-loose">
        <a v-for="w in wanted" :key="w.id" class="gd-req cc-fs-xs" :href="recipeRequestUrl(w.title)"
           target="_blank" rel="noopener" v-tooltip.top="'Ask for this recipe on GitHub'">
          {{ w.title }} <i class="pi pi-external-link" />
        </a>
      </p>
    </section>

    <section v-for="grp in groups" :key="grp.group" class="gd-group">
      <h3 class="gd-group-head cc-eyebrow cc-fs-2xs">{{ grp.group }}</h3>

      <div class="gd-row" v-for="g in grp.guides" :key="g.id">
        <i :class="['pi', g.icon, 'gd-icon']" />

        <div class="gd-main">
          <div class="gd-head">
            <span class="gd-title">{{ g.title }}</span>
            <span v-if="isDone(g)" class="gd-done" v-tooltip.top="'You have finished this guide'">
              <i class="pi pi-check" />
            </span>
            <span class="gd-meta cc-readout cc-fs-2xs">
              <!-- colour is never the only cue (docs/UI.md → Severity): the icon and the word both change -->
              <span :class="misses(g).length ? 'gd-warn' : 'gd-ok'">
                <i :class="['pi', misses(g).length ? 'pi-exclamation-circle' : 'pi-check-circle']" />
                {{ readiness(g) }}
              </span>
              · {{ g.steps.length }} steps
            </span>
          </div>

          <p class="gd-summary cc-muted cc-fs-xs">{{ g.summary }}</p>

          <p v-if="misses(g).length" class="gd-miss cc-fs-2xs">
            <i class="pi pi-exclamation-circle" />
            needs {{ misses(g).map(p => p.label).join(', ') }}
            <button v-if="firstFixable(g)" class="gd-fix" @click="start(firstFixable(g)!)"
                    v-tooltip.top="`Start '${firstFixable(g)!.title}' — it gets you what this one needs`">
              → {{ firstFixable(g)!.title }} first
            </button>
          </p>
        </div>

        <div class="gd-act">
          <button class="cc-btn cc-btn-primary cc-btn-dense cc-fs-xs" @click="start(g)"
                  v-tooltip.left="guide.prereqsMet(g)
                    ? `Start: ${g.title}`
                    : 'Start anyway — the missing pieces are only a warning'">
            {{ isDone(g) ? 'Again' : 'Start' }}
          </button>
        </div>
      </div>
    </section>

    <template #footer>
      <span class="gd-spacer" />
      <button v-if="guide.completed.size" class="cc-btn cc-btn-ghost cc-fs-xs" @click="guide.clearCompleted()"
              v-tooltip.top="'Clear the finished ticks so the list reads fresh again'">
        Reset ticks
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
/* the same whitish token as the bubble and ring, so the compass mark and the surface it opens read as
   one thing (and it is brighter than a bare button's default dim grey, so it still stands out) */
.gd-mark { color: var(--cc-guide); }

/* size comes from `cc-fs-md` in the markup — a scoped font-size here would shadow `cc-muted`'s own
   and make the utility a no-op (docs/UI.md → pick a scenario, then a size) */
.gd-intro { margin: 0 0 0.7rem; line-height: 1.4; }

.gd-group { margin-bottom: 0.7rem; }
/* no `color` — `cc-eyebrow` owns it; shadowing a utility's own property makes it a no-op
   (enforced by cssScenarios.test.ts) */
.gd-group-head { margin: 0 0 0.15rem; }

.gd-row {
  display: flex;
  align-items: flex-start;
  gap: 0.55rem;
  padding: 0.4rem 0.2rem;
  border-top: 1px solid var(--cc-border);
}
.gd-row:hover { background: var(--cc-surface-2); }

.gd-icon { color: var(--cc-accent); font-size: 0.95rem; margin-top: 0.1rem; flex: none; width: 1rem; text-align: center; }
.gd-main { flex: 1; min-width: 0; }

.gd-head { display: flex; align-items: baseline; gap: 0.35rem; }
.gd-title { font-weight: 600; font-size: var(--cc-fs-md); }
.gd-done { color: var(--cc-sev-ok); }
/* pushed right so readiness + step count line up down the list, whatever the title's length */
.gd-meta { margin-left: auto; white-space: nowrap; flex: none; }
.gd-ok { color: var(--cc-sev-ok); }
/* amber, not red: a missing prerequisite is a warning about fit, not a failure — Start still works */
.gd-warn { color: var(--cc-sev-warn); }

.gd-summary { margin: 0.05rem 0 0; line-height: 1.35; }

.gd-miss { margin: 0.15rem 0 0; line-height: 1.35; color: var(--cc-sev-warn); }
/* a link, not a button: it sits mid-sentence, where a second filled button would compete with Start */
.gd-fix {
  background: none; border: none; padding: 0; margin-left: 0.25rem;
  font: inherit; color: var(--cc-accent-soft); cursor: pointer; text-decoration: underline;
}
.gd-fix:hover { color: var(--cc-accent); }

/* FIXED width, one button — this is what keeps the right edge straight (see the header note) */
.gd-act { flex: none; width: 4.2rem; display: flex; justify-content: flex-end; }

.gd-spacer { flex: 1; }

/* ── recipes ──────────────────────────────────────────────────────────────────────────────────── */
/* the row is `.cc-section-toggle`; what's left here is the padding + divider this site wants, which is
   exactly what that utility leaves to its sites */
.gd-recipe { padding: 0.4rem 0.2rem; border-top: 1px solid var(--cc-border); }
.gd-recipe:hover { background: var(--cc-surface-2); }
/* the title stays bright on a dim row — the utility's `color` covers the rest of it */
.gd-recipe .gd-title { color: var(--cc-text); }
.gd-recipe .gd-summary { margin: 0; }

.gd-steps { list-style: none; margin: 0; padding: 0 0 0.3rem 1.1rem; }
/* the step index sits where the recipe's chevron does, so the list reads as one column */
.gd-step-n { flex: none; width: 1rem; text-align: center; margin-top: 0.15rem; }
.gd-step .gd-title { font-weight: 500; }
.gd-opt { font-style: italic; }

.gd-ask { margin: 0.45rem 0 0.2rem; }
/* `.cc-row cc-row-loose` in the markup owns the flex + wrap + gap; only the margin is ours */
.gd-wanted { margin: 0; }
/* a link, like the "X first" fix link above — these leave the app, they don't act in it */
.gd-req { color: var(--cc-accent-soft); text-decoration: underline; }
.gd-req:hover { color: var(--cc-accent); }
.gd-req .pi { font-size: var(--cc-fs-2xs); }
</style>
