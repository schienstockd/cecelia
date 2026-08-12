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
import { computed } from 'vue'
import BaseModal from './BaseModal.vue'
import { useGuideStore } from '../stores/guide'
import { guidesByGroup, guideById } from '../lib/guides'
import { closeGuides } from '../lib/guideOpen'
import type { GuideDef } from '../lib/guides/types'

const guide = useGuideStore()
const groups = computed(() => guidesByGroup())

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
.gd-mark { color: var(--cc-viewer); }

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
</style>
