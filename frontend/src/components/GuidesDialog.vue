<!--
  The guide picker: every guide, grouped like the sidebar, each with its prerequisites checked live.
  Opened from the compass button in AppHeader (and from a What's New tip card's "Show me").
  Design: docs/todo/GUIDE_SYSTEM_PLAN.md.

  Built on `BaseModal` like every other dialog (docs/UI.md → Modals & dialogs). Patterned on
  `ClaudeOverviewDialog.vue`: static content from a lib module, no store of its own beyond the shared
  open flag in `lib/guideOpen.ts`.

  Prerequisites are shown, never enforced (plan D6). Start is always enabled — a miss is a warning
  plus a pointer at the guide that fixes it, because the user may know something we can't see (they
  are about to open a project; the data is on a drive we haven't listed yet).
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

// A missing prerequisite that names a fix gets a "start that one instead" affordance. First miss only:
// listing three redirects for a guide the user can't run yet is noise.
function firstFixable(g: GuideDef) {
  const miss = guide.prereqState(g).find(p => !p.met && p.fixGuide)
  if (!miss?.fixGuide) return null
  const target = guideById(miss.fixGuide)
  return target && target.id !== g.id ? target : null
}
</script>

<template>
  <BaseModal title="Guides" icon="pi-compass" width="620px" @close="closeGuides()">
    <p class="gd-intro cc-muted">
      Each guide walks you through the real controls on your own data — bubbles appear beside the
      button to press, and step forward as you go.
    </p>

    <section v-for="grp in groups" :key="grp.group" class="gd-group">
      <h3 class="gd-group-head cc-eyebrow cc-fs-2xs">{{ grp.group }}</h3>

      <div v-for="g in grp.guides" :key="g.id" class="gd-card cc-card cc-card-2">
        <div class="gd-row">
          <i :class="['pi', g.icon, 'gd-icon']" />
          <div class="gd-main">
            <div class="gd-title-row">
              <span class="gd-title">{{ g.title }}</span>
              <span v-if="isDone(g)" class="gd-done" v-tooltip.top="'You have finished this guide'">
                <i class="pi pi-check" />
              </span>
              <span class="gd-steps cc-readout cc-fs-2xs">{{ g.steps.length }} steps</span>
            </div>
            <p class="gd-summary cc-muted cc-fs-xs">{{ g.summary }}</p>

            <!-- prerequisites: ✓ / ✗ per row, with the icon never the only cue (docs/UI.md → severity).
                 `cc-row cc-row-tight` is the shared wrapping-row scenario — the list keeps only its own
                 list-reset chrome. -->
            <ul class="gd-prereqs cc-row cc-row-tight cc-fs-2xs">
              <li v-for="p in guide.prereqState(g)" :key="p.id" :class="{ miss: !p.met }">
                <i :class="['pi', p.met ? 'pi-check-circle' : 'pi-exclamation-circle']" />
                <span>needs {{ p.label }}</span>
              </li>
            </ul>
          </div>

          <div class="gd-actions">
            <button class="cc-btn cc-btn-primary cc-btn-dense cc-fs-xs" @click="start(g)"
                    v-tooltip.left="guide.prereqsMet(g)
                      ? `Start: ${g.title}`
                      : 'Start anyway — the missing pieces are only a warning'">
              {{ isDone(g) ? 'Again' : 'Start' }}
            </button>
            <button v-if="firstFixable(g)" class="cc-btn cc-btn-ghost cc-btn-dense cc-fs-2xs"
                    @click="start(firstFixable(g)!)"
                    v-tooltip.left="`Start '${firstFixable(g)!.title}' — it gets you what this one needs`">
              {{ firstFixable(g)!.title }} first
            </button>
          </div>
        </div>
      </div>
    </section>

    <template #footer>
      <span class="cc-muted cc-fs-xs gd-foot-note">
        napari opens in its own window, so guides describe what appears there rather than pointing at it.
      </span>
      <span class="gd-spacer" />
      <button v-if="guide.completed.size" class="cc-btn cc-btn-ghost cc-fs-xs" @click="guide.clearCompleted()"
              v-tooltip.top="'Clear the finished ticks so the list reads fresh again'">
        Reset ticks
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
.gd-intro { margin: 0 0 0.7rem; line-height: 1.4; font-size: var(--cc-fs-md); }

.gd-group { margin-bottom: 0.8rem; }
/* no `color` — `cc-eyebrow` owns it (see GuideBubble's .gb-guide for the same rule) */
.gd-group-head { margin: 0 0 0.3rem; }

.gd-card { padding: 0.5rem 0.6rem; margin-bottom: 0.35rem; }
.gd-row { display: flex; align-items: flex-start; gap: 0.6rem; }
.gd-icon { color: var(--cc-accent); font-size: 1rem; margin-top: 0.15rem; flex: none; }
.gd-main { flex: 1; min-width: 0; }

.gd-title-row { display: flex; align-items: center; gap: 0.4rem; }
.gd-title { font-weight: 600; font-size: var(--cc-fs-md); }
.gd-done { color: var(--cc-sev-ok); display: inline-flex; }
.gd-summary { margin: 0.1rem 0 0.25rem; line-height: 1.35; }

/* + cc-row cc-row-tight — the row scenario supplies flex/wrap/gap; this is the list reset only */
.gd-prereqs { list-style: none; margin: 0; padding: 0; }
.gd-prereqs li { display: inline-flex; align-items: center; gap: 0.25rem; color: var(--cc-sev-ok); }
/* amber, not red: a missing prerequisite is a warning about fit, not a failure (Start still works) */
.gd-prereqs li.miss { color: var(--cc-sev-warn); }

.gd-actions { display: flex; flex-direction: column; align-items: stretch; gap: 0.2rem; flex: none; }

.gd-foot-note { flex: 0 1 auto; line-height: 1.3; }
.gd-spacer { flex: 1; }
</style>
