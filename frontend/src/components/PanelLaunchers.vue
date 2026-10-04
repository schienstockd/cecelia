<script setup lang="ts">
// The floating-panel launchers — Viewer / Correction / Lab log / Kiwi — as one row of icon toggles.
// Two homes: the sidebar (always), and the module action bar while the plot canvas is maximised
// (ModuleLayout) — that layout covers the sidebar, so without its own copy the panels could not be
// opened or closed there. `compact` = the action-bar size. Both rows carry the same `data-guide`
// anchors on purpose: `resolveAnchor` (utils/guideAnchor.ts) picks the un-occluded one, so a guide
// lands on whichever row is actually visible.
//
// Each keeps its panel's identity colour: green = viewer, purple = correction, white = lab log,
// teal = kiwi. The lab-log unseen badge overlays the icon so Claude/Cecelia notes still get noticed.
import { computed } from 'vue'
import { useSettingsStore } from '../stores/settings'

withDefaults(defineProps<{ compact?: boolean }>(), { compact: false })

const settings = useSettingsStore()
// lab-log badge: Cecelia digests colour by severity (⚠️/❌); Claude notes keep the accent tint.
const labLogBadgeStyle = computed(() =>
  settings.labLogUnseenLevel === 'fail' ? { color: 'var(--cc-sev-fail)' }
  : settings.labLogUnseenLevel === 'warn' ? { color: 'var(--cc-sev-warn)' }
  : {})
</script>

<template>
  <div class="panel-launcher-row" :class="{ compact }">
    <button class="panel-launcher panel-launcher-viewer cc-btn cc-btn-bare"
            data-guide="sidebar.viewerCta"
            :class="{ on: settings.viewerPanelOpen }"
            @click="settings.viewerPanelOpen = !settings.viewerPanelOpen"
            v-tooltip.right="settings.viewerPanelOpen
              ? 'Close viewer controls'
              : 'Viewer controls — populations, tracks, colour-by'">
      <i class="pi pi-eye" />
    </button>
    <button class="panel-launcher panel-launcher-correction cc-btn cc-btn-bare"
            data-guide="sidebar.correctionCta"
            :class="{ on: settings.correctionCockpitOpen }"
            @click="settings.correctionCockpitOpen = !settings.correctionCockpitOpen"
            v-tooltip.right="settings.correctionCockpitOpen
              ? 'Close correction cockpit (in progress)'
              : 'Correction cockpit — tracks + labels (in progress)'">
      <i class="pi pi-wrench" />
      <!-- WIP badge: the correction cockpit is shipping in phases (docs/todo/CORRECTION_PLAN.md
           + docs/todo/COCKPIT_INTERACTIVITY_PLAN.md). The hammer glyph over the wrench tells a
           pre-release user "we're still building on this" without hiding it — the working half
           (Merge / Remove / label.split, Review pager) is real and useful. Remove when the plan
           lands its remaining phases. -->
      <i class="pi pi-hammer panel-launcher-badge panel-launcher-badge-wip" />
    </button>
    <button class="panel-launcher panel-launcher-lablog cc-btn cc-btn-bare"
            data-guide="sidebar.labLogCta"
            :class="{ on: settings.labLogPanelOpen, 'has-unseen': !!settings.labLogUnseen }"
            @click="settings.labLogPanelOpen = !settings.labLogPanelOpen"
            v-tooltip.right="settings.labLogUnseen
              ? ((settings.labLogUnseenKind === 'cecelia' ? 'Cecelia: ' : 'Claude noted: ') + settings.labLogUnseen)
              : (settings.labLogPanelOpen ? 'Close lab log' : 'Lab log — analysis notes for this project (you + Claude)')">
      <i class="pi pi-book" />
      <i v-if="settings.labLogUnseen"
         :class="['pi', settings.labLogUnseenKind === 'cecelia' ? 'pi-bell' : 'pi-sparkles', 'panel-launcher-badge']"
         :style="labLogBadgeStyle" />
    </button>
    <button class="panel-launcher panel-launcher-kiwi cc-btn cc-btn-bare"
            data-guide="sidebar.kiwiCta"
            :class="{ on: settings.kiwiOpen }"
            @click="settings.kiwiOpen = !settings.kiwiOpen"
            v-tooltip.right="settings.kiwiOpen
              ? 'Close Kiwi (assist cockpit)'
              : 'Kiwi — pairing, chat handoff, and other assistant controls'">
      <i class="pi pi-comments" />
    </button>
  </div>
</template>

<style scoped>
.panel-launcher-row {
  display: flex;
  gap: 0.35rem;
  padding: 0.5rem 0.5rem 0.35rem;
  flex-shrink: 0;                 /* pinned below the scroll region — never squeezed */
}
.panel-launcher {
  flex: 1;
  height: 32px;
  display: flex;
  align-items: center;
  justify-content: center;
  position: relative;             /* the unseen badge overlays the icon */
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-md);
  cursor: pointer;
  color: var(--cc-text);
  transition: background 0.1s, border-color 0.1s, color 0.1s;
}
.panel-launcher > i { font-size: 1rem; }
/* Viewer — green (--cc-viewer). */
.panel-launcher-viewer { color: var(--cc-viewer); }
.panel-launcher-viewer:hover { border-color: #16a34a; background: #14261a; }
.panel-launcher-viewer.on { background: #0f3d24; border-color: var(--cc-viewer); }
/* Correction — purple (--cc-accent), matching the CorrectionCockpit's floating-panel border. */
.panel-launcher-correction { color: var(--cc-accent); }
.panel-launcher-correction:hover { border-color: var(--cc-accent-strong); background: var(--cc-accent-tint); }
.panel-launcher-correction.on { background: var(--cc-accent-tint); border-color: var(--cc-accent); }
/* Lab log — neutral white, matches its floating panel's --cc-guide border. */
.panel-launcher-lablog { color: var(--cc-text); }
.panel-launcher-lablog:hover { border-color: rgba(255, 255, 255, 0.55); background: rgba(255, 255, 255, 0.06); }
.panel-launcher-lablog.on { background: rgba(255, 255, 255, 0.1); border-color: rgba(255, 255, 255, 0.6); }
/* Kiwi (assist cockpit) — teal (--cc-kiwi), matching the KiwiCockpit's floating-panel border. */
.panel-launcher-kiwi { color: var(--cc-kiwi); }
.panel-launcher-kiwi:hover { border-color: var(--cc-kiwi-strong); background: var(--cc-kiwi-tint); }
.panel-launcher-kiwi.on { background: var(--cc-kiwi-tint); border-color: var(--cc-kiwi); }
/* Unseen-note border tint: Claude/Cecelia wrote a lab-log note while the panel was closed. */
.panel-launcher-lablog.has-unseen { border-color: var(--cc-accent); }
.panel-launcher-badge {
  position: absolute;
  top: -3px;
  left: -3px;
  font-size: var(--cc-fs-xs);
  background: var(--cc-surface-1);
  border-radius: 50%;
  padding: 1px;
}
/* WIP variant: amber tint over the cockpit's wrench — "still being built". Not a warning
   (which is red / --cc-danger), not a QC flag (which is a `pi-flag` on an image row). */
.panel-launcher-badge-wip { color: #d97706; }
/* Action-bar size: sits among the module's filter toggles, so it takes their height and drops the
   sidebar row's padding + stretch. */
.panel-launcher-row.compact { padding: 0; gap: 0.25rem; }
.compact .panel-launcher { flex: none; width: 28px; height: 26px; }
.compact .panel-launcher > i { font-size: var(--cc-fs-md); }
</style>
