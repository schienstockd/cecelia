<script setup lang="ts">
// The floating-panel launchers — Viewer / Correction / Lab log / Kiwi — as one row of icon toggles.
// Viewer, Lab log and Kiwi live in the header (AppHeader); this row is the copy for the module action
// bar while the plot canvas is maximised (ModuleLayout), which covers the header and sidebar, so the
// panels stay openable there. The correction cockpit's everyday home is "Correct labels" / "Correct
// tracks" on the Segment and Track pages (docs/todo/GUI_LAYOUT_PLAN.md, M3). `compact` = the action-bar
// size. `kinds` picks a subset for a host that needs one. Every copy
// carries the same role-named `data-guide` anchors (`launcher.*`): `resolveAnchor`
// (utils/guideAnchor.ts) picks the un-occluded one, so a guide lands on whichever is visible.
//
// Each keeps its panel's identity colour: green = viewer, purple = correction, white = lab log,
// teal = kiwi. The lab-log unseen badge overlays the icon so Claude/Cecelia notes still get noticed.
import { computed } from 'vue'
import { useSettingsStore } from '../stores/settings'
import { useObserverStore } from '../stores/observer'
import { showKiwiLauncher } from '../utils/aiSwitch'
import { unseenBadgeColour, labLogLauncherTip } from '../utils/labLog'

type Launcher = 'viewer' | 'correction' | 'labLog' | 'kiwi'
const props = withDefaults(defineProps<{ compact?: boolean; kinds?: Launcher[] }>(),
  { compact: false, kinds: () => ['viewer', 'correction', 'labLog', 'kiwi'] })
const has = (k: Launcher) => props.kinds.includes(k)

const settings = useSettingsStore()
const observer = useObserverStore()
// lab-log badge + tip: the shared rule in utils/labLog.ts (the header bell uses it too)
const labLogBadgeStyle = computed(() => {
  const c = unseenBadgeColour(settings.labLogUnseenLevel)
  return c ? { color: c } : {}
})
const labLogTip = computed(() => labLogLauncherTip({ unseen: settings.labLogUnseen,
  kind: settings.labLogUnseenKind, open: settings.labLogPanelOpen, aiOn: observer.ai.enabled }))
</script>

<template>
  <div class="panel-launcher-row" :class="{ compact }">
    <button v-if="has('viewer')" class="panel-launcher panel-launcher-viewer cc-btn cc-btn-bare"
            data-guide="launcher.viewer"
            :class="{ on: settings.viewerPanelOpen }"
            @click="settings.viewerPanelOpen = !settings.viewerPanelOpen"
            v-tooltip.right="settings.viewerPanelOpen
              ? 'Close viewer controls'
              : 'Viewer controls — populations, tracks, colour-by'">
      <i class="pi pi-eye" />
    </button>
    <button v-if="has('correction')" class="panel-launcher panel-launcher-correction cc-btn cc-btn-bare"
            data-guide="launcher.correction"
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
    <button v-if="has('labLog')" class="panel-launcher panel-launcher-lablog cc-btn cc-btn-bare"
            data-guide="launcher.labLog"
            :class="{ on: settings.labLogPanelOpen, 'has-unseen': !!settings.labLogUnseen }"
            @click="settings.labLogPanelOpen = !settings.labLogPanelOpen"
            v-tooltip.right="labLogTip">
      <i class="pi pi-book" />
      <i v-if="settings.labLogUnseen"
         :class="['pi', settings.labLogUnseenKind === 'cecelia' ? 'pi-bell' : 'pi-sparkles', 'panel-launcher-badge']"
         :style="labLogBadgeStyle" />
    </button>
    <button v-if="has('kiwi') && showKiwiLauncher(observer.ai, settings.kiwiHidden)"
            class="panel-launcher panel-launcher-kiwi cc-btn cc-btn-bare"
            data-guide="launcher.kiwi"
            :class="{ on: settings.kiwiOpen }"
            @click="settings.kiwiOpen = !settings.kiwiOpen"
            v-tooltip.right="settings.kiwiOpen
              ? 'Close Kiwi'
              : 'Kiwi — ask about your project, get answers you can check'">
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
/* Viewer — neutral, like Lab log: the eye icon carries it. */
.panel-launcher-viewer:hover { border-color: var(--cc-guide); background: var(--cc-wash); }
.panel-launcher-viewer.on { background: var(--cc-wash-strong); border-color: var(--cc-guide); }
/* Correction — purple (--cc-accent), matching the CorrectionCockpit's floating-panel border. */
.panel-launcher-correction { color: var(--cc-accent); }
.panel-launcher-correction:hover { border-color: var(--cc-accent-strong); background: var(--cc-accent-tint); }
.panel-launcher-correction.on { background: var(--cc-accent-tint); border-color: var(--cc-accent); }
/* Lab log — neutral white, matches its floating panel's --cc-guide border. */
.panel-launcher-lablog { color: var(--cc-text); }
.panel-launcher-lablog:hover { border-color: var(--cc-guide); background: var(--cc-wash); }
.panel-launcher-lablog.on { background: var(--cc-wash-strong); border-color: var(--cc-guide); }
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
.panel-launcher-badge-wip { color: var(--cc-warn); }
/* Action-bar size: sits among the module's filter toggles, so it takes their height and drops the
   sidebar row's padding + stretch. */
.panel-launcher-row.compact { padding: 0; gap: 0.25rem; }
.compact .panel-launcher { flex: none; width: 28px; height: 26px; }
.compact .panel-launcher > i { font-size: var(--cc-fs-md); }
</style>
