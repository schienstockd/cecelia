<script setup lang="ts">
// The header's launcher group — Viewer chip, Lab log, Help, Ask Kiwi. AppHeader mounts it, and so does
// ModuleLayout's set row while the plot canvas is maximised (which covers the header), so both places
// show the same buttons. The root is `display: contents`: the buttons take their host row's gap.
// Every copy carries the same role-named `data-guide` anchors (`launcher.*`); `resolveAnchor`
// (utils/guideAnchor.ts) picks the un-occluded one, so a guide lands on whichever is visible.
import { computed } from 'vue'
import { useSettingsStore } from '../stores/settings'
import { useObserverStore } from '../stores/observer'
import { showKiwiLauncher } from '../utils/aiSwitch'
import { unseenBadgeColour, labLogLauncherTip } from '../utils/labLog'
import { useViewerLauncher } from '../composables/useViewerLauncher'
import HelpMenu from './HelpMenu.vue'

const settings = useSettingsStore()
const observer = useObserverStore()
const viewer = useViewerLauncher()

// Lab-log launcher: the unseen dot and tip follow the shared rule in utils/labLog.ts.
const bellDotStyle = computed(() => {
  const c = unseenBadgeColour(settings.labLogUnseenLevel)
  return c ? { background: c } : {}
})
const bellTip = computed(() => labLogLauncherTip({ unseen: settings.labLogUnseen,
  kind: settings.labLogUnseenKind, open: settings.labLogPanelOpen, aiOn: observer.ai.enabled }))
</script>

<template>
  <div class="header-launchers">
    <!-- The viewer chip: brings the viewer window forward, or opens it on the last image
         (`useViewerLauncher`); the viewer's ← back is the way home. The dot says the viewer is open. -->
    <button type="button" class="viewer-chip" data-guide="launcher.viewer"
            :class="{ on: viewer.open.value }" :disabled="viewer.disabled.value"
            v-tooltip.bottom="viewer.tip.value" @click="viewer.raise()">
      <span class="cc-dot vc-on" /> Viewer
    </button>
    <button type="button" class="bell cc-btn cc-btn-bare cc-btn-icon" data-guide="launcher.labLog"
            :class="{ 'cc-btn-on': settings.labLogPanelOpen }" aria-label="Lab log"
            v-tooltip.bottom="bellTip" @click="settings.labLogPanelOpen = !settings.labLogPanelOpen">
      <i class="pi pi-book" />
      <span v-if="settings.labLogUnseen" class="cc-dot bell-unseen" :style="bellDotStyle" />
    </button>
    <HelpMenu />
    <!-- Visible with AI off: it is how AI gets turned on (GUI_CLARITY_PLAN Decision 9) -->
    <button v-if="showKiwiLauncher(observer.ai, settings.kiwiHidden)" type="button"
            class="cc-btn cc-btn-dense cc-btn-kiwi" data-guide="launcher.kiwi"
            :class="{ on: settings.kiwiOpen }"
            v-tooltip.bottom="settings.kiwiOpen ? 'Close Kiwi' : 'Ask about your project, get answers you can check'"
            @click="settings.kiwiOpen = !settings.kiwiOpen">
      Ask Kiwi
    </button>
  </div>
</template>

<style scoped>
.header-launchers { display: contents; }

/* Viewer chip — neutral like the Lab log launcher (the word carries it); the dot lights while the
   viewer is open. No identity hue: docs/UI.md → *Colour schemes*. */
.viewer-chip {
  display: inline-flex; align-items: center; gap: 0.4rem; height: 24px; padding: 0 0.6rem;
  border: 1px solid var(--cc-border); border-radius: var(--cc-radius-pill);
  background: none; color: var(--cc-text); font-size: var(--cc-fs-sm); cursor: pointer; white-space: nowrap;
}
.viewer-chip:hover { background: var(--cc-wash); border-color: var(--cc-guide); }
.viewer-chip.on { background: var(--cc-wash-strong); border-color: var(--cc-guide); }
.viewer-chip.on .vc-on { background: var(--cc-text); }
.viewer-chip:disabled { opacity: 0.5; cursor: default; }

/* Lab-log bell — the unseen dot sits on the bell's shoulder. */
.bell { position: relative; }
.bell-unseen { position: absolute; top: 3px; right: 3px; background: var(--cc-accent); }
</style>
