<script setup lang="ts">
import { computed } from 'vue'
import { useSettingsStore } from '../stores/settings'
import { useAppControlStore } from '../stores/appControl'
import { openWhatsNew } from '../lib/whatsNew'
import { useRestartToUpdate } from '../composables/useRestartToUpdate'
import ProjectCrumb from './ProjectCrumb.vue'
import JumpSearch from './JumpSearch.vue'
import HelpMenu from './HelpMenu.vue'
import ProfileMenu from './ProfileMenu.vue'
import { useObserverStore } from '../stores/observer'
import { showKiwiLauncher } from '../utils/aiSwitch'
import { unseenBadgeColour, labLogLauncherTip } from '../utils/labLog'

const settings = useSettingsStore()
const appCtl = useAppControlStore()
const observer = useObserverStore()

// Update-available badge → opens the What's New modal (release notes + inline Install button).
// The modal itself is mounted ONCE in App.vue; we just flip the shared open flag. Settings still
// hosts the Software updates panel as the durable home for the control. The × dismisses the
// badge for this session only ("remind me later"). See docs/todo/WHATS_NEW_PLAN.md.
function openUpdate() { openWhatsNew() }

const restartUpd = useRestartToUpdate()

// Lab-log launcher: the unseen dot and tip follow the shared rule in utils/labLog.ts.
const bellDotStyle = computed(() => {
  const c = unseenBadgeColour(settings.labLogUnseenLevel)
  return c ? { background: c } : {}
})
const bellTip = computed(() => labLogLauncherTip({ unseen: settings.labLogUnseen,
  kind: settings.labLogUnseenKind, open: settings.labLogPanelOpen, aiOn: observer.ai.enabled }))
</script>

<template>
  <header class="app-header">
    <button class="nav-toggle cc-btn cc-btn-bare cc-btn-icon cc-btn-lg" data-guide="header.navToggle"
      @click="settings.sidebarCollapsed = !settings.sidebarCollapsed"
      v-tooltip.bottom="settings.sidebarCollapsed ? 'Show menu' : 'Hide menu'"
      :aria-label="settings.sidebarCollapsed ? 'Show menu' : 'Hide menu'">
      <i class="pi pi-bars" />
    </button>
    <!-- The wordmark goes to Welcome; What's new + tips moved into Help ▾ (GUI_LAYOUT_PLAN). -->
    <RouterLink to="/" class="logo cc-btn cc-btn-bare cc-btn-dense" data-guide="header.brand"
                v-tooltip.bottom="'Welcome — recent projects'" aria-label="Welcome">
      <img class="logo-mark" src="/feijoa.svg" alt="" aria-hidden="true" width="18" height="18" />
      Cecelia
    </RouterLink>

    <ProjectCrumb />

    <!-- The viewer chip: opens / closes the Viewer panel, which drives the viewer window. Green is
         the viewer's identity colour; the dot says the panel is open. -->
    <button type="button" class="viewer-chip" data-guide="launcher.viewer"
            :class="{ on: settings.viewerPanelOpen }"
            v-tooltip.bottom="settings.viewerPanelOpen ? 'Close viewer controls' : 'Viewer controls — populations, tracks, colour-by'"
            @click="settings.viewerPanelOpen = !settings.viewerPanelOpen">
      <span class="cc-dot vc-on" /> Viewer
    </button>

    <span class="spacer" />

    <!-- the tip sits on the badge TEXT, not the badge: the badge also holds the dismiss button, whose
         own tip it fired on top of (docs/UI.md → nested tooltips) -->
    <span v-if="appCtl.canManageMachine && appCtl.updateAvailable && !appCtl.updateDismissed" class="update-badge"
          @click="openUpdate">
      <span class="update-txt"
            v-tooltip.bottom="appCtl.updateScope === 'system'
              ? 'Update available — a shared installation must be updated by an administrator'
              : `Update ${appCtl.updateLatest} — see what's new`">
        <i class="pi pi-arrow-circle-up" />
        Update{{ appCtl.updateLatest ? ' ' + appCtl.updateLatest : '' }}
      </span>
      <button class="update-x cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click.stop="appCtl.dismissUpdate()"
              v-tooltip.bottom="'Remind me later'" aria-label="Dismiss update notice">
        <i class="pi pi-times" />
      </button>
    </span>
    <!-- Update downloaded but not installed yet: it lands on the next restart, so say so here
         rather than leaving the user to guess. -->
    <span v-else-if="restartUpd.show.value" class="update-badge"
          :class="{ 'update-blocked': restartUpd.blocked.value }"
          v-tooltip.bottom="restartUpd.tip.value" @click="restartUpd.restart">
      <i :class="['pi', appCtl.busy ? 'pi-spin pi-spinner' : 'pi-refresh']" />
      {{ appCtl.busy ? 'Updating…' : 'Restart to update' }}
    </span>

    <JumpSearch />
    <button type="button" class="bell cc-btn cc-btn-bare cc-btn-icon" data-guide="launcher.labLog"
            :class="{ 'cc-btn-on': settings.labLogPanelOpen }" aria-label="Lab log"
            v-tooltip.bottom="bellTip" @click="settings.labLogPanelOpen = !settings.labLogPanelOpen">
      <i class="pi pi-book" />
      <span v-if="settings.labLogUnseen" class="cc-dot bell-unseen" :style="bellDotStyle" />
    </button>
    <HelpMenu />
    <!-- Visible with AI off: it is how AI gets turned on (GUI_CLARITY_PLAN Decision 9) -->
    <button v-if="showKiwiLauncher(observer.ai, settings.kiwiHidden)" type="button"
            class="kiwi-btn cc-btn cc-btn-dense" data-guide="launcher.kiwi"
            :class="{ on: settings.kiwiOpen }"
            v-tooltip.bottom="settings.kiwiOpen ? 'Close Kiwi' : 'Ask about your project, get answers you can check'"
            @click="settings.kiwiOpen = !settings.kiwiOpen">
      Ask Kiwi
    </button>
    <ProfileMenu />
  </header>
</template>

<style scoped>
.app-header {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  height: var(--cc-header-h);
  padding: 0 1rem;
  background: var(--cc-surface-1);
  border-bottom: 1px solid var(--cc-border);
  flex-shrink: 0;
  z-index: 100;
}

.nav-toggle { margin-left: -0.3rem; }   /* + cc-btn cc-btn-bare cc-btn-icon cc-btn-lg */
.nav-toggle:hover { background: var(--cc-surface-2); color: var(--cc-text); }

/* + cc-btn cc-btn-bare cc-btn-dense — the wordmark goes to Welcome. */
.logo {
  font-weight: 700;
  font-size: 0.95rem;
  letter-spacing: 0.06em;
  color: var(--cc-accent);
  white-space: nowrap;
  display: inline-flex;
  align-items: center;
  gap: 0.4rem;
}
.logo-mark { display: block; flex: none; }
.logo:hover { color: var(--cc-accent); background: color-mix(in srgb, var(--cc-accent) 12%, transparent); }


.spacer { flex: 1; }

/* Viewer chip — the viewer's identity green, a dot that lights while the Viewer panel is open. The
   pressed state is the identity colour, not `.cc-btn-on` (which paints the accent): same rule as the
   launcher row in PanelLaunchers.vue, GUI_CLARITY_PLAN Decision 15. Ask Kiwi below likewise. */
.viewer-chip {
  display: inline-flex; align-items: center; gap: 0.4rem; height: 24px; padding: 0 0.6rem;
  border: 1px solid color-mix(in srgb, var(--cc-viewer) 45%, transparent); border-radius: var(--cc-radius-pill);
  background: none; color: var(--cc-viewer); font-size: var(--cc-fs-sm); cursor: pointer; white-space: nowrap;
}
.viewer-chip:hover { background: color-mix(in srgb, var(--cc-viewer) 10%, transparent); }
.viewer-chip.on { background: color-mix(in srgb, var(--cc-viewer) 16%, transparent); border-color: var(--cc-viewer); }
.viewer-chip.on .vc-on { background: var(--cc-viewer); }   /* identity colour, not a status tone */

/* Lab-log bell — the unseen dot sits on the bell's shoulder. */
.bell { position: relative; }
.bell-unseen { position: absolute; top: 3px; right: 3px; background: var(--cc-accent); }

/* Ask Kiwi — Kiwi's identity teal, worded so it reads as an action, not one more icon. */
.kiwi-btn { color: var(--cc-kiwi); border: 1px solid color-mix(in srgb, var(--cc-kiwi) 45%, transparent); background: none; }
.kiwi-btn:hover { background: var(--cc-kiwi-tint); }
.kiwi-btn.on { background: var(--cc-kiwi-tint); border-color: var(--cc-kiwi); }

.update-badge {
  display: flex;
  align-items: center;
  gap: 0.35rem;
  font-size: var(--cc-fs-sm);
  font-weight: 600;
  padding: 0.2rem 0.4rem 0.2rem 0.6rem;
  border-radius: var(--cc-radius-pill);
  cursor: pointer;
  white-space: nowrap;
  background: color-mix(in srgb, var(--cc-accent) 22%, transparent);
  color: var(--cc-accent);
}
/* purely a tip anchor — the pill above still owns the badge's look, so the × stays inside it */
.update-txt { display: inline-flex; align-items: center; gap: 0.35rem; }
.update-badge:hover { background: color-mix(in srgb, var(--cc-accent) 34%, transparent); }
.update-badge .pi-arrow-circle-up { font-size: var(--cc-fs-md); }
.update-blocked { opacity: 0.6; cursor: default; }
.update-x { color: inherit; opacity: 0.7; }   /* + cc-btn cc-btn-bare cc-btn-icon */
.update-x:hover { opacity: 1; }
</style>
