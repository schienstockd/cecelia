<script setup lang="ts">
import { useLayoutFoldStore } from '../stores/layoutFold'
import { useAppControlStore } from '../stores/appControl'
import { openWhatsNew } from '../lib/whatsNew'
import { useRestartToUpdate } from '../composables/useRestartToUpdate'
import ProjectCrumb from './ProjectCrumb.vue'
import JumpSearch from './JumpSearch.vue'
import HeaderLaunchers from './HeaderLaunchers.vue'
import ProfileMenu from './ProfileMenu.vue'

const fold = useLayoutFoldStore()   // the SHOWN fold: narrow windows fold by default
const appCtl = useAppControlStore()

// Update-available badge → opens the What's New modal (release notes + inline Install button).
// The modal itself is mounted ONCE in App.vue; we just flip the shared open flag. Settings still
// hosts the Software updates panel as the durable home for the control. The × dismisses the
// badge for this session only ("remind me later"). See docs/todo/WHATS_NEW_PLAN.md.
function openUpdate() { openWhatsNew() }

const restartUpd = useRestartToUpdate()
</script>

<template>
  <header class="app-header">
    <button class="nav-toggle cc-btn cc-btn-bare cc-btn-icon cc-btn-lg" data-guide="header.navToggle"
      @click="fold.sidebarFolded = !fold.sidebarFolded"
      v-tooltip.bottom="fold.sidebarFolded ? 'Show menu' : 'Hide menu'"
      :aria-label="fold.sidebarFolded ? 'Show menu' : 'Hide menu'">
      <i class="pi pi-bars" />
    </button>
    <!-- The wordmark goes to Welcome; What's new + tips moved into Help ▾ (GUI_LAYOUT_PLAN). -->
    <RouterLink to="/" class="logo cc-btn cc-btn-bare cc-btn-dense" data-guide="header.brand"
                v-tooltip.bottom="'Welcome — recent projects'" aria-label="Welcome">
      <img class="logo-mark" src="/feijoa.svg" alt="" aria-hidden="true" width="18" height="18" />
      Cecelia
    </RouterLink>

    <ProjectCrumb />

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
    <HeaderLaunchers />
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
  font-weight: 600;
  font-size: 0.95rem;
  color: var(--cc-text);
  white-space: nowrap;
  display: inline-flex;
  align-items: center;
  gap: 0.4rem;
}
.logo-mark { display: block; flex: none; }
.logo:hover { color: var(--cc-text); background: var(--cc-surface-2); }


.spacer { flex: 1; }

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
