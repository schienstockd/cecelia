<script setup lang="ts">
import { useWsStore } from '../stores/ws'
import { useSettingsStore } from '../stores/settings'
import { useAppControlStore } from '../stores/appControl'
import { openWhatsNew } from '../lib/whatsNew'

const ws = useWsStore()
const settings = useSettingsStore()
const appCtl = useAppControlStore()

// Update-available badge → opens the What's New modal (release notes + inline Install button).
// The modal itself is mounted ONCE in App.vue; we just flip the shared open flag. Settings still
// hosts the Software updates panel as the durable home for the control. The × dismisses the
// badge for this session only ("remind me later"). See docs/todo/WHATS_NEW_PLAN.md.
function openUpdate() { openWhatsNew() }

const statusLabel: Record<string, string> = {
  connected:    'Connected',
  connecting:   'Connecting…',
  disconnected: 'Disconnected',
  error:        'Connection error',
}
const statusTip: Record<string, string> = {
  connected:    'Julia backend is running and reachable.',
  connecting:   'Attempting to connect to Julia backend on port 8080.',
  disconnected: 'Not connected. Check that the Julia server is running (pixi run dev).',
  error:        'WebSocket error. See the console below for details.',
}
</script>

<template>
  <header class="app-header">
    <button class="nav-toggle cc-btn cc-btn-bare cc-btn-icon cc-btn-lg" @click="settings.sidebarCollapsed = !settings.sidebarCollapsed"
      v-tooltip.bottom="settings.sidebarCollapsed ? 'Show menu' : 'Hide menu'"
      :aria-label="settings.sidebarCollapsed ? 'Show menu' : 'Hide menu'">
      <i class="pi pi-bars" />
    </button>
    <span class="logo">🍍 Cecelia</span>

    <span class="spacer" />

    <span v-if="appCtl.updateAvailable && !appCtl.updateDismissed" class="update-badge"
          @click="openUpdate"
          v-tooltip.bottom="appCtl.updateScope === 'system'
            ? 'Update available — a shared installation must be updated by an administrator'
            : `Update available — ${appCtl.updateLatest}. See what's new.`">
      <i class="pi pi-arrow-circle-up" />
      Update{{ appCtl.updateLatest ? ' ' + appCtl.updateLatest : '' }}
      <button class="update-x cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click.stop="appCtl.dismissUpdate()"
              v-tooltip.bottom="'Remind me later'" aria-label="Dismiss update notice">
        <i class="pi pi-times" />
      </button>
    </span>

    <span
      class="ws-badge"
      :class="ws.status"
      v-tooltip.bottom="statusTip[ws.status] ?? ws.status"
    >
      <span class="dot" />
      {{ statusLabel[ws.status] ?? ws.status }}
    </span>
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

.logo {
  font-weight: 700;
  font-size: 0.95rem;
  letter-spacing: 0.06em;
  color: var(--cc-accent);
  white-space: nowrap;
}

.spacer { flex: 1; }

.ws-badge {
  display: flex;
  align-items: center;
  gap: 0.4rem;
  font-size: var(--cc-fs-sm);
  font-weight: 500;
  padding: 0.2rem 0.65rem;
  border-radius: var(--cc-radius-pill);
  cursor: default;
  white-space: nowrap;
}
.dot { width: 7px; height: 7px; border-radius: var(--cc-radius-pill); }

.ws-badge.connected    { background: #14532d33; color: #86efac; }
.ws-badge.connected .dot { background: #4ade80; box-shadow: 0 0 5px #4ade80; }
.ws-badge.connecting   { background: #78350f33; color: #fcd34d; }
.ws-badge.connecting .dot { background: #fbbf24; }
.ws-badge.disconnected { background: #27272a55; color: #71717a; }
.ws-badge.disconnected .dot { background: #52525b; }
.ws-badge.error        { background: #7f1d1d33; color: #fca5a5; }
.ws-badge.error .dot   { background: var(--cc-sev-fail); box-shadow: 0 0 5px var(--cc-sev-fail); }

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
.update-badge:hover { background: color-mix(in srgb, var(--cc-accent) 34%, transparent); }
.update-badge .pi-arrow-circle-up { font-size: var(--cc-fs-md); }
.update-x { color: inherit; opacity: 0.7; }   /* + cc-btn cc-btn-bare cc-btn-icon */
.update-x:hover { opacity: 1; }
</style>
