<script setup lang="ts">
import { useWsStore } from '../stores/ws'
import { useSettingsStore } from '../stores/settings'

const ws = useWsStore()
const settings = useSettingsStore()

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
    <button class="nav-toggle" @click="settings.sidebarCollapsed = !settings.sidebarCollapsed"
      v-tooltip.bottom="settings.sidebarCollapsed ? 'Show menu' : 'Hide menu'"
      :aria-label="settings.sidebarCollapsed ? 'Show menu' : 'Hide menu'">
      <i class="pi pi-bars" />
    </button>
    <span class="logo">🍍 Cecelia</span>

    <span class="spacer" />

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

.nav-toggle {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  background: none;
  border: none;
  cursor: pointer;
  color: var(--cc-text-dim);
  padding: 0.25rem 0.4rem;
  border-radius: 0.3rem;
  font-size: 0.9rem;
  margin-left: -0.3rem;
}
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
  font-size: 0.75rem;
  font-weight: 500;
  padding: 0.2rem 0.65rem;
  border-radius: 999px;
  cursor: default;
  white-space: nowrap;
}
.dot { width: 7px; height: 7px; border-radius: 50%; }

.ws-badge.connected    { background: #14532d33; color: #86efac; }
.ws-badge.connected .dot { background: #4ade80; box-shadow: 0 0 5px #4ade80; }
.ws-badge.connecting   { background: #78350f33; color: #fcd34d; }
.ws-badge.connecting .dot { background: #fbbf24; }
.ws-badge.disconnected { background: #27272a55; color: #71717a; }
.ws-badge.disconnected .dot { background: #52525b; }
.ws-badge.error        { background: #7f1d1d33; color: #fca5a5; }
.ws-badge.error .dot   { background: #ef4444; box-shadow: 0 0 5px #ef4444; }
</style>
