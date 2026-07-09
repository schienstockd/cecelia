<!--
  Standalone console window (opened from Settings → System → Open console via window.open('#/console')).
  It's a bare, full-window mount of the SAME ErrorConsole component the docked bar uses (one console
  implementation, no second task-tracking path) — this popup is just a second mount point. Being a
  separate browser window it's a fresh app instance with its own WS connection, so it streams
  independently; on open it backfills the server's recent log lines from GET /api/logs/recent.
-->
<script setup lang="ts">
import { onMounted } from 'vue'
import ErrorConsole from '../components/ErrorConsole.vue'
import { useLogStore } from '../stores/log'

const log = useLogStore()

onMounted(async () => {
  document.title = 'Cecelia — Console'
  // backfill recent backend logs so the window isn't empty until the next line arrives
  try {
    const { logs } = await (await fetch('/api/logs/recent')).json() as { logs: { level: string; message: string }[] }
    for (const l of logs) {
      const level = (l.level === 'error' || l.level === 'warn') ? l.level : 'info'
      log.push(level as any, l.message, { source: 'server' })
    }
  } catch { /* server may be down — the live stream fills in once it's up */ }
})
</script>

<template>
  <div class="console-window cc-dark">
    <ErrorConsole fill />
  </div>
</template>

<style scoped>
.console-window { height: 100vh; width: 100vw; overflow: hidden; background: var(--cc-console-bg); }
</style>
