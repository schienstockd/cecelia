<!--
  Standalone console window (opened from the docked console bar's pop-out button via window.open('#/console')).
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

onMounted(() => {
  document.title = 'Cecelia — Console'
  // Backfill so the window isn't empty until the next line arrives. The fetch itself now lives in the
  // log store (`backfill` → `repairGap`), because the DOCKED console needs exactly the same thing on a
  // page load — it never had it, so the console actually in front of you always started blank while
  // the pop-out was the only one that showed history. One implementation, both mount points; the ws
  // store calls it again on every (re)connect.
  log.backfill()
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
