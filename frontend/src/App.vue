<script setup lang="ts">
import { onMounted, computed } from 'vue'
import { useRoute } from 'vue-router'
import { useWsStore } from './stores/ws'
import { useSettingsStore } from './stores/settings'
import AppHeader from './components/AppHeader.vue'
import AppSidebar from './components/AppSidebar.vue'
import ErrorConsole from './components/ErrorConsole.vue'

const ws = useWsStore()
const settings = useSettingsStore()
onMounted(async () => {
  ws.connect()
  // Reconcile the discrete-GPU flag with the backend once at startup. The flag is a launch-time
  // decision (the bridge starts lazily on first open), so it must be right before then.
  //  - explicit user choice saved → push it, so the backend uses it even after a backend restart
  //    reset its Ref to the config default;
  //  - no saved choice → adopt the backend/config default (don't clobber a custom.toml setting).
  try {
    const stored = localStorage.getItem('cc.napariDiscreteGpu')
    if (stored !== null) {
      await fetch('/api/napari/gpu', {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ enabled: stored === 'true' }),
      })
    } else {
      const d = await (await fetch('/api/napari/gpu')).json()
      settings.napariDiscreteGpu = !!d.discreteGpu
    }
  } catch { /* backend keeps its default until Settings sets it */ }
})

// `bare` routes (e.g. the standalone console window) render full-window without the app shell
// (header / sidebar / docked console). See the /console route in main.ts.
const route = useRoute()
const bare = computed(() => route.meta.bare === true)
</script>

<template>
  <!-- bare: full-window single view (own window via window.open) -->
  <div v-if="bare" class="cc-dark cc-bare">
    <RouterView />
  </div>
  <!-- normal app shell -->
  <div v-else class="cc-dark cc-shell">
    <AppHeader />
    <div class="cc-content">
      <AppSidebar />
      <main class="cc-main">
        <RouterView v-slot="{ Component }">
          <KeepAlive include="ChainModule">
            <component :is="Component" />
          </KeepAlive>
        </RouterView>
      </main>
    </div>
    <ErrorConsole />
  </div>
</template>

<style scoped>
.cc-shell {
  display: flex;
  flex-direction: column;
  height: 100vh;
  overflow: hidden;
}

.cc-content {
  flex: 1;
  display: flex;
  overflow: hidden;
}

.cc-main {
  flex: 1;
  overflow-y: auto;
  background: var(--cc-bg);
}

.cc-bare {
  height: 100vh;
  overflow: hidden;
}
</style>
