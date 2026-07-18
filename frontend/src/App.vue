<script setup lang="ts">
import { onMounted, computed, watch } from 'vue'
import { useRoute } from 'vue-router'
import { useWsStore } from './stores/ws'
import { useSettingsStore } from './stores/settings'
import { useAppControlStore } from './stores/appControl'
import { useObserverStore } from './stores/observer'
import { useProjectMetaStore } from './stores/projectMeta'
import AppHeader from './components/AppHeader.vue'
import AppSidebar from './components/AppSidebar.vue'
import HintCallout from './components/HintCallout.vue'
import ErrorConsole from './components/ErrorConsole.vue'
import FloatingPanel from './components/FloatingPanel.vue'
import ViewerPanel from './components/ViewerPanel.vue'
import LabLogPanel from './components/LabLogPanel.vue'
import Toast from 'primevue/toast'

const ws = useWsStore()
const settings = useSettingsStore()
const appCtl = useAppControlStore()
// Observer "Watch" runs from a store, not the lab-log panel, so it keeps working while the panel is
// closed (the panel is v-if'd). Install the always-on auto-runner once, and refresh its status/session
// whenever the open project changes.
const observer = useObserverStore()
const pm = useProjectMetaStore()
observer.installAutoWatch()
watch(() => pm.current?.uid, () => observer.refresh(), { immediate: true })
onMounted(async () => {
  ws.connect()
  appCtl.checkUpdate()   // surfaces the header update badge app-wide (fire-and-forget)
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
        <!-- first-launch only: browsers don't stop the server on tab close -->
        <HintCallout hint-key="shutdown"
          text="When you're done, use the Quit button (bottom-left) — not the browser tab — to stop Cecelia cleanly." />
        <RouterView v-slot="{ Component, route }">
          <!-- key custom-category pages by path so /custom/:category remounts (fresh task defs)
               when the category changes; other pages keep default (keyless) reuse -->
          <KeepAlive include="ChainModule">
            <component :is="Component" :key="route.meta?.customPage ? route.fullPath : undefined" />
          </KeepAlive>
        </RouterView>
      </main>
    </div>
    <!-- napari viewer controls: a floating dockable panel (toggled from the sidebar "Viewer" button),
         floating above the content so it's usable on any page while an image is open in napari -->
    <FloatingPanel v-if="settings.viewerPanelOpen" title="Viewer" icon="pi-eye" storage-key="viewer"
                   accent="var(--cc-viewer)" @close="settings.viewerPanelOpen = false">
      <ViewerPanel />
    </FloatingPanel>
    <!-- lab log: per-project append-only analysis memory (human + Claude), reachable on any page -->
    <FloatingPanel v-if="settings.labLogPanelOpen" title="Lab log" icon="pi-book" storage-key="lablog"
                   accent="rgba(255, 255, 255, 0.6)"
                   :default-x="300" :default-y="96" :default-w="340" :default-h="520"
                   @close="settings.labLogPanelOpen = false">
      <LabLogPanel />
    </FloatingPanel>
    <ErrorConsole />
    <Toast position="bottom-right" />
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
