<script setup lang="ts">
import { onMounted, computed, watch } from 'vue'
import { useRoute } from 'vue-router'
import { useWsStore } from './stores/ws'
import { useSettingsStore } from './stores/settings'
import { useAppControlStore } from './stores/appControl'
import { useObserverStore } from './stores/observer'
import { useLabCaptureStore } from './stores/labCapture'
import { useProjectMetaStore } from './stores/projectMeta'
import AppHeader from './components/AppHeader.vue'
import AppSidebar from './components/AppSidebar.vue'
import HintCallout from './components/HintCallout.vue'
import ErrorConsole from './components/ErrorConsole.vue'
import FloatingPanel from './components/FloatingPanel.vue'
import ViewerPanel from './components/ViewerPanel.vue'
import LabLogPanel from './components/LabLogPanel.vue'
import Toast from 'primevue/toast'
import { useToast } from 'primevue/usetoast'
import { useTaskStore } from './stores/tasks'
import WhatsNewDialog from './components/WhatsNewDialog.vue'
import GuidesDialog from './components/GuidesDialog.vue'
import GuideBubble from './components/GuideBubble.vue'
import { isWhatsNewOpen, closeWhatsNew, openWhatsNew } from './lib/whatsNew'
import { isGuidesOpen } from './lib/guideOpen'
import { useGuideStore } from './stores/guide'
import { todayKey } from './lib/tips'
import { useNapariAutoShow } from './composables/useNapariAutoShow'

const ws = useWsStore()
const settings = useSettingsStore()
const appCtl = useAppControlStore()
// Observer state lives in a store (not the v-if'd lab-log panel) so it survives the panel closing.
// Claude is on-demand only (Ask Claude); refresh its status/session whenever the open project changes.
const observer = useObserverStore()
// The guide runtime, instantiated here so its poll/lifecycle belongs to the shell rather than to the
// v-if'd bubble — a guide has to survive route changes (docs/todo/GUIDE_SYSTEM_PLAN.md).
const guide = useGuideStore()
const pm = useProjectMetaStore()
watch(() => pm.current?.uid, () => observer.refresh(), { immediate: true })

// Restore each image's remembered napari overlays (labels, branches, tracks, populations) when it
// opens. Mounted HERE, not in the v-if'd ViewerPanel — same reason as the observer store above: with
// the floating Viewer panel closed (its default) nothing was listening for `napari:opened`, so the
// toggles read ON but no overlay was ever requested until the user flipped them by hand.
useNapariAutoShow()

// Universal "started in background" confirmation: any client-dispatched background job (crop, copy,
// project export/import, task:run) registers via taskStore.add(), which bumps `lastStarted`. One
// toast here means no dialog needs its own "it's running" feedback and users don't have to open the
// task console to confirm a job started.
const toast = useToast()
const taskStore = useTaskStore()
watch(() => taskStore.lastStarted, (t) => {
  if (t) toast.add({ severity: 'success', summary: 'Started', life: 2500,
                     detail: `${t.label} — running in the background` })
})
// Cecelia's automatic activity summaries: fire capture_context! after a task/chain node finishes,
// which upserts the rolling DAILY [Cecelia] digest (app-lifetime install, since the lab-log panel is
// v-if'd). Firing per task is cheap — the backend regenerates today's one block. See stores/labCapture.ts.
useLabCaptureStore().installAutoCapture()
onMounted(async () => {
  ws.connect()
  appCtl.checkUpdate()   // surfaces the header update badge app-wide (fire-and-forget)

  // Tip of the day (WHATS_NEW_PLAN.md W4). Opens the What's New modal once per day with today's
  // tip on top. The `Don't show tips on launch` checkbox on any tip card sets tipsOnLaunch=false
  // permanently. We stamp the date BEFORE opening so a crash mid-open doesn't re-trigger.
  const today = todayKey()
  if (settings.tipsOnLaunch && settings.tipsLastShown !== today) {
    settings.tipsLastShown = today
    openWhatsNew({ withTip: true })
  }
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
    <!-- What's New / release-notes modal — one mount, opened from the header badge and Settings.
         State lives in lib/whatsNew.ts (isWhatsNewOpen); callers just call openWhatsNew(). -->
    <WhatsNewDialog v-if="isWhatsNewOpen" @close="closeWhatsNew" />
    <!-- Guides: the picker (a modal) and the bubble (one mount, teleported, survives navigation).
         The bubble renders only while a guide is running — see stores/guide.ts. -->
    <GuidesDialog v-if="isGuidesOpen" />
    <GuideBubble v-if="guide.active" />
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
