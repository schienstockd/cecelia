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
import ErrorConsole from './components/ErrorConsole.vue'
import FloatingPanel from './components/FloatingPanel.vue'
import ViewerPanel from './components/ViewerPanel.vue'
import LabLogPanel from './components/LabLogPanel.vue'
import Toast from 'primevue/toast'
import { useToast } from 'primevue/usetoast'
import { useTaskStore } from './stores/tasks'
import { useProjectStore } from './stores/project'
import { useLogStore } from './stores/log'
import WhatsNewDialog from './components/WhatsNewDialog.vue'
import GuidesDialog from './components/GuidesDialog.vue'
import IconLegendDialog from './components/IconLegendDialog.vue'
import ColorLegendDialog from './components/ColorLegendDialog.vue'
import GuideBubble from './components/GuideBubble.vue'
import { isWhatsNewOpen, closeWhatsNew, openWhatsNew } from './lib/whatsNew'
import { isGuidesOpen } from './lib/guideOpen'
import { isIconLegendOpen } from './lib/iconLegendOpen'
import { isColorLegendOpen } from './lib/colorLegendOpen'
import { useGuideStore } from './stores/guide'
import { todayKey } from './lib/tips'
import { useOverlayAutoShow } from './composables/useOverlayAutoShow'
import { isPopoutWindow } from './lib/popout'

const ws = useWsStore()
const settings = useSettingsStore()
const appCtl = useAppControlStore()
// Observer state lives in a store (not the v-if'd lab-log panel) so it survives the panel closing.
// Claude is on-demand only (Ask Claude); refresh its status/session whenever the open project changes.
const observer = useObserverStore()
// The guide runtime, instantiated here so its poll/lifecycle belongs to the shell rather than to the
// v-if'd bubble — a guide has to survive route changes (docs/todo/GUIDE_SYSTEM_PLAN.md).
const guide = useGuideStore()

// Set in onMounted when the launch tip fires for the FIRST time ever; consumed once, on close. Not a
// ref — nothing renders from it.
let firstEverTips = false

// The orientation tour starts by itself the first time a user closes the welcome dialog, and only
// that time: reading the cards tells you what Cecelia does, and this says where the buttons are.
// Afterwards it is opt-in — the "Show me" button on the about card, or the compass.
//
// Watches the shared flag rather than the dialog's `@close` emit, because the emit is not the only
// way it shuts: `WhatNewCard`'s "Show me" calls `closeWhatsNew()` itself. Hanging this off @close
// would leave `firstEverTips` unconsumed in that case, and the tour would then ambush the user the
// next time they closed What's New from the header — days later.
//
// Two guards, both load-bearing:
//  - `setupRequired` — /setup is a `bare` route with no header or sidebar (main.ts), and App.vue's
//    onMounted runs there too, so on a genuinely fresh install the dialog opens over the setup wizard.
//    Touring chrome that is not rendered would be worse than not touring at all.
//  - `guide.active` — "Show me" starts a guide and *then* closes the dialog, so by the time this runs
//    a guide the user explicitly asked for may already be going. Do not replace it.
watch(isWhatsNewOpen, (open) => {
  if (open || !firstEverTips) return
  firstEverTips = false
  if (appCtl.setupRequired === false && !guide.active) guide.start('find-your-way-around')
})
const pm = useProjectMetaStore()
watch(() => pm.current?.uid, () => observer.refresh(), { immediate: true })

// The one cross-store invariant nothing else can see: the loaded SETS must belong to the project the
// app says is open. It was reported broken — the image table listing a previous project's images
// under the new project's name — and the load path cannot produce that on its own, so if it happens
// again this is what says so. The views already refuse to render a mismatched set (they show an empty
// table rather than someone else's images); this makes it legible instead of looking like an empty
// project. Named uids, because "stale data" without them is not a lead.
const projectStore = useProjectStore()
const logStore = useLogStore()
watch([() => pm.current?.uid, () => projectStore.loadedProjectUid], ([openUid, loadedUid]) => {
  if (!openUid || !loadedUid || openUid === loadedUid) return
  logStore.error(`Project data mismatch: loaded sets belong to ${loadedUid} while ${openUid} is open.`,
    { source: 'project' })
}, { immediate: true })

// A pop-out window (the console, the Task Manager) is a second FULL app instance with its own WS, so
// everything App.vue starts here would run twice. That is invisible for anything that only touches
// this window, and not invisible at all for the three below, which act on the BACKEND or on shared
// state: two overlay-restore passes per image open, two lab-log captures per finished task, and a
// tip-of-the-day silently consumed (stamped by a window whose bare route never renders the dialog).
// A popout is a VIEW of the app, not a second copy of it.
// `lib/popout.ts` reads the hash, not the route: the first navigation has not resolved during setup.
const popout = isPopoutWindow()

// Restore each image's remembered viewer overlays (labels, branches, tracks, populations) when it
// opens. Mounted HERE, not in the v-if'd ViewerPanel — same reason as the observer store above: with
// the floating Viewer panel closed (its default) nothing was listening for the image-open event, so
// the toggles read ON but no overlay was ever requested until the user flipped them by hand.
if (!popout) useOverlayAutoShow()

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
if (!popout) useLabCaptureStore().installAutoCapture()
onMounted(async () => {
  ws.connect()
  appCtl.checkUpdate()   // surfaces the header update badge app-wide (fire-and-forget)

  // Tip of the day (WHATS_NEW_PLAN.md W4). Opens the What's New modal once per day with today's
  // tip on top. The `Don't show tips on launch` checkbox on any tip card sets tipsOnLaunch=false
  // permanently. We stamp the date BEFORE opening so a crash mid-open doesn't re-trigger.
  const today = todayKey()
  if (!popout && settings.tipsOnLaunch && settings.tipsLastShown !== today) {
    // `tipsLastShown` is '' until this branch has run ONCE, ever — so reading it before the stamp is
    // the first-launch signal, and no second flag has to be persisted to get it. See onWhatsNewClose.
    firstEverTips = settings.tipsLastShown === ''
    settings.tipsLastShown = today
    openWhatsNew({ withTip: true })
  }
})

// `bare` routes (e.g. the standalone console window) render full-window without the app shell
// (header / sidebar / docked console). See the /console route in main.ts.
//
// `popout ||` is not belt-and-braces, it is the whole point: `route.meta` arrives only once the FIRST
// navigation resolves, and that navigation awaits the boot guard's `refreshStartup()` fetch plus the
// route's lazy chunk. Until then `route` is the start location — no `meta`, so `bare` read false and
// a popout window painted the entire app shell before swapping it out (measured on a warm dev server:
// header + sidebar from ~230ms to ~285ms; a cold boot or a slow backend holds that frame for a
// second or more, which is exactly what it looks like — the app "greyed out", every module locked,
// because no project is open in a window that was never meant to have one). Whether this window is a
// popout is knowable synchronously here (`lib/popout.ts` reads the window's name and the hash), so
// the shell never gets the chance.
const route = useRoute()
const bare = computed(() => popout || route.meta.bare === true)
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
        <!-- The "closing the tab does not stop the backend" hint that used to sit here is now a step in
             the orientation tour, beside the Quit button it is about (lib/guides/tour.ts). -->
        <RouterView v-slot="{ Component, route }">
          <!-- key custom-category pages by path so /custom/:category remounts (fresh task defs)
               when the category changes; other pages keep default (keyless) reuse -->
          <KeepAlive include="ChainModule">
            <component :is="Component" :key="route.meta?.customPage ? route.fullPath : undefined" />
          </KeepAlive>
        </RouterView>
      </main>
    </div>
    <!-- Viewer controls: a floating dockable panel (toggled from the sidebar "Viewer" button),
         floating above the content so it's usable on any page while an image is open in the viewer -->
    <FloatingPanel v-if="settings.viewerPanelOpen" title="Viewer" icon="pi-eye" storage-key="viewer"
                   accent="var(--cc-viewer)" @close="settings.viewerPanelOpen = false">
      <ViewerPanel />
    </FloatingPanel>
    <!-- lab log: per-project append-only analysis memory (human + Claude), reachable on any page -->
    <FloatingPanel v-if="settings.labLogPanelOpen" title="Lab log" icon="pi-book" storage-key="lablog"
                   accent="var(--cc-guide)"
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
    <IconLegendDialog v-if="isIconLegendOpen" />
    <ColorLegendDialog v-if="isColorLegendOpen" />
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
