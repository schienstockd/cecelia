import { createApp } from 'vue'
import { createPinia } from 'pinia'
import { createRouter, createWebHashHistory } from 'vue-router'
import PrimeVue from 'primevue/config'
import Tooltip from 'primevue/tooltip'
import ToastService from 'primevue/toastservice'
import Aura from '@primeuix/themes/aura'
import 'primeicons/primeicons.css'
import './style.css'
import App from './App.vue'
import { useAppControlStore } from './stores/appControl'
import { useLogStore } from './stores/log'
import { installRoLoopTrace } from './utils/roLoopTrace'
import { popoutRouteOfWindow } from './lib/popout'

// Module pages are lazy-loaded so each becomes its own chunk fetched on navigation, instead of one
// giant eager `index` bundle at boot (the heavy ones — ChainModule pulls @vue-flow, the canvas pages
// pull the plot stack — should not load until visited). See docs/UI.md → "Route-level code splitting".
const pinia = createPinia()

const router = createRouter({
  history: createWebHashHistory(),
  routes: [
    // A real page, NOT a redirect. `/` used to redirect to /manage-images, which a view profile can
    // hide — and a record's `redirect` resolves BEFORE any guard, so the profile list had not arrived
    // yet and a cold boot landed on the fallback and then bounced. A neutral home removes the race
    // rather than timing around it. See docs/todo/VIEW_PROFILES_PLAN.md → Decision 9.
    { path: '/',          component: () => import('./modules/WelcomeModule.vue'), meta: { label: 'Welcome' } },
    { path: '/manage-images', component: () => import('./modules/ManageImagesModule.vue'), meta: { label: 'Manage images' } },
    { path: '/metadata',  component: () => import('./modules/MetadataModule.vue'),      meta: { label: 'Metadata' } },
    { path: '/cleanup',   component: () => import('./modules/CleanupModule.vue'),       meta: { label: 'Cleanup' } },
    { path: '/optical-flow', component: () => import('./modules/OpticalFlowModule.vue'), meta: { label: 'Optical flow' } },
    { path: '/segment',   component: () => import('./modules/SegmentModule.vue'),       meta: { label: 'Segment' } },
    { path: '/gate',      component: () => import('./modules/GatingModule.vue'),        meta: { label: 'Gate' } },
    { path: '/phenotype', component: () => import('./modules/PhenotypeModule.vue'),     meta: { label: 'Phenotype' } },
    { path: '/track',     component: () => import('./modules/TrackingModule.vue'),      meta: { label: 'Track' } },
    { path: '/behaviour', component: () => import('./modules/BehaviourModule.vue'),     meta: { label: 'Behaviour' } },
    { path: '/clust-cells',  component: () => import('./modules/ClusterCellsModule.vue'),  meta: { label: 'Cluster cells' } },
    { path: '/clust-tracks', component: () => import('./modules/ClusterTracksModule.vue'), meta: { label: 'Cluster tracks' } },
    { path: '/regions',   component: () => import('./modules/RegionClusteringModule.vue'), meta: { label: 'Cluster regions' } },
    { path: '/spatial',   component: () => import('./modules/SpatialAnalysisModule.vue'),  meta: { label: 'Spatial' } },
    { path: '/analysis',  component: () => import('./modules/AnalysisModule.vue'),      meta: { label: 'Analysis board' } },
    { path: '/notebooks', component: () => import('./modules/NotebooksModule.vue'),     meta: { label: 'Notebooks' } },
    { path: '/animation', component: () => import('./modules/AnimationModule.vue'),     meta: { label: 'Animation' } },
    { path: '/batch-movies', component: () => import('./modules/BatchMoviesModule.vue'), meta: { label: 'Batch movies' } },
    { path: '/movies',    component: () => import('./modules/MoviesModule.vue'),        meta: { label: 'Movies' } },
    { path: '/tasks',     component: () => import('./modules/TasksModule.vue'),         meta: { label: 'Tasks' } },
    { path: '/chain',     component: () => import('./modules/ChainModule.vue'),         meta: { label: 'Whiteboard' } },
    // Generic page for a user custom-module category with no built-in page (docs/CUSTOM_MODULES.md).
    { path: '/custom/:category', component: () => import('./modules/CustomModule.vue'),  meta: { label: 'Custom', customPage: true } },
    { path: '/settings',  component: () => import('./modules/SettingsModule.vue'),      meta: { label: 'Settings' } },
    // bare = rendered full-window without the app shell (opened in its own window via window.open)
    { path: '/console',   component: () => import('./modules/ConsoleView.vue'),         meta: { label: 'Console', bare: true } },
    // the Task Manager in its own window — same idiom, and it carries `?project=<uid>` because a
    // popup opens with no project and this list is scoped to one (modules/TasksView.vue).
    { path: '/tasks-window', component: () => import('./modules/TasksView.vue'),          meta: { label: 'Task Manager', bare: true } },
    // the in-browser volume viewer in its own window, seeded with
    // `?project=<uid>&image=<uid>&valueName=<vn>` because a popup opens with no project open.
    // docs/todo/WEB_VIEWER_PLAN.md.
    { path: '/viewer-window', component: () => import('./modules/ViewerWindow.vue'),      meta: { label: 'Viewer', bare: true } },
    // first-launch setup wizard — bare (clean welcome screen, no sidebar/header). The boot guard
    // below routes here when the backend reports setupRequired. See docs/todo/ONBOARDING_PLAN.md.
    { path: '/setup',     component: () => import('./modules/SetupModule.vue'),          meta: { label: 'Setup', bare: true } },
  ],
})

// First-launch boot guard: ask the backend once whether config setup is still needed. While it is,
// every route redirects to /setup; once done, /setup bounces back to the app. `setupRequired` stays
// null if the diagnostics call fails, so a backend blip never traps the user on /setup.
let _startupChecked = false
router.beforeEach(async (to) => {
  const appCtl = useAppControlStore()
  if (!_startupChecked) {
    _startupChecked = true
    await appCtl.refreshStartup()
  }
  if (appCtl.setupRequired === true && to.path !== '/setup') return '/setup'
  if (appCtl.setupRequired === false && to.path === '/setup') return '/'
  // A popout window stays the view it was opened as. Its NAME says which one (lib/popout.ts) and the
  // name survives what the hash does not — a reload, a restored session, a stale bundle whose router
  // had no such route yet. A window that ends up somewhere else is not a page you can use: the Task
  // Manager popup on `/tasks` is `TasksModule` with `standalone` false, so nothing tells it which
  // project to show and nothing follows the main window's switches — an empty list in a window with
  // no way to fix itself. Send it home instead. (Skipped while setup is required, so this and the
  // /setup redirect above cannot bounce a window between them.)
  if (appCtl.setupRequired !== true) {
    const own = popoutRouteOfWindow()
    if (own && to.path !== own) return own
  }
  return true
})

const app = createApp(App)
app.use(pinia)
app.use(router)
app.use(PrimeVue, {
  theme: {
    preset: Aura,
    options: {
      darkModeSelector: '.cc-dark',
      cssLayer: { name: 'primevue', order: 'theme, base, primevue' },
    },
  },
})
app.use(ToastService)
app.directive('tooltip', Tooltip)

// ── The browser's own failures → the console ─────────────────────────────────
// The console reported everything the SERVER side could go wrong with and nothing this half could. A
// Vue render error or a rejected promise showed up only in the browser devtools — which nobody has
// open — so the visible symptom of a frontend bug was a panel that just never appeared, with a console
// sitting underneath it saying all was well. Three hooks cover the three ways JS fails; each keeps its
// default behaviour (rethrow / log) so devtools is unaffected.
//
// Registered after `app.use(pinia)`, because `useLogStore()` needs the active pinia.
const bootLog = useLogStore(pinia)
app.config.errorHandler = (err, _instance, info) => {
  bootLog.error(`UI error (${info}): ${err instanceof Error ? err.message : String(err)}`,
                { source: 'frontend', detail: err instanceof Error ? err.stack : String(err) })
  console.error(err)                                       // keep the devtools behaviour we replaced
}
window.addEventListener('error', e => {
  // Suppress the benign ResizeObserver loop message — the browser fires it as a bare ErrorEvent
  // when an observer callback resizes something the observer sees, and it means the browser skipped
  // ONE notification tick (it'll fire again next frame). Nothing actionable to log; without this,
  // it fills the rail during ordinary layout work (Dominik, 2026-08-26). In dev the
  // `installRoLoopTrace` hook below still lands one attributed warn per observer creation site,
  // which is the useful signal.
  if (e.message && /ResizeObserver loop/.test(e.message)) return
  bootLog.error(`Script error: ${e.message}`,
                { source: 'frontend', detail: e.error instanceof Error ? e.error.stack : `${e.filename}:${e.lineno}` })
})
window.addEventListener('unhandledrejection', e => {
  const r = e.reason
  bootLog.error(`Unhandled promise rejection: ${r instanceof Error ? r.message : String(r)}`,
                { source: 'frontend', detail: r instanceof Error ? r.stack : undefined })
})
// The hook above can only log "ResizeObserver loop completed with undelivered notifications" verbatim:
// the browser fires it as a bare ErrorEvent with no script origin, so the rail says a callback resized
// what it observes and names nobody. This attributes it at the source instead — DEV only, one line per
// creation site, and it reports a dependency's observer as readily as ours (`regl-scatterplot` and
// several PrimeVue components observe too), which is what decides whether the answer is a fix or a
// classification. Installed BEFORE `app.mount`, since an observer built earlier keeps the native class.
if (import.meta.env.DEV) {
  installRoLoopTrace((message, detail) => bootLog.warn(message, { source: 'frontend', detail }))
}

app.mount('#app')
