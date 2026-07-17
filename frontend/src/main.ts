import { createApp } from 'vue'
import { createPinia } from 'pinia'
import { createRouter, createWebHashHistory } from 'vue-router'
import PrimeVue from 'primevue/config'
import Tooltip from 'primevue/tooltip'
import Aura from '@primeuix/themes/aura'
import 'primeicons/primeicons.css'
import './style.css'
import App from './App.vue'
import { useAppControlStore } from './stores/appControl'

// Module pages are lazy-loaded so each becomes its own chunk fetched on navigation, instead of one
// giant eager `index` bundle at boot (the heavy ones — ChainModule pulls @vue-flow, the canvas pages
// pull the plot stack — should not load until visited). See docs/UI.md → "Route-level code splitting".
const pinia = createPinia()

const router = createRouter({
  history: createWebHashHistory(),
  routes: [
    { path: '/',          redirect: '/import' },
    { path: '/import',    component: () => import('./modules/ImportModule.vue'),        meta: { label: 'Import' } },
    { path: '/metadata',  component: () => import('./modules/MetadataModule.vue'),      meta: { label: 'Metadata' } },
    { path: '/cleanup',   component: () => import('./modules/CleanupModule.vue'),       meta: { label: 'Cleanup' } },
    { path: '/segment',   component: () => import('./modules/SegmentModule.vue'),       meta: { label: 'Segment' } },
    { path: '/gate',      component: () => import('./modules/GatingModule.vue'),        meta: { label: 'Gate' } },
    { path: '/phenotype', component: () => import('./modules/PhenotypeModule.vue'),     meta: { label: 'Phenotype' } },
    { path: '/track',     component: () => import('./modules/TrackingModule.vue'),      meta: { label: 'Track' } },
    { path: '/behaviour', component: () => import('./modules/BehaviourModule.vue'),     meta: { label: 'Behaviour' } },
    { path: '/clust-cells',  component: () => import('./modules/ClusterCellsModule.vue'),  meta: { label: 'Cluster cells' } },
    { path: '/clust-tracks', component: () => import('./modules/ClusterTracksModule.vue'), meta: { label: 'Cluster tracks' } },
    { path: '/analysis',  component: () => import('./modules/AnalysisModule.vue'),      meta: { label: 'Analysis board' } },
    { path: '/notebooks', component: () => import('./modules/NotebooksModule.vue'),     meta: { label: 'Notebooks' } },
    { path: '/animation', component: () => import('./modules/AnimationModule.vue'),     meta: { label: 'Animation' } },
    { path: '/batch-movies', component: () => import('./modules/BatchMoviesModule.vue'), meta: { label: 'Batch movies' } },
    { path: '/tasks',     component: () => import('./modules/TasksModule.vue'),         meta: { label: 'Tasks' } },
    { path: '/chain',     component: () => import('./modules/ChainModule.vue'),         meta: { label: 'Whiteboard' } },
    // Generic page for a user custom-module category with no built-in page (docs/CUSTOM_MODULES.md).
    { path: '/custom/:category', component: () => import('./modules/CustomModule.vue'),  meta: { label: 'Custom', customPage: true } },
    { path: '/settings',  component: () => import('./modules/SettingsModule.vue'),      meta: { label: 'Settings' } },
    // bare = rendered full-window without the app shell (opened in its own window via window.open)
    { path: '/console',   component: () => import('./modules/ConsoleView.vue'),         meta: { label: 'Console', bare: true } },
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
  if (appCtl.setupRequired === false && to.path === '/setup') return '/import'
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
app.directive('tooltip', Tooltip)
app.mount('#app')
