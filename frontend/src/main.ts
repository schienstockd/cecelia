import { createApp } from 'vue'
import { createPinia } from 'pinia'
import { createRouter, createWebHashHistory } from 'vue-router'
import PrimeVue from 'primevue/config'
import Tooltip from 'primevue/tooltip'
import Aura from '@primeuix/themes/aura'
import 'primeicons/primeicons.css'
import './style.css'
import App from './App.vue'

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
    { path: '/track',     component: () => import('./modules/TrackingModule.vue'),      meta: { label: 'Track' } },
    { path: '/behaviour', component: () => import('./modules/BehaviourModule.vue'),     meta: { label: 'Behaviour' } },
    { path: '/clust-cells',  component: () => import('./modules/ClusterCellsModule.vue'),  meta: { label: 'Cluster cells' } },
    { path: '/clust-tracks', component: () => import('./modules/ClusterTracksModule.vue'), meta: { label: 'Cluster tracks' } },
    { path: '/analysis',  component: () => import('./modules/AnalysisModule.vue'),      meta: { label: 'Analysis board' } },
    { path: '/notebooks', component: () => import('./modules/NotebooksModule.vue'),     meta: { label: 'Notebooks' } },
    { path: '/tasks',     component: () => import('./modules/TasksModule.vue'),         meta: { label: 'Tasks' } },
    { path: '/chain',     component: () => import('./modules/ChainModule.vue'),         meta: { label: 'Whiteboard' } },
    { path: '/settings',  component: () => import('./modules/SettingsModule.vue'),      meta: { label: 'Settings' } },
    { path: '/test',      component: () => import('./modules/TestModule.vue'),          meta: { label: 'Test Tasks' } },
  ],
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
