import { createApp } from 'vue'
import { createPinia } from 'pinia'
import { createRouter, createWebHashHistory } from 'vue-router'
import PrimeVue from 'primevue/config'
import Tooltip from 'primevue/tooltip'
import Aura from '@primevue/themes/aura'
import 'primeicons/primeicons.css'
import './style.css'
import App from './App.vue'

import ImportModule   from './modules/ImportModule.vue'
import MetadataModule from './modules/MetadataModule.vue'
import CleanupModule  from './modules/CleanupModule.vue'
import SegmentModule  from './modules/SegmentModule.vue'
import TasksModule    from './modules/TasksModule.vue'
import SettingsModule from './modules/SettingsModule.vue'
import TestModule     from './modules/TestModule.vue'
import ChainModule    from './modules/ChainModule.vue'
import GatingModule   from './modules/GatingModule.vue'
import TrackingModule from './modules/TrackingModule.vue'
import BehaviourModule from './modules/BehaviourModule.vue'
import ClusterCellsModule  from './modules/ClusterCellsModule.vue'
import ClusterTracksModule from './modules/ClusterTracksModule.vue'

const pinia = createPinia()

const router = createRouter({
  history: createWebHashHistory(),
  routes: [
    { path: '/',          redirect: '/import' },
    { path: '/import',    component: ImportModule,   meta: { label: 'Import' } },
    { path: '/metadata',  component: MetadataModule, meta: { label: 'Metadata' } },
    { path: '/cleanup',   component: CleanupModule,  meta: { label: 'Cleanup' } },
    { path: '/segment',   component: SegmentModule,  meta: { label: 'Segment' } },
    { path: '/gate',      component: GatingModule,   meta: { label: 'Gate' } },
    { path: '/track',     component: TrackingModule, meta: { label: 'Track' } },
    { path: '/behaviour', component: BehaviourModule, meta: { label: 'Behaviour' } },
    { path: '/clust-cells',  component: ClusterCellsModule,  meta: { label: 'Cluster cells' } },
    { path: '/clust-tracks', component: ClusterTracksModule, meta: { label: 'Cluster tracks' } },
    { path: '/tasks',     component: TasksModule,     meta: { label: 'Tasks' } },
    { path: '/chain',     component: ChainModule,     meta: { label: 'Whiteboard' } },
    { path: '/settings',  component: SettingsModule,  meta: { label: 'Settings' } },
    { path: '/test',      component: TestModule,      meta: { label: 'Test Tasks' } },
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
