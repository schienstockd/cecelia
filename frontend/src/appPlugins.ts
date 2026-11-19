// What every Vue app instance needs before it mounts anything: the store, the keep-alive pause and the
// two global directives. One function so the app (`main.ts`) and the browser tests
// (`src/test/browser/mount.ts`) register the same things — `rangeFillTransform` injects `v-range-fill`
// into every compiled `<input type="range">`, so a test app missing it fails to resolve the directive.
import type { App } from 'vue'
import type { Pinia } from 'pinia'
import { keepAlivePause } from './composables/useKeepAlive'
import { tooltip } from './directives/tooltip'
import { rangeFill } from './directives/rangeFill'

export function installAppPlugins(app: App, pinia: Pinia): void {
  app.use(pinia)
  // module pages under App.vue's <KeepAlive> go inert while hidden — see composables/useKeepAlive.ts
  app.use(keepAlivePause)
  app.directive('tooltip', tooltip)
  app.directive('range-fill', rangeFill)
}
