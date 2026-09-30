import { nextTick, onActivated, onBeforeUnmount, onDeactivated, onMounted,
         type App, type ComponentInternalInstance, type EffectScope } from 'vue'

// Module pages with plots live under `<KeepAlive>` in App.vue, so switching pages does not throw their
// plots away and refetch them. A kept page is NOT unmounted when you leave it — it is moved into a
// detached container and keeps running. Everything here exists to make a hidden page inert:
//
//  - Reactive work (watchers, computed-driven renders) → `keepAlivePause`, below. ONE app-wide hook
//    pauses the component's effect scope while it is hidden; a watcher whose source changed meanwhile
//    runs ONCE on return with the latest value (Vue 3.5 `EffectScope.pause/resume`). This is what
//    stops a hidden page refetching on every image click / task finish / gate edit.
//    Caveat: coalescing drops intermediate values — a watcher that must see a TRANSITION (a→b→a)
//    must watch a monotonic counter instead (see `ws.connects`).
//  - Window listeners → `useWindowListener`: attached only while the page is visible. A raw
//    `window.addEventListener('keydown', …)` in onMounted keeps firing for a hidden page (Esc cancels
//    its selection, Ctrl+Z undoes on a hidden page's population tree).
//  - Timers → `useActiveInterval`.
//  - ResizeObservers → `observeBoxChanges` / the connected guard in `usePlotResize`: a detached page
//    reports 0×0; drawing (or persisting a size) then is wrong.
//  - Teleported content (`<Teleport to="body">`) is NOT moved out with the page — close it on
//    `onDeactivated` (TeleportPopover does). The pause waits one tick so that close can render.

// `scope` (the component's own effect scope — its watchers + render effect) is marked @internal in
// Vue's typings but is a stable runtime field (runtime-core `createComponentInstance`, Vue 3.5).
type Instance = ComponentInternalInstance & { scope: EffectScope }

// Components hidden right now. NOT `instance.isDeactivated`: Vue sets that on the kept page's ROOT
// only — every plot inside it would read false and never pause.
const hidden = new WeakSet<Instance>()

/** App plugin: pause a kept-alive component's effects while it is hidden, resume on return. */
export const keepAlivePause = {
  install(app: App) {
    app.mixin({
      // Hook order vs the component's own onActivated is NOT guaranteed (setup hooks register before
      // mixins; KeepAlive prepends descendants' hooks) — so an onActivated that needs resumed watchers
      // to have run first defers with nextTick (GatingPlots, useClusterContext).
      activated(this: { $: Instance }) { hidden.delete(this.$); this.$.scope.resume() },
      deactivated(this: { $: Instance }) {
        const inst = this.$
        hidden.add(inst)
        // one tick of grace: deactivate-time cleanup (closing a teleported popover) must still render
        void nextTick(() => { if (hidden.has(inst) && !inst.isUnmounted) inst.scope.pause() })
      },
    })
  },
}

/** A window listener that is attached only while the component is mounted AND visible. */
export function useWindowListener<K extends keyof WindowEventMap>(
  type: K, handler: (e: WindowEventMap[K]) => void, options?: boolean | AddEventListenerOptions,
): void {
  let attached = false
  const add = () => { if (!attached) { window.addEventListener(type, handler, options); attached = true } }
  const remove = () => { if (attached) { window.removeEventListener(type, handler, options); attached = false } }
  onMounted(add); onActivated(add)
  onDeactivated(remove); onBeforeUnmount(remove)
}

/** `setInterval` that runs only while the component is visible; fires once immediately on (re)show. */
export function useActiveInterval(fn: () => void, ms: number): void {
  let timer: number | undefined
  const start = () => { if (timer === undefined) { fn(); timer = window.setInterval(fn, ms) } }
  const stop = () => { if (timer !== undefined) { window.clearInterval(timer); timer = undefined } }
  onMounted(start); onActivated(start)
  onDeactivated(stop); onBeforeUnmount(stop)
}

/**
 * For NON-reactive callbacks (WS handlers): run now if the page is visible, else once on return.
 * Repeated deferrals of the same function collapse into one call.
 */
export function useWhenVisible(): (fn: () => void) => void {
  let visible = true
  const pending = new Set<() => void>()
  onDeactivated(() => { visible = false })
  onActivated(() => {
    visible = true
    const fns = [...pending]; pending.clear()
    fns.forEach(f => f())
  })
  return fn => { if (visible) fn(); else pending.add(fn) }
}
