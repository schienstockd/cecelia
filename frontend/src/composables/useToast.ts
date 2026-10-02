// The app's one notification queue (docs/UI.md → *Toast notifications*). `useToast().add(…)` from
// anywhere — a component, a store, a plain module — and `components/ToastHost.vue`, mounted once in
// App.vue, renders it. Same call shape as PrimeVue's `useToast()`, which this replaces
// (docs/todo/PRIMEVUE_RETIRE_PLAN.md, P1). Module-level state, so it needs no plugin and no
// component context.
//
// Life runs while the toast is on screen and pauses while the pointer is over it, so an error detail
// doesn't vanish mid-read.
import { readonly, ref } from 'vue'
import { toEntry, pushToast, droppedIds, lifeLeft, type ToastEntry, type ToastMessage } from '../utils/toastQueue'

export type { ToastMessage, ToastSeverity } from '../utils/toastQueue'

const toasts = ref<ToastEntry[]>([])
let nextId = 0
// per toast: its pending close and when it would fire, or its remaining life while paused
const timers = new Map<number, ReturnType<typeof setTimeout>>()
const deadlines = new Map<number, number>()
const paused = new Map<number, number>()

function arm(id: number, ms: number) {
  deadlines.set(id, Date.now() + ms)
  timers.set(id, setTimeout(() => remove(id), ms))
}

function forget(id: number) {
  const t = timers.get(id)
  if (t) clearTimeout(t)
  timers.delete(id)
  deadlines.delete(id)
  paused.delete(id)
}

function add(msg: ToastMessage) {
  const entry = toEntry(msg, ++nextId)
  const before = toasts.value
  toasts.value = pushToast(before, entry)
  for (const id of droppedIds(before, toasts.value)) forget(id)
  if (entry.life) arm(entry.id, entry.life)
}

function remove(id: number) {
  forget(id)
  toasts.value = toasts.value.filter(t => t.id !== id)
}

function clear() {
  for (const t of toasts.value) forget(t.id)
  toasts.value = []
}

/** Pointer entered the toast: stop its clock. */
function pause(id: number) {
  const deadline = deadlines.get(id)
  if (deadline == null) return
  paused.set(id, lifeLeft(deadline, Date.now()))
  const t = timers.get(id)
  if (t) clearTimeout(t)
  timers.delete(id)
  deadlines.delete(id)
}

/** Pointer left: run out what was left. */
function resume(id: number) {
  const left = paused.get(id)
  if (left == null) return
  paused.delete(id)
  arm(id, left)
}

const api = { add, remove, clear, pause, resume, toasts: readonly(toasts) }

export function useToast() {
  return api
}
