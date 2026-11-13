// The viewer window's copy of the guide running in the main window (lib/guides/remote.ts). It has the
// fields `GuideBubble` reads from `stores/guide.ts`, filled from what the main window publishes; its
// actions are commands back. No gates run here — the main window owns progression.
import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref, computed, watch, onScopeDispose } from 'vue'
import {
  GUIDE_REMOTE_KEY, GUIDE_CMD_KEY, mainStandIn, parseGuideRemote,
  type GuideRemote, type GuideCmdName, type RemoteStep,
} from '../lib/guides/remote'

const POLL_MS = 250

export const useGuideMirrorStore = defineStore('guideMirror', () => {
  const remote = ref<GuideRemote | null>(null)
  const read = () => { try { remote.value = parseGuideRemote(localStorage.getItem(GUIDE_REMOTE_KEY)) } catch { remote.value = null } }
  read()
  const onStorage = (e: StorageEvent) => { if (e.key === GUIDE_REMOTE_KEY) read() }
  window.addEventListener('storage', onStorage)

  const active = computed(() => remote.value ? { id: 'remote', title: remote.value.title } : null)
  const index = computed(() => remote.value?.index ?? 0)
  const total = computed(() => remote.value?.total ?? 0)
  const phase = computed(() => remote.value?.phase ?? 'step')
  const gateSatisfied = computed(() => !!remote.value?.gateSatisfied)
  const currentStep = computed(() => remote.value?.step
    ? { ...remote.value.step, route: undefined as string | undefined, awaitTask: undefined as { label?: string } | undefined }
    : null)
  /** the step when it is this window's, else a pointer back to the main window */
  const step = computed<RemoteStep | null>(() => {
    const s = remote.value?.step ?? null
    if (!remote.value) return null
    return s?.window === 'viewer' ? s : mainStandIn(s)
  })
  const offRoute = false
  const needsReveal = false
  const awaitedTask = null as { imageName: string; progress: number | null } | null
  const failureNote = ''

  // the bubble re-resolves its anchor on this tick (the step's control may appear later: a menu opens)
  const domTick = ref(0)
  let poll: ReturnType<typeof setInterval> | null = null
  watch(active, a => {
    if (a && !poll) poll = setInterval(() => { domTick.value++ }, POLL_MS)
    else if (!a && poll) { clearInterval(poll); poll = null }
  }, { immediate: true })

  function send(cmd: GuideCmdName) {
    try { localStorage.setItem(GUIDE_CMD_KEY, JSON.stringify({ cmd, at: Date.now() })) } catch { /* private mode */ }
  }
  const next = () => send('next')
  const back = () => send('back')
  const exit = () => send('exit')
  const satisfyClick = () => { if (remote.value?.step?.window === 'viewer') send('click') }
  const rearm = () => { /* a parked task is the main window's; its bubble offers this */ }

  onScopeDispose(() => {
    window.removeEventListener('storage', onStorage)
    if (poll) clearInterval(poll)
  })

  return { active, index, total, phase, gateSatisfied, currentStep, step, offRoute, needsReveal,
           awaitedTask, failureNote, domTick, next, back, exit, satisfyClick, rearm }
})

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useGuideMirrorStore, import.meta.hot))
