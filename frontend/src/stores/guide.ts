// The guide runtime: which guide is running, which step, and what that step is waiting for.
// Content lives in `lib/guides/`; the bubble is `components/GuideBubble.vue`; the picker is
// `components/GuidesDialog.vue`. Design + decisions: docs/todo/GUIDE_SYSTEM_PLAN.md.
//
// This store READS other stores and never writes to them (plan D1) — the guide points and observes.
// The one thing it persists is which guides you've finished (`cc.guide.<id>.done`, the HintCallout
// idiom), so the picker can tick them off.
//
// Why there is a poll: most gates are predicates over store state and update reactively, but some ask
// about a control that reports to no store (`TaskRunner`'s function `<select>`) or about whether an
// element is on screen yet — both DOM reads, which Vue cannot track. One ~250ms interval, alive only
// while a guide is open, re-evaluates everything. Cheaper than teaching six components to publish
// their local state, and it means a new gate never needs a component change.
//
// The route comes from `location.hash` rather than `useRoute()`: the app uses hash history, so the
// hash IS the route, and a store that reads it needs no router injection context (no other store
// touches vue-router — this one shouldn't be the first to require it).

import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref, computed, watch, nextTick, onScopeDispose } from 'vue'
import { useProjectStore } from './project'
import { useProjectMetaStore } from './projectMeta'
import { useSettingsStore } from './settings'
import { useTaskStore, type TaskEntry } from './tasks'
import { useCustomModulesStore } from './customModules'
import { useViewProfilesStore } from './viewProfiles'
import { allNavGroups, navLabelFor } from '../lib/navGroups'
import { applyProfile, availablePaths, hiddenGuideRoutes } from '../utils/viewProfiles'
import { GUIDES, guideById } from '../lib/guides'
import type { GuideCtx, GuideDef, GuideStep, Prereq, Reveal } from '../lib/guides/types'
import { readAnchorValue, resolveAnchor, isReachable, routePathFromHash } from '../utils/guideAnchor'

const doneKey = (id: string) => `cc.guide.${id}.done`
const POLL_MS = 250
// How long a satisfied gate stays on screen before advancing, so the user sees their action
// acknowledged instead of the bubble vanishing out from under the cursor.
const ADVANCE_DELAY_MS = 450

export type GuidePhase = 'step' | 'waiting' | 'failed' | 'done'

export const useGuideStore = defineStore('guide', () => {
  const project = useProjectStore()
  const meta = useProjectMetaStore()
  const settings = useSettingsStore()
  const tasks = useTaskStore()

  const active = ref<GuideDef | null>(null)
  const index = ref(0)
  const phase = ref<GuidePhase>('step')
  // The task an `awaitTask` step is parked on, and why it stopped if it didn't finish.
  const awaitedTaskId = ref<string | null>(null)
  const failureNote = ref('')
  const completed = ref<Set<string>>(new Set())
  // Bumped by the poll; anything DOM-derived reads this so it re-evaluates. See the header comment.
  const domTick = ref(0)

  // The route, re-read from the hash. It MUST be polled rather than only listened for: vue-router
  // navigates a hash history with `history.pushState`, which does **not** fire `hashchange` — so a
  // listener-only version sat at whatever the path was when this store was constructed (`/`, before
  // the boot redirect resolved) and every step declaring a `route` read as off-route forever.
  // `hashchange`/`popstate` still cover a manual URL edit and back/forward, which the poll would only
  // catch up with on its next tick.
  const currentPath = ref(routePathFromHash(window.location.hash))
  const syncPath = () => { currentPath.value = routePathFromHash(window.location.hash) }
  window.addEventListener('hashchange', syncPath)
  window.addEventListener('popstate', syncPath)

  // ── completion memory ────────────────────────────────────────────────────────────────────────
  function loadCompleted() {
    const s = new Set<string>()
    for (const g of GUIDES) {
      try { if (localStorage.getItem(doneKey(g.id)) === '1') s.add(g.id) } catch { /* private mode */ }
    }
    completed.value = s
  }
  loadCompleted()

  function markDone(id: string) {
    completed.value = new Set(completed.value).add(id)
    try { localStorage.setItem(doneKey(id), '1') } catch { /* private mode — this session only */ }
  }
  function clearCompleted() {
    for (const g of GUIDES) { try { localStorage.removeItem(doneKey(g.id)) } catch { /* ignore */ } }
    completed.value = new Set()
  }

  // ── the snapshot every predicate sees ────────────────────────────────────────────────────────
  const ctx = computed<GuideCtx>(() => {
    void domTick.value                        // the DOM reads below must re-run on the poll
    const set = project.activeSet()
    return {
      route: currentPath.value,
      hasProject: meta.current !== null,
      setUid: set?.uid ?? null,
      setCount: project.sets.length,
      images: set?.images ?? [],
      napariImageUid: project.napariImageUid,
      selection: (module: string) => (set ? project.getImageSelection(module, set.uid) : []),
      rightPanelCollapsed: settings.rightPanelCollapsed,
      viewerPanelOpen: settings.viewerPanelOpen,
      anchorValue: readAnchorValue,
      anchorExists: (id: string) => resolveAnchor(id) !== null,
      anchorReachable: (id: string) => isReachable(resolveAnchor(id)),
    }
  })

  // ── the current step, and the reveal bubble that may stand in front of it ────────────────────
  const currentStep = computed<GuideStep | null>(() => active.value?.steps[index.value] ?? null)
  const total = computed(() => active.value?.steps.length ?? 0)

  // Is the user on the page this step is about? The guide never navigates (D1), so when they wander
  // off we point back at the sidebar item rather than yanking them there.
  const offRoute = computed(() => {
    const s = currentStep.value
    return !!s?.route && currentPath.value !== s.route
  })

  // Which of the step's declared reveal causes applies, if any. First match wins; a step with none
  // matching but whose anchor is present-yet-hidden falls back to the LAST declared cause, so an
  // unforeseen way of hiding a control still produces advice rather than a bubble pointing at nothing.
  const activeReveal = computed<Reveal | null>(() => {
    const s = currentStep.value
    if (!s?.reveal || offRoute.value) return null
    void domTick.value
    const causes = Array.isArray(s.reveal) ? s.reveal : [s.reveal]
    for (const r of causes) if (r.needed(ctx.value)) return r
    const el = resolveAnchor(s.anchor)
    if (el && !isReachable(el)) return causes[causes.length - 1] ?? null
    return null
  })
  const needsReveal = computed(() => activeReveal.value !== null)

  // What the bubble actually renders: the reveal stand-in, or the step itself.
  const step = computed<GuideStep | null>(() => {
    const s = currentStep.value
    if (!s) return null
    const r = activeReveal.value
    if (r) {
      return {
        anchor: r.anchor ?? s.anchor,
        text: r.text,
        placement: r.placement ?? s.placement,
        route: s.route,
      }
    }
    return s
  })

  // ── gates ───────────────────────────────────────────────────────────────────────────────────
  // A click gate is satisfied imperatively — the bubble owns the listener, since it has the element.
  const clickSatisfied = ref(false)
  function satisfyClick() { if (currentStep.value?.clickAnchor) clickSatisfied.value = true }

  const gateSatisfied = computed(() => {
    const s = currentStep.value
    if (!s || needsReveal.value || offRoute.value) return false
    if (s.awaitTask) return false                        // the park watcher owns these, not this
    if (s.when && !s.when(ctx.value)) return false
    if (s.clickAnchor && !clickSatisfied.value) return false
    return !!(s.when || s.clickAnchor)                   // no gate ⇒ nothing to satisfy; Next only
  })

  // ── progression ─────────────────────────────────────────────────────────────────────────────
  let advanceTimer: ReturnType<typeof setTimeout> | null = null
  function cancelAdvance() { if (advanceTimer) { clearTimeout(advanceTimer); advanceTimer = null } }

  function resetStepState() {
    cancelAdvance()
    clickSatisfied.value = false
    awaitedTaskId.value = null
    failureNote.value = ''
    phase.value = 'step'
  }

  function start(id: string) {
    const g = guideById(id)
    if (!g) return
    syncPath()                                 // never start a guide against a stale route
    active.value = g
    index.value = 0
    resetStepState()
  }

  function exit() {
    // No resurrection: a half-finished tour reappearing days later is worse than none (plan D3).
    cancelAdvance()
    active.value = null
    index.value = 0
    resetStepState()
  }

  function next() {
    if (!active.value) return
    if (index.value >= total.value - 1) { finish(); return }
    index.value++
    resetStepState()
  }

  function back() {
    if (!active.value || index.value === 0) return
    index.value--
    resetStepState()
  }

  function finish() {
    if (active.value) markDone(active.value.id)
    cancelAdvance()
    // Drop any parked task before switching phase: a last step with `awaitTask` would otherwise stay
    // subscribed, and a later status change on that run would re-enter next() behind the done card.
    awaitedTaskId.value = null
    failureNote.value = ''
    phase.value = 'done'
  }

  // Auto-advance fires only when the gate becomes satisfied WHILE THIS STEP IS SHOWING. A step whose
  // gate was already satisfied when you arrived shows a tick and waits for Next instead.
  //
  // `armed` is per step, and it has to be: watching for a false→true transition on `gateSatisfied`
  // alone compares the NEW step's gate against the OLD step's, which is meaningless across a step
  // change — pressing Next onto an already-satisfied step read as "just satisfied" and advanced again,
  // so the guide walked itself through every step the user had already done. Arm on entry only when
  // the gate starts out unsatisfied.
  const armed = ref(false)
  watch([() => active.value?.id, () => index.value], () => {
    armed.value = false
    const at = index.value
    // one tick, so a step's anchors/derived state have settled before we judge "already satisfied"
    nextTick(() => { if (index.value === at) armed.value = !gateSatisfied.value })
  }, { immediate: true, flush: 'post' })

  watch(gateSatisfied, ok => {
    cancelAdvance()
    if (ok && armed.value) advanceTimer = setTimeout(() => { advanceTimer = null; next() }, ADVANCE_DELAY_MS)
  }, { flush: 'post' })

  // ── parking on a long task (plan D3) ────────────────────────────────────────────────────────
  // On entering an `awaitTask` step, adopt the NEWEST matching task (the user has usually just
  // pressed Run, so it already exists); if none does yet, adopt the first that shows up.
  function matchingTasks(): TaskEntry[] {
    const a = currentStep.value?.awaitTask
    if (!a) return []
    return tasks.tasks.filter(t =>
      (!a.fun || t.funName === a.fun) && (!a.module || t.module === a.module))
  }

  const awaitedTask = computed(() =>
    awaitedTaskId.value ? tasks.tasks.find(t => t.id === awaitedTaskId.value) ?? null : null)

  watch([currentStep, () => tasks.tasks.length], () => {
    if (!currentStep.value?.awaitTask) return
    if (!awaitedTaskId.value) {
      const newest = matchingTasks().reduce<TaskEntry | null>(
        (best, t) => (!best || t.seq > best.seq ? t : best), null)
      if (newest) awaitedTaskId.value = newest.id
    }
    if (phase.value === 'step' && awaitedTaskId.value) phase.value = 'waiting'
  }, { immediate: true })

  watch(() => awaitedTask.value?.status, st => {
    if (!currentStep.value?.awaitTask || !st) return
    if (st === 'done') { phase.value = 'step'; next() }
    else if (st === 'failed' || st === 'cancelled') {
      phase.value = 'failed'
      failureNote.value = st === 'cancelled' ? 'That run was cancelled.' : 'That run failed.'
    }
  })

  // Retry after a failure: drop the dead task and wait for the next matching one.
  function rearm() {
    awaitedTaskId.value = null
    failureNote.value = ''
    phase.value = 'step'
  }

  // ── the poll ────────────────────────────────────────────────────────────────────────────────
  let poll: ReturnType<typeof setInterval> | null = null
  watch(active, g => {
    if (g && !poll) poll = setInterval(() => { syncPath(); domTick.value++ }, POLL_MS)
    else if (!g && poll) { clearInterval(poll); poll = null }
  })
  onScopeDispose(() => {
    if (poll) clearInterval(poll)
    cancelAdvance()
    window.removeEventListener('hashchange', syncPath)
    window.removeEventListener('popstate', syncPath)
  })

  // ── prerequisites (plan D6) ─────────────────────────────────────────────────────────────────
  // Evaluated for the picker. A miss is a WARNING plus a better suggestion, never a blocked Start —
  // the user may well know something we can't see.
  //
  // One prereq is DERIVED rather than declared: a guide walks specific pages (`GuideStep.route`), and
  // a view profile can hide some of them — so a guide would keep nudging towards a page the user's
  // menu doesn't show. It is computed from the guide's own steps instead of being listed on each of
  // the 16 guides, so a new guide is covered without remembering anything (and a guide tied to no
  // page is never flagged). Same treatment as any other miss: an amber line, counted in "N missing",
  // Start still works — the page is still reachable, this is decluttering not access control.
  // See docs/todo/VIEW_PROFILES_PLAN.md.
  const profileNavGroups = computed(() => allNavGroups(useCustomModulesStore().categories))
  // The menu's own paths, before and after the profile. Both, because only the difference between
  // them is a hidden page: a route that is in NEITHER is chrome the sidebar reaches some other way
  // (`/settings` from the footer, `/` as the welcome route) and no profile can hide it.
  const profileCuratablePaths = computed(() => availablePaths(profileNavGroups.value))
  const profileVisiblePaths = computed(() =>
    availablePaths(applyProfile(profileNavGroups.value, useViewProfilesStore().activeItems)))

  const hiddenPagesOf = (g: GuideDef) =>
    hiddenGuideRoutes(g.steps, profileVisiblePaths.value, profileCuratablePaths.value)

  function profilePrereq(g: GuideDef): (Prereq & { met: boolean }) | null {
    const hidden = hiddenPagesOf(g)
    if (!hidden.length) return null
    const names = hidden.map(p => navLabelFor(profileNavGroups.value, p)).join(', ')
    return { id: 'profileVisible', label: `pages your view profile hides (${names})`,
             ok: () => false, met: false }
  }

  const prereqState = (g: GuideDef) => {
    const base = g.prereqs.map(p => ({ ...p, met: p.ok(ctx.value) }))
    const hidden = profilePrereq(g)
    return hidden ? [...base, hidden] : base
  }
  const prereqsMet = (g: GuideDef) =>
    g.prereqs.every(p => p.ok(ctx.value)) && !hiddenPagesOf(g).length

  return {
    // state
    active, index, phase, total, step, currentStep, ctx, completed, currentPath,
    offRoute, needsReveal, gateSatisfied, awaitedTask, failureNote, domTick,
    // actions
    start, exit, next, back, finish, satisfyClick, rearm, markDone, clearCompleted, loadCompleted,
    // picker
    prereqState, prereqsMet,
  }
})

// Replace the live instance on hot-reload — see the note in `stores/customModules.ts`.
if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useGuideStore, import.meta.hot))
