// Kiwi — the prompt box's draft, the running turn and the feed (KIWI_ASSISTANT_PLAN Phase 4–5).
//
// The DRAFT (refs attached to the next question) lives in localStorage and syncs across windows by
// the `storage` event: the pop-out viewer has its own Pinia, and its "Add to Kiwi" button must land
// in the main window's cockpit. A ref arriving from another window also opens the cockpit there.
// Turns come from `/api/kiwi/turns` and the `kiwi:step` / `kiwi:done` WS events (`api/src/kiwi_api.jl`).
// Pure logic is in `utils/kiwiTurn.ts`.

import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref, computed, watch } from 'vue'
import { useProjectMetaStore } from './projectMeta'
import { useSettingsStore } from './settings'
import { useWsStore } from './ws'
import type { KiwiRef } from '../utils/kiwiRef'
import { draftAdd, draftRemove, parseDraft, upsertTurn, startKiwiTurn, cancelKiwiTurn, fetchKiwiTurns,
         clearKiwiTurns, type KiwiDraft, type KiwiTurn } from '../utils/kiwiTurn'

const DRAFT_KEY = 'cc.kiwiDraft'
const PROMPT_KEY = 'cc.kiwiPrompt'

export const useKiwiStore = defineStore('kiwi', () => {
  const pm = useProjectMetaStore()
  const settings = useSettingsStore()
  const projectUid = computed(() => pm.current?.uid ?? '')

  // ── Draft ──────────────────────────────────────────────────────────────────────────────────────
  const draft = ref<KiwiDraft>(parseDraft(localStorage.getItem(DRAFT_KEY)))
  const prompt = ref(localStorage.getItem(PROMPT_KEY) ?? '')
  const refs = computed<KiwiRef[]>(() => draft.value.projectUid === projectUid.value ? draft.value.refs : [])

  function saveDraft() { localStorage.setItem(DRAFT_KEY, JSON.stringify(draft.value)) }
  watch(prompt, v => localStorage.setItem(PROMPT_KEY, v))

  /** Attach a ref to the next question and open the cockpit. `puid` for a window that has no open
   *  project of its own — the pop-out viewer knows its project only from `?project=`. */
  function addRef(ref: KiwiRef, puid: string = projectUid.value) {
    if (!puid) return
    draft.value = draftAdd(draft.value, puid, ref)
    saveDraft()
    settings.kiwiOpen = true
  }
  function removeRef(ref: KiwiRef) { draft.value = draftRemove(draft.value, ref); saveDraft() }
  function clearDraft() { draft.value = { projectUid: projectUid.value, refs: [] }; saveDraft() }

  if (typeof window !== 'undefined') {
    window.addEventListener('storage', e => {
      if (e.key !== DRAFT_KEY) return
      const next = parseDraft(e.newValue)
      const grew = next.projectUid === projectUid.value && next.refs.length > refs.value.length
      draft.value = next
      if (grew) settings.kiwiOpen = true
    })
  }

  // ── Turns ──────────────────────────────────────────────────────────────────────────────────────
  const turns = ref<KiwiTurn[]>([])
  const running = ref<KiwiTurn | null>(null)
  const error = ref('')
  const busy = computed(() => running.value !== null)

  async function load() {
    error.value = ''
    if (!projectUid.value) { turns.value = []; running.value = null; return }
    const r = await fetchKiwiTurns(projectUid.value)
    turns.value = r.turns
    running.value = r.running
  }
  watch(projectUid, () => { void load() }, { immediate: true })

  async function ask() {
    if (!projectUid.value || busy.value) return
    const text = prompt.value.trim()
    if (!text && refs.value.length === 0) return
    error.value = ''
    try {
      running.value = await startKiwiTurn({ projectUid: projectUid.value, prompt: text, refs: refs.value,
                                            reasoning: settings.kiwiReasoning })
      prompt.value = ''
      clearDraft()
    } catch (e) {
      error.value = e instanceof Error ? e.message : String(e)
    }
  }

  async function cancel() {
    if (running.value) { try { await cancelKiwiTurn(running.value.turnId) } catch { /* done already */ } }
  }

  async function clearFeed() {
    if (!projectUid.value) return
    await clearKiwiTurns(projectUid.value)
    turns.value = []
  }

  // WS: steps stream onto the running turn; `done` moves it into the feed. A `done` sent while the
  // socket was down is lost, so a reconnect reloads — else the box would stay "working" forever.
  const ws = useWsStore()
  watch(() => ws.status, s => { if (s === 'connected') void load() })
  ws.on('kiwi:step', (data: Record<string, unknown>) => {
    if (data.projectUid !== projectUid.value || !running.value || data.turnId !== running.value.turnId) return
    running.value = { ...running.value, steps: [...running.value.steps, String(data.step ?? '')] }
  })
  ws.on('kiwi:done', (data: Record<string, unknown>) => {
    if (data.projectUid !== projectUid.value) return
    const t = data.turn as KiwiTurn | undefined
    if (!t) return
    turns.value = upsertTurn(turns.value, t)
    if (running.value?.turnId === t.turnId) running.value = null
  })

  return { projectUid, prompt, refs, addRef, removeRef, clearDraft, turns, running, busy, error,
           load, ask, cancel, clearFeed }
})

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useKiwiStore, import.meta.hot))
