import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref, computed } from 'vue'
import { fetchProfiles, DEFAULT_PROFILE_DISPLAY_NAME } from '../utils/profileApi'

// App-level lifecycle actions (global Quit + dev backend Restart), shared by BOTH the Settings → System
// panel and the sidebar footer so the shutdown/restart logic lives in ONE place (no divergent
// re-implementation). Per-service (viewer/notebooks) controls stay local to the Settings panel.
export const useAppControlStore = defineStore('appControl', () => {
  const dev = ref(false)        // dev server → the backend Restart control is offered (prod hides it)
  const busy = ref(false)       // a quit/restart is in flight (drives spinners in both places)
  const message = ref('')
  // first-launch: no custom.toml / projects dir unset. null = not yet known (don't redirect until we
  // know). The boot guard in main.ts sends the user to /setup while true. See docs/todo/ONBOARDING_PLAN.md.
  const setupRequired = ref<boolean | null>(null)
  // Launch-time profile picker (docs/todo/USER_PROFILE_PLAN.md Phase 2). Populated by refreshStartup:
  // TRUE when more than one profile exists AND the user hasn't picked in THIS window yet, so the boot
  // guard redirects to /profile-picker. `null` while the fetch is in flight — same "don't redirect
  // until we know" discipline as setupRequired. FALSE for a single-profile install (auto-skipped) OR
  // after the picker calls completeProfilePick(). Does NOT survive a reload — pick every launch.
  const needsProfilePick = ref<boolean | null>(null)
  // Active profile name — sourced from the same fetchProfiles roundtrip. Drives the header chip
  // (AppHeader) so every window shows who is driving without a per-component fetch. Change-of-
  // profile forces a reload (PreferencesModal.pickProfile), so this is only ever set once per
  // window session — no watch needed.
  const activeProfileName = ref<string>('default')
  // Display alias for the magic `default` profile — same helper the picker uses. Header chip
  // + any UI that reads the active identity should show this, not the raw API name.
  const activeProfileDisplayName = computed(() =>
    activeProfileName.value === 'default' ? DEFAULT_PROFILE_DISPLAY_NAME : activeProfileName.value)
  const profileCount = ref<number>(1)

  const _post = (url: string, body: unknown = {}) =>
    fetch(url, { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) })

  // dev-only: the repo's git worktrees, so the System panel can relaunch the backend from another
  // checkout without the console (backend/:8080 only — a frontend branch still needs its own Vite).
  const worktrees = ref<{ path: string; branch: string; current: boolean; primary?: boolean }[]>([])
  const canSwitch = ref(false)   // server supervised → a switch can actually relaunch

  // read the dev flag from diagnostics (prod never sets CECELIA_DEV → false)
  async function refreshDev() {
    try { dev.value = !!(await (await fetch('/api/diagnostics')).json()).dev } catch { /* leave as-is */ }
  }

  // boot check: learn dev + first-launch state in one diagnostics call. Returns whether setup is
  // required so the router boot guard can redirect. Leaves setupRequired null on failure (the guard
  // then lets the app load normally rather than trapping the user on /setup when the backend blips).
  async function refreshStartup(): Promise<boolean> {
    try {
      const d = await (await fetch('/api/diagnostics')).json()
      dev.value = !!d.dev
      setupRequired.value = !!d.setupRequired
    } catch { /* leave setupRequired as null → don't redirect */ }
    // Profile picker gating (USER_PROFILE_PLAN Phase 2). Only meaningful once the setup wizard is
    // out of the way — a first-launch install has no profiles surface to speak of. On failure leave
    // needsProfilePick false so we never trap the user on the picker over a transient backend blip.
    if (setupRequired.value === false) {
      try {
        const roster = await fetchProfiles()
        activeProfileName.value = roster.active
        profileCount.value      = roster.profiles.length
        // "Just picked" grace window: a switch made through Preferences writes a sessionStorage
        // marker BEFORE forcing a reload; on the next boot within N seconds we treat the picker
        // as already answered so the user doesn't have to reaffirm what they just decided. Cold
        // reloads (new tab, long-dormant session) have no fresh marker → picker fires normally.
        const picked = _readJustPickedMarker()
        if (picked && picked.name === roster.active) {
          needsProfilePick.value = false
          _clearJustPickedMarker()
        } else {
          // Only count *selectable* profiles — a retired sibling can't be picked, so it must not
          // trigger the picker (the picker would render a single clickable row alongside greyed-
          // out retirees, forcing a redundant click). profileCount stays total-including-retired
          // for the project-panel filter (retired profiles can still OWN projects).
          const selectable = roster.profiles.filter(p => !p.retired).length
          needsProfilePick.value = selectable > 1
        }
      } catch { needsProfilePick.value = false }
    } else {
      needsProfilePick.value = false
    }
    return setupRequired.value === true
  }

  // sessionStorage marker consumed by refreshStartup. Session-scoped so a browser restart still
  // sees the picker; short freshness window because a marker outliving its intent (e.g. tab
  // reopened days later) shouldn't suppress the picker under a stale name.
  const _JUST_PICKED_KEY = 'cc.profileJustPicked'
  const _JUST_PICKED_MAX_AGE_MS = 60_000
  function _readJustPickedMarker(): { name: string; at: number } | null {
    if (typeof sessionStorage === 'undefined') return null
    try {
      const raw = sessionStorage.getItem(_JUST_PICKED_KEY)
      if (!raw) return null
      const v = JSON.parse(raw) as { name?: string; at?: number }
      if (!v?.name || typeof v.at !== 'number') return null
      if (Date.now() - v.at > _JUST_PICKED_MAX_AGE_MS) return null
      return { name: v.name, at: v.at }
    } catch { return null }
  }
  function _clearJustPickedMarker() {
    try { sessionStorage.removeItem(_JUST_PICKED_KEY) } catch { /* private mode */ }
  }
  /** Preferences → Profiles calls this right before it reloads the page. Skips the picker on
   *  the next boot as long as the roster's active profile matches (the reload re-hydrates every
   *  store against the new identity, so a mismatch means something raced and we WANT the picker). */
  function markProfileJustPicked(name: string) {
    try {
      sessionStorage.setItem(_JUST_PICKED_KEY, JSON.stringify({ name, at: Date.now() }))
    } catch { /* private mode → picker will re-fire once; acceptable */ }
  }

  // wizard finished (POST /api/setup/init succeeded): clear the flag so the guard stops redirecting.
  function completeSetup() { setupRequired.value = false }

  // The launch-time picker calls this after a successful /select — the guard then lets the app
  // through instead of bouncing back to the picker. Only affects this window; a reload re-arms.
  function completeProfilePick() { needsProfilePick.value = false }

  // ── Software updates (single source; consumed by Settings → Software AND the header badge) ──
  // The check/apply/staging backend + Settings UI already exist; this store centralises the STATE so
  // the header badge and the Settings panel never re-implement the fetch. See docs/todo/ONBOARDING_PLAN.md D5.
  const updateCurrent   = ref('')
  const updateLatest    = ref<string | null>(null)    // display label ("v0.2.0" or "dev@1a2b3c4")
  const updateLatestRef = ref<string | null>(null)    // what apply sends: a tag OR a full sha
  const updateAvailable = ref(false)
  const updateScope     = ref<'user' | 'system' | 'dev' | ''>('')  // system → admin-only; dev → no apply
  const updateChannel   = ref<'stable' | 'dev'>('stable')
  const updateChecking  = ref(false)
  const updateBusy      = ref(false)
  const updateMsg       = ref('')
  const updateDismissed = ref(false)                               // header badge "remind me later" (session)
  const updateHasPrevious = ref(false)                             // Revert button visibility
  const updateRevertBusy  = ref(false)
  // Release-notes surfacing (What's New modal — WHATS_NEW_PLAN.md). The older header badge +
  // Settings panel don't read these; they're only for the modal.
  const updateUrl       = ref('')
  const updateNotes     = ref('')                                  // GitHub release `body` (markdown)
  const updatePublished = ref('')                                  // ISO timestamp; empty if unknown
  // in-app apply is only offered for a per-user install (not a shared system install or dev checkout)
  const canApplyUpdate  = computed(() => updateScope.value === 'user')

  // Channel is a query param, not persisted server-side: the toggle is a client preference, and the
  // running install itself is not branded stable/dev (the SAME server serves either). Reads the
  // `cc.preferDevChannel` localStorage key set by `useSettingsStore` when the caller doesn't pass an
  // override, so App.vue's app-wide check and WhatsNewDialog's badge check both honour the toggle
  // without every caller threading the setting through. See docs/SHIPPING.md → install channels.
  async function checkUpdate(channel?: 'stable' | 'dev') {
    const ch = channel ?? (localStorage.getItem('cc.preferDevChannel') === 'true' ? 'dev' : 'stable')
    updateChecking.value = true; updateMsg.value = ''
    try {
      const d = await (await fetch(`/api/update/check?channel=${ch}`)).json()
      updateCurrent.value     = d.current ?? ''
      updateLatest.value      = d.latest ?? null
      updateLatestRef.value   = d.latestRef ?? d.latest ?? null   // stable: tag IS the ref; dev: full sha
      updateAvailable.value   = !!d.updateAvailable
      updateScope.value       = d.scope ?? ''
      updateChannel.value     = d.channel ?? ch
      updateUrl.value         = d.url ?? ''
      updateNotes.value       = d.releaseNotes ?? ''
      updatePublished.value   = d.publishedAt ?? ''
      updateHasPrevious.value = !!d.hasPrevious
      if (d.error) updateMsg.value = d.error
    } catch { updateMsg.value = 'Could not reach the update server.' }
    finally { updateChecking.value = false }
  }

  async function applyUpdate() {
    if (!updateLatestRef.value || updateBusy.value) return
    updateBusy.value = true; updateMsg.value = ''
    try {
      const res = await _post('/api/update/apply',
        { version: updateLatestRef.value, channel: updateChannel.value })
      const d = await res.json().catch(() => ({} as { message?: string; error?: string }))
      updateMsg.value = res.ok ? (d.message ?? `Update ${updateLatest.value} staged — restart Cecelia to finish.`)
                               : (d.error ?? 'Update failed.')
      if (res.ok) { updateAvailable.value = false; updateHasPrevious.value = true }
    } catch { updateMsg.value = 'Update failed (could not reach the server).' }
    finally { updateBusy.value = false }
  }

  async function revertUpdate() {
    if (!updateHasPrevious.value || updateRevertBusy.value) return
    updateRevertBusy.value = true; updateMsg.value = ''
    try {
      const res = await _post('/api/update/revert')
      const d = await res.json().catch(() => ({} as { message?: string; error?: string }))
      updateMsg.value = res.ok ? (d.message ?? 'Revert staged — restart Cecelia to finish.')
                               : (d.error ?? 'Revert failed.')
      if (res.ok) updateHasPrevious.value = false
    } catch { updateMsg.value = 'Revert failed (could not reach the server).' }
    finally { updateRevertBusy.value = false }
  }

  function dismissUpdate() { updateDismissed.value = true }
  async function refreshWorktrees() {
    try {
      const d = await (await fetch('/api/app/worktrees')).json() as {
        worktrees?: { path: string; branch: string; current: boolean; primary?: boolean }[]; canSwitch?: boolean }
      worktrees.value = d.worktrees ?? []
      canSwitch.value = !!d.canSwitch
    } catch { /* leave as-is */ }
  }

  // global Quit: stop everything + exit the backend. The connection drops as the server exits (expected);
  // stays busy because the app is gone.
  async function quit() {
    busy.value = true; message.value = 'Shutting down…'
    try { await _post('/api/app/shutdown') } catch { /* connection dropped on exit */ }
    message.value = 'Cecelia is shutting down — you can close this window.'
  }

  // dev-only backend restart: the supervisor relaunches; poll /api/health until it's back, then clear.
  // Returns an error string if the server refused (e.g. not supervised), else null.
  async function restartBackend(): Promise<string | null> {
    busy.value = true; message.value = 'Backend restarting…'
    try {
      const res = await _post('/api/app/restart')
      if (!res.ok) {
        const d = await res.json().catch(() => ({} as { error?: string }))
        busy.value = false; message.value = d.error ?? 'Restart failed.'
        return message.value
      }
    } catch { /* connection dropped as it exits — expected */ }
    message.value = 'Backend restarting — reconnecting…'
    await _waitForBackend()
    busy.value = false; message.value = 'Backend restarted.'
    return null
  }

  // wait for the restarted server to answer /api/health again. The initial delay lets the old process
  // actually exit first (it exits ~0.4s after responding) so we don't catch it still up.
  async function _waitForBackend(timeoutMs = 60000) {
    await new Promise(r => setTimeout(r, 1500))
    const start = Date.now()
    while (Date.now() - start < timeoutMs) {
      try { if ((await fetch('/api/health', { cache: 'no-store' })).ok) return } catch { /* not up yet */ }
      await new Promise(r => setTimeout(r, 800))
    }
  }

  // dev-only: relaunch the backend from another worktree. Same lifecycle as restartBackend — the
  // supervisor exits the current server and relaunches in the target checkout; we poll /api/health.
  async function switchWorktree(path: string): Promise<string | null> {
    busy.value = true; message.value = 'Switching worktree…'
    try {
      const res = await _post('/api/app/switch-worktree', { path })
      if (!res.ok) {
        const d = await res.json().catch(() => ({} as { error?: string }))
        busy.value = false; message.value = d.error ?? 'Worktree switch failed.'
        return message.value
      }
    } catch { /* connection dropped as it exits — expected */ }
    message.value = 'Backend switching worktree — reconnecting…'
    await _waitForBackend()
    busy.value = false; message.value = 'Switched worktree.'
    await refreshWorktrees()
    return null
  }

  return { dev, busy, message, setupRequired, needsProfilePick, activeProfileName, activeProfileDisplayName, profileCount, worktrees, canSwitch,
           updateCurrent, updateLatest, updateLatestRef, updateAvailable, updateScope, updateChannel,
           updateChecking, updateBusy, updateMsg, updateDismissed,
           updateUrl, updateNotes, updatePublished, canApplyUpdate,
           updateHasPrevious, updateRevertBusy,
           checkUpdate, applyUpdate, revertUpdate, dismissUpdate,
           refreshDev, refreshStartup, completeSetup, completeProfilePick, markProfileJustPicked,
           refreshWorktrees, quit, restartBackend, switchWorktree }
})

// Replace the live instance on hot-reload — see the note in `stores/customModules.ts`.
if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useAppControlStore, import.meta.hot))
