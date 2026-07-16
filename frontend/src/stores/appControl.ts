import { defineStore } from 'pinia'
import { ref, computed } from 'vue'

// App-level lifecycle actions (global Quit + dev backend Restart), shared by BOTH the Settings → System
// panel and the sidebar footer so the shutdown/restart logic lives in ONE place (no divergent
// re-implementation). Per-service (napari/notebooks) controls stay local to the Settings panel.
export const useAppControlStore = defineStore('appControl', () => {
  const dev = ref(false)        // dev server → the backend Restart control is offered (prod hides it)
  const busy = ref(false)       // a quit/restart is in flight (drives spinners in both places)
  const message = ref('')
  // first-launch: no custom.toml / projects dir unset. null = not yet known (don't redirect until we
  // know). The boot guard in main.ts sends the user to /setup while true. See docs/todo/ONBOARDING_PLAN.md.
  const setupRequired = ref<boolean | null>(null)

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
    return setupRequired.value === true
  }

  // wizard finished (POST /api/setup/init succeeded): clear the flag so the guard stops redirecting.
  function completeSetup() { setupRequired.value = false }

  // ── Software updates (single source; consumed by Settings → Software AND the header badge) ──
  // The check/apply/staging backend + Settings UI already exist; this store centralises the STATE so
  // the header badge and the Settings panel never re-implement the fetch. See docs/todo/ONBOARDING_PLAN.md D5.
  const updateCurrent   = ref('')
  const updateLatest    = ref<string | null>(null)
  const updateAvailable = ref(false)
  const updateScope     = ref<'user' | 'system' | 'dev' | ''>('')  // system → admin-only; dev → no apply
  const updateChecking  = ref(false)
  const updateBusy      = ref(false)
  const updateMsg       = ref('')
  const updateDismissed = ref(false)                               // header badge "remind me later" (session)
  // in-app apply is only offered for a per-user install (not a shared system install or dev checkout)
  const canApplyUpdate  = computed(() => updateScope.value === 'user')

  async function checkUpdate() {
    updateChecking.value = true; updateMsg.value = ''
    try {
      const d = await (await fetch('/api/update/check')).json()
      updateCurrent.value   = d.current ?? ''
      updateLatest.value    = d.latest ?? null
      updateAvailable.value = !!d.updateAvailable
      updateScope.value     = d.scope ?? ''
      if (d.error) updateMsg.value = d.error
    } catch { updateMsg.value = 'Could not reach the update server.' }
    finally { updateChecking.value = false }
  }

  async function applyUpdate() {
    if (!updateLatest.value || updateBusy.value) return
    updateBusy.value = true; updateMsg.value = ''
    try {
      const res = await _post('/api/update/apply', { version: updateLatest.value })
      const d = await res.json().catch(() => ({} as { message?: string; error?: string }))
      updateMsg.value = res.ok ? (d.message ?? `Update ${updateLatest.value} staged — restart Cecelia to finish.`)
                               : (d.error ?? 'Update failed.')
      if (res.ok) updateAvailable.value = false
    } catch { updateMsg.value = 'Update failed (could not reach the server).' }
    finally { updateBusy.value = false }
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

  return { dev, busy, message, setupRequired, worktrees, canSwitch,
           updateCurrent, updateLatest, updateAvailable, updateScope, updateChecking, updateBusy,
           updateMsg, updateDismissed, canApplyUpdate, checkUpdate, applyUpdate, dismissUpdate,
           refreshDev, refreshStartup, completeSetup, refreshWorktrees, quit, restartBackend, switchWorktree }
})
