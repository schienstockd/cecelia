// Kiwi profile API — the frontend half of LOGIN_CREDENTIAL_ISOLATION_PLAN P3 + P6.
//
// Routes served by `api/src/kiwi_profile_api.jl`:
//   GET  /api/kiwi/profiles                   → { active, profiles: [{name, dir, isDefault, retired}], legacyReserved }
//   POST /api/kiwi/profiles/select            → { name }               ; writes [ai].profile in custom.toml
//   POST /api/kiwi/profiles/create            → { name }               ; mkpath under user-profiles/
//   POST /api/kiwi/profiles/retire            → { name }               ; writes `.kiwi-retired` sentinel (D11)
//   GET  /api/kiwi/terminal/command?profile=N → { command, profile, profileDir }
//   GET  /api/kiwi/profiles/auth?profile=N    → { profile, state, email, method, org } ; Claude login
//   POST /api/kiwi/profiles/logout            → { name }               ; `claude auth logout` for it
//   POST /api/kiwi/profiles/login             → { name }               ; start `claude auth login` (P9)
//   GET  /api/kiwi/profiles/login?profile=N   → { state, url, browser, message } ; poll it
//   POST /api/kiwi/profiles/login/code        → { name, code }         ; the pasted fallback code
//   POST /api/kiwi/profiles/login/cancel      → { name }
//
// The picker in `KiwiCockpit.vue` reads/writes the active profile through these; the "Open profile
// terminal" button copies the one-liner. Server-side single-active-profile is the model — per-tab
// handoff via `X-Kiwi-Profile` header is deferred (see the plan).
//
// The validator mirrors backend `_valid_kiwi_profile_name` in `api/src/kiwi_profile_api.jl` so the
// dialog can fail fast without a round-trip. Kept in sync by `profileApi.test.ts` — if the
// backend rule changes, both must move together.

/** A profile as the roster surfaces it. `default` maps to `~/.claude*` (dir === ''); named
 *  profiles live under `<config_dir>/user-profiles/<name>/`. A `retired` profile keeps its data
 *  on disk (D11 immutable-name) but is non-selectable — the picker greys it out. */
export interface Profile {
  name: string
  dir: string
  isDefault: boolean
  retired: boolean
}

/** The whole roster payload. `active` is the server-side active profile (a name, never a dir).
 *  `legacyReserved` names must never appear in `profiles` — surfaced so a UI can explain WHY a
 *  user typing `legacy` in the create dialog is rejected. */
export interface ProfileRoster {
  active: string
  profiles: Profile[]
  legacyReserved: string[]
}

/** The terminal one-liner + which profile it targets. On POSIX the shape is
 *  `env -u ANTHROPIC_API_KEY … CLAUDE_CONFIG_DIR=<dir> <shell> -i`; on Windows a PowerShell
 *  `Remove-Item Env:… ; $env:CLAUDE_CONFIG_DIR = '<dir>' ; & '<shell>'` string. The frontend
 *  just copies the string — it never parses it. */
export interface TerminalCommand {
  command: string
  profile: string
  profileDir: string
}

/** Result of POST /api/kiwi/profiles/create. On success carries the newly-minted terminal
 *  one-liner so the create dialog can hand it to the user without a second round-trip. */
export interface CreateProfileResult {
  ok: boolean
  name?: string
  dir?: string
  terminalCommand?: string
  error?: string
}

/** Result of POST /api/kiwi/profiles/select. `active` is the server's new active-profile name. */
export interface SelectProfileResult {
  ok: boolean
  active?: string
  error?: string
}

/** Result of POST /api/kiwi/profiles/retire. `snappedToDefault` is true iff the retired profile
 *  was the currently-active one (server auto-switches so the next spawn doesn't silently keep
 *  using retired credentials). `alreadyRetired` is idempotent-hit sugar. */
export interface RetireProfileResult {
  ok: boolean
  name?: string
  active?: string
  snappedToDefault?: boolean
  alreadyRetired?: boolean
  error?: string
}

/** Display alias for the magic `default` profile — a gender-neutral pet name in the spirit of the
 *  R version's `random.names.animals` list. Internally the profile is still `default` (maps to
 *  `~/.claude`); this is a cosmetic-only relabel wherever the UI would otherwise say "default". */
export const DEFAULT_PROFILE_DISPLAY_NAME = 'peanut'

/** How a profile should read in the UI. `default` → `DEFAULT_PROFILE_DISPLAY_NAME`; anything else
 *  is its own name. Keep the on-disk / API name unchanged — this is display only. */
export function profileDisplayName(p: Pick<Profile, 'name' | 'isDefault'>): string {
  return p.isDefault ? DEFAULT_PROFILE_DISPLAY_NAME : p.name
}

/** `profileDisplayName` for a bare profile name off the wire (project owners, `createdBy`, …). */
export const profileNameLabel = (name: string): string =>
  profileDisplayName({ name, isDefault: name === 'default' })

const _JSON = { 'Content-Type': 'application/json' }

async function _json(res: Response): Promise<any> {
  try { return await res.json() } catch { return {} }
}

/** Fetch the full roster. Any network failure ⇒ empty roster with `default` active — the same
 *  shape the backend returns on a fresh install, so the picker can render either way. */
export async function fetchProfiles(apiBase = ''): Promise<ProfileRoster> {
  try {
    const res = await fetch(`${apiBase}/api/kiwi/profiles`)
    if (!res.ok) throw new Error(`HTTP ${res.status}`)
    return await _json(res) as ProfileRoster
  } catch {
    return { active: 'default',
             profiles: [{ name: 'default', dir: '', isDefault: true, retired: false }],
             legacyReserved: ['legacy'] }
  }
}

/** Switch the server-active profile. Returns `{ok:false, error}` on rejection so the caller
 *  can surface a message; never throws for a non-2xx (the picker needs to reset its select). */
export async function selectProfile(name: string, apiBase = ''): Promise<SelectProfileResult> {
  try {
    const res = await fetch(`${apiBase}/api/kiwi/profiles/select`,
      { method: 'POST', headers: _JSON, body: JSON.stringify({ name }) })
    const body = await _json(res) as Partial<SelectProfileResult>
    if (!res.ok) return { ok: false, error: body.error ?? `HTTP ${res.status}` }
    return { ok: true, active: body.active }
  } catch (e) {
    return { ok: false, error: e instanceof Error ? e.message : 'Network error' }
  }
}

/** Retire a profile (D11). Server writes a `.kiwi-retired` marker in the profile's dir so the
 *  picker greys it out and select rejects it; if the retired profile was active, the server
 *  auto-snaps active to `default` (see `snappedToDefault`). Idempotent — a re-retire returns
 *  200 with `alreadyRetired: true`. */
export interface RenameProfileResult {
  ok: boolean
  oldName?: string
  newName?: string
  active?: string
  error?: string
}
export async function renameProfile(oldName: string, newName: string,
                                    apiBase = ''): Promise<RenameProfileResult> {
  try {
    const res = await fetch(`${apiBase}/api/kiwi/profiles/rename`, {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ oldName, newName }),
    })
    const body = await _json(res) as Partial<RenameProfileResult>
    if (!res.ok) return { ok: false, error: body?.error ?? `HTTP ${res.status}` }
    return { ok: true, oldName: body?.oldName, newName: body?.newName, active: body?.active }
  } catch (e) { return { ok: false, error: e instanceof Error ? e.message : String(e) } }
}

export interface DeleteProfileResult {
  ok: boolean
  name?: string
  error?: string
}
export async function deleteProfile(name: string, apiBase = ''): Promise<DeleteProfileResult> {
  try {
    const res = await fetch(`${apiBase}/api/kiwi/profiles/delete`, {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ name }),
    })
    const body = await _json(res) as Partial<DeleteProfileResult>
    if (!res.ok) return { ok: false, error: body?.error ?? `HTTP ${res.status}` }
    return { ok: true, name: body?.name }
  } catch (e) { return { ok: false, error: e instanceof Error ? e.message : String(e) } }
}

export async function retireProfile(name: string, apiBase = ''): Promise<RetireProfileResult> {
  try {
    const res = await fetch(`${apiBase}/api/kiwi/profiles/retire`,
      { method: 'POST', headers: _JSON, body: JSON.stringify({ name }) })
    const body = await _json(res) as Partial<RetireProfileResult>
    if (!res.ok) return { ok: false, error: body.error ?? `HTTP ${res.status}` }
    return { ok: true, name: body.name, active: body.active,
             snappedToDefault: body.snappedToDefault, alreadyRetired: body.alreadyRetired }
  } catch (e) {
    return { ok: false, error: e instanceof Error ? e.message : 'Network error' }
  }
}

/** Create a new profile directory. Success surfaces the terminal one-liner so the caller can
 *  render it immediately — no second GET needed. */
export async function createProfile(name: string, apiBase = ''): Promise<CreateProfileResult> {
  try {
    const res = await fetch(`${apiBase}/api/kiwi/profiles/create`,
      { method: 'POST', headers: _JSON, body: JSON.stringify({ name }) })
    const body = await _json(res) as Partial<CreateProfileResult>
    if (!res.ok) return { ok: false, error: body.error ?? `HTTP ${res.status}` }
    return { ok: true, name: body.name, dir: body.dir, terminalCommand: body.terminalCommand }
  } catch (e) {
    return { ok: false, error: e instanceof Error ? e.message : 'Network error' }
  }
}

/** Fetch the terminal one-liner. Omit `profile` to get the active one; pass a name to preview
 *  a specific profile's command (used by the create-dialog's Retry-copy fallback). */
export async function fetchTerminalCommand(profile?: string, apiBase = ''):
  Promise<TerminalCommand | null> {
  try {
    const qs = profile ? `?profile=${encodeURIComponent(profile)}` : ''
    const res = await fetch(`${apiBase}/api/kiwi/terminal/command${qs}`)
    if (!res.ok) return null
    return await _json(res) as TerminalCommand
  } catch { return null }
}

/** Client-side mirror of the backend rule (`_valid_kiwi_profile_name` in
 *  `api/src/kiwi_profile_api.jl`). Same character class + length + reserved-name check, so the
 *  create dialog can show a live-validity hint. If this diverges from the backend, the round-trip
 *  will silently reject a name the dialog accepted — kept green by `profileApi.test.ts`. */
export function isValidProfileName(name: string,
                                       reserved: readonly string[] = ['legacy']): boolean {
  if (name.length < 1 || name.length > 32) return false
  if (name === 'default') return false                          // magic name — reserved
  // Also reserve the default's display alias so the picker never shows two identical rows.
  if (name === DEFAULT_PROFILE_DISPLAY_NAME) return false
  for (const r of reserved) if (name === r) return false
  for (const c of name) {
    const ok = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c === '-' || c === '_'
    if (!ok) return false
  }
  return true
}

/** A profile's Claude login, as `claude auth status` reports it. `unknown` = the CLI is missing or
 *  failed — never shown as "not logged in", which would invite a pointless re-login. */
export type ClaudeLoginState = 'loggedIn' | 'loggedOut' | 'unknown'
export interface ClaudeLogin {
  profile: string
  state: ClaudeLoginState
  email: string
  method: string
  org: string
}

/** Who a profile is logged in to Claude as. Omit `profile` for the active one; `fresh` skips the
 *  server's few-second cache (an explicit refresh). Any failure ⇒ `unknown`. */
export async function fetchClaudeLogin(profile?: string, fresh = false,
                                       apiBase = ''): Promise<ClaudeLogin> {
  const unknown: ClaudeLogin = { profile: profile ?? '', state: 'unknown', email: '', method: '', org: '' }
  try {
    const qs = new URLSearchParams()
    if (profile) qs.set('profile', profile)
    if (fresh) qs.set('fresh', '1')
    const res = await fetch(`${apiBase}/api/kiwi/profiles/auth?${qs}`)
    if (!res.ok) return unknown
    return { ...unknown, ...(await _json(res)) } as ClaudeLogin
  } catch { return unknown }
}

/** Log a profile out of Claude (`claude auth logout` in its own config dir). */
export async function logoutClaude(name: string, apiBase = ''): Promise<{ ok: boolean; error?: string }> {
  try {
    const res = await fetch(`${apiBase}/api/kiwi/profiles/logout`,
      { method: 'POST', headers: _JSON, body: JSON.stringify({ name }) })
    const body = await _json(res)
    return res.ok ? { ok: true } : { ok: false, error: body?.error ?? `HTTP ${res.status}` }
  } catch (e) {
    return { ok: false, error: e instanceof Error ? e.message : 'Network error' }
  }
}

/** One-line label for a login: the account email, "Not logged in", or `''` when unknown (render
 *  nothing rather than a guess). */
export function claudeLoginLabel(l: ClaudeLogin | null | undefined): string {
  if (!l || l.state === 'unknown') return ''
  if (l.state === 'loggedOut') return 'Not logged in'
  return l.email || 'Logged in'
}

/** A login Cecelia is running for a profile (`claude auth login`, LOGIN_CREDENTIAL_ISOLATION_PLAN
 *  P9). `url` is the paste-code fallback link (empty until the CLI prints it); `browser` names the
 *  private window it opened (empty = none, the link is the way); `message` is the CLI's verdict. */
export type ClaudeLoginRunState = 'none' | 'waiting' | 'loggedIn' | 'failed'
export interface ClaudeLoginRun {
  state: ClaudeLoginRunState
  url: string
  browser: string
  message: string
  error?: string
}

const _noRun = (error?: string): ClaudeLoginRun =>
  ({ state: 'failed', url: '', browser: '', message: error ?? '', error })

async function _loginPost(path: string, body: object, apiBase: string): Promise<{ ok: boolean; body: any }> {
  try {
    const res = await fetch(`${apiBase}/api/kiwi/profiles/login${path}`,
      { method: 'POST', headers: _JSON, body: JSON.stringify(body) })
    const b = await _json(res)
    return { ok: res.ok, body: res.ok ? b : { ...b, error: b?.error ?? `HTTP ${res.status}` } }
  } catch (e) {
    return { ok: false, body: { error: e instanceof Error ? e.message : 'Network error' } }
  }
}

/** Start logging `name` in to Claude. Returns the run's first status (or a failed one). */
export async function startClaudeLogin(name: string, apiBase = ''): Promise<ClaudeLoginRun> {
  const r = await _loginPost('', { name }, apiBase)
  return r.ok ? { ...(r.body as ClaudeLoginRun) } : _noRun(r.body.error)
}

/** Poll a running login. A failed request is reported as `none` with the error — the caller keeps
 *  polling rather than declaring the login failed on one dropped request. */
export async function fetchClaudeLoginRun(name: string, apiBase = ''): Promise<ClaudeLoginRun> {
  try {
    const res = await fetch(`${apiBase}/api/kiwi/profiles/login?profile=${encodeURIComponent(name)}`)
    const b = await _json(res)
    if (!res.ok) return { state: 'none', url: '', browser: '', message: '', error: b?.error ?? `HTTP ${res.status}` }
    return b as ClaudeLoginRun
  } catch (e) {
    return { state: 'none', url: '', browser: '', message: '', error: e instanceof Error ? e.message : 'Network error' }
  }
}

/** Hand the code from the sign-in page to the waiting login. */
export async function sendClaudeLoginCode(name: string, code: string, apiBase = ''):
  Promise<{ ok: boolean; error?: string }> {
  const r = await _loginPost('/code', { name, code }, apiBase)
  return r.ok ? { ok: true } : { ok: false, error: r.body.error }
}

/** Stop a waiting login (closing the dialog). Best-effort. */
export async function cancelClaudeLogin(name: string, apiBase = ''): Promise<void> {
  await _loginPost('/cancel', { name }, apiBase)
}

/** The one line the login dialog shows while it waits: where to sign in. */
export function claudeLoginPrompt(run: ClaudeLoginRun | null): string {
  if (!run || run.state === 'none') return 'Starting…'
  if (run.state === 'waiting') {
    if (run.browser) return `Sign in in the private ${run.browser} window`
    return run.url ? 'Open the link in a private window and sign in' : 'Starting…'
  }
  if (run.state === 'loggedIn') return 'Logged in'
  return run.message || run.error || 'Login did not finish'
}
