// Kiwi profile API — the frontend half of LOGIN_CREDENTIAL_ISOLATION_PLAN P3 + P6.
//
// Routes served by `api/src/kiwi_profile_api.jl`:
//   GET  /api/kiwi/profiles                   → { active, profiles: [{name, dir, isDefault, retired}], legacyReserved }
//   POST /api/kiwi/profiles/select            → { name }               ; writes [ai].profile in custom.toml
//   POST /api/kiwi/profiles/create            → { name }               ; mkpath under kiwi-profiles/
//   POST /api/kiwi/profiles/retire            → { name }               ; writes `.kiwi-retired` sentinel (D11)
//   GET  /api/kiwi/terminal/command?profile=N → { command, profile, profileDir }
//
// The picker in `KiwiCockpit.vue` reads/writes the active profile through these; the "Open profile
// terminal" button copies the one-liner. Server-side single-active-profile is the model — per-tab
// handoff via `X-Kiwi-Profile` header is deferred (see the plan).
//
// The validator mirrors backend `_valid_kiwi_profile_name` in `api/src/kiwi_profile_api.jl` so the
// dialog can fail fast without a round-trip. Kept in sync by `profileApi.test.ts` — if the
// backend rule changes, both must move together.

/** A profile as the roster surfaces it. `default` maps to `~/.claude*` (dir === ''); named
 *  profiles live under `<config_dir>/kiwi-profiles/<name>/`. A `retired` profile keeps its data
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
