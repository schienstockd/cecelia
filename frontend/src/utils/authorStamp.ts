/** Who made a project write — the server's `author_stamp()` (app/src/config/profile_settings.jl):
 *  the active profile, and whether it came from the app or from Claude. Absent on content written
 *  before stamps existed. */
export interface AuthorStamp {
  profile: string
  via: 'app' | 'claude'
}

export function parseAuthorStamp(raw: unknown): AuthorStamp | undefined {
  if (!raw || typeof raw !== 'object') return undefined
  const r = raw as Record<string, unknown>
  const profile = typeof r.profile === 'string' ? r.profile : ''
  if (!profile) return undefined
  return { profile, via: r.via === 'claude' ? 'claude' : 'app' }
}

/** "alice", "Claude", "Claude for alice" — or '' for the default profile in the app, so a
 *  one-person install never shows a name. */
export function authorLabel(s?: AuthorStamp): string {
  if (!s) return ''
  const person = s.profile === 'default' ? '' : s.profile
  if (s.via === 'claude') return person ? `Claude for ${person}` : 'Claude'
  return person
}
