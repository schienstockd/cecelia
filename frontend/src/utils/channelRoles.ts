/**
 * The value of a `channelRoles` param — one role per channel, `{ channelName: role }`.
 *
 * A map from channel to role cannot say two things about one channel, which separate channel lists
 * can (`docs/todo/AF_CORRECTION_AUDIT.md` → the form). An absent key or `''` means the channel takes
 * no part. Pure, so the widget, the preview gate and the AF figure read it the same way.
 */
export type ChannelRoleMap = Record<string, string>

/** The map, or `{}` for anything that is not one (a fresh form, a stale saved value). */
export function asRoleMap(v: unknown): ChannelRoleMap {
  if (!v || typeof v !== 'object' || Array.isArray(v)) return {}
  const out: ChannelRoleMap = {}
  for (const [k, r] of Object.entries(v as Record<string, unknown>)) {
    if (typeof r === 'string' && r) out[k] = r
  }
  return out
}

/** Channels holding `role`, in `order` when given (the image's channel order), else map order. */
export function channelsWithRole(v: unknown, role: string, order?: string[]): string[] {
  const m = asRoleMap(v)
  const names = order ? order.filter(c => c in m) : Object.keys(m)
  return names.filter(c => m[c] === role)
}

/**
 * The map after setting `channel` to `role` (`''` clears it). Keys for channels the selected images
 * do not have are dropped: the widget only draws the image's channels, so a stale key would be
 * invisible — and the translator refuses a channel name the image lacks.
 */
export function setChannelRole(v: unknown, channel: string, role: string,
                               available: string[]): ChannelRoleMap {
  const m = asRoleMap(v)
  const out: ChannelRoleMap = {}
  for (const c of available) {
    const r = c === channel ? role : m[c]
    if (r) out[c] = r
  }
  return out
}
