// Short client-side ids — a per-session correlation handle (module task runs, and any future
// client-minted id). Mirrors the backend's `gen_uid` shape (app/src/utils.jl: 6 base62 chars) so an
// id reads the same everywhere — frontend task list, WS messages, backend logs, the task console —
// instead of a 36-char UUID that only ever showed truncated. ~57 billion values (62^6); collision-free
// within a session. NOT security-sensitive — purely a display/correlation handle.
export const ID_CHARS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
export const ID_LENGTH = 6

export function shortId(n: number = ID_LENGTH): string {
  const bytes = crypto.getRandomValues(new Uint8Array(n))
  let s = ''
  for (let i = 0; i < n; i++) s += ID_CHARS[bytes[i] % ID_CHARS.length]
  return s
}
