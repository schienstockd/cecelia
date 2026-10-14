// The Pluto `/open` URL for a notebook row — or null when the row must not be opened in place.
//
// Pluto saves a notebook back to the file it opened on every edit and run. A shipped example lives in
// the install (`<install>/notebooks/*.jl`), so opening it in place either rewrites the shipped copy
// (owner, dev checkout) or fails to save (non-owner of a shared install). Examples are reached through
// Duplicate instead. See docs/NOTEBOOKS.md → *Using it*.

export interface OpenableNotebook { scope: 'project' | 'example'; path: string }

export function notebookOpenUrl(nb: OpenableNotebook, serverUrl: string, secret: string): string | null {
  if (nb.scope !== 'project') return null
  const s = secret ? `&secret=${encodeURIComponent(secret)}` : ''
  return `${serverUrl}open?path=${encodeURIComponent(nb.path)}${s}`
}
