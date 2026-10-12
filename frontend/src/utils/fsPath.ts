// File-browser path helpers. Extracted from FileBrowser.vue so the breadcrumb logic is unit-tested
// (docs/DEV.md → Tests: pure logic in utils/*). The browser now navigates absolute paths (to reach
// mounted network drives), so breadcrumbs are built from an absolute path, not relative-to-home.

export interface Crumb { label: string; path: string }

/**
 * Breadcrumb trail for an absolute path. Unix `/mnt/data` →
 * `[{/ , /}, {mnt, /mnt}, {data, /mnt/data}]`. Windows `C:/Users/x` (or backslashes) →
 * `[{C:, C:/}, {Users, C:/Users}, {x, C:/Users/x}]`. Empty input → `[]`.
 */
export function fsBreadcrumbs(current: string): Crumb[] {
  if (!current) return []
  const norm = current.replace(/\\/g, '/')
  const isWin = /^[A-Za-z]:/.test(norm)
  const parts = norm.split('/').filter(Boolean)
  const crumbs: Crumb[] = []

  if (isWin) {
    const drive = parts.shift() ?? ''          // "C:"
    crumbs.push({ label: drive, path: `${drive}/` })
    let acc = drive
    for (const p of parts) { acc = `${acc}/${p}`; crumbs.push({ label: p, path: acc }) }
  } else {
    crumbs.push({ label: '/', path: '/' })
    let acc = ''
    for (const p of parts) { acc = `${acc}/${p}`; crumbs.push({ label: p, path: acc }) }
  }
  return crumbs
}

/** What the picker needs of a listing entry to decide whether it may be picked. */
export interface PickableEntry { name: string; isdir: boolean; isimage: boolean }

export type FsPickMode = 'image' | 'dir' | 'bundle' | 'file'

// Case-insensitive: an export is as likely to be `.XML` as `.xml`, and a filter that silently shows
// nothing reads as "the browser is broken", not "wrong case".
function endsWithAny(name: string, exts: string[]): boolean {
  const n = name.toLowerCase()
  return exts.some(x => n.endsWith(x.toLowerCase()))
}

/**
 * Whether the browser lets `e` be picked in `mode` (dir mode picks the current folder, not an entry).
 *
 * `file` mode: a file whose name ends with one of `extensions` (empty = any file), or a DIRECTORY
 * ending with one of `dirExtensions` — a format that is a folder on disk, picked as one file (an
 * OME-Zarr store). Any other folder is navigated into, never picked, or every folder would become
 * selectable. `bundle` mode is the `.ccbundle` case of the same rule, kept as a named mode.
 */
export function isPickable(e: PickableEntry, mode: FsPickMode,
                           extensions: string[] = [], dirExtensions: string[] = []): boolean {
  if (mode === 'image')  return e.isimage
  if (mode === 'bundle') return e.isdir && endsWithAny(e.name, ['.ccbundle'])
  if (mode === 'file') {
    return e.isdir ? dirExtensions.length > 0 && endsWithAny(e.name, dirExtensions)
                   : !extensions.length || endsWithAny(e.name, extensions)
  }
  return false
}
