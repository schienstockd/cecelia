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
