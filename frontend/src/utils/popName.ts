// Population-name validation — the client-side half of the naming guards enforced server-side in
// `app/src/gating/population_manager.jl` (`is_reserved_pop_name` + `pop_name_conflict`) and
// `api/src/gating_api.jl` (pop add / rename). Two rules:
//  • the "_" prefix is RESERVED for derived populations (tracked / clustering) — a hand-named pop
//    can't use it (mirrors `is_reserved_pop_name`).
//  • a name must be UNIQUE among the populations already known here, so the mixed-type module picker
//    (which resolves a population by its path alone) is never ambiguous. The server also rejects a
//    collision with a DIFFERENT pop type in the segmentation (`pop_name_conflict`, surfaced as a
//    toast); this catches the same-list duplicate immediately as the user types.
// Returns a human-readable message, or null when the name is acceptable.

export const DERIVED_POP_PREFIX = '_'

export function popNameError(
  name: string,
  existingNames: string[],
  opts: { currentName?: string; reservedPrefix?: string } = {},
): string | null {
  const n = name.trim()
  if (!n) return 'Enter a population name.'
  const prefix = opts.reservedPrefix ?? DERIVED_POP_PREFIX
  if (n.startsWith(prefix)) {
    return `Names can't start with “${prefix}” (reserved for tracked / clustering populations).`
  }
  // case-insensitive: "T" and "t" would still collide as pop paths, so treat them as the same name
  const lower = n.toLowerCase()
  const current = opts.currentName?.trim().toLowerCase()
  if (lower === current) return null // renaming to the same name (case aside) is a no-op, not a clash
  if (existingNames.some(e => e.trim().toLowerCase() === lower)) {
    return `“${n}” already exists — choose another name.`
  }
  return null
}

// Join a parent path + leaf name into a pop path — the client twin of Julia `pop_path`
// (`app/src/gating/population_manager.jl`). Root children are `/name`; everything else `parent/name`.
// One implementation, because a hand-written variant that forgets the root case builds `root/name`,
// a path no population in the tree has.
export const popPath = (parent: string, name: string) =>
  !parent || parent === 'root' || parent === '/' ? `/${name}` : `${parent}/${name}`

// Is `path` inside `sub`'s own subtree (or `sub` itself)? The cycle guard a re-parent needs — a
// population cannot be moved under one of its own descendants (Julia `move_pop!` rejects it too).
export const isInSubtree = (path: string, sub: string) => path === sub || path.startsWith(sub + '/')

const segsOf = (path: string) => path.split('/').filter(Boolean)

/**
 * Display labels for populations SHOWN TOGETHER, by path: each pop's own name, unless another pop in
 * the same list shares it — then each clashing pop climbs its path one step at a time until they
 * differ ("/qc/OTI/_tracked" + "/qc/non-OTI/gBT/_tracked" → "OTI/_tracked", "gBT/_tracked"). Every
 * tracked pop is called `_tracked`, so a flat list of leaf names alone is a list of identical rows.
 *
 * `nameOf` overrides a pop's own (leaf) name — a payload's `name` field — when it can differ from the
 * path's last segment; the climbed parents always come from the path. A path listed twice gets one
 * label. The Julia twin is `pop_display_labels` (`app/src/gating/popmanager/types.jl`).
 */
export function popDisplayLabels(
  paths: readonly string[], nameOf?: (path: string) => string | undefined,
): Map<string, string> {
  const uniq = [...new Set(paths)]
  const segs = new Map(uniq.map(p => [p, segsOf(p)]))
  const depth = new Map(uniq.map(p => [p, 1]))
  const label = (p: string) => {
    const s = segs.get(p)!, d = depth.get(p)!
    const own = nameOf?.(p) ?? s[s.length - 1] ?? p
    return s.length <= 1 || d <= 1 ? own : [...s.slice(-d, -1), own].join('/')
  }
  for (;;) {
    const byLabel = new Map<string, string[]>()
    for (const p of uniq) byLabel.set(label(p), [...(byLabel.get(label(p)) ?? []), p])
    let climbed = false
    for (const clash of byLabel.values()) {
      if (clash.length < 2) continue
      for (const p of clash) {
        if (depth.get(p)! < segs.get(p)!.length) { depth.set(p, depth.get(p)! + 1); climbed = true }
      }
    }
    if (!climbed) break
  }
  return new Map(uniq.map(p => [p, label(p)]))
}

/**
 * Swap a display label's own (last) name for `own`, keeping the parents it climbed: for a list whose
 * pops come from several segmentations, where the same path can carry a different name in each.
 */
export const withOwnName = (label: string, own: string | undefined) => {
  if (!own) return label
  const i = label.lastIndexOf('/')
  return i > 0 ? `${label.slice(0, i + 1)}${own}` : own
}

/**
 * Labels for a list of pop ROWS shown together, possibly from several segmentations: each row's own
 * name, its parents climbed where names clash (`popDisplayLabels`), and its segmentation in front
 * only when the same label still appears for two segmentations. Index-aligned with `rows`.
 */
export function popRowLabels(rows: readonly { vn?: string; path: string; name?: string }[]): string[] {
  const climbed = popDisplayLabels(rows.map(r => r.path))
  const base = rows.map(r => withOwnName(climbed.get(r.path) ?? r.path, r.name))
  const vnsOf = new Map<string, Set<string>>()
  base.forEach((b, i) => vnsOf.set(b, (vnsOf.get(b) ?? new Set()).add(rows[i]!.vn ?? '')))
  return base.map((b, i) => (vnsOf.get(b)!.size > 1 && rows[i]!.vn ? `${rows[i]!.vn} ${b}` : b))
}
