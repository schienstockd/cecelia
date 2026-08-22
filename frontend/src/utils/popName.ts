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
    return `A population named “${n}” already exists here — choose another name.`
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
