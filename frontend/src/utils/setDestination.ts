// The "existing set OR a new set by name" destination pattern, resolved in one place.
//
// Two surfaces send an image somewhere: Copy (duplicates data into a set) and Move (manifest-only).
// Both offer a set dropdown plus a "＋ New set…" entry whose empty value means "create from the name
// input", and both have to reject the same two mistakes — nothing picked, and a name that already
// exists. That validation was written twice and worded differently each time; it lives here so the
// dialogs only render the controls.
//
// The result is exactly what the /api/images/{move,copy} body wants: `toSetUid` for an existing set,
// `newSetName` for one to create.

export type SetLike = { uid: string; name: string }

export type SetDestination =
  | { ok: true; toSetUid: string; newSetName?: undefined }
  | { ok: true; toSetUid?: undefined; newSetName: string }
  | { ok: false; error: string }

/**
 * @param sets     every set in the project (used for the name-collision check)
 * @param targetUid  the dropdown value; '' = create a new set
 * @param newName    the new-set name input (only read when targetUid is empty)
 */
export function resolveSetDestination(sets: SetLike[], targetUid: string, newName: string): SetDestination {
  if (targetUid) return { ok: true, toSetUid: targetUid }
  const name = newName.trim()
  if (!name) return { ok: false, error: 'Select a set or enter a new set name.' }
  if (sets.some(s => s.name === name)) return { ok: false, error: `A set named "${name}" already exists.` }
  return { ok: true, newSetName: name }
}

/** Body fields for the move/copy request — spread into the JSON payload. */
export function destinationParams(dest: SetDestination): Record<string, string> {
  if (!dest.ok) return {}
  return dest.toSetUid ? { toSetUid: dest.toSetUid } : { newSetName: dest.newSetName! }
}
