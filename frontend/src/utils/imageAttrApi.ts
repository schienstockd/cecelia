// Writing image attributes: the `/api/images/attr/*` routes, one function each. Each throws with
// the server's message on failure; the caller decides how to say so.
//
// `setImageAttr` returns what was STORED: the route trims values, so a caller updates its state from
// the reply, never from what it sent (`docs/API.md` → `attr/*`).

async function post(path: string, body: unknown): Promise<{ values?: Record<string, string> }> {
  const res = await fetch(path, {
    method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body),
  })
  const reply = await res.json().catch(() => ({})) as { error?: string; values?: Record<string, string> }
  if (!res.ok) throw new Error(reply.error ?? `HTTP ${res.status}`)
  return reply
}

/** Add attribute `attrName` (empty) to every image in `imageUids` — the column then exists. */
export async function createImageAttr(projectUid: string, imageUids: string[], attrName: string): Promise<void> {
  await post('/api/images/attr/create', { projectUid, imageUids, attrName })
}

/** Remove attribute `attrName` from every image in `imageUids`. */
export async function deleteImageAttr(projectUid: string, imageUids: string[], attrName: string): Promise<void> {
  await post('/api/images/attr/delete', { projectUid, imageUids, attrName })
}

/** Set `attrName` per image (`values`: uid → value); returns the values as stored. */
export async function setImageAttr(projectUid: string, attrName: string,
                                   values: Record<string, string>): Promise<Record<string, string>> {
  return (await post('/api/images/attr/set', { projectUid, attrName, values })).values ?? values
}

/**
 * Rename attribute `from` to `to` on every image in `imageUids`; returns uid → value as stored under
 * the new name. Refused (throws) when an image already has `to`. With `setUid`, the set's experiment
 * design follows the rename.
 */
export async function renameImageAttr(projectUid: string, imageUids: string[], from: string, to: string,
                                      setUid?: string): Promise<Record<string, string>> {
  return (await post('/api/images/attr/rename', { projectUid, imageUids, from, to, setUid })).values ?? {}
}
