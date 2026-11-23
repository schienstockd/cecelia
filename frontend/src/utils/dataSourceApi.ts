// The data source routes (api/src/routes/data_source.jl), one function each. Shapes in `dataSource.ts`.
import type { ImageSource, SourceReply } from './dataSource'

/** The set's source and each image's own and resolved one. Writes nothing. */
export async function fetchDataSource(projectUid: string, setUid: string): Promise<SourceReply> {
  const res = await fetch(`/api/data-source?projectUid=${encodeURIComponent(projectUid)}&setUid=${encodeURIComponent(setUid)}`)
  const body = await res.json().catch(() => ({})) as SourceReply & { error?: string }
  if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
  return body
}

/** Write the set's source, or with `imageUids` those images' own (null clears it). */
export async function saveDataSource(projectUid: string, setUid: string, source: Partial<ImageSource> | null,
                                     imageUids: string[] = []): Promise<void> {
  const res = await fetch('/api/data-source', {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ projectUid, setUid, source, ...(imageUids.length ? { imageUids } : {}) }),
  })
  if (!res.ok) {
    const body = await res.json().catch(() => ({})) as { error?: string }
    throw new Error(body.error ?? `HTTP ${res.status}`)
  }
}
