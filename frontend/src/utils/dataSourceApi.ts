// The data source routes (api/src/routes/data_source.jl), one function each. Shapes in `dataSource.ts`.
import type { ImageSource, SourceReply } from './dataSource'
import { getJson, postJson } from './api'

/** The set's source and each image's own and resolved one. Writes nothing. */
export async function fetchDataSource(projectUid: string, setUid: string): Promise<SourceReply> {
  return getJson<SourceReply>('/api/data-source', { query: { projectUid, setUid } })
}

/** Write the set's source, or with `imageUids` those images' own (null clears it). */
export async function saveDataSource(projectUid: string, setUid: string, source: Partial<ImageSource> | null,
                                     imageUids: string[] = []): Promise<void> {
  await postJson('/api/data-source', { projectUid, setUid, source, ...(imageUids.length ? { imageUids } : {}) })
}
