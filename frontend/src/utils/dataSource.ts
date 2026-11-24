// Where a set's images came from — the shapes of `/api/data-source` (api/src/routes/data_source.jl) and
// the pure logic the Source dialog renders. docs/todo/DATA_SOURCE_PLAN.md.

/** One source as stored. `by: 'import'` + `confirmed: false` is a suggestion import made from a
 *  repository ID in the path; anything the user writes is theirs and confirmed. */
export interface ImageSource {
  citation: string
  doi: string
  url: string
  licence: string
  repository: string
  id: string
  by: 'import' | 'user'
  confirmed: boolean
}

export interface SourceImageRow {
  uid: string
  name: string
  own: ImageSource | null
  resolved: (ImageSource & { from: 'image' | 'set' }) | null
}

export interface SourceReply { source: ImageSource | null; images: SourceImageRow[] }

/** The fields the user edits. */
export interface SourceForm { citation: string; doi: string; url: string; licence: string }

export const emptyForm = (): SourceForm => ({ citation: '', doi: '', url: '', licence: '' })

export const formFrom = (s: ImageSource | null | undefined): SourceForm =>
  s ? { citation: s.citation, doi: s.doi, url: s.url, licence: s.licence } : emptyForm()

/** True when the form says nothing (the backend reads such a source as unknown). */
export const formIsEmpty = (f: SourceForm): boolean =>
  !f.citation.trim() && !f.doi.trim() && !f.url.trim()

/** What Save posts: the user's fields, kept with the repository ID of the source they edited. */
export function sourcePayload(f: SourceForm, base?: ImageSource | null): Partial<ImageSource> | null {
  if (formIsEmpty(f)) return null
  return {
    citation: f.citation.trim(), doi: f.doi.trim(), url: f.url.trim(), licence: f.licence.trim(),
    repository: base?.repository ?? '', id: base?.id ?? '', by: 'user', confirmed: true,
  }
}

/** The import suggestion waiting on the user, when the set has no source of its own: the first
 *  unconfirmed import source on an image, with every image carrying that same suggestion. */
export function pendingSuggestion(reply: SourceReply | null): { source: ImageSource; uids: string[] } | null {
  if (!reply || reply.source) return null
  const first = reply.images.find(i => i.own && i.own.by === 'import' && !i.own.confirmed)?.own
  if (!first) return null
  const same = (s: ImageSource | null) => !!s && s.by === 'import' && !s.confirmed &&
    s.repository === first.repository && s.id === first.id
  return { source: first, uids: reply.images.filter(i => same(i.own)).map(i => i.uid) }
}

/** One line for the Metadata page: the set's citation, how many images override it, or "Not recorded". */
export function sourceSummary(reply: SourceReply | null): string {
  if (!reply) return ''
  const own = reply.images.filter(i => i.own).length
  const set = reply.source ? (reply.source.citation || reply.source.doi || reply.source.url) : ''
  if (set) return own ? `${set} · ${own} image(s) differ` : set
  if (pendingSuggestion(reply)) return 'Suggested from the import path'
  return own ? `${own} image(s) have their own` : 'Not recorded'
}
