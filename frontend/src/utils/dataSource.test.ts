import { describe, expect, it } from 'vitest'
import { formFrom, formIsEmpty, pendingSuggestion, sourcePayload, sourceSummary,
         type ImageSource, type SourceReply } from './dataSource'

// what import stores for the HuBMAP download path (suggest_data_source, app/src/data_source.jl)
const HUBMAP: ImageSource = { citation: 'HuBMAP HBM666.MCZF.973', doi: '10.35079/HBM666.MCZF.973',
  url: 'https://doi.org/10.35079/HBM666.MCZF.973', licence: '', repository: 'hubmap', id: 'HBM666.MCZF.973',
  by: 'import', confirmed: false }
const row = (uid: string, own: ImageSource | null) => ({ uid, name: uid, own, resolved: own ? { ...own, from: 'image' as const } : null })

describe('data source', () => {
  it('finds the import suggestion and every image carrying it, only while the set has none', () => {
    const reply: SourceReply = { source: null, images: [row('a', HUBMAP), row('b', HUBMAP), row('c', null)] }
    expect(pendingSuggestion(reply)).toEqual({ source: HUBMAP, uids: ['a', 'b'] })
    expect(pendingSuggestion({ ...reply, source: { ...HUBMAP, by: 'user', confirmed: true } })).toBeNull()
    expect(pendingSuggestion({ source: null, images: [row('a', { ...HUBMAP, confirmed: true })] })).toBeNull()
  })

  it('saves what the user wrote as theirs, keeping the repository ID; an empty form clears', () => {
    const f = { ...formFrom(HUBMAP), citation: 'HuBMAP HBM666.MCZF.973, Stanford TMC' }
    expect(sourcePayload(f, HUBMAP)).toMatchObject({ citation: 'HuBMAP HBM666.MCZF.973, Stanford TMC',
      repository: 'hubmap', id: 'HBM666.MCZF.973', by: 'user', confirmed: true })
    expect(formIsEmpty({ citation: ' ', doi: '', url: '', licence: 'CC-BY' })).toBe(true)
    expect(sourcePayload({ citation: '', doi: '', url: '', licence: 'CC-BY' })).toBeNull()
  })

  it('summarises the set for the Metadata page', () => {
    const set = { ...HUBMAP, by: 'user' as const, confirmed: true }
    expect(sourceSummary({ source: set, images: [row('a', null)] })).toBe('HuBMAP HBM666.MCZF.973')
    expect(sourceSummary({ source: set, images: [row('a', { ...set, citation: 'other' })] })).toBe('HuBMAP HBM666.MCZF.973 · 1 image(s) differ')
    expect(sourceSummary({ source: null, images: [row('a', HUBMAP)] })).toBe('Suggested from the import path')
    expect(sourceSummary({ source: null, images: [row('a', null)] })).toBe('Not recorded')
  })
})
