import { describe, it, expect } from 'vitest'
import { PDFDocument, PDFName, PDFHexString, PDFDict } from 'pdf-lib'
import * as pdfLib from 'pdf-lib'
import {
  snapshotScope, paramsScopeKey, snapshotDue, sameParamRows, resolveParamsExport,
  buildProvenanceRecord, recordForBoard, applyPdfProvenance, boardProvenanceUrl,
  PROVENANCE_SCHEMA, PDF_PROVENANCE_KEY, type ParamsSnapshot, type BoardProvenanceResponse,
  parseProvenanceRecord, svgProvenanceText, pdfProvenanceText, readFigureRecord, recordGateFiles,
  popGateStatus,
} from './boardProvenance'
import { historyParamsCsv, type HistoryParamsRow } from './chainFromHistory'
import { buildBoardSvgs } from '../plots/boardSvg'

const seg: HistoryParamsRow = {
  step: 'Segment', task: 'segment.coastalMeasure', output: 'Bcells', images: '1/1', lastRun: '2026-10-01T12:00:00',
  changedFromDefault: 'models[0].seedSize=8', conflicts: '', note: '', relation: 'made', taskIds: ['t1'],
}
const trk: HistoryParamsRow = {
  step: 'Track', task: 'tracking.bayesian_track_measures', output: '', images: '1/1', lastRun: '2026-10-02T09:00:00',
  changedFromDefault: 'popsToTrack=/qc/cells', conflicts: '', note: '', relation: 'ran on', taskIds: ['t2'],
}
const scope = snapshotScope(['img2', 'img1'], { pops: ['live::Bcells/qc/cells'], clusterRuns: [] })
const snap = (rows: HistoryParamsRow[]): ParamsSnapshot =>
  ({ takenAt: '2026-11-01T10:00:00.000Z', key: paramsScopeKey(scope, ['summary:a']), scope, rows })

describe('parameters snapshot — when to take one', () => {
  const key = paramsScopeKey(scope, ['summary:a'])
  it('is order-free in images and populations', () => {
    expect(paramsScopeKey(snapshotScope(['img1', 'img2'], { pops: ['live::Bcells/qc/cells'], clusterRuns: [] }), ['summary:a'])).toBe(key)
  })
  it('re-takes a snapshot when a plot is added or re-scoped, not otherwise', () => {
    expect(snapshotDue(snap([seg]), key, ['summary:a'], ['summary:a'])).toBe(false)
    expect(snapshotDue(snap([seg]), paramsScopeKey(scope, ['summary:a', 'summary:b']), ['summary:a', 'summary:b'], ['summary:a'])).toBe(true)
    const other = snapshotScope(['img1', 'img2'], { pops: ['live::Bcells/qc/frags'], clusterRuns: [] })
    expect(snapshotDue(snap([seg]), paramsScopeKey(other, ['summary:a']), ['summary:a'], ['summary:a'])).toBe(true)
  })
  it('never stamps an old board just for being opened — only once a plot changes', () => {
    expect(snapshotDue(undefined, key, ['summary:a'], ['summary:a'])).toBe(false)
    expect(snapshotDue(undefined, key, ['summary:a', 'summary:b'], ['summary:a'])).toBe(true)
    expect(snapshotDue(undefined, key, ['', ''], ['summary:a', ''])).toBe(false)   // emptied board
  })
})

describe('parameters export — frozen, rebuilt, or both', () => {
  it('exports the rebuild for a board with no snapshot, marked rebuilt', () => {
    const out = resolveParamsExport(undefined, [seg])
    expect(out.source).toBe('rebuilt')
    expect(out.rows.map(r => r.recorded)).toEqual(['rebuilt'])
    expect(historyParamsCsv(out.rows).split('\n')[0].startsWith('recorded,step,')).toBe(true)
  })
  it('exports the snapshot when the rebuild matches it (order aside)', () => {
    const out = resolveParamsExport(snap([seg, trk]), [trk, seg], scope)
    expect(out).toMatchObject({ source: 'frozen', differs: false, takenAt: '2026-11-01T10:00:00.000Z' })
    expect(out.rows.map(r => r.recorded)).toEqual(['frozen 2026-11-01T10:00:00.000Z', 'frozen 2026-11-01T10:00:00.000Z'])
  })
  it('keeps the frozen rows and appends today\'s differing ones, flagged, when a step re-ran', () => {
    const rerun = { ...seg, lastRun: '2026-11-05T08:00:00', changedFromDefault: 'models[0].seedSize=6', taskIds: ['t9'] }
    const out = resolveParamsExport(snap([seg, trk]), [rerun, trk])
    expect(out.differs).toBe(true)
    expect(out.rows.map(r => [r.task, r.recorded])).toEqual([
      ['segment.coastalMeasure', 'frozen 2026-11-01T10:00:00.000Z'],
      ['tracking.bayesian_track_measures', 'frozen 2026-11-01T10:00:00.000Z'],
      ['segment.coastalMeasure', 'now (differs)'],
    ])
    expect(sameParamRows([seg], [rerun])).toBe(false)
  })
  it('says a step that disappeared from the rebuild differs', () => {
    expect(resolveParamsExport(snap([seg, trk]), [seg]).differs).toBe(true)
  })
})

const server = (over: Partial<BoardProvenanceResponse> = {}): BoardProvenanceResponse => ({
  ceceliaVersion: '0.1.9', project: { uid: 'P1', name: 'MERTK' },
  images: [{ uid: 'img1', name: 'mouse 1' }],
  populations: [{ key: 'live::Bcells/qc/cells', popType: 'live', valueName: 'Bcells', pop: '/qc/cells', gateFiles: ['1/img1/gating/Bcells.json'] }],
  runs: [seg, trk], author: { profile: 'dominik', via: 'gui' }, ...over,
})

describe('provenance record', () => {
  it('carries version, project, images, populations + gate files, runs with task ids, time and author', () => {
    const rec = buildProvenanceRecord([{ title: 'Fig 1', server: server() }], '2026-11-08T01:02:03.000Z')!
    expect(rec).toMatchObject({
      schema: PROVENANCE_SCHEMA, exportedAt: '2026-11-08T01:02:03.000Z', ceceliaVersion: '0.1.9',
      author: { profile: 'dominik', via: 'gui' }, project: { uid: 'P1', name: 'MERTK' },
      images: [{ uid: 'img1', name: 'mouse 1' }],
    })
    const b = rec.boards[0]
    expect(b.populations[0].gateFiles).toEqual(['1/img1/gating/Bcells.json'])
    expect(b.runs.map(r => r.taskIds)).toEqual([['t1'], ['t2']])
    expect(b.parameters).toEqual({ source: 'rebuilt', differs: false })
  })
  it('names the frozen runs when the board has a snapshot, and the re-run too when a rebuild differs', () => {
    const rerun = { ...seg, lastRun: '2026-11-05T08:00:00', taskIds: ['t9'] }
    const rec = buildProvenanceRecord([{ title: 'Fig 1', server: server({ runs: [rerun, trk] }), snapshot: snap([seg, trk]), scope }], 'now')!
    expect(rec.boards[0].runs.map(r => [r.taskIds, r.recorded])).toEqual([
      [['t1'], 'frozen 2026-11-01T10:00:00.000Z'], [['t2'], 'frozen 2026-11-01T10:00:00.000Z'], [['t9'], 'now (differs)']])
    expect(rec.boards[0].parameters).toEqual({ source: 'frozen', takenAt: '2026-11-01T10:00:00.000Z', differs: true })
  })
  it('unions images across boards and cuts to one board for a per-board SVG', () => {
    const rec = buildProvenanceRecord([
      { title: 'A', server: server() },
      { title: 'B', server: server({ images: [{ uid: 'img2', name: 'mouse 2' }, { uid: 'img1', name: 'mouse 1' }] }) },
    ], 'now')!
    expect(rec.images.map(i => i.uid)).toEqual(['img1', 'img2'])
    expect(recordForBoard(rec, 1).boards.map(b => b.title)).toEqual(['B'])
    expect(buildProvenanceRecord([], 'now')).toBeNull()
  })
  it('sends the scope as JSON arrays (population names may hold commas)', () => {
    const q = new URL(boardProvenanceUrl('P', ['a', 'b'], { pops: ['live::B/x,y'], clusterRuns: [] }), 'http://x').searchParams
    expect(JSON.parse(q.get('pops')!)).toEqual(['live::B/x,y'])
    expect(q.get('imageUids')).toBe('a,b')
  })
})

describe('provenance in the exported files', () => {
  const rec = buildProvenanceRecord([{ title: 'Fig 1', server: server() }], '2026-11-08T01:02:03.000Z')!
  it('round-trips through a real PDF document-info entry', async () => {
    const doc = await PDFDocument.create()
    applyPdfProvenance(doc, pdfLib, rec)
    doc.addPage([100, 100])
    const back = await PDFDocument.load(await doc.save())
    const info = back.context.lookup(back.context.trailerInfo.Info, PDFDict)
    const raw = info.lookup(PDFName.of(PDF_PROVENANCE_KEY), PDFHexString)
    expect(JSON.parse(raw.decodeText())).toEqual(rec)
    expect(back.getCreator()).toBe('Cecelia 0.1.9')
    expect(back.getAuthor()).toBe('dominik')
  })
  it('puts the record in each board SVG as parseable <metadata>, even with ]]> in a name', () => {
    const tricky = buildProvenanceRecord([{ title: 'x]]>y', server: server() }], 'now')!
    const [b] = buildBoardSvgs([{ title: 'x]]>y', aspect: 1, slots: [] }], () => JSON.stringify(tricky))
    const m = b.svg.match(/<metadata id="cecelia-provenance">(.*?)<\/metadata>/s)
    expect(m).not.toBeNull()
    // what an XML parser returns as the element's text: the CDATA sections' contents, concatenated
    const text = [...m![1].matchAll(/<!\[CDATA\[(.*?)\]\]>/gs)].map(x => x[1]).join('')
    expect(JSON.parse(text)).toEqual(tricky)
    expect(buildBoardSvgs([{ title: 't', aspect: 1, slots: [] }])[0].svg).not.toContain('<metadata')
  })
})

describe('reading a record back from an exported figure', () => {
  const rec = buildProvenanceRecord([{ title: 'Fig 1', server: server() }], '2026-11-08T01:02:03.000Z')!
  const bytes = (s: string) => new TextEncoder().encode(s).buffer as ArrayBuffer
  it('reads the PDF the exporter writes', async () => {
    const doc = await PDFDocument.create()
    applyPdfProvenance(doc, pdfLib, rec)
    doc.addPage([100, 100])
    const saved = await doc.save()
    expect(parseProvenanceRecord(await pdfProvenanceText(saved))).toEqual(rec)
    expect(await readFigureRecord('figure.PDF', saved.slice().buffer)).toEqual(rec)
  })
  it('reads the SVG the exporter writes, ]]> in a title included', async () => {
    const tricky = buildProvenanceRecord([{ title: 'x]]>y', server: server() }], 'now')!
    const [b] = buildBoardSvgs([{ title: 'x]]>y', aspect: 1, slots: [] }], () => JSON.stringify(tricky))
    expect(parseProvenanceRecord(svgProvenanceText(b.svg))).toEqual(tricky)
    expect(await readFigureRecord('board.svg', bytes(b.svg))).toEqual(tricky)
  })
  it('says null for a figure with no Cecelia record, or another file type', async () => {
    const [plain] = buildBoardSvgs([{ title: 't', aspect: 1, slots: [] }])
    expect(await readFigureRecord('plain.svg', bytes(plain.svg))).toBeNull()
    const doc = await PDFDocument.create(); doc.addPage([10, 10])
    expect(await readFigureRecord('plain.pdf', (await doc.save()).slice().buffer)).toBeNull()
    expect(await readFigureRecord('notes.txt', bytes('{}'))).toBeNull()
    expect(parseProvenanceRecord('{"schema":"other/1","boards":[],"project":{}}')).toBeNull()
    expect(parseProvenanceRecord('not json')).toBeNull()
  })
  it('carries the non-default parameters of each step', () => {
    expect(rec.boards[0].runs.map(r => r.params)).toEqual(server().runs.map(r => r.changedFromDefault))
  })
  it('lists the gate files once across boards', () => {
    const two = buildProvenanceRecord([{ title: 'A', server: server() }, { title: 'B', server: server() }], 'now')!
    expect(recordGateFiles(two)).toEqual([...new Set(server().populations.flatMap(p => p.gateFiles))])
  })
})

describe('a population gates since the record', () => {
  const ge = (file: string, status: 'edited' | 'unchanged' | 'unknown' | 'missing') => [file, { file, status, edits: 0, lastAt: '' }] as const
  it('edited wins, unchanged needs every file, no files = nothing to change', () => {
    const m = new Map([ge('a', 'unchanged'), ge('b', 'edited'), ge('c', 'unknown')])
    expect(popGateStatus(['a', 'b'], m)).toBe('edited')
    expect(popGateStatus(['a', 'c'], m)).toBe('unknown')
    expect(popGateStatus(['a'], m)).toBe('unchanged')
    expect(popGateStatus(['a', 'zz'], m)).toBe('unknown')
    expect(popGateStatus([], m)).toBeNull()
  })
  it('a gate file today that the record lacked is a change (a gate added to another image since)', () => {
    const m = new Map([ge('a', 'unchanged')])
    expect(popGateStatus(['a'], m, ['a', 'b'])).toBe('edited')
    expect(popGateStatus([], m, ['b'])).toBe('edited')
    expect(popGateStatus(['a'], m, ['a'])).toBe('unchanged')
  })
})
