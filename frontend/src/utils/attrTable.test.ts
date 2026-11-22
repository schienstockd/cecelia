import { describe, it, expect } from 'vitest'
import {
  attrColumns, detectKey, imagesForKey, parseDelimited, parseTable, planImport, planSummary, planWrites,
  sniffDelimiter, writesByImage, type ParsedTable, type TableImage,
} from './attrTable'
import { imageTableCsvRows } from './imageTable'
import { rowsToCsv } from '../plots/export'
import type { CciaImage } from '../stores/project'

// Fixtures are real writer output, never typed to fit the parser:
//  - cores.csv / cores.tsv / cores-semicolon.csv / cores-bom.csv: one LibreOffice Calc 24.2 sheet (a
//    tissue microarray core map with a comma, a quote, a line break inside a cell, a trailing space,
//    a case variant, a text "007" and blank cells), saved by `soffice --convert-to csv` with the comma,
//    tab and semicolon field separators, and with "Include BOM" on (what Excel's "CSV UTF-8" writes).
//  - image-table-export.csv: `rowsToCsv(imageTableCsvRows(...))` — the image table's CSV export, with an
//    attribute `name` the export prefixed `attr_name` (attrCsvHeaders) and an excluded image's note.
const RAW = import.meta.glob('/src/utils/__fixtures__/attrTable/*', { query: '?raw', import: 'default', eager: true }) as
  Record<string, string>
const fx = (f: string) => {
  const t = RAW[`/src/utils/__fixtures__/attrTable/${f}`]
  if (t === undefined) throw new Error(`fixture ${f} missing`)
  return t
}

const CORES: TableImage[] = ['Core A1', 'Core A2', 'Core A3', 'Core B1', 'Core B2', 'Core C9', 'Core D4']
  .map((name, i) => ({ uid: `u${i}`, name: `${name}.oir`, oriPath: `/data/tma/${name}.oir`, attr: {} }))

const EXPECTED_RECORDS = [
  ['Core', 'Tissue', 'Donor', 'Age', 'Note'],
  ['Core A1', 'Liver', '007', '54', ''],
  ['Core A2', 'Liver ', '007', '54', 're-cut, "thin"'],
  ['Core A3', 'liver', '012', '61', 'fold\nat edge'],
  ['Core B1', 'Lymph node, mesenteric', '012', '61', ''],
  ['Core B2', 'Tonsil', '', '', ''],
  ['Core C9', 'Spleen', '019', '47.5', ''],
]

describe('parsing real spreadsheet output', () => {
  it.each([['cores.csv', ','], ['cores.tsv', '\t'], ['cores-semicolon.csv', ';'], ['cores-bom.csv', ',']])(
    '%s sniffs %j and parses every record', (file, delim) => {
      const t = parseTable(fx(file))!
      expect(t.delimiter).toBe(delim)
      expect([t.header, ...t.rows]).toEqual(EXPECTED_RECORDS)
    })

  it('the BOM file really starts with a BOM, and it does not reach the first header', () => {
    expect(fx('cores-bom.csv').charCodeAt(0)).toBe(0xfeff)
    expect(parseTable(fx('cores-bom.csv'))!.header[0]).toBe('Core')
  })

  it('reads \\r\\n line ends (a Windows save) the same, but keeps a line break inside quotes', () => {
    // The same LibreOffice file with each RECORD end turned into \r\n, as Excel / Windows writes it.
    const crlf = fx('cores.csv').replace(/\n(?=Core |$)/g, '\r\n')
    expect(crlf).toContain('\r\n')
    expect([parseTable(crlf)!.header, ...parseTable(crlf)!.rows]).toEqual(EXPECTED_RECORDS)
  })

  it('a lone \\r ends a record too', () => {
    expect(parseDelimited('a,b\r1,2\r', ',')).toEqual([['a', 'b'], ['1', '2']])
  })

  it('drops blank records but keeps a record of blank cells with a key', () => {
    expect(parseDelimited('a,b\n\n,,\n1,\n', ',')).toEqual([['a', 'b'], ['1', '']])
  })

  it('pads a short record to the header', () => {
    expect(parseTable('a,b,c\n1\n')!.rows).toEqual([['1', '', '']])
  })

  it('no text, no table', () => {
    expect(parseTable('')).toBeNull()
    expect(parseTable('\n\n')).toBeNull()
  })

  it('sniffs a tab over commas inside cells (a range pasted from a spreadsheet)', () => {
    expect(sniffDelimiter('Core\tTissue\nA1\tLymph node, mesenteric\n')).toBe('\t')
  })

  it('ignores delimiters inside quotes when sniffing', () => {
    expect(sniffDelimiter('"a;b;c",d\n"x;y;z",w\n')).toBe(',')
  })

  it('a one-column table falls back to a comma', () => {
    expect(sniffDelimiter('Name\nA1\n')).toBe(',')
  })
})

describe('matching rows to images', () => {
  it('a name matches with or without the extension', () => {
    expect(imagesForKey('Core A1.oir', 'name', CORES).map(i => i.uid)).toEqual(['u0'])
    expect(imagesForKey('Core A1', 'name', CORES).map(i => i.uid)).toEqual(['u0'])
    expect(imagesForKey('Core A', 'name', CORES)).toEqual([])
  })

  it('an exact name wins over an extension-less one', () => {
    const imgs = [{ uid: 'a', name: 'M1' }, { uid: 'b', name: 'M1.oir' }]
    expect(imagesForKey('M1', 'name', imgs).map(i => i.uid)).toEqual(['a'])
  })

  it('a name with a decimal tail keeps it', () => {
    const imgs = [{ uid: 'a', name: 'dose_1.5' }, { uid: 'b', name: 'dose_1' }]
    expect(imagesForKey('dose_1', 'name', imgs).map(i => i.uid)).toEqual(['b'])
  })

  it('a path matches whole, with Windows slashes, or as the tail of the original path', () => {
    expect(imagesForKey('/data/tma/Core B1.oir', 'path', CORES).map(i => i.uid)).toEqual(['u3'])
    expect(imagesForKey('tma\\Core B1.oir', 'path', CORES).map(i => i.uid)).toEqual(['u3'])
    expect(imagesForKey('ma/Core B1.oir', 'path', CORES)).toEqual([])   // a path segment, not a substring
  })

  it('a uid matches exactly', () => {
    expect(imagesForKey(' u4 ', 'uid', CORES).map(i => i.uid)).toEqual(['u4'])
    expect(imagesForKey('U4', 'uid', CORES)).toEqual([])
  })

  it('detects the key column and kind', () => {
    expect(detectKey(parseTable(fx('cores.csv'))!, CORES)).toEqual({ column: 0, kind: 'name' })
    const byPath = parseTable('Tissue,File\nLiver,/data/tma/Core A1.oir\nSpleen,/data/tma/Core C9.oir\n')!
    expect(detectKey(byPath, CORES)).toEqual({ column: 1, kind: 'path' })
    const byUid = parseTable('id,Tissue\nu1,Liver\nu2,Spleen\n')!
    expect(detectKey(byUid, CORES)).toEqual({ column: 0, kind: 'uid' })
  })

  it('no column names an image: no key', () => {
    expect(detectKey(parseTable('a,b\n1,2\n')!, CORES)).toBeNull()
  })
})

describe('the plan on the core map', () => {
  const table = parseTable(fx('cores.csv'))!
  const key = { column: 0, kind: 'name' as const }

  it('matches six rows; the seventh image has none', () => {
    const p = planImport(table, CORES, key)
    expect(p.matched.map(r => r.name)).toEqual(['Core A1.oir', 'Core A2.oir', 'Core A3.oir', 'Core B1.oir', 'Core B2.oir', 'Core C9.oir'])
    expect(p.imagesWithoutRow.map(i => i.name)).toEqual(['Core D4.oir'])
    expect(p.unmatched).toEqual([])
    expect(p.attrs).toEqual(['Tissue', 'Donor', 'Age', 'Note'])
    expect(p.newAttrs).toEqual(['Tissue', 'Donor', 'Age', 'Note'])
  })

  it('writes values trimmed, keeps the text "007", and skips blank cells', () => {
    const p = planImport(table, CORES, key)
    expect(p.writes.Tissue.u1).toBe('Liver')            // "Liver " → stored trimmed
    expect(p.writes.Donor.u0).toBe('007')
    expect(p.writes.Note.u2).toBe('fold\nat edge')
    expect(p.writes.Donor).not.toHaveProperty('u4')     // Core B2's blank Donor leaves it alone
    expect(p.counts.blank).toBe(6)            // Note ×4, Core B2 Donor + Age
  })

  it('reports Liver / liver as variants rather than merging them', () => {
    const p = planImport(table, CORES, key)
    expect(p.variants).toEqual([{ attr: 'Tissue', values: ['Liver', 'liver'] }])
    expect(p.writes.Tissue.u2).toBe('liver')
  })

  it('a variant against a value already in the set counts too', () => {
    const imgs = CORES.map(i => i.uid === 'u6' ? { ...i, attr: { Tissue: 'SPLEEN' } } : i)
    const p = planImport(table, imgs, key)
    expect(p.variants).toContainEqual({ attr: 'Tissue', values: ['SPLEEN', 'Spleen'] })
  })

  it('sorts each cell: same, set, case-only and real overwrites', () => {
    const imgs: TableImage[] = CORES.map(i => ({ ...i, attr: ({
      u0: { Tissue: 'Liver' },          // same
      u1: { Tissue: 'kidney' },         // overwrite
      u3: { Tissue: 'lymph node,  Mesenteric' },   // case + spacing only
      u4: { Tissue: '' },               // set
    } as Record<string, Record<string, string>>)[i.uid] ?? {} }))
    const p = planImport(table, imgs, key)
    const change = (uid: string) => p.matched.find(r => r.uid === uid)!.cells.find(c => c.attr === 'Tissue')!.change
    expect([change('u0'), change('u1'), change('u3'), change('u4')]).toEqual(['same', 'overwrite', 'case', 'set'])
    expect(p.writes.Tissue).not.toHaveProperty('u0')
    expect(p.newAttrs).not.toContain('Tissue')
    expect(planSummary(p)).toContain('2 overwrite')
  })

  it('a new column differing only by case from an existing one is reported, not merged', () => {
    const imgs = CORES.map(i => ({ ...i, attr: { tissue: '' } }))
    const p = planImport(table, imgs, key)
    expect(p.newAttrs).toContain('Tissue')
    expect(p.nameClashes).toEqual([{ attr: 'Tissue', existing: 'tissue' }])
  })

  it('reports a row naming no image, a duplicate row and an ambiguous name', () => {
    const t = parseTable('Name,Tissue\nCore A1,Liver\nCore Z9,Lung\nCore A1,Spleen\nM1,Skin\n')!
    const imgs = [...CORES, { uid: 'm1', name: 'M1.oir' }, { uid: 'm2', name: 'M1.czi' }]
    const p = planImport(t, imgs, { column: 0, kind: 'name' })
    expect(p.unmatched).toEqual([{ row: 3, key: 'Core Z9' }])
    expect(p.duplicates).toEqual([{ row: 4, key: 'Core A1', name: 'Core A1.oir' }])
    expect(p.ambiguous).toEqual([{ row: 5, key: 'M1', count: 2 }])
    expect(p.writes.Tissue.u0).toBe('Liver')            // the first row for an image wins
  })

  it('writesByImage turns the plan into the preview override shape', () => {
    const p = planImport(parseTable('Name,T,D\nCore A1,x,1\nCore A2,y,\n')!, CORES, key)
    expect(writesByImage(p)).toEqual({ u0: { T: 'x', D: '1' }, u1: { T: 'y' } })
  })
})

describe('round trip with the image table export', () => {
  const imgs: TableImage[] = [
    { uid: 'kQ3vXa', name: 'Core A1', attr: { Tissue: 'Liver', Donor: '007', name: 'core-a1' } },
    { uid: 'Lm8pRt', name: 'Core A2', attr: { Tissue: 'Lymph node, mesenteric', Donor: '012', name: '' } },
    { uid: 'Zc2wYb', name: 'Core B1', attr: { Tissue: 'Tonsil', Donor: '', name: '' } },
  ]

  it('the committed export reads back as a no-op, attr_name → name, own columns left out', () => {
    const t = parseTable(fx('image-table-export.csv'))!
    const key = detectKey(t, imgs)!
    expect(key).toEqual({ column: 0, kind: 'name' })
    const p = planImport(t, imgs, key)
    expect(p.attrs).toEqual(['Donor', 'Tissue', 'name'])
    expect(p.ignored.filter(i => i.why === 'imageTable').map(i => i.header)).toEqual([
      'Channels', 'Channel 1', 'Channel 2', 'Z slices', 'Frames', 'Duration', 'Time increment', 'Time unit',
      'Pixel size X', 'Pixel size Y', 'Pixel size Z', 'Pixel unit', 'Blocked', 'Excluded', 'Exclusion note'])
    expect(p.newAttrs).toEqual([])
    expect(planWrites(p)).toBe(false)
  })

  it('a fresh export from the live exporter also round-trips (guards the exporter drifting)', () => {
    const full = imgs.map(i => ({ ...i, sizeC: 1, channelNames: ['DAPI'], included: true })) as CciaImage[]
    const csv = rowsToCsv(imageTableCsvRows(full, ['Donor', 'Tissue', 'name']))
    const t = parseTable(csv)!
    const p = planImport(t, imgs, detectKey(t, imgs)!)
    expect(p.attrs).toEqual(['Donor', 'Tissue', 'name'])
    expect(planWrites(p)).toBe(false)
  })

  it('an edited export writes only what was edited', () => {
    const edited = fx('image-table-export.csv').replace('Core B1,2,DAPI,CD3,1,1,,,,0.5,0.5,1,µm,no,,Tonsil',
                                                         'Core B1,2,DAPI,CD3,1,1,,,,0.5,0.5,1,µm,no,019,Tonsil')
    expect(edited).not.toBe(fx('image-table-export.csv'))
    const p = planImport(parseTable(edited)!, imgs, { column: 0, kind: 'name' })
    expect(p.writes).toEqual({ Donor: { Zc2wYb: '019' } })
  })
})

describe('attribute columns', () => {
  const t = (csv: string): ParsedTable => parseTable(csv)!

  it('keeps attr_X when no column X is there', () => {
    expect(attrColumns(t('Name,attr_dose\nA,1\n'), 0, []).attrs.map(a => a.attr)).toEqual(['attr_dose'])
  })

  it('keeps attr_X when the images already have that attribute', () => {
    const imgs = [{ uid: 'a', name: 'A', attr: { attr_name: 'x' } }]
    expect(attrColumns(t('Name,attr_name\nA,1\n'), 0, imgs).attrs.map(a => a.attr)).toEqual(['attr_name'])
  })

  it('a blank or repeated header is left out; ordinary headers like Frames stay attributes outside an export', () => {
    const r = attrColumns(t('Name,Frames,,Frames\nA,1,2,3\n'), 0, [])
    expect(r.attrs.map(a => a.attr)).toEqual(['Frames'])
    expect(r.ignored.map(i => i.why)).toEqual(['key', 'blank', 'duplicate'])
  })
})
