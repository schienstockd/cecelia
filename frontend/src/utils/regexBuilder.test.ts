import { describe, it, expect } from 'vitest'
import { buildFieldRegex, buildLookaroundRegex, extractWith, regexSampleFor } from './regexBuilder'

// End-to-end: build a regex for a common case, then apply it to the sample the user would type it for.
const field = (sample: string, sep: string, pos: Parameters<typeof buildFieldRegex>[1], stripExt = true) =>
  extractWith(buildFieldRegex(sep, pos, stripExt), sample)

describe('regexBuilder — field split', () => {
  it('extracts the last field (treatment at the end), dropping the extension', () => {
    expect(field('Image2-testB.tif', '-', 'last')).toBe('testB')
    expect(field('Image1-testA.tif', '-', 'last')).toBe('testA')
  })

  it('extracts the first field', () => {
    expect(field('M4d-CD8-GFP-CD20-Tom_003', '-', 'first')).toBe('M4d')
    expect(field('Image1-testA.tif', '-', 'first')).toBe('Image1')
  })

  it('extracts the nth field', () => {
    expect(field('M4d-CD8-GFP-CD20-Tom_003', '-', 'second')).toBe('CD8')
    expect(field('M4d-CD8-GFP-CD20-Tom_003', '-', 'third')).toBe('GFP')
    expect(field('M4d-CD8-GFP-CD20-Tom_003', '-', 'last')).toBe('Tom_003')
  })

  it('handles underscore / space separators', () => {
    expect(field('mouse_03_ctrl.czi', '_', 'last')).toBe('ctrl')
    expect(field('sample A cd4.tif', ' ', 'last')).toBe('cd4')
  })

  it('keeps the extension when stripExt is off', () => {
    expect(field('Image2-testB.tif', '-', 'last', false)).toBe('testB.tif')
  })

  it('single field (no separator present) still resolves first/last', () => {
    expect(field('Image1', '-', 'first')).toBe('Image1')
    expect(field('Image1.tif', '-', 'last')).toBe('Image1')
  })

  it('empty separator → empty regex → empty extraction', () => {
    expect(buildFieldRegex('', 'last', true)).toBe('')
    expect(field('a-b', '', 'last')).toBe('')
  })

  it('counts fields from the end', () => {
    expect(field('M4d-CD8-GFP-CD20-Tom', '-', 'secondLast')).toBe('CD20')
    expect(field('M4d-CD8-GFP-CD20-Tom', '-', 'thirdLast')).toBe('GFP')
    expect(field('mouse_03_ctrl.czi', '_', 'secondLast')).toBe('03')
  })

  it('from-the-end positions do not match when there are too few fields', () => {
    expect(field('a-b', '-', 'thirdLast')).toBe('')
    expect(field('Image1.tif', '-', 'secondLast')).toBe('')
  })

  it('a multi-char separator means "any of these characters"', () => {
    // the folder separator: `/` and `\`, so one pattern covers both platforms
    expect(field('/data/20260714/M1b-MERTK.ori', '/\\', 'last')).toBe('M1b-MERTK')
    expect(field('C:\\data\\20260714\\M1b-MERTK.ori', '/\\', 'last')).toBe('M1b-MERTK')
  })
})

// The point of the "Original path" source: pull acquisition info out of the UPSTREAM FOLDERS the
// user organises by (a date directory per session), not out of the filename.
describe('regexBuilder — extracting from the folders of a path', () => {
  const PATHS = [
    '/mnt/imaging/20260714/M1b-MERTK.ori',
    'C:\\imaging\\20260714\\M1b-MERTK.ori',
    '/mnt/imaging/two/levels/deep/20260714/M1b-MERTK.ori',
  ]

  it('takes the containing folder as the 2nd-last field, at any depth', () => {
    const re = buildFieldRegex('/\\', 'secondLast', true)
    for (const p of PATHS) expect(extractWith(re, p)).toBe('20260714')
  })

  // Windows source paths reach ccid.json as-is (drive letters, UNC shares, and — since the browse
  // route and Julia's joinpath can disagree — mixed separators), so the folder separator is the
  // CLASS [/\\] rather than one platform's character.
  it('handles Windows paths: drive letter, UNC share, mixed separators', () => {
    const re = buildFieldRegex('/\\', 'secondLast', true)
    expect(extractWith(re, 'C:\\imaging\\INCITe\\20260716\\M1b-MERTK.oir')).toBe('20260716')
    expect(extractWith(re, '\\\\garvan-fs\\imaging\\20260716\\M1b-MERTK.oir')).toBe('20260716')
    expect(extractWith(re, 'C:/imaging/INCITe\\20260716\\M1b-MERTK.oir')).toBe('20260716')
    expect(extractWith(buildFieldRegex('/\\', 'last', true), 'C:\\imaging\\20260716\\M1b-MERTK.oir'))
      .toBe('M1b-MERTK')
  })

  // stripExt is about the FILE extension, so it must not apply to a folder field — a `2026.07.16`
  // date folder used to match nothing at all because the dot-free token class was applied to it.
  it('a dotted folder name survives, with or without the extension toggle', () => {
    for (const stripExt of [true, false])
      expect(extractWith(buildFieldRegex('/\\', 'secondLast', stripExt),
                         'C:\\imaging\\2026.07.16\\M1b-MERTK.oir')).toBe('2026.07.16')
    // …while the last field still drops its extension as before
    expect(extractWith(buildFieldRegex('/\\', 'last', true),
                       'C:\\imaging\\2026.07.16\\M1b-MERTK.oir')).toBe('M1b-MERTK')
  })

  it('the look-around builder reaches it too (digits between separators)', () => {
    const re = buildLookaroundRegex(
      { text: '/', cls: 'none' }, { kind: 'digits', text: '' }, { text: '/', cls: 'none' })
    expect(extractWith(re, PATHS[0])).toBe('20260714')
  })
})

describe('regexSampleFor', () => {
  // The bug this fixes: `filepath` is the CONVERTED store inside the project
  // (ccidDriftCorrected.ome.zarr), so matching against it can never see the source folders.
  const img = { name: 'M1b-MERTK', oriPath: '/mnt/imaging/20260714/M1b-MERTK.ori' }

  it('path → the original source location, name → the image name', () => {
    expect(regexSampleFor(img, 'path')).toBe('/mnt/imaging/20260714/M1b-MERTK.ori')
    expect(regexSampleFor(img, 'name')).toBe('M1b-MERTK')
  })

  it('falls back to the name when no source path was recorded', () => {
    expect(regexSampleFor({ name: 'M1b-MERTK' }, 'path')).toBe('M1b-MERTK')
    expect(regexSampleFor({ name: 'M1b-MERTK', oriPath: null }, 'path')).toBe('M1b-MERTK')
    expect(regexSampleFor({ name: 'M1b-MERTK', oriPath: '' }, 'path')).toBe('M1b-MERTK')
  })
})

describe('regexBuilder — look-around', () => {
  const NO: Parameters<typeof buildLookaroundRegex>[0] = { text: '', cls: 'none' }
  const around = (
    sample: string,
    before: Parameters<typeof buildLookaroundRegex>[0],
    extract: Parameters<typeof buildLookaroundRegex>[1],
    after: Parameters<typeof buildLookaroundRegex>[2] = NO,
  ) => extractWith(buildLookaroundRegex(before, extract, after), sample)

  it('extracts digits after a literal prefix (M1a → mouse 1)', () => {
    expect(around('M1a', { text: 'M', cls: 'none' }, { kind: 'digits', text: '' })).toBe('1')
  })

  it('extracts a letter after digits (M1a → location a)', () => {
    expect(around('M1a', { text: '', cls: 'digits' }, { kind: 'lower', text: '' })).toBe('a')
  })

  it('combines literal + class in the context (M[0-9] anchor for M1b/M2a/M4f)', () => {
    const re = buildLookaroundRegex({ text: 'M', cls: 'digits' }, { kind: 'letters', text: '' }, NO)
    expect(re).toBe('(?<=M\\d+)[A-Za-z]+')
    expect(extractWith(re, 'M1b')).toBe('b')
    expect(extractWith(re, 'M2a')).toBe('a')
    expect(extractWith(re, 'M4f')).toBe('f')
    expect(extractWith(re, 'M10c')).toBe('c')   // multi-digit mouse still works
  })

  it('supports a lookahead (before the next dash)', () => {
    expect(around('CD8-GFP-CD20', { text: '-', cls: 'none' }, { kind: 'upper', text: '' }, { text: '-', cls: 'none' })).toBe('GFP')
  })

  it('generates the expected lookbehind/lookahead source', () => {
    expect(buildLookaroundRegex({ text: 'M', cls: 'none' }, { kind: 'digits', text: '' }, NO)).toBe('(?<=M)\\d+')
    expect(buildLookaroundRegex({ text: '', cls: 'digits' }, { kind: 'lower', text: '' }, NO)).toBe('(?<=\\d+)[a-z]+')
  })

  it('custom extract token is used raw; empty custom → no regex', () => {
    expect(around('M1a', { text: 'M', cls: 'none' }, { kind: 'custom', text: '[0-9]' })).toBe('1')
    expect(buildLookaroundRegex(NO, { kind: 'custom', text: '' }, NO)).toBe('')
  })

  it('escapes literal context text', () => {
    expect(buildLookaroundRegex({ text: '.', cls: 'none' }, { kind: 'digits', text: '' }, NO)).toBe('(?<=\\.)\\d+')
  })
})

describe('extractWith', () => {
  it('prefers capture group 1, falls back to whole match', () => {
    expect(extractWith('M(\\d+)', 'M4d-CD8')).toBe('4')          // mouse number
    expect(extractWith('CD\\d+', 'M4d-CD8-GFP')).toBe('CD8')     // no group → whole match
  })
  it('returns "" on no match or invalid pattern', () => {
    expect(extractWith('zzz', 'abc')).toBe('')
    expect(extractWith('(', 'abc')).toBe('')                     // invalid regex
  })
})
