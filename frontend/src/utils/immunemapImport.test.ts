import { describe, it, expect } from 'vitest'
import {
  addAttrColumn, badAttrColumns, removeAttrColumn, renameAttrColumn, defaultSelection, formatBytes, groupAttrsToRegister, mixedPixelSizes,
  selectionSummary, shapeText, suggestGroupAttrs, timeFromGroupName, videosToRegister,
  type ImmunemapVideo,
} from './immunemapImport'

// Two rows of the real experiment-63 scan (test-data/immunemap/scan_63_two_videos.json): one public
// 10x overview, one row the scan could not read.
const v309: ImmunemapVideo = {
  acquisitionId: 309, groupId: 101, condition: 'Control, before immunization',
  name: 'Obino-Movie1-SteadyState', importable: true, sizeBytes: 575530182,
  width: 512, height: 512, slices: 9, frames: 60, nChannels: 3,
  spacingXYZ: [1.799537109375, 1.799537109375, 4], nCuratedTracks: 151,
}
const v310: ImmunemapVideo = {
  acquisitionId: 310, groupId: 101, condition: 'Control, before immunization',
  name: '310', importable: false, reason: 'not public',
}
const v311: ImmunemapVideo = { ...v309, acquisitionId: 311, sizeBytes: 765261191,
                               spacingXYZ: [0.45, 0.45, 4] }

describe('immunemapImport', () => {
  it('ticks only importable videos by default', () => {
    expect(defaultSelection([v309, v310])).toEqual(['309'])
  })

  it('formats sizes', () => {
    expect(formatBytes(575530182)).toBe('576 MB')
    expect(formatBytes(9168268992)).toBe('9.2 GB')
    expect(formatBytes(null)).toBe('—')
  })

  it('describes shape and pixel size', () => {
    expect(shapeText(v309)).toBe('512×512 · 9z · 60t · 1.8 µm')
    expect(shapeText(v310)).toBe('')
  })

  it('sums the selection, ignoring a non-importable row even if ticked', () => {
    expect(selectionSummary([v309, v310, v311], ['309', '310', '311']))
      .toEqual({ n: 2, bytes: 575530182 + 765261191 })
    expect(videosToRegister([v309, v310, v311], ['310', '311']).map(v => v.acquisitionId)).toEqual([311])
  })

  it('flags mixed pixel sizes only when the selection mixes them', () => {
    expect(mixedPixelSizes([v309, v311], ['309', '311'])).toEqual([0.45, 1.8])
    expect(mixedPixelSizes([v309, v311], ['311'])).toEqual([])
  })

  // Experiment 63's real group names (test-data/immunemap/experiment_63.json).
  const groups63 = [
    'Control, before immunization',
    '30 min post immunization, irrelevant antigens (BSA)',
    '90 min post immunization, irrelevant antigens',
    '30 min post immunization, specific antigens (HEL)',
    '90 min post immunization, specific antigens (HEL)',
  ].map((name, i) => ({ id: 101 + i, name, description: '', nVideos: 3 }))

  it('reads the time off a group name, and nothing else', () => {
    expect(timeFromGroupName('30 min post immunization')).toBe('30 min')
    expect(timeFromGroupName('48h after infection')).toBe('48 h')
    expect(timeFromGroupName('Day 3')).toBe('')                 // unit before number: not guessed
    expect(timeFromGroupName('5 mg/kg FTY720')).toBe('')        // a dose is not a time
    expect(timeFromGroupName('2 days, anti-beta1 integrin')).toBe('2 d')
    expect(timeFromGroupName('Control, before immunization')).toBe('')
  })

  it('suggests a time column for experiment 63, blank where the name has none', () => {
    const a = suggestGroupAttrs(groups63)
    expect(a.columns).toEqual(['time'])
    expect(a.values['101']).toEqual({})
    expect(a.values['102']).toEqual({ time: '30 min' })
    expect(a.values['105']).toEqual({ time: '90 min' })
  })

  it('suggests no column when fewer than two groups name a time', () => {
    expect(suggestGroupAttrs(groups63.slice(0, 2)).columns).toEqual([])
  })

  it('flags blank, reserved and repeated attribute names', () => {
    expect(badAttrColumns(['time', 'antigen'])).toEqual([])
    expect(badAttrColumns(['time', ' ', 'condition', 'time '])).toEqual([' ', 'condition', 'time '])
  })

  it('registers only non-empty, trimmed values under trimmed names', () => {
    expect(groupAttrsToRegister({
      columns: ['time', ' antigen '],
      values: { '101': { time: '0 min', ' antigen ': ' ' }, '104': { time: '30 min', ' antigen ': 'HEL ' } },
    })).toEqual({ '101': { time: '0 min' }, '104': { time: '30 min', antigen: 'HEL' } })
  })

  it('renames, adds and removes columns, carrying the values', () => {
    const a = suggestGroupAttrs(groups63)
    addAttrColumn(a); addAttrColumn(a)
    expect(a.columns).toEqual(['time', 'attribute', 'attribute 2'])
    a.values['104'].attribute = 'HEL'
    expect(renameAttrColumn(a, 1, 'antigen')).toBe(true)
    expect(a.values['104']).toEqual({ time: '30 min', antigen: 'HEL' })
    expect(renameAttrColumn(a, 2, 'time')).toBe(false)          // taken: refused, nothing moved
    expect(a.columns).toEqual(['time', 'antigen', 'attribute 2'])
    removeAttrColumn(a, 0)
    expect(a.columns).toEqual(['antigen', 'attribute 2'])
    expect(a.values['104']).toEqual({ antigen: 'HEL' })
  })
})
