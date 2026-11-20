import { describe, it, expect } from 'vitest'
import { filterGroups, flatOptions, labelFor, measurePickGroups } from './searchSelect'

const GROUPS = [
  { options: [{ value: '', label: 'density' }] },
  { title: 'Intensity', options: [
    { value: 'mean_intensity_0', label: 'DAPI' },
    { value: 'mean_intensity_1', label: 'CD206' },
    { value: 'mean_intensity_2', label: 'halo_CD206' },
    { value: 'mean_intensity_3', label: 'MUC6' },
  ] },
  { title: 'Shape', options: [{ value: 'area' }, { value: 'centroid_x' }] },
]

describe('filterGroups', () => {
  it('returns everything for an empty or blank query', () => {
    expect(flatOptions(filterGroups(GROUPS, '  ')).length).toBe(7)
  })
  it('matches a label case-insensitively, anywhere in it, keeping list order', () => {
    expect(flatOptions(filterGroups(GROUPS, 'cd206')).map(o => o.label)).toEqual(['CD206', 'halo_CD206'])
  })
  it('matches the value when the label differs (a raw column name)', () => {
    expect(flatOptions(filterGroups(GROUPS, 'intensity_3')).map(o => o.label)).toEqual(['MUC6'])
  })
  it('matches an unlabelled option by its value', () => {
    expect(flatOptions(filterGroups(GROUPS, 'centr')).map(o => o.value)).toEqual(['centroid_x'])
  })
  it('drops a group with no match and keeps the title of one that has', () => {
    const r = filterGroups(GROUPS, 'muc')
    expect(r).toHaveLength(1)
    expect(r[0].title).toBe('Intensity')
  })
  it('needs every word somewhere in the label or value', () => {
    expect(flatOptions(filterGroups(GROUPS, 'halo cd2')).map(o => o.label)).toEqual(['halo_CD206'])
  })
  it('returns no groups when nothing matches', () => {
    expect(filterGroups(GROUPS, 'zzz')).toEqual([])
  })
})

describe('labelFor', () => {
  it('names the picked option, falling back to its value', () => {
    expect(labelFor(GROUPS, 'mean_intensity_3')).toBe('MUC6')
    expect(labelFor(GROUPS, 'area')).toBe('area')
    expect(labelFor(GROUPS, '')).toBe('density')
  })
  it('is blank for a value not in the list', () => {
    expect(labelFor(GROUPS, 'gone')).toBe('')
  })
})

describe('measurePickGroups', () => {
  it('labels each column and puts the leading options first, untitled', () => {
    const r = measurePickGroups([{ title: 'Intensity', cols: ['m0', 'm1'] }], c => c.toUpperCase(),
      [{ value: '', label: 'density' }])
    expect(r).toEqual([
      { options: [{ value: '', label: 'density' }] },
      { title: 'Intensity', options: [{ value: 'm0', label: 'M0' }, { value: 'm1', label: 'M1' }] },
    ])
  })
})

