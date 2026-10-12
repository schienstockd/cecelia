import { describe, it, expect } from 'vitest'
import { fsBreadcrumbs, isPickable } from './fsPath'

describe('fsBreadcrumbs', () => {
  it('empty → no crumbs', () => {
    expect(fsBreadcrumbs('')).toEqual([])
  })
  it('filesystem root', () => {
    expect(fsBreadcrumbs('/')).toEqual([{ label: '/', path: '/' }])
  })
  it('unix absolute path (incl. a mount)', () => {
    expect(fsBreadcrumbs('/mnt/data')).toEqual([
      { label: '/', path: '/' },
      { label: 'mnt', path: '/mnt' },
      { label: 'data', path: '/mnt/data' },
    ])
  })
  it('home path builds the full chain', () => {
    expect(fsBreadcrumbs('/home/alice')).toEqual([
      { label: '/', path: '/' },
      { label: 'home', path: '/home' },
      { label: 'alice', path: '/home/alice' },
    ])
  })
  it('windows drive path (backslashes normalised)', () => {
    expect(fsBreadcrumbs('C:\\Users\\d')).toEqual([
      { label: 'C:', path: 'C:/' },
      { label: 'Users', path: 'C:/Users' },
      { label: 'd', path: 'C:/Users/d' },
    ])
  })
})

describe('isPickable', () => {
  const f = (name: string, isdir = false, isimage = false) => ({ name, isdir, isimage })
  it('file mode filters files by extension, case-insensitively', () => {
    expect(isPickable(f('a.CSV'), 'file', ['.csv'])).toBe(true)
    expect(isPickable(f('a.txt'), 'file', ['.csv'])).toBe(false)
    expect(isPickable(f('a.txt'), 'file')).toBe(true)
  })
  it('file mode never picks a plain folder — it is navigated into', () => {
    expect(isPickable(f('data', true), 'file')).toBe(false)
    expect(isPickable(f('labels.zarr', true), 'file', ['.zarr'])).toBe(false)
  })
  it('a folder format (OME-Zarr) is picked as one file with dirExtensions', () => {
    expect(isPickable(f('labels.ome.ZARR', true), 'file', ['.tif'], ['.zarr'])).toBe(true)
    expect(isPickable(f('raw', true), 'file', ['.tif'], ['.zarr'])).toBe(false)
  })
  it('bundle and image modes are unchanged', () => {
    expect(isPickable(f('p.ccbundle', true), 'bundle')).toBe(true)
    expect(isPickable(f('p.ccbundle'), 'bundle')).toBe(false)
    expect(isPickable(f('x.tif', false, true), 'image')).toBe(true)
    expect(isPickable(f('x', true), 'dir')).toBe(false)
  })
})
