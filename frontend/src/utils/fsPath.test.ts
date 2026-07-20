import { describe, it, expect } from 'vitest'
import { fsBreadcrumbs } from './fsPath'

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
    expect(fsBreadcrumbs('/home/dominik')).toEqual([
      { label: '/', path: '/' },
      { label: 'home', path: '/home' },
      { label: 'dominik', path: '/home/dominik' },
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
