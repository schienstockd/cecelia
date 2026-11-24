import { describe, it, expect } from 'vitest'
import { methodsReportUrl, methodsFileBase, roCrateUrl, roCrateFileName } from './methodsReport'

const params = (u: string) => new URL(u, 'http://x').searchParams

describe('methodsReportUrl', () => {
  it('asks for a chain run by its id', () => {
    const q = params(methodsReportUrl('P', { kind: 'chainRun', runId: 'r 1' }))
    expect(q.get('projectUid')).toBe('P')
    expect(q.get('runId')).toBe('r 1')
    expect(q.has('imageUids')).toBe(false)
  })
  it('asks for a set by its uid', () => {
    const q = params(methodsReportUrl('P', { kind: 'set', setUid: 'S' }))
    expect(q.get('setUid')).toBe('S')
    expect(q.has('runId')).toBe(false)
  })
  it('asks for a list of images by uid — a board sends its images, no populations', () => {
    const u = methodsReportUrl('P', { kind: 'images', imageUids: ['a', 'b'] })
    const q = params(u)
    expect(q.get('imageUids')).toBe('a,b')
    expect(q.has('pops')).toBe(false)
    expect(u.startsWith('/api/analysis/methods?')).toBe(true)
  })
})

describe('methodsFileBase', () => {
  it('makes a file-safe name', () => {
    expect(methodsFileBase('Fig 1: T cells')).toBe('Fig_1_T_cells_methods')
    expect(methodsFileBase('  ')).toBe('cecelia_methods')
    expect(methodsFileBase('Board (cropped).')).toBe('Board_cropped_methods')
  })
})

describe('roCrateUrl', () => {
  it('asks for one chain run by project and run id', () => {
    const u = roCrateUrl('P', 'r 1')
    expect(u.startsWith('/api/chains/runs/ro-crate?')).toBe(true)
    const q = params(u)
    expect(q.get('projectUid')).toBe('P')
    expect(q.get('runId')).toBe('r 1')
  })
  it('names the download after the run, as a zip', () => {
    expect(roCrateFileName('abc123')).toBe('run_abc123_ro-crate.zip')
    expect(roCrateFileName('a/b')).toBe('run_a_b_ro-crate.zip')
  })
})
