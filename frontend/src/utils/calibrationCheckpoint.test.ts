import { describe, it, expect } from 'vitest'
import { calibrationCheckpoint, checkpointCoversGate } from './calibrationCheckpoint'
import type { CciaImage } from '../stores/project'
import type { TaskDef } from '../tasks/types'

// Findings shaped as qc.jl's `metadata_qc_findings` banks them (code + detail.field, level warn).
const f = (code: string, field: string, short = code) =>
  ({ level: 'warn' as const, code, short, long: `fix ${code}`, detail: { field } })
const IMG = (uid: string, findings: ReturnType<typeof f>[], over: Partial<CciaImage> = {}): CciaImage =>
  ({ uid, name: uid, status: 'done', qc: { 'importImages.omezarr/default': { findings } }, ...over } as CciaImage)
const DEF = (scale?: string[]): TaskDef => ({ requires: scale ? { scale } : {} } as TaskDef)

describe('calibrationCheckpoint', () => {
  const noPx = f('metadata.pixel_size_unknown', 'x', 'Pixel size unknown')
  const zOdd = f('metadata.z_spacing_unusual', 'z', 'Z spacing looks unusual')
  const noDt = f('metadata.frame_interval_unknown', 't', 'Frame interval unknown')

  it('says nothing for a task that computes in pixels', () => {
    expect(calibrationCheckpoint(DEF(), [IMG('a', [noPx])])).toEqual([])
    expect(calibrationCheckpoint(undefined, [IMG('a', [noPx])])).toEqual([])
  })

  it('one line per finding, with the images it covers and the share of the selection', () => {
    const lines = calibrationCheckpoint(DEF(['xy']), [IMG('a', [noPx]), IMG('b', []), IMG('c', [noPx])])
    expect(lines).toEqual([{ code: 'metadata.pixel_size_unknown', short: 'Pixel size unknown · 2 of 3',
                             detail: 'fix metadata.pixel_size_unknown', uids: ['a', 'c'] }])
  })

  it('a single image reads without the count', () => {
    expect(calibrationCheckpoint(DEF(['xy']), [IMG('a', [noPx])])[0].short).toBe('Pixel size unknown')
  })

  it('keeps only the scales the task needs', () => {
    const img = IMG('a', [noPx, zOdd, noDt], { sizeZ: 8, sizeT: 10 })
    expect(calibrationCheckpoint(DEF(['xy']), [img]).map(l => l.code)).toEqual(['metadata.pixel_size_unknown'])
    expect(calibrationCheckpoint(DEF(['xy', 'z', 't']), [img]).map(l => l.code))
      .toEqual(['metadata.pixel_size_unknown', 'metadata.z_spacing_unusual', 'metadata.frame_interval_unknown'])
  })

  it('does not ask a static image for its interval or a single plane for its z step', () => {
    expect(calibrationCheckpoint(DEF(['xy', 'z', 't']), [IMG('a', [zOdd, noDt])])).toEqual([])
  })

  it('counts an image once even when a code is banked for two axes', () => {
    const img = IMG('a', [f('metadata.pixel_size_no_unit', 'x'), f('metadata.pixel_size_no_unit', 'y')])
    expect(calibrationCheckpoint(DEF(['xy']), [img])[0].uids).toEqual(['a'])
  })

  it('ignores findings that are not calibration', () => {
    const other = { level: 'warn' as const, code: 'import.channel_saturated', short: 's', long: 'l' }
    expect(calibrationCheckpoint(DEF(['xy']), [IMG('a', [other as never])])).toEqual([])
  })
})

describe('checkpointCoversGate', () => {
  const CAL = { physicalSizeX: 0.5, physicalSizeY: 0.5 }
  const noPx = f('metadata.pixel_size_unknown', 'x')
  const def = (scale?: string[], axes?: string[], comingSoon = false) =>
    ({ comingSoon, requires: { ...(scale ? { scale } : {}), ...(axes ? { axes } : {}) } } as TaskDef)
  const covers = (d: TaskDef, imgs: CciaImage[]) => checkpointCoversGate(d, imgs, calibrationCheckpoint(d, imgs))
  it('true when only a scale is missing and a line names the image', () => {
    expect(covers(def(['xy']), [IMG('a', [noPx])])).toBe(true)
  })
  it('false when calibrated, when an axis is missing, or when the task is coming soon', () => {
    expect(covers(def(['xy']), [IMG('a', [], CAL)])).toBe(false)
    expect(covers(def(['xy'], ['T']), [IMG('a', [noPx])])).toBe(false)
    expect(covers(def(['xy'], undefined, true), [IMG('a', [noPx])])).toBe(false)
    expect(covers(def(['xy']), [])).toBe(false)
  })
})
