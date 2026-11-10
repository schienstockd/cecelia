// RESULT-REVIEW: result-12f5547f
// The generic "Measures in microns — set the pixel size" gate note must stay when the checkpoint's
// lines do not name every image the missing scale blocks: an image whose QC predates the
// `metadata.pixel_size_unknown` finding has no line, so hiding the note left Run disabled with
// nothing on screen naming that image or why.
import { describe, it, expect } from 'vitest'
import { calibrationCheckpoint, checkpointCoversGate } from './calibrationCheckpoint'
import type { CciaImage } from '../stores/project'
import type { TaskDef } from '../tasks/types'

describe('the gate note is not hidden behind a checkpoint that misses a blocked image', () => {
  it('keeps the gate note when a blocked image has no finding', () => {
    const def = { requires: { scale: ['xy'] } } as TaskDef
    const a = { uid: 'a', name: 'a', status: 'done', qc: { 'importImages.omezarr/default': { findings: [] } } } as CciaImage
    const b = { uid: 'b', name: 'b', status: 'done', physicalSizeX: 0.3, physicalSizeY: 0.3,
      qc: { 'importImages.omezarr/default': { findings: [
        { level: 'warn', code: 'metadata.pixel_size_no_unit', short: 'Pixel size has no unit', long: 'l', detail: { field: 'x' } },
      ] } } } as CciaImage
    const lines = calibrationCheckpoint(def, [a, b])
    expect(lines.map(l => l.uids)).toEqual([['b']])
    expect(checkpointCoversGate(def, [a, b], lines)).toBe(false)
  })
})
