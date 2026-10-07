import { describe, expect, it } from 'vitest'
import { checkId, checkLine, lineCheck, lineText, TASK_CHECKS, type DiscoveryLine } from './taskDiscovery'
import type { CciaImage } from '../stores/project'

const img = (extra: Partial<CciaImage> = {}): CciaImage => ({ uid: 'u', name: 'n', status: 'done', ...extra })
const drifted = img({ filepaths: { default: 'a', driftCorrected: 'b' } })
const raw = img({ filepaths: { default: 'a' } })

describe('checkLine', () => {
  const after = { text: 'After Drift correction', check: 'driftCorrected' }
  const noPx = { text: 'Pixel sizes missing', check: '!pixelSizeSet' }

  it('a use line met on every image is ok', () => {
    expect(checkLine(after, 'use', [drifted, drifted])).toEqual(
      { text: 'After Drift correction', severity: 'ok', summary: '2 of 2 images drift-corrected' })
  })
  it('a use line met on only some stays neutral and names the misses', () => {
    expect(checkLine(after, 'use', [drifted, raw, raw])).toEqual(
      { text: 'After Drift correction', summary: '2 of 3 images not drift-corrected' })
  })
  it('a not line whose condition holds warns; otherwise neutral', () => {
    const bare = img(), calibrated = img({ physicalSizeX: 0.5, physicalSizeY: 0.5 })
    expect(checkLine(noPx, 'not', [bare, calibrated])).toEqual(
      { text: 'Pixel sizes missing', severity: 'warn', summary: '1 of 2 images without a pixel size' })
    expect(checkLine(noPx, 'not', [calibrated])).toEqual({ text: 'Pixel sizes missing' })
  })
  it('one image reads as the selected image', () => {
    expect(checkLine(after, 'use', [drifted]).summary).toBe('Selected image drift-corrected')
  })
  it('no check, an unknown check, or no images: just the text — never a verdict', () => {
    expect(checkLine('Plain line', 'use', [drifted])).toEqual({ text: 'Plain line' })
    expect(checkLine({ text: 'X', check: 'nope' }, 'use', [drifted])).toEqual({ text: 'X' })
    expect(checkLine(after, 'use', [])).toEqual({ text: 'After Drift correction' })
  })
})

// Every check a task spec names must exist here — a typo would silently leave the line unchecked.
// A relative glob (the specs live outside the vite root), not node:fs: the frontend has no @types/node.
const SPECS = import.meta.glob('../../../app/src/tasks/*/*.json', { eager: true, import: 'default' }) as
  Record<string, { useWhen?: DiscoveryLine[]; notWhen?: DiscoveryLine[] } | unknown[]>

describe('task specs name only known checks', () => {
  const refs: string[] = []
  for (const [path, spec] of Object.entries(SPECS)) {
    if (!spec || Array.isArray(spec)) continue
    for (const l of [...(spec.useWhen ?? []), ...(spec.notWhen ?? [])]) {
      const c = lineCheck(l)
      if (c !== undefined) refs.push(`${path}: ${c}`)
    }
  }

  it('found the specs and their checked lines', () => {
    expect(Object.keys(SPECS).length).toBeGreaterThan(40)
    expect(refs.length).toBeGreaterThan(5)
  })
  it('every id is in TASK_CHECKS', () => {
    expect(refs.filter(r => !(checkId(r.split(': ')[1]) in TASK_CHECKS))).toEqual([])
  })
  it('every line has text', () => {
    for (const spec of Object.values(SPECS)) {
      if (!spec || Array.isArray(spec)) continue
      for (const l of [...(spec.useWhen ?? []), ...(spec.notWhen ?? [])]) expect(lineText(l)).toBeTruthy()
    }
  })
})
