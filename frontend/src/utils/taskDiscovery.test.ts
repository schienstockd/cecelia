import { describe, expect, it } from 'vitest'
import { checkId, checkLine, lineCheck, lineText, TASK_CHECKS, type DiscoveryLine, linkTaskMentions, type MentionTarget } from './taskDiscovery'
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
  it('a use line missed on any image warns and names the misses', () => {
    expect(checkLine(after, 'use', [drifted, raw, raw])).toEqual(
      { text: 'After Drift correction', severity: 'warn', summary: '2 of 3 images not drift-corrected' })
    expect(checkLine(after, 'use', [raw])).toEqual(
      { text: 'After Drift correction', severity: 'warn', summary: 'Selected image not drift-corrected' })
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

describe('linkTaskMentions', () => {
  const t = (label: string, path: string, page = path.slice(1)): MentionTarget =>
    ({ label, path, page, module: page, task: label })
  const targets = [t('HMM states', '/behaviour', 'Behaviour'), t('HMM (states + transitions)', '/behaviour', 'Behaviour'),
                   t('Drift correction', '/cleanup', 'Cleanup'), t('Cluster tracks', '/clust-tracks', 'Cluster tracks')]

  it('links a task that lives on another page', () => {
    const segs = linkTaskMentions('After HMM states, to cluster', '/clust-tracks', targets)
    expect(segs.map(s => [s.text, s.to?.page])).toEqual([['After ', undefined], ['HMM states', 'Behaviour'], [', to cluster', undefined]])
  })

  it('leaves a task on the same page as plain text', () => {
    expect(linkTaskMentions('Before Cluster tracks', '/clust-tracks', targets)).toEqual([{ text: 'Before Cluster tracks' }])
  })

  it('matches whole words, the longest label first, and labels with brackets', () => {
    const segs = linkTaskMentions('Run HMM (states + transitions) or Drift corrections', '/segment', targets)
    expect(segs.filter(s => s.to).map(s => s.text)).toEqual(['HMM (states + transitions)'])
  })

  it('finds several mentions in one line', () => {
    const segs = linkTaskMentions('Before HMM states or Cluster tracks', '/track', targets)
    expect(segs.filter(s => s.to).map(s => s.to!.page)).toEqual(['Behaviour', 'Cluster tracks'])
  })
})
