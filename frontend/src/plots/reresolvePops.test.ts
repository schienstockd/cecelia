import { describe, expect, it } from 'vitest'
import { reresolvePops } from './reresolvePops'
import { tkey } from './series'
import type { SegmentationPops } from './types'

const pop = (path: string, uid = '', popType = 'flow') =>
  ({ path, name: path.split('/').pop() ?? path, colour: '#000', popType, uid })

const grp = (valueName: string, entries: [string, string?][], popType = 'flow'): SegmentationPops =>
  ({ valueName, populations: entries.map(([p, u = '']) => pop(p, u, popType)) })

describe('reresolvePops — UID pass', () => {
  it('matches by uid across a rename (same image, pop renamed since capture)', () => {
    // At capture: pop was named "CD4" under uid ABC123. Now renamed to "CD4+" (uid unchanged).
    const seg = [grp('A_labels', [['/root/CD4+', 'ABC123']])]
    const restored = reresolvePops(
      [tkey('flow', 'A_labels', '/root/CD4')],
      ['ABC123'],
      seg,
    )
    expect(restored).toEqual([tkey('flow', 'A_labels', '/root/CD4+')])
  })

  it('matches by uid across a move (same image, pop reparented)', () => {
    // Pop moved from /root/CD4 to /myeloid/CD4; uid unchanged.
    const seg = [grp('A_labels', [['/myeloid/CD4', 'ABC123']])]
    const restored = reresolvePops(
      [tkey('flow', 'A_labels', '/root/CD4')],
      ['ABC123'],
      seg,
    )
    expect(restored).toEqual([tkey('flow', 'A_labels', '/myeloid/CD4')])
  })

  it('falls back to path when the captured uid is empty (legacy capture)', () => {
    // Envelope from before pop uids landed — capturedUids[i] is empty; path fallback runs.
    const seg = [grp('A_labels', [['/root/CD4', 'ABC123']])]
    const restored = reresolvePops(
      [tkey('flow', 'A_labels', '/root/CD4')],
      [''],
      seg,
    )
    expect(restored).toEqual([tkey('flow', 'A_labels', '/root/CD4')])
  })

  it('falls back to path when the captured uid is not in current segPops', () => {
    // Different image loaded — uids are per-image so no uid match; path resolution still works.
    const seg = [grp('B_labels', [['/root/CD4', 'XYZ999']])]
    const restored = reresolvePops(
      [tkey('flow', 'A_labels', '/root/CD4')],
      ['ABC123'],
      seg,
    )
    expect(restored).toEqual([tkey('flow', 'B_labels', '/root/CD4')])
  })

  it('treats a shorter capturedUids array as trailing-empty', () => {
    const seg = [grp('A_labels', [['/root/CD4', 'ABC123'], ['/root/CD8', 'DEF456']])]
    const restored = reresolvePops(
      [tkey('flow', 'A_labels', '/root/CD4'), tkey('flow', 'A_labels', '/root/CD8')],
      ['ABC123'],   // uid provided for first entry only
      seg,
    )
    expect(restored).toEqual([
      tkey('flow', 'A_labels', '/root/CD4'),
      tkey('flow', 'A_labels', '/root/CD8'),
    ])
  })
})

describe('reresolvePops — path fallback', () => {
  it('identity-restores when the captured valueName is still current', () => {
    const seg = [grp('A_labels', [['/root/CD4'], ['/root/CD8']])]
    const restored = reresolvePops(
      [tkey('flow', 'A_labels', '/root/CD4'), tkey('flow', 'A_labels', '/root/CD8')],
      ['', ''],
      seg,
    )
    expect(restored).toEqual([
      tkey('flow', 'A_labels', '/root/CD4'),
      tkey('flow', 'A_labels', '/root/CD8'),
    ])
  })

  it('re-maps to the current valueName when the captured one is gone', () => {
    // Capture was from image A (A_labels); current context is image B (B_labels).
    const seg = [grp('B_labels', [['/root/CD4'], ['/root/CD8']])]
    const restored = reresolvePops(
      [tkey('flow', 'A_labels', '/root/CD4')],
      [''],
      seg,
    )
    expect(restored).toEqual([tkey('flow', 'B_labels', '/root/CD4')])
  })

  it('pools across every currently-loaded segmentation carrying the pop', () => {
    // Capture from A_labels; user now has B and C loaded.
    const seg = [
      grp('B_labels', [['/root/CD4']]),
      grp('C_labels', [['/root/CD4']]),
    ]
    const restored = reresolvePops(
      [tkey('flow', 'A_labels', '/root/CD4')],
      [''],
      seg,
    )
    expect(restored).toEqual([
      tkey('flow', 'B_labels', '/root/CD4'),
      tkey('flow', 'C_labels', '/root/CD4'),
    ])
  })

  it('preserves the SAME valueName when present, even alongside other loaded segmentations', () => {
    // User has A + B loaded and shared from A → keep A's own tkey (not add B's) so the intent is
    // preserved exactly rather than silently widened.
    const seg = [
      grp('A_labels', [['/root/CD4']]),
      grp('B_labels', [['/root/CD4']]),
    ]
    const restored = reresolvePops(
      [tkey('flow', 'A_labels', '/root/CD4')],
      [''],
      seg,
    )
    expect(restored).toEqual([tkey('flow', 'A_labels', '/root/CD4')])
  })

  it('drops entries whose popPath is missing from every current segmentation', () => {
    const seg = [grp('B_labels', [['/root/CD4']])]                        // no CD8 anymore
    const restored = reresolvePops(
      [tkey('flow', 'A_labels', '/root/CD4'), tkey('flow', 'A_labels', '/root/CD8')],
      ['', ''],
      seg,
    )
    expect(restored).toEqual([tkey('flow', 'B_labels', '/root/CD4')])
  })

  it('respects popType (a flow CD4 is not a track CD4)', () => {
    const seg = [grp('B_labels', [['/root/CD4']], 'track')]
    const restored = reresolvePops([tkey('flow', 'A_labels', '/root/CD4')], [''], seg)
    expect(restored).toEqual([])
  })

  it('deduplicates when a captured entry maps to a key already produced by an earlier one', () => {
    const seg = [grp('B_labels', [['/root/CD4']])]
    const restored = reresolvePops(
      [tkey('flow', 'A_labels', '/root/CD4'), tkey('flow', 'C_labels', '/root/CD4')],
      ['', ''],
      seg,
    )
    expect(restored).toEqual([tkey('flow', 'B_labels', '/root/CD4')])
  })

  it('returns empty on an empty capture or empty segPops', () => {
    expect(reresolvePops([], [], [grp('A_labels', [['/root/CD4']])])).toEqual([])
    expect(reresolvePops([tkey('flow', 'A_labels', '/root/CD4')], [''], [])).toEqual([])
  })
})
