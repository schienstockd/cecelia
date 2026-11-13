// RESULT-REVIEW: result-9749922d
// A re-run of a segmentation the viewer already knows, while it shows ANOTHER mask, must still reach the
// viewer as a clear it accepts — or the rerun's mask is later drawn from the browser's cache of the
// previous run's bytes, under the new run's name.
import { describe, it, expect } from 'vitest'
import { onResultActions, onStatusActions, type ViewerAutoUpdateAction } from './viewerAutoUpdate'
import { viewerCacheClearMatches } from '../lib/viewerCacheClearChannel'

describe('re-running a known mask while another is shown', () => {
  it('a clear that the viewer accepts is published', () => {
    const viewer = { imageUid: 'a', valueName: 'default', labelValueName: 'nuc', knownLabelNames: ['nuc', 'DAPI'] }
    const result = onResultActions({ imageUid: 'a', meta: { labelValueName: 'DAPI' } }, 'a', true)
    const covered = new Set<ViewerAutoUpdateAction['kind']>(result.map(a => a.kind))
    const status = onStatusActions({ status: 'done', imageUid: 'a' }, 'a', true, covered)
    const matched = [...result, ...status].filter(a => a.kind === 'cacheClear'
      && viewerCacheClearMatches({ imageUid: 'a', valueName: a.valueName, labelValueName: a.labelValueName }, viewer))
    expect(matched.length).toBeGreaterThan(0)
  })
})
