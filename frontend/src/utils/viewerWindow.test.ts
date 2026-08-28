import { describe, it, expect } from 'vitest'
import { viewerWindowQuery } from './viewerWindow'

describe('viewerWindowQuery', () => {
  it('carries only identity — project, image, optional version', () => {
    // A pop-out is a fresh app instance with no project open. The window used to carry `set=` and
    // `name=` too; both moved to /api/viewer/meta (2026-08-28) — the server already knows them.
    expect(viewerWindowQuery({
      projectUid: 'zolIMa', imageUid: 'fXgbTl', valueName: 'driftCorrected',
    })).toBe('project=zolIMa&image=fXgbTl&valueName=driftCorrected')
  })

  it('omits an unspecified version rather than sending it empty', () => {
    // `valueName=` empty would echo back through `resolve_image_version` as an explicit empty
    // string; that's not the same as "server, pick the active version".
    expect(viewerWindowQuery({ projectUid: 'p', imageUid: 'i' })).toBe('project=p&image=i')
    expect(viewerWindowQuery({ projectUid: 'p', imageUid: 'i', valueName: '' }))
      .toBe('project=p&image=i')
  })
})
