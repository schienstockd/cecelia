import { describe, it, expect } from 'vitest'
import { viewerWindowQuery } from './viewerWindow'

describe('viewerWindowQuery', () => {
  it('carries everything the pop-out cannot look up for itself', () => {
    // A pop-out is a fresh app instance with no project open, so a key missing here is a preference
    // the viewer silently forgets rather than an error anyone sees.
    expect(viewerWindowQuery({
      projectUid: 'zolIMa', imageUid: 'fXgbTl', setUid: 'obWDNS',
      valueName: 'driftCorrected', name: 'M2b (cropped)',
    })).toBe('project=zolIMa&image=fXgbTl&set=obWDNS&valueName=driftCorrected&name=M2b+%28cropped%29')
  })

  it('omits what it was not given rather than sending it empty', () => {
    // `set=` empty reads as a set whose preferences are all defaults, which is not the same as no set.
    expect(viewerWindowQuery({ projectUid: 'p', imageUid: 'i' })).toBe('project=p&image=i')
    expect(viewerWindowQuery({ projectUid: 'p', imageUid: 'i', setUid: '', name: '' }))
      .toBe('project=p&image=i')
  })
})
