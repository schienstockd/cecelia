import { describe, it, expect } from 'vitest'
import { worstDigestLevel, firstActionableLine } from './labDigest'

describe('labDigest', () => {
  it('worstDigestLevel: ❌ beats ⚠️ beats ✅', () => {
    expect(worstDigestLevel('✅ Segment — cellpose on 3 images')).toBe('ok')
    expect(worstDigestLevel('✅ Segment\n⚠️ Tracking — track_measures')).toBe('warn')
    expect(worstDigestLevel('⚠️ Tracking\n❌ Behaviour — hmm failed')).toBe('fail')
  })

  it('firstActionableLine: first ⚠️/❌ line, symbol + markers stripped', () => {
    const md = '## [Cecelia]\n✅ Segment — ok\n⚠️ Tracking — track_measures on 2 images\n❌ Behaviour — hmm on 1 image'
    expect(firstActionableLine(md)).toBe('Tracking — track_measures on 2 images')
    expect(firstActionableLine('✅ all good')).toBe('')   // nothing actionable
  })
})
