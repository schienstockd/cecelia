import { describe, it, expect } from 'vitest'
import { worstDigestLevel, firstActionableLine, flagLines, newFlagLines } from './labDigest'

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

  it('flagLines: only ⚠️/❌ lines, trimmed', () => {
    const md = '- ✅ Segment — ok\n- ⚠️ Tracking — 2 flagged\n  ↳ detail\n- ❌ Behaviour — failed'
    expect(flagLines(md)).toEqual(['- ⚠️ Tracking — 2 flagged', '- ❌ Behaviour — failed'])
  })

  it('newFlagLines: a standing flag is not re-reported when a clean task rewrites the block', () => {
    const t1 = '## d [Cecelia]\n- ✅ Segment — cellpose on 1 image'
    const t2 = '## d [Cecelia]\n- ✅ Segment — cellpose on 2 images\n- ⚠️ Tracking — 1 flagged'
    const t3 = '## d [Cecelia]\n- ✅ Segment — cellpose on 3 images\n- ⚠️ Tracking — 1 flagged'
    // task 2 introduces the warning → it is new
    expect(newFlagLines(t1, t2)).toEqual(['- ⚠️ Tracking — 1 flagged'])
    // task 3 is clean; the warning is unchanged (standing) → nothing new to badge
    expect(newFlagLines(t2, t3)).toEqual([])
    // a genuinely new/escalated flag (count bumps, or a ❌ appears) IS reported
    const t4 = '## d [Cecelia]\n- ⚠️ Tracking — 2 flagged\n- ❌ Behaviour — failed'
    expect(newFlagLines(t3, t4)).toEqual(['- ⚠️ Tracking — 2 flagged', '- ❌ Behaviour — failed'])
    // empty prev (first capture / after reload) → all current flags surface once
    expect(newFlagLines('', t2)).toEqual(['- ⚠️ Tracking — 1 flagged'])
  })
})
