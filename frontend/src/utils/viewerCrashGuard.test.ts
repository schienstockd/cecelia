import { describe, it, expect } from 'vitest'
import { isUnfinishedAttempt, attemptPayload } from './viewerCrashGuard'

describe('isUnfinishedAttempt', () => {
  const mark = attemptPayload('fXgbTl', '2026-08-25T01:02:03.000Z')

  it('is true for a mark left on the same image', () => {
    expect(isUnfinishedAttempt(mark, 'fXgbTl')).toBe(true)
  })

  it('is false for a different image', () => {
    // Keyed per image on purpose: one image's shape is usually what took the driver down, and a
    // viewer-wide guard would lock every other image out because of it.
    expect(isUnfinishedAttempt(mark, 'Dml3RG')).toBe(false)
  })

  it('is false with no mark, no image, or an unreadable one', () => {
    expect(isUnfinishedAttempt(null, 'fXgbTl')).toBe(false)
    expect(isUnfinishedAttempt('', 'fXgbTl')).toBe(false)
    expect(isUnfinishedAttempt(mark, '')).toBe(false)
    expect(isUnfinishedAttempt('not json', 'fXgbTl')).toBe(false)
    expect(isUnfinishedAttempt('{"imageUid":null}', 'fXgbTl')).toBe(false)
  })

  it('does not expire — a crash an hour ago still counts', () => {
    // The mark can only survive if the page never reached a frame. Age does not make that less true,
    // and an expiry would hide the exact case this is for: close the crashed browser, come back later.
    expect(isUnfinishedAttempt(attemptPayload('fXgbTl', '2001-01-01T00:00:00.000Z'), 'fXgbTl'))
      .toBe(true)
  })
})
