// RESULT-REVIEW: result-7ad305fc — a tracked z-stack whose tracks are all too short to step comes back
// 2D with nSteps 0 and no autocorrelation; that is "too few steps", not a structurally 2D image.
import { describe, it, expect } from 'vitest'
import { motionDimsAdvisory } from './paramAdvisors'

describe('motionDimsAdvisory — tracked but too short', () => {
  it('keeps the too-few-steps warning instead of calling the stack 2D', () => {
    const a = motionDimsAdvisory('auto', { dims: 2, zUsed: false, confidence: 'high',
      reason: 'z is a single plane (no z variation)', metrics: { nSteps: 0 } })!
    expect(a.severity).toBe('warn')
    expect(a.message).toContain('too few steps')
  })
})
