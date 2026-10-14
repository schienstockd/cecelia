import { describe, it, expect } from 'vitest'
import { retireDestroyed } from './retireDestroyed'

// A wait with nothing submitted after the destroy never advances the serial Dawn frees at, and one
// round can resolve before the release — both measured as false OOMs on real hardware.
describe('retireDestroyed', () => {
  it('submits before every wait, twice', async () => {
    const calls: string[] = []
    const device = {
      createCommandEncoder: () => ({ finish: () => ({}) }),
      queue: {
        submit: (bufs: unknown[]) => { calls.push(`submit:${bufs.length}`) },
        onSubmittedWorkDone: async () => { calls.push('wait') },
      },
    } as unknown as GPUDevice
    await retireDestroyed(device)
    expect(calls).toEqual(['submit:1', 'wait', 'submit:1', 'wait'])
  })
})
