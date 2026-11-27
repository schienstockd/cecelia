// The movie options' `res` row with real clicks: a stored size shows as `custom` with its fields, and
// `full` clears it so the recording takes every image pixel (`movieSizeParams` → `resolution`).
import { describe, expect, it } from 'vitest'
import { h, reactive } from 'vue'
import { userEvent } from 'vitest/browser'
import MovieOutputControls from './MovieOutputControls.vue'
import { frames, mountInto } from '../test/browser/mount'
import type { MovieResolution } from '../utils/movieSize'

function controls(init: { sizeX: number | null, sizeY: number | null, resolution?: MovieResolution | null,
                          show3D?: boolean, blankSize?: 'canvas' | 'full' }) {
  const m = reactive({ fps: 15, suffix: '', show3D: false, ...init })
  const host = mountInto({
    render: () => h('div', { style: 'width:360px' }, [h(MovieOutputControls, {
      fps: m.fps, sizeX: m.sizeX, sizeY: m.sizeY, suffix: m.suffix, show3D: m.show3D, resolution: m.resolution,
      ...(m.blankSize ? { blankSize: m.blankSize } : {}),
      canvasX: 1620, canvasY: 1089,
      'onUpdate:sizeX': (v: number | null) => { m.sizeX = v },
      'onUpdate:sizeY': (v: number | null) => { m.sizeY = v },
      'onUpdate:resolution': (v: MovieResolution) => { m.resolution = v },
    })]),
  })
  const chip = (label: string) => [...host.querySelectorAll<HTMLElement>('button.chip')]
    .find(b => b.textContent?.trim() === label)
  const sizeFields = () => host.querySelectorAll('input[type=number]').length
  return { host, m, chip, sizeFields }
}

describe('MovieOutputControls res row', () => {
  it('a stored size reads as custom, with its fields; full clears it', async () => {
    const { m, chip, sizeFields } = controls({ sizeX: 512, sizeY: 512, resolution: 'screen' })
    expect(chip('custom')!.classList.contains('on')).toBe(true)
    expect(sizeFields()).toBe(2)
    await userEvent.click(chip('full')!)
    expect(m).toMatchObject({ sizeX: null, sizeY: null, resolution: 'full' })
    await frames()
    expect(chip('full')!.classList.contains('on')).toBe(true)
    expect(sizeFields()).toBe(0)
  })

  it('custom starts the fields at the canvas size', async () => {
    const { m, chip, sizeFields } = controls({ sizeX: null, sizeY: null, resolution: 'screen' })
    await userEvent.click(chip('custom')!)
    expect(m).toMatchObject({ sizeX: 1620, sizeY: 1089 })
    await frames()
    expect(sizeFields()).toBe(2)
  })

  it('absent resolution (and 3D) keeps the bare px fields', () => {
    expect(controls({ sizeX: null, sizeY: null }).chip('full')).toBeUndefined()
    const three = controls({ sizeX: null, sizeY: null, resolution: 'full', show3D: true })
    expect(three.chip('full')).toBeUndefined()
    expect(three.sizeFields()).toBe(2)
  })

  it('a batch\'s blank fields say full, not the canvas size', () => {
    const { host } = controls({ sizeX: null, sizeY: null, blankSize: 'full' })
    const fields = [...host.querySelectorAll<HTMLInputElement>('input[type=number]')]
    expect(fields.map(f => f.placeholder)).toEqual(['full', 'full'])
    expect(fields[0].max).toBe('4096')
  })
})
