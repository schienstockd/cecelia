import { describe, it, expect } from 'vitest'
import { layersOf, isOverlay, framesFor, activeAnimationUid, unionRows, channelRows, popRows,
         cellState, cellToggle, cameraZoom, isEdited, keyframeTime } from './animationTimeline'
import type { AnimSnapshot } from '../stores/animation'

const kf = (id: string, imageUid: string, snapshot?: Record<string, unknown>,
            original?: Record<string, unknown>): AnimSnapshot =>
  ({ id, imageUid, snapshot, original })

describe('layersOf / isOverlay', () => {
  it('reads a keyframe with no view state as no layers', () => {
    expect(layersOf(kf('a', 'i'))).toEqual({})
    expect(layersOf(undefined)).toEqual({})
  })

  // The one rule that splits the matrix into its two row groups. napari names an overlay layer
  // "(flow) (cellsA) tracks"; a channel is just "DAPI".
  it('calls a parenthesised layer an overlay and a plain one a channel', () => {
    expect(isOverlay('(flow) (cellsA) points')).toBe(true)
    expect(isOverlay('DAPI')).toBe(false)
  })
})

describe('framesFor', () => {
  const all = [kf('a', 'i1'), kf('b', 'i2'), kf('c', 'i1')]

  it('keeps one image keyframes in list order', () => {
    expect(framesFor(all, 'i1').map(f => f.id)).toEqual(['a', 'c'])
  })

  // Not "everything": with no image chosen there is no timeline, and showing every image's keyframes
  // side by side would be a matrix of unrelated views.
  it('is empty with no image', () => {
    expect(framesFor(all, '')).toEqual([])
  })
})

describe('activeAnimationUid', () => {
  it('lets the table selection lead', () => {
    expect(activeAnimationUid(['sel'], 'napari')).toBe('sel')
  })

  // Landing on the page with nothing selected used to be the whole problem — the page said "open an
  // image in napari" and offered no way to. It now falls back the other way round: whatever napari
  // has open is a timeline worth showing until the user picks one.
  it('falls back to the image open in napari', () => {
    expect(activeAnimationUid([], 'napari')).toBe('napari')
    expect(activeAnimationUid([], null)).toBe('')
  })
})

describe('unionRows', () => {
  const frames = [
    kf('a', 'i', { layers: { DAPI: {}, '(flow) (cellsA) points': {} } }),
    kf('b', 'i', { layers: { DAPI: {}, GFP: {} } }),
  ]

  // A layer added halfway through the animation still needs a row — reading only the first keyframe
  // would drop it, and its cells are exactly where the interesting toggles are.
  it('unions across every keyframe, not just the first', () => {
    expect(channelRows(frames)).toEqual(['DAPI', 'GFP'])
  })

  it('splits overlays out of the channels', () => {
    expect(popRows(frames)).toEqual(['(flow) (cellsA) points'])
  })

  it('takes any predicate', () => {
    expect(unionRows(frames, n => n === 'GFP')).toEqual(['GFP'])
  })
})

describe('cellState', () => {
  const f = kf('a', 'i', { layers: { DAPI: { visible: true }, GFP: { visible: false }, RFP: {} } })

  it('reads visible, hidden, and a layer with no visible flag as shown', () => {
    expect(cellState(f, 'DAPI')).toBe(true)
    expect(cellState(f, 'GFP')).toBe(false)
    expect(cellState(f, 'RFP')).toBe(true)
  })

  // The third state, and the reason this returns `boolean | null` rather than a boolean: absent is not
  // hidden. The matrix draws a dot for it, with nothing to click.
  it('is null for a layer this keyframe does not have', () => {
    expect(cellState(f, 'Cy5')).toBeNull()
  })
})

describe('cellToggle', () => {
  const frames = [
    kf('a', 'i', { layers: { DAPI: { visible: true, colormap: 'blue' } } }),
    kf('b', 'i', { layers: { DAPI: { visible: true, colormap: 'blue' },
                             '(track) tracks': { visible: true, colormap: 'turbo', tail_width: 6 } } }),
  ]

  it('flips a layer the keyframe already has', () => {
    expect(cellToggle(frames, frames[0], 'DAPI')).toEqual({ visible: false, colormap: 'blue' })
    const hidden = kf('c', 'i', { layers: { DAPI: { visible: false } } })
    expect(cellToggle(frames, hidden, 'DAPI')).toEqual({ visible: true })
  })

  // The whole reason this exists: tracks were turned on and captured as a LATER keyframe, so the
  // earlier columns had no entry and their dots did nothing (Dominik, 2026-08-10).
  it('adds a layer the keyframe does not have, seeded from one that does', () => {
    expect(cellToggle(frames, frames[0], '(track) tracks'))
      .toEqual({ visible: true, colormap: 'turbo', tail_width: 6 })
  })

  // A shared props object would make two keyframes toggle as one — the bug this copy prevents.
  it('copies the seed rather than aliasing it', () => {
    const added = cellToggle(frames, frames[0], '(track) tracks') as Record<string, unknown>
    added.visible = false
    expect((frames[1].snapshot!.layers as Record<string, { visible: boolean }>)['(track) tracks'].visible).toBe(true)
  })

  it('writes nothing for a layer no keyframe has', () => {
    expect(cellToggle(frames, frames[0], 'Cy5')).toBeNull()
  })
})

describe('cameraZoom', () => {
  it('reads the zoom to one decimal', () => {
    expect(cameraZoom(kf('a', 'i', { camera: { zoom: 1.234 } }))).toBe('1.2')
  })

  it('shows a dash rather than a number it does not have', () => {
    expect(cameraZoom(kf('a', 'i', { camera: {} }))).toBe('—')
    expect(cameraZoom(kf('a', 'i'))).toBe('—')
  })
})

describe('isEdited', () => {
  it('is true once the working view diverges from the captured baseline', () => {
    expect(isEdited(kf('a', 'i', { camera: { zoom: 2 } }, { camera: { zoom: 1 } }))).toBe(true)
    expect(isEdited(kf('a', 'i', { camera: { zoom: 1 } }, { camera: { zoom: 1 } }))).toBe(false)
  })

  // No baseline = nothing to have diverged FROM. A restored keyframe gets its baseline set to what it
  // was restored as, so this only hits animations captured before baselines existed.
  it('is false with no baseline', () => {
    expect(isEdited(kf('a', 'i', { camera: { zoom: 2 } }))).toBe(false)
  })
})

describe('keyframeTime', () => {
  it('states the frame index, and the wall clock when the interval is known', () => {
    const f = kf('a', 'i', { dims: { current_step: [12, 0, 0] } })
    expect(keyframeTime(f)).toBe('t12')
    expect(keyframeTime(f, 30, 'second')).toBe('t12 · 6m')
  })

  it('says nothing for a keyframe with no time axis', () => {
    expect(keyframeTime(kf('a', 'i', { dims: {} }))).toBe('')
    expect(keyframeTime(kf('a', 'i'))).toBe('')
  })
})
