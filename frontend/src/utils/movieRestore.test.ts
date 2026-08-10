import { describe, it, expect } from 'vitest'
import { restoreKind, lookRestore, keyframeRestore, missingRefs, restoreNote, restoreTargetSet,
         RESTORE_ROUTE } from './movieRestore'

describe('restoreKind', () => {
  it('routes each kind to the page that owns it', () => {
    expect(restoreKind({ configKind: 'look', config: { fps: 15 } })).toBe('look')
    expect(restoreKind({ configKind: 'keyframes', config: { keyframes: [] } })).toBe('keyframes')
    expect(RESTORE_ROUTE.look).toBe('/batch-movies')
    expect(RESTORE_ROUTE.keyframes).toBe('/animation')
  })

  // Every movie recorded before the registry existed. Not an error — the ordinary case in an old
  // project, and the row simply offers no edit action.
  it('is null with no config banked', () => {
    expect(restoreKind({ producedBy: 'viewer' })).toBeNull()
    expect(restoreKind({ configKind: 'look' })).toBeNull()
    expect(restoreKind(null)).toBeNull()
    expect(restoreKind(undefined)).toBeNull()
  })

  it('is null for a kind it does not know — a later version must not open on the wrong page', () => {
    expect(restoreKind({ configKind: 'storyboard', config: { a: 1 } })).toBeNull()
  })
})

describe('lookRestore — the batch shape', () => {
  const banked = {
    config: { channels: { DAPI: 'blue' }, valueNames: ['corrected'], labelValueNames: ['cellsA'],
              colourBy: 'live.cell.speed', showTracks: true },
    fileAttrs: ['Treatment', '__channels__'],
    fps: 24, sizeX: 1920, sizeY: 1080, suffix: 'corrected',
    imageUids: ['abc', 'def'],
  }

  it('hands the authored config back verbatim — it is already the page own bag', () => {
    const r = lookRestore(banked)!
    expect(r.cfg.channels).toEqual({ DAPI: 'blue' })
    expect(r.cfg.valueNames).toEqual(['corrected'])
    expect(r.cfg.showTracks).toBe(true)
    // …and NOT forced on: a batch chose its own naming, which the authored config already carries
    expect(r.cfg.nameByImage).toBeUndefined()
    expect(r.output).toMatchObject({ fps: 24, sizeX: 1920, sizeY: 1080, suffix: 'corrected' })
    expect(r.imageUids).toEqual(['abc', 'def'])
    expect(r.dropped).toEqual([])
  })

  // Banked one level up (it is sent BESIDE the recorder's config), so it has to be put back into the
  // bag the page reads everything else from.
  it('folds the banked fileAttrs back into the config', () => {
    expect(lookRestore(banked)!.cfg.fileAttrs).toEqual(['Treatment', '__channels__'])
  })
})

describe('lookRestore — the viewer shape', () => {
  const banked = {
    fps: 15, sizeX: null, sizeY: null, suffix: '',
    titleCard: { enabled: true, note: 'day 3', durationSec: 4 },
    valueNames: ['default'], labelValueNames: ['cellsA'], labelContour: 2,
    show3D: false, zSlice: 7, tStart: 10, tEnd: 60,
    compareLayout: 'column', compareContrast: 'version',
    showTimestamp: false, showScaleBar: true,
    look: { channels: { GFP: 'green' }, showPopulations: true, popType: 'flow', colourBy: 'clusters.0' },
    imageUid: 'img1',
  }

  // The record request put the look in one place and the masks/versions/3D at the top level, because
  // the recorder consumed those directly. Reassembling them is this function's whole job.
  it('assembles the flat request into one config', () => {
    const r = lookRestore(banked)!
    expect(r.cfg).toMatchObject({
      channels: { GFP: 'green' }, showPopulations: true, popType: 'flow', colourBy: 'clusters.0',
      valueNames: ['default'], labelValueNames: ['cellsA'], labelContour: 2,
      show3D: false, zSlice: 7, compareLayout: 'column', compareContrast: 'version',
    })
    expect(r.imageUids).toEqual(['img1'])
  })

  // The frame range is part of what made the movie, so it has to come back with it — a recreate that
  // silently records the whole timelapse is not a recreate.
  // The batch names by uid, a viewer recording by image name. Regenerating a restored viewer config
  // therefore wrote a uid-named TWIN beside the original instead of reproducing it, so the naming rule
  // comes back with the look like everything else about it.
  it('turns on image naming, because that is how the viewer named it', () => {
    expect(lookRestore(banked)!.cfg.nameByImage).toBe(true)
  })

  it('restores the frame range, and reads a null end as "to the last frame"', () => {
    expect(lookRestore(banked)!.cfg).toMatchObject({ tStart: 10, tEnd: 60 })
    expect(lookRestore({ ...banked, tEnd: null })!.cfg.tEnd).toBeNull()
  })

  it('carries the output fields, including a deliberately cleared suffix', () => {
    const r = lookRestore(banked)!
    expect(r.output.suffix).toBe('')          // '' is an edit, not an absence
    expect(r.output.sizeX).toBeNull()          // null = the napari canvas size
    expect(r.output.showTimestamp).toBe(false)
    expect(r.output.titleCard).toEqual({ enabled: true, note: 'day 3', durationSec: 4 })
  })

  // The one field with nowhere to land. Reporting beats losing it silently: the two surfaces are the
  // same KIND, not the same controls.
  it('reports skeletons, which the batch page cannot express', () => {
    const r = lookRestore({ ...banked, branchValueNames: ['skel'] })!
    expect(r.dropped).toHaveLength(1)
    expect(r.dropped[0]).toContain('skel')
    expect(r.cfg).not.toHaveProperty('branchValueNames')
  })

  it('says nothing when there were no skeletons', () => {
    expect(lookRestore({ ...banked, branchValueNames: [] })!.dropped).toEqual([])
  })
})

describe('lookRestore — tolerance (Decision 6)', () => {
  it('reads an entry with nothing but an fps', () => {
    const r = lookRestore({ fps: 30 })!
    expect(r.output.fps).toBe(30)
    // `nameByImage` alone, because the flat shape IS a viewer recording and that is how it named its
    // file — a fact about the producer, not something the entry has to carry
    expect(r.cfg).toEqual({ nameByImage: true })
    expect(r.imageUids).toEqual([])
  })

  // An `undefined` written over a field is worse than an absent one: the reader's own default no
  // longer applies. So a garbage value must leave the key OFF, not set it to undefined.
  it('omits a field it cannot parse rather than setting it undefined', () => {
    const r = lookRestore({ fps: 'fast', valueNames: 'corrected', show3D: 1, zSlice: {} })!
    expect('fps' in r.output).toBe(false)
    expect('valueNames' in r.cfg).toBe(false)
    expect('show3D' in r.cfg).toBe(false)
    expect('zSlice' in r.cfg).toBe(false)
  })

  it('keeps a null size and a null zSlice, which both MEAN something', () => {
    const r = lookRestore({ sizeX: null, zSlice: null })!
    expect(r.output.sizeX).toBeNull()
    expect(r.cfg.zSlice).toBeNull()
  })

  it('drops an unknown layout rather than sending it on', () => {
    expect(lookRestore({ compareLayout: 'diagonal' })!.cfg).not.toHaveProperty('compareLayout')
  })

  it('clamps a title card duration the way the recorder does', () => {
    expect(lookRestore({ titleCard: { enabled: true, note: '', durationSec: 99 } })!
      .output.titleCard!.durationSec).toBe(10)
  })

  it('is null for a non-object', () => {
    expect(lookRestore(null)).toBeNull()
    expect(lookRestore(undefined)).toBeNull()
  })
})

describe('keyframeRestore', () => {
  const withMeta = {
    fps: 20, suffix: 'anim',
    keyframes: [{ viewState: { camera: { zoom: 1 } }, steps: 40 },
                { viewState: { camera: { zoom: 2 } }, steps: 20 }],
    keyframeMeta: [{ assetId: 'a1', duration: 2, title: 'wide' },
                   { assetId: 'a2', duration: 1 }],
    imageUid: 'img1',
  }

  it('zips the render payload back together with the editor metadata', () => {
    const r = keyframeRestore(withMeta)!
    expect(r.frames).toHaveLength(2)
    expect(r.frames[0]).toEqual({ viewState: { camera: { zoom: 1 } }, duration: 2, assetId: 'a1', title: 'wide' })
    expect(r.frames[1]).toMatchObject({ duration: 1, assetId: 'a2' })
    expect(r.imageUid).toBe('img1')
    expect(r.dropped).toEqual([])
  })

  // What every animation recorded before this phase looks like: the render payload only, so the
  // seconds have to come back out of the frame count it was rendered at.
  it('recovers a duration from steps/fps when the editor metadata is absent', () => {
    const r = keyframeRestore({ fps: 20, keyframes: [{ viewState: {}, steps: 40 }] })!
    expect(r.frames[0].duration).toBe(2)
    expect(r.frames[0].assetId).toBeUndefined()
    expect(r.dropped).toEqual(['keyframe thumbnails — this animation predates them being saved'])
  })

  it('falls fps back to 15, the recorder default, when it was not banked', () => {
    expect(keyframeRestore({ keyframes: [{ viewState: {}, steps: 15 }] })!.frames[0].duration).toBe(1)
  })

  it('skips a keyframe with no view state rather than restoring a blank one', () => {
    const r = keyframeRestore({ fps: 10, keyframes: [{ steps: 10 }, { viewState: { a: 1 }, steps: 10 }] })!
    expect(r.frames).toHaveLength(1)
    expect(r.frames[0].viewState).toEqual({ a: 1 })
  })

  it('is null when there is no timeline left to restore', () => {
    expect(keyframeRestore({ fps: 15, keyframes: [] })).toBeNull()
    expect(keyframeRestore({ fps: 15 })).toBeNull()
    expect(keyframeRestore({ fps: 15, keyframes: [{ steps: 3 }] })).toBeNull()
  })

  it('leaves imageUid empty when the animation predates it being banked', () => {
    expect(keyframeRestore({ keyframes: [{ viewState: {}, steps: 15 }] })!.imageUid).toBe('')
  })
})

describe('missingRefs — the dangling reference, which is the failure that bites', () => {
  const avail = { versions: ['default', 'corrected'], segmentations: ['cellsA'],
                  channels: ['DAPI', 'GFP'], colourBy: ['live.cell.speed'] }

  it('names every reference the destination cannot resolve', () => {
    const out = missingRefs({ valueNames: ['default', 'gone'], labelValueNames: ['cellsB'],
                              channels: { DAPI: 'blue', RFP: 'red' }, colourBy: 'clusters.9' }, avail)
    expect(out).toEqual(["version 'gone'", "segmentation 'cellsB'", "channel 'RFP'", "colour-by 'clusters.9'"])
  })

  it('is silent when everything still exists', () => {
    expect(missingRefs({ valueNames: ['corrected'], labelValueNames: ['cellsA'],
                         channels: { GFP: 'green' }, colourBy: 'live.cell.speed' }, avail)).toEqual([])
  })

  // The distinction the whole check rests on. `undefined` = the page has not loaded that list yet, and
  // reporting every name as dead would be worse than reporting none; `[]` = the image genuinely has
  // none, which DOES make a named one dead.
  it('skips a list it has not been told about', () => {
    expect(missingRefs({ labelValueNames: ['cellsB'] }, { versions: ['default'] })).toEqual([])
  })

  it('reports against an empty list, which is a real answer', () => {
    expect(missingRefs({ labelValueNames: ['cellsB'] }, { segmentations: [] })).toEqual(["segmentation 'cellsB'"])
  })

  it('ignores an empty colour-by — none is a valid choice, not a dead reference', () => {
    expect(missingRefs({ colourBy: '' }, avail)).toEqual([])
  })
})

describe('restoreTargetSet — a restore switches sets rather than asking you to', () => {
  it('lands in the set holding the images, whatever is active', () => {
    expect(restoreTargetSet(['setB', 'setB'], 'setA')).toBe('setB')
  })

  // Two sets have no single answer, so the page keeps the one it is on and says what it dropped.
  it('keeps the active set when the images span two', () => {
    expect(restoreTargetSet(['setB', 'setC'], 'setA')).toBe('setA')
  })

  // An old movie that banked no image, or one whose images have since been deleted.
  it('keeps the active set when nothing is known', () => {
    expect(restoreTargetSet([], 'setA')).toBe('setA')
    expect(restoreTargetSet([null, undefined], 'setA')).toBe('setA')
  })

  // The animation page passes no fallback: one image, and if it is gone there is nowhere to land —
  // '' is falsy, which is the caller's "say so" branch.
  it('is empty with no fallback and no home', () => {
    expect(restoreTargetSet([null], '')).toBe('')
  })
})

describe('restoreNote', () => {
  it('is blank when everything came back', () => {
    expect(restoreNote([], [])).toBe('')
  })

  it('states the missing and the undroppable in one line', () => {
    expect(restoreNote(["version 'gone'"], ['skeletons (skel) — no control'])).toBe(
      "Not restored: version 'gone', skeletons (skel) — no control")
  })
})
