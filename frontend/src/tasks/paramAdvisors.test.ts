import { describe, it, expect } from 'vitest'
import {
  anisoGridEstimate, anisoGridAdvisory, motionDimsAdvisory, imageVersionAdvisory, formatBytes,
  paramAdvisor, ANISO_BYTES_PER_BOX_PER_FRAME, ANISO_WARN_BYTES, ANISO_MIN_BOX_PX,
} from './paramAdvisors'
import { isImageVersionField, preferredValueName } from './paramValues'

// EaMaVq's ACTIVE (drift-corrected) version — 544x548, not the 512x512 of its default import. That
// gap is the whole reason this takes version geometry rather than a per-image stored size.
const EAMAVQ = { sizeX: 548, sizeY: 544, sizeT: 201, umPerPx: 0.596 }

describe('anisoGridEstimate', () => {
  it('converts µm → px with the image pixel size and grids the frame', () => {
    const e = anisoGridEstimate(5, EAMAVQ)!
    expect(e.boxPx).toBe(8)                        // 5 / 0.596 = 8.39 → 8
    expect(e.grid).toEqual([68, 69])               // ceil(544/8), ceil(548/8)
    expect(e.frames).toBe(201)
    expect(e.clamped).toBe(false)
  })

  it('is the SAME cost model as the Julia handler', () => {
    // `_aniso_grid_bytes(n_boxes, n_frames) = n_boxes * 40 * n_frames` in branching.jl. Pinned on
    // both sides for the same input: 1296 boxes (a 36×36 grid) over 201 frames.
    expect(1296 * ANISO_BYTES_PER_BOX_PER_FRAME * 201).toBe(10_419_840)
    expect(ANISO_BYTES_PER_BOX_PER_FRAME).toBe(40)
    expect(ANISO_WARN_BYTES).toBe(100_000_000)     // _ANISO_SIDECAR_WARN_BYTES
    expect(ANISO_MIN_BOX_PX).toBe(3)               // _ANISO_MIN_BOX_PX
  })

  it('bytes scale as 1/spacing² — halving the spacing quadruples the file', () => {
    const coarse = anisoGridEstimate(10, EAMAVQ)!
    const fine   = anisoGridEstimate(5, EAMAVQ)!
    // not exactly 4x because the box is rounded to whole pixels and the grid ceils, so assert the
    // relationship rather than a bogus exact multiple
    const ratio = fine.bytes / coarse.bytes
    expect(ratio).toBeGreaterThan(3.5)
    expect(ratio).toBeLessThan(4.5)
  })

  it('clamps a sub-pixel spacing and flags that it did', () => {
    const e = anisoGridEstimate(0.5, EAMAVQ)!      // 0.5 / 0.596 = 0.84 px
    expect(e.boxPx).toBe(ANISO_MIN_BOX_PX)
    expect(e.clamped).toBe(true)
  })

  it('treats µm as px on an uncalibrated image — matching the Julia fallback', () => {
    const e = anisoGridEstimate(8, { sizeX: 548, sizeY: 544, sizeT: 1, umPerPx: null })!
    expect(e.boxPx).toBe(8)                        // no conversion applied
    expect(e.frames).toBe(1)
  })

  it('returns null when the geometry is missing rather than guessing', () => {
    // an image imported before SizeX/SizeY were recorded — a metadata resync backfills them
    expect(anisoGridEstimate(5, { sizeX: 0, sizeY: 0, sizeT: 10, umPerPx: 0.5 })).toBeNull()
    expect(anisoGridEstimate(5, undefined)).toBeNull()
    expect(anisoGridEstimate(0, EAMAVQ)).toBeNull()
    expect(anisoGridEstimate(NaN, EAMAVQ)).toBeNull()
  })

  it('a single 2D frame is cheap at a fine spacing — the reason a static threshold would be wrong', () => {
    const still = anisoGridEstimate(1, { sizeX: 512, sizeY: 512, sizeT: 1, umPerPx: 0.5 })!
    expect(still.bytes).toBeLessThan(ANISO_WARN_BYTES)
    const movie = anisoGridEstimate(1, EAMAVQ)!
    expect(movie.bytes).toBeGreaterThan(ANISO_WARN_BYTES)
  })
})

describe('anisoGridAdvisory', () => {
  it('reports the grid and size, ok at the default', () => {
    const a = anisoGridAdvisory(5, EAMAVQ)!
    expect(a.severity).toBe('ok')
    expect(a.message).toContain('69×68')
    expect(a.message).toMatch(/MB/)
    expect(a.tip).toContain('1/spacing²')
  })

  it('warns when the stored grid gets large, without forbidding it', () => {
    // 2 µm / 0.596 = 3.36 px — fine, but NOT clamped, so this exercises the size branch alone
    const a = anisoGridAdvisory(2, EAMAVQ)!
    expect(a.severity).toBe('warn')
    expect(a.message).toContain('large')
    expect(a.tip).toContain('not a limit')
  })

  it('when a spacing is BOTH clamped and huge, the clamp wins but the size is still shown', () => {
    // 1 µm on this image is 1.68 px → clamped. The clamp is the more fundamental fact (the setting
    // is not being honoured at all), but the user still needs the size, so the line carries both.
    const a = anisoGridAdvisory(1, EAMAVQ)!
    expect(a.severity).toBe('warn')
    expect(a.message).toContain('clamped')
    expect(a.message).toMatch(/MB/)
  })

  it('warns about a clamp, naming the pixel size that caused it', () => {
    const a = anisoGridAdvisory(0.5, EAMAVQ)!
    expect(a.severity).toBe('warn')
    expect(a.message).toContain('clamped')
    expect(a.tip).toContain('0.596')
  })

  it('flags calibration as a DATA-quality signal, separate from the advice', () => {
    // ok setting + bad data: the message is an ordinary "this is fine", but the flag says the µm
    // you typed are not µm at all. Changing the spacing would not fix that — hence a flag.
    const uncal = anisoGridAdvisory(8, { sizeX: 512, sizeY: 512, sizeT: 1, umPerPx: null })!
    expect(uncal.severity).toBe('ok')
    expect(uncal.flag!.severity).toBe('warn')
    expect(uncal.flag!.tip).toContain('read as pixels')

    const cal = anisoGridAdvisory(5, EAMAVQ)!
    expect(cal.flag!.severity).toBe('ok')
    expect(cal.flag!.tip).toContain('0.596')
  })

  it('keeps the flag on every branch, including the warning ones', () => {
    for (const v of [5, 2, 1]) {
      expect(anisoGridAdvisory(v, EAMAVQ)!.flag).toBeDefined()
    }
  })

  it('says nothing when it cannot compute', () => {
    expect(anisoGridAdvisory(5, null)).toBeNull()
  })
})

describe('motionDimsAdvisory', () => {
  const M = (over: Record<string, unknown> = {}) => ({
    dims: 3, zUsed: true, confidence: 'high', reason: '',
    metrics: { nSteps: 500, autocorrZ: 0.4, persist2D: 0.5, persist3D: 0.6 }, ...over,
  }) as Parameters<typeof motionDimsAdvisory>[1]

  it('3D with high confidence is ok', () => {
    const a = motionDimsAdvisory('auto', M())!
    expect(a.severity).toBe('ok')
    expect(a.message).toContain('3D recommended')
  })

  it('2D with reversing z explains the jitter in its tip', () => {
    const a = motionDimsAdvisory('auto', M({ dims: 2, confidence: 'high',
      metrics: { nSteps: 500, autocorrZ: -0.2 } }))!
    expect(a.message).toContain('2D recommended')
    expect(a.tip).toContain('jitter')
  })

  it('2D that merely missed the cutoff is a warn', () => {
    const a = motionDimsAdvisory('auto', M({ dims: 2, confidence: 'low',
      metrics: { nSteps: 500, autocorrZ: 0.05 } }))!
    expect(a.severity).toBe('warn')
    expect(a.message).toContain('2D recommended')
  })

  it('too few steps says so instead of implying a verdict', () => {
    const a = motionDimsAdvisory('auto', M({ metrics: { nSteps: 12 } }))!
    expect(a.severity).toBe('warn')
    expect(a.message).toContain('too few steps')
    expect(a.tip).toContain('12 track steps')
  })

  it('an explicit override reports what it is doing, and does not nag', () => {
    const a = motionDimsAdvisory('2D', M())!
    expect(a.severity).toBe('ok')
    expect(a.message).toContain('using 2D')
  })

  it('carries a SEPARATE z-quality flag — merging it into the severity loses information', () => {
    // this was lost when the advisory was first generalised and had to be restored: the
    // recommendation ("2D recommended", a mild note) and the data quality ("z is jitter", severe)
    // are two different statements and the user reads both at a glance.
    const jitter = motionDimsAdvisory('auto', M({ dims: 2, confidence: 'high',
      metrics: { nSteps: 500, autocorrZ: -0.2 } }))!
    expect(jitter.severity).toBe('warn')                 // the recommendation itself is just a note
    expect(jitter.flag!.severity).toBe('fail')           // the z-axis, though, is bad
    expect(jitter.flag!.tip).toContain('jitter')

    const good = motionDimsAdvisory('auto', M())!
    expect(good.flag!.severity).toBe('ok')
    expect(good.flag!.tip).toContain('real migration')

    const borderline = motionDimsAdvisory('auto', M({ confidence: 'low' }))!
    expect(borderline.flag!.severity).toBe('warn')
    expect(borderline.flag!.tip).toContain('borderline')
  })

  it('no flag when the user overrode the recommendation, or when there is nothing to assess', () => {
    expect(motionDimsAdvisory('2D', M())!.flag).toBeUndefined()
    expect(motionDimsAdvisory('auto', M({ metrics: { nSteps: 12 } }))!.flag).toBeUndefined()
  })

  it('no assessment → no advisory', () => {
    expect(motionDimsAdvisory('auto', null)).toBeNull()
  })
})

// ── image version ──────────────────────────────────────────────────────────────────────────────
//
// The case this exists for, from live data: `WIaUjL/p6t4mC` is active on `afCorrected` (605x617,
// drift-corrected) and was re-segmented on `default` (the 512x512 raw import). The run said done and
// the labels were displaced in XY in the viewer, because a 512x512 label store was laid over a
// 605x617 image. Both versions are used throughout as the realistic pair.
const P6T4MC = {
  uid: 'p6t4mC',
  activeValueName: 'afCorrected',
  filepaths: { default: 'ccidImage.ome.zarr', driftCorrected: 'd.zarr', afCorrected: 'af.zarr' },
}

describe('imageVersionAdvisory', () => {
  it('warns, and names the version to switch to, when the pick is not the active one', () => {
    const a = imageVersionAdvisory('default', [P6T4MC])!
    expect(a.severity).toBe('warn')
    expect(a.message).toContain('afCorrected')       // the fix is IN the one line, not only the tip
    expect(a.tip).toContain('afCorrected')
  })

  it('confirms the ordinary case rather than going quiet on it', () => {
    // silence here would be ambiguous with "this widget has no advisory" — the picker should say
    // which of the two states it is in
    const a = imageVersionAdvisory('afCorrected', [P6T4MC])!
    expect(a.severity).toBe('ok')
    expect(a.message).toBe('active version')
  })

  it('says nothing when there is only one version — nothing to get wrong', () => {
    expect(imageVersionAdvisory('default', [
      { uid: 'a', activeValueName: 'default', filepaths: { default: 'i.zarr' } },
    ])).toBeNull()
  })

  it('says nothing about a name no selected image has — an upstream chain output, not a mistake', () => {
    // `ParamContext.extraValueNames`: a whiteboard node's output ("cpCorrected") is selectable
    // before it exists on disk. There is nothing to compare it to, so silence is the honest answer.
    expect(imageVersionAdvisory('cpCorrected', [P6T4MC])).toBeNull()
  })

  it('counts the images that are off, rather than judging the selection by its first image', () => {
    const onIt = { uid: 'b', activeValueName: 'default', filepaths: P6T4MC.filepaths }
    const a = imageVersionAdvisory('default', [P6T4MC, onIt, { ...P6T4MC, uid: 'c' }])!
    expect(a.severity).toBe('warn')
    expect(a.message).toBe('not the active version on 2 of 3 images')
  })

  it('reports the spread instead of offering one image\'s version to all of them', () => {
    const other = { uid: 'b', activeValueName: 'driftCorrected', filepaths: P6T4MC.filepaths }
    const a = imageVersionAdvisory('default', [P6T4MC, other])!
    expect(a.message).toBe('not the active version')          // no single name to offer
    expect(a.tip).toContain('2 different versions')
    expect(a.tip).not.toContain('Pick "')
  })

  it('treats a missing active name as `default` — an image that never had a version registered', () => {
    const raw = { uid: 'z', filepaths: { default: 'i.zarr', smoothed: 's.zarr' } }
    expect(imageVersionAdvisory('default', [raw])!.severity).toBe('ok')
    expect(imageVersionAdvisory('smoothed', [raw])!.severity).toBe('warn')
  })

  it('has nothing to say with no images, no value, or a non-string value', () => {
    expect(imageVersionAdvisory('default', [])).toBeNull()
    expect(imageVersionAdvisory('default', undefined)).toBeNull()
    expect(imageVersionAdvisory('', [P6T4MC])).toBeNull()
    expect(imageVersionAdvisory(null, [P6T4MC])).toBeNull()
  })
})

describe('the valueNameSelection advisor only speaks about IMAGE VERSIONS', () => {
  const run = (field: string | undefined) =>
    paramAdvisor({ type: 'valueNameSelection', key: 'valueName', field })!
      .advise('default', { images: [P6T4MC] }, { type: 'valueNameSelection', key: 'valueName', field })

  it('advises on `filepaths`, and on an absent field (which means filepaths)', async () => {
    expect((await run('filepaths'))!.severity).toBe('warn')
    expect((await run(undefined))!.severity).toBe('warn')
  })

  it('stays silent on label sets and spatial graphs — neither has an "active"', async () => {
    // same widget, same key (`valueName` on branching / bayesianTracking / trackMeasures), different
    // meaning. Comparing a label-set name to the image's active VERSION would warn on every one.
    expect(await run('labels')).toBeNull()
    expect(await run('spatialGraphs')).toBeNull()
  })

  it('is the same test the picker preselects with — one answer, not two', () => {
    // `preferredValueName` selects the active version for exactly the fields this advises on. If
    // these two ever disagree, the form preselects a value its own advisory then warns about.
    for (const field of ['filepaths', undefined, 'labels', 'spatialGraphs'] as const) {
      const advises = isImageVersionField(field)
      expect(preferredValueName(['default', 'afCorrected'], field, 'afCorrected'))
        .toBe(advises ? 'afCorrected' : 'default')
    }
  })
})

describe('registry lookup', () => {
  it('resolves by param TYPE first — types are global, keys are per-task', () => {
    // the motion param's key is just `dims`, which another task could reuse; its TYPE is unique
    expect(paramAdvisor({ type: 'motionDimsSelection', key: 'dims' })).toBeDefined()
    expect(paramAdvisor({ type: 'select', key: 'dims' })).toBeUndefined()
  })

  it('falls back to the key for a one-off advisor on a plain widget', () => {
    expect(paramAdvisor({ type: 'float', key: 'anisotropyBoxUm' })).toBeDefined()
    expect(paramAdvisor({ type: 'float', key: 'preDilationSize' })).toBeUndefined()
  })

  it('every advisor exposes the same single entry point', () => {
    expect(paramAdvisor({ type: 'float', key: 'anisotropyBoxUm' })!.advise).toBeTypeOf('function')
    expect(paramAdvisor({ type: 'motionDimsSelection' })!.advise).toBeTypeOf('function')
  })
})

describe('formatBytes', () => {
  it('reads as a size a person recognises', () => {
    expect(formatBytes(37_000_000)).toBe('37 MB')
    expect(formatBytes(940_000)).toBe('940 kB')
    expect(formatBytes(2_500_000_000)).toBe('2.5 GB')
    expect(formatBytes(512)).toBe('512 B')
  })
})

// ── template structure guard ───────────────────────────────────────────────────────────────────
//
// Not unit-testable through the module: this is about WHERE the advisory renders in ParamRenderer's
// template. It earned a test the hard way. The advisory block was first inserted in the middle of the
// widget `v-if`/`v-else-if` chain, which starts a NEW chain — so every `v-else-if`/`v-else` below it
// re-parented onto `advisoryLoading`, and the chain's final `v-else` fallback ("unsupported type" +
// spinner) rendered under EVERY param on every task page. Typecheck and the unit tests were all green;
// only opening the app showed it.
const SFC = import.meta.glob('/src/tasks/ParamRenderer.vue', {
  query: '?raw', import: 'default', eager: true,
}) as Record<string, string>

describe('ParamRenderer advisory placement', () => {
  const src = Object.values(SFC)[0] ?? ''

  it('renders the advisory AFTER the widget chain closes, not inside it', () => {
    expect(src).not.toBe('')
    const fallback = src.indexOf('class="picker-placeholder"')      // the chain's final v-else
    const advisory = src.indexOf('v-if="advisoryLoading"')
    expect(fallback).toBeGreaterThan(-1)
    expect(advisory).toBeGreaterThan(-1)
    // a v-if BEFORE the fallback would break the chain and make the fallback catch everything
    expect(advisory).toBeGreaterThan(fallback)
  })

  it('keeps the widget chain contiguous — nothing starts a second chain inside it', () => {
    const start = src.indexOf("v-if=\"param.type === 'int'")        // first link
    const end   = src.indexOf('class="picker-placeholder"')          // last link
    const chain = src.slice(start, end)
    // `v-if` inside the chain is fine only where it opens a NESTED element (indented deeper, e.g.
    // the empty-state inside channelSelection). A `v-if` at the chain's own indent level is the bug.
    const siblingIf = chain.split('\n').filter(l => /^ {4}<\w[^>]*\sv-if=/.test(l))
    expect(siblingIf).toEqual([])
  })
})
