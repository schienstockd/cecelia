/**
 * The smoothing figure's geometry, tested without mounting anything — the same rule the rest of the
 * figure follows (`paramVis.test.ts`).
 *
 * The claims worth pinning are the ones a reader of the FIGURE would take away, because those are the
 * ones that would be a lie if the maths drifted: the gate keeps the moving spot, the median does not,
 * both are shown the same input, and the gate's floor is the identity.
 */
import { describe, expect, it } from 'vitest'
import {
  amplitudeGap, farnebackSequence, gatedCost, gatedSequence, medianSequence, motionSequence,
  noiseSigma, smoothFigure, smoothVerdict, smoothVisColumns, GAP_WORTH_PAYING_FOR,
  bilateralPass, blur, sparseCellsFrame, smoothSpatialFigure, smoothSpatialVisColumns,
  SMOOTH_SPATIAL_METHODS,
} from './smoothVis'
import type { VisFrame } from './paramVis'

/** The brightest cell of a frame — where the punctum is, and how much of it survived. */
const peak = (f: VisFrame) => Math.max(...f.map(r => Math.max(...r)))

/** The centre frame of the loop, away from the clamped edges. */
const MID = 6

describe('motionSequence', () => {
  it('is deterministic — two renders of the figure are the same figure', () => {
    expect(motionSequence()).toEqual(motionSequence())
  })

  it('actually moves the spot', () => {
    const seq = motionSequence()
    const argmax = (f: VisFrame) => {
      let best = -Infinity, at = [0, 0]
      f.forEach((row, y) => row.forEach((v, x) => { if (v > best) { best = v; at = [y, x] } }))
      return at
    }
    expect(argmax(seq[MID])).not.toEqual(argmax(seq[0]))
  })
})

describe('median vs gated', () => {
  const seq = motionSequence()

  it('the median dims the moving spot and the gate keeps it', () => {
    const med = medianSequence(seq, 5)
    const gat = gatedSequence(seq, 5)
    // The claim the whole figure exists to make. Not a tight bound — the point is the DIRECTION, and
    // a bound tight enough to pin the exact values would break on any tuning of the schematic.
    expect(peak(med[MID])).toBeLessThan(peak(seq[MID]) * 0.9)
    expect(peak(gat[MID])).toBeGreaterThan(peak(med[MID]))
    expect(peak(gat[MID])).toBeGreaterThan(peak(seq[MID]) * 0.9)
  })

  it('both denoise the static background', () => {
    const bgSd = (f: VisFrame) => {
      const vals = f.slice(12).flat()                    // rows the spot never reaches
      const m = vals.reduce((a, b) => a + b, 0) / vals.length
      return Math.sqrt(vals.reduce((a, b) => a + (b - m) ** 2, 0) / vals.length)
    }
    expect(bgSd(medianSequence(seq, 5)[MID])).toBeLessThan(bgSd(seq[MID]))
    expect(bgSd(gatedSequence(seq, 5)[MID])).toBeLessThan(bgSd(seq[MID]))
  })

  it('window 1 is the identity for both — the temporal term is off', () => {
    expect(medianSequence(seq, 1)[MID]).toEqual(seq[MID])
    expect(gatedSequence(seq, 1)[MID]).toEqual(seq[MID])
  })

  it("the gate's worst case is the identity, never a blur", () => {
    // Nothing agrees with anything: every frame independent noise, so no patch matches and the
    // weights collapse. This is the property that makes `gated` safe to offer at all.
    const chaos: VisFrame[] = Array.from({ length: 12 }, (_, t) =>
      Array.from({ length: 16 }, (_, y) =>
        Array.from({ length: 16 }, (_, x) => ((t * 7 + y * 13 + x * 29) % 17) / 17)))
    const out = gatedSequence(chaos, 5)[MID]
    out.forEach((row, y) => row.forEach((v, x) => {
      expect(Math.abs(v - chaos[MID][y][x])).toBeLessThan(0.2)
    }))
  })

  it('a noiseless sequence does not divide by zero', () => {
    const flat: VisFrame[] = Array.from({ length: 12 }, () =>
      Array.from({ length: 16 }, () => new Array(16).fill(0.5)))
    expect(noiseSigma(flat)).toBe(0)
    expect(gatedSequence(flat, 5)[MID].flat().every(Number.isFinite)).toBe(true)
  })
})

describe('smoothVisColumns', () => {
  const vis = smoothVisColumns({ frames: 5, sigma: 1, planes: 600, channels: 2, farnebackMaxShiftPx: 8 })

  it('shows the input BESIDE the three methods, not stranded above them', () => {
    // Spanning it over the triple was true — one sequence, shared — but it read as a fourth thing
    // floating over them, and the eye cannot compare pictures it has to travel between.
    expect(vis.columns).toEqual(['input', 'median', 'gated', 'farneback'])
    expect(vis.rows.find(r => r.key === 'motion')).toBeUndefined()
    expect(vis.rows.find(r => r.key === 'result')!.cells).toHaveLength(4)
  })

  it('still shows ONE input — the same sequence, drawn where the comparison happens', () => {
    const [input, med] = vis.rows.find(r => r.key === 'result')!.cells
    expect(input.frames).toHaveLength(med.frames!.length)
    // it is the INPUT, so it is what the others were made from, not a fifth thing
    expect(input.frames![MID]).not.toEqual(med.frames![MID])
  })

  it('gives every method the same window, and does not flag it as a problem', () => {
    const w = vis.rows.find(r => r.key === 'window')!
    expect(w.cells[0].text).toBe('')                 // the input is what the window is applied TO
    expect(w.cells[1].text).toBe(w.cells[2].text)
    expect(w.cells[2].text).toBe(w.cells[3].text)
    // `uniform` colours a label as a warning. Identical windows are the POINT here, not the failure
    // it marks in the segmentation strip.
    expect(vis.rows.every(r => !r.uniform)).toBe(true)
    expect(vis.uniformKeys).toEqual([])
  })

  it('shows the max-shift knob only in the farneback column', () => {
    // Mirrors the JSON's `showIf: temporalStat=farneback`: the slider has a home in the figure, but
    // it does not clutter the columns it does not apply to.
    const c = vis.rows.find(r => r.key === 'clamp')!.cells
    expect(c[0].text).toBe('')
    expect(c[1].text).toBe('')
    expect(c[2].text).toBe('')
    expect(c[3].text).toBe('8px')
  })

  it('animates every output off the same clock', () => {
    const result = vis.rows.find(r => r.key === 'result')!
    expect(new Set(result.cells.map(c => c.frames!.length)).size).toBe(1)
    expect(result.cells[0].frames!.length).toBeGreaterThan(1)
  })

  it('normalises every grid against ONE peak, so the dimming survives the drawing', () => {
    const all = vis.rows.filter(r => r.role === 'grid').flatMap(r => r.cells)
    expect(Math.max(...all.flatMap(c => c.frames!.flatMap(f => f.flat())))).toBeCloseTo(1, 5)
    const [, med, gat] = vis.rows.find(r => r.key === 'result')!.cells
    expect(peak(gat.frames![MID])).toBeGreaterThan(peak(med.frames![MID]))
  })

  it('every frame value is a drawable opacity', () => {
    for (const r of vis.rows.filter(r => r.role === 'grid')) {
      for (const c of r.cells) {
        for (const f of c.frames!) {
          for (const v of f.flat()) expect(v).toBeGreaterThanOrEqual(0), expect(v).toBeLessThanOrEqual(1)
        }
      }
    }
  })
})

describe('farneback (flow-warped) column', () => {
  const seq = motionSequence()

  it('keeps the moving spot bright — the argument for offering it', () => {
    // Same claim shape as median-vs-gated: DIRECTION, not a tight bound.
    const med = medianSequence(seq, 5)
    const fw = farnebackSequence(seq, 5, 8)
    expect(peak(fw[MID])).toBeGreaterThan(peak(med[MID]))
  })

  it('collapses to the identity per-neighbour when the clamp is tight', () => {
    // A per-pixel clamp of 0 means any warp exceeds it; every neighbour falls back to source, and
    // farneback becomes a plain mean over the window. Not identical to median — this is the failure
    // mode the knob has to be able to expose in the figure.
    const zero = farnebackSequence(seq, 5, 0)
    // A plain window mean dims the moving spot MORE than the median does (the median rejects the
    // off-centre frames as outliers; the mean averages them in).
    expect(peak(zero[MID])).toBeLessThan(peak(medianSequence(seq, 5)[MID]))
  })

  it('window 1 is the identity — the temporal term is off', () => {
    expect(farnebackSequence(seq, 1, 8)[MID]).toEqual(seq[MID])
  })
})

describe('the verdict under the figure', () => {
  const base = { sigma: 1, planes: 600, channels: 2, farnebackMaxShiftPx: 8 }

  it('says the median is enough at the DEFAULT window, where the three agree', () => {
    // The case a user must not misread: near-identical grids are the answer, not a broken figure.
    expect(smoothFigure({ ...base, frames: 3 }).note).toBe('Median is enough at this window')
  })

  it('names a motion-compensated method at the windows where the median smears', () => {
    for (const frames of [5, 9]) {
      const note = smoothFigure({ ...base, frames }).note
      expect(note).toMatch(/(Gated|Flow-warp) keeps what the median smears/)
    }
  })

  it('widens with the window, which is the whole shape of the choice', () => {
    const gapAt = (frames: number) => {
      const seq = motionSequence()
      return amplitudeGap(medianSequence(seq, frames), gatedSequence(seq, frames))
    }
    expect(gapAt(3)).toBeLessThan(gapAt(5))
    expect(gapAt(5)).toBeLessThan(gapAt(9))
    expect(gapAt(1)).toBe(0)                     // window 1 is the identity for both
  })

  it('is read off the FRAMES being drawn, so the line cannot contradict the picture', () => {
    const fig = smoothFigure({ ...base, frames: 9 })
    // by NAME: adding the input as a column once repointed this at input-vs-median, and the figure
    // said "median is enough" over a picture of the median smearing
    const at = (c: string) => fig.vis.rows.find(r => r.key === 'result')!
      .cells[fig.vis.columns.indexOf(c)].frames!
    const gapGate = amplitudeGap(at('median'), at('gated'))
    const gapFlow = amplitudeGap(at('median'), at('farneback'))
    expect(fig.note).toBe(smoothVerdict(gapGate, gapFlow))
  })

  it('narrows as the Gaussian widens — a blurred spot smears less visibly', () => {
    const gapAt = (sigma: number) => {
      const fig = smoothFigure({ ...base, sigma, frames: 5 })
      const at = (c: string) => fig.vis.rows.find(r => r.key === 'result')!
        .cells[fig.vis.columns.indexOf(c)].frames!
      return amplitudeGap(at('median'), at('gated'))
    }
    expect(gapAt(2)).toBeLessThan(gapAt(1))
    expect(gapAt(1)).toBeLessThan(gapAt(0))
  })

  it('turns over at the threshold, not somewhere near it', () => {
    expect(smoothVerdict(GAP_WORTH_PAYING_FOR - 0.001, 0)).toContain('Median is enough')
    expect(smoothVerdict(GAP_WORTH_PAYING_FOR, 0)).toContain('Gated keeps')
  })

  it('names flow-warp when it beats gated by amplitude', () => {
    expect(smoothVerdict(GAP_WORTH_PAYING_FOR, GAP_WORTH_PAYING_FOR + 0.05)).toContain('Flow-warp')
    expect(smoothVerdict(GAP_WORTH_PAYING_FOR + 0.05, GAP_WORTH_PAYING_FOR)).toContain('Gated')
  })
})

describe('spatial method figure', () => {
  const input = sparseCellsFrame()

  it('is deterministic', () => {
    expect(sparseCellsFrame()).toEqual(input)
  })

  it('contains distinct cell peaks and isolated specks', () => {
    // The whole point of the schematic — puncta the bilateral can preserve and specks it should
    // remove. If either goes away the figure has nothing to compare.
    const peaks = input.flat().filter(v => v > 0.7)
    expect(peaks.length).toBeGreaterThan(3)
  })

  it('bilateral keeps cell peaks better than a gaussian at matched settings', () => {
    // With the same spatial reach and a sensible color tolerance, bilateral preserves the two cell
    // peaks better than a plain gaussian — the property that makes it worth offering here at all.
    const bl = bilateralPass(input, 0.3, 1.5)
    const g  = blur(input, 1.5)
    const cellPeakOf = (f: number[][]) => Math.max(...f.slice(3, 8).map(r => Math.max(...r)))
    expect(cellPeakOf(bl)).toBeGreaterThan(cellPeakOf(g))
  })

  it('bilateral flattens the background — its whole reason to exist', () => {
    const bl = bilateralPass(input, 0.3, 1.5)
    const bgSd = (f: number[][]) => {
      const vals = f.slice(0, 3).flat()
      const m = vals.reduce((a, b) => a + b, 0) / vals.length
      return Math.sqrt(vals.reduce((a, b) => a + (b - m) ** 2, 0) / vals.length)
    }
    expect(bgSd(bl)).toBeLessThan(bgSd(input))
  })

  it('a huge color tolerance collapses to a gaussian, since no neighbour is "different"', () => {
    // A sanity check that the color weight is doing what the docstring says.
    const bl = bilateralPass(input, 1e6, 1.5)
    // Every neighbour weighted equally by color → same as a spatial gaussian; a plain average of
    // the whole 3-radius neighbourhood will heavily reduce isolated speck intensity
    expect(Math.max(...bl.flat())).toBeLessThan(Math.max(...input.flat()))
  })

  it('smoothSpatialVisColumns exposes the two methods beside the input', () => {
    const vis = smoothSpatialVisColumns({
      method: 'bilateral_vst', sigma: 1, bilateralColor: 10, bilateralReach: 3, bilateralPolish: 0.6,
    })
    expect(vis.columns).toEqual(['input', ...SMOOTH_SPATIAL_METHODS])
    expect(vis.rows.find(r => r.key === 'result')!.cells).toHaveLength(3)
  })

  it('smoothSpatialFigure carries a verdict line about the CHOICE', () => {
    // Sparse-count regime with a bilateral, polish OFF: it should say bilateral is the pick, not
    // that gaussian is enough — since the whole schematic exists to show what gaussian smears.
    // (Polish is a within-bilateral refinement; testing with it OFF isolates the gaussian-vs-
    // bilateral verdict from a change to the polish default.)
    const fig = smoothSpatialFigure({
      method: 'bilateral_vst', sigma: 1, bilateralColor: 10, bilateralReach: 3, bilateralPolish: 0,
    })
    expect(fig.note.toLowerCase()).toContain('bilateral')
  })

  it('polish σ changes the bilateral column and its params text', () => {
    // The whole point of putting polish in the visual aid: moving the slider must move something
    // visible. The polished bilateral must differ from the bare one, and the params row must name
    // the polish value so the picture and the label agree.
    const bare = smoothSpatialVisColumns({
      method: 'bilateral_vst', sigma: 1, bilateralColor: 10, bilateralReach: 3, bilateralPolish: 0,
    })
    const polished = smoothSpatialVisColumns({
      method: 'bilateral_vst', sigma: 1, bilateralColor: 10, bilateralReach: 3, bilateralPolish: 0.8,
    })
    const blCol = SMOOTH_SPATIAL_METHODS.indexOf('bilateral_vst') + 1  // +1 for the input column
    const bareBl = bare.rows.find(r => r.key === 'result')!.cells[blCol].frames![0]
    const polBl  = polished.rows.find(r => r.key === 'result')!.cells[blCol].frames![0]
    // Not byte-identical — the polish had a visible effect
    let differ = false
    for (let y = 0; y < bareBl.length && !differ; y++) {
      for (let x = 0; x < bareBl[y].length; x++) {
        if (Math.abs(bareBl[y][x] - polBl[y][x]) > 1e-6) { differ = true; break }
      }
    }
    expect(differ).toBe(true)
    // The polish row names the state — "off" when 0, an "px" reading otherwise. One row per param,
    // same layout as the temporal figure's window/cost rows.
    const barePolish = bare.rows.find(r => r.key === 'polish')!.cells[blCol].text
    const polPolish  = polished.rows.find(r => r.key === 'polish')!.cells[blCol].text
    expect(barePolish).toBe('off')
    expect(polPolish).toBe('0.8px')
  })
})

describe('gatedCost', () => {
  it('is the size of YOUR movie, not the one it was measured on', () => {
    // 180t x 19z x 1ch at 0.12 s/plane is #554's ~7 min — the rate is the constant, the size is not.
    expect(gatedCost(180 * 19, 1)).toBe('~7 min')
    expect(gatedCost(31 * 32, 4)).toBe('~8 min')
  })

  it('falls back to the RATE, not to a shrug, when no image is selected yet', () => {
    // The ordinary state of opening the figure to decide before ticking anything. "minutes" was the
    // shape of an answer without being one.
    expect(gatedCost(null, 2)).toBe('0.12 s / plane')
    expect(gatedCost(0, 2)).toBe('0.12 s / plane')
  })

  it('does not print "~0 min" for a small stack', () => {
    expect(gatedCost(10, 1)).toBe('~1 s')
  })
})
