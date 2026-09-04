/**
 * The `PARAM_FIGURES` registry — one test per figure builder wiring that a caller relies on.
 *
 * The claims worth pinning are the ones a reader of the FIGURE cannot verify: that `driftEstimator`
 * hangs a CTA on the 4th column (`ask3d`) rather than any other, and that clicking that CTA opens
 * the Call for Datasets modal focused on the matching card. Renaming the ask id, dropping the
 * CTA, or moving it to the wrong column all fail here rather than silently.
 */
import { afterEach, describe, expect, it } from 'vitest'
import { PARAM_FIGURES } from './paramFigures'
import type { AdvisorContext } from './paramAdvisors'
import {
  isCallForDatasetsOpen, callForDatasetsFocusId, closeCallForDatasets,
} from '../lib/callForDatasetsOpen'
import { findAsk } from '../lib/callForDatasets'

const ctx: AdvisorContext = {
  images: [],
  values: { driftMaxLag: 3, driftMaxAngle: 5 },
} as unknown as AdvisorContext

afterEach(() => { closeCallForDatasets() })

describe('driftEstimator figure', () => {
  it('mounts a CTA on the 3D-full column and nowhere else', () => {
    const def = PARAM_FIGURES.driftEstimator(ctx)
    // Four columns → four slots. The first three columns are the shipped estimators; only the
    // deferred `ask3d` column carries a CTA. Any other layout means either a spurious chip or a
    // missing one.
    expect(def.columnCtas).toBeDefined()
    expect(def.columnCtas).toHaveLength(4)
    expect(def.columnCtas![0]).toBeFalsy()
    expect(def.columnCtas![1]).toBeFalsy()
    expect(def.columnCtas![2]).toBeFalsy()
    expect(def.columnCtas![3]).toBeTruthy()
    expect(def.columnCtas![3]!.text).toBe('Request')
  })

  it("clicking the CTA opens Call for Datasets at the sitk-rigid-3d-full card", () => {
    const def = PARAM_FIGURES.driftEstimator(ctx)
    expect(isCallForDatasetsOpen.value).toBe(false)
    def.columnCtas![3]!.onClick()
    expect(isCallForDatasetsOpen.value).toBe(true)
    expect(callForDatasetsFocusId.value).toBe('sitk-rigid-3d-full')
    // …and that id resolves in the registry. A CTA pointing at a non-existent ask would open the
    // modal on nothing, which reads as a broken link.
    expect(findAsk(callForDatasetsFocusId.value)?.id).toBe('sitk-rigid-3d-full')
  })
})
