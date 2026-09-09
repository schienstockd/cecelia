// Convergence detection for the Training convergence plot's Detail view.
//
// Why this exists: SUPPORT (and any self-supervised denoiser with a shot-noise-limited target)
// typically converges to its plateau within the first ~100 gradient steps and then wanders inside
// a ±0.01 band for the remaining ~11k steps of a MERTK-large epoch. The per-epoch mean buries
// that — a converged run reads as a flat line and looks like nothing happened. The Detail view
// plots the sub-epoch trace on a log(step) axis and annotates the plateau explicitly:
//
//   - `initial`     — moving-avg loss over the first few points, i.e. the effectively-random
//                     starting loss.
//   - `plateau`     — moving-avg loss over the tail; where the training settled.
//   - `convergedAt` — the step at which the moving avg first crosses the midpoint of those two.
//                     A "you can stop reading here" marker for the reviewer.
//
// Pure, extracted from FlowTrainingView per the frontend test rule (SFCs aren't tested; utilities
// are). Tested in `convergence.test.ts`.

export interface ConvergencePoints {
  step: number
  loss: number
}

export interface ConvergenceStats {
  /** Simple moving average of `loss` over `window` samples (same length as input minus window+1). */
  ma: number[]
  /** Steps corresponding to `ma` — matches `steps[window-1..]`. */
  maSteps: number[]
  /** Mean of the first `initialSamples` MA points. */
  initial: number
  /** Mean of the last `plateauSamples` MA points. */
  plateau: number
  /** 1-based step at which `ma` first hit `(initial + plateau) / 2`, or `null` if it never did. */
  convergedAt: number | null
  /** Fractional loss drop from initial to plateau, `1 − plateau/initial`; NaN if `initial == 0`. */
  dropFraction: number
}

export interface ConvergenceOptions {
  /** Moving-average window in samples (default 20). Smaller = jumpier, larger = laggier. */
  window?: number
  /** Number of MA samples averaged for `initial` (default 5). */
  initialSamples?: number
  /** Number of MA samples averaged for `plateau` (default 100). Capped at MA length. */
  plateauSamples?: number
}

/**
 * Simple moving average — returns an array of length `xs.length - window + 1`. Same shape as
 * `numpy.convolve(mode='valid')`; the caller is responsible for aligning to steps (the first MA
 * point corresponds to `steps[window - 1]`, not `steps[0]`).
 */
export function movingAverage(xs: number[], window: number): number[] {
  if (window < 1 || xs.length < window) return []
  const out: number[] = new Array(xs.length - window + 1)
  let sum = 0
  for (let i = 0; i < window; i++) sum += xs[i]
  out[0] = sum / window
  for (let i = window; i < xs.length; i++) {
    sum += xs[i] - xs[i - window]
    out[i - window + 1] = sum / window
  }
  return out
}

/**
 * Compute the convergence summary for a sub-epoch loss trace. `steps` and `losses` must be the
 * same length and `steps` must be strictly increasing (as `train_support_denoise_run.py` writes
 * them). Returns a summary the plot can render directly — see `ConvergenceStats`.
 *
 * Handles the too-short case by returning `null`, so the caller can fall back to the per-epoch
 * curve rather than draw a meaningless mid-point marker on a 3-point trace.
 */
export function convergenceStats(
  steps: number[],
  losses: number[],
  opts: ConvergenceOptions = {},
): ConvergenceStats | null {
  const window = Math.max(1, opts.window ?? 20)
  const initialSamples = Math.max(1, opts.initialSamples ?? 5)
  const plateauSamples = Math.max(1, opts.plateauSamples ?? 100)

  if (steps.length !== losses.length) {
    throw new Error(`convergenceStats: steps (${steps.length}) and losses (${losses.length}) length mismatch`)
  }
  // Need enough points for the window AND meaningful initial/plateau sub-averages.
  if (losses.length < window + Math.max(initialSamples, 3)) return null

  const ma = movingAverage(losses, window)
  const maSteps = steps.slice(window - 1)   // MA point i corresponds to steps[i + window - 1]
  const initial = mean(ma.slice(0, Math.min(initialSamples, ma.length)))
  const plateau = mean(ma.slice(-Math.min(plateauSamples, ma.length)))

  const midpoint = (initial + plateau) / 2
  // Only meaningful if the MA actually descended past the midpoint.
  let convergedAt: number | null = null
  if (initial > plateau) {
    for (let i = 0; i < ma.length; i++) {
      if (ma[i] <= midpoint) {
        convergedAt = maSteps[i]
        break
      }
    }
  }

  const dropFraction = initial === 0 ? Number.NaN : 1 - plateau / initial
  return { ma, maSteps, initial, plateau, convergedAt, dropFraction }
}

function mean(xs: number[]): number {
  if (xs.length === 0) return Number.NaN
  let s = 0
  for (const x of xs) s += x
  return s / xs.length
}
