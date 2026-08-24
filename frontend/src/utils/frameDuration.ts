/**
 * What a frame LAG means in seconds, for the images a task form is pointed at.
 *
 * `opticalFlow.train` picks temporal scales as frame lags — 1, 2, 4, 8 — and a lag is not a
 * displacement until you know the frame interval. The same chip row is 5–40 s on one movie and
 * 15–120 s on another, and under "read other rates as the same durations" it is the DURATIONS that
 * define the model. So the chips carry both, and this is the arithmetic behind them.
 *
 * In a util rather than the SFC because of the rule it encodes: the anchor is the COARSEST interval
 * in the set, which is the same choice `train_run.reference_interval` makes on the server. If these
 * two disagreed, the form would promise spans the run does not train. One rule, tested here, mirrored
 * there — and deliberately NOT a third copy of the resolver: whether a coarser movie can be read at
 * these spans is the runner's call, and it refuses per movie by name.
 *
 * **The unit conversion is `timeAxis.toSeconds`, not a local check.** The first cut of this compared
 * the unit to `'s'` and reported "no frame interval in seconds" for fXgbTl, which records 15 — the
 * store's OME-XML says `'s'` (the OME enum's own value, which is what the Python side reads) while
 * `ccid.json` keeps Bioformats' word `'second'`, and the FORM reads the latter. Two readers, two
 * spellings of one fact. `toSeconds` already knew all of them.
 */

import { toSeconds } from './timeAxis'   // ONE time-unit rule, shared with the time axis

export interface FrameRate {
  /** Seconds per frame — the COARSEST among the images that record one. See `spanAnchorRate`. */
  seconds: number
  /** Which image it came from, for a readout that can be checked. */
  uid: string
  /** How many of the selected images record an interval in seconds. */
  known: number
  /** How many were selected. `known < total` means some cannot be read at durations at all. */
  total: number
  /** True when the images disagree — then the anchor is one of several, and worth saying so. */
  mixed: boolean
  /**
   * True when the anchor's unit was NOT seconds and had to be converted (a movie recorded in ms).
   *
   * Worth carrying because the RUNNER does not convert: `train_run` skips a movie whose interval is
   * in another unit rather than guessing at it (see `_physical_scale` for why). So a converted rate
   * is one the form can display and the run will refuse — the caller says so instead of promising it.
   */
  converted: boolean
}

/** The minimal image shape this reads. Structural, so it accepts the task form's `CciaImage`. */
export interface RateImage {
  uid?: string
  timeIncrement?: number | null
  timeIncrementUnit?: string | null
}

/**
 * The interval the spans are anchored on, or `null` when no image records one in seconds.
 *
 * The COARSEST of the set, which is the rule `train_run.reference_interval` applies on the server. The
 * spans must be representable on every selected movie, and this is what guarantees it: every finer
 * movie scales the lags UP, so none falls below one frame and consecutive lags stay distinct. The
 * finest does the opposite — on lags [1,2,4] across 5/10/15 s/frame it leaves 2 of the 3 movies unable
 * to carry the shortest span. A mean would be a rate no movie has.
 */
export function spanAnchorRate(images: readonly RateImage[] | undefined): FrameRate | null {
  const imgs = images ?? []
  // `toSeconds` is the unit rule (`s` / `sec` / `second` / `ms` / `min` / `h`); the positive guard is
  // `frameSeconds`', restated because that one is typed to the full `CciaImage` and this is
  // structural so a task form's param context fits without a cast.
  const usable = imgs.flatMap(im => {
    if (typeof im.timeIncrement !== 'number' || !(im.timeIncrement > 0)) return []
    const unit = (im.timeIncrementUnit ?? '').toLowerCase()
    // An ABSENT unit reads as seconds, which `toSeconds` deliberately will not do — it answers `null`
    // so a PLOT axis is not relabelled off an assumption. Here the assumption is the right one and
    // matching it is the point: `dim_utils.im_time_increment_unit` defaults to `'s'`, and OME's spec
    // default is `'s'`, so the runner WILL train such a movie. Refusing it here would have the form
    // say "cannot be resolved" about a run that then works. (`metadata.frame_interval_no_unit` is the
    // QC finding that says the gap out loud; that is its job, not this readout's.)
    const secs = unit ? toSeconds(im.timeIncrement, im.timeIncrementUnit) : im.timeIncrement
    if (secs == null || !(secs > 0)) return []
    return [{ im, secs, converted: !!unit && !unit.startsWith('s') }]
  })
  if (!usable.length) return null
  const best = usable.reduce((a, b) => (b.secs > a.secs ? b : a))
  const distinct = new Set(usable.map(u => u.secs))
  return {
    seconds: best.secs,
    uid: best.im.uid ?? '',
    known: usable.length,
    total: imgs.length,
    mixed: distinct.size > 1,
    converted: best.converted,
  }
}

/** `60` → `"60s"`, `7.5` → `"7.5s"`. A whole number of seconds prints as one. */
export const secondsLabel = (v: number): string =>
  `${Number.isInteger(v) ? v : Number(v.toFixed(2))}s`

/**
 * Chip options relabelled with what each lag spans — `"4"` → `"4 · 60s"`.
 *
 * Untouched when no rate is known, which is the honest answer: a chip reading `4 · 4s` because the
 * interval defaulted to 1.0 would be a measurement nobody made. The VALUE never changes, only the
 * label, so the submitted params and `validate_params`' option check are unaffected.
 */
export function withDurationLabels<T extends { value: string; label?: string }>(
  options: readonly T[], rate: FrameRate | null,
): { value: string; label: string }[] {
  return options.map(o => {
    const label = o.label ?? o.value
    const lag = Number(o.value)
    if (!rate || !Number.isFinite(lag) || lag <= 0) return { value: o.value, label }
    return { value: o.value, label: `${label} · ${secondsLabel(lag * rate.seconds)}` }
  })
}
