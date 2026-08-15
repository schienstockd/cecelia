// Frame index → real elapsed time, for the per-timepoint (temporal groupBy) plots.
//
// The aggregator groups by the temporal column (`centroid_t`), whose levels are FRAME INDICES — the
// x axis therefore reads 0…179 and says "centroid_t", which is not a quantity anyone measures in.
// The interval that turns a frame into a time is per IMAGE (`timeIncrement` + `timeIncrementUnit`
// from the OME metadata), so the conversion has to be per series, not one factor for the plot: two
// movies at 30 s and 60 s per frame put the same frame index at different times.
//
// THE RULE THIS ENCODES (docs/ARCHITECTURE.md → *Calibration*): an interval that is absent is NOT
// 1.0 — "we don't know" must never silently become "1 second per frame". So this returns `null`
// unless EVERY plotted image carries a usable interval, and the caller then keeps the frame axis
// rather than labelling frames as seconds. Mixing a converted and an unconverted series on one axis
// would be worse than not converting at all: the numbers would be incomparable and nothing would say so.
import type { CciaImage } from '../stores/project'

/** Convert `v` in `unit` to seconds; null when the unit isn't a recognised time unit. */
export function toSeconds(v: number, unit?: string | null): number | null {
  const u = (unit ?? '').toLowerCase()
  if (!u) return null
  if (u.startsWith('ms') || u.startsWith('millis')) return v / 1000
  if (u.startsWith('s')) return v                          // s / sec / second(s)
  if (u === 'm' || u.startsWith('min')) return v * 60      // m / min / minute(s)
  if (u.startsWith('h')) return v * 3600                   // h / hr / hour(s)
  return null
}

/** Seconds per frame for ONE image — null when it has no usable interval. */
export function frameSeconds(img: CciaImage | null | undefined): number | null {
  if (!img || img.timeIncrement == null || !(img.timeIncrement > 0)) return null
  const s = toSeconds(img.timeIncrement, img.timeIncrementUnit)
  return s != null && s > 0 ? s : null
}

/**
 * Seconds-per-frame for each of `uids`, or **null if any one of them is unknown** — see the rule at
 * the top of this file. `lookup` is the project store's `imageByUid`.
 *
 * An empty `uids` also yields null: there is nothing to scale, so there is no basis for relabelling
 * the axis in seconds.
 */
export function frameSecondsByImage(uids: string[],
                                    lookup: (uid: string) => CciaImage | null): Record<string, number> | null {
  if (!uids.length) return null
  const out: Record<string, number> = {}
  for (const uid of uids) {
    const s = frameSeconds(lookup(uid))
    if (s == null) return null
    out[uid] = s
  }
  return out
}

/**
 * The ONE interval covering all of `uids` — null unless every image is known AND they all agree.
 *
 * For a series that carries no image identity: a single-image plot, or a `summarised` scope that has
 * already folded several movies into one curve. There is only one x axis to label, so two different
 * intervals cannot both be honoured — and silently picking one would place a 30 s/frame movie and a
 * 60 s/frame movie on the same seconds axis, off by 2×. Frames are the honest fallback.
 */
export function sharedFrameSeconds(uids: string[],
                                   lookup: (uid: string) => CciaImage | null): number | null {
  const m = frameSecondsByImage(uids, lookup)
  if (!m) return null
  const vals = [...new Set(Object.values(m))]
  return vals.length === 1 ? vals[0] : null
}

/**
 * The x-axis title when the levels stayed FRAME INDICES — i.e. the conversion above returned null.
 *
 * The axis used to fall back to the raw group column, so an unconvertible movie drew an axis labelled
 * `centroid_t`. That is both unreadable and less informative than it looks: it names the column rather
 * than the quantity, and says nothing about the numbers being frame counts.
 *
 * `(frames)` is carried deliberately. The rule at the top of this file is that an unknown interval must
 * never be presented as time, so this must not shorten to a bare "Time" — that is exactly the claim the
 * seconds axis makes. Anything that isn't the temporal column keeps its own name.
 */
export function frameAxisLabel(groupBy?: string | null): string {
  const g = groupBy ?? ''
  return g.toLowerCase() === 'centroid_t' ? 'Time (frames)' : (g || 't')
}
