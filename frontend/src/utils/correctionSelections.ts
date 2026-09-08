// Pure logic for the correction cockpit's selection displays. Extracted so the summary line +
// chip-strip options are unit-testable without mounting the .vue (the frontend test convention:
// no jsdom, pure utils only — frontend/CLAUDE.md).
//
// One rule to remember: the cockpit has THREE modes (tracks, labels, review) and TWO independent
// selection sets (tracks, labels). Review is a rank over the SAME `/Pick selection` set that
// Labels uses — so both modes read labels-side state, and only the summary/chip-strip framing
// differs. The Tracks mode reads its own client-side track-id set (`cockpit.selectedTracks`).

import type { ChipOption } from '../components/ChipSelect.vue'

export type CockpitMode = 'tracks' | 'labels' | 'review'

/** Which selection set the cockpit's summary + chip strip should render, per mode. */
export function selectionKind(mode: CockpitMode): 'labels' | 'tracks' {
  return (mode === 'labels' || mode === 'review') ? 'labels' : 'tracks'
}

/** One-line footer summary for the picked LABELS. `t` may be null when the viewer hasn't
 *  published a viewState yet (label ops need a frame — say so rather than lie about "no picks"). */
export function labelSelectionSummary(ids: readonly number[], t: number | null): string {
  if (t === null) return 'Viewer not ready'
  const n = ids.length
  if (!n) return `No label picked (frame ${t})`
  if (n === 1) return `Label ${ids[0]} @ frame ${t}`
  if (n <= 4) return `Labels ${ids.join(', ')} @ frame ${t}`
  return `${n} labels picked @ frame ${t}`
}

/** One-line footer summary for the picked TRACKS. `splitFrame` is the frame a Split op would
 *  target; null when no split has been armed. */
export function trackSelectionSummary(ids: readonly string[], splitFrame: number | null): string {
  if (!ids.length) return 'No track selected'
  if (ids.length === 1) {
    return splitFrame !== null
      ? `Track ${ids[0]} @ frame ${splitFrame}`
      : `Track ${ids[0]}`
  }
  if (ids.length === 2) return `Tracks ${ids.join(' + ')}`
  return `${ids.length} tracks selected`
}

/** Build the ChipSelect options for a picked-label strip. Chip label is the id; clicking removes.
 *  `focused` (Review's cursor) gets a subtle accent so the user can see which chip is "current". */
export function labelChipOptions(ids: readonly number[], focused: number | null = null): ChipOption[] {
  return ids.map(id => ({
    value: String(id),
    label: String(id),
    tip: focused === id ? `Label ${id} · focused — click to drop from pick`
                         : `Label ${id} · click to drop from pick`,
    accent: focused === id ? 'var(--cc-accent)' : undefined,
  }))
}

/** Build the ChipSelect options for a picked-track strip. No accent — the cockpit currently has
 *  no "focused track" signal to feed one (Review's cursor is a labels-only concept). Add a
 *  parameter here when a real signal shows up, not before. */
export function trackChipOptions(ids: readonly string[]): ChipOption[] {
  return ids.map(id => ({
    value: id,
    label: id,
    tip: `Track ${id} · click to drop from pick`,
  }))
}

/** Parse the ChipSelect's `string[]` model back to a number list for the labels case. Anything
 *  the strip can emit that isn't a positive integer id is dropped silently — the source list is
 *  already validated, so this is a defensive parse rather than a filter. */
export function parseLabelChipValues(vals: readonly string[]): number[] {
  const out: number[] = []
  for (const v of vals) {
    const n = Math.floor(Number(v))
    if (Number.isFinite(n) && n >= 1) out.push(n)
  }
  return out
}
