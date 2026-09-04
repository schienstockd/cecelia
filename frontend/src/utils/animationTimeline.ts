// The animation timeline's pure reading logic — what a captured viewer view SAYS, with no store, no
// fetch and no DOM. Extracted when the page was split into a controls panel and a timeline matrix
// (modules/animation/), because both halves read the same snapshots: the panel needs "is there
// anything to render", the matrix needs the rows and the per-cell state, and a second copy of
// `isOverlay` in one of them is exactly the divergence this repo keeps paying for.
//
// A "snapshot" here is one keyframe's viewer view state: `layers` (per-layer visibility + colormap),
// `camera`, `dims`. It is stored verbatim as the bridge returned it, so every read is defensive.
import { elapsedLabel } from './stillOverlay'
import type { AnimSnapshot } from '../stores/animation'

export type Layers = Record<string, { visible?: boolean; colormap?: string }>

export const layersOf = (s: AnimSnapshot | undefined): Layers =>
  (s?.snapshot?.layers ?? {}) as Layers

/** Overlays (populations / tracks / labels) are viewer layers whose name is parenthesised —
 *  "(popType) (vn) …", "(vn) Labels". Image channels are the plain-named layers. */
export const isOverlay = (name: string): boolean => name.startsWith('(')

/** A timeline is per-image: the keyframes of ONE image, in list order. */
export const framesFor = (snapshots: AnimSnapshot[], imageUid: string): AnimSnapshot[] =>
  imageUid ? snapshots.filter(s => s.imageUid === imageUid) : []

/** Which image the page is working on. The table's selection leads; with nothing selected we fall back
 *  to whatever viewer has open, so the page still shows a timeline the moment you land on it (and a
 *  restored animation for another image is not invisible). */
export const activeAnimationUid = (selectedUids: string[], viewerUid: string | null | undefined): string =>
  selectedUids[0] || viewerUid || ''

/** Row set = the union of layer names across the image's keyframes, split by `pred`. Union, not the
 *  first frame's: a layer added halfway through the animation still needs a row. */
export function unionRows(frames: AnimSnapshot[], pred: (n: string) => boolean): string[] {
  const set = new Set<string>()
  for (const f of frames) for (const n of Object.keys(layersOf(f))) if (pred(n)) set.add(n)
  return [...set]
}
export const channelRows = (frames: AnimSnapshot[]): string[] => unionRows(frames, n => !isOverlay(n))
export const popRows     = (frames: AnimSnapshot[]): string[] => unionRows(frames, isOverlay)

/** Is a layer visible in this keyframe? `null` = the layer isn't in that keyframe at all, which is a
 *  third state the matrix draws differently — neither on nor off, nothing to toggle. */
export function cellState(s: AnimSnapshot, name: string): boolean | null {
  const l = layersOf(s)[name]
  return l === undefined ? null : l.visible !== false
}

/**
 * What a click on one cell should WRITE into that keyframe's layer entry — flipping a layer it has,
 * or ADDING one it doesn't.
 *
 * The add is the point. A layer captured after the first keyframes (turn on tracks, capture a fourth)
 * exists only in the keyframe that saw it, and the matrix could only toggle it there — the other
 * columns were dead dots. Nothing about the data forbids it: a keyframe is JSON, and the render
 * applies a keyframe's layer props by NAME against whatever the viewer has. Worse, an absent entry is
 * not "hidden" at render time — `apply_view_state` skips names it wasn't given, so the layer keeps
 * whatever the previous keyframe left it as. Writing the entry is what makes the cell mean something.
 *
 * The new entry is seeded from the first keyframe that HAS the layer, so it arrives with that layer's
 * colormap / contrast / opacity rather than as a bare `{visible}` that would reset them mid-animation.
 * Deep-copied: two keyframes sharing one props object would toggle together.
 *
 * Returns null when no keyframe has the layer at all — unreachable from the matrix (a row exists
 * because one does), and a guard against writing a meaningless entry.
 */
export function cellToggle(frames: AnimSnapshot[], target: AnimSnapshot,
                           name: string): Record<string, unknown> | null {
  const cur = layersOf(target)[name] as Record<string, unknown> | undefined
  if (cur) return { ...cur, visible: cur.visible === false }
  const seed = frames.map(f => layersOf(f)[name]).find(Boolean)
  return seed ? { ...(JSON.parse(JSON.stringify(seed)) as Record<string, unknown>), visible: true } : null
}

export const cameraZoom = (s: AnimSnapshot): string => {
  const z = (s.snapshot?.camera as { zoom?: number } | undefined)?.zoom
  return typeof z === 'number' ? z.toFixed(1) : '—'
}

/** A keyframe is "edited" once its working view state diverges from the captured baseline. */
export const isEdited = (s: AnimSnapshot): boolean =>
  !!s.original && JSON.stringify(s.snapshot) !== JSON.stringify(s.original)

/** Where a snapshot sits in the timelapse — its T index plus wall-clock when the image's frame
 *  interval is known, so you can tell which frame it came from. T is the first dims axis. */
export function keyframeTime(s: AnimSnapshot, inc?: number | null, unit?: string | null): string {
  const step = (s.snapshot?.dims as { current_step?: number[] } | undefined)?.current_step
  const t = Array.isArray(step) ? step[0] : undefined
  if (t === undefined || t === null) return ''
  return inc ? `t${t} · ${elapsedLabel(t, inc, unit)}` : `t${t}`
}
