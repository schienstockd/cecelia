// The viewer's own view of `/Pick selection` — the cells the user picked ON the viewer (pick cell /
// pick rectangle) to light up on the gating plots. The plots highlight them; this is what marks them on
// the viewer itself: the dots outside the selection dim (`PICK_DIM`, `buildPointBufferLayers`) and,
// when the picked segmentation's mask is drawn, the picked cells get the pick outline.
//
// Transient UI state: never persisted, never recorded — the movie and still export are server-side and
// never read it (api/src/gating_api.jl `PICK_SEL_PATH` is excluded from everything durable).

import { getJson } from '../api'

/** The transient pop the picker writes (`PICK_SEL_PATH` in api/src/gating_api.jl). */
export const PICK_SELECTION_PATH = '/Pick selection'

/** The selection as the viewer holds it: the segmentation the labels belong to, and the labels. */
export interface ViewerPickSelection { valueName: string; labels: number[] }

/** The slice of a `gating:pop-tree` broadcast this reads. */
interface PopTreeLike { populations?: { name: string; transient?: boolean }[] }

/** Does this pop tree carry the pick selection? It sits at the top level, transient. */
export function treeHasPickSelection(tree: PopTreeLike | null | undefined): boolean {
  return !!tree?.populations?.some(p => p.transient && '/' + p.name === PICK_SELECTION_PATH)
}

/**
 * What a broadcast for `valueName` means for the selection `held`: 'fetch' its membership, 'clear'
 * it, or 'ignore' it. A tree with the selection always re-reads it (a pick, a pick-set, a gate edit
 * — membership is cheap, and the tree does not carry it). A tree without it clears only a selection
 * held for the SAME segmentation; a broadcast for another one says nothing about ours.
 */
export function pickBroadcastAction(
  held: ViewerPickSelection | null, valueName: string, hasPick: boolean,
): 'fetch' | 'clear' | 'ignore' {
  if (hasPick) return 'fetch'
  return held && held.labels.length && held.valueName === valueName ? 'clear' : 'ignore'
}

const NONE: ReadonlySet<number> = new Set()

/**
 * The selection as each population layer applies it — `PointLayer.selected`: `undefined` = draw as
 * usual, a set = dim every dot whose label is not in it. Labels are per segmentation, so the picked
 * segmentation's layers get the labels and every other layer gets an empty set (none of its cells were
 * picked). When no drawn layer is of the picked segmentation the selection marks nothing on screen,
 * and dimming every dot would only hide the image's populations — so nothing dims.
 */
export function pickLayerSelections(
  sel: ViewerPickSelection | null, layerVns: readonly string[],
): (ReadonlySet<number> | undefined)[] {
  if (!sel || !sel.labels.length || !layerVns.includes(sel.valueName)) return layerVns.map(() => undefined)
  const labels = new Set(sel.labels)
  return layerVns.map(vn => (vn === sel.valueName ? labels : NONE))
}

/** The labels to outline on the drawn mask, or none: only when the mask drawn IS the picked
 *  segmentation — the outline is drawn against the visible mask, and another segmentation's ids name
 *  unrelated cells there. */
export function pickOutlineLabels(sel: ViewerPickSelection | null, maskVn: string): number[] {
  return sel && maskVn && sel.valueName === maskVn ? sel.labels : []
}

/** The picked labels of `/Pick selection` under `valueName` — `[]` when nothing is picked (the
 *  membership route answers 404 for a pop that is not there) or the read fails. Quiet: every reader
 *  re-reads on each pick, and an empty answer is the normal one. The ONE reader — the viewer
 *  (`useViewerPickSelection`) and the correction cockpit both go through it. */
export async function fetchPickLabels(projectUid: string, imageUid: string, valueName: string): Promise<number[]> {
  try {
    const d = await getJson<{ membership?: Record<string, number[]> }>('/api/gating/membership', {
      query: { projectUid, imageUid, valueName, pops: PICK_SELECTION_PATH }, quiet: true,
    })
    return (d?.membership?.[PICK_SELECTION_PATH] ?? []).map(Number).filter(Number.isFinite)
  } catch { return [] }
}
