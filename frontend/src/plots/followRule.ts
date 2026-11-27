// A rectangle or polygon that follows the cloud it was drawn on — docs/POPULATION.md → Rule gates.
//
// With `rule: {kind: 'follow'}` the server moves the shape on each parent by how far that cloud's density
// mode moved from where it was drawn (app/src/gating/follow_rule.jl). The outline it sends is already
// moved; `rule_ok: false` = no such cloud on this parent, so the shape stands where it was drawn. The
// anchor never reaches the plot: an edit sent back is a new template, anchored again where it is saved.
import type { GateSpec } from '../stores/gating'

export const canFollow = (g?: Partial<GateSpec> | null): boolean =>
  g?.kind === 'rectangle' || g?.kind === 'polygon'

export const follows = (g?: Partial<GateSpec> | null): boolean =>
  canFollow(g) && g!.rule?.kind === 'follow'

/** A rectangle / polygon switched between staying put and following its cloud. Off, it stays where it is
 *  shown — where the rule last moved it. Any other gate is returned as is. */
export function toggleFollow(g: GateSpec): GateSpec {
  if (!canFollow(g)) return g
  const out = { ...g }
  if (follows(g)) { delete out.rule; delete out.rule_ok; delete out.rule_peak; return out }
  return { ...out, rule: { kind: 'follow' } }
}

/** The follow switch's tooltip — on the naming bar and on a selected rectangle / polygon. */
export const FOLLOW_TIP = 'Move with its cloud on each image'

/** What a hovered rectangle / polygon does, in one short line drawn beside the cursor. */
export function followTip(g: GateSpec): string {
  if (!follows(g)) return 'Double-click: follow the cloud on each image'
  return g.rule_ok === false ? 'No cloud found here: stays where drawn'
                             : 'Follows its cloud on each image · double-click: fix it'
}

/** One image in the "Check images" grid (`GET /api/gating/rule-preview`). */
export interface RulePreviewImage { uid: string; png?: string; n?: number; rule_ok?: boolean | null; error?: string }

/** What a preview card says under its picture. */
export function previewStatus(im: RulePreviewImage): { text: string; ok: boolean } {
  if (im.error) return { text: im.error, ok: false }
  if (im.rule_ok === false) return { text: 'No cloud found: stays where drawn', ok: false }
  if (im.rule_ok == null) return { text: 'Not on these axes', ok: false }
  return { text: 'Found its cloud', ok: true }
}

/** The grid's one-line summary: on how many of the images drawn so far the gate found its cloud. */
export function previewSummary(ims: RulePreviewImage[]): string {
  const found = ims.filter(i => previewStatus(i).ok).length
  return `Found its cloud on ${found} of ${ims.length} image${ims.length === 1 ? '' : 's'}`
}
