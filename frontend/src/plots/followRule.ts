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
  if (follows(g)) { delete out.rule; delete out.rule_ok; return out }
  return { ...out, rule: { kind: 'follow' } }
}

/** What a hovered rectangle / polygon does, in one short line drawn beside the cursor. */
export function followTip(g: GateSpec): string {
  if (!follows(g)) return 'Double-click: follow the cloud on each image'
  return g.rule_ok === false ? 'No cloud found here: stays where drawn'
                             : 'Follows its cloud on each image · double-click: fix it'
}
