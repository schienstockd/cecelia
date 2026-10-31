// The behaviour axes a track clustering run names for itself (docs/todo/TRACK_BEHAVIOUR_AXES_PLAN.md):
// per diffusion component its top features by Spearman rho, sign-aligned so the first is positive, plus
// where the path position starts. Written by `clustering_utils.behaviour_axes`, recorded on the run's
// sidecar entry as `behaviourAxes`, served per suffix as `clusterAxes` by /api/gating/channels.
import { shortTrackMeasure } from './measureGroups'

export interface AxisFeature { feature: string; rho: number }
export interface BehaviourAxis { name: string; top: AxisFeature[] }
export interface BehaviourAxes {
  axes: BehaviourAxis[]
  root: { end: 'low' | 'high'; cluster: string | null }
  evals?: number[]
}

/** A cluster feature column, short enough for an axis title. Display-only: the column keeps its name. */
export function axisFeatureLabel(col: string, nameMap: Record<string, string> = {}): string {
  if (nameMap[col]) return nameMap[col]
  let m = /^live\.cell\.hmm\.state\.[^.]+\.(\d+)$/.exec(col)
  if (m) return `HMM state ${m[1]}`
  m = /^live\.cell\.hmm\.transitions\.[^.]+\.(\d+)_(\d+)$/.exec(col)
  if (m) return `HMM ${m[1]}→${m[2]}`
  return shortTrackMeasure(col)
}

/** "DC1: speed, HMM state 3" — the axis named by its top `n` features; a negative one carries "−". */
export function axisTitle(axis: BehaviourAxis | undefined, fallback: string,
                          nameMap: Record<string, string> = {}, n = 2): string {
  if (!axis?.top?.length) return fallback
  const feats = axis.top.slice(0, n).map(t => `${t.rho < 0 ? '−' : ''}${axisFeatureLabel(t.feature, nameMap)}`)
  return `${axis.name}: ${feats.join(', ')}`
}
