// RESULT-REVIEW: result-697b4dea
// A tracking node whose `popsToTrack` list holds a ref with no segmentation prefix (an agent-authored
// or hand-edited chain) derives its `_tracked` pop on the node's own label set — not on label set ''.
import { it, expect } from 'vitest'
import { prospectivePops } from './chainProspective'
import type { ParamDef } from '../tasks/types'

const defs: Record<string, { params: ParamDef[] }> = {
  'tracking.bayesian_track_measures': { params: [
    { key: 'popsToTrack', type: 'popSelection', accepts: ['live'], producesPop: '_tracked' },
  ] as ParamDef[] },
}

it('reads a bare path in a list on the node’s label set', () => {
  expect(prospectivePops([{ fn: 'tracking.bayesian_track_measures',
                            params: { valueName: 'Tcells', popsToTrack: ['/qc/OTI'] } }], fn => defs[fn]))
    .toEqual([{ valueName: 'Tcells', path: '/qc/OTI/_tracked', kind: 'track' }])
})
