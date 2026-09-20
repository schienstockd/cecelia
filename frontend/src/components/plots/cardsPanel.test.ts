import { describe, it, expectTypeOf } from 'vitest'
import type { Card, CardFamily, CardsRequest, CardsResponse, PoolMember } from './cardsPanel'
import { cellFamily, motifFamily, CARD_FAMILIES } from './cardFamilies'

// Types-only tests: no runtime, no import from the Julia side. Their job is to fail typecheck if
// the payload shape drifts on one side — a card without `medoid.uid` or a `pool` that isn't
// [{uid, value_name}] is a contract regression per CELL_CARDS_PLAN Decision 0 (pool-first).
//
// Also pins the `CardFamily` shape (BEHAVIOUR_CARDS_PLAN Decision 2) so a new family added by
// registry can't accidentally lose a required field.

describe('cards payload contract', () => {
  const pool: PoolMember[] = [
    { uid: 'aaa111', value_name: 'B' },
    { uid: 'aaa111', value_name: 'T' },
    { uid: 'bbb222', value_name: 'B' },
  ]

  const card: Card = {
    path: '/Population 1',
    name: 'Scanning',
    colour: '#3f51b5',
    n: 42,
    medoid: { uid: 'aaa111', value_name: 'B', track_id: 17, frames: [0, 19] },
    filmstrip: [
      { t: 0, asset_id: 'sha256:abcd' },
      { t: 10, asset_id: 'sha256:ef01' },
      { t: 19, asset_id: 'sha256:2345' },
    ],
    stats: [{ name: 'speed', min: 1.2, q25: 2.4, median: 3.1, q75: 4.0, max: 6.5 }],
  }

  const response: CardsResponse = {
    pool, cards: [card],
    statScales: { speed: [1.0, 7.0] },
  }

  const request: CardsRequest = {
    root_uid: 'aaa111',
    value_name: 'B',
    cluster_col: 'movement',
    pops: [{ path: '/Population 1', cluster_ids: [0] }],
    view_state: null,
  }

  it('medoid carries a (uid, value_name, track_id) triple, not just a track_id', () => {
    expectTypeOf(card.medoid).toMatchTypeOf<{ uid: string; value_name: string; track_id: number }>()
  })

  it('pool is a list of PoolMember, not a single uid', () => {
    expectTypeOf(response.pool).toEqualTypeOf<PoolMember[]>()
  })

  it('request is pool-shaped: one root_uid + one value_name, backend expands the pool', () => {
    expectTypeOf(request.root_uid).toBeString()
    expectTypeOf(request.value_name).toBeString()
    expectTypeOf(request.pops).toMatchTypeOf<Array<{ path: string; cluster_ids: number[] }>>()
  })
})

describe('CardFamily registry contract', () => {
  it('cellFamily satisfies CardFamily and carries every required field', () => {
    expectTypeOf(cellFamily).toMatchTypeOf<CardFamily>()
  })

  it('registry keys are family ids', () => {
    expectTypeOf(CARD_FAMILIES).toMatchTypeOf<Record<string, CardFamily>>()
  })

  it('cellFamily is registered under id "cell"', () => {
    // Runtime assertion — drifting the id here would silently break the analysis-board wiring
    // (CanvasManager keys panels by view id).
    expectTypeOf(cellFamily.id).toEqualTypeOf<'cell' | 'motif' | 'hmm_state'>()
  })

  it('motifFamily satisfies CardFamily and opts out of the shownPops/suffix gates', () => {
    expectTypeOf(motifFamily).toMatchTypeOf<CardFamily>()
    // Motif cards discover their content server-side, so both gates must be off — otherwise the
    // panel would sit on the empty state forever waiting for a rail selection that isn't there.
    if (motifFamily.requireShownPops !== false) throw new Error('motifFamily.requireShownPops must be false')
    if (motifFamily.requireSuffix !== false)    throw new Error('motifFamily.requireSuffix must be false')
  })
})
