import { describe, it, expectTypeOf } from 'vitest'
import type { Card, CardsRequest, CardsResponse, PoolMember } from './cellCards'

// Types-only tests: no runtime, no import from the Julia side. Their job is to fail typecheck if
// the payload shape drifts on one side — a card without `medoid.uid` or a `pool` that isn't
// [{uid, value_name}] is a contract regression per CELL_CARDS_PLAN Decision 0 (pool-first).

describe('cellCards payload contract', () => {
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
    stats: [{ name: 'speed', median: 3.1, q25: 2.4, q75: 4.0 }],
  }

  const response: CardsResponse = { pool, cards: [card] }

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
