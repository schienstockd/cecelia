// RESULT-REVIEW: result-e6b6560c
// A boolean pop's membership follows the pops it combines, though it sits under none of them — a
// redrawn operand must refetch a panel plotting the boolean pop (reviewer repro, asserting the fix).
import { it, expect } from 'vitest'
import { gateSignatures, changedPopPaths, booleanDeps, popChangeKey, seriesReloadToken,
         type SigNode } from './popChanges'

const tree = (x: number): SigNode[] => [
  { name: 'T', gate: { kind: 'rectangle', x_min: x } },
  { name: 'B', gate: { kind: 'rectangle', x_min: 2 } },
  { name: 'TorB', boolean: { pops: ['/T', '/B'] }, children: [{ name: 'big', gate: { kind: 'rectangle', x_min: 4 } }] },
  { name: 'notT', boolean: { pops: ['/B'], not: ['/T'] } },
]

it('a redrawn operand changes every boolean pop combining it, and what sits under them', () => {
  const next = tree(9)
  const ch = changedPopPaths(gateSignatures(tree(1)), gateSignatures(next), booleanDeps(next))
  expect([...ch].sort()).toEqual(['/T', '/TorB', '/TorB/big', '/notT'])
  const changes: Record<string, number> = {}
  for (const p of ch) changes[popChangeKey('default', p)] = 1
  expect(seriesReloadToken(0, changes, [{ valueName: 'default', pop: '/TorB' }], 1)).toBeGreaterThan(0)
})
