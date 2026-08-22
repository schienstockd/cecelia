/**
 * The task form must not drop the user's params when a run starts.
 *
 * THE BUG, reported as "the params always revert back to default when a task finishes" — and then,
 * decisively, "it reverts after STARTING a task. or finishing":
 *
 *   1. `run()` cleared the in-progress draft, on the assumption that the server-side funParams record
 *      had replaced it.
 *   2. That record is not written synchronously. Meanwhile starting or finishing a task mutates the
 *      project store (`bumpDataVersion` / `refreshImageMeta` in `stores/ws.ts`), which re-fires
 *      `watch([taskDef, setUid, projectUid])` and `watch(drivingImageUid)` and re-runs `initParams`.
 *   3. In that window there was no draft AND no record. `/api/tasks/funparams` answered
 *      `{"params":null}` — verified against a running server — which `fetchSavedParams` maps to `{}`.
 *   4. `buildParamValues(def, {})` is every param's spec DEFAULT.
 *
 * So the form reset itself on every task, seconds after Run, and the two guards that exist for exactly
 * this class (`resolveInitialParams` returning `null` for "the load did not happen", and the draft
 * taking precedence over a saved record) could not help: the draft had been deleted on purpose.
 *
 * This is a SOURCE ratchet rather than a behavioural test because the decision lives in an SFC, and
 * this suite mounts nothing (`frontend/CLAUDE.md`). It is narrow on purpose: it pins the one line whose
 * removal reintroduces a bug that took three rounds of questions to locate.
 */
import { describe, it, expect } from 'vitest'

import { buildParamValues, resolveInitialParams } from './paramValues'
import type { TaskDef } from './types'

// Vite's raw glob, not `node:fs` — the same mechanism `utils/uiCopy.test.ts` and
// `utils/cssScenarios.test.ts` use, and the only one that typechecks here: this app's tsconfig has no
// node types, so `readFileSync`/`__dirname` fail `vue-tsc` even though vitest would run them.
const SFC = import.meta.glob('/src/tasks/TaskRunner.vue',
                             { query: '?raw', import: 'default', eager: true }) as
  Record<string, string>
const RUNNER = Object.values(SFC)[0] ?? ''

describe('the run handler and the draft', () => {
  it('found the runner source at all', () => {
    // A glob that matches nothing would make every assertion below vacuously pass.
    expect(RUNNER).toContain('function run()')
  })

  it('does not clear the draft', () => {
    // `drafts.clear(...)` anywhere in the runner is this bug returning. If a future change genuinely
    // needs to drop a draft, it needs a different guarantee that the params survive the window — and
    // this comment is the place to argue it.
    expect(RUNNER).not.toMatch(/drafts\s*\.\s*clear\s*\(/)
  })

  it('writes the submitted params as the draft instead', () => {
    // Same values, so it converges with the record the server is about to write; the next edit
    // replaces it as before.
    expect(RUNNER).toMatch(/drafts\.set\(currentDraftKey\.value, paramValues\.value\)/)
  })

  it('still reports a failed load as "leave the form alone"', () => {
    // The other half of the guard, and the reason `{}` and `null` are not the same answer.
    expect(RUNNER).toMatch(/if \(!projectUid\.value\) return null/)
    expect(RUNNER).toMatch(/if \(!res\.ok\) return null/)
  })
})

describe('why an empty answer resets a form', () => {
  const DEF = {
    fun_name: 'segment.coastal', task: 'coastal', label: 'Coastal', category: 'segment', env: [],
    params: [
      { key: 'blockSize', label: 'Tile', type: 'int', default: 512 },
      { key: 'valueName', label: 'Image', type: 'select', default: '' },
    ],
  } as unknown as TaskDef

  it('an empty record IS every default — which is right on a first run and wrong after one', () => {
    // Not a bug in itself: a task nobody has run should open on its defaults. It becomes the bug when
    // something deletes the user's draft and this is what fills the gap.
    expect(buildParamValues(DEF, {})).toEqual({ blockSize: 512, valueName: '' })
  })

  it('a load that did not happen leaves the form alone', () => {
    expect(resolveInitialParams(DEF, undefined, null)).toBeNull()
  })

  it('a draft always wins over a saved record', () => {
    // The precedence the fix relies on: with the draft kept, the window has an answer that is the
    // user's, whatever the server has or has not written yet.
    const got = resolveInitialParams(DEF, { blockSize: 4096 }, { blockSize: 512 })
    expect(got).toEqual({ blockSize: 4096, valueName: '' })
  })
})
