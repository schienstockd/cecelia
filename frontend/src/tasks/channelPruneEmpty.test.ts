// RESULT-REVIEW: result-9f7f22cd
import { describe, it, expect } from 'vitest'
import { pruneChannelSelections, flattenParams } from './paramValues'
import type { ParamDef, TaskDef } from './types'

// smooth / OME-TIFF export read `channels: []` as "every channel". A selection whose every name is
// missing from the image must therefore never be sent as `[]` — it is reported so the form refuses
// the run, instead of the run quietly covering every channel.
const PARAMS: ParamDef[] = [{ key: 'channels', label: 'Channels', type: 'channelSelection', multiple: true }]
const DEF = { task: 'smooth', fun_name: 'cleanupImages.smooth', label: 'Smooth', params: PARAMS } as unknown as TaskDef

describe('a selection with no channel on the image', () => {
  it('is not sent empty, and is reported', () => {
    const { values, emptied } = pruneChannelSelections(PARAMS, { channels: ['CD4'] }, ['DAPI2', 'CD8', 'THG'])
    expect(flattenParams(DEF, values).channels).not.toEqual([])
    expect(emptied).toEqual(['Channels'])
  })
})
