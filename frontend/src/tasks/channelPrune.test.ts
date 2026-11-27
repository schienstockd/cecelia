import { describe, it, expect } from 'vitest'
import { pruneChannelSelections, sharedChannels, flattenParams } from './paramValues'
import type { ParamDef, ParamValues, TaskDef } from './types'
import type { CciaImage } from '../stores/project'

// A set mixing two channel panels — the shape that made a run send channels its image lacks.
const PANEL_A = ['P14-CTDR', 'AF', 'OTI-uGFP', 'gBT-CTV']
const PANEL_B = ['AF', 'P14-ubiTom', 'gBT-uGFP', 'OTI-CTV']
const img = (channelNames: string[]) => ({ channelNames }) as unknown as CciaImage

// cellposeMeasure's shape: channels inside a repeatable `models` group, plus a top-level section.
const PARAMS: ParamDef[] = [
  { key: 'models', label: 'Models', type: 'group', params: [
    { key: 'cellChannels', label: 'Cell channels', type: 'channelSelection' },
    { key: 'nucChannels', label: 'Nucleus channels', type: 'channelSelection' },
    { key: 'cellDiameter', label: 'Diameter', type: 'int' },
  ] },
  { key: 'adv', label: 'Advanced', type: 'section', params: [
    { key: 'refChannels', label: 'Reference', type: 'channelSelection' },
  ] },
  { key: 'tags', label: 'Tags', type: 'chipSelect', options: [] },
]
const DEF = { task: 't', fun_name: 't', label: 't', params: PARAMS } as unknown as TaskDef

describe('sharedChannels', () => {
  it('is the channels every image carries, in the first image\'s order', () => {
    expect(sharedChannels([img(PANEL_A)])).toEqual(PANEL_A)
    expect(sharedChannels([img(PANEL_A), img(PANEL_B)])).toEqual(['AF'])
    expect(sharedChannels([])).toEqual([])
    expect(sharedChannels(undefined)).toEqual([])
  })
})

describe('pruneChannelSelections', () => {
  // the stored value from the failed run: both panels' names, shown as "All 6/4"
  const stale: ParamValues = {
    models: { '0': { cellChannels: ['gBT-uGFP', 'P14-ubiTom', 'OTI-CTV', 'P14-CTDR', 'OTI-uGFP', 'gBT-CTV'],
                     nucChannels: [], cellDiameter: 10 } },
    adv: { refChannels: ['OTI-CTV', 'AF'] },
    tags: ['gBT-uGFP'],
  }

  it('sends only channels the selected images have — group entries and sections alike', () => {
    const { values, emptied } = pruneChannelSelections(PARAMS, stale, sharedChannels([img(PANEL_A)]))
    const flat = flattenParams(DEF, values)
    expect((flat.models as Record<string, ParamValues>)['0'].cellChannels)
      .toEqual(['P14-CTDR', 'OTI-uGFP', 'gBT-CTV'])
    expect(flat.refChannels).toEqual(['AF'])
    expect(emptied).toEqual([])
  })

  it('leaves other params, other entry keys and the input untouched', () => {
    const { values } = pruneChannelSelections(PARAMS, stale, PANEL_A)
    expect(values.tags).toEqual(['gBT-uGFP'])   // not a channel param
    expect((values.models as Record<string, ParamValues>)['0'].cellDiameter).toBe(10)
    expect((stale.models as Record<string, ParamValues>)['0'].cellChannels).toHaveLength(6)
  })

  it('returns the same object when nothing is hidden', () => {
    const clean: ParamValues = { models: { '0': { cellChannels: ['AF'], nucChannels: [] } }, adv: { refChannels: [] } }
    expect(pruneChannelSelections(PARAMS, clean, PANEL_A).values).toBe(clean)
  })

  it('keeps everything when the channel list is unknown', () => {
    expect(pruneChannelSelections(PARAMS, stale, [])).toEqual({ values: stale, emptied: [] })
  })

  it('skips a picker its showIf hides', () => {
    const params: ParamDef[] = [
      { key: 'mode', label: 'Mode', type: 'select' },
      { key: 'chans', label: 'Channels', type: 'channelSelection', showIf: { mode: 'per' } },
    ]
    const vals = { mode: 'all', chans: ['gBT-uGFP'] }
    expect(pruneChannelSelections(params, vals, PANEL_A)).toEqual({ values: vals, emptied: [] })
  })

  // same rule as the form (`paramApplies`): a picker the user cannot see is never refused over
  it('skips a picker the server hid', () => {
    const params: ParamDef[] = [{ key: 'chans', label: 'Channels', type: 'channelSelection', hidden: true }]
    const vals = { chans: ['gBT-uGFP'] }
    expect(pruneChannelSelections(params, vals, PANEL_A)).toEqual({ values: vals, emptied: [] })
  })

  it('skips a picker the images lack the axes for — but only when the images are known', () => {
    const params = [{ key: 'chans', label: 'Channels', type: 'channelSelection',
                      requires: { axes: ['T'] } }] as unknown as ParamDef[]
    const vals = { chans: ['gBT-uGFP'] }
    const still = { sizeT: 1, sizeZ: 1, sizeC: 3 } as unknown as CciaImage
    expect(pruneChannelSelections(params, vals, PANEL_A, [still])).toEqual({ values: vals, emptied: [] })
    expect(pruneChannelSelections(params, vals, PANEL_A).emptied).toEqual(['Channels'])
  })

  it('skips every picker inside a section or group that is off the form', () => {
    const params = [
      { key: 'mode', label: 'Mode', type: 'select' },
      { key: 'adv', label: 'Advanced', type: 'section', showIf: { mode: 'per' }, params: [
        { key: 'chans', label: 'Channels', type: 'channelSelection' }] },
      { key: 'passes', label: 'Passes', type: 'group', showIf: { mode: 'per' }, params: [
        { key: 'gch', label: 'Group channels', type: 'channelSelection' }] },
    ] as unknown as ParamDef[]
    const vals = { mode: 'all', adv: { chans: ['gBT-uGFP'] }, passes: { '0': { gch: ['gBT-uGFP'] } } }
    expect(pruneChannelSelections(params, vals, PANEL_A)).toEqual({ values: vals, emptied: [] })
    expect(pruneChannelSelections(params, { ...vals, mode: 'per' }, PANEL_A).emptied)
      .toEqual(['Channels', 'Group channels'])
  })

  it('judges showIf in the scope the form does: form values in a section, entry over form in a group', () => {
    const params = [
      { key: 'mode', label: 'Mode', type: 'select' },
      { key: 'adv', label: 'Advanced', type: 'section', params: [
        { key: 'chans', label: 'Channels', type: 'channelSelection', showIf: { mode: 'per' } }] },
      { key: 'passes', label: 'Passes', type: 'group', params: [
        { key: 'kind', label: 'Kind', type: 'select' },
        { key: 'gch', label: 'Group channels', type: 'channelSelection', showIf: { mode: 'per', kind: 'x' } }] },
    ] as unknown as ParamDef[]
    const vals = { mode: 'per', adv: { chans: ['gBT-uGFP'] },
                   passes: { '0': { kind: 'x', gch: ['gBT-uGFP'] }, '1': { kind: 'y', gch: ['gBT-uGFP'] } } }
    expect(pruneChannelSelections(params, vals, PANEL_A).emptied).toEqual(['Channels', 'Group channels'])
    expect(pruneChannelSelections(params, { ...vals, mode: 'all' }, PANEL_A).emptied).toEqual([])
  })
})

describe('pruneChannelSelections — channelRoles', () => {
  const params: ParamDef[] = [{ key: 'afCombinations', label: 'AF', type: 'group', params: [
    { key: 'targetChannel', label: 'Target', type: 'channelSelection', multiple: false },
    { key: 'channelRoles', label: 'Other channels', type: 'channelRoles', exclude: 'targetChannel' },
  ] }]

  it('drops roles for channels the images lack', () => {
    const vals = { afCombinations: { '0': { targetChannel: ['gBT-CTV'],
      channelRoles: { 'OTI-uGFP': 'competing', 'gBT-uGFP': 'competing', AF: 'af' } } } }
    const { values, emptied } = pruneChannelSelections(params, vals, PANEL_A)
    expect((values.afCombinations as Record<string, ParamValues>)['0'].channelRoles)
      .toEqual({ 'OTI-uGFP': 'competing', AF: 'af' })
    expect(emptied).toEqual([])
  })

  it('reports, not empties, a map whose every channel is missing', () => {
    const vals = { afCombinations: { '0': { targetChannel: ['AF'], channelRoles: { 'gBT-uGFP': 'competing' } } } }
    const { values, emptied } = pruneChannelSelections(params, vals, PANEL_A)
    expect((values.afCombinations as Record<string, ParamValues>)['0'].channelRoles).toEqual({ 'gBT-uGFP': 'competing' })
    expect(emptied).toEqual(['Other channels'])
  })
})

describe('pruneChannelSelections — channelRoles without a role', () => {
  it('does not count a stale key with no role as a channel', () => {
    const params: ParamDef[] = [{ key: 'channelRoles', label: 'Other channels', type: 'channelRoles' }]
    const { values, emptied } = pruneChannelSelections(params, { channelRoles: { 'gBT-uGFP': '', AF: 'af' } }, PANEL_A)
    expect(values.channelRoles).toEqual({ AF: 'af' })
    expect(emptied).toEqual([])
    expect(pruneChannelSelections(params, { channelRoles: { 'gBT-uGFP': '' } }, PANEL_A).emptied).toEqual([])
  })
})
