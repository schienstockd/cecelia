import { describe, it, expect } from 'vitest'
import { measureGroups, groupedCols, isIntensityColumn, isTrackPopType } from './measureGroups'

// The real payloads of `/api/gating/channels` for one image of "unimelb 3P spleen" (4kS67f/LUkCpP,
// segmentation "B") — captured from the running backend, not invented, so the group boundaries are
// checked against columns that actually exist.
const FLOW = {
  columns: ['area', 'extent', 'major_axis_length', 'interm_axis_length', 'minor_axis_length',
    'surface_area', 'volume_mesh', 'convex_hull_area', 'convex_hull_volume', 'euler_number_mesh',
    'solidity', 'surface_to_volume', 'sphericity', 'compactness', 'feret_diameter_max_mesh',
    'ellipticity_oblate', 'ellipticity_prolate', 'ellipticity_interm_oblate',
    'ellipticity_interm_prolate', 'mean_intensity_0', 'mean_intensity_1', 'mean_intensity_2',
    'mean_intensity_3'],
  channels: ['mean_intensity_0', 'mean_intensity_1', 'mean_intensity_2', 'mean_intensity_3'],
  spatialAxes: ['centroid_x', 'centroid_y', 'centroid_z', 'centroid_t'],
  obsColumns: ['track_id', 'track_parent', 'track_root', 'track_state', 'track_generation',
    'cell_id', 'live.cell.speed', 'live.cell.angle', 'live.cell.hmm.state.movement',
    'live.cell.hmm.transitions.movement', 'regions.immune', 'spatial.comp.B_qc__tracked.immune',
    'regions.default', 'live.cell.contact#live.T_qc__tracked',
    'live.cell.min_distance#live.T_qc__tracked'],
  popType: 'flow',
}
const MOTILITY = ['live.track.speed', 'live.track.duration', 'live.track.trackLength',
  'live.track.displacement', 'live.track.straightness', 'live.track.asphericity']

const titles = (i: Parameters<typeof measureGroups>[0]) => measureGroups(i).map(g => g.title)
const group = (i: Parameters<typeof measureGroups>[0], t: string) =>
  measureGroups(i).find(g => g.title === t)?.cols

describe('measureGroups — the flow axis picker', () => {
  it('splits morphology from channels, spatial last', () => {
    expect(titles(FLOW)).toEqual(['Morphology', 'Channels', 'Spatial / Time',
                                  'Behaviour', 'Regions', 'Other measures'])
  })

  it('puts every intensity column under Channels and none of them under Morphology', () => {
    expect(group(FLOW, 'Channels')).toEqual(FLOW.channels)
    expect(group(FLOW, 'Morphology')).not.toContain('mean_intensity_0')
    expect(group(FLOW, 'Morphology')?.[0]).toBe('area')
  })

  it('keeps the server order inside a group', () => {
    expect(group(FLOW, 'Morphology')?.slice(0, 3)).toEqual(['area', 'extent', 'major_axis_length'])
  })

  it('offers every column exactly once', () => {
    const all = groupedCols(measureGroups(FLOW))
    const want = [...FLOW.columns, ...FLOW.spatialAxes, ...FLOW.obsColumns]
    expect([...all].sort()).toEqual([...new Set(want)].sort())
    expect(new Set(all).size).toBe(all.length)
  })

  it('groups the obs measures by family', () => {
    expect(group(FLOW, 'Behaviour')).toEqual(['live.cell.speed', 'live.cell.angle',
      'live.cell.hmm.state.movement', 'live.cell.hmm.transitions.movement',
      'live.cell.contact#live.T_qc__tracked', 'live.cell.min_distance#live.T_qc__tracked'])
    expect(group(FLOW, 'Regions')).toEqual(['regions.immune', 'spatial.comp.B_qc__tracked.immune',
                                            'regions.default'])
    // ids are filterable but are not a measure family — they land in the catch-all, not in Behaviour
    expect(group(FLOW, 'Other measures')).toEqual(['track_id', 'track_parent', 'track_root',
      'track_state', 'track_generation', 'cell_id'])
  })

  it('drops the groups a caller has nothing for', () => {
    expect(titles({ columns: FLOW.columns, channels: FLOW.channels })).toEqual(['Morphology', 'Channels'])
    expect(measureGroups({})).toEqual([])
  })
})

describe('measureGroups — track', () => {
  it('titles a popType-only motility list "Track measures", never Morphology', () => {
    const g = measureGroups({ columns: MOTILITY, spatialAxes: ['centroid_t'], popType: 'track' })
    expect(g.map(x => x.title)).toEqual(['Track measures', 'Spatial / Time'])
    expect(g[0]!.cols).toEqual(MOTILITY)
  })

  it('keeps motility and cell vars apart when both are named', () => {
    const g = measureGroups({ trackColumns: MOTILITY, columns: FLOW.columns,
                              channels: FLOW.channels, obsColumns: ['live.cell.speed'],
                              popType: 'track' })
    expect(g.map(x => x.title)).toEqual(['Track measures', 'Morphology', 'Channels', 'Behaviour'])
    expect(g[1]!.cols).not.toContain('live.track.speed')
  })

  it('trackclust routes the same way as track', () => {
    expect(titles({ columns: MOTILITY, popType: 'trackclust' })).toEqual(['Track measures'])
    expect(isTrackPopType('track')).toBe(true)
    expect(isTrackPopType('trackclust')).toBe(true)
    expect(isTrackPopType('flow')).toBe(false)
    expect(isTrackPopType(undefined)).toBe(false)
  })
})

describe('measureGroups — data that is not the happy path', () => {
  // legacy / partially-migrated h5ads list the centroids as ordinary var columns AND on
  // `spatialColumns`; the picker used to render both, so `centroid_x` appeared twice.
  it('emits a centroid listed on both keys once, under Spatial / Time', () => {
    const g = measureGroups({ columns: ['area', 'centroid_x', 'mean_intensity_0'],
                              channels: ['mean_intensity_0'], spatialAxes: ['centroid_x'] })
    expect(g).toEqual([{ title: 'Morphology', cols: ['area'] },
                       { title: 'Channels', cols: ['mean_intensity_0'] },
                       { title: 'Spatial / Time', cols: ['centroid_x'] }])
  })

  it('groups a centroid var the server never declared as spatial', () => {
    expect(group({ columns: ['area', 'centroid_y'] }, 'Spatial / Time')).toEqual(['centroid_y'])
  })

  // the track branch of the endpoint returns `channelNames` but no `channels`, so the intensity
  // columns must still be recognised by name — otherwise they read as morphology there.
  it('recognises intensity columns by name when the caller has no channel list', () => {
    expect(group({ columns: ['area', 'mean_intensity_0', 'nuc_median_intensity_2'] }, 'Channels'))
      .toEqual(['mean_intensity_0', 'nuc_median_intensity_2'])
  })

  it('isIntensityColumn matches Julia channel_columns', () => {
    expect(isIntensityColumn('mean_intensity_0')).toBe(true)
    expect(isIntensityColumn('median_intensity_12')).toBe(true)
    expect(isIntensityColumn('nuc_mean_intensity_3')).toBe(true)
    expect(isIntensityColumn('area')).toBe(false)
    expect(isIntensityColumn('mean_intensity')).toBe(false)
    expect(isIntensityColumn('mean_intensity_0_sd')).toBe(false)
  })

  it('a column on two keys is emitted once, earliest group wins', () => {
    const all = groupedCols(measureGroups({ columns: ['track_id'], obsColumns: ['track_id'] }))
    expect(all).toEqual(['track_id'])
  })
})
