// What the browser viewer draws, as one description — the draw spec (docs/todo/DRAW_SPEC_PLAN.md).
//
// `readDrawSpec` is the ONE reader of the settings that decide the picture. Every recording (Record,
// batch Fill from view, Animation) turns it into a movie config through `viewerLook`; the viewer itself
// moves onto it in the plan's Phase 2. `RECORDED` says, per field, whether a recording draws it — adding
// a field without deciding is a type error, and `drawSpec.test.ts` checks every `recorded` field reaches
// the look.

import { useSettingsStore } from '../../stores/settings'
import { useViewerStore } from '../../stores/viewer'
import { trackableValueNames } from '../overlayAutoShow'
import type { ViewerLookInput } from './viewerLook'

/** The pop manager's current (segmentation, popType) for one image, as published to
 *  `cc.gatingCurrent` by the gating store. Empty strings = nothing selected yet. */
export function readGatingCurrent(imageUid: string): { valueName: string; popType: string } {
  if (typeof localStorage === 'undefined' || !imageUid) return { valueName: '', popType: '' }
  try {
    const bag = JSON.parse(localStorage.getItem('cc.gatingCurrent') ?? '{}') as
                Record<string, { valueName?: string; popType?: string }>
    const e = bag[imageUid] ?? {}
    return { valueName: String(e.valueName ?? ''), popType: String(e.popType ?? '') }
  } catch { return { valueName: '', popType: '' } }
}

export interface LookImage {
  uid: string
  channelNames?: string[]
  sizeZ?: number | null
  labels?: Record<string, unknown>
  branchLabels?: Record<string, unknown>
  labelPropsNames?: string[]
  activeValueName?: string
}

/** Everything that decides the viewer's picture. The recorded half is `viewerLook`'s input. */
export interface ViewerDrawSpec extends ViewerLookInput {
  /** per skeleton segmentation, its eye (`settings.getBranchVisibility`) */
  branchVisible: Record<string, boolean>
  scaleBar: boolean
  timestamp: boolean
  scaleBarPx: number
  timestampPx: number
  /** 3D ray steps (`settings.viewerSteps`) */
  steps: number
  grid: boolean
  gridDensity: number
  landscape: boolean
  landscapeLabels: boolean
  roiSpotlight: boolean
}

/** `recorded`: a recording draws it, through `viewerLook`. `notRecorded`: an on-screen aid — the
 *  maintainer's call, with the reason. `pending`: decided recorded, not wired yet (the plan's Phase 3). */
export type RecordStatus = 'recorded' | { notRecorded: string } | { pending: string }

const AID = { notRecorded: 'on-screen aid (maintainer, 2026-11-12)' }

export const RECORDED: Record<keyof ViewerDrawSpec, RecordStatus> = {
  viewState: 'recorded', channelNames: 'recorded', nZ: 'recorded', version: 'recorded',
  maskValueName: 'recorded', labelFilesHidden: 'recorded', gating: 'recorded', popVisible: 'recorded',
  trackVisible: 'recorded', trackSourceColours: 'recorded', showGatedTracks: 'recorded',
  hiddenTrackPops: 'recorded', popEyes: 'recorded', ribbonEyes: 'recorded', pointSize: 'recorded',
  pointBorder: 'recorded', labelOpacity: 'recorded', pointZTol: 'recorded', trackZTol: 'recorded',
  tailWidth: 'recorded', tailLength: 'recorded', labelContour: 'recorded', maskColour: 'recorded',
  trackColourMode: 'recorded', colourBy: 'recorded', colourOverrides: 'recorded',
  branchVisible: { pending: 'skeletons in every recording, Fill from view too' },
  scaleBar: { pending: 'today the record options decide on / off' },
  timestamp: { pending: 'today the record options decide on / off' },
  scaleBarPx: { pending: 'the viewer’s size' },
  timestampPx: { pending: 'the viewer’s size' },
  steps: { pending: 'the viewer’s Steps by default; Render quality stays an override' },
  grid: AID, gridDensity: AID, landscape: AID, landscapeLabels: AID, roiSpotlight: AID,
}

/** The draw spec of `img` from the stores: the published view state of the browser viewer, and the
 *  per-set / per-image settings the viewer draws from. `onImg`: the viewer shows `img`, so its version
 *  is the one on screen. */
export function readDrawSpec(img: LookImage, setUid: string, onImg: boolean): ViewerDrawSpec {
  const settings = useSettingsStore()
  const viewer = useViewerStore()
  const labelNames = Object.keys(img.labels ?? {})
  // The viewer draws ONE mask: the first ticked segmentation (ViewerWindow `labelName`).
  const labelVis = settings.getLabelVisibility(img.uid, labelNames)
  const maskValueName = labelNames.find(n => labelVis[n]) ?? ''
  const colourBy = setUid ? settings.getColourBy(setUid) : ''
  return {
    viewState: (viewer.viewState ?? null) as unknown as ViewerLookInput['viewState'],
    channelNames: img.channelNames ?? [],
    nZ: img.sizeZ ?? 0,
    version: (onImg ? viewer.openImage?.valueName : '') || img.activeValueName || '',
    maskValueName,
    labelFilesHidden: maskValueName ? [...settings.getLabelFilesHidden(img.uid, maskValueName)] : [],
    gating: readGatingCurrent(img.uid),
    popVisible: pt => setUid ? settings.getPopVisible(setUid, pt) : false,
    trackVisible: settings.getTrackVisibility(img.uid, trackableValueNames(img)),
    trackSourceColours: setUid ? settings.getTrackSourceColours(setUid) : {},
    showGatedTracks: setUid ? settings.getShowGatedTracks(setUid) : false,
    hiddenTrackPops: Object.fromEntries(trackableValueNames(img).map(vn =>
      [vn, [...settings.getTrackPopHidden(img.uid, vn)]])),
    popEyes: settings.getImagePopEyes(img.uid, 'points'),
    ribbonEyes: settings.getImagePopEyes(img.uid, 'ribbons'),
    pointSize: setUid ? settings.getPointSize(setUid) : settings.viewerPointSize,
    pointBorder: setUid ? settings.getPointBorder(setUid) : settings.viewerPointBorder,
    labelOpacity: settings.viewerLabelOpacity,
    pointZTol: settings.viewerPointZTol,
    trackZTol: settings.viewerTrackZTol,
    tailWidth: settings.viewerTailWidth,
    tailLength: settings.viewerTailLength,
    labelContour: settings.viewerLabelContour,
    maskColour: settings.viewerMaskColour,
    trackColourMode: setUid ? settings.getTrackColorMode(setUid) : 'track',
    colourBy,
    colourOverrides: setUid && colourBy ? settings.getColourOverrides(setUid, colourBy) : {},
    branchVisible: settings.getBranchVisibility(img.uid, Object.keys(img.branchLabels ?? {})),
    scaleBar: settings.viewerScaleBar,
    timestamp: settings.viewerTimestamp,
    scaleBarPx: settings.viewerScaleBarPx,
    timestampPx: settings.viewerTimestampPx,
    steps: settings.viewerSteps,
    grid: settings.viewerGrid,
    gridDensity: settings.viewerGridDensity,
    landscape: settings.viewerLandscape,
    landscapeLabels: settings.viewerLandscapeLabels,
    roiSpotlight: settings.viewerRoiSpotlight,
  }
}
