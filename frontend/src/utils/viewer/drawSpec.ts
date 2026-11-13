// What the browser viewer draws, as one description — the draw spec (docs/todo/DRAW_SPEC_PLAN.md).
//
// `readDrawSpec` is the ONE reader of the settings that decide the picture. Every recording (Record,
// batch Fill from view, Animation) turns it into a movie config through `viewerLook`; the viewer itself
// reads the same fields live (`drawSpecOf`). `RECORDED` says, per field, whether a recording draws it — adding
// a field without deciding is a type error, and `drawSpec.test.ts` checks every `recorded` field reaches
// the look.

import { useSettingsStore } from '../../stores/settings'
import { useViewerStore } from '../../stores/viewer'
import { SPEC_POP_TYPES, trackableValueNames } from '../overlayAutoShow'
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
  branchVisible: 'recorded',
  scaleBar: { pending: 'today the record options decide on / off' },
  timestamp: { pending: 'today the record options decide on / off' },
  scaleBarPx: { pending: 'the viewer’s size' },
  timestampPx: { pending: 'the viewer’s size' },
  steps: { pending: 'the viewer’s Steps by default; Render quality stays an override' },
  grid: AID, gridDensity: AID, landscape: AID, landscapeLabels: AID, roiSpotlight: AID,
}

/** What a draw spec is read for: one image, in one set. Each field may be a getter, so a live viewer
 *  passes its own reactive state (`meta`'s names, its gating ref) and a recording passes plain values. */
export interface DrawSpecSource {
  imageUid: string
  setUid: string
  /** the image's segmentations — the mask is the first ticked one */
  labelNames: string[]
  /** the segmentations with a cell table — the track eyes' and ribbon hides' keys */
  trackNames: string[]
  /** the skeleton segmentations */
  branchNames: string[]
  channelNames: string[]
  nZ: number
  version: string
  viewState: ViewerLookInput['viewState']
  gating: { valueName: string; popType: string }
}


/** The draw spec of `src`, every field a getter over the stores. Reading one field tracks only the
 *  settings it reads, so the viewer's computeds react as they did when they read the settings
 *  themselves; `{ ...drawSpecOf(src) }` is a snapshot (`readDrawSpec`). */
export function drawSpecOf(src: DrawSpecSource): ViewerDrawSpec {
  const settings = useSettingsStore()
  const perSet = <T>(read: (su: string) => T, none: T): T => src.setUid ? read(src.setUid) : none
  return {
    get viewState() { return src.viewState },
    get channelNames() { return src.channelNames },
    get nZ() { return src.nZ },
    get version() { return src.version },
    // The viewer draws ONE mask: the first ticked segmentation.
    get maskValueName() {
      const names = src.labelNames
      if (!src.imageUid || !names.length) return ''
      const vis = settings.getLabelVisibility(src.imageUid, names)
      return names.find(n => vis[n]) ?? ''
    },
    get labelFilesHidden() {
      const vn = this.maskValueName
      return vn && src.imageUid ? [...settings.getLabelFilesHidden(src.imageUid, vn)] : []
    },
    get gating() { return src.gating },
    // one getter per pop type, so a reader of one type tracks that type's setting only
    get popVisible() {
      const out: Record<string, boolean> = {}
      for (const pt of SPEC_POP_TYPES)
        Object.defineProperty(out, pt, { enumerable: true, get: () => perSet(su => settings.getPopVisible(su, pt), false) })
      return out
    },
    get trackVisible() {
      return src.imageUid ? settings.getTrackVisibility(src.imageUid, src.trackNames) : {}
    },
    get trackSourceColours() { return perSet(su => settings.getTrackSourceColours(su), {}) },
    get showGatedTracks() { return perSet(su => settings.getShowGatedTracks(su), false) },
    get hiddenTrackPops() {
      return Object.fromEntries(src.trackNames.map(vn => [vn, [...settings.getTrackPopHidden(src.imageUid, vn)]]))
    },
    get popEyes() { return settings.getImagePopEyes(src.imageUid, 'points') },
    get ribbonEyes() { return settings.getImagePopEyes(src.imageUid, 'ribbons') },
    get pointSize() { return perSet(su => settings.getPointSize(su), settings.viewerPointSize) },
    get pointBorder() { return perSet(su => settings.getPointBorder(su), settings.viewerPointBorder) },
    get labelOpacity() { return settings.viewerLabelOpacity },
    get pointZTol() { return settings.viewerPointZTol },
    get trackZTol() { return settings.viewerTrackZTol },
    get tailWidth() { return settings.viewerTailWidth },
    get tailLength() { return settings.viewerTailLength },
    get labelContour() { return settings.viewerLabelContour },
    get maskColour() { return settings.viewerMaskColour },
    get trackColourMode() { return perSet(su => settings.getTrackColorMode(su), 'track') },
    get colourBy() { return perSet(su => settings.getColourBy(su), '') },
    get colourOverrides() {
      const by = this.colourBy
      return src.setUid && by ? settings.getColourOverrides(src.setUid, by) : {}
    },
    get branchVisible() {
      return src.imageUid ? settings.getBranchVisibility(src.imageUid, src.branchNames) : {}
    },
    get scaleBar() { return settings.viewerScaleBar },
    get timestamp() { return settings.viewerTimestamp },
    get scaleBarPx() { return settings.viewerScaleBarPx },
    get timestampPx() { return settings.viewerTimestampPx },
    get steps() { return settings.viewerSteps },
    get grid() { return settings.viewerGrid },
    get gridDensity() { return settings.viewerGridDensity },
    get landscape() { return settings.viewerLandscape },
    get landscapeLabels() { return settings.viewerLandscapeLabels },
    get roiSpotlight() { return settings.viewerRoiSpotlight },
  }
}

/** The draw spec of `img` from the stores, as plain data: the published view state of the browser
 *  viewer, and the per-set / per-image settings the viewer draws from. `onImg`: the viewer shows `img`,
 *  so its version, and the segmentation names it draws from (`openImage`), are the ones on screen. */
export function readDrawSpec(img: LookImage, setUid: string, onImg: boolean): ViewerDrawSpec {
  const viewer = useViewerStore()
  const open = onImg ? viewer.openImage : null
  const live = drawSpecOf({
    imageUid: img.uid, setUid,
    labelNames: open?.labelNames ?? Object.keys(img.labels ?? {}),
    trackNames: open?.trackNames ?? trackableValueNames(img),
    branchNames: open?.branchNames ?? Object.keys(img.branchLabels ?? {}),
    channelNames: img.channelNames ?? [],
    nZ: img.sizeZ ?? 0,
    version: open?.valueName || img.activeValueName || '',
    viewState: (viewer.viewState ?? null) as unknown as ViewerLookInput['viewState'],
    gating: readGatingCurrent(img.uid),
  })
  return { ...live, popVisible: { ...live.popVisible } }
}
