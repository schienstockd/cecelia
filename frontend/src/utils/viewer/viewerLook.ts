// What the browser viewer is DRAWING, read into the one movie-config shape (`BatchMovieCfg`).
//
// Three surfaces want "the look on screen": the viewer panel's Record (match-viewer mode), the Batch
// page's Fill from view, and the Animation page's render. They used to read it from the published
// `viewState` alone (`seedConfigFromViewState`), which only carries CHANNEL layers — the browser viewer
// draws populations, tracks and masks from per-set / per-image settings, not from layers, so every
// one of those surfaces recorded channels and nothing else. The overlay half lives in settings; this
// is the one place that reads it.
//
// Split in two so the mapping is testable without stores: `viewerLook` is pure, `readViewerLook`
// gathers its inputs from the settings store + the published viewer state.

import { seedConfigFromViewState, clampContour, type BatchMovieCfg, type ViewStateLike } from '../batchMovie'
import { viewPlaneZ, type ViewerViewState } from './viewState'
import { useSettingsStore } from '../../stores/settings'
import { useViewerStore } from '../../stores/viewer'
import { trackableValueNames } from '../overlayAutoShow'
import { colourForRender, channelsForRender } from '../viewerColormap'

export interface ViewerLookInput {
  viewState: (ViewStateLike & Partial<Pick<ViewerViewState, 'dims' | 'camera' | 'canvas'>>) | null
  channelNames: string[]
  /** the image version on screen ('' = unknown → the recorder's own default) */
  version: string
  /** the segmentation whose mask is drawn ('' = none) */
  maskValueName: string
  /** the pop manager's current (segmentation, popType) — what the viewer draws pops from */
  gating: { valueName: string; popType: string }
  popVisible: (popType: string) => boolean
  /** per-segmentation "directions" eye */
  trackVisible: Record<string, boolean>
  trackSourceColours: Record<string, string>
  showGatedTracks: boolean
  pointSize: number
  pointBorder: number
  labelOpacity: number
  tailWidth: number
  tailLength: number
  labelContour: number
  trackColourMode: string
  colourBy: string
  colourOverrides: Record<string, string>
}

/** Default colour for a track source with no override — the viewer's own grey. */
const TRACK_SOURCE_GREY = '#9ca3af'

export function viewerLook(i: ViewerLookInput): BatchMovieCfg {
  const popType = i.gating.popType || 'flow'
  const tracked = Object.keys(i.trackVisible).filter(vn => i.trackVisible[vn])
  const popsOn = i.popVisible(popType)
  const popSeg = (popsOn ? i.gating.valueName : '') || tracked[0] || i.gating.valueName || i.maskValueName
  const is3D = i.viewState?.dims?.ndisplay === 3
  const c = i.viewState?.camera
  const canvas = i.viewState?.canvas
  const cam = c && Array.isArray(c.angles) && typeof c.zoom === 'number'
    ? { angles: [c.angles[0] ?? 0, c.angles[1] ?? 0, c.angles[2] ?? 0] as [number, number, number], zoom: c.zoom,
        ...(canvas?.width && canvas?.height ? { width: canvas.width, height: canvas.height } : {}),
        ...(c.perspective ? { perspective: 1 } : {}) }
    : null
  const z = viewPlaneZ(i.viewState)
  const out: BatchMovieCfg = {
    ...seedConfigFromViewState(i.viewState, i.channelNames),
    ...(i.version ? { valueNames: [i.version] } : {}),
    labelValueNames: i.maskValueName ? [i.maskValueName] : [],
    labelContour: clampContour(i.labelContour),
    show3D: is3D,
    zSlice: z,
    // the 3D camera, for a batch that renders every image from the viewer's angle (Fill from view)
    ...(is3D && cam ? { camera3d: cam } : {}),
    showPopulations: popsOn,
    popType,
    // The segmentation the overlays are drawn from: the pop manager's while pops are on; with only
    // tracks ticked, the first tracked one (the renderers take one segmentation for pops + tracks).
    ...(popSeg ? { popValueName: popSeg } : {}),
    showGatedTracks: i.showGatedTracks,
    showTrackclust: i.popVisible('trackclust'),
    showTracks: tracked.length > 0,
    trackSources: Object.fromEntries(tracked.map(vn =>
      [vn, { visible: true, colour: i.trackSourceColours[vn] || TRACK_SOURCE_GREY }])),
    pointsSize: i.pointSize,
    pointBorder: i.pointBorder,
    labelOpacity: i.labelOpacity,
    tailWidth: i.tailWidth,
    tailLength: i.tailLength,
    trackColourMode: i.trackColourMode,
  }
  if (i.colourBy) {
    out.colourBy = i.colourBy
    if (Object.keys(i.colourOverrides).length) out.colourOverrides = i.colourOverrides
  }
  return out
}

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
  labels?: Record<string, unknown>
  labelPropsNames?: string[]
  activeValueName?: string
}

/** `viewerLook` fed from the stores: the published view state of the browser viewer, and the per-set /
 *  per-image overlay settings the viewer draws from.
 *
 *  `requireOpen` (default): `null` unless the browser viewer is showing `img` — there is no "on
 *  screen" to read. Off: always a look — the overlay half is settings and exists whether or not the
 *  viewer is open; channels / z / 3D come from whatever the viewer publishes, if anything. */
export function readViewerLook(img: LookImage, setUid: string,
                               { requireOpen = true }: { requireOpen?: boolean } = {}): BatchMovieCfg | null {
  const settings = useSettingsStore()
  const viewer = useViewerStore()
  const onImg = !!viewer.viewState && viewer.openImage?.imageUid === img.uid
  if (requireOpen && !onImg) return null
  const labelNames = Object.keys(img.labels ?? {})
  // The viewer draws ONE mask: the first ticked segmentation (ViewerWindow `labelName`).
  const labelVis = settings.getLabelVisibility(img.uid, labelNames)
  const colourBy = setUid ? settings.getColourBy(setUid) : ''
  return viewerLook({
    viewState: (viewer.viewState ?? null) as unknown as ViewerLookInput['viewState'],
    channelNames: img.channelNames ?? [],
    version: (onImg ? viewer.openImage?.valueName : '') || img.activeValueName || '',
    maskValueName: labelNames.find(n => labelVis[n]) ?? '',
    gating: readGatingCurrent(img.uid),
    popVisible: pt => setUid ? settings.getPopVisible(setUid, pt) : false,
    trackVisible: settings.getTrackVisibility(img.uid, trackableValueNames(img)),
    trackSourceColours: setUid ? settings.getTrackSourceColours(setUid) : {},
    showGatedTracks: setUid ? settings.getShowGatedTracks(setUid) : false,
    pointSize: setUid ? settings.getPointSize(setUid) : settings.viewerPointSize,
    pointBorder: setUid ? settings.getPointBorder(setUid) : settings.viewerPointBorder,
    labelOpacity: settings.viewerLabelOpacity,
    tailWidth: settings.viewerTailWidth,
    tailLength: settings.viewerTailLength,
    labelContour: settings.viewerLabelContour,
    trackColourMode: setUid ? settings.getTrackColorMode(setUid) : 'track',
    colourBy,
    colourOverrides: setUid && colourBy ? settings.getColourOverrides(setUid, colourBy) : {},
  })
}

/** A look whose channel colours are the hex the viewer shows (`colourForRender`). */
export const lookForRender = (l: BatchMovieCfg): BatchMovieCfg => ({ ...l, channels: channelsForRender(l.channels) })

/** A view state whose layer colormaps are the hex the viewer shows (`colourForRender`). Not mutated. */
export function hexViewState<T extends { layers?: Record<string, { colormap?: unknown }> }>(vs: T): T {
  if (!vs?.layers) return vs
  const layers = Object.fromEntries(Object.entries(vs.layers).map(([n, l]) =>
    [n, typeof l?.colormap === 'string' ? { ...l, colormap: colourForRender(l.colormap) } : l]))
  return { ...vs, layers }
}

/** The view a 3D recording renders from: the viewer's own when it is in 3D, else straight on (zoom 1,
 *  no centre → the volume midpoint) with the viewer's channels, in the projection the viewer's 3D view
 *  would use (`perspective`, the toggle). For a Record set to 3D while the viewer sits in 2D — there
 *  is no 3D camera on screen to copy. */
export function volumeViewState(vs: ViewerViewState | null, perspective = false): ViewerViewState {
  if (vs?.dims?.ndisplay === 3) return vs
  const t = vs?.dims?.current_step?.[0] ?? 0
  return {
    camera: { zoom: 1, angles: [0, 0, 0], perspective: perspective ? 1 : 0 } as unknown as ViewerViewState['camera'],
    dims: { ndisplay: 3, current_step: [t, 0], point: [t, 0] },
    layers: vs?.layers ?? {},
    canvas: vs?.canvas ?? { width: 0, height: 0 },
  }
}

/** A timelapse of ONE view: two keyframes identical but for t, `tStart` → `tEnd`, one frame per
 *  timepoint. How a live view is recorded through the keyframe renderer — the renderer that draws a
 *  3D volume with the viewer's camera — without authoring an animation. */
export function timelapseKeyframes(vs: ViewerViewState, tStart: number, tEnd: number):
    { viewState: ViewerViewState; steps: number }[] {
  const t0 = Math.max(0, Math.round(tStart))
  const t1 = Math.max(t0, Math.round(tEnd))
  const at = (t: number): ViewerViewState => {
    const step = [...(vs.dims?.current_step ?? [0, 0])]
    const point = [...(vs.dims?.point ?? step)]
    step[0] = t; point[0] = t
    return { ...vs, dims: { ...vs.dims, current_step: step, point } }
  }
  return [{ viewState: at(t0), steps: 1 }, { viewState: at(t1), steps: Math.max(1, t1 - t0) }]
}
