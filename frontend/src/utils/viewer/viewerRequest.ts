// The viewer's side of an MCP "ask the open viewer" request (`api/src/viewer_request.jl`): the
// server broadcasts `viewer:request` {kind, requestId, args}; the window showing the image answers on
// `/api/viewer/reply`. `kind: 'screenshot'` → the Save-image composite; `kind: 'view'` → a partial
// view state merged onto the live one and applied through the keyframe restore. The pure parts live
// here; ViewerWindow wires them to its live state.

import type { ViewerViewState, ViewerLayerState } from './viewState'

/** A partial view state as Claude sends it: any subset of camera / dims / layers, a layer naming a
 *  channel with any of its fields. */
export interface ViewStatePatch {
  camera?: Partial<ViewerViewState['camera']>
  dims?: Partial<ViewerViewState['dims']>
  layers?: Record<string, Partial<ViewerLayerState>>
}

const isObj = (v: unknown): v is Record<string, unknown> => !!v && typeof v === 'object' && !Array.isArray(v)

/** `patch` laid over `base`, field by field: a field the patch leaves out keeps the live value. A
 *  layer the image has no channel of is dropped (returned in `unknown`) rather than applied to nothing. */
export function mergeViewState(base: ViewerViewState, patch: unknown): { view: ViewerViewState; unknown: string[] } {
  const p = isObj(patch) ? patch as ViewStatePatch : {}
  const layers: Record<string, ViewerLayerState> = {}
  for (const [name, l] of Object.entries(base.layers)) layers[name] = { ...l }
  const unknown: string[] = []
  for (const [name, l] of Object.entries(isObj(p.layers) ? p.layers : {})) {
    if (!layers[name]) { unknown.push(name); continue }
    if (isObj(l)) Object.assign(layers[name], l)
  }
  const dims = { ...base.dims, ...(isObj(p.dims) ? p.dims : {}) }
  // a new t or z given without the other: keep the live one in its slot of current_step
  const step = [...base.dims.current_step]
  const ps = isObj(p.dims) && Array.isArray(p.dims.current_step) ? p.dims.current_step : []
  ps.forEach((v, i) => { if (typeof v === 'number' && Number.isFinite(v)) step[i] = v })
  dims.current_step = step
  dims.point = [...step]
  return {
    view: { ...base, camera: { ...base.camera, ...(isObj(p.camera) ? p.camera : {}) }, dims, layers },
    unknown,
  }
}

/** The size a `w`×`h` picture is scaled to so its long side is at most `maxSide` (never up). */
export function fitLongSide(w: number, h: number, maxSide?: number): { width: number; height: number } {
  const long = Math.max(w, h)
  if (!maxSide || maxSide <= 0 || long <= maxSide) return { width: w, height: h }
  const s = maxSide / long
  return { width: Math.max(1, Math.round(w * s)), height: Math.max(1, Math.round(h * s)) }
}
