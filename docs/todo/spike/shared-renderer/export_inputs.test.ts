// SHARED_RENDERER_PLAN Phase 0 — the viewer side of the spike. Runs the viewer's OWN TS (not a copy)
// to produce what `volumeRenderer.ts` would hand the GPU for one view: the MIP WGSL with its constants
// interpolated, the uniform block, the LUT texture and the label palette. The Python host
// (`render_frame.py`) uploads these verbatim, so the only thing it re-derives is the volume itself.
//
// The uniforms are packed by lane NAME (`shaders/uniforms.json`, the table `volumeRenderer.ts` writes
// by) — Phase 1 replaced the slot-by-slot mirror this spike started with.
//
// Run (from frontend/):
//   SPIKE_IN=<inputs.json> SPIKE_OUT=<gpu_inputs.json> npx vitest run --dir ../docs/todo/spike/shared-renderer
import { it } from 'vitest'
import { readFileSync, writeFileSync } from 'node:fs'
import { MIP_WGSL } from '../../../../frontend/src/lib/webgpu/mipShader'
import { packUniforms } from '../../../../frontend/src/lib/webgpu/shaderSource'
import {
  MAX_CHANNELS, LUT_STOPS, VIEW_HALF_ANGLE, extentUm, lutTextureBytes, lutFromHex, fitCamera,
  type ViewerMeta, type ViewerChannel,
} from '../../../../frontend/src/utils/volumeViewer'
import { applyViewStateToBrowser, type ViewerViewState } from '../../../../frontend/src/utils/viewer/viewState'
import { LABEL_PALETTE_N, labelPaletteBytes, PICK_BUFFER_BYTES } from '../../../../frontend/src/utils/viewerLabels'

it.skipIf(!process.env.SPIKE_IN)('export one view as GPU inputs', () => {
  const inp = JSON.parse(readFileSync(process.env.SPIKE_IN!, 'utf-8')) as {
    viewState: ViewerViewState, names: string[], nX: number, nY: number, nZ: number,
    voxelUm: [number, number, number], steps: number, width: number, height: number,
  }
  const vs = inp.viewState
  // A channel per store channel, coloured the way the viewer colours a hex pick (`lutFromHex`).
  const channels: ViewerChannel[] = inp.names.map(name => {
    const l = vs.layers[name]
    const [lo, hi] = l?.contrast_limits ?? [0, 1]
    return { name, lo, hi, visible: !!l?.visible, lut: lutFromHex(String(l?.colormap ?? '#000000')) }
  })
  const meta = { nC: channels.length, nZ: inp.nZ, nX: inp.nX, nY: inp.nY, voxelUm: inp.voxelUm,
                 channels, bytesPerVoxel: 2 } as unknown as ViewerMeta
  const ext = extentUm(meta, inp.nZ)
  const applied = applyViewStateToBrowser({
    vs, meta, currentCam: fitCamera(ext, inp.width / inp.height, false),
    canvasH: inp.height, viewHalfAngle: VIEW_HALF_ANGLE,
  })
  const cam = applied.cam

  const lanes: Record<string, number> = {
    'cam.yaw': cam.yaw, 'cam.pitch': cam.pitch, 'cam.dist': cam.dist, 'cam.steps': inp.steps,
    'vp.nch': channels.length, 'vp.canvasW': inp.width, 'vp.canvasH': inp.height,
    'vp.ortho': 1,                          // the plane view, and 3D's default projection
    'ext.x': ext[0], 'ext.y': ext[1], 'ext.z': ext[2],
    'dims.nx': inp.nX, 'dims.ny': inp.nY, 'dims.nz': inp.nZ, 'dims.zPerChannel': inp.nZ,
    'ov.pointPx': 1, 'ov.planeLo': -1, 'ov.tailPx': 1, 'ov.planeHi': -1,              // overlays: none
    'pan.x': cam.panX || 0, 'pan.y': cam.panY || 0, 'pan.ribbonLo': -1, 'pan.ribbonHi': -1,
  }
  for (let c = 0; c < MAX_CHANNELS; c++) {
    const ch = applied.channels[c]
    lanes[`ch[${c}].lo`] = ch ? ch.lo : 0
    lanes[`ch[${c}].hi`] = ch ? ch.hi : 1
    lanes[`ch[${c}].visible`] = ch?.visible ? 1 : 0
  }
  const u = packUniforms('mip', lanes)

  writeFileSync(process.env.SPIKE_OUT!, JSON.stringify({
    wgsl: MIP_WGSL,
    uniforms: Array.from(u),
    lut: Buffer.from(lutTextureBytes(channels)).toString('base64'), lutSize: [LUT_STOPS, MAX_CHANNELS],
    pal: Buffer.from(labelPaletteBytes()).toString('base64'), palSize: [LABEL_PALETTE_N, 1],
    pickBytes: PICK_BUFFER_BYTES,
    cam,
  }))
})
