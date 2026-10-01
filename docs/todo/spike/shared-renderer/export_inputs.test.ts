// SHARED_RENDERER_PLAN Phase 0 — the viewer side of the spike. Runs the viewer's OWN TS (not a copy)
// to produce what `volumeRenderer.ts` would hand the GPU for one view: the MIP WGSL with its constants
// interpolated, the uniform block, the LUT texture and the label palette. The Python host
// (`render_frame.py`) uploads these verbatim, so the only thing it re-derives is the volume itself.
//
// The uniform packing below MIRRORS `volumeRenderer.ts` slot by slot (setImage / setCamera /
// setChannels / render) — it is not exported from there, which is exactly Decision 3's point.
//
// Run (from frontend/):
//   SPIKE_IN=<inputs.json> SPIKE_OUT=<gpu_inputs.json> npx vitest run --dir ../docs/todo/spike/shared-renderer
import { it } from 'vitest'
import { readFileSync, writeFileSync } from 'node:fs'
import { MIP_WGSL } from '../../../../frontend/src/lib/webgpu/mipShader'
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

  const u = new Float32Array((7 + MAX_CHANNELS) * 4)
  u[0] = cam.yaw; u[1] = cam.pitch; u[2] = cam.dist; u[3] = inp.steps            // setCamera + render
  u[4] = channels.length; u[5] = inp.width; u[6] = inp.height                       // setImage + render
  u[7] = 1                                    // orthographic: the plane view, and 3D's default projection
  u[8] = ext[0]; u[9] = ext[1]; u[10] = ext[2]; u[11] = 0
  u[12] = inp.nX; u[13] = inp.nY; u[14] = inp.nZ; u[15] = inp.nZ
  u[16] = 1; u[17] = -1; u[18] = 1; u[19] = -1                                      // overlays: none
  u[24] = cam.panX || 0; u[25] = cam.panY || 0; u[26] = -1; u[27] = -1
  for (let c = 0; c < MAX_CHANNELS; c++) {                                          // setChannels
    const ch = applied.channels[c]
    u[28 + c * 4] = ch ? ch.lo : 0; u[29 + c * 4] = ch ? ch.hi : 1; u[30 + c * 4] = ch?.visible ? 1 : 0
  }

  writeFileSync(process.env.SPIKE_OUT!, JSON.stringify({
    wgsl: MIP_WGSL,
    uniforms: Array.from(u),
    lut: Buffer.from(lutTextureBytes(channels)).toString('base64'), lutSize: [LUT_STOPS, MAX_CHANNELS],
    pal: Buffer.from(labelPaletteBytes()).toString('base64'), palSize: [LABEL_PALETTE_N, 1],
    pickBytes: PICK_BUFFER_BYTES,
    cam,
  }))
})
