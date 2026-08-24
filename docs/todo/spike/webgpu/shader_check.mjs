// Generate a standalone page that compiles and runs the app's ACTUAL MIP shader.
//
// The one failure the app cannot rule out on its own is the shader: WGSL is compiled by the browser,
// there is no offline validator in this repo, and nothing here can open a WebGPU context (headless
// Firefox has no compositor — see NAPARI_WEBGPU_AUDIT.md → G0). So a broken shader would be found by
// Dominik, on his click, in a window that also needs the backend restarted first.
//
// This is NOT a second copy of the shader. It READS `frontend/src/lib/webgpu/mipShader.ts` and
// substitutes the two constants, so the page runs the exact string the app runs; a divergence is
// impossible by construction. It renders a PHANTOM (no server, no zarr), which is what lets it check
// the two things a real image cannot distinguish:
//
//   1. does the WGSL compile at all,
//   2. is the LUT texture indexed the right way round? Each channel gets a known primary and a known
//      radius, so a swapped row/column shows up as the wrong colour at the wrong place — whereas on
//      real data "odd colours" reads as the data being odd. It asserts the pixels, so the answer does
//      not depend on anyone's eyes, and
//   3. does the 2D path draw? A one-plane volume, orthographic, `steps = 1` — the single sample has to
//      land on the box midpoint, which IS the plane. Off by anything and a 2D view renders black, which
//      on real data is indistinguishable from an empty channel or a bad contrast window.
//
// Run:  node docs/todo/spike/webgpu/shader_check.mjs   → writes ~/Downloads/TMP/shader_check.html
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs'
import { join } from 'node:path'
import { homedir } from 'node:os'

const SRC = join(import.meta.dirname, '..', '..', '..', '..',
                 'frontend', 'src', 'lib', 'webgpu', 'mipShader.ts')
const src = readFileSync(SRC, 'utf8')

// The constants the shader interpolates come from volumeViewer.ts; read them rather than restate them.
const VV = readFileSync(join(import.meta.dirname, '..', '..', '..', '..',
                             'frontend', 'src', 'utils', 'volumeViewer.ts'), 'utf8')
const constOf = (name) => {
  const m = VV.match(new RegExp('export const ' + name + ' = (\\d+)'))
  if (!m) throw new Error('could not read ' + name + ' from volumeViewer.ts')
  return Number(m[1])
}
const MAX_CHANNELS = constOf('MAX_CHANNELS')
const LUT_STOPS = constOf('LUT_STOPS')
// The framing constant the camera and the shader share. Read, not retyped — the guard below fails if a
// NEW interpolation appears, which is how this one announced itself.
const VIEW_HALF_ANGLE = (() => {
  const m = VV.match(/export const VIEW_HALF_ANGLE = ([\d.]+)/)
  if (!m) throw new Error('could not read VIEW_HALF_ANGLE from volumeViewer.ts')
  return Number(m[1])
})()

// The WGSL is a template literal; take it verbatim and resolve only the two interpolations.
const open = src.indexOf('export const MIP_WGSL = `')
if (open < 0) throw new Error('MIP_WGSL not found — did the export shape change?')
const body = src.slice(open + 'export const MIP_WGSL = `'.length)
const close = body.indexOf('`')
if (close < 0) throw new Error('unterminated MIP_WGSL template literal')
let wgsl = body.slice(0, close)
if (wgsl.includes('${') === false) throw new Error('no interpolation found — check the extraction')
wgsl = wgsl.replaceAll('${MAX_CHANNELS}', String(MAX_CHANNELS))
           .replaceAll('${LUT_STOPS}', String(LUT_STOPS))
           .replaceAll('${VIEW_HALF_ANGLE}', String(VIEW_HALF_ANGLE))
if (wgsl.includes('${')) throw new Error('unresolved interpolation left in the WGSL: ' + wgsl.match(/\$\{[^}]*\}/))

const NCH = 3
const page = `<!doctype html><html><head><meta charset=utf-8><title>Cecelia — MIP shader check</title>
<style>
 body{font:13px/1.6 system-ui,sans-serif;background:#0f1115;color:#e6e6e6;margin:0;padding:16px}
 h1{font-size:15px;margin:0 0 6px}
 p{color:#9aa0a8;max-width:80ch;margin:0 0 12px}
 canvas{background:#000;border:1px solid #262b33;border-radius:6px}
 pre{background:#171a20;border:1px solid #262b33;border-radius:6px;padding:10px;margin:12px 0 0;
     font:12px ui-monospace,monospace;white-space:pre-wrap}
 .ok{color:#8ce99a}.bad{color:#ff8a80}
</style></head><body>
<h1>MIP shader check — generated from the app's own source</h1>
<p>Runs <code>frontend/src/lib/webgpu/mipShader.ts</code> verbatim on a phantom volume, so it needs no
backend and no zarr. Channel 0 is a <b>red</b> shell, 1 <b>green</b> at a larger radius, 2 <b>blue</b>
larger again — through the same LUT texture the app builds. It asserts the pixels, so a wrong answer is
reported rather than left to the eye.</p>
<canvas id=cv width=480 height=360></canvas>
<pre id=out>starting…</pre>
<script type="module">
const out = document.getElementById('out')
const lines = []
const say = (t, cls) => { lines.push(cls ? '<span class="'+cls+'">'+t+'</span>' : t); out.innerHTML = lines.join('\\n') }

const WGSL = ${JSON.stringify(wgsl)}
const MAX_CHANNELS = ${MAX_CHANNELS}, LUT_STOPS = ${LUT_STOPS}, NCH = ${NCH}
const N = 64                                     // phantom is N x N x N per channel

try {
  if (!('gpu' in navigator)) throw new Error('navigator.gpu absent — this browser has no WebGPU')
  const adapter = await navigator.gpu.requestAdapter({powerPreference: 'high-performance'})
  if (!adapter) throw new Error('no adapter')
  const d3 = adapter.limits.maxTextureDimension3D
  say('adapter: maxTextureDimension3D=' + d3 + (d3 > 2048 ? ' (discrete)' : ' (INTEGRATED)'),
      d3 > 2048 ? 'ok' : 'bad')
  const device = await adapter.requestDevice()
  device.addEventListener('uncapturederror', e => say('!! ' + e.error.message, 'bad'))

  const mod = device.createShaderModule({code: WGSL})
  const info = await mod.getCompilationInfo()
  const errs = info.messages.filter(m => m.type === 'error')
  if (errs.length) throw new Error('WGSL: ' + errs.map(m => m.lineNum + ':' + m.message).join(' | '))
  say('WGSL compiled OK (' + info.messages.length + ' messages, 0 errors)', 'ok')

  const cv = document.getElementById('cv')
  const ctx = cv.getContext('webgpu')
  const format = navigator.gpu.getPreferredCanvasFormat()
  ctx.configure({device, format, alphaMode: 'opaque'})

  const bgl = device.createBindGroupLayout({entries: [
    {binding: 0, visibility: GPUShaderStage.FRAGMENT, buffer: {type: 'uniform'}},
    {binding: 1, visibility: GPUShaderStage.FRAGMENT, texture: {sampleType: 'uint', viewDimension: '3d'}},
    {binding: 2, visibility: GPUShaderStage.FRAGMENT, texture: {sampleType: 'float', viewDimension: '2d'}}]})
  const rt = device.createTexture({size: [cv.width, cv.height], format: 'rgba8unorm',
    usage: GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.COPY_SRC})
  const pipe = f => device.createRenderPipeline({
    layout: device.createPipelineLayout({bindGroupLayouts: [bgl]}),
    vertex: {module: mod, entryPoint: 'vs'},
    fragment: {module: mod, entryPoint: 'fs', targets: [{format: f}]},
    primitive: {topology: 'triangle-list'}})
  const pipeCanvas = pipe(format), pipeOff = pipe('rgba8unorm')

  // volume: channels stacked along z, exactly as the app does
  const vol = device.createTexture({size: [N, N, N * NCH], dimension: '3d', format: 'r16uint',
    usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST})
  const plane = new Uint16Array(N * N)
  for (let c = 0; c < NCH; c++) {
    const R = 0.25 + 0.16 * c
    for (let z = 0; z < N; z++) {
      const zc = (z / N - 0.5) * 2
      for (let y = 0; y < N; y++) {
        const yc = (y / N - 0.5) * 2
        for (let x = 0; x < N; x++) {
          const xc = (x / N - 0.5) * 2
          const r = Math.hypot(xc, yc, zc)
          plane[y * N + x] = Math.abs(r - R) < 0.05 ? 4000 : 0
        }
      }
      device.queue.writeTexture({texture: vol, origin: [0, 0, c * N + z]},
        plane, {bytesPerRow: N * 2, rowsPerImage: N}, [N, N, 1])
    }
  }

  // LUT: one row per channel, black→primary — the layout lutTextureBytes produces
  const PRIM = [[255,0,0],[0,255,0],[0,0,255]]
  const lutBytes = new Uint8Array(LUT_STOPS * MAX_CHANNELS * 4)
  for (let c = 0; c < NCH; c++) for (let i = 0; i < LUT_STOPS; i++) {
    const f = i / (LUT_STOPS - 1), o = (c * LUT_STOPS + i) * 4
    lutBytes[o] = PRIM[c][0] * f; lutBytes[o+1] = PRIM[c][1] * f; lutBytes[o+2] = PRIM[c][2] * f
    lutBytes[o+3] = 255
  }
  const lut = device.createTexture({size: [LUT_STOPS, MAX_CHANNELS], format: 'rgba8unorm',
    usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST})
  device.queue.writeTexture({texture: lut}, lutBytes,
    {bytesPerRow: LUT_STOPS * 4, rowsPerImage: MAX_CHANNELS}, [LUT_STOPS, MAX_CHANNELS])

  const U = 4 * 16 + MAX_CHANNELS * 16
  const ubuf = device.createBuffer({size: U, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST})
  const bind = device.createBindGroup({layout: bgl, entries: [
    {binding: 0, resource: {buffer: ubuf}}, {binding: 1, resource: vol.createView()},
    {binding: 2, resource: lut.createView()}]})

  const u = new Float32Array(U / 4)
  function setUniforms(w, h, chVisible) {
    u[0] = 0.7; u[1] = 0.35; u[2] = N * 1.7; u[3] = 256          // yaw, pitch, dist, steps
    u[4] = NCH; u[5] = w; u[6] = h                                // nch, viewport
    u[8] = N; u[9] = N; u[10] = N                                 // physical extent (isotropic)
    u[12] = N; u[13] = N; u[14] = N; u[15] = N                    // nx, ny, nz, z per channel
    for (let c = 0; c < MAX_CHANNELS; c++) {
      const o = 16 + c * 4
      u[o] = 0; u[o+1] = 3000; u[o+2] = (chVisible[c] ?? 0)       // lo, hi, visible
    }
    device.queue.writeBuffer(ubuf, 0, u)
  }

  function drawTo(view, p) {
    const enc = device.createCommandEncoder()
    const pass = enc.beginRenderPass({colorAttachments: [{view,
      clearValue: {r:0,g:0,b:0,a:1}, loadOp: 'clear', storeOp: 'store'}]})
    pass.setPipeline(p); pass.setBindGroup(0, bind); pass.draw(3); pass.end()
    return enc
  }

  // read back a 64x1 strip through the middle row, so a channel's shell radius is measurable
  const BPR = 256, ROWS = 1
  async function strip(chVisible) {
    setUniforms(cv.width, cv.height, chVisible)
    const rb = device.createBuffer({size: BPR * ROWS,
      usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ})
    const enc = drawTo(rt.createView(), pipeOff)
    enc.copyTextureToBuffer({texture: rt, origin: [Math.floor(cv.width/2) - 32, Math.floor(cv.height/2), 0]},
                            {buffer: rb, bytesPerRow: BPR, rowsPerImage: ROWS}, [64, 1, 1])
    device.queue.submit([enc.finish()])
    await rb.mapAsync(GPUMapMode.READ)
    const px = new Uint8Array(rb.getMappedRange().slice(0))
    rb.unmap(); rb.destroy()
    return px
  }

  const brightest = px => {
    let best = [0,0,0], bs = -1
    for (let i = 0; i < 64; i++) {
      const s = px[i*4] + px[i*4+1] + px[i*4+2]
      if (s > bs) { bs = s; best = [px[i*4], px[i*4+1], px[i*4+2]] }
    }
    return best
  }

  // one channel at a time: each must light up in its OWN primary and nothing else
  const NAMES = ['red', 'green', 'blue']
  let bad = 0
  for (let c = 0; c < NCH; c++) {
    const vis = [0,0,0]; vis[c] = 1
    const [r, g, b] = brightest(await strip(vis))
    const dom = [r, g, b].indexOf(Math.max(r, g, b))
    const lit = Math.max(r, g, b) > 40
    const okc = lit && dom === c
    if (!okc) bad++
    say('channel ' + c + ' alone → rgb(' + r + ',' + g + ',' + b + ')  expected ' + NAMES[c] +
        (okc ? '  OK' : lit ? '  WRONG COLOUR — LUT row/column swapped?' : '  NOTHING DREW'),
        okc ? 'ok' : 'bad')
  }
  // all three together must add, not replace
  const [r, g, b] = brightest(await strip([1,1,1]))
  const additive = [r, g, b].filter(v => v > 40).length >= 2
  if (!additive) bad++
  say('all three → rgb(' + r + ',' + g + ',' + b + ')  ' +
      (additive ? 'additive OK' : 'NOT ADDITIVE — only one channel reached the accumulator'),
      additive ? 'ok' : 'bad')

  // ── the 2D path: one plane, orthographic, a single ray step ──────────────────────
  // A depth-1 texture per channel with a HALF-FILLED plane, so a black result and a "drew something"
  // result are distinguishable, and so is drawing the wrong half.
  {
    const plane = new Uint16Array(N * N)
    for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) plane[y * N + x] = x < N / 2 ? 4000 : 0
    const tex2 = device.createTexture({size: [N, N, 1 * NCH], dimension: '3d', format: 'r16uint',
      usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST})
    for (let c = 0; c < NCH; c++) {
      device.queue.writeTexture({texture: tex2, origin: [0, 0, c]},
        plane, {bytesPerRow: N * 2, rowsPerImage: N}, [N, N, 1])
    }
    const bind2 = device.createBindGroup({layout: bgl, entries: [
      {binding: 0, resource: {buffer: ubuf}}, {binding: 1, resource: tex2.createView()},
      {binding: 2, resource: lut.createView()}]})

    async function plane2d(ortho) {
      // depth 1: dims.z = 1, zpc = 1, extent z = 1 voxel — what setImage(meta, budget, 1) produces
      u[0] = 0; u[1] = 0; u[2] = N * 1.7; u[3] = 1            // face-on, ONE step
      u[4] = NCH; u[5] = cv.width; u[6] = cv.height; u[7] = ortho ? 1 : 0
      u[8] = N; u[9] = N; u[10] = 1
      u[12] = N; u[13] = N; u[14] = 1; u[15] = 1
      for (let c = 0; c < MAX_CHANNELS; c++) { const o = 16 + c*4; u[o] = 0; u[o+1] = 3000; u[o+2] = c === 0 ? 1 : 0 }
      device.queue.writeBuffer(ubuf, 0, u)
      const rb = device.createBuffer({size: BPR * ROWS,
        usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ})
      const enc = device.createCommandEncoder()
      const pass = enc.beginRenderPass({colorAttachments: [{view: rt.createView(),
        clearValue: {r:0,g:0,b:0,a:1}, loadOp: 'clear', storeOp: 'store'}]})
      pass.setPipeline(pipeOff); pass.setBindGroup(0, bind2); pass.draw(3); pass.end()
      // a strip across the middle, spanning the lit half and the dark half
      enc.copyTextureToBuffer({texture: rt, origin: [Math.floor(cv.width/2) - 32, Math.floor(cv.height/2), 0]},
                              {buffer: rb, bytesPerRow: BPR, rowsPerImage: ROWS}, [64, 1, 1])
      device.queue.submit([enc.finish()])
      await rb.mapAsync(GPUMapMode.READ)
      const px = new Uint8Array(rb.getMappedRange().slice(0))
      rb.unmap(); rb.destroy()
      return px
    }

    for (const ortho of [true, false]) {
      const px = await plane2d(ortho)
      const lit = brightest(px)
      const anyLit = Math.max(...lit) > 40
      const dark = px.filter((_, i) => i % 4 === 0).filter(v => v < 10).length
      const okc = anyLit && dark > 0            // both halves present in the strip
      if (!okc) bad++
      say('2D plane, ' + (ortho ? 'orthographic' : 'perspective') +
          ' → brightest rgb(' + lit.join(',') + '), ' + dark + '/64 dark  ' +
          (okc ? 'OK' : anyLit ? 'ALL LIT — the plane sampled uniformly?' : 'BLACK — the single step missed the plane'),
          okc ? 'ok' : 'bad')
    }
  }

  // and leave something on screen
  setUniforms(cv.width, cv.height, [1,1,1])
  device.queue.submit([drawTo(ctx.getCurrentTexture().createView(), pipeCanvas).finish()])
  say(bad === 0 ? '\\nALL CHECKS PASSED — the shader compiles and the LUT is indexed correctly'
                : '\\n' + bad + ' CHECK(S) FAILED', bad === 0 ? 'ok' : 'bad')
} catch (e) {
  say('FAILED: ' + (e && e.message ? e.message : String(e)), 'bad')
}
</script></body></html>`

const dir = join(homedir(), 'Downloads', 'TMP')
mkdirSync(dir, { recursive: true })
const dest = join(dir, 'shader_check.html')
writeFileSync(dest, page)
console.log('wrote ' + dest)
console.log('WGSL extracted: ' + wgsl.split('\n').length + ' lines, MAX_CHANNELS=' + MAX_CHANNELS +
            ', LUT_STOPS=' + LUT_STOPS)
