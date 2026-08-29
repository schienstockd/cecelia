import { describe, it, expect } from 'vitest'
import {
  slabUrl, metaUrl, parseSlabShape, slabShapeError, extentUm, lutTextureBytes, sampleLut,
  fitCamera, orbitDrag, orbitZoom, contrastFromSlab, slabMax, slabView, contrastCeiling,
  slabZ, visibleExtentUm, pickTileLevel, pickVolumeLevel,
  MAX_CHANNELS, LUT_STOPS, VIEW_HALF_ANGLE, TILE_LOD_HYST_LOG2,
  type ViewerMeta,
} from './volumeViewer'

const meta = (over: Partial<ViewerMeta> = {}): ViewerMeta => ({
  nT: 10, nC: 2, nZ: 4, nX: 5, nY: 3, bytesPerVoxel: 2, slabBytes: 5 * 3 * 4 * 2,
  contrastSource: 'sampled', voxelUm: [0.5, 0.5, 2],
  calibrated: { xy: true, z: true, t: true }, spaceUnit: null, frameIntervalMin: 2,
  channels: [
    { name: 'a', lo: 0, hi: 10, visible: true, lut: [[0, 0, 0], [1, 0, 0]] },
    { name: 'b', lo: 1, hi: 20, visible: false, lut: [[0, 0, 0], [0, 1, 0]] },
  ],
  ...over,
})

describe('slab + meta urls', () => {
  it('omits valueName when the active version is wanted', () => {
    const u = slabUrl({ projectUid: 'P', imageUid: 'I', t: 3, c: 1 })
    expect(u).toContain('t=3'); expect(u).toContain('c=1')
    expect(u).not.toContain('valueName')
    // identity by default: compression is the client's call, and loopback does not want it
    expect(u).toContain('enc=identity')
  })
  it('carries an explicit version and encoding', () => {
    const u = slabUrl({ projectUid: 'P', imageUid: 'I', valueName: 'corrected', t: 0, c: 0, enc: 'zstd' })
    expect(u).toContain('valueName=corrected'); expect(u).toContain('enc=zstd')
  })
  it('escapes what it puts in the query', () => {
    expect(metaUrl({ projectUid: 'a b', imageUid: 'c&d' })).toBe(
      '/api/viewer/meta?projectUid=a+b&imageUid=c%26d')
  })
})

describe('slabShapeError — the guard against rendering the wrong thing', () => {
  const m = meta()
  const bytes = m.nX * m.nY * m.nZ * m.bytesPerVoxel
  it('accepts a slab that matches', () => {
    expect(slabShapeError('4,3,5', bytes, m)).toBeNull()
  })
  it('rejects a missing header rather than trusting the body', () => {
    expect(slabShapeError(null, bytes, m)).toMatch(/no X-Slab-Shape/)
  })
  it('rejects a TRANSPOSED slab — same byte count, wrong axes', () => {
    // 5x3x4 has exactly as many voxels as 4x3x5, so a byte-length check alone passes it. This is the
    // failure that renders a believable image of the wrong thing.
    expect(slabShapeError('5,3,4', bytes, m)).toMatch(/was asked for/)
  })
  it('checks against the depth ASKED FOR, not the image depth', () => {
    // In the 2D view one plane is requested, so a full-stack slab is the error — the guard has to know
    // which of the two was asked for, or the plane view rejects every correct response.
    const oneplane = m.nX * m.nY * 1 * m.bytesPerVoxel
    expect(slabShapeError('1,3,5', oneplane, m, 1)).toBeNull()
    expect(slabShapeError('4,3,5', bytes, m, 1)).toMatch(/1x3x5 was asked for/)
    expect(slabShapeError('1,3,5', oneplane, m)).toMatch(/4x3x5 was asked for/)
  })
  it('rejects a truncated body', () => {
    expect(slabShapeError('4,3,5', bytes - 2, m)).toMatch(/expected/)
  })
  it('rejects an unparseable or non-positive header', () => {
    expect(slabShapeError('4,3', bytes, m)).toMatch(/no X-Slab-Shape/)
    expect(slabShapeError('4,0,5', bytes, m)).toMatch(/no X-Slab-Shape/)
    expect(parseSlabShape('a,b,c')).toBeNull()
  })
})

describe('extentUm', () => {
  it('scales each axis by its own voxel size — z is the anisotropic one', () => {
    expect(extentUm(meta())).toEqual([2.5, 1.5, 8])
  })
  it('falls back to voxel counts for an uncalibrated image, so it renders isotropic', () => {
    expect(extentUm(meta({ voxelUm: [1, 1, 1] }))).toEqual([5, 3, 4])
    // a zero (or absent) size must not collapse the axis to nothing
    expect(extentUm(meta({ voxelUm: [0, 0, 0] }))).toEqual([5, 3, 4])
  })
  it('takes the depth LOADED, and never lets it reach zero', () => {
    // 2D loads one plane: the box is one voxel thick, not the whole stack.
    expect(extentUm(meta(), 1)).toEqual([2.5, 1.5, 2])
    // A zero-thickness box makes the ray's entry and exit coincide, the fragment early-outs, and the
    // frame comes back black — so the floor is one plane, not none.
    expect(extentUm(meta(), 0)).toEqual([2.5, 1.5, 2])
  })
})

describe('LUT texture', () => {
  it('lays out one row per channel and leaves the unused rows black', () => {
    const b = lutTextureBytes(meta().channels)
    expect(b.length).toBe(LUT_STOPS * MAX_CHANNELS * 4)
    // channel 0 is a black→red ramp: first stop black, last stop full red
    expect([b[0], b[1], b[2]]).toEqual([0, 0, 0])
    const last0 = (LUT_STOPS - 1) * 4
    expect([b[last0], b[last0 + 1], b[last0 + 2]]).toEqual([255, 0, 0])
    // channel 1 is black→green, on its own row
    const last1 = (LUT_STOPS + LUT_STOPS - 1) * 4
    expect([b[last1], b[last1 + 1], b[last1 + 2]]).toEqual([0, 255, 0])
    // row 2 was never given a channel — black, NOT white. White adds to all three accumulators and
    // washes the composite out, which is exactly how a missing colormap once ruined every render.
    const row2 = 2 * LUT_STOPS * 4
    expect([b[row2], b[row2 + 1], b[row2 + 2]]).toEqual([0, 0, 0])
  })
  it('leaves a channel with no stops black rather than guessing', () => {
    const b = lutTextureBytes([{ name: 'x', lo: 0, hi: 1, visible: true, lut: [] }])
    expect(Array.from(b.slice(0, 4))).toEqual([0, 0, 0, 0])
  })
  it('ignores channels past MAX_CHANNELS instead of overrunning the row budget', () => {
    const many = Array.from({ length: MAX_CHANNELS + 3 }, () =>
      ({ name: 'c', lo: 0, hi: 1, visible: true, lut: [[1, 1, 1], [1, 1, 1]] }))
    expect(lutTextureBytes(many).length).toBe(LUT_STOPS * MAX_CHANNELS * 4)
  })
  it('interpolates between stops, and a white→colour ramp keeps its zero end', () => {
    expect(sampleLut([[0, 0, 0], [1, 0, 0]], 0.5)).toEqual([0.5, 0, 0])
    // napari's `I *` set runs white→colour; no name table could express it, so the zero end matters
    expect(sampleLut([[1, 1, 1], [0, 0, 1]], 0)).toEqual([1, 1, 1])
    // a 3-stop ramp lands ON the middle stop at the midpoint
    expect(sampleLut([[0, 0, 0], [0, 1, 0], [0, 0, 1]], 0.5)).toEqual([0, 1, 0])
    // out-of-range is clamped, not extrapolated
    expect(sampleLut([[0, 0, 0], [1, 0, 0]], 5)).toEqual([1, 0, 0])
    expect(sampleLut([], 0.5)).toEqual([0, 0, 0])
  })
})

describe('orbit camera', () => {
  const fit = fitCamera([10, 4, 20])
  // What the shader shows at a given distance — the other half of the framing convention. Asserting
  // through this rather than against a number is the point: the constant lives in one place and the
  // tests describe the PROPERTY ("does the image fit"), so retuning it cannot silently unfit the view.
  const halfHeight = (c: { dist: number }) => c.dist * VIEW_HALF_ANGLE
  const halfWidth = (c: { dist: number }, aspect: number) => halfHeight(c) * aspect

  it('opens FACE-ON, so a reset squares the image to the screen', () => {
    // Not a tilt: a tilted default reads as a demo of a renderer, and on a 2D image it renders one
    // plane as a foreshortened sheet. Straight on, the MIP is a plain z-projection.
    expect(fit.yaw).toBe(0)
    expect(fit.pitch).toBe(0)
  })

  it('FILLS the frame — snugly, not zoomed out, in 2D and 3D alike', () => {
    // The bug: fitting off `max(extent) * 1.7` left the image at ~64% of the viewport height and ~55%
    // of its width, i.e. a reset that looked zoomed out.
    for (const aspect of [1, 16 / 9, 0.6]) {
      const c = fitCamera([10, 4, 1], aspect)
      expect(halfWidth(c, aspect)).toBeGreaterThanOrEqual(5)     // the image fits…
      expect(halfHeight(c)).toBeGreaterThanOrEqual(2)
      const slack = Math.min(halfWidth(c, aspect) / 5, halfHeight(c) / 2)
      expect(slack).toBeLessThan(1.1)                            // …and only just, on one axis
    }
  })

  it('picks whichever axis is limiting', () => {
    // A tall image in a wide window is height-limited; a wide image in a tall window is width-limited.
    const wideWindow = fitCamera([4, 10, 1], 2)
    expect(halfHeight(wideWindow)).toBeCloseTo(5 * 1.02)
    const tallWindow = fitCamera([10, 4, 1], 0.5)
    expect(halfWidth(tallWindow, 0.5)).toBeCloseTo(5 * 1.02)
  })

  it('ignores z depth under ORTHOGRAPHIC, where magnification is depth-independent', () => {
    // The 3D fit used the bounding SPHERE, so a deep stack opened zoomed out. Both views fill now.
    expect(fitCamera([10, 4, 200], 1).dist).toBeCloseTo(fitCamera([10, 4, 1], 1).dist)
  })

  it('fits the NEAR face under perspective, not the middle of the volume', () => {
    // Distance is to the box centre but what is on screen is bounded by the front of it, half a depth
    // closer and so magnified. Fitting the centre let the front overflow: 3D filled the width and
    // clipped top and bottom. Asserted through the near-face half-height for the same reason as above —
    // the property, not the number.
    for (const ez of [1, 40, 200]) {
      for (const aspect of [0.5, 1, 2.2]) {
        const c = fitCamera([10, 4, ez], aspect, true)
        const nearHalfH = (c.dist - ez / 2) * VIEW_HALF_ANGLE
        expect(nearHalfH * aspect).toBeGreaterThanOrEqual(5)   // fits…
        expect(nearHalfH).toBeGreaterThanOrEqual(2)
        expect(Math.min((nearHalfH * aspect) / 5, nearHalfH / 2)).toBeLessThan(1.1)   // …only just
      }
    }
  })

  it('backs off by exactly half the depth, so the two projections frame the front face alike', () => {
    // Same framing either way at the face the user is looking at — otherwise the 2D/3D toggle jumps.
    const ortho = fitCamera([10, 4, 60], 1.4)
    const persp = fitCamera([10, 4, 60], 1.4, true)
    expect(persp.dist - ortho.dist).toBeCloseTo(30)
  })

  it('never reaches the pole, where the up vector degenerates and the frame blanks', () => {
    let c = fit
    for (let i = 0; i < 100; i++) c = orbitDrag(c, 0, 1000, 800)
    expect(c.pitch).toBeLessThan(Math.PI / 2)
    let d = fit
    for (let i = 0; i < 100; i++) d = orbitDrag(d, 0, -1000, 800)
    expect(d.pitch).toBeGreaterThan(-Math.PI / 2)
  })
  it('turns the same amount for the same drag whatever the canvas width', () => {
    expect(orbitDrag(fit, 400, 0, 800).yaw).toBeCloseTo(orbitDrag(fit, 800, 0, 1600).yaw)
    // a full canvas width is one full turn
    expect(orbitDrag(fit, 800, 0, 800).yaw - fit.yaw).toBeCloseTo(2 * Math.PI)
  })
  it('keeps zoom inside a band, so the volume cannot be lost off-screen', () => {
    let c = fit
    for (let i = 0; i < 200; i++) c = orbitZoom(c, -1000, fit.dist)
    expect(c.dist).toBeCloseTo(fit.dist * 0.15)
    let f = fit
    for (let i = 0; i < 200; i++) f = orbitZoom(f, 1000, fit.dist)
    expect(f.dist).toBeCloseTo(fit.dist * 6)
  })
  it('zooms multiplicatively, so a notch feels the same at any distance', () => {
    const near = orbitZoom({ ...fit, dist: 10 }, -100, fit.dist).dist / 10
    const far = orbitZoom({ ...fit, dist: 20 }, -100, fit.dist).dist / 20
    expect(near).toBeCloseTo(far)
  })
  it('with a cursor anchor, keeps the world point under the pointer fixed (ImageJ)', () => {
    // World point at the cursor before the zoom must equal the world point at the same cursor
    // after — the pointer should feel glued to whatever it was over. Formula matches the SHADER
    // (`tileShader.ts`): ndcX = (wx - panX) / halfW, ndcY = -(wy + panY) / halfH, so
    //   wx = panX + ndcX*halfW,  wy = -panY - ndcY*halfH.
    // Panning both non-zero so the wrong sign would show — the first shipped version was
    // consistent with itself at panX = panY = 0 but mirrored the cursor across the origin
    // (dominik, 2026-08-29: "mouse bottom left, it zooms in top right").
    const cam0 = { ...fit, dist: 100, panX: 30, panY: -20 }
    const anchor = { ndcX: -0.7, ndcY: -0.6, aspect: 16 / 9 } // bottom-left of a wide canvas
    const worldAt = (c: typeof cam0) => {
      const halfH = c.dist * VIEW_HALF_ANGLE
      return { x:  c.panX + anchor.ndcX * halfH * anchor.aspect,
               y: -c.panY - anchor.ndcY * halfH }
    }
    const w0 = worldAt(cam0)
    const zin  = orbitZoom(cam0, -200, fit.dist, { min: 0.005, max: 6 }, anchor)
    const zout = orbitZoom(cam0,  200, fit.dist, { min: 0.005, max: 6 }, anchor)
    expect(worldAt(zin).x).toBeCloseTo(w0.x)
    expect(worldAt(zin).y).toBeCloseTo(w0.y)
    expect(worldAt(zout).x).toBeCloseTo(w0.x)
    expect(worldAt(zout).y).toBeCloseTo(w0.y)
  })
  it('zoom in at a bottom-left cursor drags the camera center TOWARD it, not the mirror', () => {
    // Regression pin for "mouse bottom-left, it zooms in top-right" (dominik, 2026-08-29). The
    // buggy version inverted panX AND panY. Screen center in world = (panX, -panY). A cursor at
    // screen bottom-left is NDC (-,-). Zoom in ⇒ the pointed-at world moves closer to center ⇒
    // camera center shifts toward the cursor. So panX shifts SAME SIGN as ndcX (both negative here)
    // and -panY shifts SAME SIGN as ndcY_screen = -ndcY_ndc; equivalently panY shifts SAME SIGN
    // as ndcY_ndc — bottom cursor (ndcY<0) ⇒ panY decreases.
    const cam0 = { ...fit, dist: 200, panX: 0, panY: 0 }
    const anchor = { ndcX: -0.8, ndcY: -0.8, aspect: 1 }
    const zin = orbitZoom(cam0, -200, fit.dist, { min: 0.005, max: 6 }, anchor)
    expect(zin.panX).toBeLessThan(0)
    expect(zin.panY).toBeLessThan(0)
  })
  it('without an anchor, zoom does not touch the pan (back-compat)', () => {
    const cam0 = { ...fit, dist: 100, panX: 7, panY: -3 }
    const z = orbitZoom(cam0, -200, fit.dist)
    expect(z.panX).toBe(7)
    expect(z.panY).toBe(-3)
  })
  it('at the band clamp, the anchor does not drift the pan', () => {
    // Otherwise repeated wheel notches at max zoom-out slide the image sideways.
    const cam0 = { ...fit, dist: fit.dist * 6, panX: 0, panY: 0 }
    const anchor = { ndcX: 0.8, ndcY: 0.8, aspect: 1 }
    const z = orbitZoom(cam0, 1000, fit.dist, { min: 0.15, max: 6 }, anchor)
    expect(z.dist).toBe(cam0.dist)
    expect(z.panX).toBe(0)
    expect(z.panY).toBe(0)
  })
})

describe('slabView — dtype-aware wrap around the slab ArrayBuffer', () => {
  // Fixes `RangeError: byte length of Uint16Array should be a multiple of 2` hit on 35uedD
  // (Human_Spleen_Manual_IBEX): bioformats2raw preserved the source's 8-bit dtype, so the store
  // is `|u1` and a `Uint16Array` view over an odd-byte-length response throws. `slabView` reads
  // the same `X-Slab-Bpv` / `meta.bytesPerVoxel` the server already sends and picks the right
  // typed-array constructor.
  it('picks Uint8Array for bpv=1 — a uint8 store\'s bytes are 1 per voxel', () => {
    const buf = new Uint8Array([0, 100, 255, 42]).buffer
    const v = slabView(buf, 1)
    expect(v).toBeInstanceOf(Uint8Array)
    expect(v.length).toBe(4)
    expect(v[0]).toBe(0); expect(v[1]).toBe(100); expect(v[2]).toBe(255); expect(v[3]).toBe(42)
  })
  it('picks Uint16Array for bpv=2 — the default for uint16 stores', () => {
    const src = Uint16Array.of(0, 100, 65535, 42)
    const v = slabView(src.buffer, 2)
    expect(v).toBeInstanceOf(Uint16Array)
    expect(v.length).toBe(4)
    expect(v[2]).toBe(65535)
  })
  it('accepts an odd byte length for bpv=1 — the actual bug repro', () => {
    // A 35uedD-shaped level tile with an odd row × col × 1 byte count that a Uint16 view rejects.
    const odd = new Uint8Array(12977).buffer     // 12977 is odd — the L0 nX on 35uedD
    expect(() => slabView(odd, 1)).not.toThrow()
    expect(() => new Uint16Array(odd)).toThrow()
    expect(slabView(odd, 1).length).toBe(12977)
  })
  it('bpv anything-not-1 falls back to Uint16 — pre-uint8 callers stay identical', () => {
    // Keeps the old code path byte-identical for anyone who hasn't opted in yet (bpv=2, or a stale
    // meta that didn't populate the field).
    const buf = Uint16Array.of(1, 2, 3, 4).buffer
    expect(slabView(buf, 2)).toBeInstanceOf(Uint16Array)
    expect(slabView(buf, 4)).toBeInstanceOf(Uint16Array)  // never seen in practice; kept for defence
    expect(slabView(buf, 0)).toBeInstanceOf(Uint16Array)
  })
  // contrastFromSlab / slabMax were widened to accept the union — the sampling logic doesn't care
  // about the underlying storage width, only about the numeric values.
  it('contrastFromSlab + slabMax work on a Uint8Array without a copy', () => {
    const v = new Uint8Array(4096)
    for (let i = 0; i < v.length; i++) v[i] = 10 + (i % 100)
    v[0] = 0; v[1] = 255                          // one dead pixel, one saturated
    const { lo, hi, max } = contrastFromSlab(v)
    expect(lo).toBeGreaterThanOrEqual(10)
    expect(hi).toBeLessThan(200)                  // the 255 does not set the ceiling
    expect(max).toBeLessThanOrEqual(255)
    // slabMax is the exact max of the strided subsample, so it can sit at 109 (10 + 99) or 255 —
    // both are legitimate given the stride. Just prove it stays within the dtype range.
    expect(slabMax(v)).toBeLessThanOrEqual(255)
    expect(slabMax(v)).toBeGreaterThanOrEqual(10)
  })
})

describe('contrastFromSlab', () => {
  it('windows on the bulk of the data, not on the outliers', () => {
    const v = new Uint16Array(10_000)
    for (let i = 0; i < v.length; i++) v[i] = 100 + (i % 50)
    v[0] = 0; v[1] = 60000                        // one dead pixel and one hot one
    const { lo, hi, max } = contrastFromSlab(v)
    expect(lo).toBeGreaterThanOrEqual(100)
    expect(hi).toBeLessThan(200)                  // the hot pixel does not set the ceiling
    expect(max).toBeGreaterThanOrEqual(hi)
  })
  it('never returns a zero-width window, which would divide by zero in the shader', () => {
    const flat = new Uint16Array(1000).fill(7)
    const { lo, hi } = contrastFromSlab(flat)
    expect(hi).toBeGreaterThan(lo)
  })
  it('subsamples a big slab rather than sorting all of it', () => {
    const big = new Uint16Array(4_000_000)
    for (let i = 0; i < big.length; i++) big[i] = i % 997
    const t0 = performance.now()
    const { hi } = contrastFromSlab(big, 1104, 20_000)
    expect(performance.now() - t0).toBeLessThan(200)
    expect(hi).toBeGreaterThan(900)
  })
  it('does not sample a lattice of COLUMNS when the stride divides the row length', () => {
    // A slab is periodic with period nX. Here only x < 4 carries signal, so a stride that is a
    // multiple of the row length samples background forever and the window comes back empty.
    const row = 200, rows = 20_000
    const v = new Uint16Array(row * rows)
    for (let r = 0; r < rows; r++) for (let x = 0; x < 4; x++) v[r * row + x] = 1000 + x
    // stride from the budget alone would be exactly 200 = the row length → column 0 only
    expect(Math.floor(v.length / 20_000)).toBe(200)
    const { hi } = contrastFromSlab(v, row, 20_000)
    expect(hi).toBeGreaterThan(1000)              // it found the signal past column 0
  })
  it('handles an empty slab without throwing', () => {
    expect(contrastFromSlab(new Uint16Array(0))).toEqual({ lo: 0, hi: 1, max: 1 })
  })
})

describe('the contrast slider follows the data, not the first frame', () => {
  // The bug: the ceiling came from timepoint 0, so on a movie that brightens you could not open the
  // window far enough to see the later frames — "you can't because it's clipped".
  it('samples the same voxels as the percentiles, so the ceiling cannot land under `hi`', () => {
    const v = new Uint16Array(4096)
    for (let i = 0; i < v.length; i++) v[i] = i % 700
    const stats = contrastFromSlab(v, 64)
    expect(slabMax(v, 64)).toBe(stats.max)
    expect(contrastCeiling(slabMax(v, 64))).toBeGreaterThan(stats.hi)
  })

  it('finds a maximum that sits between the sampled voxels of a coarse walk', () => {
    // A budget forces a stride, and the point of sharing `sampleStride` is that both samplers then
    // agree about which voxels exist at all.
    const v = new Uint16Array(1000)
    v[500] = 9000
    expect(slabMax(v, 7, 1000)).toBe(9000)          // stride 1 — every voxel
    expect(slabMax(new Uint16Array(1000), 7)).toBe(0)
  })

  it('leaves HEADROOM above the brightest voxel, so the window can be opened past saturation', () => {
    expect(contrastCeiling(100)).toBeGreaterThan(100)
    expect(contrastCeiling(545)).toBe(818)
  })

  it('never runs past what the dtype can hold, and never collapses to zero', () => {
    expect(contrastCeiling(60000)).toBe(65535)
    expect(contrastCeiling(200, 1)).toBe(255)
    expect(contrastCeiling(0)).toBe(1)              // an all-black first frame must still be draggable
  })
})

describe('the request is sized by the TEXTURE, not the view mode', () => {
  // The two came apart and the client fetched 326 MB volumes into textures shaped for 8.8 MB planes,
  // which the read-ahead then treated as cheap and queued dozens of.
  it('asks for one plane when the texture holds one plane of a stack', () => {
    expect(slabZ(1, 37, 13)).toEqual({ z: 13 })         // a scalar z drops the dim server-side
  })
  it('asks for the whole stack when the texture holds it', () => {
    expect(slabZ(37, 37, 13)).toEqual({})               // no z at all
  })
  it('asks for the whole stack on a genuinely 2D image, which has no plane to choose', () => {
    expect(slabZ(1, 1, 0)).toEqual({})
  })
  it('asks for a RANGE when the texture is shallower than the stack but deeper than a plane', () => {
    // The cropped 3D view. `zTo` is inclusive, and the count is what has to match the texture.
    expect(slabZ(8, 41, 0, 10)).toEqual({ z: 10, zTo: 17 })
    expect(slabZ(2, 41, 0, 0)).toEqual({ z: 0, zTo: 1 })
  })
  it('slides the range back rather than reading past the end of the stack', () => {
    // The slider's bound and the store's depth disagree for a moment after a version switch, and a
    // range that runs off the end would come back SHORT — a shape the texture is not holding.
    expect(slabZ(8, 41, 0, 40)).toEqual({ z: 33, zTo: 40 })
    expect(slabZ(8, 41, 0, -5)).toEqual({ z: 0, zTo: 7 })
  })
})

describe('spatial audit — slab URL carries level/x/y, guard is level-aware', () => {
  it('omits level=0, x/y — a timecourse caller produces byte-identical URLs to before the tile route', () => {
    const before = slabUrl({ projectUid: 'P', imageUid: 'I', t: 3, c: 1 })
    const withZero = slabUrl({ projectUid: 'P', imageUid: 'I', t: 3, c: 1, level: 0 })
    expect(withZero).toBe(before)
    expect(before).not.toContain('level')
    expect(before).not.toContain('x=')
  })
  it('carries an explicit level when non-zero', () => {
    const u = slabUrl({ projectUid: 'P', imageUid: 'I', t: 0, c: 0, level: 2 })
    expect(u).toContain('level=2')
  })
  it('carries an XY tile — same pairing as z/zTo', () => {
    const u = slabUrl({ projectUid: 'P', imageUid: 'I', t: 0, c: 0, x: 100, xTo: 199, y: 200, yTo: 299 })
    expect(u).toContain('x=100'); expect(u).toContain('xTo=199')
    expect(u).toContain('y=200'); expect(u).toContain('yTo=299')
    // xTo without x makes no sense — the server would treat lo as 0, but the URL wouldn't say so
    const partial = slabUrl({ projectUid: 'P', imageUid: 'I', t: 0, c: 0, xTo: 199 })
    expect(partial).not.toContain('xTo')
  })
  it('checks the slab against the LEVEL dims, not L0', () => {
    // L0 is 5x3x4; the client picked L1 = 2x1 XY. A slab that matches L1 must pass, and a slab that
    // matches L0 must FAIL — otherwise the coarse-level fetch renders L1 data into an L0 texture.
    const m = meta()
    const l1nx = 2, l1ny = 1
    const okBytes = l1nx * l1ny * m.nZ * m.bytesPerVoxel
    expect(slabShapeError(`4,${l1ny},${l1nx}`, okBytes, m, m.nZ, m.bytesPerVoxel, l1nx, l1ny)).toBeNull()
    expect(slabShapeError('4,3,5', 5 * 3 * 4 * 2, m, m.nZ, m.bytesPerVoxel, l1nx, l1ny))
      .toMatch(/was asked for/)
  })
})

describe('pickTileLevel — 2D pan/zoom LOD', () => {
  const withLevels = (levels: Array<{ nX: number; nY: number }>) => meta({
    levels: levels.map((lv, i) => ({ level: i, nX: lv.nX, nY: lv.nY, chunkX: 1024, chunkY: 1024 })),
  })
  it('stays on L0 when there is no pyramid or only one level', () => {
    expect(pickTileLevel(1, meta())).toBe(0)
    expect(pickTileLevel(8, meta())).toBe(0)
    expect(pickTileLevel(8, withLevels([{ nX: 100, nY: 100 }]))).toBe(0)
  })
  it('picks the coarsest level whose native pixel is still <= one device pixel', () => {
    const m = withLevels([{ nX: 800, nY: 800 }, { nX: 400, nY: 400 }, { nX: 200, nY: 200 }])
    expect(pickTileLevel(1, m)).toBe(0)         // 1:1 — full res
    expect(pickTileLevel(1.5, m)).toBe(0)       // slightly zoomed out — still L0
    expect(pickTileLevel(2, m)).toBe(1)         // 2× zoomed out → L1 (2× downsample)
    expect(pickTileLevel(3.9, m)).toBe(1)
    expect(pickTileLevel(4, m)).toBe(2)
    expect(pickTileLevel(16, m)).toBe(2)        // capped at deepest
  })
  it('stays on L0 for zoom < 1 — magnified past 1:1, nothing finer exists', () => {
    const m = withLevels([{ nX: 800, nY: 800 }, { nX: 400, nY: 400 }])
    expect(pickTileLevel(0.5, m)).toBe(0)
    expect(pickTileLevel(0, m)).toBe(0)
  })

  // Hysteresis path — adapted from Kiln (kiln-render/src/streaming/streaming-manager.ts:747-754,
  // MIT-licensed, https://github.com/mpanknin/kiln-render). Boundary at zoom = 2, band up to
  // 2 * 2^HYST ≈ 2.859; anywhere INSIDE the band the finer level wins, OUTSIDE the coarser does.
  describe('hysteresis around integer boundaries — adapted from Kiln SSE selector', () => {
    const m3 = () => withLevels([{ nX: 800, nY: 800 }, { nX: 400, nY: 400 }, { nX: 200, nY: 200 }])
    const boundary = Math.pow(2, 1 + TILE_LOD_HYST_LOG2)  // ~= 2.857
    it('previousLevel undefined / -1 falls back to the classic floor picker', () => {
      const m = m3()
      expect(pickTileLevel(2.5, m)).toBe(1)          // classic picker: floor(log2(2.5)) = 1
      expect(pickTileLevel(2.5, m, undefined)).toBe(1)
      expect(pickTileLevel(2.5, m, -1)).toBe(1)      // sentinel: no textures resident yet
    })
    it('going finer (zoom in) commits immediately — Kiln\'s certain-split branch', () => {
      const m = m3()
      // Sitting on L2, zoom drops below the L1 boundary → commit to L1 immediately, no band.
      expect(pickTileLevel(3.9, m, 2)).toBe(1)
      expect(pickTileLevel(2, m, 2)).toBe(1)         // exactly on the boundary is already finer
      expect(pickTileLevel(1.5, m, 2)).toBe(0)       // drops past two boundaries at once
    })
    it('going coarser is delayed until zoom clears the hysteresis band past the boundary', () => {
      const m = m3()
      // Sitting on L0, wobbling around the L0/L1 boundary at zoom = 2 must NOT flip to L1.
      expect(pickTileLevel(2.0, m, 0)).toBe(0)
      expect(pickTileLevel(2.5, m, 0)).toBe(0)       // still inside the band [2, 2^(1+HYST)]
      expect(pickTileLevel(boundary - 0.001, m, 0)).toBe(0)
      expect(pickTileLevel(boundary + 0.001, m, 0)).toBe(1)  // decisive zoom-out crosses the band
    })
    it('same level in and out is a no-op — no thrash on identical picks', () => {
      const m = m3()
      expect(pickTileLevel(2.5, m, 1)).toBe(1)
      expect(pickTileLevel(1, m, 0)).toBe(0)
    })
    it('bias is asymmetric toward finer — quality regressions cost more than bandwidth wobble', () => {
      // At zoom = 2.5 the raw picker would say L1. If we're already on L0 (finer), we STAY on L0
      // because it looks better; if we're already on L1 (coarser), we stay on L1 (no thrash).
      const m = m3()
      expect(pickTileLevel(2.5, m, 0)).toBe(0)       // stick with finer
      expect(pickTileLevel(2.5, m, 1)).toBe(1)       // no thrash
    })
    it('respects the clamp — a previousLevel beyond nLevels-1 is treated as the deepest', () => {
      const m = m3()   // 3 levels → max = 2
      expect(pickTileLevel(2.5, m, 99)).toBe(1)      // clamps prev to 2, baseline is 1 → finer wins
    })
  })
})

describe('pickVolumeLevel — 3D LOD (napari-parity: coarsest by default)', () => {
  const withLevels = (n: number) => meta({
    levels: Array.from({ length: n }, (_, i) => ({ level: i, nX: 1000 / (1 << i), nY: 1000 / (1 << i),
                                                    chunkX: 1024, chunkY: 1024 })),
  })
  it('defaults to the DEEPEST level — the answer to the maxBufferSize crash', () => {
    // The user hit "Buffer size (1278131712) exceeds max buffer size limit (268435456)" from a full-res
    // volume of `f8gzA2`. napari also renders 3D at the coarsest level; Imaris-style octree LOD was
    // wishlisted but never shipped. Deepest by default is the always-correct floor.
    expect(pickVolumeLevel(withLevels(6))).toBe(5)
    expect(pickVolumeLevel(withLevels(1))).toBe(0)
    expect(pickVolumeLevel(meta())).toBe(0)     // no levels array at all → L0
  })
  it('honours an override and clamps it', () => {
    const m = withLevels(4)
    expect(pickVolumeLevel(m, 0)).toBe(0)
    expect(pickVolumeLevel(m, 2)).toBe(2)
    expect(pickVolumeLevel(m, 99)).toBe(3)      // clamped to deepest
    expect(pickVolumeLevel(m, -5)).toBe(0)      // clamped to L0
  })
})

describe('the scale bar is drawn against what is on screen', () => {
  it('is the inverse of the fit, so a reset bar spans the image', () => {
    const extent: [number, number, number] = [400, 300, 10]
    const cam = fitCamera(extent, 4 / 3)
    const [vx, vy] = visibleExtentUm(cam.dist, 4 / 3)
    // 2% of breathing room, on whichever axis is limiting — here the height.
    expect(vy).toBeCloseTo(300 * 1.02)
    expect(vx).toBeCloseTo(300 * 1.02 * (4 / 3))
  })
  it('halves when the camera comes twice as close, so the bar tracks the zoom', () => {
    const [wide] = visibleExtentUm(1000, 1)
    const [close] = visibleExtentUm(500, 1)
    expect(close).toBeCloseTo(wide / 2)
  })
})
