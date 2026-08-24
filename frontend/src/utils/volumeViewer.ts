// Pure logic behind the in-browser volume viewer (docs/todo/WEB_VIEWER_PLAN.md → P1). Everything here
// is data-in/data-out so it can be tested without a GPU: the SFC and `lib/webgpu/*` hold the parts
// that need a device, this holds the parts that can be wrong.
//
// The two things worth testing are the two silent failures. A slab whose shape does not match the
// metadata still uploads and still renders — it just renders the wrong thing (`slabShapeError`). And a
// channel colour derived from a name rather than from the server's LUT is how napari's SHG channel
// once came out WHITE (`lutTextureBytes` takes the server's stops and nothing else).

import { parseHex } from './colour'

/** Per-channel display state as the server resolved it — see `api_viewer_meta`. */
export interface ViewerChannel {
  name: string
  lo: number
  hi: number
  visible: boolean
  /** Black→colour (or white→colour) ramp stops, 0-1 RGB. napari owns its palette; we interpolate. */
  lut: number[][]
}

export interface ViewerMeta {
  nT: number
  nC: number
  nZ: number
  nX: number
  nY: number
  bytesPerVoxel: number
  slabBytes: number
  contrastSource: 'viewer' | 'sampled'
  /** µm per voxel, [x, y, z]. 1.0 for an axis the image was never calibrated on. */
  voxelUm: number[]
  calibrated: { xy: boolean; z: boolean }
  channels: ViewerChannel[]
}

/** Channels the shader can composite in one pass. Beyond this the viewer shows the first MAX_CHANNELS
 *  and says so — a silent truncation would read as "that channel is empty". */
export const MAX_CHANNELS = 8
/** Stops per channel in the LUT texture. Matches the bridge's `_LUT_MAX_STOPS`, which is what caps the
 *  stops the props file can carry in the first place. */
export const LUT_STOPS = 64

export interface SlabQuery {
  projectUid: string
  imageUid: string
  valueName?: string
  t: number
  c: number
  /**
   * One z plane instead of the whole stack. This is the difference between a timecourse you can watch
   * and one you wait on: on `Dml3RG` (37 z, 4 ch, 181 t) a whole timepoint is 326 MB and ~400 ms of
   * server read, one plane is 8.8 MB and ~13-22 ms — and the whole movie is 1.59 GB at one plane
   * against 59 GB at full depth, so it fits in VRAM and the second pass is all cache hits.
   */
  z?: number
  /** `zstd` costs ~60 ms of server CPU and saves ~97% of the wire on real data — worth it over a
   *  network, a loss on loopback. The client picks because only the client knows which it is. */
  enc?: 'identity' | 'zstd'
}

export function slabUrl(q: SlabQuery): string {
  const p = new URLSearchParams({
    projectUid: q.projectUid, imageUid: q.imageUid,
    t: String(q.t), c: String(q.c), enc: q.enc ?? 'identity',
  })
  if (q.valueName) p.set('valueName', q.valueName)
  // Only when asked for: an absent `z` means the whole stack, and `z=0` is a legitimate plane.
  if (q.z !== undefined) p.set('z', String(q.z))
  return '/api/viewer/slab?' + p.toString()
}

export function metaUrl(q: { projectUid: string; imageUid: string; valueName?: string }): string {
  const p = new URLSearchParams({ projectUid: q.projectUid, imageUid: q.imageUid })
  if (q.valueName) p.set('valueName', q.valueName)
  return '/api/viewer/meta?' + p.toString()
}

/** `X-Slab-Shape` (`nz,ny,nx`) → the three numbers, or null if the header is absent/unparseable. */
export function parseSlabShape(header: string | null): [number, number, number] | null {
  if (!header) return null
  const n = header.split(',').map(s => Number(s.trim()))
  if (n.length !== 3 || n.some(v => !Number.isFinite(v) || v <= 0)) return null
  return [n[0], n[1], n[2]]
}

/**
 * Why a response cannot be uploaded, or `null` when it can. This is the guard, not a formality: a slab
 * with the wrong axis order or a truncated body writes into the texture without complaint and renders
 * a plausible-looking image of something else. Reading `.zarray`'s `dimension_separator` wrong already
 * produced exactly that once — every chunk looked absent, the slab was all zeros, and the render was
 * black with no error anywhere.
 */
export function slabShapeError(
  header: string | null, byteLength: number, meta: ViewerMeta, zDepth = meta.nZ,
): string | null {
  const shape = parseSlabShape(header)
  if (!shape) return 'Slab response carried no X-Slab-Shape header'
  const [nz, ny, nx] = shape
  if (nx !== meta.nX || ny !== meta.nY || nz !== zDepth) {
    return `Slab is ${nz}x${ny}x${nx} (z,y,x) but ${zDepth}x${meta.nY}x${meta.nX} was asked for`
  }
  const want = nx * ny * nz * meta.bytesPerVoxel
  if (byteLength !== want) return `Slab is ${byteLength} bytes, expected ${want}`
  return null
}

/**
 * Physical extent of the loaded box in µm, `[x, y, z]`. Uncalibrated axes come back as voxel counts,
 * which renders isotropic — the same thing napari shows for an uncalibrated stack.
 *
 * `zDepth` is how many planes are actually loaded (1 in the 2D view), not how many the image has. It
 * must never reach zero: a zero-thickness box makes the ray's entry and exit distances coincide, the
 * fragment takes the early-out, and the frame comes back black.
 */
export function extentUm(meta: ViewerMeta, zDepth = meta.nZ): [number, number, number] {
  const [vx, vy, vz] = meta.voxelUm
  return [meta.nX * (vx || 1), meta.nY * (vy || 1), Math.max(zDepth, 1) * (vz || 1)]
}

/**
 * The LUT texture body: `MAX_CHANNELS` rows of `LUT_STOPS` RGBA8 pixels, row `c` being channel `c`'s
 * ramp resampled to a fixed width. Rows past `channels.length` are left black, so an out-of-range
 * channel index contributes nothing rather than picking up a neighbour's colour.
 *
 * Stops come from the server (`resolved_display_specs`), which is the ONE place a napari colormap name
 * becomes RGB. Nothing here guesses a colour from a name — that is the bug this shape exists to
 * prevent, and it is why a channel with an empty `lut` renders black instead of falling back to white
 * (white adds to all three accumulators and washes the whole composite out).
 */
export function lutTextureBytes(channels: ViewerChannel[]): Uint8Array {
  const out = new Uint8Array(LUT_STOPS * MAX_CHANNELS * 4)
  for (let c = 0; c < Math.min(channels.length, MAX_CHANNELS); c++) {
    const stops = channels[c].lut
    if (!stops || stops.length === 0) continue
    for (let i = 0; i < LUT_STOPS; i++) {
      const [r, g, b] = sampleLut(stops, i / (LUT_STOPS - 1))
      const o = (c * LUT_STOPS + i) * 4
      out[o] = clamp255(r * 255); out[o + 1] = clamp255(g * 255)
      out[o + 2] = clamp255(b * 255); out[o + 3] = 255
    }
  }
  return out
}

/** Linear interpolation across LUT stops at `n` ∈ [0,1]. The Julia twin is `_lut_at`. */
export function sampleLut(stops: number[][], n: number): [number, number, number] {
  const k = stops.length
  if (k === 0) return [0, 0, 0]
  if (k === 1) return [stops[0][0], stops[0][1], stops[0][2]]
  const p = Math.max(0, Math.min(1, n)) * (k - 1)
  const i = Math.min(Math.floor(p), k - 2)
  const f = p - i
  const a = stops[i], b = stops[i + 1]
  return [a[0] + f * (b[0] - a[0]), a[1] + f * (b[1] - a[1]), a[2] + f * (b[2] - a[2])]
}

const clamp255 = (v: number) => Math.max(0, Math.min(255, Math.round(v)))

/**
 * A channel's LUT stops for a napari colormap picked in the browser: black → that colormap's colour.
 *
 * Two stops is EXACT for the channel colormaps, not an approximation — `image_render.jl` verified that
 * every one of them is a linear ramp from black (max deviation 0.007), which is why a name and an end
 * colour are enough. It is NOT exact for the perceptual maps (viridis/turbo) or the white→colour `I *`
 * set, and `CHANNEL_COLORMAP_OPTIONS` deliberately offers neither.
 *
 * The server stays the authority on load (`resolved_display_specs` ships the real stops, however many);
 * this is a local override for looking at something a different way.
 */
export function lutFromHex(hex: string): number[][] {
  const rgb = parseHex(hex)
  return rgb ? [[0, 0, 0], rgb.map(v => v / 255)] : [[0, 0, 0]]
}

// ── Orbit camera ─────────────────────────────────────────────────────────────────
// yaw/pitch/dist rather than a matrix, because the raycast shader builds its own basis from them (it
// needs the ray origin, not a projection). Pitch is clamped just short of the poles: at exactly ±π/2
// the up vector is parallel to the view direction and `cross` returns zero, which blanks the frame.

export interface OrbitCamera { yaw: number; pitch: number; dist: number }

const PITCH_LIMIT = Math.PI / 2 - 0.01

/**
 * Half-height of the view at unit distance — the ONE framing constant, shared with the shader.
 *
 * It is interpolated into the WGSL rather than written twice, because the camera solves for `dist` from
 * it and the shader turns `dist` back into a half-height. Two copies drift, and the symptom is a
 * "Reset view" that does not fit.
 */
export const VIEW_HALF_ANGLE = 0.45

/**
 * Hard ceiling on total cache bytes. Invisible to the user, and it is what makes the viewer
 * uncrashable rather than merely apologetic.
 *
 * WebGPU exposes no free-VRAM figure, so the safe size cannot be computed — and an
 * `out-of-memory` error scope around each allocation turned out NOT to be enough on its own: asked for
 * 181 volume timepoints (59 GB) the driver LOST THE DEVICE rather than failing the texture, which the
 * scope cannot intercept. So the cache must never approach the limit in the first place.
 *
 * A count of timepoints cannot express that, which was the actual bug: one timepoint is 8.8 MB in the
 * 2D view and 326 MB in 3D, so "keep all 181" is 1.6 GB in one and 59 GB in the other. Bytes are the
 * only unit that means the same thing in both.
 *
 * 1.5 GB is chosen to be safe on any discrete card we might meet rather than tuned for this one: it
 * holds ~170 plane timepoints (nearly a whole movie) and ~4 volume ones, which is the right shape — the
 * plane view is what plays, and 3D at ~400 ms a frame was never going to stream anyway.
 */
export const SAFE_CACHE_BYTES = 1.5e9

/**
 * Camera that FILLS the frame with the image, looking straight down z.
 *
 * Face-on, not tilted (Dominik, 2026-08-24): what you want when a view opens or is reset is the image,
 * square to the screen, and rotating away from it is then a deliberate act. `yaw = pitch = 0` is safe —
 * the degenerate basis is at `pitch = ±π/2`, where the up vector is parallel to the view direction, and
 * `orbitDrag` clamps just short of it.
 *
 * `aspect` is the canvas's width/height, and it is required to fit rather than merely frame: the shader
 * derives its visible width from the height times the aspect, so a camera that ignores it cannot know
 * whether width or height is the limiting axis. Fitting off `max(extent) * 1.7` — which is what this
 * did first — left the image at ~64% of the viewport height and ~55% of its width.
 *
 * **Both views fill, 2D and 3D** (Dominik, 2026-08-24). An earlier version fitted 3D to the bounding
 * SPHERE so that rotating could never swing a corner out of frame; that traded a permanently zoomed-out
 * reset for a problem the wheel already solves. Do not reinstate it — a reset that does not fill is the
 * bug being fixed, and the x/y rect is exactly right at the face-on orientation a reset returns to.
 *
 * `perspective` is which projection the shader will use, and it is NOT cosmetic: under perspective the
 * distance is measured to the box CENTRE while what the user sees is bounded by the NEAR face, half a
 * depth closer and therefore magnified. Ignoring it fits the middle of the volume and lets the front of
 * it overflow — reported (Dominik, 2026-08-24) as 3D filling the width and clipping top and bottom,
 * which is the signature: a deep stack overflows the tighter axis first. Under orthographic there is no
 * such term, magnification being depth-independent, so the plane view is unaffected either way.
 */
export function fitCamera(
  extent: [number, number, number], aspect = 1, perspective = false,
): OrbitCamera {
  const [ex, ey, ez] = extent
  const halfH = Math.max(ey / 2, ex / 2 / Math.max(aspect, 1e-6))
  const toNearFace = perspective ? Math.max(ez, 0) / 2 : 0
  //                            2% of breathing room ↓
  return { yaw: 0, pitch: 0, dist: (halfH / VIEW_HALF_ANGLE) * 1.02 + toNearFace }
}

/** Drag in canvas px → a new camera. `width` normalises so the same drag turns the same amount
 *  whatever the canvas size. */
export function orbitDrag(cam: OrbitCamera, dx: number, dy: number, width: number): OrbitCamera {
  const k = (2 * Math.PI) / Math.max(width, 1)
  return {
    ...cam,
    yaw: cam.yaw + dx * k,
    pitch: Math.max(-PITCH_LIMIT, Math.min(PITCH_LIMIT, cam.pitch + dy * k)),
  }
}

/** Wheel → dolly. Multiplicative, so a notch feels the same at every distance; clamped to a band
 *  around the fit distance so the volume can never be lost off-screen. */
export function orbitZoom(cam: OrbitCamera, deltaY: number, fitDist: number): OrbitCamera {
  const d = cam.dist * Math.exp(deltaY * 0.001)
  return { ...cam, dist: Math.max(fitDist * 0.15, Math.min(fitDist * 6, d)) }
}

// ── Contrast from the data the client already holds ───────────────────────────────

/**
 * `[p01, p999, max]` of a `Uint16Array`, from a strided subsample of ~`budget` samples. Same
 * convention as `percentile_spec` in `image_render.jl`, so an "Auto" here and the server's cold-start
 * answer agree about what a sensible window is.
 *
 * Strided, not full: a 44 M-voxel slab is a ~100 ms sort in JS, which a contrast button cannot spend.
 * `max` is from the same subsample and is what the slider's range is drawn from — the exact maximum
 * would need the full pass this is avoiding, and a slider bound does not need it.
 *
 * WHY `rowLength` IS AN ARGUMENT. A slab is periodic with period `nX`, so a stride sharing a factor
 * with it samples a lattice of COLUMNS rather than the volume — at the worst case, one column, whose
 * distribution is nothing like the image's. It is not a hypothetical: `4e6 / 20e3` gives stride 200,
 * and any row length that is a multiple of 200 collapses onto every 200th x. So the stride is nudged
 * up until it is coprime with the row length, which makes the walk cross every column.
 */
export function contrastFromSlab(
  v: Uint16Array, rowLength = 1, budget = 200_000,
): { lo: number; hi: number; max: number } {
  const stride = sampleStride(v.length, rowLength, budget)
  const s: number[] = []
  for (let i = 0; i < v.length; i += stride) s.push(v[i])
  if (s.length === 0) return { lo: 0, hi: 1, max: 1 }
  s.sort((a, b) => a - b)
  const at = (f: number) => s[Math.min(s.length - 1, Math.max(0, Math.floor(f * s.length)))]
  const lo = at(0.01)
  return { lo, hi: Math.max(at(0.999), lo + 1), max: Math.max(s[s.length - 1], lo + 1) }
}

/**
 * Brightest voxel in the same strided subsample — no percentiles, so no sort, which is what makes it
 * cheap enough to run on EVERY timepoint instead of only the first.
 *
 * That distinction is the fix for a real complaint (Dominik, 2026-08-24): the contrast slider's ceiling
 * came from the first timepoint loaded, so on a movie whose later frames are brighter the window could
 * not be opened far enough to see them — "you might want to push it up a bit, but you can't because
 * it's clipped". The AUTO window still comes from one timepoint (a window that chases each frame's own
 * distribution makes playback flicker — decision 5); it is only the RANGE that follows the data.
 */
export function slabMax(v: Uint16Array, rowLength = 1, budget = 200_000): number {
  const stride = sampleStride(v.length, rowLength, budget)
  let mx = 0
  for (let i = 0; i < v.length; i += stride) if (v[i] > mx) mx = v[i]
  return mx
}

/** Headroom above the brightest voxel seen, so the window can always be opened PAST saturation — which
 *  is a legitimate thing to want (it dims the whole channel) and was impossible when the ceiling was
 *  exactly the maximum. */
export const CONTRAST_HEADROOM = 1.5

/**
 * Where a channel's contrast slider ends. The brightest voxel seen so far plus headroom, capped at what
 * the dtype can hold.
 *
 * Not simply the dtype maximum: a 16-bit slider on data that peaks at 545 puts the whole useful range
 * in the first 1% of its travel. Not simply the maximum either — see `CONTRAST_HEADROOM`. And it must
 * only ever GROW, which is the caller's job: a ceiling that shrinks re-scales the slider under a value
 * the user set.
 */
export function contrastCeiling(maxSeen: number, bytesPerVoxel = 2): number {
  const dtypeMax = bytesPerVoxel >= 2 ? 65535 : 255
  return Math.max(1, Math.min(dtypeMax, Math.round(maxSeen * CONTRAST_HEADROOM)))
}

/**
 * Step for a strided walk of ~`budget` samples, nudged until it is coprime with the row length.
 *
 * Shared by both samplers so they cannot disagree about which voxels they look at — a maximum taken on
 * a different lattice from the percentiles could come out BELOW `hi`, which would put the slider's
 * ceiling under its own handle.
 */
function sampleStride(length: number, rowLength: number, budget: number): number {
  let stride = Math.max(1, Math.floor(length / budget))
  const row = Math.max(1, Math.floor(rowLength))
  while (stride > 1 && gcd(stride, row) !== 1) stride++
  return stride
}

function gcd(a: number, b: number): number {
  while (b) { const t = a % b; a = b; b = t }
  return a
}
