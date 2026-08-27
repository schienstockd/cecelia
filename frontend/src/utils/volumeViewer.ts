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

export interface ViewerLevel {
  /** 0-based pyramid level. 0 is the highest resolution — same as `open_level0`'s L0. */
  level: number
  nX: number
  nY: number
  chunkX: number
  chunkY: number
}

export interface ViewerMeta {
  nT: number
  nC: number
  nZ: number
  nX: number
  nY: number
  bytesPerVoxel: number
  slabBytes: number
  /**
   * Per-level shape + chunk shape, from the store's `multiscales` metadata. The 2D pan/zoom viewer
   * picks a level from these against its viewport zoom (`pickTileLevel`); the 3D volume renderer picks
   * one whose whole-slab BYTES fit under a VRAM cap (`pickVolumeLevel`) — the audit's answer to the
   * `maxBufferSize` error a full-res volume request hit. An empty list means the store is single-level
   * or its multiscales metadata is unreadable — the client falls back to L0 only, same shape as a
   * caller before the tile route.
   */
  levels?: ViewerLevel[]
  contrastSource: 'viewer' | 'sampled'
  /** µm per voxel, [x, y, z]. 1.0 for an axis the image was never calibrated on. */
  voxelUm: number[]
  /** Whether each axis carries a REAL measurement. `voxelUm`/`frameIntervalMin` default a missing
   *  axis to 1.0, which is indistinguishable from a genuine 1 — so an overlay that shows a scale must
   *  consult this rather than the number. */
  calibrated: { xy: boolean; z: boolean; t: boolean }
  /** Unit `voxelUm` is really in, from the image's OME metadata. Null → µm, the accessor's contract. */
  spaceUnit: string | null
  /** Minutes per frame, or null when the image has no real timecourse. */
  frameIntervalMin: number | null
  /**
   * Segmentations with a mask ON DISK, so the viewer can offer them (P4). Not simply the registered
   * label names: `labels` and `label_props` are independent registries — an imported track set has a
   * table and no mask, and a store can be registered before it is written — so the server checks the
   * directory and this list is the answer.
   */
  labelNames?: string[]
  /**
   * Every registered version of this image, and the one these numbers describe.
   *
   * Both come from the server because the viewer is a pop-out with no project open — it cannot look
   * up either. `valueName` is what `resolve_image_version` ACTUALLY resolved, so a picker shows what
   * it is on rather than guessing; absent `valueName` in the request resolves the ACTIVE version, the
   * one a task would run against, which is the only definition of "active" this app should have.
   */
  valueNames?: string[]
  valueName?: string
  /** The ACTIVE version, whatever was asked for — so a picker can say whether what is on screen is
   *  the version every task runs against. `valueName` echoes an explicit request and cannot. */
  activeValueName?: string
  /**
   * The store the browser viewer is looking at, and the image's meta dir on disk. Body-carried into
   * `/api/preview/run` so the task-preview API uses these as source of truth for "what's on screen"
   * rather than reaching sideways into napari (P7).
   */
  zarrPath?: string
  taskDir?: string
  channels: ViewerChannel[]
}

/** Channels the shader can composite in one pass. Beyond this the viewer shows the first MAX_CHANNELS
 *  and says so — a silent truncation would read as "that channel is empty". Sized for 25-ch CODEX-style
 *  stacks and multi-marker IF; the cost is one row in the LUT texture and one vec4 in the uniform block
 *  per slot. In 3D mode this also multiplies the vol texture's z-layer count (nZ * nT * MAX_CHANNELS),
 *  which can bump into `maxTextureDimension3D` (2048 on integrated) for deep z-stacks — but 2D and
 *  moderate 3D fit fine. */
export const MAX_CHANNELS = 32
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
  /**
   * Last plane of a RANGE starting at `z`, inclusive — a cropped 3D view. Every cost here is linear in
   * the plane count, so 8 planes of 37 is 70 MB rather than 326 MB and four times as many timepoints
   * fit the same VRAM budget. Structure usually lives in a few planes, so a full-depth MIP is mostly
   * paying for empty stack.
   */
  zTo?: number
  /** `zstd` costs ~60 ms of server CPU and saves ~97% of the wire on real data — worth it over a
   *  network, a loss on loopback. The client picks because only the client knows which it is. */
  enc?: 'identity' | 'zstd'
  /**
   * Serve the MASK for this segmentation instead of the image (P4). Same route, same reader, same
   * headers and the same `z`/`zTo` selection — a mask is another zarr of the same geometry, which is
   * what makes it one parameter rather than a second route. The dtype differs and `X-Slab-Bpv` says so.
   */
  labels?: string
  /**
   * Retarget a labels request to the SCRATCH preview store the task-preview worker just wrote (P7).
   * Rides the same reader and headers as `labels` alone; only the file on disk differs
   * (`<vn>__preview.ome.zarr` instead of `<vn>.ome.zarr`). No-op without `labels`.
   */
  preview?: boolean
  /**
   * Cache-buster for the preview slab (P7). Two runs on the same (vn, t, z, preview=1) produce a
   * byte-identical URL, so the browser's HTTP cache would serve the previous run's bytes even
   * though the scratch store on disk has been rewritten. Set to the current `previewLabels.updateId`
   * — a monotonic per-run counter — so every re-run misses cache. No-op without `preview`.
   */
  previewId?: number
  /**
   * XY tile bounds, 0-based inclusive `[lo, hi]` in the LEVEL's coordinate space (a level=1 tile at
   * x=100..199 is a different region on disk from level=0 at the same numbers). Absent means the whole
   * axis — same shape as `z`. Together with `level` this is the pan/zoom viewer's access pattern.
   */
  x?: number
  xTo?: number
  y?: number
  yTo?: number
  /**
   * 0-based pyramid level, defaults to 0. The client picks it — the server does not know the viewport
   * zoom. `pickTileLevel` for the 2D pan/zoom view, `pickVolumeLevel` for the 3D raycaster.
   */
  level?: number
}

export function slabUrl(q: SlabQuery): string {
  const p = new URLSearchParams({
    projectUid: q.projectUid, imageUid: q.imageUid,
    t: String(q.t), c: String(q.c), enc: q.enc ?? 'identity',
  })
  if (q.valueName) p.set('valueName', q.valueName)
  if (q.labels) p.set('labels', q.labels)
  if (q.labels && q.preview) p.set('preview', '1')
  // Cache-bust identical preview URLs across re-runs. Server-side is harmless (unknown query params
  // are ignored by `try_serve_slab`), so this stays a pure client-side concern.
  if (q.labels && q.preview && q.previewId !== undefined) p.set('_pv', String(q.previewId))
  // Only when asked for: an absent `z` means the whole stack, and `z=0` is a legitimate plane.
  if (q.z !== undefined) p.set('z', String(q.z))
  // `zTo` promotes `z` from one plane to a RANGE of planes, which is a different rank of answer (the
  // server keeps the z dim). Never sent without `z`.
  if (q.z !== undefined && q.zTo !== undefined) p.set('zTo', String(q.zTo))
  // XY tile bounds — omit `xTo`/`yTo` when absent, same shape as `z`/`zTo`. `level` is only sent when
  // non-zero, so the timecourse callers (which always want L0) produce byte-identical URLs.
  if (q.x !== undefined) p.set('x', String(q.x))
  if (q.x !== undefined && q.xTo !== undefined) p.set('xTo', String(q.xTo))
  if (q.y !== undefined) p.set('y', String(q.y))
  if (q.y !== undefined && q.yTo !== undefined) p.set('yTo', String(q.yTo))
  if (q.level !== undefined && q.level !== 0) p.set('level', String(q.level))
  return '/api/viewer/slab?' + p.toString()
}

export function metaUrl(q: { projectUid: string; imageUid: string; valueName?: string }): string {
  const p = new URLSearchParams({ projectUid: q.projectUid, imageUid: q.imageUid })
  if (q.valueName) p.set('valueName', q.valueName)
  return '/api/viewer/meta?' + p.toString()
}

/**
 * 2D pan/zoom LOD: the level whose native pixel is closest to (without going finer than) one device
 * pixel, given the viewport zoom. `zoom` is L0 pixels per DEVICE pixel — 1 means 1:1, 2 means one
 * device pixel shows two L0 pixels, 0.5 means magnified past 1:1.
 *
 * Formula: `level = clamp(floor(log2(zoom)), 0, nLevels-1)`. Level `n`'s native pixel is `2^n` L0
 * pixels wide, so this is the coarsest level whose pixel is still ≤ one device pixel — don't ship
 * pixels the screen can't show. At `zoom < 1` (magnified past 1:1) we stay on L0; nothing finer
 * exists and the renderer upscales.
 *
 * ASSUMES CLEAN 2× STEPS — true for every store `bioformats2raw` or `create_multiscales` writes today.
 * If a future writer ships a non-2× pyramid, this needs to consult `levels[n].nX` for the actual
 * per-level factor. The meta payload already carries the shapes, so the change is here rather than in
 * the server contract. (Spatial audit Phase 3, 2026-08-25.)
 */
export function pickTileLevel(zoom: number, meta: ViewerMeta): number {
  const n = meta.levels?.length ?? 1
  if (n <= 1 || !Number.isFinite(zoom) || zoom <= 1) return 0
  const raw = Math.floor(Math.log2(zoom))
  return Math.max(0, Math.min(n - 1, raw))
}

/**
 * 3D volume LOD: the pyramid level to load a whole (t, c) volume from.
 *
 * napari also renders 3D at the coarsest level; Imaris-style octree LOD was on the wishlist but never
 * shipped. Default to the DEEPEST level so a big-XY volume request can never exceed WebGPU's
 * `maxBufferSize` (256 MB on a Dawn adapter, and a full-res f8gzA2-shape volume is 687 MB per
 * channel — the error the audit's user hit).
 *
 * `override` (0-based) is the user's choice from the level dropdown; clamped to `[0, nLevels-1]`.
 * `undefined` picks the deepest.
 */
export function pickVolumeLevel(meta: ViewerMeta, override?: number): number {
  const n = meta.levels?.length ?? 1
  if (n <= 1) return 0
  if (override === undefined || !Number.isFinite(override)) return n - 1
  return Math.max(0, Math.min(n - 1, Math.floor(override)))
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
 *
 * `bytesPerVoxel` is overridable for exactly one caller: a MASK has the image's geometry but not its
 * dtype (UInt32 label ids against UInt16 intensities), so the shape half of the guard asks the same
 * question and the length half does not.
 */
export function slabShapeError(
  header: string | null, byteLength: number, meta: ViewerMeta, zDepth = meta.nZ,
  bytesPerVoxel = meta.bytesPerVoxel,
  expectNX = meta.nX, expectNY = meta.nY,
): string | null {
  const shape = parseSlabShape(header)
  if (!shape) return 'Slab response carried no X-Slab-Shape header'
  const [nz, ny, nx] = shape
  if (nx !== expectNX || ny !== expectNY || nz !== zDepth) {
    return `Slab is ${nz}x${ny}x${nx} (z,y,x) but ${zDepth}x${expectNY}x${expectNX} was asked for`
  }
  const want = nx * ny * nz * bytesPerVoxel
  if (byteLength !== want) return `Slab is ${byteLength} bytes, expected ${want}`
  return null
}

/**
 * Which planes to ask the slab route for, from the depth the TEXTURE actually has.
 *
 * Three answers, and the DEPTH picks between them — derived from the renderer rather than from the view
 * mode on purpose. Those were two copies of one fact, and when they disagreed the client fetched 326 MB
 * volumes into textures shaped for 8.8 MB planes (see `setImage`). The renderer is the one that knows.
 *
 *  - `{}` — the whole stack. The texture is as deep as the image, so there is nothing to select; also
 *    the answer for a genuinely 2D image, which has one plane and no plane to choose.
 *  - `{ z }` — ONE plane, which DROPS the z dim server-side (the 2D view).
 *  - `{ z, zTo }` — a range, which keeps it (a cropped 3D view).
 *
 * `lo` is the only thing here the renderer cannot supply: it knows how deep it is, not where the slab
 * starts. Getting it wrong shows the wrong planes at the right size, so the shape guard cannot catch
 * it — which is why it comes straight from the slider and nothing else derives it.
 */
export function slabZ(
  textureDepth: number, nZ: number, zPlane: number, lo = 0,
): { z?: number; zTo?: number } {
  if (textureDepth >= nZ) return {}
  if (textureDepth === 1) return nZ > 1 ? { z: zPlane } : {}
  const start = Math.max(0, Math.min(lo, nZ - textureDepth))
  return { z: start, zTo: start + textureDepth - 1 }
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

export interface OrbitCamera {
  yaw: number
  pitch: number
  dist: number
  /**
   * Where the camera is pointed, in µm across the SCREEN's own axes — right and up, not world x and y.
   *
   * Screen axes rather than world ones because that is what a drag means: the image must follow the
   * pointer at any orientation, and at yaw 90° world x runs into the screen. The shader adds this to
   * the ray origin, so the overlays pan with the pixels for free — they invert the same basis.
   */
  panX: number
  panY: number
}

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
  return { yaw: 0, pitch: 0, dist: (halfH / VIEW_HALF_ANGLE) * 1.02 + toNearFace, panX: 0, panY: 0 }
}

/**
 * Physical extent the camera can currently SEE, `[x, y]` in the same unit as `extentUm`.
 *
 * The inverse of `fitCamera`, and it exists for the scale bar: the bar has to shrink as you zoom in, so
 * it is drawn against what is on screen rather than against the image. Exact under the orthographic
 * projection the plane view uses — `VIEW_HALF_ANGLE` is the half-height at unit distance, so the
 * visible height is `2 · dist · VIEW_HALF_ANGLE` at every depth. Under perspective the same expression
 * is exact at the depth of the box CENTRE, which is the plane the camera distance is measured to.
 *
 * ORIENTATION DOES NOT MATTER, which is worth stating because it looks as though it should. The
 * raycast marches a box whose sides are the physical extent in µm on all three axes, so anisotropic
 * voxels are already baked into the geometry and the rendered space is metrically uniform: a screen
 * distance converts to µm identically along any direction, at any yaw or pitch. (An earlier version of
 * this hid the bar unless the camera was face-on, reasoning that a rotated horizontal axis would mix x
 * and z — true of the VOXEL grid, not of the space actually rendered.)
 */
export function visibleExtentUm(dist: number, aspect: number): [number, number] {
  const h = 2 * Math.max(dist, 0) * VIEW_HALF_ANGLE
  return [h * Math.max(aspect, 1e-6), h]
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

/**
 * Drag in canvas px → a pan, in µm across the screen's axes.
 *
 * `height` is the canvas height, and the conversion is exact rather than a feel constant: the visible
 * height is `2 · dist · VIEW_HALF_ANGLE` µm (see `visibleExtentUm`), so one pixel of drag is that over
 * the canvas height. The image therefore tracks the pointer at every zoom — a fixed µm-per-pixel would
 * crawl when zoomed in and fly when zoomed out, which is the tell of a pan that was tuned rather than
 * derived.
 *
 * The signs are the ones that make content FOLLOW the pointer, which is the whole point of a drag: the
 * camera moves the other way. Drag right and the eye moves left, so the image comes with you.
 */
export function panDrag(cam: OrbitCamera, dx: number, dy: number, height: number): OrbitCamera {
  const umPerPx = (2 * Math.max(cam.dist, 0) * VIEW_HALF_ANGLE) / Math.max(height, 1)
  return { ...cam, panX: cam.panX - dx * umPerPx, panY: cam.panY + dy * umPerPx }
}

/** Wheel → dolly. Multiplicative, so a notch feels the same at every distance; clamped to a band
 *  around the fit distance so the volume can never be lost off-screen.
 *
 *  `band` scales the fit distance. The default `{min: 0.15, max: 6}` is tuned for the 3D volume view,
 *  where a wider band lets the user rotate a rotated volume out of frame. The 2D plane view of a
 *  whole slide is a bounded rectangle — it cannot be lost — so it passes a much smaller `min` so the
 *  user can zoom to actual pixels (`camZoom ≤ 1`) and `pickTileLevel` reaches L0. At 0.15 on a
 *  20k×17k slide, max zoom-in is `camZoom ≈ 2` — L1 is the finest level `pickTileLevel` ever picks
 *  (Dominik, 2026-08-26). */
export function orbitZoom(
  cam: OrbitCamera, deltaY: number, fitDist: number,
  band: { min: number; max: number } = { min: 0.15, max: 6 },
): OrbitCamera {
  const d = cam.dist * Math.exp(deltaY * 0.001)
  return { ...cam, dist: Math.max(fitDist * band.min, Math.min(fitDist * band.max, d)) }
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
