// Param advisories — the "before you run this, know THIS about your data" line under a task param.
//
// A param that is easy to set wrongly deserves an answer at the point of setting it, not a surprise
// after the run. The first of these was hand-rolled inside `ParamRenderer.vue` for
// `motionDimsSelection` (a `motionDims` ref + `motionWarn`/`motionMsg`/`motionFlag`/`motionTip`
// computeds + its own template block). The second one — the anisotropy grid's stored size — would
// have been a second copy of that shape, which is how a pattern becomes four bespoke variants. So
// the mechanism is generalised here and `ParamRenderer` renders ONE advisory block for all of them.
//
// Deliberately a plain `.ts` module, not more logic in the SFC: the frontend test scope is pure
// logic in `src/utils`/`src/tasks` modules (docs/DEV.md → Tests), so the judgement — thresholds,
// wording, severity — is unit-tested, and the component only fetches and renders.
//
// ONE kind: `advise(value, ctx) => Promise<ParamAdvisory | null>`. It began as two (a pure `compute`
// and an async `load`), but once both real advisors needed to fetch, `compute` had no user — and a
// purely local advisory is just an `advise` that never awaits. One shape, no dead branch in the
// renderer, nothing to choose between.
//
// Adding one: write the advisor, register it under the param key, done — no template edit. See
// docs/MODULES.md → *Param advisories*.

import type { Severity } from '../lib/severity'
import { isImageVersionField } from './paramValues'
import { DEFAULT_VALUE_NAME } from '../utils/imageDelete'
import { spanAnchorRate, secondsLabel, type RateImage } from '../utils/frameDuration'
import { getSystemEnvs } from '../utils/systemEnvs'

/** What the renderer shows: a one-line readout, a severity, and the full reasoning on hover. */
export interface ParamAdvisory {
  severity: Severity
  /** One short line, shown inline under the control. No trailing period — it is a fragment. */
  message: string
  /** The reasoning, on hover. May be a sentence or two; this is the one place prose is allowed. */
  tip: string
  /** Optional second signal — see `DataFlag`. */
  flag?: DataFlag
  /** Optional trailing action button beside the note. Descriptive, no closures — `ParamRenderer`
   *  dispatches by `kind`. Today only `install-env` is defined (opt-in pixi env from the app).
   *  The InlineNote docstring notes a host may add a trailing control without the tooltips firing
   *  on top of each other, which is exactly what this is. */
  action?: {
    kind: 'install-env'
    label: string
    /** The env name to install (matches `_OPT_IN_ENVS` in `api/src/system_api.jl`). */
    env: string
  }
}

/**
 * A **data-quality flag**: how trustworthy the input is, as opposed to how concerning the advice is.
 * Rendered as a second shape-distinct severity icon after the message, with its own tooltip.
 *
 * **The split is the point, and it is easy to get wrong.** The `message`/`severity` answer *"what
 * should I do?"*; the flag answers *"how much can I trust what this is based on?"*. They move
 * independently:
 *
 * | | message / severity | flag |
 * |---|---|---|
 * | motion dims | "2D recommended" — a mild, actionable note | z is *reversing*: the axis is jitter (`fail`) |
 * | grid spacing | "64×64 grid · 33 MB" — a fine, ordinary setting | image is uncalibrated, so µm are being read as px (`warn`) |
 *
 * Merging them loses information in a specific way: the advice's tone swallows the data's. That is
 * exactly what happened when the motion advisory was first generalised — "2D recommended" reads as
 * mild, and the severe fact underneath ("your z axis is noise") vanished from the UI.
 *
 * Rule of thumb: if re-running the task with a different setting would NOT change it, it is a flag,
 * not a severity.
 */
export interface DataFlag {
  severity: Severity
  /** One short phrase, shown on hover. No trailing period — it is a fragment. */
  tip: string
}

/** The image fields an advisor may read — a structural subset of `CciaImage` (stores/project.ts),
 *  kept narrow on purpose so an advisor cannot quietly grow a dependency on the whole payload.
 *
 *  NOTE there is no frame extent here. X/Y is NOT a per-image property: `filepath` is versioned, and
 *  drift correction expands the canvas while a crop shrinks it, so the extent belongs to a specific
 *  VERSION. Fetch it for the active version (`/api/images/geometry`) rather than reading a stored field. */
export interface AdvisorImage {
  uid?: string
  sizeT?: number | null
  /** Frame interval + its unit, as `ccid.json` recorded them — Bioformats' word, not the OME enum. */
  timeIncrement?: number | null
  timeIncrementUnit?: string | null
  sizeZ?: number | null
  physicalSizeX?: number | null
  /** the version the image is currently ON — what the viewer shows and what a picker preselects */
  activeValueName?: string
  /** every registered image version, valueName → filename */
  filepaths?: Record<string, string>
  /** valueName → segmentation label filenames (matches `CciaImage.labels`). Used by cross-image
   *  advisors to check whether a selected pop's VN is present on every image. */
  labels?: Record<string, string[]>
}

/** Frame geometry of ONE resolved image version — what the grid estimate actually needs. */
export interface FrameGeometry {
  /** full-resolution frame width/height, in pixels, of the version this was read from */
  sizeX: number
  sizeY: number
  sizeT: number
  /** µm per pixel; unchanged by drift correction (which moves the canvas, not the sampling) */
  umPerPx?: number | null
}

/** Everything an advisor may read: the current value + the task panel's context. */
export interface AdvisorContext {
  projectUid?: string
  images?: AdvisorImage[]
  values?: Record<string, unknown>
}

/** The param an advisor is running ON — a structural subset of `ParamDef` (tasks/types.ts).
 *
 *  Needed because an advisor registered under a TYPE serves every param of that type, and one type
 *  can mean different things: a `valueNameSelection` names image versions, label sets or spatial
 *  graphs depending on its `field`, and only the first has an "active version" to compare against.
 *  The two key-registered advisors ignore this argument, which is fine — they can only ever run on
 *  the one param they are registered for. */
export interface AdvisorParam {
  type?: string
  key?: string
  field?: string
  /** `optionsFrom` on a `select` — surfaced so a key-registered advisor (`model` is used by BOTH
   *  `segment.cellpose` and `segment.coastal`) can self-gate to the source that actually applies. */
  optionsFrom?: string
}

export interface ParamAdvisor {
  /**
   * Produce the advisory, or `null` when there is nothing useful to say (a missing input included —
   * silence beats a wrong number). Async so an advisor MAY fetch; one that doesn't simply returns.
   * Never throws: an advisory is not load-bearing.
   */
  advise: (value: unknown, ctx: AdvisorContext, param?: AdvisorParam) => Promise<ParamAdvisory | null>
  /** Context values whose change should re-run `advise`, beyond the param value itself. */
  reloadOn?: (ctx: AdvisorContext) => unknown[]
}

// ── anisotropy grid spacing ────────────────────────────────────────────────────────────────────
//
// Bytes per grid box per frame. MUST match `_aniso_grid_bytes` in app/src/tasks/segment/branching.jl
// — five `orientation_*` arrays at 10 float32 per box (2 coords + 2 eigenvalues + 4 eigenvector
// components + length + coherence). The duplication is deliberate and narrow: the estimate has to be
// live in the browser, and shipping a round-trip for a multiplication would be worse. Both sides are
// pinned to the same number for the same input by a test, so a change to one fails the other.
export const ANISO_BYTES_PER_BOX_PER_FRAME = 40

/** Warn past this. Matches `_ANISO_SIDECAR_WARN_BYTES` in branching.jl. */
export const ANISO_WARN_BYTES = 100_000_000

/** Below this many pixels a box stops summarising and just resamples noise (`_ANISO_MIN_BOX_PX`). */
export const ANISO_MIN_BOX_PX = 3

export interface AnisoGridEstimate {
  boxPx: number
  clamped: boolean
  nBoxes: number
  grid: [number, number]
  frames: number
  bytes: number
}

/**
 * Grid and stored size for a µm spacing on a given image. Mirrors the Julia handler's conversion
 * (µm → px via the image's own pixel size, clamped to a usable minimum) and its cost model, so the
 * number shown before the run is the number the run reports afterwards.
 *
 * Takes the geometry of the ACTIVE version, so it stays right when drift correction has expanded
 * the canvas or a crop has shrunk it. Reading a stored per-image SizeX/SizeY instead was wrong for
 * exactly that reason (Dominik, 2026-07-30) — it described the default import while the task runs on
 * whichever version is active.
 *
 * `null` when there is not enough to say anything, rather than a guess.
 */
export function anisoGridEstimate(
  boxUm: number,
  geom: FrameGeometry | undefined | null,
): AnisoGridEstimate | null {
  if (!geom) return null
  const { sizeX, sizeY } = geom
  if (!sizeX || !sizeY || sizeX <= 0 || sizeY <= 0) return null
  if (!(boxUm > 0)) return null
  // No pixel size → the Julia side treats µm as px (and raises `branching.uncalibrated`); match it
  // rather than refusing to estimate, so the readout still reflects what will actually happen.
  const umPerPx = geom.umPerPx && geom.umPerPx > 0 ? geom.umPerPx : 1
  const raw = boxUm / umPerPx
  const boxPx = Math.round(Math.max(raw, ANISO_MIN_BOX_PX))
  const ny = Math.max(1, Math.ceil(sizeY / boxPx))
  const nx = Math.max(1, Math.ceil(sizeX / boxPx))
  const frames = Math.max(1, geom.sizeT ?? 1)
  return {
    boxPx,
    clamped: raw < ANISO_MIN_BOX_PX,
    nBoxes: ny * nx,
    grid: [ny, nx],
    frames,
    bytes: ny * nx * ANISO_BYTES_PER_BOX_PER_FRAME * frames,
  }
}

/** "37 MB" / "940 kB" — a size a person reads, not bytes. */
export function formatBytes(bytes: number): string {
  if (bytes >= 1e9) return `${(bytes / 1e9).toFixed(1)} GB`
  if (bytes >= 1e6) return `${Math.round(bytes / 1e6)} MB`
  if (bytes >= 1e3) return `${Math.round(bytes / 1e3)} kB`
  return `${bytes} B`
}

export function anisoGridAdvisory(value: unknown, geom: FrameGeometry | null): ParamAdvisory | null {
  const est = anisoGridEstimate(Number(value), geom)
  if (!est) return null
  const { grid, nBoxes, frames, bytes, boxPx, clamped } = est
  const size = formatBytes(bytes)
  const msg = `${grid[1]}×${grid[0]} grid · ${size}`

  // Data-quality flag: is this image calibrated? Without a pixel size the µm setting is silently
  // read as PIXELS (the Julia handler does the same and raises `branching.uncalibrated`), so every
  // number here — and the run's own output — means something other than what the label says. That
  // is a property of the DATA: changing the spacing does not fix it.
  const flag: DataFlag = geom?.umPerPx && geom.umPerPx > 0
    ? { severity: 'ok', tip: `calibrated at ${geom.umPerPx.toFixed(3)} µm/px` }
    : { severity: 'warn', tip: 'no pixel size on this image — µm are being read as pixels' }

  if (clamped) {
    return {
      severity: 'warn',
      flag,
      message: `${msg} · clamped to ${boxPx} px`,
      tip: `This image's pixels are ${(geom?.umPerPx ?? 1).toFixed(3)} µm, so the `
        + `spacing you asked for is under ${ANISO_MIN_BOX_PX} px and was raised to that. Below ~3 px `
        + `a box holds too few pixels to pool and the grid resamples noise instead of summarising it.`,
    }
  }
  if (bytes >= ANISO_WARN_BYTES) {
    return {
      severity: 'warn',
      flag,
      message: `${msg} — large`,
      tip: `${nBoxes} boxes on each of ${frames} frame(s) stores ${size} in the branch sidecar. `
        + `Boxes scale as 1/spacing², so doubling the spacing cuts this to about a quarter. `
        + `A fine grid is a legitimate choice — this is a heads-up, not a limit.`,
    }
  }
  return {
    severity: 'ok',
    flag,
    message: msg,
    tip: `${nBoxes} boxes (${boxPx} px each) on ${frames} frame(s) → ${size} stored. `
      + `Boxes scale as 1/spacing², so halving the spacing quadruples this.`,
  }
}

// ── track-measure motion dimensionality (migrated from ParamRenderer) ──────────────────────────

export interface MotionDims {
  dims: number
  zUsed: boolean
  confidence: string
  reason: string
  metrics?: { nSteps?: number; autocorrX?: number; autocorrY?: number; autocorrZ?: number
              persist2D?: number; persist3D?: number }
}

/** Pure shaping of a fetched z-assessment into an advisory. Split out so it is testable without a fetch. */
export function motionDimsAdvisory(value: unknown, m: MotionDims | null): ParamAdvisory | null {
  if (!m) return null
  const chosen = (value as string) ?? 'auto'
  if (chosen !== 'auto') {
    return { severity: 'ok', message: `using ${chosen} (auto: ${m.dims}D)`,
             tip: `You have overridden the recommendation. The detector read this data as ${m.dims}D.` }
  }
  const mt = m.metrics ?? {}
  const f = (x?: number) => typeof x === 'number' ? x.toFixed(2) : '?'
  const n = mt.nSteps

  // too little data to judge — say so rather than implying a verdict
  if (typeof n === 'number' && n < 50) {
    return { severity: 'warn', message: `${m.dims}D — too few steps to judge`,
             tip: `Only ${n} track steps, which is too few to assess the z-axis reliably. Kept `
                + `${m.dims}D to be safe; review whether that is right for this acquisition.` }
  }
  const aZ = mt.autocorrZ
  const straight = (typeof mt.persist2D === 'number' && typeof mt.persist3D === 'number')
    ? ` Path straightness is ${f(mt.persist2D)} in-plane vs ${f(mt.persist3D)} once z is included.` : ''

  // The Z-AXIS QUALITY flag — a separate signal from the recommendation. ok = real 3D motion,
  // warn = borderline/uncertain, fail = z is clearly jitter (anti-persistent, i.e. reversing).
  const reversing = typeof aZ === 'number' && aZ <= 0
  const flagSev: Severity = m.dims === 3
    ? (m.confidence === 'high' ? 'ok' : 'warn')
    : (reversing ? 'fail' : 'warn')
  const flagTip = flagSev === 'ok' ? 'z carries real migration'
    : flagSev === 'fail' ? 'z is clearly jitter — 2D strongly advised'
    : 'borderline — only just decided'
  const flag: DataFlag = { severity: flagSev, tip: flagTip }

  if (m.dims === 3) {
    return { severity: m.confidence === 'high' ? 'ok' : 'warn', flag,
             message: `3D recommended${m.confidence === 'high' ? '' : ' — uncertain'}`,
             tip: `z carries real migration (autocorrelation ${f(aZ)}).${straight}` }
  }
  return {
    severity: 'warn',                     // the RECOMMENDATION is a mild note; severity of the DATA
    flag,                                 // is what `flag` carries — do not merge them
    message: `2D recommended${m.confidence === 'low' ? ' — uncertain' : ''}`,
    tip: reversing
      ? `z reverses direction step to step (autocorrelation ${f(aZ)}), i.e. jitter rather than `
        + `migration — 2D strongly advised.${straight}`
      : `z did not clear the migration cutoff (autocorrelation ${f(aZ)}).${straight}`,
  }
}

// ── image version: is this the version the image is actually ON? ───────────────────────────────
//
// A `valueNameSelection` over `filepaths` picks WHICH VERSION of the image the task reads. The form
// preselects the active one (`preferredValueName`), but every option is selectable and nothing said
// what picking another one meant.
//
// THE COST, measured: `WIaUjL/p6t4mC` was re-segmented on `default` (the 512x512 raw import) while
// the image is active on `afCorrected` (605x617 — drift correction expands the canvas). The run
// reported done, banked 92374 cells, and wrote a 512x512 label store that the viewer then laid over
// a 605x617 image, so every neutrophil sat displaced in XY. Nothing in the app said the run and the
// view were on different versions; it read as a segmentation bug for as long as it took to compare
// the two store shapes on disk.
//
// NOT a guard. Running an older version is a legitimate thing to do — re-segmenting the raw import
// to compare against a correction is the obvious case — which is exactly why this is one advisory
// line and not a block.
//
// Applies only where `field` names image versions (`isImageVersionField`). Label sets and spatial
// graphs use the same widget and have no "active", so there is nothing to compare them to.
export function imageVersionAdvisory(
  value: unknown, images: AdvisorImage[] | undefined,
): ParamAdvisory | null {
  const chosen = typeof value === 'string' ? value : ''
  if (!chosen || !images?.length) return null

  // Only images that HAVE this version can be judged against it. A name on NONE of them is an
  // upstream chain node's future output ("driftCorrected" — `ParamContext.extraValueNames`), which
  // does not exist yet and is not a mistake. Nothing to compare, so say nothing.
  const known = images.filter(i => chosen in (i.filepaths ?? {}))
  if (!known.length) return null

  // Nowhere to go wrong where there is only one version to pick. Keeps the line off every task in a
  // project that has never run a correction, which is most of them on first use.
  if (known.every(i => Object.keys(i.filepaths ?? {}).length < 2)) return null

  const activeOf = (i: AdvisorImage) => i.activeValueName || DEFAULT_VALUE_NAME
  const off = known.filter(i => activeOf(i) !== chosen)

  if (!off.length) {
    return {
      severity: 'ok',
      message: 'active version',
      tip: `"${chosen}" is the version these images are on, so the run reads the pixels you are looking at.`,
    }
  }

  // Name the version to switch TO when the selection agrees on one. With a mixed selection there is
  // no single name to offer, so report the spread rather than picking one image's answer for all.
  const actives = [...new Set(off.map(activeOf))]
  const partial = off.length < known.length ? ` on ${off.length} of ${known.length} images` : ''
  return {
    severity: 'warn',
    message: actives.length === 1 && !partial
      ? `not the active version ("${actives[0]}")`
      : `not the active version${partial}`,
    tip: actives.length === 1
      ? `These images are on "${actives[0]}", so this run reads pixels the viewer is not showing and `
        + `writes its output against a different canvas. Pick "${actives[0]}", or make "${chosen}" active first.`
      : `The selected images are on ${actives.length} different versions, so "${chosen}" is not what `
        + `the viewer shows for all of them. Check the version on each before running.`,
  }
}

// ── cellpose model backend (Mac-only opt-in v3 env) ───────────────────────────────────────────
//
// Cellpose 4 (Cellpose-SAM) runs slowly on Apple Silicon MPS; cellpose 3 CNNs (`cyto2`, `cyto3`)
// are ~10× faster there. On Mac the app offers a second pixi env (`cellpose-v3`) as a one-click
// install. This advisory fires ONLY on macOS and:
//   * v4 model + env not installed → warn, "cellpose 4 is slow on Apple Silicon", + Install button
//   * v4 model + env installed     → warn, "switch to cyto3 for ~10× speedup" (no button)
//   * v3 model + env not installed → fail, "cellpose-v3 env is not installed", + Install button
// Silent on Linux/Windows (where CUDA v4 is fast and the v3 env isn't shipped).
//
// See docs/todo/CELLPOSE_V3_OPTIN_PLAN.md. Kept in step with the Python side (`_CELLPOSE_V3_BUILTINS`
// in `python/cecelia/utils/cellpose_utils.py`) and the Julia side (`BUILTIN_CELLPOSE_MODELS` in
// `app/src/config.jl`). Add a v3 model → update all three.

const CELLPOSE_V3_MODELS: ReadonlySet<string> = new Set(['cyto2', 'cyto3'])
const CELLPOSE_V3_ENV_NAME = 'cellpose-v3'

/** Platforms where the advisor is allowed to fire. Ship-time: `['osx-arm64']` only. Temporarily
 *  broadened to all workspace platforms so the install/advisor flow can be tested off a Mac —
 *  narrow back in the same change that tightens `[feature.cellpose-v3]` in `pixi.toml` and
 *  `_OPT_IN_ENVS[..].supported_platforms` in `api/src/system_api.jl`. */
const CELLPOSE_V3_ADVISORY_PLATFORMS: ReadonlySet<string> = new Set(['osx-arm64', 'linux-64', 'win-64'])

/** Pure: takes the selected model, platform, and env installed state. Testable without a fetch. */
export function cellposeModelAdvisory(
  selectedModel: unknown,
  platform: string | undefined,
  envInstalled: boolean,
): ParamAdvisory | null {
  if (!platform || !CELLPOSE_V3_ADVISORY_PLATFORMS.has(platform)) return null
  const name = typeof selectedModel === 'string' ? selectedModel : ''
  if (!name) return null

  const isV3 = CELLPOSE_V3_MODELS.has(name)

  if (isV3 && !envInstalled) {
    return {
      severity: 'fail',
      message: 'cellpose-v3 env is not installed',
      tip: 'This model runs in a second pixi environment that ships cellpose 3. It is not '
         + 'installed yet — click Install to fetch it (~500 MB, one-time). The run will fail '
         + 'until the env is present.',
      action: { kind: 'install-env', env: CELLPOSE_V3_ENV_NAME, label: 'Install cellpose-v3 (~500 MB)' },
    }
  }

  if (!isV3 && !envInstalled) {
    return {
      severity: 'warn',
      message: 'cellpose 4 is slow on Apple Silicon',
      tip: 'Cellpose-SAM (v4) is a large transformer and MPS is much slower than CUDA on it. '
         + 'The v3 CNN models (cyto2, cyto3) run ~10× faster on Apple Silicon. Install the opt-in '
         + 'cellpose-v3 env to get the faster path.',
      action: { kind: 'install-env', env: CELLPOSE_V3_ENV_NAME, label: 'Install cellpose-v3 (~500 MB)' },
    }
  }

  if (!isV3 && envInstalled) {
    return {
      severity: 'warn',
      message: 'switch to cyto3 for ~10× speedup on Apple Silicon',
      tip: 'Cellpose-SAM (v4) is slow on MPS. You already have the cellpose-v3 env installed — '
         + 'picking cyto3 (or cyto2) in this dropdown will use it and run much faster.',
    }
  }

  return null   // v3 model + env installed = the happy path, no advisory
}

// ── registry ───────────────────────────────────────────────────────────────────────────────────
//
// Looked up by param TYPE first, then by param KEY. Types are global (`motionDimsSelection` is a
// dedicated widget, so it can only ever mean one thing), whereas keys are per-task and can repeat —
// the motion param's key is just `dims`, which another task could plausibly reuse. So: register a
// widget-type advisor under the type, and a one-off advisor for a specific param under its key,
// picking a key distinctive enough not to collide (`anisotropyBoxUm`, not `box`).

/**
 * Spatial smoothing at zero, when a temporal statistic is about to run over it.
 *
 * **Spatial before temporal is load-bearing, and measured.** A temporal statistic alone keeps 8.5% of
 * the reference channel's signal past background subtraction — worse than doing nothing (15.4%) —
 * because at single-digit photon counts a median over three mostly-zero samples is zero. The Gaussian
 * has to fill the counts first (`docs/todo/SMOOTHING_PLAN.md`). The task cannot repair this for the
 * user: a sigma silently raised from what they set is a worse surprise than the run being wrong, and
 * 0 is a legitimate setting for the two other statistics on well-exposed data.
 *
 * **The wording states the effect, not the downstream task.** That 8.5%/15.4% pair was measured as
 * signal surviving AF's background subtraction, and the first draft of this tip said so — but AF is
 * one consumer of a smoothed image, not the reason smoothing is on the form. A user who smooths for
 * segmentation, or to watch a movie, would be reading a number about a task they never run. The fact
 * survives; the pipeline it was measured through stays here, where it is provenance rather than copy.
 *
 * **`gated` fails rather than warns**, because there it is not merely weaker — on the data this task
 * exists for it is nothing at all. The gate's scale is `2*(k*sigma)^2` with sigma the MAD of the
 * temporal difference, and on sparse data that difference is a majority of exact zeros, so the MAD is
 * exactly 0, the scale clamps to 1e-12 and every weight collapses: measured on `zolIMa/fXgbTl` at
 * sigma 0, amplitude kept 1.00 and background noise kept 1.00 on all four channels — the output IS
 * the input. Well-exposed data at sigma 0 still has noise to measure, so this is a warning about a
 * regime rather than an arithmetic certainty; `smooth_run.py` makes the actual call, refusing the run
 * when the estimate really does come back at zero rather than spending minutes producing a copy.
 */
export function spatialSigmaAdvisory(value: unknown, stat: unknown): ParamAdvisory | null {
  const sigma = typeof value === 'number' ? value : Number(value)
  if (!Number.isFinite(sigma) || sigma > 0) return null
  return stat === 'gated'
    ? { severity: 'fail',
        message: 'gated needs spatial smoothing first',
        tip: 'The gate scales its weights by the noise it measures between frames. With no Gaussian '
           + 'to fill the counts first, photon-limited data measures zero noise, every weight '
           + 'collapses and each frame is returned as it was. The run stops rather than spending '
           + 'minutes a channel copying the input.' }
    : { severity: 'warn',
        message: 'temporal smoothing alone keeps less signal than none',
        tip: 'On photon-limited data a median over three mostly-zero frames is zero, so the temporal '
           + 'term removes signal rather than noise — measured worse than not smoothing at all. The '
           + 'Gaussian has to fill the counts before a statistic across frames means anything.' }
}

/**
 * What the picked lags mean in seconds, and whether every selected movie can be read at them.
 *
 * `opticalFlow.train` picks frame lags, and a lag is not a displacement until you know the interval —
 * the same chips are 5–40 s on one movie and 15–120 s on another. Under *read other rates as the same
 * durations* it is those SECONDS that define the model, so the number the run will actually train on
 * has to be visible before the run.
 *
 * Anchored on the COARSEST interval in the set, which is what `train_run.reference_interval` picks —
 * the only anchor every selected movie can carry. If this readout used a different one it would
 * promise spans the run does not train.
 *
 * Deliberately NOT a re-implementation of the resolver: whether a COARSER movie can be read at these
 * spans is the runner's call and it refuses per movie by name. What this can say without a second
 * spelling of that rule is when the set mixes rates at all, which is when the question arises.
 */
export function temporalSpanAdvisory(
  value: unknown, images: readonly RateImage[] | undefined, mode: unknown,
): ParamAdvisory | null {
  const lags = (Array.isArray(value) ? value : []).map(Number)
    .filter(n => Number.isFinite(n) && n > 0).sort((a, b) => a - b)
  if (!lags.length) return null
  const rate = spanAnchorRate(images)

  // No rate, nothing to say in seconds — but say THAT, because under `seconds` mode the run cannot
  // start at all, and a silent form would let it be discovered at the runner.
  if (!rate) {
    return mode === 'seconds'
      ? { severity: 'warn',
          message: 'no frame interval in seconds — spans cannot be resolved',
          tip: 'Reading other rates as the same durations needs at least one selected movie with a '
             + 'frame interval recorded in seconds. Switch to "Same lags", or fix the metadata.' }
      : null
  }

  const spans = lags.map(n => secondsLabel(n * rate.seconds))
  const anchor = rate.mixed ? `coarsest of ${rate.known}` : rate.uid || 'the set'
  const partial = rate.known < rate.total ? `, ${rate.total - rate.known} without one` : ''
  return {
    // `ok` — this is a readout, not a concern. The doubts ride on `flag`.
    severity: 'ok',
    message: `${spans.join(', ')} at ${secondsLabel(rate.seconds)}/frame (${anchor})${partial}`,
    tip: mode === 'seconds'
      ? 'These durations are what the model is fitted on. Every other movie is read at the same '
      + 'durations, i.e. at its own frame lags — one too coarse for them is skipped, by name, in the '
      + 'run log.'
      : 'Every movie is read at these LAGS, so a movie at another frame rate sees a different '
      + 'displacement. Switch to "Same durations" to read it at these times instead.',
    // Two different doubts, one slot, so the more serious wins. A CONVERTED rate is one the runner
    // will not use at all — it skips a movie whose interval is not in seconds rather than guessing —
    // so the form must not quietly promise it. Mixed rates are merely the case the mode exists for:
    // legitimate to pool, wrong to read as one timescale.
    flag: rate.converted
      ? { severity: 'fail',
          tip: 'this interval is not recorded in seconds — the run skips a movie it cannot read '
             + 'in seconds rather than converting it' }
      : rate.mixed && mode !== 'seconds'
        ? { severity: 'warn',
            tip: 'the selected movies were acquired at different rates, so one set of lags is not '
               + 'one timescale' }
        : undefined,
  }
}

/**
 * Fetch a form-time advisory from the Julia backend. Use this INSTEAD of writing a client-side
 * mirror of a rule the runner already enforces: the same helper the task's `_run_task` calls at Run
 * time also serves this endpoint, so the two cannot drift. The rule lives in Julia
 * (`app/src/tasks/param_validators.jl` + a `register_param_validator!` call in the task file); the
 * frontend just picks a name and renders the reply.
 *
 * See `POST /api/tasks/validate` (`api/src/task_validate_api.jl`).
 */
export function backendAdvisor(funName: string, paramKey: string): ParamAdvisor {
  return {
    // The set of images matters (min sizeT across the selection is what most validators look at) —
    // re-run when the picked images change even if the value hasn't. `sizeT` is on the payload the
    // form already has, so a change there tracks a re-import, not just a re-selection.
    reloadOn: ctx => [(ctx.images ?? []).map(i => `${i.uid}:${i.sizeT ?? ''}`).join(',')],
    advise: async (value, ctx) => {
      try {
        const res = await fetch('/api/tasks/validate', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            funName, paramKey, value,
            projectUid:    ctx.projectUid ?? '',
            imageUids:     (ctx.images ?? []).map(i => i.uid).filter(Boolean),
            siblingValues: ctx.values ?? {},
          }),
        })
        if (!res.ok) return null
        const body = await res.json()
        // The endpoint returns `null` (as JSON) when the validator had nothing useful to say. Fall
        // through to `null` on any other shape — an advisory is never load-bearing.
        if (!body || typeof body !== 'object') return null
        const b = body as Partial<ParamAdvisory>
        if (typeof b.severity !== 'string' || typeof b.message !== 'string' ||
            typeof b.tip !== 'string') return null
        return b as ParamAdvisory
      } catch {
        return null      // network failed / server down — silence beats an error banner
      }
    },
  }
}

/** Distinct VN prefixes from a pop list, first-seen order. Duplicates the `scopeValueNames` rule
 *  (`paramValues.ts`) so this file's advisor stays free of a task-value dependency. */
function _vnsFromPops(pops: readonly unknown[]): string[] {
  const seen = new Set<string>()
  const out: string[] = []
  for (const p of pops) {
    const s = String(p)
    if (!s || s.startsWith('/')) continue
    const idx = s.indexOf('/')
    if (idx <= 0) continue
    const vn = s.slice(0, idx)
    if (!seen.has(vn)) { seen.add(vn); out.push(vn) }
  }
  return out
}

/** Advisory for `popsToCluster` — see the registration comment for the invariant it defends. */
export const popsCompatAdvisor: ParamAdvisor = {
  // rerun on: images changing, their label sets shifting (a re-import can wipe a VN), or the pop pick
  reloadOn: ctx => [
    (ctx.images ?? []).map(i => `${i.uid}:${Object.keys(i.labels ?? {}).sort().join('/')}`).join(','),
    JSON.stringify(ctx.values?.popsToCluster ?? []),
  ],
  advise: async (value, ctx) => {
    const pops = Array.isArray(value) ? value as unknown[] : []
    if (!pops.length) return null
    const vns = _vnsFromPops(pops)
    if (!vns.length) return null
    const imgs = ctx.images ?? []
    // (a) VN presence per image — client-side, `img.labels` is on the payload already
    const gapsPerImage: string[] = []
    for (const img of imgs) {
      const has = img.labels ?? {}
      const missing = vns.filter(v => !(v in has))
      if (missing.length) gapsPerImage.push(`${img.uid ?? '?'} (${missing.join(', ')})`)
    }
    // (b) channel names per VN, one backend hop — only when multi-VN. Reuses the multi-VN branch of
    // /api/gating/channels; single-VN would carry nothing to compare.
    let chanMsg: string | null = null
    if (vns.length >= 2 && imgs[0]?.uid && ctx.projectUid) {
      try {
        const q = `projectUid=${ctx.projectUid}&imageUid=${imgs[0].uid}`
              + `&valueNames=${vns.map(encodeURIComponent).join(',')}&popType=track`
        const res = await fetch(`/api/gating/channels?${q}`)
        if (res.ok) {
          const d = await res.json() as { channelNamesPerVn?: Record<string, string[]> }
          const perVn = d.channelNamesPerVn ?? {}
          const ref = perVn[vns[0]] ?? []
          const mismatched = vns.slice(1).some(v => {
            const list = perVn[v] ?? []
            return list.length !== ref.length || list.some((c, i) => c !== ref[i])
          })
          if (mismatched) {
            chanMsg = vns.map(v => `${v}: [${(perVn[v] ?? []).join(', ')}]`).join(' vs ')
          }
        }
      } catch { /* silence — advisory never load-bearing */ }
    }
    if (!gapsPerImage.length && !chanMsg) return null
    const parts: string[] = []
    if (chanMsg) parts.push('channel names differ across VNs')
    if (gapsPerImage.length) parts.push(`${gapsPerImage.length} image${gapsPerImage.length > 1 ? 's' : ''} missing a VN`)
    const tipParts: string[] = []
    if (chanMsg) tipParts.push(`Channel names must match for intensity features to be comparable — ${chanMsg}.`)
    if (gapsPerImage.length) tipParts.push(
      `Missing: ${gapsPerImage.slice(0, 4).join('; ')}${gapsPerImage.length > 4 ? '; …' : ''}.`)
    return {
      severity: 'warn',
      message: parts.join(' — '),
      tip: tipParts.join(' '),
    }
  },
}

export const PARAM_ADVISORS: Record<string, ParamAdvisor> = {
  // Registered under the KEY, not `chipSelect`: every chipSelect in every task would match the type,
  // and this judgement is about what a temporal LAG means.
  temporalScales: {
    // the anchor is the selected images' coarsest interval, and the phrasing depends on the mode
    reloadOn: ctx => [(ctx.images ?? []).map(i => `${i.uid}:${i.timeIncrement ?? ''}`).join(','),
                      ctx.values?.temporalScaleMode],
    advise: async (value, ctx) =>
      temporalSpanAdvisory(value, ctx.images, ctx.values?.temporalScaleMode),
  },

  // ASYNC, not pure: the frame extent belongs to the ACTIVE image version, and only its store knows
  // it. `/api/images/geometry` reads it off that version (omit `valueName` ⇒ the ACTIVE one, which
  // is what the task will run against).
  anisotropyBoxUm: {
    reloadOn: ctx => [ctx.images?.[0]?.uid],
    advise: async (value, ctx) => {
      const img = ctx.images?.[0]
      if (!img?.uid || !ctx.projectUid) return null
      const geom = await frameGeometry(ctx.projectUid, img.uid, img.physicalSizeX)
      return anisoGridAdvisory(value, geom)
    },
  },

  // Every image-version picker in every task, which is the point: the mismatch is not a property of
  // one task, it is a property of the widget. Purely local — the image payload already carries both
  // the version list and the active name, so this never awaits.
  valueNameSelection: {
    // the active version changes under the form: a correction finishing mid-session re-points it
    reloadOn: ctx => [(ctx.images ?? []).map(i => `${i.uid}:${i.activeValueName ?? ''}`).join(',')],
    advise: async (value, ctx, param) =>
      isImageVersionField(param?.field) ? imageVersionAdvisory(value, ctx.images) : null,
  },

  // Smoothing's Gaussian. Registered under the KEY: `float` is the widget type and would match every
  // slider in every task, and the judgement here is about what the SMOOTHING pipeline does with it.
  // SUPPORT's temporal window. Backend-served (`/api/tasks/validate`) — the rule lives in
  // `app/src/tasks/opticalFlow/train_support_denoise.jl` next to the runner that enforces it, so
  // the pre-run line and the post-Run refusal cannot drift.
  inputFrames: backendAdvisor('opticalFlow.trainSupportDenoise', 'inputFrames'),

  spatialSigma: {
    // the verdict depends on the statistic beside it, so it has to re-run when that changes
    reloadOn: ctx => [ctx.values?.temporalStat],
    advise: async (value, ctx) => spatialSigmaAdvisory(value, ctx.values?.temporalStat),
  },

  // Cellpose model dropdown. `model` is the KEY on `segment.cellpose.json` (and also on
  // `segment.coastal.json`), so the advisor self-gates on `optionsFrom === 'cellposeModels'`. Fires
  // only on osx-arm64 — see `cellposeModelAdvisory` for the decision matrix.
  model: {
    // env-installed state changes when the install job completes (`stores/ws.ts` invalidates the
    // cache) — the watch on `val` in `ParamRenderer` doesn't cover it, so re-run whenever the picker
    // is touched OR the panel remounts. Cheap: `getSystemEnvs()` is cached and served from a small
    // JSON endpoint.
    advise: async (value, _ctx, param) => {
      if (param?.optionsFrom !== 'cellposeModels') return null
      const envs = await getSystemEnvs()
      if (!envs) return null
      const v3 = envs.envs['cellpose-v3']
      return cellposeModelAdvisory(value, envs.platform, !!v3?.installed)
    },
  },

  // Cluster-tracks / cluster-pops compatibility check. A joint clustering only makes sense when the
  // selected pops sit on VNs that (a) exist on every selected image, and (b) share the same channel
  // names — otherwise `mean_intensity_0` means different molecules on different rows. Not a blocker:
  // R permitted the union with NA→0, but Dominik prefers the flag so a genuine mistake doesn't get
  // silently zero-padded. Registered under the KEY (both `clustTracks.cluster` and
  // `clustPops.cluster` name their pop param `popsToCluster`).
  popsToCluster: popsCompatAdvisor,

  motionDimsSelection: {
    reloadOn: ctx => [ctx.images?.[0]?.uid, ctx.values?.valueName],
    advise: async (value, ctx) => {
      const img = ctx.images?.[0]
      if (!img || !ctx.projectUid) return null
      const vn = (ctx.values?.valueName as string) ?? 'default'
      try {
        const q = `projectUid=${ctx.projectUid}&imageUid=${img.uid}&valueName=${encodeURIComponent(vn)}`
        const res = await fetch(`/api/tracking/motion-dims?${q}`)
        if (!res.ok) return null
        return motionDimsAdvisory(value, await res.json() as MotionDims)
      } catch {
        return null      // an advisory is never load-bearing; silence beats an error banner
      }
    },
  },
}

/** Type wins over key — see the note on the registry. */
// Geometry is memoised per (project, image): the advisory re-runs on every slider tick, but the
// frame extent does not depend on the value — only the arithmetic does. One metadata read per image,
// not per keystroke. Kept here rather than made a contract concept: one advisor needs it, and a
// `fetch`-then-`compute` third kind would be machinery for a single case.
const _geomCache = new Map<string, Promise<FrameGeometry | null>>()

export function frameGeometry(
  projectUid: string, imageUid: string, umPerPx?: number | null,
): Promise<FrameGeometry | null> {
  const key = `${projectUid}|${imageUid}`
  const hit = _geomCache.get(key)
  if (hit) return hit.then(g => g && { ...g, umPerPx })
  const p = (async (): Promise<FrameGeometry | null> => {
    try {
      const res = await fetch(`/api/images/geometry?projectUid=${projectUid}&imageUid=${imageUid}`)
      if (!res.ok) return null
      const d = await res.json() as { sizeX?: number; sizeY?: number; sizeT?: number }
      if (!d.sizeX || !d.sizeY) return null
      return { sizeX: d.sizeX, sizeY: d.sizeY, sizeT: d.sizeT ?? 1 }
    } catch {
      return null      // an advisory is never load-bearing; silence beats an error banner
    }
  })()
  _geomCache.set(key, p)
  return p.then(g => g && { ...g, umPerPx })
}

/** Drop cached geometry — call after anything that can change a version's extent (crop, correction). */
export function clearFrameGeometryCache(): void { _geomCache.clear() }

export function paramAdvisor(param: AdvisorParam): ParamAdvisor | undefined {
  return (param.type ? PARAM_ADVISORS[param.type] : undefined)
    ?? (param.key ? PARAM_ADVISORS[param.key] : undefined)
}
