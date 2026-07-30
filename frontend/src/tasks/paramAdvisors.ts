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

/** What the renderer shows: a one-line readout, a severity, and the full reasoning on hover. */
export interface ParamAdvisory {
  severity: Severity
  /** One short line, shown inline under the control. No trailing period — it is a fragment. */
  message: string
  /** The reasoning, on hover. May be a sentence or two; this is the one place prose is allowed. */
  tip: string
  /** Optional second signal — see `DataFlag`. */
  flag?: DataFlag
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
  sizeZ?: number | null
  physicalSizeX?: number | null
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

export interface ParamAdvisor {
  /**
   * Produce the advisory, or `null` when there is nothing useful to say (a missing input included —
   * silence beats a wrong number). Async so an advisor MAY fetch; one that doesn't simply returns.
   * Never throws: an advisory is not load-bearing.
   */
  advise: (value: unknown, ctx: AdvisorContext) => Promise<ParamAdvisory | null>
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

// ── registry ───────────────────────────────────────────────────────────────────────────────────
//
// Looked up by param TYPE first, then by param KEY. Types are global (`motionDimsSelection` is a
// dedicated widget, so it can only ever mean one thing), whereas keys are per-task and can repeat —
// the motion param's key is just `dims`, which another task could plausibly reuse. So: register a
// widget-type advisor under the type, and a one-off advisor for a specific param under its key,
// picking a key distinctive enough not to collide (`anisotropyBoxUm`, not `box`).

export const PARAM_ADVISORS: Record<string, ParamAdvisor> = {
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

export function paramAdvisor(param: { type?: string; key?: string }): ParamAdvisor | undefined {
  return (param.type ? PARAM_ADVISORS[param.type] : undefined)
    ?? (param.key ? PARAM_ADVISORS[param.key] : undefined)
}
