// Reading a movie's saved generation config BACK into the page that authors it — Phase 6 of
// docs/todo/MOVIE_MANAGEMENT_PLAN.md. The write side is `register_movie!` (api/src/movies_api.jl); this
// is the read side, kept pure so the shape-juggling below is testable without a napari or a project.
//
// The one rule from Decision 6: **read tolerantly, report what could not be restored, never reject.**
// Every field goes through a default, an entry from before a field existed simply has none, and a
// config naming something that has since been deleted still opens — with the dead names listed. A
// version number that refused to load an entry would only ever punish the person who recorded it.
//
// Three producers bank two SHAPES, and this file is where that stops being anyone else's problem:
//
//   viewer    → flat: the record request itself, with a `look` (a partial BatchMovieCfg seeded from the
//               live view) alongside the mask/version/3D fields the request carried at the top level
//   batch     → nested: `{config: <the authored BatchMovieCfg>, fileAttrs, fps, sizeX, sizeY, suffix}`
//   animation → flat, plus `keyframes` — which are the RENDER payload (`{viewState, steps}`), not the
//               editor's own model, so the page's thumbnails and durations ride along in `keyframeMeta`
//
// The first two are one kind (`look`) and land on the Batch page; the third lands on Animation. That
// is Decision 7, and it is why there are two destinations for three producers.

import { TITLE_CARD_DEFAULT, type BatchMovieCfg, type TitleCardCfg } from './batchMovie'
import { COMPARE_LAYOUTS, type CompareLayout } from './movieCompare'

/** A registry entry as `GET /api/movies/meta` returns it (api/src/movies_api.jl). */
export interface MovieRegistryEntry {
  producedBy?: string
  configKind?: string
  configVersion?: number
  recordedAt?: number
  config?: Record<string, unknown>
}

/** The two config kinds (Decision 7), and the page that owns each. */
export type RestoreKind = 'look' | 'keyframes'
export const RESTORE_ROUTE: Record<RestoreKind, string> = {
  look: '/batch-movies',
  keyframes: '/animation',
}

/** The output fields every producer shares — they live in the per-set `movie` bag, not the batch one. */
export interface MovieOutputCfg {
  fps?: number
  sizeX?: number | null
  sizeY?: number | null
  suffix?: string | null
  titleCard?: TitleCardCfg
  showTimestamp?: boolean
  showScaleBar?: boolean
}

const isObj = (v: unknown): v is Record<string, unknown> =>
  typeof v === 'object' && v !== null && !Array.isArray(v)
const strList = (v: unknown): string[] | undefined =>
  Array.isArray(v) ? v.filter((x): x is string => typeof x === 'string') : undefined
const num = (v: unknown): number | undefined => (typeof v === 'number' && isFinite(v) ? v : undefined)

/** Only assign keys whose value survived parsing — an absent field must stay absent, so the reader's
 *  own default applies rather than an `undefined` that overwrites it. */
function put<T extends object>(out: T, key: keyof T, v: unknown) {
  if (v !== undefined) (out as Record<string, unknown>)[key as string] = v
}

/**
 * Which page can edit this movie, or `null` if there is nothing banked to edit.
 *
 * Keyed off the STORED kind rather than `producedBy`: the producer says who made it, the kind says what
 * shape it is, and only the second determines where it can be opened. A movie recorded before the
 * registry existed has neither and returns null — that is the ordinary case for an old project, not an
 * error worth surfacing.
 */
export function restoreKind(entry: MovieRegistryEntry | null | undefined): RestoreKind | null {
  if (!entry || !isObj(entry.config)) return null
  const k = entry.configKind
  return k === 'look' || k === 'keyframes' ? k : null
}

/** Title card, read through its defaults and clamped the way `buildBatchMovieConfig` clamps it. */
function titleCardOf(v: unknown): TitleCardCfg | undefined {
  if (!isObj(v)) return undefined
  return {
    enabled: v.enabled === undefined ? TITLE_CARD_DEFAULT.enabled : v.enabled === true,
    note: typeof v.note === 'string' ? v.note : '',
    durationSec: Math.min(10, Math.max(1, num(v.durationSec) ?? TITLE_CARD_DEFAULT.durationSec)),
  }
}

/** The output half — identical in both shapes, since both were assembled from the same record request. */
function outputOf(c: Record<string, unknown>): MovieOutputCfg {
  const out: MovieOutputCfg = {}
  put(out, 'fps', num(c.fps))
  put(out, 'sizeX', c.sizeX === null ? null : num(c.sizeX))
  put(out, 'sizeY', c.sizeY === null ? null : num(c.sizeY))
  put(out, 'suffix', typeof c.suffix === 'string' ? c.suffix : undefined)
  put(out, 'titleCard', titleCardOf(c.titleCard))
  put(out, 'showTimestamp', typeof c.showTimestamp === 'boolean' ? c.showTimestamp : undefined)
  put(out, 'showScaleBar', typeof c.showScaleBar === 'boolean' ? c.showScaleBar : undefined)
  return out
}

export interface LookRestore {
  /** What the Batch page authors — its whole persisted bag, ready to replace. */
  cfg: BatchMovieCfg
  /** The shared per-set output fields (fps / size / suffix / title card / baked overlays). */
  output: MovieOutputCfg
  /** Which images it was recorded for. Empty for anything recorded before this was banked. */
  imageUids: string[]
  /** What the destination cannot express, in words — reported, never silently dropped. */
  dropped: string[]
}

/**
 * A `look` config → the Batch page's two bags.
 *
 * The nested (batch-authored) shape is taken as-is: it IS a `BatchMovieCfg`, authored on the page it is
 * going back to. The flat (viewer) shape is assembled — `look` holds the channels and overlays read off
 * the live view, and the masks/versions/3D fields sat at the top level of the record request, because
 * the recorder consumed them directly rather than through a config object.
 *
 * `branchValueNames` is the one field with nowhere to land: the viewer can record skeletons, the batch
 * page has no control for them, so it is REPORTED rather than quietly lost. That is the honest version
 * of "edit this on the page that owns the kind" — the kinds match, the surfaces are not identical.
 */
export function lookRestore(config: Record<string, unknown> | null | undefined): LookRestore | null {
  if (!isObj(config)) return null
  const output = outputOf(config)
  const dropped: string[] = []

  // Nested = the batch authored it. `config.config` is the bag the page already speaks.
  if (isObj(config.config)) {
    const cfg = { ...(config.config as BatchMovieCfg) }
    // `fileAttrs` is authored on the page and sent BESIDE the recorder's config, so it is banked one
    // level up — it belongs back in the same bag the page reads everything else from.
    put(cfg, 'fileAttrs', strList(config.fileAttrs))
    return { cfg, output, imageUids: strList(config.imageUids) ?? [], dropped }
  }

  // Flat = a viewer recording. The look is the channels/overlays half; everything else was a top-level
  // field of the record request.
  const cfg: BatchMovieCfg = isObj(config.look) ? { ...(config.look as BatchMovieCfg) } : {}
  put(cfg, 'valueNames', strList(config.valueNames))
  put(cfg, 'labelValueNames', strList(config.labelValueNames))
  put(cfg, 'labelContour', num(config.labelContour))
  put(cfg, 'show3D', typeof config.show3D === 'boolean' ? config.show3D : undefined)
  put(cfg, 'zSlice', config.zSlice === null ? null : num(config.zSlice))
  put(cfg, 'compareLayout', COMPARE_LAYOUTS.includes(config.compareLayout as CompareLayout)
    ? config.compareLayout as CompareLayout : undefined)
  put(cfg, 'compareContrast', config.compareContrast === 'reference' || config.compareContrast === 'version'
    ? config.compareContrast : undefined)
  put(cfg, 'titleCard', output.titleCard)
  put(cfg, 'tStart', num(config.tStart))
  put(cfg, 'tEnd', config.tEnd === null ? null : num(config.tEnd))
  // A viewer recording is named after its IMAGE (`_movie_named_path`), a batch after the uid. Without
  // this, regenerating a restored single recording wrote a uid-named twin beside the original instead
  // of reproducing it — so the naming rule comes back with the look, like everything else about it.
  cfg.nameByImage = true

  const branches = strList(config.branchValueNames) ?? []
  if (branches.length) dropped.push(`skeletons (${branches.join(', ')}) — the batch page has no control for them`)

  const uid = typeof config.imageUid === 'string' ? config.imageUid : ''
  return { cfg, output, imageUids: uid ? [uid] : [], dropped }
}

/** One keyframe, in the shape the Animation page's editor wants it back. */
export interface RestoredKeyframe {
  viewState: Record<string, unknown>
  /** Seconds this keyframe tweens FROM the previous one. Recovered from `steps / fps` when the
   *  editor's own duration was not banked — the render payload only carries whole frame counts. */
  duration: number
  assetId?: string
  title?: string
}

export interface KeyframeRestore {
  frames: RestoredKeyframe[]
  output: MovieOutputCfg
  /** The image the timeline belongs to. Empty for animations recorded before this was banked — the
   *  page filters keyframes by image, so an empty one is what the caller has to report. */
  imageUid: string
  dropped: string[]
}

/**
 * A `keyframes` config → the Animation page's timeline.
 *
 * What was banked is the RENDER payload — `{viewState, steps}` per keyframe — because that is what the
 * page sends and what the recorder consumes. It is missing everything the editor needs and the renderer
 * does not: the thumbnail, the title, and the duration in seconds. Those ride along in a parallel
 * `keyframeMeta` array (same order, added with this phase), so an animation recorded from here restores
 * with its strip intact and an older one restores as bare views — degraded, listed, never refused.
 */
export function keyframeRestore(config: Record<string, unknown> | null | undefined): KeyframeRestore | null {
  if (!isObj(config)) return null
  const raw = Array.isArray(config.keyframes) ? config.keyframes : null
  if (!raw || !raw.length) return null
  const output = outputOf(config)
  const fps = output.fps ?? 15
  const meta = Array.isArray(config.keyframeMeta) ? config.keyframeMeta : []
  const dropped: string[] = []

  const frames: RestoredKeyframe[] = []
  for (let i = 0; i < raw.length; i++) {
    const k = raw[i]
    if (!isObj(k) || !isObj(k.viewState)) continue
    const m = isObj(meta[i]) ? (meta[i] as Record<string, unknown>) : {}
    const f: RestoredKeyframe = {
      viewState: k.viewState as Record<string, unknown>,
      // the editor's own seconds when banked; otherwise back out of the frame count it was rendered at
      duration: num(m.duration) ?? Math.max(0.1, (num(k.steps) ?? fps) / fps),
    }
    if (typeof m.assetId === 'string') f.assetId = m.assetId
    if (typeof m.title === 'string' && m.title) f.title = m.title
    frames.push(f)
  }
  if (!frames.length) return null
  if (!meta.length) dropped.push('keyframe thumbnails — this animation predates them being saved')

  const imageUid = typeof config.imageUid === 'string' ? config.imageUid : ''
  return { frames, output, imageUid, dropped }
}

/**
 * Names the config asks for that no longer exist — the failure Decision 6 says is the real one. A
 * config names versions, segmentations, channels and a colour-by column BY STRING, so deleting a
 * segmentation leaves a config that is structurally perfect and semantically dead.
 *
 * A list left `undefined` is NOT checked: the caller passes it only once it genuinely knows what the
 * destination offers. An empty array means "this image has none", which is a real answer and does make
 * a named item missing; `undefined` means "not loaded yet", where reporting everything as dead would be
 * worse than reporting nothing.
 */
export interface AvailableNames {
  versions?: string[]
  segmentations?: string[]
  channels?: string[]
  colourBy?: string[]
}
export function missingRefs(cfg: BatchMovieCfg, avail: AvailableNames): string[] {
  const out: string[] = []
  const check = (label: string, wanted: string[] | undefined, have: string[] | undefined) => {
    if (!have || !wanted) return
    const known = new Set(have)
    for (const w of wanted) if (w && !known.has(w)) out.push(`${label} '${w}'`)
  }
  check('version', cfg.valueNames, avail.versions)
  check('segmentation', cfg.labelValueNames, avail.segmentations)
  check('channel', cfg.channels ? Object.keys(cfg.channels) : undefined, avail.channels)
  check('colour-by', cfg.colourBy ? [cfg.colourBy] : undefined, avail.colourBy)
  return out
}

/**
 * WHICH SET a restore should land in: the one holding the movie's images, not whichever set happens to
 * be active. Pass each image's home set (`project.setUidOfImage`, `null` when the image is gone).
 *
 * Both destinations store per set — the batch page's config and output, and both pages' image
 * selection — so the target has to be settled before anything is written. Reading the ACTIVE set
 * instead is what made a restore report "images from another set" and leave the user to go and switch
 * it themselves (Dominik, 2026-08-10); one click should repair what it can.
 *
 * Images spanning two sets have no single answer, so the caller's fallback wins and the difference is
 * reported. Same when nothing is known (an old movie that banked no image).
 */
export function restoreTargetSet(homes: (string | null | undefined)[], fallback: string): string {
  const found = [...new Set(homes.filter((s): s is string => !!s))]
  return found.length === 1 ? found[0] : fallback
}

/** One line naming what could not be restored, or '' when everything came back. Kept here so both
 *  pages word it identically — and short, per the UI copy budget (docs/UI.md). */
export function restoreNote(missing: string[], dropped: string[]): string {
  const all = [...missing, ...dropped]
  if (!all.length) return ''
  return `Not restored: ${all.join(', ')}`
}
