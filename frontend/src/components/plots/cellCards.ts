/**
 * Cell-card payload contract — mirror of `api/src/cell_cards_api.jl`.
 *
 * A cell card is one trackclust pop rendered as a filmstrip of the pop's MEDOID track over its
 * frame range, with a stats footer. Cards are pool-shaped from day one (`CELL_CARDS_PLAN` Decision
 * 0): the run's pool = `partOf` (multi-image, from `{props}.clustfeatures.json`) × co-clustered
 * value_names (multi-segmentation, `co_clustered_value_names`). A pool of 1 is still a pool. The
 * medoid resolves to a single `(uid, value_name, track_id)` triple; the card's image comes from
 * that image, its trace from that value_name's `pop_df`.
 *
 * This file is TYPES ONLY — no runtime, no imports. The Julia side owns the encoding; the frontend
 * owns the rendering; both sides must move together when a field changes.
 */

/** One member of the clustering run's pool. `value_name` is a segmentation label (e.g. "B", "T"). */
export interface PoolMember {
  uid: string
  value_name: string
}

/** 5-number summary for one measure on one pop — the box + whiskers of a mini-boxplot. */
export interface CardStat {
  /** Canonical `live.track.*` name — e.g. `"speed"`, `"straightness"`. */
  name: string
  /** Raw min (whisker low), q25 (box low), median (line), q75 (box high), max (whisker high). */
  min: number
  q25: number
  median: number
  q75: number
  max: number
}

/** Reference to one PNG in the board-assets sidecar (served via `/api/board-assets/{asset_id}`). */
export interface CardFrame {
  /** Frame index in the medoid track's image. */
  t: number
  asset_id: string
  /**
   * Frame time in seconds (frame × TimeIncrement). Present only when the medoid's image RECORDS a
   * time increment; absent when only frame index is meaningful (the FE falls back to "t=N"). See
   * `img_physical_sizes` / `img_scale_axes` — the same discipline `pop_df(centroids=:physical)` uses.
   */
  t_s?: number
}

/** The medoid cell for one card. `frames = [t0, t1]` is the medoid track's inclusive frame span. */
export interface CardMedoid extends PoolMember {
  track_id: number
  frames: [number, number]
}

/** One card = one trackclust pop. `name`, `path`, `colour` come straight from the pop. */
export interface Card {
  /** Pop path — the population manager's tree address (e.g. `"/Population 1"`). */
  path: string
  /** Pop display name. Renames flow through unchanged. */
  name: string
  /** Pop colour, hex (`"#RRGGBB"`). Also the trace colour baked into the frames server-side. */
  colour: string
  /** Cluster-membership count over the FULL pool. Small pops (n<10) still render; the card notes n. */
  n: number
  medoid: CardMedoid
  filmstrip: CardFrame[]
  stats: CardStat[]
}

/** `POST /api/cell_cards` response. `pool` echoes what the backend expanded from the run's sidecar. */
export interface CardsResponse {
  pool: PoolMember[]
  cards: Card[]
  /**
   * Per-measure pool-wide `[globalMin, globalMax]` across EVERY card in this response — the shared
   * scale each measure's mini-boxplot draws against so a box's position is comparable card-to-card.
   * Absent for measures that appear on no card. Same ends the boxes use (raw min/max, no Tukey clip).
   */
  statScales: Record<string, [number, number]>
}

/** `POST /api/cell_cards` request. `root_uid` is the image the analysis board was opened from. */
export interface CardsRequest {
  root_uid: string
  value_name: string
  /** The clustering run's suffix — column is `clusters.{suffix}`. */
  cluster_col: string
  /** Which pops to render, and which cluster ids each maps to (usually one). */
  pops: Array<{ path: string; cluster_ids: number[] }>
  /** Same viewer state `ImageStripView` captures — channels, LUT, per-image resolution. */
  view_state: unknown
}
