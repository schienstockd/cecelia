// The lens chip's Find similar (docs/todo/LENS_QUERY_PLAN.md → P2): runs `clustRegions.lensQuery` for
// the pinned lens on a region run, then saves a LENS FIGURE — a still of the lens carrying where it was,
// the look on screen when it ran, and the score column it wrote — to Movies & figures (rename, tags,
// delete with its scores). The viewer's Lenses section lists the lens figures on this image: Show scores
// per lens (colour by its column), Go to (re-pin it with its look), delete.
import { computed, onUnmounted, ref, watch } from 'vue'
import { useSettingsStore } from '../stores/settings'
import { useTaskStore } from '../stores/tasks'
import { useWsStore } from '../stores/ws'
import { usePushStore } from '../stores/push'
import { useViewerStore } from '../stores/viewer'
import { fetchColourByColumns } from '../utils/viewer/colourByColumns'
import { niceFigureBarUm, type FigureSpec } from '../utils/figureSpec'
import type { MovieEntry } from '../utils/movies'
import { getJson, postJson, tryApi } from '../utils/api'
import type { BatchMovieCfg } from '../utils/batchMovie'
import {
  LENS_QUERY_FUN, figureLensBody, lensFigureSpec, lensFiguresFor, lensOpenPending, lensQueryParams, lensScoreColumn,
  nextLensName, regionRunsIn, type LensFigureRow, type LensLook, type LensRegionRun,
} from '../utils/viewer/lensQuery'

export interface VwLensQueryDeps {
  projectUid: string
  imageUid: string
  imageName: () => string
  setUid: () => string
  /** the image's segmentations with a cell table */
  segmentations: () => string[]
  /** the pinned lens on the tissue (level-0 px), or null when not pinned */
  pinned: () => { x: number; y: number; r: number } | null
  /** the frame whose cells form the lens (a time-lapse), or null */
  frame: () => number | null
  /** the view as a figure spec (channels, contrast, frame) — the lens figure's base; null when it can't be one */
  figureBase: () => FigureSpec | null
  /** µm per level-0 pixel, for the figure's scale bar (0 = uncalibrated) */
  umPerPx: () => number
  /** what is toggled on screen now — saved with the lens, restored when it is reopened */
  look: () => LensLook
  /** the viewer's recorded look (a movie config) — the figure draws its overlays where the renderer can */
  movieLook: () => BatchMovieCfg
}

export function useVwLensQuery(d: VwLensQueryDeps) {
  const settings = useSettingsStore(), tasks = useTaskStore(), ws = useWsStore(), push = usePushStore()
  const viewer = useViewerStore()
  const columns = ref<Record<string, string[]>>({})       // per segmentation, its cell columns
  const runs = computed<LensRegionRun[]>(() =>
    Object.entries(columns.value).flatMap(([vn, cols]) => regionRunsIn(vn, cols)))
  const runKey = (r: LensRegionRun) => `${r.vn}|${r.suffix}`
  const picked = ref('')
  const run = computed(() => runs.value.find(r => runKey(r) === picked.value) ?? runs.value[0] ?? null)
  const busy = ref(false), error = ref(''), saved = ref('')
  /** Bumped when a query wrote a column — the colour-by list refetches on it. */
  const columnsTick = ref(0)
  let taskId = ''
  interface Pending { run: LensRegionRun; lens: { x: number; y: number; r: number }; t: number | null; name: string
                      look: LensLook; base: FigureSpec | null; movieLook: BatchMovieCfg }
  let pending: Pending | null = null

  async function refreshColumns() {
    if (!d.projectUid || !d.imageUid) return
    const out: Record<string, string[]> = {}
    for (const vn of d.segmentations()) out[vn] = (await fetchColourByColumns(d.projectUid, d.imageUid, vn)).cells
    columns.value = out
  }
  // fetched once a lens is pinned (the only time the button shows), and again after a query wrote
  watch(() => !!d.pinned(), on => { if (on && !Object.keys(columns.value).length) void refreshColumns() }, { immediate: true })

  // ── The lens figures on this image (the Lenses section) ─────────────────────────────────────────
  const lensFigures = ref<LensFigureRow[]>([])
  /** every lens figure's column in the project — their names are taken (`nextLensName`) */
  let figureColumns: string[] = []
  async function refreshLensFigures() {
    if (!d.projectUid) return
    const r = await tryApi(getJson<{ movies?: MovieEntry[] }>('/api/movies', { query: { projectUid: d.projectUid } }))
    if (!r.ok && r.status === 0) { lensFigures.value = []; return }   // no reply: keep the taken names
    const movies = r.ok ? (r.data?.movies ?? []) : []
    lensFigures.value = lensFiguresFor(movies, d.imageUid)
    figureColumns = movies.flatMap(m => (m.lens ? [m.lens.column] : []))
  }
  void refreshLensFigures()
  // a figure saved, renamed or deleted anywhere — the server broadcasts `movies:changed`
  watch(() => push.tick, () => {
    const ev = push.lastEvent
    if (ev?.kind === 'movies:changed' && (!ev.projectUid || ev.projectUid === d.projectUid)) void refreshLensFigures()
  })
  /** Show scores for one lens: the view coloured by its column (again = off). */
  const shownScores = computed(() => (d.setUid() ? settings.getColourBy(d.setUid()) : ''))
  function setShowScores(row: LensFigureRow, on: boolean) {
    if (d.setUid()) settings.setColourBy(d.setUid(), on ? row.lens.column : '')
  }
  /** Re-pin a lens with its look — on its own image (the section offers Go to only there). */
  function goToLens(row: LensFigureRow) {
    if (row.lens.sourceUid === d.imageUid) viewer.setPendingViewState(lensOpenPending(row.lens))
  }
  /** Rename a lens — its Movies & figures display name (the file and the score column keep theirs);
   *  '' goes back to the lens's own name. */
  async function renameLens(row: LensFigureRow, displayName: string) {
    error.value = ''
    const r = await tryApi(postJson('/api/movies/meta', { projectUid: d.projectUid, names: [row.name], displayName }))
    if (!r.ok) error.value = `Could not rename ${row.label}`
    await refreshLensFigures()
  }
  /** Delete a lens figure — the server takes its score column with it. */
  async function deleteLens(row: LensFigureRow) {
    error.value = ''
    const r = await tryApi(postJson('/api/movies/delete', { projectUid: d.projectUid, name: row.name }))
    if (!r.ok) error.value = `Could not delete ${row.label}`
    if (shownScores.value === row.lens.column && d.setUid()) settings.setColourBy(d.setUid(), '')
    await Promise.all([refreshLensFigures(), refreshColumns()]); columnsTick.value++
  }

  async function find() {
    const lens = d.pinned(), r = run.value
    if (!lens || !r || busy.value) return
    busy.value = true
    // what is on screen NOW, at the click — the figure and the look are of this, not of the scores after
    const atClick = { lens: { ...lens }, t: d.frame(), look: d.look(), base: d.figureBase(), movieLook: d.movieLook() }
    // named against what is there now — another window may have run a query, or a column was deleted
    try { await Promise.all([refreshColumns(), refreshLensFigures()]) } catch { /* name from what was read */ }
    const name = nextLensName(Object.values(columns.value).flat(), figureColumns)
    const params = lensQueryParams(r, atClick.lens, atClick.t, name)
    const t = tasks.add({ module: 'spatialAnalysis', label: 'Find regions like the lens', imageUid: d.imageUid,
                          imageName: d.imageName(), imageUids: [d.imageUid], status: 'queued', taskName: 'lensQuery',
                          funName: LENS_QUERY_FUN, params, projectUid: d.projectUid })
    taskId = t.id; busy.value = true; error.value = ''; saved.value = ''
    pending = { run: r, name, ...atClick }
    ws.send({ type: 'task:run', taskId: t.id, funName: LENS_QUERY_FUN, params, imageUid: d.imageUid,
              imageUids: [d.imageUid], projectUid: d.projectUid, setUid: d.setUid(), poolName: 'cpu' })
  }

  // The lens figure: a still of the lens as the view showed it, carrying the lens, its look and its column.
  async function saveFigure(p: Pending, column: string) {
    if (!p.base) { error.value = 'Scored — no figure saved (needs a visible channel in 2D)'; return }
    const spec: FigureSpec & Record<string, unknown> = { ...lensFigureSpec(p.base, p.lens), look: p.movieLook }
    if (d.umPerPx() > 0) spec.scaleBarUm = niceFigureBarUm(spec.sizePx, d.umPerPx())
    const r = await tryApi(postJson('/api/figures/render', {
      projectUid: d.projectUid, imageUid: d.imageUid, spec, name: `${d.imageName()}_${p.name}`,
      lens: figureLensBody(p.run, p.lens, p.t, column, p.look),
    }))
    if (r.ok) saved.value = p.name
    else error.value = `Scored — figure not saved: ${r.error}`
    void refreshLensFigures()
  }

  function onStatus(msg: Record<string, unknown>) {
    if (!taskId || msg.taskId !== taskId) return
    const s = String(msg.status ?? '')
    const p = pending
    if (s === 'done' && p) {
      busy.value = false; taskId = ''; pending = null
      const column = lensScoreColumn(p.name)
      void refreshColumns().then(() => {
        columnsTick.value++
        if (d.setUid()) settings.setColourBy(d.setUid(), column)   // you asked for it: show the result
      })
      void saveFigure(p, column)
    } else if (s === 'failed' || s === 'cancelled') {
      busy.value = false; taskId = ''; pending = null
      error.value = s === 'failed' ? 'Find similar failed — see its log in the task manager' : ''
    }
  }
  ws.on('task:status', onStatus)
  onUnmounted(() => ws.off('task:status', onStatus))

  return { runs, run, picked, runKey, busy, error, saved, columnsTick, find,
           lensFigures, shownScores, setShowScores, goToLens, renameLens, deleteLens }
}
