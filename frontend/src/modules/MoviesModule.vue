<script setup lang="ts">
// Movies page (/movies) — a simple in-app player for the project's rendered mp4s (single-image,
// animation and batch recordings, all under {proj}/movies/). Native <video> element (no player
// library), streamed from the range-capable backend route so seeking works; playback speed + zoom on
// top. Motivated by there being no good desktop player to rely on. See docs/todo/ANIMATION_PLAN.md.
import { ref, computed, watch, nextTick, onMounted, onBeforeUnmount } from 'vue'
import { useRouter } from 'vue-router'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useProjectStore } from '../stores/project'
import { useSettingsStore } from '../stores/settings'
import { useLogStore } from '../stores/log'
import { useTaskStore } from '../stores/tasks'
import { formatBytes } from '../utils/storage'
import { movieStreamUrl, sortMovies, anchoredScroll, movieRows,
         filterMovieRows, movieFilterOptions, parseMovieTags,
         movieChannelCells, movieChannelCount, nextMovieName,
         type MovieEntry, type MovieRow } from '../utils/movies'
import { sortRows, parseSortState, type SortState, type SortValue } from '../utils/sortRows'
import { rafCoalesce } from '../utils/rafCoalesce'
import { RESTORE_ROUTE, type RestoreKind } from '../utils/movieRestore'
import { attrKeysOf, emptyAttrFilter, attrFilterActive, matchesAttrFilter, pruneAttrFilter,
         type AttrFilterState } from '../utils/attrFilter'
import { useInlineEdit } from '../composables/useInlineEdit'
import CcToggle from '../components/CcToggle.vue'
import ModulePage from '../components/ModulePage.vue'
import CollapsiblePanel from '../components/CollapsiblePanel.vue'
import SuggestInput from '../components/SuggestInput.vue'
import ChipSelect, { type ChipOption } from '../components/ChipSelect.vue'
import ConfirmButton from '../components/ConfirmButton.vue'
import BaseModal from '../components/BaseModal.vue'
import SelectionTable, { type SelectionColumn } from '../components/SelectionTable.vue'
import AttrFilterPanel from '../components/AttrFilterPanel.vue'
import { isTypingTarget } from '../utils/typingTarget'

const projectMeta = useProjectMetaStore()
const project = useProjectStore()
const settings = useSettingsStore()
const log = useLogStore()
const tasks = useTaskStore()
const router = useRouter()

const projectUid = computed(() => projectMeta.current?.uid ?? '')
const hasProject = computed(() => projectMeta.hasProject)

const movies = ref<MovieEntry[]>([])
const selected = ref<string>('')          // name of the currently-loaded movie
const loading = ref(false)
const videoEl = ref<HTMLVideoElement | null>(null)
const viewportEl = ref<HTMLElement | null>(null)

// Layout-based zoom: give the <video> a concrete pixel box (fit-to-viewport × zoom) rather than a CSS
// transform, so a zoomed movie actually grows the layout and the overflow:auto viewport can scroll/pan
// to any edge. Needs the video's intrinsic size (from loadedmetadata) and the live viewport size (a
// ResizeObserver — the viewport reflows with the sidebar/window).
const natW = ref(0), natH = ref(0)        // video intrinsic pixel size
const vpW = ref(0), vpH = ref(0)          // viewport content size
let ro: ResizeObserver | null = null

const displaySize = computed(() => {
  if (!natW.value || !natH.value || !vpW.value || !vpH.value) return null
  const fit = Math.min(vpW.value / natW.value, vpH.value / natH.value)   // "contain" at zoom 1
  const s = fit * settings.moviesZoom
  return { w: Math.round(natW.value * s), h: Math.round(natH.value * s) }
})
// concrete px box once measured; before metadata, fall back to a plain max-fit so it still shows
const videoStyle = computed(() =>
  displaySize.value
    ? { width: `${displaySize.value.w}px`, height: `${displaySize.value.h}px` }
    : { maxWidth: '100%', maxHeight: '100%' })

// ── Cursor/centre-anchored zoom (mirrors the plot canvas's shift+wheel / shift +/-) ──────────────
// 1× = fit; up to 8×. Wheel anchors to the cursor, keyboard to the viewport centre, the slider too. We
// change the zoom (→ new px box) then, on the next tick, set the scroll so the focal point stays put
// (anchoredScroll) — so it grows about the cursor, not the top-left corner.
const MOVIES_ZOOM_MIN = 1, MOVIES_ZOOM_MAX = 8, ZOOM_STEP = 1.15
const clampZoom = (z: number) => Math.max(MOVIES_ZOOM_MIN, Math.min(MOVIES_ZOOM_MAX, z))
const zoomLabel = computed(() => `${Math.round(settings.moviesZoom * 10) / 10}×`)

function zoomAround(target: number, fx: number, fy: number) {
  const vp = viewportEl.value
  const before = displaySize.value
  const next = clampZoom(target)
  if (!vp || !before) { settings.moviesZoom = next; return }
  const scroll = { left: vp.scrollLeft, top: vp.scrollTop }
  settings.moviesZoom = next
  nextTick(() => {
    const after = displaySize.value
    if (!after) return
    const s = anchoredScroll(before, after, { w: vp.clientWidth, h: vp.clientHeight }, { x: fx, y: fy }, scroll)
    vp.scrollLeft = s.left
    vp.scrollTop = s.top
  })
}
function onWheel(e: WheelEvent) {
  if (!e.shiftKey) return          // plain wheel scrolls the viewport; shift+wheel zooms (like the canvas)
  e.preventDefault()
  const vp = viewportEl.value
  if (!vp) return
  const r = vp.getBoundingClientRect()
  zoomAround(settings.moviesZoom * (e.deltaY < 0 ? ZOOM_STEP : 1 / ZOOM_STEP), e.clientX - r.left, e.clientY - r.top)
}
function onKey(e: KeyboardEvent) {
  if (!e.shiftKey) return
  if (isTypingTarget(e)) return
  const vp = viewportEl.value
  if (!vp) return
  const cx = vp.clientWidth / 2, cy = vp.clientHeight / 2
  if (e.key === '+' || e.key === '=') { e.preventDefault(); zoomAround(settings.moviesZoom * ZOOM_STEP, cx, cy) }
  else if (e.key === '-' || e.key === '_') { e.preventDefault(); zoomAround(settings.moviesZoom / ZOOM_STEP, cx, cy) }
  else if (e.key === '0' || e.key === ')') { e.preventDefault(); settings.moviesZoom = 1 }
}
// slider zooms about the viewport centre too, so it's consistent with wheel/keys
function onZoomSlider(v: number) {
  const vp = viewportEl.value
  if (!vp) { settings.moviesZoom = clampZoom(v); return }
  zoomAround(v, vp.clientWidth / 2, vp.clientHeight / 2)
}

// Measuring FEEDS the video's box (`displaySize`), so the observer must not write it during delivery:
// a video sized past the viewport makes a scrollbar appear, which shrinks the box the observer reports
// — the loop behind "ResizeObserver loop completed with undelivered notifications" in the log rail.
// It stayed anonymous because the viewport's BORDER box never moves while that happens; only the inner
// box does (`utils/roLoopTrace.ts`). The fix is the canonical pair (`usePlotResize`): coalesce the
// write into a frame, so the resize is delivered as a fresh cycle, and skip one the size didn't ask
// for — which here is free, since a ref written its own value changes nothing.
function measureViewport() {
  const el = viewportEl.value
  if (!el) return
  vpW.value = el.clientWidth
  vpH.value = el.clientHeight
}
const measureFrame = rafCoalesce(measureViewport)

watch(viewportEl, (el, prev) => {
  ro?.disconnect()
  prev?.removeEventListener('wheel', onWheel)
  if (el) {
    ro = new ResizeObserver(() => measureFrame.schedule())
    ro.observe(el)
    measureViewport()          // the first one is not inside a delivery cycle — no frame to wait for
    el.addEventListener('wheel', onWheel, { passive: false })   // passive:false → we can preventDefault
  }
})
onBeforeUnmount(() => {
  measureFrame.cancel()
  ro?.disconnect()
  viewportEl.value?.removeEventListener('wheel', onWheel)
  window.removeEventListener('keydown', onKey)
})

const currentUrl = computed(() =>
  selected.value && projectUid.value ? movieStreamUrl(projectUid.value, selected.value) : '')

async function refresh() {
  if (!projectUid.value) { movies.value = []; return }
  loading.value = true
  try {
    const res = await fetch(`/api/movies?projectUid=${encodeURIComponent(projectUid.value)}`)
    const body = await res.json().catch(() => ({}))
    movies.value = sortMovies((body.movies ?? []) as MovieEntry[])
    // keep the selection if it still exists, else pick the newest
    if (!movies.value.some(m => m.name === selected.value)) {
      selected.value = movies.value[0]?.name ?? ''
    }
  } catch (e) {
    log.error(`Could not list movies: ${e}`, { source: 'movies' })
    movies.value = []
  } finally {
    loading.value = false
  }
}

let forcePlay = false     // the next movie in a chain plays even with Autoplay off — see `onEnded`
// <video> resets playbackRate to 1 whenever a new source loads, so re-apply the persisted speed on
// load (and whenever the user changes it). Also grab the movie's intrinsic size for the zoom box, and
// start playback if autoplay is on (covers selecting a new movie, not just the first load — the native
// `autoplay` attr only reliably fires on first load; the .play() call catches the re-select case).
function onLoadedMeta() {
  const v = videoEl.value
  if (!v) return
  v.playbackRate = settings.moviesPlaybackRate
  natW.value = v.videoWidth
  natH.value = v.videoHeight
  const play = settings.moviesAutoplay || forcePlay
  forcePlay = false
  if (play) v.play().catch(() => { /* autoplay may be blocked until user gesture */ })
}

// What happens when a movie ENDS (`settings.moviesEndMode`). `loop` is the <video> element's own
// attribute, so only `next` is ours: step to the following row IN THE SHOWN ORDER and play it. It
// plays whatever Autoplay says — that setting is about selecting a movie by hand, and asking for the
// next one to play is asking for it to play. The chain stops at the end of the list rather than
// wrapping (`nextMovieName`), so a page left open doesn't cycle all night.
function onEnded() {
  if (settings.moviesEndMode !== 'next') return
  const next = nextMovieName(shownOrder.value, selected.value)
  if (!next) return
  forcePlay = true
  selected.value = next
}
watch(() => settings.moviesPlaybackRate, () => { if (videoEl.value) videoEl.value.playbackRate = settings.moviesPlaybackRate })

const SPEEDS = [0.25, 0.5, 1, 1.5, 2, 4]

onMounted(() => { refresh(); window.addEventListener('keydown', onKey) })
// re-list when the project changes (opening a different project) — the page can be left mounted
watch(projectUid, refresh)
// Auto-refresh when a movie:record/animation/batch task lands terminal — otherwise a render
// completing while this page is open never shows up until the user navigates away and back.
// Cheap watch: count the number of finished movie tasks for the current project; refresh when
// it goes up. Same shape (module = 'viewer' for records/animations, 'batch' for batches).
const finishedMovieCount = computed(() => {
  const uid = projectUid.value
  if (!uid) return 0
  let n = 0
  for (const t of tasks.tasks) {
    if (t.projectUid !== uid) continue
    if (t.status !== 'done') continue
    const fn = t.funName ?? ''
    if (fn === 'movie.record' || fn === 'movie.animation' || fn === 'movie.batch') n++
  }
  return n
})
watch(finishedMovieCount, (n, prev) => { if (n > prev) refresh() })

function movieTime(mtime: number): string {
  return new Date(mtime * 1000).toLocaleString()
}

// ── The list ──────────────────────────────────────────────────────────────────
// A `SelectionTable`, not a bespoke playlist: picking one movie by comparing its date and size is
// exactly what that component is for, and it carries the sorting. `sortMovies` (newest first) is still
// the order it is HANDED — clearing the sort with a third header click comes back to it.
// Per-column widths: a size needs a fraction of what a name does, and one width for all of them is
// what pushed the table off its panel.
//
// The Details columns (`settings.moviesShowDetails`) are the SOURCE IMAGE's, joined on per row — one
// column per channel slot and one per attribute key, mirroring the image table's own attribute view so
// the same facts read the same way on both pages. Off by default: they only say anything once a project
// has attributes, and this table lives in a side panel where every column costs width.
const MOVIE_COLUMNS = computed<SelectionColumn[]>(() => [
  // Pinned: with the Details columns on, the table is wider than the panel, and scrolling sideways past
  // the name leaves rows you can no longer tell apart. Same reason the image table pins its own.
  { key: 'label',    label: 'Movie',    sortable: true, width: 190, sticky: true },
  { key: 'tagText',  label: 'Tags',     sortable: true, width: 120 },
  ...(settings.moviesShowDetails
    // A channel column is not sortable — the same as the image table's, because its value is a slot's
    // name rather than a per-movie measure, and ordering a list by "what channel 2 is called" answers
    // nothing. The attribute columns are, which is the axis a cohort is actually organised on.
    ? [...channelIndices.value.map(i => ({ key: `ch:${i}`, label: String(i), width: 90 })),
       ...attrKeys.value.map(k => ({ key: `attr:${k}`, label: k, sortable: true, width: 110 }))]
    : []),
  { key: 'timeText', label: 'Recorded', sortable: true, sortKey: 'mtime', width: 120 },
  { key: 'sizeText', label: 'Size',     sortable: true, sortKey: 'size',  width: 70 },
])
// ── Filters ───────────────────────────────────────────────────────────────────
// Star and tags COMPOSE with each other and with the column sort — they answer different questions,
// so one never replaces another (Decision 3/4). Both persist: a filter that resets on navigation is
// one the user has to re-apply every time they come back to compare two movies.
const starredOnly = ref(localStorage.getItem('cc.movies.starredOnly') === 'true')
watch(starredOnly, v => localStorage.setItem('cc.movies.starredOnly', String(v)))
const pickedTags = ref<string[]>(JSON.parse(localStorage.getItem('cc.movies.tags') ?? '[]'))
watch(pickedTags, v => localStorage.setItem('cc.movies.tags', JSON.stringify(v)), { deep: true })

// Filtering by the source image's ATTRIBUTES — the same control the image table has, over the shared
// `AttrFilterPanel` (utils/attrFilter.ts). It composes with star and tags like everything else here.
// Persisted, and TOLERANT of a stale shape: a filter is not worth failing the page over.
const attrFilter = ref<AttrFilterState>((() => {
  try {
    const raw = JSON.parse(localStorage.getItem('cc.movies.attrFilter') ?? 'null')
    return raw && typeof raw === 'object' ? { ...emptyAttrFilter(), ...raw } : emptyAttrFilter()
  } catch { return emptyAttrFilter() }
})())
watch(attrFilter, v => localStorage.setItem('cc.movies.attrFilter', JSON.stringify(v)), { deep: true })
const filtersOpen = ref(localStorage.getItem('cc.movies.filtersOpen') === 'true')
watch(filtersOpen, v => localStorage.setItem('cc.movies.filtersOpen', String(v)))

// Declared BEFORE the rows that read them. `movieTableRows` is a lazy computed, but the `watch` on it
// further down is NOT lazy — a watcher evaluates its source once at creation to capture the old value,
// and that reached these two while they were still in the temporal dead zone. Setup threw, so the
// whole page rendered blank; neither vue-tsc nor the production build sees it, because TS does not
// track TDZ through a closure.
// The project's images, from the store that already holds them — a movie is joined back to its image
// client-side, so the Details columns cost no request (utils/movies.ts → resolveMovieImageUid).
const allImages = computed(() => project.sets.flatMap(s => s.images))
const allRows = computed(() => movieRows(movies.value, formatBytes, movieTime, allImages.value))
const movieTableRows = computed(() =>
  filterMovieRows(allRows.value, starredOnly.value, pickedTags.value)
    .filter(r => matchesAttrFilter(r.attr, attrFilter.value)))
// The chips are derived from ALL rows, not the filtered ones — a value you just picked must not
// disappear from the row it was picked in, and the next attribute's chips have to still be offered.
const filterAttrKeys = computed(() => attrKeysOf(allRows.value))
// A persisted filter outlives the project it was picked in: `Treatment: MERTK` carried into a project
// without it hides every movie, and an empty list looks like an empty folder. Same reflex as the tag
// chips above — drop what no longer exists rather than leaving it silently narrowing.
watch(allRows, rows => {
  const next = pruneAttrFilter(attrFilter.value, rows)
  if (next !== attrFilter.value) attrFilter.value = next
})

// ── The order the list is SHOWN in ────────────────────────────────────────────
// The table renders the header cycle and reports it (`v-model:sort`, the ImageTable pattern), but the
// ORDER is applied here — "play the next movie" means the next one on screen, so this page has to be
// able to read the same sequence the table renders. `sortRows` is the shared rule, and the column's
// `sortKey` is what a formatted column (a date, a size) actually sorts by.
// Controlling the sort turns off `SelectionTable`'s own persistence, so it is kept here — under the
// key the table used, so a sort chosen before this landed still comes back.
const SORT_KEY = 'cc.movies.sort'
const movieSort = ref<SortState>(parseSortState(localStorage.getItem(SORT_KEY)))
watch(movieSort, s => {
  if (s) localStorage.setItem(SORT_KEY, JSON.stringify(s))
  else localStorage.removeItem(SORT_KEY)
})
const shownRows = computed(() => {
  const s = movieSort.value
  if (!s) return movieTableRows.value          // no sort = the order it arrives in (newest first)
  const field = MOVIE_COLUMNS.value.find(c => c.key === s.key)?.sortKey ?? s.key
  // read by column key, as the table does — declared fields plus the flattened `attr:<key>`s
  return sortRows(movieTableRows.value, r => (r as unknown as Record<string, SortValue>)[field], s.dir)
})
const shownOrder = computed(() => shownRows.value.map(r => r.name))

// ── The Details columns: the source image's channels and attributes ───────────
// Derived from the rows ACTUALLY SHOWN, so filtering to one cohort drops the columns none of them has
// rather than leaving a screenful of blanks.
const attrKeys = computed(() =>
  settings.moviesShowDetails ? attrKeysOf(movieTableRows.value) : [])
const channelIndices = computed(() => {
  const n = settings.moviesShowDetails
    ? movieChannelCount(movieTableRows.value, settings.moviesChannelMode) : 0
  return Array.from({ length: n }, (_, i) => i + 1)     // 1-based, like the image table's
})
// One cell array per row, computed once rather than per cell — a slot renders `cells[i - 1]`.
const channelCells = computed(() => {
  const n = channelIndices.value.length
  const out: Record<string, string[]> = {}
  for (const r of movieTableRows.value)
    out[r.name] = movieChannelCells(r.imageChannels, r.movieChannels, n, settings.moviesChannelMode)
  return out
})
// Word-labelled, so the group tooltip is the coverage — per-option tips on top of it would say the
// same thing twice and render over the chips (docs/UI.md → Tooltips, pinned by uiCopy.test.ts). Same
// for `END_MODES` below.
const CHANNEL_MODES: ChipOption[] = [
  { value: 'image', label: 'image' },
  { value: 'movie', label: 'in movie' },
]

// What happens when a movie ends — ONE control, because the three are mutually exclusive outcomes of
// the same moment (`utils/movies.ts` → MovieEndMode).
const END_MODES: ChipOption[] = [
  { value: 'stop', label: 'Stop' },
  { value: 'loop', label: 'Loop' },
  { value: 'next', label: 'Next' },
]

// ── Managing the collection (docs/todo/MOVIE_MANAGEMENT_PLAN.md) ──────────────
// The metadata lives in settings/movies.json, keyed by filename, and is patched one field at a time —
// an absent field means "leave alone", so these never have to read each other's values.
//
// ONE call for N movies, never a loop: the registry is a single JSON file, so N requests rewrite it N
// times and any two in flight lose one side's edit. Categorising a selection is exactly that case.
async function patchMeta(names: string[], patch: Record<string, unknown>) {
  if (!projectUid.value || !names.length) return
  try {
    const res = await fetch('/api/movies/meta', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectUid.value, names, ...patch }),
    })
    if (!res.ok) throw new Error((await res.json().catch(() => ({}))).error ?? res.statusText)
    await refresh()
  } catch (e) {
    log.error(`Could not update ${names.length} movie(s): ${e instanceof Error ? e.message : String(e)}`,
              { source: 'movies' })
  }
}

// Rename is a DISPLAY NAME — the file is never renamed (Decision 2), so nothing that resolves a movie
// by path can break. Clearing the field is a real edit: it goes back to the file name.
const { draft: nameDraft, start: startRename, cancel: cancelRename, commit: commitRename,
        focusInput: focusRenameInput, isEditing: isRenaming } = useInlineEdit()
const beginRename = (r: MovieRow) => startRename(r.name, r.renamed ? r.label : '')
const saveRename = (r: MovieRow) =>
  commitRename(r.name, r.renamed ? r.label : '', v => patchMeta([r.name], { displayName: v }))

const toggleStar = (r: MovieRow) => patchMeta([r.name], { starred: !r.starred })

// ── Editing how a movie was made (Phase 6) ────────────────────────────────────
// A movie carries the config that produced it, so the way to make another one is to reopen the page
// that AUTHORED it, prefilled — not to re-render blind from here. Which page follows from the config's
// KIND, not from who recorded it: a viewer look and a batch look edit in the same place (Decision 7).
// The movie name rides in the query; the destination reads it, applies it, and takes it back out of the
// URL so a reload can't silently re-apply it (composables/useMovieRestore.ts).
const canEdit = (r: MovieRow) => r.hasConfig && r.configKind in RESTORE_ROUTE
function editConfig(r: MovieRow) {
  if (!canEdit(r)) return
  router.push({ path: RESTORE_ROUTE[r.configKind as RestoreKind], query: { fromMovie: r.name } })
}
// The bytes on disk are from a LATER run than the config banked against them (Decision 5) — the movie
// was re-recorded, and this config is not what made the file you are looking at. It still opens; the
// tooltip is where that gets said, since the config is the only thing being reused.
const editTip = (r: MovieRow) => r.configStale
  ? 'Edit the saved settings — this movie was re-recorded since, so they may not be what made it'
  : 'Edit the settings that made this movie'

// Tags are free text (comma or newline separated), parsed to a list — the taxonomy grows without a
// code change (Decision 3). Editing them is the same inline-edit primitive as the name.
const { draft: tagDraft, start: startTags, cancel: cancelTags, commit: commitTags,
        focusInput: focusTagInput, isEditing: isTagging } = useInlineEdit()
// Keyed by the plain movie name, like the rename above: the two `useInlineEdit` instances are already
// separate, so a `tags:` prefix namespaced nothing and only had to be kept in step in three places —
// which it wasn't, and the tag input never rendered.
const beginTags = (r: MovieRow) => startTags(r.name, r.tags.join(', '))
const saveTags = (r: MovieRow) =>
  commitTags(r.name, r.tags.join(', '), v => patchMeta([r.name], { tags: parseMovieTags(v) }))

// ── The selection, and what acts on it ────────────────────────────────────────
// Two independent things, as on the image table: `checked` is the working SET the bulk actions apply
// to, and `playing` is the one movie in the player, set by its eye. Conflating them is what made a
// single-select list unable to delete or categorise more than one at a time.
const checked = ref<string[]>([])
// a filter (or a delete) can take a checked movie off the list; it must not stay in the selection,
// where it would be acted on invisibly
watch(movieTableRows, rows => {
  const live = new Set(rows.map(r => r.name))
  const kept = checked.value.filter(n => live.has(n))
  if (kept.length !== checked.value.length) checked.value = kept
})

// Add tags ACROSS the selection — `addTags` is a set operation server-side, so it never wipes tags an
// individual movie already carries. That is the difference between categorising and relabelling.
const tagDialog = ref(false)
const bulkTags = ref('')          // new tags, typed
const pickedExisting = ref<string[]>([])   // tags already in use elsewhere, chosen
// Picking beats retyping: a taxonomy is only useful if the same word lands on every movie, and
// "cohort 1" typed a second time as "Cohort 1" is two categories that look like one.
const tagsInUse = computed(() => movieFilterOptions(movies.value).tags)
const bulkTagsToAdd = computed(() =>
  [...new Set([...pickedExisting.value, ...parseMovieTags(bulkTags.value)])])
function openTagDialog() {
  bulkTags.value = ''
  pickedExisting.value = []
  tagDialog.value = true
}
async function applyBulkTags() {
  const tags = bulkTagsToAdd.value
  tagDialog.value = false
  if (!tags.length) return
  await patchMeta(checked.value, { addTags: tags })
  log.info(`Tagged ${checked.value.length} movie(s)`, { source: 'movies' })
}

async function deleteChecked() {
  if (!projectUid.value || !checked.value.length) return
  const n = checked.value.length
  try {
    const res = await fetch('/api/movies/delete', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectUid.value, names: checked.value }),
    })
    if (!res.ok) throw new Error((await res.json().catch(() => ({}))).error ?? res.statusText)
    const body = await res.json().catch(() => ({})) as { deleted?: string[]; rejected?: string[] }
    log.info(`Deleted ${body.deleted?.length ?? n} movie(s)`, { source: 'movies' })
    if (body.rejected?.length) log.warn(`Could not delete: ${body.rejected.join(', ')}`, { source: 'movies' })
    checked.value = []
    await refresh()
  } catch (e) {
    log.error(`Could not delete ${n} movie(s): ${e instanceof Error ? e.message : String(e)}`, { source: 'movies' })
  }
}

const filterOptions = computed<ChipOption[]>(() => {
  const { tags, producers } = movieFilterOptions(movies.value)
  return [...tags.map(t => ({ value: t, label: t })),
          ...producers.map(p => ({ value: p, label: p, icon: 'pi pi-video' }))]
})
// A picked chip whose tag no longer exists anywhere would hide every row with no visible cause.
watch(filterOptions, opts => {
  const live = new Set(opts.map(o => o.value))
  const kept = pickedTags.value.filter(t => live.has(t))
  if (kept.length !== pickedTags.value.length) pickedTags.value = kept
})
const starredCount = computed(() => allRows.value.filter(r => r.starred).length)
const hiddenCount = computed(() => allRows.value.length - movieTableRows.value.length)
</script>

<template>
  <ModulePage layout="fill">
    <p v-if="!hasProject" class="cc-empty">Open a project to browse its movies.</p>

    <div v-else class="mov-body">
      <!-- Player -->
      <div class="mov-stage">
        <div v-if="currentUrl" ref="viewportEl" class="mov-viewport">
          <video ref="videoEl" class="mov-video" :src="currentUrl" controls
                 :autoplay="settings.moviesAutoplay" :loop="settings.moviesEndMode === 'loop'"
                 :style="videoStyle" @loadedmetadata="onLoadedMeta" @ended="onEnded" />
        </div>
        <!-- Both empty states live in the STAGE, so the panel beside it (and its Refresh) stays
             reachable in a project with no movies yet — it used to replace the whole page. -->
        <p v-else-if="!movies.length && !loading" class="cc-empty">No movies yet — record one from the
          Animation, Batch movies or Viewer panels; they appear here.</p>
        <p v-else class="cc-empty">Select a movie to play.</p>
      </div>

      <!-- The list — folds away and drags wider, like the module pages' functions panel -->
      <CollapsiblePanel storage-key="cc.movies.width" label="movie list" :default-width="520" :max="900">
        <div class="mov-side">
          <!-- Playback options sit WITH the list rather than in a page header: they are what you reach for
               while picking a movie, and a header row holding four of them across an empty page read as
               stray chrome (Dominik, 2026-08-10). -->
          <div class="mov-ctls cc-card">
            <div class="cc-row cc-row-tight">
              <label class="mov-ctl cc-muted" v-tooltip.bottom="'Playback speed'">
                <i class="pi pi-forward" />
                <select v-model.number="settings.moviesPlaybackRate" class="mov-select">
                  <option v-for="s in SPEEDS" :key="s" :value="s">{{ s }}×</option>
                </select>
              </label>
              <label class="mov-ctl mov-zoom cc-muted" v-tooltip.bottom="'Zoom the video (Shift + wheel, Shift +/−, Shift + 0 to reset)'">
                <i class="pi pi-search-plus" />
                <input type="range" :min="MOVIES_ZOOM_MIN" :max="MOVIES_ZOOM_MAX" step="0.25" :value="settings.moviesZoom"
                       @input="onZoomSlider(($event.target as HTMLInputElement).valueAsNumber)" class="mov-range" />
                <span class="mov-num cc-readout">{{ zoomLabel }}</span>
              </label>
            </div>
            <div class="cc-row cc-row-tight">
              <CcToggle class="mov-ctl cc-muted" v-model="settings.moviesAutoplay" label="Autoplay"
                        v-tooltip.bottom="'Play a movie automatically when you select it'" />
              <!-- a <div>, not a <label>: a label has to point at one control, and this is a
                   segmented group of buttons -->
              <div class="mov-ctl cc-muted"
                   v-tooltip.bottom="'At the end: stop, repeat it, or play the next movie in the list'">
                <span class="cc-eyebrow cc-fs-2xs">At end</span>
                <ChipSelect variant="segmented" :options="END_MODES" v-model="settings.moviesEndMode"
                            aria-label="When a movie ends" />
              </div>
              <button class="cc-btn cc-btn-ghost cc-btn-micro mov-refresh" :disabled="loading" @click="refresh"
                      v-tooltip.left="'Re-scan the project movies folder'">
                <i :class="['pi', loading ? 'pi-spin pi-spinner' : 'pi-refresh']" /> Refresh
              </button>
            </div>
          </div>

          <div class="mov-list cc-card">
            <div class="mov-list-head cc-row cc-row-tight">
              <!-- the eyebrow is on the COUNT, not the row: it uppercases, and the toggle beside it is
                   a control with its own label -->
              <span class="cc-eyebrow">{{ movies.length }} movie{{ movies.length === 1 ? '' : 's' }}<template
                v-if="hiddenCount"> · {{ hiddenCount }} hidden</template></span>
              <!-- The source image's channels + attributes beside each movie. The channel picker only
                   appears once the columns are on — it has nothing to switch otherwise. -->
              <ChipSelect v-if="settings.moviesShowDetails" variant="segmented" :options="CHANNEL_MODES"
                          v-model="settings.moviesChannelMode" aria-label="Channel columns"
                          v-tooltip.bottom="'Which channels the columns show'" />
              <CcToggle v-model="settings.moviesShowDetails" label="Details"
                        v-tooltip.bottom="'Show the image channels and attributes'" />
            </div>

            <!-- Bulk actions on the CHECKED movies — the Import page's model: file operations act on the
                 selection, not one row at a time. Only present while something is checked. -->
            <div v-if="checked.length" class="mov-bulk cc-row cc-row-tight">
              <span class="cc-eyebrow cc-fs-2xs">{{ checked.length }} selected</span>
              <button class="cc-btn cc-btn-ghost cc-btn-micro" @click="openTagDialog"
                      v-tooltip.bottom="'Add tags to the selected movies'">
                <i class="pi pi-tag" /> Tag
              </button>
              <!-- arm → confirm, the canonical destructive pattern. The buttons live HERE, not inside
                   ConfirmButton, because a child's DOM can't take this file's scoped styles. -->
              <ConfirmButton @confirm="deleteChecked" v-slot="{ armed, arm, confirm, cancel }">
                <button v-if="!armed" class="cc-btn cc-btn-ghost cc-btn-micro mov-danger" @click="arm"
                        v-tooltip.bottom="'Delete the selected movies'">
                  <i class="pi pi-trash" /> Delete {{ checked.length }}
                </button>
                <template v-else>
                  <button class="cc-btn cc-btn-primary cc-btn-micro" @click="confirm"
                          v-tooltip.bottom="'Permanently delete these files'">
                    <i class="pi pi-check" /> Delete {{ checked.length }}
                  </button>
                  <button class="cc-btn cc-btn-ghost cc-btn-micro" @click="cancel"
                          v-tooltip.bottom="'Keep them'"><i class="pi pi-times" /></button>
                </template>
              </ConfirmButton>
            </div>
            <!-- Star and tags compose with each other and with the column sort — one never replaces
                 another. Both persist, so coming back to compare two movies doesn't mean re-filtering. -->
            <div v-if="starredCount || filterOptions.length || filterAttrKeys.length"
                 class="mov-filters cc-row cc-row-tight">
              <button v-if="starredCount" class="cc-btn cc-btn-ghost cc-btn-micro"
                      :class="{ 'cc-btn-on cc-btn-on-tint': starredOnly }"
                      @click="starredOnly = !starredOnly"
                      v-tooltip.bottom="starredOnly ? 'Show all movies' : 'Show only starred movies'">
                <i :class="starredOnly ? 'pi pi-star-fill' : 'pi pi-star'" /> {{ starredCount }}
              </button>
              <ChipSelect v-if="filterOptions.length" multiple :options="filterOptions" v-model="pickedTags"
                          aria-label="Filter by tag" v-tooltip.bottom="'Filter by tag or by what recorded it'" />
              <!-- the image table's attribute filter, same panel. Opens a dropdown under this row. -->
              <button v-if="filterAttrKeys.length" class="cc-btn cc-btn-ghost cc-btn-micro mov-filter-btn"
                      :class="{ 'cc-btn-on cc-btn-on-tint': attrFilterActive(attrFilter) || filtersOpen }"
                      @click="filtersOpen = !filtersOpen"
                      v-tooltip.left="filtersOpen ? 'Hide filters' : 'Filter by image attribute'">
                <i class="pi pi-filter" /> Filter{{ attrFilterActive(attrFilter) ? ' •' : '' }}
              </button>
            </div>
            <AttrFilterPanel v-if="filtersOpen" class="mov-attr-filter" noun="movies"
                             :rows="allRows" v-model="attrFilter" />
            <!-- Multi-select: the checkbox is the working SET the bulk actions apply to, and the eye is
                 what plays — the same split as the image table, where selection drives the run and the
                 eye drives napari. -->
            <!-- The TABLE scrolls, not the card. With the Details columns on it is wider than the
                 panel, and a card-level `overflow:auto` slid the count, the chip picker and the Details
                 toggle out of view along with it (Dominik, 2026-08-10). -->
            <div class="mov-table-scroll">
            <SelectionTable class="mov-table" selection-mode="multi" :columns="MOVIE_COLUMNS"
                            :rows="shownRows" v-model:selected="checked" id-key="name"
                            v-model:sort="movieSort" column-width-key="cc.movies.colw"
                            fit="content"
                            :row-tooltip="r => `Select ${r.label} — click the eye to play it`">
              <!-- eye · star · the name, editable in place. Renaming from the row is the point: the
                   alternative was selecting each movie and editing a field under the player. -->
              <template #cell-label="{ row }">
                <span class="mov-labelcell">
                  <button class="mov-eye cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
                          :class="{ on: selected === row.name }" @click.stop="selected = row.name"
                          v-tooltip.right="selected === row.name ? 'Playing' : 'Play this movie'">
                    <i class="pi pi-eye" />
                  </button>
                  <button class="mov-star cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" :class="{ on: row.starred }"
                          @click.stop="toggleStar(row)"
                          v-tooltip.right="row.starred ? 'Unstar' : 'Star this movie'">
                    <i :class="row.starred ? 'pi pi-star-fill' : 'pi pi-star'" />
                  </button>
                  <!-- Only for a movie that banked a config. Absent rather than disabled: every movie
                       recorded before the registry has none, and a row of dead buttons in an older
                       project reads as broken rather than as "nothing was saved". -->
                  <button v-if="canEdit(row)" class="mov-edit cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
                          :class="{ stale: row.configStale }" @click.stop="editConfig(row)"
                          v-tooltip.right="editTip(row)">
                    <i class="pi pi-sliders-h" />
                  </button>
                  <input v-if="isRenaming(row.name)" :ref="focusRenameInput" v-model="nameDraft"
                         class="cc-input-2xs mov-cell-edit" v-tooltip.bottom="'Enter to save, Esc to cancel'"
                         @click.stop @keyup.enter="saveRename(row)" @keyup.esc="cancelRename"
                         @blur="saveRename(row)" />
                  <span v-else class="mov-label" :title="row.name"
                        @click.stop="beginRename(row)">{{ row.label }}</span>
                </span>
              </template>

              <!-- The image's channel slots. A blank cell in "in movie" mode is information — that
                   movie does not show this channel — so it reads as an em dash, not as missing data. -->
              <template v-for="i in channelIndices" :key="'ch-' + i" #[`cell-ch:${i}`]="{ row }">
                <span class="mov-dim" :title="channelCells[row.name]?.[i - 1] || ''">{{
                  channelCells[row.name]?.[i - 1] || '—' }}</span>
              </template>

              <!-- Attributes, read-only: the Metadata page is where an attribute is edited, and a movie
                   is one of several rows pointing at the same image. -->
              <template v-for="k in attrKeys" :key="'attr-' + k" #[`cell-attr:${k}`]="{ row }">
                <span class="mov-dim" :title="row.attr?.[k] ? `${k}: ${row.attr[k]}` : ''">{{
                  row.attr?.[k] || '—' }}</span>
              </template>

              <!-- Tags in the row too, for the same reason -->
              <template #cell-tagText="{ row }">
                <!-- The row editor offers the tags already in use, like the bulk panel's ChipSelect
                     does. `separator` scopes the suggestion to the tag being typed, so accepting one
                     does not replace the tags already in the box. This was a native <datalist>, whose
                     popup is browser chrome — ~16px options over a 2xs input; TeleportPopover
                     teleports to <body>, so a table cell does not clip it either. -->
                <SuggestInput v-if="isTagging(row.name)" :ref="focusTagInput"
                       v-model="tagDraft" :options="tagsInUse" separator=","
                       input-class="cc-input-2xs mov-cell-edit" placeholder="tags, comma separated"
                       :tip="'Enter to save, Esc to cancel'"
                       @click.stop @keyup.enter="saveTags(row)" @keyup.esc="cancelTags"
                       @blur="saveTags(row)" />
                <span v-else class="mov-tags" @click.stop="beginTags(row)"
                      v-tooltip.right="'Click to edit tags'">
                  <span v-for="t in row.tags" :key="t" class="mov-tag cc-fs-2xs">{{ t }}</span>
                  <span v-if="!row.tags.length" class="cc-muted cc-fs-xs">+ tag</span>
                </span>
              </template>
            </SelectionTable>
            </div>
          </div>
        </div>
      </CollapsiblePanel>
    </div>

    <!-- Bulk tagging. `addTags` is a set operation server-side, so this ADDS to what each movie already
         carries — categorising a selection must not flatten the tags they differ by. -->
    <BaseModal v-if="tagDialog" title="Tag movies" width="420px" @close="tagDialog = false">
      <p class="cc-muted cc-fs-md">Added to {{ checked.length }} selected movie(s).</p>
      <!-- Pick one already in use, rather than retyping it. A tag typed a second time with different
           capitalisation is a second category that looks like the first. -->
      <ChipSelect v-if="tagsInUse.length" multiple v-model="pickedExisting"
                  :options="tagsInUse.map(t => ({ value: t, label: t }))"
                  aria-label="Existing tags" v-tooltip.bottom="'Tags already used in this project'" />
      <input v-model="bulkTags" class="cc-input-xs mov-bulk-input"
             :placeholder="tagsInUse.length ? 'or a new tag, comma separated' : 'tags, comma separated'"
             v-tooltip.bottom="'Comma separated; a movie keeps the tags it already has'"
             @keyup.enter="applyBulkTags" />
      <template #footer>
        <button class="cc-btn cc-btn-ghost" @click="tagDialog = false">Cancel</button>
        <button class="cc-btn cc-btn-primary" :disabled="!bulkTagsToAdd.length" @click="applyBulkTags">
          <i class="pi pi-tag" /> Add {{ bulkTagsToAdd.length || '' }}
        </button>
      </template>
    </BaseModal>
  </ModulePage>
</template>

<style scoped>
.mov-ctl { display: flex; align-items: center; gap: 0.35rem; }
.mov-select {
  border-radius: var(--cc-radius-sm); padding: 0.15rem 0.35rem;
}
/* The zoom control takes what the speed picker leaves; the slider itself has a fixed intrinsic width
   and won't shrink on its own, so it needs `min-width: 0` to share a narrow panel. */
.mov-zoom { flex: 1; min-width: 0; }
.mov-range { flex: 1; min-width: 3rem; }
.mov-num { min-width: 2.2rem; }   /* + .cc-readout (tabular-nums/colour/size) */
.mov-refresh { margin-left: auto; }

/* The panel's own column: options pinned at the top, the list taking the rest and scrolling itself */
.mov-side { display: flex; flex-direction: column; gap: 0.4rem; flex: 1; min-width: 0; min-height: 0; }
.mov-ctls { flex-shrink: 0; display: flex; flex-direction: column; gap: 0.35rem; padding: 0.4rem 0.5rem; }

.mov-body { display: flex; gap: 1rem; flex: 1; min-height: 0; }

/* Player stage — the video area. The viewport is the scroll container; the video is sized to a
   concrete px box (fit × zoom, see videoStyle). `margin:auto` on a flex child centres it when it fits
   AND lets the viewport scroll to every edge when zoomed (plain justify/align-center would clip the
   top-left and block scrolling there — the well-known flexbox overflow bug). */
.mov-stage { flex: 1; min-width: 0; display: flex; flex-direction: column; gap: 0.5rem; }
.mov-viewport {
  flex: 1; min-height: 0; display: flex; overflow: auto;
  background: #000; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-md);
}
.mov-video { margin: auto; display: block; flex-shrink: 0; }
/* the selected movie's metadata strip — name, what recorded it, tags */
.mov-meta { align-items: baseline; }
.mov-name-edit { flex: 1 1 12rem; max-width: 24rem; }
.mov-tag-edit { flex: 1 1 10rem; max-width: 20rem; }
.mov-tags { display: inline-flex; flex-wrap: wrap; gap: 0.25rem; align-items: center; cursor: text; }
.mov-tag { background: var(--cc-surface-2); border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-pill); padding: 0 0.4rem; color: var(--cc-text-dim); }
/* Star — dim until hovered when unset, so a column of unset stars doesn't compete with the row text.
   Same treatment as ImageTable's per-image star. */
.mov-star { opacity: .25; }
.mov-star:hover { opacity: .7; }
.mov-star.on { opacity: 1; color: var(--cc-warn); }
.mov-filters { padding: 0 0.5rem 0.4rem; }
.mov-filter-btn { margin-left: auto; }
/* the shared panel is sized for a page-wide action bar; inside a side panel it needs the card's own
   padding, and it is not the thing that scrolls */
.mov-attr-filter { flex-shrink: 0; padding: 0.4rem 0.5rem 0.5rem; margin-bottom: 0.35rem; }
/* a Details cell is secondary to the movie's own columns, and ellipsises rather than widening one */
.mov-dim { color: var(--cc-text-dim); overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
/* the name cell: star, then the name, which takes the rest and ellipsises rather than widening */
.mov-labelcell { display: flex; align-items: center; gap: 0.2rem; min-width: 0; }
/* the name fills what the two icons leave, ellipsising rather than widening the column. `cursor:text`
   because clicking it starts an edit. */
.mov-label { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
  cursor: text; }
.mov-cell-edit { flex: 1; min-width: 0; }
/* Eye — which movie is in the player. Dim until hovered when off, like the star beside it, so a
   column of them doesn't compete with the names. */
.mov-eye { opacity: .25; }
.mov-eye:hover { opacity: .7; }
.mov-eye.on { opacity: 1; color: var(--cc-accent); }
/* Edit the saved config — same dim-until-hovered treatment as the two beside it. `stale` tints it when
   the file was re-recorded after the config was banked, so the tooltip's caveat has something visible
   to belong to. */
.mov-edit { opacity: .25; }
.mov-edit:hover { opacity: .7; }
.mov-edit.stale { opacity: .6; color: var(--cc-warn); }
.mov-bulk { padding: 0 0.5rem 0.4rem; }
/* the chip row and the field are two ways to say the same thing, so they need a gap between them —
   flush, they read as one control */
.mov-bulk-input { width: 100%; margin-top: 0.6rem; }
.mov-danger:hover:not(:disabled) { color: var(--cc-danger); border-color: var(--cc-danger); }

/* The list — width/collapse are CollapsiblePanel's; this is just the card inside it */
/* The card is the COLUMN — head, bulk actions and filters pinned, the table taking the rest and
   scrolling itself in both axes. It used to scroll as a whole, which was invisible until a table grew
   wider than the panel. */
.mov-list { flex: 1; min-width: 0; min-height: 0; overflow: hidden; padding: 0.35rem;
  display: flex; flex-direction: column; }   /* + .cc-card (surface/border/radius) */
.mov-table-scroll { flex: 1; min-height: 0; min-width: 0; overflow: auto; }
.mov-list-head { padding: 0.35rem 0.5rem 0.5rem; flex-shrink: 0; }   /* + .cc-row (flex/wrap/gap) */
.mov-list-head > :first-child { margin-right: auto; }
.mov-bulk, .mov-filters { flex-shrink: 0; }
/* the table brings its own sized-column layout (column-width-key); drag a header edge to widen one */
.mov-table { min-width: 100%; }

</style>
