import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref, watch } from 'vue'
import { TITLE_CARD_DEFAULT, type TitleCardCfg, type BatchMovieCfg } from '../utils/batchMovie'
import { COMPARE_LAYOUT_DEFAULT, COMPARE_CONTRAST_DEFAULT,
         type CompareLayout, type CompareContrast } from '../utils/movieCompare'
import { parseMovieEndMode, type MovieChannelMode, type MovieEndMode } from '../utils/movies'

export const useSettingsStore = defineStore('settings', () => {
  const taskListAutoFollow = ref(
    localStorage.getItem('cc.taskListAutoFollow') !== 'false'  // default true
  )

  // Whether the Task Manager lists only the open project's tasks. ON by default: the store is not
  // cleared when a project is opened (a run keeps reporting into the tab that launched it), so after
  // a switch the manager was showing the previous project's rows with nothing to distinguish them —
  // every other view of the same store already scopes itself (`forModule(module, projectUid)`).
  // A setting rather than a bare ref because the cross-project view is a real one — a project
  // export/import runs against a project that is NOT the open one — so it stays one click away.
  const tasksThisProjectOnly = ref(
    localStorage.getItem('cc.tasksThisProjectOnly') !== 'false'  // default true
  )

  // Whether the Task Manager also lists the project's DURABLE run history (each image's run log), not
  // just the runs this browser session watched. ON by default, because off is what the manager used to
  // be and it was reported as the list looking empty: a window opened after the work finished — a
  // reload, or the pop-out task window — had nothing to show for a project with hundreds of runs in it.
  // A toggle rather than a rule because the two questions are different ("what is happening" vs "what
  // has been done"), and the session view is the one you want while you are running things.
  const tasksShowHistory = ref(
    localStorage.getItem('cc.tasksShowHistory') !== 'false'  // default true
  )

  // Auto-refresh plots + pop lists when a task finishes successfully (the per-image task-refresh; see
  // composables/useDataRefresh). On by default; users who find plots refetching under them distracting
  // can turn it off (they then refresh on the next navigation / input change).
  const autoRefreshOnTask = ref(
    localStorage.getItem('cc.autoRefreshOnTask') !== 'false'   // default true
  )

  const napariUpdateImage = ref(
    localStorage.getItem('cc.napariUpdateImage') === 'true'    // default false
  )

  // Animation page: selecting a keyframe pushes its saved view into napari, so you SEE the snapshot
  // (and can tweak it there and Update). Off by default — it drives the shared viewer, which is not
  // what someone only reordering a timeline wants.
  const animationSyncNapari = ref(
    localStorage.getItem('cc.animationSyncNapari') === 'true'  // default false
  )

  // Clean capture (E1): hide napari's baked scale bar + timestamp when taking a screenshot, for a clean
  // publication still (add a vector scale bar / timestamp externally). Applies to strip + animation
  // captures. Default false (keep the on-screen annotations). See docs/todo/ANIMATION_PLAN.md → E.
  const cleanCapture = ref(
    localStorage.getItem('cc.cleanCapture') === 'true'         // default false
  )

  // Reload behaviour: reloading a shown image (the eye / a finished task) refreshes DATA only
  // (labels + population/track overlays, re-read from disk) — NOT the image pyramid. Tick "reset" to
  // reopen the image too (needed when a task changed the pixels: drift/denoise). Default false.
  const napariResetOnReload = ref(
    localStorage.getItem('cc.napariResetOnReload') === 'true'  // default false
  )

  // Dask opportunistic cache for label layers (napari's `resize_dask_cache`). Default ON — the
  // common case is browsing, where cache-on gives noticeably smoother slice scrubbing. The
  // dangerous case is running segmentation to the same output name: `da.from_zarr(same_path)`
  // produces the same dask task name across opens, so cached results serve STALE bytes after
  // a re-run (see `python/cecelia/utils/napari_utils.add_labels` docstring). The Viewer panel
  // surfaces a hint recommending the user flip it off when a segment task is running.
  const napariLabelsCache = ref(
    localStorage.getItem('cc.napariLabelsCache') !== 'false'  // default true
  )

  // Contrast/colormap/T-Z, autosaved per image the moment they change and reloaded on open. Default ON
  // (docs/todo/MOVIE_MANAGEMENT_PLAN.md Decision 8): contrast is deliberately image state rather than a
  // copy inside every movie config, and the movie path force-loads it (`autoLoadProps = true` in
  // `_apply_movie_config!`). With this off nothing was ever written, so that load found no file and
  // napari auto-contrasted per image — a recorded look was not reproducible.
  const napariAutoSaveLayerProps = ref(
    localStorage.getItem('cc.napariAutoSaveLayerProps') !== 'false'  // default true
  )

  const napariAsDask = ref(
    localStorage.getItem('cc.napariAsDask') !== 'false'  // default true
  )

  // Render napari on the discrete GPU (hybrid-graphics machines, Linux only). Persisted here and
  // re-asserted to the backend each session (App.vue) so the bridge launches on the right GPU; the
  // backend holds the authoritative launch-time flag. Effective on the next bridge (re)start.
  const napariDiscreteGpu = ref(
    localStorage.getItem('cc.napariDiscreteGpu') === 'true'  // default false
  )

  // Movie player (/movies) viewing prefs — playback speed, zoom, autoplay-on-select, end mode. Persisted
  // globally (not per-set): they're a viewing preference, not a project attribute, and the player is a
  // project-agnostic page.
  const moviesPlaybackRate = ref(Number(localStorage.getItem('cc.moviesPlaybackRate') ?? '1') || 1)
  const moviesZoom = ref(Number(localStorage.getItem('cc.moviesZoom') ?? '1') || 1)
  const moviesAutoplay = ref(localStorage.getItem('cc.moviesAutoplay') !== 'false')   // default true
  // What happens when a movie ENDS: stop (default), repeat it, or play the next one in the shown
  // list. ONE setting rather than a Loop toggle plus an Advance toggle — the outcomes are mutually
  // exclusive, so two booleans could ask for both. Migrates the `cc.moviesLoop` boolean it replaced.
  const moviesEndMode = ref<MovieEndMode>(
    parseMovieEndMode(localStorage.getItem('cc.moviesEndMode'), localStorage.getItem('cc.moviesLoop')))
  // The movie list's Details columns — the source image's channels and attributes beside each movie.
  // Off by default: they only mean something once a project has attributes, and the list lives in a
  // side panel where every extra column costs width. `moviesChannelMode` picks which channels fill
  // them ('image' = the image's own, 'movie' = only the ones that movie shows).
  const moviesShowDetails = ref(localStorage.getItem('cc.moviesShowDetails') === 'true')
  const moviesChannelMode = ref<MovieChannelMode>(
    localStorage.getItem('cc.moviesChannelMode') === 'movie' ? 'movie' : 'image')

  // ── Layout: collapse the main nav sidebar (left) and the module function/tasks panel (right)
  // to free up working space. Both default expanded, both persist across sessions.
  const sidebarCollapsed = ref(localStorage.getItem('cc.sidebarCollapsed') === 'true')
  const rightPanelCollapsed = ref(localStorage.getItem('cc.rightPanelCollapsed') === 'true')
  // the napari Viewer controls are a floating dockable panel (not a sidebar section) — this is its
  // open/closed state, toggled from the sidebar "Viewer" button. Off by default (opt-in, no intrusion).
  const viewerPanelOpen = ref(localStorage.getItem('cc.viewerPanelOpen') === 'true')
  // the lab log is a floating dockable panel too (open/closed state, toggled from the sidebar).
  // Off by default (opt-in). See components/LabLogPanel.vue, docs/ai-assist/LAB-LOG.md.
  const labLogPanelOpen = ref(localStorage.getItem('cc.labLogPanelOpen') === 'true')
  // Account-managed MCP connectors the user hid in Settings → MCP connections (by name). Machine-wide
  // and permanent, not per project: these are claude.ai account connectors we cannot detect, and
  // plenty of institutes have none of them (LabArchives is site-hosted), so an undismissable row
  // would nag forever with nothing to act on. Generic by name — more connectors are coming.
  const hiddenMcpAccounts = ref<string[]>(
    JSON.parse(localStorage.getItem('cc.hiddenMcpAccounts') || '[]') as string[])
  // auto-capture app activity digests ([Cecelia] entries) — on project open AND after tasks/chains
  // finish (stores/labCapture.ts). One toggle for all automatic capture; off ⇒ only the manual
  // "Capture" button fires. Default ON — Cecelia is the always-on activity reporter (local-only, no
  // tokens; routine ✅ digests append silently, only ⚠️/❌ badge). Turn off to silence auto-capture.
  const labLogAutoContext = ref(localStorage.getItem('cc.labLogAutoContext') !== 'false')
  // lab-log: show image NAMES instead of the stored UIDs. The log stores stable image UIDs (names
  // change; UIDs don't), so this is a display-only swap resolved against live project data. Default
  // false (show the compact, stable UIDs). See components/LabLogPanel.vue.
  const labLogShowNames = ref(localStorage.getItem('cc.labLogShowNames') === 'true')
  // which model "Ask Claude" spawns (Claude CLI --model alias). Default Sonnet — Opus is overkill for
  // the observer's work; Haiku is the cheap option. Sent per feedback call; the backend allow-lists it.
  // See app/src/ai/agent_runner.jl OBSERVER_MODELS.
  const labLogObserverModel = ref(localStorage.getItem('cc.labLogObserverModel') || 'sonnet')
  // Tip of the day (WHATS_NEW_PLAN.md → W4). On app launch, if these say "show + last shown was
  // not today", the What's New modal opens with today's tip prepended. Opt-out from a checkbox on
  // the tip card. Default ON — biologists opening the app benefit from a nudge; power users can
  // switch it off from the card and never see one again.
  // Which view profile curates the sidebar — the id of a <config_dir>/profiles/<id>.json, or '' for
  // the implicit "All" (the full menu, today's behaviour and always the fallback). PER USER, not per
  // project: a profile is about who is driving, so it must not travel with a shared project. The
  // definitions live in files (stores/viewProfiles.ts); only the choice lives here.
  // See docs/todo/VIEW_PROFILES_PLAN.md.
  const viewProfile = ref(localStorage.getItem('cc.viewProfile') ?? '')
  const tipsOnLaunch = ref(localStorage.getItem('cc.tipsOnLaunch') !== 'false')  // default true
  const tipsLastShown = ref(localStorage.getItem('cc.tipsLastShown') ?? '')      // YYYY-MM-DD

  // transient (not persisted): a one-line preview of an unseen lab-log addition — set when Claude
  // (observer) or Cecelia (auto-digest) appends while the panel is closed; drives the sidebar badge,
  // cleared when opened. `kind` picks the badge icon (Claude sparkles vs Cecelia bell); `level` its
  // colour (Cecelia digests badge only on ⚠️/❌). See docs/todo/QC_OBSERVER_PLAN.md.
  const labLogUnseen = ref('')
  const labLogUnseenKind = ref<'' | 'claude' | 'cecelia'>('')
  const labLogUnseenLevel = ref<'' | 'warn' | 'fail'>('')

  // per-image label-layer visibility: { [imageUid]: { [valueName]: boolean } }
  // unknown labels default to true; persisted across sessions
  const _labelVisStore = ref<Record<string, Record<string, boolean>>>(
    JSON.parse(localStorage.getItem('cc.napariLabelVisibility') ?? '{}')
  )
  function getLabelVisibility(imageUid: string, labelNames: string[]): Record<string, boolean> {
    const stored = _labelVisStore.value[imageUid] ?? {}
    const out: Record<string, boolean> = {}
    for (const vn of labelNames) out[vn] = stored[vn] ?? true   // default visible
    return out
  }
  function setLabelVisibility(imageUid: string, vis: Record<string, boolean>) {
    _labelVisStore.value = { ..._labelVisStore.value, [imageUid]: { ...vis } }
    localStorage.setItem('cc.napariLabelVisibility', JSON.stringify(_labelVisStore.value))
  }

  // per-image track-overlay visibility: { [imageUid]: { [valueName]: boolean } } — which
  // segmentations have their tracks shown in napari. Default OFF (tracks are a heavier overlay).
  const _trackVisStore = ref<Record<string, Record<string, boolean>>>(
    JSON.parse(localStorage.getItem('cc.napariTrackVisibility') ?? '{}')
  )
  function getTrackVisibility(imageUid: string, valueNames: string[]): Record<string, boolean> {
    const stored = _trackVisStore.value[imageUid] ?? {}
    const out: Record<string, boolean> = {}
    for (const vn of valueNames) out[vn] = stored[vn] ?? false   // default hidden
    return out
  }
  function setTrackVisibility(imageUid: string, vis: Record<string, boolean>) {
    _trackVisStore.value = { ..._trackVisStore.value, [imageUid]: { ...vis } }
    localStorage.setItem('cc.napariTrackVisibility', JSON.stringify(_trackVisStore.value))
  }

  // per-image branch-overlay visibility (skeleton labels from segment.branching). Default ON —
  // if a user has branch labels registered, they almost always want to see them (running the
  // task is the opt-in; hiding the layer is the exception, not the rule — mirrors cell labels).
  const _branchVisStore = ref<Record<string, Record<string, boolean>>>(
    JSON.parse(localStorage.getItem('cc.napariBranchVisibility') ?? '{}')
  )
  function getBranchVisibility(imageUid: string, valueNames: string[]): Record<string, boolean> {
    const stored = _branchVisStore.value[imageUid] ?? {}
    const out: Record<string, boolean> = {}
    for (const vn of valueNames) out[vn] = stored[vn] ?? true    // default visible
    return out
  }
  function setBranchVisibility(imageUid: string, vis: Record<string, boolean>) {
    _branchVisStore.value = { ..._branchVisStore.value, [imageUid]: { ...vis } }
    localStorage.setItem('cc.napariBranchVisibility', JSON.stringify(_branchVisStore.value))
  }

  // ── Per-SET napari viewer preferences, keyed by set uid: { [setUid]: {...} } ──────────────────
  // These are the viewer-level DISPLAY toggles (colour-by, show-3D, point size, per-popType overlay
  // visibility, show-gated-tracks). They were always MEANT to be per-set (one experiment = consistent
  // viewing); the old R app made them global only because Shiny bookmarks made that easy. Per-set (not
  // global) so a choice made in one experiment never bleeds onto another's images (e.g. a colour-by
  // column that a different set's segmentation doesn't have), and not per-image so you set it ONCE and
  // it holds as you click through the set's images. Per-image state (which segmentations/tracks are
  // shown — the per-segmentation rows) stays keyed by image uid above.
  interface NapariSetPrefs {
    colourBy?: string                       // obs column to colour labels/tracks by ('' = default)
    show3D?: boolean                        // open images volumetric (only applied where a z-axis exists)
    showGatedTracks?: boolean               // overlay gated track populations
    pointSize?: number                      // population centroid point size in napari
    popVis?: Record<string, boolean>        // per-popType point-overlay visibility (flow/clust/track/trackclust)
    // user recolouring of a categorical colour-by, keyed by column then category value → hex. For
    // categories with no population (HMM states, raw clusters) there's no colour defined anywhere, so
    // the user can override the default palette; these win over pop/default when colouring. Per-column
    // so different colour-by columns keep independent schemes.
    colourByOverrides?: Record<string, Record<string, string>>
    // timelapse-recording params (extensible — F1.2 adds channels/pops/T-range here). fps = frame rate;
    // sizeX/sizeY = output pixels, absent/null = the napari canvas size. Per-set like the other viewer
    // prefs. A `scale` supersample lived here and was removed (see MovieOutputControls.vue); an older
    // prefs file may still carry the key, it is simply unread, and the name is deliberately NOT reused —
    // a stale 1-3 multiplier must never be read as a pixel width.
    // `suffix` is a filename addition (a movie is named after the IMAGE, so the corrected version and
    // the raw import would collide). null = never set, so the UI's version-derived default applies;
    // '' = deliberately cleared, which must survive a reload.
    // `compareVersions` are the image versions the recorder shows side by side, in COLUMN order (2+;
    // [] or one = an ordinary single-version movie), with the layout + contrast mode that go with them.
    // `showTimestamp`/`showScaleBar` are napari's BAKED overlays — burnt into every recorded frame, so
    // leaving them out is a record-time decision, not something the movie can be edited to undo.
    // `compareSegmentations` is the same idea for the segmentation masks. The two together give the
    // layout with nothing to store: versions across, masks down (see `compareShape`).
    movie?: { fps?: number; sizeX?: number | null; sizeY?: number | null; suffix?: string | null
              titleCard?: TitleCardCfg; compareVersions?: string[]; compareSegmentations?: string[]
              labelContour?: number; zSlice?: number | null
              // Which stretch of the timelapse to record, as FRAME INDICES — `tEnd` null/absent = the
              // last frame, which is what every recording did before the control existed. Deliberately
              // NOT the `cropT` pair above: that one is a 0-100 % crop range for making a new image.
              tStart?: number; tEnd?: number | null
              // 3D multiscale detail: level index (0 = full resolution, higher = coarser), or null for
              // napari's own choice (its coarsest level). Per set, like the other viewer prefs.
              detail3d?: number | null
              compareLayout?: CompareLayout; compareContrast?: CompareContrast
              showTimestamp?: boolean; showScaleBar?: boolean }
    // 3D-crop z-range and t-range as 0–100 % (per set — the XY crop box itself is per-session, drawn in
    // napari each time since a region is image-specific). Only the ranges persist, like other prefs.
    cropZ?: { lo?: number; hi?: number }
    cropT?: { lo?: number; hi?: number }
    // batch-movie authoring config (F1.3 "make a movie for all images"): one config applied across the
    // selected images. `channels` = {channelName → colormap} for channels to SHOW (rest hidden). Per-set.
    //
    // THE type, imported — not a copy of it. This used to restate all twenty fields, in a file that
    // already imports from `utils/batchMovie`; the two then drifted, and the copy grew three fields
    // nothing read. What a movie config IS belongs beside the builder that consumes it.
    batchMovie?: BatchMovieCfg
  }
  const _setPrefs = ref<Record<string, NapariSetPrefs>>(
    JSON.parse(localStorage.getItem('cc.napariSetPrefs') ?? '{}')
  )
  function _patchSet(setUid: string, patch: Partial<NapariSetPrefs>) {
    _setPrefs.value = { ..._setPrefs.value, [setUid]: { ...(_setPrefs.value[setUid] ?? {}), ...patch } }
    localStorage.setItem('cc.napariSetPrefs', JSON.stringify(_setPrefs.value))
  }
  const getColourBy = (setUid: string): string => _setPrefs.value[setUid]?.colourBy ?? ''
  const setColourBy = (setUid: string, column: string) => _patchSet(setUid, { colourBy: column })
  const getShow3D = (setUid: string): boolean => _setPrefs.value[setUid]?.show3D ?? false
  const setShow3D = (setUid: string, v: boolean) => _patchSet(setUid, { show3D: v })
  const getShowGatedTracks = (setUid: string): boolean => _setPrefs.value[setUid]?.showGatedTracks ?? false
  const setShowGatedTracks = (setUid: string, v: boolean) => _patchSet(setUid, { showGatedTracks: v })
  const getPointSize = (setUid: string): number => _setPrefs.value[setUid]?.pointSize ?? 6   // old GUI default 6
  const setPointSize = (setUid: string, v: number) => _patchSet(setUid, { pointSize: v })
  const getPopVisible = (setUid: string, popType: string): boolean =>
    _setPrefs.value[setUid]?.popVis?.[popType] ?? false                                       // default hidden
  function setPopVisible(setUid: string, popType: string, v: boolean) {
    _patchSet(setUid, { popVis: { ...(_setPrefs.value[setUid]?.popVis ?? {}), [popType]: v } })
  }
  // user colour-by recolouring, per column: {value → hex}
  const getColourOverrides = (setUid: string, column: string): Record<string, string> =>
    _setPrefs.value[setUid]?.colourByOverrides?.[column] ?? {}
  function setColourOverride(setUid: string, column: string, value: string, hex: string) {
    const all = _setPrefs.value[setUid]?.colourByOverrides ?? {}
    _patchSet(setUid, { colourByOverrides: { ...all, [column]: { ...(all[column] ?? {}), [value]: hex } } })
  }
  function clearColourOverrides(setUid: string, column: string) {
    const all = { ...(_setPrefs.value[setUid]?.colourByOverrides ?? {}) }
    delete all[column]
    _patchSet(setUid, { colourByOverrides: all })
  }
  // timelapse-recording params (per set); defaults match the backend (fps 15, size = canvas)
  const getMovieConfig = (setUid: string): {
    fps: number; sizeX: number | null; sizeY: number | null; suffix: string | null; titleCard: TitleCardCfg
    compareVersions: string[]; compareSegmentations: string[]; labelContour: number
    zSlice: number | null
    detail3d: number | null
    compareLayout: CompareLayout; compareContrast: CompareContrast
    showTimestamp: boolean; showScaleBar: boolean
    tStart: number; tEnd: number | null
  } => ({
    fps: _setPrefs.value[setUid]?.movie?.fps ?? 15,
    sizeX: _setPrefs.value[setUid]?.movie?.sizeX ?? null,
    sizeY: _setPrefs.value[setUid]?.movie?.sizeY ?? null,
    suffix: _setPrefs.value[setUid]?.movie?.suffix ?? null,
    titleCard: _setPrefs.value[setUid]?.movie?.titleCard ?? { ...TITLE_CARD_DEFAULT },
    // side-by-side version comparison (docs/todo/MOVIE_COMPARE_PLAN.md); [] = record the active version
    compareVersions: _setPrefs.value[setUid]?.movie?.compareVersions ?? [],
    // …and the masks; [] = draw whatever the viewer already shows
    compareSegmentations: _setPrefs.value[setUid]?.movie?.compareSegmentations ?? [],
    // 0 = filled masks, which is what every movie drew before the control existed
    labelContour: _setPrefs.value[setUid]?.movie?.labelContour ?? 0,
    // null = record whatever slice is showing (what every recording did before the setting existed).
    // The 3D half is the EXISTING per-set `show3D` pref — one stored value, so the viewer's 3D button
    // and the movie's z control cannot disagree.
    zSlice: _setPrefs.value[setUid]?.movie?.zSlice ?? null,
    // 0 = full resolution. napari's own 3D choice is the COARSEST level, which erases a strided label
    // pyramid — so the default is full and the cost is a visible control (docs/NAPARI.md → 3D detail).
    detail3d: _setPrefs.value[setUid]?.movie?.detail3d ?? 0,
    compareLayout: _setPrefs.value[setUid]?.movie?.compareLayout ?? COMPARE_LAYOUT_DEFAULT,
    compareContrast: _setPrefs.value[setUid]?.movie?.compareContrast ?? COMPARE_CONTRAST_DEFAULT,
    // default ON — what every movie was before the toggles existed
    showTimestamp: _setPrefs.value[setUid]?.movie?.showTimestamp ?? true,
    showScaleBar: _setPrefs.value[setUid]?.movie?.showScaleBar ?? true,
    // the whole timelapse — 0 to the last frame, `null` meaning "however long this image is"
    tStart: _setPrefs.value[setUid]?.movie?.tStart ?? 0,
    tEnd: _setPrefs.value[setUid]?.movie?.tEnd ?? null,
  })
  function setMovieConfig(setUid: string,
                          patch: { fps?: number; sizeX?: number | null; sizeY?: number | null;
                                   suffix?: string | null; titleCard?: TitleCardCfg
                                   compareVersions?: string[]; compareSegmentations?: string[]
                                   labelContour?: number; zSlice?: number | null
                                   detail3d?: number | null
                                   compareLayout?: CompareLayout
                                   compareContrast?: CompareContrast
                                   showTimestamp?: boolean; showScaleBar?: boolean
                                   tStart?: number; tEnd?: number | null }) {
    _patchSet(setUid, { movie: { ...(_setPrefs.value[setUid]?.movie ?? {}), ...patch } })
  }
  // 3D-crop z-range (per set) as 0–100 %; default full depth (0–100)
  const getCropZ = (setUid: string): { lo: number; hi: number } => ({
    lo: _setPrefs.value[setUid]?.cropZ?.lo ?? 0,
    hi: _setPrefs.value[setUid]?.cropZ?.hi ?? 100,
  })
  function setCropZ(setUid: string, patch: { lo?: number; hi?: number }) {
    _patchSet(setUid, { cropZ: { ...(_setPrefs.value[setUid]?.cropZ ?? {}), ...patch } })
  }
  const getCropT = (setUid: string): { lo: number; hi: number } => ({
    lo: _setPrefs.value[setUid]?.cropT?.lo ?? 0,
    hi: _setPrefs.value[setUid]?.cropT?.hi ?? 100,
  })
  function setCropT(setUid: string, patch: { lo?: number; hi?: number }) {
    _patchSet(setUid, { cropT: { ...(_setPrefs.value[setUid]?.cropT ?? {}), ...patch } })
  }
  // batch-movie authoring config (per set); the reactive bag the BatchMovies page drives via useViewState
  const getBatchMovieConfig = (setUid: string): BatchMovieCfg => _setPrefs.value[setUid]?.batchMovie ?? {}
  function setBatchMovieConfig(setUid: string, patch: Partial<BatchMovieCfg>) {
    _patchSet(setUid, { batchMovie: { ...(_setPrefs.value[setUid]?.batchMovie ?? {}), ...patch } })
  }
  // REPLACE the whole bag, for the two operations that are about the config as a WHOLE rather than one
  // of its fields: loading a movie's saved config into the page, and undoing that. A merge cannot do
  // either — it has no way to remove a key, so undoing a restore would leave behind every option the
  // restored config set and the previous one did not (docs/todo/MOVIE_MANAGEMENT_PLAN.md Phase 6).
  function replaceBatchMovieConfig(setUid: string, cfg: BatchMovieCfg) {
    _patchSet(setUid, { batchMovie: { ...cfg } })
  }

  watch(taskListAutoFollow,       v => localStorage.setItem('cc.taskListAutoFollow',       String(v)))
  watch(tasksThisProjectOnly,     v => localStorage.setItem('cc.tasksThisProjectOnly',     String(v)))
  watch(tasksShowHistory,         v => localStorage.setItem('cc.tasksShowHistory',         String(v)))
  watch(autoRefreshOnTask,        v => localStorage.setItem('cc.autoRefreshOnTask',        String(v)))
  watch(napariUpdateImage,        v => localStorage.setItem('cc.napariUpdateImage',        String(v)))
  watch(animationSyncNapari,      v => localStorage.setItem('cc.animationSyncNapari',      String(v)))
  watch(cleanCapture,             v => localStorage.setItem('cc.cleanCapture',             String(v)))
  watch(napariResetOnReload,      v => localStorage.setItem('cc.napariResetOnReload',      String(v)))
  watch(napariLabelsCache,        v => localStorage.setItem('cc.napariLabelsCache',        String(v)))
  watch(napariAutoSaveLayerProps, v => localStorage.setItem('cc.napariAutoSaveLayerProps', String(v)))
  watch(napariAsDask,             v => localStorage.setItem('cc.napariAsDask',             String(v)))
  watch(napariDiscreteGpu,        v => localStorage.setItem('cc.napariDiscreteGpu',        String(v)))
  watch(moviesPlaybackRate,       v => localStorage.setItem('cc.moviesPlaybackRate',       String(v)))
  watch(moviesZoom,               v => localStorage.setItem('cc.moviesZoom',               String(v)))
  watch(moviesAutoplay,           v => localStorage.setItem('cc.moviesAutoplay',           String(v)))
  watch(moviesEndMode,            v => localStorage.setItem('cc.moviesEndMode',            v))
  watch(moviesShowDetails,        v => localStorage.setItem('cc.moviesShowDetails',        String(v)))
  watch(moviesChannelMode,        v => localStorage.setItem('cc.moviesChannelMode',        String(v)))
  watch(sidebarCollapsed,         v => localStorage.setItem('cc.sidebarCollapsed',         String(v)))
  watch(rightPanelCollapsed,      v => localStorage.setItem('cc.rightPanelCollapsed',      String(v)))
  watch(viewerPanelOpen,          v => localStorage.setItem('cc.viewerPanelOpen',          String(v)))
  watch(labLogPanelOpen,          v => localStorage.setItem('cc.labLogPanelOpen',          String(v)))
  watch(labLogAutoContext,        v => localStorage.setItem('cc.labLogAutoContext',        String(v)))
  watch(hiddenMcpAccounts, v => localStorage.setItem('cc.hiddenMcpAccounts', JSON.stringify(v)), { deep: true })
  watch(labLogShowNames,          v => localStorage.setItem('cc.labLogShowNames',          String(v)))
  watch(labLogObserverModel,      v => localStorage.setItem('cc.labLogObserverModel',      v))
  watch(viewProfile,              v => localStorage.setItem('cc.viewProfile',              v))
  watch(tipsOnLaunch,             v => localStorage.setItem('cc.tipsOnLaunch',             String(v)))
  watch(tipsLastShown,            v => localStorage.setItem('cc.tipsLastShown',            v))
  watch(labLogPanelOpen, open => { if (open) {          // opening clears the badge (all facets)
    labLogUnseen.value = ''; labLogUnseenKind.value = ''; labLogUnseenLevel.value = ''
  } })

  return { viewProfile, taskListAutoFollow, tasksThisProjectOnly, tasksShowHistory, autoRefreshOnTask, napariUpdateImage, animationSyncNapari, cleanCapture, napariResetOnReload, napariLabelsCache, napariAutoSaveLayerProps, napariAsDask, napariDiscreteGpu, moviesPlaybackRate, moviesZoom, moviesAutoplay, moviesEndMode, moviesShowDetails, moviesChannelMode, sidebarCollapsed, rightPanelCollapsed, viewerPanelOpen, labLogPanelOpen, hiddenMcpAccounts, labLogAutoContext, labLogShowNames, labLogObserverModel, labLogUnseen, labLogUnseenKind, labLogUnseenLevel, tipsOnLaunch, tipsLastShown, getLabelVisibility, setLabelVisibility, getTrackVisibility, setTrackVisibility, getBranchVisibility, setBranchVisibility, getColourBy, setColourBy, getShow3D, setShow3D, getShowGatedTracks, setShowGatedTracks, getPointSize, setPointSize, getPopVisible, setPopVisible, getColourOverrides, setColourOverride, clearColourOverrides, getMovieConfig, setMovieConfig, getCropZ, setCropZ, getCropT, setCropT, getBatchMovieConfig, setBatchMovieConfig, replaceBatchMovieConfig }
})

// Replace the live instance on hot-reload — see the note in `stores/customModules.ts`.
if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useSettingsStore, import.meta.hot))
