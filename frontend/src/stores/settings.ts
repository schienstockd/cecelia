import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref, watch } from 'vue'
import { TITLE_CARD_DEFAULT, type TitleCardCfg, type BatchMovieCfg } from '../utils/batchMovie'
import { COMPARE_LAYOUT_DEFAULT, COMPARE_CONTRAST_DEFAULT,
         type CompareLayout, type CompareContrast } from '../utils/movieCompare'
import { parseMovieEndMode, type MovieChannelMode, type MovieEndMode } from '../utils/movies'
import { decodeViewerBagEvent } from '../utils/viewerBagChannel'

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
  // just the runs this browser session watched. The two are different questions — "what is happening"
  // vs "what has been done" — and the session view is the one you want while you are running things,
  // so this is OFF by default and the manager keeps the meaning it has always had. Turn it on when the
  // list looks empty for a reason it shouldn't: a window opened after the work finished (a reload, or
  // the pop-out task window) has no session to show for a project with hundreds of runs in it.
  const tasksShowHistory = ref(
    localStorage.getItem('cc.tasksShowHistory') === 'true'   // default false
  )

  // Auto-refresh plots + pop lists when a task finishes successfully (the per-image task-refresh; see
  // composables/useDataRefresh). On by default; users who find plots refetching under them distracting
  // can turn it off (they then refresh on the next navigation / input change).
  const autoRefreshOnTask = ref(
    localStorage.getItem('cc.autoRefreshOnTask') !== 'false'   // default true
  )

  // Auto-refresh the viewer when a task finishes for the open image (labels + overlays re-read
  // from disk). Off by default — heavy on large images (Dominik has 20k+ frame timelapses).
  const viewerAutoUpdate = ref(
    localStorage.getItem('cc.viewerAutoUpdate') === 'true'    // default false
  )

  // Animation page: selecting a keyframe pushes its saved view into the viewer, so you SEE the snapshot
  // (and can tweak it there and Update). Off by default — it drives the shared viewer, which is not
  // what someone only reordering a timeline wants.
  const animationSyncViewer = ref(
    localStorage.getItem('cc.animationSyncViewer') === 'true'  // default false
  )

  // Reload behaviour: reloading a shown image (the eye / a finished task) refreshes DATA only
  // (labels + population/track overlays, re-read from disk) — NOT the image pyramid. Tick "reset" to
  // reopen the image too (needed when a task changed the pixels: drift/denoise). Default false.
  const viewerResetOnReload = ref(
    localStorage.getItem('cc.viewerResetOnReload') === 'true'  // default false
  )

  // Contrast/colormap/T-Z, autosaved per image the moment they change and reloaded on open. Default ON
  // (docs/todo/MOVIE_MANAGEMENT_PLAN.md Decision 8): contrast is deliberately image state rather than a
  // copy inside every movie config, and the movie path force-loads it (`autoLoadProps = true` in
  // `_apply_movie_config!`). With this off nothing was ever written, so that load found no file and
  // the viewer auto-contrasted per image — a recorded look was not reproducible.
  //
  // Autosave the browser viewer's per-image layer props (contrast, colormap, T/Z, ndisplay) as the
  // user changes them, so the animation page — which banks per-keyframe view state on top of these —
  // has a durable reference. Writes go through `/api/viewer/props` (PY).
  const viewerAutoSaveLayerProps = ref(localStorage.getItem('cc.viewerAutoSaveLayerProps') !== 'false')  // default true

  // Browser volume viewer (/viewer-window) — the two options that are the WINDOW's, not the image's.
  // Per-channel contrast deliberately is not here: the server answers it (the viewer's saved props, or a
  // percentile sample), so a local copy would be a second source of truth. See ViewerWindow.vue.
  // `viewerSteps` is ray steps per pixel — 256 measured 5.3 ms/frame on real data at 1566x1003.
  // `viewerCompress` asks the slab route for zstd: ~28x smaller on real data for ~60 ms of server CPU,
  // so it is a clear win over a network and a small loss on this machine (docs/todo/WEB_VIEWER_PLAN.md).
  const viewerSteps = ref(Number(localStorage.getItem('cc.viewerSteps') ?? '256') || 256)
  const viewerCompress = ref(localStorage.getItem('cc.viewerCompress') === 'true')   // default false
  const viewerFps = ref(Number(localStorage.getItem('cc.viewerFps') ?? '10') || 10)
  const viewerLoop = ref(localStorage.getItem('cc.viewerLoop') !== 'false')          // default true
  // How many timepoints to keep instant — NOT a byte budget. It used to be megabytes of VRAM, which is
  // a question nobody can answer: WebGPU exposes no free-VRAM figure, so neither the user nor the app
  // can compute a safe number, and setting it too high LOST THE GPU DEVICE (unrecoverable — the viewer
  // can only offer a reload). Timepoints is the outcome the user actually wants ("how much of my movie
  // is instant"), and it is a request rather than a promise: the viewer clamps it to `SAFE_CACHE_BYTES`,
  // which is the safety net, since a timepoint is 8.8 MB as a plane and 326 MB as a volume and no count
  // can be safe in both. 0 = as much of the movie as fits.
  // On-image overlays — default ON for both (the scale bar and, for a timecourse, elapsed time). Same
  // two things the movie compositor draws, and
  // the same helpers draw them here (`StillOverlay`), so all three surfaces agree.
  const viewerScaleBar = ref(localStorage.getItem('cc.viewerScaleBar') !== 'false')    // default true
  const viewerTimestamp = ref(localStorage.getItem('cc.viewerTimestamp') !== 'false')  // default true
  // Overlay text size, in screen px. Two numbers rather than one because they annotate different
  // things — and a setting rather than a constant because "readable" depends on the window size and on
  // whether the shot is going into a talk.
  // Overlay point size, in SCREEN px — a cell marker is annotation, so it stays legible zoomed out and
  // must not swallow the cell zoomed in. Default 6 (inherited from the R-Shiny era).
  const viewerPointSize = ref(Number(localStorage.getItem('cc.viewerPointSize') ?? '6') || 6)
  // Track tails. `viewerTailLength` is in FRAMES (default 30) and `viewerTailWidth` in screen px
  // (default 4). 0 length hides them.
  const viewerTailLength = ref(Number(localStorage.getItem('cc.viewerTailLength') ?? '30') || 30)
  const viewerTailWidth = ref(Number(localStorage.getItem('cc.viewerTailWidth') ?? '4') || 4)
  // Segmentation mask overlay (P4). `viewerLabelOpacity` is the fill opacity (default 0.7) and
  // `viewerLabelContour` is the outline width in voxels — an outline that many voxels thick instead of
  // a filled region, which is what lets the channel signal under the mask stay readable. 0 = filled.
  // Not `|| 0.7`: a deliberate 0 is a valid opacity and would otherwise spring back.
  const viewerLabelOpacity = ref(Number(localStorage.getItem('cc.viewerLabelOpacity') ?? '0.7'))
  const viewerLabelContour = ref(Number(localStorage.getItem('cc.viewerLabelContour') ?? '0') || 0)
  // How many z planes either side of the one on screen still draw their cell's marker.
  //
  // 0 is the strict reading — a marker appears only on the plane its centroid falls on. On real data
  // that reads as RANDOM (Dominik, 2026-08-25): the mask layer draws every cell
  // that INTERSECTS the plane, while the points draw only the few centred on it, so the two look
  // unrelated. A cell spans several planes, so a small tolerance is the honest default; it is a setting
  // rather than a constant because the right number is the cell diameter, which is per experiment.
  const viewerPointZTol = ref(Number(localStorage.getItem('cc.viewerPointZTol') ?? '2'))
  // Track ribbon Z reach — same idea as viewerPointZTol but for the tail path. A track spans several
  // planes and often reads best with more slack than a centroid dot, so the two are decoupled.
  const viewerTrackZTol = ref(Number(localStorage.getItem('cc.viewerTrackZTol') ?? '2'))
  const viewerScaleBarPx = ref(Number(localStorage.getItem('cc.viewerScaleBarPx') ?? '20') || 20)
  const viewerTimestampPx = ref(Number(localStorage.getItem('cc.viewerTimestampPx') ?? '20') || 20)
  const viewerCacheFrames = ref(Number(localStorage.getItem('cc.viewerCacheFrames') ?? '0') || 0)
  // 3D pyramid LEVEL for the volume view. -1 = auto (the DEEPEST level = coarsest resolution),
  // 0..N-1 = force that level. The default is the deepest level because a full-res volume
  // exceeds WebGPU's `maxBufferSize` on wide-XY images (`f8gzA2` → 1.28 GB against a 256 MB cap).
  // Imaris-style octree LOD was on the wishlist but never shipped, so "coarsest by default, user
  // may override" is the answer this ships with. (Spatial audit Phase 2.5, 2026-08-25.)
  const viewerVolumeLevel = ref(Number(localStorage.getItem('cc.viewerVolumeLevel') ?? '-1'))
  // 3D projection: 'ortho' (default) = Imaris-style head-on, easier on the eye for intravital movies
  // and matches the offline movie renderer's parallel-ray MIP. 'persp' = perspective with
  // foreshortening. Both are already implemented in the shaders (`brickShader.ts`, `mipShader.ts`);
  // this only picks which one the renderer's `ortho` uniform ships. Live-toggled — no reallocate.
  const viewerVolumeProjection = ref<'ortho' | 'persp'>(
    (localStorage.getItem('cc.viewerVolumeProjection') as 'ortho' | 'persp') || 'ortho')
  // 2D plane pyramid LEVEL. -1 = auto (zoom-driven — the coarsest level whose native pixel is still
  // ≤ one device pixel, so we never ship pixels the screen cannot show; recomputes as the user zooms,
  // debounced). 0..N-1 = pin that level. Different policy from 3D (which defaults to the coarsest and
  // stays there): the plane view is what you pan/zoom, so the pyramid does what pyramids are for.
  // Phase B of `docs/todo/VIEWER_TILES_PLAN.md`.
  const viewerPlaneLevel = ref(Number(localStorage.getItem('cc.viewerPlaneLevel') ?? '-1'))
  // 3D renderer selection. `auto` (default) delegates to `shouldUseBricks(meta)` — the movie-fits-
  // cache predicate that routes small movies to flat (fXgbTl) and large ones to brick (Dml3RG, f8gzA2,
  // …). `brick` and `flat` are per-user overrides for images the auto-select gets wrong. The URL
  // `?bricks=0|1` still wins over this — dev-only, ephemeral. Changing this triggers a full renderer
  // reallocate, so the flip is visible (~1-2 s canvas flash) but reliable.
  const viewerBricksMode = ref<'auto' | 'brick' | 'flat'>(
    (localStorage.getItem('cc.viewerBricksMode') as 'auto' | 'brick' | 'flat') || 'auto')
  // Brick-scheduler quality tier — caps `maxIntersect` (the core-brick ceiling in the over-fetch
  // guard). Balanced = the shipped default (256, `MAX_INTERSECT_BRICKS`). Quick trades detail for
  // wide-viewport headroom; Detailed pushes toward the pinhole SSE at the cost of more resident
  // bricks. `?brickThr=N` still wins over this — dev override for A/B measurement. Applies live via
  // `setSchedulerKnobs`; no reallocate. Hidden when the effective renderer is Flat.
  const viewerBrickTier = ref<'quick' | 'balanced' | 'detailed'>(
    (localStorage.getItem('cc.viewerBrickTier') as 'quick' | 'balanced' | 'detailed') || 'balanced')
  // Viewer VRAM budget in MB. `-1` = Auto (currently 1500 MB — the shipped `SAFE_CACHE_BYTES`
  // default from before this setting existed). Applies to both renderers: flat's timepoint cache
  // and brick's atlas both read this via `setImage`'s `budgetBytes`. Changing it triggers a
  // reallocate — the atlas texture has to be rebuilt at the new size. `?cacheMB=N` in the URL
  // still wins over this (dev override).
  const viewerCacheMB = ref(Number(localStorage.getItem('cc.viewerCacheMB') ?? '-1'))

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
  // The WebGPU viewer's controls sidebar (`ViewerWindow.vue`) uses the same CollapsiblePanel but its
  // collapse is INDEPENDENT — the module-page right panel and the viewer-window sidebar hold
  // different things, and a shared flag folded the viewer's own controls away whenever the module
  // page's task list was collapsed (Dominik, 2026-08-28). CollapsiblePanel accepts a `collapsedRef`
  // override; ViewerWindow points it here.
  const viewerWindowSideCollapsed = ref(localStorage.getItem('cc.viewerWindowSideCollapsed') === 'true')
  // the Viewer controls are a floating dockable panel (not a sidebar section) — this is its
  // open/closed state, toggled from the sidebar "Viewer" button. Off by default (opt-in, no intrusion).
  const viewerPanelOpen = ref(localStorage.getItem('cc.viewerPanelOpen') === 'true')
  // WebGPU viewer's SELECT MODE. Off by default so a click never picks accidentally while a user is
  // panning around; the gating toolbar's pencil toggles it on. When 'select', a click sends a
  // /api/viewer/pick-cell request; drag will draw a rectangle (follow-up). Shift/alt still route to
  // the multi-select modes. Cross-window via the same localStorage event bridge the other viewer
  // bags use (the popup viewer is a separate Pinia instance — see decodeViewerBagEvent).
  // See docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md → P8.
  const viewerSelectMode = ref<'off' | 'select'>(
    (localStorage.getItem('cc.viewerSelectMode') === 'select') ? 'select' : 'off')
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
  // The WebGPU viewer renders ONE segmentation at a time (single-slot bind group), so the panel
  // is radio-like: exactly one label ticked. The default therefore picks the FIRST name, not
  // "everything true" — the R-Shiny-era default made every fresh image read as all-ticked while
  // only the first one actually drew, and adding a segmentation later reintroduced the same lie
  // because `?? true` treated every unknown name as visible.
  // Persisted across sessions.
  const _labelVisStore = ref<Record<string, Record<string, boolean>>>(
    JSON.parse(localStorage.getItem('cc.viewerLabelVisibility') ?? '{}')
  )
  function getLabelVisibility(imageUid: string, labelNames: string[]): Record<string, boolean> {
    const stored = _labelVisStore.value[imageUid]
    const out: Record<string, boolean> = {}
    if (stored) {
      // Existing selection: honour every stored flag; new names arriving after a persist stay off
      // rather than silently flipping the visible one (radio-like invariant).
      for (const vn of labelNames) out[vn] = stored[vn] ?? false
      // Legacy bags from the R-Shiny era carry every name true; on the WebGPU viewer that would
      // read as "all ticked but only one draws". Collapse to the FIRST true one.
      const firstTrue = labelNames.find(n => out[n])
      if (firstTrue) for (const vn of labelNames) if (vn !== firstTrue) out[vn] = false
      // If the stored bag has no true entries, HONOUR IT — the user explicitly unticked the last
      // segmentation to hide the mask (Dominik, 2026-08-26: "when i turn off the segmentation
      // toggle. the last segmentation is still showing on the image. it never disappears"). An
      // earlier revision re-ticked the first here on the theory that "nothing rendering reads
      // worse than a default", but it makes the untick a no-op — the mask stays on because the
      // read still returns true for the same first name.
    } else {
      // First open on this image: pick the first name, not "everything true".
      for (const vn of labelNames) out[vn] = false
      if (labelNames.length) out[labelNames[0]] = true
    }
    return out
  }
  function setLabelVisibility(imageUid: string, vis: Record<string, boolean>) {
    _labelVisStore.value = { ..._labelVisStore.value, [imageUid]: { ...vis } }
    localStorage.setItem('cc.viewerLabelVisibility', JSON.stringify(_labelVisStore.value))
  }

  // per-image track-overlay visibility: { [imageUid]: { [valueName]: boolean } } — which
  // segmentations have their tracks shown in the viewer. Default OFF (tracks are a heavier overlay).
  const _trackVisStore = ref<Record<string, Record<string, boolean>>>(
    JSON.parse(localStorage.getItem('cc.viewerTrackVisibility') ?? '{}')
  )
  function getTrackVisibility(imageUid: string, valueNames: string[]): Record<string, boolean> {
    const stored = _trackVisStore.value[imageUid] ?? {}
    const out: Record<string, boolean> = {}
    for (const vn of valueNames) out[vn] = stored[vn] ?? false   // default hidden
    return out
  }
  function setTrackVisibility(imageUid: string, vis: Record<string, boolean>) {
    _trackVisStore.value = { ..._trackVisStore.value, [imageUid]: { ...vis } }
    localStorage.setItem('cc.viewerTrackVisibility', JSON.stringify(_trackVisStore.value))
  }

  // per-image branch-overlay visibility (skeleton labels from segment.branching). Default ON —
  // if a user has branch labels registered, they almost always want to see them (running the
  // task is the opt-in; hiding the layer is the exception, not the rule — mirrors cell labels).
  const _branchVisStore = ref<Record<string, Record<string, boolean>>>(
    JSON.parse(localStorage.getItem('cc.viewerBranchVisibility') ?? '{}')
  )
  function getBranchVisibility(imageUid: string, valueNames: string[]): Record<string, boolean> {
    const stored = _branchVisStore.value[imageUid] ?? {}
    const out: Record<string, boolean> = {}
    for (const vn of valueNames) out[vn] = stored[vn] ?? true    // default visible
    return out
  }
  function setBranchVisibility(imageUid: string, vis: Record<string, boolean>) {
    _branchVisStore.value = { ..._branchVisStore.value, [imageUid]: { ...vis } }
    localStorage.setItem('cc.viewerBranchVisibility', JSON.stringify(_branchVisStore.value))
  }

  // Per-image active version: { [imageUid]: valueName }. Written by the ViewerPanel's version
  // <select> (main window) and mirrored into the popup viewer via the storage-event bridge, so the
  // two windows never disagree about which version is on screen (VIEWER_CONTROLS_SPLIT_PLAN.md P3
  // extended to the version picker, Dominik 2026-08-26). Empty entry = no user pick, fall back to
  // the URL query / active-in-project default; DO NOT default to the first name here — that would
  // decide the version silently before the user has expressed one.
  const _imageVersionStore = ref<Record<string, string>>(
    JSON.parse(localStorage.getItem('cc.viewerImageVersion') ?? '{}')
  )
  function getImageVersion(imageUid: string): string {
    return _imageVersionStore.value[imageUid] ?? ''
  }
  function setImageVersion(imageUid: string, valueName: string) {
    _imageVersionStore.value = { ..._imageVersionStore.value, [imageUid]: valueName }
    localStorage.setItem('cc.viewerImageVersion', JSON.stringify(_imageVersionStore.value))
  }

  // ── Per-SET viewer preferences, keyed by set uid: { [setUid]: {...} } ─────────────────────────
  // These are the viewer-level DISPLAY toggles (colour-by, show-3D, point size, per-popType overlay
  // visibility, show-gated-tracks). They were always MEANT to be per-set (one experiment = consistent
  // viewing); the old R app made them global only because Shiny bookmarks made that easy. Per-set (not
  // global) so a choice made in one experiment never bleeds onto another's images (e.g. a colour-by
  // column that a different set's segmentation doesn't have), and not per-image so you set it ONCE and
  // it holds as you click through the set's images. Per-image state (which segmentations/tracks are
  // shown — the per-segmentation rows) stays keyed by image uid above.
  interface ViewerSetPrefs {
    colourBy?: string                       // obs column to colour labels/tracks by ('' = default)
    show3D?: boolean                        // open images volumetric (only applied where a z-axis exists)
    showGatedTracks?: boolean               // overlay gated track populations
    pointSize?: number                      // population centroid point size in the viewer
    popVis?: Record<string, boolean>        // per-popType point-overlay visibility (flow/clust/track/trackclust)
    // How the viewer colours track ribbons — 'track' (palette by track id), 'speed' (heat ramp by
    // per-hop distance), 'solid' (one palette colour per source vn, so multiple track sources are
    // visually separable). Per-set: mirrors `colourBy` above.
    trackColorMode?: 'track' | 'speed' | 'solid'
    // Per-source hex overrides for the SOLID track colour mode: {[vn]: '#rrggbb'}. Absent = the
    // default from the palette. Dominik, 2026-08-26: "can we make that the source color can be
    // changed. same color picker as for the channels just with the cecelia palette".
    trackSourceColour?: Record<string, string>
    // user recolouring of a categorical colour-by, keyed by column then category value → hex. For
    // categories with no population (HMM states, raw clusters) there's no colour defined anywhere, so
    // the user can override the default palette; these win over pop/default when colouring. Per-column
    // so different colour-by columns keep independent schemes.
    colourByOverrides?: Record<string, Record<string, string>>
    // timelapse-recording params (extensible — F1.2 adds channels/pops/T-range here). fps = frame rate;
    // sizeX/sizeY = output pixels, absent/null = the viewer canvas size. Per-set like the other viewer
    // prefs. A `scale` supersample lived here and was removed (see MovieOutputControls.vue); an older
    // prefs file may still carry the key, it is simply unread, and the name is deliberately NOT reused —
    // a stale 1-3 multiplier must never be read as a pixel width.
    // `suffix` is a filename addition (a movie is named after the IMAGE, so the corrected version and
    // the raw import would collide). null = never set, so the UI's version-derived default applies;
    // '' = deliberately cleared, which must survive a reload.
    // `compareVersions` are the image versions the recorder shows side by side, in COLUMN order (2+;
    // [] or one = an ordinary single-version movie), with the layout + contrast mode that go with them.
    // `showTimestamp`/`showScaleBar` are the viewer's BAKED overlays — burnt into every recorded frame, so
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
              // the viewer's default (its coarsest level). Per set, like the other viewer prefs.
              detail3d?: number | null
              compareLayout?: CompareLayout; compareContrast?: CompareContrast
              showTimestamp?: boolean; showScaleBar?: boolean }
    // 3D-crop z-range and t-range as 0–100 % (per set — the XY crop box itself is per-session, drawn in
    // the viewer each time since a region is image-specific). Only the ranges persist, like other prefs.
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
  const _setPrefs = ref<Record<string, ViewerSetPrefs>>(
    JSON.parse(localStorage.getItem('cc.viewerSetPrefs') ?? '{}')
  )
  function _patchSet(setUid: string, patch: Partial<ViewerSetPrefs>) {
    _setPrefs.value = { ..._setPrefs.value, [setUid]: { ...(_setPrefs.value[setUid] ?? {}), ...patch } }
    localStorage.setItem('cc.viewerSetPrefs', JSON.stringify(_setPrefs.value))
  }
  const getColourBy = (setUid: string): string => _setPrefs.value[setUid]?.colourBy ?? ''
  const setColourBy = (setUid: string, column: string) => _patchSet(setUid, { colourBy: column })
  const getShow3D = (setUid: string): boolean => _setPrefs.value[setUid]?.show3D ?? false
  const setShow3D = (setUid: string, v: boolean) => _patchSet(setUid, { show3D: v })
  const getShowGatedTracks = (setUid: string): boolean => _setPrefs.value[setUid]?.showGatedTracks ?? false
  const setShowGatedTracks = (setUid: string, v: boolean) => _patchSet(setUid, { showGatedTracks: v })
  const getPointSize = (setUid: string): number => _setPrefs.value[setUid]?.pointSize ?? 6   // old GUI default 6
  const setPointSize = (setUid: string, v: number) => _patchSet(setUid, { pointSize: v })
  const getTrackColorMode = (setUid: string): 'track' | 'speed' | 'solid' =>
    _setPrefs.value[setUid]?.trackColorMode ?? 'track'                                        // default: cycle palette by track id
  const setTrackColorMode = (setUid: string, mode: 'track' | 'speed' | 'solid') =>
    _patchSet(setUid, { trackColorMode: mode })
  const getTrackSourceColours = (setUid: string): Record<string, string> =>
    _setPrefs.value[setUid]?.trackSourceColour ?? {}
  function setTrackSourceColour(setUid: string, vn: string, hex: string) {
    const cur = _setPrefs.value[setUid]?.trackSourceColour ?? {}
    _patchSet(setUid, { trackSourceColour: { ...cur, [vn]: hex } })
  }
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
    // 0 = full resolution. The default is full because the coarsest level erases a strided label
    // pyramid — the cost is a visible control.
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
  watch(viewerAutoUpdate,         v => localStorage.setItem('cc.viewerAutoUpdate',         String(v)))
  watch(animationSyncViewer,      v => localStorage.setItem('cc.animationSyncViewer',      String(v)))
  watch(viewerResetOnReload,      v => localStorage.setItem('cc.viewerResetOnReload',      String(v)))
  watch(viewerAutoSaveLayerProps, v => localStorage.setItem('cc.viewerAutoSaveLayerProps', String(v)))
  watch(viewerSteps,              v => localStorage.setItem('cc.viewerSteps',              String(v)))
  watch(viewerCompress,           v => localStorage.setItem('cc.viewerCompress',           String(v)))
  watch(viewerFps,                v => localStorage.setItem('cc.viewerFps',                String(v)))
  watch(viewerLoop,               v => localStorage.setItem('cc.viewerLoop',               String(v)))
  watch(viewerCacheFrames,        v => localStorage.setItem('cc.viewerCacheFrames',        String(v)))
  watch(viewerVolumeLevel,        v => localStorage.setItem('cc.viewerVolumeLevel',        String(v)))
  watch(viewerVolumeProjection,   v => localStorage.setItem('cc.viewerVolumeProjection',   v))
  watch(viewerPlaneLevel,         v => localStorage.setItem('cc.viewerPlaneLevel',         String(v)))
  watch(viewerBricksMode,         v => localStorage.setItem('cc.viewerBricksMode',         v))
  watch(viewerBrickTier,          v => localStorage.setItem('cc.viewerBrickTier',          v))
  watch(viewerCacheMB,            v => localStorage.setItem('cc.viewerCacheMB',            String(v)))
  watch(viewerScaleBar,           v => localStorage.setItem('cc.viewerScaleBar',           String(v)))
  watch(viewerTimestamp,          v => localStorage.setItem('cc.viewerTimestamp',          String(v)))
  watch(viewerPointSize,          v => localStorage.setItem('cc.viewerPointSize',          String(v)))
  watch(viewerTailLength,         v => localStorage.setItem('cc.viewerTailLength',         String(v)))
  watch(viewerTailWidth,          v => localStorage.setItem('cc.viewerTailWidth',          String(v)))
  watch(viewerLabelOpacity,       v => localStorage.setItem('cc.viewerLabelOpacity',       String(v)))
  watch(viewerLabelContour,       v => localStorage.setItem('cc.viewerLabelContour',       String(v)))
  watch(viewerPointZTol,          v => localStorage.setItem('cc.viewerPointZTol',          String(v)))
  watch(viewerTrackZTol,          v => localStorage.setItem('cc.viewerTrackZTol',          String(v)))
  watch(viewerScaleBarPx,         v => localStorage.setItem('cc.viewerScaleBarPx',         String(v)))
  watch(viewerTimestampPx,        v => localStorage.setItem('cc.viewerTimestampPx',        String(v)))
  watch(moviesPlaybackRate,       v => localStorage.setItem('cc.moviesPlaybackRate',       String(v)))
  watch(moviesZoom,               v => localStorage.setItem('cc.moviesZoom',               String(v)))
  watch(moviesAutoplay,           v => localStorage.setItem('cc.moviesAutoplay',           String(v)))
  watch(moviesEndMode,            v => localStorage.setItem('cc.moviesEndMode',            v))
  watch(moviesShowDetails,        v => localStorage.setItem('cc.moviesShowDetails',        String(v)))
  watch(moviesChannelMode,        v => localStorage.setItem('cc.moviesChannelMode',        String(v)))
  watch(sidebarCollapsed,         v => localStorage.setItem('cc.sidebarCollapsed',         String(v)))
  watch(rightPanelCollapsed,      v => localStorage.setItem('cc.rightPanelCollapsed',      String(v)))
  watch(viewerWindowSideCollapsed, v => localStorage.setItem('cc.viewerWindowSideCollapsed', String(v)))
  watch(viewerPanelOpen,          v => localStorage.setItem('cc.viewerPanelOpen',          String(v)))
  watch(viewerSelectMode,         v => localStorage.setItem('cc.viewerSelectMode',         String(v)))
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

  // Cross-window sync for the per-image / per-set viewer state bags. The volume viewer runs in a
  // popup with its OWN Pinia store; localStorage `storage` events are the bridge. Decoder + full
  // rationale in utils/viewerBagChannel.ts (pure, tested); this file's job is to dispatch. See
  // docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md P2.
  if (typeof window !== 'undefined') {
    window.addEventListener('storage', e => {
      const ev = decodeViewerBagEvent(e.key, e.newValue)
      if (!ev) return
      switch (ev.kind) {
        case 'labelVis':     _labelVisStore.value     = ev.value as Record<string, Record<string, boolean>>; break
        case 'trackVis':     _trackVisStore.value     = ev.value as Record<string, Record<string, boolean>>; break
        case 'branchVis':    _branchVisStore.value    = ev.value as Record<string, Record<string, boolean>>; break
        case 'setPrefs':     _setPrefs.value          = ev.value as Record<string, ViewerSetPrefs>; break
        case 'imageVersion': _imageVersionStore.value = ev.value as Record<string, string>; break
      }
    })
    // Direct string keys — not JSON, so they don't go through the bag decoder. `storage` events
    // only fire in OTHER windows, so this is the path the popup viewer's mode toggle takes to
    // reach the main window's settings store (Dominik, 2026-08-26). Without this the pop-manager
    // pencil stayed in its old state when the viewer flipped the mode.
    window.addEventListener('storage', e => {
      if (e.key === 'cc.viewerSelectMode') {
        viewerSelectMode.value = e.newValue === 'select' ? 'select' : 'off'
      }
    })
  }

  return { viewProfile, taskListAutoFollow, tasksThisProjectOnly, tasksShowHistory, autoRefreshOnTask, viewerAutoUpdate, animationSyncViewer, viewerResetOnReload, viewerAutoSaveLayerProps, viewerSteps, viewerCompress, viewerFps, viewerLoop, viewerCacheFrames, viewerVolumeLevel, viewerVolumeProjection, viewerPlaneLevel, viewerBricksMode, viewerBrickTier, viewerCacheMB, viewerScaleBar, viewerTimestamp, viewerScaleBarPx, viewerTimestampPx, viewerPointSize, viewerTailLength, viewerTailWidth, viewerLabelOpacity, viewerLabelContour, viewerPointZTol, viewerTrackZTol, moviesPlaybackRate, moviesZoom, moviesAutoplay, moviesEndMode, moviesShowDetails, moviesChannelMode, sidebarCollapsed, rightPanelCollapsed, viewerWindowSideCollapsed, viewerPanelOpen, viewerSelectMode, labLogPanelOpen, hiddenMcpAccounts, labLogAutoContext, labLogShowNames, labLogObserverModel, labLogUnseen, labLogUnseenKind, labLogUnseenLevel, tipsOnLaunch, tipsLastShown, getLabelVisibility, setLabelVisibility, getTrackVisibility, setTrackVisibility, getBranchVisibility, setBranchVisibility, getImageVersion, setImageVersion, getColourBy, setColourBy, getShow3D, setShow3D, getShowGatedTracks, setShowGatedTracks, getPointSize, setPointSize, getPopVisible, setPopVisible, getTrackColorMode, setTrackColorMode, getTrackSourceColours, setTrackSourceColour, getColourOverrides, setColourOverride, clearColourOverrides, getMovieConfig, setMovieConfig, getCropZ, setCropZ, getCropT, setCropT, getBatchMovieConfig, setBatchMovieConfig, replaceBatchMovieConfig }
})

// Replace the live instance on hot-reload — see the note in `stores/customModules.ts`.
if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useSettingsStore, import.meta.hot))
