// The Share flow for any canvas host.
//
// Extracted from `SummaryCanvas.vue` when a second host (ClusterPlots, GatingPlots, AnalysisModule)
// needed the same two-phase Share machinery — panel-selection overlay → frozen composite +
// FrameAnnotator → POST → outcome toast. All that boilerplate now lives here; the per-canvas
// difference is just the envelope shape (how panels are described on the wire) + the address
// `plotSpec` (specId + params).
//
// Deliberately keeps RESHOW inline in each host — CaptureViewSurface's zoom-to-source restores
// panels via `restorePanelsFromCapture`, and the panel-state re-hydrate is host-specific (each
// canvas has its own PanelState shape). The reshow ref + handlers stay in the SFC; the composable
// only owns Share.
//
// The composable owns:
// - `shareSel` (a `useCanvasShareSelection()` instance) — Phase 1 selection state, passed to the
//   overlay
// - Kiwi share-host registration (`useCanvasShareHost({ beginShare, label })`)
// - `sharePanelHits` — reactive PanelHit list built from the geom store, filtered to panels with
//   a known geometry (see the comment on the equivalent block in the pre-extraction SummaryCanvas)
// - `pendingShare` — the Phase 1→2 transition state (frozen composite + envelope + workspace
//   origin), gates FrameAnnotator's mount
// - `shareBusy` — a single busy flag both phases key on
// - `shareToast` — post-Save outcome chip, matches the viewer's `.vw-share-chip`
// - Handlers: `onShareCancel`, `onShareConfirm`, `onAnnotateCancel`, `onAnnotateSave`,
//   `dismissShareToast`

import { computed, ref, type Ref } from 'vue'
import { useCanvasPanelsStore } from '../stores/canvasPanels'
import { useCanvasPanelExportsStore } from '../stores/canvasPanelExports'
import { useCanvasShareSelection } from './useCanvasShareSelection'
import { useCanvasShareHost } from '../stores/shareTarget'
import { composePanelGrid, type PanelTile } from '../utils/overlayCompose'
import type { CaptureAddress, OverlayMark } from '../utils/captureAddress'
import type { CaptureEnvelope } from '../utils/kiwiCaptures'
import type { PanelHit } from '../utils/panelSelectionHit'
import { announceShareOutcome, shareFailMessage, type ShareOutcome } from '../utils/shareOutcome'
import { useCaptureDestination } from './useCaptureDestination'
import { useKiwiStore } from '../stores/kiwi'

/** Per-panel input the host passes for the geom-lookup step. `id` is stable across the panels()
 *  array; the composable uses it to key the exporter registry (`${canvasKey}:${id}`) and to look
 *  up the panel geom via the canvas-panels store. */
export interface SharePanel { id: number }

/** Per-panel data the composable hands the host's `buildEnvelope` callback. `pngDataUrl` is the
 *  panel's own rasteriser output (may be null if the panel failed to register or export — the
 *  host decides what to write for that panel in that case). `geom` is workspace-CSS-px. */
export interface SharePanelExported {
  id: number
  geom: PanelHit['geom']
  pngDataUrl: string | null
}

export interface UseCanvasShareOpts {
  /** Project uid — read via a getter so a swap during share doesn't get baked in. */
  projectUid: () => string
  /** Canvas key — the same key the geom store + exporter store are keyed by (e.g. `summary:cellCards`,
   *  `cluster:tracks`, `gate:pairs`). `${canvasKey}:${panelId}` is the exporter lookup key. */
  canvasKey: () => string
  /** All panels currently on the canvas. Only panels with a known geometry contribute to
   *  `sharePanelHits` — an undocked panel with no persisted geom would render at (0,0) and eat
   *  every click, so it's filtered here (same policy the pre-extraction SummaryCanvas used). */
  panels: () => SharePanel[]
  /** Optional geom-source override — a host whose panels don't write to `useCanvasPanelsStore`
   *  (LayoutCanvas grid slots: `CanvasPanel[docked]` skips `setGeom`) supplies its own hit list
   *  computed from DOM rects. When present, the default store lookup is skipped entirely — the
   *  host takes full responsibility for filtering out empty / zero-size hits. */
  panelHits?: () => PanelHit[]
  /** Short human label for Kiwi's tooltip AND for the selection overlay's `addressLine`. Same
   *  string on both surfaces so the user reads a consistent name. */
  label: () => string
  /** Build the per-canvas `panels[]` array Claude reads — one entry per selected panel.
   *  Position is composite-relative (the composable has already subtracted the union bbox origin;
   *  see `workspaceOrigin` on the return + envelope docs on `SummaryCanvas.vue::onShareConfirm`
   *  for why this split matters for zoom-to-source). */
  buildEnvelope: (opts: {
    selected: SharePanelExported[]
    workspaceOrigin: { x: number; y: number }
  }) => Array<Record<string, unknown>>
  /** The `address.plotSpec` bag on the POST. `panelCount` is the size of the envelope so callers
   *  can echo it in `params` (SummaryCanvas already does this: `{ module, panelCount }`). */
  buildPlotSpec: (opts: { panelCount: number }) => CaptureAddress['plotSpec']
  /** Optional module string that lets Kiwi refocus scope back to the right canvas (see
   *  `captureReshowStore.consumeFor`). Kept as a plain string on the address so a refocus without
   *  a live canvas still remembers where it came from. */
  module?: () => string
  /** Optional hook fired when the POST succeeded. Lets the host set its `reshown` ref so the
   *  CaptureViewSurface mounts over the canvas with the just-shared frame. If omitted, the flow
   *  still completes silently (POST OK, chip up, no reshow). */
  onSaveSuccess?: (env: CaptureEnvelope) => void
}

/** Phase-1 → Phase-2 transition state. `composite` is the frozen PNG data URL FrameAnnotator
 *  draws on; `panels` is the wire-shape array built by the host's `buildEnvelope`;
 *  `workspaceOrigin` is the union bbox min-corner in workspace CSS px so a later
 *  zoom-to-source restores the panels to their original workspace positions. */
export interface PendingShare {
  composite: string
  panels: Array<Record<string, unknown>>
  workspaceOrigin: { x: number; y: number }
}

export function useCanvasShare(opts: UseCanvasShareOpts) {
  const geomStore = useCanvasPanelsStore()
  const exportStore = useCanvasPanelExportsStore()
  const shareSel = useCanvasShareSelection()
  // KIWI_CAPTURE_AND_BLACKBOARD_PLAN P1: FrameAnnotator's two capture-destination toggles resolve
  // through this composable. Same helper the viewer path uses, so both surfaces read/write the
  // same persisted setting + share the pairing WS watcher.
  const captureDest = useCaptureDestination(opts.projectUid)
  const kiwiStore = useKiwiStore()

  // `pendingShare` is the Phase 1→2 handoff; null when share is idle or when the selection was
  // cancelled before the composite was built.
  const pendingShare = ref<PendingShare | null>(null)
  const shareBusy = ref(false)

  // Post-Save toast — same role as ViewerWindow's share chip. FrameAnnotator dismisses on Save,
  // taking the "paste to Claude" hint with it, so surface the outcome inside the canvas for a
  // few seconds. Auto-dismisses after ~7s to match the viewer's timing.
  const shareToast = ref<ShareOutcome | null>(null)
  let shareToastTimer: number | null = null
  function showShareToast(kind: 'ok' | 'fail', message: string, ms = 7000) {
    shareToast.value = { kind, message }
    if (shareToastTimer) clearTimeout(shareToastTimer)
    shareToastTimer = window.setTimeout(() => { shareToast.value = null; shareToastTimer = null }, ms)
  }
  function dismissShareToast() {
    shareToast.value = null
    if (shareToastTimer) { clearTimeout(shareToastTimer); shareToastTimer = null }
  }

  // Kiwi enable — the button in the cockpit fires `beginShare()` here; label goes into its
  // tooltip. Registered once per composable instance (i.e. once per host mount).
  useCanvasShareHost({
    beginShare: () => {
      // A fresh Share lands on a clean state — clear any pendingShare from a prior aborted flow.
      // Reshow (if any) is the host's concern; the host can clear it in its own `beginShare` hook
      // by mutating the reshown ref before calling into here (or by watching `shareSel.active`).
      pendingShare.value = null
      shareSel.begin()
    },
    get label() { return opts.label() },
  })

  // PanelHit list the overlay hit-tests against. Two branches:
  //   1. Host provided a `panelHits` override (LayoutCanvas grid slots — geoms computed from DOM
  //      rects since `CanvasPanel[docked]` skips `setGeom`). Take it verbatim.
  //   2. Default — read from `useCanvasPanelsStore`; only PANELS WITH A KNOWN GEOMETRY count. An
  //      undocked panel with no persisted geom would appear at (0,0) which would silently swallow
  //      every click. First render writes geometry immediately (CanvasPanel does it on mount), so
  //      a panel that visibly exists is a panel with a geom.
  const sharePanelHits = computed<PanelHit[]>(() => {
    if (opts.panelHits) return opts.panelHits()
    const key = opts.canvasKey()
    const out: PanelHit[] = []
    for (const p of opts.panels()) {
      const g = geomStore.getGeom(`${key}:${p.id}`)
      if (g && g.w > 0 && g.h > 0) out.push({ id: p.id, geom: g })
    }
    return out
  })

  function onShareCancel() { shareSel.end() }

  async function onShareConfirm(payload: { panelIds: number[] }) {
    const puid = opts.projectUid()
    if (!puid || shareBusy.value) return
    shareBusy.value = true
    try {
      // Gather tiles for the selected panels: PNG (via each panel's registered exporter) + its
      // workspace-relative geom. A panel that failed to register or failed to export still gets an
      // empty box in the composite rather than dropping the whole share.
      const canvasKey = opts.canvasKey()
      const selectedHits = sharePanelHits.value.filter(p => payload.panelIds.includes(p.id))
      const exported: SharePanelExported[] = await Promise.all(selectedHits.map(async p => {
        const exporter = exportStore.get(`${canvasKey}:${p.id}`)
        const png = exporter ? await exporter() : null
        return { id: p.id, geom: p.geom, pngDataUrl: png }
      }))
      const tiles: PanelTile[] = exported.map(e => ({ pngDataUrl: e.pngDataUrl, geom: e.geom }))
      const composite = await composePanelGrid(tiles)
      if (!composite) { shareBusy.value = false; shareSel.end(); return }

      // Composite-relative origin: subtract the union bbox origin so each panel's `position` in
      // the envelope matches where it sits in the shared PNG (not the on-screen workspace).
      let x0 = Infinity, y0 = Infinity
      for (const e of exported) {
        if (e.geom.x < x0) x0 = e.geom.x
        if (e.geom.y < y0) y0 = e.geom.y
      }
      if (!Number.isFinite(x0)) { x0 = 0; y0 = 0 }
      const workspaceOrigin = { x: x0, y: y0 }

      // Host builds the per-panel envelope (specId, ui state, dataSlice, …) — the composable stays
      // oblivious to what each canvas puts in.
      const panels = opts.buildEnvelope({ selected: exported, workspaceOrigin })

      pendingShare.value = { composite, panels, workspaceOrigin }
      shareSel.end()
    } finally {
      shareBusy.value = false
    }
  }

  function onAnnotateCancel() { pendingShare.value = null }

  async function onAnnotateSave(payload: { overlay: OverlayMark[]; composedPng: string; notes: string }) {
    const pending = pendingShare.value
    const puid = opts.projectUid()
    if (!pending || !puid || shareBusy.value) return
    shareBusy.value = true
    // FrameAnnotator hands us a composed PNG (marks baked in). Empty ⇒ no marks drawn / compose
    // failed; ship the bare composite so the POST still succeeds either way.
    const png = payload.composedPng || pending.composite
    const address: CaptureAddress = {
      projectUid: puid,
      plotSpec: opts.buildPlotSpec({ panelCount: pending.panels.length }),
    }
    // Read the toggles ONCE, same discipline as ViewerWindow.onDrawSave — a flip mid-flight
    // would otherwise split request vs. post-response behaviour.
    const sendToPaired = captureDest.sendToPaired.value
    const attachToKiwi = captureDest.attachToKiwi.value
    try {
      const res = await fetch('/api/viewer/capture', {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          projectUid: puid, surface: 'plot',
          address, panels: pending.panels,
          workspaceOrigin: pending.workspaceOrigin,
          frames: [{ png }],
          overlay: payload.overlay,
          ...(payload.notes ? { notes: payload.notes } : {}),
          ...(sendToPaired ? {} : { noPush: true }),
        }),
      })
      let respJson: Record<string, unknown> | null = null
      try { respJson = await res.json() as Record<string, unknown> } catch { /* legacy */ }
      if (!res.ok) throw new Error(respJson?.error ? String(respJson.error) : `HTTP ${res.status}`)
      const captureId = String(respJson?.captureId ?? '')
      if (!captureId) throw new Error('capture POST returned no captureId')
      const pushOutcome = String(respJson?.push ?? 'not_paired')

      // Attach-to-Kiwi: same shape AddToKiwiButton uses on capture rows.
      let attachedToKiwi = false
      if (attachToKiwi) {
        try {
          kiwiStore.addRef({ kind: 'capture', captureId }, puid)
          attachedToKiwi = true
        } catch { /* attach failure is silent — the capture itself succeeded */ }
      }

      // Envelope for the host's reshow ref — mirrors the pre-extraction shape one-for-one so the
      // per-canvas CaptureViewSurface mount reads the same fields it did before.
      const env: CaptureEnvelope = {
        captureId,
        surface: 'plot',
        address,
        overlay: payload.overlay,
        viewStateSnapshot: null,
        landscape: null,
        notes: payload.notes ?? '',
        panels: pending.panels,
        workspaceOrigin: pending.workspaceOrigin,
        frame: png,
      }
      opts.onSaveSuccess?.(env)

      // Transition NOW — annotator down, CVS up (via the host's reshown ref) — before we announce
      // the outcome. `announceShareOutcome` awaits `copyText`, and a stalled clipboard call would
      // leave the annotator visible with the POST already landed.
      pendingShare.value = null
      shareBusy.value = false

      void announceShareOutcome(pushOutcome, payload.notes,
                                 { sendRequested: sendToPaired, attachedToKiwi })
        .then(outcome => { showShareToast(outcome.kind, outcome.message) })
        .catch(e => {
          // eslint-disable-next-line no-console
          console.warn('[share-in] canvas capture outcome-announce failed', e)
          showShareToast('fail', shareFailMessage(e))
        })
    } catch (e) {
      // eslint-disable-next-line no-console
      console.warn('[share-in] canvas capture POST failed', e)
      showShareToast('fail', shareFailMessage(e))
      pendingShare.value = null
      shareBusy.value = false
    }
  }

  return {
    shareSel,
    sharePanelHits,
    pendingShare: pendingShare as Readonly<Ref<PendingShare | null>>,
    shareBusy,
    shareToast,
    onShareCancel,
    onShareConfirm,
    onAnnotateCancel,
    onAnnotateSave,
    dismissShareToast,
    /** Bind these to FrameAnnotator's v-model:attach-to-kiwi + v-model:send-to-paired so the
     *  destination toggles render on the plot-canvas annotate overlay. */
    captureDest,
  }
}
