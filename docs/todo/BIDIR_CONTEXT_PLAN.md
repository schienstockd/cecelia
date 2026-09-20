# Bidirectional context sharing — plan

**Status:** planning (design complete 2026-09-18, no implementation yet). Design outputs from the
audit/design run described in [`docs/archive/bidirectional-context-sharing-audit-prompt_v2.md`](../archive/bidirectional-context-sharing-audit-prompt_v2.md)
and the prior-art audit in [`docs/archive/landscape-anchor-prior-art-audit.md`](../archive/landscape-anchor-prior-art-audit.md).
Written to be picked up cold by another session.

## Goal

The Vue app functions as a shared workspace both the user and Claude bring context to and read
context from, not a one-way display the user narrates and Claude never sees. Three parts, designed
as one connected capability, shipped as several reviewable PRs:

- **Share-in** — the user shares current viewer / plot / UI context (state + pixels + drawn marks)
  into a running Claude Code session, on demand.
- **Point-out** — Claude places a marker on a data entity (track / cell), a UI element, a grid
  cell / landscape candidate, or a freeform region — using the same drawing primitive as share-in,
  the other direction.
- **Blackboard** — a versioned Mermaid + notes space for concepts developed over time, distinct
  from the executable chain whiteboard, gated on the existing Claude Code setup check.

The connective purpose is bigger than the three parts individually: this is the foundational
**visual grounding layer** for a broader "Claude Imaging" direction — the analysis-side counterpart
to what Anthropic's Model Hardware Standard (MHS, 2026-08-27) does upstream on acquisition. Context
in [`docs/archive/bidirectional-context-sharing-audit-prompt_v2.md`](../archive/bidirectional-context-sharing-audit-prompt_v2.md)
→ *The bigger frame this sits inside*.

## Cross-cutting constraints (non-negotiable)

- **MCP-only, at both ends.** Every id, coord, or anchor Claude uses at runtime comes through an
  MCP tool call — never inferred from reading the Vue / Julia source. Cecelia is intended to run
  on cloud VMs where Claude Code has no code access; any design that "happens to work" only because
  Claude peeked at the source will silently break there.
- **Additive-write discipline.** New endpoints follow the `/api/lablog` / `/api/notebooks/write` /
  `/api/chains/create` pattern: create-only, recoverable, allow-listed, pinned by a test in
  `mcp/tests/test_server.py::GuidanceTest`. Nothing new mutates viewer state, gates, populations,
  or existing analysis data.
- **Pull, not push, for share-in.** Matches the existing MCP model (`poll_observations` is a pull
  tool fed by a background listener). Point-out uses WS push into the already-open viewer, since
  the target is a live surface.
- **One drawing primitive, four id-anchor renderers.** Freeform marks (both directions) use one
  `DrawSurface` component. Data / UI / grid anchors each reuse an existing well-fit renderer
  (highlight bags / `GuideBubble` / `GridOverlay`). Not four rendering variants of a single
  component.
- **Every new entity is referenceable from the others it's related to.** Captures reference to
  Blackboard entries reference to plan docs / lab log / chains. No entity uses an addressing
  shape that differs from an existing one.
- **Maintainability standard applies from the outset** ([`docs/MAINTAINABILITY.md`](../MAINTAINABILITY.md)
  landed 2026-09-18): typed params, one canonical helper per job, comment/docstring rules,
  structure register. This plan's new files/seams are exactly the kind of addition that standard
  exists to keep legible.

## Locked decisions

Numbered so code and other docs can cite them (`Decision 5`).

1. **Three parts, one primitive, shipped as separate PRs.** Design everything together (this
   document); implement in a seven-PR sequence (below) so review capacity can be planned against
   independently-mergeable slices — repeat the mistake PR #915 corrected.
2. **The viewer is a pop-out window (`modules/ViewerWindow.vue` at `/viewer-window`, owns the
   single WebGPU canvas).** Two capture triggers: main-window button captures UI / plot; pop-out
   button captures viewer / slab. No cross-window ImageData shuttle. If the pop-out isn't open,
   viewer-surface captures are unavailable — say so, don't build a fallback that reopens.
3. **Share-in reuses `__cceceliaViewerCapture()` (`modules/ViewerWindow.vue`).** Extend the
   payload with: `buildViewState()` output verbatim as a sidecar (not a new "cameraState"
   invention), a `viewerPropsRef` id for durable per-image settings if pinning them matters,
   selection state (from existing bags), and the structured drawing overlay. Do NOT re-invent
   a `cameraState` / `channelSetups` field — `buildViewState()` (`utils/viewer/viewState.ts`) is
   the atomic snapshot primitive; `viewerProps` (`utils/viewerProps.ts` + `/api/viewer/props`) is
   the durable-settings sidecar.
4. **Four capture surfaces, one auto-detect by last-focused surface.** UI region (main-window
   DOM), plot (main-window canvas, carries plot spec + data refs), viewer frame (pop-out canvas),
   viewer slab (pop-out canvas + Δt / Δfov). Hybrid = "also include surrounding UI context"
   toggle on any capture.
5. **The context slab (share-in extension).** Optional on viewer captures. Two composable
   toggles: `± N frames` (default 3, cap 20) for temporal; `+25 % FOV margin` for spatial.
   Explicitly NOT the whole movie / whole image at native res — that would be a separate
   grid+landscape MCP call, not this capture. Wire format: short `.mp4` reusing `movie_io.stitch_movies`
   for temporal slabs; MCP tool result gets one image gallery + a link.
6. **Capture address = the citation.** `{projectUid, imageUid?, valueName?, t?, z?, extentUm?,
   domAnchor?, plotSpec?}`. Minimal, no view-mechanic leakage. MHS-adjacent (expressible as a
   future state-dictionary reference) without depending on MHS today.
7. **Share-in storage: `<proj>/captures/`.** Parallels `<proj>/notebooks/` and `<proj>/settings/chains/`.
   Not under `<proj>/settings/` — captures are content, not settings. Per-project registry at
   `<proj>/settings/captures.json` (id → `{title?, address, createdAt}`), same shape as
   `settings/notebooks.json`.
8. **Shared drawing primitive extraction.** `DrawSurface.vue` + `drawGeometry.ts` extracted from
   `ImagePickerModal.vue` (pointer state + `NormRect` + geometry helpers) and `GateOverlay.vue`
   (corner / edge / vertex hit-testing). New: freehand stroke via `pointermove` → polyline +
   Douglas-Peucker decimation. Coordinate discipline: steal `StillOverlay.vue`'s
   `object-fit: contain` invariant wholesale.

   **Amendment 2026-09-19 — mark palette + composite.** The original ship gave every mark the
   same accent-colour stroke and stored marks as a colourless vector overlay next to a bare
   frame PNG. Two failures the moment a user tried to POINT at things: "red circle vs green
   circle" was inexpressible, and `get_capture(id)` returned a frame WITHOUT the marks (Claude
   saw pixels + colourless geometry). Fix: `OverlayMark.color` is a palette-name slot
   (`magenta | cyan | yellow | white`); server safelists to those four (any other value is
   dropped) so a tampered payload can't smuggle CSS. On save, DrawSurface's strokes composite
   onto a copy of the frame canvas *before* `toDataURL` (`utils/overlayCompose.ts`) — the
   assistant reads pixels-with-marks. Palette is CVD-safe (deutan / protan / tritan) and
   microscopy-neutral (none of the four is a common fluorophore emission). The plan's earlier
   rejection of RASTER paint still stands — this is vector + composited, not raster.

   **Amendment 2026-09-19 — re-annotate + trinity (Kiwi PR B).** Once the user shares a frame,
   the discussion with Claude often needs MORE strokes ("look here too", "and this one over
   there") to disambiguate. Ship: (i) DrawSurface mounts as a peer on `CaptureViewSurface`, so
   the frozen-frame view supports adding new marks. Save composites new marks OVER the already-
   composited frame and POSTs a NEW capture with `previousCaptureId` linking to the original —
   additive-only shape, no mutate. (ii) Kiwi's captures list gains thumbnails (backend already
   serves the composed PNG; row shows a 2.4-rem `<img>` at `object-fit: contain`) and a "refocus"
   button that reuses the analysis-board's Zoom-to-source mechanism —
   `stores/viewer.ts::setPendingViewState` + `openViewerWindow(...)` — so the popup restores the
   exact camera / channels / t / z the capture was written at (from `viewStateSnapshot`), or
   nudges t / z on a legacy capture without one. (iii) The "refined from" glyph on refined rows is a visual only;
   the parent copy button's tooltip carries the human phrasing so the nested-tooltip ratchet
   stays green.
9. **Marks are structured overlay data alongside the frame, not baked into pixels.**
   Round-trippable, editable, Claude reasons about the geometry not just visually, and Part 3's
   Claude-placed marks share the exact schema so the layer is truly one primitive.
10. **Feijoa sketch style is the visual reference for `DrawSurface` defaults.** Not a new visual
    vocabulary — check the sibling repo's established stroke / color / marker language before
    locking defaults. Open item — see *Open items* below.
11. **Data-anchor point-out reuses the existing highlight bags** (`stores/viewer.ts`).
    `trackHighlight` narrows ribbon render per track ids; `pickHighlight` shader-outlines picked
    labels with `focusId`. New MCP tools feed these bags server-side via a new WS `viewer:mark`
    outbound frame; the bags themselves need no changes. Two intentional bags, not one — do not
    collapse; the render paths differ (narrow render vs shader outline).
12. **UI-anchor point-out reuses the guide-anchor scheme** (`data-guide="<area>.<control>"` +
    `nav:/route`), resolved through `utils/guideAnchor.ts::resolveAnchor()`. `GuideBubble.vue`
    gets a bare "point" variant (no walkthrough progression) as the point-out UI.
13. **Grid/landscape anchors ship as a standalone Vue feature** (viewer toggle + on-request
    landscape button), MCP tool as one consumer. Not a hidden backend-only pass. Grid default
    8×8, cells `A1..H8` (spreadsheet-style, speakable), user-configurable density.
14. **Landscape = cheap tile-level heatmap over the shown frame, not a segmentation** (2026-09-20
    reframe — the earlier Cellpose-SAM / μSAM / SAM defaults were wrong; those are the
    "just run the real pipeline" case the prompt explicitly warned against, and if segmentation
    exists it's already reached via *data anchors*). The landscape's job is to hand Claude a
    rough semantic prior BEFORE it reads the RGB — "there's dense signal top-left, sparse
    bottom-right" — so it can navigate a frame it has no other context for. Per grid cell,
    compute cheap statistics (mean/max intensity per channel, local variance, edge density) on
    the shown frame's pixels; k-means (4–6 clusters) over the tile-vectors labels each tile
    with a human-readable category (`dark` / `bright-uniform` / `bright-textured` / `edge` /
    `mixed`). No models, no GPU, no segmentation of any kind in this feature category.
15. **Landscape correlation check on Cecelia data** (replaces the earlier SoM+SAM /
    SoM+μSAM / SoM+Cellpose-SAM eval — those region sources are out of scope per Decision 14).
    Small internal eval on 2–3 frames from `zolIMa` and `jFWePN`: does the k-means labelling
    match what a domain expert would call the tile? Not a segmentation metric; just does the
    cheap heatmap agree with an eyeball at tile resolution. Executed as part of PR #6.
16. **Landscape pass compute budget: < 100 ms at native res on the browser's already-decoded
    frame.** 64 tiles × ~4 stats × N channels + one small k-means is trivial. If it exceeds
    this, the tile stats are the wrong ones (over-computing) — cut them, don't fall back to
    "plain grid + Claude squints at pixels" (that IS the current state; the landscape has to
    add something).
17. **Freeform marks default target = most-recent shared capture,** with a soft warning if the
    capture is > 15 min old ("this capture is old, are you sure you meant the current viewer?").
    Alternative: `target: "live_viewer"` for the currently-open frame.
18. **Marker lifecycle: 5-min TTL + per-mark "pin".** Server-side expiry; pin removes the TTL
    for the long-review case; explicit dismiss chip per-marker + "clear all Claude marks".
19. **Board-plot subscription (per point-out).** Gating module page plot components subscribe
    to `trackHighlight` + `pickHighlight` bags. A track / cell marked in the viewer highlights on
    every open board plot showing it. Visual distinction between user selection and Claude mark
    is mandatory (color + small "C" glyph); the two must never look identical.
20. **Blackboard storage: dir-per-entry** at `<proj>/blackboard/<entryId>/`, containing
    `entry.md` (Markdown + Mermaid fences), `meta.json`, `.snapshots/<entryId>@v<N>.md`,
    `attachments/<captureId>.json` (link records, not copied captures). Per-project registry at
    `<proj>/settings/blackboard.json` mirrors `settings/notebooks.json`.
21. **Blackboard versioning reimplements the notebook shape locally.** Below rule-of-three for a
    shared component extraction (only two consumers). Name the convention in
    [`docs/MAP.md`](../MAP.md); trigger extraction when a third consumer appears. Fix the
    "restore loses un-snapshotted edits" papercut in Blackboard (auto-snapshot the current live
    state before restore); notebooks can pick this up later if it proves out.
22. **Blackboard pruning is a manual button.** Two-click confirm, keeps `current`, deletes the
    rest. No age / count / scheduled / configurable policy. Mermaid + text entries are ~1–5 KB
    per version; measure before automating. Mirror `NotebookTable.vue`'s Restore + Prune inline
    UX pattern exactly.
23. **Blackboard lab-log companion write — DROPPED (2026-09-20).** Originally: Claude-authored
    revisions auto-write a `[Claude]` line to the lab log; human edits stay silent. Not shipped —
    the Blackboard entry already carries the same prose, so a lab-log echo is redundant. The
    Blackboard is the record for a Blackboard change; the lab log stays the record for what
    happened to the *analysis*.
24. **Blackboard visibility gated on `observerSetupReason(available, lastFailedAuth) === null &&
    state === 'current'`** — same helper the lab-log install / login band and Settings → MCP
    connections use. Reuse, don't build a second detector.
25. **Blackboard vs `docs/todo/*_PLAN.md` "Locked decisions" boundary.** Blackboard = working
    history (ideas evolving; can be wrong). Plan doc Locked decisions = settled outcomes (facts
    to build against). Manual graduation — user copies the settled point out of a Blackboard
    entry into the plan doc, referencing the entry id. No auto-copy.
26. **This design is orthogonal to the OBSERVER phased plan** ([`docs/ai-assist/OBSERVER.md`](../ai-assist/OBSERVER.md)).
    OBSERVER Phase 2 (Designer) shipped; Phase 3 (Analyst) not touched; the plan is silent on
    pixel / visual grounding. Bidir is a new orthogonal axis (visual grounding) — do not amend
    OBSERVER.md; this plan is the reference.
27. **MHS positioning.** MHS is upstream (acquisition) and this design is downstream (analysis
    perception). Addressing scheme is *expressible* in a future MHS state-dictionary reference
    without designing to MHS today. Do not couple to MHS data shapes; do not assume acquisition
    metadata will arrive via MHS.

## Audit summary — what Part 1 found

Compact record of what was audited. Full findings live in the fork transcripts referenced from the
session record; retained here as the design's evidence base.

- **MCP surface (`mcp/`).** 28 read tools + 7 additive writes, allow-listed. `guidance.py` =
  single tool catalogue (both `SERVER_INSTRUCTIONS` = short entry rules and `BRIEFING_GUIDANCE` =
  long working rules). `GuidanceTest` (`mcp/tests/test_server.py`) fails on any unlisted tool.
  Design mantra baked in: *"DESIGN work, don't START it."*
- **Guide/pointer system.** `data-guide="<area>.<control>"` DOM attrs + `nav:/route` for sidebar
  items. Anchor id existence test-enforced (`frontend/src/lib/guides/guides.test.ts`).
  `utils/guideAnchor.ts::resolveAnchor()` handles multi-candidate ranking (reachable > active
  panel > unoccluded > DOM order), occlusion, scroll-container clipping, scroll-into-view.
  `GuideBubble.vue` renders bubbles. Test-enforced rule *"a guide points and observes — never
  clicks / navigates / runs anything"* directly aligns with the ephemeral-marker discipline.
- **Viewer state ownership.** `modules/ViewerWindow.vue` owns the single WebGPU canvas.
  Atomic snapshot via `utils/viewer/viewState.ts::buildViewState()`. Cross-window state bus:
  `stores/viewer.ts` (localStorage-bridged).
- **Still-frame capture already shipped**: `window.__cceceliaViewerCapture()` returns
  `{png, extentUm, imageUid, valueName, overlayLayers}`. Overlays baked by shader — what the user
  sees is what the caller gets. Share-in extends this, doesn't reinvent.
- **WS event stream.** Generic (`broadcast_ws(msg::Dict)`) — already carries non-task frames
  (`boards:changed`, `notebooks_changed`). Adding `viewer:mark` outbound is one dispatch case.
  Viewer state does NOT flow through WS today — but share-in uses HTTP + pull, not WS push, so
  this doesn't matter for share-in.
- **OBSERVER phased plan.** Phase 1 (observe) + Phase 2 (Designer, PRs #250–#258) shipped.
  Phase 3 (Analyst) not touched. Silent on pixel / visual grounding. Deliberately unwired:
  `submit_task`, `adjust_params`, `acknowledge_flag` (architectural: WS `chain:run` has no HTTP
  route, MCP is HTTP-only).
- **Claude Code setup check.** `observerSetupReason(available, lastFailedAuth) → 'missing' |
  'auth' | null` + `terminalCta(available, state)` in `frontend/src/utils/observerSetup.ts`,
  reading state from `GET /api/observer/status`. Reusable AS-IS.
- **Correction cockpit highlight.** MIXED. Viewer ↔ cockpit UNIFIED via two intentional bags in
  `stores/viewer.ts` (`trackHighlight`, `pickHighlight`). Viewer ↔ board plots NOT unified —
  plot components don't subscribe. Two-bag design deliberate (different render paths); do not
  collapse. Plot subscription is PR #4 work.
- **Freeform drawing / markup.** No general freeform layer exists. `ImagePickerModal.vue` +
  `utils/crop3d.ts` = pointer-driven rect draw over server z-MIP; header comment explicitly
  says "box2d, point, line and polygon can slot into the same modal." `GateOverlay.vue` = full
  interactive rect + polygon editor with corner / edge / vertex hit-testing. `StillOverlay.vue` =
  SVG playback overlay with `object-fit: contain` invariant. Raster brush for label correction
  is a *different mechanism* (Phase 4 label correction) — do NOT conflate. Extraction is
  partial: freehand stroke capture is the small genuinely-new bit.
- **Notebook versioning + pruning.** Storage: `<proj>/notebooks/.snapshots/<stem>@v<N>.jl` + a
  per-project `settings/notebooks.json` registry. Create auto-snapshots v1; revise
  auto-snapshots-then-overwrites; explicit snapshot bumps N; restore overwrites live but does
  NOT bump `current` and does NOT re-snapshot. Pruning is a MANUAL button, not a policy (audit
  prompt overstated this). Extraction ROI marginal — below rule-of-three; Blackboard
  reimplements the shape locally.

## Part 2 — share-in (design)

**Trigger + surface.** Capture button + shortcut in the pop-out (viewer + slab captures) and in
the main window (UI + plot captures). When triggered, the last-focused surface freezes, the
drawing overlay appears, user marks with pointer, then Share or Cancel.

**Auto-detection.** A small `lastFocusedSurface` in `useViewerStore` writes on every meaningful
focus event (viewer canvas mouse events, plot canvas mouse events, main-window panel focus). The
capture button reads it and picks the surface. Hybrid toggle in the pre-share overlay bumps to
"also grab surrounding UI." No 3-way menu per capture.

**Captured payload shape.**

```
{
  captureId, createdAt,                            # stable id, promotable to Blackboard
  surface: "ui" | "plot" | "viewer_frame" | "viewer_slab",
  address: {                                        # the citation
    projectUid, imageUid?, valueName?,
    t?: int | [int, int],                          # single frame or slab range
    z?, extentUm?,                                  # for viewer surfaces
    domAnchor?: "<area>.<control>",                 # for UI/plot
    plotSpec?: { specId, params, dataRefs }         # for plot
  },
  frames: [{ png, overlayLayers? }],                # single for frames, N for slabs
  overlay: [ { kind: "circle"|"arrow"|"rect"|"poly"|"stroke", geom, label? } ],
  viewStateSnapshot?: <buildViewState output>,      # sidecar, optional
  viewerPropsRef?: <id/hash>                        # per-image durable settings pin
}
```

**Backend endpoint(s).** `POST /api/viewer/capture` writes to `<proj>/captures/<captureId>.{json,png}`
(or `.mp4` for slabs). `GET /api/viewer/captures?limit=N` lists newest-first. `GET /api/viewer/capture/<id>`
reads one. No delete, no mutate. Additive-only, allow-listed.

**MCP tools.**
- `get_recent_captures(limit=10)` → list of `{captureId, createdAt, surface, address}`
- `get_capture(captureId)` → full payload including image content blocks

**Guidance addition** (into `BRIEFING_GUIDANCE`, per `GuidanceTest`):

> The user can share what's on screen — a viewer frame, a plot, a UI region — with marks drawn on
> it. Check `get_recent_captures` when they say "look at this" / "I just shared something" / when
> your last message asked them to point at something. Each capture carries an address (image / t /
> z / extent / dom anchor / plot spec) so you don't have to ask "which image" — read it.

**Fits the OBSERVER arc how?** It doesn't sit inside the phased plan (Decision 26). This is a
parallel arc — visual grounding — that adds no autonomous-action capability (no `submit_task`) and
therefore doesn't disturb existing phase boundaries.

## Part 3 — point-out (design)

Five target classes, five MCP tools. Not one tool with a `target_type` param — separate tools
keep each lifecycle explicit and let `guidance.py` teach Claude when to reach for which.

**Data anchors — track.** Reuses `stores/viewer.ts::setTrackHighlight` directly.
- MCP: `mark_tracks(image_uid, value_name, track_ids: [string], focus_id?: string, label?: string, ttl_s?: 300)` → `{markerId}`
- Backend: `POST /api/viewer/marks/tracks` → writes in-memory `viewerMarks` bag → `broadcast_ws({kind: "viewer:mark", ...})`
- Frontend: `stores/ws.ts::dispatch()` new case → routes to `setTrackHighlight` + a new `viewerMarks` store for the ephemeral marker overlay (with label balloon)

**Data anchors — cell / label.** Same shape via `setPickHighlight` with `focus_id`.
- MCP: `mark_cells(image_uid, value_name, label_ids: [int], focus_id?: int, label?: string, ttl_s?: 300)`
- Backend: `POST /api/viewer/marks/cells` → same WS frame kind

**UI anchors.** Reuses `data-guide` scheme + `resolveAnchor()` + `GuideBubble.vue` (bare "point" variant).
- MCP: `point_at_ui(anchor_id, label?: string, ttl_s?: 300)`
- Backend: `POST /api/viewer/marks/ui` → WS frame
- Anchor id sources: from a shared UI capture's `domAnchor` field, or `nav:/<route>` (known convention)

**Freeform marks.** Same `DrawSurface` primitive as Part 2, running the other direction.
- MCP: `mark_freeform(target: {captureId | "live_viewer"}, overlay: [{kind, geom, label?}], ttl_s?: 300)`
- Default target: most-recent capture (Decision 17)
- Two coordinate modes: image-space (µm) for viewer targets; screen-space for UI targets

**Grid / landscape anchors.**
- Grid overlay: `frontend/src/utils/gridOverlay.ts` (pure geometry, tested) + `components/GridOverlay.vue`
  (SVG over the viewer canvas, respects `object-fit: contain`), toggled from viewer panel.
  User-configurable density (Decision 13); default 8×8 with `A1..H8`.
- Landscape overlay = **cheap tile-level heatmap** (Decision 14 reframe): per-tile stats over
  the shown frame's pixels (mean/max intensity per channel, local variance, edge density) →
  small k-means (4–6 clusters) → each tile gets a human-readable category label
  (`dark` / `bright-uniform` / `bright-textured` / `edge` / `mixed`). Translucent tile fills
  by cluster (CVD-safe palette), a small legend chip in the viewer panel names the categories.
  Toggleable, on-request only, sits alongside GridOverlay.
- MCP: `get_landscape(image_uid, t, z?, grid?: {rows: 8, cols: 8})` →
  `{grid, tiles: [{id: "B3", label, channels: {…}, stats: {…}}], legend: [{label, swatch, n_tiles}]}`
- MCP: `mark_tile(image_uid, t, cell_id, label?, ttl_s?)` (renamed from `mark_landscape` — you're
  marking a grid TILE now, there is no "candidate" object to mark).
- Standalone Vue feature first (Decision 13); MCP is one more consumer of the same computation.
- **No SAM / μSAM / Cellpose-SAM / any segmentation model in this feature** — that would
  duplicate the data-anchors path and blow the compute budget (Decisions 14, 16).

**Ephemeral by default.** All markers share Decision 18's lifecycle: 5-min TTL + per-mark pin +
explicit dismiss chip.

**Board-plot linkage.** Per Decision 19, gating-module plot components subscribe to
`trackHighlight` + `pickHighlight`. Ships as part of PR #4; may split as PR #4b if per-plot
subscription work grows.

**Grid/landscape false-confidence framing.** Grid cells rendered translucent with speakable labels
(`A1..H8`), never as authoritative segmentation. Landscape tile fills are *categorical*
(k-means-cluster colour), not intensity or per-pixel — the visual makes the tile-level nature
obvious at a glance. Legend names the categories in words (`bright-textured`, not `cluster 3`).
Framing pinned in the Vue component default styling, not just docs.

## Part 4 — Blackboard (design)

**Storage** (Decision 20): `<proj>/blackboard/<entryId>/{entry.md, meta.json, .snapshots/, attachments/}` +
per-project registry at `<proj>/settings/blackboard.json`.

**MCP tools.**
- `create_blackboard_entry(title, content_md, attach_capture_ids?: [string])` → `{entryId}`
- `revise_blackboard_entry(entry_id, content_md, note?: string)` → `{version}` (snapshots current, then overwrites)
- `read_blackboard_entry(entry_id, version?: int)` → `{title, content_md, version, updatedAt, attachments}`
- `list_blackboard_entries()` → `[{entryId, title, current, updatedAt}]`

**Guidance addition** (into `BRIEFING_GUIDANCE`):

> ON BLACKBOARD. You can create / revise a Blackboard entry — Mermaid diagrams + notes for
> concepts developed over time. Distinct from a chain (executable, needs the user to Run) and a
> notebook (analysis code the user opens and edits). Create when a shared idea is worth keeping
> across sessions; revise snapshots first, so nothing is lost. Attach captured frames by id when
> the visual is load-bearing. Nothing here starts work.

**Frontend surfacing.** New `/blackboard` page using `ModulePage.vue` shell. Gated per
Decision 24. Renders Markdown via `marked` (already used by `lib/whatsNew.ts`) + `mermaid.js` for
```` ```mermaid ```` fences. Version list UI mirrors `NotebookTable.vue`'s Restore + Prune inline
pattern.

**Ship 2026-09-19 — backend + MCP first, Vue page held for eyeballing.** `api/src/blackboard_api.jl`
lands with the full CRUD/versioning shape (create + revise + restore + prune + delete + list + read
at a version). Storage is per-project `<proj>/blackboard/<entryId>/{entry.md, meta.json,
.snapshots/entry@v<N>.md}` + registry at `<proj>/settings/blackboard.json`, mirroring the
notebooks shape (Decision 21 — reimplement locally, no shared versioning helper before rule-of-
three fires). MCP exposes `list_blackboard_entries`, `read_blackboard_entry`,
`create_blackboard_entry`, `revise_blackboard_entry` (server + client + guidance + allow-list;
restore / prune / delete stay off the MCP surface, matching notebooks). Restore also snapshots
CURRENT before restoring, fixing the "un-snapshotted edits vanish" papercut called out in
Decision 21. Vue page ships in a follow-up PR (Markdown + Mermaid rendering is untestable via
unit tests and wants a browser eyeball).

**Ship 2026-09-20 — Vue page** (`modules/BlackboardModule.vue`). Route `/blackboard`, nav entry in
the Analysis group (Analysis board → Blackboard → Notebooks — the two writing surfaces sit next to
their canvas). NOT gated on the observer/MCP connection (a bad-connection day still renders every
diagram). Two-pane: entry list left, one entry right (viewer OR editor). Mermaid is
dynamic-imported only when the current entry contains a ```mermaid fence — zero cost on entries
without diagrams. Utils: `utils/blackboardApi.ts` (typed fetchers), `utils/blackboardMd.ts`
(`renderBlackboardMarkdown` + `mermaidBlocks`, tested). Store: `stores/blackboard.ts` (WS
`blackboard:changed` → tick → silent list reload).

**Ship 2026-09-21 — Refocus collapses into Zoom-to-source (one mechanism).** Earlier attempts wrote
a bespoke `viewerSeekChannel` (BroadcastChannel-based) — that shipped briefly then got called out
as a duplicate: the analysis-board's Zoom-to-source already had the localStorage-backed handoff
(`stores/viewer.ts::pendingViewState`) and could apply full view state on the popup, opened or
not. Collapse: (i) `ViewerWindow.onDrawSave` populates `viewStateSnapshot` on the capture at share
time via `buildViewState`; (ii) `PendingViewState` grew optional `overlay: {captureId, marks}` +
`focus: {t?, z?}` fields, plus an optional `imageUid` filter — the ViewerWindow's existing
pendingViewState apply path branches: full restore (Zoom-to-source / modern capture Refocus) OR
seek-only (`focus`, for legacy captures without a snapshot); the overlay sidecar sets `activeMarks`
in both branches. (iii) Kiwi Refocus + Blackboard attachment click both do `setPendingViewState`
+ `openViewerWindow(...)` — same as `ImageStripView.zoomToSource`. `viewerSeekChannel.ts`
+ tests deleted. Attachment thumbnails now composite the marks via
`overlayCompose.ts::composeImageWithOverlay` (the same fix `Kiwi PR A` applied for shared frames).
Page shell rebuilt to mirror ChainModule (full-height flex, canonical `SelectionTable` for the
list, `ConfirmDeleteButton` for delete).

**Versioning + pruning.** Reimplement notebook shape locally (Decision 21). Fix the "restore loses
un-snapshotted edits" papercut in Blackboard.

**Lab-log companion write.** Dropped — see Decision 23. Redundant with the entry text itself.

**Ship 2026-09-20 — reannotate inherits `viewStateSnapshot`.** `CaptureViewSurface` gained a
`viewStateSnapshot` prop and forwards it on the re-annotate POST (alongside `previousCaptureId`)
so a refined capture carries the same camera / channels / t / z the original share was framed on.
A later Refocus on the refined capture then takes the full-restore branch, not the seek-only
fallback. Two files (`ViewerWindow.vue` populates + binds the prop, `CaptureViewSurface.vue`
plumbs it into the fetch body).

**BIDIR Part 4 status: shipped.** All Blackboard PRs (#1066 backend + MCP; #1070 Vue page;
#1072 versioned attachments + no-op skip; #1073 restore annotation overlay; #1074 canonical
primitives + composited thumbnails; #1078 Refocus collapsed into Zoom-to-source; #1079 draggable
list divider; reannotate-viewstate inherit) landed 2026-09-20.

## Cross-piece linkage

Every new entity references the entities it's related to, using stable ids not prose.

```
                        Capture (Part 2)
                      ↗    ↑         ↖
                     /      \         \
             mark_freeform  attach    (referenced by)
             (Part 3)         │            │
                              ↓            │
                        Blackboard entry (Part 4)
                        ↓            ↓            ↑
                   Lab-log       *_PLAN.md    Chain / Notebook
                (Claude-authored  Locked
                 auto-write)      decisions
                                                  
             Mark (Part 3 — data / UI / grid / freeform)
                    ↑                    ↓
             existing bags         viewer / GuideBubble / GridOverlay
             + gating plots
             (subscribe to bags — Decision 19)
```

| Reference | Held as |
|---|---|
| Blackboard entry → capture | `attachments/<captureId>.json` |
| Blackboard entry → chain | text reference by chain uid, resolved on render |
| Blackboard entry → plan-doc decision | text reference by plan path + section |
| Lab-log entry → Blackboard entry | text reference by entryId |
| Blackboard revision → lab-log | Claude-authored only (Decision 23) |
| Chain / notebook / plot → Blackboard entry | text reference by entryId |
| Capture → Blackboard entry | via `attach_capture_ids` on create |
| Mark → capture | freeform via `target: {captureId}` |

## PR sequence (phased build)

Independently mergeable in this order. Each ships a working, tested slice.

1. **Annotation drawing state primitive (pure).** `utils/drawGeometry.ts` — the state machines
   for rect drag, polygon click-add + close-by-proximity, and freehand stroke (with a
   Ramer-Douglas-Peucker simplify pass), plus the click-vs-drag / polygon-degeneracy primitives
   moved out of `plots/gateGeometry.ts` (re-exported there for `GateOverlay`'s existing imports).
   Coord-agnostic — the caller decides what a pixel means. Tested (`utils/drawGeometry.test.ts`).
   **No Vue component this PR** — inventing a `DrawSurface.vue` ahead of a real host would fix
   its rendering choices (SVG vs canvas, stroke style, save/cancel semantics) against no
   requirement. Component lands with PR #3, where capture annotation is the first host.
   **`GateOverlay.vue` untouched** — flow-cytometry gating is a different engine (canvas-2D in
   data-coords, 8-variant edit-handle system, own downstream to `gating/{value_name}.json`); the
   ACTUAL code overlap with the annotation pathway is ~5 lines of rect drag and the
   click-vs-drag helpers, which this PR centralises in one place. Claude-driven gate authoring
   is a valid future capability but goes through the gating engine, not this primitive.
   ~150 lines new + ~25 moved.
2. **Grid overlay as standalone viewer feature.** `utils/gridOverlay.ts` + `components/GridOverlay.vue` +
   viewer-panel toggle. User-facing regardless of any Claude session. ~200 lines.
3. **Share-in.** Extends `__cceceliaViewerCapture()` payload; `POST /api/viewer/capture` + list /
   read routes; `get_capture` + `get_recent_captures` MCP tools; guidance addition. **Adds
   `DrawSurface.vue`** — the Vue component that renders the annotation overlay for capturing
   drawn marks. Uses PR #1's `drawGeometry.ts` as its state layer. ~500 lines (was going to be
   ~350 lines of share-in + ~150 lines of DrawSurface, previously counted under PR #1).
4. **Point-out data anchors + gating-plot linkage.** WS `viewer:mark` frame;
   `POST /api/viewer/marks/{tracks,cells}`; `mark_tracks` + `mark_cells` MCP tools; frontend
   `viewerMarks` bag + ephemeral overlay; **gating-module plot components subscribe to
   `trackHighlight` + `pickHighlight`** (Decision 19). ~600 lines. May split as PR #4b if
   subscription work grows.
5. **Point-out UI anchors + freeform marks.** `mark_ui` + `mark_freeform` MCP tools; bare-style
   `GuideBubble` variant; freeform overlay reuses PR #3's `DrawSurface.vue`. Shares WS
   `viewer:mark` transport with PR #4. ~400 lines.
6. **Landscape overlay + `get_landscape` / `mark_tile` MCP tools** (rescoped 2026-09-20 —
   Decision 14 reframe). Cheap tile-level heatmap over the shown frame's pixels:
   `utils/landscape.ts` (pure — per-tile stats + k-means, tested), `components/LandscapeOverlay.vue`
   (SVG fills + legend, sits alongside `GridOverlay`), viewer-panel toggle, MCP surface, small
   correlation eval on `zolIMa` / `jFWePN` (Decision 15). Depends on PR #2 (grid). No SAM /
   Cellpose / any segmentation — those are out of scope for this feature category. ~300 lines
   (well down from the earlier ~600, because SAM/Cellpose/eval-of-three-models are gone).
7. **Blackboard.** `<proj>/blackboard/` storage + local versioning helpers; CRUD MCP tools;
   `/blackboard` Vue page gated on Decision 24; mermaid lazy-load. ~700 lines.

Dependencies: PR #4 and #5 share the WS `viewer:mark` transport (defined in #4, reused in #5).
PR #6 depends on PR #2. Nothing else cross-depends.

## Verdict (from the design run)

The four elevated questions, answered with real conviction:

1. **Is the MCP-tool shape right, or would clipboard-paste get 80 % of the value?** MCP tool.
   Clipboard-paste is 30–40 % — it gives Claude pixels without state, without the drawing
   markup that IS the disambiguation value, without a path to plot data, without a return
   direction for point-out.
2. **Did the drawing layer end up as one primitive across all four anchor types?** One
   drawing primitive plus three specialized renderers — deliberate. Data anchors via existing
   highlight bags; UI anchors via `GuideBubble`; grid anchors via `GridOverlay`; freeform marks
   via `DrawSurface`. Forcing all four into one component would have been worse.
3. **For Cecelia's actual use cases, does routing ROI-finding through Claude add real value
   over a classical detector?** Only for ad hoc, language-specified, one-off criteria — build
   the language-flexible path (grid+landscape), park a "classical detector library" as
   explicitly out of scope. For definable, repeated criteria, a classical detector wins on
   speed and reliability, and Cecelia's exploratory-intravital-microscopy bread-and-butter is
   precisely the case Anthropic's own MHS launch called out as poor fit for executor mode.
4. **Where does this rank vs the maintainability audit's issues?** Structural work is
   higher-ROI, lower-risk, compounds silently. This design is visible and exciting. Run in
   parallel through different PRs — the audit already landed 2026-09-18, so it's no longer
   competing scheduling.

Lower-stakes but plain:

- **MCP/observer architecture resistance to this addition?** None — additive-write shape is
  exactly the established pattern.
- **Does "foundational primitive for Claude Imaging" framing hold?** Yes, reinforced by MHS.
  Frame as "the analysis-side counterpart to what MHS does upstream" — don't inflate beyond
  what this design actually delivers.
- **Does the design assume MHS-shaped upstream metadata?** No. Every id / coord is Cecelia's
  own state; MHS-adjacent, not MHS-specific.
- **Is the guide/pointer system a good fit to extend for Part 3?** Yes — `data-guide` +
  `resolveAnchor()` is exactly right. Bare "point" `GuideBubble` variant is the small
  extension.
- **Correction cockpit highlight: one shared implementation or two?** Two intentional bags.
  Not a unification job — the render paths differ.
- **Does Blackboard overlap enough with `*_PLAN.md` Locked decisions to make maintaining both
  busywork?** No as designed. Blackboard = working history; plan doc = settled outcomes.
  Manual graduation, no auto-copy.
- **Lab-log / Blackboard / plan-doc boundary workable?** Yes. Claude-authored-only auto-write
  keeps it thin.
- **Did the design end up connected?** Yes — see cross-piece linkage table above. Every new
  entity references its neighbours by stable id.
- **Grid/landscape false-confidence risk?** Real. Mitigation pinned in the Vue component
  default styling (translucent grid, categorical tile fills with named legend, speakable ids),
  not just docs. Reframed 2026-09-20 (Decision 14) — the landscape is a cheap tile heatmap, not
  a segmentation, which drops the "SAM regions look like cells but aren't" failure mode entirely.
- **UI/viewer/hybrid + plot-as-third-surface cover the real cases?** Yes.
  Auto-detect-by-last-focused-surface works because every relevant surface is well-defined.
- **Notebook versioning generalize?** Not extracted (Decision 21 — below rule-of-three).
  Reimplement locally; fix the restore-loses-edits papercut in Blackboard.
- **Anything MCP-only-violating in the proposed design?** None found. Every id and coord
  comes from existing MCP reads or from a Part 2 capture. The design holds on a remote-VM
  deployment.
- **PR sequence independently mergeable?** Yes, per PR list above. PR #4 and #5 share a WS
  frame kind defined in #4; #6 depends on #2; nothing else cross-depends.

## Open items

Not blocking the design — resolve during their PRs.

- **Feijoa sketch style spec.** Check the sibling repo's stroke / color / marker language
  before locking `DrawSurface` defaults. If richer than a color pick, may affect PR #1 rendering
  work. (Decision 10.)
- **Mermaid dep verification.** Confirm `mermaid` in `frontend/package.json` or lazy-load as a
  new dep on the `/blackboard` page only. Not blocking; PR #7 concern.
- **Landscape correlation check.** 2–3 frames from `zolIMa` / `jFWePN` — does the k-means
  tile labelling match a domain-expert eyeball at tile resolution? Not a segmentation metric;
  just "does the heatmap agree." Dominik's time, not code time (Decision 15).
- **Gating-plot subscription line-count in PR #4.** May exceed the per-PR budget; PR #4b split
  possible.

## References

- Design run source (archived, do not act on directly): [`docs/archive/bidirectional-context-sharing-audit-prompt_v2.md`](../archive/bidirectional-context-sharing-audit-prompt_v2.md)
- Prior-art audit (grid overlay / SoM / μSAM / SBEMimage / etc.): [`docs/archive/landscape-anchor-prior-art-audit.md`](../archive/landscape-anchor-prior-art-audit.md)
- Cross-cutting Cecelia rules: [`CLAUDE.md`](../../CLAUDE.md), [`docs/MAINTAINABILITY.md`](../MAINTAINABILITY.md)
- Where things live: [`docs/MAP.md`](../MAP.md)
- MCP surface: `mcp/`, guidance in `mcp/cecelia_mcp/guidance.py`
- Existing viewer state: `frontend/src/modules/ViewerWindow.vue`, `frontend/src/utils/viewer/viewState.ts`, `frontend/src/stores/viewer.ts`
- Guide/pointer system: `frontend/src/utils/guideAnchor.ts`, `frontend/src/lib/guides/`, `frontend/src/components/GuideBubble.vue`
- Correction cockpit highlights: `frontend/src/stores/viewer.ts` (`setTrackHighlight`, `setPickHighlight`)
- Notebook versioning to mirror: `api/src/notebooks_api.jl`, `frontend/src/components/NotebookTable.vue`
- Observer arc (adjacent, not this): [`docs/ai-assist/OBSERVER.md`](../ai-assist/OBSERVER.md)
- MHS positioning: `~/Downloads/prompts/claude-imaging-pitch.md` (framing input, external to the repo — not authoritative)
