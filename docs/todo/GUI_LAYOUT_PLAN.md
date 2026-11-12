# GUI layout — build the final mocks for both windows

**Status:** built, awaiting the maintainer's look on a real GPU (2026-11-12). Every slice merged (#2142–#2163, V2 landing inside #2163); the playback bar was then tightened to two fixed rows with a fold (#2183). Open: M7's "to review" count (what it counts is undecided); V0 left the Debug section and the render core in the window. Follows [`GUI_CLARITY_PLAN.md`](GUI_CLARITY_PLAN.md): its 12
slices landed the colour tokens, Next, recipes, the checks cluster and Run wording, but most of the
mocked *layout* did not land (the helper icons were never consolidated, the launchers still sit in the
sidebar, the viewer kept its single sidebar). This plan builds the layout.

**The spec is the mock:** [`gui-layout/mock_final.html`](gui-layout/mock_final.html), rendered as
`gui-layout/mock_final_1.png` … `_5.png` (frames 1 Segment, 2 Gate, 3 Welcome, 4 Viewer View mode,
5 Viewer Edit slot; frame 6, the menus strip, is in the HTML). A slice is done when its screenshot on
an isolated instance sits next to its mock frame and matches, not when the code merges.

## Decisions

1. **Simplify by moving and disclosing, never by cutting a function** (maintainer, 2026-11-11). Every
   control in today's build gets a home in the tables below. A slice that removes a control without a
   row there is wrong.
2. **The viewer is a viewer: no checks cluster** (maintainer, 2026-11-11). QC flags stay inside its
   Segmentation section, as today.
3. **Gating controls live on the plot canvas, never in a right panel** (maintainer, 2026-11-11). Pages
   with no task (Gate) have no right panel; Next becomes a footer in the same corner.
4. **The viewer keeps its histograms** and every channel control (maintainer, 2026-11-11).
5. **The correction cockpit is redesigned from scratch, in a separate plan** (maintainer, 2026-11-11).
   This plan only reserves the slot: Edit is a viewer mode with an alpha line, a banner that says what
   is queued and that nothing is written until Apply, and an empty right panel the new design fills.
   The cockpit keeps working from its launcher's new home until the redesign replaces it.
6. **`ViewerWindow.vue` is split along the seams the layout moves** (maintainer, 2026-11-11). See
   § Splitting the viewer.
7. **A jump search goes in the header** (maintainer, 2026-11-11): Ctrl K, pages / functions / images.
   The one new feature in this plan; everything else is a move.
8. **Accordions open one section by default** (maintainer, 2026-11-11): the sidebar's groups and
   Settings' sections. A mock drawing every group open shows content, not fold behaviour.
9. **Default plots are declared on the plot spec** (`defaultOn: [page]`, M6): a page's canvas places
   them once, when first seen empty; a removed default stays removed.
10. Supersedes `GUI_CLARITY_PLAN` Decision 17's "the mocks are not fixed": the final mocks are the
   layout spec. The journey bar stays parked.

## Today → new home

### Main window

| Today | New home |
|---|---|
| Compass (guides), key (icons), palette (colours) | Help ▾ · Learn |
| Megaphone (call for datasets), GitHub, Zulip | Help ▾ · Get help |
| Logo → What's new | Help ▾ · What's new (logo goes to Welcome) |
| Page maturity dot + "Something wrong?" | dot beside the page in the sidebar; report in Help ▾ |
| Profile chip | avatar menu |
| "dev" + version + Connected | console row; connection shown only when it drops |
| Update badge | stays in the header, only when there is one |
| Hamburger (hide menu) | stays left of the wordmark |
| Launcher: Viewer | viewer chip in the header (click = open / bring to front) |
| Launcher: Correction cockpit | "Correct labels" on Segment/Track + viewer Edit mode |
| Launcher: Lab log | bell in the header (dot = unseen) |
| Launcher: Kiwi | Ask Kiwi in the header (visible with AI off: it is how AI is turned on) |
| Footer: Settings | stays, bottom of the sidebar |
| Footer: Quit, Log out, Restart backend | avatar menu |
| Project row + ⋯ (switch project, sidebar layout) | header crumb ▾; layout in Settings |
| Set ID + copy | set menu |
| CSV, Task ▾, Max | ⋯ in the set row |
| Task list in the right panel | "Last run · Tasks" line + Tasks page (watch and cancel stay one click away) |
| Floating Populations panel | stays on the canvas; collapsed on pages with no populations yet |

### Viewer window (nothing removed)

| Today | New home |
|---|---|
| Image name, version chip | header crumb + provenance chip |
| Kiwi @ button | Ask Kiwi (hidden with AI off, as today) |
| Shortcuts ? | panel footer + Help ▾ |
| 2D/3D, Pan/Select | top of the panel (mode switch View / Select / Edit) |
| Minimap, axes gizmo, reset view | on the image (top right) + ⟳ in the panel |
| Advanced popover (renderer, cache, quality, projection, maps) | ⚙ in the panel |
| Debug section | ⚙ Advanced → Debug |
| Plane / depth / ± planes, Level, Play, Loop, scrub, cache strip, Fps | playback bar on the image |
| Integrated-GPU warning, loading / bricks / error chips | console row (errors that break the view stay on the image) |
| Channels: master row, cards (LUT, eye, auto, reset, histogram + contrast), AF preview A/B, "N of M" | Channels section, unchanged |
| Segmentation, Populations, Tracks, ROIs, Annotations | same sections, one open at a time, unchanged |
| Scale bar + timestamp, landscape, grid, solo stepper, ROI spotlight, crop bar, preview box | on the image, unchanged |
| Draw surface, capture view | Share ▾ → Draw and note (today only startable from the main window) |
| Screenshot / capture hooks | Share ▾ (Save image, Add to board, Send to Kiwi); the `__cceceliaViewer*` hooks keep their names |

## Splitting the viewer

`frontend/src/modules/ViewerWindow.vue` is 7,437 lines: script 29–5839, template 5841–7136, style
7138–7437, 449 top-level bindings. Splitting it is worth it **where the layout moves code anyway**: an
extracted block can be moved by changing its parent, and a slice's diff stays reviewable. Splitting
the render core (fetching, pumping, tile mode, pointer, crop) buys nothing for this plan and risks the
renderer, so it waits.

Each split is its own PR, **behaviour-neutral**, and lands before the slice that moves the block.
Proof of neutrality: typecheck, the full vitest suite, and a pixel-identical screenshot of the window
before and after on an isolated instance.

| Block (today's lines) | Becomes | Moved by |
|---|---|---|
| Sidebar sections: Annotations, Channels, Segmentation, Populations, Tracks (6435–6920) | `components/viewerWindow/Vw*Section.vue`, props down, events up; ROIs is already `RoiSection.vue` and stays it | V3 panel |
| Debug section (6929–7135) + Advanced popover (6133–6214) | `VwAdvanced.vue` | V3 panel ⚙ |
| Z / level / time / fps controls (6280–6434) + playback script (3131–3215) | `VwPlaybackBar.vue` + `useVwPlayback.ts` (V0: the loop is `useVwPlayback.ts`, the time rows `VwTimeControls.vue`) | V2 playback bar |
| Status chips (5947–6010) | the console row: reuse `ErrorConsole.vue` (the main window's docked bar) if it can carry loading / bricks / GPU state; a viewer-only strip only if it cannot, with the reason in the PR | V2 console row |
| Share-in draw + capture hooks (5312–5839) | `useVwShare.ts` | V4 Share |

Source-scanning tests name `ViewerWindow.vue` and must follow the moved code:
`utils/continuousControls.test.ts`, `utils/viewerOverlays.test.ts`, `utils/keepAlive.test.ts`,
`utils/sfcParses.test.ts`. Persistence keys stay the same: `cc.vw.section`,
`viewerWindowSideCollapsed`, the per-image viewer props.

Later and optional, not part of this plan: tile fetching (2592–3130), pointer (3216–3761) and crop
mode (3786–4588) as composables, once the layout is in.

## Slices

Sizes: S under a day, M one to two days, L more. Each slice has its own PR, its row group from the
tables above as a checklist, and a before/after screenshot beside its mock frame.

| # | Slice | Frame | Size | Watch out for |
|---|---|---|---|---|
| M1 | Header: Help ▾, bell, Ask Kiwi, avatar menu, viewer chip, crumb | 1, 6 | S–M | 13 guide anchors (`sidebar.viewerCta` ×8, `sidebar.kiwiCta` ×5, …) re-pointed in the same PR; observer `point_at_ui` targets; `scripts/gui_capture.mjs` |
| M2 | Sidebar: text only, groups open, launchers and footer out | 1 | S | the collapsed sidebar is icons only, so icons stay there; the Preferences → Layout launcher toggles go |
| M3 | Set row (Set ▾, recipe chip, counts, checks, ⋯) + result line | 1 | M | the result line reads existing task history |
| M4 | Right panel: When to use closed, task list → Last run line | 1 | S | watching and cancelling a run stay one click away |
| M5 | Next footer on pages with no task panel | 2 | S | — |
| M6 | Default plots per page | 1, 2 | M–L | new mechanism plus a default for each of the 18 ModuleLayout pages |
| M7 | Welcome: counts, relative dates, "to review", recipe cards | 3 | S–M | per-project counts may need a small backend field |
| M8 | Jump search in the header (Ctrl K): pages, task functions, images of the open project | 1 | M | new feature, not a move (maintainer, 2026-11-11); reuses `navGroups.ts`, the task catalogue and the image list, no new backend route; Ctrl K must not clash with an existing shortcut |
| V0 | Split the viewer (table above), behaviour-neutral | — | M | pixel-identical before/after |
| V1 | Viewer header: ← back, image ‹ 1 of 3 ›, provenance chip, Help ▾, Ask Kiwi | 4 | M | bringing the main window to the front from the popout needs a fallback |
| V2 | Playback bar on the image + console row | 4 | M | the riskiest viewer slice; needs the maintainer's eyes on a real GPU |
| V3 | Panel: mode switch View/Select/Edit, 2D/3D, ⟳, ⚙ | 4 | S–M | sections unchanged inside |
| V4 | Share ▾ | 6 | S–M | hooks keep their names; Draw and note gets a button in the window |
| V5 | Edit slot: alpha line + banner, empty panel | 5 | S | the cockpit still opens from "Correct labels" |

Order: M1, M2 (largest visible win, low risk) → M3–M5 → M8 → V0 → V1, V3, V4, V5 → M7 → M6 and V2 last.

## Will it break anything

Nothing is rewritten, so behaviour should not change. What can break is anything that **points at a
control by name**: guide anchors, observer `point_at_ui` targets, `gui_capture.mjs`, the viewer's
window hooks, persistence keys and source-scanning tests. Each slice greps for its anchors and updates
them in the same PR. The viewer slices cannot be judged in headless Chromium: the maintainer checks
them on a real GPU before they merge.
