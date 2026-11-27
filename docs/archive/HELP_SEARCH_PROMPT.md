> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in [`docs/todo/HELP_SEARCH_PLAN.md`](../todo/HELP_SEARCH_PLAN.md), written from
> this banner (H1 + H2 built 2026-11-27: guides in the jump search, an Ask Kiwi row on a miss), and in [`docs/todo/GUIDE_SYSTEM_PLAN.md`](../todo/GUIDE_SYSTEM_PLAN.md),
> [`docs/todo/GUI_LAYOUT_PLAN.md`](../todo/GUI_LAYOUT_PLAN.md) (M8) and
> [`docs/todo/CONTEXTUAL_HINTS_PLAN.md`](../todo/CONTEXTUAL_HINTS_PLAN.md).
>
> **Outcome:** not run as of archiving (2026-11-27, `origin/main` `e2e073c8a`); no
> `docs/todo/HELP_SEARCH_PLAN.md` exists. Read against main, these premises had moved:
>
> 1. **The motivating question already has a "show, not tell" answer.** The *Record a movie* guide
>    (`frontend/src/lib/guides/movies.ts`, id `record-a-movie`) walks the image ↗ → Viewer chip →
>    Share → Record movie → recorder → Record → Movies page, with its viewer steps run in the viewer
>    window (`window: 'viewer'`, `lib/guides/remote.ts`). What is missing is a route *to* it from the
>    search box: guides are reachable only via Help ▾ → Guides, What's New and the bubble's
>    "More guides" (`lib/guideOpen.ts`), and `jumpHits` has no guide kind.
> 2. **Today's jump search sends that question to the wrong place.** `matchesQuery` is every-word
>    substring over a page's label, group *and tip*, so "capture movie" and "record movie" match the
>    *Animation* page (its tip: "Capture viewer snapshots and record them as movies") — the keyframe
>    route, not the viewer's Share → Record movie. "export video" matches nothing.
> 3. **Anchor liveness (Q2) and cross-window (Q3) are solved for guides.** A step's `reveal`
>    (plan D5; several causes, first match wins) points at whatever makes an unreachable control
>    reachable, `onlyIf` drops steps this window can never show, and `window: 'viewer'` relays a step
>    to the viewer window. Building on guide steps inherits all three; a separate control registry
>    would have to re-solve them. The `__cceceliaViewer*` hooks (`Capture`, `Screenshot`,
>    `BeginDraw`) are test/capture hooks, not a navigation path.
> 4. **Half of drift check 3 already exists.** `lib/guides/guides.test.ts` ("guide anchors exist in
>    the source") fails when any step or `reveal` anchor is absent from the markup, and checks every
>    `nav:` anchor names a registered route. The other direction (every `data-guide` has help text)
>    would put ~141 distinct static anchors, plus dynamic `:data-guide` bindings
>    (`plot:${persistKey}`, `c.guide`), on a help-or-exempt list — most are guide plumbing, not
>    things a user would search for.
> 5. **The contextual-hints overlap (Q6) is closed.** That plan ran (2026-11-24) and decided clarity
>    first; P3 re-scored 0 of 9 hint targets without a visible surface, so the hint system is not
>    built. There is no hint registry for search to share.
> 6. **Runtime liveness (drift 5) has a runner but no home.** `scripts/gui_capture.mjs`
>    (`pixi run gui-capture`) drives an isolated instance, but `GUIDE_SCREENSHOTS_PLAN.md` and
>    `GUIDE_RUNS_PLAN.md` are both parked, and nothing under `scripts/judge/` drives the GUI.
> 7. **Path and detail fixes.** `guides.json` is `mcp/cecelia_mcp/guides.json` (the MCP's text
>    mirror for `get_guide`), not a frontend file — guides are defined in `frontend/src/lib/guides/*.ts`
>    (`GUIDES` in `index.ts`). `KiwiAsk.vue` is `components/kiwi/KiwiAsk.vue`; `ViewerWindow.vue` is
>    `modules/ViewerWindow.vue`. `searchRefs` lives in `utils/kiwiTurn.ts` (KiwiAsk calls it) and
>    does its own single-substring scoring, not `matchesQuery`. The "duplicate flagged in the M8
>    convention check" (Q5) was not found in PR #2144's body or comments, nor in
>    `docs/ai-assist/CONVENTION_CHECK.md`. Every canonical helper the constraints name exists
>    (`moveHighlight` in `utils/suggestInput.ts`, `useWindowListener` in `composables/useKeepAlive.ts`,
>    `TASK_STATUS`/`isLiveStatus` in `lib/taskStatus.ts`, `profileStorage`).
>
> If picked up, the smallest version that answers the motivating question is a **guide** hit kind in
> `jumpHits` (title + summary over the 29 entries in `GUIDES`) that starts the guide, with `jumpSearch.test.ts` pinning
> "record movie" / "capture movie" to *Record a movie*. Its drift guard is the existing anchor
> ratchet. Per-control `data-help` and the both-directions registry test are the larger, separate
> decision.

# HELP_SEARCH_PROMPT

Audit-and-plan prompt for Opus. Output is a plan, not code: write `docs/todo/HELP_SEARCH_PLAN.md`.

## Why

Users keep asking where things are, e.g. *"could you send me a screenshot of the button where to actually capture a movie?"*. Each answer is a manual support reply, and a screenshot goes stale when the layout moves (see GUI layout M1-M8).

PR #2144 (M8) added the header jump search (`Ctrl K` / `Cmd K`) over pages, task functions and images. Idea: extend it so a user can type *what they want to do* ("capture movie", "export video", "correct labels") and be shown the control, not told about it.

## What exists (from PR #2144; verify in the code, do not trust this)

- `frontend/src/components/JumpSearch.vue` (header, `TeleportPopover flush`, `.cc-actions-menu`)
- `frontend/src/utils/jumpSearch.ts` `jumpHits`: ranks label prefix, then page > function > image; matching via `matchesQuery` (`utils/textQuery.ts`); capped
- Task function hits open the page with the function pre-selected (`cc-fn:<module>` via `mentionTargets` in `utils/kiwiTurn.ts`)
- Guide system: `data-guide` anchors (e.g. `segment.correct`, `header.project`, `header.helpMenu`, `layout.nextStep`, `launcher.*`), `GuideBubble.vue`, `GuidesDialog.vue`, `guides.json`, `lib/guideOpen.ts`
- `lib/iconLegend.ts` (glyph meanings), Help menu (`HelpMenu.vue`), Kiwi Ask (`KiwiAsk.vue`, `searchRefs`)
- The viewer is a separate window (`ViewerWindow.vue`); the capture/Share controls live there

## Proposed direction (challenge it)

Add a fourth hit kind, **control** (and possibly **guide**), to `jumpHits`:

1. **Control registry**: label + synonyms + owning page/window + guide anchor. Prefer deriving from the existing `data-guide` anchors rather than a second hand-kept list.
2. **Action = show, not tell**: picking a hit navigates (opening the viewer window if needed) and fires the guide bubble on that anchor.
3. **Guide hits**: index guide titles; picking one opens it in `GuidesDialog`.
4. **Fallback row**: weak/no match -> "Ask Kiwi: <query>" handing the text to Kiwi Ask.
5. **Ranking**: decide where controls sit relative to pages/functions/images.

## Questions to answer in the plan

1. **Source of truth.** Can the registry be generated from the guide anchors plus a synonym map, or must it be separate? What is the cheapest design that cannot silently drift?
2. **Anchor liveness.** Anchors exist only while mounted. How do we reach controls on collapsed sections, in the viewer window, in the ⋯ set-row menu, or behind a popover? Does the guide bubble already handle "open the section first"?
3. **Cross-window.** How does a hit in the main window open the viewer and highlight a control there? Is there an existing hook (`__cceceliaViewer*`)?
4. **Synonyms.** Where do they come from? Mine real user questions (forum.image.sc, Zulip, support messages), the UI copy (`docs/ui/COPY.md`), and `iconLegend.ts`. Propose a seed list for at least: capture/record movie, export video, screenshot, share, correct labels, correct tracks, gating, channel lens, find ROIs.
5. **Matcher reuse.** `searchRefs` was not reused for pages because of its output type. Does a fourth kind justify unifying the two matchers now, or does it deepen the duplicate flagged in the M8 convention check?
6. **Overlap with contextual hints.** See `CONTEXTUAL_HINTS_AUDIT_PROMPT.md`. Can one registry feed both search and hints?
7. **Kiwi boundary.** What stays deterministic search, and what goes to Kiwi? No LLM call on the keystroke path.
8. **Disabled/locked.** Controls on disabled or project-locked pages: omit (as M8 does for pages) or show with a reason?
9. **Keyboard/focus.** Esc, arrow wrap (`moveHighlight`), `isJumpShortcut`: confirm nothing regresses.
10. **Measuring.** How do we know it helps? Log (locally, privacy-safe) queries with no hit, so the synonym list improves.

## Constraints

- Follow the repo's convention check: read `docs/ai-assist/CONVENTION_CHECK.md`, `docs/inventory/FRONTEND.md`, `docs/ui/PRIMITIVES.md`, `frontend/CLAUDE.md` first. Reuse canonical pieces (`matchesQuery`, `moveHighlight`, `TeleportPopover`, `useWindowListener`, `isLiveStatus`/`TASK_STATUS` where relevant). Name any new shared file in the inventory.
- No new backend route unless the plan justifies it.
- Persist any user-settable option (`profileStorage`).
- **Staleness is a first-class requirement, not a risk note.** Guide anchors renamed in M1-M3 show how fast this drifts. The design must make a stale or missing entry fail a check. See "Drift prevention (required)" below.

## Drift prevention (required)

The plan must specify and slice all of these:

1. **Declare at the element.** Help metadata lives on the anchor itself (e.g. `data-guide="viewer.capture"` plus `data-help="capture movie, record, export video"`), so synonyms change in the same diff as the button.
2. **Build-time scan, not a hand-kept list.** A script collects every `data-guide` / `data-help` in `frontend/src` into the registry. A new anchor with help text becomes searchable with no extra step.
3. **CI test, both directions.** Every `data-guide` anchor has help text or is on an explicit, justified exempt list. Every registry entry (including guide titles) resolves to a live anchor. A PR that adds an unregistered control or removes a still-registered anchor fails.
4. **Reviewer rule.** Add to `docs/ai-assist/CONVENTION_CHECK.md` and `frontend/CLAUDE.md`: a new user-facing button or menu item without an anchor and help entry is a finding. Propose the exact wording, and say whether the fanout audit should also flag it.
5. **Runtime liveness check.** Source anchors can exist yet be unreachable (collapsed sections, popovers, the ⋯ menu, the viewer window). Reuse the headless-Chrome capture engine (see `GUIDE_SCREENSHOTS_PROMPT.md` / docshots) to open each entry's page and assert the anchor mounts. Wire it into the weekly judge (`scripts/judge`), with failures as decide items.
6. **Learn from misses.** Log queries with no hit locally (privacy-safe, no image names or paths). The weekly judge lists the top unmatched queries so the synonym list stays current.

State which of these are slice 1 (items 1-3 should be) and which follow, and what each costs in maintenance.

## Deliverable: `docs/todo/HELP_SEARCH_PLAN.md`

1. Findings: what exists, what is reusable, what is missing (with file:line).
2. Recommended design and the alternatives rejected, one line each.
3. Registry schema (fields, example entries for the capture-movie case end to end).
4. Slices, each small enough for one PR, in order. Slice 1 should be shippable alone and cover the capture-movie question.
5. Drift prevention: the six mechanisms above, with the exact CI test, the reviewer-rule wording, the liveness-check design, and the miss-logging format.
6. Risks and open questions for the maintainer.

Do not implement. Do not change existing files except to add the plan.
