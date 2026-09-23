# Rubber-duck fit — plan

**Status:** planning (2026-09-23) · branch `rubber-duck-audit`. Drafted from
[`docs/archive/opus-audit-rubber-duck-fit-v026.md`](../archive/opus-audit-rubber-duck-fit-v026.md).
Written to be picked up cold by another session.

## Goal

Close the honest-labelling gap between what `docs/archive/kiwi-purpose-and-framing.md` promises
(Kiwi = documentation helper / passive rubber duck: reflects, organises, prompts the next
question; doesn't conclude) and what shipped v0.2.6 actually does (guardrail-injects on repeat
failure shapes, ranks by verdict-severity, exposes a `list_plots` / `mark_plot` addressing model
that stops one step short of pointing at things not currently on screen).

Two shapes of gap, addressed together:

- **Framing drift.** Shipped `PROJECT_MEMORY_PLAN` P5.2 guardrails + `bad`>`good`>untagged
  briefing order are *warning* / *curating* behaviours, not *reflecting* behaviours. Defensible
  (they solve "nobody logs failure modes, so nobody sees them next time"), but the framing doc
  doesn't acknowledge them. Fix by updating the framing, not by removing the behaviours.
- **Addressing model stops at "currently mounted".** `list_plots` + `mark_plot` (BIDIR PR #8)
  let Claude point at any panel the user has on screen. Nothing lets Claude point at a plot the
  user hasn't opened, or at a viewer position by t/z. That's exactly the "prompt the next
  question" role the duck framing gives Claude.

## Audit findings — outcome per item

Verdicts from the 2026-09-23 audit against actual code (not changelog wording):

| Gap (from prompt) | Verdict | Action |
|---|---|---|
| #1 Cross-plot `linkedTo` across panels | Partially closed (data-anchor axis shipped in PR #1165) | Residual = position-without-ID linking. Park until asked (Phase 5). |
| #1b Point at uncaptured target | Partial — mounted plots covered; unmounted plots + viewer t/z not | Build two MCP tools (Phase 2). Highest-value change. |
| #2 Landscape category-only | **Closed by shipped `LANDSCAPE_COMPLEMENTARY_PLAN` phases 1–5** | None. |
| #3 Fingerprint vs similarity retrieval | Real, deliberate v1 scope (server.py:820, guidance.py:245) | Leave parked in `PROJECT_MEMORY_PLAN` P5.2b. |
| #4 Provisional vs observed at claim level | Real. Blanket "replies are provisional" copy exists in `frontend/src/lib/claudeOverview.ts:8`, but no per-claim distinction | Prompt-side citation rule (Phase 3). |
| #5 Required-note friction | Real, deliberate (`api/src/blackboard_api.jl:73`), telemetry absent | Add tiny hook (Phase 4). Only worth doing if we'd act on the number. |
| #6 Export / methods writeup | Real, roadmap; nothing quietly depends on it | None. |

Part 2 drift check confirmed all three shipped mechanics: `_mine_guardrails` fires in every
briefing (server.py:961), `_outcome_rank` biases both open-entries and search (server.py:879),
behaviour cards/medoids render as data not as Claude synthesis.

## Locked decisions

Numbered so code and other docs can cite them (`Decision 5`).

1. **Update the framing, not the behaviours.** Guardrail-inject-on-repeat and bad/good/untagged
   ordering stay as shipped — they solve `PROJECT_MEMORY_PLAN`'s payoff problem. The framing
   doc (or its live successor) acknowledges that the shipped duck curates and warns at ≥3-hit
   thresholds, and explains why that's still duck-shaped enough.

2. **`docs/archive/kiwi-purpose-and-framing.md` stays archived, gets a companion.** The archived
   framing note is preserved as history (it's already cited from `frontend/src/lib/claudeOverview.ts`
   and its banner points at the modal as the durable design). Add a **short** companion outcome
   note under the ARCHIVED banner: "Shipped Kiwi now curates and warns on repeat traps at ≥3
   `bad`-tagged hits per fingerprint bucket, and biases briefing/search by `bad`>`good`>untagged.
   See `docs/todo/RUBBER_DUCK_FIT_PLAN.md` Decision 1." No new `docs/KIWI.md`; the modal copy
   remains the durable design surface.

3. **Two additive MCP tools, no schema fights.** `open_analysis_board_plot(project_uid, family,
   filter?)` — asks the browser (via WS push to the paired session) to navigate to and mount a
   plot the user hasn't opened yet. `seek_viewer(project_uid, image_uid, t, z)` — asks the
   viewer to move to a specific frame. Both follow the additive-write pattern (`/api/lablog`,
   `/api/notebooks/write`, `/api/chains/create` precedent from `BIDIR_CONTEXT_PLAN`
   cross-cutting constraint 2): create-only from Claude's side, recoverable, allow-listed,
   pinned by a test in `mcp/tests/test_server.py::GuidanceTest`. Neither mutates data.

4. **Delivery uses the existing bidir push channel, not a new one.** `BIDIR_PUSH_PLAN`'s
   Claude Code inbox socket (shipped 2026-09-19 via `#1048`/`#1049`/`#1051`) is what carries
   push notifications *from* Cecelia to Claude Code. For **Claude → Cecelia** navigation asks,
   reuse the WS push path that `mark_*` tools already use to reach the open browser. If no
   browser is paired for the project, the tool returns `{ok: false, reason: "no_browser"}` —
   Claude falls back to asking the user in prose. Do not build a new pairing channel.

5. **Provisional/observed = prompt rule, not UI chip.** Instead of a per-claim UI badge (which
   launders inference into "labeled inference"), the MCP guidance ratchet gets a rule: any
   numeric claim in a reply MUST cite the tool call that produced it (`get_measure_summary`,
   `get_behaviour_summary`, `get_capture_landscape_tiles`, etc.). Statements without a tool
   citation read as inference by construction. Ratchet-enforced via a `GuidanceTest` grep for
   the rule text in `guidance.py`, so it can't drift out silently. The blanket
   "replies are provisional" copy at `frontend/src/lib/claudeOverview.ts:8-9` stays as the
   product-level narration.

6. **Telemetry, if built, is a counter not a survey.** If we add friction telemetry on
   `set_blackboard_outcome`, it's a single opaque counter of `{attempted, succeeded_with_note,
   dropped_no_note}` written to a rolling file — no per-user, no per-entry, no session. If the
   counter shows a real drop-off after a month, revisit Decision 11 of `PROJECT_MEMORY_PLAN`.
   Skip entirely if we wouldn't act on the number.

7. **Position-without-ID cross-plot linking parks.** #1's residual (mark a spatial region on
   plot A → point at corresponding data on plot B by joint scale, not shared label) is not
   built and not planned. The data-ID path (`mark_cells` / `mark_tracks` fanning across 10
   families via PR #1165) covers the common case. Wait for a real user ask; don't preemptively
   build the region-projection resolver.

## Phases

Independently mergeable, ordered by ROI (framing first, addressing second, honesty rule third,
telemetry last).

### P1 — Framing update (doc-only, ~50 lines)

- Under the ARCHIVED banner in `docs/archive/kiwi-purpose-and-framing.md`, add the
  Decision-2 companion outcome note pointing to this plan.
- Amend the "replies are provisional" copy at `frontend/src/lib/claudeOverview.ts:8-9` if
  needed — a single line acknowledging the ≥3-hit guardrail behaviour ("Kiwi surfaces past
  failures that recurred ≥3× so you don't propose on top of a known trap.").
- One test in `frontend/src/lib/claudeOverview.test.ts` pinning the new line so it doesn't
  drift.
- **Ship as one PR.** Under 60 lines.

### P2 — `open_analysis_board_plot` + `seek_viewer` MCP tools (~2 PRs)

- **PR A:** `seek_viewer(project_uid, image_uid, t, z)`. Server route
  `POST /api/viewer/seek`, WS push into paired browser, viewer store dispatches to
  `stores/viewer.ts` t/z setter. MCP tool in `mcp/cecelia_mcp/server.py` + client wrapper.
  Test: `test_server.py::GuidanceTest` allow-list pin.
- **PR B:** `open_analysis_board_plot(project_uid, family, filter?)`. Server route
  `POST /api/analysis/open`, WS push, frontend routes via `moduleRouteFor(family)` +
  mounts a filter-matching panel. If no board exists, fall back to `add_analysis_board`.
  Same test pattern.
- Guidance (`mcp/cecelia_mcp/guidance.py`): add a `HOW TO POINT AT SOMETHING NOT YET ON
  SCREEN.` paragraph — reach for these tools before asking the user to open it themselves.

### P3 — Provisional/observed prompt rule (~1 PR)

- `mcp/cecelia_mcp/guidance.py` gets a `CITATION.` paragraph: any numeric claim MUST name
  the tool call that produced it, in the form `(via get_measure_summary)`. Statements
  without a citation are inference — Claude should mark them as "based on the pattern I'm
  seeing" or similar.
- `test_server.py::GuidanceTest` grep-pins the paragraph.
- No frontend change.

### P4 — Outcome-note telemetry (optional, ~1 PR)

- Add a rolling counter `settings/blackboard_outcome_telemetry.json` with `{attempted,
  succeeded_with_note, dropped_no_note}`. Incremented in `api/src/blackboard_api.jl` at the
  `set_blackboard_outcome` handler.
- Never sent anywhere, never surfaced in UI. Readable by-hand for a one-month check.
- **Skip this phase if we agree we wouldn't act on the number** — no point adding a counter
  that gets checked once and then rots.

### P5 (parked) — Position-without-ID cross-plot linking

Do not build until a real user ask. Design sketch for whoever picks it up: extend
`stores/viewer.ts` with a `plotRegionHighlight` bag keyed by `{plotId, facet, u, v}`;
each subscribing family maps u/v back to a data slice via its own frame resolver
(`frontend/src/plots/frame.ts` precedent). Effort: medium (~1 PR).

## Cross-cutting constraints (from BIDIR_CONTEXT_PLAN)

- **MCP-only, at both ends.** Every id, coord, or anchor Claude uses comes through an MCP
  tool call — never inferred from reading source. Both P2 tools comply (project_uid,
  image_uid, family are inputs; t/z are inputs).
- **Additive-write discipline.** Both P2 tools follow the `/api/lablog` / `/api/chains/create`
  create-only pattern.
- **Pull-not-push for data; push for viewer state.** P2 tools push to the viewer (same as
  `mark_*`), which is the exception the existing model allows for.

## References

- Audit source: [`docs/archive/opus-audit-rubber-duck-fit-v026.md`](../archive/opus-audit-rubber-duck-fit-v026.md)
- Framing doc: [`docs/archive/kiwi-purpose-and-framing.md`](../archive/kiwi-purpose-and-framing.md)
- Cross-cutting design: [`docs/todo/BIDIR_CONTEXT_PLAN.md`](BIDIR_CONTEXT_PLAN.md)
- Guardrail design: [`docs/todo/PROJECT_MEMORY_PLAN.md`](PROJECT_MEMORY_PLAN.md)
- Kiwi cockpit design: [`docs/todo/KIWI_PLAN.md`](KIWI_PLAN.md)
- Landscape closure (Gap #2): [`docs/todo/LANDSCAPE_COMPLEMENTARY_PLAN.md`](LANDSCAPE_COMPLEMENTARY_PLAN.md)
