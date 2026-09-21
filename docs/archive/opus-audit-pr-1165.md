**ARCHIVED — audit prompt.** This file is not authoritative; it is the Opus-audit prompt for PR
#1165 (plot point-out rollout, merged 2026-09-22) and the plot-id resolution follow-up. The
follow-up recommendation lives at `~/Downloads/prompts/list-plots-mcp.md` for a future session.

# Audit: PR #1165 (plot point-out rollout) + plot-id resolution gap

## Context

PR #1165 rolls the point-out consumer (`PlotPointOutMark`, `mark_plot`) out to
every remaining plot family, on top of the gating-scatter pilot in #1163.
Frontend/backend tests pass (3598/3598, 1142/1142) but the PR is
**untested in a browser** and flags several reservations. Separately, using
`mark_plot` in practice still requires the human to supply the exact
`plot_id` by hand — there's no mechanism yet for Claude to resolve "the UMAP
on cluster-tracks" to an id on its own.

**Reference the design doc:**
[`docs/todo/BIDIR_CONTEXT_PLAN.md`](https://github.com/schienstockd/cecelia/blob/feat/plot-pointout-rollout/docs/todo/BIDIR_CONTEXT_PLAN.md)
(status: planning, design complete 2026-09-18). PR #1165 sits under Part 3
(point-out) / PR #4 ("point-out data anchors + gating-plot linkage," split
into #4a shipped / #4b open). Treat this doc's Locked decisions as the
constraints to audit against — in particular:

- **Decision "MCP-only, at both ends."** Every id/coord Claude uses at
  runtime must come through an MCP tool call, never inferred from reading
  source — Cecelia is meant to run on cloud VMs where Claude Code has no
  code access. Any plot-id resolution design that only works because Claude
  peeked at the Vue source is out.
- **Decision 6 — "Capture address = the citation."** The existing
  `{projectUid, imageUid?, valueName?, t?, z?, extentUm?, domAnchor?,
  plotSpec?}` shape from share-in captures is the closest existing
  resolvable-id mechanism. Check whether it already covers plot
  identification (via `plotSpec`) before designing something new.
- **Decision 19 / PR #4b scope note.** The plan explicitly says #4b needs
  to teach `InteractivePanel`, cluster panels, and gating-page panels to
  subscribe to `trackHighlight`/`pickHighlight` — confirm #1165 is that
  work (or a superset of it) and not a divergent parallel path.
- The plan's own **Open items** and **Verdict** sections list what was
  explicitly deferred — check nothing in #1165 quietly reopens a decision
  that was locked (e.g. one-drawing-primitive-plus-renderers, ephemeral
  5-min TTL + pin lifecycle, additive-write-only endpoints).

Do both of the following.

## Part 1 — Review PR #1165

Read the diff at `feat/plot-pointout-rollout` against `main` and check:

1. **Frame math per family.** `UmapView` (letterbox + subFrame per facet),
   `ClusterHeatmapPanel` / `ClusterHmmStatesPanel` / `ClusterHmmTransitionsPanel`
   / `SummaryPanel` (axis-rect path), `StripCell`, `GateScatterCell`. Confirm
   each family's `:style` positioning actually resolves to the plot's data
   area, not the axis gutter — this is exactly what reservation #1 says is
   unverified.
2. **Reservation #2** — `hmm-transitions` faceted plot exposes only the outer
   axis rect. Confirm this is a real limitation (not fixable trivially now)
   and that it's documented somewhere a future dev/agent will see it before
   calling `mark_plot(family='hmm-transitions', cell=...)`.
3. **Reservation #4** — the drop rule ("`cell` on a single-cell family is
   dropped; no `cell` on a faceted family is dropped"). Verify this is
   enforced consistently across all families and fails *silently* vs with a
   clear error — silent drops will look like a Claude/MCP bug later.
4. **Reservation #5** — `InteractivePanel` forwarding `plot-id`
   unconditionally. Confirm no view breaks by receiving an unexpected prop,
   and that this doesn't collide with any existing `plot-id`-named prop.
5. **`usePlotResize` / no-self-loop claim.** Spot-check that the tick
   "render" genuinely doesn't write into the observed element for at least
   one Observable-Plot family (not just the ratchet test).
6. Anything in the MCP `mark_plot` docstring / `guidance.py` that's now
   stale or incomplete relative to the actual family list.

Flag anything you'd block merge on vs. ship-and-follow-up.

## Part 2 — Design the plot-id resolution step

Today: human reads a plot_id off... nothing — they have to already know it,
or copy it from somewhere. That's the missing link before `mark_plot` is
actually usable hands-free. **BIDIR_CONTEXT_PLAN.md does not solve this** —
its discovery mechanism is capture-driven (share-in gives Claude an address
via a prior capture), not a standing registry of currently-mounted plots.
Confirm that reading before proposing a fix, then design one that's
consistent with the plan's locked decisions above (MCP-only, additive-write,
one-drawing-primitive-plus-renderers).

Evaluate and recommend between (or combine):

1. **`list_plots` registry tool** — since `InteractivePanel` already
   forwards `persistKey` as `plot-id`, expose a live registry: every
   currently-mounted plot's `plot_id`, `family`, human-readable title,
   route, and bounding rect. Claude calls this before `mark_plot` to resolve
   a natural-language description to an id.
2. **Id labels in the screen capture** — overlay small `plot_id` tags in
   each panel's corner in whatever capture path Claude uses to "see" the
   page, so the id can be read directly off the image instead of inferred.
3. Any simpler alternative you'd prefer (e.g. deterministic id generation
   from route + family + position, so Claude can construct it without a
   round-trip at all).

For your recommended approach, sketch:
- Where the registry/labels would live in the existing Vue/MCP wiring from
  this PR (what hooks into `persistKey`/`plot-id` already give you for free).
- How it disambiguates duplicate families (e.g. two `hmm-states` panels on
  the same route).
- Rough scope: is this a follow-up PR on top of #1165, or does it belong in
  the same rollout?
- Where it fits in the plan doc's own PR sequence / decision numbering (a
  new locked decision + PR #8, or folded into #4b?).

## Output

A written audit: merge-blocking issues, ship-and-follow-up issues, and a
concrete recommendation (with rough implementation sketch) for the plot-id
resolution step.
