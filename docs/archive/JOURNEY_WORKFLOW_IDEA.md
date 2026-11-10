> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in [`docs/todo/GUI_CLARITY_PLAN.md`](../todo/GUI_CLARITY_PLAN.md); the review this
> asked for is [`docs/todo/JOURNEY_WORKFLOW_REVIEW.md`](../todo/JOURNEY_WORKFLOW_REVIEW.md) (draft).
>
> **Outcome:** reviewed 2026-11-10 (`origin/main` 293cb038), verdict **adopt in part**. Written with
> another model in a design chat with the maintainer; the two mocks it cites (now+, journey bar) and
> the three painting photos the palette comes from are not in the repo. Read against main:
>
> 1. **The now+ row is mostly the audit's own slices** (1, 3, 5, 7, 8, plus 11 for colour), plus three
>    small pieces: a recipe stored per set feeding `#next`, a viewer chip, and a plain message when a
>    pop-up is blocked (today `popout.ts` fails silently).
> 2. **The journey bar is held** until now+ has been tried on users; if built, its stages are the
>    existing `navGroups` sidebar groups, behind a layout switch.
> 3. **"The data picks what shows" already exists as recipes**, but the pick is forgotten when the
>    Guides dialog closes. The review recommends lighting pages from a per-set recipe, not hiding
>    pages, which would conflict with View Profiles D2 and the map's "no default path". This framing
>    came from the maintainer's chat, not from this file.
> 4. **The mocks break five locked rules and fail AA twice** (saffron plots and gate dots, coloured
>    Run/Next fills, saffron Kiwi, a worded maturity badge, plum/lavender buttons at 4.0–4.3:1) and
>    retint every surface warm, which is a theme change rather than a ≤10% accent.
> 5. **"Viewer: popout only" is recorded here as a maintainer decision.** It is not yet in the plan.
>
> Decisions N1–N8 in the review's §6 are open.

# JOURNEY_WORKFLOW_IDEA

**Status:** loose idea, not a spec. **Model:** Opus. **Mode:** think, challenge, assess. No code changes.
**Output:** `docs/todo/JOURNEY_WORKFLOW_REVIEW.md`, short (about 2 pages) plus a recommendation.
**Mock:** https://claude.ai/artifact/BHWnHgBaQA5RyXkhgUdYdT
- Row 1, "Later: journey bar": Segment and Populations with a top journey bar and no sidebar.
- Row 2, "Now+: sidebar kept, audit slices applied": Welcome, Segment and Gate on the classic layout with the audit's fixes.

**Context:** `GUI_CLARITY_PLAN.md` and `GUI_CLARITY_AUDIT.md`. The audit keeps the sidebar (depth is not the problem:
every page is at most 2 clicks away) and fixes within pages. This idea asks a narrower question: once those fixes land,
is replacing the sidebar with a journey bar worth it? Treat it as a possible later shell swap, not a competing first step.

## The idea in one paragraph

A **journey bar** across the top replaces the permanent sidebar: the analysis stages in order (Import, Check, Correct,
Segment, Populations, Track, Figures), each with a one-line status rolled up from its checks. The page under it is the
module's work area. The viewer is **never docked**: it is always a pop-out window reached from a **viewer chip** in the header.
Every page has the same skeleton, and every stage owns checks you can act on. The feel is a sketch: light surface, firm skeleton.

## Built on the audit's slices

The journey bar needs the audit's building blocks, so it should come after them, not before:

- slice 4: spacing and chrome tokens on the shared shells (the lighter feel)
- slice 5: one panel-section primitive, one Run button, the `#next` slot, `ModuleLayout` empty state
- slice 7: `CheckButton` and the checks cluster in the action bar
- slice 8: a real Welcome page

Both mock rows use the same pieces. The only structural difference is **sidebar vs. journey bar** as page navigation.
Everything else (checks cluster, `#next`, panel layout, tokens, colour) is shared.

## Skeleton (shared by both rows)

1. **Header:** logo, project crumb, viewer chip (open/closed + image), jump box (Ctrl K), one worded Help, Ask Kiwi (stays visible with AI off).
2. **Navigation:** sidebar in journey order with stage hue dots (now+), or the journey bar (later).
3. **Action bar:** image selector with its menu on the left, the **checks cluster** on the right (Acquisition, Cohort, "n QC-flagged", Claims).
4. **Work area:** a result strip (what just happened, "Overlay shown in the viewer", Focus viewer, plus actions like Correct labels), then plots and tables.
5. **Task panel:** function picker opening on the recipe's first step, a "When to use" line, few visible params, a Run on switch, one full-width Run, More options collapsed, sections separated by dividers.
6. **`#next` slot** at the bottom of the panel, with its source ("From recipe Cell tracking", or "from what has run on these images" when no recipe is picked).

## Checks: owned by the stage, actionable, advisory

- A check has one contract: name, state (ok / flagged / not run), a one-line reason, and **one action** (fix, rerun, accept with a note, open detail).
- Each stage declares its checks; the checks cluster renders them with the one check accent. The audit's inventory (24 checks, `p3_buried.md`) is the starting point.
- Severity changes **prominence, not gating**. Pre-run checkpoints stay advisory and never block (audit decision). A must-interrupt check is the loudest flag, with its action beside it.
- In the journey-bar version, a stage's status line is a roll-up of its checks, so the bar doubles as a project health strip, and clicking a flagged stage lands on the flagged check.
- Accepting a flag records why, which feeds the claim record and can carry into figures and reports.

## The viewer: popout only (maintainer decision)

No docking, no split. The cost is that results can land out of sight, so:

- the viewer chip is always present; one click opens or focuses the window (the viewer window already has 7 entry points, so this consolidates them)
- every push to the viewer is acknowledged on the page ("Shown in the viewer") and offers Open viewer when closed
- remember window size and position; a shortcut to bring it forward; one-click snap-to-half for single monitors
- a blocked pop-up must produce a plain message, never silent failure

## What I want Opus to think about

1. **What does the bar add?** Over sidebar + `#next` + checks cluster + view profiles, what does a top journey bar actually improve, and for whom? Give an honest case against it.
2. **Stages vs. pages.** There are 24 routed pages; sidebar order is already close to the journey. Could stages simply group the existing `navGroups` catalogue (still the single source for sidebar, view profiles and guide map)? Where do a stage's modules live (second row of tabs, a picker, recipe-driven)?
3. **Linear bias and "no default path".** A horizontal bar implies a path, and analysis loops back. How does it stay a map? Does it conflict with the guide map's decision to light nothing by default?
4. **Pages that don't fit.** Chains, Settings, Tasks, Blackboard, Notebooks, model training. Same skeleton, or declared exceptions?
5. **Popout-only risks.** Single monitor and laptop screens, browser pop-up rules, the Tauri shell, remote or cloud sessions. Is there a cheaper trick that keeps the see-it-on-the-image loop?
6. **Checks.** Which of the 24 are actionable now, which need a new action, and what happens when many flags stack? How does the cluster collapse on narrow windows?
7. **Migration.** Can it ship behind a layout switch so the old sidebar remains? What is the smallest slice that tests it on real users?
8. **How would we decide?** Propose a cheap comparison of the two mock rows (for example, silent first-run walkthroughs of import to gating) that would settle whether the bar is worth building.
9. **Alternatives.** Rate at least two: keep the sidebar but lighten it (the audit's path); whiteboard-as-home; a hub of image sets. Say when each wins.

## Constraints

- The app stays dark; palette is the desert-earth direction at roughly 10% of a screen or less. Viewer and plot areas stay neutral.
- Cecelia works fully without Claude; AI stays an optional layer under the toggle.
- Reuse canonical shared components and tokens; no new primitive without evidence nothing fits (the audit proposes only `SplitPage`).
- Status never relies on colour alone; WCAG AA on the dark theme.
- Existing design is not gospel. Challenge this idea too.

## Output

A short review: verdict (adopt later, adopt in part, or drop), the strongest case for and against, a line or two on each question above,
the smallest first slice if adopted, and the decisions needed from D. Use repo evidence where possible and label anything inferred.
