> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in [`docs/todo/GUI_CLARITY_PLAN.md`](../todo/GUI_CLARITY_PLAN.md) (parked).
>
> **Outcome:** not run as of archiving (2026-11-10, `origin/main` 71509170). Read against main, six
> premises had moved:
>
> 1. **PrimeVue and Aura are already gone.** "Deliver as tokens in the shared theme (replacing PrimeVue
>    Aura)" has nothing to replace: `frontend/package.json` has no `primevue` or `@primeuix/*`, and the
>    only traces of Aura are a comment in `frontend/src/style.css` and the `primevueRetired.test.ts` guard. The tokens are the
>    `--cc-*` custom properties in `style.css` (54 defined; about 1,600 `var(--cc-)` uses across `.vue`
>    files, against 228 hard-coded hex literals). Read with `utils/cssTokens.ts`. Ignore the
>    `PRIMEVUE_RETIRE_PLAN.md` status header, which still reads "in progress". Phase 5's "what the
>    PrimeVue retirement will change" therefore asks about the past.
> 2. **"Light" means the feel, not a colour scheme (maintainer, on archiving).** "Light and loose"
>    is about density and weight: less chrome, more air. Dark stays; no light theme is wanted.
>    `App.vue` hard-codes `cc-dark` on the shell, and `cc-light` exists only as a capture scope for
>    card export (`CardsPanelInner.vue`). So the brief's "WCAG AA in light and dark" and "dark/light
>    behaviour" lines drop out: check contrast on the dark theme only.
> 3. **The global AI toggle is built** (`AI_TOGGLE_PLAN.md`, 2026-11-08: default off, surfaces hidden).
>    `AI_SURFACES_AUDIT.md` already has the inventory of AI surfaces. Phase 1's AI column and Phase 4's
>    "AI elements that should vanish" are a check against that registry, not new findings.
> 4. **Some named docs have different names.** `HARDENING_AUDIT.md` is `docs/todo/HARDENING_AUDIT_PLAN.md`.
>    `INVENTORY.md` is the root index, and the families are in `docs/inventory/FRONTEND.md`.
>    `VIEW_PROFILES_PLAN.md` is in `docs/todo/`. Also read `WORKFLOW_RECIPES_PLAN.md` and
>    `GUIDE_SYSTEM_AUDIT.md`, which the brief does not list.
> 5. **"One spine along the journey" contradicts an existing decision.** The in-app map
>    (`GuideMap.vue`, `WORKFLOW_RECIPES_PLAN.md` → *The map*) shows nothing lit by default on purpose:
>    "there is no default path, it depends on the data and the question". A picked recipe lights its
>    pages instead. The map also already carries maturity dots per page and the ✦ Claude toggle. A
>    spine has to be a per-recipe spine, or reopen that decision. Default sidebar order is already
>    roughly journey order (`lib/navGroups.ts`: images → preprocess → cleanup → segment → gate/track/
>    cluster → phenotype/behaviour/spatial → board/notebooks/movies → tasks/chains).
> 6. **Two methods conflict with how this machine is run.** "Run the app" means the maintainer's
>    running instance: agents never start or kill servers on 8080/5173. The repo has no route
>    screenshot harness (no Playwright). "Check telemetry" will find no UI-usage telemetry, so every
>    "rarely used" claim is "unknown" unless the observer's capture log counts as usage. The census
>    needs either the maintainer's screenshots or a capture harness built first, and that harness is
>    a code change, which an audit-only run may not make. `scripts/ui_copy_inventory.mjs`
>    (`pixi run ui-copy`) already does the string/terminology census for Phase 5.
>
> **Added by the maintainer on archiving: consistency is a goal in its own right.** The new layout
> must be consistent and functional. Today some parts feel unconnected, with completely different
> layouts and feel from one page to the next. Any run should add an explicit cross-page pass: the
> same page skeleton (header, controls, work area, output), the same panel and section components,
> and the same placement of run, status and next-step everywhere. Name the pages that break it and
> the shared component that should carry each part. Rank this alongside "where do I click now?" and
> "buried feature", not under Phase 5 polish.

# GUI_CLARITY_AUDIT_PROMPT

**Model:** Opus. **Mode:** audit only. No code changes, no new features.
**Output:** `docs/todo/GUI_CLARITY_AUDIT.md` (+ screenshots in `docs/todo/gui-audit/`).

## Why

Main UX, structure and AI integration have landed. The GUI has grown dense.
Goal: Cecelia reads as a structured, clean, clear image analysis platform.

Two failure modes to eliminate:

1. A user asks "where do I click now?"
2. A user misses an important feature (acquisition checks, claim checks, QC) because it is buried.

## Feel target

The GUI should feel **light and loose**, not dense. This is a visual goal as much as a structural one.
Treat density as perceived, not just counted: spacing, borders, dividers, type weight, saturation,
nested panel chrome and simultaneous visible controls all contribute.
Guiding image: a sketch. Keep it light, keep it loose, but with a firm skeleton underneath.
Loose surface (calm, airy, minimal chrome), rigid structure (consistent layout, predictable positions,
a clear journey, nothing important hidden, never lost). Judge every proposal against both halves:
does it lighten the surface without weakening the structure?
Prefer fixing it centrally (tokens, shared panel/section components) over per-screen tweaks.

## Colour target

Add a bit more playful colour. Calm warm neutral surfaces; colour carries meaning and orientation, not decoration.
Palette direction: warm desert-painting earths and fires (ochre/saffron, burnt sienna, vermilion, coral/blush,
deep plum/aubergine, cream, near-black brown). Take the palette only, never motifs or patterns from the source art.
Constraints:

- colour covers a small share of any screen (rough ceiling ~10%); the work area stays calm
- viewer and plot areas stay neutral so channel pseudo-colours and data colours are never confused with UI colour
- audit current token usage first; propose roles (stage hues along the journey, check-status colours, maturity-tier colours, selection/focus, empty states, welcome page), not a free-for-all
- status must not rely on red vs green alone; check contrast (WCAG AA) in light and dark
- checks (acquisition, claims, QC) get a distinctive, consistent accent so they cannot be missed
- playfulness lives in empty states, progress, welcome and toasts; workflow controls stay quiet
- deliver as tokens in the shared theme (replacing PrimeVue Aura), with before/after screenshots for 3 key screens

## Read first

`CLAUDE.md`, `INVENTORY.md`, `VIEW_PROFILES_PLAN.md`, `docs/todo/FEATURE_MATURITY_PLAN.md`,
`docs/todo/PRIMEVUE_RETIRE_PLAN.md`, `HARDENING_AUDIT.md` (if present), the AI surfaces audit (if present).
Do not duplicate their findings; reference them and flag conflicts.

## Method (do all phases; check in after Phase 2)

### Phase 1 — Surface census (measured, not impressions)

Run the app. For every route, sidebar group, tab, modal, drawer and floating panel record:

- depth: clicks from the welcome page
- control count (buttons, inputs, selects, toggles, tabs) visible on first paint vs. after expansion
- entry points: how many distinct ways reach it
- maturity tier and whether the badge is shown
- AI-related elements (feeds the global AI toggle)
- screenshot at default window size

Produce one table. Rank screens by control count and by depth.

### Phase 2 — Journey walk ("where do I click now?")

Walk each journey as a first-time user, using only what the UI shows:

1. Import a new image set
2. Check acquisition / image quality
3. Correct (drift, denoise, versions)
4. Segment, then cleanup
5. Define populations / gate
6. Track, then behaviour clusters
7. Build a figure / analysis board / report
8. Build a chain and run it on a batch
9. Ask Claude/Kiwi about a result

For every step record: the click path, whether the next step is signposted, dead ends,
places where the user must already know the answer, and terminology that changes between screens.
Mark each step PASS / WEAK / FAIL on the 5-second test: can a new user tell what to do next?

**Check-in:** summarise Phases 1–2 in ≤20 lines before continuing.

### Phase 3 — Buried-critical-features audit

List every feature that protects result validity or prevents silent errors
(acquisition checks, claim checks, QC measures, version/param pinning warnings, maturity warnings, and any others you find).
For each: depth, label clarity, whether it appears *in the flow* at the moment it matters,
and whether a user who never opens its page would still be exposed to it.
Propose where each should surface (inline checkpoint, status chip, gate before a step, empty-state hint).
Distinguish "must interrupt" from "should be visible" from "fine where it is".

### Phase 4 — Density and cut candidates

Find, with evidence (file paths, counts):

- elements that duplicate another entry point
- controls rarely/never used (check telemetry or logs if they exist; otherwise say "unknown")
- hand-rolled components that diverge from canonical ones (see INVENTORY.md Families)
- always-visible advanced options that should be progressive disclosure
- tooltips carrying information that should be visible text, or the reverse
- surfaces that are Coming soon / Experimental taking prime space
- AI elements that should vanish under the global AI toggle

For every cut/merge candidate: state who would miss it and what replaces it.
Never recommend a cut without that answer.

### Phase 5 — Visual and language consistency

Lead with a "lightness" pass: padding/gap scale and how often it is overridden, border and divider count per
screen (could whitespace replace them?), nested boxes/cards inside panels, shadow and fill usage, font weight
and size variety, number of accent colours on one screen, label/helper text volume, row heights in tables and
lists, and always-visible vs. on-hover/on-focus controls. Propose a small set of spacing and chrome tokens
that make the default calmer, and which shared components carry them.
Then audit spacing, type scale, colour use, icon semantics, button hierarchy (primary/secondary),
status colours, empty/loading/error states, panel chrome, terminology (same thing, same word),
and dark/light behaviour. Note what the PrimeVue retirement will change and what to fix during it
rather than twice.

### Phase 6 — Proposal

Propose a navigation model, not a redesign. Evaluate at least:

- a persistent "you are here / next step" spine following the analysis journey
- progressive disclosure tied to view profiles
- a checks dock that always shows validity status (acquisition, claims, QC) for the current image/project
- default sidebar order = journey order
- one consistent empty state pattern with a single next action

State trade-offs and what each would cost in the existing architecture.

## Output format

1. **Top 10 findings**, ranked by user impact. Each: finding, evidence, recommendation (cut / merge / move / surface / rename / hide), effort S/M/L.
2. **Census table** (Phase 1).
3. **Journey table** (Phase 2) with PASS/WEAK/FAIL.
4. **Buried-features table** (Phase 3).
5. **Cut/merge list** (Phase 4) with the "who would miss it" column.
6. **Consistency issues** (Phase 5), grouped by fix-once-centrally vs per-screen.
7. **Proposed slicing**: independent PRs, ordered, each small enough to review by eye. Mark which need human eyes on real pages before merge.
8. **Decisions needed from D**: only questions that change the plan, each with your recommended answer.

## Rules

- Evidence over opinion: counts, paths, screenshots. Label anything inferred as inferred.
- Existing design is not gospel; it is what Claude Code built from one person's decisions. Challenge it.
- No user-research claims you cannot support. If observation is missing, list what to observe.
- Cecelia must work fully without Claude; judge AI surfaces as optional layers.
- Reuse canonical components in all proposals; no new primitives unless you show none fits.
- If you cannot run the app or capture screenshots, say so at the top and mark every finding that depends on it.
