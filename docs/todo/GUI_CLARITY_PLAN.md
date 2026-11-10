# GUI clarity — a light, consistent, structured GUI

**Status:** **built, awaiting the maintainer's look** (2026-11-11). P0 audit: [`GUI_CLARITY_AUDIT.md`](GUI_CLARITY_AUDIT.md)
(evidence in [`gui-audit/`](gui-audit/)). All 12 audit slices merged, #2085–#2106:
1 default functions (#2085) · 2 AI copy (#2086) · 3 cross-page links (#2089) · 4 spacing tokens (#2090) ·
5 one Run / empty state (#2092), recipe + Next + Welcome (#2099) · 6 density cuts (#2091) ·
7 stats notice (#2093), checks cluster + cohort state (#2097), pre-run checkpoint (#2100) ·
8 Welcome (#2099) · 9 Chains on the shells (#2101) · 10 panel sections (#2096), Settings (#2102),
Notebooks/Movies + panel frame (#2103), SplitPage (#2104) · 11 colour scheme (#2106) ·
12 stale outputs (#2098). Open: the maintainer's layout review; the journey bar stays parked
(Decision 17); a user test of "where do I click" against the built app.
**Brief:** [`docs/archive/GUI_CLARITY_AUDIT_PROMPT.md`](../archive/GUI_CLARITY_AUDIT_PROMPT.md). Read
its banner first: six of the brief's premises had moved by the time it was archived. This plan is the
corrected version of the brief.

## Goal

Cecelia should read as a structured, clean, clear image-analysis platform. There are three failure
modes to remove:

1. **"Where do I click now?"** The next step is not signposted.
2. **A buried feature.** A user misses acquisition checks, claim review or QC because they sit too deep.
3. **Unconnected pages.** Parts of the GUI have completely different layouts and feel, so moving
   between pages feels like moving between apps.

**Feel target: a sketch.** The surface is light and loose: less chrome, more air, fewer simultaneous
controls. The skeleton underneath is firm: the same layout, predictable positions, a clear journey.
Judge every change against both halves. Fix centrally (tokens, shared panel and section components)
rather than one screen at a time.

## Decisions

1. **"Light" means the feel, not a colour scheme** (maintainer, 2026-11-10). The app stays dark
   (`App.vue` hard-codes `cc-dark`), and no light theme is built. Contrast (WCAG AA) is checked on the
   dark theme only.
2. **Consistency is a goal in its own right** (maintainer, 2026-11-10). It ranks with failure modes 1
   and 2, not as Phase 5 polish. Every page uses the same skeleton (header, controls, work area,
   output) and the same panel and section components, with run, status and next step in the same place.
   The audit names the pages that break this and the shared component that should carry each part.
3. **Audit before change.** P0 changes no code. Each later phase is a set of small PRs that can be
   reviewed by eye. Anything visual needs the maintainer's eyes on the real pages before it merges.
4. **Colour carries meaning, not decoration** (from the brief). The palette is warm desert earths and
   fires: ochre/saffron, burnt sienna, vermilion, coral, plum, cream, near-black brown. Colour stays at
   or below ~10% of a screen. Viewer and plot areas stay neutral, so UI colour never competes with
   channel or data colours. Colour roles: journey stages, check status, maturity tiers, selection and
   focus, empty states and the welcome page. Status never relies on red versus green alone. Checks
   (acquisition, claims, QC) get one distinctive accent. Playfulness goes in empty states, progress,
   welcome and toasts, while workflow controls stay quiet.
5. **Cecelia works fully without Claude.** AI surfaces are optional layers, already gated by the AI
   toggle (`AI_TOGGLE_PLAN.md`). This plan does not re-inventory them.
6. **Reuse canonical components** (`docs/inventory/FRONTEND.md` families, `docs/ui/PRIMITIVES.md`). A
   new primitive needs evidence that none of the existing ones fits.
7. **One worded Help menu** replaces the 6 icon-only help buttons in the header (maintainer,
   2026-11-10; audit D1). Supersedes the "compass, not a `?`" rationale in `AppHeader.vue`.
8. **Maturity stays as dots** (maintainer, 2026-11-10; audit D2). A worded badge took too much space;
   FEATURE_MATURITY_PLAN Decision 10 stands.
9. **The Kiwi launcher stays visible with AI off** (maintainer, 2026-11-10; audit D3): it is how AI
   gets turned on. All other AI copy hides under the toggle.
10. **Three write-up homes stay** (maintainer, 2026-11-10; audit D4), each empty state saying its
    role: lab log = automatic record, Blackboard = authored notes, Notebooks = code.
11. **Spacing ladder without a 10 px step** (maintainer, 2026-11-10; audit D5): the 137 10 px values
    round to 8 or 12 per site, shared shells first.
12. **Each module opens on its recipe's first step** (maintainer, 2026-11-10; audit D6): drift
    correction on Cleanup, Cellpose on Segment; import functions move down.
13. **The recipe is stored per set** (maintainer, 2026-11-10; journey review N2): a key in the set's
    project data, suggested from the image axes and confirmed by the user. It feeds the next-step slot
    (slice 5), map lighting and Welcome (slice 8).
14. **The viewer is a popout only** (maintainer, 2026-11-10): it has never had a docked version and
    won't get one. The journey review's viewer chip and blocked-popup message build on that.
15. **Colour scheme = the "hybrid v3" mock** (maintainer, 2026-11-10; refines Decision 4 and settles
    journey review N3–N6). Each colour has one job:
    - **Surfaces:** near-neutral dark with a trace of warmth (`#121212` / `#1a1918` / `#23211f`), not
      Sonnet's warm brown (too much) and not today's blue cast. Plot cards and the viewer stay cool
      neutral; plot marks stay grey.
    - **Primary action (Run, Open project):** saffron `#FCC172` fill with dark text (11:1). This is the
      `--cc-accent-fill` role; lavender `--cc-accent` stays for links and focus.
    - **Next step and checks:** plum. The Next button is a plum-light `#D27A98` fill with dark text
      (6.4:1); the checks cluster keeps a plum frame and label. Both mean "act on this".
    - **Selection:** today's orange `--cc-selected #ff8c1a` stays, so saffron only ever means go.
    - **Stage families** (Data ochre, Populations coral, Explore mauve, Analysis cream) tint only the
      sidebar group heading and the active-page bar, never a dot per row.
    - **Unchanged:** viewer green, Kiwi teal, `--cc-sev-*` status colours, Okabe-Ito maturity dots.
    - **Vermilion and crimson** only on Welcome and empty states (too close to fail/danger).
    Mock: `hybrid v3` (not committed; rendered from the review's palette).
    **Built** (tokens in `style.css`: surfaces, `--cc-primary-fill`, `--cc-next-fill`, `--cc-stage-*`;
    the checks cluster's plum frame lands with the checks slice).
16. **The data type lights pages, never hides them** (maintainer, 2026-11-10; journey review N1). A
    set's recipe is suggested from its axes (time axis → a timelapse recipe), confirmed once per set,
    never applied silently. With no recipe, the next-step slot falls back to "from what has run on
    these images".
17. **The journey bar stays parked** until now+ has been tried on users (N7). The mocks are not fixed
    (N8): the built app on an isolated instance is what gets judged.
## What main already has (2026-11-10, `origin/main` 71509170)

- **Theme:** PrimeVue and Aura are gone. The tokens are the 54 `--cc-*` custom properties in
  `frontend/src/style.css`, read through `utils/cssTokens.ts`. `.vue` files use them about 1,600
  times and still carry 228 hard-coded hex literals. A token pass here is where the calmer chrome
  and the new palette land.
- **Navigation:** `lib/navGroups.ts` is the single catalogue, read by the sidebar, the view-profile
  editor and the guide map. Its order is already roughly the journey.
- **Map and recipes:** `GuideMap.vue` (`WORKFLOW_RECIPES_PLAN.md` → *The map*) deliberately lights
  nothing by default ("there is no default path"). A picked recipe lights its pages. The map carries
  maturity dots and the ✦ Claude toggle.
- **Declutter:** view profiles (`VIEW_PROFILES_PLAN.md`) give each user a curated sidebar.
- **Maturity:** tiers and page badges (`FEATURE_MATURITY_PLAN.md`): Experimental is badged on pages
  and never hidden.
- **Copy census:** `pixi run ui-copy` (`scripts/ui_copy_inventory.mjs`) finds wording drift and
  repeated phrases.
- **Shared page chrome:** `ModulePage.vue`, `ModuleLayout.vue`, `CollapsiblePanel.vue`,
  `CollapsibleSection.vue` and `FloatingPanel.vue`. P1 and P2 build on these. The empty-state
  pattern already exists as `.cc-empty` (variants `-inline`, `-overlay`, `-lg`;
  `docs/ui/PRIMITIVES.md` forbids a new `.*-empty` class). P4 builds on it.
- **Missing:** a route screenshot harness and any UI-usage telemetry.

## Phases

- **P0 — Audit** (`GUI_CLARITY_AUDIT.md`, no code changes). The brief's method with the corrections:
  1. Surface census: depth, control count at first paint and after expanding, entry points, maturity
     badge, and a screenshot per surface.
  2. Journey walk: the nine journeys, PASS / WEAK / FAIL on "can a new user tell what to do next?".
     **Check in after this step.**
  3. Buried critical features: for each, decide whether it must interrupt, should be visible, or is
     fine where it is.
  4. Cut and merge candidates. Each one names who would miss it and what replaces it. Usage is
     "unknown" unless a usage source is agreed (open question 2).
  5. **Consistency pass (Decision 2):** a page-by-page skeleton comparison, then the lightness pass
     (spacing overrides, borders and dividers, nested cards, type variety, accents per screen), then
     terminology.
  6. A navigation proposal, not a redesign: a next-step spine (per recipe, open question 3), a checks
     dock, progressive disclosure through view profiles, and one empty-state pattern with one next
     action.

  Output: top 10 findings, the tables, a proposed PR slicing, and questions for the maintainer.
- **P1 — Central tokens and shared chrome.** Spacing and chrome tokens and the colour roles in
  `style.css`. The shared page, panel and section components carry them. Hex literals move onto
  tokens. Before/after screenshots of three key screens.
- **P2 — One page skeleton.** Move the pages that the P0 consistency pass flagged onto the shared
  skeleton, a few pages per PR.
- **P3 — Surface the checks.** Put acquisition, claim and QC status where the step happens, using
  whatever shape P0 recommends (dock, chip, or inline checkpoint).
- **P4 — Next step and empty states.** Next-step signposting and the single empty-state pattern.

The order of P1–P4 is provisional. P0's slicing replaces it.

## Open questions

1. ~~**Where do the screenshots come from?**~~ **Resolved:** `scripts/gui_capture.mjs`
   (`pixi run gui-capture`, #2084) against an isolated instance (own `CECELIA_DEV_DIR` and
   `CECELIA_PORT_SLOT`, project copies with symlinked image stores). It produced the P0 census and
   will produce the before/after shots.
2. **What counts as usage evidence?** *Recommended:* treat the observer's capture log as a weak
   signal where it exists, and say "unknown" everywhere else. Don't add telemetry for this.
3. ~~**Spine versus "no default path":**~~ **Resolved by Decision 13** (recipe per set). A per-recipe spine. With no recipe picked,
   pages show their own next step instead of a global one. This keeps the map's decision.
4. ~~**Palette:**~~ **Agreed (maintainer, 2026-11-10):** the desert-earth direction is confirmed
   against a three-screen mock in P1 before any colour token merges.

## Related

- [`JOURNEY_WORKFLOW_REVIEW.md`](JOURNEY_WORKFLOW_REVIEW.md) (draft): the journey-bar idea and the
  now+ / journey mocks judged against this plan. Verdict adopt in part: now+ is mostly slices 1, 3, 5,
  7, 8 and 11 plus a per-set recipe, a viewer chip and a blocked-pop-up message; the journey bar is
  held. Palette extracted from the source paintings, with roles and contrast. Decisions N1–N8 open.
- [`AI_TOGGLE_PLAN.md`](AI_TOGGLE_PLAN.md), [`AI_SURFACES_AUDIT.md`](AI_SURFACES_AUDIT.md): the AI
  layers and their gate.
- [`VIEW_PROFILES_PLAN.md`](VIEW_PROFILES_PLAN.md), [`WORKFLOW_RECIPES_PLAN.md`](WORKFLOW_RECIPES_PLAN.md),
  [`GUIDE_SYSTEM_AUDIT.md`](GUIDE_SYSTEM_AUDIT.md): declutter, recipes, the map.
- [`FEATURE_MATURITY_PLAN.md`](FEATURE_MATURITY_PLAN.md), [`HARDENING_AUDIT_PLAN.md`](HARDENING_AUDIT_PLAN.md):
  tiers, badges, depth scores.
- [`PRIMEVUE_RETIRE_PLAN.md`](PRIMEVUE_RETIRE_PLAN.md): retirement done. Its status header is stale.
