# GUI clarity — a light, consistent, structured GUI

**Status:** **parked** (2026-11-10). P0 draft done, unreviewed: [`GUI_CLARITY_AUDIT.md`](GUI_CLARITY_AUDIT.md), with
the measured screenshots and counts plus per-phase evidence in [`gui-audit/`](gui-audit/). Nothing else is built.
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

## Open questions (each with a recommended answer)

1. **Where do the screenshots come from?** Agents never start the app. *Recommended:* a small
   capture harness against an isolated dev instance (`CECELIA_DEV_DIR`), built as its own PR before
   P0. It gets reused for P1's before/after shots and for later visual passes. The alternative is
   the maintainer capturing them by hand.
2. **What counts as usage evidence?** *Recommended:* treat the observer's capture log as a weak
   signal where it exists, and say "unknown" everywhere else. Don't add telemetry for this.
3. **Spine versus "no default path":** *Recommended:* a per-recipe spine. With no recipe picked,
   pages show their own next step instead of a global one. This keeps the map's decision.
4. **Palette:** *Recommended:* confirm the desert-earth direction against a three-screen mock in P1
   before any token is merged.

## Related

- [`AI_TOGGLE_PLAN.md`](AI_TOGGLE_PLAN.md), [`AI_SURFACES_AUDIT.md`](AI_SURFACES_AUDIT.md): the AI
  layers and their gate.
- [`VIEW_PROFILES_PLAN.md`](VIEW_PROFILES_PLAN.md), [`WORKFLOW_RECIPES_PLAN.md`](WORKFLOW_RECIPES_PLAN.md),
  [`GUIDE_SYSTEM_AUDIT.md`](GUIDE_SYSTEM_AUDIT.md): declutter, recipes, the map.
- [`FEATURE_MATURITY_PLAN.md`](FEATURE_MATURITY_PLAN.md), [`HARDENING_AUDIT_PLAN.md`](HARDENING_AUDIT_PLAN.md):
  tiers, badges, depth scores.
- [`PRIMEVUE_RETIRE_PLAN.md`](PRIMEVUE_RETIRE_PLAN.md): retirement done. Its status header is stale.
