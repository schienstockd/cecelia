# Journey workflow idea: review against GUI_CLARITY_PLAN, the audit and the code

**Status:** **draft** (2026-11-10), unreviewed; all decided (2026-11-10): N2 = plan Decision 13, viewer popout-only = 14, colour N3–N6 = 15 (a hybrid of the mock and §5, which overrides N3/N4 below), N1 = 16, N7/N8 = 17. Written by an agent from the
archived brief [`../archive/JOURNEY_WORKFLOW_IDEA.md`](../archive/JOURNEY_WORKFLOW_IDEA.md), the two mocks
(*now+* and *journey bar*, not committed) and three painting photos (palette source, not committed).
Plan: [`GUI_CLARITY_PLAN.md`](GUI_CLARITY_PLAN.md). Audit: [`GUI_CLARITY_AUDIT.md`](GUI_CLARITY_AUDIT.md).

**Verdict: adopt in part.** Keep the now+ row. It is mostly the audit's slices 1, 3, 5, 7 and 8 drawn
out, plus three small new pieces. **Do not** build "pick a data type and only the relevant pages
show". Recipes already do that job ("the data picks the recipe"), and hiding pages conflicts with
View Profiles D2 and the map's "no default path". Instead, store the recipe per set (the system
suggests it from the T axis, the user confirms it), and let it light pages and feed `#next`. Hold the
journey bar until now+ has been tested on users. The mocks also contradict 5 locked rules and fail
AA in 2 places (§4). Fix those before the maintainer judges the look.

Sources: plan and audit read on `docs/gui-clarity-audit-archive`, code read on `origin/main`
(293cb0388 on the worktree). "Inferred" marks anything not read in code.

---

## 1. What the idea proposes

The md (`JOURNEY_WORKFLOW_IDEA.md`) and the brief that came with it (the Sonnet chat) propose:

1. **A journey bar instead of the sidebar** (later): "the analysis stages in order (Import, Check,
   Correct, Segment, Populations, Track, Figures), each with a one-line status rolled up from its checks."
2. **The data type decides the pages** (from the chat, not in the md). The user or the system says
   what kind of data this is, and only the relevant parts show, as a journey.
3. **A viewer that only pops out**: "never docked … always a pop-out window reached from a viewer
   chip". That means a remembered window size, a shortcut, snap-to-half, and "a blocked pop-up must
   produce a plain message".
4. **One skeleton for every page**: header (crumb, viewer chip, a Ctrl K jump box, worded Help,
   Ask Kiwi), navigation, an action bar with the image selector and a checks cluster, a work area with
   a result strip, and a task panel with a `#next` slot.
5. **Checks owned by stages**: "name, state (ok / flagged / not run), a one-line reason, and one
   action". Severity changes "prominence, not gating". Accepting a flag records why.
6. **A health strip**: in the bar, "a stage's status line is a roll-up of its checks". Clicking a
   flagged stage lands on the flagged check.
7. **`#next` with its source**: "From recipe Cell tracking", or "from what has run on these images".
8. **Built on audit slices 4, 5, 7 and 8**, coming after them as "a possible later shell swap".
9. **Feel and palette**: "light surface, firm skeleton". The app stays dark, the desert-earth palette
   covers ≤10% of a screen, and the viewer and plot areas stay neutral.
10. **Rollout**: behind a layout switch, decided by a cheap comparison of the two mock rows.

## 2. Each proposal against what exists

| # | Proposal | Status | Evidence |
|---|---|---|---|
| 1 | Journey bar replaces the sidebar | **New, and it fights the audit's verdict.** The audit says "Depth is not the problem. Every sidebar page is at most 2 clicks away" and recommends a spine per page, "not a global bar" (Audit §7, row 1). | `navGroups.ts:28-78` is the single catalogue. The sidebar also carries guide anchors: `AppSidebar.vue:111` `data-guide="sidebar.projectBlock"`, `:190` `sidebar.settings`, and the `nav:` anchors used in ~10 guide files and ratcheted by `guides.test.ts` |
| 2 | Data type → only the relevant pages show | **Already exists as recipes, minus the hiding and the persistence.** Recipes are keyed on data: "The data picks the recipe" (`recipes.ts:78`). The five recipe ids are data-shaped: `few-marker-tissue` :93, `multiplex-tissue` :125, `intravital-timelapse` :159, `live-structures` :230, `in-vitro-migration` :261. A picked recipe **lights** pages and hides none (`GuideMap.vue:5-6`). **Conflicts:** VIEW_PROFILES D2 (:52-55: "A profile describes who is driving, not what the data is"); its scope (:24: "a hidden page must stay reachable"); WORKFLOW_RECIPES "Not this" (:532, "Not automatic task selection"); and the spirit of FEATURE_MATURITY D1 ("Badge, never hide"). There is a precedent: the old R app hid pages by live/static, and the rewrite dropped that (VIEW_PROFILES_PLAN:9-10) | The picked recipe is **not persisted**: `GuidesDialog.vue:54` `const openRecipe = ref('')`, reset every time the dialog opens. Nothing outside the Guides dialog reads it (grep `recipeId\|openRecipe`: only `GuideMap.vue:37,50,79` and `GuidesDialog.vue:54,100`) |
| 3 | Viewer pops out only, behind a chip | **Mostly exists.** The viewer is already a popout (`popout.ts` `POPOUT_WINDOW_NAMES['/viewer-window']`; `viewerWindow.ts:30-33`). **Gaps are real:** the size is fixed at 1200×800 (`viewerWindow.ts:32`), and a blocked popup fails silently (`popout.ts:80-83`: `window.open` returns null and nothing reports it). The open state is partly knowable through `POPOUT_HANDLES` (`popout.ts:94`) | 7 entry points (Audit §2) |
| 4 | One skeleton | **This is Decision 2 and audit slices 4, 5, 9 and 10.** `#next` does **not** exist yet: `ModuleLayout.vue` slots are `actions` :299, `plots` :441, `below-table` :450 and the default slot :463; `ModulePage.vue` has `controls` :34 and the default :36. **The Ctrl K jump box is new and in no slice** (grep for `key === 'k'`, "Ctrl K", a command palette or quick-open in `frontend/src`: no match) | Help menu = Decision 7. Kiwi visible = Decision 9 |
| 5 | Checks with one contract | **Duplicates audit slice 7** (`CheckButton`, a checks cluster in the action bar, server cohort state). The "one action" rule extends p3's "Next lines". "Accept with a note → claim record" is **new** and has no slice | p3 rule of three: AcqCheckButton, CohortCheckButton, ClaimStrip. The cohort state is a session ref (`CohortCheckButton.vue:22`, per p3 #9) |
| 6 | Stage health roll-up | **New.** The roll-up is cheap where the state is local (`imageState.ts:19` segmented, `:23` tracks, `:26` drift-corrected, `:29` neighbour graph; all answered with no request) and costly where it is not (acquisition and cohort need fetches, claims live on plots) | The audit already calls the "what's done for these images" join new (§7) |
| 7 | `#next` with its source | **Is audit slice 5 plus plan open question 3** (spine per recipe, else per page). It needs the persisted recipe (row 2), which does not exist | `GuidesDialog.vue:54` |
| 8 | After slices 4/5/7/8 | Agrees with the audit's order | Audit §8 |
| 9 | Palette and dark theme | Decisions 1 and 4. **The mocks retint every surface warm brown** (§4), which is outside the ≤10% budget | `style.css:22-24` cool surfaces |
| 10 | Behind a switch | New. Cheap for the sidebar and bar shell; the guide anchors are the catch (row 1) | — |

## 3. The key question: data type, "no default path", and view profiles

**Compatible only if the data type lights pages and never hides them.** The map's rule
(WORKFLOW_RECIPES_PLAN:400-402, `GuideMap.vue:5-6`) is: "None picked = nothing lit: there is no
default path … A picked recipe lights its pages." A data type that filters the sidebar is a second,
stronger version of the same choice. It would also reopen a decision the rewrite made on purpose:
the old R app's live/static page hiding was dropped (VIEW_PROFILES_PLAN:9-10), and view profiles took
its place as an opt-in, per-user tool. Hiding by data type would stack three filters on one menu:
profile, then data type, then the task axis gating already in TaskRunner (p3 #14, `TaskRunner.vue:625-627, 860-875`).
A user would then have three reasons a page might be missing.

**The data type is already modelled. It is the recipe.** WORKFLOW_RECIPES D10 (:272-276) names two
axes: data ("does it move … how many markers") picks the recipe, and the question becomes steps inside
it. So the honest form of "say what data you have" is **pick (or confirm) a recipe**.

**Where it should live: per set, in project data.** *Recommended:*
- **Not per user.** VIEW_PROFILES D2 keeps the profile per user because it is "who is driving". The
  same sentence says the data type is "what the data is", i.e. a property of the data.
- **Not per project.** One project can hold static and live sets. *Inferred:* the MERTK and demo
  projects mix image kinds. `CciaImage` axes are per image (`project.ts:20-22`).
- **Per set.** The frontend's `CciaSet` today is `{uid, name, images}` (`project.ts:75-77`), but the
  backend set already has a persisted `meta::Dict{String,Any}` with `save!(::CciaSet)`
  (`app/src/model/set.jl:3-29`). The recipe is a key in that `meta` plus exposing it to the frontend,
  not a new field or an export/import change (`project_io.jl` only handles bundles). A collaborator
  would see it, which is right, because it describes the data.
- **Today nothing reads a data type.** The only consumer of a recipe is the Guides dialog, and it
  forgets the recipe on close.

**Can the system infer it? Partly: one axis reliably, the second as a hint, never all five.**
- The **T axis** is already known locally: `isTimeSeries = imageAxes(i).has('T')` (`imageState.ts:13`),
  built on `CciaImage.sizeT` (`project.ts:21`). The `timeSeries` guide prerequisite already reads it
  (`prereqs.ts:51-55`), and so does the task-discovery check (`taskDiscovery.ts:35`). This splits
  {few-marker, multiplex} from {intravital, live-structures, in-vitro}. That split is reliable.
- The **marker count** (`sizeC`, `channelNames`, `project.ts:20,30`) separates few-marker from
  multiplex only with a threshold. *Recommended:* show it as a suggestion with the count stated
  ("24 channels → Multiplex tissue?"), never decide silently. The memory rules avoid task thresholds.
- **Not inferable** from axes: intravital vs in vitro vs live structures. *Inferred:* the
  `acquisition` settings (`project.ts:33`, e.g. a 2-photon microscope) could hint at intravital. That
  is a guess, so the user confirms.
- **Result:** the system can pre-select between 2 and 3 recipes and the user confirms. It fits
  "Auto shows what was picked" (the suggestion carries its reason).

## 4. The two mocks

**Now+** (3 pages: Welcome, Segment, Gate) has the sidebar in journey order with stage-hue dots, a
header with a viewer chip, Ctrl K, a worded Help and a saffron Ask Kiwi. Its action bar has
"Image 12 of 12" and a CHECKS cluster (Acquisition, Cohort, "2 QC-flagged" or "Claims: 1 to review").
Below that come a result strip ("Overlay shown in the viewer", Correct labels, Focus viewer), the
plots, then a right panel (Function, a When to use line, 3 params, This image / All 12, a full-width
Run, More options, and NEXT "From recipe Cell tracking"). The Welcome page has Open / New, recent
projects with "1 check to review", and three recipe cards.

**Journey bar** (2 pages: Segment, Populations) drops the sidebar for a 7-stage bar with status
lines ("1 to review", "done", "you are here", "next"), adds "Recipe: Cell tracking" on the right
and a gradient rule along the top edge, puts the task panel on the **left**, the checks in a right
card with reasons and Review, and a floating "Looks right? Next: Clean up" pill.

**Audit findings each one fixes**

| Finding | Now+ | Journey |
|---|---|---|
| 1 next step and Welcome | yes (`#next`, Welcome) | yes (pill and bar), no Welcome shown |
| 2 default function | yes (Cellpose-SAM preselected) | yes |
| 3 control count | yes (3 params and More options; chrome cut from 26-32 to ~16) | yes, fewer still |
| 4 different-app pages | not shown (Chains, Settings) | not shown |
| 5 tokens / heavy feel | yes (air, dividers) | yes |
| 6 must-interrupt checks | no (no PlotNotice, no pre-run checkpoint, no stale state) | no |
| 7 checks look | yes (cluster) | yes (card with reasons) |
| 8 prerequisites | yes ("Correct labels" in the result strip) | partly |
| 9 terminology | **gets worse** (see the conflicts below) | **gets worse** |
| 10 AI copy | n/a | n/a |

**What each would cost**
- **Now+** is about audit slices 1, 3, 5, 6, 7 and 8 plus: a persisted recipe per set (S, a key in the
  set's existing `meta`), a viewer chip (S, on `openViewerWindow` / `viewerWindowQuery` in `utils/viewerWindow.ts`, reusing the `PanelLaunchers.vue` Viewer toggle's `--cc-viewer` green; `getOpenPopoutWindow` only for the open/closed state), a result strip
  (M: no shared component exists yet; it would be a `ModuleLayout` slot, which is evidence for
  Decision 6), Ctrl K (M, new) and "n checks" on Welcome's recent projects (M, which needs a per-project
  roll-up without opening the project; *inferred:* there is no such endpoint).
- **Journey bar** is all of the above plus: an `App.vue` shell swap and an AppSidebar replacement, a
  `stage` field on `NavItem` (or headings renamed into stages), re-anchoring every `nav:` and
  `sidebar.*` guide anchor (ratcheted by `guides.test.ts`), the project block moving out of
  `AppSidebar.vue:111`, GuideMap cards becoming stages, and the roll-up (M–L). Renaming the group
  headings touches **66 heading literals in ~20 files** (guides, `acqCheck.ts`, `titleCard.ts`,
  `CanvasSidePanel.vue`, …). *Inferred:* L overall, and that is before the pages with no stage are
  given a home: Spatial, Phenotype, the cluster pages, Chains, Tasks, Blackboard, Notebooks, Settings
  and Custom. The bar shows 7 stages; the app has 21 sidebar pages plus custom ones.

**Where the mocks break a locked rule or the code's facts**

1. **Decision 4 (plots stay neutral):** the histograms and frame-trend lines are filled saffron, and
   the gate dots are saffron and pink. Here UI colour *is* the data colour, and T cells share the
   Run button's hue.
2. **Decision 4 (workflow controls stay quiet)** and the existing tokens: Run, New gate and Open
   project are saffron fills, and Next is plum or lavender. Primary buttons are not one of
   Decision 4's colour roles. Today they are `--cc-accent-fill` (`style.css:42`).
3. **KIWI_PLAN Decision 2 (locked), quoted at `style.css:48-51`:** Kiwi is teal. The mock's Ask Kiwi
   is a saffron fill. It stays visible (Decision 9 is satisfied) but in the wrong colour.
4. **`--cc-viewer` is "THE green" for the app's own instrumentation (`style.css:76-81`).** The mock's
   viewer chip is saffron.
5. **Decision 8 and FEATURE_MATURITY D10:** the Segment panel shows "● Beta" as a dot plus the word,
   and the dot is orange. Today's Beta dot is Okabe-Ito blue (`style.css:568`), and the orange tone
   means *Experimental* (`:567`). The mock also puts **stage dots** on every sidebar row, which gives
   one shape (`.cc-dot`) two meanings next to the map's maturity dots.
6. **Recipes that don't exist, and the wrong recipe shape.** "Cell tracking / Segmentation only /
   Spatial analysis" are *questions*. WORKFLOW_RECIPES D10 says the *data* picks the recipe, and D9
   says no recipe without measured data. "Next: Clean up after Segment, from recipe Cell tracking"
   matches no recipe: in `intravital-timelapse` the step after segmentation is gate
   (`recipes.ts:181-203`).
7. **Finding 9 drift baked in.** The bar has "Correct" for image correction while Segment's next step
   is "Clean up", and audit finding 9 says these two words already swap meanings.
8. **The set concept is gone.** The action bar shows "Image 12 of 12". The real page has a SET
   selector (screenshot `RFtn9f_segment.jpg`) and the image table. *Inferred:* this is a design
   omission, not a proposal.
9. **AA failures:** dark text on the plum Next fill `#B4547C` is **4.00:1**, and on the lavender Track
   fill `#8C6CA4` it is **4.27:1** (AA for normal text is 4.5). The Run fill (8.4:1) passes.
10. **The rows differ in more than the sidebar.** The task panel is on the right in one and the left
    in the other, the checks sit in the action bar in one and a right card in the other, and Next is
    in the panel in one and a floating pill in the other. The md says the only structural difference
    is sidebar vs bar. In practice, a user test would compare four variables at once.

Colour coverage measured on the rasterised mocks (pixels with saturation >0.35 and value >0.35):
1.4–3.2% of the screen, so inside the ≤10% budget. **But** every surface is retinted warm brown
(`#1a100e` / `#291c16` / `#342319` vs `--cc-bg #0f1117`, `--cc-surface-1 #161b22`,
`--cc-surface-2 #21262d`). That is a theme change, not an accent, and it also tints the plot
surround, which Decision 4 wants neutral.

## 5. The palette from the three paintings

Method: pixels binned by hue, then the median of the mid and bright value bands among saturated
pixels (k-means alone returned muddy averages of dots and ground). Below, "% px" means the share of
pixels in that hue bin. The palette takes **colour only, no motifs or patterns**.

| Plan's name | What is actually there | Hex (mid / bright) | Where |
|---|---|---|---|
| ochre / saffron | present, but only bright in painting 3 | `#CEA966` / `#FCC172` | 3 (16% px), 1 (2%) |
| yellow ochre | present (pale straw) | `#AFA469` / `#FCEB96` | 3 |
| orange | dominant | `#C97E4E` / `#FCA261` | 1 (13%), 2 (20%) |
| burnt sienna | dominant | `#9D4C33` / `#E77451` | all three (12–25%) |
| vermilion | dominant in painting 1 | `#BD483C` / `#E3584A` (deep `#C73B2A`) | 1 (26%), 2 (11%) |
| crimson | present (not in the plan) | `#8C242D` / `#BC4951` | 1, 2 (4%) |
| coral / blush | present | `#E9A587` / `#FDB098` | 2 (9%), 1 and 3 (3–4%) |
| plum | **raspberry-plum, not aubergine**; scarce | `#A34B68` / `#BF577B` | 1 (~1%) |
| aubergine / mauve | low-saturation mauve | `#774B53` / `#8C5862` | 3 (~4%) |
| cream | present | `#F5EFD0` | 2 (11%), 3 (12%) |
| near-black brown | present | `#2E221E` (`#2C1B12` warmer) | all (8–14%) |

**Contrast** (as text on `--cc-bg` / `--cc-surface-1` / `--cc-surface-2`; AA = 4.5 for text, 3.0
for UI marks):
- **Passes for text everywhere:** saffron 11.7/10.7/9.4, ochre 8.5/7.8/6.9, orange 9.4/8.6/7.6,
  sienna-light 6.3/5.8/5.1, coral 9.2/8.4/7.4, blush 10.6/9.8/8.6, cream 16.3/14.9/13.2.
- **Borderline:** vermilion 5.2/4.75/**4.2** (fails on surface-2).
- **Marks only (fails for text):** plum-light 4.4/4.0/3.5, burnt sienna 3.2/2.9/2.6, crimson
  3.8/3.5/3.1, mauve 3.3/3.0/2.7. A lightened plum `#D27A98` reaches 6.3/5.8/5.1 for text.
- **On fills:** the light hues take **dark text** (`#0f1117` on saffron 11.7, on coral 9.2). The dark
  hues take **white text** (white on plum 5.6, on sienna 6.0, on crimson 5.0, on aubergine 7.2).
  White on vermilion is 3.65 and fails.

**Collisions** (ΔE76; under 25 is flagged):
- **Vermilion ≈ status fail** (`--cc-sev-fail #d03b3b` ΔE 9, `--cc-danger` 12, `--cc-danger-fill` 16).
  Deep vermilion ΔE 8, crimson ≈ danger-fill ΔE 9. A vermilion stage hue would read as an error.
- **Sienna-light ≈ `--cc-claude #d97757`** (ΔE 8). Orange, coral and blush are 21–23 from it too.
- **Saffron of the mock (`#DCA43C`)**: ΔE 16 from the Experimental tier dot `#E69F00` and 19 from
  `--cc-sev-warn`. The paintings' paler saffron `#FCC172` sits at 30, which is better.
- **Channel colours** (`viewerColormap.ts:10-17`: red, bop orange `#ff7f0e`, yellow, magenta,
  bop purple): none closer than ΔE 25. They are still all warm, so keep them off the viewer, as
  Decision 4 already says.
- **Colour vision:** the warm family collapses for deuteranopes. Simulated (Machado) pairs:
  sienna-light vs vermilion ΔE 6, sienna-light vs ochre 8, plum vs mauve 9, saffron vs ochre 14.
  **Seven stage hues cannot be told apart by colour.** The stages must lean on their labels.

**Role assignments (per Decision 4), recommended:**

| Role | Colour | Why |
|---|---|---|
| **Check accent** (the one) | plum-light `#BF577B` for marks and borders; `#D27A98` for text | the only palette hue far from every status (nearest is danger-fill at ΔE 29) and from the channel colours |
| **Check status** | **keep** `--cc-sev-*` with its icons (`lib/severity.ts`) | the existing set is CVD-validated (`style.css:63-75`). Do not invent a coral "flagged" |
| **Maturity tiers** | **keep** Okabe-Ito (`style.css:563-570`) | FEATURE_MATURITY D10 and the CVD rationale. Take maturity out of Decision 4's roles |
| **Selection / focus** | saffron `#FCC172` replaces `--cc-selected #ff8c1a` | ΔE 26 from `#ff8c1a`, so it is the same role, just warmer |
| **Journey stages** | at most 4 families, one per navGroup (Data = ochre `#CEA966`, Populations = coral `#E9A587`, Explore = mauve `#8C5862`, Analysis = cream `#F5EFD0`), only on the active indicator and the panel's top rule, **never as row dots** | label-backed; avoids the vermilion and Claude collisions |
| **Empty states, Welcome, toasts, progress** | the whole palette, including vermilion and crimson | the only place playfulness is allowed (Decision 4), and nothing reads as status there |
| **Primary buttons** | stay `--cc-accent-fill` until Decision N4 below | not a Decision 4 role |
| **Surfaces** | stay cool unless N3 is decided | — |

## 6. Recommendation: how to fold this into the plan

**The now+ row = audit slices 1, 3, 5, 6, 7, 8 (+ 11 for colour), in the existing order.** On top of
them it adds three small pieces, to be added to existing slices rather than new phases:
- **into slice 5:** `#next` reads a **persisted recipe per set** (a key in `CciaSet.meta`), with the source line
  ("From recipe …" / "from what has run on these images"). Plan open question 3 becomes a decision.
- **into slice 7:** the check contract (state, reason, *one action*) and its narrow-window collapse.
  "Accept with a note" becomes a later slice, because it writes to the claim record.
- **into slice 8:** Welcome confirms the set's recipe, suggested from axes (§3), and shows "n checks"
  only if a cheap roll-up exists.
- **a new small slice, 12b:** viewer chip, remembered window geometry, and a plain message when the
  popup is blocked (`popout.ts:80`). It is S and independent.

**The journey version adds, after now+ has been tested:** the bar shell behind a layout switch, the
stage roll-up, and stage = navGroup (headings kept, so the stages *are* the 5 groups and the 66
literals don't move). Pages with no stage stay reachable from Ctrl K and the Help menu. Before
building it, run the md's own test: silent first-run walkthroughs of import to gate on **now+ vs
today's main** first. Only if now+ still leaves "where do I click" WEAK does the bar earn the
comparison.

**Drop:** hiding pages by data type; Ctrl K inside now+ (it belongs in the journey version, where
the sidebar is gone); warm surfaces (pending N3); stage dots on every row; and question-shaped recipe
cards.

**Decisions needed from the maintainer (each with a recommended answer):**
- **N1. Does the data type hide pages or light them?** *Recommended: light.* Reasons: VIEW_PROFILES
  D2 and its scope, the map's "no default path", and the old R hiding that was dropped.
- **N2. Where is the recipe stored?** **Decided (maintainer, 2026-11-10): per set.** *Was recommended: per set, in project data, suggested from axes
  and confirmed by the user.* Consumers: `#next`, map lighting and Welcome.
- **N3. Do surfaces turn warm brown, as in the mocks?** *Recommended: no for now.* It is a theme
  change outside the ≤10% budget, and it tints the plot surround. Revisit with the P1 three-screen
  mock (plan open question 4).
- **N4. Primary buttons: lavender, or a palette colour?** *Recommended: keep lavender
  (`--cc-accent-fill`).* Controls stay quiet, and the palette goes to meaning.
- **N5. Revise Decision 4's roles:** remove *maturity tiers* (keep Okabe-Ito), make *check status* =
  the existing severity tokens, and make the *check accent* = plum. *Recommended: yes.*
- **N6. Vermilion and crimson:** *Recommended: use them only in empty states and Welcome, never as
  a stage or control hue* (they are ΔE 8–16 from the fail and danger colours).
- **N7. Journey bar:** *Recommended: park it until now+ has been tested on users. If built, stages =
  the existing navGroups, and the bar ships behind a layout switch.*
- **N8. Must the mocks be fixed before he judges them?** *Recommended: yes.* Fix the 10 items in §4
  first, at least Kiwi teal, neutral plots, no "Beta" word, real recipes and the set selector.
  Otherwise the comparison judges the wrong things.

**The md's nine questions, one line each:**
1. *What the bar adds:* a health strip and a sense of place, at the cost of the 2-click reach and the
   guide anchors. The case against it: the audit found depth is not the problem.
2. *Stages:* use the navGroups as they are.
3. *Linear bias:* light nothing until a recipe is picked, and give the bar no "next" without one.
4. *Pages that don't fit:* declare them exceptions on the shared shells.
5. *Popout-only:* the cheap trick is the "Shown in the viewer · Open" acknowledgement and snap-half.
   The blocked-popup message is a must.
6. *Checks:* p3 lists 3 must-interrupt checks with actions that already exist (inline at Run, plot
   and export) and 3 cluster checks (#5, #8, #9). "Accept with a note" is new.
7. *Migration:* a layout switch, but the anchors make it M+.
8. *Deciding:* silent walkthroughs comparing now+ with main first.
9. *Alternatives:* the lightened sidebar wins now. A hub of image sets wins if sets keep getting lost
   (as they do in the mock). Whiteboard-as-home only suits users who already run Chains.
