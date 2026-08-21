# Processing guides for common scenarios — "what are you trying to do?"

**Status:** P0 shipped (2026-08-21, #610). **P1 + P2 BUILT** on branch `feat/workflow-recipes`
(2026-08-21): the intravital recipe, the three request rows (D9), the picker section, the
`recipe_request.yml` issue form, docs. Not yet seen in a browser — see *Reservations*. P3 is deferred
until a request tells us what to write. Came out of the cellpose 4 migration
(`CELLPOSE_V4_PLAN.md`), which asked the wrong question first.

The review that produced D6–D8, *Constraints* and the phase list read the shipped system rather than
this plan's assumptions about it: `lib/guides/*` (18 guides), `stores/guide.ts`, `GuidesDialog.vue`,
`guides.test.ts`, the two Julia guide ratchets in `app/test/suite.jl`, `docs/UI.md` → *Guides*,
`GUIDE_SYSTEM_PLAN.md` and `VIEW_PROFILES_PLAN.md`. Three of the four recipes in D2 turned out to be
unwritable against today's catalogue (D7) — and unwritable in prose too, since we do not know what
those users image — so they became requests; one whole phase turned out to be already built (D8).

## Where it came from

The prompt behind the cellpose 4 work opened with "make coastal the default for live-cell
segmentation". There is no such default to change — `segment.cellpose` and `segment.coastal` are two
sibling entries in the Segment category and you click one. Asked whether to add a per-task
"suits timelapse / suits static" hint instead, Dominik reframed it:

> *"this was more of a .. guide. i mean we could come up with a sort of.. builder i guess. to say.
> ok. what do you want to do? intravital? then do these steps. do you need behaviour or tracks or
> interactions? then follow these steps. do you have large multiplex images. then do these steps. do
> you have many small regular confocal images to quantify interactions. then do these steps."*

and, on being told the GUI still lets someone point Cellpose at a dim 3D movie with no hint:

> *"this is why i outlined that we should provide processing guides for commonly used scenarios"*

So the unit is not a task default and not a badge. It is a **scenario**: what your data is and what
you want out of it, answering which steps to run, in which order, and — the part nothing in the app
says today — **which branch to take at each fork**.

## The framework already exists, and this is not a new subsystem

`docs/UI.md` → *Guides* / `docs/todo/GUIDE_SYSTEM_PLAN.md`. Built, shipped, 18 guides:

- **A guide is a point-and-observe walkthrough** on the user's own data — a bubble beside the real
  control. The load-bearing rule (plan D1) is that it never clicks, selects, navigates or runs
  anything; there is no action field in the step type. That rule is exactly right here and must not
  be relaxed: a scenario that ran a 12-minute segmentation for you would be the worst version of this.
- **`moduleTaskGuide()`** (`lib/guides/moduleTask.ts`) builds a whole "run this function on these
  images" guide from a small options object — route, `funName`, which parameters matter, what to do
  with the output. `taskRunSteps()` is the same block for use mid-sequence.
- **Prerequisites** (`lib/guides/prereqs.ts`) already answer "is this guide even runnable yet", and
  the picker shows only the *missing* ones with the fix inline.
- **`awaitTask`** parks a step on a running task and resumes when it finishes — so a multi-step
  processing arc is already expressible.
- **Completion is persisted** (`cc.guide.<id>.done`), which is how a scenario can tick its steps off.
- **The catalogue is pure data** over a `GuideCtx` snapshot, and `guides.test.ts` ratchets every
  anchor, so a guide pointing at a control that no longer exists fails the build.

The guides are currently indexed on **one axis** — the sidebar's own arc (Start / Data / Populations /
Explore / Analysis / Pipeline), i.e. *where in the pipeline am I*. What is missing is the second axis:
*which pipeline is mine*.

## The gap this had to close first, and it is concrete — CLOSED by P0 (#610)

*Kept as the record of why P0 came before the chooser. Both guides now exist
(`train-flow-model`, `segment-by-motion`), which is also why the intravital recipe needs no new guide
at all (D7).*

**There was no guide for `segment.coastal` or `opticalFlow.train`.** The only segmentation guide,
`segment-an-image`, teaches `segment.cellposeMeasure`. Post-migration that means the in-app guidance
points an intravital user at the tool `SEG_QUALITY_PLAN.md` measured at **0.0% QC-pass** on exactly
that kind of data (against tuned `cyto2`'s 13.4%) — and `cyto2` is no longer selectable, so there is
no longer even a bad fallback. The recommended path for dim, moving, 3D data (train a flow model →
`segment.coastal`) is the one arc with no guidance at all.

That is not a scenario-chooser problem. It is a missing guide, and it is worth doing on its own
whatever happens to the rest of this plan.

## Design

### D1 — A scenario COMPOSES guides; it is not a new runtime

```ts
interface ScenarioDef {
  id: string
  title: string                  // "Intravital timelapse"
  whenThisIsYou: string          // one line, the recognition test — not a description of the steps
  steps: ScenarioStep[]
}
interface ScenarioStep {
  guide: string                   // an existing GuideDef id
  why: string                     // one line: why this step, in THIS scenario
  optional?: boolean              // "only if your movie drifts"
}
```

Starting a scenario step starts the existing guide. Nothing about the guide runtime changes; the
scenario is a list with reasons attached, and the reasons are the product. A step that needs a guide
that does not exist yet is a build error, same ratchet as the anchors.

### D2 — The scenario names the FORK, which is the whole point

`segmentGuide` cannot say "use coastal instead" — it is the cellpose guide. The scenario is the only
place where "for this data, that tool, and here is why" can be said once instead of as a tip on every
affected control. Concretely, the first four (Dominik's own list):

| scenario | the fork it exists to state |
|---|---|
| Intravital timelapse | flow model + `segment.coastal`, **not** cellpose — with the QC number behind it |
| Behaviour / tracks / interactions | segment **+ measure** (the composite), then track + measures, then HMM — the "labels with no measures" trap |
| Large multiplex images | AF correction before segmentation; cellpose (static is its case); tiling params |
| Many small confocal images, interactions | do it as a **chain** over a set, not image by image |

**Only the first of those four is being written (Dominik, 2026-08-21).** The other three go in as
REQUESTS, not recipes — see D9. The reason is in D7: the honest version of each needs a fork we cannot
state yet, because we do not know what those users actually image or what they want out of it, and a
generic recipe over an unknown scenario is the guessed prose this plan's own D5 exists to prevent.

### D3 — Not view profiles, and not access control

`VIEW_PROFILES_PLAN.md` (built) already has a curated-sidebar mechanism, and its Decision 2 is that a
profile describes **who is driving**, per user, not what the data is. A scenario is the other thing:
what the data is and what you want. They compose (a scenario may *mention* that a profile hides the
pages it does not use) and must not merge — a scenario that hid pages would be a profile with an
opinion about biology, and a profile that implied an analysis path would travel between users who do
different work.

### D4 — Where it lives: the Guides picker, not the Welcome page  *(confirmed, Dominik 2026-08-21)*

The compass already means "show me how", it is already in the header on every page, and `GuidesDialog`
already groups and ranks by readiness. A scenario section at the TOP of that dialog is one section in
one component. Considered and rejected:

- **The Welcome page** (`modules/WelcomeModule.vue`) is deliberately empty of copy, on the recorded
  grounds that a paragraph there is read once and skipped forever. A chooser is not a paragraph, so
  this is a real option — but it is only seen at `/`, and the question "which pipeline is mine" recurs
  every time a new dataset arrives, not once at boot.
- **A new page** would need a sidebar entry, which every view profile then has to decide about.

### D5 — The copy is the deliverable, and it is subject to the existing ratchets

**A number appears only where it has been measured** (Dominik, 2026-08-21). "Cell diameter ≈ 10 µm for
T cells" is the most useful sentence in the idea and the easiest to be wrong in public, so a scenario
may state a value only where we measured it on real data and can cite where; everywhere else it names
the control and says what it decides. This also keeps the scenarios from becoming a second, drifting
copy of the task specs' own defaults — which is the failure mode `moduleTask.ts` already warns about
for parameter bullets.

`docs/UI.md` is the ratchet; every guide's prose is already checked for length and shape. A scenario's
`whenThisIsYou` is one line and has to be a *recognition test* ("photon-limited movie of moving cells
in tissue"), not a summary of the steps. Parameter advice must not become a second copy of the task
specs' defaults — cite the control, not a number, unless the number has a measurement behind it.

### D6 — In the code it is a RECIPE, whatever the picker calls it  *(2026-08-21)*

"Scenario" is already load-bearing vocabulary in this frontend: `utils/cssScenarios.ts` +
`cssScenarios.test.ts` own it, and `docs/UI.md`'s own copy rule is *"pick a scenario, then a size"*.
A `ScenarioDef` in `lib/guides/` would make every grep for either concept return both, in a codebase
whose whole doc discipline is one canonical name per job. So: type `RecipeDef`, file
`lib/guides/recipes.ts`, matching this plan's own filename. D1's shape is unchanged — only the noun.

The USER-facing heading is a separate, copy-only decision (D5 applies): proposed **"What are you
trying to do?"** as the first section of the picker, with the guide groups below it unchanged.

### D7 — Intravital needs no new guide; the other three each need one we should not write blind  *(2026-08-21)*

D1 says a recipe step naming a guide that does not exist is a build error. Checked against the
catalogue as it stands (18 guides, `lib/guides/index.ts`):

**Intravital timelapse is fully covered.** Every step it wants already shipped —
`drift-correct` (optional), `train-flow-model`, `segment-by-motion`, `track-cells`,
`behaviour-states`. So the one recipe being built adds **no guide, no anchor, no task**: it is a list
with reasons attached, which is what D1 always claimed it was. Its steps line up on prerequisites too —
the two coastal guides both declare `timeSeries`, so a still-image project reads amber for the whole
recipe rather than half of it.

**The other three each need a guide the catalogue lacks**, which is the second reason they are requests
rather than recipes (the first being that we do not know the scenario):

| recipe | step with no guide | what it would be, when we get to it |
|---|---|---|
| Large multiplex images | **AF correction** | `/cleanup`, task `afCorrect` (`cleanupImages.afCorrect`), `selectionModule: 'cleanup'` — a `moduleTaskGuide()` beside `driftCorrectGuide`, ~20 lines |
| Behaviour/interactions · many small confocal | **spatial interactions** | `/spatial`, `selectionModule: 'spatialAnalysis'` — its own `GuideDef` splicing `taskRunSteps()` twice, ~50 lines |

Both were already named as unbuilt candidates in `GUIDE_SYSTEM_PLAN.md` ("still-obvious candidates NOT
built: spatial neighbour analysis…"). Two findings from this review recorded so they are not
rediscovered the day someone writes them:

- **AF correction is the bare half of a composite.** `cleanupImages.afCorrect` is a constituent of
  `cleanupImages.afDriftCorrect` (`app/src/tasks/cleanupImages/af_drift_correct.json`), so teaching it
  trips the Julia ratchet *a guide teaching a composite's bare half is declared* and needs an entry in
  `bare_by_design` (`app/test/suite.jl`) with the reason. The reason is real and is the mirror image of
  drift's: **a static multiplex slide has nothing to drift-correct**, so AF on its own is the whole
  operation there, not half of one.
- **On `/spatial` the graph comes first.** The page's own header states the order: the squidpy
  neighbour graph (`cellNeighbours`) is the substrate persisted to `spatialGraph/{suffix}.h5ad`, and
  every readout — interaction matrix, contacts, aggregates — LOADS it rather than building its own
  (`SPATIAL_REGIONS_PLAN.md` Decision 17). So that guide is two runs on one page: the `importImages`
  shape, `taskRunSteps()` then again with `withSet: false`. And "interactions" is itself two different
  questions there — a population×population matrix with a permutation test, or per-cell contact
  columns — which is exactly the kind of thing a request has to tell us before it can be a recipe.

### D8 — Readiness is already answered; the old P3 was mis-sized  *(2026-08-21)*

The store already exposes everything this plan's original *"P3 — readiness, if it earns it"* was
going to build:
`guide.prereqState(g)` returns every prereq with a `met` flag **plus a derived miss for pages the
active view profile hides** (`stores/guide.ts` → `profilePrereq`), and `guide.completed` is a `Set` of
finished guide ids persisted as `cc.guide.<id>.done`. So a recipe row showing *"2 steps ready · 1
needs a segmented image"* and a tick per finished step is a few lines in the picker, not a phase —
and it comes with D3's profile interaction handled for free, without a recipe ever mentioning
profiles.

What genuinely remains deferred is the ONE new predicate: `flowModelTrained` needs a new `GuideCtx`
field fed from the `taskDefs` store (the vault arrives as the coastal Model select's `optionsFrom`
options), which is the first prereq to read a store the guide runtime does not already hold. Stays out
of this build; the dependency lives in the copy, as P0 decided.

### D9 — The three we are not writing go in as REQUESTS, and the request asks for data  *(Dominik, 2026-08-21)*

A `RecipeDef` has one of two bodies: `steps` (a written recipe) or `wanted` (a name and a request
link). Both render in the same list, in the same order, because the list IS the answer to "which
pipeline is mine" — someone whose data is a multiplex slide should find their case named, see that
there is no recipe for it yet, and be told what would make one. An absent row teaches nothing; a
named row with a link asks for the one thing we are missing.

**What the request has to ask for is what we cannot guess: what they image, what they want out of it,
and example images.** Every fork in a real recipe (D2) came from knowing a specific dataset — the
intravital one is only writable because `SEG_QUALITY_PLAN.md` measured this lab's own movies. A
generic "large multiplex" recipe written from imagination is precisely the invented prose D5 bans.

Copy: **not a sentence per row.** A request row is the name plus a compact `Request →`. What we want
from them is said ONCE, above the three, in one short line. Three identical CTAs each carrying their
own explanation is the wrapping-per-row failure `GuidesDialog`'s own layout notes already record.

The link lives in `lib/links.ts` — the one place outward URLs live, which exists because three GitHub
literals had been pasted into components — as `recipeRequestUrl(name)`, not a template string in the
SFC. Recommended target: a new `.github/ISSUE_TEMPLATE/recipe_request.yml` asking exactly those three
things, because the existing `feature_request.yml` requires a *proposed solution*, which is the one
thing a requester here does not have. Cheaper alternative if that is one file too many: prefill the
existing template (`?template=feature_request.yml&title=Recipe:%20<name>`) and let the in-app line
carry the "send us images" ask on its own.

**Nothing here posts anything.** The row opens a browser at a prefilled issue — the same mechanism as
the What's New *Report a problem* link. No form in the app, no upload path, no new endpoint.

## Constraints the build has to satisfy

Not advice — these all fail CI:

1. **`guides.test.ts`** — every anchor id exists as `data-guide` in some SFC; every `nav:`/`route`
   names a real route in `main.ts`; ids unique; step text ≤140 chars, ≤4 bullets, each ≤110 chars;
   every `fixGuide` resolves and is not itself.
2. **`app/test/suite.jl` → *guide catalogue names real tasks*** globs **every non-test `.ts` in
   `lib/guides/`**, joins it, and asserts `count(funName:) == count(taskKey:)` plus
   `spec(funName).task == taskKey` against the Julia registry. Consequence for this build:
   **`recipes.ts` must not contain either literal** — a recipe names *guides*, not funs, so this is a
   discipline to state in the file header, not a blocker.
3. **`app/test/suite.jl` → *a guide teaching a composite's bare half is declared*** — see D7.
4. **`uiCopy.test.ts`** (every control needs a `v-tooltip`, copy budgets) and
   **`cssScenarios.test.ts`** (a scoped rule must not shadow the property a `cc-*` utility owns) apply
   to the new picker section like any other markup.

## Phases

**P0 — the coastal guides. DONE (2026-08-21, merged in #610).** Two `moduleTaskGuide()` calls in
`lib/guides/taskGuides.ts` — `train-flow-model` (`opticalFlow.train`) and `segment-by-motion`
(`segment.coastalMeasure`) — plus one line in the cellpose guide's `funHint` saying when not to use it.
Two guides rather than one because they are two runs on two pages, and a model is trained once per
kind of movie then reused across projects. Both ratchets pass. **No `flowModelTrained` prereq** — every
prereq in `prereqs.ts` is answerable from state the frontend already holds, and the vault list only
arrives with the served task spec, so the dependency is stated in the copy instead (see D8).

**P1 — one recipe, three requests, one picker section. BUILT (2026-08-21).** No new guide, no new
anchor, no new task (D7). What landed:

- `lib/guides/recipes.ts` — `RecipeDef` / `RecipeStep` (D1's shape, D6's name, D9's two bodies), the
  `RECIPES` array (intravital + the three requests), `recipeById()`. Exported from `index.ts`.
- `lib/links.ts` — `recipeRequestUrl(name)`, and `.github/ISSUE_TEMPLATE/recipe_request.yml` if D9's
  recommendation stands.
- `guides.test.ts` — every `RecipeStep.guide` resolves via `guideById` (D1's build error); a `wanted`
  recipe has no steps and a written one has at least one, so a half-written recipe cannot ship looking
  finished; ids unique and distinct from the guide ids they compose; `whenThisIsYou` / `why` within a
  one-line budget; no task name in `recipes.ts` (the Julia glob, see *Constraints*); and
  `recipeRequestUrl` points at the recipe form, prefilled.
- `GuidesDialog.vue` — one section above the guide groups. The written recipe is a `.cc-section-toggle`
  row (the canonical bare chevron row, `docs/ui/PRIMITIVES.md`) expanding to its steps: index, guide
  title, the `why` line, a tick from `guide.completed`, and a `Start` that calls the same
  `guide.start(id)` + `closeGuides()` the guide rows already use; readiness per row from `prereqState`
  (D8). The three requests are plain rows plus the one shared ask line.
- Docs, same pass — `docs/UI.md` → *Guides* gains a short **Recipes** subsection (what a recipe is,
  that it composes guides and adds no runtime, where to add one, and that a request is a row not a
  stub); `docs/inventory/FRONTEND.md` on its existing guides line; `docs/todo/README.md` status.
- One stale pointer to fix while here: `lib/guides/taskGuides.ts` cites "WORKFLOW_RECIPES_PLAN P3" for
  the version that could gate on a trained model. That is **D8** now — cite the decision, which is what
  survives a rephasing.
- Verify: `npm run typecheck`, `npx vitest run src/lib/guides`, and `pixi run test-pkg` for the two
  Julia guide ratchets (grep the log for `did not pass` / `Test Failed` — the exit code lies).

**P2 — the copy, as its own read. WRITTEN (2026-08-21), still wants your eyes.** Now small: one `whenThisIsYou`, five `why` lines, three request
names, one ask line. Small does not mean unchecked — `docs/UI.md`'s rule that a guide's prose is an
assertion the ratchets cannot verify applies to every one of them, and the only number that may appear
is the measured cellpose-4-vs-`cyto2` QC pair from `SEG_QUALITY_PLAN.md`, on this lab's own data.

The recipe being written:

| step | guide | the fork it states |
|---|---|---|
| 1 (optional) | `drift-correct` | only if the movie drifts; it makes a version, it does not overwrite |
| 2 | `train-flow-model` | motion, not brightness — trained once per kind of movie, reused after |
| 3 | `segment-by-motion` | **not** cellpose, with the measured QC pair behind it |
| 4 | `track-cells` | the composite, so tracks carry measures |
| 5 | `behaviour-states` | what tracks are for |

**P3 — deferred until a request says what to write.** The multiplex and interaction recipes, the two
guides they need (D7), and `flowModelTrained` as a real prereq (D8). Recipes as drop-in JSON when a lab
asks for its own — declarative until then, so it stays a one-way door only if the type grows
predicates.

## Reservations — all of them, up front

- **Nothing here can be judged from a test.** The new picker section is markup: whether an accordion
  inside a modal that already scrolls reads well, and whether a recipe row's readiness line competes
  with the guide rows' own, are exactly the questions `vitest` cannot answer. P2 stops for Dominik to
  look at it before P3's copy goes in.
- **The recipes duplicate judgement that lives nowhere else.** A fork stated in a recipe ("not
  cellpose, for this data") is a claim with a shelf life — it was true the day cellpose 4 landed. When
  coastal's own quality moves, the recipe is a second place that has to be corrected. Accepted
  deliberately: the alternative is the tip-on-every-control shape this plan already rejected.
- **A request row is a promise-shaped hole in the UI.** "Not written yet — tell us what you image"
  reads as responsive the first time and as neglect the tenth, so the rows are only worth shipping if
  an answered request actually turns into a recipe. Three of them is the most the picker should carry.
- **P2's copy states a fork with a shelf life.** "Not cellpose, for this data" was measured the day
  cellpose 4 landed; when coastal's own quality moves, the recipe is a second place needing the
  correction. Accepted for the same reason as the bullet above it.

## Open questions

1. **The picker heading.** "What are you trying to do?" is the recognition question, and it is longer
   than every other heading in the dialog. Alternative: a plain `Recipes` eyebrow matching
   `Start`/`Data`/`Populations`, with the question as the section's one-line intro.
2. **A `recipe_request.yml` template, or a prefilled `feature_request.yml`?** D9 recommends the new
   template — it can ask for example images, which is the thing that actually unblocks a recipe, and
   the existing one requires a proposed solution the requester does not have. One more file in
   `.github/ISSUE_TEMPLATE/` is the whole cost.
3. **Is "intravital" one recipe or two axes crossed?** Dominik's list mixes *what the data is*
   (intravital, large multiplex, many small confocal) with *what you want* (behaviour, tracks,
   interactions). Moot while only one is written — and the requests are what will answer it, since a
   request names the data and the wanted output separately.

## Not this

- **Not a per-task "recommended for timelapse" badge.** That was the alternative offered and
  rejected: it decorates the symptom (a picker with two options and no guidance) rather than
  answering the question the user actually has.
- **Not automatic task selection**, and not a guide that acts. The guide system's D1 rule stands.
- **Not chain templates.** A chain runs unattended, which is the wrong shape for the first pass over
  new data where every step wants eyes on it. Scenario step 4 of the "many small confocal" recipe is
  *"now do it as a chain"* — the recipe teaches the chain, it is not one.
