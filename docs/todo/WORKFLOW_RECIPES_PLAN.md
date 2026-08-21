# Processing guides for common scenarios — "what are you trying to do?"

**Status:** planning (2026-08-21). No branch. Came out of the cellpose 4 migration
(`CELLPOSE_V4_PLAN.md`), which asked the wrong question first.

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

`docs/UI.md` → *Guides* / `docs/todo/GUIDE_SYSTEM_PLAN.md`. Built, shipped, 16 guides:

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

## The gap this has to close first, and it is concrete

**There is no guide for `segment.coastal` or `opticalFlow.train`.** The only segmentation guide,
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

## Phases

**P0 — the coastal guides. DONE (2026-08-21, on `feat/cellpose-v4`).** Two `moduleTaskGuide()` calls
in `lib/guides/taskGuides.ts` — `train-flow-model` (`opticalFlow.train`) and `segment-by-motion`
(`segment.coastalMeasure`) — plus one line in the cellpose guide's `funHint` saying when not to use it.
Two guides rather than one because they are two runs on two pages, and a model is trained once per
kind of movie then reused across projects. Both ratchets pass: the frontend anchor/copy-budget test
and the Julia `funName`/`taskKey`-pair test. **No `flowModelTrained` prereq** — every prereq in
`prereqs.ts` is answerable from state the frontend already holds, and the vault list only arrives with
the served task spec, so the dependency is stated in the copy instead (see P3).

**P1 — scenarios as data + the picker section.** Types, `lib/guides/scenarios.ts`, the section in
`GuidesDialog`, the build-time check that every referenced guide id exists, `docs/UI.md` entry.

**P2 — the four scenarios written.** Content, reviewed as copy.

**P3 — readiness, if it earns it.** A scenario could tick off finished steps (`cc.guide.*.done` is
already persisted) and grey out steps whose prerequisites are unmet (`prereqs.ts` already answers
this). Cheap, but it is polish on top of a thing whose value is the words. The one prereq worth adding
here is **`flowModelTrained`** with `fixGuide: 'train-flow-model'`: it needs a `GuideCtx` field fed
from the `taskDefs` store (the vault arrives as the coastal Model select's `optionsFrom` options), so
it is the first prereq that would read a store the guide runtime does not already hold.

## Open questions

1. **Is "intravital" one scenario or two axes crossed?** Dominik's list mixes *what the data is*
   (intravital, large multiplex, many small confocal) with *what you want* (behaviour, tracks,
   interactions). Two axes read truer but square the matrix. A flat list of the four common
   combinations, named for how someone would recognise their own work, is probably right — and is
   what the table in D2 assumes.
2. **Who can add one?** If scenarios are TypeScript in `lib/guides/`, only we can. If they are
   drop-in JSON like view profiles and custom modules, a lab can ship its own — the same argument the
   plugin system already won. Starting in TS and moving to JSON later is a one-way door only if the
   scenario type grows predicates.

## Not this

- **Not a per-task "recommended for timelapse" badge.** That was the alternative offered and
  rejected: it decorates the symptom (a picker with two options and no guidance) rather than
  answering the question the user actually has.
- **Not automatic task selection**, and not a guide that acts. The guide system's D1 rule stands.
- **Not chain templates.** A chain runs unattended, which is the wrong shape for the first pass over
  new data where every step wants eyes on it. Scenario step 4 of the "many small confocal" recipe is
  *"now do it as a chain"* — the recipe teaches the chain, it is not one.
