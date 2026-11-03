# Feature maturity — tiers on the guides, badges, report

**Status:** **P1 + P2 built** (2026-11-02); P3 open. Decisions 1–9 agreed in discussion; starting tiers
seeded from `FEATURE_MATURITY_TIERS.md` — the 7 Tested proposals are seeded as Beta with a "maintainer
review → Tested" item, pending his review of each fails-if line.

**Related:**
- Parent: [`HARDENING_AUDIT_PLAN.md`](HARDENING_AUDIT_PLAN.md) — the depth scores and gaps that fill
  the graduation checklists; [`HARDENING_SURFACES.md`](HARDENING_SURFACES.md) — the surface inventory
  (which tasks have no guide); [`HARDENING_RESULT_CORRECTNESS.md`](HARDENING_RESULT_CORRECTNESS.md) —
  per-feature gaps and the test evidence that exists.
- Data gaps: [`CALL_FOR_DATASETS_PLAN.md`](CALL_FOR_DATASETS_PLAN.md) — `frontend/src/lib/callForDatasets.ts`.
- Guides: `frontend/src/lib/guides/` (`types.ts` → `GuideDef`, `mapData.ts` → `MAP`), the map at the
  front of the Guides dialog (`components/GuideMap.vue`, joins in `utils/guideMap.ts`), its checks in
  `utils/guideMap.test.ts`, and `user-docs/README.md` generated from the same data.
- Pill extended: `components/ComingSoonBadge.vue` ("the ONE way to signal an incomplete surface").
- Release checklist: [`../RELEASING.md`](../RELEASING.md).
- Starting tiers: [`FEATURE_MATURITY_TIERS.md`](FEATURE_MATURITY_TIERS.md) — 58 features, provisional, with his
  decisions of 2026-11-02.

## Goal

Know where we're at, and let a user see it too: how mature each feature is, what it has been tested
on, what would move it up, and what data would help. And see where guides are missing — either
because nobody wrote one, or because the feature isn't there yet. Users often work around a problem
instead of reporting it; seeing that a feature is early makes "should I report this?" easier, and the
report should cost one click.

Precedent: spacr labels every feature Stable/Beta/Alpha in a table.

## Decisions (agreed 2026-11-02)

1. **Badge, never hide.** Early features stay reachable; no opt-in toggle.
2. **The scale below, with Stable reserved and unused.** Each tier names the evidence it takes, not
   how it feels.
3. **Tiers live on the guides.** A guide teaches one feature, so the guide carries that feature's
   tier. The Guides dialog's map becomes the maturity overview; no new header modal.
4. **A feature with no guide is listed, not invisible.** It gets a tier too and shows on the map as
   "no guide yet". That list is the gap list: either write the guide, or the feature isn't ready.
5. **Every feature has an explicit tier.** No silent default; a test fails on an untiered page or task.
6. **Only the maintainer promotes** (today: Dominik). A tier changes only by editing the registry, in a
   PR that carries the evidence; the maintainer merges it. An agent may open that PR and argue for it, never promote.
7. **Demotion on a confirmed wrong-result bug.** A Tested feature with a confirmed wrong-result bug
   drops to Beta in the fix PR, until a test guarding that bug lands.
8. **Promotion happens in the PR, and is swept at the release cut** — not in the weekly judge.
9. **Features with no scientific result are exempt** (blackboard, Movies, Tasks, tour, profiles): a tier
   says how far to trust a result, and these produce none. No badge, no maturity row.
10. **On the map, a dot per tier with a legend, not a pill.** A pill on every page button was clutter
   (nearly every page is Beta); an unexplained dot, or marking only Experimental, said nothing. One
   `.cc-dot` tone per tier, a legend line under the grid (2026-11-02, from his look at P1).
11. **Test sets go through the call for datasets** — one registry, a `kind: 'build' | 'test'` field;
   the modal shows "Help us build" then "Help us test" (`CALL_FOR_DATASETS_PLAN.md`, Decision 9). Every
   record with `needs` carries a `datasetAsk`, or a `noAsk` reason when the need isn't data (an HPC
   site); `maturity.test.ts` enforces it.

## The scale

| Tier | Meaning | What the user should do | Requires |
|---|---|---|---|
| Coming soon | Not usable yet | Nothing; it's a placeholder | Nothing |
| Experimental | Runs, but not checked on realistic data. May be wrong; may change between patches | Check every result by hand | An explicit entry |
| Beta | Works on real data, and someone has checked the results by hand | Spot-check results. Result changes are announced in the CHANGELOG | `testedOn` filled in honestly; who checked and when; a dataset ask linked if there is a known data gap |
| Tested | An automated test fails when the result is wrong | Use it normally. Result changes are announced in the CHANGELOG | A result-checking test, its path recorded as evidence |
| Stable | *Reserved, unused.* Provenance recorded, upgrades safe | Rely on it across versions | Audit level 4: outputs record what produced them, and saved work migrates |

- **Breadth is not level.** `testedOn` ("intravital 2-photon, one lab") is separate from the tier.
  Outside users and other data widen `testedOn`; they don't gate a tier. Requiring another person's
  feedback for Tested would leave the tier empty until there are outside users.
- **No re-run promise yet.** "Result changes come with a re-run note" needs enforcement (the deferred
  recital check in `HARDENING_AUDIT_PLAN.md`); until then the promise is only the CHANGELOG line.
- **Today gating and cluster assignment are both Beta.** The difference shows in `testedOn` and
  `needs` (cluster assignment needs multiplex data). Gating is the first candidate for Tested.

## Proposed shape

- **`GuideDef` gains a `maturity` record:**
  `{ tier, testedOn, checkedBy?, evidence?, next?, needs?, datasetAsk? }`.
  - `checkedBy` — who checked results by hand, at which version, on which data (image or dataset
    id). Beta's human attestation can't be tested and goes stale as code changes, so the map shows its
    age and the release-cut sweep lists old ones. "Code changed since" needs a feature → code-path
    map, which is deferred; until then age is the signal.
  - `evidence` — per claim: test file + testset/test name, and a "fails if…" line saying what wrong
    result the test would catch, so whoever promotes can judge it (Tested).
  - `next` — the graduation checklist: 1–3 items, each open or done. Filled from the hardening
    audit's per-feature gaps, not from opinion; refreshed when the matrix is re-scored.
  - `datasetAsk` — the call-for-datasets entry id.
- **Features with no guide** get the same record in one list beside the guides (e.g.
  `lib/guides/unguided.ts`), keyed by page route or task.
- **Tests.** `guideMap.test.ts` already proves every sidebar page has a map entry. It extends to:
  - every page and every registered task is taught by a guide or listed as unguided, with a tier;
  - every Tested feature's `evidence` paths and test names exist and are not marked skipped or broken
    (`@test_skip`/`@test_broken`, `skip`/`xfail`, `it.skip`), so deleting or disabling the evidence
    fails CI until the tier drops or the evidence is fixed. A test that skips only when a fixture file
    is absent is fine — the fixture is committed;
  - every `datasetAsk` id exists in `callForDatasets.ts`.
- **The map shows it, one expandable row per feature.** Collapsed: the badge. Expanded: tested on,
  evidence, the "to reach the next tier" checklist, and "needs data" with the call-for-datasets link —
  for users that reads as "your data could help here". Unguided features show as a dimmed "no guide
  yet" row. `user-docs/README.md` gets the tier column for free.
- **Coming soon stays on the flags that already carry it** — the task JSON `comingSoon` /
  `comingSoonNote` (`frontend/src/tasks/types.ts`, read in `utils/taskGating.ts`) and the nav item
  `soon` (`lib/navGroups.ts`, rendered in `AppSidebar.vue`). The map derives Coming soon from them; a
  `maturity` record covers Experimental and up, and the test fails on a feature that is both flagged
  `comingSoon` and given a tier. One source per fact.
- **Tiers on the existing pill.** `ComingSoonBadge` gains a `tier` prop; "Coming soon" is the bottom
  of the same scale.
- **A quiet "maturity" link in every page header** that opens that feature's map row — so no badge
  doesn't read as "fine" (with nothing Tested yet, an unbadged page is Beta: spot-check results).
- **Badges on the pages** for Experimental and Coming soon only: page header and the task in the task
  picker. Nothing is Tested at the start, so badging Beta would mark every page and mean nothing. The
  map shows every tier.
- **"Something wrong?"** next to each badge and on each map row: opens a prefilled GitHub issue with
  feature, tier, version, OS and the last task name. Task params only on an explicit tick — GitHub is
  public and params carry paths and project names. The URL builder lives in `lib/links.ts`, next to `recipeRequestUrl` and `CECELIA_NEW_ISSUE_URL`.

## Promotion and demotion

- **In the PR.** The PR that ticks the last `next` item also proposes the tier change in the registry.
  The discussion happens there; merging is the maintainer's decision.
- **At the release cut.** A step in `docs/RELEASING.md`: list features whose `next` checklist is
  complete but tier unchanged, and confirmed wrong-result bugs in Tested features since the last tag.
  Also list Beta attestations older than N releases. An agent drafts the list; the maintainer signs
  off. The CHANGELOG carries the tier lines at the same time.
- **Not the weekly judge.** It runs unattended and fixes; promotion is the maintainer's call and rare. Later, if a
  demotion slips past a release cut: the judge may flag "confirmed wrong-result bug in a Tested
  feature" as an issue. It never changes a tier.

## Example — gating (Beta → Tested)

Evidence that exists today (read 2026-11-02): gate membership against known answers incl. a logicle
gate selecting by raw value (`app/test/suite/gating.jl` → "Gates"), logicle golden values vs
FlowUtils ("Transforms"), a real-fixture gate matching an independent count
(`app/test/suite/pop_df.jl` → "pop_df integration (KDIeEm)"). `next`:
- ☐ Compare channel names when copying gates or pooling across images; refuse or warn on mismatch
  (`popmanager/copy.jl` checks column presence only — hardening gap 2).
- ☐ A test that copies a gate onto a fixture with swapped channels and asserts it does not silently
  select the wrong marker.

Not checked: whether nested populations have a real-data test that a child is a subset of its parent.

## Gating goes through the loop first

Before the registry is built, gating's two `next` items land in their own PR (channel-name check on
copy/pool + the swapped-channel test), and this plan's gating example records them as evidence. That
tests the process end to end on one real feature; P1 then seeds gating as Tested with that PR as its
evidence, and the registry shape is informed by a real case.

## Phases

- **P1** — **built.** `maturity` on every guide + the unguided list + the tests; expandable rows on the map;
  tiers in user-docs. Starting tiers drafted from the inventory and step 2, corrected by him, marked
  provisional until the matrix re-scores them. Where it landed:
  - Records: `lib/guides/maturityRecords.ts` (`GUIDE_MATURITY`, keyed by guide id; each guide reads its
    own) and `lib/guides/unguided.ts` (`UNGUIDED`); the type is `Maturity` in `lib/guides/types.ts`. A
    record's `covers` lists the task fun_names / routes it tiers beyond the guide's own run and page; a
    composite's steps are covered through the composite. `handCheck` points at a written hand-check
    (PROVENANCE.md and co.) where there is no dated `checkedBy`.
  - Logic + checks: `utils/maturity.ts` / `maturity.test.ts` — every page and every task JSON under
    `app/src/tasks/` tiered exactly once; flagged Coming soon never tiered; every evidence path and test
    name exists (all tiers), none skipped or broken for Tested; every `datasetAsk` exists; copy budgets.
  - Map: the page buttons carry the badge; a Maturity section (`components/MaturityMap.vue`) lists every
    non-exempt feature by group, expandable; Coming soon rows derive from the task flags. Guide rows in
    the picker carry the badge. Words of the scale: `lib/guides/tiers.ts`.
  - user-docs: a Maturity column in the sidebar tables.
- **P2** — **built.** Badges on pages and the task picker; "Something wrong?" link. Where it landed:
  - Pages: `components/PageMaturity.vue` in the app HEADER, following the route — pages have no shared
    header (ModuleLayout, ModulePage and the pipeline pages each draw their own), so one mount covers
    every page. Experimental / Coming soon: `ComingSoonBadge tier` (now the tier dot + word, no pill).
    Any other tiered page: a quiet `pi-signal` icon. Both open the map on that feature's row
    (`openMaturityRow`, `lib/guideOpen.ts`). Exempt pages show nothing (no row to open).
  - Task picker: TaskRunner's `<option>` gets `· Experimental` (text only in a native select); the picked
    task gets the badge + report button. Coming soon tasks already read "— Coming soon" and can't be
    picked. The Chains palette is not badged.
  - "Something wrong?": `components/ReportProblemButton.vue`, a popover → `problemReportUrl`
    (`lib/links.ts`) on the bug form: feature, tier, page, version, OS (the server's pixi platform), last
    task name + status. **Params: an explicit per-report toggle in the popover**, never remembered.
  - Logic + tests: `pageMaturity` / `taskMaturity` / `pageBadged` / `taskOptionSuffix` in
    `utils/maturity.ts` (every page and task link lands on a map row); `lib/links.test.ts`.
- **P3** — the release-cut step in `RELEASING.md`; CHANGELOG entries carry the tier.

## Deferred

- Prompts at task end/failure in early areas, and local friction counting (re-runs with changed
  params, cancels, repeated errors). Worth it once badges and the report link are in use.
- The weekly judge flagging demotions (see above).
- Recital check on tiers (from the hardening brief) — flag-only at most; see `HARDENING_AUDIT_PLAN.md`.

## Open questions

- Starting tiers — drafted in [`FEATURE_MATURITY_TIERS.md`](FEATURE_MATURITY_TIERS.md); 7 proposed for
  Tested pending his review of each fails-if line; cluster assignment needs his PROVENANCE.md line.
- A guide that spans several features (a recipe, or segment → measure): tier the feature it is named
  for; a recipe shows each step's badge.
