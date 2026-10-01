# Maintainability enforcement — parked plan

**Status:** **P0–P4 built** (2026-10-01) on `feat/maintainability-lint`. Open: watch the
`[wrong home]` outcome tags and the lint's warnings for a few weeks, then revisit *Deferred*.

**Related:**
- Brief: [`../archive/fold_MAINTAINABILITY_into_convention_check_prompt.md`](../archive/fold_MAINTAINABILITY_into_convention_check_prompt.md) — the source prompt, archived. Its three-way split stands; its code pointers were stale (see *Corrections to the brief*).
- The standard being enforced: [`../MAINTAINABILITY.md`](../MAINTAINABILITY.md).
- Reviewer being extended: [`../ai-assist/CONVENTION_CHECK.md`](../ai-assist/CONVENTION_CHECK.md) (plan: [`CONVENTION_CHECK_PLAN.md`](CONVENTION_CHECK_PLAN.md)).
- Template for the mechanical check: `python/cecelia/effectiveness/inventory_coverage.py` (#1297), which replaced the retired citation-currency check ([`GOVERNANCE_LAYER_AUDIT_PLAN.md`](GOVERNANCE_LAYER_AUDIT_PLAN.md) Item 4).

## Goal

`MAINTAINABILITY.md` calls itself "the check that runs *before* new code lands", but nothing enforces
most of it. Enforce the parts that can be enforced, through the two mechanisms recital already runs
(the convention-check reviewer and mechanical checks), and make the doc say which parts are enforced.

## Corrections to the brief (verified 2026-10-01 on origin/main)

- `app/src/tasks/scheduler.jl` is now a 30-line aggregator over `app/src/tasks/scheduler/*.jl`
  (807 lines, five files). Its header marks the **child files** as concurrency-critical.
  `MAINTAINABILITY.md:262` still says "733 lines".
- The six stringly state machines are already `@enum`: `TaskStatus`, `ChainScope`,
  `ChainBarrierPolicy`, `ChainNodeStatus`, `ImageStatus`, `PopType`. `MAINTAINABILITY.md:210`
  still describes them as open.
- Marker set: `OUTCOME_VOCABULARY` holds outcomes, not markers, and `_MECHANICAL_RUN_COUNT` is for
  mechanical checks. The markers are listed in `recital.py` (`_CONVENTION_MARKER`),
  `.claude/hooks/check_commit_recital.py` (`_FINDING_MARKERS`), `console.py` (the marker colour
  table) and `EFFECTIVENESS_METHODOLOGY.md` (the `convention_check_finding` verdict enum).
- "For now … handles" has zero matches in source; plain "for now" has 11.
- 28 of the 77 files under `app/src/tasks/` are already over 200 lines.

## Decisions (2026-10-01)

1. **Keep the brief's three-way split.** Type-shape reuse goes into `should reuse`. Placement goes
   into a new `[wrong home]` marker. Incident history, file size and protected-comment trims go
   into a mechanical check. Docstring shape and the "third responsibility" test stay deferred.
2. **Item 1 adds no marker.** `should reuse` points at the existing enums (see *Corrections*) and
   typed boundary structs (`AfCombinationSpec`, `AfChannelStats`) as the canonical equivalents.
3. **`[wrong home]` is outcome-tagged, like `should reuse`.** It's a judgment finding, and the
   outcome tag is the only record of its false-positive rate. Without that record it can never
   earn its place or be cut, which is how citation-currency was judged. The hook requires the tag.
   Recital emits one pending `convention_check_finding` row per bullet.
4. **Convention-check's job widens to comments.** Today it only sees "new named entities" and skips
   diffs with no additions, so a misplaced comment in a modified hunk never reaches it. Added or
   changed comment lines become addition-shaped for `[wrong home]` only. The "no additions" valve
   stays for everything else.
5. **Mechanical check triggers must be quiet on old debt.** They fire on what the diff does, never
   on what a touched file already is:
   - Size: the diff pushes a file under `app/src/tasks/**` past 200 lines, or adds ≥20 lines to a
     file already over 200.
   - ~~Protected trims~~ — cut by P0 (see *Replay results*).
   - Incident history: only inside comment lines. Dataset IDs = six-character tokens with mixed case,
     minus the word shapes the replay found. Plus dated authorship, phase codes and `commit <sha>`.
     (Quoted user reports were cut by P0.)
6. **One opt-out: `MAINT-EXEMPT: <reason>`,** the same shape as `INVENTORY-EXEMPT`/`COHORT-EXEMPT`/`DASK-OK`.
   Not one per sub-check. The provenance comment on a constant (`smoothVis.ts`) is covered by the
   "comment sits next to a numeric constant" heuristic, failing safe (don't flag). The marker is the fallback.
7. **"For now … handles" is cut before it's built:** 0 hits in the repo. Revisit only if a real case turns up.
8. **Prove each sub-check against old PRs before wiring it in**, as #1297 did, with the hit rate
   recorded here (*Replay results*) and pointed to from the module docstring — not recited in it,
   which is the narrative `wrong home` exists to catch (it caught exactly that on this PR). A sub-check that never catches anything real is cut, and the
   plan says so. *Amended by P0:* 600 PRs, not 150 — the last 150 touched no task file.
9. **One shared diff parser: `git_context.parse_diff`.** Both mechanical checks build on it; the
   inventory check's `new_files_from_diff` / `exempt_files_from_diff` / `new_routes_from_diff` keep
   their names but now call it instead of regexing the raw text. No check imports from a sibling check.

## Replay results (P0, 2026-10-01)

The last 150 merged PRs (#1162–#1315, two weeks) touched no task file, so size and protected trims
couldn't fire there. The replay was widened to 600 PRs (#712–#1315, from 2026-08-31). "Later
cleaned" = the flagged text or size no longer on main, i.e. someone fixed it by hand afterwards.

| Sub-check | PRs flagged | Hits | Labelled | Later cleaned | Verdict |
|---|---|---|---|---|---|
| Dated authorship | 29 | 71 | all real | 63 | kept |
| Dataset uid | 29 | 95 | all real after filter | 10 | kept |
| Commit SHA | 4 | 7 | real | 5 | kept |
| Phase code | 1 | 5 | real | 5 | kept |
| Quoted user report | 3 | 4 | 1 real, 1 an error message, 2 deleted | — | **cut** |
| Size (task files) | 16 | 24 | real under the rule; `af_correct`, `omezarr.jl` ×3 flagged before their hand splits | 4 | kept |
| Protected-comment trim | 1 | 1 | the scheduler split (#1010) moving comments, not deleting them | — | **cut** |
| "For now … handles" | — | — | 0 matches in the repo | — | **cut** (not built) |

The uid filter was tightened on this same replay (it first let `GitHub`, `SetBar`, `UiMark`,
`show3D`, `cpSAM2` through), so its precision here is optimistic. `test_maintainability_lint.py`
pins all 18 real uids it found. The size check also excludes the framework dirs (`task/`, `chain/`,
`scheduler/`, `testTasks/`), which added 13 hits of pure noise.

## Reviewer checks (P1 + P2, 2026-10-01)

The widened prompt, run through recital's own `claude -p` path against real diffs:

- `db8275e5` (correction-plan engine): `source::Symbol` (5 values) and `validation_status::Symbol`
  → `potential duplicate` pointing at `TaskStatus` / `ImageStatus`; `wizard_answers::Dict` crossing
  API↔Julia → flagged; a rejected-alternative comment → `wrong home`. `exclusion_reason::Union{Nothing,String}`
  was judged not a discriminant (included/excluded are separate vectors), which is correct.
- `d24e18b9` (Kiwi refs): `Dict{String,Any}` result crossing API→frontend with a 4-value string field → flagged.
- `de8149cd` (correction staleness): a rejected alternative in the header → `wrong home`. "Old R
  shipped them without a peep" was judged design motivation, not a code citation. Defensible.
- The reverse of `378ae2c9` (re-adding `TaskJob.imgs::Union{Nothing,Vector{CciaImage}}`) →
  `should reuse` pointing at `TaskJobTarget`.
- Negative control, `6346e818`'s `scheduler/run.jl`: no findings; moved code was not treated as new,
  and the header's ordering contract was recognised as protected.

## Phases

- **P0 — Replay (decides P3's scope). ✅** A script over the 150 merged PRs for each item-3 sub-check,
  using the Decision 5 triggers. Output per sub-check: PRs fired, locations flagged, a hand-labelled
  real-vs-noise sample. Checkpoint: Dominik sees the table before anything is wired.
- **P1 — Type-shape reuse (prompt edit). ✅** Three example additions in step 1 of `CONVENTION_CHECK.md`,
  three grep sets in step 4 (`@enum`, `struct .*Spec`, `Union{Nothing`). Check it against one real past
  diff per category, and record the result here.
- **P2 — `[wrong home]`. ✅** Prompt: marker definition with its exceptions (bridging code, protected
  comments, constant provenance) and the widened scope (Decision 4). Wiring: `recital.py` convention
  markers become a pair; add it to the hook's `_FINDING_MARKERS`; the console colour; the methodology
  verdict enum. Tests: recital emitting a `[wrong home]` row, the hook requiring its tag. (The
  exceptions are prompt judgment, checked by the reviewer runs above, not unit-testable.)
  Check it against one real past diff.
- **P3 — `maintainability_lint.py`. ✅** Only the sub-checks that survived P0. Pure diff → locations
  functions, `MAINT-EXEMPT`, `maintainability_lint_run` in `EVENT_TYPES` and `_MECHANICAL_RUN_COUNT`,
  a `_Maintainability lint: <verdict>_` tail line, and called from `recital.py` next to `run_inventory_check`.
- **P4 — Docs. ✅** `MAINTAINABILITY.md`: fix the stale scheduler and enum lines, and mark each rule
  *enforced by* (reviewer / lint / CI) or *reference only*. The intro line then describes what is
  actually enforced. Update `GOVERNANCE_INDEX.md` and `CONVENTION_CHECK_PLAN.md` in the same PR,
  plus an outcome note on the archived brief.

## Deferred

- **Docstring shape (2–4 lines):** too soft to grade. Revisit after a few weeks of P2 outcomes.
- **"Acquires a third responsibility":** needs a whole-file judgment, not a per-addition one.
  Accepted gap. Only the line-count half of the split rule is enforced.
