# Hardening audit — plan

**Status:** **in progress** (2026-11-02). Step 1 done (surface inventory); step 2 starting with result correctness. Steps 1–4
are agreed in principle; the enforcement steps are deliberately deferred.

**Related:**
- Brief: [`../archive/hardening-audit-prompt.md`](../archive/hardening-audit-prompt.md) — includes
  *Notes for the run*.
- Step 1 output: [`HARDENING_SURFACES.md`](HARDENING_SURFACES.md) — surface inventory, draft, 175 rows.
- Step 2a output: [`HARDENING_RESULT_CORRECTNESS.md`](HARDENING_RESULT_CORRECTNESS.md) — result correctness, draft.
- Test-data route: [`CALL_FOR_DATASETS_PLAN.md`](CALL_FOR_DATASETS_PLAN.md) — the in-app
  call-for-datasets registry (`frontend/src/lib/callForDatasets.ts`).
- Prior decision: [`DRIFT_PREVENTION_ASSESSMENT.md`](DRIFT_PREVENTION_ASSESSMENT.md) — declined a
  general prevention harness.
- Prior audit: [`GOVERNANCE_LAYER_AUDIT_PLAN.md`](GOVERNANCE_LAYER_AUDIT_PLAN.md) →
  [`../archive/governance_layer_audit.md`](../archive/governance_layer_audit.md).
- Prior enforcement work: [`CLAUDE_MD_ENFORCEMENT_PLAN.md`](CLAUDE_MD_ENFORCEMENT_PLAN.md) — ratchet
  tests for grep-able rules; the cheap, already-accepted kind of enforcement.
- Release process: [`../RELEASING.md`](../RELEASING.md).

## Goal

Before 0.3.0, find which user-facing surfaces are shallower than they look, score them 0–4 by
evidence, and say what would make each one solid. Depth, not new features.

## Where it came from

A conversation about release pace in AI-assisted tools. Two comparable projects, spacr and
BEHAV3D-Explorer, ship as fast as Cecelia. Speed doesn't separate a solid project from a shaky one;
what does is whether users can tell which parts are solid (spacr labels features
Stable/Beta/Alpha) and whether a result can be traced to the settings and version that produced
it.

## Constraints that shape it

- **The 0.3.0 gate is an ambition, not a release condition.** The audit's checklist says what
  "solid" would mean; it does not hold a tag.
- **There is one user, on intravital data.** Every score rests on the maintainer's current
  projects. Other people's images fail in other places, and that has happened repeatedly. So a
  score of 2 or 3 means "on intravital data from one lab" until someone else's data has been through
  it, and the matrix should say which data each score rests on.
- **Robustness is expected to come from other people's data**, over time. The audit's job is to make
  that possible: say which modules need which test images, and make reporting a problem cheap.

## Steps

| # | Step | Who | Decision |
|---|---|---|---|
| 1 | **Surface inventory**: every surface from CHANGELOG, guides, MCP tools, task registry | 1 agent | Do first, alone, and review before step 2. |
| 2 | **Per-area audits**, smallest first: result correctness, provenance/staleness, upgrade contracts | ≤3 agents | Do. The other 5 areas only if these earn it. |
| 3 | **Matrix + ranked gaps**, each score naming the data it rests on | 1 agent | Do, after 2. The audit proposes the matrix layout; judge it once there is one to look at. |
| 4 | **Test-set needs**: per module, the small self-contained image that would catch a wrong result, and what makes it different from intravital data | from 3 | Do. Feeds a test-set section in the call for datasets. |
| 5 | **Browser passes** on features nobody has seen running | Dominik | Do, from the list the agent writes. |
| 6 | **First-run observation** by a non-maintainer | — | Not possible yet; no other users. Comes with the first external dataset. |
| 7 | **Manifest + tier badges** | — | Design only, after 3. Flag-only first. |
| 8 | **Blocking recital check** | — | Don't yet. See below. |
| 9 | **Opt-in toggle for Experimental surfaces** | — | Don't yet. Decide from the matrix. |

## Test sets via the call for datasets

The call for datasets currently lists capabilities **not built** for lack of data
(`CALL_FOR_DATASETS_PLAN.md` decision 8: an entry is removed once built). A test-set ask is
different: the module exists, and the data would check it. Either widen the registry with a second
kind of entry ("help us test this") or give test sets their own section in the same modal. Decide
when step 4 produces the first entries, not before.

## Why not the blocking check yet

- It is the harness `DRIFT_PREVENTION_ASSESSMENT.md` declined, and nothing new has happened since to
  reopen that decision. A matrix showing blocker-class gaps that shipped as plain "Added" would be
  that evidence; the audit produces it or it doesn't.
- Rule 3 ("result-changing change") is a judgement, not a path match. A mechanical check either
  misses default changes or flags every touch to a task file.
- Path globs mapping files to surfaces will rot as fast as the frontend churns.
- A flag-only version (unregistered path, badge ≠ manifest) costs little and shows how noisy it
  would be before anything blocks.

## Out of scope

- A try-before-install demo (BEHAV3D-Explorer has a Colab notebook that opens the GUI on precomputed
  data). Lowers the first barrier, but it is a feature, not hardening.

## When to un-park

0.2.12 is tagged.
