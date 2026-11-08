> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in [`docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md`](../todo/IMAGE_ANALYSIS_RECITAL_PLAN.md).
>
> **Outcome:** not run as of archiving (2026-11-08, `origin/main` 355850d3). The reviewer read d3acf56,
> the first B6 slice; #1985 then reworked the GUI and the review before merging. Read against main:
> - **Stage chips and the success toast are gone.** The footer is one button with a popover of
>   checks grouped by category; "Unit" is now Replicate (`unitAttrs`, can combine attributes).
> - **R5 is half done.** The review is now told what the checks found and asked not to repeat it
>   (`claim_review_prompt`). Review section ids are still keyed by text.
> - **R6 is worse than stated.** `get_image_info` returns the image `note`, so withholding
>   `get_image_notes` doesn't withhold notes. `read_lab_log`, `get_notebook` and the boards are allowed.
> - **R8 within-image is already handled.** S1.betweenGroups compares populations per image.

# CLAIM_RECITAL_RESERVATIONS_PROMPT

Verify and plan only. Do not implement. Output: `docs/todo/CLAIM_RECITAL_RESERVATIONS.md`. Self-contained: do not assume other context.

## Goal

A reviewer outside the project raised reservations about the claim recital as built so far. Check each one against the code, say whether it holds, and propose the smallest fix that would address it. Be willing to conclude that a reservation is wrong or already handled.

## What was read, and what was not

The reservations come from reading **two things only**: commit d3acf56 ("recital: state a claim from a summary plot": `frontend/src/components/canvas/ClaimStrip.vue`, `frontend/src/utils/claimApi.ts`, `SummaryPanel.vue`, docs) and `api/src/claim_review.jl`. The reviewer did **not** read `app/src/claim_checks.jl` (the rule checks) or the plan. Treat every reservation as a hypothesis to verify, especially where it depends on backend behaviour.

Context: `docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md`. The design is that rules (S0 record, S1 consistency, S2 conventions, S3b perturbations) decide, a model only reads (S3a translator, S4 open review), the review is advisory, and nothing blocks an export. The GUI takes one sentence plus a unit attribute, and the scope (set, populations, measure) comes from the plot.

## Reservations to check

### R1. The rules do not read the sentence, so green chips can be misread

The chips (Record, Consistency, Conventions) are computed from the plot's scope and the unit attribute. The sentence appears to be stored and sent only to the model review. A pass therefore means "the comparison drawn here is supportable", not "the sentence is supported". A sentence such as "in all mice, after treatment" can pass if the plot is a simple pooled comparison.

Check: does `claim_checks.jl` use the claim text anywhere? What do the footer chips and tooltips say about what was checked? Propose: wording that says the checks cover the comparison drawn, not the sentence; and a structured claim field set (for example direction or claim type: A > B, A ≠ B, association), pre-filled from the plot, so S3b does not need free-text parsing. Say how the sentence and these fields stay consistent.

### R2. The review prompt has no explicit sentence-versus-scope check

The system prompt asks what would change the analysis (missing data, populations measured differently, statistical unit, repeated tests, dropped images, unfollowed log flags). The user prompt gives the sentence, set, populations, measure and unit and asks whether the record supports the claim "as stated". Overreach in the wording (all, after treatment, causal language, strength) seems to be caught only implicitly, by the single optional `interpretation` finding.

Check: does the current prompt reliably produce wording findings? Propose: one explicit instruction to compare the sentence's population, group, condition, direction and strength claims with the scope, and a test set of sentences that overreach and sentences that do not, with expected outcomes.

### R3. Test leakage: the review prompt contains the P0 findings

The system prompt lists exactly what the P0 hand-walk found (2D vs 3D tracks, other parameters or gates, cells or tracks pooled vs animals, the same test run many times, images that left the set). On the P0 project and claim (4kS67f, set XcPcu8), a finding by the model cannot be told apart from the model repeating what it was told.

Check and propose an evaluation design: held-out projects and claims not used to write the prompt; two prompt variants (with and without the P0-derived list) to measure what the list adds and whether the model generalises; the planned comparison against a plain "review this claim" prompt run on the same record access (including logs) and not on the P0 project. State what each variant is meant to show.

### R4. A restated claim can silently discard a finished review

`_claim_review_done!` writes nothing if the claim's round changed while the review ran, which is right. The frontend shows a success toast ("Review is on the Blackboard") when the turn appears in the Kiwi feed, regardless of what the backend did.

Check: can a user restate during the roughly one-minute review and get a success message with no review written? Propose: how the UI should show a stale or discarded review (and whether to keep it, marked as belonging to an earlier round, rather than dropping it), and a test for the restate-during-review case.

### R5. Rule findings and model findings can overlap, and IDs are keyed by text

The review is not told what the rules found, so it may re-report the same problems. Review sections get IDs keyed by the finding's text, so a reworded duplicate becomes a new section.

Check how often this happens on real runs. Decide, with reasons: keep the review independent and merge or tag duplicates for display, or pass the rule findings so the model focuses on what rules missed (and say what independence would be lost). Propose how duplicates and outcome tags (Good / Bad / Unsure) behave across rounds.

### R6. Leak-by-default tool list

`claim_review_tools()` is all Kiwi read tools minus a hard-coded withheld list. A read tool added later that exposes the analyst's reasoning would be allowed to the review by default.

Check: list every tool in `KIWI_READ_TOOLS`, classify each as exposing analyst-authored content or not, and check that none of the allowed ones leaks reasoning (task log free text, notes, entries). Propose inverting to an allowlist, or a test that fails when a read tool is added without a classification.

### R7. The default unit attribute is a name heuristic

`defaultUnitAttr` picks the attribute named like the animal; with no exact match it picks the first attribute (for example "Mousetrap" or "Location"). A wrong unit silently changes what S2 counts as n.

Check what S2 reports when the unit is wrong or odd. Propose: show the chosen unit and the resulting number of units in the footer, flag when the default came from a non-exact match, and treat "none" as an explicit state.

### R8. Carried over from the earlier plan review (confirm whether already addressed)

- A within-image check that populations compared within an image were measured the same way (dims, parameters, interval), not only across images per population.
- Findings that carry how sure they are (inferred vs reproduced) and the options for what the analyst can do next, including a matched-dims recompute as an S3b perturbation where saved tracks allow it.
- "No findings" never reads as a pass: say what ran, what was skipped and what was not checked.
- A gap in one stage skips only the checks that need the missing part, and the plan's Approach says so.
- Evaluation includes what the analyst does with a finding (changed analysis, restated claim, kept with a reason, ignored), rounds per claim, and a plain-prompt baseline.

## Output requirements

- A table: reservation, verdict (holds / partly / does not hold / already handled), evidence with file and line references, severity for the recital's goal (helping the analyst, not blocking them), proposed fix, size, test.
- For each fix: whether it can start during the hardening phase.
- A prioritised order, smallest and most reliable first.
- Separate verified facts (from the repo) from assumptions and open questions for the maintainer.
- Say where a reservation is wrong, and what you could not verify.
- No new features beyond the fixes. Claims stay advisory and nothing blocks an export.
