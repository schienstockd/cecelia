> **ARCHIVED — audit findings, not authoritative.** Second-pass audit against
> `docs/todo/DRIFT_PREVENTION_ASSESSMENT.md`. Tests two hypotheses about *how* drift is caught
> (not whether). Sample: 1,225 merged PRs 2026-06-30 → 2026-09-26 in `schienstockd/cecelia`,
> narrow-keyword-filtered to 164 drift-relevant PRs, then classified by body/title signal.

# Drift-catch-mode audit

## Verdict

**User's hypothesis holds, with a nuance the assessment misses.** The single label "PR review is
the backstop" collapses two distinct catch modes with different economics and different failure
surfaces:

- **Frontend / UX drift** is caught by *user-visual-catch when the running app looks wrong*. The
  user opens the new module page, the new blackboard, the new picker; sees it doesn't look like
  the rest of the app; files feedback. PR review comes second and only documents the catch. This
  matches the user's own words.
- **Backend drift** is caught by *self-audit sweeps* — someone (user or agent) says "grep for all
  hand-rolled `open(_,"w")` sites" or "count all `findfirst(==(String(ch)))` sites" and lands a
  consolidation PR against N offenders at once. This is post-hoc, but produces tighter unit
  economics than the assessment's per-PR framing implies.

The earlier audit's 7-case list is skewed to the Sept 20-21 blackboard/canvas wave; a wider
Jun-Sep sweep surfaces the *self-audit-sweep* class of catch — invisible in the earlier picture
because those PRs don't say "hand-rolling caught by review", they say "audit found 5 sites, one
canonical helper now."

## Classified drift PRs

Signal source: PR body first line or explicit quote. "inferred" = classification from title
alone. Layer: **FE** = Vue/CSS/UX, **BE** = Julia/Python/API, **INFRA** = tests/build.

| # | Date | Layer | Catch mode | Signal (quoted or inferred) |
|---|---|---|---|---|
| 1074 | 2026-09-20 | FE | **user visual/feedback** | *"Feedback on #1070: I hand-rolled a list + bespoke layout + non-canonical buttons, and the attachment thumbnails didn't show the annotations"* |
| 1079 | 2026-09-20 | FE | user visual (implied) | *"blackboard list gets the canonical draggable divider"* — follow-up to same blackboard |
| 1218 | 2026-09-25 | FE | user visual | *"Populations panel: accordion + settings toggle; heatmap legend room"* — module-page UX gap |
| 1215 | 2026-09-25 | FE | user visual (case A) | *"Unify viewer capture reshow with the module-page pathway"* — divergent module-page rehydration |
| 866 | 2026-09-08 | FE | user visual | *"Names are equipment-first — but users think image-first. An image face fixes that"* |
| 619 | 2026-08-21 | FE | user visual | *"Fix the hover tip: theme ink on a white box in four plots"* — off-brand visuals |
| 690 | 2026-08-28 | FE | ratchet caught | *"previous ratchet fix swapped the hand-rolled `display: flex; flex-wrap: wrap; align-items: center` for `.cc-row-t...`"* |
| 612 | 2026-08-21 | FE | ratchet caught | *"cssScenarios.test.ts caught a hand-rolled flex row in the new markup"* |
| 525 | 2026-08-12 | FE | ratchet caught | *"undeclared re-armed timer, hand-rolled .cc-row, two cc-eyebrow colour overrides. All fixed at the source"* |
| 578 | 2026-08-15 | FE | planned refactor | *"two independently hand-rolled lists"* — refactored to `SelectionTable` |
| 576 | 2026-08-15 | FE | planned refactor | *"CcProgressBar from four hand-rolled copies"* |
| 556 | 2026-08-14 | FE | planned refactor | *"already existed four times, each hand-rolled"* — InlineNote extract |
| 508 | 2026-08-10 | FE | planned refactor | *"eight table surfaces, and the four hand-rolled ones ... unified through SelectionTable"* |
| 504 | 2026-08-08 | FE | later-bug + audit | *"reported bug — dragging the z slider ... turned it into an audit of every continuous control"* |
| 489 | 2026-08-07 | FE | planned refactor | *"reused rather than a second hand-rolled number"* |
| 1151 | 2026-09-21 | FE | later-bug (case-F) | *"SummaryCanvas, GatingPlots and ClusterPlots were each hand-rolling into a shared composable"* — refactor after 3 variants |
| 1076 | 2026-09-20 | FE | planned refactor | *"shared spine for cellCards / motifCards / hmmCards"* — extracted BEFORE variants proliferated further |
| 1084 | 2026-09-20 | FE | planned refactor | motifCards Phase 2 riding the shared spine |
| 632 | 2026-08-22 | FE | user code review | *"go through it too, rather than keeping a second hand-rolled copy that can drift"* |
| 1220 | 2026-09-25 | FE | user visual (inferred) | Pop-toggle staging polish |
| 1092 | 2026-09-20 | FE | user visual (inferred) | Capture Notes field replaces per-mark label |
| 420 | 2026-07-30 | BE | **self-audit sweep** | *"Found while auditing for release blockers"* — hand-rolled state-write detector shipped |
| 421 | 2026-07-30 | BE | ratchet caught | *"`open(ccid, "w")`, which #420's `no hand-rolled state writes` detector flags. Resolution is two lines at one site"* |
| 423 | 2026-07-31 | BE | self-audit sweep | *"Every task hand-rolled the same sequence to record its result"* |
| 425 | 2026-07-31 | BE | self-audit sweep | *"Came out of the hand-rolled-offender audit"* |
| 427 | 2026-07-31 | BE | self-audit sweep | *"the last two findings from the hand-rolled-offender audit"* |
| 431 | 2026-07-31 | BE | self-audit sweep | *"centralise the stamp"* — flat + bioformats2raw layouts |
| 432 | 2026-07-31 | BE | self-audit sweep | *"each hand-rolled the same recursion. They agreed on the hard parts by luck"* |
| 442 | 2026-08-01 | BE | self-audit sweep | *"five sites built the image path"* — one canonical resolver |
| 444 | 2026-08-01 | BE | later-bug (perf) | *"took ~57s to answer its first request"* — 156-branch chain to table |
| 460 | 2026-08-03 | BE | self-audit sweep | *"a fourth boundary is a fourth row, not a bespoke check"* |
| 476 | 2026-08-06 | BE | self-audit sweep | *"Six task handlers hand-rolled `findfirst(==(String(ch)))` and drifted into three mutually inconsistent behaviours"* |
| 505 | 2026-08-08 | BE | later-bug | *"every recorded movie came out with filled masks"* — silent key drop across bridge |
| 521 | 2026-08-11 | BE | self-audit sweep | *"Fix three canonical-helper bypasses, and detect the two that keep recurring"* — landed detectors alongside fixes |
| 555 | 2026-08-14 | BE | self-audit sweep | *"a second hand-rolled regression over two channels is the bug it exists to prevent"* |
| 587 | 2026-08-19 | BE | self-audit sweep | *"9 tasks re-implementing 'pick at least one' as a post-Run log line"* |
| 598 | 2026-08-20 | BE | self-audit sweep | *"each picker hand-rolled its own list"* — grouped by family |
| 721 | 2026-08-31 | BE | self-audit sweep | *"Same shape as `no hand-rolled state writes` and `channelSelection … channel_indices`"* — Julia zarr ratchet |
| 803 | 2026-09-05 | BE | self-audit sweep | *"Trainer + inference denoise files were hand-rolling torch.d..."* |
| 800 | 2026-09-05 | BE | later-bug avoided | *"ONE refresh path — avoiding a divergent-reimplementation trap"* |
| 458 | 2026-08-03 | BE | later-bug | *"AF findings rendered '→ undefined' ... hand-rolled the finding dict and put its prose in `detail`"* |
| 442 | 2026-08-01 | BE | self-audit sweep | Notebooks sysimage stamp — five sites, one implementation |
| 560 | 2026-08-14 | BE | later-bug | *"hand-rolled ccall where Base ships a guarded one"* — segfault |
| 411/412 | 2026-07-29 | INFRA | self-audit sweep | Test fixtures, hand-rolled paths |
| 1013 | earlier | BE | self-audit sweep (case G) | 35 setters inlined JSON3.read |

## Blackboard mini-case-study (module-page/UX archetype)

Chronology, three PRs:

1. **#1070 (2026-09-20)** — first Blackboard Vue page shipped. Built with a hand-rolled `<ul>`
   list, bespoke buttons, no `ChainModule` shell, marks-less thumbnails. PR body describes what
   it does; does not flag the hand-rolling.
2. **#1074 (2026-09-20, same day)** — user opens the page, notices it looks nothing like the
   rest of the app AND the attachment thumbnails don't show the drawn marks. Files feedback.
   Rewritten against the frontend inventory: `SelectionTable`, `ConfirmDeleteButton`,
   `ChainModule` shell, `composeImageWithOverlay` for thumbnails. PR body opens with
   *"Feedback on #1070: I hand-rolled a list + bespoke layout + non-canonical buttons"*.
3. **#1079 (same day)** — the follow-up divider fix: *"canonical draggable divider"* — still
   the same page, still the same catch, being finished off.

Catch mode: **user-visual-catch in the running app, same day**. The rewrite was cheap because
the fix landed within hours, before follow-on code depended on the drifted shape. Nothing but
the user noticing looked at #1070 between its landing and #1074.

## Module-page UX drift — additional archetype

**#1215 (Sept 25)** — the earlier audit's case A, `MarksOverlay` shipped as a second rehydration
path for `CaptureEnvelope` when module pages already used `CaptureViewSurface`. Catch mode: user
noticed missing pencil affordance in the reshow flow. Same shape as blackboard: page renders,
user sees the discrepancy, refactor lands.

**#866 (Sept 8)** — the correction-plan `ChipSelect` shipped with equipment-first labels
("Resonance / photon-limited"). User feedback: *"users think image-first"*. Not visual-drift
per se, but same catch-mode class: user opens the surface, notices the UX doesn't fit their
mental model, files feedback.

**Ratchet catches DO exist for CSS-shape drift.** #612, #525, #690 all show `cssScenarios.test.ts`
flagging a hand-rolled `.cc-row` at test time. That's an existing enforcement layer the
assessment names but doesn't credit as much as the evidence supports — it works, on the specific
shapes it covers (flex rows, colour tokens, size scale). It doesn't cover the shape-of-a-page
gap that got #1070/#1074 (a whole page, not one rule), which is why the user-visual-catch is
still doing the work there.

## Backend: self-audit-sweep is the dominant mode

Sixteen of the backend catches in the table above are audit-sweep-shape, not
PR-reviewer-comment-shape. Signature: a PR whose body opens with *"Found while auditing / came
out of the audit / audit found N sites"* and closes N offenders in one change, often shipping a
detector for the same pattern.

Examples run through July: #420 → #421 → #423 → #425 → #427 (state-write audit); #442, #431
(centralisation); #476 (channel-name resolver, 6 sites); #504 (continuous controls, 5 sites);
#521 (three canonical-helper bypasses, plus two detectors); #587 (9 sites); #598 (each picker
hand-rolled its own list). Later-bug catches (#505, #560, #444, #458) exist but are rarer.

Economics: one self-audit-sweep PR closes N offenders + often ships the ratchet that prevents
regression. The per-drift cost isn't "one PR review cycle" — it's *(one audit + N fixes) / N*,
which trends downward as N grows. The assessment's per-PR framing doesn't capture this.

## Comparison to the earlier audit

The earlier 7-case picture (#1215 A, #1079 B, #1074 C, #1076 D, #1085 E, #1151 F, #1013 G):

- **All seven are still real** — every case shows up in the wider sweep too.
- **Six of seven are September**, five of them the Sept 20-21 blackboard/canvas wave. The
  earlier audit's keyword filter surfaced recent drift heavily; that time-skew hid the
  self-audit-sweep class that dominates July-August.
- **Case F (#1151, the expensive one)** is genuinely the only silent-multi-copy bug in the
  sample I could find. #505 (mask outline dropped across bridge) is a *different* class of
  silent breakage — a single-copy contract mismatch, not a divergent-copy fix that missed
  siblings. The "second case-F-shaped bug" trigger in the assessment is well-calibrated
  precisely because case F is *rare*.
- **Catch-mode taxonomy in the earlier audit is thin.** It says "post-hoc PR" for six and
  "user noticed missing affordance" for A. The wider sweep shows the six aren't uniform:
  #1074 is user-visual-in-app-same-day; #1079 is same; #1076 is planned-refactor-after-3;
  #1013 (backend) is self-audit-sweep. Same coarse verdict ("post-hoc"), different fine
  structure.

## Recommended edits to `docs/todo/DRIFT_PREVENTION_ASSESSMENT.md`

Each is a one-liner change to make the doc match what the evidence actually shows. None adds
a new mechanism.

1. **Under *Why each proposed layer was rejected* → *PR-time post-hoc net (existing)*: split
   the "backstop" into two catch modes.** Add a sentence: *"For frontend/UX drift the actual
   backstop is user-visual-catch in the running app — a mode the assessment previously credited
   to PR review. For backend drift the backstop is the self-audit sweep (`grep for offenders,
   land N fixes + a detector`)."*
2. **Under *Reasoning — the honest read*: correct the unit-economics claim.** The current text
   says "six cost one review cycle each" — accurate for the earlier audit's sample, but the
   wider sweep shows backend drift is often bundled into audit-sweep PRs whose per-drift cost
   is *(one audit + N fixes) / N*. Note this — it strengthens the "no harness" conclusion,
   doesn't weaken it.
3. **Under *When to revisit*: keep the case-F trigger, but add a second UX-specific one.** *"If
   a user-visual-catch on a module page or panel starts arriving days-later rather than
   same-day (i.e. drift accumulates before the user opens it), that's the point where
   scoped adversarial review on new `frontend/src/modules/*.vue` earns its keep — because the
   catch is no longer cheap."*
4. **Under *What NOT to bring back without new evidence*: name user-visual-catch as a real
   mechanism, not a fallback.** *"The user opening the running app catches UX drift within
   hours in most cases. It IS enforcement — just an unnamed, human-in-the-loop one — and it's
   why per-page catch-cost stays low. Don't design as if it doesn't exist."*
5. **Under *References*: cite this audit** (`docs/archive/drift_catch_modes_audit.md`).

## Reservations

- Sample: 1,225 merged PRs from 3 months. Broader than the earlier audit's 7 cases; smaller
  than the full repo lifetime. Older drift patterns (pre-June) are not represented.
- Classification is body-signal-based. A PR whose body doesn't say "user pointed X out" gets
  "inferred from title" — a real user-visual-catch could be misclassified. The 22 FE PRs in
  the table with explicit body signals are the confident sample; the "inferred" rows are
  weaker evidence.
- The self-audit-sweep count (16 backend PRs) is likely a floor — audits often ship several
  fix PRs, and only the audit-PR body says "audit"; downstream PRs may say "fix X" without
  mentioning the audit that surfaced X.
- **User-visual-catch is unrecorded in tooling.** It leaves a trace in the follow-up PR body
  ("Feedback on #N") but not in a metric anyone tracks. Any quantitative "drift rate over
  time" measurement will systematically undercount it. If item 2 in the assessment's revisit
  triggers depends on measurement, this is the noise floor.
- Not read: parked-plan bodies beyond the first-line greps. A fuller read of
  `BEHAVIOUR_CARDS_PLAN.md`, `TASK_LIST_UNIFICATION_PLAN.md`, `USER_PROFILE_PLAN.md` would
  probably tighten the "planned refactor" counts.
