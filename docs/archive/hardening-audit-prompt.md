# HARDENING_AUDIT_PROMPT

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

Read-only audit of Cecelia (currently 0.2.12). Do not change source. Write findings to
`docs/todo/HARDENING_AUDIT.md`, and the matrix to `docs/HARDENING_MATRIX.md`.

## Purpose

The main UX has landed, including Claude and Kiwi. The next phase (candidate: 0.3.0) is
making it solid enough to use without hurdles, as a long-term commitment to a scientific
image analysis tool, not a feature race.

Some surfaces are shallower than they look: a button, tool or guide exists, but the
behaviour underneath is thin. Track and label correction are known to be early. Find out
where else this is true.

**The question is depth, not breadth. Do not propose new features.**

## Ground rules

1. **Evidence over narrative.** Cite file and line, a test, or something you ran. Mark each
   claim `observed` or `inferred`.
2. **Do not trust our own prose.** CHANGELOG and PR text were largely written from commit
   messages. Treat them as claims to verify.
3. **Run things** rather than concluding from reading code. A test existing is not coverage:
   say what input would make it fail.
4. **Current design is not the spec.** It is mostly one maintainer deciding and Claude Code
   building, not validated with users. If a surface's shape looks wrong for a real user,
   say so with evidence.
5. **Try the second case, not the demo case:** the second image, second segmentation,
   multi-group, old crop, shared install.
6. **State what you did not look at.** Silence is not a pass.
7. **No fixes.** One or two lines on the shape of a fix at most.

## Depth score

Score each user-facing surface 0-4 by the highest level you can evidence.

| Level | Meaning |
|---|---|
| 0 | Named or stubbed; no working path |
| 1 | Works on the demo path only |
| 2 | Works on realistic input; errors are surfaced, not swallowed |
| 3 | Level 2, plus automated coverage that would catch a wrong result |
| 4 | Level 3, plus provenance and a safe upgrade path |

Build the surface list from the CHANGELOG, in-app guides, MCP tools and task registry.
Include Claude/Kiwi, install/update/HPC, and the core pipeline (import, segment, gate,
track, cluster, figures, chains).

## What to look at

- **Provenance and staleness.** Does an output record what produced it? When a default
  changes, can the app flag stale results, or only the CHANGELOG ("Read this first")?
- **Result correctness.** Is there a golden dataset that fails CI on drift? Look for the
  class of bug that gives plausible wrong answers (wrong segmentation returned, wrong
  group's motion, stale metadata) and for the same shape elsewhere.
- **Claude and Kiwi.** Trace one real request per responsibility end to end. Where is
  behaviour enforced in code, and where only requested in a prompt or guide? What happens
  on auth expiry, tool error, cancelled or concurrent sessions? Is the Kiwi/Claude split
  described consistently across UI, docs and MCP?
- **Release verification.** What exercises the core workflow in a real browser before a
  tag? Which merged features have never been seen running?
- **User-facing failure modes.** Install, update, restart mid-task, out of memory, no GPU,
  shared machine, HPC. Clear message, silent wrong result, or hang?
- **Platform parity.** Linux, macOS, Windows: decision or drift?
- **Upgrade contracts.** Which saved chains, settings, boards and stores survive an
  upgrade, and which need "re-run to fix"?
- **Diagnosability.** What do we get when a user reports a problem?

## Maturity and feedback

- Which surfaces are closer to early-alpha than the UI suggests? Base this on scores, not
  on the maintainer's feel.
- Can a user see how mature a surface is, and report "this is wrong" cheaply? Users often
  work around a problem and never report it.
- What local friction signals (repeated undo, task cancelled and re-run with changed
  params, flow abandoned, same error twice, export-and-reimport workarounds) already leave
  evidence in logs, run logs or task history? Nothing leaves the machine.
- Which early surfaces should sit behind an opt-in toggle so new users cannot hit them?
- List 3-5 first-run workflows a person who is not the maintainer should be observed doing
  on their own data. Usability cannot be audited from the repo.

## Enforcement (design only, after the audit)

The audit is worthless if new features can keep shipping as plain "Added". Design a
binding rule and a check. Do not implement them.

**Rule (human-owned; draft the text for CLAUDE.md and RELEASING.md):**
- Every user-facing surface has one manifest entry: name, path globs, tier, evidence.
  The matrix from this audit is its first content.
- New surfaces enter as Experimental. Promotion to Stable needs level 3 evidence: a
  result-checking test or golden dataset, provenance recorded, a recorded browser pass.
- A tier changes only by editing the manifest, in the same PR as the evidence.
- CHANGELOG entries carry the tier. Experimental surfaces do not count toward a
  feature-complete claim.

**Recital check (mechanical; it flags, it does not judge):**
Read how recital works today (stamps, findings, `python/cecelia/effectiveness/`) and
propose a check that fits it. At commit, map changed files to surfaces via the globs, then:
1. Path claimed by no surface: flag as unregistered.
2. Tier raised but the evidence is not in the diff or repo: **block**.
3. Result-changing change (default, algorithm, output columns) with no "re-run" CHANGELOG
   note and no provenance or version change: **block**.
4. UI badge and manifest disagree: flag.

It stamps one line in the commit, e.g. `Surfaces: gating=Stable (unchanged)`. Any depth
estimate it gives is advisory text in a finding. The badge always comes from the manifest,
never from the agent's judgement. Block only on 2 and 3; blocking on every surface-touching
commit will drown a maintainer running 10+ sessions.

Flag anything in recital's current design that makes this check unreliable or noisy.

## Output

`docs/todo/HARDENING_AUDIT.md`, in this order:

1. **Summary** (five lines): ready to declare feature-complete?
2. **Ranked gaps** (max 15): severity, surfaces, evidence, `observed`/`inferred`, rough fix.
3. **0.3.0 gate**: checklist of concrete conditions someone else could verify.
4. **Design questions**: where the current shape of a surface looks wrong, with evidence.
5. **Enforcement draft**: the manifest format, the rule text, and the recital check spec.
6. **Do not do yet**: hardening that is not worth it now.
7. **Not audited**, and **open questions** you could not resolve from the repo.

`docs/HARDENING_MATRIX.md`: one row per surface with depth score, evidence, the next gap,
and a proposed tier (Stable needs level 3+; otherwise Beta or Experimental). Keep it a
tracked file, to be re-scored and diffed at every minor release.

Severity: **blocker** = can give a wrong scientific result without any signal, or loses
data; **serious** = blocks a normal user from a core workflow or needs support to recover;
**minor** = friction or a clearly messaged edge case.

## Notes for the run

Added on review (2026-11-02), before the audit was run.

1. **Prior decisions on enforcement.** `docs/todo/DRIFT_PREVENTION_ASSESSMENT.md` declined a
   general prevention harness (2026-09-26), and `docs/todo/GOVERNANCE_LAYER_AUDIT_PLAN.md` →
   `docs/archive/governance_layer_audit.md` audited the governance layer for sprawl. A manifest plus
   a blocking recital check is that kind of harness. Read both first; the enforcement design must
   either fit inside them or say, with evidence, why the decision should change.
2. **"Provenance" here means output provenance** — what produced a given result (task, params,
   version, model). It is not `docs/PROVENANCE.md`, which records how the software was built and
   validated.
3. **Version.** Score against the latest tag at the time of the run (`git describe --tags`), not the
   number in this brief.
4. **Output homes.** `docs/todo/HARDENING_AUDIT.md` is deliberate: it is the 0.3.0 work list, not a
   frozen record. `docs/HARDENING_MATRIX.md` is the living, re-scored file.
5. **Don't break the maintainer's machine.** Never start or kill servers (8080/5173/7655 are his).
   Use an isolated `CECELIA_DEV_DIR`; never write the shared dev config or call
   `set_projects_dir`. Run tasks via `run_task`/`run_tasks` in the REPL. Any `claude -p` used to
   trace Claude/Kiwi sets `CECELIA_OBSERVER_NO_PAIR=1`. Copy project data before running anything
   that writes to it.
6. **Browser passes belong to the maintainer.** List the merged features with no recorded browser
   pass and the exact steps for each; do not claim a pass you could not watch.
7. **Run it split, not as one session.** One agent per area under *What to look at*, then one pass
   that merges scores into the matrix, then the enforcement design last, reading the merged result.
8. **Second-case data.** There is one user so far, on intravital data. Use the maintainer's current
   projects and test set `jFWePN` (3 tracked live images), and record which data each score rests
   on. A score earned only on intravital data from one lab is not general; say so. For each module,
   name the small self-contained test image that would catch a wrong result and how it differs from
   intravital data. These feed a test-set section in the in-app call for datasets.
9. **The 0.3.0 gate is an ambition, not a release condition.** Write it as what "solid" would mean.
