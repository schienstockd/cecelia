> **ARCHIVED — audit prompt, not authoritative.** Read-only audit brief evaluating whether the existing drift-prevention mechanisms (`INVENTORY.md`, discovery-first `CLAUDE.md` rule, `docs/MAINTAINABILITY.md`, CSS/UI/backend ratchets) are actually preventing divergent reimplementation, or only documenting it after the fact. Outcome note to be added under this banner once the audit ships.

# Audit: Drift-Prevention Mechanism (Cecelia dev stream)

**Model:** Opus (audit/judgment pass, not implementation)
**Type:** Audit prompt — read and report, do not modify code or config in this pass.
**Reference:** https://code.claude.com/docs/en/best-practices (Anthropic's official Claude
Code best-practices guide — read this before starting the audit; specific sections cited below).
**Also read:** `docs/MAINTAINABILITY.md` in full before starting — it is itself one of the
mechanisms under audit (see item 7).

## Why this audit

We have several mechanisms meant to stop agent sessions from hand-rolling
components, CSS, or backend logic that already exist elsewhere in the repo:
`INVENTORY.md` as a canonical-component index, a discovery-first rule in
`CLAUDE.md`, `docs/MAINTAINABILITY.md` as a living correctness/structure
standard, and existing CSS/UI/backend ratchets. In practice, sessions still
reimplement things these mechanisms were meant to prevent — the PR trail
already has plenty of documented instances. This audit exists to find out
*why*, using those real failures as evidence, and to determine whether the
existing ratchets are actually earning their keep or just documenting
problems after the fact — not to add more mechanisms on top of ones we
haven't confirmed are working.

## Ground truth from Anthropic's own docs

Two things from the official best-practices guide bear directly on this audit,
so start from them rather than re-deriving the same conclusions from scratch:

- **"Unlike CLAUDE.md instructions which are advisory, hooks are
  deterministic and guarantee the action happens." / "Use hooks for actions
  that must happen every time with zero exceptions."** — this is Anthropic's
  own framing of exactly the enforcement-level question in item 1 below. If
  discovery-first only lives in `CLAUDE.md`, the docs themselves predict it
  will sometimes be skipped — that's not a bug in our setup, it's documented
  behavior of advisory instructions. The existing CSS/UI/backend ratchets and
  `MAINTAINABILITY.md` fall on the advisory side of this line too, unless
  something enforces them mechanically — that's exactly what item 7 below
  needs to establish.
- **"The over-specified CLAUDE.md" failure pattern** — Anthropic names this
  explicitly: *"If your CLAUDE.md is too long, Claude ignores half of it
  because important rules get lost in the noise... If Claude already does
  something correctly without the instruction, delete it or convert it to a
  hook."* Check whether our `CLAUDE.md` has grown to the point where the
  discovery-first rule is one instruction among many and getting diluted,
  independent of whether a hook exists yet. The same question applies to
  `MAINTAINABILITY.md` itself as it grows — see item 7.

Also relevant, as a second-layer option rather than a fix for the root
cause: the guide's **adversarial review step** pattern — a subagent that
reviews only the diff, in a fresh context, against stated criteria, without
seeing the reasoning that produced it. This is a plausible additional layer
alongside the write-time gate and post-hoc PR review (see "Proposed
direction" below), distinct from both because it runs in-session, after
implementation but before the human ever sees the diff.

## What to audit

1. **Enforcement level of each existing mechanism.** For each of
   `INVENTORY.md`, the discovery-first `CLAUDE.md` rule, `MAINTAINABILITY.md`,
   and the CSS/UI/backend ratchets: is it enforced (a hook or CI check that
   can block/fail) or advisory (a rule/doc the agent is trusted to consult
   and follow)? Cite the actual file/config that enforces it, or state
   plainly that none exists and it's currently advisory only. Frame the
   finding using Anthropic's own advisory/deterministic distinction above,
   not just "seems to work" or "seems to not work."

2. **Where in the session lifecycle each mechanism is checked.** Session
   start (loaded into context automatically vs. requiring the agent to think
   to read it), write-time (before a file is created/edited), or post-hoc
   (PR review / CI, after the code already exists). Flag any mechanism that
   only fires post-hoc — that's a detection step, not a prevention step, and
   should be labeled as such rather than assumed to be preventing anything.

3. **Test against real past failures, not hypotheticals.** The PR trail
   already contains plenty of occurrences of divergent reimplementation and
   app-wide standards being ignored — do not search for hypothetical
   examples. Pull the actual instances from PR history. For each: what
   existed already, what got reimplemented instead, whether the discarded
   attempt (if any) shows the agent ever consulting `INVENTORY.md`,
   `MAINTAINABILITY.md`, or the relevant standard before writing the
   duplicate/violation, and what the PR review comment (if any) said when it
   was caught. Then, for each proposed mechanism layer (write-time gate,
   session-start injection, adversarial review subagent), state explicitly
   whether it would have caught *that specific* instance — not "this class
   of problem in general." A mechanism that can't be shown to catch a real
   logged failure isn't validated yet.

4. **INVENTORY.md staleness as a contributing cause.** Check whether
   `INVENTORY.md` itself is current. A discovery-first rule pointing at a
   stale index will fail to catch a duplicate even when followed correctly.
   Report what fraction of recently-added canonical components are actually
   listed, and whether any of the PR-trail failures in item 3 involve a
   component that was missing from the index at the time.

5. **CLAUDE.md size and signal-to-noise, per the over-specified-CLAUDE.md
   pattern above.** Roughly how many instructions does our `CLAUDE.md`
   currently carry, and does the discovery-first rule stand out or is it
   buried among unrelated rules? If Claude already follows some of the
   other instructions correctly without needing them stated, the guide's own
   advice is to delete those lines or convert them to hooks — check whether
   that pruning has ever been done, or whether the file has only grown.

6. **Session-length effects.** Where possible, note whether the PR-trail
   failures cluster later in long sessions (consistent with instruction
   dilution / context pressure) or happen early too (which would point at
   the rule simply not being read/found, not at it being forgotten or
   crowded out).

7. **Effectiveness of the existing CSS/UI/backend ratchets and
   MAINTAINABILITY.md specifically — the core new question for this pass.**
   `MAINTAINABILITY.md` states it is "the check that runs *before* new code
   lands" and that findings from every future audit update the file rather
   than spawning a new one. Establish, with evidence, whether that's true in
   practice or aspirational:
   - Is there anything that actually runs *before* new code lands and
     checks against it (a hook, a CI lint step), or is "before new code
     lands" describing intent rather than a real gate? If nothing enforces
     it, it's a reference document an agent may or may not consult, not a
     check.
   - Pull the concrete examples the doc cites (the six-instance
     `@enum`/state-machine pattern from the Phase-4 audit is one) and check
     whether new code written *after* that pattern was documented has
     repeated it anywhere. If the pattern recurs post-documentation, the
     doc's checklist isn't preventing what it describes — it's cataloguing
     it after the fact.
   - Same question for the CSS/UI ratchets: are they a CI check with a pass/
     fail gate, or a style-guide document? If they're CI checks, what do
     they actually verify (visual regression, class-naming convention,
     duplicate-component detection) and does that verification method
     overlap with or differ from what a `PreToolUse` inventory-check hook
     would catch?
   - Would converting any of `MAINTAINABILITY.md`'s checklist items into
     hooks (write-time) or a Stop-hook gate (session-end, per Anthropic's
     Stop hook pattern) actually be tractable, given how many of the items
     are structural/mechanical (untyped dict at a boundary, file over ~200
     lines, missing `# invariant:` prefix on a flagged pattern) versus how
     many require judgment (is this cross-reference "genuinely load-
     bearing," is this file's third responsibility actually a landing
     spot). Sort the checklist items into "mechanically hookable now,"
     "hookable with a cheap model call," and "needs human/adversarial-
     review judgment" — this sorting is the deliverable, not a yes/no.

## What NOT to do in this pass

- Do not propose a full redesign yet. First establish what's actually
  happening across items 1-7 above, grounded in the real PR-trail instances.
- Do not assume the fix is "add a stronger rule to CLAUDE.md" or to
  `MAINTAINABILITY.md` — per Anthropic's own guidance, if the finding is
  that existing rules are advisory, the fix is enforcement mechanics
  (hooks/gates), not more prose, and a longer document is more likely to
  hurt than help.
- Do not substitute a general argument for a specific one. Every claim
  about a mechanism's effectiveness should trace to a named PR-trail
  instance it did or didn't catch, or to a specific checklist item from
  `MAINTAINABILITY.md` and whether it recurred after being documented.
- Do not treat `MAINTAINABILITY.md`'s existence as evidence it's working.
  A well-written standard that nothing enforces has the same enforcement
  profile as no standard at all — item 7 exists specifically to test this.

## Proposed direction to evaluate (not to implement yet)

We're considering a three-layer harness, and want item 7's findings to
determine whether `MAINTAINABILITY.md`'s mechanical checklist items should
be folded into the same hooks rather than staying a separate, parallel,
advisory document:

- **Write-time gate:** a `PreToolUse` hook on `Write`/`Edit`, scoped to the
  directories where reimplementation actually recurs (Vue components, Julia
  modules, CSS), that checks the proposed new symbol/component against
  `INVENTORY.md` before the write is allowed, and blocks with a message
  naming the existing match and asking the agent to reuse it or justify a
  new one. Candidate for folding in: the mechanical `MAINTAINABILITY.md`
  checks identified in item 7 (untyped dict at a boundary, file-length/
  responsibility split, missing `# invariant:` prefix).
- **Session-start injection:** a `SessionStart` hook that loads the current
  `INVENTORY.md` (and/or a compressed `MAINTAINABILITY.md` digest) into
  context automatically, so discovery doesn't depend on the agent
  remembering to look.
- **In-session adversarial review:** before a change is treated as done,
  a subagent reviews the diff in a fresh context against `INVENTORY.md` and
  `MAINTAINABILITY.md`'s judgment-requiring items (cross-reference
  load-bearing-ness, "for now this just handles X" red flags, third-
  responsibility creep), following Anthropic's documented adversarial-
  review pattern, scoped to what item 7 sorts as non-mechanical.
- **Post-hoc net (existing):** keep PR-time review and the current ratchets
  as the backstop for whatever the earlier layers' necessarily-heuristic
  matching misses.

Run each real PR-trail instance from item 3 through this design on paper:
would the write-time gate, or the adversarial reviewer, have flagged the
actual duplicate or violation that was written, given what `INVENTORY.md`
and `MAINTAINABILITY.md` contained (or didn't) at that time? Report
per-instance, per-layer pass/fail, not an aggregate impression.

## Reference repos — inspiration, not gospel

These are examples of people building similar mechanisms, included so we're
not reinventing terminology or missing an obvious existing approach. Most of
them are small, recent, low-adoption projects (some are solo tools or
hackathon submissions) — treat as "here's how someone else framed this
problem," not as validated, battle-tested infrastructure. Read for the
mechanism, not the implementation. Anthropic's own docs (linked above) take
precedence over all of these where they conflict.

- **Sorbet's gradual-typing ratchet pattern** (Stripe's Sorbet type checker,
  well-established, unrelated to AI agents) — the pattern worth borrowing:
  a baseline of pre-existing offenders is locked in at a point in time, and
  the check only fails on *new* violations, never on the ones already there.
  Relevant if `INVENTORY.md`/`MAINTAINABILITY.md` enforcement needs to start
  without blocking on the backlog of existing violations (e.g. the six
  known enum-pattern instances already catalogued).
- `0xwilliamortiz/ratchet` — closest conceptually to our problem: closes the
  loop on "the ruleset goes into the prompt, the agent reads it, and nothing
  ever checks whether it actually followed it."
- `leonkacowicz/ratchet` — language-agnostic structural-metric snapshot +
  CI regression gate. Useful for the "how do you snapshot a metric and block
  regressions" mechanics, not for the discovery/dedup problem specifically.
- `mehmethk88-dot/eval-gate-ratchet` — detects when an agent "fixes" a
  failing check by quietly weakening the check itself rather than the
  underlying code. Worth checking our own ratchets aren't gameable this way.
- `praveenvijayan/Ratchet` — an example of `PreToolUse`/skill-based hook
  wiring for Claude Code specifically; useful as an implementation reference
  for the write-time gate design above.
- PyPI `drift-analyzer` — commercial-ish tool for cross-file structural
  drift detection (duplicate helpers, boundary violations) with a
  "baseline + ratchet" CI mode. Post-hoc detection, not write-time
  prevention — relevant to the post-hoc net layer, not the gate.

## Output format

- Findings for items 1-6, each with file/config citations or explicit
  "no enforcement found" statements — no speculation presented as fact.
- Item 7's checklist sort (mechanically hookable now / hookable with a
  cheap model call / needs judgment), as an explicit table against
  `MAINTAINABILITY.md`'s actual checklist items.
- A table or list of PR-trail instances from item 3, each with a per-layer
  pass/fail against the proposed three-layer design.
- A short verdict on whether the proposed direction addresses the actual
  root cause(s), or needs to change first — explicitly say whether the root
  cause looks more like "no enforcement," "stale inventory," "over-specified
  advisory documents," or "genuinely judgment-dependent and no hook will
  fix it," since each implies a different fix.
- List anything from the reference repos that changes the proposed design,
  with a one-line reason each.