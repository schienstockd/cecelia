> **ARCHIVED — audit prompt, not authoritative.** Brief for a direct-API Kiwi service sharing the
> `cecelia-observer` MCP server with Claude Code (2026-09-23). Not run as written. **Outcome:** the
> design lives in [`docs/todo/KIWI_ASSISTANT_PLAN.md`](../todo/KIWI_ASSISTANT_PLAN.md). Premises that
> turned out wrong: no direct API (no Console access — v1 engine is the `claude` CLI, so no shared key
> or per-user logging); `hypothesis` + mandatory `action` replaced by an `observation` /
> `interpretation` / `question` split because an action is a recommendation; "WebGPT / verified quotes
> (Menick et al. 2022)" is two papers (WebGPT = Nakano 2021, GopherCite = Menick 2022). The plan's
> *Prior art* section is the verified version of the list below. Do not act on this file as a spec.

# Audit prompt: shared MCP server, two consumers — Kiwi direct-API service + Claude Code

## Context

Kiwi is moving from "chat inside Claude Code, reachable via MCP" to
"its own structured duck," per `kiwi-duck-not-oracle.md` — replies must
always ground in a pointee (a `dataRef`, plot ID, capture ID, module/run
ID, or viewer t/z), never freeform prose alone. That requires calling
the Anthropic API directly with enforced structured output, which
Claude Code's own agent runtime doesn't give you control over.

Concern going in: does structured Kiwi require a second, parallel MCP
tool implementation to maintain alongside whatever Claude Code already
talks to? No — the tool server (get_capture, list_plots, get_landscape,
lineage, etc.) is business logic that doesn't care who calls it. Only
the system prompt / output-schema enforcement layer is consumer-specific.
Audit and confirm this split, then plan the two pieces.

## Target architecture

**One MCP tool server** (existing tool set: captures, plots, landscape,
lineage, outcome tags/blackboard, etc.) — unchanged, single codebase,
no fork.

**Two consumers of that same server:**
1. **Claude Code** — existing setup, freeform chat, its own agent loop.
   Escape hatch for exploratory/ad-hoc work. Whether this stays
   available at all is a product/governance decision (see Open
   questions), not an engineering constraint — don't let ease-of-reuse
   here quietly settle that decision.
2. **Kiwi backend service** — new. Also an MCP client of the same tool
   server, but instead of Claude Code's runtime, makes its own direct
   `api.anthropic.com` calls. This is the only place the
   pointee-required schema and system prompt live.

## Kiwi service requirements

- **Structured output enforcement**: every reply must resolve to claims
  of type `observation` (requires a `support` pointer: dataRef/capture/
  landscape-tile/etc.) or `hypothesis` (requires an `action`: a concrete,
  checkable next step — "try AF correction on this frame," not a
  verdict). No claim may exist without its paired field. Enforce via
  the API call's structured-output/tool-schema mechanism, not a
  secondary review pass — do not build an LLM-judges-the-LLM confidence
  checker; it inherits the same unreliability it's meant to catch.

  Why this is the actual mechanism, not just a safety wrapper:
  hand-waving and long-context memory drift are the same failure in two
  guises. Hand-waving is what fills a claim that has no pointee to
  check against ("roughly," "seems like" is the sound of nothing
  concrete behind the sentence) — making `support`/`action` mandatory
  removes the option to produce it, rather than flagging it after the
  fact. Long-thread memory confusion is subtler: an early inference
  restated enough times starts getting treated as settled fact by turn
  40, because nobody re-checks something that *feels* already agreed.
  Requiring every claim to carry its own fresh pointer back to source
  data — not to "what was said earlier in the thread" — makes each
  claim independently checkable regardless of conversation length,
  instead of accumulating unverified weight the way freeform chat does.

  Caveat worth tracking, not solving now: a `support` pointer can be
  *present* and still wrong — pointing at *a* dataRef without that
  dataRef actually backing the specific claim next to it satisfies the
  schema while still producing hand-waving, just structurally-valid
  hand-waving instead of the obvious kind. Schema enforcement
  guarantees a pointee exists; it doesn't guarantee relevance. Put
  pointee-relevance checking on the roadmap (schema layer or a review
  step) once presence-enforcement is live and stable — don't conflate
  the two or treat presence-checking as the finished job.
- **UI implication**: this likely means moving Kiwi's presentation away
  from a scrolling chat transcript toward a referenced-claims feed —
  each claim rendered with its pointee as a clickable chip (jump to
  plot / capture / tile / viewer position). Freeform text is connective
  tissue between chips, not the payload. Flag as a UI architecture
  decision, not just a schema change — confirm with the person building
  this before assuming chat-transcript UI carries over unchanged.
- **API key**: one shared key, held server-side in the Kiwi service, not
  distributed to clients. No per-user Anthropic accounts.
- **Attribution**: log `{cecelia_user, timestamp, tokens_in, tokens_out,
  tool_calls}` per Kiwi call, keyed to Cecelia's existing user/session
  identity — not a new auth system. Purpose is cost visibility, not
  enforcement.
- **Client mode toggle, required, not optional.** Capture/context
  delivery is not the same mechanism in both consumer modes: a Claude
  Code-paired session delivers captures via the existing peer/broadcast
  path (pairing tokens, project ID, permission-laundering guard —
  assumes a paired session is listening); Kiwi-direct mode has no peer
  to broadcast to — captures go straight from Kiwi's UI to its own
  backend, internally. These are mutually exclusive per session, not
  cosmetic settings: the peer-broadcast path is meaningless with no
  paired Code session live, and running both simultaneously risks a
  capture being delivered down a path nothing is listening on. Kiwi
  needs an explicit mode toggle (Claude Code-paired vs. Kiwi-direct)
  that determines which delivery path is active, not just which
  consumer answers.

## Explicitly out of scope for this pass

- Per-user Anthropic login/auth.
- Quotas or rate-limiting per user — revisit only if logging surfaces an
  actual cost or abuse problem.
- Billing/chargeback across users or projects.
- A confidence-checking review pass on Kiwi's own output — structural
  schema enforcement replaces this, not a second model call.

## Open questions for the audit

- **Does Claude Code access stay available at all?** If the concern is
  that a freeform escape hatch lets people route around duck discipline
  and get judgment calls anyway, that's a real tension worth deciding
  explicitly rather than leaving as a side door. Not an engineering
  question — flag it back to the team, don't resolve it unilaterally.
- **Where does the pointee-schema layer physically live** — inside the
  Kiwi service as a wrapper around the API call, or as a distinct
  gateway both the service and (potentially) other future structured
  consumers route through? Pick whichever avoids duplicating the schema
  logic if a second structured consumer ever shows up.
- **Mode-toggle UI and state**: where does the Claude Code-paired vs.
  Kiwi-direct choice live (a setting, a per-session prompt, a visible
  indicator so the user always knows which mode/delivery path is
  active), and what happens if a user switches mid-session — does the
  capture history carry over, or does switching modes start a fresh
  thread given the two paths aren't interchangeable?
- **Outcome-tag/guardrail-retrieval bias-ordering** (bad-outcomes-first)
  — confirm this still counts as `observation`-type (retrieving the
  user's own past judgment) rather than `hypothesis`, since the ordering
  itself is a small editorial choice, not neutral reflection. Named as
  gray area in `kiwi-duck-not-oracle.md`; worth pinning down formally
  once the schema is enforced everywhere.

## Prior art — this is not a novel pattern

The pointee-required approach maps onto an established line of work;
worth building on it rather than treating this as invented from
scratch.

- **Grounded / citation-grounded generation** — the general pattern:
  constrain output so every claim is traceable to a supplied source,
  enforced structurally rather than checked after the fact. Same logic
  as requiring `support`/`action` per claim here.
- **WebGPT / "verified quotes" (Menick et al. 2022)** — direct
  ancestor: a QA system required to back every answer with a literal
  quoted excerpt from a retrieved source, specifically to prevent
  confident unsupported claims. Same idea, applied to documents instead
  of captures/dataRefs.
- **Structured-output / constrained decoding (JSON schema enforcement)**
  — the actual mechanism to build on, not a research idea: standard
  across major LLM API providers now, with self-healing retry when the
  model returns invalid JSON. This is what enforces the
  `observation`/`hypothesis` schema at the API layer.
- **NLI-based citation verification** — the existing answer to the
  presence-vs-relevance caveat above: treat the cited source as premise
  and the claim as hypothesis, run entailment checking rather than
  trusting that a pointer being present means it's relevant.
  Atomic-claim factuality scoring (splitting a response into individual
  claims, checking each independently) is the more rigorous version of
  the same idea. Not needed for v1, but don't reinvent it later —
  reference this line of work when it's time.
- **VeriFact** (clinical-NLP, LLM-generated summaries verified against
  EHR data) — closest domain analog: structured-output generation
  constrained to a schema, checked against a real data source, in a
  domain with a similar stakes profile (wrong confident claim in a
  patient chart ≈ wrong confident claim about tissue data). Worth
  reading directly, not just citing.

## Ask

0. Before designing from scratch: identify concrete open-source
   implementations or reference architectures for citation-grounded/
   structured-claim generation (e.g. existing libraries for JSON-schema-
   enforced structured output with citation fields, any open
   implementation in the WebGPT-verified-quotes or grounded-generation
   lineage, VeriFact's actual approach if published/open) that the
   `observation`/`hypothesis` schema and enforcement layer could build
   on or adapt, rather than inventing the mechanism from zero. Cite
   specific repos/papers with implementation detail, not just the
   pattern name.
1. Confirm the tool server needs no forking or duplication — same
   codebase serves both consumers.
2. Scope the Kiwi service: API call layer, schema enforcement, minimal
   logging, as defined above.
3. Propose the claims-feed UI shape (or confirm chat-transcript can
   still work with chips inline) — flag if this changes capture/thread
   display more broadly than just new replies.
4. Surface the Claude Code access-scope question as a decision for the
   team, not a default to assume.
