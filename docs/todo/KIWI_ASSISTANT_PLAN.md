# Kiwi assistant (structured duck) — plan

**Status:** parked (2026-09-23) · branch `docs/kiwi-assistant-plan`. Nothing built. Decisions 1–3
are the user's; Decisions 4–8 were proposed by Claude in the same conversation and accepted without
objection; Decisions 9–10 came out of the literature search the same day (*Prior art* below) and are
proposals. Read the *Open decisions* before building: several of them change a phase's shape.
**Supersedes** `KIWI_PLAN.md` Decisions 1 and 5 and bends Decision 12 (see *What this changes in
KIWI_PLAN*). Everything else in `KIWI_PLAN.md` (cockpit shape, pairing chip, captures list) stands.

## Goal

Kiwi stops being only a display panel for a paired assistant and becomes a **structured duck**: an
assistant surface inside the app where

- **the user links app context into a prompt** — a plot, a task function, a viewer position, a cell or
  track, a capture, a Blackboard entry — as typed chips, not pasted ids, and
- **Kiwi points back at anything in the app**, with an AI engine as the reasoning layer. Every claim in
  a reply carries a pointer the user can click to check it.

The reply rule is the duck line from `docs/archive/kiwi-duck-not-oracle.md`: Kiwi **points**
(notices, quantifies, redirects attention) and does not **judge** (explain cause, recommend
include/exclude, reassure). Describing is silent-safe; interpreting is flagged.

## Why (user, 2026-09-23)

The original framing — "Kiwi is the panel, not the assistant; the terminal is for driving" — left the
two directions (share-in and point-out, both already shipped via BIDIR) without an in-app place where
they meet. A freeform chat can't carry the duck line: nothing in prose forces a claim to be checkable.
A structured reply can.

## Decisions

1. **Kiwi is the assistant surface** (user, 2026-09-23). A prompt input and a reply feed live in the
   Kiwi cockpit. Reverses `KIWI_PLAN.md` Decision 5's "OUT permanently: chat prompt bar".
2. **v1 engine = the `claude` CLI under the user's own login** (user, 2026-09-23). Garvan's Enterprise
   licence covers claude.ai + Claude Code seats; there is no Console/API org access for the domain
   (checked 2026-09-23 — "You are not a member of any organizations under your domain"). Consequences:
   no shared API key, no key on anyone's machine, no per-user attribution system — each user's own
   login is the account. A user without Claude access gets no Kiwi replies (the panel's other rows
   still work). Measured 2026-09-23: `claude -p … --output-format json --json-schema <schema>` returns
   a schema-valid `structured_output` under a seat login (Haiku, 3 turns). `--bare` is **not usable** —
   it reads only `ANTHROPIC_API_KEY`, never the OAuth login.
3. **The engine sits behind a generic adapter** (user, 2026-09-23: "in theory plug in any other AI
   service"). Claude is the only implementation and no second engine exists to test against, so the
   contract is designed for transfer but **not proven to transfer** — see *The engine seam* for what
   is and isn't expected to survive a swap.
4. **One reference type, both directions.** The chip the user drops into a prompt, the evidence
   pointer on a claim, and the target Kiwi points at are the **same `KiwiRef` object**. One definition,
   one validator, one resolver. Three shapes for these would be the divergent re-implementation
   `CLAUDE.md` warns about.
5. **Claim kinds encode the duck line.** A reply is a list of claims, each with ≥1 `KiwiRef`:
   - `observation` — what is shown ("panel A +12%, panel B −4% for the same treatment");
   - `interpretation` — what Kiwi thinks it means, **rendered with a visible flag** (the duck note's
     "interpreting needs a flag");
   - `question` — a checkable next look ("does AF correction change t=40?"), **not** an instruction.
   There is no `recommendation` kind. This replaces the audit prompt's `hypothesis` + mandatory
   `action` ("try AF correction on this frame"), which would have *forced* a recommendation into every
   hypothesis — the opposite of the duck line. One fact per claim: every claim checker in the
   literature (FActScore, SAFE, VeriFact) first splits text into atomic facts; a two-fact claim can be
   half-supported and pass.
6. **Reference validation is deterministic, in Cecelia, not a second model.** After every reply,
   Cecelia resolves each `KiwiRef`; any ref that doesn't resolve (unknown plot id, track id not in the
   image, capture that doesn't exist) rejects the reply and triggers one re-ask naming the bad refs. No
   LLM-judges-the-LLM pass. The 2026-09-23 test showed why this is needed: the model filled a required
   `ref` string with invented labels (`"atmospheric-optics"`) — schema enforcement guarantees a pointer
   *exists*, not that it's *real*. Mechanics on the CLI route: the CLI already validates against the
   JSON Schema and re-prompts itself; treat both `error_max_structured_output_retries` and a `success`
   with no `structured_output` as failure. The ref re-ask is one `--resume <session>` carrying the
   resolver's errors (~30 lines; copy the semantics of Pydantic AI's `output_validator` + `ModelRetry`,
   don't take the dependency). Cheaper still: expose a `check_refs` observer tool the model calls
   before answering, so most bad refs are fixed inside the one run.
7. **A ref must come from this turn, not from memory.** Every `KiwiRef` in a reply must be one the user
   attached or one that appeared in a tool result during this turn; anything else fails Decision 6.
   The reply schema constrains `KiwiRef.kind` to the enum. Why this is stricter than "the engine is
   given the refs in play": in RAG systems, up to 57% of citations lacked faithfulness — the model
   answered from memory and attached a source that fits ("post-rationalisation", Wallat et al.
   2025). A ref that resolves and even supports the claim can still not be what the claim was based on.
8. **Tools = the existing `cecelia-observer` MCP server, unforked.** Kiwi's engine turn gets its own
   system prompt (`--system-prompt`, replacing, not `--append-system-prompt`) and a restricted tool
   allow-list. The freeform-chat guidance in `mcp/cecelia_mcp/guidance.py` stays for Claude Code
   sessions; Kiwi does not inherit it. Push auto-pairing is already a no-op outside a Claude Code
   session (`_maybe_pair` gates on `CLAUDE_CODE_MESSAGING_*`, `mcp/cecelia_mcp/client.py`), so a
   Kiwi-spawned engine cannot pair by accident.
9. **Abstention is a reply, not a failure** (proposed, from GopherCite). A reply may be a single
   "can't say from what's shown" with the refs it looked at. Without it the schema forces the engine to
   produce *some* claim, and the easiest claim to produce is reassurance — the duck note's worst case.
10. **The UI shows how far a chip was checked** (proposed). Citations raise user trust even when they
    are random; 42% of participants checked any citation, and only ~10% of cited answers were checked
    (Ding et al. 2025); explanations raised acceptance of the AI's answer regardless of whether it was
    correct (Bansal et al. 2021). So a chip must not look the same when it has only been
    shown to *exist* (Decision 6) as when it has been checked to *support* its claim (Open decision 5).
    `interpretation` flags use first-person wording ("I think…") — it reduced over-reliance; impersonal
    hedging ("It's not clear…") had weaker, non-significant effects (Kim et al. 2024).

## The reference type (`KiwiRef`)

Every kind below already has an address and, for most, a point-out route. The plan unifies them; it
does not invent new addressing.

| kind | address fields | exists today as | point-out route |
|---|---|---|---|
| `plot` | `family`, `plotId`, optional `(u,v)` | `CaptureAddress.plotSpec`, `list_plots` | `mark_plot` |
| `viewer` | `imageUid`, `valueName?`, `t`, `z?` | `CaptureAddress` | (navigate; no mark route) |
| `cells` | `imageUid`, `valueName`, `labelIds[]` | — | `mark_cells` |
| `tracks` | `imageUid`, `valueName`, `trackIds[]` | — | `mark_tracks` |
| `tile` | `imageUid`, `cellId` | landscape grid | `mark_tile` |
| `capture` | `captureId` | `<proj>/captures/<id>` | `mark_freeform` |
| `task` | `funName`, optional run / `valueName` | task registry | `point_at_ui` (nav path) |
| `ui` | `anchor` (data-guide id or nav path) | `CaptureAddress.domAnchor` | `point_at_ui` |
| `blackboard` | `entryId`, `version?` | Blackboard | (open entry) |

- **One canonical definition** as a JSON Schema file both the TS and the Python/Julia sides read —
  the shared-asset pattern `VIEWER_PARITY_PLAN.md` uses for the palette. `CaptureAddress`
  (`frontend/src/utils/captureAddress.ts`) becomes a producer of `viewer`/`plot`/`ui` refs, not a
  parallel type.
- **Resolver** (Julia, one function + one route): `KiwiRef → exists? + display label`. Used by
  Decision 6's validation, by chip rendering, and by the click-to-jump handler.
- **Pointing = rendering a ref.** Clicking a claim's chip calls the existing mark route for its kind.
  No new point-out machinery.

## The engine seam

Today `app/src/ai/agent_runner.jl` has `abstract type AgentBackend` but every function dispatches on
the concrete `ClaudeAgent` — the abstraction is a name, not a contract. Phase 1 makes it one.

**The contract** (what any engine adapter implements):

- `engine_available(a)` → ready / reason not ready (drives the cockpit's setup CTA);
- `engine_label(a)` → display name for the UI ("Claude"), so components never hard-code it and the
  `KIWI_PLAN.md` Decision 12 naming ratchet keeps meaning something;
- `run_turn(a, prompt, context_refs, schema, tools, session)` → `{reply_json, usage, session_id, error}`;
- capability flags: `native_schema`, `native_mcp`, `resumable`.

**What is engine-independent (expected to transfer):** the `KiwiRef` type, the reply schema, the
resolver, Decision 6's validate-and-re-ask loop, the UI. The guarantee that every claim carries a
*real* pointer lives in Cecelia, not in the engine — so an engine without native schema enforcement
still gets the same guarantee, at the cost of more re-asks.

**What is expected NOT to transfer cleanly** (named so nobody assumes the seam is proven):

- the system prompt's wording and how reliably an engine follows the duck line;
- tool-calling quality over MCP and the number of turns an engine needs;
- CLI-specific plumbing — `--resume` session ids, stale-session self-heal, the JSON result envelope
  `_parse_claude_result` reads. A second CLI engine needs its own parser; an HTTP engine needs a
  different process model entirely.

**How the contract looks against real engines** (checked 2026-09-23, not tested):

| engine | schema | MCP | resume | verdict |
|---|---|---|---|---|
| Claude Code CLI | `--json-schema` (validates + re-prompts; draft-07, `format` not enforced) | yes | `--resume` | v1 |
| OpenAI Codex CLI | `codex exec --output-schema` | yes | `exec resume` | transfers — but schema-with-MCP was broken until 2026-04 (issues #15451, #14343/#22998); re-test |
| Antigravity CLI (`agy`) | `--json-schema` → `structured_output`, same shape as Claude Code | headless MCP unverified | ? | probably transfers; no licence listed; empty-output bug #1065 |
| Gemini CLI | none (#13388 closed not-planned) | yes | yes | **don't target** — no schema flag, and consumer/Pro accounts dropped 2026-06-18 |
| Anthropic API (direct) | structured outputs | via client | client-side | the future adapter — but **Citations and structured outputs are mutually exclusive** (a 400), so it keeps our `KiwiRef` schema rather than native citations; the CLI has no flag to attach citation documents |

Reference implementations worth reading for the adapter, not depending on: the Agent Client
Protocol (common interface over Claude Code / Codex / Gemini CLI; no schema output, so it can't carry
`native_schema`) and `ben-vargas/ai-sdk-provider-claude-code` / `-codex-cli` (TS providers that wrap
the CLIs, not HTTP APIs).

## UI shape (proposal — confirm before Phase 4)

- **Prompt input with chips** in the Kiwi cockpit. Chips arrive two ways: an "Add to Kiwi" affordance
  on the things that have a `KiwiRef` (plot panels, viewer, task pages, capture rows, Blackboard
  entries), and typed search inside the input.
- **Replies as a claims feed**, not a chat transcript: each claim renders its kind (with the
  `interpretation` flag), its text, and its ref chips. Free text between claims is connective tissue,
  not the payload.
- Existing cockpit rows (pairing chip, recent captures, session identity) stay; the prompt + feed are a
  new section.

## Open decisions

1. **Does the Claude Code terminal route stay?** The paired terminal session is freeform and bypasses
   the duck line. Keeping it as an escape hatch vs. retiring it is a product call, not an engineering
   one — don't let "it's already built" settle it.
2. **Two delivery paths for captures.** A paired terminal session gets captures via push; Kiwi's own
   engine gets them as refs in its turn context. If both are live, does a capture go to both, or does
   the user pick a mode? Needs a visible indicator either way.
3. **Where threads and replies persist.** Ephemeral per session, a `<proj>/kiwi/` store, or folded into
   the lab log / Blackboard. Affects whether a past Kiwi claim can itself be a `KiwiRef`.
4. **Turn latency on the CLI route.** Each turn spawns a process. Not measured — Phase 0 measures it
   before the UI is designed around it.
5. **Support, not just presence.** Decision 6 proves a ref is real, not that it backs the claim next
   to it — and the gap is large: in audited generative search engines only 74.5% of citations
   supported their sentence (Liu et al. 2023). Checking this does **not** need an LLM judge: small
   fine-tuned checkers reach GPT-4-level accuracy on CPU — **HHEM-2.1-Open** (Apache-2.0; vendor-reported <600 MB,
   ~1.5 s per 2k tokens on CPU) or **MiniCheck-Flan-T5-Large** (MIT, 770M; skip Bespoke-7B — GPU-sized, licence unclear).
   Three-way label, not two: *supported* / *contradicted* / *not addressed* (VeriFact), and
   AttrScore's *extrapolatory* (related, but the claim goes beyond it) is exactly an `observation`
   that should have been an `interpretation`. **The real cost is not the checker but the text
   rendering:** these models read (premise, claim) text, so each `KiwiRef` kind needs a text
   serialisation of its content (plot summary stats, a cell's measurement row, a track's features).
   Expect this to be harder for plots and images than for text — multimodal models attribute poorly
   to figures and tables (MCiteBench, 2025).
8. **Schema during generation vs after.** Strict format constraints measurably degrade reasoning
   (Tam et al. 2024). Options: a free-text `reasoning` field ordered before `claims` in the schema, or
   a two-step turn (answer freely, then fill the schema). Measure in Phase 0 before choosing.
9. **Check after, or attribute first?** Decisions 6–7 check refs after generation. The alternative is
   *attribute-first*: pick the refs, then write each claim against its ref (Slobodkin et al. 2024) —
   faithful by construction, and it cut human verification time. Heavier turn; revisit if Phase 3's
   re-ask rate is high.
6. **Blackboard bad-outcomes-first ordering** (`_outcome_rank`, `mcp/cecelia_mcp/server.py`) — does a
   resurfaced outcome count as an `observation` (the user's own past judgment) or does the ordering
   make it an `interpretation`? Gray in the duck note; pin it when the schema lands.
7. **Direct API engine** — a second adapter once/if Console access exists. Would be the first real test
   of Decision 3.

## Phases

Each independently shippable.

0. **Measure.** Time a CLI turn with `--json-schema` + `--mcp-config` + `--system-prompt` on a real
   project (cold and `--resume`). Decides whether Phase 4's UI streams, spins, or needs a warm process.
   Same run: reasoning-field-first vs bare schema (Open decision 8).
1. **Engine contract.** Lift `AgentBackend` into the contract above; `ClaudeAgent` implements it; the
   observer's existing Watch/lab-log turns move onto it unchanged. Tests: pure command builder + result
   parser (already the pattern in `app/test/suite/observer.jl`).
2. **`KiwiRef` + resolver.** Shared JSON Schema, TS type, Julia resolver + route. Tests: every kind
   round-trips; unknown ids fail resolution.
3. **Headless Kiwi turn.** Reply schema, Kiwi system prompt, validate-and-re-ask loop, run from the
   REPL against a real project. Tests: a fabricated ref is rejected; a real ref not seen this turn is
   rejected (Decision 7); re-ask fires once. Track **citation recall** (every claim has a ref) and
   **citation precision** (every ref supports its claim — by hand until Open decision 5 lands) on a
   small fixed set of prompts, per Liu et al. 2023; that set is the regression check for prompt
   changes and for a second engine.
4. **Cockpit UI.** Prompt input with chips, claims feed, click-to-point via existing mark routes.
5. **"Add to Kiwi" affordances** across plots, viewer, task pages, captures, Blackboard.

## What this changes in KIWI_PLAN

- **Decision 1** ("Kiwi is the panel, not the assistant") — superseded by Decision 1 here.
- **Decision 5** (prompt bar "OUT permanently") — superseded; the prompt input is Phase 4.
- **Decision 12** (naming ratchet) — kept, and the engine seam is what makes it hold: UI copy reads
  `engine_label`, so the literal "Claude" still never appears in `components/kiwi/`.

## Prior art (searched 2026-09-23)

Every paper verified 2026-09-23 against the arXiv API (title, authors, year) and every DOI against
Crossref; each number quoted in this plan was checked in the paper's abstract or full text. Repo
status, issue numbers and states verified with `gh api`. The source brief's "WebGPT / verified
quotes (Menick et al. 2022)" merged two papers — corrected below. Not found: any paper showing that
structured claim+evidence output reduces hallucination vs free text (plausible, unsourced), and any
imaging assistant that grounds claims in viewer or plot objects.

**Grounded generation**
- WebGPT — Nakano et al. 2021, arXiv:2112.09332. Browsing + collected references during answering.
- GopherCite — Menick et al. 2022, arXiv:2203.11147. Verbatim supporting quotes, and *abstains* when
  unsure → Decision 9.
- Attribute First, then Generate — Slobodkin et al., ACL 2024, arXiv:2403.17104 → Open decision 9.
- Correctness is not Faithfulness in RAG Attributions — Wallat et al., ICTIR 2025,
  arXiv:2412.18004, DOI 10.1145/3731120.3744592. Post-rationalised citations → Decision 7.

**Attribution and claim checking**
- AIS ("Measuring Attribution in Natural Language Generation Models") — Rashkin et al.,
  *Computational Linguistics* 49(4), 2023, DOI 10.1162/coli_a_00486, arXiv:2112.12870. Two-stage rating:
  interpretable, then supported.
- Evaluating Verifiability in Generative Search Engines — Liu, Zhang, Liang, Findings EMNLP 2023,
  arXiv:2304.09848. Citation recall / precision → Phase 3 metrics, Open decision 5.
- ALCE — Gao et al., EMNLP 2023, arXiv:2305.14627. Automated citation scoring with an NLI model.
- AttrScore — Yue et al., Findings EMNLP 2023, arXiv:2305.06311. *Extrapolatory* label.
- FActScore — Min et al., EMNLP 2023, arXiv:2305.14251; SAFE — Wei et al., NeurIPS 2024,
  arXiv:2403.18802. Atomic facts first → one fact per claim (Decision 5).
- MiniCheck — Tang, Laban, Durrett, EMNLP 2024, arXiv:2404.10774. Small checkers at GPT-4 accuracy.
- VeriFact — Chung et al., arXiv:2501.16672; *NEJM AI* 2026, DOI 10.1056/AIdbp2500418;
  github.com/philipchung/verifact. Closest domain analogue: clinical text checked claim by claim
  against the patient record. Its judge **is** an LLM, limited to one claim plus retrieved evidence,
  and agreed with adjudicated clinician labels 92.7–93.2% (best inter-clinician 88.5%). Evidence that
  a scoped LLM judge can work; this plan still prefers a small checker first (Open decision 5).
- MCiteBench — Hu et al., arXiv:2503.02589, Findings EMNLP 2025. Citation over figures
  and tables; multimodal models do badly.
- Let Me Speak Freely? — Tam et al., EMNLP 2024 Industry, arXiv:2408.02442 → Open decision 8.

**Trust and verification (HCI)**
- Citations and Trust in LLM Generated Responses — Ding et al., AAAI 2025, arXiv:2501.01303 →
  Decision 10.
- Does the Whole Exceed its Parts? — Bansal et al., CHI 2021, DOI 10.1145/3411764.3445717,
  arXiv:2006.14779.
- "I'm Not Sure, But…" — Kim et al., FAccT 2024, DOI 10.1145/3630106.3658941, arXiv:2405.00623 →
  first-person flags.
- How Do Analysts Understand and Verify AI-Assisted Data Analyses? — Gu et al., CHI 2024,
  arXiv:2309.10947. Analysts verify the *procedure* as well as the data → the `task` ref kind matters.

**Code** (maintenance checked via `gh api`, 2026-09-23)
- USE: Claude Code `--json-schema` (v1 engine); HHEM-2.1-Open or MiniCheck-Flan-T5-Large (Open
  decision 5).
- COPY SHAPE: Pydantic AI `output_validator`/`ModelRetry` (re-ask semantics); LangChain / LlamaIndex
  "cited answer" pattern (number the sources, cite ids — our `KiwiRef` list); Agent Client Protocol
  and `ben-vargas/ai-sdk-provider-*` (adapter naming, CLI flag mapping).
- SKIP: Instructor (patches HTTP SDK clients, no CLI path), Outlines (needs logit access), BAML (own
  DSL + codegen, HTTP-only), PromptingTools.jl (HTTP-only), Guardrails / Marvin (heavier than the
  ~30 lines needed), LettuceDetect (span-level; only if we later need *which part* is unsupported).
- Dead, skipped: princeton-nlp/ALCE (2024-10), RARR (2023-06), FActScore (2025-04), AlignScore
  (2024-03) — use the papers, not the repos.

## References

- Source briefs (not authoritative): `docs/archive/kiwi-duck-not-oracle.md`,
  `docs/archive/opus-audit-kiwi-service-shared-mcp.md`, `docs/archive/kiwi-purpose-and-framing.md`
  (the "structured rubber duck" reframe).
- [`KIWI_PLAN.md`](KIWI_PLAN.md) — the cockpit this extends.
- [`BIDIR_CONTEXT_PLAN.md`](BIDIR_CONTEXT_PLAN.md) — share-in captures and point-out marks.
- [`PROJECT_MEMORY_PLAN.md`](PROJECT_MEMORY_PLAN.md) — Blackboard, outcome tags, Decision 12 ordering.
- Code: `app/src/ai/agent_runner.jl` (engine seam), `mcp/cecelia_mcp/server.py` (`mark_*`,
  `point_at_ui`, `get_capture`, `list_plots`), `mcp/cecelia_mcp/guidance.py`,
  `frontend/src/utils/captureAddress.ts`, `frontend/src/components/kiwi/KiwiCockpit.vue`.
