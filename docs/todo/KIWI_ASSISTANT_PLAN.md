# Kiwi assistant (structured duck) — plan

**Status:** in progress — Phases 0–1 shipped (#1196); Phase 2 shipped (#1198); Phase 3 built (#1199), exit gate settled by the user (bare default, Think-first switch — Open decision 8); Phases 4–5 built on `feat/kiwi-cockpit`, not yet tried in a browser. Decisions 1–3
are the user's; Decisions 4–8 were proposed by Claude in the same conversation and accepted without
objection; Decisions 9–10 came out of the literature search the same day (*Prior art* below) and
Decision 11 out of Phase 0; all three are proposals. Read the *Open decisions* before building: several of them change a phase's shape.
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
   *Credential isolation on a shared OS login (needed for "each user's own login" to hold at more
   than one seat) is designed in [`LOGIN_CREDENTIAL_ISOLATION_PLAN.md`](LOGIN_CREDENTIAL_ISOLATION_PLAN.md);
   its P1 pre-rollout script must pass before enabling multi-profile.*
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
11. **Pack the context; don't make the engine browse** (proposed, from Phase 0). Cecelia resolves the
    attached chips into a compact text context pack and sends it with the prompt; the engine calls
    tools only for what the pack lacks. Phase 0 showed the turn is 13–29 tool round-trips to rebuild
    context Cecelia already has, and a 3 s spawn floor — so a warm process would save ~nothing, while
    fewer round-trips save most of the minute *and* most of the tokens. The pack is the same text
    serialisation Open decision 5's support checker needs, so it is built once and used twice.

## The reference type (`KiwiRef`)

Every kind below already has an address and, for most, a point-out route. The plan unifies them; it
does not invent new addressing.

| kind | address fields | exists today as | point-out route |
|---|---|---|---|
| `plot` | `plotId`, optional `(u,v)` | the live plot registry (`list_plots`) | `mark_plot` |
| `viewer` | `imageUid`, `t?`, `z?` | `CaptureAddress` | (navigate; no mark route) |
| `cells` | `imageUid`, `valueName`, `labelIds[]` | — | `mark_cells` |
| `tracks` | `imageUid`, `valueName`, `trackIds[]` | — | `mark_tracks` |
| `tile` | `imageUid`, `valueName`, `cellId`, `t?`, `z?` | landscape grid | `mark_tile` |
| `capture` | `captureId` | `<proj>/captures/<id>` | `mark_freeform` |
| `task` | `funName`, optional run / `valueName` | task registry | `point_at_ui` (nav path) |
| `ui` | `anchor` (data-guide id or nav path) | `CaptureAddress.domAnchor` | `point_at_ui` |
| `blackboard` | `entryId`, `version?` | Blackboard | (open entry) |
| `project` | `projectUid` | `get_project_info` | (navigate) |
| `set` | `setUid` | `list_images` | (navigate) |
| `image` | `imageUid` | `list_images`, `get_image_info` | (open image) |
| `population` | `imageUid`, `valueName`, `popPath` | `get_populations` | (open gating) |

**Typed fields per kind, never a flat `id` string** (Phase 0, 2026-09-23). The smoke test used a flat
`{kind, id}` ref and the model cited `tracks:VJy1Nx` — an *image* uid under the `tracks` kind, on
images with no tracking — and `task:get_project_info`, a *tool name* offered as evidence. Both passed
the CLI's schema check. With per-kind fields (`tracks` requires `imageUid` + `valueName` +
`trackIds[]`), the first is a missing-field schema failure and the second can't be expressed. The
`project` / `set` / `image` / `population` kinds were missing from the first draft; an orientation
answer points mostly at those.

- **One canonical definition** — built (Phase 2): `frontend/src/lib/kiwiRef.schema.json`, read by
  `frontend/src/utils/kiwiRef.ts` (union + parity test) and `api/src/kiwi_refs.jl` (shape check +
  resolver) — the same shared-asset pattern as `frontend/src/plots/palettes.json`.
  `refsFromCaptureAddress` turns a capture into `viewer` / `ui` refs. A capture's `plotSpec.specId`
  names the plot *type*, not the live panel, so it is **not** a `plot` ref — that comes from the plot
  registry's `plotId`. Two schema choices differ from the first draft: `plot` has no `family` (the
  registry holds it) and `viewer` has no `valueName` (there it means the image *file* version, not a
  segmentation — too easy to confuse).
- **Resolver** — built (Phase 2): `POST /api/kiwi/refs/resolve` → `{ok, check, label, error}` per ref.
  `check` is how far the answer goes — `exists` (on disk), `live` (an open plot panel or landscape:
  true now, gone when it closes or the server restarts), `format` (UI anchors: only the browser knows
  which exist), `shape` (malformed). Those are Decision 10's chip levels. Read-only by construction.
- **Pointing = rendering a ref.** Clicking a claim's chip calls the existing mark route for its kind.
  No new point-out machinery.

## The engine seam

Today `app/src/ai/agent_runner.jl` has `abstract type AgentBackend` but every function dispatches on
the concrete `ClaudeAgent` — the abstraction is a name, not a contract. Phase 1 makes it one.

**The contract** (what any engine adapter implements — built in Phase 1, names as in the code):

- `agent_available(a)` → usable here? (drives the cockpit's setup CTA; the default assumes a CLI on
  `PATH`, a non-CLI engine overrides it);
- `agent_label(a)` → display name for the UI ("Claude"), so components never hard-code it and the
  `KIWI_PLAN.md` Decision 12 naming ratchet keeps meaning something;
- `agent_capabilities(a)` → `(; native_schema, native_mcp, resumable)`;
- `_run_agent_once(a, prompt, mcp_config_path; system_prompt, session_id, json_schema, …)` → one spawn,
  no retries, returns `AgentResult` (`structured` holds the schema-validated reply).

On top sits `run_agent_turn`, engine-independent: availability gate, stale-session self-heal (only if
`resumable`), and "asked for a schema, got none" = failure. Context refs (Decision 11's pack) go into
the prompt, not the contract.

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

## UI shape (built in Phase 4 — user, 2026-09-23: "implement the whole thing by plan")

- **Prompt input with chips** in the Kiwi cockpit. Chips arrive two ways: an "Add to Kiwi" affordance
  on the things that have a `KiwiRef` (plot panels, viewer, task pages, capture rows, Blackboard
  entries), and typed search inside the input.
- **Replies as a claims feed**, not a chat transcript: each claim renders its kind (with the
  `interpretation` flag), its text, and its ref chips. Free text between claims is connective tissue,
  not the payload.
- Existing cockpit rows (pairing chip, recent captures, session identity) stay; the prompt + feed are a
  new section — the FIRST one, since it is now the reason to open the panel.

## Open decisions

1. **Does the Claude Code terminal route stay?** The paired terminal session is freeform and bypasses
   the duck line. Keeping it as an escape hatch vs. retiring it is a product call, not an engineering
   one — don't let "it's already built" settle it.
2. **Two delivery paths for captures.** A paired terminal session gets captures via push; Kiwi's own
   engine gets them as refs in its turn context. If both are live, does a capture go to both, or does
   the user pick a mode? Needs a visible indicator either way.
3. **Where threads and replies persist — decided (Phase 4, Claude, under the user's "go through
   autonomously").** `<project>/kiwi/turns.json`, the last 50 turns: a turn costs minutes and seat
   quota, so a reload must not lose it, and the eval harness can read the same records. No threads —
   each turn is fresh (Decision 7 already forbids citing from an earlier turn). A past claim is NOT a
   `KiwiRef` kind; it is a record, not an app object. Revisit if replies should reach the lab log or
   the Blackboard — that is a user call.
4. **Turn latency on the CLI route — measured (Phase 0), resolved.** ~1 min per cold turn, dominated
   by tool round-trips, not process spawn. Phase 4's UI must show progress while the engine works
   (tool calls as they happen), not a bare spinner. See Decision 11 for the speed lever.
5. **Support, not just presence.** Decision 6 proves a ref is real, not that it backs the claim next
   to it — and the gap is large: in audited generative search engines only 74.5% of citations
   supported their sentence (Liu et al. 2023). Checking this does **not** need an LLM judge: small
   fine-tuned checkers reach GPT-4-level accuracy on CPU — **HHEM-2.1-Open** (Apache-2.0;
   vendor-reported <600 MB, ~1.5 s per 2k tokens on CPU) or **MiniCheck-Flan-T5-Large** (MIT, 770M;
   skip Bespoke-7B — GPU-sized, licence unclear).
   Three-way label, not two: *supported* / *contradicted* / *not addressed* (VeriFact), and
   AttrScore's *extrapolatory* (related, but the claim goes beyond it) is exactly an `observation`
   that should have been an `interpretation`. **The real cost is not the checker but the text
   rendering:** these models read (premise, claim) text, so each `KiwiRef` kind needs a text
   serialisation of its content (plot summary stats, a cell's measurement row, a track's features).
   Expect this to be harder for plots and images than for text — multimodal models attribute poorly
   to figures and tables (MCiteBench, 2025).
6. **Blackboard bad-outcomes-first ordering** (`_outcome_rank`, `mcp/cecelia_mcp/server.py`) — does a
   resurfaced outcome count as an `observation` (the user's own past judgment) or does the ordering
   make it an `interpretation`? Gray in the duck note; pin it when the schema lands.
7. **Direct API engine** — a second adapter once/if Console access exists. Would be the first real test
   of Decision 3.
8. **Schema during generation vs after.** Strict format constraints measurably degrade reasoning
   (Tam et al. 2024). Options: a free-text `reasoning` field ordered before `claims` in the schema, or
   a two-step turn (answer freely, then fill the schema). Phase 0 ran one rep of each: inconclusive
   (differences within run-to-run noise). **Must** be settled on Phase 3's fixed prompt set — it is
   Phase 3's exit gate, not a nice-to-have.
   **Settled (user, 2026-09-23): bare by default, reasoning behind a "Think first" switch**
   (`settings.kiwiReasoning`, sent as `reasoning` on each turn). On the 18-turn comparison (Phase 3
   below) reasoning cost more (9.7k vs 6.8k output tokens, 91 vs 69 s) and gained nothing the automatic
   checks could see. The planned hand score was dropped: claims restating tool output were all
   trivially "backed", so claim-level scoring could not tell the variants apart — and it showed the
   real gap is usefulness (most claims were readouts; the valuable ones were checkable questions
   about something odd), which a claim-support score does not measure.
9. **Check after, or attribute first?** Decisions 6–7 check refs after generation. The alternative is
   *attribute-first*: pick the refs, then write each claim against its ref (Slobodkin et al. 2024) —
   faithful by construction, and it cut human verification time. Phase 0 makes this more attractive
   than it looked: Decision 11's context pack is most of the way there.

## Phases

Each independently shippable.

0. **Measure — DONE 2026-09-23.** Sonnet, project `zolIMa`, read-only observer tools only
   (`--tools ""`, `--strict-mcp-config`, an explicit read allow-list; `CLAUDE_CODE_*` env stripped so
   the observer couldn't auto-pair). One rep — the latency answer was clear, more reps would only spend
   quota:

   | turn | wall | tool round-trips | usage (API-price equivalent) |
   |---|---|---|---|
   | spawn floor (no tools, no schema) | 3 s | 0 | — |
   | cold, bare schema | 68 s | 13 | $0.43 |
   | cold, `reasoning` field first | 53 s | 15 | $0.44 |
   | `--resume`, bare | 26 s | 5 | $0.52 |
   | `--resume`, `reasoning` | 100 s | 29 | $0.78 |

   Findings: time is tool round-trips, not spawn (→ Decision 11, Open decision 4); cache reads run
   137k–548k tokens a turn and grow on resume — on a seat login that's quota, and real use will meet
   rate limits; flat-`id` refs invite wrong-kind citations (→ typed fields in the `KiwiRef` table);
   reasoning-vs-bare inconclusive at n=1 (Open decision 8). Harness: a throwaway script, not
   committed — the flags above are the reproducible part.
1. **Engine contract — BUILT on `feat/kiwi-engine-contract`.** `AgentBackend` is now the contract
   above; `ClaudeAgent` implements it; `run_observer_turn` is a thin call into `run_agent_turn`, the
   observer's turns unchanged. `_build_claude_cmd` gained the Kiwi isolation options Phase 0 used;
   `_parse_claude_result` reads `structured_output` and treats `error_max_structured_output_retries` as
   failure. Tests: a fake engine pins the driver (self-heal, missing-schema failure, unimplemented
   engine fails loudly), plus the new builder flags and parser paths (`app/test/suite/observer.jl`).
   **Also fixed here — a pairing bug Phase 0 exposed:** a headless `claude -p` hands its MCP children
   its own messaging socket, so every in-app turn (Ask Claude, Watch, and the Phase 0 harness) re-paired
   the project to a session about to exit, overwriting the user's pairing. App-spawned turns now load a
   separate `observer-mcp-headless.json` with `CECELIA_OBSERVER_NO_PAIR`; the observer skips auto-pair
   and refuses `register_push_target` under it. The terminal config still pairs.
2. **`KiwiRef` + resolver — BUILT on `feat/kiwi-ref-resolver`.** Shared schema, TS union + parity test
   (mutation-checked: a field added to the schema alone fails it), Julia shape check + per-kind
   resolver + route. Every kind tested both ways against `testpr` (a real object resolves, a
   near-miss fails), including Phase 0's wrong-kind citations. Not yet consumed — Phase 3 calls the
   resolver on every reply, Phase 4 renders `check`.
   **Live check, 2026-09-23** — sets XcPcu8 (4kS67f, 8 images) and obWDNS (zolIMa, 5 images), run
   in-process against the real projects dir: 249 refs across project / set / image / viewer / cells /
   tracks (both the per-track table and the `track_id`-column path) / population (flow, region, track,
   trackclust) / Blackboard / capture; 169 resolved, 76 failed as intended, 0 unexpected; SHA of all
   174 metadata files identical before and after (read-only confirmed). It changed two things: a
   `viewer` ref on an image with **no pixels** (2 of obWDNS's 5 are unconverted) now fails instead
   of passing as `format`, with extents read from the zarr (`image_geometry`) rather than ccid.json;
   and a population label names its type, because one name often exists under several types.
3. **Headless Kiwi turn.** Reply schema, Kiwi system prompt, validate-and-re-ask loop, run from the
   REPL against a real project. Tests: a fabricated ref is rejected; a real ref not seen this turn is
   rejected (Decision 7); re-ask fires once. Track **citation recall** (every claim has a ref) and
   **citation precision** (every ref supports its claim — by hand until Open decision 5 lands) on a
   small fixed set of prompts, per Liu et al. 2023; that set is the regression check for prompt
   changes and for a second engine.
   **Exit gate — Phase 3 is not done, and Phase 4 does not start, until Open decision 8 is settled on
   that prompt set** (reasoning-field-first vs bare schema, ≥5 reps each, claim quality scored by hand
   against the refs). Phase 0's n=1 was inconclusive, and the schema is what every claim is generated
   under — leaving it "TBD" would ship the whole UI on an untested assumption. (Raised in review,
   2026-09-23.)
   **Loop BUILT on `feat/kiwi-headless-turn` (2026-09-23); exit gate NOT yet run.**
   `api/src/kiwi_turn.jl` → `run_kiwi_turn(project_uid, prompt; refs, agent, reasoning)`: reply schema
   (`kiwi_reply_schema`, embedding the shared `KiwiRef` definitions), Kiwi's own system prompt, a v1
   context pack (attached refs + resolved labels — the fuller Decision 11 pack is still open), a
   read-only tool allow-list (`KIWI_READ_TOOLS`), streamed turns so tool results are visible
   (`stream = true` → `_parse_claude_stream`), and validate → one re-ask on the same session.
   "Seen this turn" is textual: a ref's identifying values must all appear in this turn's tool results
   or the pack (whole-number match for ids), or the ref must have been attached. Tested with a scripted
   fake engine (clean reply, re-ask fixes it, re-ask fails, attached ref, abstain, engine failure).
   Eval harness: `scripts/kiwi_eval.jl` + the fixed prompt set `scripts/kiwi_eval_prompts.json`
   (5 prompts incl. a judge trap and a reassurance trap; writes automatic proxies + a blinded
   hand-scoring sheet).
   **First live turns (Sonnet, zolIMa):** orientation of obWDNS — 9 claims, 12 tool calls, 37 s, every
   ref real and seen, no re-ask; judge trap — no verdict, two observations and a checkable question,
   19 s. **What they show the gate must measure:** refs are real but COARSE (claims about populations or
   task runs cite only the image — 0 of 12 refs more specific than `image`), and claims still bundle
   several facts despite the prompt. Presence is enforced; specificity and one-fact-per-claim are not.
   **First gate attempt (2026-09-23) — stopped, not a result.** A 50-turn run spent the seat window at
   turn 29 (uneven per prompt × variant, 2–4 each). What the 29 turns did show: the "coarse refs"
   reading above was prompt mix, not a fault — on the two population prompts 87 of 88 claims cite the
   population; image refs come from the three image prompts, where they are right. So `specificRefFrac`
   only compares within a prompt, and the reasoning variant's higher value (0.49 vs 0.33) was it drawing
   more population-prompt turns. Bundling IS the fault. **Now enforced in the validator, feeding the
   same one re-ask:** `kiwi_claim_bundling` (over 160 chars, `;`/dash joins, a second sentence, a
   3+-item list) and `kiwi_claim_underspecified` (a population path / "track N" / "cell N" in the text
   needs its own ref). Replayed over the 29 recorded turns: 63 of 146 claims bundled (44 on length),
   4 underspecified, 0 misfires after excluding file paths — 22 of 29 turns would have re-asked, so a
   re-ask is the common case and its cost is the next number to measure. **Order from here:** a
   5-turn sanity run (one per prompt, bare) → does the re-ask repair bundling, at what spend → then the
   reasoning-vs-bare comparison sized to the window (e.g. 3 prompts × 2 × 3 = 18 turns, split across
   windows) → hand scoring. The harness prints running output tokens and stops at the first
   session-limit error.
   **Sanity run (5 turns, bare):** every turn re-asked; the re-ask does split claims into single short
   facts; ~5k output tokens a turn. **Comparison (2026-09-23, 18 valid turns = 3 prompts × 2 variants
   × 3 reps, in two `--slice` halves, merged with `--from`; one invalid turn re-run — it had called
   read tools missing from the allow-list, now completed and test-enforced):** automatic proxies favour
   BARE — reasoning cost more (9.7k vs 6.8k output tokens, 91 vs 69 s a turn), re-asked as often
   (8/9 each), ended valid less often (6/9 vs 8/9) and produced more claims (20 vs 14). Refs specific
   on the population prompts in both (≥0.98). No reassure/recommend flags in either. **Not yet the
   verdict** — the gate is the hand score: a blinded, stratified 60-claim sample (10 per prompt ×
   variant) of the 303. **Cost finding for Phase 4:** nearly every turn re-asks, doubling wait and
   spend (~7–10k output tokens a turn, 1–2 min) — the first attempt must pass more often before this
   is a UI.
4. **Cockpit UI — BUILT on `feat/kiwi-cockpit` (2026-09-23), not yet tried in a browser.**
   Backend `api/src/kiwi_api.jl`: `POST /api/kiwi/turn` starts `run_kiwi_turn` on a worker thread (one
   per project — 409 otherwise; 503 without the CLI), WS `kiwi:step` streams each tool call as it
   happens (the engine contract grew an optional `on_progress`; `_claude_stream_steps` reads the
   stream-json line by line) plus "checking refs" / "re-asking: N problems", WS `kiwi:done` carries
   the record; cancel kills the engine (`jobs.jl`); turns kept per Open decision 3. Frontend:
   `components/kiwi/KiwiAsk.vue` as the cockpit's first section — attached chips, typed search (sets,
   images, task functions — this project's, searched locally), the prompt, the Think-first switch,
   live steps, the claims feed (interpretation flagged "I think", questions marked, abstention shown as
   a reply, failed checks counted). `KiwiRefChip.vue` renders every ref: solid = exists, dashed =
   live/format, red = doesn't resolve or not seen this turn — no state claims support (Decision 10).
   Clicking points, via `composables/useKiwiPoint.ts` over the existing machinery: tracks/cells →
   viewer highlight + pop-out, viewer/image → pop-out at t/z, population → the gating page on that
   image, task → its page with that function selected, capture → the shared refocus path
   (`composables/useCaptureFocus.ts`, extracted from its two copies), plot → its page + a plot mark at
   (u,v), ui → a pointer bubble, Blackboard → `/blackboard?entry=`. Tested: API routes with a fake
   engine (`api/test/suite/kiwi_turn.jl`), the pure half in `utils/kiwiTurn.test.ts`.
   **Known gaps:** a population chip lands on the image's gating page, not the population (the
   selected pop is per-panel state nothing outside can set); a tile chip does nothing (no frontend
   tile-mark handler exists — `mark_tile` broadcasts are dropped today too); a task chip whose page is
   already open keeps its current function.
   **First use (user, 2026-09-24, 4kS67f — "which is the best measure to see differences between B
   and T", two summary plots attached):** 21 claims, most a median per image per population ("way too
   many references"); an attached plot failed as "not open" because its panel deregistered during the
   2-minute turn; "I think I think"; attachments showed only "plot"; no elapsed time, model or tokens;
   no way to follow up; chips and the step log read as hand-rolled. Changed: at most
   `KIWI_MAX_CLAIMS` (8) claims (schema + validation), a prompt that answers first, summarises across
   images and takes an attached plot's scope as the answer's; attached refs keep their ask-time result
   through validation; a plot publishes its series / grouping / set + images (`SummaryPanel` →
   registry `content`) and resolves to a label + `detail` + `route`; attachments and claims are
   `SelectionTable` rows; one ticking status line (`useNowTick`); the model picker shared with the lab
   log (`AgentModelSelect`); follow-ups (`followUp` → the earlier turn's engine session, its cited
   refs count as seen); a plot ref scrolls to its panel (`data-guide="plot:<id>"`) and points at it.
   **Second pass (same day):** a population ref outlines its cells in the viewer (`POST
   /api/kiwi/refs/cells` → `PickHighlight`) instead of opening a bare gating plot; pointing at a plot
   folds the page's image table (`utils/sectionOpen.ts`) and rolls Kiwi up when the plot sits under it;
   the sidebar opens the group of the page you land on; a turn fails only on REF problems — shape
   problems that survive the re-ask (`shapeErrors`) don't fail it and aren't shown ("4 claims didn't
   check out — what does this tell me?"); a follow-up can cite a plot attached earlier even once its
   panel is closed; the prompt asks for image names (not uids), no tool names, and the plot cited for
   a claim about it.
   **Reply-quality pass (user: "this first try was terrible"; audit of the four saved turns):** an
   attached plot now carries the numbers it draws — `SummaryPanel` summarises its `/api/plot_data`
   response (`utils/plotSummary.ts`: one row per series, the chart's statistic, any stats test) into the
   registry entry's `summary`, and the context pack includes it, so Kiwi answers from the picture across
   every image instead of rebuilding per-image medians for 2 of 7; a comparison is one fact (length cap
   260, no dash rule); population labels name the segmentation (B vs T read identically); an optional
   `note` carries what isn't a claim. **"Plot this" (user, same day: "that's the whole point of the duck
   pointing towards something you haven't looked at"):** a `proposedPlot` KiwiRef is a plot nobody has
   made — one `add_analysis_board` plot entry; resolved by a dry run of `expand_board` (check
   `proposal`); a click (`POST /api/kiwi/plot/open`) opens the board that already shows it, else adds one
   — the user's write, never the turn's. `board_summaries` now names a slot's DEFAULT measure (a speed
   plot read as "not shown"). **Set stats (user: "why is it always missing the set stats"):** the engine's
   session log showed the set-wide `get_measure_summary` WAS called — its 203 k-character result exceeded
   the CLI's tool-result limit and never reached the model, so it fell back to 3 of 7 images. Now
   `measure_summary(; kind, value_names)` narrows it (14 k for B+T motility over 8 images), the context
   pack carries that table for an attached plot over a set, and it lists the project's boards; a
   proposal already on a board says which. Prompt: exact project uid, units only when the data states
   them. Not yet: a per-measure separation score for "which measure is
   best" questions — new capability, the user's call.
5. **"Add to Kiwi" affordances — BUILT with Phase 4.** One button, `components/kiwi/AddToKiwiButton.vue`
   (`pi-at`), on: every live registered plot panel (`CanvasPanel`, when its `persistKey` is in the plot
   registry — the plotId the resolver knows), the pop-out viewer (the user's selected tracks or cells
   if any, else this view with t and — in 2D — z; `viewerRefFor`), `TaskRunner`'s function, Kiwi's
   capture rows, the Blackboard pane (the previewed version if one is open). The image table and
   population ⋯ menus add an "Add to Kiwi" item. The draft lives in localStorage so the pop-out's button
   reaches the main window's cockpit, and opens it.

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
