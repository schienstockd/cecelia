# Kiwi capture destination + Blackboard persistence

**Status:** planning (2026-09-25) · branch `docs/kiwi-capture-dest-followup` · unbuilt

**Supersedes:** the prompt at `docs/archive/kiwi-capture-destination-followup.md` and re-frames its capture-destination premise (see Decision 1). The audit that grounds this plan is `docs/archive/kiwi-capture-destination-audit.md` (@bda43465).

## Goal

Two persistence gaps left open after #1202 (feat/kiwi-cockpit):

1. **Input side.** The capture overlay implicitly sends every capture to the paired Claude Code session, whether the user wants it there or not, and never auto-attaches the resulting capture to the open Kiwi prompt even when that is clearly the intent.
2. **Output side.** Kiwi turns are throwaway (last 50 in `<project>/kiwi/turns.json`) and cannot be preserved as a Blackboard entry. A useful Kiwi answer disappears when the ring rolls.

Close both in one pass. Do not open the "durable object identifiers for every ref kind" problem; snapshot the fragile refs and lean on live resolution for the stable ones.

## Locked decisions

1. **Attach-to-Kiwi and Send-to-paired are independent, not a picker.** The brief framed them as a mutually-exclusive destination choice. They are not. Sending is a backend side-effect of the pairing at Save time (`captures_api.jl:293` → `push_capture_notification`); attaching is a post-Save action on the `captureId` (`stores/kiwi.ts::addRef({kind:'capture', …})`). A user can want zero, one, or both. Ship as **two `CcToggle` chips** under the notes row on `DrawSurface`, not as a picker.
2. **Send-to-paired default = pairing state.** ON when `pushTarget.parsePushTarget` says the session is paired; OFF otherwise. Matches today's implicit behaviour, so the default is a no-op change for existing users.
3. **Attach-to-Kiwi default = cockpit-open state.** ON when the Kiwi cockpit is open (`settings.kiwiOpen`), OFF otherwise. Reflects that a user with the cockpit open is almost certainly asking about the thing they just captured.
4. **Both defaults are persisted per-user.** Two settings keys (`captureAttachToKiwi`, `captureSendToPaired`); on subsequent Saves the last user choice wins. `settings.ts`, additive.
5. **Backend flag: `noPush: bool` on `/api/viewer/capture`, default false (= today's behaviour).** Keeps the change additive; unaffected callers (e.g. the canvas Share path if not migrated in the same pass) behave as they do today.
6. **Save-Kiwi-to-Blackboard is a per-turn Save with an optional per-claim extract.** Per-turn is the primary affordance on the claims feed: one Save button on the whole turn, one entry produced. A secondary "extract this claim" on individual claim rows produces a smaller entry when the user wants to curate. Rationale: matches how outcome notes are entry-scoped (`blackboard_api.jl:23` Decision 11), avoids inflating entry counts, still allows curation.
7. **Selective sidecar snapshot — six of twelve ref kinds only.** Fragile / live-only kinds (`population`, `cells`, `tracks`, `plot`, `tile`, `ui`) carry a minimal snapshot inside `meta.json`. Stable kinds (`project`, `set`, `image`, `viewer`, `task`, `blackboard`, `proposedPlot`) resolve-or-say-"gone" with no snapshot. The stability table is §3 of the audit; do not re-derive it. Rationale: half the refs are already stable enough; a blanket sidecar taxes disk and codepaths for no gain.
8. **Plot snapshot = full `plotSummaryText`, not cited-numbers-only.** The 12 KB cap × N refs is trivial (~5 MB over a project's saved-entry lifetime); `plotSummaryText` is computed at ask-time anyway (see #1202's `plotSummary.ts`) so this is a storage decision, not a computation one. The cited-numbers-only alternative needs a fragile claim-text extraction step that silently loses numbers. If a claim was made against numbers that were later fixed, the discrepancy IS the finding — that is the point of a record.
9. **Sidecar lives inside `meta.json`, not a new file.** Additive field `kiwiRefs: {refKey: {label, snapshot, savedAt}}`, keyed by the canonical shape from `_kiwi_canon` (`api/src/kiwi_api.jl:38`). Matches how `outcome` and `fingerprint` are additive fields today; absent on non-Kiwi entries; no schema migration.
10. **Chip fallback contract.** `KiwiRefChip` tries live resolution first (as today) and, on failure, falls back to `was: <label> · <snapshot>` when the sidecar is present, or the existing "gone" state when it is not. Snapshot invalidation (data regenerated on disk but sidecar unchanged) is accepted and documented — do NOT add an ETag or hash to detect it. The "reads the same way months later" contract is the point.
11. **A saved Kiwi entry is a normal Blackboard entry.** No new entry type, no visual differentiator in the Blackboard list — deferred, cheap to add later if it matters. Body is structured Markdown (claims as a bullet list, refs as inline chips); `attachments[]` already handles capture ids.
12. **No follow-up conservation.** Saving a turn to Blackboard does NOT copy or freeze the turn in `kiwi/turns.json`; the 50-turn ring rolls as normal. A saved entry may point at a turn that can no longer be followed-up; that is a state the UI shows plainly, not a bug to prevent.

## Phases

Each phase ends with a testable checkpoint. P1 and P2 can ship as separate PRs.

### P1 — Capture destination toggles (input side)

Landing bar: capture overlay respects both flags; existing users see no behavioural change on first launch.

- P1.1 — Backend `noPush` flag on `/api/viewer/capture`; skip `push_capture_notification` when set. Test in `api/test/suite/captures.jl`.
- P1.2 — Two `CcToggle` chips on `DrawSurface.vue`'s `.ds-notes` area, emitting `{attachToKiwi, sendToPaired}` on Save.
- P1.3 — `ViewerWindow.onDrawSave` reads the flags: pass `noPush` when Send=off; call `useKiwiStore().addRef({kind:'capture', captureId})` when Attach=on. Mirror in `composables/useCanvasShare.ts::onAnnotateSave` in the same PR (lockstep — leaving one behind is the bug from `_repair_dont_warn`-style drift).
- P1.4 — `stores/settings.ts` persists `captureAttachToKiwi` + `captureSendToPaired`; defaults per Decisions 2 + 3.
- **Checkpoint:** unit test on the backend flag; manual Chrome/Firefox check on the overlay with all four toggle combinations (memory: Dominik cannot check Safari; do not ask him to).

### P2 — Blackboard persistence (output side)

Landing bar: a saved turn round-trips — appears in the Blackboard list, `?entry=<id>` opens it, ref chips resolve live where they can and fall back to the sidecar where they can't.

- P2.1 — New `frontend/src/utils/kiwiTurnSave.ts`: pure builder `turn → {markdown, kiwiRefs, attachments}`. Per-kind snapshotters for the six fragile kinds only (Decision 7). Unit-tested.
- P2.2 — `blackboardApi.ts` `create`/`revise` type extended with the optional `kiwiRefs` sidecar; `api/src/blackboard_api.jl::_write_bb_meta!` gains the additive field, pass-through in handlers. Round-trip test.
- P2.3 — Save button on the whole turn in `KiwiAsk.vue` (per Decision 6), calling `blackboardApi.createBlackboardEntry` with the builder's output.
- P2.4 — `KiwiRefChip.vue` fallback rendering per Decision 10; visual states: live-resolved (as today) / gone-with-sidecar (`was: <label>`) / gone-no-sidecar (existing). Component test.
- P2.5 — `useKiwiPoint.ts` soft-fails when the target no longer resolves; no-op on unchanged live behaviour.
- P2.6 — MCP `create_blackboard_entry` grows an optional `kiwiRefs` parameter (`mcp/cecelia_mcp/server.py` + `guidance.py` — the two-file rule, see `feedback_mcp_tool_three_files`). Purely additive.
- P2.7 — Per-claim "extract this claim" secondary Save on individual claim rows in `KiwiAsk.vue`. Same builder, smaller input.
- **Checkpoint:** on the mertk zolIMa project, save a turn with a plot ref + a population ref + an image ref; delete the plot's underlying registry entry (simulate WS disconnect), reload the Blackboard entry, verify the plot chip falls back to `was: <label>` while the population chip resolves live.

### P3 — (deferred / non-goal)

- Visual entry-type differentiator on the Blackboard list — Decision 11 says wait until there's a use.
- Snapshot invalidation detection — Decision 10 says accept-and-document.
- Durable plot identifiers (persist `_PLOTS_BY_PROJECT`) — out of scope; the sidecar is the answer to plot fragility.

## Touchpoint accounting

~12 files, 5 for P1 + 7 for P2. Seven of the twelve are additive-shape changes (new field, new toggle, new setting, new fallback branch) rather than logic rewrites. Full list in §4 of the audit.

## Open questions — resolved before P1

None. Everything the brief left open (attach vs send framing, per-claim vs per-turn, snapshot content, sidecar location, entry visual marker) is a decision above. Two follow-up questions surfaced during audit and were resolved:

- **Refresh semantics** (chip label follows live rename vs freezes at save time): live wins where resolution succeeds; sidecar label is only shown on fallback. Matches Decision 10 without adding a rule.
- **Snapshot invalidation:** accepted and documented — Decision 10.

## References

- Audit that grounds this plan: `docs/archive/kiwi-capture-destination-audit.md`
- Original brief (archived): `docs/archive/kiwi-capture-destination-followup.md`
- Merged predecessor: PR #1202 (feat/kiwi-cockpit) — `AddToKiwiButton`, `plotSummary.ts`, `?entry=<id>` refocus
- Ref schema: `frontend/src/lib/kiwiRef.schema.json`
- Blackboard shape: `api/src/blackboard_api.jl:16` (disk layout), `:217` (attachments), `:23` (Decision 11)
- Kiwi turn ring: `api/src/kiwi_api.jl:36` (`_kiwi_conversation`), `:38` (`_kiwi_canon`)
- Capture path: `frontend/src/components/DrawSurface.vue:615`, `frontend/src/modules/ViewerWindow.vue:4758`, `api/src/captures_api.jl:293`, `frontend/src/composables/useCanvasShare.ts:214`
