> **ARCHIVED — not authoritative, not to be acted on.** Audit of `kiwi-capture-destination-followup.md` against origin/main @ bda43465 (PR #1202, #1213, #1209, #1212 merged). Frozen; the code below has moved by the time you read this — re-verify before implementing.

# Audit: Kiwi capture destination + Blackboard persistence

## §1 Capture destination — current state

**Today's Save on the overlay does not choose a destination — the backend routes it.** `DrawSurface.vue:615` emits `{overlay, notes}`; `ViewerWindow.onDrawSave` (`modules/ViewerWindow.vue:4758`) POSTs to `/api/viewer/capture`, which in `api/src/captures_api.jl:293` calls `push_capture_notification` — sent to the paired Claude Code session's inbox socket if paired, else `:not_paired`. The response `push` field drives `utils/shareOutcome.ts::announceShareOutcome`: `"sent"` shows *"Sent to your Claude session"*, anything else copies the clipboard prompt. **Same flow for canvas Share** via `composables/useCanvasShare.ts:214`.

`AddToKiwiButton` (`components/kiwi/AddToKiwiButton.vue`) attaches a `{kind:'capture', captureId}` ref to the Kiwi prompt (`stores/kiwi.ts::addRef`) — it operates on **already-saved captures**. #1202 mounts it on the capture rows in `KiwiCockpit.vue:521`, not on the overlay itself.

**The brief's premise is right that captures can now be attached to Kiwi**, but wrong that attach-vs-send is a simple choice on Save: send is a backend side-effect of pairing, and "attach to Kiwi" is a **post-Save** action on the captureId. A user who wants both today can — they'll both happen.

The real ask is: *at Save time, should the backend `push` step be conditional on user intent, and should the freshly-created captureId auto-attach to the Kiwi prompt.* Both are useful, and neither replaces the other.

**Recommended UX:** two chip toggles under DrawSurface's notes row (`.ds-notes` area, `frontend/src/components/DrawSurface.vue:883`), rendered as `CcToggle` (per `docs/ui/PRIMITIVES.md:20`): **"Attach to Kiwi"** (default ON when the cockpit is open, else OFF) and **"Send to paired session"** (default = pairing state — ON when paired, OFF otherwise, `pushTarget.parsePushTarget`). Persist last choices in `settings` (mirror `settings.kiwiOpen`). Justification: two independent toggles reflect the fact that they're not mutually exclusive; a menu or radio would falsely suggest they are.

## §2 Blackboard persistence — current shape

`<proj>/blackboard/<entryId>/{entry.md, meta.json, .snapshots/entry@v<N>.md}` (`api/src/blackboard_api.jl:16`). Registry `<proj>/settings/blackboard.json`. Entry meta already carries `attachments: string[]` = a list of captureIds on disk (`api/src/blackboard_api.jl:217`) — attachments are cited by id, not copied. Status (`open|resolved|parked`), outcome (`good|wrong|unknown` + note), and a P5.1 fingerprint sit alongside.

`?entry=<id>` on `/blackboard` opens that entry (`BlackboardModule.vue:411`); `useCaptureFocus.focusCapture(projectUid, envelope)` (`composables/useCaptureFocus.ts:29`) is the ONE refocus path — plot capture → reshow store + module page; viewer capture → `pendingViewState` + pop-out. Consumed by Kiwi rows, Blackboard attachment chips, and Kiwi capture chips.

**A saved Kiwi entry fits as a normal Blackboard entry** with structured Markdown (claims as a list, refs as chips) — no schema change required for a v1. Anything extra (typed entry, sidecar) is an additive `meta.json` field.

## §3 Ref-kind stability

| Kind (from `frontend/src/lib/kiwiRef.schema.json`) | Identifier(s) carried | Stability | Sidecar needed? | Why |
|---|---|---|---|---|
| `project` | – | permanent | no | project uid is the address |
| `set` / `image` | `setUid` / `imageUid` | permanent (identity is the dir name — `project_identity_invariant`) | no | resolve or "gone" |
| `population` | `imageUid + valueName + popPath` | **fragile — rename / rerun / delete all break `popPath`** | **yes (label + last count)** | `_fresh_pop_uid` survives rename in the CURRENT session (via `reresolvePops.ts`) but is not carried in the ref |
| `cells` / `tracks` | `imageUid + valueName + labelIds/trackIds` | fragile — label ids can be renumbered by a rerun | **yes (image name + count)** | ids re-address after re-segmentation |
| `viewer` | `imageUid + t? + z?` | permanent shape; t/z make sense only against the current dims | no | image gone ⇒ chip says so |
| `plot` | `plotId` | **live only** — `_PLOTS_BY_PROJECT` is in-memory (`plots_registry_api.jl:42`), evicted on WS disconnect | **yes (plot summary text)** | current live-only resolution is exactly what the brief flags; `plotSummaryText` already exists |
| `tile` | `imageUid + valueName + cellId + t/z` | live only — landscape must be open | yes (image name + cellId) | same reason as `plot` |
| `capture` | `captureId` | permanent while on disk; user can delete | tiny sidecar (address line) | already stored in the capture's meta.json, cheap to duplicate |
| `task` | `funName` | permanent (it's a registered task name) | no | resolve or "gone" |
| `ui` | `anchor` | churn — DOM `data-guide` values change | yes (label) | anchor may not exist next release |
| `blackboard` | `entryId + version?` | permanent while entry exists | no | self-referential |
| `proposedPlot` | `plot + measure + pops + …` | as stable as `expand_board` is | no | it's a spec, not a pointer |

**Six kinds need a sidecar; six don't.** The lean is neither "sidecar everything" nor "resolve-or-say-gone" — it's **selective**.

## §4 Recommendation — B refined by A

Do **selective sidecar** (option B, but honest about which kinds need one). Sidecar the six fragile / live-only kinds with a minimal snapshot per kind (label + one line of value, no full plot summary except for `plot` where `plotSummaryText` already gives the exact text); for the six stable kinds, resolve-or-"gone" is enough. The saved entry is a normal Blackboard entry; the sidecar rides inside `meta.json` as an additive `kiwiRefs: {refKey: {label, snapshot, savedAt}}` map keyed by canonical ref shape (mirror `_kiwi_canon` in `api/src/kiwi_api.jl:38`).

**The one hard tradeoff:** *how much of the plot's numbers to freeze.* `plotSummaryText` is capped at 12 KB and enumerates every series; a saved entry with three plot refs is 36 KB of frozen text that will never re-compute even if the plot's underlying data was fixed later. Alternative — freeze **only the numbers that a claim cites** (mine claim text for numbers, snapshot those). Cheaper but ambiguous when a claim says "in 6 of 7 images". Dominik decides: full plot summary vs. cited-numbers only.

**Costed touchpoint list** (recommended approach — capture destination + Blackboard persistence):

*Capture destination (5 touchpoints):*
1. `frontend/src/components/DrawSurface.vue` — add two `CcToggle`s + emit `{attachToKiwi, sendToPaired}`.
2. `frontend/src/modules/ViewerWindow.vue::onDrawSave` — read flags; pass `noPush` to POST when send=off; call `useKiwiStore().addRef({kind:'capture', captureId})` when attach=on.
3. `frontend/src/composables/useCanvasShare.ts::onAnnotateSave` — same treatment (keep in lockstep).
4. `api/src/captures_api.jl::api_viewer_capture` — accept `noPush: bool`; skip `push_capture_notification` when set (test: `api/test/suite/captures.jl`).
5. `frontend/src/stores/settings.ts` — two persisted keys `captureAttachToKiwi`, `captureSendToPaired`.

*Blackboard persistence (7 touchpoints):*
6. New `frontend/src/utils/kiwiTurnSave.ts` — pure builder: turn + claim → Markdown + `kiwiRefs` sidecar; per-kind snapshotters (call sites limited to fragile kinds); tested.
7. `frontend/src/components/kiwi/KiwiAsk.vue` — Save button per claim + whole-turn on the claims feed; POST via existing `blackboardApi.createBlackboardEntry`.
8. `frontend/src/utils/blackboardApi.ts` — extend create/revise types to include `kiwiRefs` sidecar dict.
9. `api/src/blackboard_api.jl` — `_write_bb_meta!` gains an additive `kiwiRefs` field (same absent-on-untagged pattern as `outcome`, `fingerprint`); pass-through in create/revise handlers.
10. `frontend/src/components/kiwi/KiwiRefChip.vue` — fallback rendering when live resolution fails + sidecar present ("was: <label> · <snapshot>"); test.
11. `frontend/src/composables/useKiwiPoint.ts` — no-op / soft-fail when the target no longer resolves; unchanged for live.
12. `mcp/cecelia_mcp/server.py` + `guidance.py` — the MCP `create_blackboard_entry` tool signature grows `kiwiRefs` optional (additive; two files per `feedback_mcp_tool_three_files`).

**Total: ~12 touchpoints**, seven of them additive-shape changes rather than logic rewrites.

**Decide before starting:**
- Full plot summary vs. cited-numbers-only in the plot sidecar (12 KB × N vs. mining risk).
- Default for the two overlay toggles (attach-to-Kiwi defaults ON when cockpit open?).
- Whether saved Kiwi entries need a visual marker in the Blackboard list — brief says undecided, and it's cheap to defer.

## §5 Open questions surfaced by the audit

- **Refresh semantics.** If a Kiwi entry is saved, then the underlying population is renamed via a fresh chain, does the entry's ref chip update to the current name (live resolution wins) or freeze to the label at save time? Both are defensible; the brief says "reads the same way months later" which argues for freeze, but chip label update would be less confusing during active work.
- **Follow-up conservation.** `_kiwi_conversation` (`api/src/kiwi_api.jl:36`) rebuilds a follow-up's context from the earlier turn's `priorRefs` + `priorResults`. Saving a turn to Blackboard doesn't (and shouldn't) delete it from `kiwi/turns.json` — but the 50-turn cap means a saved entry may reference a turn no longer follow-uppable. Not a bug, just a state that should be visible in the UI.
- **Per-claim vs. per-turn.** Brief allows either. Per-claim reads clean (one thought per entry) but multiplies entry rows fast; per-turn keeps context (all claims + note together) but a "save this one claim" affordance encourages curation. Recommend per-turn as the primary Save with an optional "extract this claim" secondary — matches how the brief's outcome-note discipline (Decision 11 in blackboard_api.jl:23) is entry-scoped.
- **Snapshot invalidation.** If the on-disk data a snapshot describes is later regenerated, the sidecar is silently stale. No cheap way to detect this without hashing. Accept and document, or add a lightweight ETag per ref kind? Recommend accept-and-document; matches the "reads the same way months later" contract.
