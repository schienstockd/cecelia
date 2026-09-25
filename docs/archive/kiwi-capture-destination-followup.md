> **ARCHIVED — not authoritative, not to be acted on.** Original prompt handed to Opus 4.7 as a follow-up to PR #1202. Outcome pending.

# Follow-up: Kiwi input and output persistence

Context: PR #1202 (feat/kiwi-cockpit) added `AddToKiwiButton` on capture rows, among other places, so a capture can now be attached to the Kiwi prompt like any other ref.

Two persistence gaps to close in this pass — one on the input side, one on the output side.

## 1. Capture destination choice (input)

Earlier guidance (given to you, Opus, before this PR) said captures should always go to the paired Claude Code session. That's now out of date — captures can be attached to Kiwi directly.

On the capture overlay, give the user an explicit choice of destination when they save/send a capture:

1. **Attach to Kiwi prompt** — adds it as a ref chip in the cockpit's `KiwiAsk`, same as `AddToKiwiButton` elsewhere.
2. **Send to paired Claude Code session** — the original behavior.

Things to sort out:

- Where the choice lives on the overlay (inline toggle vs. two buttons vs. a small menu) — your call, match existing overlay UI patterns.
- Whether to remember the user's last choice as a default, or always ask.
- Confirm `AddToKiwiButton`'s existing capture-row wiring (from #1202) is what "attach to Kiwi" should reuse, rather than building a second path.
- Check whether the paired-Claude-Code-session path still needs to exist as-is, or if it should also go through the same ref/attachment plumbing.

## 2. Persisting Kiwi output to the Blackboard

Right now Kiwi turns are throwaway — kept in `<project>/kiwi/turns.json` (last 50) but not surfaced anywhere durable. A Kiwi answer (claims + their ref pointers) should be saveable to the Blackboard as an entry, sitting alongside other Blackboard content with the same navigable ref chips (`KiwiRefChip` / `useKiwiPoint`).

Recommendation, to start from rather than reopen from scratch:

- **Save action: a button on the claims feed** (per-claim or whole-turn), not drag-to-blackboard — reuses the existing `useCaptureFocus` / `?entry=<id>` machinery from #1202 instead of building a new interaction.
- **Freeze the claims at save time, don't re-resolve refs live.** The point of a record is that it reads the same way months later. Snapshot the claim text and resolved values; keep the ref chips navigable so you can still click through to the plot/image, but if the underlying object is gone, the chip should say so plainly rather than silently drifting to different data.
- A saved Kiwi entry should open the same way as other Blackboard entries, via `?entry=<id>`.

**Lean toward a sidecar snapshot, not a durable-reference/resolution engine.** Solving permanent object resolution (stable IDs that survive renames, reruns, deletions) is a much bigger problem than this needs. #1202 already snapshots plot data at ask-time (`SummaryPanel`/`plotSummary.ts`) purely so Kiwi can answer from something concrete — do the same at save-time: store that summary (or the population/image equivalent — label, image name, measure values) in the Blackboard entry next to the claim. The ref chip still tries to resolve live (same solid/dashed/red states as now); when it can't, fall back to the sidecar data instead of a dead link, so the claim stays checkable against a frozen snapshot rather than needing the object to still exist unchanged.

Have Opus audit the actual approaches here rather than just implementing the above — weigh sidecar-snapshot against any lighter alternative (e.g. is a stable-enough ID already available for some ref kinds, is a full sidecar overkill for those), and confirm before building.

Open questions either way:

- What exactly goes in the sidecar per ref kind (plot summary is clear; population/image refs need their own minimal shape — label, image name, measure values, etc.).
- How `useKiwiPoint` behaves against a saved entry's refs months later — does it still fold/scroll/highlight correctly, or does that machinery assume a live turn's state, and does it need a fallback path when live resolution fails.

Still open: exact entry schema/shape on the Blackboard side, and how a saved Kiwi entry is visually distinguished from other entry types.

## Scope

Both the capture-destination choice and the Blackboard persistence need to be addressed in this pass — don't land one and defer the other.

Repo: `schienstockd/cecelia`, PR for reference: #1202.
