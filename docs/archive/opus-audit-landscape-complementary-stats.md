# Audit prompt: complementary landscape — per-tile stats beyond category

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

## Context

PR #1086 snapshots the Landscape overlay (`{grid, tiles, legend}`, category
per tile: dark/bright-textured/mixed) into the capture envelope so it
travels with the PNG instead of a live 1hr-TTL bag.

Live use (see attached session transcript) showed the category dimension
mostly restates what's visible in the RGB composite — dark border vs.
textured interior is not a hard call from pixels. The category field
didn't change Claude's actual finding in that session; comparing peer
marks to segmentation outlines in the image did.

Decision: don't drop the landscape, extend it — but only with data that's
genuinely **complementary** to the pixels, not a duplicate encoding of
them. Category stays (cheap, occasional grounding value); everything new
must clear the bar of "not readable off the RGB."

## Design principle (non-negotiable, keep flagging violations of this)

No field goes in unless it's either (a) not visible in the composite at
all, e.g. per-channel signal untangled from colour-blend, or population
/track/HMM membership, or (b) currently on-screen but ambiguous by eye.
If a proposed field can be answered by looking at the image, it doesn't
belong in the tile.

## Scope: mirror the RGB's own visibility rule

Only layers actually toggled on in the viewer at share time go into the
tile. Not "everything available for this image" — same rule that already
governs which channels appear in the RGB composite. This means:

- No per-image capability-detection needed — read whatever layer
  state/stores already drive the viewer's rendering, snapshot those.
- Sparse by construction: a tile has no `pops` key if the population
  layer isn't on, not `pops: []`. Envelope size tracks what's visible,
  not a fixed max schema.

## Proposed shape

```
tile = {
  category,                              // unchanged, keep
  channels: {                            // only currently-visible channels
    <channelName>: { mean, snr },        // untangles colour-blend ambiguity
  },
  segCount,                              // objects segmented in this tile — only if seg layer on
  pops: [{ popId, name, count }],        // only if population/gating layer on
  tracks: {                              // only if tracks/props layer on
    count, meanDuration, meanSpeed,
    hmmStates: [{ state, count }],       // part of tracks+props, not separate
    motifs: [{ motifId, count }],        // landed motifs, part of tracks+props
  },
  sourceRun: <analysisRunId>,            // required whenever any computed field is present
}
```

`sourceRun` is not optional once a tile carries anything beyond raw pixel
stats — a visible population/track layer doesn't say which
gating/clustering/HMM run produced it, and that ambiguity is the thing
most likely to cause a confidently-wrong cross-reference later (stale run
vs. current). If different fields can come from different runs, decide
whether `sourceRun` is per-tile or per-field before implementing.

## Explicitly out of scope for this pass

- Anything not currently toggled visible in the viewer, even if computed
  and available for the image. (HMM state and motifs are in scope — they
  land as part of the tracks+props layer, so they're included whenever
  that layer is on, same visibility rule as everything else.)
- Full track paths — presence/summary stats only (`tracks.count`,
  `meanDuration`, `meanSpeed`), not the geometry.

## Validation before/alongside building

Run the disagreement test on the *current* category-only landscape: find
a capture where a tile reads `mixed` or `edge` in a spot that looks
unambiguous by eye, and check whether Claude's own visual read agrees.
Agreement everywhere across a few samples confirms category is
decorative and the real leverage is in channels/segCount/pops as
designed here — useful confirmation to have before/alongside the build,
not a blocker to it.

## Relation to other in-flight capture work

Structured sidecar-next-to-flattened-image is the same pattern under
discussion for module-page multi-panel captures (dataRef-addressable
marks + PNG). Worth checking whether landscape's tile sidecar and the
module-page capture's mark/panel sidecar should share one convention for
"structured ground truth attached to an image capture" rather than
evolving two independent shapes — flag if they diverge for good reason,
otherwise converge them.

## Ask

Produce an implementation plan: exact viewer layer-state read points to
snapshot from, `sourceRun` resolution per field, envelope size impact at
realistic layer-on counts, and MCP tool docstring updates — following the
scoping and sparsity rules above. Flag anything here that doesn't fit
those rules rather than building around it.
