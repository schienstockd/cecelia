# Standardizing Behavior Readouts — Cecelia

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

## Background

Cecelia's `hmm.jl` fits per-image HMMs to track features and assigns each
timepoint a numeric state (e.g. state 1, 2, 3...). That state numbering is
**not portable**: state "2" in one HMM fit means whatever that fit happened to
converge on — it has no fixed relationship to state "2" in a different image,
animal, or experiment. This was flagged earlier (in the Claude Imaging pitch
doc) as a real, field-level gap: genomics has stable identifiers (a gene
symbol means the same thing everywhere); imaging behavior has no equivalent —
there's no "Gene Ontology for cell motility."

Immunemap (the open intravital-microscopy atlas, EMBO J 2025) is a relevant
reference point here, not a solution to copy wholesale: their published
analysis derived **four motility patterns** from unsupervised learning over
track features — directed movement, arresting, focused (small-area)
patrolling, and extended (large-area) patrolling. That's one field-tested
taxonomy, but it's their own unsupervised result on their own dataset, not a
standard anyone has adopted — worth treating as informative, not authoritative.

**Why this matters for the MCP connection specifically**: right now, any MCP
tool that surfaces behavior/track data from Cecelia can only report raw,
per-fit HMM state indices — which means Claude has no stable vocabulary to
reason with across images, and no way to compare a finding in one image to a
finding in another without re-deriving what each state number happens to mean
in that specific fit. A standardized readout would let an MCP tool report
something Claude can actually reason over and compare — "this track shows
arrest-like behavior" — consistently, regardless of which image or project
it's querying.

## Task

### Part 1 — Audit what's actually computed today

Read `hmm.jl` and its callers in full. Establish:
- What features actually go into the HMM fit (speed, turning angle,
  displacement, confinement, etc. — whatever `_running_mean_vecs` and the
  fitting pipeline actually use).
- Whether any project currently maps numeric states to human-readable labels
  by hand (a lookup table, a convention in a notebook, anything informal) —
  if so, that's evidence of a need already being worked around ad hoc.
- Whether population/clustering code elsewhere in the codebase (`clustTracks`,
  `clustPops` task families) already computes anything resembling
  motility-descriptor summary statistics that could feed a standardized
  readout, so this doesn't get built as a third parallel computation.

### Part 2 — Design a standardized behavior-readout layer

Two things need to be standardized, and they're not the same problem — keep
them separate:

1. **Continuous motility descriptors — model-independent, always computable,
   always comparable.** Things like mean speed, straightness/directionality
   index, arrest coefficient (fraction of time below a speed threshold),
   confinement ratio. These don't depend on any HMM fit or clustering — they're
   computable directly from a track's positions over time, and they're
   comparable across images/animals/experiments *by construction*, because
   they're not fit-relative. This is the layer worth prioritizing: it doesn't
   require agreeing on category boundaries, just agreeing on a fixed formula
   and a fixed field name for each descriptor.
2. **A canonical label set for common qualitative patterns — harder, and
   should be scoped modestly.** Don't try to force every project onto a fixed
   4-category taxonomy (Immunemap's own categories emerged from *their* data,
   not as a universal standard, and forcing a T-cell-specific taxonomy onto
   every future cell type/tissue would be exactly the kind of premature
   universal-ontology overreach the pitch doc already flagged as a field-level
   problem, not a one-project problem). Instead: define a small, extensible
   label set with a handful of common labels (e.g. `arrested`, `directed`,
   `patrolling_local`, `patrolling_extended`, `unclassified`) that a project
   *may* map its own HMM states onto voluntarily, alongside project-specific
   custom labels that stay project-specific and are never assumed to be
   comparable elsewhere. Make the "I don't fit any standard label" case
   (`unclassified` or a project-specific label) a first-class, unpenalized
   outcome — this must not pressure someone into mislabeling a real, novel
   behavior just to fit the standard set.

### Part 3 — Expose it consistently through MCP

- Define one canonical shape for a "behavior readout" (the descriptors from
  Part 2.1, plus an optional label from 2.2, plus the project's own raw
  state if present) that every relevant MCP tool returns, rather than each
  tool inventing its own field names for the same kind of data — this is the
  same "implicit contract" risk (pattern 4 from the maintainability audit)
  applied to MCP tool outputs instead of internal Julia structs.
- Update `guidance.py`/the session briefing so Claude knows this vocabulary
  exists and how to use it when asked to compare or characterize behavior
  across images.
- Note where this belongs in `docs/MAINTAINABILITY.md` and `docs/MAP.md` —
  this is exactly the kind of standing convention those docs exist to record.

## Your own verdict

- Is trying to standardize *any* qualitative label set premature, given how
  early-stage and dataset-specific Immunemap's own four patterns are — should
  Part 2.2 be dropped entirely for now, with only the continuous descriptors
  (2.1) standardized?
- Does the continuous-descriptor layer (2.1) actually generalize across all
  of Cecelia's current use cases (immune cell motility, the tumor/collagen
  T-cell project, whatever else exists), or are there use cases where these
  specific descriptors don't make sense and a different set is needed?
- Is this something worth proposing back to the Immunemap team or the wider
  field (per the earlier pitch doc's framing) rather than just solving
  locally for Cecelia — or is it too early/too Cecelia-specific for that to
  make sense yet? Give a clear recommendation, not just the tradeoff.

Don't hedge this section to match the tone of the rest of the report — this is
the part meant to be read first.
