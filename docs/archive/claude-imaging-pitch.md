> **ARCHIVED — not authoritative, do not act on this.** A frozen record of the framing at the time.
> Not a description of how the code works now, not instructions to re-run. Cited from
> `docs/todo/PROJECT_MEMORY_PLAN.md` and `docs/todo/BIDIR_CONTEXT_PLAN.md` as the field-level context
> those plans sit inside. The visual-grounding primitive this pitch names as missing is what the
> BIDIR plan built (share-in, point-out, Blackboard, captures — all shipped 2026-09-20).

# The case for Claude Imaging

## Where Claude Science is today

Claude Science's demonstrated strengths — autonomous drug candidate ranking,
genomics QC (catching a contaminant in RNA-seq data), literature synthesis into
cited evidence — share a common precondition: the underlying data is already
legible to a model. Expression matrices, sequence data, and structured
databases are tabular, queryable, and embeddable. An agent can reason over them
directly, cite its source, and hand off a defensible conclusion.

## Where imaging is different

Bioimage data — intravital microscopy, multichannel z-stacks, cell tracking,
behavior over time — is not legible in that way. A raw acquisition isn't
tabular until *after* a segmentation and tracking pipeline has already made a
long chain of judgment calls: which pixels are a cell, which detections belong
to the same track, which channel crosstalk needs correcting first. The
scientific content lives in *spatial and temporal structure* that a table
cannot capture without first flattening away the thing a person would actually
look at to judge whether the analysis is right.

Concretely, none of Claude Science's public demos are imaging-based, even
though computational biology nominally includes it. That's not an oversight —
it reflects that the current tooling (database connectors, tabular skills,
text-based citation trails) doesn't have a way to reach this kind of data at
all.

## What's actually missing: a visual grounding layer

Claude Science doesn't need to solve this to be valuable where it already is.
But a "Claude Imaging" direction can't be built by pointing the same
architecture at microscopy data — it needs a layer Science doesn't have: **a
way for the model to perceive spatial/temporal structure directly**, not just
receive a derived table.

Concretely, that means:
- **State + pixels together** — a way to hand the model an exact viewer
  state (image, channel, timepoint, selection) alongside the actual visual
  content, not a text description of it.
- **Addressable, not just visible** — a conclusion about "this track" or "this
  channel" needs to trace back to an exact frame/state, the same way a Science
  conclusion traces back to a cited dataset row.
- **Motion, not just snapshots** — many imaging problems (a track jump, a
  segmentation flicker, a transient artifact) are only visible across frames,
  not in one image.

Without this layer, none of the imaging-specific skills you'd eventually want —
segmentation QC, track-review, channel-bleedthrough diagnosis, spatial-pattern
interpretation — have anything to be built on top of. It's the same relationship
database connectors have to Claude Science's tabular skills: the connector has
to exist before the skill can.

## What genomics has that imaging doesn't — and the one place it partly does

Genomics' agentic tooling can lean on decades of standardization: reference
genomes, canonical gene identifiers, public structured databases. Imaging has
no real equivalent across the board, but two existing efforts are worth naming
directly rather than treating the gap as total:

- **[Immunemap](https://immunemap.org/)** — an open, cloud-hosted, curated
  database specifically for intravital microscopy: over 58,000 human-curated
  single-cell tracks and 1M+ cell-centroid annotations across 400 videos, with
  public APIs, built by a 20-lab consortium under FAIR principles. This is the
  closest thing imaging has to what a reference database is in genomics —
  curated, structured, and queryable rather than sitting on individual labs'
  hard drives. It's a plausible source of external ground truth (to validate an
  agent's segmentation/tracking judgment against expert-curated data) and a
  plausible target for Cecelia's own outputs to become interoperable with,
  rather than staying siloed.
- **[image.sc](https://forum.image.sc/)** — the de facto community knowledge
  base for bioimage analysis (ImageJ/Fiji, CellProfiler, ilastik, Bio-Formats,
  napari, and more). This is a different kind of gap: not missing data, but
  missing *structure*. The tacit, hard-won knowledge of how to actually solve a
  segmentation or tracking problem lives scattered across thousands of forum
  threads, not as indexed protocols or skills the way a genomics workflow might
  be documented. Before this becomes agent-usable domain knowledge (the
  imaging equivalent of a skill), it needs the same kind of distillation this
  project's own maintainability audit is doing internally — turning scattered,
  ad hoc knowledge into a small number of durable, checkable references.

Neither of these makes the visual-grounding gap go away — Immunemap holds
curated tracks and metadata, not raw pixels for an agent to perceive directly,
and image.sc is unstructured community text, not a skill. But both are
concrete, existing infrastructure a "Claude Imaging" effort could build on
rather than starting from nothing, and are worth naming explicitly rather than
treating the domain as having no standardization story at all.

## Other gaps worth naming honestly

Some of these are further out than the visual-grounding layer, and some may
turn out to be beyond what's actually achievable or in scope here. Listing them
isn't a roadmap commitment — it's marking directions worth auditing toward, so
they're visible rather than only discovered later.

- **Provenance as a first-class, citable fact.** In genomics a value is stable
  and unambiguous. In imaging, every downstream number depends on the
  segmentation/tracking params that produced it — change a threshold, get a
  different answer. Cecelia already records this (per-run params, content-hashed
  chain runs), so the groundwork exists; what's missing is making sure it's
  surfaced *alongside* every number an agent reasons over, not just logged
  separately. This is plausibly in scope — mostly a matter of exposing what's
  already tracked, through the MCP layer, consistently.
- **A canonical vocabulary for what's in an image.** A gene symbol means the
  same thing everywhere; an HMM behavior state numbered "2" means nothing
  outside the one fit that produced it. There's no imaging equivalent of a Gene
  Ontology for cell behavior or phenotype. This is genuinely beyond what one
  platform can solve — it's a field-level standardization problem, closer to
  what a body like the Immunemap consortium might eventually take on than
  something to build alone. Worth being honest that this is aspirational, not
  a near-term deliverable.
- **Ground truth to check an agent's visual judgment against.** Unlike a
  reference genome, there's no clean "correct answer" for whether a
  segmentation or a track is right — even human experts disagree. Immunemap's
  curated tracks are the best available proxy, but building an actual benchmark
  (does Claude's QC judgment agree with expert-curated ground truth on a held-out
  set) is real, non-trivial work — worth scoping as a possible collaboration
  output rather than assuming it falls out for free.
- **Scale mismatch between imaging data and how agents consume context.**
  Tabular data fits in a prompt; a single acquisition doesn't. OME-Zarr's
  chunked format already makes partial/patch access tractable, which is the
  right foundation — but any imaging-specific skill needs to be designed around
  "operate on a patch or a frame," not "the model sees the whole volume." This
  is more an engineering discipline to hold onto than a missing piece, but
  worth stating so it doesn't get assumed away later.

None of these block the visual-grounding work already underway — they're the
next layer of questions once that's real, and some may simply turn out not to
be this project's problem to solve alone.

## What Anthropic just started building — and what it doesn't touch

On 2026-08-27 Anthropic previewed the **Model Hardware Standard (MHS)**, a
shared specification letting Claude discover, operate, and troubleshoot
physical lab equipment (microscopes, liquid handlers, robotic arms) through
standardized drivers, with HHMI Janelia Research Campus as the anchor
microscopy partner and an MBF Bioscience driver for ScanImage in flight. Early
results are real: seven vendor programs on one Janelia rig unified into a
single scriptable interface, a multi-day imaging workflow compressed to a day,
laser stabilization on QuEra's quantum computer taken from 58% to 99.3%.

Two things follow from MHS existing, both directly relevant to the case above:

- **Imaging as a distinct scientific domain for Claude is not speculative
  anymore.** MHS is Anthropic itself moving on imaging-specific primitives, not
  Claude Science pointed at a new database. The "Claude Imaging" framing here
  is now the analysis-side counterpart to what MHS does upstream, not a
  standalone bet.
- **MHS solves acquisition/control, not perception.** The launch post is
  explicit that Claude "still requires expert oversight" for spatial and
  physical reasoning, and the worked failure example — a liquid-handling
  misdiagnosis at Genentech where Claude's default was retry-with-different-
  params, agitating the fluid worse — is exactly the class of failure a visual
  grounding layer would catch. MHS makes the missing piece *more* visible, not
  less.

Where MHS's demonstrated wins concentrate is worth naming honestly: highly
structured, repeatable workflows — drug-discovery screens, laser calibration
loops, standardized imaging runs — the same shape as slide scanners and
histology screens in a clinical setting. What stays open is exploratory
live-imaging work, where the operator is making real-time judgment calls
(follow that rolling neutrophil, that vessel just bled, this channel just
photobleached) the agent can't yet make. The natural role for Claude there
is trainer/advisor to a human operator, not autonomous executor — matching
Anthropic's own oversight language rather than fighting it.

**The natural fit:** MHS upstream (agent drives acquisition, safely, on a
supported rig), an analysis platform like Cecelia downstream (agent perceives
and reasons over what was acquired), the bidirectional context-sharing
primitive proposed in the companion design document sitting between them as
the shared perception layer. Both halves need to exist for the loop to close;
MHS on its own gives you a scriptable rig with no eyes on the data, and
Cecelia on its own gives you eyes on the data with no way to influence what
gets acquired next.

## Why this is worth building now, and why from here

This isn't a hypothetical gap — we're actively building the first piece of it:
an MCP-based mechanism for an agent to receive exact viewer state plus pixel
content from a running bioimage analysis platform (Cecelia), on demand,
following the same additive/auditable philosophy Claude Science already
commits to (append-only writes, conclusions traceable to source state).

Two things make this a good place to develop the primitive rather than
somewhere more central:
- **A real, running platform already in the loop.** This isn't a green-field
  proposal — it's grounded in an existing pipeline (segmentation, tracking,
  behavior/HMM analysis, QC) with real failure modes to design against, not
  synthetic ones.
- **A deliberately narrow, well-scoped first step.** The ask isn't "build an
  imaging workbench" — it's "let an agent see exact state + pixels on demand,"
  which is buildable as one MCP tool and testable immediately, the same way
  Science's own connectors were presumably built and validated one at a time.

## The ask

Not a feature request against Claude Science as it exists today — a flag that
imaging is a scientific domain the current architecture structurally can't
reach yet, plus an offer: a concrete, running testbed to develop and validate
the missing primitive against, before it becomes a wider imaging direction.
