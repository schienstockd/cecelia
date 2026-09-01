# Provenance — how this software was built, and how we know it works

The short version is in the [README](../README.md#how-this-software-was-built). This is the longer
version, with the field context that shaped the disclosure choices, the full per-subsystem
validation record, and the position on questions the field hasn't settled yet. It exists so that a
reader who wants to judge the work in detail — a reviewer, a downstream user, a lab picking up the
code — has enough to work with.

## Field context — what the disclosure was calibrated against

At the time of writing (September 2026), the field hasn't converged on how AI-assisted scientific
software should be developed, disclosed, credited, or validated. What exists is:

- **JOSS's January 2026 AI-use policy** ([announcement](https://blog.joss.theoj.org/2026/01/preparing-joss-for-a-generative-ai-future/)) —
  the only formal editorial policy that names the software case directly. Authors must state which
  tools/models they used and where, describe the scope of assistance, and assert that human authors
  reviewed and validated the output. Failure to disclose is treated as an ethical breach. JOSS also
  moved the same year from an effort-based bar ("three months of work") to one about research
  impact and intellectual contribution.
- **Nature Portfolio** policies cover manuscript preparation but not software development
  specifically. AI cannot be a listed author; AI-generated figures are banned. There is no
  Nature-side rule for what a Methods section should say about AI-written code.
- **rewrites.bio** (a community manifesto) is prescriptive for AI-assisted *rewrites* of existing
  tools: credit the originals, emulate output byte-for-byte where possible, disclose which tools
  did the writing. It's silent on original tools that have no upstream reference. It also names
  its own limitation plainly: "output comparison catches what you tested, not what you haven't."
- **Community precedents** — Seqera's RustQC, Fulcrum's fgumi, Rob Patro's sshash-rs/piscem
  rewrites, MorPhiC/STAR-Flex — all single-tool AI-written open-source scientific projects, all
  validated by output equivalence against their references. They differ mostly in how loudly they
  disclose the AI role: RustQC leads with "Built with AI", fgumi tucks a single sentence
  mid-article, and both are published as if that were normal. Community reaction (Heng Li's
  ["AI rewrite dilemma"](https://lh3.github.io/2026/04/17/the-ai-rewrite-dilemma) is the
  best-known write-up) is cautiously permissive, and names **long-term maintenance** as the
  unsettled elephant — not attribution and not validation.
- **What isn't in that list.** Every AI-disclosed scientific project we could find is a single
  tool. No open-source multi-subsystem framework substantially written by an AI, with a public
  disclosure statement saying so, surfaced in the searches. That absence isn't the same as absence
  of the *practice* — someone could have quietly built a framework without disclosing — but for
  the *literature*, the framework case simply isn't there yet. This is the honest reason Cecelia's
  validation story looks different: the field's default check (parity vs. a reference) doesn't
  apply where there is no reference.

The AI-disclosure section in the README was calibrated against those references. It does not try
to claim consensus that doesn't exist, and it does not adopt any one manifesto as if it were a
standard.

## Validation record — subsystem by subsystem

### Ported subsystems — validated by matching the R reference

- **celltrackR track measures** (`app/src/tasks/tracking/track_measures.jl`) — the track-measurement
  functions from the R `celltrackR` package, translated into Julia. Validated by matching the R
  version's numeric output on the same input.
- **btrack pipeline** — the Bayesian cell tracker (`btrack`) is invoked identically to the prior
  stack; only the wrapper changed. There is no algorithmic delta to validate.
- **Drift correction** — ported from the R version and believed output-equivalent. If a future
  contributor edits this code, revalidating parity against the R implementation is the natural
  check.

### Reference-implementation cross-checks

- **Logicle transform** (`app/src/gating/transforms.jl`) — from Moore & Parks 2012, cross-checked
  against the FlowUtils `logicle_c` implementation. Golden values asserted in
  `app/test/runtests.jl`. This is the existing template referenced in `CLAUDE.md` for the
  "cite sources, validate against golden values" convention.

### Original subsystems — validated by real-image visual inspection

- **Segmentation on real intravital data.** Early during the port, model tuning was going by an
  accuracy metric computed without ground truth. On those numbers, temporal smoothing looked
  unhelpful, and the AI proposed dropping it. Visual inspection on real intravital output showed
  the temporal-smoothed model was in fact what captured the cells being analysed — the metric had
  been optimising something other than the biological signal. The final choice was made on the
  images.
- **Gating — population plausibility.** Beyond the logicle golden values, gated populations were
  checked on real projects by looking at where the gated cells land in the image — sitting on the
  correct cells, in the correct compartments. This is additional to the numeric transform check
  and is what would catch a wrong gate that nonetheless transformed correctly.
- **WebGPU viewer / shader rendering.** Eyeballed on real data for whether the image looked
  natural, and cross-checked against the offline renderer. The two renderers disagreed initially;
  the disagreement was tracked down and resolved rather than either side being trusted on its own.
  Independent renderers agreeing on the same pixel value is the strongest correctness check
  available for a rendering path with no analytical ground truth.
- **Autofluorescence correction.** AF touches roughly 12% of signal on the projects it's been run
  on. The correction was reworked to a
  derivation-from-histogram model (triangle thresholding, Zack et al. 1977) after visual inspection
  showed the earlier knob-driven version was setting the wrong ceilings on real images (see
  [FAQ.md](../FAQ.md) → "Why does autofluorescence correction have almost no settings?").
  A downstream-impact audit is in flight; when it lands, this line updates to reference the
  concrete PR.
- **Offline renderer.** Validated 2026-08-27 on a real project (zolIMa / fXgbTl): the
  `record_view_movie` + encoder path was confirmed to produce a movie that plays in the browser.

### Exercised on common scenarios, not exhaustively covered

- **Chain executor.** The scheduler that runs analysis chains has been exercised on the
  arrangements Dominik runs day-to-day — the "common shapes" of the analysis DAG. Component parts
  are unit-tested (`pixi run test-pkg`), but arbitrary user-driven chain shapes — parallel
  branches, resume-then-retry mid-chain, unusual resource-pool combinations, cancellations
  interleaved with dependencies — have not been covered exhaustively. Real bugs are expected to
  surface once other people run combinations Dominik hasn't. This is a stated gap rather than a
  claim of coverage.

For most of the above, real intravital data was the validation, not synthetic fixtures. Synthetic
inputs were useful for correctness tests during development; their outcomes diverged from real
data often enough that they weren't a substitute for looking.

## Attribution position

Claude wrote essentially all of the code. Dominik directed it, made every scientific and design
decision, and reviewed as much as was practical. Neither of them is the sole author in the
pre-2024 sense of the word. The one convention every surveyed disclosure agrees on is that AI is
not listed as an author, and Cecelia follows that. On the rest — how loudly to name the AI role,
what the human's role is called, whether a tool built this way is publishable — the field has not
converged. Cecelia's README describes what happened rather than picking a position on questions
the field hasn't answered.

## Publication position

Undecided. If publication happens, two shapes are on the table:

- A *Nature Methods*-style paper for the parts that are genuinely new — the browser WebGPU viewer,
  the offline renderer, the analysis board — where the technical contribution has no direct
  predecessor.
- A *Nature Protocols*-style paper for the full pipeline, referencing the 2025 *Nature
  Communications* paper on the original R/Shiny `cecelia` for the underlying scientific framework.

Neither is a commitment.

## When this document changes

Add a dated line here when the disclosure or validation record is materially updated, so that a
future reader can see whether the position has drifted from the one first written down.

- **2026-09-01** — first version. Written from the AI-disclosure-audit exercise; supersedes an
  earlier README paragraph that asserted validated correctness without the specific per-subsystem
  detail.
