# spaCR-inspired adoptions — plan

**Status:** parked (2026-09-24) · branch `docs/spacr-inspired-plan`. Two small, additive features
lifted from spaCR (Olafsson et al.), plus the citation policy that goes with them. Written to
survive a context break — the surveys that produced it are in
`scratchpad/spacr-analysis.md` and `scratchpad/spacr-items-vs-cecelia.md`.

## Goal

Absorb the two design patterns from spaCR that we don't already do — **HeldPin** (chain-edge
inputs a user can pin against upstream drift) and **`pixi run doctor`** (one command that
audits everything that makes cecelia fail on a fresh clone) — and set a policy for citing
design debts we take from other single-lab tools.

## Source

- **spaCR** — Olafsson et al., BSD-3, https://github.com/EinarOlafsson/spacr
- Cloned locally at `../../spacr` (git sha `79d71a92`, dated 2026-09-22)
- Preprint DOI: to be filled from `spacr/README.md` when citation lands

Both patterns are Einar Olafsson's design decisions. The code we might read is largely LLM
output at scale, but the *ideas* — pinned chain inputs, one-command environment doctor — are his.
We are borrowing ideas, not code, so nothing about spaCR's BSD-3 licence attaches to us; the
obligation is academic, not legal (Decision 3).

## What already ships (do not re-do)

Verified 2026-09-24 (`scratchpad/spacr-items-vs-cecelia.md`):

- **Chain graph plumbing** — `app/src/tasks/chain/types.jl` (`ChainTemplate`/`ChainNode`/
  `ChainEdge`), `frontend/src/modules/ChainModule.vue`, `frontend/src/utils/taskOutput.ts`
  (write-namespace registry + edge prefill), per-image sticky funparams via `TaskRunner.vue`
  drafts. **What's missing is only the pin policy — the graph knows what an edge derives; it
  doesn't yet know how to lock a value against upstream change.**
- **Lineage view** — `ChainModule.vue` is already authoring + live runtime in one; spaCR's
  FlowView is runtime only. Nothing to steal there.
- **QC layer** — `1/{uid}/qc/{funName}/{valueName}.json` + `read_all_qc` + badges everywhere
  (`QC_PLAN.md` Phase 1). One unified layer keyed by `(funName, valueName)` — stricter than
  spaCR's one-QC-module-per-stage. Nothing to steal there.
- **CurationLog append-only ledger** is `CORRECTION_DIRECT_PLAN.md` Decision 4, P2+. This
  plan does not touch it — see that plan.
- **Native installers with bundled Python** — declined in `docs/SHIPPING.md`; the pixi +
  juliaup bootstrap script is v1 by design. Not in scope here.

## Locked decisions

### Decision 1 — HeldPin: `source` field on chain-node params (2026-09-24)

Every entry in `ChainNode.params` gains a `source` discriminator:

- `"default"` — value from the task's declared default; re-resolves when the default changes.
- `"edge"` — value derived from an upstream node's output via `taskOutput.ts`; re-resolves when
  the upstream `value_name` shifts.
- `"pinned"` — user typed or explicitly locked a literal; **never re-resolves**. An unpin action
  returns the entry to `"edge"` (if the edge still exists) or `"default"`.

Migration is trivial: every existing chain becomes `source: "edge"` where an incoming edge
exists, `source: "default"` otherwise. No behaviour change until a user pins.

**Why the discriminator lives on the node param, not on the edge:** a param can have both an
edge and a pin at once — the edge is still declared, the pin overrides the resolution. Deleting
the edge does not delete the pin. This matches spaCR's `HeldPin`/`PinStore` split.

### Decision 2 — Divergence between a pin and its upstream is a QC finding (2026-09-24)

When a param is `pinned` and its would-be `edge` resolution differs from the pinned value, the
chain node emits a QC finding via the shipped `(funName, valueName)` QC layer:

- **Short (badge):** `pinned input differs from upstream`
- **Long (tooltip):** `pinned to '<literal>'; upstream now writes '<current>'. Unpin to follow
  upstream.`

This puts the pin/upstream divergence on the same colour-blind-safe severity token used
everywhere else (`QC_OBSERVER_PLAN.md`) — no new UX invention, and the user sees drift where
they already look for drift.

### Decision 3 — Ideas-only citation policy for prior-art adoptions (2026-09-24)

When we adopt a **design pattern** (not code) from another single-lab tool, we cite it in three
places:

1. **At the point of use** — one line at the top of the module that lands the pattern:
   `# Design pattern adapted from spaCR (Olafsson et al., <doi>). See docs/todo/SPACR_INSPIRED_PLAN.md.`
2. **`docs/CREDITS.md`** under a "Prior art / design inspirations" heading — one line per
   borrowed idea, name the concept, link the repo + preprint. Grows over time; this plan seeds
   the file with HeldPin and doctor.
3. **The cecelia paper** — cite the source tool in the related-tools paragraph and again in the
   methods sentence describing the borrowed behaviour.

No copied code means no BSD-3 header travels; if any actual code is later lifted, the licence
header comes with it and lands in `LICENSES/`. That case is not this plan.

### Decision 4 — `pixi run doctor` is a dispatcher over existing checks (2026-09-24)

`scripts/doctor.jl`, wired as `pixi run doctor`, prints one ordered report of green/amber/red
rows plus a fix hint per row. It does NOT re-implement any check that already exists; it calls:

- Pixi env sanity: declared-in-`pixi.toml` vs installed vs actually-in-`$CONDA_PREFIX` — inline
- Juliaup channel + `Manifest.toml` resolvable — `Pkg.status()`
- `pnpm install --dry-run` — inline
- Env-var shadowing — shells out to `scripts/check_claude_env.sh`
- Release bundle integrity + `/api/health` — shells out to `scripts/bundle_check.sh`
- Optional envs (`cellpose3`, `cellpose4`, `leidenalg`) — presence + which task expects each
- Observer/MCP link + `set_projects_dir` isolation guard — inspect config
- Disk space + `projects_dir` writable — trivial

Row format: `<glyph> <check name> — <one-line status>. Fix: <command>` (fix omitted on green).
Exit code non-zero on any red row so CI and installers can wrap it.

### Decision 5 — Doctor is CLI-only for v1; the Settings→System button is v2 (2026-09-24)

The dispatcher runs in a terminal and prints text. A "Run doctor" button in Settings→System
that pipes through the API and renders the same rows is a natural v2, but not part of P1 or P2 —
it needs the API surface and a rendering component that don't exist yet, and the CLI already
covers the "why isn't this working on a fresh clone" case that motivates the whole feature.

## Non-goals (things spaCR does that we deliberately do not take)

- **FASTQ/screen regression arm** — different domain (fixed-cell CRISPR screens vs live-cell/
  spatial). Out of scope forever.
- **SQLite-per-plate storage** — cecelia's project dir + zarr + H5AD is the invariant. Do not
  touch.
- **Qt monolith GUI** — Vue 3 + browser is the invariant. Do not touch.
- **Native OS installers with bundled Python** — declined in `docs/SHIPPING.md`.
- **CurationLog append-only ledger** — same idea, but that work lives in
  `CORRECTION_DIRECT_PLAN.md` Decision 4. Do not re-plan it here.

## Phases

### P1 — `pixi run doctor` (half day)

- New `scripts/doctor.jl`, ~200 lines, dispatcher over the checks in Decision 4.
- `pixi.toml` gains one `[tasks]` entry `doctor = "julia --project=. scripts/doctor.jl"`.
- Docstring at the top of `scripts/doctor.jl` per Decision 3.
- `docs/CREDITS.md` seeded with the "Prior art / design inspirations" section, one entry for
  the doctor pattern.
- No frontend change.
- **Checkpoint:** `pixi run doctor` on a fresh clone prints amber for anything missing with an
  actionable fix hint, green when the tree is set up.

### P2 — HeldPin (one weekend)

- `app/src/tasks/chain/types.jl` — `ChainNodeParam` gains `source: "default"|"edge"|"pinned"`
  and optional `derived_from: "nodeId.port"` for `edge` entries. Serialisation additive; loader
  fills `source` from presence-of-incoming-edge on old chains.
- `frontend/src/utils/taskOutput.ts` — resolver short-circuits on `pinned`, returns the literal.
- `frontend/src/modules/ChainModule.vue` — pin/unpin affordance per input row (icon; filled =
  pinned). Row tooltip shows the divergence text from Decision 2 when relevant.
- `app/src/correction_staleness.jl` — pin/upstream divergence becomes a staleness reason;
  emitted as a QC finding via the shipped layer (Decision 2).
- Tests: pin survives an upstream `value_name` change; unpin follows the current edge; pin
  without an edge is legal; migration of an old chain leaves it identical in behaviour.
- Docstring at the top of the changed modules per Decision 3.
- `docs/CREDITS.md` gains a second entry for the HeldPin pattern.
- **Checkpoint:** a chain with a pinned input keeps its literal across an upstream rerun that
  changes `value_name`; the badge lights up; unpin returns to edge-follow behaviour.

### P3 (optional) — Settings→System "Run doctor" button

- `/api/doctor` endpoint that runs `scripts/doctor.jl` and streams rows as JSON.
- Settings→System gains a "Run doctor" button rendering the rows with the same severity tokens
  as QC badges.
- Only start once P1 has been in use long enough that the CLI report is stable.

## References

- `scratchpad/spacr-analysis.md` — the initial survey that produced the shortlist
- `scratchpad/spacr-items-vs-cecelia.md` — the item-by-item verification of what cecelia
  already does
- `scratchpad/spacr-authorship.md` — evidence on which parts of spaCR are Einar's design vs
  LLM output at scale (relevant for the citation calibration in Decision 3)
- `docs/todo/CORRECTION_DIRECT_PLAN.md` — the parallel CurationLog work (out of scope here)
- `docs/SHIPPING.md` — the deliberate decline of native installers
- `docs/QC_PLAN.md`, `docs/QC_OBSERVER_PLAN.md` — the shipped QC layer HeldPin plugs into
- `../../spacr/spacr/chaining.py`, `../../spacr/spacr/ports.py`, `../../spacr/spacr/artifacts.py`
  — the `HeldPin`/`PinStore` source (read-only reference; no code lifted)
- `../../spacr/spacr/doctor.py` (if that's where it lives) — the `spacr doctor` source
