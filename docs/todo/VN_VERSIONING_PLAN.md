# Value-name versioning — every writer targets a new `vn@v`, never overwrites

**Status:** planning (2026-09-18). No branch. Comes out of the chain-execution-prerequisites prompt
(`docs/archive/chain-execution-prerequisites-prompt.md`) — closing items **#1** (non-destructive
default) and **#3** (mechanically-can't-overwrite invariant) collapses into this one primitive.
Worth building on its own merit for human-triggered runs; not conditional on ever granting execution
rights.

## Goal

Every chain-produced output — zarr image store, zarr label store, `.h5ad`, `gating/{vn}.json`, and
any other artifact keyed by `value_name` — is written to a new *version* of that `vn`. Existing
versions stay on disk until an **explicit** prune. In-place overwrite becomes impossible at the
writer, not by convention.

**Explicit non-goal.** This is not project-directory snapshotting (the old-R "duplicate the data
dir" shape). It is per-`value_name` versioning at the writer level.

## Why now / why this shape

- The R version's whole-project-dir shape doesn't fit. Store sizes are multi-GB per image; on a
  per-image chain (which is the concurrency model the scheduler already runs on), a 4-image set
  with denoise + seg + tracking + gating would 8×–16× disk cost. ~99% of the bytes are raw input
  that doesn't move — a whole-dir copy pays the invariant part to protect the delta.
- The primitive already exists in shape. `ccid.json` versioned-fields is content-hashed history for
  small values, and `value_name` is the addressable axis every writer already uses. Extending that
  pattern is cheaper than inventing a new one, and the chain-execution prompt asks for exactly this
  ("the same content-hashed-history pattern chain templates already use").
- Filesystem snapshots (btrfs / zfs / APFS clone) are ruled out on Windows — see *Non-goals*.

## Locked decisions

### D1 — Monotonic per-vn integer, not a content hash

Version is `1, 2, 3, …` per `value_name`. Not a hash.

- Content hash needs the store to exist before the path is known — chicken-and-egg for a directory
  of chunks written incrementally.
- Monotonic ints are readable in the file browser and stable at write-plan time.
- Hash-of-tree for integrity is a separate future concern, orthogonal to addressing (see *Open
  questions*).

### D2 — `latest` pointer in `ccid.json`, per (vn, kind)

Every reader that asks for `vn` unqualified resolves to `latest`. The pointer lives in `ccid.json`
under a versioned-field, one entry per `(value_name, kind)`. Promoting a run's output to `latest`
is one atomic ccid patch.

### D3 — Chain runs pin their inputs at plan time

When a chain node's params say `input_value_name: "cells"`, the run **freezes** that to `cells@3`
(whatever `latest` was at plan time). Re-running upstream later cannot silently change what a
running downstream node sees. Lockfile-vs-manifest.

### D4 — Directory layout: `default/<vn>/v3/…`, not `default/<vn>@3/…`

Grouped by vn.

- Prune is `rm -rf default/<vn>/v2/`, no glob.
- One directory per vn in the file browser, not N sibling dirs cluttering the root.
- Backward compat is a one-shot migration: `default/<vn>/<contents>` becomes
  `default/<vn>/v1/<contents>` (P4).

### D5 — Prune is explicit only, at least at first

No age-based or count-based auto-eviction until real disk numbers say otherwise. First
implementation ships a Settings → Storage → "Prune versions" surface + a `POST /api/versions/prune`
route. Auto policies are a follow-up plan, not this one.

### D6 — The write path refuses to target an existing `(vn, version)`

`zarr_utils.staged_store`, `write_h5ad_atomic`, and the Julia writers (`write_atomic` for gating
JSON) refuse to promote to `default/<vn>/vN/` if that directory exists. Enforcement lives in the
writer, not in a caller check. This is item **#3** of the chain-execution list; it falls out for
free once D1+D2 are in place — no separate mechanism.

### D7 — Kinds covered

All of these are versioned per `(vn, kind)`:

- Image stores (`default/<vn>/vN/` — image data)
- Label stores (`default/<vn>/vN/` — label data)
- `.h5ad` files (label props / cell tables)
- `gating/{vn}.json`
- Any per-vn sidecar (denoise manifest, correction `plan.json` when keyed by vn, per-vn track output)

**Not versioned here:** raw input (immutable by convention), `ccid.json` itself (already versioned
fields), project-scope files.

## Phases

Each phase is an independently-shippable PR.

### P1 — `ccid.json` schema + reader resolution

Add per-vn version list + `latest` pointer to `ccid.json`. Add (or extend) `resolve_value_name(vn)`
on both sides — Julia (`app/src/model/image.jl`), Python (`python/cecelia/utils/…`) — returning
`(vn, version)`. Route every existing reader (`label_props`, `zarr_utils.open_zarr` /
`open_as_zarr`, `series_base`, `read_axes` / `read_scale`, `LabelPropsView`, gating loader,
frontend vn pickers) through it. Legacy single-version projects return `v1` implicitly.

**No writer changes yet.**

### P2 — Writer path: versioned target + can't-overwrite guard

`zarr_utils.staged_store`, `write_h5ad_atomic`, and Julia `write_atomic` take a `(vn, version)`
target. Version defaults to `latest+1` at plan time. Refuse if `default/<vn>/vN/` exists (D6).
`test_zarr_access_convention.py` + the `zarr-access ratchet` testset extend to cover this. Item #3
lands here.

### P3 — Chain planner pins inputs

`chain.jl` freezes `input_value_name` refs to `vn@version` at plan time (D3). Chain params grow an
optional explicit version qualifier for advanced use; default is "pin to `latest` now." Also settle
how gating references a specific version — probably "the gate is authored against `vn@v` and only
follows `latest` if the user opts in" (see *Open questions*).

### P4 — One-shot migration for existing projects

Sweep every existing project's `default/<vn>/<contents>` into `default/<vn>/v1/<contents>` and
update `ccid.json`. Idempotent, atomic per-vn. Runs on first project open after upgrade, with a
progress log entry.

### P5 — Prune surface (Settings + route + UI)

Settings → Storage: per vn, list versions with size + timestamp + "in use by chain run X" flag;
explicit prune button per version. `POST /api/versions/prune {vn, versions[]}`. No autoprune.

### P6 — Frontend vn pickers grow a version chip

Every place a vn is selected — LabelView, PopManager, plot vn selectors, ChainDesigner, Correction
worklists — gets an optional version chip. Default `latest`; explicit past-version selection is
read-only downstream (a task run against `v2` writes `v4`, not overwriting `v3`).

## Touchpoints — grep-free estimate

Central helpers (few) drive many callsites:

- **Julia readers:** `label_props`, `zarr_utils.open_zarr` / `open_as_zarr`, `series_base`,
  `read_axes` / `read_scale`, gating loader — ~4 helpers, ~15–20 callsites.
- **Julia writers:** `staged_store`, `create_multiscales`, `write_atomic`, chain-node writers —
  ~3 helpers, ~10 callsites.
- **Python:** `LabelPropsView`, `zarr_utils.staged_store`, `write_h5ad_atomic`, `open_as_zarr` —
  same shape.
- **`ccid.json` schema:** one migration.
- **Chain planner:** one pin site.
- **Frontend vn pickers:** ~5 components.
- **MCP tools referencing `value_name`:** audit in P1.

The refactor is many callsites but few primitives — that's what makes it tractable.

## Open questions — non-blocking, decide in-phase

- **Content-hash integrity** as a separate future item (hash-of-tree after the write completes,
  recorded next to the version in ccid.json). Not in this plan; different concern.
- **Cross-project vn versioning** — `PROJECT_IO_PLAN.md` needs to know how to serialise version
  chains in `.ccbundle`. Coordinate at P4/P5.
- **Gating vs `latest`** — does a gate authored on `v2` re-evaluate against `latest`, or stay
  pinned? Probably pinned unless the user opts in; settle in P3.
- **Failed-run cleanup.** If P2 crashes mid-write, does the partial `vN/` dir need `latest` never
  to have moved (yes — atomic promote is only on success), *and* a way to remove the partial? P2
  should refuse to reuse a version number even if the previous attempt was aborted; prune surface
  (P5) exposes the orphan.

## Non-goals

- **Filesystem snapshots** (btrfs, zfs, APFS clone). Zero-cost when they work; Windows kills it and
  item #1 explicitly asks for a structural default, not FS-dependent.
- **Whole-project directory snapshots** (the R shape). Rejected on cost + granularity — see *Why
  now*.
- **Automatic eviction policies.** Explicit prune only in v1.
- **Granting chain execution rights.** This plan closes prerequisites #1 and #3 from the
  chain-execution list. Granting execution is a separate decision this plan does not authorise.
