# Verification Pass: Package / API / GUI Separation

`phase-separation-prompt.md` was executed already (by Sonnet). Your job is not to redo the work — it's to independently verify it's coherent. Don't read the prior audit table first; re-derive your own, then diff.

Reference CLAUDE.md and ARCHITECTURE.md for context.

## Step 1 — Independent re-audit

Without looking at any existing audit table or commit messages explaining the rationale, go through the current `Cecelia.jl/src/` and `api/` (or wherever the layers landed) and classify what you find, same scheme as the original phase:
- **PACKAGE** — pure domain/computation, no client I/O
- **API** — request/response shaping, route/socket handling
- **MISPLACED** — code that's in the wrong layer given the rule below

> Rule: Cecelia.jl must be usable headless from the Julia REPL with zero knowledge of HTTP, WebSockets, or Vue.

Output your own `file → function → classification` table.

## Step 2 — Diff against what's actually there

Compare your independent classification to the actual current placement. Flag every disagreement. For each one, state which layer it should be in and why — don't just flag it, resolve it.

## Step 3 — Run the actual contracts, don't just read code

Where possible, verify by execution, not inspection:

- **REPL-only test**: with `api/` not running, no websocket, no Vue — can you call `run_task` and `run_tasks` (single image and image set) successfully? Can you call the data-read accessors (`popDT`-equivalent, `summary`-equivalent, `tracksMeasures`-equivalent)?
- **Napari from REPL**: can Cecelia.jl drive napari directly from the REPL (per `vignettes/run_animation.Rmd` precedent), or does it silently require `api/` to be running?
- **Python interop from REPL**: same check for PythonCall-based module functions (segmentation, torch, zarr) — REPL-runnable standalone?
- **Param validation**: call `run_task` with a deliberately invalid param (wrong type, out of range, missing required field). Does it fail with a clear error citing the spec constraint, or does it run anyway / crash deep in the module function / silently coerce?
- **Logging**: does a REPL-run task produce a log in the same location/format as a GUI-run task?
- **HPC/SSH**: is job submission callable from the REPL without `api/` running?
- **Lockfile/transaction**: read what was actually implemented for the naive concurrency guard. Does it match the sketch (pid:timestamp, stale reclaim, ownership check, `with_transaction` do-block), or did it drift? Either is fine — but check it actually guards against a leaked lock on exception (force an error mid-transaction and confirm the lock releases).
- **Spec single-sourcing**: confirm the Vue form still reads param definitions from Cecelia.jl's `.json` spec files rather than a hand-maintained duplicate.

## Step 4 — Report

For each item in Steps 2 and 3, give a verdict: **OK**, **DRIFTED** (works but deviates from the intended design — note if the deviation is actually fine), or **BROKEN** (contract violated). Don't fix anything yet — report first, so D can decide what's worth fixing now vs. accepting as a documented deviation.

## Step 5 — Add targeted tests (after Step 4 is reviewed, not before)

Not full coverage — this codebase's real risk is structural/architectural, not per-module correctness (wrong science gets caught by eyeballing plots, not unit tests). Write tests for these specifically, check if any already exist from Step 3 before duplicating:

1. **Boundary contract**: load `Cecelia.jl` with `api/` not on the load path at all, run one real module function end-to-end. This is the test that catches accidental coupling creeping back in later.
2. **Param validation**: for each module, one test with a deliberately invalid param (wrong type / out of range / missing required) — assert it raises with a spec-citing error rather than running or silently coercing.
3. **Lockfile/transaction**: force an exception mid-`with_transaction`, assert the lock is actually released afterward.
4. **Set expansion**: assert a project/image-set resolves to the correct member IDs (the `uIDs <- names(cciaObj$cciaObjects())` equivalent) — `run_tasks` and the batch data accessors depend on this everywhere.
5. **One golden-output canary per language boundary**: one Julia-native module (e.g. gating) and one PythonCall-routed module (e.g. segmentation), each run against a small fixture image with known expected output, asserted within tolerance. Purpose is to catch the Julia/Python IPC seam breaking silently, not to validate the science.

Roughly 8-10 tests total. Place them in `Cecelia.jl/test/`, runnable via the package's normal test entrypoint, no `api/` or fixture servers required.
