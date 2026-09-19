# Julia package — `Cecelia.jl`

This file loads only when a session touches `app/`. Root [`CLAUDE.md`](../CLAUDE.md) holds the
cross-cutting rules (H5AD, zarr, `run_py`, git, tests); this holds the Julia-and-task-specific ones,
so a frontend session never pays for them.

**Revise tracks `app/src/` only.** `api/src/*.jl` is `include`d by the server script and is **not**
Revise-tracked — changes there need a server restart. Struct/macro changes in `app/` also need a
restart; function bodies reload on save.

**What already exists:** [`docs/inventory/JULIA_APP.md`](../docs/inventory/JULIA_APP.md) —
`grep -n -i '<thing>'` it rather than reading it.

---


## Julia conventions

- Mutating functions: `!` suffix (`open_image!`, `set_channel_names!`)
- Strings: double quotes only (single quotes = `Char`)
- Multiple dispatch: separate method per type, not OOP overloading
- `@infiltrate` = `browser()` from R
- Shell commands: always platform-safe. Use `_kill_tree` (`app/src/jobs.jl`) and `_dir_bytes` (`app/src/utils.jl`); never write `pgrep`/`kill`/`du` inline.
- **Don't `export` generic names that collide with common deps or Base.** Exports land in any
  user's namespace; if Cecelia and another `using`'d package both export the same name, Julia
  leaves it *unbound* (ambiguous), breaking unqualified calls. In particular avoid clashing with
  **DataFrames** (`transform`, `select`, `groupby`, `combine`, `subset`, `rename`, `stack`,
  `unstack`, `nrow`, `describe`, `order`) and Base. Prefer specific names — e.g. we export
  `apply_transform`/`invert_transform`, not `transform`/`invert`. If you must share a generic
  verb, extend the owner's function (`import DataFrames: transform`) rather than exporting your own.

### HTTP.jl v2 WebSocket
Use `HTTP.listen`, not `HTTP.serve` — the latter is request→response only and doesn't support
WS upgrades. Full stream-handler/WS-upgrade/response conventions are in
[`docs/API.md`](../docs/API.md) → *HTTP.jl v2 conventions* — read that before touching
`api/src/server.jl` or `api/src/sockets.jl`.

---


## Channel names → indices — always `channel_indices` (and `channel_names` for the list)

**A `channelSelection` param holds channel NAMES, not indices.** Two helpers, both in
`app/src/model/image.jl`, and a handler needs both:

```julia
names    = something(channel_names(img; value_name = value_name), String[])
channels = channel_indices(get(params, "channels", nothing), names; what = "channels")
```

- **`channel_indices`** returns **0-based** indices (what the Python side slices with), accepts an
  already-resolved index unchanged, and **errors by name** on a miss — with a "differs only in case"
  hint, because two images from one experiment shipped `mem-TOM` and `mem-Tom`. Six handlers once
  hand-rolled `findfirst(==(String(ch)), ch_names)` and drifted into three separately wrong
  behaviours: an index crashed four of them, an unmatched name was silently **dropped** by five, and
  drift correction silently fell back to channel 0 — registering a whole timelapse against SHG.
- **`channel_names(img; value_name)`** is where the list comes from, because it **falls back to the
  active version**. Names are typically registered only under `default` while a processed version
  carries none of its own, so reading the raw versioned field (`versioned_get_field(raw,
  "imChannelNames", value_name)` / `ccid_channel_names(raw, value_name)`) returns `nothing` and the
  task reports "(none registered)" for an image whose channels the picker is happily listing — the
  picker is fed by `channel_names(img)` in the image payload, so any other source disagrees with what
  the user just clicked.

Enforced by the `channelSelection params resolve through channel_indices` testset: a task whose
resolved spec declares one and whose handler never calls `channel_indices` fails the suite.


## Spawning Python — always go through `run_py`

**Never spawn a Python subprocess by hand. There is one launcher — `run_py` in `app/src/py_runner.jl`
— use it for every Python task runner and data-layer writer.** It writes the params JSON to the
run's task dir (`task_run_dir(<obj>._dir)`, never a temp dir), sets `PYTHONPATH=python/` (so the script
does `import cecelia.*` with **no `sys.path` bootstrapping**), streams `[PROGRESS] n/total` → `on_progress`
and the rest → `on_log`, registers the process for cancellation, and returns clean-exit (checks
`exitcode` AND `termsignal`). It's the analogue of the old R `self$pyScript`.

```julia
ok = run_py("tasks/<category>/<name>_run.py", (; …params…), task_run_dir(img._dir);
            on_log = on_log, on_progress = on_progress, on_process = on_process)
ok || return nothing
```

- **Do not** write `run(pipeline(\`$python …\`))`, build a params file, or parse `[PROGRESS]`
  inline in a task — that boilerplate (and the bugs that come with hand-rolling exit/signal checks
  and param-file locations) is exactly what `run_py` exists to delete.
- **Python runners therefore carry NO `sys.path` manipulation** — `import cecelia.*` resolves via the
  PYTHONPATH `run_py` sets. A new `sys.path.insert(... __file__ ...)` in a runner is a red flag.
- This is the same principle as the H5AD rule above: a cross-cutting operation gets **one**
  canonical helper, and reimplementing it inline is the bug. (See `docs/MODULES.md` → *Running a
  Python subprocess*.)

---


## Task system

See [`docs/MODULES.md`](../docs/MODULES.md) for the complete step-by-step guide: Julia handler, Python script, JSON spec, registry, module page, route/nav wiring, composite tasks, and tests.

### Key invariants (read before writing any task)

**Tasks are sink-agnostic.** They report through injected callbacks and never call `ws_progress`/`ws_log` directly — those are API-layer concerns. The same `_run_task` runs unchanged from the REPL, a test, or the GUI.

```julia
# inside _run_task — always use callbacks, never ws_* directly
on_progress(n, total)
on_log("message")
```

**Implement `_run_task`, not `run_task`.** The scheduler's public `run_task` validates params, acquires a pool slot, writes the log file, then delegates to `_run_task`. Overriding `run_task` bypasses all of that.

**`on_process(proc)` is required** when launching a subprocess. It registers the process handle with the cancellation system. Omitting it means `task:cancel` cannot kill the subprocess.

**`proc.exitcode == 0` doesn't mean success on cancel.** libuv sets `exitcode = 0` for signal-killed processes. Always check both:
```julia
ok = proc.exitcode == 0 && proc.termsignal == 0
```

**`resource_pool` is required in every task JSON.** One pool per real bottleneck resource; the name says *what* it rations. Standard values: `"cpu"` (limit 20, general compute — the default), `"gpu"` (1, cellpose family), `"io"` (8, local disk — import/convert/crop), `"network"` (1, remote/SMB — reserved for HPC, unused today). Defined in `app/config.toml [pools]`; limits are adjustable live in the `PoolThrottle` popover (Task Manager, any module page, the Chain page) — throttle e.g. `io` to 1 for slow-share imports. The `tasksLimit` field and old single concurrent-task slider have been removed — use pools instead.

**QC is required for every result-producing task.** After the work succeeds, bank an objective
`metrics` count + a `warn` finding for the unambiguous bad case via `write_qc` (`app/src/qc.jl`), and
add cohort-comparable metrics to `COHORT_METRICS` (`app/src/qc_cohort.jl`). Advisory only (never
`error`, never gates). Keep the finding logic in a pure, unit-tested helper. The only exemption is a
task with genuinely no objective signal, stated as an explicit comment. See `docs/MODULES.md` → *QC —
REQUIRED for every new task*.

---


## Versioned variable pattern (ccid.json)

Two orthogonal axes on the same shape:

```json
{ "default": { "v1": "ccidImage.ome.zarr", "_latest": "v1" }, "_active": "default" }
```

- **Outer axis — value_name variant** (`default`, `dtype`, `cropped`, …). Read: `versioned_get_field(raw, "filepath", value_name)`; write: `versioned_set_field!(raw, "filepath", value, value_name)`.
- **Inner axis — version per value_name** (`v1`, `v2`, …). Read: `version_get(inner, version)` or the composer `versioned_get_field_at(raw, "filepath", value_name; version=nothing)`. Write: `version_write!(inner, value)` — mints the next `vN` (via `version_next`), refuses to overwrite (D6), updates `_latest`. `version_set!` remains as the unguarded escape hatch for the P4a legacy migrator. First-ever v2 write on a legacy value_name goes through `versioned_upgrade_entry!(outer, value_name)` first — that wraps the bare scalar/vector as `v1` in place and returns the versioned entry ready for `version_write!`. For a struct-field accessor that already has the resolved inner value, use `unversion_value(value, version=nothing)`.
- **Reprocessing writes route through `keep_previous_version()`**: default off → overwrite the current version (unchanged behaviour). Toggled on (Settings → Storage, `[zarr].keepPreviousVersion`) → the writer mints a new `vN` sibling instead of overwriting. Autonomous execution forces the toggle on programmatically — that's the mechanically-can't-overwrite invariant the whole primitive exists for. Pilot writer: ingest (`app/src/tasks/importImages/omezarr/task.jl` → `_plan_import_target` → `_merge_zarr_meta_into_ccid!(as_new_version=true)`). Bucket A helpers (`img_filepath` etc) already unwrap versioned entries; Bucket B (QC / corrections / gating / trackProps / branchProps / clustfeatures) stays single-file and follows `_latest` — safety comes from chain/task pinning (P3), not per-artifact files. Full design: `docs/audit/vn-versioning-p4-design.md`.
- **Chain pinning (P3)**: every task reader uses `versioned_get_field_at(raw, "filepath", value_name; version = get(params, "version", nothing))` — unset (the default) follows `_latest`, a concrete `vN` in `params["version"]` pins the read. Enforced by the `P3 pinning ratchet` testset. A chain node with `params["version"] = "v2"` reads v2 no matter what `_latest` currently points at; unset chains keep the "auto-follow latest" semantics existing users rely on. Frontend picker + freeze-all button are the follow-up.
- A bare scalar / vector at the value_name key is treated as **implicit `v1`** — old projects and every legacy reader keep working.
- Companion: `resolve_version(img, field, value_name=nothing)::String` for callers that need the version string (chain-planner pinning, logging).

Full design: `docs/todo/VN_VERSIONING_PLAN.md`.

**JSON3 gotcha — Symbol keys**: JSON3 yields Symbol keys (`:default`, `:_active`). Always convert when building a `Dict`:
```julia
Dict{String,Any}(String(k) => v for (k, v) in obj
                 if string(k) != VERSIONED_ACTIVE_KEY)
```
Without this, `get(dict, "default", nothing)` returns `nothing` silently even when the key exists.

**JSON3 gotcha — `isa Dict` vs `isa AbstractDict`**: `JSON3.Object <: AbstractDict` but `JSON3.Object isa Dict` is `false`. Any type guard that checks `isa Dict` will fail for values read from JSON3. Use `isa AbstractDict` everywhere. The versioned-field helpers (`versioned_set_field!`, `_to_str_dict`) already handle this — don't add new `isa Dict` checks.

---

