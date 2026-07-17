# User-drop-in custom modules — PLAN

**Status:** P1 + P2 + P3 BUILT. Restore the old R-version capability: a user adds a custom
**module/task function** by dropping files into their **user directory** (next to `custom.toml`) — no
package edit, no rebuild. The GUI shows it like any built-in task. Promote the durable parts into
`docs/CUSTOM_MODULES.md` (done) — this plan can be retired once merged.

**Shipped (P1 Julia drop-in + P2 Python compute):** runtime registry (`register_task!` in
`app/src/tasks/task.jl`, consulted by `_task_from_fun_name`/`_spec_path`, built-ins win on clash);
`load_custom_modules!` (`app/src/tasks/custom_modules.jl`) scans `<config_dir>/modules/sources/**/*.jl`
on server start + on demand; `api_task_definitions` merges `<config_dir>/modules/inputDefinitions/**`
(built-ins win); `run_py` runs absolute-path `modules/python/` scripts with the user dir on
`PYTHONPATH`.

**Shipped (P3 UX):** Settings → Custom modules panel (dir + per-module loaded/error list + Reload
button); a **generic page** `/custom/:category` + a dynamic **"Custom"** sidebar group for
new-category modules, driven by `categories` (with a `builtin` flag) on
`GET`/`POST /api/tasks/custom-modules[/reload]`; guide `docs/CUSTOM_MODULES.md`; two runnable examples
in `docs/examples/custom-modules/` (Julia-only `behaviour.exampleNormalise`; Julia+Python+nested-params
`customExamples.trackContext`); tests in `app/test` + `api/test`.

**Decisions that landed:** name-clash = built-ins win; hot-reload picks up NEW files, edited Julia
still needs a restart (struct redefinition), documented; a new category's generic page has no plot
canvas (results plot on the Analysis board / Explore pages once written to the h5ad).

## The gap (why this is needed)
Old R cecelia `source()`d a `modules/` folder at startup, so a dropped `.R` + `.json` was live
immediately (see the legacy docs' *Creating Custom Modules*). The Julia rewrite made the task
registry **static and compiled**: a task needs a `struct <: CciaTask`, a `_run_task` method, and a
literal entry in `_fun_name_map()` (`app/src/tasks/task_registry.jl`) + an `include` in `Cecelia.jl`.
Adding one today = editing package source + rebuild. But the **UI half is already directory-driven**
(`api_task_definitions` in `api/src/routes.jl` does `readdir(_TASK_SPECS_ROOT)`), so only the
executable/registration half needs to become dynamic.

## Locked decisions
1. **Location = the user config dir**, resolved by the existing `Cecelia.config_dir()` (prod
   `~/.cecelia`, dev `CECELIA_DEV_DIR`/`.env`). So custom modules live **outside the package, in the
   user directory, beside `custom.toml`** — one resolver, dev/prod uniform. Layout mirrors old R:
   ```
   <config_dir>/modules/
     sources/<category>/<name>.jl            # Julia: struct <: CciaTask + _run_task
     inputDefinitions/<category>/<name>.json # param/UI spec (same schema as app/src/tasks/*.json)
     python/<category>/<name>_run.py         # optional compute (run via run_py)
   ```
   `<category>.<name>` (dir + filename) forms the `fun_name`, exactly like built-ins.
2. **No new package deps.** Pure Julia `include` + a registry refactor; no plugin framework.
3. **Trust model = the user's own machine.** A custom module is arbitrary Julia with full access
   (same as R `source()`ing it). No sandbox; document it plainly. Only the local user can drop files
   into their own config dir.

## Mechanism (what changes)
1. **Runtime registry.** Replace the static `_fun_name_map()` Dict with a registry populated from
   (a) the built-ins + (b) discovered user modules. Provide a registration hook the dropped `.jl`
   calls at include time:
   ```julia
   # <config_dir>/modules/sources/behaviourAnalysis/cumulativeChange.jl
   struct CumulativeChange <: Cecelia.CciaTask end
   function Cecelia._run_task(::CumulativeChange, img, params; on_log, on_progress, on_process) … end
   Cecelia.register_task!("behaviourAnalysis.cumulativeChange", CumulativeChange();
                          spec = @__DIR__() * "/../../inputDefinitions/behaviourAnalysis/cumulativeChange.json")
   ```
   `register_task!` records the instance + spec path in the runtime registry that `_task_from_fun_name`
   and `_spec_path` consult. (Self-registration is explicit and simplest; the alternative —
   convention-based path→struct inference — is more magic and brittle. **Decision: self-register.**)
2. **Discovery/load.** A `load_custom_modules!()` that scans `<config_dir>/modules/sources/**/*.jl`
   and `Base.include(Cecelia, path)` each (which runs its `register_task!`). Called on server start
   after the package loads, and re-runnable (a "Reload custom modules" action / Settings button).
   First call of a custom task pays Julia compile latency (not in the sysimage) — acceptable.
3. **Definitions scan.** Extend `api_task_definitions` to also read
   `<config_dir>/modules/inputDefinitions/**/*.json` and merge with the built-in specs (built-ins win
   on name clash, or namespace user ones — **open decision**). The module page then renders the form
   with no extra code.
4. **Python compute.** Let a custom task resolve its runner from `<config_dir>/modules/python/…`.
   `run_py` currently resolves only under `python/cecelia/` and sets `PYTHONPATH=python/`; add an
   optional absolute-script path (and add the user modules dir to `PYTHONPATH`) so a dropped
   `<name>_run.py` can `import cecelia.*` and be launched. **Decision: support in v1** (many modules
   need compute); if too big, phase it (Julia-only custom tasks first).
5. **Validation + errors.** On load, validate each spec (same validator as built-ins), and surface
   load failures (bad Julia, missing spec, name clash) as warnings in the log / a Settings panel —
   never crash the server because a user module is broken.

## Phases
- **P1 — Julia-only drop-in:** runtime registry + `register_task!` + `load_custom_modules!` on start +
  definitions scan of the user dir. A dropped `.jl`+`.json` (no Python) runs and shows in the GUI.
- **P2 — Python compute:** `run_py` resolves user-dir scripts + `PYTHONPATH`. Custom tasks can shell
  to their own `_run.py`.
- **P3 — UX:** a "Reload custom modules" action, a Settings list of loaded/failed custom modules, and
  docs (a `docs/CUSTOM_MODULES.md` guide mirroring the old R page, Julia edition).

## Open decisions
1. Name-clash policy (user overrides built-in? refuse? auto-namespace under e.g. `custom.*`?).
2. Load timing: server start only, or also hot-reload via Revise-style file watching?
3. P2 scope: full Python compute in v1, or ship Julia-only first.

## Non-goals
- Not a sandbox / not a marketplace. Local, trusted, single-user drop-in only.
- Not changing the built-in task authoring flow (`docs/MODULES.md` stays for in-repo tasks).

## Pointers
- Registry + dispatch: `app/src/tasks/task_registry.jl` (`_fun_name_map`, `_spec_path`,
  `_task_from_fun_name`). Definitions endpoint: `api/src/routes.jl` `api_task_definitions`
  (`_TASK_SPECS_ROOT`). Config resolver: `app/src/config.jl` `config_dir()`. Python launcher:
  `app/src/py_runner.jl` `run_py`. Built-in authoring reference: `docs/MODULES.md`.
