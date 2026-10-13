# HPC dispatch — run Python-backed tasks on a cluster over SSH

**Status:** building. P1 merged (#1632). P2 built (#1637; see *P2 as built*). P3 built (branch
`feat/1619-hpc-p3`; see *P3 as built*). P4–P6 open. Un-parked by issue #1619. Renamed from `HPC_NEXTFLOW_PLAN.md` when Nextflow was dropped (Decision 2).

## Goal

Whole-slide segmentation (Cellpose-SAM at ~45k × 28k px) is impractical on a laptop. A user ran it on
their institute's SLURM cluster by hand and copied the labels back. The old R version could dispatch
any module to HPC. Feijoa cannot dispatch anything.

The goal is to send the **Python half of a task** to a batch cluster (SLURM first, PBS next) from the
workstation Cecelia runs on. Everything else stays local: the Julia handler, the state, the QC, the
run log, the Task Manager.

## Decisions

All dated 2026-10-12 unless marked otherwise. They are numbered so code can cite them
(`docs/todo/HPC_PLAN.md` → Decision N).

**D1 — Don't replace the scheduler or the chain runner** (kept from 2026-09-17). Cecelia's scheduler is
an in-process runtime: shared resource pools, a per-image run log, QC banking under `value_name`,
WS feedback, and cancel. A batch DAG executor fits none of these. HPC is a **transport** for one
task's compute.

**D2 — No Nextflow.** This reverses the 2026-09-17 premise, and the reasons are in the design itself:
- **Staging** is our own rsync mirror (D5). Nextflow's staging only moves files inside the cluster.
- **Fan-out** is Cecelia's scheduler: one `run_py` call is one job.
- **Resume** comes from the mirror plus Cecelia's rerun semantics.
- **Containers** need no Nextflow: Apptainer is `apptainer exec` inside the job script.

What it would cost:
- Java and Nextflow installed on the cluster.
- A driver job, which doubles the queue wait. On sites that forbid `sbatch` from compute nodes it
  does not work at all.
- A `work/` dir that duplicates the mirror.
- One more layer to debug.

The old R version used plain `sbatch` (`old-R-shiny-version/inst/scripts/hpc/slurm/watchJobCompletion.sh`).
Where Nextflow *would* earn its place is listed under *Non-goals*.

**D3 — Generic by construction: dispatch at `run_py`.**
- `run_py(...; reads = [...], writes = [...])` declares project-relative I/O.
- A call that declares them is HPC-eligible. A call without them runs locally, as today.
- Porting a task means adding its declaration. The first port is the segmenters (see P3 for the list).
- The Julia handler and every state mutation stay local: `commit_state!`, `ccid.json`, QC, the run
  log. A remotely edited `ccid.json` is never merged back.
- Pure-Julia tasks (no `run_py`) are out of scope until one needs it.

**D4 — Topology: Cecelia stays on the workstation, and everything goes over SSH with the user's key.**
- rsync runs over SSH.
- Each job is one `sbatch`/`qsub` submission of a small script. Nothing long-lived runs on the login
  node.
- Status and logs are read over SSH.
- No passwords are stored. The user's key file is referenced by path. A passphrase-protected key must
  be loaded in the user's `ssh-agent`.

**D5 — Staging is a persistent per-project mirror on the cluster, synced rsync-style on each run.**
- Before a job, `reads` are synced, so unchanged zarr chunk files are skipped. A rerun uploads only
  the params.
- After the job, only `writes` come back.
- The mirror is a cache. The local project is authoritative. A scratch purge, whole or partial, is
  detected and resynced.
- Under shared storage the sync is a no-op.

**D6 — Pool: rename the unused `network` pool to `hpc`** (`docs/SCHEDULER.md` → *Resource pools*).
- Its limit is the number of concurrently dispatched jobs.
- A dispatched task takes no local `cpu`/`gpu` slot.
- Sync counts against `io`.

**D7 — A scheduler-backend tier between Cecelia and the batch system.** The interface is small, and
every call runs over the D4 SSH transport:

| Call | Does |
|---|---|
| `render_script(b, job)` | Turns the resource model (D8) into the backend's directives plus a common body |
| `submit(b, conn, script)` | Returns a job id |
| `status(b, conn, ids)` | Batched: an `@enum RemoteJobState` (queued / running / gone), modelled on `TaskStatus` |
| `cancel(b, conn, id)` | Cancels the job |
| `exit_reason(b, conn, id)` | Optional: why a job vanished (walltime, OOM, node failure) |

- **SLURM is the first backend:** `sbatch` / `squeue` / `sacct` / `scancel`.
- **PBS is designed-for and named as the next one:** `qsub` / `qstat` / `qstat -x` / `qdel`.
- **Backend-independent parts:**
  - the SSH/rsync mirror;
  - the log fetch (the script points stdout at a known file, which is tailed);
  - the terminal status: the common job body writes `<job>.exit` with the Python exit code, as the
    old R `.success`/`.fail` did.
- The backend only has to tell queued from running, and explain a job that vanished without writing
  `.exit`.
- The backend type is picked in Settings.

**D8 — One Cecelia-side job-resource model, mapped per backend.**

| Cecelia field | SLURM | PBS (Pro/OpenPBS) |
|---|---|---|
| `queue` (per class: cpu/gpu) | `--partition=` | `-q` |
| `cpus` | `--cpus-per-task=` | `-l select=1:ncpus=` |
| `mem_gb` | `--mem=NG` | `:mem=NGB` |
| `gpus` | `--gres=gpu:N` (site may prefer `--gpus=N`: configurable) | `:ngpus=N` |
| `walltime` | `--time=HH:MM:SS` | `-l walltime=HH:MM:SS` |
| `account` (per class) | `--account=` | `-A` (or `-P` at some sites: configurable) |
| `qos` (per class) | `--qos=` | none, omitted |
| `email` + events {begin,end,fail} | `--mail-user=` `--mail-type=` | `-M` `-m abe` |
| `name`, `log` | `--job-name=` `--output=` | `-N` `-o` `-j oe` |
| `extra_directives` (per class) | raw lines | raw lines |
| `preamble` (per class) | `module load …` / `apptainer` setup, verbatim | same |

The class (cpu/gpu) picks the queue, account and QoS. Login-node specifics, Apptainer, modules and
partition flags are **configuration, not assumptions** (`extra_directives`, `preamble`).

**D9 — Settings live in the app's Settings page (GUI), not in a config file only.** They are modelled
on the old R project-settings HPC box (`old-R-shiny-version/inst/app/modules/ui/projectSettingsUI.R:28-140`,
`inst/config.yml` `hpc:`). That box is a reference, not gospel. *Proposed* split, for review:

- **Global** (`custom.toml` `[hpc]`): backend type, address, port, remote base dir, queues, QoS,
  account, email, extra directives, preamble, runtime (Q1).
  - Why: the site is a property of the machine's network, not of the data.
  - The old R version stored these per project as a side effect of Shiny bookmarking, and lost CPU QoS
    to a bug (no observer). That is not a design to copy.
- **Per profile** (`config/profile_settings.jl`): username and SSH key path.
  - Why: they are a person's identity. A shared workstation with two profiles must not submit under
    one login.
- **Per project** (`<project>/settings/hpc.json`): an optional account override only.
  - Why: billing follows the grant, and that is per project.
  - It is project-relative and holds no uid (`docs/OBJECTMODEL.md` → *Project identity*).
- The concurrent-job limit is the `hpc` pool limit, set in `PoolThrottle`, not in Settings.

**D10 — Testing.**
- **CI:** a single-node SLURM container reachable over SSH. It covers:
  - real submission, queueing, exit codes and cancel;
  - **rerun-skips-upload**;
  - **purge-triggers-resync**.
- **Acceptance:** on a real site, through the reporter, at the next release.

## Code reality — what the design must absorb

Traced on `main` at the time of writing. These are the costs behind the decisions, not changes to them.
The ones that bend a decision are also flagged in *Open questions*.

1. **The pool slot is taken before the handler runs, so dispatch is inside a slot it did not choose.**
   - `run_task` → `_pool(name)` acquires the slot first. `run_py` is reached later, inside the handler.
   - D6 ("no local cpu/gpu slot") therefore means **the target must be decided at submit time**: a
     `target = "hpc"` on the run request, which routes to the `hpc` pool and tells `run_py` to dispatch
     (task-local context, so the handlers need no new argument).
   - The job record already has `target` (`app/src/runner/execute.jl:44`, `TASK_RUNNER_PLAN.md` D9). It is
     a free `String` today; with a second value it becomes an `@enum` (local / hpc) with typed
     (de)serialisation at the record boundary, as `TaskStatus` does (`task_record.jl:11`).
   - Eligibility has to be known **before** the run, so the env chip can offer HPC. A `run_py` kwarg
     cannot be read statically, so the spec needs a marker (Q2).
2. **Params carry absolute local paths, and none are project-relative.**
   - All 38 `run_py`-calling tasks build `imPath`, `taskDir` and friends from `img._dir`.
   - `run_py` must rewrite every string in the params under a known local root to its mirror path.
   - Any **other** absolute path while `target = "hpc"` is a refusal that names the path. That is the
     guard for an undeclared read.
3. **Segmenters read outside the project.**
   - Custom cellpose and coastal models are absolute paths under `<config_dir>/models/` (`cellpose.jl:89-121`,
     `coastal.jl:102-119`).
   - Built-in cellpose weights download into `~/.cellpose/models`. Compute nodes often have no internet.
   - "Project-relative" therefore needs a second root: **assets** (`<config_dir>/models` →
     `<remote_base>/assets/models`), plus `CELLPOSE_LOCAL_MODELS_PATH` set on the remote.
4. **An undeclared write lands next to the image.** `norm_cache` writes `<image>.normstats` into
   `{proj}/0/{uid}/`. It is a cache, so losing it is harmless, but it must be declared or exempted. If
   it is not, the undeclared-change check below would cry wolf.
5. **Labels are written staged then renamed** (`zarr_utils.py:779-803`). The pull-back must keep that
   atomicity: rsync into a local staging path, then rename, and only after the job succeeded.
6. **The task-record process handle is `Base.Process`** (`task_record.jl:50`, `_kill_proc_tree`, `run_py`'s
   `wait`/`exitcode`). Cancel and wait need a small handle abstraction: a local process or a remote job.
7. **The remote Python environment must be computed for the allocation.**
   - `_py_task_env` derives `CECELIA_USABLE_CPUS` and `CECELIA_TASK_WORKERS` from the local machine.
   - `measure_labels.jl:70` sizes `nThreads` from local `pool_status()`.
   - On the cluster both must come from the job's `cpus` (`SLURM_CPUS_PER_TASK` / `NCPUS`).
8. **The composite runs two `run_py` calls in one slot.**
   - The segmentation page runs `segment.cellposeMeasure` (`cellpose_measure.json`): segment, then
     measure, both inside one `gpu` slot.
   - If only segment is declared, measure runs locally while holding the `hpc` slot.
   - Measure is the step that OOMed on whole slides (`MEASURE_TILING_PLAN.md`), so **`measureLabels`
     is ported with the segmenters.**
9. **The language-boundary version applies remotely too.** The remote runner checks `CECELIA_PY_CONTRACT`
   (`docs/ARCHITECTURE.md` → *Every language boundary carries a version*). A stale remote install is
   refused loudly. It does not misread params.
10. **A production app has no "quit and keep processing"** (`docs/RUNNER.md` → *Why dev-only*).
    - A cluster job outlives an app quit. Its local handler does not.
    - See Q6 for reattachment.
11. **`"env": ["local"]` in 38 task specs** is an R-era field (R had `"hpc"`). Nothing in Julia reads it,
    but the frontend renders it as the env chip under the function name (`TaskRunner.vue:758-763`).
    - **Decided 2026-10-12: `env` stays the eligibility marker, and the chip becomes the target switch.**
      In the R version, switching that env to `hpc` ran the function on the cluster with no other step;
      that is the UX to keep. A task listing `"hpc"` in `env` is eligible; the chip toggles
      local ⇄ hpc and sets `target` on `task:run`. No second eligibility marker.

## Architecture

```
Run (target=hpc) ─► hpc pool slot ─► handler (local) ─► run_py(reads, writes)
                                                          │ target=hpc
                                                          ├─ io slot: sync code+env check, rsync reads ─► mirror
                                                          ├─ rewrite params paths → mirror/assets roots
                                                          ├─ backend.render_script + submit ─► job id (persisted)
                                                          ├─ poll: backend.status (batched) + tail job log ─► on_log/on_progress
                                                          ├─ read <job>.exit ─► exit code
                                                          └─ io slot: rsync writes back → local staging → rename
                                               handler continues locally: commit_state!, QC, run log
```

- **Remote layout:** `<remote_base>/projects/<project dirname>/…` mirrors the local tree, so path
  rewriting is a prefix swap. Next to it:
  - `<remote_base>/assets/` (models);
  - `<remote_base>/code/<hash>/` (the `python/` + `app/src/tasks/**.py` tree, see Q1);
  - `<remote_base>/env/<lockhash>/` (the Python env);
  - `<remote_base>/jobs/<task id>/` (script, params, log, `.exit`).
- **Purge detection is mostly free.** rsync compares every declared file, so a file that age-based
  purging deleted is re-uploaded on the next sync. A `.cecelia-mirror` marker (install id + local
  project path) adds two checks:
  - **ownership:** two workstations with the same project dirname refuse instead of overwriting each
    other;
  - **a whole-mirror purge** is named in the log rather than shown as a silent full upload.
- **Undeclared-change check:** after a successful job, an rsync `--dry-run --itemize` of the image's
  mirror dir lists changes outside `writes`.
  - Tests assert there are none.
  - Production logs a warning naming them. Those files are lost, by design (D3).
- **Polling:**
  - One SSH call per poll interval (default 15 s) serves every job in flight. It is batched in
    `status(ids)` and multiplexed with `ControlMaster` where available (not Windows OpenSSH: see Q8).
  - The log tail feeds `on_log`/`on_progress` unchanged, because `[PROGRESS] n/t` lines are just log
    lines.
- **Cancel:** `task:cancel` → handle `kill` → `backend.cancel` (`scancel`/`qdel`). Partial writes are
  never pulled back. If SSH is down, the task is marked cancelled locally and the id is queued for a
  retried cancel on the next connection.
- **Lock order:** a job holds `hpc` and briefly takes `io`. `io` jobs never take `hpc`, so there is no
  cycle. The inline `io` claim extends the existing `_acquire_slot!` / `_release_slot!`
  (`app/src/tasks/scheduler/pools.jl`) — today only the pool dispatcher calls them; no third primitive.

## Open questions — each with a **recommendation**, not a decision

**Q1 — How does the Python env exist on the cluster?**
- Options:
  - (a) pixi install on the remote from our synced `pixi.toml` + `pixi.lock`;
  - (b) an Apptainer image built in CI;
  - (c) a site-shared install, as the old R version kept under `hpc.dirs.cecelia`.
- **Recommendation: (a), with the code synced separately.**
  - rsync the code tree (`python/` + the task runners under `app/src/tasks/`) on each run. It is
    cheap, skipped when unchanged, and makes a dev worktree's code what runs.
  - Rebuild the env only when the `pixi.lock` hash changes. That rebuild is an in-app "Install on
    cluster" action submitted **as a job**: the old R version learned that login nodes kill long conda
    installs (`updateUserLibraries.sh`).
  - (b) Apptainer becomes a `runtime` setting later, for sites that require it. A CUDA torch image is
    several GB per release, so it should not be the default.
  - (c) needs a site admin, and that is not ours to require.

**Q2 — How is the GPU vs CPU class picked, and how are resources sized?**
- **Recommendation: the class defaults from the spec's `resource_pool`** (`gpu` → GPU class, else CPU).
- The spec gains an optional `"hpc": {"cpus", "mem_gb", "walltime", "gpus"}` resource-defaults block.
  Eligibility stays on `"env"` (code reality 11); a spec listing `"hpc"` in `env` without the block
  falls back to class defaults.
- When the env chip is set to `hpc`, the task form shows those values, editable per run. That is the old R "Use GPU"
  checkbox plus memory and walltime fields, but defaulted per task instead of 20 GB / 1 h for
  everything.

**Q3 — How is the remote mirror keyed?**
- **Recommendation:** by project **dirname** under `<remote_base>/projects/`, matching the identity
  invariant (`docs/OBJECTMODEL.md`), guarded by the `.cecelia-mirror` marker.
- A re-identified project gets a fresh mirror, i.e. one full upload. That is acceptable.
- The old R version keyed by uid + version under a per-user dir. Feijoa has no project versions, and a
  per-user dir is already implied by `remote_base`.

**Q4 — What does the Task Manager show for a dispatched job?**
- **Recommendation:** keep the five `TaskStatus` states. Local `queued` means waiting for an `hpc` slot.
- While `running`, show a sub-status: *syncing up* → *queued on cluster (job 123, gpu-a100)* →
  *running on cluster* → *syncing back*.
- Show `target` as the badge the runner plan reserved for it (`TASK_RUNNER_PLAN.md` D9).
- Elapsed time counts from the slot, as now. Cluster queue time is shown, not hidden: a long queue must
  not look like a hang (`docs/SCHEDULER.md` → *Queue visibility*).

**Q5 — Cancel semantics.**
- **Recommendation:** as in *Architecture*. Cancel is final, partial writes are discarded, and a
  `scancel` is retried if SSH is down.
- A walltime or OOM kill (no `.exit`) is `failed`, with `exit_reason` in the log.

**Q6 — The app quits or crashes while a job runs.**
- **Recommendation: reattach by rerun** (P4).
  - The job id, params hash and remote paths are persisted in `tasks/<stem>.hpc.json`.
  - Re-running the same task with the same params hash finds the finished remote result (or the
    still-running job) and attaches to it instead of submitting.
- A background reattach that needs no rerun waits for evidence that it is needed. It is the same
  trigger as "quit and keep processing" (`docs/RUNNER.md`).

**Q7 — Should the interface have a fake/local backend?**
- **Recommendation: yes.** A `LocalShellBackend` runs the rendered script with `bash` as a background
  process and reports status by pid.
- It runs the dispatch, mirror and cancel tests on the Linux and macOS CI legs, with no container.
  Windows has no `bash`, and dispatch from Windows is deferred anyway (Q8). The Windows leg runs the
  backend-independent unit tests: params rewrite, the refusal guard, and the golden renders.
- Rendering for SLURM and PBS gets golden-file tests, so PBS directives are pinned before an OpenPBS
  container exists.
- Possible later use, not decided: a no-scheduler backend for "SSH to the lab's GPU box".

**Q8 — Windows and macOS transport.**
- Windows ships OpenSSH but no rsync, and its OpenSSH has no `ControlMaster`.
- macOS now ships `openrsync`.
- **Recommendation:** rsync as a pixi dependency on unix targets, so macOS gets one rsync on every
  machine.
- For Windows, either `m2-rsync` or a fallback of tar-over-ssh driven by our own size+mtime manifest.
  The choice waits until a Windows user asks. P1–P3 run on Linux and macOS.
- This needs the CI `macos-latest` leg, because there is no Mac to test on.

**Q9 — Sites that need MFA on SSH.** Many clusters require OTP at login, which a key alone cannot pass.
- **Recommendation:** out of scope for P1–P4. Document it.
- Later option: the user opens one `ssh -M` master session and Cecelia reuses the socket.

**Q10 — Host-key trust.** The old R version used `StrictHostKeyChecking=no`.
- **Recommendation:** `accept-new` on the Settings "Test connection" only, showing the fingerprint.
- Every other call uses `BatchMode=yes` and the strict default.

**Q11 — Is the first PR one PR?** Issue #1619 bundles settings, dispatch, sync and the segmenters into
one PR.
- **Recommendation:** split it as P1 + P3 below (P2 is small).
- About 16 and about 18 touchpoints is two reviewable diffs. 34 in one is not.

## Phases

The counts are files touched, new and existing, including tests and docs.

**P0 — this plan** (2026-10-12). Rename, design, index row. 2 files.

**P1 — Connection + settings, no dispatch.**
- **Delivers:**
  - `[hpc]` config and the Settings → HPC section (global site, per-profile identity, per-project
    account override);
  - the key picker via `FileBrowser` in `file` mode (the pattern from `ParamRenderer.vue`);
  - "Test connection": SSH in batch mode, remote base dir writable, backend binary present
    (`sbatch --version` / `qstat --version`);
  - the `network` → `hpc` pool rename, with a `custom.toml` migration for a stored `network` limit.
- **Tested by:** a settings round-trip unit test, and the connection test against the SLURM+sshd CI
  container (new workflow job, `ubuntu-latest`).
- **Touchpoints ≈ 16:**
  - Julia: `app/config.toml`, `config/hpc.jl` (new), `hpc/transport.jl` (new; spawns only),
    `config/binaries.jl` (`ssh_bin`/`rsync_bin` resolvers beside `bioformats2raw_bin`, not re-derived in
    the transport), `config/profile_settings.jl`,
    `tasks/scheduler/pools.jl`;
  - API: `api/src/routes.jl`, `api/src/server.jl`;
  - frontend: `SettingsModule.vue`, `HpcSettings.vue` (new), `PoolThrottle.vue`;
  - CI and tests: the CI container (Dockerfile + workflow, 2 files), `app/test/suite/hpc.jl` (new);
  - docs: `docs/SCHEDULER.md` + `docs/MODULES.md` pool tables, `docs/inventory` lines.

**P1 as built** (deviations from the list above):
- Routes are in a new `api/src/hpc_api.jl`, not `routes.jl` (1 000+ lines; the per-area `*_api.jl` files
  are the current pattern).
- Host keys go to Cecelia's own `<config_dir>/hpc/known_hosts`, listed before `~/.ssh/known_hosts` in
  `UserKnownHostsFile`: an existing trust is honoured, and the test never writes the user's file.
- The probe runs as `sh -s` on stdin (works under a csh login shell) and sources `/etc/profile` once if
  the scheduler binary is not on `PATH` (modules). P3's job submission must use the same environment.
- Site fields cover D8's per-class queue/account/QoS/directives/preamble plus email; the GPU-flag style
  (`--gres` vs `--gpus`), mail events and `runtime` (Q1) wait for the phases that use them.
- Also touched: `pixi.toml` (`test-hpc`), `app/test/hpc_e2e.jl` (the standalone runner the workflow
  calls), `FileBrowser.vue` (`startPath`, so the key picker opens in `~/.ssh`), the ssh/rsync spawn
  ratchet, and the docs/comments that named the `network` pool.
- The CI container lives at `.github/hpc-test/Dockerfile`, workflow `verify-hpc.yml` (path-filtered).

**P2 — Remote env + code sync.**
- **Delivers:**
  - the "Install on cluster" action, submitted as a job: pixi env keyed by the lock hash;
  - the code-tree rsync;
  - a version check (`PY_CONTRACT_VERSION`, lock hash) shown on the Settings HPC section.
- **Tested by:** CI installs the env into the container, and a stale contract is refused with a named
  cause.
- **Touchpoints ≈ 6:** `hpc/remote_env.jl` (new), the job body template (new), `HpcSettings.vue`,
  `routes.jl`, `pixi.toml` (rsync dep), `app/test/suite/hpc.jl`.

**P2 as built** (deviations from the list above):
- **The backend tier moved here from P3.** The install is the first cluster job, so it needs submit,
  poll and cancel: `hpc/backend.jl` (`HpcJobSpec`, `hpc_poll` = one remote call for every job in flight,
  `hpc_watch_job`), `backends/slurm.jl`, `backends/local.jl` (Q7). P3 keeps dispatch, the mirror and the
  handle abstraction.
- **A connection layer under everything** (`hpc/transport.jl`): `HpcConn` with an ssh kind and a local
  kind (this machine as the "remote"), so the sync, the install and submit/cancel run end-to-end in the
  package tests with no sshd. Calls share one ssh master, opened on its own (`-M -f`, output discarded):
  a `ControlPersist` master started by a call whose output is captured keeps that pipe open until it exits.
- **The version check is the env's pixi.lock hash, not `PY_CONTRACT_VERSION`.** The code runs from a
  content-hashed tree (`code/<hash>/`) synced before each run, so runner and params are always the same
  revision and the contract cannot drift remotely. The env can: `hpc_require_env` refuses a missing one
  by name ("Install it in Settings → HPC"), and Settings shows the lock hash it is ready for.
- **pixi is fetched by the install job** from its GitHub release (`HPC_PIXI_VERSION`, the version CI
  pins), and its cache lives under the remote folder (cluster home quotas are small). So the compute
  node needs outbound HTTPS. A site without it is the Q1 (b)/(c) case, still open.
- **Linux x86_64 only on the cluster** (the platform pixi.lock has). The job refuses anything else by name.
- **Install job size:** 2 CPUs, 8 GB, 2 h (the CI node has 4 cores and 16 GB). Unmeasured on a real site.
- **An install already on the cluster is attached to** (`env/<lock>/.install-job`), not repeated: the
  first piece of Q6.
- **rsync is a conda-forge dependency on unix** (`[target.unix.dependencies]`), as Q8 recommended.
- **Not here:** built-in cellpose weights on the cluster. They belong with the assets root (P3).
- Routes `GET /api/hpc/env`, `POST /api/hpc/env/install` (`api/src/hpc_api.jl`); the job is
  `hpc-env-install` on the task rail.

**P3 — Dispatch + mirror + SLURM backend + the first ports.**
- **Delivers:**
  - `run_py(...; reads, writes)` with params path rewrite and its refusal guard;
  - the mirror sync, including assets and the staged pull-back;
  - the backend interface plus `SlurmBackend` and `LocalShellBackend`;
  - the handle abstraction for wait and cancel;
  - `target` on `task:run` and on the job record;
  - the env chip as the local ⇄ hpc target switch, plus the resource fields shown when it is `hpc`;
  - the spec `"hpc"` resource block, and `"hpc"` added to `env` on the ported tasks;
  - declarations on `segment.cellpose`, `segment.coastal` and `segment.measureLabels`, so the
    composites dispatch whole.
- **Tested by:**
  - `LocalShellBackend` tests on Linux/macOS (undeclared-change check, cancel); rewrite + guard on all OSes;
  - SLURM golden render;
  - in the SLURM container: a tiny cellpose+measure end-to-end, **rerun-skips-upload** (asserted from
    rsync's transferred-bytes stats: params only), **purge-triggers-resync** (delete mirror chunks,
    rerun, outputs identical), `scancel`, and a walltime kill reported as `failed` with its reason.
- **Touchpoints ≈ 18:**
  - core: `py_runner.jl`, `hpc/mirror.jl`, `hpc/backend.jl`, `hpc/backends/slurm.jl`,
    `hpc/backends/local.jl`, `hpc/job.jl` (all new except `py_runner.jl`);
  - scheduler: `task_record.jl`, `tasks/scheduler/jobs.jl`, `runner/execute.jl`, `api/src/sockets.jl`,
    `tasks/task/spec.jl`;
  - frontend: `TaskRunner.vue`;
  - the three task `.jl` + `.json` pairs (counted as 3);
  - tests: `app/test/suite/hpc.jl`;
  - docs: `docs/MODULES.md` ("porting a task to HPC").

**P3 as built** (deviations from the list above):
- **No handle type.** A dispatched `run_py` has no local process, so `TaskRecord.proc` stays a
  `Base.Process` and is simply never set. Cancel is the record's status, which `cancel_task!` already
  sets: the job's watch loop polls `is_cancelled(task_id)` and runs `scancel` (code reality 6).
- **The dispatch reaches `run_py` through a scoped value** (`HPC_DISPATCH`), not task-local storage: a
  composite's steps and any task a handler spawns inherit it. Set by `_with_run_target` in the job
  executor, which also fails the task before its handler starts when the cluster is unconfigured or
  unreachable.
- **`target` is a `RunTarget` enum** on `TaskRequest` and `TaskRecord` (wire `"local"`/`"hpc"`), and
  `task:run` gains `hpc` (`{cpus, memGb, gpus, walltimeMin}` overrides). `list_tasks` reports `target`.
  Chains stay local.
- **A composite runs every step with its own class and `"hpc"` block.** `segment.cellposeMeasure` is
  `gpu`, so its measure step also runs on the GPU class. Simpler than per-step classes; revisit if a
  site's GPU queue is the bottleneck.
- **Assets are `<config_dir>/models` only.** Built-in cellpose weights are fetched by cellpose on the
  compute node into `<root>/assets/cellpose` (`CELLPOSE_LOCAL_MODELS_PATH`), like pixi in P2.
- **The params travel inside the job script** (a quoted heredoc), so submitting is one call.
- **The mirror marker holds `hostname:local project path`.** A mismatch refuses, naming the owner and
  the folder to remove.
- **The undeclared-change check** ignores `tasks/`, `logs/` and `*.normstats` (a cache, code reality 4).
- **Ported:** `segment.cellpose`, `segment.coastal`, `segment.measureLabels` and both composites. The
  `"hpc"` blocks (8 CPUs, 64 GB, 4–8 h) are unmeasured guesses for whole-slide images.
- **Tests:** a real `segment.measureLabels` runs end to end with the local backend. It is checked for
  the same table as a local run, a rerun that sends 0 B, a purged mirror that resyncs, an undeclared
  write that is named, a foreign mirror that is refused, and a cancel that reaches the job. CI runs the
  same task against the SLURM container. A **cellpose** run in the container is not here: its 1.2 GB
  weights download would dominate the job, so it is left to the real-site acceptance (P6).
- **Frontend:** `utils/hpcRun.ts` + the env chip as a `ChipSelect` (local/hpc) with the resource fields
  under it, remembered per function. The Task Manager sub-status is P4.

**P4 — Task Manager surface + cancel retry + reattach-by-rerun.**
- **Delivers:** the cluster sub-status (Q4), a retried `scancel` (Q5), and `tasks/<stem>.hpc.json`
  with reattach by rerun (Q6).
- **Tested by:** in the container, kill the backend mid-job, rerun, and assert it attaches with no
  second submission.
- **Touchpoints ≈ 7:** `hpc/job.jl`, `task_record.jl`, `stores/tasks.ts`, `TaskList.vue`,
  `TasksModule.vue`, the test, `docs/SCHEDULER.md`.

**P5 — PBS backend.**
- **Delivers:** `PbsBackend` (`qsub`/`qstat -x`/`qdel`, select-statement resources).
- **Tested by:** golden render (already pinned in P3), then an OpenPBS container job in CI running the
  same end-to-end suite, parameterised over the backend.
- **Touchpoints ≈ 4:** `hpc/backends/pbs.jl`, the CI container + workflow, the test parameterisation.

**P6 — Acceptance + promotion.**
- **Delivers:** a run on the reporter's real site at the next release; durable docs in `docs/HPC.md`,
  with this plan kept as history; more ports (ridges, branching, tracking …), one declaration each.
- **Tested by:** the reporter's run, plus the per-port end-to-end in the container.
- **Touchpoints:** ≈ 3, plus 1–2 per ported task.

## Non-goals

- **Running a whole chain as one pipeline on the cluster.** That is where Nextflow would earn its place
  (D2). Chains stay in Cecelia, and each node dispatches on its own.
- **A headless pipeline other labs run without Cecelia.** Results computed elsewhere come in through
  label import (#1618).
- Pure-Julia tasks on the cluster (D3).
- Storing passwords or passphrases (D4).
- Replacing the local task runner or the scheduler (D1).

## References

- Issue #1619 (this ask); #1618 (label import, independent).
- `app/src/py_runner.jl` (`run_py`, `_py_task_env`, `PY_CONTRACT_VERSION`).
- `docs/SCHEDULER.md` → *Resource pools*, *Queue visibility*, *pool_name override in run_task*.
- `docs/todo/TASK_RUNNER_PLAN.md` → *HPC — what this must not preclude*, D9 (`target`).
- `docs/RUNNER.md` → *Why dev-only*.
- `docs/OBJECTMODEL.md` → *Project identity*.
- Old R, as a reference for cost, not a template:
  - `R/taskLauncher.R` (sbatch templating, submit, cancel);
  - `R/sshUtils.R` (system ssh/sftp);
  - `inst/templates/hpc/slurm/*.tpl`;
  - 17 per-module `upload.R`/`retrieve.R` files, 1330 lines, full copies with no incremental sync.
