# HPC — running a task's Python on a batch cluster

Cecelia can send the Python half of a task to a SLURM or PBS cluster over SSH, while everything else
stays on the workstation: the Julia handler, `ccid.json`, QC, the run log and the Task Manager. This
doc covers how it works, how to set it up, and how to test it on a real site. The design history and
the decisions behind it are in [`todo/HPC_PLAN.md`](todo/HPC_PLAN.md) (D1–D10).

**Status:** built and tested against a SLURM container in CI. **Not yet run on a real site**, and
Cellpose has not run on any cluster (CI skips it because of the weights download). That first real
run is what tells us what is missing. Follow *Testing on a real cluster* below.

## What runs where

```
workstation                                         cluster (over ssh, the user's key)
───────────                                         ──────────────────────────────────
handler (Julia) ─ run_py(reads, writes) ──rsync──►  <remote>/projects/<project>/  (mirror of the inputs)
                                         ──ssh───►  sbatch / qsub job: <remote>/env python + <remote>/code
             ◄── log lines, stage ────── poll ───   job.log, .exit
             ◄── declared writes ────── rsync ───   outputs → <path>.hpc-incoming → rename
handler continues: register labels, QC, run log
```

- **Only `run_py` calls leave**, and only those that declare what they read and write. A dispatched
  task that reaches an undeclared `run_py` fails with a message rather than running locally.
- **No process runs on the login node** except `ssh` commands: `sbatch`, `squeue`, `cat`, `rsync`.
  No Nextflow and no daemon (HPC_PLAN D2).
- **The workstation is authoritative.** The cluster copy is a cache: delete it and the next run
  re-uploads what it needs.

## Setup — Settings → HPC and Preferences → HPC

| Where | Fields | Stored in | Why there |
|---|---|---|---|
| **Settings → HPC** | scheduler, host, port; Cluster environment | `custom.toml` `[hpc]` | which cluster this machine reaches is not personal |
| **Preferences → HPC** | username, SSH key, remote folder, email, CPU/GPU queue · account · QoS · directives · preamble | the profile (`hpcUser`, `hpcKeyFile`, `hpc` table) | a remote folder is usually `/scratch/<user>/…`, and accounts follow a person's group |
| **Settings → HPC → This project** | account override | `<project>/settings/hpc.json` | billing follows the grant |

Preferences opens from the profile chip in the header. Test connection is on both.

- **Directives** are extra `#SBATCH`/`#PBS` lines, used verbatim. **Preamble** is shell run before
  the task, e.g. `module load cuda/12`.
- **Test connection** is the only call that may trust a new host key. It writes to Cecelia's own
  `<config_dir>/hpc/known_hosts` and shows the fingerprint. Every other call is strict and runs with
  `BatchMode`.
- **Passwords are never stored.** A key with a passphrase must be loaded in `ssh-agent`. MFA logins
  are not supported yet (HPC_PLAN Q9).
- **The number of cluster jobs in flight** is the `hpc` pool in the throttle popover, not a setting.

### Cluster environment

**Install on cluster** sends a job that:
1. fetches pixi into `<remote>/bin/`;
2. runs `pixi install --frozen` of `pixi.lock` into `<remote>/env/<lock hash>/`;
3. checks that it imports.

- The job is 2 CPUs, 8 GB and up to 2 h.
- It needs a **Linux x86_64** compute node with **outbound HTTPS**.
- It is rebuilt only when `pixi.lock` changes, i.e. a new Cecelia version.
- A dispatch refuses a missing or outdated env, naming the fix.
- The code (`python/` and the task runners) is synced on every run into a content-hashed
  `<remote>/code/<hash>/`, so the runner always matches the Julia that wrote its params.

## Running a task on the cluster

1. Pick a task whose spec lists `"hpc"` in `env`. The **env chip** under the function name becomes a
   **local / hpc** switch.
2. On **hpc**, the job size appears: CPUs, GB, GPUs, minutes. The defaults come from the spec's
   `"hpc"` block, and your changes are remembered per function.
3. The run waits in the `hpc` pool, then goes through these stages, shown on the Task Manager's pool
   tag (`hpc · queued`, with the job id in the tooltip):

| Stage | What happens |
|---|---|
| uploading | env check, code sync, the declared inputs rsynced into the mirror |
| queued / running | the scheduler's own state, polled every 15 s (one ssh call for every job in flight) |
| downloading | the declared outputs copied back through `<path>.hpc-incoming` and swapped in by rename |

- **Cancel** runs `scancel`/`qdel`. If the cluster is unreachable at that moment, the cancel is kept
  and retried every minute for a day, also across a restart (`<config_dir>/hpc/pending_cancels.json`).
- **Reattach by rerun:** if Cecelia quits or crashes mid-job, run the same task again. It finds the job
  through `<task dir>/<runner>.hpc.json` and follows it, or collects its outputs, instead of
  submitting again. "The same" means equal runner, params, code, env, resources and input files.
- **Undeclared outputs:** anything the job changed beyond its declared outputs is named in the log
  (`[WARN] The cluster job also changed …`) and not copied back.

**Tasks that can run there:**
- `segment.cellpose`
- `segment.coastal`
- `segment.measureLabels`
- `segment.cellposeMeasure` and `segment.coastalMeasure` (composites run every step with the
  composite's class and job size)

**Not yet:**
- set-scope tasks;
- custom (drop-in) tasks;
- the `cellpose-v3` env;
- chains, which always run locally;
- Windows workstations.

Built-in Cellpose weights are downloaded by Cellpose on the compute node into `<remote>/assets/cellpose`
on first use. Custom models from `<config_dir>/models` are synced to `<remote>/assets/models`.

## On the cluster

```
<remote folder>/
  bin/pixi-<version>        projects/<project dir>/   mirror; .cecelia-mirror = owner (host:path)
  env/<lock hash>/          code/<code hash>/         assets/{models,cellpose}/
  cache/pixi/               jobs/<task>-<runner>-<n>/ job.sh · job.log · .exit · params.json
```

A project folder that another workstation already mirrors (a different `.cecelia-mirror`) is refused,
naming the folder to remove. Every job's `job.sh` and `job.log` stay in `jobs/`, which is the first
place to look when something fails.

## Schedulers

One resource model, mapped per backend (HPC_PLAN D8):

| | SLURM | PBS Pro / OpenPBS |
|---|---|---|
| queue | `--partition` | `-q` |
| CPUs / memory / GPUs | `--cpus-per-task` `--mem=NG` `--gres=gpu:N` | `-l select=1:ncpus=…:mem=…gb:ngpus=…` |
| walltime | `--time` | `-l walltime` |
| account / QoS | `--account` `--qos` | `-A` / — |
| status | `squeue -u me` | `qselect -u me -s <state>` |
| why it vanished | `sacct`, `scontrol show job` | `qstat -x -f` (Exit_status, comment) |
| cancel | `scancel` | `qdel` |

- **Completion does not depend on the scheduler:** the job writes `.exit` with the runner's exit
  code. A job that disappears without one is reported with the scheduler's reason, e.g. "Walltime
  reached" or "Out of memory".
- **Torque is not supported.**
- **A site that wants `--gpus=N`** instead of `--gres` can set GPUs to 0 and add the line under GPU
  Directives.

Code: `app/src/hpc/` (`transport.jl`, `backend.jl`, `backends/{slurm,pbs,local}.jl`, `remote_env.jl`,
`dispatch.jl`, `job.jl`), `app/src/config/hpc.jl`, `api/src/hpc_api.jl`,
`frontend/src/components/HpcSettings.vue`, `frontend/src/utils/hpcRun.ts`.

## Porting a task

Porting a task means declaring its `run_py` reads and writes, and giving its spec `"hpc"` in `env`
plus a complete `"hpc"` block. The steps are in [`MODULES.md`](MODULES.md) → *Porting a task to
HPC*, and a ratchet in `app/test/suite/hpc_dispatch.jl` holds them.

## Testing

| Where | What | How |
|---|---|---|
| Every CI leg | settings, renderers, parsers; the whole path against a local "remote" (`HpcLocalConn` + `LocalShellBackend`): code sync, env install with a stub pixi, a real Measure-labels dispatch (same table as local, rerun sends 0 B, purged mirror resyncs, undeclared write named, foreign mirror refused), cancel, reattach after a crash, cancel retry; PBS through shim `qsub`/`qselect`/`qdel` | `pixi run test-pkg` / `test-hpc` |
| `verify-hpc.yml` | a single-node SLURM + sshd container: connection test, real `sbatch`/`scancel`/walltime kill, a real `pixi install` of the env, a dispatched Measure labels | path-filtered on HPC code and `pixi.lock` |
| A real site | Cellpose, GPUs, the site's partitions and modules | below |

### Testing on a real cluster

We have no cluster. This is the procedure for whoever tests on one, and what we need back.

1. **Before:** a Cecelia build that has HPC in Settings, running on Linux or macOS. SSH to the login
   node with a key (a passphrase key loaded in `ssh-agent`; no MFA). The compute nodes must be Linux
   x86_64 with HTTPS access, and the remote folder needs about 15 GB.
2. **Settings → HPC:** scheduler, host and port. Then **Preferences → HPC** (the profile chip in the header):
   username, key, a remote folder on scratch or project space (not in home), the queue/account/QoS
   you normally use, and a preamble if jobs need `module load`. Click **Test connection**: three
   green lines (host key, folder, SLURM version).
3. **Install on cluster.** Follow it in the Task Manager until "Environment ready".
4. **Cellpose segment + measure** on a small calibrated image. Switch the chip to **hpc**, lower the
   job size for a quick queue, and Run. The row goes uploading → queued → running → downloading, and
   the labels and table appear as after a local run.
5. **Then:**
   - rerun with Overwrite existing; the log should say `Inputs synced: 0 B sent`;
   - cancel one while queued; `squeue` should drop it within about 15 s;
   - quit mid-job and rerun the same; the log should say `Re-attached` or `already finished`, and
     there should be no second job;
   - run a whole slide.
6. **Send back:**
   - the Settings → HPC and Preferences → HPC screens;
   - each run's task log;
   - for a failure: `jobs/<…>/job.sh` and `job.log`, plus
     `sacct -j <id> --format=JobID,State,ExitCode,Elapsed,MaxRSS,ReqMem`;
   - for a whole slide: queue time, run time and `MaxRSS`. They replace the guessed `"hpc"` blocks
     (8 CPUs, 64 GB, 4–8 h).

## Troubleshooting

| Message | Meaning |
|---|---|
| `Host key not trusted. Run Test connection in Settings` | the host was never trusted, or `known_hosts` was cleared |
| `Host key changed. Check with your site before trusting it` | the server's key differs from the trusted one: ask the site |
| `Key rejected. A passphrase key must be loaded in ssh-agent` | wrong key, or a passphrase key that is not loaded |
| `SLURM not found on the login node` | `sbatch` is not on PATH, even after `/etc/profile`. Tell us what the site needs |
| `The cluster has no environment for this version of Cecelia` | run Install on cluster (once per version) |
| `[ERROR] … needs Linux x86_64; this node is …` | the install landed on a non-x86 node: pick another partition |
| `rsync is not installed on the cluster` | the login node has no rsync. Rare; tell us |
| `Cannot send <path> to the cluster: it is outside the project` | the task reads a file it has not declared; it needs porting work |
| `Cluster job N failed: Walltime reached` / `Out of memory` | raise Minutes / GB on the run form |
| `The cluster copy of project 'X' belongs to …` | another workstation mirrors a project with the same folder name; remove the named folder on the cluster |
