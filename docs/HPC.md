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
| **Settings → HPC** | scheduler, host, port, internet queue; Cluster environment | `custom.toml` `[hpc]` | which cluster this machine reaches, and which of its queues can download, is not personal |
| **Preferences → HPC** | username, SSH key, remote folder, email, CPU/GPU queue · account · QoS · directives · preamble, GPU CPUs per GPU | the profile (`hpcUser`, `hpcKeyFile`, `hpc` table) | a remote folder is usually `/scratch/<user>/…`, and accounts follow a person's group |
| **Settings → HPC → This project** | account override | `<project>/settings/hpc.json` | billing follows the grant |

Preferences opens from the profile chip in the header, or from **Your HPC settings** on Settings → HPC. Test connection
is on both; when it is disabled, the reason shows beside it. Each field saves on change and flashes **Saved**.

- **Directives** are extra `#SBATCH`/`#PBS` lines, used verbatim. **Preamble** is shell run before
  the task, e.g. `module load cuda/12`.
- **Accounts.** A task job bills the project's account when one is set, else the class account (CPU
  or GPU) from Preferences. The cluster environment install always uses the CPU class account: one env
  serves every project.
- **Nodes** (a compact table under the job classes) is what the scheduler says each class's queue runs
  on: one row per kind of node, with CPUs, GPUs and memory per node (`GET /api/hpc/nodes`). PBS follows a routing queue (Gadi's `gpuvolta`) to where it sends jobs
  and matches nodes by their `queue`; a queue that owns no nodes draws on the untagged pool, marked
  *(shared)* with its three commonest kinds and a "+N more kinds" row, because PBS picks among them by
  internal tags Cecelia cannot read. SLURM
  reads the partition from `sinfo`. Facts only; nothing is applied from them.
- **CPUs per GPU** (GPU class) is a site rule, set by you: blank on most sites, where a job may ask for
  any count. Where the site wants an exact count (Gadi's GPU queues refuse anything but 12 per GPU),
  set it, or click **Use N**, which offers the GPU nodes' CPUs ÷ GPUs. With it set, the run form shows
  a GPU job's CPUs as `GPUs × N` and greys the slider, and dispatch asks for exactly that
  (`hpc_apply_site_rules`), logging `[HPC] 12 CPUs: this cluster takes 12 per GPU`.
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

- The job is 1 CPU (an internet queue may allow no more), 8 GB and up to 30 min. On a SLURM site it took 1 min 16 s and 0.6 GB;
  on NCI Gadi's copyq 2 min, with PBS reporting all 4 GB then requested as used; the env
  is about 8.3 GB on disk.
- It needs a **Linux x86_64** node with **outbound HTTPS**. Where compute nodes are offline, set
  **Internet queue** (Settings → HPC) to a queue that can download, e.g. `copyq` on NCI Gadi; the
  install job goes there. Empty = the CPU queue.
- It is rebuilt only when `pixi.lock` changes, i.e. a new Cecelia version.
- A dispatch refuses a missing or outdated env, naming the fix.
- **Clean up** (the ⋯ beside Install) removes other versions' envs, old pixis, the download cache, the code
  copies and job folders over 30 days old; this version's env, the project mirrors and model files
  stay. **Delete all** (confirm) deletes everything Cecelia made in the remote folder, project mirrors
  included, but never the folder itself or files of your own in it; Install on cluster then starts
  from nothing. Both are refused while a Cecelia job is queued or running on the cluster (from any
  workstation) or a cluster run is in progress here. Each runs as a Task Manager row. Things are moved
  into `.cecelia-trash-<n>/` first, then deleted, so an interrupted clean-up never leaves a
  half-deleted env that still looks installed; the next one removes the leftover.
- The install is a Task Manager row (*Install cluster environment*) with the same stages as a run,
  and its log. The line beside **Install on cluster** follows it: *Uploading the environment files*,
  *Queued on the cluster (job N)*, *Installing on the cluster (job N)*. A page opened mid-install
  gets the row and the stage from `GET /api/hpc/env`.
- The code (`python/` and the task runners) is synced on every run into a content-hashed
  `<remote>/code/<hash>/`, so the runner always matches the Julia that wrote its params.

## Running a task on the cluster

1. Pick a task whose spec lists `"hpc"` in `env`. The **env chip** under the function name becomes a
   **local / hpc** switch.
2. On **hpc**, the job size appears as a 2×2 grid of stepped sliders, sized per image: CPUs (1–64),
   memory (4–512 GB), GPUs (0–4, GPU tasks only) and time limit (15 min–2 d). The task's default from
   the spec's `"hpc"` block is always a stop and is named in each tooltip; changes are saved on release,
   remembered per function, and an undo button puts the defaults back.
3. Under it, one line says where the job goes: `GPU job → queue gpu-a100 on <host>`, with the account
   and QoS on hover. The class is the task's pool, so a GPU task sent with 0 GPUs still uses the GPU
   queue. While the setup is incomplete, the line names the first missing field and where to set it.
   Once it is complete, the runner asks the cluster for the env (one ssh call): if this version's env is
   not installed, a **Cluster env out of date — reinstall** line appears with **Install on cluster**,
   the same job as in Settings → HPC. It is asked again when the install job ends.
4. Above that, a **memory line** says whether the memory covers the most demanding selected image
   (*Memory hint* below), with a **Use N GB** button when it does not.
5. The run waits in the `hpc` pool, then goes through these stages, shown on the Task Manager's pool
   tag (`hpc · queued`, with the job id in the tooltip):

| Stage | What happens |
|---|---|
| uploading | env check, code sync, the declared inputs rsynced into the mirror |
| queued / running | the scheduler's own state: polled every 5 s while running, every 30 s while queued (one ssh call per job). The log streams while it runs; a wait reason, when the scheduler gives one, is logged as `[HPC] Waiting: …` |
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

### Memory hint

The runner estimates each selected image's peak memory from the image size, using a model measured for
each step and stored in the step's spec as `"hpc".mem`:

```
GB = base_gb + per_plane_gb·Z + per_frame·frame + CPUs·(per_cpu_gb + per_cpu_frame·frame)
frame = one timepoint in memory, C·Z·Y·X·bytes, in GB      (/api/images/geometry)
```

A composite gets its steps' models merged in by `/api/tasks/definitions` and uses the largest. Only one
timepoint is held at a time (`predict_from_zarr` streams frames), so T does not appear in the model.

| Step | base_gb | per_plane_gb | per_frame | per_cpu_gb | per_cpu_frame | What drives it |
|---|---|---|---|---|---|---|
| `segment.cellpose` | 2.5 | 0.06 | 4.5 | | | model ~1.4 GB; per-frame label and stitch buffers |
| `segment.coastal` | 8 | 0.1 | 30 | | | model ~5 GB at one tile, ~8 GB once a frame spans several; window features |
| `segment.measureLabels` | 1 | | | 0.65 | 0.75 | one spawned worker per job CPU |

**How these were measured.** A probe ran each step's Python on synthetic 2-channel uint16 images and
sampled the peak memory of the whole process tree (PSS, so spawned workers' shared libraries are not
counted twice).
- **Cellpose and measure-labels:** the per-frame slope used a stub in place of the model call, so
  frames from 0.04 to 1.3 GB ran in seconds. The fixed and per-plane costs came from real Cellpose-SAM
  runs on 512 × 512 frames at 4 and 16 planes. The measure-labels coefficient is a hand-set bound that
  covers every measured point (1–8 workers).
- **Coastal:** the stub did not work. Real Coastal holds window features that grow with the frame,
  about 4× what the stub showed, so its model is fitted to real runs only (*Checked against real runs*
  below). Growth flattens as frames get bigger, so a straight line is conservative beyond 0.17 GB,
  where it has not been measured.

Measured rise over the ~0.6 GB a Python process starts at, in GB (model call stubbed; the Coastal
column is the stub's and is NOT what the model uses):

| frame (GB) | Cellpose, per-frame part | Coastal, per-frame part | measure, 8 workers | measure, 1 worker |
|---|---|---|---|---|
| 0.04 | 0.2 | 0.8 | 2.0 | |
| 0.34 | 1.5 | 2.9 | 6.7 | 0.6 |
| 0.67 | 2.8 | | 9.0 | 0.7 |
| 1.34 | 5.5 | | 11.2 | |

**Checked against real runs** (GPU, peak total in GB; Coastal with 20 timepoints and a model with
temporal scales 1–8):

| Step | Frame | Real | Model |
|---|---|---|---|
| Cellpose | 1024 × 1024 × 10, 0.042 GB | 2.8 | 3.3 |
| measure-labels, 8 workers | same | 1.9 | 6.5 |
| Coastal | 512 × 512 × 4 / × 16 | 5.5 / 6.6 | 8.5 / 9.7 |
| Coastal | 1024 × 1024 × 10, 0.042 GB | 9.6 | 10.3 |
| Coastal | 1536 × 1536 × 10, 0.094 GB | 11.6 | 11.8 |
| Coastal | 2048 × 2048 × 10, 0.168 GB | 12.8 | 14.0 |

**Not measured:**
- a CPU-only run of a GPU step;
- a `blockSize` other than 512 (a bigger tile raises the model's share);
- a Coastal model with temporal scales other than 1–8;
- a real cluster.

Treat the hint as a floor with some headroom, not a guarantee. If a job is stopped for memory, the
scheduler's reason appears in the log, and the measured numbers above should be updated.

## On the cluster

```
<remote folder>/
  bin/pixi-<version>        projects/<project dir>/   mirror; .cecelia-mirror = owner (host:path)
  env/<lock hash>/          code/<code hash>/         assets/{models,cellpose}/
  cache/pixi/               jobs/<task>-<runner>-<n>/ job.sh · job.log · .exit · params.json
```

Nothing here is removed by a run; Settings → HPC → **Clean up** / **Delete all** (*Cluster
environment* above) is how it goes. An env is ~8.3 GB and tens of thousands of files, which counts
against file-number quotas (Gadi's), and each Cecelia version that changes `pixi.lock` adds one.

A project folder that another workstation already mirrors (a different `.cecelia-mirror`) is refused,
naming the folder to remove. Every job's `job.sh` and `job.log` stay in `jobs/`, which is the first
place to look when something fails.

- **`job.log` is written by the job itself** (`exec > job.log` at the top of the body, Python
  unbuffered), so the Task Manager shows lines as they are printed. PBS spools a job's `-o` file on
  the node and copies it over only when the job ends; measured on Gadi, the old `-o job.log` appeared
  only at state E.
- **`scheduler.log`** is the scheduler's own output (`--output` / `-o`): mostly empty, plus PBS's
  end-of-job footer (resource usage, SUs). A failed job's last 20 lines are added to the task log as
  `[scheduler] …`.

## Schedulers

One resource model, mapped per backend (HPC_PLAN D8):

| | SLURM | PBS Pro / OpenPBS |
|---|---|---|
| queue | `--partition` | `-q` |
| CPUs / memory / GPUs | `--cpus-per-task` `--mem=NG` `--gres=gpu:N` | `-l ncpus=…` `-l mem=…gb` `-l ngpus=…` (job-wide; Gadi refuses `select`) |
| walltime | `--time` | `-l walltime` |
| account / QoS | `--account` `--qos` | `-A` and `-P` / — |
| status | `squeue -u me` | `qstat -f <ids>` (one query per poll; `job_state`) |
| why it waits | `squeue` `%r` (`Priority`, `Resources` in words; others as named) | `qstat -f` `comment` (Gadi sets none on a fresh job) |
| why it vanished | `sacct`, `scontrol show job` | `qstat -x -f` (Exit_status, comment) |
| cancel | `scancel` | `qdel` |

- **Completion does not depend on the scheduler:** the job writes `.exit` with the runner's exit
  code. A job that disappears without one is reported with the scheduler's reason, e.g. "Walltime
  reached" or "Out of memory".
- **Torque is not supported.**
- **The account goes out as `-P` as well as `-A`.** NCI Gadi charges, and grants `/scratch/<project>`,
  by `-P` alone; without it a job runs under the user's default project and cannot reach another
  project's storage. Storage beyond the project's own scratch (`gdata/…`) still needs
  `#PBS -l storage=…` under Directives.
- **A site that wants `--gpus=N`** instead of `--gres` can set GPUs to 0 and add the line under GPU
  Directives.

Code: `app/src/hpc/` (`transport.jl`, `backend.jl`, `backends/{slurm,pbs,local}.jl`, `remote_env.jl`,
`dispatch.jl`, `job.jl`), `app/src/config/hpc.jl`, `api/src/hpc_api.jl`,
`frontend/src/components/HpcSettings.vue`, `frontend/src/utils/hpcRun.ts`.

## Porting a task

Porting a task means declaring its `run_py` reads and writes, and giving its spec `"hpc"` in `env`
plus a complete `"hpc"` block. The steps are in [`MODULES.md`](MODULES.md) → *Porting a task to
HPC*, and a ratchet in `app/test/suite/hpc_dispatch.jl` holds them.

Declare the path the runner opens; its companions go along by themselves (`hpc_read_companions`,
applied once per dispatch, so they also count toward the reattach fingerprint). A series inside a store
(`ccidImage.ome.zarr/0`) takes the store root's `OME/` (or legacy root `METADATA.ome.xml`) and group
attributes, since the OME-XML reader falls back there. Any read takes its `<path>.normstats` cache.

## Testing

| Where | What | How |
|---|---|---|
| Every CI leg | settings, renderers, parsers; the whole path against a local "remote" (`HpcLocalConn` + `LocalShellBackend`): code sync, env install with a stub pixi, a real Measure-labels dispatch (same table as local, rerun sends 0 B, purged mirror resyncs, undeclared write named, foreign mirror refused), cancel, reattach after a crash, cancel retry; PBS through shim `qsub`/`qstat`/`qdel` | `pixi run test-pkg` / `test-hpc` |
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
   - cancel one while queued; `squeue` should drop it within about 30 s;
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
| `Connection dropped. Check the port (SSH is usually 22)` | something listens on that port but it is not SSH; fix the port in Settings → HPC |
| `Key rejected. A passphrase key must be loaded in ssh-agent` | wrong key, or a passphrase key that is not loaded |
| `SLURM not found on the login node` | `sbatch` is not on PATH, even after `/etc/profile`. Tell us what the site needs |
| `The cluster has no environment for this version of Cecelia` | run Install on cluster (once per version) |
| `[ERROR] … needs Linux x86_64; this node is …` | the install landed on a non-x86 node: pick another partition |
| `Submit: qsub: Error: …` beside Install on cluster | the scheduler refused the install job; the reason is PBS's or SLURM's own |
| `Submit: the cluster did not answer within 180 s` | the login node's qsub/sbatch stalled; try again. A submit that timed out can still land, so the next Install first asks the scheduler and stops with `An install job is already on the cluster (id)` if one did |
| `qsub: Error: You have requested N CPUs which is not 12 times the number of GPUs` | Gadi wants exactly 12 CPUs per GPU: set **CPUs per GPU** on the GPU class in Preferences → HPC (or **Use 12**) |
| `No queue named X on the cluster` beside Nodes | the class's queue is not one the scheduler knows: check its spelling |
| `rsync is not installed on the cluster` | the login node has no rsync. Rare; tell us |
| `Cannot send <path> to the cluster: it is outside the project` | the task reads a file it has not declared; it needs porting work |
| `Cluster job N failed: Walltime reached` / `Out of memory` | raise Minutes / GB on the run form |
| `[HPC] Waiting: …` | the scheduler's reason the job has not started: a busy queue, or a limit of your account |
| `The cluster copy of project 'X' belongs to …` | another workstation mirrors a project with the same folder name; remove the named folder on the cluster |
