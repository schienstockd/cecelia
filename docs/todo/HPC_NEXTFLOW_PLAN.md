# HPC via Nextflow — parked design

**Status:** parked. No HPC access as of 2026-09-17. Re-open when a cluster is available again (old-R-shiny-version used one).

## Decision

**Do not replace Cecelia's scheduler with Nextflow.** Add HPC as a fourth transport for individual task/chain nodes, dispatched through Nextflow.

## Why not a wholesale replacement

Cecelia's scheduler is a live, in-process runtime, not a batch DAG executor:

- Named resource pools (`cpu`/`gpu`/`io`/`network`) shared across heterogeneous task classes, with per-pool sliders planned. Nextflow's per-process `cpus`/`memory` directives don't model shared pools.
- Per-project/per-image `runLog` (opened at `:running`, closed at terminal), QC banking under the run's `value_name`, sidecars, `ChainRun` content-hashed template freeze — state mutations, not files in `work/`.
- Browser feedback loop: WS events (`runner:log`, chain lane progress) drive the Task Manager + chain Live tab. Nextflow's execution model doesn't fit interactive re-runs, per-pool throttling, cancel-on-worktree-switch.

## Where Nextflow fits

**As a transport for one node's compute**, not as the orchestrator.

- The `network` resource pool is already reserved for exactly this (see `project_resource_pools`). A node marked for HPC dispatches via `nextflow run … -profile slurm`; Cecelia polls / listens for completion; runlog closes; downstream nodes unlock.
- Chain machinery stays in Cecelia: DAG, lane view, QC banking, `ChainRun` outcomes. Only the payload hops to the cluster.
- Cecelia produces the samplesheet (image paths + metadata), calls Nextflow, waits or polls, then re-enters its own world with the outputs at known paths.

## Why Nextflow (vs a hand-rolled SLURM wrapper like the old R version)

- Native executors: SLURM/PBS/LSF/SGE from the same `.nf` script — no `sbatch` scripting.
- Singularity/Apptainer support survives the local→HPC jump (most sites disallow Docker).
- Fan-out is free: 500 images = 500 jobs, `-resume` skips completed ones.
- Handles input staging + output copy-back (the dance the old R version had to do by hand).
- Language-agnostic: the process `script:` block calls `julia`/`python`/anything. Cecelia.jl steps drop straight in.

## Open questions (for when it un-parks)

1. **Submission topology.** Two options:
   - Cecelia runs on the login node — direct `run(\`nextflow run …\`)`.
   - Cecelia stays on the workstation, submits remotely via SSH executor or `ssh headnode nextflow …`.
2. **Live-tab representation for a fan-out node.** An HPC node with 200 subtasks running on the cluster needs a display that isn't "1 node running." Display question, not scheduler.
3. **New pool or reuse `network`?** `network` is currently a placeholder. Might warrant a dedicated `hpc` pool once the transport lands, or `network` may just be renamed.
4. **Completion signal.** File-existence polling vs Nextflow's `-with-report` / trace file vs an event hook. Cheapest path is probably watching the pipeline's exit code + a manifest file in a known output dir.

## Non-goals

- Replacing the local task runner.
- Replacing the chain runner.
- Using Nextflow for anything Cecelia already handles locally (segmentation on the workstation stays on the workstation).

## References

- `docs/SCHEDULER.md` — current scheduler + resource pools.
- `project_resource_pools` memory — `network` pool reserved for future HPC/remote.
- `old-R-shiny-version/` — how HPC dispatch was done before.
