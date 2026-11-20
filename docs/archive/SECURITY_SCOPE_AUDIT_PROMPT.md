> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run.
>
> **Outcome:** run 2026-11-19 → [`../todo/SECURITY_AUDIT.md`](../todo/SECURITY_AUDIT.md) (verdict URGENT FIX; F1–F3 fixed 2026-11-20).

# SECURITY_SCOPE_AUDIT_PROMPT

You are auditing the Cecelia repository (`schienstockd/cecelia`) to decide **whether a security vulnerability scan is warranted, and if so how deep**. You are NOT hunting for every bug. You are building a threat model from evidence and returning a verdict.

## Rules

- Read-only. Do not modify files, open PRs, or run anything that writes outside a scratch dir.
- Every claim needs evidence: `path:line` or a command and its output. Mark anything you did not verify as **unverified**.
- No speculation about exploits. State what is reachable, by whom, with what input.
- Do not use an expensive full-repo vulnerability sweep. Scoping first; deep review only on hot spots you identify.
- If something is out of reach to check (deployment config, facility network), list it under **Questions for the owner** instead of guessing.
- Never start or stop servers, and never touch the user's config dir, projects dir or `~/.cecelia*`. Read code, docs and git history only.

## Known controls — verify, don't rediscover

These exist already. Your job is to check whether each **holds on every path**, not to report its existence as a finding. A header that looks permissive (`Access-Control-Allow-Origin: *`, a WebSocket `check_origin` that returns true) is a finding only if you show a path where the token gate below does not cover it.

- **Loopback bind by default.** `api/src/server.jl` → `const HOST = get(ENV, "CECELIA_HOST", "127.0.0.1")`. Exposure needs an explicit env var.
- **API token gate.** `app/src/api_token.jl` (token file mode 0600 in the config dir; `Authorization: Bearer` for programmatic clients; per-port HttpOnly `SameSite=Lax` cookie for the browser, set by `/api/auth?token=…`). Checked in `handle_stream` in `api/src/server.jl` before routing, the static frontend, binary routes and the WS upgrade. Exempt: `OPTIONS`, `/api/health`, `/api/auth`. Design: `docs/ARCHITECTURE.md` → *API token*. Questions: does every listener go through `handle_stream`? Is there any other port (Vite dev proxy, preview worker, runner, notebooks/Pluto, task console, MCP server, napari bridge) with its own listener and no gate? What happens when `_API_TOKEN[]` is empty in a served process? Is `_safe_next` sound?
- **Debug REPL** (`POST /api/repl`, `api/src/repl_api.jl`): arbitrary code execution, gated by a runtime toggle AND loopback bind AND the token. Confirm the toggle endpoint itself is token-gated, and what a same-site page (another `localhost` port) could do with a `SameSite=Lax` cookie.
- **Remote access** is an SSH tunnel to a loopback-bound server by design, no public port (`docs/todo/REMOTE_ACCESS_PLAN.md`, `docs/todo/REMOTE_BOX_PLAN.md`).
- **Dependabot** is on for npm + GitHub Actions only (`.github/dependabot.yml`); pixi and Julia are refreshed by `.github/workflows/update-deps.yml`. No CodeQL / gitleaks / osv-scanner workflow was found on a first look — confirm.
- **`judge --auto`** runs an unattended agent with a sandbox (commit `6082bfd16`, "nothing the --auto agent writes runs outside its sandbox"). Confirm what the sandbox actually restricts.

## Step 1: Exposure (who can reach it)

Find and report:
- Every listener the product opens: Julia HTTP/WS server, Vite dev server (dev only), detached runner, preview worker, Pluto notebooks, MCP servers (`mcp/`), anything else. For each: bind address, port, and whether the token gate covers it.
- Bind address and port defaults. Flag `0.0.0.0` or configurable-to-public, and what the app does when that is set.
- Deployment modes, documented vs. merely possible: local single-user; **shared multi-account workstation / shared install** (#1487, bwrap — the realistic case: other local accounts share loopback); lab GPU box over RDP/SSH; cloud VM over SSH tunnel; HPC (Cecelia SSHes out to SLURM/PBS — `docs/HPC.md`). The old R/Shiny app is not shipped; only legacy migration calls `Rscript`.

## Step 2: Untrusted input — data files (what it opens)

The realistic untrusted input is **other people's image and project files**: users analyse data someone else acquired, and import projects exported from another machine. Trace entry to sink for:
- Image readers: OME-XML parsing (`python/cecelia/utils/ome_xml_utils.py` — XML entity/expansion handling), `.ims`/HDF5, OIR, LIF, CZI, bioformats2raw (a JVM subprocess), zarr stores (`.zattrs`/`.zarray` JSON, symlinks inside stores).
- Project import / export archives (`docs/JOBS.md` → export/import; `jobs.jl`): zip/tar extraction (zip-slip, symlinks, absolute paths), `ccid.json` and other JSON trusted as paths.
- Legacy migration: `.rds` read via `Rscript` (`python/cecelia/utils/read_ccid_rds.R`, `legacy_migrate.py`).
- Any `pickle`, `joblib`, `torch.load` (without `weights_only`), `np.load(allow_pickle=True)`, YAML `load`, h5ad with object columns — especially for models in the model vault or custom cellpose models that a user could receive from someone else.
- Custom modules (`docs/CUSTOM_MODULES.md`, `<config_dir>/modules/`): code loaded by design — state who can write that dir.

## Step 3: Untrusted input — requests and subprocesses

- Paths derived from request or project data (traversal, symlinks, absolute path override) in `api/src/**` handlers — sample the ones taking a path, file name, `valueName` or asset id.
- Anything reaching a shell or subprocess (`run`, backticks, `Cmd`, `run_py` params, `system`, ffmpeg, headless Chrome, `ssh`/`rsync` command construction in the HPC path from settings fields).
- `eval`, `include`, `Meta.parse`, dynamic code loading, template rendering, query construction.
- Outbound requests built from user input (SSRF): LabArchives, GitHub (`report_issue`, user-reports), HPC hosts, model downloads.

Produce one table across Steps 2–3: `entry point | input | sink | sanitisation present | reachable by`.

## Step 4: AI agent surfaces

The newest and least-reviewed surface. Trace what an agent can be steered into by **text it reads**, not by the user who started it:
- Text an agent reads that someone else may have written: image/channel names and notes, OME-XML metadata, blackboard entries, notebook text, lab-log entries, LabArchives pages, GitHub issue bodies in user-reports triage.
- What it can do with that: the observer MCP write tools (`mcp/`; gates, pops, figures, notebooks, chains, blackboard, `report_issue`), Kiwi turns (`api/src/kiwi_api.jl`), the autonomous server (`run_chain` / `run_task`), `claude -p` spawns (recital, judge, runner), any tool that writes files or runs code (notebooks are Julia/Pluto — is `create_notebook`/`revise_notebook` code execution?).
- Controls present: the AI toggle (`docs/todo/AI_TOGGLE_PLAN.md`, `_from_claude` refusal in `server.jl`), `via: kiwi` provenance stamps, `report_issue` never posting, the `--auto` sandbox, allowed-tool lists in spawned `claude -p` calls.

Answer: can text inside a data file or a shared project make an agent run code or write outside the project, without the user approving a step?

## Step 5: Blast radius (what is at stake)

- Data on the host: unpublished datasets, other accounts' projects on a shared install, HPC SSH keys (`hpcKeyFile` in the profile), GitHub tokens used by `gh`, the API token file, LabArchives credentials.
- Where secrets live (env, config, repo history). Run a quick secret scan if tooling is installed (`gitleaks`, `trufflehog`); otherwise say so.
- Privileges the process runs with. Can it read outside the project dir?
- Note: profile gates (peanut vs. other profiles) are UX, not a security boundary — every profile runs as the same OS account. Do not report them as an auth weakness.

## Step 6: Supply chain

- Dependency manifests: npm (`frontend/package-lock.json`), Julia (`api/`, `app/` Project/Manifest), Python via `pixi.toml` + `pixi.lock`. Note pinned vs unpinned, and how many.
- Whether Dependabot, CodeQL, secret scanning, or CI security checks are enabled (see Known controls).
- Install/update paths for external users (the R predecessor is published in Nature Communications 2025, so outside labs install this): `install.sh` / `install.ps1` (README: `curl … install.sh | sh`), pixi / juliaup / fnm bootstraps in `docs/INSTALL.md`, auto-update, the cellpose-v3 env installer (`/api/system/envs/install`), HPC cluster env install (fetches pixi on the cluster), model weight downloads, the Claude Code plugin install, custom-module loading.
- If `osv-scanner`, `trivy`, or `npm audit` are available, run them and summarise counts by severity. Do not dump raw output. `npm audit` needs network — fine; do not `npm install`.

## Step 7: Verdict

Return exactly one of:

- **NO ACTION**: the token gate covers every listener, no meaningful untrusted input reaches a dangerous sink. Enable free GitHub tooling and stop.
- **CHEAP TOOLS ONLY**: limited exposure. Specify which of CodeQL / Dependabot / Semgrep / gitleaks / osv-scanner to enable, with config.
- **TARGETED MODEL REVIEW**: exposed or high blast radius. List the specific files and functions for a diff-scoped or file-scoped model pass, ranked, with the reason for each.
- **URGENT FIX**: a concrete, reachable, high-impact issue found. Describe it, the evidence, and the minimal mitigation.

Justify the verdict in 3-5 sentences tied to Steps 1-6.

## Output format

1. **Verdict** (one line, then justification)
2. **Known controls** — one line each: holds / gap (with evidence)
3. **Threat model summary** (exposure, data files, requests, AI agents, blast radius, supply chain — one short paragraph each)
4. **Findings table**: `id | severity | location | what | evidence | suggested action`
5. **Action list**, ordered by effort vs. risk reduction, each marked free / cheap / model-review
6. **Hot spots for model review** (only if verdict warrants; max 10 file paths with the question to ask of each)
7. **Questions for the owner** (deployment, facility policy, who else runs this)
8. **Unverified**: everything you could not confirm

Keep it terse. No background explanations of what vulnerability classes are.
