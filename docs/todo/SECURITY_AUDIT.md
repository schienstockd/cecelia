# SECURITY_AUDIT

Status: **F1–F9, F12–F15 and bundle symlinks fixed** (#2386, #2390, branch `security-audit-batch3`,
2026-11-20); F10, F11, F16 left by decision (below). GitHub secret scanning + push protection were
already on (public-repo default); CodeQL default setup enabled; Dependabot security updates off.
Scoping audit run 2026-11-19 from the archived brief
[`../archive/SECURITY_SCOPE_AUDIT_PROMPT.md`](../archive/SECURITY_SCOPE_AUDIT_PROMPT.md) by a subagent;
F1, F2 and the plugin-install sink re-read by the parent session.

## Goal

Decide whether Cecelia needs a security scan, and how deep. Verdict: **URGENT FIX** for F1–F2, then a
targeted model review of the hot spots in §6. Below the fix log is the audit as it was returned.

## Fixed (2026-11-20, `security-audit-fixes`)

- **F1:** `import_project` / `bundle_info` (`app/src/project_io.jl`) refuse a manifest whose
  `projectUid` fails `valid_uid` (`app/src/utils.jl`, same rule as the API's `_valid_asset_id`) or
  whose `packedStores` entry is absolute, has a `..` segment, or is not a `*.zarr.tar`. Test:
  `project import refuses a hostile manifest` (`app/test/suite/ome_qc.jl`) — fails without the fix,
  and the escape directory was really created.
  The API's `_valid_asset_id` now delegates to `valid_uid`, so the rule has one home.
  **Not covered:** symlinks in the bundle tree (`cp` keeps them) or created by tar members — §6 item 1.
  **Still open (F7):** request handlers that join a uid onto `projects_dir()` without the check —
  `api_projects_delete` (`api/src/routes/project.jl`, confirmed: discards whatever directory the uid
  names) and the `isdir`-only guards in `notebooks_api.jl`, `captures_api.jl`, `movies_api.jl`,
  `landscape_api.jl`, `storage_api.jl`, `preview_api.jl`. Token-gated, and F3 now keeps other local
  pages out, so it needs the token; one `project_dir(uid)` helper is the fix.
- **F2:** `renderBlackboardMarkdown` (`frontend/src/utils/blackboardMd.ts`) and What's New's
  `renderMarkdown` (`frontend/src/lib/whatsNew.ts`, GitHub release body) go through `sanitizeHtml`
  (`frontend/src/utils/sanitizeHtml.ts`: DOMPurify, now a direct dependency `^3.4.17`, which also
  clears the `npm audit` dompurify advisory; form controls forbidden). Test:
  `sanitizeHtml.browser.test.ts` (real Chromium) — fails without either call. The non-markdown
  `innerHTML` sites (`TrackSchemeView.vue`, `ClusterHmmTransitionsPanel.vue`, `tooltip.ts`) are
  unaudited — §6 item 3.
- **F3:** `handle_stream` refuses a browser request not from this backend's own page
  (`request_origin_ok` in `app/src/api_token.jl`: `Sec-Fetch-Site`, else `Origin` vs `Host`; a plain
  navigation to the frontend passes); the WebSocket `check_origin` uses the same check. Tests:
  `API token` (`app/test/suite/config.jl`) and `API token gate — another localhost page is refused`
  (`api/test/suite/api_token_gate.jl`, a real listener) — fails without.

## Fixed, batch 2 (2026-11-20, `security-audit-batch2`)

- **F7:** a request's project uid is checked once at the door instead of in ~40 handlers: the query
  and the body (`bad_uid_in_json`, a scan at any depth, `app/src/utils.jl`) in `handle_http`, the
  query on the binary routes in `handle_stream`, every WebSocket message in `handle_message`, and the task runner's own listener (`_runner_stream`). The
  key is `projectUid`; on the project-manager routes whose body is `{uid}` (load, rename, delete,
  claim, unclaim, share) `uid` too. Test: `API: a request's project uid must be a uid` (live
  listener; `/api/projects/delete` with an absolute path left the directory alone) — fails without.
  **Limit:** a key written with a JSON `\u` escape is not recognised by the scan; it needs the token.
- **Bundle symlinks (§6 item 1):** import drops every symlink after copying the bundle and again
  after unpacking, and logs the count. Refusing was not an option: a legacy-migrated project can hold
  store links, dead on any other machine. Without the first drop, tar unpacked through a linked folder
  and the cleanup deleted the archive there (the test showed both).
- **F4 / F12:** the Pluto secret (`pluto/launch.jl`) and `push_target.json` are written 0600 before
  the content goes in — `write_secret` (Base-only, `config_dir.jl`; the API token uses it too) and `write_atomic(…; mode)`. The config dir itself is unchanged.
- **F5:** the project's HPC account is validated on read (`read_hpc_project_settings`) as on save; a
  malformed one is ignored with a warning.
- **F6:** legacy migrate runs a stored Rscript (ccid.json or chain params) only if it is named
  Rscript and lies outside the projects dir (`_trusted_rscript`); otherwise this machine's.
- **F8:** `plugin_name_from_url` is anchored to `^https?://github.com/` and refuses `.`/`..`.
  Plugin install still accepts any http(s) URL — a capability, so left for the maintainer.

## Fixed, batch 3 (2026-11-20, `security-audit-batch3`)

- **F9:** a notebook revision (`revise_notebook`, Claude's only) writes every cell it adds or changes
  **disabled**, in Pluto's own on-disk form; one still disabled from an earlier revision stays so
  (`_cells_to_disable`). Pluto's auto-reload runs added and changed cells (`update_from_file` →
  `update_save_run!`), so a revision ran in an open notebook with no click. Checked against Pluto's
  loader: the cell reads back `is_disabled`. The user reads the greyed cell and enables it.
  **Not covered:** a notebook Claude *creates* runs all its cells when the user opens it.
- **lodash-es** 4.17.23 → 4.18.1 inside mermaid's range (lock only); `npm audit` reports 0.
- **F13:** sandboxed agents may not read `~/.cecelia` or the `CECELIA_DEV_DIR` in their env.
- **F14 / F15:** containment by `path_inside` (separator, not bare prefix): the static frontend, and
  the OME-Zarr `.zattrs` dataset path in the Julia reader and `set_ngff_axes`.
- **Plugin install** takes https only.

**Left by decision:** F10 (autonomous server path params — evals only), F11 (the AI-off switch is a
label; documented), F16 (Actions pinned by tag, bioformats downloads without a checksum — churn for
little gain).

---

Audited `origin/main` @ `3352caa66`, read-only. Nothing was exploited end to end and no server was started.

## 1. Verdict

**URGENT FIX.**

Untrusted data files reach dangerous sinks in two places, and nothing in between asks the user.
- **Shared projects (F1).** A project bundle exported by another lab can write a directory tree to any path the user can write to.
- **Blackboard entries (F2).** Imported projects and prompt-injected Claude sessions can both write blackboard entries. The app renders them as raw HTML in its own origin. A script running there holds the auth cookie, so it can install a plugin (downloaded and loaded as code at once) or switch on and call the debug console: arbitrary code as the user.

The token gate itself holds on every listener. Cookies cannot cross sites, but a page on any other `localhost` port counts as the same site, so it can still drive the API and the WebSocket (F3).

Deserialisation and supply chain are in reasonable shape:
- No pickle, `yaml.load`, `allow_pickle` or `shell=True` anywhere in scope.
- `torch>=2.6` makes `torch.load` default to `weights_only=True`.
- OME-XML is parsed by stdlib expat, so no external entities.

Minimal mitigations, all small:
- Check `projectUid` against the existing uid regex at import and in every `joinpath(projects_dir(), uid)` handler.
- Sanitise blackboard HTML with DOMPurify.
- Reject a request whose `Origin`/`Sec-Fetch-Site` is not our own origin, in `handle_stream` and in the WebSocket `check_origin`.

A targeted model review should follow (§6).

## 2. Known controls

- **Loopback bind — holds.**
  - Backend: `api/src/server.jl:1009`.
  - Runner: `runner_serve` defaults to `127.0.0.1`.
  - Preview worker: `preview/preview_worker.py:277`.
  - Vite: `frontend/vite.config.ts:60`.
  - Pluto: `pluto/launch.jl` sets no host, so Pluto's default applies. I believe that is `127.0.0.1` but did not verify it.
  - With `CECELIA_HOST=0.0.0.0` the gate still applies, but TLS is off by default, so the token and cookie would travel in cleartext.
- **API token gate — holds on every listener.**
  - Both the plain listener (`server.jl:1131`) and the TLS listener (`:1146`) go through `handle_stream`. The check at `:912` runs before the WebSocket upgrade, OPTIONS, binary routes, static files and the router.
  - An empty `_API_TOKEN[]` would leave the gate open, but `start()` sets it at `:1077` before the bind. The runner also sets its token before listening, and only `/ping` is open there.
  - The preview worker refuses every connection when it has no token (`preview_worker.py:1616-1634`). Pluto has its own secret. The MCP servers talk over stdio. The Vite proxy forwards to the gated backend.
  - DNS rebinding is covered: an attacker's hostname never gets the cookie.
- **`_safe_next` — holds in practice.**
  - It rejects `//`, `\` and anything not starting with `/` (`server.jl:862`).
  - It does not reject tab or newline: `"/\t/evil"` may be read as `//evil` by a browser (unverified).
  - That only matters after a correct token, so it is not exploitable.
- **SameSite=Lax cookie — gap (F3).**
  - The design claim "a cross-site page never carries it" (`docs/ARCHITECTURE.md:832`) is true. But any `http://localhost:<other port>` page is the same site: cookies ignore ports, and the cookie is set on host `localhost` (`app.py:184`).
  - There is no Origin, Sec-Fetch-Site or Content-Type check (`server.jl:907-1003`).
  - The WebSocket accepts any origin: `check_origin=(req, origin) -> true` (`server.jl:928`).
- **Debug console (`/api/repl`) — gap: in effect it is gated by the token alone.**
  - The toggle endpoint `/api/repl/config` is token-gated, but one POST `{"enabled":true}` flips it (`repl_api.jl:42-45`, whose own comment says "not a security boundary").
  - So the console is protected only by the cookie or token plus the loopback bind.
  - A same-site page can send `text/plain` POSTs, which skip the CORS preflight, to switch it on and then evaluate code. It cannot read the response, but the code still runs (F3).
- **Remote access over an SSH tunnel — holds as designed.** `docs/INSTALL.md:339,359` say to keep `CECELIA_HOST` unset and not open 8080. Whether facilities follow that is unverified.
- **Dependabot (npm + GitHub Actions only) — holds.**
  - Confirmed there is no CodeQL, gitleaks, osv-scanner, trivy, semgrep or `npm audit` workflow.
  - Actions are pinned by tag (`@v7`, `setup-pixi@v0`), not by commit SHA.
  - No workflow uses `pull_request_target`.
- **`judge --auto` sandbox — partial.**
  - What it restricts (`python/cecelia/effectiveness/agent_sandbox.py:26-43`):
    - Bash runs under bubblewrap and may not run unsandboxed.
    - All network domains are denied.
    - Reads of `~/.ssh`, `~/.gnupg`, `~/.config/gh`, `~/.claude` and `/run/user/<uid>` are denied.
    - Writes and edits under `~/**` are denied.
    - GitHub tokens and the DBus session address are stripped from the environment.
  - Commit `6082bfd16` also pins the git hooks path and refuses diffs to `.github`, hooks, pixi files and `CLAUDE.md`.
  - **Gap:** `~/.cecelia`, which holds `api-token`, is not in the read-deny list. Whether the network deny also blocks `127.0.0.1` is unverified. This is dev tooling and is not shipped.

## 3. Threat model summary

**Exposure.**

| Listener | Bind | Gate |
|---|---|---|
| Backend HTTP and WebSocket | 127.0.0.1 | API token |
| Task runner | 127.0.0.1 | API token, except `/ping` |
| Preview worker WebSocket | 127.0.0.1 | API token; refuses all without one |
| Pluto | Pluto's default, believed 127.0.0.1 (unverified) | Pluto's own secret |
| Vite (dev only) | 127.0.0.1 | Proxies to the gated backend |
| MCP servers | stdio | not applicable |

The realistic adversaries:
- **Another account on a shared workstation or shared install.** It shares loopback, so it can (a) read files Cecelia leaves at the default umask (F4) and (b) serve a page on a localhost port (F3).
- **Data from another lab:** bundles, images and legacy projects.

**Data files.**
- `.rds` files go through `readRDS`. On R older than 4.4 that is CVE-2024-27322 territory; the R version in use is unverified.
- The weak points are project-level fields, not file formats:
  - the bundle manifest (F1);
  - blackboard markdown (F2);
  - `meta.legacyRscript` in ccid.json (F6);
  - the HPC account in `settings/hpc.json` (F5);
  - `datasets[].path` in zarr `.zattrs`, joined onto paths with no containment check (read-only, F15).
- Notebooks are Julia code that runs when opened. That is by design.

**Requests.**
- Every handler is behind the token, so path problems matter mainly as payloads for F2 and F3.
- There is no central uid check:
  - `api_projects_delete` (`api/src/routes/project.jl:527-530`) recursively discards whatever directory the uid names, including absolute paths and `..`.
  - Export accepts any output directory.
- `/api/plugins/install` downloads any http(s) URL and loads it as code at once (`api/src/plugins_api.jl:50-57`). That is one POST to code execution, with no toggle in the way.
- HPC shell construction is otherwise careful, with the exception of F5.

**AI agents.**
- The MCP tools can only call a fixed list of routes (`mcp/cecelia_mcp/client.py`). The list does not include `/api/repl` or `/api/plugins/install`, and the tools write only inside the project.
- Kiwi and review turns run `claude -p --tools ""` with read-only allow-lists.
- `report_issue` never posts: it has no HTTP client and only saves a draft.
- No GitHub issue body is fed to an agent; only a hash is stored.
- **Can text inside a data file make an agent run code without the user approving? Yes, indirectly:**
  - Injected text can steer Claude into writing HTML into a blackboard entry. That HTML runs in the app's origin when the user views the entry (F2).
  - `revise_notebook` on a notebook already open in Pluto may re-run the new code with no click, because `auto_reload_from_file=true` (F9; whether Pluto re-runs the cells is unverified).
- The AI-off switch keys on a header the caller declares about itself (`server.jl:255`). It is a label, not a gate.

**Blast radius.**
- The app runs as the user with full file access.
- At stake on the host:
  - all of that user's projects;
  - the HPC SSH key and `gh` credentials;
  - `~/.cecelia/api-token` (mode 0600);
  - `~/.cecelia/pluto/.plutosecret`, written at the default umask (F4);
  - `push_target.json` in each project's settings, which holds the Claude Code inbox token, also at umask (F12).
- No secret-scanning tools are installed. A regex scan of all 5,861 commits for GitHub, Anthropic, AWS and private-key shapes found 0 hits. That is not a substitute for gitleaks.

**Supply chain.**
- `npm audit`: 1 high (lodash-es) and 1 low (dompurify), both transitive.
- Python and conda are locked with hashes in `pixi.lock`; `pixi.toml` uses `>=` ranges.
- The Julia `api/`, `app/` and `pluto/` environments all have Manifests.
- `install.sh` is `curl | sh`:
  - It checks a `.sha256` fetched from the same place as the tarball, which proves integrity but not authenticity.
  - It skips that check on the dev channel or when no hashing tool is installed.
  - The bioformats2raw and bftools zips are downloaded with no checksum (`install.sh:282,301`).
- The dev-channel self-update has no integrity check.
- Model weights are downloaded with no hash check.

## 4. Findings table

Severity is for the realistic case: bundles from collaborators, and shared workstations.

| id | severity | location | what | evidence | suggested action |
|---|---|---|---|---|---|
| F1 | **High** | `app/src/project_io.jl:362-420` | Import trusts `manifest.projectUid` as a path. An absolute path or `../` puts the project tree anywhere. If nothing exists there, it is created with no prompt. In Replace mode the existing directory is moved aside and discarded. `packedStores` entries are not contained either (`:386`: extract, then delete). | Code: `uid = String(manifest.projectUid)` → `target = joinpath(projects_dir(), dest_uid)` → `mkpath(tmp)` / `mv(tmp, target)`. No uid check in `import_project`, `bundle_info` (`:298`) or `handle_project_import` (`api/src/sockets.jl:320-325`). The UI asks only when the target exists (`frontend/src/components/ProjectPanel.vue:257-267`). Scratch Julia: `joinpath("/srv/projects", "/home/v/.julia/config")` gives `/home/v/.julia/config`; `normpath(joinpath(p, "../../home/v/.config/autostart"))` gives `/home/v/.config/autostart`. Either is code execution at the next login or Julia start. | Reject a uid that fails the existing uid regex (`_valid_asset_id`, `api/src/routes.jl:12`) in `import_project` and `bundle_info`. Require each `packedStores` entry to be relative, free of `..`, and end in `.zarr.tar`. |
| F2 | **High** | `frontend/src/utils/blackboardMd.ts:6-9,79-88`; `frontend/src/modules/BlackboardModule.vue:847,851,856,868` | Stored XSS in the app's origin. Blackboard markdown goes through `marked`, which keeps raw HTML, into `v-html`. There is no sanitiser and no CSP. Two writers: an imported bundle (the `blackboard/` folder is copied verbatim, `project_io.jl:380-383`), and Claude via the MCP blackboard create/revise tools (`server.py:1834,1872`; the server checks only size, `blackboard_api.jl:236`). The script holds the cookie, so it can call `/api/plugins/install` (code is loaded at once), or switch on `/api/repl` and use it, or read the Pluto secret from `GET /api/notebooks/status` (`notebooks_api.jl:216`). Result: code execution as the user when they open the entry. | Ran the repo's `marked` 18.1.2 in node: `<img src=x onerror="fetch(1)">` and a markdown link to a `javascript:` URL come through unchanged. No DOMPurify in `frontend/src`; no Content-Security-Policy anywhere. The full chain was not run. | Run the `marked` output through DOMPurify (already a transitive dependency). Correct the "trusted source" comment. Optionally add a CSP to the static frontend. |
| F3 | **Medium** | `api/src/server.jl:907-1003`, `:928`; cookie at `:900-901` | Same-site request forgery and WebSocket hijack. Any page on `http://localhost:<port>` carries the Lax cookie. `text/plain` JSON POSTs skip the preflight, and handlers parse the body whatever its Content-Type (`repl_api.jl:43,159`; `plugins_api.jl:28`). The WebSocket accepts any origin, giving full read and write: `task:run`, `project:import`, and `project:export` to any output directory. Blind POSTs can reach `/api/plugins/install` (code execution), the debug console switch plus `/api/repl`, and `/api/projects/delete` with a traversal uid. Precondition: attacker content served from a localhost port and opened in the victim's browser (another account on a shared box, or XSS in any other local web app). | No `Origin` or `Sec-Fetch-Site` reference in `server.jl`. The cookie host is `localhost` (`app.py:184,450`). The browser behaviour is from the spec, not exercised. | 403 when `Origin` is present and is neither this server's origin nor the Vite dev origin; apply the same rule in the WebSocket `check_origin`. |
| F4 | **Medium** | `pluto/launch.jl:88-91`; `app/src/config_dir.jl:101,110` | The Pluto session secret is written at the default umask, unlike `api-token` (0600). If the home directory is readable by others, another account can read `~/.cecelia/pluto/.plutosecret` and run Julia as the victim through Pluto. | Scratch Julia under umask 0002: `mkpath` gave 775, `write` gave 664. The only secret chmod in the repo is `api_token.jl:56`. Your home is 750. macOS and lab-box modes are unverified. | Create the file 0600 before writing, as `ensure_api_token!` does, and make the config dir 0700. Do the same for `push_target.json`. |
| F5 | **Medium** | `app/src/config/hpc.jl:314`; `app/src/hpc/backends/slurm.jl:29`; `pbs.jl:41` | Command injection into the HPC job script. The project's `settings/hpc.json` `account` travels in bundles. It is read with only `strip`, which trims the ends but keeps inner newlines (`hpc.jl:85`). It is then concatenated into `#SBATCH --account=` / `#PBS -A`. A newline adds a shell line to `job.sh`, which runs on the cluster as the user. Only the writer `set_hpc_project_account!` (`:325`) validates it. | Code read. `job.sh` is written via a quoted heredoc and run by the scheduler (`app/src/hpc/backend.jl:90-100`). | Apply `_hpc_token!` (`hpc.jl:149`) when reading the project settings. |
| F6 | **Medium** | `app/src/tasks/importImages/migrateLegacy.jl:84`; `app/src/config/binaries.jl:112-114` | ccid.json chooses the program that runs. `img.meta["legacyRscript"]` is used as the Rscript binary, and any value containing a directory is returned unchanged and executed (`legacy_migrate.py:40`). The request body field `rscript` (`api/src/routes/image.jl:179`) does the same. Trigger: the user runs Migrate legacy on an imported project. | Code read. | Resolve Rscript only from settings or PATH, never from ccid.json. |
| F7 | Low | `api/src/routes/project.jl:527-530,345,504-510`; `api/src/sockets.jl:301` | No central uid check. Delete removes any directory; canvas and rename handlers write into any existing directory; export reads any directory and writes to any output directory. Token-gated, but these are the payloads for F2 and F3. | Code read. | One validating `project_dir(uid)` helper, used everywhere. |
| F8 | Low | `api/src/plugins_api.jl:35-57`; `app/src/tasks/plugins.jl:529-531,559` | Plugin install accepts any http(s) URL and loads it as code. A separate unanchored name regex can produce the name `..`, which would delete the plugins folder's parent (unverified at runtime). | Code read. | Anchor the regex to `^https://github.com/` and reject `.` and `..`. |
| F9 | Low | `pluto/launch.jl:71`; `api/src/notebooks_api.jl:695-698` | `revise_notebook` rewrites a notebook in place and Pluto reloads it, so an injected cell may run in an open session without a click. | Whether Pluto re-runs cells on reload is unverified. | Don't auto-reload Claude revisions, or write them to a new file. |
| F10 | Low | `app/src/tasks/exportImages/ome_tiff.jl:144-160`; `mcp/cecelia_mcp/autonomous.py` | The `outDir` / `filePath` task parameters can point outside the project, and the autonomous server's guard does not check them. That server is used only for evals and `pixi run chat`. | Subagent read; not fully re-read. | Restrict path parameters in `autonomous.py`. |
| F11 | Low | `api/src/server.jl:255,918` | The AI-off switch keys on the self-declared `X-Cecelia-Client` header, so a token holder that omits it bypasses the switch. Documented as a label. | Code read. | Document it as UX, not a boundary. |
| F12 | Low | `push_target.json` in project settings | The Claude Code inbox token is stored at umask. On a shared projects dir, another account could push text into the paired session. | Code read; socket permissions unverified. | Write it 0600. |
| F13 | Low | `python/cecelia/effectiveness/agent_sandbox.py:33` | The `--auto` sandbox does not deny reading `~/.cecelia`, which holds the API token. | Code read. | Add the config dir and the dev dir to the read-deny list. |
| F14 | Low | `api/src/server.jl:696-703` | `_resolve_static` checks the prefix without a trailing separator, so sibling folders named `frontend/dist_*` are reachable. Gated. | Code read. | Compare against `_DIST_DIR * "/"`. |
| F15 | Low | `python/cecelia/utils/zarr_utils.py:816`; `app/src/tasks/importImages/omezarr/reader.jl:289-291` | The `.zattrs` dataset path is joined with no containment check, and symlinks inside stores are followed. Read-only. | Subagent read. | Reject absolute and `..` dataset paths. |
| F16 | Info | `.github/workflows`; `install.sh:154-159,282,301`; `update_api.jl` | Actions pinned by tag; no code or secret scanning; no checksum on the bioformats2raw and bftools downloads; no integrity check on dev-channel updates. | grep and `npm audit`. | See §5. |

## 5. Action list

Ordered by effort against risk reduction.

1. **Cheap:** uid check plus `packedStores` containment at import (F1). About 10 lines.
2. **Cheap:** DOMPurify on blackboard HTML (F2). About 5 lines.
3. **Cheap:** Origin / `Sec-Fetch-Site` check in `handle_stream` and the WebSocket `check_origin` (F3). About 15 lines.
4. **Cheap:** mode 0600 for `.plutosecret` and `push_target.json`; 0700 for the config dir (F4, F12).
5. **Cheap:** validate the HPC account when it is read (F5); drop the ccid.json Rscript fallback (F6).
6. **Cheap:** central `project_dir(uid)` check (F7); anchor the plugin URL regex (F8).
7. **Free:** turn on GitHub secret scanning with push protection, and Dependabot security alerts.
8. **Free:** CodeQL default setup for JavaScript/TypeScript and Python (it does not support Julia).
9. **Free:** an osv-scanner workflow over `pixi.lock` and `package-lock.json`.
10. **Cheap:** pin GitHub Actions by commit SHA; add sha256 checks for the bioformats2raw and bftools downloads.
11. **Model review:** the files in §6.

## 6. Hot spots for model review

1. `app/src/project_io.jl` (`import_project`, `bundle_info`): can any manifest field, symlink in the bundle tree (`cp` keeps symlinks) or tar member land outside `<projects>/<uid>`?
2. `api/src/server.jl` (`handle_stream`, `_handle_auth!`): after an Origin fix, can anything still be forged from another localhost origin (WebSocket, OPTIONS, binary routes, the Vite proxy)?
3. Every `v-html` and `innerHTML` in `frontend/src` (tooltips with `escape:false`, `TrackSchemeView.vue:838`, `ClusterHmmTransitionsPanel.vue:169`): can text from a project reach them unescaped?
4. Every `joinpath(projects_dir(), …)` in `api/src/routes/*.jl` and `api/src/*_api.jl`: is the input validated?
5. `app/src/hpc/{backend,dispatch}.jl` and `backends/{slurm,pbs}.jl`: does any project, settings or task value reach `job.sh` or `sh -s` unquoted?
6. `api/src/plugins_api.jl` and `app/src/tasks/plugins.jl`: how URL becomes name becomes target path; can `_plugin_place!` delete outside `plugins/`?
7. The MCP write tools in `mcp/cecelia_mcp/server.py` and the revise path in `api/src/notebooks_api.jl`: can any MCP write make code run without a user step?
8. `migrateLegacy.jl` and `legacy_migrate.py`: does any other field from a project choose a binary or script?
9. `pluto/launch.jl` and `app/src/config_dir.jl`: what file mode does every secret written into the config or project dirs get?
10. `app/src/ai/agent_runner.jl:102-127,385-412`: on Windows, `cmd /c` re-parses the arguments; can any argument carry cmd metacharacters?

## 7. Questions for the owner

- Do outside labs exchange bundle folders today? That decides how urgent F1 and F2 are.
- On shared installs (#1487): is the projects dir shared between accounts, and what are the home and config dir modes? Are any shared machines Macs with default home permissions?
- Which R version do people running legacy migration have? CVE-2024-27322 is fixed in R 4.4.0 and later.
- Do other local web servers (Jupyter, RStudio Server, dev servers) run on the same machines? Any of them would give F3 its attacker page.
- Is the debug console ever left on in installed builds?
- Is `CECELIA_HOST=0.0.0.0` used anywhere in practice?

## 8. Unverified

- Pluto's default bind host.
- The browser behaviour behind F3 and the `_safe_next` tab case.
- The full F2 chain; each piece was verified separately.
- The full F1 import; only the path arithmetic was checked in scratch Julia.
- macOS and lab-box home directory modes; the Claude Code inbox socket's permissions.
- Whether Pluto re-runs cells on reload (F9).
- Whether the judge sandbox blocks loopback.
- How system tar handles hostile archive members.
- The R version in use.
- Windows `cmd /c` and `.bat` argument re-parsing.
- How zarr 3 handles `..` in dataset keys.
- F10, F15 and some line numbers that came from subagents were not individually re-read.
- Only `npm audit` was available; osv-scanner, trivy, gitleaks and trufflehog are not installed.
