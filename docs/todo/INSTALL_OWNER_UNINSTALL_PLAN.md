# System install owned by the admin account + a complete uninstaller

Status: building (2026-10-06) · branch `feat/install-scope-uninstall`

Two asks from the first real system-scope install (Ubuntu, admin account `cecelia`):

1. `CECELIA_INSTALL_SCOPE=system sudo -E sh` put Julia and everything else "into root". Could it go into
   the admin's account instead, or does it have to be root for other accounts to use it?
2. An option to remove Cecelia completely from disk, choosing whether to wipe or keep projects and
   user settings.

## Findings (traced, not inferred)

- **Why "into root".** Under `sudo` the whole installer runs as root with `HOME=/root` (Ubuntu's sudo
  resets `HOME` even with `-E`). The runtime itself goes to `/opt/cecelia` as designed. But everything
  keyed on `$HOME` lands under `/root`: the pixi/rattler package cache (`~/.cache/rattler`, several GB),
  the uv cache, the npm cache (dev channel), the Pixi installer's PATH line in `/root/.bashrc`, and
  `cecelia-connection.json`. The install tree itself is root-owned, so only root can update it.
- **It does need to be outside the admin's home.** Ubuntu ≥21.04 creates home dirs `750`: other
  accounts cannot traverse `/home/cecelia`, so a shared install there would be unusable by them. It
  does **not** need to be root-*owned*. Other accounts need read + execute, nothing more.
- **Non-admin accounts could not have started it** (reproduced in a `bwrap` sandbox, other uid,
  install dir read-only). The launcher sets `JULIA_DEPOT_PATH=<install>/juliaup/depot` with no
  trailing separator. That drops Julia's bundled stdlib depot and leaves a depot the user cannot
  write first in the path. Julia then tries to precompile stdlibs into it and dies with
  `IOError: … .ji.pidfile: read-only file system (EROFS)`.
- **And before Julia, pixi.** On the real install, a plain `pixi run app` as another uid failed with
  `failed to acquire install lock on prefix '…/.pixi/envs/default' — Read-only file system`. A tiny
  test env did not show this; the full one did. The juliaup launcher works against a read-only
  `JULIAUP_DEPOT_PATH`.

## Decisions

**D1. System scope = a shared dir outside any home, owned by the admin who installs it.**
Default stays `/opt/cecelia` (`/Applications/cecelia` on macOS). The installer is run **as the admin,
without sudo**:

    curl -LsSf …/install.sh | CECELIA_INSTALL_SCOPE=system sh

It uses `sudo` itself for the only two root-only steps: creating the install dir (then `chown` to the
admin) and writing the all-users menu entry (`/usr/share/applications`). Everything else runs as the
admin, so caches land in the admin's own home and are reused on the next update. Updates are a re-run
of the same command and need no sudo once the dir exists. Other accounts get read + execute access.

**D2. `sudo … sh` keeps working (repair, not refuse).** When the installer starts as root with
`SUDO_USER` set, it treats `SUDO_USER` as the owner and runs every download and provisioning step as
that user via `sudo -u`. The result is the same as D1. Plain root with no `SUDO_USER` (a cloud VM
logged in as root) keeps a root-owned install, as before.

**D3. No shell-profile edits in system scope.** `PIXI_NO_PATH_UPDATE=1`, because the launcher wrapper
sets PATH. Juliaup already runs with `--add-to-path=no`.

**D4. Stacked Julia depot for every launch of a shared install.** The launcher sets
`JULIA_DEPOT_PATH=<per-user writable>:<install>/juliaup/depot:`. The trailing empty entry restores
Julia's bundled stdlib depot. The per-user depot is `~/.cecelia/julia-depot` and only receives
something if a shared cache is stale. Install-time precompile still writes into the shared depot.
Windows uses `;` and the same shape.

**D4b. The system launcher runs `pixi run --as-is app`** (`--frozen --no-install`), so it never
takes the env's write lock. The installer provisions the env, and updates re-run the installer.

**D5. In-app update stays refused for system scope.** Even the owner gets "re-run the installer".
Another account's launcher cannot apply a staged update into a dir it cannot write, so letting the
owner stage one through the app would leave it half-applied for everyone else.

**D6. Uninstaller: `uninstall.sh` / `uninstall.ps1`, shipped in the bundle and at the raw URL.**

    sh ~/.local/share/cecelia/uninstall.sh                         # or
    curl -LsSf …/uninstall.sh | sh                                 # (CECELIA_INSTALL_SCOPE=system for /opt)

| What | Default | Flag |
|---|---|---|
| Install dir (app, env, shared Pixi/Julia in system scope), menu entry / `.app` / Start Menu shortcut, macOS launcher logs, `~/cecelia-connection.json`, the `cecelia-observer` MCP registration | **removed** | — |
| Settings `~/.cecelia` (custom.toml, profiles incl. Claude logins, models, custom modules, TLS cert) | **kept** | `--wipe-settings` / `CECELIA_WIPE_SETTINGS=1` |
| Projects (each `<projects>/<uid>/` holding a `project.json`) | **kept** | `--wipe-projects` / `CECELIA_WIPE_PROJECTS=1` |

- **Interactive by default.** On a terminal it shows exactly what it found, with sizes, and asks once
  per data class (default No). Deleting projects needs `delete` typed in full. `--yes` skips the
  questions and uses only the flags, for scripted use.
- **Projects are removed project by project, never the whole folder.** The projects dir is
  user-chosen (it could be `~/Documents`), so the uninstaller deletes only subdirs that contain a
  `project.json`. It removes the folder itself only if it is empty afterwards. The path comes from
  `[dirs] projects` in `~/.cecelia/custom.toml`, with `~` expanded.
- **Shared tools are never removed:** `~/.pixi`, `~/.juliaup`, `~/.julia`, `~/.cellpose`, `~/.cache/*`,
  `~/.claude*`. Other software uses them. The summary lists the ones present with their sizes, so
  "completely" is an informed choice.
- **Refuses while Cecelia is running.** It looks for a julia/python/pixi/node/java process whose
  working dir or binary is under the install dir. It doesn't read `cecelia.lock`: that lock is per
  user, not per install, and a stale pid there would block for no reason.
  Deleting a live env would corrupt in-flight writes.
- **System scope:** removes the shared install and the all-users entry (using sudo as in D1), plus the
  invoking user's own data per the flags. Other accounts' `~/.cecelia` and projects are never touched.
  Each user removes their own data with `--data-only`.

## Phases

1. **Installer owner model (D1–D4)** in `install.sh`; D4 in `install.ps1` too. Docs: INSTALL.md,
   SHIPPING.md → *Install scope*.
2. **Uninstallers (D6)**: `uninstall.sh` + `uninstall.ps1`, the release tar + assets, and the
   `bundle_required_paths.txt` entry. Docs: INSTALL.md → *Uninstall*.
3. **Verification in a `bwrap` sandbox:**
   - a full system-scope install as a non-root owner;
   - another uid launches the server against a read-only install;
   - uninstall with each flag combination against fixture data;
   - `/root` / `HOME` checked for leftovers.

## Verified (2026-10-06, `bwrap`, Linux)

- **Install:** a full v0.2.10 system install as a non-root owner, with `/opt` faked. It exited 0. The
  owner's home ended up holding only the package cache: no rc edits, no `~/.julia`. Without sudo,
  the menu entry falls back to the printed launcher path.
- **Launch as another uid:** read-only install, fresh HOME, no network. `/api/health` answered in
  10 s, and `~/.cecelia/julia-depot` stayed empty.
  - The control launch with the old `JULIA_DEPOT_PATH` died after 180 s with `EROFS` on
    `Cecelia/*.ji.pidfile`.
  - The control launch without `--as-is` died immediately on the pixi lock.
- **`uninstall.sh`:** keep/wipe flag combinations, the interactive confirm, a stray file in the
  projects dir surviving, refusal with no terminal, refusal while running, refusal on a git checkout.
- **`uninstall.ps1`:** the same cases under pwsh on Linux, with fake profile dirs.

## Not verified here

- Pluto notebooks for a non-owner of a shared install. Notebook setup and the sysimage build write
  under `<install>/pluto`, which is outside the per-user depot, so they will most likely fail for
  everyone but the owner. Optional pixi envs (Settings → System) are now refused with a 403 for
  non-owners (`api/src/system_api.jl`).

- macOS and Windows: there is no box. `uninstall.ps1` is authored and parse-checked only.
- The sudo/`SUDO_USER` hand-over, because `sudo` cannot run inside the sandbox. The non-root path it
  delegates to is the one that gets verified.
