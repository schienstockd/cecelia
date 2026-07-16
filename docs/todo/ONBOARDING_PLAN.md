# First-Launch Setup Wizard + Onboarding — PLAN

Real users are now installing Cecelia. The first wall is the `custom.toml` config file: a
non-terminal user cannot get past it. This phase removes that wall and adds just enough in-app
guidance to get a new user from install → first image without reading docs.

**Scope framing.** This is a *UX* phase, but it is **not** zero-code in the backend: it relocates
config resolution, adds a `setup_required` API state + a `/api/setup/*` surface, and reuses the
already-built shutdown route. No new *subsystems* — but the config-resolution change is load-bearing
and ships with tests (same reflex as the `ccid.json` round-trip rule in CLAUDE.md).

Reference: `CLAUDE.md`, `docs/ARCHITECTURE.md`, `docs/SHIPPING.md`, `app/src/config.jl`,
`api/src/app_api.jl` (shutdown already exists).

---

## Locked decisions

### D1 — Config lives at a fixed per-user location; dev overrides via `.env` (THE coordination)

This is the load-bearing decision and the one that must be right. Today `app/src/config.jl`
resolves `custom.toml` inside a `dev_dir` chosen by: explicit arg → `CECELIA_DEV_DIR` env →
`.env` file → default `~/cecelia`. The onboarding prompt's earlier claim that "Julia already looks
in the home dir, no code change needed" was **wrong** — that default is `~/cecelia` (a dev dir), not
a fixed home location, and nothing writes it for a real user.

**Decision: one resolver, dev overrides, prod falls through.** Keep the exact resolution *order*,
change only the final fallback and factor the path out:

```
config_dir() resolution (first wins):
  1. explicit arg to init_cecelia!(...)          — tests / REPL
  2. CECELIA_DEV_DIR env var                      — dev, CI
  3. CECELIA_DEV_DIR in cecelia-pineapple/.env    — dev checkout (gitignored)
  4. ~/.cecelia            ← CHANGED (was ~/cecelia)   — the installed-app default
```

The **presence of `.env` / `CECELIA_DEV_DIR` is the dev signal.** A dev checkout has `.env`
(gitignored, machine-specific); an installed app has neither, so it falls through to `~/.cecelia`.
Consequences that make this the right shape:

- **No install-scope detection anywhere.** User vs system-wide install does not change the config
  path — it is always per-user `~/.cecelia/custom.toml`. Julia startup never needs to know how the
  app was installed.
- **Dev config stays isolated from a developer's own "real" config.** A dev running the app never
  clobbers `~/.cecelia`; their `.env` points elsewhere. This is the coordination guarantee.
- **The setup wizard writes to the *resolved* path**, not a hardcoded `~/.cecelia`. In prod that is
  `~/.cecelia/custom.toml`; in dev it is the dev dir. One function answers "where is my config" for
  both the reader (`init_cecelia!`) and the writer (setup endpoint) — they can never disagree.

**Extract `config_dir()` / `custom_toml_path()`** as the single source of truth; `init_cecelia!`,
the `setup_required` check, and `/api/setup/init` all call it. Do not re-derive the path in three
places (that is the divergent-re-implementation trap CLAUDE.md warns about).

Also fix the one stale hardcoded pointer: `app/src/tasks/importImages/omezarr.jl:464` still tells
users to edit `~/cecelia-pineapple/dev/custom.toml` — update it to reference the resolved path.

### D2 — One config path on all platforms: `~/.cecelia/custom.toml`

`~/.cecelia/` on Linux/macOS **and** `%USERPROFILE%\.cecelia\` on Windows (`expanduser("~")` already
resolves both). A single dotfolder name = one mental model; Windows simply doesn't hide it
(cosmetic only). No `.cecelia` vs `cecelia` asymmetry. Projects dir default offered by the wizard:
`~/cecelia-projects` (a normal, visible folder the user will browse to — deliberately *not* hidden,
unlike config).

### D3 — Config hot-reloads; restart is the fallback, not the default

`_CONF[]` is a `Ref` and every accessor (`projects_dir()`, …) reads it live, so `/api/setup/init`
calls `init_cecelia!()` to reload in place — no restart needed for the common path. Keep
`restart_required: true` in the response contract as a fallback the frontend already handles (poll
`/api/health`), but the expected path is hot-reload.

### D4 — System-wide install ✅ DONE (single script, `CECELIA_INSTALL_SCOPE=system`)

Built into the existing `install.sh` / `install.ps1` (one script, scope via env — *not* a second
`install-system` script, to avoid doubling the matrix). System scope installs to `/opt/cecelia` ·
`/Applications/cecelia` · `%ProgramFiles%\cecelia`, provisions Pixi + Juliaup + the multi-GB env
*inside* the install dir (shared runtime via `PIXI_HOME`/`JULIAUP_DEPOT_PATH` + a launcher wrapper),
writes a `.cecelia-scope` marker, and installs an all-users menu shortcut. Config + projects stay
per-user for free (D1). Updates are admin-only (see D5). Full model + **verification status** (the
system path is authored-not-verified — no Windows/multi-user box here): `docs/SHIPPING.md` → *Install
scope*.

### D5 — In-app update ✅ DONE (reused existing backend; added badge + admin-note)

The update backend (`/api/update/check` + `/api/update/apply` staging, applied by `app.py` on restart)
and the Settings → Software UI **already existed** — so this did **not** rebuild them. It added: (1)
scope awareness — `_install_scope()` on `/api/update/check`; apply returns 403 on a `system` install;
(2) an app-wide **header badge** surfacing the check (click → Settings; × = remind-me-later, session);
(3) the **admin note** shown instead of the Update button on a system install. Update state is
centralised in the `appControl` store so the badge and Settings share one check (no duplicate fetch).

### D6 — No native directory picker; validated free-text path

A browser cannot open a reliable native folder picker. The wizard uses a **free-text path field**
validated server-side (exists-or-creatable + writable). If we add a `[Browse]` affordance it is a
server-driven directory lister (`GET /api/setup/browse?path=`), not an OS dialog — but v1 is plain
text + validate. The mockup must not imply an OS file dialog.

---

## Build sequence

### P1 — Config resolution: relocate + factor out (backend, tested)

- `app/src/config.jl`: extract `config_dir()` (resolution order above; default `~/.cecelia`) and
  `custom_toml_path() = joinpath(config_dir(), "custom.toml")`. `init_cecelia!` uses them.
- Update `omezarr.jl:464` error text to the resolved path.
- **Tests (`app/test/`)**: resolution order — explicit arg wins; `CECELIA_DEV_DIR` beats `.env`;
  neither present → `~/.cecelia`; `custom_toml_path` composes correctly. This is core config
  behaviour, so it ships with the change.

### P2 — First-launch detection + setup wizard

- **Detection**: on API startup, `setup_required = true` when `custom_toml_path()` is missing **or**
  `dirs.projects` is missing / equals the `/path/to/projects` placeholder / is not a valid dir.
  Expose in existing app state (e.g. `/api/health` or `/api/diagnostics`).
- **Routing**: frontend redirects to `/setup` when `setup_required`.
- **Endpoints**:
  - `POST /api/setup/init { projects_dir }` → validate, create dir if needed, write
    `custom_toml_path()` with `[dirs] projects = ...`, call `init_cecelia!()` to reload, return
    `{ ok, restart_required }`.
  - (optional) `GET /api/setup/validate?path=` → `{ ok, creatable, writable, message }` for live
    field feedback.
- **The setup page** — one screen, no tabs:

  ```
  Welcome to Cecelia 🍍

  Where would you like to store your projects?

  [ ~/cecelia-projects                    ]   ← validated free-text (D6)

  [Get started →]
  ```

  Pre-filled `~/cecelia-projects` (`%USERPROFILE%\cecelia-projects` on Windows). "Get started"
  validates, creates, writes config, reloads, redirects to the app. On `restart_required`, show
  "Restarting…" and poll `/api/health`.

### P3 — Empty states

- **No projects**: "No projects yet / A project holds all your images and analysis for one
  experiment." `[ + Create your first project ]`. Default name "My first project", renamable, don't
  block on naming.
- **No images**: "No images in this project / Import your first image to get started. Cecelia
  supports OME-ZARR, CZI, LIF, ND2, and most microscopy formats (anything bioformats2raw reads)."
  `[ + Import image ]`.

### P4 — Contextual hints (first-use only)

One-line callouts on first visit to each major section; dismiss permanently on click; stored in
`localStorage` per hint ID. Not a tour, not a modal.

- Any page, first launch: "When you're done, use the Shut down button — not the browser tab — to
  stop Cecelia cleanly."
- Segmentation: "Run segmentation to detect cells before gating or tracking."
- Gating: "Draw a gate by clicking polygon vertices, or drag to draw a rectangle."
- Tracking: "Tracking links cells across timepoints. Run segmentation on all timepoints first."

### P5 — Shutdown: educate, don't add a third button

Shutdown is **fully built already**: `POST /api/app/shutdown` (`api/src/app_api.jl`) plus two
user-facing controls that call it via `appControl.quit()` — the **sidebar footer Quit (bottom-left)**
and the Settings → System panel. The bottom-left button is the convenient, primary one. Adding a
*third* control in the header would be redundant (and the divergent-re-implementation trap), so we
**don't**. The onboarding gap is purely *education*: new users close the browser tab and leave the
server running.

So P5 is just the first-use hint (P4): "When you're done, use the Quit button (bottom-left) — not the
browser tab — to stop Cecelia cleanly." No new shutdown UI.

*Update badge is reserved but not built here — see D5.*

---

## Verify

- Dev checkout (`.env` present): config still resolves to the dev dir; wizard writes there; a
  developer's `~/.cecelia` is untouched.
- Fresh install (no `.env`, no `custom.toml`): `setup_required = true`, browser opens `/setup`.
- Setup wizard: creates the projects dir, writes `~/.cecelia/custom.toml`, hot-reloads, redirects.
- Re-open after setup: goes straight to the app (no `/setup`).
- `CECELIA_DEV_DIR` overrides the `~/.cecelia` default (dev/CI).
- Invalid / non-writable projects path: wizard shows a clear error, does not write config.
- Empty project / empty image states render.
- First-use hints appear once and stay dismissed.
- The shutdown hint points at the existing bottom-left Quit button (no new control added).
- Network-share `dirs.projects` path works (labs point all users at one location — no extra setup).
- `CECELIA_INSTALL_SCOPE=system` installs to the shared location, writes `.cecelia-scope`, and shares
  one runtime; config + projects stay per-user. *(system path unverified on any multi-user box —
  Linux/macOS/Windows all apply; macOS is the primary target)*
- Update badge appears when a newer release exists; a `user` install offers Update, a `system` install
  shows the admin note, `/api/update/apply` returns 403 on `system`.

## Out of scope

- Multi-project-folder support; user accounts / auth; any analysis-workflow change.

## Docs to update in the same change

- `docs/SHIPPING.md` — config now at `~/.cecelia/custom.toml`; the setup-wizard first-run path; note
  system-wide install as the deferred follow-on.
- `docs/INSTALL.md` — new users don't hand-edit `custom.toml`; the wizard writes it.
- `docs/UI.md` (or a short onboarding note) — `/setup` route, empty states, first-use hint pattern.
- `CLAUDE.md` "Dev dir config" section — document the dev(`.env`)-vs-prod(`~/.cecelia`) resolver.
