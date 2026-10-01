# Automated dependency updates

**Status:** in-progress on `chore/deps-updates` (2026-10-01).

## Goal

Dependency refreshes arrive as PRs on a schedule, with CI on them, instead of happening by hand when
someone remembers. Four dependency surfaces exist:

| Surface | Lock | Owner after this plan |
|---|---|---|
| npm | `frontend/package-lock.json` | Dependabot |
| GitHub Actions | `uses:` refs in `.github/workflows/*.yml` | Dependabot |
| Pixi (conda + PyPI) | `pixi.lock` | `update-deps.yml` (scheduled workflow) |
| Julia | `app/`, `api/`, `pluto/` `Manifest.toml` | `update-deps.yml`, same PR as pixi |

## Decisions (2026-10-01)

1. **Dependabot for npm and Actions only.** Dependabot has no pixi support. Its `conda` ecosystem
   does not read `pixi.toml` or `pixi.lock`.
2. **Julia is not on Dependabot.** `api/` and `pluto/` path-source `Cecelia` from `../app`
   (`[sources]`), so the three manifests have to be resolved together. Dependabot treats each listed
   directory separately and opens one PR per dependency, and its announcement warns that this
   "may not fully test the change due to resolver conflicts". It also bumps `[compat]`, which we
   don't want automated. Instead, `Pkg.update()` runs in app → api → pluto order inside the workflow.
3. **Pixi and Julia go in one PR (`update-deps`).** Both are "refresh the lock within the declared
   ranges", both need the full CI matrix, and both change what a release installs. One PR means one
   matrix run.
4. **Monthly, grouped.** Every PR runs 3 OS × 3 jobs, including torch. Each lock change also adds new
   pixi caches against a 10 GB cap that is already about 6.4 GB full (see the `ci.yml` header).
   Dependabot puts minor+patch updates in one group per ecosystem. Majors stay individual so that
   one breaking upgrade can't block the others.
5. **No PAT.** When `GITHUB_TOKEN` opens a PR, its `pull_request` runs start in an
   *approval-required* state (GitHub docs, `GITHUB_TOKEN` → triggering workflows). Approve them from
   the PR banner. Dependabot PRs trigger CI normally.
6. **The update workflow uses CI's pixi version (`v0.71.1`).** A lock written by a newer pixi may not
   be readable by CI's. Bumping the pixi version stays manual, and so does Julia (`1.12`).
7. **Ranges stay manual.** A lock refresh never moves `>=` floors, `<` caps (`squidpy <1.9`,
   `skan <0.14`), the `coastal` git `rev`, or Julia `[compat]`. These are deliberate edits.
8. **Majors come in; no blanket caps.** Most `pixi.toml` specs are bare floors, so the refresh
   brings in majors (the first dry run: pandas 3, mcp 2, trimesh 5, websockets 17, anndata 0.13).
   Dominik: *"I'm fine to bring in majors. we just need to review those carefully and the failure
   modes in ci. i dont want to get stuck in old versions."* A cap is a temporary hold only, and it
   needs a matching `docs/TODO.md` item (`docs/DEV.md` → *Dependency updates* → *When CI goes red*).
9. **The PR body leads with what to review.** Pixi: explicit-dep majors, downgrades and 0.x minor
   bumps, pulled from the collapsed `pixi-diff-to-markdown` tables. Julia: downgrades,
   semver-breaking bumps, and packages the envs carry at different versions. Then everything
   **held back**: pixi `<` caps and `*` pins, plus Julia direct deps marked ⌅ by
   `Pkg.status(outdated=true)`. This list is the guard against falling behind. `pixi upgrade
   --dry-run` was tried as the pixi held-back signal and rejected: it loosens deliberate caps (it
   put cellpose 4 into the `cellpose-v3` env) and fails outright unless Python is excluded.

## Constraints found while planning

- **Repo setting.** *Settings → Actions → General → "Allow GitHub Actions to create and approve pull
  requests"* is currently **off** (`can_approve_pull_request_reviews: false`). The workflow can't
  open its PR until this is on. This is Dominik's call.
- **Dependabot PRs get a read-only token,** so `julia-actions/cache` can't prune old depot caches on
  those runs. The failure is non-fatal: `handle_caches.jl` logs it and the job continues. Caches pile
  up on the PR ref until LRU eviction. Monthly grouping keeps this to a handful.
- **`pixi update` installs on the runner.** The workspace has PyPI deps, so `--no-install` doesn't
  apply. The solve installs the default env, including torch cu124, on `ubuntu-latest`.
- **Julia envs disagree, structurally and after a refresh.** Pluto.jl caps `pluto/` at HTTP 1.x
  while `app/` is on 2.x, and that's on `main` today. A local `Pkg.update()` also *downgraded*
  `pluto/`'s HDF5_jll (2.1.2 → 1.14.3) and OpenMPI_jll (5 → 4.1.9), while `app/` moved HDF5_jll up
  to 2.2.2. `app/` and `api/` ended the run in exact agreement. The drift section reports this each
  month instead of hiding it.
- **Python 3.13+ is blocked by the cu124 torch index**, which has no `cp314` wheels for torch 2.6+
  (found via `pixi upgrade --dry-run`). Lifting `python = "3.12.*"` means moving the CUDA index first.
- **Dependabot security alerts are off** on the repo. Version updates (this plan) don't need them.
  Turning them on is a separate setting.

## Phases

- **P1 — Dependabot.** Add `.github/dependabot.yml` with npm (`/frontend`) and github-actions (`/`),
  monthly, with a minor+patch group each.
- **P2 — Update workflow.** Add `.github/workflows/update-deps.yml`: monthly cron plus
  `workflow_dispatch`. It runs `pixi update --json | pixi-diff-to-markdown`, then `Pkg.update()` in
  app/api/pluto, then `scripts/julia_manifest_diff.jl` (old→new versions per env, as markdown), then
  `peter-evans/create-pull-request` on branch `update-deps`.
  Checkpoint: the diff script runs locally against a real `Pkg.update()`, and
  `pixi update --dry-run --json | pixi exec pixi-diff-to-markdown` renders. **Done 2026-10-01**:
  both run, `actionlint` and the Dependabot schema check pass, and the PR body comes to about 40 KB
  (GitHub's limit is 65 KB).
- **P3 — Docs.** **Done 2026-10-01.** Added `docs/DEV.md` → *Dependency updates*: the bots, how to
  review, and *When CI goes red*. Added a `docs/RELEASING.md` checklist note: merge a pending
  `update-deps` PR before cutting a release, never as part of it, because the lock is what the
  release installs. `docs/SHIPPING.md` is unchanged, since its update table covers user-side update
  paths, not repo maintenance.
- **P4 — After merge (Dominik).** Flip the repo setting, run the workflow once with
  `workflow_dispatch`, and approve CI on the PR it opens.
