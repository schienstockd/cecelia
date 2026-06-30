# Development workflow

How we change this repo: branches, commits, pull requests, releases. For *what* the code
does see the per-area docs linked from [`CLAUDE.md`](../CLAUDE.md); for the dev loop and
testing commands see the **Development** and **Testing** sections of `CLAUDE.md`.

Repository: `git@github.com:schienstockd/cecelia.git` (default branch **`main`**).

## Golden rule — never commit or push to `main`

`main` is protected by convention. **All work lands via a feature branch + pull request**, even
docs and one-line fixes. Never `git commit`/`git push` directly onto `main`. Releases are tagged
off `main` *after* the PR has merged (see below).

Agents (Claude Code): **commit or push only when the user explicitly asks.** If the current
branch is `main`, branch first.

## Branches

Branch off the latest `main`, named with a conventional-commit-style prefix matching the change:

```
feat/<short-slug>      # new feature        e.g. feat/leiden-clustering
fix/<short-slug>       # bug fix            e.g. fix/track-id-nan
docs/<short-slug>      # documentation      e.g. docs/ci-badges
chore/<short-slug>     # deps, tooling, infra  e.g. chore/drop-pythoncall
refactor/<short-slug>  # behaviour-preserving cleanup
```

```bash
git switch main && git pull
git switch -c feat/<short-slug>
```

Keep a branch scoped to one logical change. Don't pile an unrelated fix onto a branch that
already has someone else's work in progress — branch again.

## Commits

Conventional-commits style, matching the existing history (`feat(import): …`,
`docs(readme): …`, `chore: …`):

```
<type>(<scope>): <imperative summary>
```

`type` ∈ `feat | fix | docs | chore | refactor | test | perf`. Scope is optional but
encouraged (`import`, `update`, `gating`, …).

When a commit is authored by Claude Code, end the message with the trailer:

```
Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>
```

Ship the test in the same commit as the code (see `CLAUDE.md` → **Testing**), and update the
relevant doc in the same change (see `CLAUDE.md` → the *Keep the docs current* table).

## Pull requests

Open a PR against `main` for review; **Dom reviews and merges** (PR #1 merged this way).

- The `gh` CLI is **not installed in the agent environment**. An agent therefore **pushes the
  branch and relays the PR-creation URL** (the `https://github.com/schienstockd/cecelia/pull/new/<branch>`
  link printed by `git push`) for Dom to open — it does not attempt `gh pr create`.
- End PR bodies (when an agent drafts one) with:

  ```
  🤖 Generated with [Claude Code](https://claude.com/claude-code)
  ```

```bash
git push -u origin feat/<short-slug>
# relay the "Create a pull request" URL git prints
```

## CI

Every push/PR runs `.github/workflows/ci.yml` (smoke: fresh checkout → `pixi install` →
`julia … instantiate` → frontend build → server serves `/api/health` + the frontend). Keep it
green before requesting a merge. See `docs/SHIPPING.md` for the release pipeline.

## Releases

Cut **off `main`** after the relevant PRs have merged, by pushing a tag:

- `v*` tag → `.github/workflows/release.yml` builds the OS-independent `cecelia.tar.gz` bundle
  and publishes a GitHub Release with the install scripts.
- **Hyphenated tags are prereleases** (`v0.1.0-rc1`); a clean `vX.Y.Z` is the public release that
  makes the `releases/latest` install one-liner resolve.

Rationale and the full packaging/update model live in [`docs/SHIPPING.md`](SHIPPING.md).
