# Cecelia Feijoa — Releasing

*When* to cut a release and *what a version means* — the policy. For the tag **mechanics** (branch +
PR, tag off `main`, prerelease suffix) see [`docs/DEV.md`](DEV.md) → *Releases*; for the build/update
**pipeline** see [`docs/SHIPPING.md`](SHIPPING.md); for the durable ledger of coarse shippable states
see [`docs/MILESTONES.md`](MILESTONES.md).

## The shape of this project

Development is **solo and demand-driven**: the maintainer dogfoods Cecelia on his own images and fixes
things as he hits them, plus the occasional ad-hoc request ("here's my image, make it do X" → implement
→ ship). There is **no schedulable feature roadmap** to release against. So the cadence is a
**time-boxed heartbeat + event triggers** — not "release when phase N is done".

Two channels already carry this (see SHIPPING.md):

- **dev channel = `main` HEAD.** Every merged PR is immediately available to anyone on dev. New
  features do **not** need a tag to reach testers — so *"we added a bunch of stuff"* is **not** a
  reason to release.
- **stable channel = tagged release.** A tag is for people who need a **fixed point** they can install,
  roll back to, or cite.

## Three things, one mechanism

A `v*` tag fires `release.yml` (bundle + GitHub Release), so a tag *is* a release event. The
conceptual split:

| | What it is | When |
|---|---|---|
| **rc tag** `v0.1.0-rcN` | a snapshot to install / roll back to (GitHub *prerelease*) | cut liberally — the heartbeat, and before risky work |
| **release** `v0.1.0` | the version you point people at (onboard, cite, demo from) | when someone's work will depend on it |
| **milestone** (M-entry) | a coarse "shippable state" note in MILESTONES.md | only at big boundaries (a capability lands, v1.0 freeze) — **not** every tag |

## The semi-schedule (heartbeat)

**Cut a tag roughly every ~2 weeks** off green `main`, *if* there are meaningful new commits (skip a
cycle if nothing changed). This is the whole schedule. It gives you:

- a **rollback anchor** that's never more than ~2 weeks / a handful of commits stale, and
- a fresh **stable** channel without having to think about it.

> Why a heartbeat and not "tag when a feature lands": the stable↔dev gap once grew to **152 commits**
> past a tag with no anchor. A calendar heartbeat prevents that regardless of how lumpy the feature
> flow is.

**Event triggers** fire a tag *off-cycle*, on top of the heartbeat:

- **an ad-hoc request delivered** → tag it and hand that person that exact version;
- **before onboarding a new user** → so they don't start on a moving target;
- **before a talk / demo** → cut the demo build, then **freeze features** and fix only demo-blockers;
- **before/after a risky refactor** → a known-good bracket to roll back to.

## Versioning (pre-1.0)

- `0.1.x` — "works end-to-end for me" (current state). Patch bumps ride the heartbeat.
- `0.2`, `0.3` — something **substantial** lands (e.g. clustering, behaviour hardening).
- `1.0.0` — R-parity **and** a couple of external people use it without hand-holding.

**Hard blocker before *any* external handoff:** a root `LICENSE` (GPL-3-or-later) + third-party
acknowledgements (celltrackR GPL-2) — TODO #00060. Fine to skip for personal rc's; required the moment
someone else installs it.

### One-time constraint: everything shipped so far is stuck behind `v0.1.0-rc9`

`api/src/update_api.jl` compares tags with Julia's `VersionNumber`, which parses `rc10`'s prerelease
as the single **string** `"rc10"` — and `"rc10" < "rc9"` lexicographically. `_parse_ver` now rewrites
`-rc10` → `-rc.10` so the digits compare numerically, **but that fix only helps clients that already
have it.** Anyone running `v0.1.0-rc9` compares with their own copy of the old function, so:

| Next tag | Reaches an rc9 client? |
|---|---|
| `v0.1.0-rc10` (…and rc11 … rc19) | **no** — sorts *below* rc9; the GUI says "up to date" |
| `v0.1.0` / `v0.2.0` / any release | **yes** — a release always outranks its own prereleases |
| `v0.1.0-rc9.1` | yes, but it is an unreadable version to hand anyone |

Worse, an rc8 client offered the "newest" release is sent to **rc9** and then dead-ends there, so
rc9 is a terminal state for every existing install until a tag outranks it.

**So the next tag must be a plain release** (`v0.1.0` or higher). After that the ladder is safe and
`-rcN` works normally again. This also fixes `releases/latest`, which only ever resolves to a
non-prerelease and has therefore never worked (`docs/SHIPPING.md` → *Install channels*).

## Cutting a release — the short checklist

1. CI matrix green on `main` (all three OSes).
2. Decide the version (heartbeat patch vs substantial minor; `-rcN` if you'll soak before announcing).
3. Draft release notes (features / fixes / infra since the last tag — `git log <lasttag>..HEAD`).
   The same log feeds **`CHANGELOG.md`**: rename its `[Unreleased]` block to the new version + date
   and paste the notes in (see snippet below). Don't hand-maintain `[Unreleased]` between releases —
   it's generated here, so it can't silently drift from what actually shipped.
4. Bump the `version:` + `date-released:` in **`CITATION.cff`** to this tag.
5. Tag off `main` and push (`release.yml` builds + publishes). Hyphenated tag = prerelease.
6. If it's a demo/onboarding/external build: verify the published artifact **installs clean** on a
   fresh machine + the target dataset before relying on it.
7. Only at a coarse boundary: add a MILESTONES entry (append-only).

### Regenerating the CHANGELOG section

The `CHANGELOG.md` `[Unreleased]` block is filled in *at release time* from the commit log, so it
never has to be maintained by hand between tags. Grab the one-liners since the last tag:

```sh
git log $(git describe --tags --abbrev=0)..HEAD --oneline --no-merges
```

Rename `## [Unreleased]` to `## [<version>] — <YYYY-MM-DD>`, drop the grouped notes under it
(Added / Changed / Fixed), add a fresh empty `## [Unreleased]` above it, and update the compare
links at the bottom of the file. This is the same content as the GitHub Release — the CHANGELOG is
just its offline-readable, in-repo mirror.

## Overdue check

Being solo, it's easy to let the heartbeat slip. Rule of thumb: if it's been **> ~2 weeks since the
last tag and `main` has meaningful new commits**, you're overdue — cut one.

```sh
git describe --tags            # last tag
git log $(git describe --tags --abbrev=0)..HEAD --oneline | wc -l   # commits since
```
