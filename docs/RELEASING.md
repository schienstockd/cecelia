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

Two install channels exist (see SHIPPING.md), but **only one of them has users**:

- **stable channel = a tag.** This is how *every* real user installs. Biologists do not install from
  a branch, and there is no reason they should — the dev channel needs Node on PATH and builds the
  frontend locally.
- **dev channel = `main` HEAD.** This is for **people who want to work ON Cecelia** — contributors
  tracking HEAD, not people using it to analyse images. **It has no users today.** That may change
  if the project picks up contributors; if it does, they are still developers, not the audience a
  release is cut for.

**The consequence, and it is the one that matters:** *a tag is the only thing that reaches a user.*
Nothing merged to `main` is available to anyone doing science with Cecelia until it is tagged. So
untagged work is not "already shipped to testers" — it is undelivered, and the gap grows with every
merge.

> This paragraph used to say the opposite — that new features "do not need a tag to reach testers", so
> *"we added a bunch of stuff"* was not a reason to release. That inference rested on a dev channel
> nobody uses, and it produced the wrong call in review at least once: an accumulated 281-commit gap
> was waved off as already-delivered when in fact no user could see any of it. If dev ever does pick
> up contributors, that still doesn't revive the argument — a contributor on HEAD is not someone a
> release is cut for.

## Three things, one mechanism

A `v*` tag fires `release.yml` (bundle + GitHub Release), so a tag *is* a release event. The
conceptual split:

| | What it is | When |
|---|---|---|
| **release** `v0.1.0` | **the default.** A version a user installs and can name | the heartbeat, and any fix a user is waiting on (bump the patch) |
| **rc tag** `v0.1.0-rcN` | a build you intend to **soak before promoting** | bracketing a risky refactor; a candidate you will re-cut as a release |
| **milestone** (M-entry) | a coarse "shippable state" note in MILESTONES.md | only at big boundaries (a capability lands, v1.0 freeze) — **not** every tag |

**Prefer a plain release.** The rc ladder was the default for nine tags and never converged, because
none of them were candidates *for* anything — they were snapshots wearing a candidate's label, while
being the only thing users could install. A prerelease also flags "don't rely on this" to the exact
people relying on it, and GitHub's `releases/latest` never resolves while every tag is a prerelease.
If a user hits a bug you have already fixed, that is a **patch release** (`v0.1.1`), not an rc.

### An rc is not the quiet option — both channels ship it

The distinction is **intent, not size**. Nothing mechanical makes a prerelease safer, and it is worth
knowing exactly what `-rcN` does and does not change before reaching for it.

| Tag | in-app updater | `install.sh` one-liner |
|---|---|---|
| `v0.1.1` | offered | installed |
| `v0.1.1-rc1` | **offered** | **installed** |

- **The updater does not filter prereleases.** `api_update_check` (`api/src/update_api.jl`) skips only
  *drafts* and takes the max by `VersionNumber`. A prerelease sorts below its own release but *above* the
  previous one, so `v"0.1.1-rc1" > v"0.1.0"` — every install already on 0.1.0 is offered the rc.
- **Neither does the installer.** `install.sh` deliberately does **not** use `releases/latest` (that
  404s while every tag is a prerelease, which was true for nine tags). It calls `/repos/…/releases` and
  takes the first `tag_name` — the newest *published* release, prereleases included.

So `-rcN` buys you the GitHub prerelease label and a "don't rely on this" signal, and nothing else: the
build still goes to everyone, both to existing installs and to the next person who runs the one-liner.
Two consequences:

1. **An rc is not a safety valve.** If the hesitation is "this might break someone", a prerelease does
   not achieve that. Cut nothing, or cut a release you are willing to stand behind.
2. **An rc must actually be promoted.** It only earns the label if re-cutting it as `vX.Y.Z` is a real
   plan with an end — otherwise it is another snapshot wearing a candidate's label, which is exactly how
   the nine-tag ladder happened.

**Wanting a rollback anchor before a risky change is a plain release cut *before* the merge** — already
one of the event triggers below — not a candidate published after it.

> **Latent inconsistency, if you ever backport.** The two paths order releases differently: the updater
> takes the max **by version**, the installer takes the newest **by date**. Publish `v0.2.0` and then a
> `v0.1.2` patch off an older line, and a new install gets `v0.1.2` while an existing one is offered
> `v0.2.0`. Nothing today does this — pre-1.0 the heartbeat only ever moves forward — but the two are not
> the same rule, and only one of them is version-aware.

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
- `1.0.0` — the module functions **this** work needs are in, and a couple of external people use it
  without hand-holding.

> **1.0 is not R-parity, and deliberately so.** This used to read "R-parity and a couple of external
> people…", which set a bar nobody wants to clear: the old R app carries ~75 module functions across
> flow-cytometry import and gating sets, pixel/object classification, N2V model training, signal-peak
> analysis, HPC dispatch, and seven segmentation backends. This stack has ~25 and covers the spine —
> import → segment → track → measure → gate → behaviour → cluster → spatial — plus a good deal the R
> version never had (QC + cohort QC, chains, the analysis canvas, notebooks, the MCP observer, project
> bundles). Chasing the remaining count would mean porting things the maintainer does not use.
>
> So the 1.0 bar is **fitness for the science being done with it**, not feature-count equivalence, and
> a missing R function is a request-driven port (or a deliberate non-goal in `docs/FUTURE.md`) rather
> than a debt against 1.0. If a specific absent capability ever blocks real work, that's a TODO item
> on its own merits.

**Hard blocker before *any* external handoff:** a root `LICENSE` (GPL-3-or-later) + third-party
acknowledgements (celltrackR GPL-2). **Satisfied** — `LICENSE`, `THIRD_PARTY.md`, and the
`license` key in `app/Project.toml` are all in place.

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
3. **Write the `CHANGELOG.md` section — this IS the release body, so it must land before the tag.**
   Rename `[Unreleased]` to the new version + date and write the notes in (see snippet below).
   `release.yml` extracts that section (`scripts/changelogSection.mjs`) and publishes it as the
   GitHub Release body; a **missing section fails the release build** rather than shipping an empty
   one. Don't hand-maintain `[Unreleased]` between releases — it's written here, so it can't silently
   drift from what actually shipped.

   > This used to be `generate_release_notes: true` — GitHub's auto PR list. That list is also what
   > the in-app **What's New** modal renders, since the update check passes the release body through
   > as markdown, and 450 lines of `* title by @user in #N` told a user nothing they could act on
   > (Dominik, 2026-08-10). Write for the person opening that modal: what changed for them, and what
   > they now have to decide. The commit log is one click away on the compare link appended below.
4. Bump the `version:` + `date-released:` in **`CITATION.cff`** to this tag.
5. **`bash scripts/bundle_check.sh --launch`** — packs the bundle `release.yml` will pack, extracts
   it, and boots the API server from it on `:8099` (~20 s, leaves the running app alone). The tar
   list is an allow-list, so a runtime directory nobody named is missing from the *stable* channel
   only, and the `dev` channel keeps working — which is how v0.1.1 shipped without `pluto/` and died
   at launch for every installer (#540). CI pins the same list, but this is the one that runs the
   thing. See `docs/SHIPPING.md` → *Building & releasing*.
6. Tag off `main` and push (`release.yml` builds + publishes). Hyphenated tag = prerelease.
7. If it's a demo/onboarding/external build: verify the published artifact **installs clean** on a
   fresh machine + the target dataset before relying on it.
8. Only at a coarse boundary: add a MILESTONES entry (append-only).

### Regenerating the CHANGELOG section

The `CHANGELOG.md` `[Unreleased]` block is filled in *at release time* from the commit log, so it
never has to be maintained by hand between tags. Grab the one-liners since the last tag:

```sh
git log $(git describe --tags --abbrev=0)..HEAD --oneline --no-merges
```

Rename `## [Unreleased]` to `## [<version>] — <YYYY-MM-DD>`, drop the grouped notes under it
(Added / Changed / Fixed), add a fresh empty `## [Unreleased]` above it, and update the compare
links at the bottom of the file.

The CHANGELOG is no longer a *mirror* of the GitHub Release — it is the **source**. `release.yml`
runs `node scripts/changelogSection.mjs "$TAG"` and publishes what comes out, so the two cannot drift.
Check it renders before you tag:

```sh
node scripts/changelogSection.mjs v0.1.1        # exactly what the release (and the modal) will show
```

**Lead with what a user can act on wrongly.** A format change, a default that moved, a one-way door —
those go first and in full. Feature bullets can be discovered by using the app; a store written in a
format the previous version cannot read cannot.

## Overdue check

Being solo, it's easy to let the heartbeat slip. Rule of thumb: if it's been **> ~2 weeks since the
last tag and `main` has meaningful new commits**, you're overdue — cut one.

```sh
git describe --tags            # last tag
git log $(git describe --tags --abbrev=0)..HEAD --oneline | wc -l   # commits since
```
