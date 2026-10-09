# User reports — intake, triage, follow-through

How a bug report from a user (an email, a write-up their own Claude produced) becomes tracked,
fixed work and a reply. Same procedure whether a person or an agent does the triage.

## Where reports live

Raw reports are **private**: they carry the reporter's name, machine and data details. They live in
the private repo [`schienstockd/cecelia-user-reports`](https://github.com/schienstockd/cecelia-user-reports)
(ask Dominik for access), cloned beside the cecelia checkouts as `../cecelia-user-reports`.

- `reports/YYYY-MM-DD_<initials>.md` — the report **verbatim**, never edited. A cover email or
  message goes beside it as `..._cover.md`.
- `index.md` — one row per item: report → issue → status. The only link between a private report
  and its public issues.

Public tracking is GitHub issues on `schienstockd/cecelia`. **This repo is public**: nothing from the
private repo is copied into an issue except the technical content.

## 1. Intake

1. Save the report (and cover note) under `reports/`.
2. Add a section to `index.md` with one row per item, status `triage`. Commit.

## 2. Triage

For each item:

1. **Split by root cause, not by paragraph.** Two symptoms of one bug are one item; one paragraph
   describing two bugs is two.
2. **Check it against current `origin/main`, by tracing the code.** Reports are usually against a
   tagged release that `main` has moved past — note the reporter's version, and if it's already
   fixed, mark it `already-fixed` with the PR.
3. **Treat the reporter's diagnosis as a hypothesis.** Reports written with an AI assistant come
   with a confident cause and a suggested patch. Verify both. (The first report's cause for a
   cellpose crash was wrong, and its suggested patch would have produced the crashing shape.)
4. **Search existing issues and `docs/TODO.md`** for duplicates.
5. **Record what triage turns up on the way** — a neighbouring bug found while tracing is its own
   item.

## 3. File issues

One issue per item, following [`bug_report.yml`](../.github/ISSUE_TEMPLATE/bug_report.yml)'s fields
(what happened, version, OS, install method, logs, data context).

- **Labels:** `user-report` always; `bug` or `enhancement`; `blocker` if it stops the reporter's
  work; `needs-mac` (or another platform) if it can't be reproduced on a Linux workstation.
- **Body:** technical content only — no reporter name or initials, and data described by shape,
  dtype and pixel size, never by what it is or whose it is.
- **Separate the evidence:** what the reporter saw (error text, logs) / what triage verified on
  `main` (with `file:line`) / the reporter's diagnosis, labelled *unverified* if triage didn't
  confirm it.
- Write the issue number back into `index.md`, status `open`.

Issues are public and posted under Dominik's account — the attribution rule in
[`DEV.md`](DEV.md) → *Agent-authored public replies are attributed* applies.

## 4. Fix

- One worktree and one PR per issue; the PR says `Fixes #N`. Blockers first.
- **Reproduce before fixing.** The fix lands with a regression test that fails without it.
- A platform-only bug that can't be reproduced locally still gets a test that pins the behaviour
  (e.g. stub the library call to return the shape the platform returns), and the reporter is asked
  to confirm on the next release.

## 5. Close the loop

1. When an issue closes, set its row to `fixed` with the PR and the release it ships in. A fix on
   `main` hasn't reached the reporter until it's tagged ([`RELEASING.md`](RELEASING.md)).
2. When every item is `fixed`, `already-fixed` or `wontfix` (with a reason), draft a reply: what was
   fixed and in which release, what wasn't and why, and any questions for the reporter. Dominik
   sends it.
3. Mark the report closed in `index.md`.
