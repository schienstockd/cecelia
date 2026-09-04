# Call for Datasets modal

**Status:** **planning** (2026-09-04). Prompted by `DRIFT_RIGID_PLAN.md` P5 needing a home: the
extended 6-DOF 3D rigid drift estimator is a fit that can *only* be validated on a movie where
tissue actually tilts through Z, and shipping it speculatively would risk trading noise against
tilt on every clean movie. Same problem for the large-static-3D-registration ask.

## Goal

A discoverable list of **capabilities Cecelia can build but has no data to build against**, with a
one-click path for a user to open a GitHub issue tagged with the capability name and attach a link
to a cloud-hosted dataset. Reachable from the app's help-links row — the row that already carries
"report a problem" (GitHub issues) and "ask a question" (Zulip chat).

Not a roadmap, not a request board, not a feature-voting system. Deliberately narrow: the entries
are things where the *engineering decision is unblocked as soon as a test dataset lands*. If we
can't act on the dataset, it doesn't belong here.

## Why an in-app modal, not an issue label

- **Discoverability.** A user with a 3D-tilting dataset doesn't know we would build the fit if
  they shared it. An issue label is invisible until someone searches for it.
- **Right-time-right-place.** The user is looking at the drift-correct params page thinking
  "this doesn't quite fit my movie" — the modal surfaces the capability that WOULD fit, and
  what would unblock it, before they walk away.
- **The prompt survives a repo swap.** Everything routes through `frontend/src/lib/links.ts`
  (`CECELIA_REPO_URL`, `CECELIA_NEW_ISSUE_URL`), which already has the "one file, one rename"
  rule for the pending repo move (`docs/SHIPPING.md → Repo swap`).

## Locked decisions (2026-09-04)

1. **One icon, next to the GitHub icon.** `frontend/src/components/AppHeader.vue:91` — new
   `<a>` (or button opening a modal) inserted *before* the GitHub link so the help row reads
   "help us build this → report a broken thing → chat". Icon: TBD from the primevue icon set;
   `pi pi-database` (matches "dataset") or `pi pi-inbox` are the current candidates. **UI
   copy rule applies** — tooltip is one short line, no essay.

2. **Modal, not a page.** `BaseModal` (canonical, see `frontend/CLAUDE.md` → PRIMITIVES).
   Reachable from anywhere in the app without breaking navigation. Content is a card list, one
   card per capability.

3. **Registry, not a hand-rolled list per capability.** New
   `frontend/src/lib/callForDatasets.ts` — a plain module exporting `CALL_FOR_DATASETS:
   CapabilityAsk[]`. Adding an entry is one object literal (`{ id, title, blurb, oneLiner,
   sceneryHint, issueTitle, labels }`); the modal renders whatever's in the array. Same shape as
   `PARAM_FIGURES` / `PARAM_ADVISORS`, for the same reason — one place, one grep.

4. **Each entry drives a prefilled GitHub issue.** The card's primary action opens
   `CECELIA_NEW_ISSUE_URL` with a query string carrying `title`, `labels=call-for-datasets`, and
   a `body` template that already asks the two things we cannot guess: *what does the imaging
   look like* (modality, ~size, dimensionality) and *what is a link to a cloud-hosted example*
   (Google Drive / Zenodo / equivalent). No file upload from the app itself — every mailbox for
   real data is external, and the modal's job is routing not hosting. Same shape as
   `recipeRequestUrl(name)` in `links.ts`, adjacent helper `datasetAskUrl(ask: CapabilityAsk)`.

5. **The modal is not the only entry point.** A `<VisualAid>` or task-param figure whose
   capability lives in the registry can render a small "Want this for 3D-tilting movies?" chip
   that opens the modal pre-scrolled to that entry (query param `?ask=<id>`). The
   `DRIFT_RIGID_PLAN` P4 vis aid renders that chip on the "full 3D rigid" placeholder column —
   see below.

6. **Seed entries at launch.** Two — both already surfaced by the author on 2026-09-04:
   - `sitk-rigid-3d-full` — full 6-DOF 3D rigid drift. Needs a movie where tissue actually tilts
     through Z (heartbeat / breathing / prep shift), not just in-plane rotation.
   - `static-3d-registration` — large static 3D registration (staining cycles, multi-tile
     confocal). Existing `editImages.register` uses sitkibex for 2D staining alignment; the 3D
     extension needs a real multi-cycle 3D stack.

7. **No status field on entries.** A capability is either in the registry (needed, not built) or
   removed (built, or definitively deferred). Status is a whole system this plan is deliberately
   not building.

8. **Entries are code, not settings.** No user-editable list. The point is the capability lands
   in the registry when engineering decides "we could build this if we had data" — same
   discipline as `docs/todo/README.md`, which is human-curated.

## Phases

### P1 — the registry + the modal

- `frontend/src/lib/callForDatasets.ts`: `CapabilityAsk` type + `CALL_FOR_DATASETS` array +
  `datasetAskUrl(ask)` helper. Two seed entries (Decision 6).
- `frontend/src/components/CallForDatasetsModal.vue`: `BaseModal` shell, card list, per-card
  primary action `Open a GitHub issue`. Empty-state: never (Decision 8 says entries exist or the
  feature is off).
- `frontend/src/components/AppHeader.vue:91`: new anchor / button before the GitHub link.
- `frontend/src/lib/links.ts`: `datasetAskUrl` helper (mirrors `recipeRequestUrl`).
- Unit test `frontend/src/lib/callForDatasets.test.ts`: shape of a `CapabilityAsk`, URL builder
  produces a valid GitHub-issue link with `title`, `labels`, and a `body` template that names
  the two "cannot guess" questions.

### P2 — deep-link from a task param

- `?ask=<id>` query-string handled by `App.vue`: opens the modal on mount, scrolled to that
  card.
- `frontend/src/tasks/paramFigures.ts` — new figure kind that renders a placeholder column
  labelled *On request*, with a chip button that opens `?ask=<id>`.
- `DRIFT_RIGID_PLAN` P4 uses this for its "full 3D rigid" column (`ask=sitk-rigid-3d-full`).

### P3 — the GitHub side

- **Not code, but part of the rollout.** Create the `call-for-datasets` label on the repo, and a
  minimal issue template `call_for_datasets.yml` under `.github/ISSUE_TEMPLATE/` that asks the
  same two questions the modal frontloads — so an issue opened directly (not via the modal)
  still lands in a shape we can act on.
- Update `docs/UI.md` → *Help row* to describe the third icon.

## What could go wrong

- **Silent bit-rot.** An entry sits in the registry for months, someone shares a dataset, and no
  one notices because the issue was opened without the `call-for-datasets` label (a user typing
  it into `CECELIA_NEW_ISSUE_URL` directly, say). The modal's primary CTA writes the label
  explicitly, and P3 adds the issue template, but a stray issue still needs a triage habit.
- **Ask inflation.** Every wish becomes an entry. Decision 7 (no status field) is the guardrail —
  if we can't remove an entry within a reasonable window of a dataset landing, it wasn't
  actually blocked on the dataset.
- **The "we'll build this if you share" promise.** The modal has to phrase this without
  overpromising — no "we will implement X within Y days" language. Draft copy: *"If you can
  share a dataset with this scenario, we can build the fit for it."* — passive, no timeline,
  reviewed against `docs/ui/COPY.md`.

## References

- Anchor: `frontend/src/components/AppHeader.vue:91` (GitHub issues link, existing help row).
- Links module: `frontend/src/lib/links.ts` (existing `recipeRequestUrl` is the shape to copy).
- Consumer plan: [`DRIFT_RIGID_PLAN.md`](DRIFT_RIGID_PLAN.md) P5 — first entry
  (`sitk-rigid-3d-full`) originates here.
- Static 3D registration ask: no parked plan yet — audit report at
  [`../audit/simpleitk-opportunities.md`](../audit/simpleitk-opportunities.md) noted `register`
  is 2D staining-cycle alignment via sitkibex; a 3D extension is a separate design.
