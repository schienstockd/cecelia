# Audit prompt: per-profile settings layer + project-settings field audit (post-release cleanup)

> **ARCHIVED — not authoritative, do not act on this.** The prompt scoped three overlapping problems
> (per-profile settings field audit, preferences modal, project ownership/filtering) around a
> "reconcile with the Kiwi picker" framing. Superseded by
> [`docs/todo/USER_PROFILE_PLAN.md`](../todo/USER_PROFILE_PLAN.md), which reframes the primitive as
> **the launch-time user profile**: identity is chosen once, everything (per-profile settings,
> project filtering, Kiwi's `CLAUDE_CONFIG_DIR`, attribution) reads from it, and the Kiwi
> picker in [`KiwiCockpit.vue`](../../frontend/src/components/kiwi/KiwiCockpit.vue) is dropped
> when the launch-time picker lands (not reconciled with).

## Context

Deferred deliberately, not forgotten. This was originally scoped as a
prerequisite for the Kiwi profile picker's on/off toggle — that toggle
got designed away (single profile on a box auto-loads, no setting
needed), so the picker shipped (#1209) as a small addition to
`KiwiCockpit.vue` with no app-wide config layer required. What's left
is organizational debt, not a blocker: some fields on the existing
project settings page are arguably app-wide, not project-scoped, and
this is the cheap moment to sort that before more settings accumulate
in the wrong place.

Release was cut before this was picked up — first post-release
cleanup. Priority note: this isn't just code hygiene. If app-wide
values are currently duplicated or displayed per-project in the
projects table, moving them to an app-wide layer directly declutters
that table — a concrete UI payoff, not just tidiness. Worth confirming
in Task 1 whether the clutter is (a) app-wide fields genuinely
duplicated as data across every project row (a real redundancy bug,
worth fixing regardless of timing), or (b) app-wide fields mixed among
project-specific ones making the table/settings page noisy (a UI-only
win, scales with how many fields turn out misclassified). That answer
should set how soon this gets picked up after release, not "whenever
there's a slow week."

## Terminology correction: "app-wide" means per-profile, not shared-universal

Confirmed design: launch flow is choose-a-profile first ("Hello Ben") →
Ben sees only his projects → Kiwi is ready under his identity → and his
settings (whatever Task 1 reclassifies as above-project-scope) are
*his own*, not shared with other profiles on the same machine. "App-wide"
in Tasks 1–3 below means "scoped above individual projects, persists
across a profile's projects" — it does NOT mean "one config row shared
identically by every profile." If Ben changes a setting, it must not
change anything for other profiles on the same box. Every "app-wide
config layer" reference below should be read as "per-profile config
layer" — keyed to the active profile, not global to the install.

## Task 1 — Audit the existing project settings page

Classify every field on the current project settings page as genuinely
project-scoped vs. actually app-wide. For each misclassified field,
recommend move-now or leave-as-is with reasoning — don't decide
unilaterally, some fields may be project-scoped for a reason that isn't
obvious just from the field itself. Report findings before doing any
migration work.

## Task 2 — Scope the per-profile config layer, if Task 1 justifies it

If Task 1 finds real candidates, scope a minimal per-profile settings
store — keyed to the active profile, not a single shared row: where the
state lives, migration path for any fields moved out of project
settings, and whether existing project-settings persistence can be
reused/extended or needs something new.

## Task 3 — Settings modal UI, if Task 2 proceeds

Two-pane layout: category list on the left, selected category's fields
on the right — same well-worn pattern as Firefox/Thunderbird
preferences or GNOME Settings, no need to invent a new layout. This
modal shows and edits the *active profile's* settings — per-profile,
not a shared machine-wide panel. The existing Kiwi profile dropdown in
`KiwiCockpit.vue` stays where it is, subject to the reconciliation
question in Part 2 below; this modal is for the settings Task 1
surfaces, not a place to relocate the profile picker itself.

## Part 2 — Project ownership/filtering by profile

Separate problem from Tasks 1–3, bundled into this same prompt because
it reuses the same identity primitive. Real-world driver: on a
shared-machine lab setup with multiple people, the load-project screen
currently shows every project from every user mixed together — no
concept of ownership at all. This isn't a settings-misclassification
issue; it needs projects associated with an identity and the load
screen filtered by it.

**Profile selection at launch is the parent concept — Kiwi identity
follows from it, not the reverse.** Correction to the original framing
of this section: the direction isn't "reuse Kiwi's profile to filter
projects," it's "the profile chosen at launch (before a project is even
loaded) is the source of truth; Kiwi reads the active profile and sets
`CLAUDE_CONFIG_DIR`/attribution from it, same as project filtering
would." (This is the launch-time selection itself, not to be confused
with the per-profile settings from Tasks 1–3 — one is "which profile is
active," the other is "what that profile's own settings are.")

This has a real consequence for what already shipped: the profile
dropdown in `KiwiCockpit.vue` (#1209) is currently the *only* profile
selector in the app, but it's scoped to Kiwi specifically rather than
being the app-wide selector. Under this model that needs reconciling —
options to weigh, not decided here:
- Move profile selection out of `KiwiCockpit.vue` into a launch-time
  surface (or the settings modal from Tasks 1–3 above), and have
  Kiwi's row become a read-only reflection of the active profile rather
  than its own dropdown.
- Or keep Kiwi's dropdown as *a* place profile can be changed, but make
  clear it's changing the one active-profile value, not a Kiwi-local
  one — any change there should propagate to project filtering too,
  not just to Kiwi's own `CLAUDE_CONFIG_DIR`.
Don't build project filtering as if the Kiwi dropdown didn't already
exist; reconcile with it explicitly.

Open questions to resolve before scoping implementation:

- **Ownership model** — single owner per project, or shared/visible to
  multiple profiles? Lab reality (people collaborating on the same
  dataset) suggests "visible to a set of profiles" is more realistic
  than strict single-ownership; confirm which fits actual usage rather
  than defaulting to the simpler model.
- **Default view** — load screen filtered to the active profile's
  projects by default, with an explicit "show all" toggle, vs.
  unfiltered unless a profile is actively set. Consider that Kiwi
  profiles auto-skip the picker when only one exists (per the earlier
  picker design) — does project filtering follow the same "invisible
  until there's more than one profile" principle, or does it need to
  behave differently since an empty/wrong project list is a bigger
  usability problem than a skipped identity picker?
- **Migration** — every existing project predates profiles entirely.
  Decide: unowned/visible-to-all by default, or a one-time assignment
  step at first load post-upgrade. Same category of question as the
  pre-identity-data migration already flagged for captures/attribution
  — worth resolving consistently with that decision rather than
  separately.

## Explicitly not in scope for this pass

- Anything about the Kiwi identity/credential picker itself — that
  shipped (#1209) and is working as a standalone row, not gated on this
  work.
- Re-opening the on/off-toggle question for the picker — settled,
  auto-load-on-single-profile is the design, not reversed by this
  cleanup.

## Ask

1. Produce the field-by-field classification from Task 1 with
   move/leave recommendations.
2. Only if Task 1 finds enough to justify it: scope Tasks 2–3 as
   above.
3. If Task 1 comes back mostly empty (few or no genuinely misplaced
   fields), say so plainly and recommend closing this out rather than
   building a per-profile settings layer for one or two fields — don't
   manufacture scope to justify the prompt.
4. For Part 2: resolve the three open questions above with a
   recommendation each, then scope the minimum implementation —
   project-to-profile association field, load-screen filtering logic,
   and migration approach for existing projects.
