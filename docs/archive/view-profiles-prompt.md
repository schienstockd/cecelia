> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.
>
> **Outcome:** answered in [`docs/todo/VIEW_PROFILES_PLAN.md`](../todo/VIEW_PROFILES_PLAN.md), which
> is authoritative. Two premises of this brief were corrected there: the per-user-vs-per-project
> question conflated *where profile definitions live* (config-dir files, as this brief itself locks)
> with *where the active selection lives* (a per-user setting) — they are separate axes; and the brief
> missed the one place that does assume the full static list, the `/` → `/manage-images` redirect in
> `frontend/src/main.ts`, which strands a profile that hides Manage images on boot.

# Prompt: View profiles — hiding menu complexity for focused workflows

Planning pass. No code yet. Investigate and come back with a proposal + locked decisions before
touching `AppSidebar.vue`.

---

## Context

Old R Cecelia had a live/static distinction that hid module pages from the main menu depending on
what the project/user needed. The Julia+Vue rewrite dropped that — `AppSidebar.vue` renders one fixed
set of groups (Data / Populations / Explore / Analysis / Pipeline) for every user, every project, all
the time.

That's fine for someone running the full pipeline. It's not fine for a user doing narrow, focused
work (e.g. gating + behaviour analysis only, on already-segmented data) who has to navigate a
20-item menu of stuff they'll never touch.

We already have a plugin surface for this kind of thing — custom modules
(`docs/CUSTOM_MODULES.md`, `docs/todo/CUSTOM_MODULES_PLAN.md`, shipped P1-P3) let a user drop in a
task function and get either an existing page or an auto-generated one. That solves "add a new
capability." It does not solve "curate which of the existing pages I even see." Different problem —
don't conflate them, and don't reach for a plugin framework to solve this one. Read
`CUSTOM_MODULES_PLAN.md`'s locked decisions first — it explicitly rejected a framework for the task
side ("pure Julia include + a registry refactor"); hold the same line here on the UI side.

## What to investigate

1. Read `frontend/src/components/AppSidebar.vue` — confirm the `groups` constant and the
   `customGroup`/`allGroups` computed are still the only things driving the menu, and check nothing
   downstream (route guards, breadcrumbs, `requiresProject` handling) assumes the full static list.
2. Read `frontend/src/stores/settings.ts` for the existing pattern of `localStorage`-backed refs —
   any profile-selection state should follow that precedent, not invent a new one.
3. Check whether project metadata (`useProjectMetaStore`) has anywhere a per-project setting like
   this could live, in case per-project turns out to be the right scope instead of per-user.

## What to propose

A **View Profiles** mechanism: a named, ordered subset of the existing sidebar routes (built-in +
custom-module categories), stored as data under `<config_dir>/profiles/`, same drop-in/no-rebuild
spirit as custom modules. A profile can only include/exclude/reorder pages that already exist — it
cannot invent one. One implicit "All" profile (today's behaviour) is always the fallback.

Explicitly out of scope for this pass — flag if you think either belongs, but don't design it in:
- Per-page layout/widget customization. This is menu-level only.
- Access control. A profile hides clutter; a hidden page must still be reachable by direct URL.
- Letting a custom-module category own a full custom `.vue` page instead of the generic one. That's
  a materially bigger, separate ask (real plugin-page loading) — only worth scoping if a concrete
  case shows the filtered menu + generic custom page genuinely isn't enough.

## Questions to resolve before locking anything

- Per-user (`localStorage`, matches existing settings pattern) vs per-project (would need to live in
  project metadata so the profile travels with the project across machines) — pick one and say why.
- Does a profile address individual custom-module category routes (`/custom/<category>`)
  independently, or only show/hide the whole "Custom" group at once?
- Profile switch: hard reload (simplest, matches the `ChainModule.vue` `<KeepAlive>`
  precedent for state-loss boundaries) or live-reactive?
- What happens when a profile references a route that no longer exists (renamed page, removed custom
  module) — silent drop, a Settings-panel warning like the custom-module error list, or a Lab Log
  entry?

Come back with locked decisions on the above, a sketch of the JSON shape for a profile file, and
where the Settings UI for picking/reloading profiles should live (propose reusing the Custom modules
panel's shape). Don't start on `AppSidebar.vue` until that's agreed.
