# View profiles — a curated sidebar for focused workflows

**Status:** **BUILT** (P1 + P2 + P2b + the welcome page) on branch
`docs/correction-plugins-profiles-plans`; kept as the rationale record. Not yet seen in a browser —
the backend routes live in `api/src/` (not Revise-tracked), so they need a server restart before the
Settings panel can load anything.
**Supersedes** `docs/archive/view-profiles-prompt.md` (the brief that asked for this).
**Origin:** a lab that used the old R version does narrow work — gating, tracking and behaviour on
already-segmented data — and needs none of the clustering pages. Old R hid module pages via a
live/static distinction; the Julia+Vue rewrite dropped it and renders one fixed menu of **20 items**
for everyone. Their other asks — manual segmentation/track correction, and their own format-specific
importers as installable plugins — are being designed and built separately, each with its own plan.

## Goal

A named, ordered **subset** of the sidebar pages that already exist, stored as drop-in data, selected
per user. A profile hides clutter. It cannot invent a page, and it is not access control.

## Scope

**In:** which sidebar entries are shown, and their order within a group.
**Out** (flag if one turns out to belong, do not design it in):
- Per-page layout/widget customisation — this is menu-level only.
- Access control — a hidden page **must** stay reachable by direct URL.
- Letting a custom-module category own a bespoke `.vue` page (real plugin-page loading). Materially
  bigger; only worth scoping if a concrete case shows the filtered menu + the generic custom page is
  genuinely not enough.

## What the investigation found

1. `groups` (`frontend/src/components/AppSidebar.vue:65`, 5 groups / 20 items) plus the
   `customGroup` / `allGroups` computeds (`:122`, `:136`) are still the **only** things driving the
   menu. `allGroups` is already a `computed`, so filtering it is free.
2. `frontend/src/stores/settings.ts` is 40+ `localStorage.getItem('cc.*')` refs — the established
   pattern for per-user state. No new persistence mechanism is needed or wanted.
3. **No route guard depends on the nav list.** The only `router.beforeEach`
   (`frontend/src/main.ts:58`) is the first-launch setup guard. Direct-URL reachability therefore
   needs no work — it is already true.
4. **The one real downstream assumption of the full list:** `frontend/src/main.ts:22` —
   `{ path: '/', redirect: '/manage-images' }`. A profile that hides Manage images still **boots onto
   it**. Resolved by Decision 9 — `/` became a page of its own rather than a redirect to be resolved.
5. `meta.label` in `main.ts` is a second copy of every page label. A profile must key on **path**, not
   label, or the two lists drift.
6. Project metadata (`useProjectMetaStore`) has no natural home for this, which reinforces Decision 2.

## Decisions

1. **Definitions are files; the selection is a setting.** Two different things, and the brief's
   question list conflated them. Profile *definitions* live as drop-in JSON under
   `<config_dir>/profiles/<name>.json` (same no-rebuild spirit as custom modules, same
   `config_dir()` resolver). The *active selection* is a single string.
2. **The selection is per-user, not per-project** — `cc.viewProfile` in `settings.ts`, following the
   pattern in finding 2. A profile describes **who is driving**, not what the data is; a project
   shared with a collaborator must not dictate their menu. (Per-project would also mean the profile
   travels in project metadata and mutates a shared artefact to change one person's UI.)
3. **Profiles are BUILT in the GUI. A config file is the storage format, not the authoring path**
   (Dominik, 2026-08-17 — this reverses an earlier "picked, not authored" draft). Settings gets the
   selector *and* a builder: create, rename, duplicate, delete a profile, choose its pages and their
   order, all in the app. Hand-editing a `<config_dir>/profiles/*.json` keeps working (that is how a
   profile is shared between machines, and how a plugin can ship one —
   the plugin work, which keeps the two systems decoupled), but nobody should have to write JSON to get
   a smaller menu.
   **The builder is assembled from canonical primitives, not hand-rolled:**
   - one **`ChipSelect`** per sidebar group, `multiple` + `reorderable` (`variant="pill"`) — the
     selection *is* that group's items and the chip order *is* their order, so include/exclude and
     reorder are the same control. Logic already lives in `utils/chipSelect.ts`
     (`toggleValue`/`moveItem`), tested.
   - **`BaseModal`** as the editor shell (`docs/UI.md` → *Modals & dialogs*).
   - **`ConfirmDeleteButton`** for delete — THE app-wide delete affordance (an icon that arms on the
     first click, fires on the second). Hand-rolling `ConfirmButton` + a text button instead produced a
     Delete that did nothing, because the trigger called `arm` in both states and `confirm` never fired.
   - **`ChipSelect`'s own `selectAll`** for all/none — not a pair of hand-rolled buttons beside it.
   - Row actions are **icons** (`cc-btn-icon` + a `pi` glyph + a tooltip), matching every other row
     action in the app.
   Rendering a bespoke drag-and-drop list here would be a new variant of a primitive that already
   exists — the exact bug `CLAUDE.md` names.
4. **Writes go through the same file store, atomically.** `POST /api/profiles` (create/update) and
   `DELETE /api/profiles/:id`, writing `<config_dir>/profiles/<id>.json` via `write_json_atomic`. The
   **id is derived from the label** with `safe_name_part` (`app/src/utils.jl`) — the user types a name,
   never a filename. Renaming the label of an existing profile keeps its id, so a selection doesn't
   break; "Duplicate" is how you get a new id.
5. **A profile is a flat, ordered list of route paths; built-in group headings are retained.**
   Groups appear in their built-in order; a group with no surviving item is hidden entirely (the same
   behaviour `customGroup` already has when empty); within a group, items follow the profile's order.
   *Rejected for v1:* letting a profile declare its own group headings. It is more expressive but lets
   a profile invent structure, which is a bigger surface than "hide what I don't use". A `groups` key
   can be added later without breaking `items`.
6. **Custom-module routes are addressable individually** (`/custom/<category>`), exactly like any
   other path — no special case. Hiding the whole Custom group is just the case where none of its
   paths are listed.
7. **Switching is live-reactive, not a reload.** `allGroups` is a computed; filtering it re-renders the
   sidebar and unmounts nothing. The `<KeepAlive>`/hard-reload precedent in `ChainModule.vue` exists
   for a different problem (state-loss boundaries on heavy canvases) and would be cargo-culted here.
8. **A path that no longer exists is dropped silently from the menu and listed as a warning in
   Settings** — the shape the Custom modules panel already uses for load errors. Not a Lab Log entry:
   the Lab Log records science, not config lint. Silent-only is not enough either, because a renamed
   route would make a profile quietly shrink with no way to notice.
9. **`/` is its own neutral welcome page — the landing route is profile-INDEPENDENT.**
   (Dominik, 2026-08-17, superseding "the landing route follows the profile".) `modules/WelcomeModule.vue`
   is a greyed watermark of the brand mark and no copy.
   *Why the first answer was wrong:* a record's `redirect` is resolved **before any router guard**, so
   `redirect: () => firstVisiblePath(…)` fired while the profile list was still in flight, landed on
   the fallback, and needed a second re-resolve in `beforeEach` to correct itself — a visible bounce
   held together by ordering. Making `/` a real destination deletes the race instead of timing around
   it: nothing to resolve, no page privileged as "the start", and finding 4 stops being a problem
   rather than being worked around. `firstVisiblePath` was removed with it.
10. **Never hide `/settings`.** It is in the sidebar footer, not in `groups`, and it is the only way
   back to the profile selector. A profile that could hide Settings could strand the user.

## Profile file shape

`<config_dir>/profiles/gating-behaviour.json`:

```json
{
  "label": "Gating + behaviour",
  "items": [
    "/manage-images",
    "/gate",
    "/track",
    "/behaviour",
    "/tasks"
  ]
}
```

- `label` — what the selector shows. Falls back to the filename stem when absent.
- `items` — ordered route paths. Unknown paths are dropped (Decision 8). An empty or missing `items`
  makes the profile invalid, not empty — report it rather than rendering a blank sidebar.
- The filename stem is the profile **id** (what `cc.viewProfile` stores). No id field, so a file can
  be renamed without editing it — same convention as a custom module's `fun_name`.
- The implicit **"All"** profile (today's behaviour) is always present as the fallback and is not a file.

## Phases

- **P1 — backend.** `<config_dir>/profiles/` reader + shape validation + `GET /api/profiles` returning
  `{ dir, profiles: [{ id, label, items }], errors: [{ file, error }] }`. The server validates
  **shape only** — it does not know the route table, which lives in `frontend/src/main.ts`, so
  *unknown paths are resolved in the frontend against the live router* (Decision 8). Package test: a
  good file parses, a bad one is reported and does not throw.
- **P2 — frontend, read path.** `stores/viewProfiles.ts` (fetch + selection), `AppSidebar.vue` filters
  `allGroups`, `main.ts` landing redirect, Settings section with the selector + Reload + the unknown
  list. Extract the filter/order logic to `utils/viewProfiles.ts` and unit-test it there — per
  `docs/DEV.md`, logic goes in `.ts`, not the SFC.
- **P2b — the builder** (Decisions 3-4). `POST /api/profiles` + `DELETE /api/profiles/:id`, and a
  `ViewProfileEditor.vue` on `BaseModal`: profile list + new/duplicate/rename/delete, then one
  reorderable `ChipSelect` per sidebar group. The editor is fed by the SAME group list the sidebar
  renders, so a page can never be offered that does not exist.
- **P3 — docs + example.** `docs/UI.md` (how the sidebar is filtered) and a runnable example profile
  under `docs/examples/`, mirroring `docs/examples/custom-modules/`.

## Cross-file architecture

| Concern | File |
|---|---|
| Profiles dir + reader | `app/src/profiles.jl` (new); `config_dir()` in `app/src/config.jl` |
| Routes | `api/src/routes.jl` — `GET`/`POST /api/profiles`, `DELETE /api/profiles/:id` (+ registration in `api/src/server.jl`) |
| The one authoritative route list to validate against | `frontend/src/main.ts` route table (server-side: the profile reader validates shape only; the frontend resolves against the live router) |
| Selection | `frontend/src/stores/settings.ts` — `cc.viewProfile` |
| Filter logic (tested) | `frontend/src/utils/viewProfiles.ts` + `.test.ts` |
| Menu | `frontend/src/components/AppSidebar.vue` — `allGroups` |
| Landing page | `frontend/src/modules/WelcomeModule.vue` (route `/` in `frontend/src/main.ts`) |
| Catalogue-vs-router ratchet | `frontend/src/lib/navGroups.test.ts` |
| Settings surface | `frontend/src/modules/SettingsModule.vue` |
| Builder | `frontend/src/components/ViewProfileEditor.vue` (new) over `BaseModal` + `ChipSelect` |
| Docs on landing | `docs/UI.md` |

## Open questions

1. Whether a profile should be able to declare its own group headings (Decision 5's rejected
   alternative). Revisit if a real profile wants a heading the built-in five do not provide.
2. Whether the selector belongs in Settings only, or also in the sidebar footer next to the collapse
   control. Settings-only for v1 — a second surface for a rarely-changed setting is the kind of
   duplication that produced two shutdown buttons.
3. The editor's chip rows carry **no per-option tips** by design — a page's own tip describes the page,
   not the choice, and it collided with the control's tooltip. If per-page help is ever wanted here it
   has to replace the control's tooltip, not join it (`docs/UI.md` → *Tooltips*).
4. Nothing checks that a profile FILE's paths are routable at write time — the editor can only offer
   real pages, but a hand-edited file can name anything. It is caught on read (`unknownPaths` → the
   Settings warning), which is where it matters; a save-time rejection would also have to be taught the
   route table.
5. Whether `requiresProject` locking interacts with profiles at all. It should not: a profile decides
   *visibility*, `hasProject` decides *enabled*. Keep them independent.

## Related

- `docs/archive/view-profiles-prompt.md` — the originating brief (superseded by this plan).
- The plugin-distribution plan — a plugin may ship a profile file; the two systems stay decoupled.
- The manual-correction plan — the pages that lab is actually here for.
