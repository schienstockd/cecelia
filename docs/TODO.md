# TODO

**Open work only — things someone intends to do.** When an item is done, **delete it**; what changed
is recorded in git history, merged PRs and the release notes, not here.

**Does it belong here?** This is the tracker with the loosest edges, so it collects orphans. Before
adding, check the others:

| If the item is… | It goes in |
|---|---|
| a deliberate **non-goal**, or conditional on something that may never happen | `docs/FUTURE.md` |
| a **known-better approach set aside** (scale/ecosystem not ready) | `docs/FUTURE.md` |
| big enough to need **locked decisions + phases** before building | a `docs/todo/<FEATURE>_PLAN.md` |
| a **phase goal** for the current arc | `docs/ROADMAP.md` |
| something that **already shipped** | `docs/MILESTONES.md` (or nowhere — git has it) |
| **how a built subsystem works** | the relevant `docs/<AREA>.md` |

A fact you want recorded but that nobody should act on is **not** a TODO item. That is the drift this
table exists to stop: something worth knowing turns up, has no obvious home, and lands in the backlog.

**Referencing an item.** Items are keyed by their **title** — there are no numeric IDs. Cite one as
`docs/TODO.md` → *Title*.

> Numeric IDs (`#00042`) were retired on 2026-08-05. They existed so code could cite an item, and they
> failed at exactly that: of the eight code comments citing an ID, **four pointed at an item that no
> longer existed**. The cause was structural — completion *deletes* an item, so "increment the highest"
> was evaluated against a shrinking set and reissued numbers that plans, comments and git history still
> referenced. `#00087` ended up meaning two different things; `#00003` was a known duplicate. Meanwhile
> `docs/FUTURE.md` (keyed by title) and `docs/todo/*_PLAN.md` (cited by path) never collided once.
>
> Every citation in the docs and the code was repointed at the same time. `docs/archive/` is left as
> written — those are frozen records of finished work, so a retired number there is history, not a
> broken pointer.

**From code, prefer the permanent reference.** A `docs/<AREA>.md` section or a `docs/todo/X_PLAN.md`
path cannot dangle when the work ships, which is what a TODO citation does by construction. Cite the
tracker only for work that is genuinely still open, and by title.

Items marked **🔹 needs-input** need something only Dominik can provide — a test asset, a
domain-specific expected value, or a decision an agent shouldn't make alone. Grep `needs-input`.

---

## Next up

### Value-name input — the one namespace left out

Design + locked decisions: [`docs/todo/VALUE_NAME_INPUT_PLAN.md`](todo/VALUE_NAME_INPUT_PLAN.md).
Phases 1-3 have shipped: 9 of the 11 output-naming params declare a `namespace`, offer the names
already in use, and remember their params per name.

**`obsCols` was deliberately left out** — `colName` on `behaviour.hmmStates` and
`behaviour.hmmTransitions` is still bare text. Every other namespace lists from `ccid.json`, one
`readdir` or a small sidecar; this one would have to open the `.h5ad` to read its obs columns, on
every form render, for the two least-used tasks in the app.

To do it anyway: add an obs-column accessor behind the same lazy fetch the pop pickers use (NOT the
image payload), then flip the two specs to `valueNameInput` + `namespace: "obsCols"`. The param recall
follows for free — it keys off `task_output_name`, which already reads the declaration.

Also still open, and separate: **the Metadata "new attribute name" field**. `createAttr` rejects an
existing name, so offering the existing ones would suggest guaranteed failures. The useful version
merges the existing-attribute `<select>` with the create-input into one control — a real interaction
change, not a drop-in.

### Per-notebook reset (re-run a notebook on new data without killing the Pluto server)
Pluto has no filesystem watcher, so a notebook keeps rendering **stale data with no visible sign**
after a pipeline task rewrites its inputs. The `DATA_STAMP` convention
(`docs/NOTEBOOKS.md` → *Refreshing after a pipeline re-run*) is the working answer today, but it is
manual and easy to forget. The only reset currently available is the Notebooks page's **Restart**,
which stops the whole Pluto server and takes **every** open session (and their unsaved edits) with it
— which is what prompted this (Dominik, 2026-07-29).

**Mechanism, verified against the pinned Pluto (`Pluto/F6SNP`, `src/webserver/Router.jl`):**
- `GET /notebooklist` → a Dict of `notebook_id => path`. **MsgPack-encoded** (`pack`), not JSON, and
  `api/` has no MsgPack dep — so prefer the redirect route below over adding one.
- `GET|POST /shutdown?id=<uuid>` → `SessionActions.shutdown` for **that one session**.
- `GET /open?path=<abs>` → 302 to `/edit?id=<uuid>`, so the id can be read from the `Location`
  header with no new dependency. (Opening an already-open path returns the existing session.)

So: `POST /api/notebooks/reset { projectUid, file }` = resolve abs path → `open` without following
redirects → parse `id` → `shutdown?id=` → return ok. Then the row's existing **Open** runs it fresh.
UI: one per-row button in `NotebookTable.vue` (`pi-replay`), beside Snapshot/History.

**Risks to handle, not ignore:**
- These are Pluto's **desktop-app** endpoints ("normally shutdown is done through Dynamic.jl" per its
  own comment) — semi-public, not a stability guarantee. Fail gracefully and pin the Pluto version
  the behaviour was verified against.
- A reset **discards unsaved in-session edits**. Snapshot first, the way `/api/notebooks/revise`
  already does, so nothing is unrecoverable.
- `open`-then-`shutdown` on a notebook that was not running starts a session just to kill it. Cheap,
  but check `/notebooklist` first if it ever matters.

Related, and a bigger decision: **`PlutoUI` is not in `pluto/Project.toml`**. Adding it would give
`Button` (an in-notebook refresh) *and* sliders for a timepoint, but costs a re-resolve of all three
manifests. Decide that separately.

### Ship a prebuilt Notebooks sysimage in the bundle (release optimisation)
Build-on-demand already covers every user: the Notebooks page's **Enable fast plots** builds
`pluto/deps.so` in the background and re-stamps it when Julia/deps move. What is left is the
packaging half — once the constructor/pixi packaging pins Julia per platform, build the `-full`
variant in CI and ship it for the primary OSes so even the *first* open is instant. It falls through
to the on-demand path wherever no prebuilt image is present, and the freshness stamp means a shipped
image that predates the user's Julia/deps self-heals. Belongs with the packaging phase in
`docs/ROADMAP.md`; not urgent, since one on-demand build already gives every user a fast cache.

### Set-scope / incremental node subprocesses not killed on chain cancel
The per-image cancel path kills running subprocesses. Set-scope (`_run_set_scope_node!`)
and incremental (`_run_incremental_node!`) runners call the multi-image `_run_task` directly with
`on_process = _ -> nothing` and are **not** registered in `_TASKS`, so `cancel_chain_run!` can't
reach their subprocesses mid-run (the between-node flag still stops not-yet-started ones). No real
set-scope subprocess task exists yet (only mock/plot tasks), so impact is currently nil. When the
first real set-scope subprocess task lands (e.g. HMM training), give the multi-image `_run_task`
path a `TaskRecord` + `chain_run_id` so it's cancellable like the per-image path. Low priority.

