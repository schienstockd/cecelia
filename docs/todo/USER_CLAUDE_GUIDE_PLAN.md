# A guide for a user's own Claude Code session

**Status:** planning (2026-10-27). Drafted, nothing built. Decisions 3 and 5 are Dominik's.

## Goal

Users are already pointing their own Claude Code session at Cecelia: at the install, at their project
data, at the code when something breaks. That session is productive, but it does its work around the
platform rather than through it. The goal is that the same work lands where our checks are: as a
registered task (a custom module or plugin), or as a user report with the patch attached. It should
not end up as hand edits to core files or `ccid.json`.

This is our answer to MicroClaw's "microtubule case", where their agent writes new code at run time to
cover a capability no tool has (see [`MICROCLAW_LESSONS_PLAN.md`](MICROCLAW_LESSONS_PLAN.md)). We already
have a supported place for that code (custom modules and plugins). What's missing is anything that points
a user's Claude at it.

## What happened (one reporter, four reports, 2026-10-09 → 10-19)

Source: the private intake repo (`docs/USER_REPORTS.md`), the four reports dated 2026-10-09 to 10-19
from one reporter. The reporter works with a Claude Code session, and their reports are written with it.

| What their session did | Outside the platform because | What it became |
|---|---|---|
| Patched two clustering crashes in core code (pandas copy-on-write left `X` read-only; `logBase` arrived as a string). "Each update overwrites the patches" | A core bug has no other route | Fixed in #1743 and #1744. Their second patch covered only the parser, not the form that sent the string |
| Segmented on their HPC cluster, wrote two label stores with our own writer (`staged_store`, `kind='labels'`), then **hand-edited `ccid.json`** with the app closed | No import task existed. The documented custom-module surface can't register a label set (see below) | `segment.importLabels` (#1618 → #1626, merged 2026-10-12), halo mode (#1623 → #1631), HPC transport (#1619). By 10-19 their label set is "imported labels", i.e. they switched to the task |
| The hand-made two-store layout | — | Surfaced the gate-axis naming bug (#1622) |
| Built population-coloured cell masks outside Cecelia for figures | No such view | #1731 → #1756 |

So the session acted as a requirements prototyper. Every gap it hit became product within about two weeks.
On the way, it produced unversioned state with no provenance, and patches that each update wiped.

## Trace: could a custom module have done the import?

- **Documented surface** (`docs/CUSTOM_MODULES.md`): `register_task!`, reading and writing cell data
  through the label-props view (`label_props |> add_obs |> save!`), `write_qc`, `register_cohort_metrics!`,
  and Python through `run_py`. That covers a new measure or a new obs column. **It does not cover a new
  label set.**
- **What a label set needs**:
  - a store written through `zarr_utils.staged_store(…, kind='labels')`;
  - then `register_label_files!(img, vn, files)` (`app/src/segmentation.jl:108`). It writes the `labels`
    field inside the image's transaction (`commit_state!`), so two concurrent registrations can't drop
    each other and a killed write can't leave a half-written `ccid.json`.
  - `register_label_files!` is **not exported** and appears nowhere in the custom-module docs.
- **So:** a custom module *could* have done it, by calling `Cecelia.register_label_files!` qualified.
  But nothing a user's Claude would read says so, and hand-editing `ccid.json` was the only visible
  path. Today the question is moot for imports: `segment.importLabels` (`app/src/tasks/segment/import_labels.jl`)
  does exactly this, plus ID matching, QC and the refusal of a taken output name.

## Why a user's Claude patches core

- Dev-channel installs extract the whole `main` tarball into the install dir (✔ `install.sh:191`). That
  includes our **developer** `CLAUDE.md` and `.claude/` hooks.
- A user who opens Claude Code in the install dir therefore gets rules written for developing Cecelia:
  branch and PR, recital, discovery before building. Nothing is written for *using* it.
- Not checked: whether the stable release bundle ships them too, and where this reporter actually
  opened their session. *Inferred, not traced.*
- No user-facing doc tells a user's Claude session how to *extend* or *fix* Cecelia. `README.md`
  mentions Claude Code only as how Cecelia was built. `docs/ai-assist/OBSERVER-SETUP.md` → *Chatting in
  your own terminal* does address that session, but only for connecting it to the read-only observer.

## Decisions (proposed)

1. **No run-time pipeline code from the agent.** We won't add MicroClaw-style hooks. The in-platform answer
   to "no task does X" is a custom module or plugin: a registered task with logs, QC, versioned params and
   lineage. Downstream code stays in notebooks (`create_notebook`, read-only on canonical data).
2. **The guide is for a user's Claude Code session working in a terminal**, not for Kiwi. Once the
   user has run Kiwi → Assistant → **Set up**, that terminal session *is* an observer session: the
   observer is registered user-scope, so its `SERVER_INSTRUCTIONS` are in the system prompt of every
   `claude` session on the machine (`mcp/cecelia_mcp/guidance.py:8-11`). This reporter had it registered:
   their first report covers the **Set up** button, and the 10-19 one covers the assistant's highlights.
3. **Where it lives — Dominik's call.** Options:
   - (d) **The observer's own channel** (recommended). Add one or two lines to `SERVER_INSTRUCTIONS`
     ("add a capability as a custom module; never hand-edit Cecelia's files; report bugs with the patch"),
     pointing at the full text. That text goes in a section of `docs/ai-assist/OBSERVER-SETUP.md` →
     *Chatting in your own terminal*, which already addresses this audience. It reaches every session
     that has the observer, needs no first-run file writes, and keeps one user doc about Claude Code
     instead of two. Constraint: `SERVER_INSTRUCTIONS` is in context for *every* session on the machine,
     most not about Cecelia, so it must stay short. Gap: sessions without the observer get nothing.
   - (a) A separate user doc (`docs/USING_CLAUDE.md`), linked from `README.md` and `docs/CUSTOM_MODULES.md`.
     A session only sees it if someone points it there, and it would duplicate `OBSERVER-SETUP.md`'s
     audience.
   - (b) A `CLAUDE.md` at the place a user opens Claude: the projects or config dir, written at install
     or first run. A session there loads it automatically.
   - (c) Make installs ship a user-facing `CLAUDE.md` in place of the developer one. This touches the
     install scripts and the bundle checker, and a dev-channel user who *does* want to contribute then
     loses the dev rules.

   Recommendation: (d) now. (b) only if sessions without the observer turn out to matter.
4. **What the guide says** (target: one screen):
   - **Need a capability Cecelia lacks?** Write a custom module (`docs/CUSTOM_MODULES.md`), or a plugin
     if it should be shared. It runs as a task, so it gets logs, QC and provenance.
   - **Never hand-edit** core files, `ccid.json`, `.h5ad` or label/image stores. Use the task that does the
     job: importing labels is `segment.importLabels`. If no task does it, that's a module or a report.
   - **Hit a bug?** A local patch is a stopgap the next update wipes. Write it up for Dominik with the
     patch, as reporters already do. It goes through `docs/USER_REPORTS.md`, and diagnoses are verified there.
   - **Looking at your data?** Read through the Cecelia accessors (`init_object`, `pop_df`, `label_props`),
     and for figures use a notebook. Nothing is written back into the project except through a task.
5. **Expose `register_label_files!` to custom modules?** Dominik's call. Recommendation: **no** for now.
   `segment.importLabels` covers the one observed case, and a module writing label sets would also need
   the name-collision guard (`assert_labels_free`) and versioning rules. Revisit if a second, non-import
   case appears.

## Phases

- **P1 — the guide (Decision 3 d).** Write Decision 4's text as a section of
  `docs/ai-assist/OBSERVER-SETUP.md` → *Chatting in your own terminal*. Add the one- or two-line rule to
  `SERVER_INSTRUCTIONS` in `mcp/cecelia_mcp/guidance.py`, plus a test that pins it (`mcp/tests/test_server.py`
  → `GuidanceTest`). Link the section from the top of `docs/CUSTOM_MODULES.md`. Touchpoints: 3 files + 1
  test. No new doc.
- **P2 — try it.** Point the reporter's next session at it (reply draft in `cecelia-user-reports/triage/`),
  then see whether the next report carries a module or a patch-plus-report instead of a hand edit.
  Nothing to build.
- **P3 — reach sessions without the observer (only if that matters).** Decision 3 (b): write a short
  `CLAUDE.md` into the config or projects dir at first run, via `ensure_config_dir()` + `write_atomic`
  (`app/src/config.jl`). There's no shared first-run hook today. This touches the first-run path and the
  install scripts on 3 operating systems. Cost it before starting.

## Risks

- **One reporter.** This might be one person's working style, not a pattern.
- **A guide is advice, not a guarantee** (MicroClaw's design/22: "the schema is advice, the gate is the
  guarantee"). Nothing stops a session editing `ccid.json`. If hand edits keep arriving, the gate-shaped
  fix is load-time validation of `ccid.json` fields that only a task should write. That's out of scope here.
- **Dev installs keep the developer `CLAUDE.md`.** A session in the install dir still reads the
  contributor rules, alongside the observer's short rule from P1.
- **`SERVER_INSTRUCTIONS` bloat.** Every line costs context in every session on the machine. P1 adds a
  rule, not a guide.
