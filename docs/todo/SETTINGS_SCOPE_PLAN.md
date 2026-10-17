# Settings by scope — machine sections for peanut only — plan

**Status:** built 2026-10-17 (P1–P3, branch `settings-hpc-below-mcp`). Kept as the record of *why*;
how it works now lives in `docs/UI.md` → *Settings — who sees which section*. Follows the Shared
workstation section (`LOGIN_CREDENTIAL_ISOLATION_PLAN.md` → P8), the first section gated on the
default profile.

## Goal

Settings (`frontend/src/modules/SettingsModule.vue`) mixes four scopes on one page: settings that
change the **whole machine** for every user, settings that are **just yours**, actions on the
**open project**, and **read-only** status. On a shared workstation anyone can change the machine
ones. Show the machine-wide sections to the default profile (peanut) only, and give each control
one obvious home, without gating item by item.

## The page today (audited 2026-10-17)

| Scope | Sections | Visible to |
|---|---|---|
| **Machine** — every user is affected | Software updates (apply / restart), Storage *defaults* (compressor, store layout, keep previous version, server read cache), Custom modules, Plugins, System (backend restart, threads, HTTPS, task runner), Shared workstation, HPC (cluster site), Developer, Debug console (runs arbitrary code) | peanut (this plan) |
| **Person** | Interface toggles (task-list auto-follow, plot auto-refresh, pyramid advisor, view profile). Already per-profile (`PROFILE_KEYS` in `stores/settings.ts`) **and** already in Preferences — Settings shows them a second time | everyone, in Preferences only |
| **Project** | Project, Data patches, Storage scan / reclaim for the open project, Versions prune | everyone |
| **Read-only** | Diagnostics, Other MCP connections (reads the active profile's Claude config) | everyone |

Storage and System each mix scopes, so they cannot be gated whole.

## Decisions

Locked 2026-10-17 (Dominik).

1. **Gate by scope, per section — not per item.** One predicate, `canManageMachine`, in the
   `appControl` store; each machine section gets one `v-if` on it. Today it is "active profile is
   `default`". If a real login with an admin role ever lands, only the predicate changes.
2. **Visibility, not security.** Anyone can pick peanut in the launch picker, and the routes do not
   check who is asking. Revisit with a real login (then the routes check the same role).
3. **Hide, don't show read-only.** One `v-if` per section, against disabling dozens of
   controls one by one. Cost: a non-peanut user can't see what the machine is set to (e.g. which
   compressor). Revisit if someone actually needs to see it.
4. **Data patches are for everyone.** They change the open project's data, which is the project
   owner's call, not the machine owner's. The same reasoning covers Storage scan / reclaim of the
   open project and Versions prune.
5. **Quit stays visible to everyone.** A person at a shared box must be able to shut the app down.
   No new Quit control — the sidebar footer and Settings → System are its two homes; backend
   Restart is already dev-only.
6. **Person settings live in Preferences only.** Delete the duplicated Interface toggles from
   Settings, rather than gating them.
7. **A single-profile install sees no change** — its only profile is peanut.
8. **No master "this machine is shared" switch** (carried over from P8): the Shared workstation
   section keeps one toggle per item and moves onto `canManageMachine`.

## Phases

### P1 — The predicate + whole-section gates
- `canManageMachine` computed in `stores/appControl.ts` (next to `activeProfileName`).
- `v-if` on the sections that are machine-only end to end: Software updates, Custom modules,
  Plugins, Shared workstation (replace its own `activeProfileName === 'default'` check), HPC,
  Developer, Debug console.
- Guide anchors (`data-guide`) inside gated sections: a guide step pointing at a hidden anchor must
  skip, not stall — check the guide runner for each anchor in these sections.

### P2 — Split Storage and System, gate the machine halves
- Storage → *Storage* (open project: scan, reclaim, versions) + *Storage defaults* (compressor,
  layout, keep previous version, read cache) under the predicate.
- System → service status + Quit (everyone) + the machine controls (threads, HTTPS, task runner
  enable) under the predicate. Verify each control's scope while splitting — see open questions.

### P3 — Remove the duplicated Interface toggles from Settings
- Delete the Interface section's per-profile toggles; Preferences → Interface is their home.
  Check `data-guide` anchors and `docs/todo/GUIDE_ADDITIONS_PLAN.md` for steps that pointed at them.

## Answered while building (2026-10-17, Dominik)

9. **Updates are peanut's.** Not just the Settings section: the header update badge, the What's-new
   update card and in-app install / restart-to-update (`canApplyUpdate`) are gated on
   `canManageMachine` too. Other profiles see no update prompts at all.
10. **Service controls (Notebooks, preview worker) are for everyone** — they stay in System.
11. **The Cecelia MCP row moved to Preferences → Kiwi.** It reads the active profile's own Claude
    config. `composables/useMcpConnections.ts` now serves both it and Settings → Other MCP
    connections.

## How it was built

- P1: `appControl.canManageMachine`; `v-if` on Software updates, Custom modules, Plugins, Shared
  workstation, HPC, Developer, Debug console. The tour's *Updates* step carries
  `onlyIf: c => c.canManageMachine` — new `GuideStep.onlyIf`, applied once by `guideStepsFor`
  when a guide starts, because a guide step pointing at a hidden section shows a "not found" card.
- P2: Storage → *Storage* (open project) + *Storage defaults* (gated). System → service rows +
  Quit (everyone) + a new gated *Server* section (HTTPS, CPU threads). Dev-only rows (Restart,
  worktree switch, task runner) stay in System — they only exist on a dev checkout.
- P3: the three Interface toggles left Settings (they were already in Preferences). *View profile*
  stays in Settings → Interface: it is per-person, but its editor and guide anchor live there and it
  has no Preferences twin to fall back on.
