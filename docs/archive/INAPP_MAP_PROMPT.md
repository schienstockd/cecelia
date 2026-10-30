> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

# INAPP_MAP_PROMPT

Build an in-app **Help → Map**: a compact, clean, clear visual map of Cecelia. It is a map, not a manual. One screen, scannable in seconds, every node clickable. Replaces reading `user-docs/README.md` (PR #1841) as the way users find their way around.

## Where it lives (audit first, then decide)

Working hypothesis: the map is the **front page of Guides** (compass icon, top right), because users already go there for help, the nodes point at guides, and the "Intravital timelapse" recipe already sits at the top. Also surface it on **first run** (welcome dialog) and in the **empty-project state**, with the day-one path highlighted. The generated `user-docs/README.md` stays as the GitHub fallback.

This is a hypothesis, not a decision. Before building, audit the Guides UI, welcome dialog, empty states, sidebar and Help/compass menu, and **recommend the best integration**: front page of Guides, a dedicated Help page linked from the compass, or something better you find. Weigh: discoverability, one place to learn, no new sidebar clutter, fit with existing patterns, and how the map and guide list relate (map as index, guides as detail). Post the recommendation with reasons and the rejected alternatives in the plan, and wait for my go-ahead before building.

## Principles

- **A map, not a document.** No paragraphs, no tables on the map itself. Names, icons, one short label at most.
- **One screen.** Fits without scrolling at normal window size. Detail appears on hover or click, never inline.
- **Day one first.** The default view shows only the day-one path. Everything else is visible but dimmed, or behind one "Show all" toggle.
- **Every node goes somewhere.** Click = open that page/panel, or the matching in-app guide. No dead nodes.
- **One source.** Structure lives in a data file. The map, the markdown reference and the CI check all derive from it. No hand-kept copies.

## Do first (read, then post a plan before coding)

1. Read `CLAUDE.md`, `docs/MAINTAINABILITY.md`, `user-docs/README.md`, the guides system (`frontend/src/lib/guides/singleTopicGuides.ts`, `mcp/cecelia_mcp/guides.json`), the Vue router, sidebar and view-profile code.
2. Check existing deps and components. Prefer reuse. Any new dependency needs a justification.
3. Find how the app opens a guide and navigates to a page/panel today.
4. Audit the Guides selector as it stands: how many guides, how they are grouped, how recipes are shown, and how a user picks one. Count the entries and report what's hard to scan.
5. Audit entry points for the map (Guides front page, welcome dialog, empty-project state, compass menu) per "Where it lives".
6. Stop and report if this brief conflicts with the code.

## Data file (the source of truth)

One typed file (YAML or JSON plus a TS type; follow repo conventions). For each entry:
`id`, `label`, `group`, `icon`, `short` (max ~6 words), `dayOne` (bool), `target` (route or panel id), `guide` (id or null), `needs` (ids), `pairedWith` (id or null).

Groups follow the sidebar: Data, Populations, Explore, Analysis, Pipeline. Plus Panels, Claude (optional), Scale up, Settings. Seed it from `user-docs/README.md`; do not invent entries.

## The map (UI)

- **Hero: the day-one path as a horizontal flow.** Import → Metadata → Segment → Gate → Phenotype, with Track → Behaviour branching for timelapses. Node = icon + label. Arrows show order.
- **Below or beside it: the groups as compact clusters.** Each group is a small card holding its pages as chips (icon + name). Optional and "No" pages dimmed.
- **Pairs drawn as linked pairs:** Gate ↔ Phenotype, Track ↔ Behaviour, Cluster regions ↔ Spatial. Drawn once, not as a table column.
- **Hover/focus:** tooltip with `short`, what it needs, and the guide name.
- **Click:** go to the page/panel; secondary action opens its guide.
- Optional/advanced (Claude, Scale up, Extending, Settings) are small, quiet clusters at the edge. They must not compete with the day-one path.
- Respect view profiles (hidden sidebar pages are dimmed or hidden consistently).
- Match app theme; light and dark.
- Keyboard accessible; not colour-only.
- Prefer plain Vue + SVG/CSS grid. Use Mermaid only if it can meet all of the above, otherwise don't.

## Guide selector → accordion

The guide list is now long. Make it an **accordion**, same grouping as the map (Data, Populations, Explore, Analysis, Pipeline, Claude, Scale up, Extending), so the map and the list use one vocabulary.

- Recipes (e.g. "Intravital timelapse") stay pinned at the top, not inside an accordion section.
- Groups collapsed by default, except the one containing the current page's guide, or the day-one group on first run. Only one or several open at a time: pick what fits existing patterns and say why.
- Clicking a map node opens Guides with that guide's group expanded and the guide highlighted.
- Group membership comes from the same data file as the map (`group`, `guide`), not a second list.
- Keep search/filter if it exists; if it doesn't and the list is still hard to scan, propose one.
- Keyboard accessible; state (open groups) not persisted unless existing patterns do so.
- Don't change guide content or ids.

## Generated outputs

- `user-docs/README.md` becomes generated: a short intro, the Mermaid overview, and the reference tables, all produced from the data file by a script (`npm run`/`just`, follow conventions). Keep it readable on GitHub.
- Hand-written prose that isn't structural (glossary of Project/Set/Image/Version, "Where to write things down", Claude section) stays in a separate hand-edited markdown that the generator includes. Say how you split it.

## Enforce

CI check fails when:
- a router page/panel has no entry in the data file,
- an entry's `target` or `guide` doesn't exist,
- generated `user-docs/README.md` is out of date.

Update the `CLAUDE.md` index row to say the check exists.

## Out of scope

- The renames from #1841's open questions (Whiteboard → Pipeline, "Capture activity" button, Profile terms). Separate PR. Use current names in the data file; renaming should then be a one-line change per entry.
- New guides or rewriting guide content.
- Fixing the stale dev docs listed in #1841.

## Constraints

- Follow `docs/MAINTAINABILITY.md` and the inventory check (new shared files/routes may need inventory entries).
- Regenerate `guides.json` only if guide text changes.
- Don't ship it if it needs scrolling or reads as a wall of boxes. Cut nodes before shrinking text.

## Verify

- Run the app. Open the map from every entry point you implemented, and test the Guides accordion (expand/collapse, deep link from a map node, recipes pinned).
- Run the app. Open the map. Click every node, chip and pair; confirm each lands correctly.
- Check at a small window size and in dark mode.
- Run the CI check, then break it on purpose (rename a route, delete a guide id) and confirm it fails.
- Confirm generated `user-docs/README.md` renders on GitHub.
- State plainly what was run in the app and what was checked only in code.

## PR

One PR. Branch off `main`, or off `docs/user-docs` if #1841 is unmerged; say which. Title: `feat: in-app map and guide accordion generated from a single data file`. List reservations at the end.
