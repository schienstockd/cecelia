<!--
Keep it short. Delete sections that don't apply. The checklist mirrors the
conventions in CLAUDE.md and docs/DEV.md — it's a reminder, not paperwork.
-->

## What & why

<!-- One or two sentences: what changes, and the problem it solves. -->

## Checklist

- [ ] Scoped to one logical change (branch off latest `main`, conventional prefix).
- [ ] Tests updated/added and passing (`pixi run test-pkg` / `test-api` / `test-frontend` / `test-py` / `test-mcp` as relevant).
- [ ] Docs updated (the per-area doc in `docs/`, and `CLAUDE.md` if a convention changed).
- [ ] **Discovery-first**: checked `INVENTORY.md` + grepped before adding anything cross-cutting; reused the canonical helper rather than rolling a new one. Updated `INVENTORY.md` if a new component was added.
- [ ] No analysis logic leaked into the API or frontend (it lives in `Cecelia.jl`); no new language added.

## Notes / caveats

<!-- Known limitations, follow-ups, or anything a reviewer should scrutinise. -->
