# Prompts & audit records

Historical, **reference only** — the exploratory prompt docs and one-off audits produced while
building features (feature briefs handed to an agent, porting watch-lists, code audits, redesign
prompts). They are kept as a record of *what was asked and investigated*, not as instructions to
re-run.

This is deliberately separate from [`../todo/`](../todo/README.md):

| Dir | Holds | Status |
|---|---|---|
| `docs/todo/` | **Parked plans** — `*_PLAN.md` with locked decisions + a phased build sequence | live design; delete/promote when built |
| `docs/prompts/` | **Prompts & audits** — the briefs/investigations behind that work | frozen record; not re-run |

When a prompt here has produced a durable design, that design lives in a permanent `docs/<AREA>.md`
(or a `docs/todo/*_PLAN.md` while in flight) — not here. Nothing in this folder is authoritative.
