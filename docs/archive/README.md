# Archive — prompts & audit records

**Nothing in this folder is authoritative, and nothing here should be acted on.** Every file carries
that banner on line 1, because a grep hit shows a path and the first thing a reader opens is the top
of the file — that is the only place the warning reliably lands.

These are the exploratory prompt docs and one-off audits produced while building features: feature
briefs handed to an agent, porting watch-lists, code audits, redesign prompts. They are kept as a
record of *what was asked and investigated*, and because the project is openly AI-assisted and the
asking is part of how it was built. They are not documentation.

**Why this is `archive/` and not `prompts/`.** The old name described where the files came from, and
left a reader to infer their status. A brief reads like a confident spec long after it has stopped
being one — and the design it describes has usually moved, been rejected, or shipped differently. The
folder name now states the status, since that is the thing a reader needs and the provenance is not.

| Dir | Holds | Status |
|---|---|---|
| `docs/` | The architecture and design reference | **authoritative**; keep current |
| `docs/todo/` | Parked plans — `*_PLAN.md`, locked decisions + a phased build sequence | live design; delete/promote when built |
| `docs/archive/` | Prompts & audits — the briefs and investigations behind that work | frozen; not authoritative, not re-run |

When a prompt here produced a durable design, that design lives in a permanent `docs/<AREA>.md` (or a
`docs/todo/*_PLAN.md` while in flight). Several `*_PLAN.md` files name the brief they superseded.

**When a brief's work lands, give it an outcome note** — under the banner, saying what shipped and
where the design lives, and flagging any premise that turned out to be wrong. Do it in the PR that
ships the work, not in a later sweep. `prompt-welcome-guides-entry.md` is the worked example: it asked
for a CTA on a welcome page that does not exist, and for a first-time ring that was rejected, so a
reader without that note has a plausible spec for two things deliberately not built.

**One file here is cited from outside:** `python-audit-report.md` — `FAQ.md` sends readers here for
the full "why is there still Python" breakdown, and `docs/FUTURE.md` cites it. Read it as a
point-in-time analysis, not as current fact.

Its companion `julia-port-watchlist.md` was written as a *living* checklist and is the clearest case
for why this folder is named what it is: the recheck it describes stopped, because the port was judged
**not feasible** rather than not-yet-possible, and nothing in the file said so. A reader would have
found a maintained-looking watch list for an abandoned idea.
