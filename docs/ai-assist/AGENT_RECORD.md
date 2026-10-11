# How the agents are doing — the README chart, and what it can't tell you

The README's *How this software was built* section shows a chart of how the agents' pre-commit
reviews go, week by week. This page says what it measures, where the numbers come from, and what
they don't show. It is written by hand; the chart is generated. Nothing rewrites this page.

## What is counted

Every change an agent commits here is first read by two reviewer agents (`pixi run recital`): one
looks for a fix that missed its sibling call sites ([`FANOUT_AUDIT.md`](FANOUT_AUDIT.md)), the other
for new code that duplicates an existing helper or skips a convention
([`CONVENTION_CHECK.md`](CONVENTION_CHECK.md)). Each finding they raise has to be resolved in the
commit message, and a git hook blocks the commit until it is. All of it is logged to the
effectiveness log ([`EFFECTIVENESS_METHODOLOGY.md`](EFFECTIVENESS_METHODOLOGY.md)).

Per ISO week (Monday–Sunday, UTC):

- **Changes reviewed before commit.** One branch + parent commit a recital reviewed. Running recital
  again on the same change counts once; a reviewer that errored counts none.
- **What happened to the findings.** Each finding once, in the week it was first raised, with the
  latest outcome logged for it: *fixed before commit*, *shipped knowingly* (the agent said why),
  *called a false positive* (the agent said why), *dropped*, or *no outcome logged* (mostly from
  before the hook required one). Shown as a share of that week's findings, with the counts beside
  the bars; *per 100 changes* above each bar is findings ÷ changes reviewed.
- **Notes under each week.** How many sessions; *partial week* (the first, and the one still
  filling up); *few sessions* under 10; how many merged changes edited the reviewer prompts and
  `CLAUDE.md` that week.
- **The weekly judge line.** From the newest judge pass ([`WEEKLY_JUDGE.md`](WEEKLY_JUDGE.md)):
  the findings nobody fixed, re-checked against `main` — how many are open bugs, parked (can't
  happen in today's code), gone, or dismissed.

## What it can't tell you

- **It starts on 2026-09-27.** A few weeks is not a trend, so none is drawn: no rolling average, no
  fitted line. Read the weeks side by side, not as a curve.
- **The ruler moves.** The reviewer prompts and the rules they check against were edited most weeks
  so far — that is what the notes under each week count. A week with more findings may have a
  stricter reviewer, not a worse agent.
- **Fixed before commit is the agent's own word.** The hook checks that every finding got an
  outcome, not that the outcome is true. The weekly judge re-checks the ones that weren't fixed;
  nothing re-checks the ones marked fixed.
- **Silent misses aren't here.** A bug neither reviewer raised doesn't appear anywhere. A falling
  finding rate can mean cleaner code or blinder reviewers.
- **No causal claims.** A rule edited one week and fewer findings the next is a coincidence until
  shown otherwise; the chart puts them side by side and leaves it there.
- **One maintainer, one codebase.** These are this project's sessions, not a benchmark.

## What is published

Aggregates only: counts per week and the judge's counts per status. No finding text, file path,
branch, session id or uid leaves the machine. Every label in the image is fixed text or a number.

## Where it lives

`pixi run effectiveness-chart` (`scripts/judge/chart.py`) renders the SVG and the numbers behind it
(`effectiveness.json`) from the local log and the judge's records. The weekly judge runs it after
each pass and force-pushes both, as the only commit, to the orphan
[`judge-assets`](https://github.com/schienstockd/cecelia/tree/judge-assets) branch. The README
embeds the SVG from there, so `main` gets no weekly commits. Operator notes:
[`WEEKLY_JUDGE.md`](WEEKLY_JUDGE.md) → *The chart*.
