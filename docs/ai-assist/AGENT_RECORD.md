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

Four line panels, one point per ISO week (Monday–Sunday, UTC):

- **Changes reviewed and PRs merged.** A change reviewed is one branch + parent commit a recital
  reviewed. Running recital again on the same change counts once; a reviewer that errored counts
  none. PRs merged counts merge and squash commits on `main` (`git log --first-parent`) beside it:
  a PR usually carries more than one reviewed change.
- **Findings per 100 changes**, one line per reviewer (fan-out audit, convention check): the week's
  findings ÷ its changes reviewed. Each finding counts once, in the week it was first raised.
- **Findings fixed before commit**, one line per reviewer: the share of the week's findings whose
  latest logged outcome is *fixed before commit*. The rest were shipped knowingly, called a false
  positive, dropped, or have no outcome logged (mostly from before the hook required one); that
  split is in `effectiveness.json` and the image's description, not drawn.
- **The judge's verdict on fan-out notes nobody fixed**, from the newest judge pass
  ([`WEEKLY_JUDGE.md`](WEEKLY_JUDGE.md)), by the week each note was raised: how many are a *real
  bug* (open, or parked: real but can't happen in today's code), *gone* (the code changed under
  it), or *dismissed*. "Notes" because most of them are fan-out **advisories** — lower-confidence
  calls that don't block a commit and so aren't counted as findings above — plus the findings that
  shipped unfixed. The judge doesn't re-check convention findings (they feed its rule proposals),
  so that panel has no convention line. A week the pass fell inside is hollow: it isn't fully
  judged yet.
- **Hollow points and dashed lines** mark a week that is partial (logging began inside it, or it
  hadn't ended when the chart was drawn) or had under 10 sessions.

## What it can't tell you

- **It starts on 2026-09-27.** A few weeks is not a trend, so none is drawn: no rolling average, no
  fitted line: the lines only join one week's point to the next.
- **The ruler moves.** The reviewer prompts and the rules they check against were edited most weeks
  so far (counted per week in `effectiveness.json`). A week with more findings may have a stricter
  reviewer, not a worse agent.
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
