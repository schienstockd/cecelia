> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run.
>
> **Outcome:** answered as a plan, [`docs/todo/PR_CAPTURES_PLAN.md`](../todo/PR_CAPTURES_PLAN.md) (2026-11-15, in #2243); nothing built. Two of its guardrails are partly in
> place already: since #2243, `pixi run doc-images` warns about image files no shot produces and drops
> stale `images.json` entries, and pictures of the image are JPEG at the width the page shows them.
> One premise does not hold: the fixtures are lab projects on the developer's machine, so CI cannot
> capture anything; review captures have to be made locally.

# Brief for Opus: GUI captures on PRs without growing the repo or package

Context: PR #2240 ended the booklet series, made C1 a generated page (`user-docs/using-ai.md`) and kept the screenshot engine as docshots (`pixi run doc-fixture`, `doc-shots`, `doc-images`). That PR added about 310 kB of PNGs under `user-docs/images/`. The PR body asked the reviewer to "look at the rendered page" but carried no images.

Question: should PRs that change visible UI or copy carry GUI captures, and how do we do that **without stacking images in git or in the shipped package**?

This is a plan request. Check what exists first, then report. Do not build anything yet.

## 1. Proposed policy (to confirm or correct)

1. **PRs that change visible UI or copy attach before/after captures.** Backend, refactor and docs-only PRs say "no visible change".
2. **Review captures never go into the repo.** They live on the PR (attachment) or as CI artifacts, and expire with the PR.
3. **Only images the docs actually display are committed**, under `user-docs/images/`, under a size budget (section 3).
4. **Docs images never ship in the app package** unless the app displays them.
5. **Captures come from docshots on the same fixture, before and after,** cropped to the changed control via `data-guide` anchors. No new capture tool.

## 2. Why git is the wrong home for PR captures

- A binary blob stays in history forever, even if the file is deleted later. Clone size only grows.
- Many PRs each adding a few hundred kB adds up quickly.
- Regenerated PNGs often differ byte-for-byte even when the pixels are the same, so every regeneration can become a new blob for no reason.
- Fixtures are lab images and the repo is public. Review captures should not become permanent public history.

## 3. Guardrails to propose

- **Image budget for `user-docs/images/`:** a per-file cap and a total cap, checked in CI. Fail with the offending file names.
- **Max pixel width** per image, matched to how the page displays it (plus 2x at most).
- **Lossless optimisation** (and WebP where GitHub renders it) as part of `doc-images`.
- **No rewrite when unchanged:** `doc-images` compares pixels (or a perceptual hash) with the committed file and skips writing if they match. This stops churn.
- **No orphans:** CI fails if an image in `user-docs/images/` is not referenced by any page, and if a page references a missing image.
- **A block on stray binaries in PRs:** flag new image files outside `user-docs/images/`, and any new binary over a threshold.
- **Package exclusion:** confirm `user-docs/` is excluded from installers and release bundles, and add a check that the packaged size doesn't include it.

## 4. Where review captures live

Options to evaluate; recommend one.

| Option | Good | Risk |
|---|---|---|
| PR attachment (image in the PR body or comment) | Reviewers see it inline, nothing in git | Unclear whether `gh` or an agent session can upload images; may need a manual step |
| CI artifact with links posted by a bot comment | Automatable, expires | Artifacts expire; images not inline unless a comment embeds a URL |
| Release asset or a separate assets repo | Stable URLs | Another place to maintain; still permanent and public |
| Orphan "screenshots" branch | Inline via raw URL | Still git history; needs pruning; bloats clones that fetch all branches |

I lean towards attachment or CI artifact. Please verify what `gh` can actually do for images, and say so plainly if it can't.

## 5. Check before planning

- Current repo size and `.git` size, and how much of it is binary. Which paths hold the most?
- Whether `user-docs/` and its images are packaged anywhere (installer, MCP package, Julia package, docs site).
- Whether docshots output is byte-deterministic. If not, what is the cheapest way to avoid churn (pixel compare)?
- Whether `docs/archive/` or old booklet outputs left any binaries in history.
- Which CI jobs exist today that could host a size check and an artifact upload.
- How `data-guide` anchors can drive a crop of "the changed control", and whether a diff-to-anchor mapping is realistic or over-engineered.
- Whether PR templates and the recital checks can carry a "GUI: before/after or N/A" line without adding noise.
- Fixture data rights: are the lab images used in docshots fine to show on public PRs, given they are already shown in committed docs images?

## 6. Report back with

- Current sizes and where growth would come from.
- A recommended home for review captures, with what is and isn't possible for agents.
- The exact CI checks and budgets, with numbers based on the current state.
- What changes to docshots are needed (no-rewrite compare, crop by anchor, optimise).
- A short PR-template addition and a rule for what counts as a "visible change".
- Risks and anything in this brief you disagree with.
