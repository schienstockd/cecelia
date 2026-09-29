> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was
> researched at the time. Reference material for the bespoke-eval indirect-prompt work
> only — not a spec, not gospel. The other session (feat/bespoke-eval-additions,
> indirect-prompt track) can consume this as one input among others when picking
> prompts; anything here should be re-verified before it lands in the suite.

# Reference: sourcing indirect eval prompts from past init messages

## Ask that produced this note

Current `scripts/claude_md_eval/prompts/*.md` are **direct** — they name the guarded
util (`zarr_utils`, `label_props`, `run_py`) and hand the agent a tightly-scoped
one-shot ("add a helper that opens an OME-ZARR store …"). Direct prompts prove the
canary case (did CLAUDE.md load at all?) but don't prove the realistic case (does a
guardrail survive when the ask is phrased like a real user bug report and the agent
has to discover the util itself).

Dominik asked: could we sample from his past init prompts (the first user message of
past `claude` sessions on this repo) to seed **indirect** prompts — real app tasks in
his voice, no naming of the rule under test, natural discovery pressure on CLAUDE.md?

## Source

`~/.claude/projects/-home-dominik-cc-workspace-cecelia/*.jsonl` — 290 session files,
197 with a real first-user message after skipping meta and tag-only records.

Session ID = filename prefix (first 8 hex chars). The first `type:"user"` record with
non-tag content is the init prompt.

## What was excluded

Not every past session is a coding task. These patterns filtered out (all excluded
from the shortlist below):

- `move <X>.md ... read it` — meta-tasks pointing at `~/Downloads/prompts/*.md`,
  not app work. **Dominik explicitly asked for these to be excluded.**
- PR / CI / release audits — `audit 1232 for release cut`, `780 ci is failing`,
  `advance main cecelia-feijoa`, `peek at feat/...`, `check <repo>`.
- Worktree/plan reviews — meta about design docs, not the code the docs describe.
- `<pasted_content>` blocks — long peer-agent excerpts that dominate the message.

## Shortlist by guarded surface

Curated with cheap keyword heuristics — grep on the message text, one bin per
CLAUDE.md rule. Session IDs are pointers; the other session should re-read the full
transcript before using anything.

`[IMG]` = message contains `[Image #N]` — screenshot bug reports. Not replayable as
text-only prompts; either strip the image reference and rewrite around it, or skip.

### tracking / gating (16 candidates)

- `0e40f886` [IMG] — multiple tracked pops in the viewer, colour selection missing
- `153008f2`       — clustered tracks with hmm states (fXgbTl)
- `3285f103`       — bidirectional shared-context marking
- `4786352d`       — analysis-board image strip lost scalebar/legend/tracks post-napari
- `4d71cec4` [IMG] — duplicate-population-name error text is squished
- `6918f95c`       — viewer point outlines + size knob
- `6f10f3cd`       — PR 936 (WebGPU binding_array plan) sonnet review
- `7c1e4dcc`       — gating plots reset on tab return; show-gate action
- `aac1036c`       — stray `plotting-canvas-and-track-df.md` misfiled
- `b6656610` [IMG] — track-cluster display audit (XcPcu8)
- `bcd4d525` [IMG] — magenta tracks don't align with yellow dots
- `cdfc0de8`       — napari-vizsla reference, resurrect track correction
- `e61a3b5f`       — what's-new modal animations missing post-napari
- `e9fcc9ac`       — hmm states/transitions not available as cluster params
- `f016eaf7`       — tracked pops can't be coloured by population colour

### segmentation (14 candidates)

- `105345fd` [IMG] — temporal smoothing produces pixelated holes
- `24a9c411`       — intravital viewer flickers, prefetch too conservative
- `2cba885b`       — flowKat mask 404 at timepoint 5
- `47178f26`       — thunderbolt live-preview icon missing from viewer control panel
- `6ebccc17`       — temporal smoothing speckles; explore denoise alternatives
- `90b46627`       — preprocessing modules: `editImages.bin` unknown; needs version disclosure
- `9c9fc0f6`       — live label preview icon missing during seg run
- `a1668f3d`       — cellpose on M3 mac very slow
- `ba7a1879`       — spatial smoothing blocked on non-timecourse images
- `cd0e9574`       — "correct labels report stale downstream" — what is this?
- `e184d481`       — flow-warped temporal stat not in vis aid
- `f856002e`       — MIP-along-Z preprocessing; imagej format
- `fadcc046`       — normalise-percentile step size 0.01 → 0.1
- `ff9d1f76`       — copy function params between images in the GUI

### h5ad / label_props (7 candidates)

- `450fb697`       — re-imported image, `Metadata 404: No filepath registered`
- `62a5a45e`       — nextflow-comparison thinking exercise (weak fit — skip)
- `7a7e7563`       — image-version lineage flow in metadata modal
- `b388993d`       — populations panel layout accordion + hideable options
- `bfce10ec`       — audit julia zarr access post-webgpu port
- `f683211c`       — c91ICQ missing timescale — find it in metadata
- `fb73262e`       — legacy-quotes migration issue on qcN9Br

### ccid.json / versioning (1 candidate)

- `ec70ddce`       — rename value_names; tracker ignores population selection

### zarr / image (1 candidate)

- `9fb138d2`       — crop image failure, path + dims mismatch

### Surfaces with no organic hits

- `py-spawn` — no past init prompt naturally forces `run_py`. Needs a **synthetic**
  indirect prompt (a task where the agent would obviously reach for `subprocess.run`
  on python — e.g. "run this python one-liner to compute X across every image
  version").
- `atomic-utf8` — same. Needs a task that writes durable JSON or reads a text file.
- `windows-compat` — same. Needs a task that touches paths / processes / `~`.
- `config / dev-dir` — same.

**Don't** synthesise indirect prompts by rewriting the direct ones into narrative
form — that just launders the naming. A useful indirect prompt is one where the
canonical util is *implied* by the task, not paraphrased from a doc line.

## Caveats for whoever picks these up

1. **Session ID = filename prefix** under
   `~/.claude/projects/-home-dominik-cc-workspace-cecelia/`. Full path:
   `~/.claude/projects/-home-dominik-cc-workspace-cecelia/<8hex>-<rest>.jsonl`. The
   init prompt is the first `type:"user"` record with `content` that isn't a system
   tag block, a `<pasted_content>` block, or a short interjection.

2. **Strip `"in a separate worktree."`** — the runner already spawns its own detached
   worktree; leaving the preamble in confuses the arm.

3. **`[IMG]` prompts need rewriting** — replace image references with a text
   description or drop the prompt. A CLI eval can't render a screenshot.

4. **Realism costs money.** Direct prompts are ~one-shot; indirect ones are longer,
   more discovery, more turns. Expect 2–5× cost per run vs the current direct suite.
   Pilot one indirect prompt before batching, and reconsider the ablation cadence if
   the with/without pair doubles the multiplier again.

5. **Two-tier suite is worth considering** — keep the direct prompts as the fast
   pre-flight (does CLAUDE.md load at all?), add ~5 indirect ones as the realism
   pass. Different budgets, different cadences. The canary already sits at the
   pre-flight tier by design.

6. **Grader signals stay the same.** The existing `compliant_signal` /
   `anti_signal` regexes (canonical util call vs bare `zarr.open`) still work on the
   diff — the *prompt* changes, not the grader shape. `tool_order` graders (grep
   before write) may want the `input_match` widened once the ask is less specific.

7. **This note is REFERENCE not gospel.** The heuristic bins are one-pass regex
   matches on ~200 messages; the "surfaces with no organic hits" claim is an
   absence-of-evidence statement, not evidence of absence. If a bin looks under-
   represented, re-run the search with different keywords before concluding the
   session history doesn't have what you need.

## How this list was produced (so it can be regenerated)

```python
import json, glob, os, re
files = sorted(glob.glob(os.path.expanduser(
    "~/.claude/projects/-home-dominik-cc-workspace-cecelia/*.jsonl")))
# for each file, take the first type:"user" record whose content is
# non-empty, doesn't start with < or [, and doesn't contain "system-reminder"
# then apply the exclusion patterns above, then bin by keyword.
```

Exclusion + keyword regexes are inline in the "What was excluded" and "Shortlist"
sections above. Re-run with different keyword bins to explore other guarded
surfaces.
