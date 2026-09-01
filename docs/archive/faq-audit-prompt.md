# Task: Audit and trim `FAQ.md`

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.
>
> **Outcome:** applied in the same PR — `FAQ.md` cut from 156 → 78 lines. Merged/trimmed the
> language-choice defence chain, cut the compile-latency war story and the Cellpose-3 pin rationale,
> and stripped the AF-correction historical justification. Follow-up destinations for the cut
> content (Cellpose pin comment, `docs/SHIPPING.md` version pins, `docs/MILESTONES.md` router/test
> compile-fix entry, AF rescale-ceiling story) were named in the audit but not relocated in this PR.

File: https://github.com/schienstockd/cecelia/blob/main/FAQ.md

This doc has drifted from "FAQ" into a dumping ground for implementation trivia, decision rationale, and PR-style detail. Bring it back to what an FAQ should be: short answers to the questions people actually ask.

## What to do

1. Go through each Q&A entry and classify it:
   - **Keep as FAQ** — genuinely a question a new user/contributor would ask, answerable in 2-4 sentences.
   - **Trim** — legitimate FAQ entry but padded with detail that belongs elsewhere (e.g. version-pin rationale, specific library names, migration mechanics).
   - **Cut/relocate** — not FAQ material at all; belongs in `docs/ARCHITECTURE.md`, `docs/SHIPPING.md`, a PR description, or a code comment instead.

2. For anything you cut or relocate, say where it should go instead (don't just delete the information — point to the right home).

3. Rewrite the surviving entries to be genuinely short — aim for 1-3 sentences per answer, no nested justification chains. Cut hedge phrases, repeated qualifiers, and "why not X, why not Y" digressions that aren't the actual question asked.

4. Flag any entry that reads like it's defending a decision rather than answering a question a reader would ask — that's usually the tell it doesn't belong in an FAQ.

## Output

Give me:
- A short list of which entries to keep/trim/cut, one line each, with reasoning.
- A rewritten `FAQ.md` with the trimmed content.
- Anything cut, with a suggested destination file.

Don't rewrite the whole doc's tone or restructure sections unless the content itself demands it — this is a length/scope pass, not a rewrite for style.
