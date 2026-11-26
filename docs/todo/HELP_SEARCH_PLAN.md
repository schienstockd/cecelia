# Help search — find a guide from the jump search

**Status:** **parked** (2026-11-27, `origin/main` `e2e073c8a`). Nothing built. Brief:
[`docs/archive/HELP_SEARCH_PROMPT.md`](../archive/HELP_SEARCH_PROMPT.md). Read its banner first: most of
what the brief asks for already exists in the guide system.
Related: [`GUIDE_SYSTEM_PLAN.md`](GUIDE_SYSTEM_PLAN.md), [`GUI_LAYOUT_PLAN.md`](GUI_LAYOUT_PLAN.md) (M8,
the jump search), [`CONTEXTUAL_HINTS_PLAN.md`](CONTEXTUAL_HINTS_PLAN.md).

## The problem

A user asks "where is the button to capture a movie?". The app already has the answer: the guide
*Record a movie* (`lib/guides/movies.ts`) points at each control in turn, across into the viewer window.
But the user can reach it only through Help ▾ → Guides. If they type the question into the jump search
(Ctrl K) instead, "capture movie" lands on the *Animation* page (it matches that page's tip), which is
the keyframe route, not Share → Record movie.

## Approach

**Make guides searchable.** Don't build a second registry of controls. A guide already holds what
"show, not tell" needs: the anchors, how to reveal a hidden control (`reveal`), the viewer-window relay
(`window: 'viewer'`), and a test that fails when a step's anchor leaves the markup (`guides.test.ts`).
Search only has to find the right guide and start it.

### H1 — guide hits in the jump search (one PR, the whole first step)

1. **`keywords` on `GuideDef`** (optional, `lib/guides/types.ts`): the words a user would type that are
   not in the title or summary. They live in the guide's own file, so they change in the same diff as
   the guide. Seed:
   - `record-a-movie`: capture, video, export, mp4, recording
   - `correct-a-mask`: correct labels, fix segmentation, edit tracks, relink
   - `gate-populations`: gating, gate, threshold
   - `build-an-animation`: keyframe, camera move, fly-through
2. **A fourth hit kind, `guide`, in `jumpHits`** (`utils/jumpSearch.ts`). It matches over title,
   summary, group and keywords with `matchesQuery`, the same rule as everything else. Second line:
   "Guide · <group>", or the unmet prerequisite ("needs a time series"), as `GuidesDialog` shows it.
3. **Ranking:** a label that starts with the query still comes first. After that, guides rank above
   pages, then functions, then images. A how-to question should open the walkthrough before a page that
   only mentions the words in its tip. So "movies" still gives *Movies & figures* (label prefix) and
   "capture movie" gives *Record a movie*.
4. **Picking a hit** calls `guide.start(id)`, the same call the Guides dialog makes, then closes the
   box.
5. **Tests** (`jumpSearch.test.ts`): "record movie", "capture movie" and "export video" put
   `record-a-movie` first, "movies" still puts the Movies page first, and "correct labels" puts
   `correct-a-mask` first. Esc, arrow wrap and Ctrl K stay unchanged (the existing tests cover them).
6. Update the inventory line in `docs/inventory/FRONTEND.md`.

Cost: one field, one loop in a pure function, one `go()` branch, about six test cases. There is no new
registry to maintain. A stale guide anchor already fails `guides.test.ts`.

### H2 — "Ask Kiwi" row on a miss (small, independent)

When there are no hits, show one row: "Ask Kiwi: <query>". Picking it opens the cockpit with the text
in Kiwi Ask's question box. That needs a small new entry point: `stores/kiwi.ts` keeps only the attached
refs as its draft, not the question text. Nothing calls a model while the user types. Show the row only
when AI is on (`observer.ai.enabled`), so it doesn't point at a feature that is switched off.

## Not built, and what would reopen it

- **Per-control `data-help` on every `data-guide` anchor, plus a test that runs both ways.** There are
  about 141 static anchors, most of them guide plumbing that nobody would search for. Reopen it if H1 is
  in use and people search for controls that no guide covers (screenshot, Share, channel lens, Find
  ROIs: none of these has a guide today). Even then, the better answer may be **write the guide**, so
  that search finds it.
- **Runtime liveness check in the judge.** The doc-shots engine already fails on a missing anchor for
  the pages it captures. A judge pass that drives every guide would reuse it, but it waits on
  `GUIDE_RUNS_PLAN.md` (parked).
- **Miss logging.** Only build it if H1 ships and we want evidence about which queries find nothing.
  The format would be local only, query text and a timestamp, with no image names or paths.
- **Unifying `searchRefs` with `jumpHits`.** H1 doesn't touch Kiwi's matcher, so the question doesn't
  come up yet.

## Open questions

- Should a guide whose prerequisites are unmet still start from search, as it does from the dialog, or
  should it open the dialog instead?
- The seed keywords above are my guess at what users type. Real questions (support messages) would be
  better.
