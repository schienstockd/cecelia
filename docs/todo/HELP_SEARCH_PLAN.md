# Help search — find a guide from the jump search

**Status:** **H1 and H2 built** (2026-11-27). The rest is parked (*Not built* below). The plan was reviewed 2026-11-27 by a fresh agent
against the code, and H1 and H2 below include its fixes. Brief:
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

1. **`keywords` on `GuideDef`** (optional, `lib/guides/types.ts`; also on `ModuleTaskGuideOpts`, since
   task guides are built by `moduleTaskGuide()`): words a user would type that are not in the title.
   They live in the guide's own file, so they change in the same diff as the guide. Seed:
   - `record-a-movie`: capture, video, export, mp4, recording, movies
   - `correct-a-mask`: fix segmentation, edit labels, edit tracks, join, split
   - `gate-populations`: gating, gate, threshold
   - `build-an-animation`: keyframe, camera move, fly-through
2. **A fourth hit kind, `guide`, in `jumpHits`** (`utils/jumpSearch.ts`), matched over **title and
   keywords only**. Summaries are left out: a summary names other tools ("instead of gating two
   channels" is the clustering guide's), so matching on it sends "gating" to the wrong guide. Guides
   come from `guideGroupsFor(guidesByGroup(), ai.enabled)`, so the AI-only and AI-off-only guides drop
   out as they do in the dialog.
3. **Questions, not only keywords.** "where is the button to capture a movie?" must find the guide.
   All kinds use one per-word rule: strip trailing punctuation, skip filler words, and let a word be met
   by one of its synonyms. If every word is filler, match on all of them as typed. The plot picker
   already has this rule (`plots/plotPicker.ts` → `matchesPlotQuery`, `STOP_WORDS`, `SYNONYMS`). This
   is its second user, so extract it into `utils/textQuery.ts` (say `matchesWords(fields, query,
   { stop, synonyms })`) and move the plot picker onto it. Each caller keeps its own word lists.
4. **Ranking, two tiers.** Tier 1: hits whose label starts with the query, in the order page > guide >
   function > image. Pages stay first here, so Enter on "track" or "segment" still opens the page.
   Tier 2: every other hit, in the order guide > page > function > image. A how-to question should
   open the walkthrough before a page that only mentions the words in its tip. Ties break on label.
5. **Row.** Icon `pi-compass` (the guides launcher's). Second line: "Guide · <group>", or
   "Guide · N missing" when `guide.prereqState(g)` has unmet entries, as the dialog counts them. The
   component passes that state in, because `jumpHits` is pure. The second line must be allowed to
   shrink, so the label keeps its width (`.jump-sub` is `flex-shrink: 0` today).
6. **Picking a hit** calls `guide.start(id)`, the same call the dialog makes. Start is never gated on
   prerequisites, there or here. A guide already running is replaced, as from the dialog. The search
   is not mounted in the viewer window, so a guide always starts from the main window.
7. **Copy and docs.** The placeholder, `aria-label` and tooltip name guides ("Jump to page, task,
   guide or image…"), as do both header comments. Update `docs/inventory/FRONTEND.md` (jump search),
   `docs/UI.md` (where it lists the ways into a guide), and the entry-point list in `lib/guideOpen.ts`.
8. **Tests** (`jumpSearch.test.ts`, against the real `GUIDES` and `allNavGroups()`):
   - "capture movie", "record movie", "export video" and "where is the button to capture a movie?" put
     `record-a-movie` first.
   - "movies" puts the Movies page first and the guide in the list.
   - "gating" puts `gate-populations` first among the guides.
   - "fix segmentation" puts `correct-a-mask` first.
   - "track" still puts the Track page first.
   - With AI off, `assist-with-kiwi` never shows up.

   The plot picker's own tests must still pass after the matcher is extracted. "Correct labels" is not a
   target: five Segment functions start with that label, and taking the user to them is right.

Cost: one field, a matcher extracted from code that already exists, one branch in `jumpHits` and
`go()`, and about eight test cases. There is no new registry. A stale guide anchor already fails
`guides.test.ts`.

### H2 — "Ask Kiwi" row on a miss (small, independent)

When there are no hits, show one row: "Ask Kiwi: <query>". Picking it puts the text in Kiwi's question
box (`kiwi.prompt` in `stores/kiwi.ts`, already persisted) and opens Kiwi on its Ask section
(`settings.kiwiOpen`; `cc.kiwi.openSection` = `ask`). If the box already holds an unsent question, add
the text after it rather than replace it. Show the row only with a project open and AI on
(`observer.ai.enabled`). Without a project, the cockpit can't ask (`kiwi.ask` returns early). Nothing
calls a model while the user types. Kiwi already answers "how do I…" from `get_guide`.

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

- The seed keywords above are my guess at what users type. Real questions (support messages) would be
  better.
