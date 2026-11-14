# Booklet style guide

How the user booklets are written and built. The plan behind them is
[`docs/todo/BOOKLETS_PLAN.md`](../../docs/todo/BOOKLETS_PLAN.md). In-app wording follows
[`docs/ui/COPY.md`](../../docs/ui/COPY.md) → *House style* (sentence case, one verb per action, the
verb vocabulary). This file adds only what is specific to print.

**Reader:** a new lab member who knows microscopy but not Cecelia. Every term they would not know is
defined the first time it appears, or not used.

## Two kinds

A **guide** (A1, C1–C3) walks the reader through steps; the frame below applies in full. A **showcase**
(B1–B5) shows what a workflow gives and hands the doing to the in-app guide: outcome first, *You need* and
rough time, *Before you image*, three or four capability shots with one line each, honest limits, then
**Do it** naming the recipe's in-app guide. A showcase has no Do/See/If-wrong steps, and no booklet is both.
Plan → D6. Each capability names its recipe step (`step`), and the build prints that step's own
why-line under *In the guide*; the booklet's text adds only what you get, never a rewording of the step,
so the booklet and the in-app guide cannot drift. A step the recipe drops fails the build.

## Fixed: the same in every booklet

The build enforces these (`checkBooklets` in `spec.ts`) or the template renders them, so a booklet
cannot leave them out.

1. **Cover.** Title, the generated number (A1, B2, C1), *For*, *You get*, *You need* and *Ends at*.
   B-series booklets also have *Sample*, which names the tissue in one line.
2. **Before you image** (B-series). Three to five rows: *setting at the microscope → what it limits
   later → where Cecelia shows it*. Use the acquisition advisor's own terms; don't invent thresholds.
3. **Steps.** Each one is *Do* → *You'll see* → *If it goes wrong*, plus *Why* for a setting that
   isn't obvious. The build numbers them. Never type "5b".
4. **Ask AI.** One prompt that works, and where to ask it. It always says it is optional and that AI
   is off until the user turns it on. C-series booklets may drop it, because AI is their subject.
5. **Next.** The booklet that follows, by id; the build fills in its number and title.
6. **Stamp.** App version, commit and build date in every page footer.

The pointer style, the print scale (0.19 mm per CSS px for screenshots) and the colours (the app's
light theme) are fixed too.

## Free: varies between booklets

How many steps there are, how deep the booklet goes, extra figures (a deliberately bad example, a
before and after), side notes, and tables. Use them where they help, not to fill space.

## Writing

- **Use the app's own words.** A control is named exactly as the app labels it, marked `**Label**`
  in the spec (it prints as a chip). Where the app already holds the text (Kiwi's steps, the AI data
  notice), the spec **imports** it from the app's source instead of retyping it, so the two cannot
  drift.
- **Short sentences, active voice, second person.** "Select the channels", not "The channels should be
  selected".
- **One idea per step.** If *Do* needs "and then", it is two steps.
- ***If it goes wrong* is an action**, not a warning: what the reader sees, then what to do. A QC flag
  always gets the action that clears it.
- ***Why* says when to pick it**: "Pick 3D when cells move between planes", not the algorithm.
- **Experimental features are marked** "(Experimental)" in the step title, as the app marks them.
- **No internal names.** No project ids, sample ids, test names or other people's tabs in text or
  shots.
- **Define words at first use.** A booklet that needs jargon puts a `terms` block before it (Blackboard,
  ref, chain, gate).
- **A step past *Ends at* is marked `optional`**, and *If it goes wrong* holds only real failures; a
  caution goes under *Why*.
- **Numbers come from the fixture, never the keyboard.** Write `{{lt.FtGoJO.planes}}`, `.channels`,
  or `.<gate>.count` / `.pct`: `pixi run booklet-fixture` measures them on the copy through the app,
  the capture checks a shot's `expect` against the same number, and the build prints it. A count
  typed by hand ("26 z-planes") fails the build. Anything else numeric a person checks on the PDF.

## Acquisition first

Every B-series booklet opens with *Before you image*, because what was acquired decides what can be
measured. The message to carry: **aim for images you can measure, not images that only look good.**
Catch a problem at the microscope instead of reshaping the analysis afterwards to rescue weak data.
Point at where the app already shows the settings (the image metadata dialog, the QC flags, the set
check, the acquisition card) instead of explaining optics.

## AI

AI is optional everywhere, and every booklet works with it off. Booklets say what Kiwi or Claude Code
can do **at that step**. The C-series explains the two in full. Never promise more than the tool's
real abilities: the app's own cards (`frontend/src/lib/claudeOverview.ts`) are the source.

## Screenshots

A step names a shot by id; the booklet's `shots` list says how to reach it (project, image, route,
user actions), what to point at, what to crop to and which labels it must show. Capture with
`pixi run booklet-shots -- --base <url> --config-dir <dir>` against an **isolated** instance (own
`CECELIA_DEV_DIR` and `CECELIA_PORT_SLOT`; it refuses 8080/5173), then `pixi run booklets`. Before
that, `pixi run booklet-fixture` sets up what the booklet's `fixture` names: lab images copied raw
into the instance (never public data, never the source written) and profiles with AI on or off. A
shot names those by key and runs as its `profile`. The pointer's wording and side live in the spec
and apply at build time; a pointer with no label is the ring alone, for a narrow panel. The crop and the
pointer are drawn by the build from the recorded boxes, so restyling never means recapturing.

Anchors are the guides' `data-guide` ids, so a shot and an in-app guide name the same control. A control
with no anchor gets one in the app (same change), not a CSS path.

The build warns on a crop too wide to read (`--strict` fails). The rules come from the review of
the first drafts ([`docs/archive/BOOKLET_RESERVATIONS.md`](../../docs/archive/BOOKLET_RESERVATIONS.md)
§8; read its banner first):

- set the page, the task function and the image explicitly, never inherited;
- capture the state *before* the step unless the result is the point;
- crop to what the caption is about;
- no relative times, ids or unrelated images in the pixels;
- callouts never cover a value the text cites;
- legible at print size.
- AI output (a Kiwi reply) varies between runs: capture it from a **saved** entry on a fixed question,
  never a live turn, and name the entry in the spec.

## Pre-ship checklist (a person, on the PDF)

- [ ] Every label in the text matches the app
- [ ] Each shot shows only its target and is legible
- [ ] No timestamps, internal names or unrelated images
- [ ] No callout covers a cited value
- [ ] Shots show the *before* state, or the result where that is the point
- [ ] Terms are defined, and *Why* lines are there for non-obvious settings
- [ ] Every check step says what failure looks like
- [ ] Shared modules are used, not copied
- [ ] Any app bug found while capturing is filed (and the step is held until it is fixed)
