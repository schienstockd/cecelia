# UI copy — keep it short (mandatory)

> Split out of `docs/UI.md` (2026-08-20) — the second of the two unconditional frontend lookups.
> Component/class choices are in [`docs/ui/PRIMITIVES.md`](PRIMITIVES.md); everything else frontend
> is in [`docs/UI.md`](../UI.md).


**Default to no explanatory text.** The sidebar entry plus the page's controls almost always says what
the page is. Where orientation genuinely isn't self-evident, one short phrase — **under ~10 words, never
two sentences**.

Why: a paragraph written to explain a feature once sits permanently on a page its owner uses daily,
so it buys clarity once and costs noise forever. Verbose in-app prose is also the most reliable tell
that a screen was generated rather than designed — it makes the whole app read that way. The real
explanation belongs in `docs/`, which is where it actually gets looked up.

| Surface | Budget |
|---|---|
| Page title / subtitle | **none** — the sidebar already names the page (`ModulePage` has no title slot) |
| Tooltip (`v-tooltip`) | one line — what the control does, not why it exists |
| Task-JSON `tip` | **required on every param** — one short line (see *Tooltip coverage*). Lead with a recommended value where one exists (`Start ~5 µm; …`): a tip that only names the trade-off leaves "what do I put here?" unanswered |
| Param advisory (`tasks/paramAdvisors.ts`) | one muted line under the control + the reasoning on hover. For when the right value depends on the user's DATA rather than on wording — e.g. the grid a spacing produces and what it costs to store. See `docs/MODULES.md` → *Param advisories* |
| QC finding | short = the problem, long = the action, imperative (`docs/MODULES.md`) |
| Data-patch `description` (`app/src/maintenance.jl`) | title = what it does, description = one line + the one caveat that matters. Capped at 160 chars by `app/test/suite.jl`. Never restate Dry-run/Apply (both are buttons) and never explain HOW it detects — that belongs in the runner |
| Empty state (`.cc-empty`) | one line; a following action, not a rationale. **Exception:** the two *first-run* states (no projects / no images) get title + ≤2 lines + one CTA — bounded in *Onboarding*, which is the rule for them |
| First-use hint (`HintCallout`) | one line, by construction |
| Guide step (`lib/guides/*.ts`) | **the second carve-out**, and bounded the same way: an optional short `title`, **one** sentence of `text` (≤140 chars), plus **at most four** imperative `bullets` (≤110 chars each). Enforced by `lib/guides/guides.test.ts`. Same reasoning as the first-run states — a guide step is read once, by someone who does not yet know the app, and then never again; it is not sitting on a page they use daily. Anything past the shape is what the budget exists to stop. See *Guides* |

Rewriting long copy short is always in scope — it does not need its own task. When you catch yourself
explaining, put it in the relevant `docs/<AREA>.md` and leave the UI silent.

### House style — how it's written

Length is only half of it. The other half is writing the same thing the same way twice, which nothing
watched until `pixi run ui-copy` could show the whole corpus at once. It found the two halves of the
app had drifted apart along the storage boundary: **task specs had gone Title Case while the frontend
stayed sentence case**, 60 phrases had picked up a second spelling, and one action had up to four
verbs. None of that is visible a file at a time — which is the argument for the inventory.

| Rule | Do | Not |
|---|---|---|
| **Sentence case** for every label, button, header and menu item. Acronyms and proper nouns keep their case. | `Bayesian tracking`, `Drift correction`, `Calculate UMAP` | `Bayesian Tracking`, `Drift Correction` |
| **No trailing period** on a tooltip or a task-spec `tip`. It's a fragment, not a sentence. | `Which image version to crop` | `Which image version to crop.` |
| **One verb per action** — see the table below. | `Select channels` | `Choose channels`, `Pick channels` |

Full sentences still take a period: QC `long` text is imperative prose and keeps its punctuation, as
do multi-sentence notifications.

**Verb vocabulary.** Where two words mean the same thing, one wins. Where they mean different things,
both stay — the distinction is the point, so it's written down rather than left to taste.

| Use | For | Not |
|---|---|---|
| **Select** | choosing from options that already exist | ~~Choose~~, ~~Pick~~ |
| **Show** | toggling visibility | ~~Display~~ |
| **Create** | making a new object | — |
| **Add** | attaching an existing object to a collection | — |
| **Delete** | destroying data permanently | — |
| **Remove** | detaching from a list; the data survives | — |
| **Run** | a task or chain | ~~Execute~~ |
| **Start** | a long-lived service (napari, Pluto) | ~~Launch~~ (as a verb; the noun "on launch" is fine) |

`Create`/`Add`, `Delete`/`Remove` and `Run`/`Start` are **not** synonyms — picking the wrong one is a
copy bug, not a style preference.

**Two of these are now enforced, not just asked for.** `utils/uiCopy.test.ts` fails the build on a
`v-tooltip` literal, a `ModuleLayout` `hint`, or a task-JSON `tip` that runs past **90 characters** or
into a **second sentence** — across every SFC and every task spec. It holds an **exact allow-list**,
not a count (the `cssScenarios` lesson: a count silently permits swapping one violation for another,
and stops meaning anything at zero). Both surfaces were swept to zero, so the bar is that list and
nothing else, and the current single entry is a *notification* whose second sentence is a call to
action rather than an explanation. Before adding an entry, check whether the fact belongs in
`docs/<AREA>.md` instead — that was true of every one of the ~100 strings the sweeps shortened.

**Reading the whole corpus.** `pixi run ui-copy` (`scripts/ui_copy_inventory.mjs`) dumps every
front-facing string — SFCs, task specs, Julia QC text and the What's-New/tip cards, ~1,700 of them —
to a git-ignored `UI_COPY_INVENTORY.md`, grouped by kind, with the drift signals over the top. Use it
to review the app's language end-to-end; the build-failing subset stays the ratchet. It imports
`utils/uiCopy.ts`, which is the one canonical parser — add an extractor there rather than writing a
second scraper.

**Visibility is not enforcement.** Two surfaces are in the inventory on purpose and ratcheted on
purpose *not*: bare template text nodes (the largest bucket — extraction is heuristic, so a guard
would fail builds on parse noise) and the **tip cards** in `lib/tips.ts` (long-form explainers with a
sketch; there is no agreed length, tone or punctuation rule to hold them to, and they are excluded
from the length signal for that reason). A ratchet needs a decided rule — without one it grows an
allow-list until it stops meaning anything, which is the `cssScenarios` lesson. So these are here to
be *read* and judged by a person, not to break CI.

> **Measure the rendered string, not the binding.** A tooltip binding is an *expression*, so
> `v-tooltip="flagged ? 'Deselect flagged images' : 'Select all N flagged image(s)'"` is 95 characters
> while both branches a user actually sees are well inside budget. Counting expressions over-reports by
> roughly 80% (73 "violations" against a true 41) and sends you off to rewrite ternaries that were
> already fine. `uiCopy.tooltipStrings` extracts the string literals inside each binding and strips
> `${…}`, whose rendered width is unknowable at check time; page subtitles, empty states and QC text
> are not machine-checked at all and stay a review question.

### Tooltip coverage — the presence half

Everything above polices the copy that *exists*. This polices the copy that **doesn't**: an input a
user can change with no hover help anywhere on it. Length had a ratchet from the start; presence
didn't, and presence is the half that drifted — a panel picks up tooltips on six of its ten rows and
nothing can see the four. The first sweep found **94** bare controls across 32 SFCs, **4** icon-only
buttons, and **18** task params with no `tip` — `segment/branching.json` worst at twelve of twelve.

**The rule: every control a user sets a value on carries a tooltip, and every task-spec param carries
a `tip`.** Both are ratcheted to zero with an empty allow-list.

**One exception, and it is a real one: chips, swatches and toggles are covered by their heading.**
`ChipSelect`/`SwatchSelect` are not one hit target but many small ones, and `CcToggle` is a switch you
aim at — a tooltip anchored there renders **on top of the control**, so the hover help hides the thing
you were about to click. Here the blanket rule is actively wrong rather than merely redundant
(Dominik, 2026-08-07, on the channel selection and then the bool params' switch). Such a control
counts as covered when a tipped label or heading precedes it inside the same row, including one
wrapper deeper — the ordinary label-then-control shape, and where the explanation belongs anyway. One
with no tipped heading anywhere is still reported, and the exemption is these three: a plain `select`
beside a tipped label still needs its own.

**Enforced from BOTH sides, because fixing one re-breaks the other.** The presence ratchet is what put
a second `param.tip` on every bool param's switch in the first place. So `duplicateTooltips` fails on
a chip row / swatch / toggle that repeats its heading's tooltip **expression for expression** —
`<label v-tooltip.left="param.tip">` above `<CcToggle v-tooltip.right="param.tip">` is the same tip
twice, and the second one is the one that covers the switch. Comparison is on the source text, so it
catches a repeated literal and a repeated binding alike. `HEADING_COVERED` in `utils/uiCopy.ts`; both
directions pinned in `uiCopy.test.ts`.

**Never put a `v-tooltip` on a CONTAINER that also holds tipped controls.** Hovering the inner button
then fires both — the row's tip and the button's — and they overlap on screen. This is a different
failure from the duplicate-tip rule above (the texts differ, so no detector catches it): it is about
*hover areas nesting*, not about repeated words. Anchor the row's tip on a leaf that no control sits
on top of — the status pill, or the truncated text that actually needs expanding — and leave every
button owning its own. Sibling anchors are fine: two tips on two elements side by side can never both
be hovered. Example: the MCP-connections rows in `SettingsModule.vue`.

Enforced by `nestedTooltips` (`utils/uiCopy.ts`, pinned in `uiCopy.test.ts`), which reads the same
ancestor stack the coverage check uses — a tipped ancestor makes a child *covered* and, at the same
time, makes its own tip a double. 29 pre-existing sites were fixed when the rule landed, so the check
is now a plain "none". Two shapes account for almost all of them, and the fix differs:

| The container tip is… | Fix |
|---|---|
| repeating what the buttons already say (`<div class="cc-btn-group" v-tooltip="'Arrange windows'">` over *Tile* / *Cascade*) | delete it |
| the ONLY cover for an untipped control in the row (a slider, a mode `<select>`) | move it **onto that control** — deleting it trips the coverage rule instead |

Rows whose tip describes the row itself ("drag to reorder", "click to sort") anchor it on the row's
**text** — the title, the tab name, the column label — never on the row element.

**A chip row carries ONE tooltip — group or per-option, never both**, and `duplicateTooltips` now
reports either double. The second one is the reason the coverage rule above had to be amended rather
than extended: the group tooltip and the per-option `tip`s say the same thing in *different words*, so
no comparison finds them, and the tips live in the SCRIPT (`const AXIS_OPTIONS = [{…, tip}]`), where a
template pass cannot see them — `hasPerOptionTips` resolves the `:options` identifier back into the
script block, and answers "cannot tell" (never "no tips") for a prop it cannot follow.

**Which one to keep is per-row, and the label decides it.** On an ICON-ONLY row the per-option tip is
the only thing naming a glyph, so the group tooltip goes (six rows: scope, axis, render mode, draw
tool, movie overlays, delete scope). On a WORD-labelled row where the tips only restate the label —
`Show info messages` on a chip that already says `info` — the tips go and the group tooltip stays. If
both say something, put the row's explanation on its heading, where it does not cover anything
(`BatchMoviesPanel`'s filename attrs).

The eight rows this found were previously *required* to have that group tooltip by the presence
ratchet, which is why the amendment and the sweep are one change: enforcing either half alone turns
correct code red.

| Surface | Checker | Ratchet |
|---|---|---|
| SFC controls — `input`, `select`, `textarea`, `CcToggle`, `ChipSelect`, `SwatchSelect`, `RangeSlider`, `CcCycleButton` | `uncoveredControls` (`utils/uiCopy.ts`) | `uiCopy.test.ts` |
| **Icon-only buttons** — a `<button>` whose whole content is an `<i>` glyph | same | same |
| A chip/swatch/toggle repeating its heading's tooltip | `duplicateTooltips` (same file) | same |
| `params[].tip` in `app/src/tasks/**` and `docs/examples/custom-modules/**` | `each_spec` + `collect_settable!` | `app/test/runtests.jl` |

Both land in `pixi run ui-copy` as *Settable control or task param with NO tooltip*, with a
`file:line` per hit, and the report prints task-param coverage as a fraction.

What is deliberately **out of scope**, so the signal stays worth reading:

- **Buttons with a caption.** "Run" / "Delete set" is already its own help, so requiring a tooltip
  on all 152 of them buys tautologies — the generated-screen noise the copy budget exists to
  prevent. An input's *value* has no caption, which is why inputs are in. **Icon-only buttons have
  no caption either, so they ARE checked** — a bare trash glyph is the CellProfiler case at its
  purest. This is a rule the codebase already followed unasked (139 of 150 carried a tooltip before
  anything checked), so the handful that didn't read as oversights, not as a new imposition.
  An `aria-label` is not coverage: it is read out, never shown on hover.
- **`section` / `group` params.** Container headers ("Advanced", "Filters"), not inputs. Their
  children are checked normally.
- **The wrapper primitives' own definitions.** `CcToggle.vue` holds the checkbox every toggle renders
  through; its tooltip belongs at the call site, so the internal input is skipped.
- **`app/src/plotDefinitions/**`.** These carry a `params` array of the same *shape*, which makes
  them look like another spec surface — they aren't. It is a **defaults bag, not a form**: the only
  consumer is `SummaryPanel.vue`, `props.spec.params?.find(p => p.key === k)?.default ?? d`, which
  reads `default` and nothing else. A `label` or `tip` there renders to nobody, so requiring one
  buys strings that look maintained and reach no user. The controls a user really operates for those
  plots are hand-rolled in the SFC, and the frontend ratchet already covers them. (The top-level
  `spec.label` *is* rendered, in the plot picker, and is unchecked — a small separate gap. Don't
  close it by dragging the whole directory into the param walk.)

`docs/examples/custom-modules/**` is the opposite case and IS in scope: those are real task specs
that `load_custom_modules!` loads and `ParamRenderer` renders, living in `docs/` only because they
are the template a user copies.

**Only `v-tooltip` counts.** A native `title=` is not coverage — it renders as the browser's own
unstyled tooltip, appears on a delay we don't control, and is invisible to the copy ratchets, so
accepting it would let a control pass the check looking nothing like the rest of the app. (Most
`title=` in the codebase is a component *prop* — `BaseModal`, `ConfirmDeleteButton` —
not a native tooltip.) **Per-option `tip`s don't count either**: `ChipSelect`/`CcCycleButton` options
may each carry one, and they're worth having, but they explain the individual choices, not what the
control as a whole is for — so the control still needs its own `v-tooltip`.

> **…but not BOTH.** A chip row with per-option tips *and* its own `v-tooltip` shows two tooltips at
> once, the control's landing on top of the chip's. `duplicateTooltips` flags that (`why: 'per-option'`),
> and `hasPerOptionTips` is what feeds it — so **how the `:options` binding is written decides whether
> the rule can see anything at all.** It resolves a bare identifier, an inline literal, and (since
> 2026-08-17) the root of a call or member expression — `optionsFor(g.heading)`, `byGroup[k]`. Before
> that last case a function-built options list answered a flat `false`, which broke the rule in **both**
> directions at once: coverage called the row unexplained and pushed a `v-tooltip` onto it, then the
> duplicate check stayed silent about the pair that created. That is how `ViewProfileEditor` shipped a
> double tooltip. If your options come from somewhere none of those forms can follow, the answer is
> `null` — "cannot tell" — and you get no help from either half; prefer a followable binding.

> **A tooltip on an ANCESTOR counts.** Most of this app puts it on the row, not the control —
> `<label class="po-row" v-tooltip.left="'X tick angle'"><span>X angle</span><input type="range" /></label>`
> — and the user does get help on hover. Checking the tag alone calls that a violation and
> over-reports by ~90% (155 hits against a true 82), which is enough noise to make the signal
> ignorable. Same failure mode, and same fix, as measuring the rendered string above.

One knock-on: `ParamRenderer` used to bind `v-tooltip="param.tip ?? ''"` (and, for some types, a
generic fallback like `?? 'Select channels to process'`). With every param now guaranteed a `tip`
those are dead, and worse than dead — they render an *empty* tooltip, or a plausible generic one,
where a missing tip should be visibly missing. All ten are gone; the binding is plain `param.tip`.

Presence is the half a machine can decide. Whether a tip is the *right* tip is still a review
question — exactly as the length check can't tell you a short line is a good one.

### A picker shows the QUANTITY, not the column name

A control that offers data columns must label them with what they *are*. The stored name is an
implementation detail of the `.h5ad`, and a user picking an axis to gate on has no reason to know it.

Two mappings exist, both display-only:

| Raw column | Shown as | Where |
|---|---|---|
| `mean_intensity_0`, … | the channel name (`CD3`) | `gating.colLabel` |
| `centroid_x` / `_y` / `_z` | `X position` / `Y position` / `Z position` | `utils/gatingAxes.ts` → `centroidLabel` |
| `centroid_t` | `Time` (`Time (frames)` on a frame axis, `Time (s)` once converted) | `centroidLabel`, `timeAxis.ts` → `frameAxisLabel` |

`centroid_t` is the one that made the rule: it was the option you pick to split a movie into
timepoints, and it read as neither "time" nor anything else a biologist measures. `centroid_x` had the
same problem more quietly — `x` is only obviously spatial if you already know the schema.

**Display-only, and that is the point.** The stored column, CSV exports and the REPL keep the raw
name, so a rename here cannot desynchronise anything downstream — the same split `colLabel` already
made for intensity columns. Compose with the unit rather than baking one in
(`axisLabelWithUnit(colLabel(c), unit)` → `X position (µm)`), so a label never carries two bracketed
clauses.

**A frame axis must keep its unit.** `frameAxisLabel` returns `Time (frames)`, never a bare `Time` —
that is the claim the seconds axis makes, and this is the axis whose interval was unknown
(`docs/ARCHITECTURE.md` → *Calibration*).

---

