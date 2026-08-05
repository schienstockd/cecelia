# Cecelia UI Guide

Frontend conventions, component catalog, and how to add new UI features. Purely Vue/CSS — the
language boundary and WS protocol are in `ARCHITECTURE.md`.

**Start here:** *UX primitive catalog* and *UI copy — keep it short* are the two mandatory lookups
before you render anything. Both are enforced by tests, so skipping them fails the build.

| Looking for | Go to |
|---|---|
| Which component/class to use for a control | **UX primitive catalog — CHECK BEFORE BUILDING** |
| How much text a tooltip / tip / empty state may carry | **UI copy — keep it short** |
| Whether a control needs a tooltip *at all* | **Tooltip coverage — the presence half** |
| Colours, radii, font sizes, fixed dimensions | **Design tokens** |
| Buttons · inputs · toggles · chips | **Button utilities** · **Form controls** |
| Modals · confirms · deletes · popovers | **Modals & dialogs** · **No native browser dialogs** |
| Floating windows, legends | **Floating panels** · **View legend** |
| Building a new module page (route, sidebar, layout) | **Adding a new module page** |
| The image table, task runner, sidebar, viewer | **ImageTable** · **TaskRunner** · **AppSidebar** · **ViewerPanel** |
| Adding a plot to a page or the board | **Adding a plot or visualization panel** · **Generic plot-integration interface** |
| Floating/draggable plot panels, tile & cascade | **Shared canvas shell** |
| Making a plot option survive navigation | **Persisting view state — the three scopes** |
| Keeping a plot fresh after a task | **Data freshness — task-refresh** |
| The chain whiteboard | **Chain whiteboard** |

**Neighbouring docs — this one does not restate them.** `docs/PLOTS.md` (plot-spec schema, chart
types), `docs/ANALYSIS.md` (the Analysis board: tabs, plates, export), `docs/POPULATION.md` (gating
model + the gating plot stack's internals), `docs/MODULES.md` (task JSON + param widgets),
`docs/NAPARI.md` (viewer process + layers), `docs/todo/UX_PRIMITIVES_PLAN.md` (unification status).

---

## UX primitive catalog — CHECK BEFORE BUILDING (mandatory)

Before you render **any** of these controls, use the canonical component/utility below — **do not
hand-roll a variant**. A new copy of a primitive that already has a canonical form is a bug (same rule
as the H5AD/zarr/`run_py` single-helpers; see `CLAUDE.md` → *Before implementing anything*). This is
the one glanceable lookup; each row links to its detailed section. Unification status for the few
primitives still being extracted lives in `docs/todo/UX_PRIMITIVES_PLAN.md`.

| Need | Use | Never |
|------|-----|-------|
| Button | `.cc-btn` + `-primary`/`-ghost`/`-bare`/`-danger`/`-danger-ghost` (`style.css`) | scoped `.btn-sm`/`.btn-primary` in a component |
| Icon-only button | `.cc-btn` + `-bare`\|`-ghost` + `-icon` (+ `-micro`/`-dense`/`-lg`) | a per-file `.icon-btn`/`.opt-btn`/`.gear` class |
| Engaged / pressed toggle button | `.cc-btn-on` (+ `-on-tint` washed, `-on-solid` filled) | a scoped `.on`/`.active` colour rule |
| Joined strip of related buttons | `.cc-btn-group` wrapping ordinary `.cc-btn`s | a hand-rolled `.seg { } .seg button { }` block |
| On/off option (applies on flip) | `components/CcToggle.vue` | a native checkbox styled as a switch |
| Select from a list (multi/single) | native `<input type="checkbox">`, or `ChipSelect` for chips | a column of toggle switches |
| Chips / segmented picker | `components/ChipSelect.vue` | hand-rolled pill/`.seg` rows |
| Colour picker dropdown | `components/SwatchSelect.vue` | a bespoke swatch grid |
| Pick ONE option where numbers decide it | `components/SelectionTable.vue` | a `<select>` that hides the trade-off, or an inline `<table>` |
| Movie output options (fps + res) | `components/MovieOutputControls.vue` (`v-model:fps` / `v-model:scale`) | a per-panel pair of sliders |
| Movie title-card options (on/off + duration + note) | `components/TitleCardControls.vue` (`v-model` a `TitleCardCfg`) | a per-panel toggle + duration slider + note input |
| Modal / dialog | `components/BaseModal.vue` | a hand-rolled `position:fixed` backdrop |
| Popover / dropdown menu | `components/TeleportPopover.vue` | an absolutely-positioned panel |
| Tabs | `components/canvas/TabbedCanvas.vue` | a hand-rolled tab strip |
| Standalone module page (not the image-table layout) | `components/ModulePage.vue` — title + `#controls` slot + content | a per-page `.x-page`/`.x-head` wrapper, or a descriptive subtitle paragraph |
| Collapsible section (chevron + heading) | `components/CollapsibleSection.vue`, or `.cc-section-toggle` for the bare row without the panel-bar chrome | a per-file chevron toggle |
| Confirm / destructive-confirm | `components/ConfirmButton.vue` / `ConfirmDeleteButton.vue` | `window.confirm` or an inline arm flag |
| Range slider (min+max) | `components/RangeSlider.vue` | a hand-rolled dual-thumb range |
| Single-value slider | a plain `<input type="range">` — the global base themes it | a wrapper component (there is deliberately none) |
| Loading state in a plot area | `components/plots/PlotSpinner.vue` (delayed — see *Plot loading state*) | an immediate inline spinner |
| Transient "just did a thing" feedback | `useToast()` — the one `<Toast />` in `App.vue` | a second notification system |
| Copy-to-clipboard (+ the "Copied!" flash) | `composables/useCopyFlash.ts` — `copy(text[, key])` + `isCopied([key])`; `utils/clipboard.ts` for the bare write | `navigator.clipboard.writeText` + a per-file `ref` and `setTimeout` |
| Side panel of two stacked halves, either expandable to the whole panel | `composables/usePaneExpand.ts` + `components/PaneExpandBar.vue` (`utils/paneExpand.ts`) — see *Two-half side panels* | a per-panel mode `ref` + its own pair of toggle buttons |
| Draggable / detached panel | `components/FloatingPanel.vue` | a bespoke `position:fixed` panel |
| Dismissible first-use hint | `components/HintCallout.vue` | a one-off info box |
| QC severity (ok/warn/fail) | `lib/severity.ts` + `--cc-sev-*` tokens | a hand-typed traffic-light colour |
| Task/chain status (5-state) | `lib/taskStatus.ts` (`TASK_STATUS`) | a per-file status→icon/colour map |

**Semantic role utilities** (global classes in `style.css` — *compose* them, add only layout in scoped CSS). These generalise recurring text/surface **scenarios** rather than a component per widget:

| Scenario | Use | Never |
|------|-----|-------|
| Secondary / muted text (hint, subtitle, caption, meta) | `.cc-muted` (+ a `.cc-fs-*` step) | a scoped `color: var(--cc-text-dim); font-size: …` |
| Small dim label beside a control | `.cc-muted` — same scenario, no separate utility | a per-file `.*-lbl`/`.*-label` |
| Empty / "nothing here yet" state | `.cc-empty` (+ `-inline` one-liner / `-overlay` over a plot / `-lg` rich page empty) | a new `.*-empty` class |
| Numeric value readout beside a control | `.cc-readout` (+ `-strong` prominent; + a `.cc-fs-*` step) | a bespoke `.*-val`/`.*-num` |
| Eyebrow / section label (uppercase dim heading) | `.cc-eyebrow` (base is 11px; + a `.cc-fs-*` step) | a scoped uppercase-heading rule |
| Card / panel / surface container | `.cc-card` (+ `-2` when it sits *on* a surface-1 panel) | a scoped `surface + 1px border + radius` block |
| Corner radius | `--cc-radius-xs/sm/md/lg/pill` | a raw `rem`/`px` radius |
| Small text size | the `--cc-fs-*` token in CSS, or the `.cc-fs-*` class in markup | a raw `rem`/`px` font-size (incl. inline `style=`) |
| Compact input / select / textarea | `.cc-input-xs` (11px) / `.cc-input-2xs` (10px) — sets size AND padding. **The base is already 12px**, so most fields need neither | a scoped class re-typing the base's border/colour/background to change the size |
| A colour a token already holds | that token — `var(--cc-accent)`, not `#a78bfa` | a hex literal, **or** a `var(--x, #hex)` fallback (add the token, never a fallback) |

**Pick a scenario, then a size.** `.cc-muted .cc-fs-xs` · `.cc-eyebrow .cc-fs-2xs` ·
`.cc-readout .cc-fs-2xs` · `.cc-empty-inline .cc-fs-3xs`. The size ladder is ONE shared set of classes
(`.cc-fs-lg/-md/-sm/-xs/-2xs/-3xs`, the same steps as the `--cc-fs-*` tokens), not a per-scenario one:
`.cc-muted-2xs`, `.cc-eyebrow-2xs` and `.cc-readout-2xs` were three names for one declaration, and
naming them per scenario only made you guess which to reach for. Modifiers that carry real semantics
DO stay on their scenario — `.cc-readout-strong` is prominence, `.cc-empty-inline` is layout.

**The step is named, not relative** (`-xs`, not `-dense`). A relative name can only express the steps
someone thought of: `.cc-muted` had no 11px step — the single largest cluster of hand-rolled muted text
in the app — because "dense" was already spent on 10px, and nothing named the step *above* the base. Reach for the modifier instead of re-declaring the
scenario locally: baking a value into the base is what stranded ~10 sites as "bespoke" before. Per-site
*emphasis* (`font-style: italic`) and *geometry* (width/margin/flex/padding) still belong in scoped CSS.

**There are no raw sizes or radii left, and the tests keep it that way.** Both scales were derived from
the actual distribution rather than guessed: 33 distinct font-size spellings collapsed onto 6 steps (98%
of them were already within 0.5px of a step), and 15 radius spellings onto 5. `--cc-radius-sm` was retuned
`0.25rem`→`0.3rem` because 4.8px is the modal radius, which halved the worst-case shift. A literal
`font-size`/`border-radius` anywhere — scoped CSS **or** an inline `style=` — now fails
`utils/cssScenarios.test.ts`. Exempt by rule: display type (>15px), pill radii, `0`, and `em` (which is
deliberately container-relative, e.g. `ViewLegend` scaling with the export).

**Icon buttons are a fixed square**, so a toolbar row lines up regardless of glyph width — that's why
`-icon` is a modifier rather than per-site padding (48 sites had each discovered they needed a fixed box,
at nine different sizes). `-bare` is transparent/dim-until-hover, `-ghost` is its boxed counterpart; tone
comes from `-danger-ghost` or a scoped `color` for the one-offs (the napari viewer green). A `<button>`
whose whole content is an icon and which doesn't use `.cc-btn` fails `utils/cssScenarios.test.ts`.

> **Where the checks live:** `frontend/src/utils/cssScenarios.ts` and `cssTokens.ts`, each with a
> `.test.ts` beside it (`pixi run test-frontend`). Full index of what each detector owns and what its
> bar is: [`docs/todo/UX_PRIMITIVES_PLAN.md`](todo/UX_PRIMITIVES_PLAN.md) → *The detectors*. Need a
> count for this area? Read those — never re-derive one by grep.

**Never re-declare a utility in scoped CSS — compose it.** A `<style scoped>` rule whose selector *is*
a global utility (`.cc-muted { color: …; font-size: … }`) outranks the global one on specificity, because
scoping adds `[data-v-…]`. The component then silently stops tracking the utility: change the global and
this one place doesn't follow. It happens by accident during migration — rename the class in the template,
rename the *rule* alongside it instead of deleting it — which is exactly how `LegacyMigrateDialog` ended up
shadowing `.cc-muted` with a byte-identical copy. `utils/cssScenarios.test.ts` now fails on it, with no
allow-list: per-site layout (`.cc-muted { margin-top: 0.3rem }`), descendants (`.panel .cc-muted`) and
modifier compounds (`.cc-btn-bare.viewer-green`) are all legal by construction, so anything it reports is
the bug. Add layout in scoped CSS; never re-state a property the utility itself declares.

**A tier that most sites override is the wrong default.** The form-control base was `--cc-fs-md` (= body)
and read as "the fields are too big" in every dialog — twice reported from the running app. The fix was
not another opt-in class: **33 form controls across 24 files had each hand-written
`font-size: var(--cc-fs-sm)`**, while exactly *one* site had ever adopted the density class. When two
thirds of the population corrects the default by hand, the default is wrong. The base is `--cc-fs-sm`
(12px) now, the density steps re-pitched below it, and those 33 declarations are gone as provable no-ops.
The same rounding caused the tooltip regression (`0.72rem` → nearest step, which happened to be the larger
one → now `--cc-fs-xs`). **When tokenising a value that sits between two steps, check which side the
element belongs on** — dense chrome rounds down, not to the nearest.

**Re-implementing a scenario is a test failure, not a style opinion.** `utils/cssScenarios.test.ts`
detects a scoped rule that spells out a canonical utility's defining declarations — a dim colour plus a
hard-coded size *is* `.cc-muted` — and holds a per-file baseline that **may shrink and must never grow**.
Touch a file, migrate its rules and lower its number; add a new one and the suite names it. Card chrome
is deliberately *not* checked: `surface + border + radius` is the shape of a card, an input, a chip, a
badge and an icon-button alike, and ~60% of matches wanted `.cc-btn`/`ChipSelect` instead, so it stays a
review-time rule.

**Standalone pages use `ModulePage`; the image-table pages use `ModuleLayout`.** 15 of the 23 module
pages are built on `ModuleLayout` and were already consistent. The 8 standalone ones were not: Notebooks,
Animation and Movies had each grown their own frame — three h1 sizes (1.1 / 1.15 / 1.4rem), two paddings,
two subtitle widths, and `.nb-header`/`.anim-head`/`.mov-head` doing the same flex-space-between under
different names. (The h1 sizes escaped the size sweep because `findRawValues` exempts anything over 15px
as display type.) `ModulePage` fixes title, controls and spacing; `layout="flow|scroll|fill"` is the one
real axis — whether the page flows, scrolls itself, or is a full-height pane whose child scrolls. Per-page
extras go on the call site as a class (Vue puts the parent's scope ID on a child's root, so a scoped rule
still applies).

**Do not write a page subtitle.** All three carried a paragraph explaining the feature to a first-time
reader — permanent noise on a screen its owner uses daily, and the clearest tell that a page was
AI-written. `ModulePage` has no subtitle slot on purpose. The title and the controls say what the page is;
the explanation belongs in `docs/`. Same rule as tooltips and QC findings: if you are tempted to explain
in the UI, that text goes in the relevant `docs/<AREA>.md` instead.

**Tokens live on `:root`, and that is load-bearing.** `.cc-dark` is a `<div>` inside `<body>`
(`App.vue`'s shell), so anything a library appends to `document.body` is a *sibling* of it and inherits
nothing declared there. PrimeVue's tooltip does exactly that — so while the scale sat on `.cc-dark`,
every `var(--cc-*)` in the tooltip override was invalid at computed-value time and the tooltip rendered
at the browser default **16px**, with `<body>`'s own `font-size` dead the same way. Declared ≠ reachable,
and the symptoms are identical, which is why the token guard stayed green throughout. If you style
anything that mounts outside the app shell (a portal, teleport, or library overlay), check that the
properties it references resolve *there*. `utils/cssTokens.test.ts` now fails if the global scale is
declared anywhere but `:root`.

**Every custom property you reference must be declared somewhere.** An undeclared one does not warn —
it makes the whole declaration invalid at computed-value time, so `var(--cc-text-muted, #888)` silently
freezes a hard-coded grey that never tracks the theme, and a fallback-less `background: var(--cc-surface)`
drops the *entire* `background` shorthand (a `<select>` lost both its fill and the global custom caret this
way). `utils/cssTokens.test.ts` fails the build on any such reference — add the token, don't add a fallback.
It checks **all** `--*` properties, not just `--cc-*` (a stray `--text-muted` had been hiding behind the
prefix), and counts a component-local declaration — including an inline `:style="{ '--foo': … }"` for a
dynamic value — as valid.

If what you need isn't here and isn't obviously covered by an existing component, grep first
(`INVENTORY.md` → *Frontend*); only build new if the search is genuinely empty, and then add it here +
to `INVENTORY.md` in the same change.

---

## UI copy — keep it short (mandatory)

**Default to no explanatory text.** A page title plus its controls almost always says what the page
is. Where orientation genuinely isn't self-evident, one short phrase — **under ~10 words, never two
sentences**.

Why: a paragraph written to explain a feature once sits permanently on a page its owner uses daily,
so it buys clarity once and costs noise forever. Verbose in-app prose is also the most reliable tell
that a screen was generated rather than designed — it makes the whole app read that way. The real
explanation belongs in `docs/`, which is where it actually gets looked up.

| Surface | Budget |
|---|---|
| Page / panel subtitle | none by default; a short phrase only if the page is genuinely opaque |
| Tooltip (`v-tooltip`) | one line — what the control does, not why it exists |
| Task-JSON `tip` | **required on every param** — one short line (see *Tooltip coverage*). Lead with a recommended value where one exists (`Start ~5 µm; …`): a tip that only names the trade-off leaves "what do I put here?" unanswered |
| Param advisory (`tasks/paramAdvisors.ts`) | one muted line under the control + the reasoning on hover. For when the right value depends on the user's DATA rather than on wording — e.g. the grid a spacing produces and what it costs to store. See `docs/MODULES.md` → *Param advisories* |
| QC finding | short = the problem, long = the action, imperative (`docs/MODULES.md`) |
| Data-patch `description` (`app/src/maintenance.jl`) | title = what it does, description = one line + the one caveat that matters. Capped at 160 chars by `app/test/suite.jl`. Never restate Dry-run/Apply (both are buttons) and never explain HOW it detects — that belongs in the runner |
| Empty state (`.cc-empty`) | one line; a following action, not a rationale. **Exception:** the two *first-run* states (no projects / no images) get title + ≤2 lines + one CTA — bounded in *Onboarding*, which is the rule for them |
| First-use hint (`HintCallout`) | one line, by construction |

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

| Surface | Checker | Ratchet |
|---|---|---|
| SFC controls — `input`, `select`, `textarea`, `CcToggle`, `ChipSelect`, `SwatchSelect`, `RangeSlider`, `CcCycleButton` | `uncoveredControls` (`utils/uiCopy.ts`) | `uiCopy.test.ts` |
| **Icon-only buttons** — a `<button>` whose whole content is an `<i>` glyph | same | same |
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
`title=` in the codebase is a component *prop* — `BaseModal`, `ModulePage`, `ConfirmDeleteButton` —
not a native tooltip.) **Per-option `tip`s don't count either**: `ChipSelect`/`CcCycleButton` options
may each carry one, and they're worth having, but they explain the individual choices, not what the
control as a whole is for — so the control still needs its own `v-tooltip`.

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

---

## Design tokens

All tokens live in `frontend/src/style.css` under `.cc-dark` (always applied at the `<body>` level).

| Token | Value | Use |
|-------|-------|-----|
| `--cc-bg` | `#0f1117` | Page background |
| `--cc-surface-1` | `#161b22` | Sidebar, panels |
| `--cc-surface-2` | `#21262d` | Hover states, inset boxes |
| `--cc-text` | `#e6edf3` | Primary text |
| `--cc-text-dim` | `#7d8590` | Secondary text, labels |
| `--cc-border` | `#30363d` | All borders |
| `--cc-accent` | `#a78bfa` | Active elements, buttons, links |
| `--cc-accent-strong` | `#7c3aed` | Deeper violet — the border of an engaged/active control |
| `--cc-accent-soft` | `#c4b5fd` | Pale violet — text on an accent-tinted surface |
| `--cc-accent-tint` / `-tint-2` | `#2d1b69` / `#3b2382` | The tinted "option is on" surface, and its hover step (`.cc-btn-on-tint`) |
| `--cc-selected` | `#ff8c1a` | Amber selection/active highlight for BOXES (panels, cards, timeline keyframes) — distinct from `--cc-accent` (form controls) |
| `--cc-warn` | `#f59e0b` | Amber that is *not* a severity — a decorative/identity hue (a chain node's colour, a keyframe badge) |
| `--cc-danger` | `#ef4444` | The **destructive-action** tone (a delete button's hover/armed state) — an action, not a status |
| `--cc-viewer` | `#22c55e` | Green accent for the napari viewer controls button + its floating-panel border (stands apart from purple chrome) |
| `--cc-sev-ok` | `#0ca30c` | Severity **ok** (QC/traffic-light). Colour-blind-safe status palette |
| `--cc-sev-warn` | `#fab219` | Severity **warn** — any *status indicator* saying "heads up" (a validation warning, a stale-data strip, an advisory axis flag) |
| `--cc-sev-fail` | `#d03b3b` | Severity **fail** — any *status indicator* saying "this is broken" (an invalid field, an error dot, a failed task) |

| `--cc-mono` | system monospace stack | Log output, code |

**Scales and fixed sizes.** Never write a raw `rem`/`px` for these — `cssScenarios.test.ts` fails the build.

| Token | Value | Use |
|-------|-------|-----|
| `--cc-radius-xs` `-sm` `-md` `-lg` `-pill` | `0.2` `0.3` `0.4` `0.5rem` `999px` | chips/swatches · buttons/inputs · cards/panels/dialogs · large dialogs · pills |
| `--cc-fs-3xs` `-2xs` `-xs` `-sm` `-md` `-lg` | `0.56` `0.62` `0.68` `0.75` `0.82` `0.9rem` | ≈9 · 10 · 11 · 12px · body · 14px |
| `--cc-header-h` · `--cc-sidebar-w` · `--cc-runner-w` | `40px` · `190px` · `280px` | Header · sidebar · TaskRunner panel |
| `--cc-console-bar-h` · `--cc-console-open-h` | `30px` · `210px` | Console collapsed · expanded |

### Toast notifications (transient foreground feedback)

PrimeVue `<Toast />` is mounted once in `App.vue` (registered via `ToastService` in `main.ts`); call
`useToast()` anywhere. **Do not add a second notification system.** Toast is for a foreground action
the user just triggered and is waiting on (a cohort check, a longer save) — NOT for background
scheduler progress (that's the task manager) nor for every lab-log entry (those badge). Severity maps
to the traffic-light scale: `info` (in progress) · `success` (done, all-clear) · `warn` (done, findings)
· `error` (failed). First consumer: `CohortCheckButton.vue`.

The four notification surfaces — pick the one that fits, don't invent a fifth:
- **Toast** — transient, auto-dismiss; a foreground op in progress / just done.
- **Badge** — persistent "needs attention" (unseen lab-log entry, QC warning) until acknowledged.
- **Lab log entry** — durable record, kept across sessions.
- **Traffic light** — per-image summary state in the image table, always current.

### Severity (QC / traffic-light) — colour is never the only cue

`--cc-sev-ok`/`-warn`/`-fail` are the ONE severity palette (colour-blind-safe). Import the mapping from
`frontend/src/lib/severity.ts` (`SEVERITY`, `worstSeverity`, `severityFor`); the lab-log glyph counterpart
is `qc.jl` `severity_symbol` (✅/⚠️/❌, never 🟢🟡🔴). **Never render a severity as colour alone** — always
pair the hue with a shape-distinct icon + label. See `docs/todo/QC_OBSERVER_PLAN.md`.

**Which amber/red? The split is status vs not.** If the colour states *the condition of something*
(valid/invalid, fresh/stale, ok/warn/fail) it is a severity → `--cc-sev-*`; opting out silently costs
colour-blind separation. If it is a destructive **action**'s tone (a delete button) or a decorative
identity hue (a chain node), it is not a severity → `--cc-warn`/`--cc-danger`.

---

## Hard requirements

**Tooltips: every control a user *sets*, and every icon-only button, carries a `v-tooltip`.** Place
it where it reads best (`.left` / `.top` / `.bottom` / `.right`) — there is no default side. A button
with a visible caption does **not** need one. CellProfiler is the reference for *density*, one line
each. The exact scope, what counts as coverage, and the ratchet are in *Tooltip coverage* above —
**that section is the rule; this is the pointer.** Don't restate it here.

All errors go to `useLogStore().error(msg, { source, detail })`.
Task failures must never be silent — errors must reach the console bar visible to the user.

The **console** is one component — `components/ErrorConsole.vue` over the `log` store — mounted in two
places: the docked bar at the bottom of the app shell, and (with the `fill` prop) full-window in the
standalone **console window**. Do not build a second console. The window is a `bare` route
(`/console`, `meta.bare` → `App.vue` renders it without the shell) opened via
`window.open(origin + pathname + '#/console', …)` from the docked console bar's pop-out (↗) button;
being a separate browser window it's a fresh app instance with its own WS, and it backfills recent lines from
`GET /api/logs/recent` on open. The stream includes the backend's own logs (WS `server:log`, see
`docs/API.md`), so it's a real "pixi console", not just task logs.

## Settings → System (service control panel)

`SettingsModule.vue` has a **System** section: one row per runtime component (Application / Napari /
Notebooks) with a status pill (Running / Starting… / Stopped, polled every ~4 s from the existing
`/api/{napari,notebooks}/status` endpoints — ephemeral UI state, a plain `ref`, NOT persisted) and
start/stop/restart buttons that reuse the existing control endpoints, plus a
global **Quit** (`POST /api/app/shutdown`, behind a two-click `ConfirmButton` — see *No native browser
dialogs* below). (The pop-out **console window** is
launched from the docked console bar, not this panel.) Status→verb/label mapping is the pure,
unit-tested `utils/serviceStatus.ts`. Backend self-restart is planned (see
`docs/todo/SERVICE_PANEL_PLAN.md`, Phase 3).

---

## Button utilities

Global classes in `style.css`. `.cc-btn` is always the base, plus modifiers on **four independent
axes** — compose them; never re-declare a button in a component's scoped `<style>`.

| Axis | Modifiers |
|---|---|
| Tone | `-primary` · `-ghost` · `-bare` (transparent, dim-until-hover) · `-danger` · `-danger-ghost` |
| Density | `-micro` · `-dense` · `-lg` |
| Shape | `-icon` (fixed square, so a toolbar row aligns regardless of glyph width) |
| State | `-on` (+ `-on-tint` washed / `-on-solid` filled) for an engaged toggle button |

`.cc-btn-group` joins a strip of them. All support `:disabled` (opacity 0.35) and `v-tooltip`.

```html
<button class="cc-btn cc-btn-ghost" @click="…">Apply</button>
<button class="cc-btn cc-btn-primary" @click="…"><i class="pi pi-plus" /> Add images</button>
<button class="cc-btn cc-btn-danger" @click="…">Delete project</button>          <!-- standalone -->
<button class="cc-btn cc-btn-danger-ghost" @click="…">Delete</button>            <!-- inline in a bar -->
<button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip="'Settings'"><i class="pi pi-cog" /></button>
```

**Never hand-roll `.btn-sm` / `.btn-primary` / `.btn-danger` in scoped CSS.** A class that *looks*
shared but is re-declared per file drifts (the danger colour was `#b91c1c` in one place and `#7f1d1d44`
in three; disabled opacity varied 0.35/0.4/0.55), and a page that used the class without its own copy
rendered a raw browser button. `cssScenarios.test.ts` now fails on an icon-only `<button>` that skips
`.cc-btn`.

## Form controls

`style.css` styles **all native form controls app-wide** — bare `<select>`, `<input type="text|
number|search|…">`, `<textarea>` get the consistent surface/border/rounded look, accent
focus ring, a custom `<select>` chevron (the native arrow is hidden via `appearance:none`), and
accent-tinted `range`/`checkbox`/`radio`. **Do not re-declare background/border/border-radius/
padding/outline on inputs in component styles** — it diverges from the rest of the app (this was
the "old-school inputs look inconsistent" bug). Keep only layout in scoped styles (width, flex,
`min-width`) plus state modifiers (`.input-error`, `[readonly]`, `:disabled`, `.mono`). If a
`<select>` sets `background:` (shorthand) it will wipe the chevron — use `background-color`.

### On/off toggles — `CcToggle`

`components/CcToggle.vue` is the ONE boolean on/off switch. Use it for an **immediate boolean
OPTION** — a setting that applies the moment you flip it (autoplay, loop, show-legend, title card,
dark theme, "pool to groups", …). It's a styled `<input type="checkbox">` under the hood, so it
works with `v-model` (or `:model-value` + `@update:model-value` when you also run a side-effect
handler), keyboard focus, and `disabled`. Label via the `label` prop or the default slot (slot wins);
add a tooltip at the call site with `v-tooltip`. It renders its own `<label>`, so **don't nest it
inside another `<label>`** — for a label-left / control-right settings row, make the row a `<div>` and
drop in a label-less `<CcToggle>` (see `PlotOptions`/`SummaryPanel` `.po-row`/`.sp-pop-row`).

**When NOT to use it — keep a native `<input type="checkbox">`:** a multi-SELECT list or a value
staged as part of a form (image / channel / feature / measure pickers, "select all", per-row
selection). A column of sliding switches reads worse and misuses the on/off affordance. So the rule
is: **toggle = one immediate option; checkbox = selection from a list.** Don't hand-roll another.

### Selection chips / segmented controls — `ChipSelect`

`components/ChipSelect.vue` is the ONE canonical inline selector — use it for any pill/capsule or
segmented button-row that picks from a set. Two variants: `variant="pill"` (wrapping
capsules, the default) and `variant="segmented"` (a joined control). `multiple` for multi-select
(`modelValue` is an ordered `string[]`; single-select is a `string`); add `reorderable` (pill only)
for drag-to-reorder. Per-option `icon` / `tip` / `disabled` / `badge` (a count) / `accent` (a
semantic colour — rendered as a readable tint). Pure logic in `utils/chipSelect.ts` (tested).

Active colour is `--cc-accent`. **Don't** use it for: independent-boolean toolbars that also fire
actions or open dropdowns (e.g. `ModuleLayout`'s filter-toggle bar, gate arrange/nav clusters),
colour-swatch grids (`PopulationManager` palette, `SwatchSelect`), the cluster-assignment matrix
(cross-population-exclusive + integral solid colour), or reorderable tab strips (`TabbedCanvas`).

---

## Modals & dialogs — always use `BaseModal`

**Every centred modal/dialog is built on `frontend/src/components/BaseModal.vue`. Never hand-roll an
overlay (`position:fixed; inset:0`)** — that copy-paste produced four near-identical shells before this
existed. We do **not** use PrimeVue Dialog.

`BaseModal` provides the dimmed overlay, the centred surface box, the header (icon + title + ✕), and
close-on-✕ / click-outside / **Escape**. You provide the content via slots.

- **Props:** `title` (string), `icon` (a PrimeIcons class, e.g. `pi-box`), `width` (CSS, default
  `480px`), `height` (optional fixed CSS height; omit to size to content, capped at `90vh`).
- **Slots:** default = the scrolling **body**; `#footer` = pinned action row; `#toolbar` = a pinned row
  under the header (search bars, tabs, breadcrumbs); `#title` = override the whole title area (e.g. to
  add an info-dot tooltip). The body scrolls; header/toolbar/footer stay pinned.
- **Emits:** `close` — the host owns visibility (`v-if` + `@close`).

Minimal dialog — copy this:

```vue
<script setup lang="ts">
import BaseModal from './BaseModal.vue'
const emit = defineEmits<{ (e: 'close'): void }>()
</script>

<template>
  <BaseModal title="My dialog" icon="pi-cog" width="520px" @close="emit('close')">
    <div style="padding: 1rem">…body…</div>            <!-- scrolls -->
    <template #footer>
      <span style="flex:1" />                            <!-- push buttons right -->
      <button class="cc-btn cc-btn-ghost cc-btn-dense" @click="emit('close')">Cancel</button>
      <button class="cc-btn cc-btn-primary cc-btn-dense" @click="…">Save</button>
    </template>
  </BaseModal>
</template>
```

Host it with `v-if`: `<MyDialog v-if="show" @close="show = false" />`. Put dialog-specific CSS in the
child's scoped `<style>`; the shell (overlay/box/header/footer) is BaseModal's — don't restyle it.
Working examples: `PackagesDialog.vue` (toolbar + body), `PhysicalSizeDialog.vue` (`#title` slot +
footer), `FileBrowser.vue` (toolbar + footer), `ClaudeOverviewDialog.vue` (a static how-to — content
from `lib/claudeOverview.ts`, opened by the `?` button in the lab-log toolbar), `ImageMetadataDialog.vue`
(read-only "everything we know about this image" — original source file path + dimensions/calibration/
channels/files/attrs, opened by the info icon on every `ImageTable` row). *(An in-canvas overlay
like `GateOverlay` is a different thing — that's `position:absolute` inside a plot, not a modal.)*

### No native browser dialogs — use `ConfirmButton` for confirms

**Never use `window.confirm` / `alert` / `prompt`.** Native dialogs look out of place (OS-styled, not
our theme), block the JS thread, and can't be positioned or styled. For a destructive-action confirm,
use **`frontend/src/components/ConfirmButton.vue`** — a logic-only wrapper with a **scoped slot**: the
first click arms it, showing **Confirm + Cancel** in place; `@confirm` fires only on the second click
(auto-disarms on an outside click / timeout).

**The host renders the buttons** (via the slot props `{ armed, arm, confirm, cancel }`), NOT the
component — this is deliberate: a child component's rendered DOM does **not** receive a parent's
*scoped* CSS, so if `ConfirmButton` rendered the button, host `.footer-btn` / `.btn-danger` styling
wouldn't reach it (this bit us once — the Quit button rendered unstyled). Rendering the buttons in the
host keeps them in the host's style scope. The wrapper is `display:contents`, so the buttons lay out as
if direct children of the host.

```vue
<ConfirmButton @confirm="doDelete" v-slot="{ armed, arm, confirm, cancel }">
  <button v-if="!armed" class="cc-btn cc-btn-danger cc-btn-dense" :disabled="!selected" @click="arm"
          v-tooltip.bottom="'Delete…'"><i class="pi pi-trash" /></button>
  <template v-else>
    <button class="cc-btn cc-btn-danger cc-btn-dense" @click="confirm">Confirm</button>
    <button class="cc-btn cc-btn-ghost cc-btn-dense" @click="cancel">Cancel</button>
  </template>
</ConfirmButton>
```

`needsConfirm=false` makes `arm` fire immediately with no arm step (e.g. closing an already-empty
board). Used by the sidebar/Settings **Quit** and the board close in `TabbedCanvas`. For a bigger
modal decision (not a single button), use `BaseModal`.

#### Delete affordance — `ConfirmDeleteButton` (the app-wide standard)

For a **destructive icon delete** (label set, population, attribute, notebook, chain, node, …) use
**`frontend/src/components/ConfirmDeleteButton.vue`** — the ONE delete affordance. It's a single icon
button that arms on the first click (**trash → warning triangle, solid danger fill**) and fires
`@confirm` on the second (the ViewerPanel labels pattern D picked as the standard). It **wraps**
`ConfirmButton` for the arm/confirm/dismiss logic and renders its own self-contained chrome (`.cc-del`)
— self-styled *because* it must look identical everywhere (and hosts' scoped `.opt-btn`/`.pm-icon`/
`.wb-btn` classes can't reach a button rendered inside it anyway). Don't hand-roll a per-site
icon-flip or a Confirm+Cancel pair for deletes; that inconsistency is exactly what this replaced.

```vue
<ConfirmDeleteButton title="Delete population"
                     armed-title="Click again to delete this population"
                     @confirm="deletePop(path)" />
```

Props: `title` / `armedTitle` (tooltips), `disabled`, `needsConfirm`, `autoDismissMs`; default slot →
a text label beside the icon (e.g. "Delete set"). Tooltip position is PrimeVue's default + its
out-of-bounds flip (a `tip` prop can't drive position — dynamic directive modifiers aren't possible).
For a host with a **hover-reveal** row action, target the inner button with `:deep(.cc-del)` (see
`ViewerPanel`). The louder **named** text confirms for whole-image / whole-set deletion (`ImageTable`,
`SetBar`: "Delete NAME? [Confirm] [Cancel]") are a deliberate higher tier and stay as-is.

### Coord-fixed plots — 1:1 square

Plots whose axes must stay isotropic (the cluster **UMAP**, the **gating** scatter) render as a
**1:1 square**, so the embedding/flow cloud never warps and HTML overlays (centroid labels, facet
titles, gate labels) line up with the canvas dots.

- **`components/plots/SquarePlot.vue`** — the shared square *primitive*: a container-query box sized to
  `min(100cqw, 100cqh)`, centred. Use it to square a plot whose canvas fills the box with no internal
  padding (UMAP wraps its plot in it).
- **Gating** can't use `SquarePlot`: (a) its axis labels live in the capture box's asymmetric padding
  and the PNG export reads `.panel-plot`'s `offsetLeft/Top` (zoom-immune), so a positioned wrapper /
  squaring the *outer* box would break the export or leave the *dots* rectangular; (b) `SquarePlot`'s
  container-query needs a **definite parent height**, which the montage tiles (content-driven height)
  don't have. So `GateScatterCell` squares **`.panel-plot`** with **`aspect-ratio: 1`** — ONE method
  across both gating contexts (the gate module page *and* the montage tiles).
- **`CanvasPanel :square="true"`** — the shared **resize-box** logic: snaps a *free-floating* panel's
  height to its width on resize so the square plot fills it with no blank space. Used identically by the
  gate plot + pairs panels (pass it directly) and the UMAP (opts in via `interactiveViews.ts` →
  `square: true`, forwarded by `InteractivePanel`). No-op when docked (the board grid owns slot size) or
  collapsed. This is the "same 1:1 resize box for gating and UMAP".

---

## Floating panels — `FloatingPanel`

**A floating, draggable, resizable, collapsible box that floats above the app content
(`position: fixed`).** Use it for tool controls that should be reachable on any page rather than
pinned into the sidebar. `frontend/src/components/FloatingPanel.vue` is generic (not viewer-specific);
the **napari Viewer controls** are its first consumer — mounted in `App.vue`, toggled by the sidebar's
"Viewer controls" button (`settings.viewerPanelOpen`, persisted):

```vue
<FloatingPanel v-if="settings.viewerPanelOpen" title="Viewer" icon="pi-eye" storage-key="viewer"
               @close="settings.viewerPanelOpen = false">
  <ViewerPanel />
</FloatingPanel>
```

- **Parent owns visibility** (`v-if` + `@close`); the panel owns position/size/collapsed, persisted per
  `storageKey` under `cc.floating.<storageKey>` (reopens where you left it). Drag by the header, resize
  from the bottom-right grip, collapse to header-only. Position is clamped into the viewport on mount +
  window resize so a stale/off-screen box always comes back.
- **Stacking** — panels start at z-index 60 (above content and the right panel, below
  modals/console) and are ordered **most-recently-touched on top**: opening a panel or pressing
  anywhere inside it raises it above its siblings, so two open panels no longer stack by DOM
  declaration order. The ordering lives in [`utils/panelStack.ts`](../frontend/src/utils/panelStack.ts)
  (`PANEL_Z_BASE` + one step per open panel, always well below the modal layer); `FloatingPanel`
  binds the result inline, so don't reintroduce a flat `z-index` in its stylesheet.
- Rationale: the viewer controls grew (populations, tracks, colour-by + legend) and crowded the left
  nav; a floating panel frees the nav and lets you place the controls beside the napari window.

## View legend — `ViewLegend` + `utils/viewLegend`

The shared **legend backbone** for describing what a napari view shows as colour swatches — image
**channels** (by colormap), **populations**, and a categorical **colour-by**. One model, many consumers
(the analysis-board image strip, the animation page, later movie overlays), so a colour reads the same
everywhere.

- **`utils/viewLegend.ts`** (pure, unit-tested) — `LegendItem`/`LegendSection` types; `channelLegend(layers)`
  (visible single-hue channel layers → swatches, via `napariColormap.ts`); `viewLegendSections({channels,
  populations, colourBy})` (drops empty groups, stable channel→pop→colour-by order).
- **`components/ViewLegend.vue`** — presentational: renders `LegendSection[]` as grouped swatches.
  Style-light (text inherits `color`, sizes with parent `font-size`), so each host styles it via its
  container (e.g. the image-strip overlay makes it white-on-dark). Section headings show only when there
  is more than one section.

The viewer panel's **colour-by** legend is deliberately NOT this component — it's an *editor*
(recolourable swatches), not a static legend.

## Pinia array reactivity

Use `splice()` to mutate arrays in place inside setup stores.
**Do not** replace the ref (`store.items = store.items.filter(...)`) — Vue loses reactivity.

---

## Adding a new module page

A module page is the full screen that opens when the user clicks a sidebar item.
The standard layout is: **SetBar** (top bar) + **image panel** (left, scrollable image list) + **right panel** (task runner, metadata editor, or a custom panel).

**Convention — attributes + filtering.** Every module page **except Import** must show
attribute columns and allow filtering by them: pass `:show-attrs="true" :show-filter="true"`
to `ModuleLayout`. Import is the only exception (images are imported there, before attrs
exist) — it uses `:show-filter="false"`. Metadata shows attrs but omits the filter (it's
where attrs are edited). Pages that operate on a single image (e.g. gating) add
`:single-select="true"` — this is independent of attrs/filter and composes with them.

### 1 — Create the Vue file

Use `ModuleLayout` (see below). The minimal template is:

```vue
<!-- frontend/src/modules/SegmentModule.vue -->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs, reload } = useTaskDefs('segment')   // category = the task JSON's fun_name prefix
</script>

<template>
  <ModuleLayout module="segment" :show-attrs="true" :show-filter="true">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner :defs="defs" :on-reload-defs="reload" module="segment"
        :selected-uids="selectedUids" :selected-names="selectedNames" />
    </template>
  </ModuleLayout>
</template>
```

**Props to consider:**

| Prop | Default | When to change |
|------|---------|----------------|
| `module` | — | Always set; passed to ImageTable for per-module column config |
| `allow-manage` | `false` | `true` for Import (New/Rename/Delete set controls visible) |
| `show-attrs` | `false` | `true` for modules where attr columns (treatment, genotype…) are useful |
| `show-filter` | `true` | `false` for Import and other modules where filtering doesn't apply |
| `no-set-hint` | `"Select a set…"` | Custom empty-state message |

**Slots:**

- `#actions="{ hasSet, setUid, selectedUids, selectUids }"` — items injected into the action bar before the image count (e.g. "Add images" button). `hasSet` is `true` when a set is active — use it to disable the button. The selection is passed in so a bar item can **act on it** (Import's Copy / Move / Delete, see *File operations*); `selectUids([])` clears it afterwards.
- `#right="{ setUid, selectedUids, selectedNames }"` — the right-hand panel. All three slot props are computed inside `ModuleLayout`; the module page does not need its own refs for them.
- `#plots="{ setUid, selectedUids, selectedNames, selectUids, orderedUids }"` — **the module's plot canvas.** `ModuleLayout` wraps it in ONE consistent, collapse-persisted `CollapsibleSection` (label via the `plotsLabel` prop, default `'Plots'`). **Do not wrap it yourself** — this is what makes every module page's plot canvas collapse the same way. This is the canonical place for the summary/gating/cluster canvas. `selectUids(uids)` drives the table selection from the canvas; `orderedUids` is the visible image list in table order (filtered/hide-excluded applied) — used by the gating prev/next (`«`/`»`) buttons to step selection through the list.
- `#below-table="{ setUid, selectedUids, selectedNames, selectUids }"` — extra *custom* content below the plots (rare). Wrap each piece in `<CollapsibleSection>` yourself; multiple sections supported.

If you need the active set in the module page itself (e.g. Import's file-browser guard), import `useProjectStore` and call `project.activeSet()` directly.

### Adding the plot canvas below the image table

Put the canvas in the `#plots` slot — nothing else. `ModuleLayout` gives it a consistent, collapsible, **collapse-persisted** section (per module, under `cc-plots-open:<module>`); pass `plots-label` to rename the header:

```vue
<ModuleLayout module="behaviourAnalysis" :show-attrs="true" plots-label="Plots">
  <template #plots="{ selectedUids }">
    <SummaryCanvas :image-uids="selectedUids" module="behaviourAnalysis" />
  </template>
</ModuleLayout>
```

Every module page uses this same slot, so the plot canvas collapses identically everywhere — don't hand-wrap a `CollapsibleSection` in the module (that's exactly the divergence this replaced: SegmentModule once rendered its canvas un-collapsible). The image table itself is in a `CollapsibleSection` ("Images") managed by `ModuleLayout`; all sections scroll together and the panel collapses horizontally with the ‹/› button.

`CollapsibleSection` props:
- `label` — section heading (uppercased in the toggle bar)
- `defaultOpen` — whether open on mount (default: `true`)
- `maxHeight` — CSS `max-height` for the body (default: `'320px'`; pass `'none'` to allow full growth). With `'none'` the body is `overflow-y: visible` (not a scroll container) so a `position: sticky` descendant sticks to the outer page scroll instead of a box that never scrolls.
- `storageKey` — when set, the open/closed state persists in localStorage under this key (the `#plots` wrapper uses this so a collapsed canvas stays collapsed across navigation)

### Two-half side panels — `usePaneExpand` + `PaneExpandBar`

A recurring **scenario**, not a widget: a right-hand panel made of two stacked halves where, on a laptop
screen, neither gets enough vertical room. Both panels that have this shape use the same primitive:

| Panel | Top half | Bottom half | Storage key |
|---|---|---|---|
| `tasks/TaskRunner.vue` (every module page) | function + params + run + pool | the module's task list | `cc-taskrunner-pane` |
| `modules/batchmovies/BatchMoviesPanel.vue` | the movie config | the batch's task list | `cc-batchmovies-pane` |

Three modes — `split` (default), `top`, `bottom` — persisted per panel. Each half's button also
*un*-expands it, so whichever half is hidden its own button brings it back and there is no state the user
can't click out of. The recipe is the mode on the root plus one CSS rule per half:

```ts
const { pane, toggle } = usePaneExpand('cc-mypanel-pane')
```
```vue
<div class="mypanel" :class="'pane-' + pane">
  <PaneExpandBar :pane="pane" top-label="movie config" bottom-label="task list"
                 top-icon="pi-cog" bottom-icon="pi-bars" @toggle="toggle" />
  …
```
```css
.mypanel.pane-bottom > .my-config { display: none; }   /* bottom expanded → hide the top half */
.mypanel.pane-top    > .my-tasks  { display: none; }
```

- **A rule per half, not a guard per element.** A half is usually several sibling elements, so
  `v-show` on each means a section added later is silently left visible — the rule matches by class and
  covers it. Give a single-component half a plain wrapper div (`BatchMoviesPanel`'s `.bm-tasks`) rather
  than reaching into the child's root from scoped CSS.
- **Never `v-if`.** Unmounting a config half discards whatever its children have fetched (population
  lists, model lists) and refetches on the way back. `display: none` keeps them alive.
- **The bar owns the tooltip wording**, so every panel phrases the action identically; the consumer only
  names its halves (lower-case, short — they go straight into "Expand the …").
- **Growth is the consumer's** — the primitive decides what's visible, not how the survivor uses the
  space. `TaskRunner` lifts its `params-section` `max-height` cap under `.pane-top`; `BatchMoviesPanel`
  needs nothing, because `ModuleLayout`'s `.right-slot` already scrolls.
- Anything in **neither** half stays visible in every mode — `BatchMoviesPanel`'s "napari is busy" banner
  is deliberately outside both, since it matters most while you are watching the task list.
- **The bar's default slot is a readout for the hidden half.** Expanding a half means losing sight of the
  other one, so put back the one thing you'd miss rather than nothing: `TaskRunner` shows
  `3 running · 1 queued` there while the task list is collapsed (and nothing when it's visible, which
  would just restate the list). One line, `.cc-readout`, no new row — the bar is already paid for.

**Popovers — use `TeleportPopover`, don't hand-roll an absolute one.** Any ⚙/dropdown popover that
lives inside a panel (canvas, table, plot) WILL be clipped by the panel's `overflow`/scroll/transform.
`TeleportPopover` (`components/TeleportPopover.vue`) teleports to `<body>` so it escapes all of that,
positions `fixed` from an anchor element, re-anchors on scroll/resize, carries the `.cc-dark` theme
tokens, and dismisses on outside-click/Escape. Usage: `<TeleportPopover v-model="open" :anchor="btnEl"
placement="bottom-end">…</TeleportPopover>` where `btnEl` is a template ref on the trigger. The
popover owns only the shell (surface/border/shadow/position); the slot supplies the content + its own
inner styling. It clamps to the viewport and flips above when there's no room below. Reuse this rather
than another absolute/fixed popover that will clip or need its own dismiss/positioning logic — it is
the single implementation, used by the image-strip settings, the image-table run-log cog, the board
grid-size + custom-plate popovers, the summary-plot options, the gating-strategy options, and the
gate-pairs channel picker.

### 2 — Register the route

`frontend/src/main.ts` — **lazy-load the component** (see *Route-level code splitting* below); do **not**
add a static `import` at the top:
```ts
{ path: '/segment', component: () => import('./modules/SegmentModule.vue'), meta: { label: 'Segment' } },
```

#### Route-level code splitting

Every module page uses `component: () => import('./modules/…')` so each becomes its own chunk fetched on
navigation, not part of the initial `index` bundle. This matters: eagerly importing all pages once put
the whole app (Chain whiteboard + `@vue-flow`, the plot stack, every modal) into a single ~1.2 MB chunk
at boot; lazy routes cut the **initial** JS to ~240 KB (~54 KB gzip, an ~87% drop). A new page **must**
follow the lazy form — a static top-of-file `import X from './modules/X.vue'` silently pulls that page
(and its deps) back into the boot bundle.

Same rule for a **heavy library used on one screen**: dynamic-import it at the call site rather than at
module top, so it splits into its own on-demand chunk. Precedents: `@observablehq/plot`
(`await import('@observablehq/plot')` in `PlotChart`/cluster panels) and `pdf-lib`
(`await import('pdf-lib')` inside `plots/pdf.ts`'s export function — loads only when the user exports).

### 3 — Add the sidebar entry

`frontend/src/components/AppSidebar.vue`, inside the relevant `groups` array:
```ts
{
  to:               '/segment',
  label:            'Segment',
  icon:             'pi-th-large',
  tip:              'Run cell segmentation.',
  requiresProject:  true,
  // disabled: true, soon: true,   ← add while not yet implemented
}
```

`requiresProject: true` greys the item and shows a lock badge when no project is open.

### 4 — Add the task category (backend)

See `CLAUDE.md` (Adding a new Python task) for the Julia + Python side.
The frontend never maintains a copy of task definitions — they're fetched from `/api/tasks/definitions?category=segment`.

> **Tracking page** (`frontend/src/modules/TrackingModule.vue`, route `/track`) is a plain
> `ModuleLayout` + `TaskRunner` page in the Analysis group **after Gate**. It uses the
> `popSelection` param widget (added to `ParamRenderer.vue`): a dropdown listing
> `NONE (whole segmentation)` plus the flow population paths for the selected image +
> chosen segmentation (fetched from `/api/gating/popmap`). The widget reads its sibling
> `valueName` value and the selected image via the extended `ParamContext`
> (`{ images, projectUid, values }` from `TaskRunner`). It emits a population path string;
> the Julia handler resolves membership. See `docs/MODULES.md` (Param types) and
> `docs/TRACKING.md`.

---

## Onboarding — setup wizard, first-use hints, empty states

New-user UX (see `docs/todo/ONBOARDING_PLAN.md`):

- **First-launch setup wizard** — `frontend/src/modules/SetupModule.vue`, a `bare` route `/setup`
  (full-window, no shell). The boot guard in `main.ts` (`router.beforeEach`) asks the backend once via
  `appControl.refreshStartup()` (reads `/api/diagnostics` `setupRequired`); while setup is required
  every route redirects to `/setup`, and once done `/setup` bounces back to `/import`. The wizard
  picks a projects dir (`GET /api/setup/defaults`, live `GET /api/setup/validate`, `POST
  /api/setup/init`); the backend writes `custom.toml` (`Cecelia.set_projects_dir!`).
- **First-use hints** — `frontend/src/components/HintCallout.vue`: a one-line, dismiss-permanently
  callout keyed by id in `localStorage` (`cc.hint.<id>`). Module pages declare one via `ModuleLayout`'s
  `hint` + `hint-key` props (don't hand-roll it per page); the global "use the bottom-left Quit button,
  not the browser tab" hint is in `App.vue`.
- **Empty states** — exactly two, and they already exist: `ProjectPanel.vue` (`.pp-empty.cc-empty`,
  no projects) and `ImageTable.vue` (`.cc-empty.cc-empty-lg`, no images). Extend the copy there;
  don't add a parallel component. These are the ONE carve-out from *UI copy — keep it short*: that
  budget exists because prose on a page you use daily is noise forever, which doesn't apply to a
  state a user sees once, before they know the app reads CZI. Everywhere else the budget holds.

  **The carve-out has a shape, not a blank cheque** — both existing states already fit it, so match
  them rather than inventing a third form:

  > a **title** (`No images yet`) · **at most two short lines** of orientation · **one CTA button**.

  Anything past that is the thing the budget exists to stop. The carve-out is for the *first-run*
  states above only: a "no populations yet" or "no results yet" empty state is an ordinary one and
  gets the one-line budget.
- **Shutdown** — reuse the existing sidebar-footer Quit (bottom-left) / Settings control
  (`appControl.quit()`); do **not** add another. Onboarding only *points at* it via the hint.

## Explainer sketches + tips (What's New modal)

The tip-of-the-day / release-notes cards (`components/WhatNewCard.vue`, content in `lib/tips.ts` +
`lib/whatsNew.ts`) render an animated sketch through `<SketchCanvas>` from **feijoa** — a sibling
sketchbook repo (`github:schienstockd/feijoa`), consumed as a git dependency. A card points at one by id
(`sketchAnimation: { id: 'claude_mcp' }`); an id the catalogue doesn't have falls through to a grey
"Animation coming soon" placeholder.

**Adding a sketch is a TWO-repo change, and skipping the second half fails invisibly:**

1. Author it in feijoa (`~/cc-workspace/feijoa/src/sketches/<name>.ts`), register it in
   `src/sketches/index.ts` (map + `sketchList` + named export), `npm run typecheck`, **push `main`**.
2. In cecelia: `npm update feijoa` in `frontend/`, and **commit the changed `package-lock.json`**.

Step 2 is the one that gets forgotten. `frontend/package.json` declares the branch
(`github:schienstockd/feijoa#main`) but the **lock pins a commit sha**, and that's what installs — Linux
CI and `release.yml` run `npm ci` (which also trips on lockfile drift), the installers run `npm install`
and keep the locked sha. Meanwhile **dev resolves feijoa through the sibling-checkout Vite alias in
`vite.config.ts`**, so a new sketch renders perfectly on your machine while every release build shows the
grey placeholder. Verify with `ls frontend/node_modules/feijoa/src/sketches/` after the update, not by
looking at the dev server.

Tip copy follows *UI copy — keep it short*: a one-paragraph description plus 2-4 imperative steps. The
sketch carries the explanation; the card is not the place for prose.

Rationale + the sketch-act format: `docs/todo/SKETCH_ENGINE_PLAN.md`.

## ModuleLayout component

`frontend/src/components/ModuleLayout.vue`

Owns the full two-column layout, SetBar, image selection state, attr filtering, and the `filteredUids` / `selectedUids` / `selectedNames` derived state. Module pages receive these as slot props — they do not need their own refs.

**Selection is remembered across navigation.** The run-table checkbox selection is persisted in the project store (`getImageSelection`/`setImageSelection`, keyed by `${module}|${setUid}`), so leaving a module page and coming back restores it. `ImageTable` is the writer (seeds from the store on mount / set switch, commits on every toggle); `ModuleLayout` reads it to initialise `selectedUids` and to restore on set switch. Keying by module name keeps each page's selection its own (e.g. gating's single-select doesn't bleed into segment). It's in-memory/session-scoped and cleared on project load/close. This is generic — every module page gets it for free via `ModuleLayout`.

The filter panel renders automatically when `show-filter="true"` and the active set has either images with `attr` values or images with a run history. It disappears when there is neither, so it is safe to leave enabled even for modules that may or may not have attrs.

**Two filter families in the one dropdown:**
- **Attributes** — chips per attr key/value (Apply/Reset/Invert).
- **Processed with** — a function picker + an *ever* / *last run* mode, to narrow the list to the images a given function has been run on. This answers "which images have I already denoised/segmented?" It is **derived** from each image's automatic run log (`CciaImage.runLog`) via the pure helpers in `frontend/src/utils/runLog.ts` (`wasProcessedWith`, `funsRunAcross`) — there is deliberately **no** separate persisted status attribute to keep in sync; the run log is the single source of truth. Only functions that have actually been run across the set are offered, and both modes ignore **failed** runs (a failed run left no output). The same run log backs the **run tag** shown beside each image's UID in `ImageTable` — a task-manager-style module pill for the image's last **successful** run (`lastSuccessfulRun` + `taskDefs.labelFor`, coloured from the shared `frontend/src/utils/taskModule.ts` palette that also colours the task manager) — and the per-row run-history cog popover. All three filter/hide states persist per module in `localStorage`.

**Collapsible chrome (free up working space).** Two persisted toggles, both in the `settings` store (`localStorage`):
- **Left nav** — the `pi-bars` button in `AppHeader` toggles `settings.sidebarCollapsed`; `AppSidebar` `v-show`s its `<nav>` off, so the main canvas reclaims the full width. (The `v-show` lives on the `<nav>`, not on the `<AppSidebar>` element — the component has two root nodes, so a component-level `v-show` has no single root to bind and silently no-ops.)
- **Right panel** — `ModuleLayout` wraps the `#right` slot (TaskRunner / MetadataPanel / custom) with a thin always-visible left-edge handle (`pi-angle-double-*`) that toggles `settings.rightPanelCollapsed`. Collapsed → only the handle remains; the function/tasks panel folds away to the right. Every module page gets this for free.

Both default expanded and persist across sessions/navigation.

**Left-panel collapse — two axes.** *Horizontal* (‹/›) shrinks the whole left panel to a 2.4rem strip.
*Vertical* collapses each section inside it: the image table ("Images") and the module's `#plots` slot,
each a `CollapsibleSection`. Both the plots wrapper (`cc-plots-open:<module>`) and any section given a
`storageKey` persist their open/closed state in localStorage; a section without one is transient. The
panel body scrolls when the sections together exceed the height.

---

## ImageTable component

`frontend/src/components/ImageTable.vue`

| Prop | Type | Notes |
|------|------|-------|
| `setUid` | `string` | Required. Drives the image list from the project store. |
| `module` | `string?` | Selects per-module column config (status column label, etc.) |
| `show-attrs` | `bool` | Show attr columns. Default: `false`. |
| `filter-uids` | `string[]?` | When set, only these UIDs are shown. Managed by `ModuleLayout`. |

Emits `selectionChange(uids: string[])`. `ModuleLayout` handles this internally.

**File operations live in the action bar, not in the rows.** Copy / Move / Delete act on the whole
checkbox **selection** and are rendered by `components/ImageFileActions.vue` in the Import page's
`#actions` slot — next to *Add images*, where a file manager puts them. Two rules follow:

- **They are Import-only.** Creating, re-filing and removing images is import-time curation; no other
  module page mounts `ImageFileActions`, so an analysis page cannot delete or move an image. **Crop to
  new image…** stays in the row's ⋯ menu (it needs the one image you clicked) but is likewise gated to
  `module === 'import'` — it creates an image too.
- **The ⋯ menu is per-image only.** Metadata, physical size, crop, copy UID, include/exclude, run
  history — anything that applies to *one* row. An action that reads "do this to the images I ticked"
  belongs in the action bar. That split is what keeps the menu short; it previously held Copy and Move
  as well, each one image at a time.

Every dialog takes the *selection*: `CopyDialog.vue` (`images: CciaImage[]`) dispatches one
`editImages.copyImage` task per image, and Move walks `/api/images/move` per image (both routes are
per-image).

### Deleting is one modal with four scopes

**There are exactly two places that delete image data**, and that is a deliberate ceiling
(`docs/todo/IMAGE_DELETE_PLAN.md`): the Import page's **Delete** modal (`DeleteImagesDialog.vue`) for
anything per-image, and **Settings → Storage** for the automatic whole-project reclaim. It used to be
five, spread across four screens.

The modal offers four scopes, radio-selected because they answer different questions and must not be
silently combinable. Each maps to one route, and `ImageFileActions.vue` runs it over the selection:

| Scope | Deletes | Route |
|---|---|---|
| Whole images | the image stores **and** everything derived | `/api/images/delete` |
| Versions | specific image versions + which one stays active | `/api/images/version/remove` |
| Label sets | specific segmentations + their measurements | `/api/images/labels/delete` |
| All analysis | everything derived, keeping the images | `/api/images/analysis/reset` |

Three rules that are easy to get wrong in a template live in the pure, tested
`utils/imageDelete.ts` — **use it, don't re-derive them**:

- Names are offered as the **union** across the selection, each chip badged `k/n` when only some
  images carry it, and skipped for the images that don't. An intersection hides the name entirely:
  select three images where two carry `B` and one doesn't, and `B` becomes undeletable until you
  re-select. The badge is what keeps the skip visible rather than silent.
- `default` is removed **last**, so `remove_image_version!`'s safe-primary un-import lands at the end
  of the loop rather than mid-way.
- The version that stays active is resolved **per image** (`resolveNewActive(own, removing, preferred,
  current)`). With a union list the user's pick may not exist on every image, and writing it into
  `_active` there would leave `ccid.json` naming a version that was never registered. For the same
  reason the "becomes un-imported" warning counts *images*, not the selection.

**One conflict blocks; a skip only warns.** The distinction is whether the user's stated intent can be
honoured:

- **Blocking** (`activeMismatches` → confirm greyed): an image keeps a version but *not* the one chosen
  to stay active. Substituting another version per image would look like it worked while quietly
  leaving that image on something the user didn't choose, so the modal says which count is affected and
  waits. An image that loses *every* version is **not** a conflict — it has no active to set, and that
  un-import is a legitimate outcome, warned about separately.
- **Non-blocking** (`partialNames` → a note): a version or label set that simply isn't on every selected
  image. It is applied where present and skipped elsewhere, which is the whole point of union
  semantics; the chip badge plus the note make the skip visible.

The versions scope **pre-selects every non-active version** — once a corrected version exists, the raw
import and the intermediates are what you no longer need.

Deleting a label set takes its **companions**: the registered labels zarr, the branch-label zarr, and
every `labelProps/` sidecar derived from that name (`{vn}__tracks.h5ad`, `{vn}__branch.h5ad`,
`{vn}.clustfeatures.json`, …). Prefix-driven (`{vn}.` / `{vn}__`) so a companion added later is swept
too, and so value_name `B` can't eat `B2.h5ad` — every `labelProps/` filename is built from the props
path (`img_label_props_path` / `img_track_props_path` / `img_branch_props_path`, and clustfeatures via
an extension swap), so the prefix rule is exhaustive by construction rather than by inspection.

**What a label-set delete deliberately does NOT take:** `gating/{vn}.json` — gate polygons are user
work, not output, so re-running the segmentation under the same name brings the strategy back. And
`spatialGraph/{suffix}.h5ad` / `spatialStats/{suffix}.json`, which are keyed by **run suffix, not
value_name** (the graph pools across segmentations), so there is no per-value_name file to take.

The modal **collects a plan and emits it**; the execution, the `k/N` readout and the toast stay in
`ImageFileActions`. Its own footer button carries the arm/confirm, so no scope is ever one click from
deleting. Two surfaces were removed once it covered them: the napari **ViewerPanel** no longer deletes
a label set (the viewer shows and hides layers; it does not curate the disk), and the
`importImages.remove` **task** is `hidden` from the module page — see *MODULES.md → hidden tasks*; it
stays registered, REPL-runnable and valid as a chain node.

**Progress on a bulk action comes from one of two places, never neither.** Copy goes over the **task
rail**, so the task console, the progress bar and the universal toast are free — but a batch must use
`taskStore.addMany(items, toastLabel)`, not N × `add()`, or `lastStarted` fires one "running in the
background" toast *per image*. Move and Delete are plain HTTP loops with no rail entry, so they report
themselves: a `k/N` `.cc-readout` beside the buttons while the loop runs, plus a `useToast()` line at
the end (and on partial failure, "k of N — see the log"). A destructive loop with no visible counter is
indistinguishable from a hang; `rm -r` on a multi-GB zarr is seconds per image. The "existing set OR new set by name" destination — the dropdown plus its collision check —
is the shared `utils/setDestination.ts` (`resolveSetDestination`/`destinationParams`), used by both;
don't re-validate it inline.

**Metadata warning icon.** A row shows a `pi-exclamation-triangle` next to the image name when
`metadataWarning(img)` (`frontend/src/lib/imageMetadataWarnings.ts` — the single source of truth,
shared with `PhysicalSizeDialog`'s inline warning so the two never disagree) flags missing/suspect
physical size or time-interval metadata. This includes `physicalSizeZCorrected` (the import-time
ImageJ-TIFF Z-spacing auto-fix, `omezarr.jl`) — an auto-corrected value stays flagged for human
confirmation even when it now looks plausible, since the source tag it was derived from (the
file's own ImageJ `spacing`/`unit`) isn't independently verifiable and has been observed to be a
placeholder rather than a real per-slice calibration on real data. Clicking the icon opens
`PhysicalSizeDialog.vue` right there (own local `physSizeDialogUid` ref — no page navigation),
focused on that image with the current checkbox selection carried in as the target set for
Apply/Fill-flagged. Shown on every module page — the icon isn't gated behind `showAttrs`/`module`.

**Frozen left columns.** The table lives in a `.table-scroll` (`overflow-x: auto`) wrapper; the
checkbox, viewer-eye, and **name** columns are `position: sticky` at fixed left offsets (0 / 36 / 68px)
so the image identity stays put while the channel/attr columns scroll (Excel-style freeze). Frozen
cells carry an opaque per-row background (`--row-bg`, set for hover/selected) so scrolled columns pass
under them; the header row sits above the body via `z-index`.

**Dimension columns.** A **Z** column (z-slice count) shows only when some image in the set is a
z-stack (`sizeZ > 1`); a **Duration** column (timelapse span = `(sizeT − 1) × timeIncrement`, formatted
via `utils/imageTable.ts → timelapseDuration`) shows only when some image is a timelapse (`sizeT > 1`)
— so 2D single-timepoint sets aren't cluttered with empty columns. All fields come straight from the
`CciaImage` payload (`sizeZ`/`sizeT`/`timeIncrement`/`timeIncrementUnit`).

**Excluded images are selectable on the import + metadata pages only** (`module === 'import' |
'metadata'` → `canSelectExcluded`): you curate/edit metadata there, including on excluded images, so
their checkboxes are enabled and select-all includes them. Everywhere else the selection stays the
runnable (included) subset (`includedUids`).

**CSV export** lives in `ModuleLayout`'s table-tools bar (next to Filter/Excluded): `exportCsv` →
`utils/imageTable.ts → imageTableCsvRows` (pure, tested) → `rowsToCsv`/`downloadBlob`
(`plots/export.ts`). It exports **every** image including excluded ones (flagged `Excluded` + the
`Exclusion note`), one aligned column per channel (`Channel 1…N`, value = the channel name) plus
Z/frames/duration/pixel-size and one column per attr.

**QC badge.** Separate from the metadata warning (which is import-metadata-specific), a row shows a
`pi-flag` **QC** badge when `qcSummary(img)` (`frontend/src/lib/qc.ts`) finds any QC finding on the
image. QC is the general "we processed this, but the output looks off" layer: the **backend** computes
findings per (task, output) into `1/{uid}/qc/{funName}/{valueName}.json` (see ARCHITECTURE → *QC
sidecars* and `docs/todo/QC_PLAN.md`); `qc.ts` only aggregates + formats them. The badge hover shows
the finding detail (e.g. drift correction's jump / canvas-expansion). It's **advisory** — never blocks.
`warn` findings tint amber; `info` are neutral. (MetadataPanel + chain-whiteboard surfaces are later phases.)

**Include / exclude an image.** Any image can be excluded from further processing/analysis — the
systematic successor to the old R app's `Include=Y/N` keyword (`CciaImage.included`, default `true`;
optional free-text `note`). The rule lives in ONE place — `frontend/src/utils/inclusion.ts`
(`isExcluded`/`isIncluded`/`includedUids`), so graying, select-all, and run-selection all agree.
- **Greyed, not hidden.** Excluded rows render dimmed (`.row-excluded`) with a persistent `pi-ban`
  **Excluded** badge (its tooltip shows the note), an editable note line under the UID, and an
  always-visible include/exclude toggle (`.incl-toggle`). Every other row shows the toggle on hover.
- **Unselectable.** Excluded images can't be checkbox-selected — dropped from select-all, "select
  flagged", and the remembered selection on reseed. Since every run (single task *and* chain) builds
  from the selection, this makes exclusion honored everywhere. `ChainModule`'s run list mirrors it
  (greyed, auto-select-all uses the included subset).
- **Hard-skipped in the backend too.** Belt-and-suspenders for run paths that bypass the checkboxes
  (chain resume, REPL): `_drop_excluded` (`api/src/sockets.jl`) filters excluded uids before dispatch
  and logs each skip. Set via `POST /api/images/inclusion/set`; `project.setInclusion` reflects it live.
- **Hide-excluded toggle.** One of the row filters below (default: show excluded rows, greyed).

**Row filters — add one by adding a table row, not a component.** The on/off toggles next to the
**Filter** button that hide image-table rows (**Excluded** / **Imported** / **Starred**) are declared
as data in `frontend/src/utils/rowFilters.ts` (`ROW_FILTERS`) and rendered by ONE `v-for` in
`ModuleLayout`. Each entry supplies its id, label, both icons, the `hides(img)` predicate, its count,
whether the button is worth showing at all, and a two-state tooltip. `ModuleLayout` holds a single
`rowFilterActive` bag, persisted per module under `cc-hide-<id>:<module>`.
**Do not hand-write a new toggle** — that is how this became three near-identical blocks of ref +
watch + computed + template. Add a `RowFilterDef`; the persistence, the button, the count and the
`filteredUids` clause all follow. Active filters AND together, and combine with the attribute and
processed-with filters.

**Star.** A plain per-image bookmark (`CciaImage.starred`, `isStarred`) — click the star in any row,
any number per set. It drives the Starred row filter and **nothing else**: no effect on selection,
runs, or processing. (It replaced a set-level single "reference image" nomination that an import-time
intensity window was derived from; that whole mechanism is gone — see `docs/FUTURE.md`.)

**Attribute extraction — regex + builder** (`MetadataPanel.vue` → *Extract via regex*). Pulls an
attr value out of each image's filename (or original path) with a JavaScript regex: the first
capture group `()` is used if present, else the whole match (`extractWith` in
`frontend/src/utils/regexBuilder.ts` — the single extractor, so the live preview equals the applied
result). The field's tooltip carries a brief example for people who don't know regex. There is **one** regex
input with **one** live preview (`regexSample → regexPreview` against the first target image); a
collapsible **Builder** with two modes — **Split into fields** (separator × 1st/2nd/3rd/last field ×
drop-extension, `buildFieldRegex`) and **Around a marker** (extract a token *preceded/followed by*
context via lookbehind/lookahead, `buildLookaroundRegex`). Each context side is a **literal text +
a class that varies** (so "M" `+ number` → `(?<=M\d+)` anchors M1b/M2a/M4f without hardcoding the
mouse number → `b`/`a`/`f`); the extract token is a class or a raw custom pattern. Both modes write
straight into that same field on any change, so it's a way to construct the visible regex, not a
second input.
The user then watches the preview and can hand-edit the pattern. The pure builder/extract logic
lives in the util (Vitest-covered); the component only wires refs.

**Physical size & timing editor** (`frontend/src/components/PhysicalSizeDialog.vue`) is a modal,
not a sidebar section — the first version crammed six fields + long explanatory paragraphs into
the 280px `MetadataPanel` sidebar and was unreadable. Built on the shared `BaseModal` shell (see
*Modals & dialogs* above — no PrimeVue Dialog). Explanatory text lives in tooltips
(the header's `pi-info-circle`, per-field labels, button tooltips), not inline paragraphs.
Actions all write only the toggled fields (X/Y/Z/Δt chips — untick what's already correct so a fix
to one axis doesn't also rewrite ones that are fine): **Apply** (to the selection it was opened
with, or just the focused image if none), **Copy to selected** (the other selected images),
**Fill flagged** (only the *other* selected images that currently show a warning — the
batch-fix-from-a-known-good-reference workflow). Also reachable via an "Open editor" button in
`MetadataPanel`'s sidebar (no specific image clicked — focuses the first selected/set image)
alongside a flagged-count badge for the set. When a target already has processed versions or
segmentations, a second (informational) line via `downstreamArtifactsNote` reminds that those were
built with the current calibration and must be re-run — corrections/measurements read pixel size
from the zarr, not this dialog, so an edit doesn't reach them retroactively.

**Name-column header buttons** (`ImageTable.vue`, next to "Name"): a `pi-exclamation-triangle`
toggle to select/deselect every currently-flagged image in one click (`selectFlagged`, amber when
active, shown on every module page), and a `pi-sync` **"Resync flagged from file"** button
(`resyncFlagged` → `POST /api/images/meta/resync`), shown only on `module === 'metadata' | 'import'`
(same gating as the page-icon "open editor" button), for images that were imported *before*
physical-size/timing `meta` was tracked at all. Their OME-ZARR is already correct, so this
re-derives `meta` straight from the `"default"` (original bioformats2raw) zarr, deliberately never
whichever version is currently `active` — drift/cellpose-correct outputs carry no OME calibration
metadata at all, see CLAUDE.md → *OME-ZARR dual-format* — rather than asking the user to type
known-good values back in or re-import. Both header buttons operate on `flaggedUids`, not the
checkbox selection.

**Inline cell editing** (`ImageTable.vue`). Attribute cells, **channel-name** cells, and the
exclusion **note** are all click-to-edit through ONE generic core (`startEdit`/`commitEdit`/
`cancelEdit`/`focusEditInput`, keyed `${uid}:${namespacedKey}`) — each field only supplies a
`save*(val)` persister (`saveAttr` → `attr/set`, `saveChannel` → `channelnames`, `saveNote` →
`inclusion/set`). Add a new editable cell by reusing the core + a saver, never a second edit
lifecycle. Channel edits replace one index in the image's name list and re-send the whole list
(the endpoint is list-valued); a cell is editable only up to the image's channel count
(`channelEditable`). This is why the metadata panel's channel section has no "copy to all" button —
naming is done per-cell in the table (bulk-assign-to-selection via the textarea remains).

**Attribute + channel editing is Metadata-page-only.** The attr/channel columns are *shown*
read-only on every page that sets `show-attrs` (so you can see the metadata in context), but they're
only *editable* where `ModuleLayout` is given `:editable-meta="true"` — i.e. `MetadataModule`. This
keeps metadata a single place to change (no accidental edits from the segment/track/cluster pages).
The exclusion note + include/exclude toggle stay editable everywhere (excluding an image from
processing is a per-page action, not metadata).

---

## TaskRunner component

`frontend/src/tasks/TaskRunner.vue`

Fetches task definitions for a category, renders parameter forms, and submits tasks over WebSocket.
Always rendered in the `#right` slot of `ModuleLayout`.

**Right-sidebar resize** is a shared composable — `usePanelResize` (`frontend/src/composables/
usePanelResize.ts`): a left-edge drag handle, min/max clamp, and (with a `storageKey`) width
persisted to localStorage. Used by both `TaskRunner` (`cc-taskrunner-width`) and `MetadataPanel`
(`cc-metadata-width`) so the behaviour isn't reimplemented per panel — add a resizable panel by
calling it, not by copying the drag math. (MetadataPanel wraps a non-scrolling outer element around
its scrolling body so the handle stays put while the panel scrolls.)

| Prop | Type | Notes |
|------|------|-------|
| `defs` | `TaskDef[]` | From `useTaskDefs('categoryName')` |
| `module` | `string` | Passed through to task dispatch |
| `selected-uids` | `string[]` | Images the task will run on |
| `selected-names` | `string[]` | Display names matching `selectedUids` |

Task definitions are loaded once per session via `useTaskDefs`, which calls `GET /api/tasks/definitions?category=X`.

**Pool dropdown**: a `<select>` populated from `GET /api/pools`. On task switch, automatically
selects the pool matching the task def's `resource_pool` field. The chosen pool name is sent as
`poolName` in the `task:run` WS message, which `handle_task_run` in `sockets.jl` passes to
`run_task` as the `pool_name` override kwarg. The old concurrent-task slider
(`task:setLimit` / `tasksLimit`) has been removed entirely.

**Task list scoping.** `useTaskStore().forModule(module, projectUid?)` and `clearFinished(module,
projectUid?)` take an optional `projectUid` — `TaskList.vue`/`TaskRunner.vue` always pass the
current project's uid so switching projects doesn't leave a previous project's (e.g. cancelled)
tasks visible in the module sidebar. The global `/tasks` manager (`TasksModule.vue`) intentionally
omits it — that page is the cross-project view. `TaskEntry.projectUid` is what makes the filter
possible; it's stamped on every entry at `add()`/`addFromChainEvent()`.

**Cancel all** — a `pi-times-circle` button next to "Clear finished" in the Tasks section header,
shown only when the current module+project has running/queued tasks. Cancels every one of them via
the same per-task path as the individual cancel button (`task:cancel`/`chain:cancel` over WS,
deduping so a multi-node chain run only sends one `chain:cancel`).

---

## Adding a plot or visualization panel

**First: is it a summary plot?** If the data is server-aggregated, you do not write a component at all —
drop a `app/src/plotDefinitions/<id>.json` and it appears in every "+ Plot" picker. See `docs/PLOTS.md`
→ *Hosting — ONE way*. The rest of this section is for the cases that need their own component.

**Where it goes.** Left column (`#plots` slot — `ModuleLayout` wraps it in the shared collapsible
section) for canvases that belong beside the image list; `#right` slot for a panel alongside or instead
of `TaskRunner`. Both slots hand you `setUid` / `selectedUids` / `selectedNames`, so a panel needs no
refs of its own. Reserve `#below-table` for rare extra custom content. Fetch over REST in
`onMounted`/`watch`, or subscribe with `ws.on` (see *WS events*).

Plot libraries in use — **two renderers, split by job**:
- **2D canvas** (no library) — **per-cell dot plots**: the gating scatter and the UMAP. Every point is
  drawn coloured by its LOCAL density (`plots/density.ts` `pointDensities` → the blue-heat ramp in
  `plots/flowColors.ts`) — that per-point colouring is the FlowJo/OMIQ look; contours come from
  **d3-contour** (`plots/contour.ts`). `PlotLayers` draws dots-or-contours plus population overlays and
  `GateScatterCell` composites it with `GateOverlay`. There is **no WebGL**: regl-scatterplot was
  removed (it survives only as an unused `package.json` entry). A 2D canvas suffices because the cloud
  is non-interactive, and export re-renders the same content at any scale instead of screen-grabbing a
  GPU buffer. See *Gating page* and `docs/PLOTS.md` §0.
- **Observable Plot** (`@observablehq/plot`, SVG) — **summary charts**: histogram, box/violin/beeswarm,
  bar, frequency/stacked, and (roadmap) heatmaps/tiled maps via `Plot.cell`/`Plot.raster`. Used
  wherever the data is **server-aggregated** (tiny payloads) and the ggplot `theme_classic` look /
  beeswarm / resize matter more than raw point throughput. See "Analysis-plot canvas (summary plots,
  Observable Plot)" and `docs/PLOTS.md` §0.

Why two: a per-point renderer is needed to draw every cell and sketch gates on it; an SVG
grammar-of-graphics library gives the cleaner publication look for pre-aggregated summaries. Never add
or swap a charting library without updating `docs/PLOTS.md` §0 (which owns the rationale) and this list.

### Plot loading state — delayed spinner

Heavy plots (a slow `/api/plot_data`, a big point fetch) must show they're working — a blank
panel reads as "frozen". But a spinner that flashes on every quick plot is worse noise. So the rule:
**a delayed spinner, never an immediate one.**

- `composables/useDelayedLoading.ts` — `useDelayedLoading(loadingRef, delayMs = 350)` → a `show` ref
  that flips true ONLY if loading stays true past the threshold, and clears instantly when it ends.
  Fast/cheap plots finish before 350 ms, so they never flash it; only genuinely heavy loads reveal it.
  Use `toRef(props, 'loading')` when the loading state is a prop.
- `components/plots/PlotSpinner.vue` — the shared wheel overlay. Put it inside a `position: relative`
  container: `<PlotSpinner v-if="showSpinner" label="Loading…" />`. It's `pointer-events: none`, so it
  never blocks the plot underneath, and honours `prefers-reduced-motion`.

Do **not** hand-roll per-plot "…" text or an immediate spinner. **Small/embedded plots stay out**: the
gate montage tiles (compact `GateScatterCell`, rendered by `GateMontage`) keep an unobtrusive dot, not a
wheel per tile — gate the overlay on `!compact` (or equivalent). Wired today in `SummaryPanel` and the
full-size `GateScatterCell` (Gate page); `UmapView` has its own empty-state wheel. New heavy plots: reuse
these two primitives.

**Gate scatters — one renderer, three hosts.** `components/plots/GateScatterCell.vue` is the ONE
scatter+gate body (2D-canvas dots + contour/pop-colour layer + gate overlay). The interactive Gate page
(`GatePlotPanel`, `mode` = rectangle/polygon) and every read-only montage tile (`mode="off"`) share it.
**Render modes** via the shared `RenderModeToggle.vue`: `points` (per-point pseudocolour), `contour`
(rings only — the fast path, dot pass skipped), `outliers` (rings + dots for the sparse tail, FlowJo /
old-R "contour ± outliers"). The maths is the pure, unit-tested `plots/density.ts` + `plots/contour.ts`.

Montages go through `components/plots/GateMontage.vue` — a grid of `GateScatterCell` tiles owning the
per-tile fetch (`plotmeta`/`plotdata`/`stats`), transpose reuse, optional coloured population overlays
and PNG/PDF export. Two tile producers: `GatingStrategyView` (tree-derived, responsive wrap) and
`GatePairsPanel` (a `ggpairs` matrix, `cols` set). A tile's `role` is `scatter` (fetch + render, the
default — so tree-derived defs need no role), `diagonal` (a labelled name cell, no fetch) or `corr` (an
upper-triangle Pearson-r cell reusing its mirror's points, no fetch). In matrix mode tiles get
`hideAxisLabels`, since the diagonal already names each channel. **Add a new gate-montage view by
building `PanelDef[]` and rendering `<GateMontage>` — never a second gate renderer.**

The gate scatter's axis chrome is HTML (tick labels + rotated axis names), so it doesn't inherit
Plot's `style.fontSize`. It takes an explicit **`fontSize`** prop (default 11) exposed as the
`--gate-font` CSS var and used by the tick/axis-name rules (so the vis **Font size** slider works on
the board's gating-strategy plot); `GatingStrategyView` forwards `vis.fontSize` through `GateMontage`.
Gate `%` labels (`GateOverlay.drawGateLabel`) are clamped to the plot box on **both** axes — vertical
fallback (above→below→inside) plus a horizontal clamp on the centred text — so a gate at the edge
doesn't clip the trailing `…%`.

### Generic plot-integration interface (reuse across surfaces)

A plot is defined **once** and appears on any surface — module page, **Analysis board**, and (future)
the **chain whiteboard** (`docs/SCHEDULER.md`) — via a flag. **No per-plot host wiring.** This is how you
"drop a plot onto the board" without touching `LayoutCanvas`/`ClusterPlots`.

**The contract a plot component must honour:**
- **Self-contained**: renders from a standard prop bag + persisted `state`, and **seeds its own defaults**
  (e.g. `ClusterHeatmapPanel` seeds `features` from the run — never rely on the host to seed). Persist
  every user-settable option in `state` (see "Persisting view state").
- **Standard bag**: `projectUid, setUid, imageUids, vis, state` (+ for cluster plots `popType, suffix,
  shownPops`; + panel chrome `index, active, docked, persistKey`).
- **Export hooks** for the board's PDF/CSV: `exportImage()` → a plot-only **light-theme** PNG (dark theme
  is on-screen only), and `getCsv()` → the shown data. (Interactive views may instead expose
  `exportFormats`/`exportAs`.)

**Two registries carry the surface "checkboxes":**
- `components/canvas/interactiveViews.ts` — interactive VIEWS (hosted by `InteractivePanel`), flags
  `clusterPage` / `analysisBoard`.
- `modules/cluster/clusterPanels.ts` — summary-family cluster PANELS (wrap `CanvasPanel`), flags
  `analysisBoard` / `trackOnly` / `needsCols`, plus a `props(ctx)` mapper so the host binds panel-specific
  props generically.

**Hosts render from the registries**: each builds its `+Plot` picker by filtering on its own flag and
renders every slot with one generic `<component :is v-bind>`. So adding a plot to a surface = write the
component to the contract + one registry line + tick the flag. The cluster page (`ClusterPlots.vue`) and
the board (`LayoutCanvas.vue`) do this identically — there is no "cluster page way" and "board way", and
a future chain-whiteboard host consumes the same registries rather than re-wiring plots per node.

**`docked` is the contract's chrome switch** — a panel reads it to hide what only makes sense
free-floating (its own Export dropdown), since the board exports via PDF/CSV instead. Details:
`docs/ANALYSIS.md` → *`docked` — the chrome switch*.

**Exception — the gating page (`gate/GatingPlots.vue`) is intentionally NOT registry-hosted.** It is a
single, *write-capable* gate-drawing workspace (`GatePlotPanel` draws/edits gates), not a multi-type
read-only plot host — the opposite of the board contract. The board hosts gating **read-only** via
`GatingStrategyView` (an interactive-registry view, `analysisBoard: true`). Don't try to fold the
gate-drawing surface into the registry.

See **`docs/ANALYSIS.md`** for the Analysis board itself (tabs, comic-plate layout, persistence keys,
the read-only cluster manager, and PDF/CSV export incl. the shared hi-res raster path).

### Auto-hide panel controls (plot fills the whole box)

`CanvasPanel` gives its **plot the whole box** and overlays the control surfaces, revealing them only on
hover (or when pinned). This is why a board plot — and its PDF export — fills its slot instead of being
squashed by a stack of dropdowns (the squashed plot exported as a clipped sliver; see `docs/ANALYSIS.md`).

- **Default ON** (`autoHide` prop, default `true`). The `#actions` (top) and `#footer` (bottom) slots
  render as absolute overlay strips over the body; a **pin** toggle (`pi-thumbtack`, next to the drag
  icon) keeps them visible. Pin/collapse are transient local refs (chrome preferences), not persisted.
- **Interactive views whose toolbar lives INSIDE the body** (`GatingStrategyView` `.gs-bar`, `UmapView`
  `.uv-ctrl` — which carries the cluster-label **and** population-legend toggles, each persisted per
  panel in `state`, `ImageStripView` `.is-bar`) opt in by tagging that bar `.cc-panel-controls` **and** giving
  their root `position: relative` — the global rule in `style.css` (`.panel:hover`/`.panel.controls-pinned`)
  then auto-hides it by the same trigger. One mechanism for every control surface; don't add a second.
- **Opt OUT with `:auto-hide="false"`** where you interact with the plot constantly and controls popping
  over it would fight the tools — the gate-**drawing** panels (`GatePlotPanel`/`GatePairsPanel`) do this,
  so their render-mode / gate tools stay in flow.
- **Capture safety**: a `.capturing` ancestor (set on the board grid during export) force-hides every
  `.cc-panel-controls`, so a pinned/hovered strip never leaks into a snapshot.

### Canvas zoom (fit-to-view)

Every plot canvas — the Analysis board's fixed grid AND **all** free-floating module canvases
(`SummaryCanvas`, `GatingPlots`, `ClusterPlots`) — shares one visual zoom, so a big workspace fits the
screen without hiding the sidebar. `composables/useCanvasZoom.ts` owns the `zoom` ref + `fitWidth`/`fitHeight`;
`components/canvas/CanvasZoomControl.vue` is the shared slider/fit/% control. It's a **CSS
`transform: scale`** — purely visual: it never resizes a plot's own canvas or changes what's exported
(the export re-renders at full logical resolution; the board neutralises the zoom during PDF capture).

- **Fixed-grid board**: the grid scales inside a `.lc-zoom` footprint (sized to the scaled dims so the
  viewport scrolls); auto-fits width on first render if the board would overflow.
- **Free-floating canvases**: the panels scale inside a `.sc-zoom`/`.gp-zoom`/`.cp-zoom` layer; the
  population manager sits OUTSIDE it so the control panel stays full-size. Because panels are dragged in
  screen px, the host `provide()`s the zoom under `CANVAS_ZOOM_KEY` and `CanvasPanel` injects it into
  `useFloatingPanel`, which divides drag deltas by the zoom (else a panel moves `zoom`× too fast).
  - **Workspace grows on zoom-out** (`composables/useCanvasWorkspace.ts`): the zoom layer is sized to
    `viewport / min(zoom, 1)`, so zooming OUT enlarges the *logical* workspace (Tile spreads into it, a
    panel can be dragged across it) instead of shrinking everything into the top-left and wasting the
    page — the layer is the panels' `offsetParent`, so `useFloatingPanel`'s clamp and `useCanvasPanels`'
    `arrangeGrid` both use the enlarged size. At ≥ 100% it stays viewport-sized (zoom-in inspects). "Fit"
    fits the actual plot bounding box (`useCanvasPanels.contentBounds`), not the zoom-dependent workspace.

**Zoom shortcuts** (all canvases, wired once in `useCanvasZoom`): **shift + mouse-wheel** over the canvas
zooms; **shift +/-** steps; **shift + 0** resets. Keys are ignored while typing in an input.

### Show/hide the population manager

The floating population manager (`PopulationManager` on gate/tracking + cluster pages, `SeriesPicker`
on summary pages) has a **toggle** (`pi-sitemap`) next to the arrange-windows icons on **every** module
canvas that has one (`SummaryCanvas`, `GatingPlots`, `ClusterPlots`), persisted per canvas in the
`shared` bag (`shared.showManager`, default shown). Wrap the manager `v-if="showManager"`.

---

## WS events — frontend side

Subscribe in `onMounted`, unsubscribe in `onUnmounted`:

```ts
import { ws } from '../ws'

onMounted(() => {
  ws.on('napari:event:mySignal', (data) => { ... })
})
onUnmounted(() => {
  ws.off('napari:event:mySignal')
})
```

For task results, the `task:result` message updates `img.filepaths[valueName]` in the Pinia project store automatically (handled in `ws.ts`). Panels that need to re-fetch when a task changes data should use **`useDataRefresh`** (see *Data freshness* below), not a hand-rolled watch.

Full WS message-type reference is in `ARCHITECTURE.md`.

### A dropped terminal frame is recovered, not tolerated

**Every frame reaches listeners through one function — `dispatch(data)` in `stores/ws.ts`.** `onmessage`
just parses and calls it. That matters because a task's terminal frame (`task:status` done/failed, or
`chain:node:done`/`failed` for a whiteboard node) is the ONE frame carrying its outcome, and the server
drops frames for a slow client **by design** (per-client drop-on-full queue — `docs/API.md`). Lose it and
the store pinned the task at `running` forever *and* silently skipped everything hanging off completion:
the image status, `bumpDataVersion` (so plots never auto-refresh), `refreshImageMeta`, the napari reload,
the observer's completion watch. Five listeners, one missing frame.

So while this tab has work in flight, the ws store polls `GET /api/tasks/recent` (the rail's banked terminal
frames — every producer, jobs and batch movies included) and **re-emits the frame that went missing**
through that same `dispatch`. The reconstruction lives in `utils/taskReconcile.ts`. Rules worth keeping:

- **Rebuild the carrier the socket would have used**, not a stand-in: a chain run emits no `task:status`
  at all, so a chain node is recovered as `chain:node:*`. Swapping carriers would be a behaviour change.
- **A chain row is keyed by a synthetic `runId::nodeId::imageUid`**, so matching goes through
  `backendTaskId` (the `taskId` the chain frames carry) while addressing uses the store id.
- **Only act on an outcome the server can NAME.** A task that vanished without one (the backend restarted
  under us) is left alone — never guess a completion.
- **A late real frame for a recovered task is swallowed** (`recovered` set, keyed by scheduler task id):
  re-running the side effects would refetch plots, reload napari, and double-count an observer attempt.
- Adding a new completion listener needs none of this — subscribe with `ws.on` as usual and a recovered
  frame reaches you like any other. Do **not** add a second poller; `taskReconcile.ts` owns
  `/api/tasks/recent` and `utils/runningTasks.ts` owns `/api/tasks`.

### …and work that started before this tab did is adopted, not ignored

The other half of the same problem. The `tasks` store is built purely from WS events **this tab**
received, and nothing ever asked what was already running — so a page reload mid-run (or a second tab, or
the app opened on another machine) showed an **empty** task list while the backend segmented 20 images,
and each terminal frame then landed on a row that didn't exist (`setStatus` matches by id and returns
early). The tasks never appeared, not even as they finished — while the plots refreshed anyway, because
`bumpDataVersion` keys off the frame's `imageUid` rather than a row.

So on every (re)connect — and again when a project loads, since the socket usually opens first — the ws
store fetches `GET /api/tasks` and adopts the in-flight set (`adoptableTasks` in `utils/runningTasks.ts`,
the module that already owns that endpoint). Adopted rows show a true elapsed (the snapshot carries
`started_at`), take live progress/log frames from then on, and **can be cancelled** — `task:cancel` goes
by the scheduler's own id.

- **They support Re-run, because the snapshot carries the params the run was submitted with**
  (`list_tasks()` → `GET /api/tasks`). `rerun()` sends `params`, so without them the button would
  silently relaunch the task with the JSON spec's defaults — which is why the row is only offered Re-run
  once they are known. A snapshot that carries none (a backend predating the field, or a param set that
  can't be written as JSON — the route publishes `null` rather than a partial one) sets `paramsUnknown`
  on the row, which withholds the button. **No badge marks that**: it needs a backend older than the
  field to happen at all, and a permanent icon on every adopted row to explain a case nobody meets is
  the kind of standing UI noise `UI copy — keep it short` exists to prevent.
- **One predicate decides it — `canRerunTask` (`utils/taskRerun.ts`).** Both surfaces that draw the
  button (the per-module `TaskList`, the `/tasks` manager) call it. They had their own copies and had
  already drifted: the manager offered Re-run on a **chain node**, whose `params` are `{}` because chain
  rows are built from `chain:node:*` frames — so the click relaunched the node standalone on defaults.
- **The log backfills from disk on first open** (`utils/taskLogBackfill.ts`). The scheduler tees every line
  to `{img._dir}/logs/{fun_name}.log`, so the output from before this tab connected is not lost — but that
  file is CUMULATIVE (one per image+fun, appended by every run), so the fetch passes the task's
  `started_at` as `since` and the server slices it. Slicing is server-side because the file's stamps are
  local time and the server is the process whose clock wrote them (`_tasklog_since`). Fetched lazily, on
  the click that opens the log — twenty adopted rows must not fire twenty requests for output nobody
  asked to see. No `started_at` (a queued task, an older backend) → no fetch, because the unsliced file
  would show a previous run's output as this row's.
- **Chain nodes are adopted under the key their own frames use** (`runId::nodeId::imageUid`), so the next
  `chain:node:*` frame updates that row instead of adding a second one — which is why `list_tasks()`
  reports `chain_node_id`. A node with no node id is skipped: a **set-scope** node bypasses `run_task`, so
  it has no record at all. (The chain *board* recovers a reloaded run separately and more completely, from
  the run's own persisted state via `/api/chains/run` — it has every node, not just the in-flight ones.
  This is only the task list's copy.)
- **Also skipped**: a row this tab already tracks (its own entry is richer — matched on the scheduler id,
  which for a chain row lives on `backendTaskId`), an image the loaded project doesn't have (the snapshot
  carries no `projectUid`, so it may be another project's work), and anything not `queued`/`running`.
- **It does NOT copy the console's retire-on-miss rule.** `api/task_console.jl` drops a row that vanishes
  from the snapshot and tallies it "ended", because it may never see the terminal frame; the browser has
  the outcome poll above and recovers the *real* outcome instead of guessing.
- `runningTaskCount()` still counts the **whole** snapshot, including the rows adoption drops — "is the
  backend busy?" is a different question from "what can this tab show?", and a quit must warn about a
  chain node mid-write.

### Task elapsed time — the backend's timestamps, one formatter, one clock

**A task's start and end come from the backend, not from when this tab received a frame.** `task:status`
and `chain:node:*` carry `startedAt`/`finishedAt`, and the recovered frames carry the outcome row's
(`docs/API.md` → *Elapsed time is served, not guessed*). `stores/ws.ts` parses them with `parseRailTime`
and passes them to `tasks.setStatus(id, status, { startedAt, finishedAt })`, which **prefers them over
stamping `new Date()`** — a recovered terminal frame arrives seconds or minutes late, so stamping arrival
inflated every recovered task's duration by the poll delay. `new Date()` remains the fallback for a
producer whose start the backend never noted.

Two shared pieces, and a new elapsed counter must use both rather than hand-rolling a fourth copy (there
were three, and they had already drifted in what they printed):

| Need | Use |
|---|---|
| parse / format / compute an elapsed | `utils/taskElapsed.ts` — `parseRailTime`, `formatTaskDuration`, `taskElapsed(startedAt, finishedAt, now)` |
| a reactive `now` that ticks | `composables/useNowTick()` — ONE shared 1s interval, reference-counted, released with the component scope |

`useNowTick` exists because each counter owning a `setInterval` means N timers on N phases (two counters
on screen disagreeing by up to a second) and N chances to leak one. The counting logic lives in
`utils/nowTick.ts` so it is testable without mounting a component. Consumers today: `tasks/TaskList.vue`,
`modules/TasksModule.vue`, `components/ChainLiveNode.vue`.

Known gap: the `tasks` store is built from WS events only, so a tab opened mid-run has no row for work
already in flight — nothing to time. Rebuilding rows from `GET /api/tasks` would be a separate change.

---

## Data freshness — task-refresh (no per-plot reload buttons)

A task can rewrite data **in place** (same `value_name` / clustering `suffix`), so `img.filepaths`
doesn't change and a plot keyed on it never re-fetches. Rather than give every plot a manual reload
button, plots auto-refresh off a **targeted, per-image version signal**:

- `stores/project.ts` holds `dataVersion: Record<imageUid, number>`. On a successful task (`ws.ts`,
  `task:status == 'done'`) it bumps the touched image(s) — `bumpDataVersion(uid)`. A **set/combined**
  task reports all its members in the status message's `imageUids` (the backend sends the member list,
  not just the representative — see `api/src/sockets.jl`), so every member is bumped.
- Plots subscribe with the **one primitive**, `composables/useDataRefresh.ts`:
  ```ts
  useDataRefresh(() => props.imageUids, load)   // refetch only when a task touches one of THESE images
  ```
  It watches `project.dataVersionFor(theirImages)` and calls the reload fn only when an image *that plot
  shows* changed — never on unrelated tasks. Used by `useSummaryData`, `UmapView`, the cluster panels
  (heatmap / HMM) and `GatingStrategyView`. **Do not** re-import the store and hand-weave a `dataVersion`
  watch in a new plot — call `useDataRefresh`.
- Gated by the global **`autoRefreshOnTask`** setting (Settings → Interface, on by default). Because
  `useDataRefresh` is the single chokepoint, that one toggle governs every plot; off → plots refresh on
  the next navigation / input change instead.

This mirrors the older gate path (`gating:popmap` → `reloadToken`) and the old R app's success-time
`retrieveState`. The **napari viewer** refresh is a separate, data-vs-image path — see `docs/NAPARI.md`.

---

## AppSidebar

`frontend/src/components/AppSidebar.vue`

All nav group headings are collapsible buttons. Clicking a heading toggles the group open/closed;
a chevron icon (`pi-chevron-down` / `pi-chevron-right`) reflects the current state.

The **napari viewer controls** are NOT in the sidebar — the sidebar only carries the button that
toggles them. They live in a `FloatingPanel` mounted in `App.vue`; see *Floating panels* above and
*ViewerPanel component* below.

### Nav item reference

```ts
interface NavItem {
  to:               string      // Vue Router path
  label:            string      // sidebar label
  icon:             string      // PrimeIcons class e.g. 'pi-th-large'
  tip:              string      // tooltip text (required)
  disabled?:        boolean     // grey out the link entirely
  soon?:            boolean     // adds a "soon" badge
  requiresProject?: boolean     // grey + lock when no project open
}
```

Icons: browse at https://primevue.org/icons — use the `pi-*` name, prefix with `pi` in the class list: `['pi', item.icon]`.

---

## ViewerPanel component

`frontend/src/components/ViewerPanel.vue`

Shows the current napari image and switches between versions (value names). Mounted in `App.vue`
inside a `FloatingPanel` (`storage-key="viewer"`), toggled by the sidebar's "Viewer controls" button
(`settings.viewerPanelOpen`, persisted) — it was a sidebar group once and outgrew the 190px nav.

**State**: image name, `valueName` dropdown (options from `img.filepaths` keys in the project
store). Changing `valueName` auto-opens the image in Napari via the REST `/api/napari/open`
endpoint.

**Auto-refresh**: subscribes to `task:status` WS events in `onMounted`; when a task transitions
to `"done"` the viewer refreshes its image data so newly written versions appear immediately.

**Populations sub-menu** (per-pop-type point toggles, after a `.opt-sep` divider): one icon per
CELL-grained pop type — `flow` (`pi-chart-scatter`) and `clust` (`pi-palette`) — each showing that
pop type's populations as coloured cell-centroid Points in napari. **Icons match the sidebar module
nav** (Gate/Cluster-cells/Track/Cluster-tracks) so a pop type reads the same everywhere. POSTs
`/api/napari/show-populations` with `popType` + `show` and **blank valueName → the server resolves
the ACTIVE segmentation** (the one gating/clustering write to; sending `labelNames[0]` was a bug —
the first label set isn't necessarily active, so clust pops never resolved). The bridge namespaces
layers by `(popType)` so flow and clust coexist.
State is per-pop-type and **remembered** (`settings.popVisible`/`setPopVisible`, keyed by pop type;
default off), auto-applied on image open, and **re-pushed on every `gating:popmap`** for the changed
pop type so the overlay tracks edits live. Only cell-grained types are here: `track`/`trackclust`
are track-grained (membership is track_ids, not cell labels) so points would be wrong — their viz is
**ribbons** via `show-tracks` (two more toggles: `pi-directions` = gated track pops, `pi-sitemap` =
trackclust cluster pops; both route through `pushTracks`, which sends `showGatedTracks` +
`showTrackclust` in one call, and the bridge namespaces Tracks layers by `(popType)`). Per-pop
visibility and the dot-size slider live in the population manager — see the gating section.
(`docs/NAPARI.md` — linked brushing.) **Icon convention**: append new toggles at the end of the
row; group unrelated toggles behind an `.opt-sep` divider.

---

## Task definition fields — resource_pool

The TypeScript type `TaskDef` has `resource_pool?: string` (optional string). Every task JSON
in `app/src/tasks/<category>/<name>.json` should include this field:

```json
{ "resource_pool": "cpu" }     // general CPU compute — most tasks
{ "resource_pool": "gpu" }     // the GPU — cellpose family (limit 1)
{ "resource_pool": "io" }      // local disk — import/convert/crop
{ "resource_pool": "network" } // remote/SMB — reserved for HPC, unused today
```

The `tasksLimit` field and the concurrent-task slider have been removed. `TaskDef` no longer
has a `tasksLimit` field. The pool dropdown in `TaskRunner.vue` reads `resource_pool` and
pre-selects the matching pool from `/api/pools`.

---

## Chain whiteboard

Route `/chain` → `frontend/src/modules/ChainModule.vue`.

The whiteboard is the visual authoring tool for chain templates. It reads and writes the same `chains/<name>.json` format that `run_chain` and `save_chain_template!` use from the REPL — one format, two authoring paths.

`ChainModule` is wrapped in `<KeepAlive>` in `App.vue` so navigating to other pages and back does **not** reset unsaved edits. Edits only clear on an explicit reload (↻ button) or chain switch.

### Layout — Edit tab

```
Left (190px)               Center (flex)             Right (260px, opens on click)
────────────────           ──────────────────────    ───────────────────────────
Chain selector             @vue-flow/core canvas     Node config panel
+ New / Reload / Save      Node palette drop target  - Scope select
Task palette               Background grid           - Barrier policy (set nodes)
(by category,              Nodes + edges             - Resource pool dropdown
draggable)                                             (from /api/pools)
────────────                                         - ParamRenderer for params
Run table (bottom)
- Set selector
- Image checkbox list
- Run chain button
```

The ↻ Reload button explicitly discards unsaved edits and reloads the chain from disk. Save (💾) writes the current canvas state to disk.

The **Run table** is pinned at the bottom of the palette sidebar. Select a set, check/uncheck individual images, then click "Run chain". Images default to all-selected when you switch sets. The run table auto-seeds from the first available set on project open.

The canvas uses `v-show` (not `v-if`) so VueFlow's state is preserved when switching to the Live tab and back.

### Layout — Live tab

The Live tab shows real-time status of chain nodes received via WebSocket. Each `chain:node:*`
event upserts a task in the task store (keyed `runId::nodeId::imageUid`). The Live canvas renders
these as a grid: one row per `nodeId`, one column per `imageUid`.

**Run selector**: a dropdown showing `"chainName / runId"` for each known run. Auto-switches to
the newest run when a new `chain:run:started` event arrives.

**Queued vs running**: the backend emits `chain:node:queued` when a node is submitted to its pool
and `chain:node:running` only when a worker actually starts it. A node waiting for a (e.g. GPU)
slot shows as `:queued` with no elapsed time; it flips to `:running` at the real start. With a
`gpu = 1` pool and three images, the grid shows one running and two queued — not three running.

**Elapsed timer**: `ChainLiveNode.vue` ticks elapsed time via a local `setInterval` using
`startedAt` / `finishedAt` passed as epoch milliseconds. `startedAt` is stamped on the `running`
event (real slot acquisition), so each node's elapsed reflects its own duration. A `new Date()`
call inside a Vue `computed` is not reactive and would freeze — use `setInterval` + a
`ref(Date.now())` tick instead.

**Node labels**: `ChainLiveNode.vue` shows the human-friendly `label`, resolved in `ws.ts` from the
task-defs store (`useTaskDefsStore().labelFor(fn)`) before calling `taskStore.addFromChainEvent`,
falling back to `fn.split('.').pop()` only if defs haven't loaded yet.

**Cancel**: a `chain:node:failed` event with `status === 'cancelled'` maps to a `cancelled` entry
(not `failed`). `setStatus` makes user-initiated `cancelled` sticky, so a late backend event can't
flip a cancelled task back to running/done/green.

**Resume / resume-from-here**: the Live toolbar has a **Resume** button (`resumeRun`) that re-runs
the selected run — WS `chain:run` with `runId` (no `chain`/`imageUids` needed; the backend restores
them from the run). By default it re-runs only failed / unfinished / params-changed nodes (see
`docs/SCHEDULER.md` → *Resume*). Clicking a **task node** picks it as the **start node**
(`restartNodeId`, a chain-template node id); the button then sends `startNode` too, force-re-running
that node **and everything downstream** even if `:done` — so it's obvious *where* a resume begins.
The picked node (solid accent + "resume from" badge) and its descendants (`rerunNodeIds`, dashed
accent) are highlighted; a ✕ clears the pick. Resume is disabled while the run is busy (`resumeBusy`
— any node running/queued). A resumed run **merges** live status over the persisted snapshot
(`selectedRunTasks`), so skipped `:done` nodes stay on the graph while the re-run section updates
live, rather than the graph collapsing to only the re-run nodes.

The tab badge shows the count of currently-running nodes.

### Node types

| VueFlow type | Julia scope | Visual cue |
|---|---|---|
| `"task"` | `"image"` or `"incremental"` | Purple accent border, solid (image) or dashed (incremental) |
| `"picnic"` | `"set"` | Amber/orange border, ◆ badge, barrier policy shown |
| `"start"` | (not a task) | UML initial node — a filled dot; drag + link to the first task(s). Moveable, source-only |
| `"live"` | (live view only) | Status-colored header bar; grey=queued, blue=running, green=done, red=failed, grey=cancelled |

Custom node components: `ChainTaskNode.vue`, `ChainStartNode.vue`, `ChainPicnicNode.vue`, `ChainLiveNode.vue`.

**Start dot (UML initial node).** A moveable dot (reserved id `__start__`, one per chain) marking where a
run begins — added by the toolbar button and **by default on a new chain** (which then centers/zooms on
it so it's obviously visible). You link it to the task(s) a run should start from; **only tasks reachable
from it run**, the rest stay in the editor as drafts (backend `_prune_to_start`, `docs/SCHEDULER.md`). So
drop it mid-chain to run just the later tasks, or link it to one branch and leave another as a draft. It's
not a task: excluded from `nodes` on save and recorded as `startTargets` (the linked node ids); its
position persists under `positions['__start__']`. No start dot / unlinked ⇒ `startTargets` empty ⇒ run the
whole chain (backward-compatible). The config panel shows only a hint for it (no scope/params).

### Chain JSON format

The whiteboard sends the standard `{name, nodes[], edges[]}` template format plus optional `positions:
{nodeId: {x, y}}` and `startTargets: string[]` (the UML start-dot links) fields. The backend preserves all
fields verbatim (the scheduler ignores unknown fields when loading). Positions are purely a whiteboard
concern; `startTargets` drives which subgraph a run executes (`_prune_to_start`).

### Per-node param form

`ParamRenderer.vue` is the shared param-rendering component (the "DynamicWidget" referenced in design docs). The whiteboard config panel uses it directly — the same component used in `TaskRunner`. Don't build a second param-form implementation for the whiteboard.

### API endpoints

| Method | Path | Purpose |
|---|---|---|
| `GET` | `/api/chains?projectUid=X` | List template names |
| `GET` | `/api/chains/get?projectUid=X&name=Y` | Fetch template JSON |
| `POST` | `/api/chains/save` `{projectUid, template}` | Write template JSON |

### Chain → task store bridge

Chain events flow: `_update_node_state!` (Julia) → `subscribe_chain_events!` subscriber in `server.jl` → `broadcast_ws` → `ws.ts` `chain:node:*` handler → `taskStore.addFromChainEvent(...)`.

The synthetic task ID is `runId::nodeId::imageUid` — stable across updates so the same entry is updated in place. Chain tasks appear in `TaskList` with a purple `pi-sitemap` badge. The rerun button is suppressed for chain tasks (they're driven by `run_chain`, not the task queue).

`addFromChainEvent` stores `label` from `opts.label` (which may be empty — the backend events
don't include a `label` field yet). Fallback is `fn.split('.').pop()`.

**Cancel from TaskList**: when `t.chainRunId` is set, the cancel button sends `chain:cancel {runId}`
over WS and calls `cancelChainRun(runId)` in the task store (which marks all tasks with that
`chainRunId` as `:cancelled`). Without `chainRunId`, the standard `task:cancel {taskId}` path
is used. Tooltip text adjusts: "Stop chain run" vs "Cancel task".

**Cancel status stickiness**: `stores/tasks.ts` `setStatus` guards against overwriting a
user-initiated `'cancelled'` status with any other status. Processes that don't die immediately
and finish naturally won't flip the task back to green.

### Adding a new node to the canvas

Drag from the left palette. On drop, the node is added at the drop position with default param values from the task definition. The node type defaults to `"task"` (image scope); change scope in the config panel to convert to a picnic node.

### REPL ↔ whiteboard round-trip

A chain built in the REPL with `make_chain` / `save_chain_template!` opens on the whiteboard unchanged (nodes positioned in order, left to right). A chain saved from the whiteboard runs correctly with `run_chain(proj, uids; chain="name")` — the extra `positions` field is ignored by the scheduler.

---

## Analysis-plot canvas (summary plots, Observable Plot)

The summary-plot surface — distributions/frequencies of cell & track measures — built on the shared
canvas shell (see *Shared canvas shell*). **Charting library: Observable Plot
(`@observablehq/plot`)** — chosen over Vega-Lite (jitter/resize/look walls) and Plotly (removed); see
`docs/PLOTS.md` §0 for the rationale. All plot data is **server-aggregated** (`POST /api/plot_data`
→ histogram bins / frequency counts / box stats / downsampled raw points), so Vue never receives raw
cells and payloads stay tiny — see `docs/API.md` and `docs/ARCHITECTURE.md` (layer boundary:
aggregation is a PACKAGE function, the route is thin, rendering is frontend-only).

- **`components/plots/PlotChart.vue`** — renders with Observable Plot (lazy-imported). Props: `data`
  (the `/api/plot_data` response) + `opts` (`BuildOpts`); it calls `plots/plot.ts`'s
  `buildPlotOptions(Plot, data, opts)` to get a `Plot.plot()` options object, injects the panel's
  width/height, and appends the node. Resize is trivial (no Vega signal graph): a `ResizeObserver` on
  the host just re-renders with the new size. Exposes `toImageURL('png'|'svg')` — SVG serialises the
  node (native), PNG rasterises it at the DPR-aware `EXPORT_SCALE`. The summaries counterpart of the 2D-canvas dot plots.

> **The Analysis board itself is documented in `docs/ANALYSIS.md`** — tabs, the three stores
> (`analysisTabs` / `analysisLayout` / `canvasPanels`), plate layout, persistence keys, and export. A
> stale summary lived here and had already drifted (it credited `canvasPanels` with the layout and
> called persistence a manual Save; `analysisLayout` owns the layout and autosaves). One owner: that doc.

These canvas components are **generic** (`components/canvas/`, NOT under a module) so every module
page — and the Analysis board — reuses them unchanged:
- **`components/canvas/SummaryPanel.vue`** — one summary plot, wrapping `CanvasPanel`. Layout: the
  **controls row** (`#actions`) holds a **measure dropdown** (from the spec's `measureOptions`) and a
  **chart-type dropdown** (from `chartTypes`, shown when >1); the secondary options — **Split by**
  (groupBy, discovered from obs columns) and the per-chart param (histogram → bins; bar → error metric;
  frequency → proportion) — live in a **⚙ options popover** so the bar never clips at min width. The
  **footer** (`#footer`) holds the utility actions: a **duplicate** button (clones the panel's full
  state so you can change one thing) and the **export** dropdown. Fetches `/api/plot_data`, then passes
  `result` + a `BuildOpts` to `PlotChart`. Chart
  types (by measure type): numeric → `histogram`, `boxplot` (+ beeswarm raw-point overlay that sits on
  the box by construction), `violin` (client-side KDE), `bar` (mean ± selectable SD/SEM/95% CI),
  `strip`/beeswarm; categorical → `frequency`, `stacked`, `stacked100`. An **export** dropdown saves
  the shown plot as **CSV** (the aggregated data, via `plotDataToCsv`), **PNG** or **SVG** (`PlotChart`
  exposes `toImageURL`). Visual properties come from the host via the `vis` prop (`VisProps`). See
  `docs/PLOTS.md`.
- **`components/canvas/SeriesPicker.vue`** — the summary canvas's **read-only** population picker
  (distinct from the gating `PopulationManager`, which is single-tree + mutating). Lists the
  populations available across the selected images, **grouped by segmentation** (`value_name`), from
  `GET /api/plots/populations`. Eye-selecting a population makes it a plot series; because the list
  spans segmentations, populations from **different segmentations** can be overlaid on one plot.
  Selection is keyed by `tkey(valueName, pop)` (`plots/series.ts`). A footer **global/local scope**
  toggle and an **Options** box (log scale, legend, point size/opacity — `VisProps`) both obey that
  scope: global = one value shared by every plot, local = the active plot only (mirrors the gating
  manager's plot-options model).
- **`components/canvas/PopulationPanelShell.vue`** — the **shared chrome** for the floating population
  panels: the draggable/collapsible container + top-right placement (`useFloatingPanel`), the header
  (icon · title · count · collapse), the global/local **scope footer**, and the optional `PlotOptions`
  block (rendered when the host passes a `vis` bag). Both `SeriesPicker` and `PopulationManager` wrap
  it — the differing population LIST is the default slot; host-specific controls (the gating manager's
  gate/viewer options) go in the `#options` slot. Slotted rows keep their own component's scoped CSS;
  the shell owns only the chrome. One place for the chrome → the universal analysis board reuses it.
- **`components/canvas/PlotOptions.vue`** — the **shared** `VisProps` styling controls (collapsible
  Layout / Points / Colours / Labels sub-sections; props `vis`, emits `update:vis`). Embedded by BOTH
  `SeriesPicker` (summary canvas) and `PopulationManager` (gating / cluster canvas), so the styling
  knobs live in ONE place. `PopulationManager` renders it only when the host passes a `vis` bag (the
  cluster canvas does; the gate canvas doesn't) — the "add plot styling to the pop manager" keyword.
  The universal Analysis board (`/analysis`) gets the same controls for free.
- **`plots/export.ts`** — the ONE plot-export module: PNG/SVG rasterise, CSV, and the true-vector SVG
  builders. Two capture paths, because they solve different problems: `elementToImageURL` wraps a
  style-inlined clone in an SVG `<foreignObject>` (catches an HTML overlay legend alongside the `<svg>`),
  while `plotHostToImageURL` composites every `<canvas>` then the overlay on top — canvas pixels can't
  go through `foreignObject`. Two DPR-aware scales: `EXPORT_SCALE` (vector) and the higher `RASTER_SCALE`,
  where every stacked canvas **re-renders its content at export scale** rather than being upscaled, so a
  dot plot exports crisp and cannot clip. Full API + the two subtleties
  that bit us (clearing ancestor backgrounds in the overlay pass; capturing the axis-margin wrapper, not
  the inner plot box) are indexed in `INVENTORY.md` → *Plot export*; board figure export is `docs/ANALYSIS.md`.
- **`plots/overlays.ts`** — the **shared** themed legend / title overlays (`legendOverlay`,
  `titleOverlay`). Canvas plots render a BARE `<svg>` and float the legend/title as absolute overlays
  with the theme ink — Observable Plot's inline `legend: true` wraps the chart in a `<figure>` whose
  swatch legend sits on a white ground (light-grey text → invisible on the dark theme) and eats layout
  height (clips the axis). Used by `PlotChart` AND the cluster HMM panels; the host must be
  `position: relative` and ship the `.plot-legend-overlay` / `.plot-title-overlay` scoped CSS.
- **`components/canvas/SummaryCanvas.vue`** — the workspace (`useCanvasPanels` + `CanvasPanel` +
  `SeriesPicker`). The **"+ Plot" picker** lists plot types from the registry
  (`GET /api/plots/definitions?module=…`). **Series come from the picker's eye-selection** — each is
  a `{valueName, pop}` target, sent to `/api/plot_data` as `series:[…]`. A **"compare" selector**
  (shown when a set is active **and >1 image is selected**) switches the **data source**: *this
  image* / *per image* (one series per selected image) / *pooled* — orthogonal to the chart type, so
  any chart works with any scope. Per-image series are coloured by image (stable palette); else by
  population colour. Has its own populations fetch + selection state (not the gating store), but
  **subscribes to `gating:popmap`** so gate edits (gate page, napari, other clients) live-refresh the
  population list and re-pull the panels' data. Series are keyed by every varying dimension (image ·
  segmentation · pop) so populations sharing a path across segmentations get **separate** boxes/bars
  (no overlap).
- **`modules/BehaviourModule.vue`** — route `/behaviour`, sidebar "Behaviour". Minimal page (full
  HMM/behaviour pipeline later): `ModuleLayout module="behaviourAnalysis"` (**multi-select** —
  unlike gating's single-select — so several images can be compared) + `SummaryCanvas` below the
  table. Doubles as the clean test ground for the canvas. **Comparison plots live here, not in the
  Tracking module** (Tracking hosts the interactive track-gating canvas only).

**Data source ⊥ chart type.** The plot spec defines the **data source** (popType, granularity,
`measureOptions`) and the **chart types valid for it** (`chartTypes`); the user picks the chart type
in the panel and the data scope (single/cross-image) in the canvas. The two compose freely.

**Plot specs** live in `app/src/plotDefinitions/*.json` (one data source per file) and are served like
task defs, so adding a plot type is a JSON drop with no UI code. Schema + the current set:
`docs/PLOTS.md`.

## Gating page (2D-canvas scatter + gate overlay)

`frontend/src/modules/GatingModule.vue` — route `/gate`. Pick ONE image in the table; the
gating workspace renders **below the table** (`#plots` slot, wide left column),
mirroring the old `flowPlotManager` layout. `gate/GatingPlots.vue` is the container:
page-level **segmentation (value_name) select** + a **"+ Plot" button** + **Tile/Cascade** window-
arrange icons (ImageJ-style: grid vs staggered), and a full-height **`.gp-canvas`** workspace
(`min-height: 80vh`) holding **free-floating, draggable, resizable** `gate/GatePlotPanel.vue` boxes
plus the floating `components/canvas/PopulationManager.vue`. Arrange works by pushing an `arrange`
command (`{x,y,w,h,seq}`) to each panel — position is otherwise drag-controlled and size
resize-controlled, so the command sets both imperatively (the `seq` bump forces re-apply). Plots
are an array keyed by stable id (no fixed count); per-plot state (displayed parent, local
highlight) lives in `GatingPlots` keyed by id. State otherwise lives in `stores/gating.ts` (tree,
columns, stats, CRUD, `applyBroadcast` for the `gating:popmap` WS push; `valueName` self-heals to a
real segmentation). API: `docs/API.md` gating routes.

**Track-property gating reuses the SAME canvas (`popType` prop) — no clone.** `GatingPlots` takes a
`popType` prop (`'flow'` default | `'track'`); `TrackingModule.vue` (route `/track`) renders it in
its `#plots` slot as `<GatingPlots :image-uid pop-type="track" />` (active when exactly one
image is selected, alongside the task runner in `#right`). `popType` only changes (a) the data source
the store/API read — flow cells vs the per-track table, handled server-side (`docs/API.md` →
`popType=track`) — and (b) the napari overlay: flow shows the cell-selection brush (linked brushing)
+ Points layers, track shows a **"Tracks"** button (`g.showTracks()` → napari Tracks layers via
`POST /api/napari/show-tracks`). `GatePlotPanel` and `PopulationManager` are shared unchanged; two
small popType-driven touches: panels default the axis transform to **linear** for track (motility is
continuous, not logicle-scaled), and the manager hides its **"Napari dots"** point-size option for
track (tracks are ribbons, not points — `popType` prop). The store gained `cellMeasures` /
`trackAggregates` (track `/channels` fields, for building `{measure}.{agg}` axes) and
`showTracks` / `refreshNapari` (the latter routes the per-pop visibility re-push to Tracks vs Points
by `popType`).

## Interactive vs summary plots

Two plot families share the canvas shell; the distinction matters for where a new plot type plugs in:

- **Summary** — server-aggregated (`POST /api/plot_data`), drawn by the ONE generic `PlotChart`
  (Observable Plot). Histogram, bar, boxplot, **heatmap/matrix**, frequency. Add one = drop a
  plot-def JSON (`app/src/plotDefinitions/`); no UI code. Hosted by `SummaryPanel`.
- **Interactive** — client-side 2D-canvas point clouds with per-point interaction, each with its own
  data endpoint + rendering. Gating scatter, **UMAP**. These can't be a single generic renderer, so
  they live in a **registry** of self-contained view components:
  **`components/canvas/interactiveViews.ts`** → `INTERACTIVE_VIEWS = { umap: { label, component } }`.
  A view (e.g. **`components/plots/UmapView.vue`**) fetches + renders + owns its controls; the generic
  **`components/canvas/InteractivePanel.vue`** wraps any view in `CanvasPanel` and spreads the plot
  `context` (project/images/popType/suffix) + the panel's persisted `state` onto it. **Adding an
  interactive plot = one `XView.vue` + one registry line** — no panel/canvas changes. (Shared infra,
  so the future universal canvas reuses it.)

## Cluster pages (UMAP + heatmap on the shared canvas)

`ClusterCellsModule` (`/clust-cells`, popType `clust`) and `ClusterTracksModule` (`/clust-tracks`,
`trackclust`) — one page per granularity, mirroring the gate/track split. Each is `ModuleLayout`
(multi-select; clustering is set-scope) + `TaskRunner` + a `#plots`-slot `modules/cluster/ClusterPlots.vue`
canvas, the cluster analogue of `GatingPlots` (`useCanvasPanels` keyed `clust:${popType}` + "+ Plot" +
Tile/Cascade). The picker lists every `INTERACTIVE_VIEWS` entry plus the summary **Heatmap**, routed by
family — interactive → `InteractivePanel`, summary → `ClusterHeatmapPanel`.

UI conventions specific to these pages:

- **`suffix` is page-level** — a dropdown of the discovered `clusters.{suffix}` runs, one at a time like
  a segmentation, persisted in the canvas `shared` bag.
- **Heatmap features are exactly what the run clustered on**, read from the `{props}.clustfeatures.json`
  sidecar via `GET /api/gating/channels`; channel rows aggregate by RAW name and relabel via `nameMap`.
- **The population manager is the shared, pop_type-agnostic one.** A cluster pop has no gate — it is a
  filter on `clusters.{suffix}` — so in cluster mode the manager shows "Add population" plus per-pop
  **cluster-ID toggle chips**, and ticking a chip moves that cluster out of any other pop (a cluster
  lives in at most one). Writes mirror across the run's images (`mirrorUids`); a banner names selected
  images outside the run, with a "Select clustered images" button driving `selectUids`.
- **Highlight → overlays.** The manager's per-pop eye feeds `shownPops`: UMAP recolours from cached codes
  (no refetch), the heatmap switches its columns from clusters to populations. Scope (global/local) works
  as on the gating canvas.
- **HMM behaviour plots (track clustering only)** — `ClusterHmmStatesPanel` (100%-stacked state
  frequencies) and `ClusterHmmTransitionsPanel` (from→to dot grid). Categorical behaviour, so they are
  filtered out of the heatmap's numeric features.

The clustering model behind all of this — run membership (`partOf`), co-clustered value_names, the
per-run sidecar, set-pooled aggregation — is `docs/POPULATION.md` and `docs/todo/CLUSTERING_PLAN.md`.


---

## Shared canvas shell

Reused by the gating, track-gating, summary and universal canvases — the floating-panel mechanics are
factored out of the gating page so every module canvas reuses them unchanged:
- **`composables/useFloatingPanel.ts`** — drag-to-move + clamp-to-`offsetParent` + Tile/Cascade
  `arrange` handling for any floating panel (one implementation; was duplicated in the plot panel
  and the manager).
- **`components/canvas/CanvasPanel.vue`** — the generic panel chrome, stacked in rows: a **title row**
  (the whole row is the drag handle, like `PopulationManager`; holds title + collapse + remove —
  buttons `@mousedown.stop` so they don't drag), an optional **controls row** (`#actions` slot, which
  `flex-wrap`s so it never clips at min width), the **body** (default slot), and an optional **footer
  row** (`#footer` slot, for utility actions). `resize:both`, active border. `GatePlotPanel` and
  `SummaryPanel` wrap their content in it.
- **`composables/useCanvasPanels.ts`** — the workspace logic: the panels array (`{id, arrange,
  state}` with host-owned per-panel `state`), `add`/`remove`/`arrangeGrid`/`arrangeCascade`, the
  active panel, and a per-canvas **`shared`** bag for canvas-level options. **Takes a `key`** (e.g.
  `summary:behaviourAnalysis`, `gate:flow`); everything lives in the **`canvasPanels` store** under
  that key, so open plots **persist across navigation** (re-binds the same panels instead of starting
  empty). Cleared on project open/close. ⚠️ **Seed default panels only when the canvas is empty**
  (`if (panels.value.length === 0) add()`) — an unconditional `add()` in `onMounted` stacks duplicates
  every remount (the Gate↔Tracking 2→4→6 bug).

---

## Persisting view state — the three scopes (important; read before adding any plot option)

Every user-settable option MUST live in a persisted bag, or it silently resets on remount (a plain
`ref()` in a canvas/panel component does NOT survive navigation). There are three scopes, all backed
by the `canvasPanels` store and keyed per canvas.

**The canvas key is per-image (module pages).** Module-page canvases embed the active object in their
key — `summary:{module}:{imageUid}`, `gate:{popType}:{imageUid}:{valueName}` (per segmentation too),
`clust:{popType}:{setUid}` (clustering is set-scope). `useCanvasPanels` takes a **reactive** key
(Ref/getter) and rebinds to that object's own entry when the selection changes — so each image keeps
its own plots/selections instead of the old single shared-per-module entry being pruned. Add
`imageUid` (or set/value_name) to a NEW canvas's key the same way. The `/analysis` board keeps its own
`analysis:{projectUid}:tab:{id}` key (persisted separately — see below).

**Persistence is per-image AND survives reload** (debounced autosave). The store groups the
module-page entries (`summary:`/`gate:`/`clust:` keys) BY OBJECT and writes each with its object at
**`{proj}/1/{objUid}/moduleCanvases.json`** (like `ccid.json`/`labelProps` — locality, and it's
removed when the object is deleted), ~400 ms after any change (off the interaction path — no
perceptible lag). The object is the image (summary/gate) or set (clust) the canvas is scoped to,
parsed from the canvas key's 3rd segment. `api_projects_load` reassembles the per-object files into
one keyed map; `projectMeta.openProject` restores it after `loadFromApi`'s clear. The board still
persists separately to `settings/analysisBoards.json` (manual Save). Nothing to wire per page.

The three scopes:

1. **Per-panel** (chart type, measure, bins, error metric, …) → the panel's own `state` object
   (`CanvasItem.state`). `SummaryPanel` receives it as the `ui` prop and reads/writes it via computed
   get/set; each field falls back to the spec default until the user changes it.
2. **Per-canvas / global-scope** (the global selection, vis props, compare mode, scope toggle;
   gating's highlight set, line width, …) → the per-canvas **`shared`** bag.
3. **Geometry** (drag position + size) → the `geom` record, keyed `${canvasKey}:${panelId}`
   (`CanvasPanel` writes it on drag/resize; restored on mount).

**The mechanism: `composables/useViewState.ts` (Shiny-`reactiveValues`-style).** Pass it the `shared`
bag `Ref` + a `defaults` literal; it seeds missing keys and returns one ref per option, so **every
option declared in `defaults` persists automatically — there is nothing to wire per-field**. The
convention is therefore forget-proof: *put every option in the `defaults` object*; that single step is
all that's needed. Do **not** introduce a bare `ref()` for a user option in a canvas component. The
returned refs track the bag's **identity**, so when the per-image key rebinds `shared` to another
image's entry, global-scope state follows the image too (no remount / per-page code needed).

```ts
const { compareMode, scope, sel: gSel, vis: gVis } = useViewState(shared, {
  compareMode: 'image' as 'image' | 'per_image' | 'summarised',
  scope: 'global' as 'global' | 'local',
  sel: [] as string[],
  vis: defaultVis() as VisProps,
})   // each is a Ref backed by the persisted bag; setting .value persists across navigation
```
Used by `SummaryCanvas` and `GatingPlots`. In-memory/session-scoped (survives in-app navigation, not
a hard browser reload — same as the panels); cleared on project open/close.
- **`components/canvas/PopulationManager.vue`** — the shared, pop_type-agnostic manager (renders
  whatever `g.popType` the store holds — flow/live/clust; not flow-only). Plot-options (gate labels,
  line width, axis) are passed in by the host canvas since they belong to the plot panels.

Each **`GatePlotPanel`** is `position:absolute`, **dragged by its title** (clamped on-screen like
the manager) and **resized from its corner** (`resize:both`; the plot area is `flex:1` and the
canvas layers re-render via `ResizeObserver`). Self-contained (own X/Y column + transform
on **stacked rows**, parent-population select, **render mode**, gate mode) with a **"−"** in the
header to remove it. New gates are added under that panel's selected parent population. Click a
panel to make it **active** (orange border); the active panel follows the population you select in
the manager (sets it as the displayed parent).

Plot stack — two superimposed 2D canvases, both mapping data→pixel through the same `viewExtents`
so they stay aligned (`xMin`→left, `xMax`→right, `yMax`→top). There is no third (WebGL) layer any more:
- **`components/plots/PlotLayers.vue`** — the base. In `points` mode it draws every cell coloured by
  its local density; in `contour`/`outliers` mode it draws d3-contour rings (plus the sparse tail).
  Also draws the **population-colour overlay** (per-pop dots or contours). Bucketing points by colour
  keeps `fillStyle` writes to ~64 rather than one per point.
- **`components/plots/GateOverlay.vue`** — canvas2D (top). **Draws** new **rectangle** (drag)
  and **polygon** (click vertices, double-click/click-near-start to close; Esc cancels) gates,
  and **edits** existing ones: move / resize rectangles (corner + edge handles), drag polygon
  vertices, double-click an edge to insert a vertex, right-click a vertex to delete. Live local
  redraw while dragging; persists (`pop/set-gate`) only on release. Emits `draw`/`edit` only on
  explicit user completion — programmatic repaint never emits, so no re-entrancy loop (the old
  Plotly `flowNumGateUpdates` guard is unneeded).

**Render modes** (mirror old `cciaConf fcs.gating.plotTypes`): `points` = FlowJo *pseudocolour*
(density-coloured points); `contour` = density contours over faint points. Highlighting
populations in their colours (the **eye** in the manager) overlays on top of either mode.

Workflow: pick X/Y columns + per-axis transform (linear/log/asinh/logicle) → click a parent
population in the manager (sets it as the active plot's parent) → draw a gate → name it → it's
POSTed (`pop/add`), recomputed server-side, and appears in the manager with count + %-of-parent.
Edit a gate by dragging its handles → `pop/set-gate` on release. The manager (draggable,
clamped on-screen, collapsible) does recolour (`pop/update`), inline rename (`pop/rename`),
delete (`pop/delete`, cascades), and per-plot colour **highlight** (see below).

### Gating plot — rendering & UX hacks

Moved to **`docs/POPULATION.md`** → *Gating plot — rendering & UX hacks*: the client-side density and
contour maths, gate hit-testing without stealing pointer events, and cross-plot propagation. Those are
gating-model internals rather than UI conventions. **Read them before touching `PlotLayers` /
`GateOverlay`.**

## Auto-overridden settings — never silent

When the app cannot honour a chosen option and substitutes another, it says so. Silently substituting
leaves the user looking at a plot that disagrees with its own controls, with no way to tell whether the
setting is broken or the data made it impossible.

One mechanism — `frontend/src/plots/autoOverride.ts`:

1. Build an `AutoOverride` (`{ setting, from, to, why }`) **where the substitution is decided** — nothing
   downstream knows the reason.
2. Mark the affected control with the shared **`.cc-auto-override`** utility (amber, `style.css`).
3. Use **`overrideTooltip(o, fallback)`** for its hover text, so the explanation can't be left out. Where
   there is no single control to mark, `overrideNote([...])` gives a one-line footer for the panel.

Today's overrides:

| Where | Substitution | Decided by |
|---|---|---|
| Gate plot / gate pairs | axis transform → `linear` when the measure's range can't take logicle | the server (`plotmeta` reports the transform it USED) |
| Any summary plot | x tick labels → rotated when they wouldn't fit their bands | `needsXRotation` (measured label widths vs the panel width) |

This replaced two ad-hoc copies. `GatePlotPanel` and `GatePairsPanel` each did their own
preferred-vs-used comparison with their own amber class and their own wording — and `GatePlotPanel`'s
transform select was tooltipped just "Axis transform", so the amber announced that *something* had
happened without ever saying what. A third case (auto-rotation) was the point at which a third variant
stopped being acceptable.

**A marked control SHOWS the effective value and WRITES the preference** (`effectiveOf`). This is the
half that's easy to miss: an ambered control still displaying the value that was *not* used reads as
"your setting is being ignored". The gating transform selects have always done it — the select's getter
reads the transform the server USED, its setter writes the user's preference — and the rotate toggle sat
at *off* beside a rotated plot until it did the same. The control is then effectively stuck while the
override holds, which is correct (the plot really is rotated) and lifts on its own when the cause does:
a wider panel, shorter labels, a compatible measure. In **Global** vis scope the amber reflects the
ACTIVE plot while the toggle governs every plot — same convention as the stats-test readout beside it.

**Mark the CONTROL, not just the plot.** The notice reaches two places, and both matter: the affected
control in the population picker goes amber with `overrideTooltip` (so the toggle never sits at *off*
beside a rotated plot), and the panel shows a short footer note. Both read the same
`PlotReadout` — `{ stats, overrides }`, threaded as **one object** through
`SummaryPanel → host → SeriesPicker → PopulationPanelShell → PlotOptions`. Parallel props are how the
first attempt failed: the override was emitted and the toggle never heard about it.

**A panel notice belongs in the panel CHROME.** `.sp-body` is `overflow: hidden` with a `height: 100%`
chart in it, so a sibling rendered after the chart is pushed out of view — the first version of these
notes was emitted correctly and simply never seen. Put them in the `#footer` slot.

**Auto-rotation is a decision, not a guess.** Each of `n` categories gets an equal band of the plotting
area, so a label wider than its band must collide with its neighbour; `needsXRotation` measures the
widest label with the same canvas text metric the axis margins use. It needs the panel width, which the
option builders don't have — `PlotChart` passes `plotWidth` (the same value it hands `Plot.plot`), and
the builder reports the outcome back on `_autoRotatedX` → `@auto-override` → the panel's note.
