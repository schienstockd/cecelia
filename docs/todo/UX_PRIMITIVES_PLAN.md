# UX primitives — unification tracker

**Purpose.** A living checklist of the app's UX primitives (buttons, toggles, sliders, empty states,
…) recording, for each: whether a **single canonical shared component/util** exists, where
**divergent hand-rolled copies** still live, and the unification status. This exists because the
"looks shared but is re-declared per file, and drifts" pattern (the [divergent re-implementation
trap](../../CLAUDE.md)) keeps recurring — a checkbox styled as a toggle in one place and a native box
in another; a `.btn-sm` that renders as a raw browser button on a page that forgot to copy the CSS.
The rule for every entry: **one canonical thing; the second copy is the bug.**

**How to use it.** Before adding a boolean/button/slider/empty-state/etc., check the "canonical" column
and use it. When you unify a category, tick it here and add the rule to `docs/UI.md`.

**Get counts from the detectors, never from a grep.** Every number in this doc that came from grepping
class names turned out to be wrong, in both directions: "~80 remaining scenarios" (most muted text has no
`hint`/`sub`/`val` in its class name), "~29 card blocks" (mostly *controls*, wanting `.cc-btn`/`ChipSelect`),
"~15 chevron toggles" (six weren't sections), "48 ad-hoc spinners" (a non-issue), "~90 icon buttons, mostly
intentional" (2 shapes × 4 sizes spelled 60 ways). The font-size audit counted only `rem` and missed 149
`px` declarations of the same scale. Run `pixi run test-frontend` — `utils/cssScenarios.ts` and
`utils/cssTokens.ts` are the authority, and the ratchet baseline in `cssScenarios.test.ts` is the live
backlog.

### Why this doc kept changing between sessions (2026-07-25) — and the rule that fixes it

Three fresh sessions each re-audited this area and each produced a slightly different picture. That is
not "the nature of CSS". Measured against the record, **the picture was stable everywhere a committed
detector held the number, and drifted everywhere prose held it.** The `BASELINE`, the `ALLOWED` list and
the `SEG_BUTTONS` list are assertions — they cannot go stale without failing the suite. The *Remaining*
section was prose, and every one of its claims that a session re-checked turned out wrong:

| Prose claim | Measured |
|---|---|
| "three **byte-identical** `.seg` copies" | **five** copies — two of them **dead CSS** with no markup left, and the live three had **drifted** (two spell `.on` as accent-text-on-surface-1, three as a solid accent fill) |
| "should be `ChipSelect` … a component swap (`v-model`)" | none of them is a select — they are strips of independent actions (tile/cascade, prev/next). There is no value to bind, so it was a **class** swap all along |
| "**~10** chevron toggles bypass it: … `ModuleLayout` ×2 …" | **8**, in 4 files. `ModuleLayout`'s two are chevron-**up/down** dropdown carets on popover triggers, not sections — the same species of miscount this doc already corrected once for six other sites |

There is a real CSS-specific cause underneath, worth stating because it bounds what any future audit can
know: **CSS cannot express intent.** A button, a card, a chip, a badge and an input are all `surface +
1px border + radius`; that is why the `card` matcher had to be dropped, and why `.cc-btn-icon` collapsed
the full-height strips — markup *and* CSS together still could not encode "this one stretches". Any
measurement that reads the stylesheet infers role from a shadow of it, and a grep over class names is
worse again. So the number must come from a detector, and where a detector cannot be precise, the
category gets a review-time rule and **no number at all**.

**The rule, going forward: this doc states no counts.** Every open category below points at the
committed detector that owns its number. Add a category → add a detector or an explicitly pinned list.
A number written in prose here is stale the moment someone touches a file, and re-deriving it by grep is
how four sessions produced four pictures.

## Canonical components/utilities (the baseline — use these)

| Primitive | Canonical | Notes |
|-----------|-----------|-------|
| On/off toggle (option) | `components/CcToggle.vue` | `v-model`, `label` prop/slot, `disabled`. Rule: toggle = one immediate option; checkbox = selection from a list. |
| Chips / segmented select | `components/ChipSelect.vue` | pill/segmented, single/multi, reorderable. **Gotcha:** an option with `label: ''` renders its span-less — that's how icon-only chips work, so an empty label *and* no icon gives a blank chip. Pass something visible (the attribute filter shows `—` for an unset value). |
| Colour dropdown | `components/SwatchSelect.vue` | |
| Confirm / delete | `components/ConfirmButton.vue`, `ConfirmDeleteButton.vue` | arm→confirm; some dialogs still inline their own confirm flow. |
| Buttons | `.cc-btn` + `-primary`/`-ghost`/`-bare`/`-danger`/`-danger-ghost` (`style.css`) | never hand-roll `.btn-*` in scoped CSS. `-bare` = transparent/dim-until-hover (the commonest); `-ghost` is its boxed counterpart. |
| Icon-only button | `.cc-btn` + `-bare`\|`-ghost` + `-icon` (+ `-micro`/`-dense`/`-lg`) | a fixed **square**, so toolbar rows align regardless of glyph width. Guarded: `utils/cssScenarios.test.ts` fails on an icon-only `<button>` not built from `.cc-btn`. **Exception:** a button with a width and NO height *stretches* (a full-height edge strip / tab-strip cell) and is NOT this primitive — see *Icon-only buttons* below. |
| Engaged/pressed button | `.cc-btn-on` (+ `-on-tint` / `-on-solid`) | the button vocabulary's **fourth axis** (tone / density / icon / state), and the one that was missing. Intensity is the axis. The `:hover` rules double the class on purpose — a scoped `.foo:hover` weighs (0,3,0) and would repaint the engaged state on mouseover. |
| Joined button strip | `.cc-btn-group` + ordinary `.cc-btn` children | the group owns the outline and the hairline dividers, so children use the `-bare` tone. **Not** `ChipSelect` — that's a select; this is a row of independent actions. |
| Section-toggle row | `.cc-section-toggle` | the chevron+heading ROW without `CollapsibleSection`'s panel-bar chrome, for a sub-section inside a popover or param form. `CollapsibleSection` composes it (the reference adoption). |
| Modal / dialog | `components/BaseModal.vue` | backdrop + panel + close; ~9 consumers. |
| Popover / dropdown | `components/TeleportPopover.vue` | ~11 consumers. |
| Tabs | `components/canvas/TabbedCanvas.vue` | + `ChipSelect` segmented. |
| Collapsible section | `components/CollapsibleSection.vue` | ~7 consumers; **but ~10 hand-rolled chevron toggles bypass it** (see *Remaining*). |
| Range (dual-thumb) | `components/RangeSlider.vue` | min+max only; no single-value wrapper (not recommended — see *Remaining*). |
| Plot-area spinner | `components/plots/PlotSpinner.vue` | plot area only. Inline busy spinners were long listed here as "ad-hoc" — **measured, and they aren't**: of 48 `pi-spin` usages only 13 carry a styled class, all token-sized or inheriting, and the rest are the shared `<i class="pi pi-spin pi-spinner" />` idiom inline in a button. Nothing to unify. |
| Severity colours | `lib/severity.ts` + `--cc-sev-*` | status colours should route through this. |
| Task/chain status | `lib/taskStatus.ts` (`TASK_STATUS`) | the ONE 5-state status→icon/colour map. |
| Semantic text/surface scenarios | `.cc-muted` · `.cc-empty` · `.cc-readout` · `.cc-eyebrow` · `.cc-card` (+ one modifier axis each) | full catalog in `docs/UI.md`. |
| Design tokens | `--cc-fs-3xs/2xs/xs/sm/md/lg`, `--cc-radius-xs/sm/md/lg/pill`, colours in `style.css` | **three** guards: `cssTokens.test.ts` fails on a reference to an undeclared custom property (all `--*`, not just `--cc-*`; an inline `:style` declaration counts as valid); `cssScenarios.test.ts` fails on a literal `font-size`/`border-radius` — in scoped CSS **or** an inline `style=` (exempt: display type >15px, pill radii, `0`, `em`); and `findRawColours` fails on a hex literal that a token already holds exactly, **or** any `var(--x, #hex)` fallback. |

## Approach — generalise by SCENARIO, not one component per widget

The first sweep unified two **components** where the divergence was a real bug (buttons rendered
unstyled; a checkbox-as-toggle was the wrong affordance). But the audit's remaining "phases" were
ranked by raw count, and count ≠ value: the rest is progressively cosmetic. More importantly, they are
**not distinct problems** — they collapse into a handful of recurring **scenarios (semantic roles)**
that repeat across many different elements. E.g. a prominent pool-size count and a dim slider `°/s/×`
readout look different but are the SAME scenario ("a value readout"), differing only in *prominence*.

So the generalisation grain is the scenario → a **semantic token / utility class** (with variant
modifiers), NOT a `CcRange`/`CcEmptyState`/`CcCard`/… per widget. Adoption is **incremental**: define
the vocabulary, make the lookup mandatory (so new code uses it), and migrate existing sites
opportunistically. **Do not** force a 300-site sweep, and do not chase the cosmetic tail.

Done — components (real bugs):
- [x] **Toggles** → `CcToggle`; all immediate-option checkboxes migrated, multi-select lists kept native. (PR #341)
- [x] **Buttons** → global `.cc-btn` + `-primary/-ghost/-danger/-danger-ghost`; 8 scoped `.btn-*` blocks deleted, drifted danger schemes resolved. (PR #343)

Done — the mandatory lookup:
- [x] `CLAUDE.md` discovery clause + `docs/UI.md` "check before building" catalog + `INVENTORY.md` pointer, so new divergence can't accrue unreviewed. (PR #345)

Done — semantic-role vocabulary (the generalisation; adopt incrementally):
- [x] Tokens `--cc-radius-sm/md`, `--cc-fs-xs/sm/md`; utilities `.cc-muted` (secondary text),
  `.cc-empty` (empty state), `.cc-readout` + `.cc-readout-strong` (value readout), `.cc-eyebrow`
  (section label), `.cc-card` (surface). Seeded in `MoviesModule` + `AnimationModule` as the reference
  adoptions. This scenario vocabulary replaces the would-be Phase 2/3/6/7 components — the ~23 `*-empty`
  classes, the slider readouts, the subtitles/hints, and the card chrome all compose from it.
- [x] **One modifier axis per scenario** — density (`-dense`/`-micro` + `--cc-fs-2xs/3xs`), surface
  (`.cc-card-2`), layout (`.cc-empty-inline/-overlay/-lg`). This is what unblocked the stalled
  adoption sweep; see *Re-read of the roadblocks* below for why the axes were the whole problem.
- [x] **Dead-token guard** — `utils/cssTokens.ts` + test: referencing a `--cc-*` token that `style.css`
  doesn't declare now fails the suite (it was silently freezing hard-coded greys and, once, killing a
  `<select>`'s fill *and* caret).

Done — the correctness item:
- [x] **Status/severity colours.** `lib/taskStatus.ts` (`TASK_STATUS`) — the ONE 5-state status map;
  `done`/`failed` route through the CVD-safe `--cc-sev-*`, `running` = new `--cc-active`, queued/cancelled
  neutral. Replaced the 3 drifted per-file maps (`TasksModule`, `TaskList`, `ChainLiveNode`);
  `ParamRenderer`'s QC flag already used `lib/severity.ts`. (PR #TBD)

Done — the button state axis + the strips + the section row (2026-07-25):
- [x] **`.cc-btn-on` — the missing fourth axis.** `.cc-btn` could say what a button looked like (tone,
  density, icon) but not that it was **pressed**, so every toggle in the app hand-rolled a `.on`/`.active`
  rule. Measured: 46 state rules across 24 files, ~17 of them on buttons, in three intensities — solid
  accent (6 spellings; `ChipSelect`'s `.chip.on` was already exactly this), outline (~9, several
  hard-coding `#7c3aed` for the token), and a violet tint (3, two of them byte-identical in `ViewerPanel`
  and `PopulationManager`). Same failure as the text utilities — *a utility that hard-codes a value on an
  axis where sites legitimately differ* — which is why `.seg` and the icon-button leftovers could not be
  finished before it existed. Intensity is the axis: `.cc-btn-on` / `-on-tint` / `-on-solid`.
  Two things worth carrying forward: the `:hover` rules **must** double the class, because a scoped
  `.foo:hover` weighs (0,3,0) once Vue adds `[data-v]` and would otherwise repaint an engaged button back
  to its resting colours (`ViewerPanel`'s `.opt-btn:hover` did exactly that); and a hover-reveal rule
  keyed on the old state class (`.row-act.active { opacity: 1 }`) has to be re-keyed to `.cc-btn-on` or
  the engaged control silently becomes invisible until hover.
- [x] **`.cc-btn-group` — the `.seg` strips.** Five copies, not three: two were **dead CSS** (deleted),
  and the live three had drifted. They were never `ChipSelect` candidates (see the table above), so this
  was a class swap. Unpinned from `SEG_BUTTONS`, which is now just the two full-height strip exemptions.
- [x] **`.cc-section-toggle` — the 8 section headers** (`PlotOptions` ×4, `ParamRenderer` ×2,
  `MetadataPanel`, `PopulationManager`). They could not adopt `CollapsibleSection` because it is a
  panel-rail *bar* (its own surface, generous padding, a scrolling body) and these are bare inline rows in
  popovers and param forms — the chrome was the axis they differed on. So the ROW was extracted and the
  component now composes it. Three of the adopters also turned out to be hand-rolled eyebrows and took
  `.cc-eyebrow` (weight and tracking normalise — the one intended visual change).
- [x] **Colours tokenised + ratcheted.** Colour was the last scale with no guard at all: 67 hex literals
  in scoped CSS exactly duplicated a token (16 × `#a78bfa`, which *is* `--cc-accent`), plus 33 dead
  `var(--token, #hex)` fallbacks that misreported the rendered value. New `--cc-accent-strong/-soft/-tint`
  tokens for the violet family the engaged states are built from, and `findRawColours` + test to hold it.
  Severity-*meaning* warn/danger routed to the CVD-safe `--cc-sev-*`; see *the status-vs-not split* below.

Remaining — incremental adoption only (no forced sweeps). **No counts here on purpose — the detector owns them:**
- [ ] **The scenario backlog** (muted text / empty / readout / eyebrow / card re-declarations). The live
  list is the per-file `BASELINE` in `utils/cssScenarios.test.ts`; it may shrink and must never grow.
  Touch a file in it → migrate its rules and lower the number. This is deliberately decoupled from
  "finish the sweep": new divergence fails immediately, the backlog drains as files get touched.
- [ ] **Per-row disclosure in a list** (`TaskList`, `ErrorConsole`) — distinct from a section header, and
  deliberately NOT extracted yet. Inspected, the two sites differ on **four** axes: `TaskList` uses a
  focusable `<button>` (already `.cc-btn-bare .cc-btn-icon`) with a tooltip, the icon as click target and
  multi-expand (a `Set`); `ErrorConsole` uses a non-focusable `<span>`, the whole row as click target and
  single-expand. **Four differences from two samples** — you discover an axis by watching sites differ on
  it, and at n=2 a real axis is indistinguishable from an accident of those two files. The cost is
  asymmetric: extract now, both adopt, then site three differs on the un-modelled axis and hand-rolls,
  leaving a primitive *and* divergence — strictly worse than divergence alone, and precisely the history
  this doc records. Waiting costs two small scoped rules that no detector flags. There may also be
  nothing to extract: one is already the canonical icon button, the other isn't a button at all, and the
  only shared thing is "the chevron points up when open" — one line, needing no abstraction.
- [x] **Form-control density — done (2026-07-25).** The `input`/`select`/`textarea` base was a SINGLE
  size, and that turned out to be the *mechanism* of the divergence, not a side effect: to make a
  control smaller you had to write a class, and once writing a class sites re-typed everything they
  could see. **67 declarations across 19 files restated the base's own `color`/`border`/`background`
  verbatim** — no-ops by the cascade, so removing them is provably neutral.
  Font-size was never the broken axis (50 uses, 5 values, all already tokens), and padding's 23
  spellings were not 23 choices: sorted, they collapse to two tiers below the base, because padding
  **tracks** the size rather than varying independently. So `.cc-input-dense` / `.cc-input-micro` set
  both, exactly as `.cc-btn-icon` bundles box and glyph size. `BatchMoviesPanel`'s `.bm-note` — the
  field that surfaced all this by rendering a tier too large — is the reference adoption.
  Held by `findRestatedInputBase` as an exact list, one survivor: `.cby-swatch`, whose class is shared
  with a `<span>` that gets nothing from the input base and so genuinely needs its own border.

  Two detector-precision bugs found while building it, both now unit-tested, and both the *same shape*
  as the blind spots above — a pattern matching more (or less) than the thing it names:
  `\b(select)\b` matches inside `.chip-select` and `.select-flagged-btn`, because a hyphen is a word
  boundary; and a rule must be judged by its **subject** compound, since
  `.cc-toggle-input:checked ~ .cc-toggle-track` styles the track, not the input. The first run of the
  check reported 77 hits across 22 files; after both fixes, 68 across 19. Nine were never real.

Not swept, and NOT because they were forgotten: `ChainPicnicNode`'s amber and `AnimationModule`'s
`.tl-badge` are **identity** hues (a node's colour, a keyframe badge), and the `.cc-del`/`.footer-btn
.danger`/`.save-btn.danger`/`.opt-btn.danger` reds are **destructive-action tones**. Neither is a status,
so both correctly stay on `--cc-warn`/`--cc-danger` rather than `--cc-sev-*`.

### Icon-only buttons — done (2026-07-25)

Long parked here as *"~90, mostly intentional — different sizes/hover-reveal/viewer-green"*. Measured,
that was half wrong: **116 icon-only buttons carrying 60 distinct class names, but only TWO shapes**
(boxed `surface+border`, 45; bare `transparent`, 50) **and four size tiers**. Colour was not an axis at
all — 96 of 99 resolvable base rules were `--cc-text-dim`; the danger/viewer tones live in modifier
classes. So the divergence was 60 spellings of 2×4.

Canonical form hangs off the existing button vocabulary rather than a new family, since `.cc-btn-ghost`
already had exactly the right colour behaviour and 4 sites already spelled it that way:

```
<button class="cc-btn cc-btn-bare  cc-btn-icon">              bare
<button class="cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense"> boxed, dense
```

- `.cc-btn-bare` — transparent, no border, dim until hover. The app's most common button.
- `.cc-btn-icon` — a fixed **square** (`1.5rem`), so toolbar rows align regardless of glyph width. This
  is the reason it's a modifier and not per-site padding: 48 sites had independently discovered they
  needed a fixed box, at **nine** different sizes.
- Size steps `-micro`/`-dense`/`-lg` on the same density axis as the text scale (measured tiers).

**103 sites across 35 files migrated**, 45 bespoke classes collapsed (−415 lines). Hover-reveal
(`opacity: 0` + a parent `:hover` rule) and one-off tones are preserved as scoped CSS — they were the
genuinely intentional part. `findHandRolledIconButtons` + its test now fail on any icon-only `<button>`
not built from `.cc-btn`, with the ten `.seg` buttons pinned by path.

**Regression, found in the GUI (not by any check): full-height strip controls.** `ModuleLayout`
`.right-handle` (the right-panel collapse strip) and `TabbedCanvas` `.tab-add` (the "+" cell in the tab
strip) are markup-identical to an icon-only button — one `<i>`, nothing else — but their rules set a
**width and no height**, so they stretch as a flex child to fill the panel edge. `.cc-btn-icon`'s fixed
square collapsed both to a chip at the top. Both restored to their own rules and exempted by name in
the test. **Markup cannot distinguish these**, so the rule to carry forward is: if a `<button>` has no
height of its own and relies on stretching, it is not a square icon button — check the geometry, not
just the contents. The class name said so (`-handle`) and I noted it and moved on anyway.

Three further defects in my own migration script, all caught by inspecting output rather than by the
green tests: an anchored selector regex silently skipped 11 classes whose rule was preceded by a comment; the
splice consumed the leading whitespace **and any preceding comment**, gluing rules onto one line; and
the button scan matched `<button class="footer-btn">` inside `ConfirmButton`'s usage-example doc
comment. Also resolved: three surviving `.foo .pi { font-size }` child rules that would have overridden
the primitive's size — they render identically but weren't actually unified.

**Healthy / no action:** Modals (`BaseModal`), popovers (`TeleportPopover`), tabs (`TabbedCanvas`),
chips (`ChipSelect`), colour dropdown (`SwatchSelect`).

## Adoption sweep — log (in progress)

Migrating existing sites onto the semantic vocabulary, scenario by scenario. Roadblocks are noted and
skipped (kept bespoke), not forced.

**Done — muted text / subtitles / simple empties → `.cc-muted`:** `NotebooksModule` (`.nb-sub`/`.nb-hint`),
`SetupModule` (`.setup-sub`), `LegacyMigrateDialog` (`.lm-sub`), `GatingCopyDialog` (`.cg-empty`),
`FileBrowser` (`.fb-empty`), `NotebookTable` (`.nbt-empty`), `SummaryCanvas` (`.sc-empty`), `LayoutCanvas`
(`.lc-empty`), `GatingPlots` (`.gp-empty`), `SeriesPicker`/`PopulationManager` (`.pm-empty`), `UmapView`
(`.uv-pop-empty`), `TaskRunner` (`.defs-empty-msg`) — plus `MoviesModule`/`AnimationModule` (seeded
earlier). This also **fixed several dead `--cc-text-muted` references** (an undefined token that silently
fell back to `#888` instead of the real `--cc-text-dim`).

### Re-read of the roadblocks (2026-07-25) — they were one problem, and it's fixed

Every roadblock above had the same cause: **the utility hard-coded a value on an axis where sites
legitimately differ**, so adopting it forced a visual change, so the site stayed hand-rolled and got
written down as "bespoke". There were only three such axes. Giving each scenario its missing axis as a
modifier turned the whole list into mechanical, visually-neutral migrations:

| Axis | Was baked in | Now |
|---|---|---|
| Density | one font size per util | `.cc-muted/-readout/-eyebrow` + `-dense` (≈10px) / `-micro` (≈9px) |
| Surface | `.cc-card` = surface-1 | `.cc-card` (border+radius) + `.cc-card-2` (raised) |
| Layout | `.cc-empty` = padded centred column | + `-inline` / `-overlay` / `-lg` |

Two measurement errors had kept the picture wrong:

1. **The size audit only counted `rem`.** There are ~430 `rem` *and* ~149 `px` font-size declarations,
   and they are the same scale in two spellings: `12px` **is** `--cc-fs-sm`, `11px` **is** `--cc-fs-xs`,
   and `10px`/`9px` are two tiers *below the old floor*. So the "tiny/italic micro-empties" were never
   exceptions — they were exactly the two missing steps. Added `--cc-fs-2xs` / `--cc-fs-3xs`.
   The tell: `CollapsibleSection` — the *canonical* component — hand-rolled its own eyebrow (uppercase +
   600 + 0.06em + dim) at `0.65rem`, below `--cc-fs-xs`. The reference implementation could not adopt the
   vocabulary it was meant to model. It does now.
2. **The card count conflated cards with controls.** Of the "~29 panel/card blocks", nearly all are small
   *controls* (search wraps, mini/gear buttons, tabs, pills, segmented rows, native inputs) whose canonical
   form is the control — `.cc-btn`, `ChipSelect`, the global `select`/`input` base — not `.cc-card`. The
   genuine card population is a handful. Radius was never the blocker either: `0.3rem`→`0.25rem` is **0.8px**.
   Surface was, and `.cc-card-2` covers it. **There is no card sweep to do** — the leftovers are the
   icon/mini-button question the entry below already parks.

Migrated on the back of that (each visually equivalent unless noted):
`ChainQcNode` `.qc-empty` · `PopulationManager` `.pm-chip-empty` · `UmapView` `.uv-empty` (overlay) +
`.uv-pop-head` (eyebrow; now 600-weight, the one intended change) · `ImageTable` `.empty-state` (rich) ·
`GatePlotPanel` `.gate-empty` (inline empty *wearing card chrome* — the composite case) ·
`CollapsibleSection` `.cs-label` · `NotebooksModule` `.nb-empty` · `ClaudeOverviewDialog` `.co-entry`
(`.cc-card-2`) · `CropPanel` `.crop-hint` · `CopyDialog` `.copy-hint`.

**Still deliberately bespoke:**
- **`HintCallout.vue`** is a full component (icon + dismiss), not a text scenario — leave it.
- **`ChainModule`** empties/hints (`.live-empty`, `.canvas-empty`, `.no-chains-hint`, palette hints):
  several are richer (icons, multi-line CTAs) inside the whiteboard; adopt opportunistically, not swept.
- **Accent-coloured readouts** (`.pt-val` and friends) — a readout that is *deliberately* accent or
  `--cc-text` rather than dim. Prominence, not density; `.cc-readout-strong` covers the common case.
- **Per-site italic and geometry.** `font-style: italic` marks provisional/advisory prose in ~20 places;
  it is emphasis, not a tier, and stays a one-line scoped rule (as do width/margin/flex/padding).

### Correctness find — dead design tokens (the actually-broken thing)

Measuring the above turned up **16 references to 4 tokens `style.css` never declares**. An undeclared
`--cc-*` doesn't warn: the declaration is invalid at computed-value time and drops to `unset`.
- 13 × `--cc-text-muted` — the `, #888` fallback *masked* it, freezing a hard-coded grey instead of
  `--cc-text-dim` `#7d8590`. (This is why the earlier sweep only caught "several": the fallbacks hid them.)
- 2 × `--cc-font-mono` (the token is `--cc-mono`) — silently fell back to bare `monospace`.
- 1 × `--cc-surface-3` — never existed.
- 2 × fallback-less `background: var(--cc-surface)` in `LabLogPanel`. Worst case: on a `<select>` the
  invalid **shorthand** dropped `background-image` too, killing the global custom caret — that dropdown
  rendered transparent *and* arrowless.

All fixed, and `frontend/src/utils/cssTokens.ts` + `cssTokens.test.ts` now fail the suite on any
reference to an undeclared custom property (`vite.config.ts` sets `test.css: true`, without which Vitest
stubs `style.css` to `''` and the guard would pass vacuously). **Add the token, never a fallback.**
The guard was later widened from `--cc-*` to **all** `--*` properties — and immediately found a 17th
site, `SpatialContactHeatmap`'s `var(--text-muted, #888)`, which had been hiding behind the prefix
filter. Component-local declarations count as valid, including inline `:style="{ '--foo': … }"` for
dynamic values (`--gate-font`, `--sk`, `--sep-thick`, `--pct` are all legitimately set that way).

### Colour — the last unratcheted scale, and the status-vs-not split (2026-07-25)

The sizes-and-radii sweep tokenised two scales and stopped. Colour had **no** guard: `cssTokens.ts`
catches referencing a token that doesn't exist, but nothing caught never referencing one — so 67 hex
literals sat in scoped CSS holding a value a token already held *exactly*, `#a78bfa` sixteen times when
`--cc-accent` **is** `#a78bfa`. Alongside them, 33 `var(--token, #hex)` fallbacks: all provably dead
(`cssTokens.test.ts` proves every referenced token is declared), all misreporting the real colour to the
next reader — `var(--cc-accent, #a855f7)` reads as if accent were a different purple.

`findRawColours` is deliberately the **narrow** check, the same precision-over-recall call that killed the
`card` matcher. Only ~67 of ~330 raw hex declarations were flagged, because most raw hex is a genuine
one-off (chart series, chain-node hues) and *nothing in the stylesheet distinguishes those from a system
colour*. An exact match to a token is not a judgement call, so the check has zero false positives and
needs no allow-list to rot. `#fff`/`#000` are exempt — not a scale, and what `.cc-btn-primary` uses.

**The status-vs-not split.** `style.css` says `--cc-sev-*` supersedes `--cc-warn`/`--cc-danger` for
severity semantics, and the values genuinely differ (`#fab219` vs `#f59e0b`, `#d03b3b` vs `#ef4444`), so a
hard-coded `#ef4444` on a *status* indicator silently opted out of the CVD-validated palette. The rule
applied, now in `docs/UI.md`: if the colour states **what condition something is in** (valid/invalid,
fresh/stale, ok/warn/fail) it is a severity → `--cc-sev-*`. If it is a destructive **action**'s tone or a
decorative/identity hue, it is not → `--cc-warn`/`--cc-danger`. That reading is why `.viewer-stale`,
`.field-input.warn`, `.input-error`, `.ax-warn`, `.name-invalid`, `.svc-pill.warn` and the `ErrorConsole`
level dots moved, and why the delete-button reds and the chain-node ambers did not. This is the one part
of the sweep that is a **visible** change, and it is a judgement call per site — it is written down here
rather than left implicit precisely so it can be argued with.

### Enforcement — the ratchet (2026-07-25)

The mandatory-lookup clause in `CLAUDE.md` and the catalog in `docs/UI.md` are review-time discipline,
and this project's history *is* the record of that discipline failing across fresh context windows. So
the scenarios are now machine-checked: `utils/cssScenarios.ts` + `cssScenarios.test.ts` detect a scoped
rule that spells out a utility's defining declarations, and hold a **per-file baseline that may shrink
and must never grow**. ~130 rules remain in 45 files; new divergence fails immediately, and the backlog
drains as files get touched. This decouples "stop the drift" from "finish the sweep" — which is what had
stalled the whole exercise.

Precision was chosen over recall, deliberately, because the allow-list a noisy check would force is
where this kind of thing rots. Three matchers were dropped or tightened during calibration:
- **`card` — dropped entirely.** `surface + 1px border + radius` is the shape of a card, an input, a
  chip, a badge *and* an icon-button; ~60% of matches wanted `.cc-btn`/`ChipSelect`/the global input
  base, not `.cc-card`. Nothing in scoped CSS distinguishes them. Review-time rule only.
- **Controls excluded from the text matchers.** A dim colour + a size also describes every ghost/icon
  button (`.action-btn`, `.icon-btn`, `.ra-btn`, …), whose canonical form is `.cc-btn-ghost`.
- **`empty` narrowed to rules that re-declare the colour.** `.foo-empty p { margin: 0; font-size: 0.8rem }`
  is styling its own contents, not re-implementing the scenario.

Calibration took the count from 310 → 262 → 155 → 130. **The first three figures were wrong**, and so
was the "~80" estimated from class-name greps (`*-hint`/`*-sub`/`*-val`) — most muted text has no such
word in its class name. That is the fourth miscount in this saga; the lesson is that every count here
should come from a committed detector, not a grep.

**A detector can have blind spots too, and they share one shape.** Stripping the dead colour fallbacks
made the ratchet jump by six rules in four chain-node files. Not new divergence — the `muted` matcher keys
on `color: var(--cc-text-dim)` and never matched the `color: var(--cc-text-dim, #8b8ca7)` spelling, so it
had been blind to them all along. That is the **third** blind spot of exactly this shape (the others: the
detector only reading `<style>` blocks, so inline `style="font-size:…"` was invisible; and the `muted`
matcher keying on a *literal* size, so tokenising a rule would have silently un-flagged it). All three have
one cause: **a matcher pinned to one spelling of a value the codebase writes more than one way.** When
adding a matcher, ask which other spellings of the same declaration exist — token vs literal, with
fallback vs without, scoped vs inline.

Those six are **migrated** (all four files back to zero), and doing it surfaced one more missing step on
the density axis: `.cc-muted` always had `-dense` *and* `-micro`, `.cc-eyebrow` only `-dense`. The 9px
uppercase labels in the whiteboard nodes (start / scope / the QC footer) therefore had nowhere to land —
the same "utility hard-codes a value on an axis where sites differ" trap, one step further down. Added
`.cc-eyebrow-micro`, so the two text scenarios now carry the same three density steps. Also routed the
five raw `font-family: monospace` stacks through `--cc-mono` while in those files.

### Raw sizes and radii — done, and it was never churn

This was written off twice as "churn: touches every file for no behavioural gain". That was wrong, and
the reasoning was inconsistent: the argument for adding `--cc-fs-2xs/3xs` was *"the scale should have the
steps the app actually uses"*, and it simply wasn't applied to the middle of the scale. Worse, the ratchet
had been built with the largest category carved out of it, so those values would have kept growing.

Measuring settled it — **33 distinct font-size spellings, of which 98% (541/553) were already within
0.5px of an existing token.** 33 spellings of 5 values. The "it's a visual change" objection was about
sub-pixel shifts. So both scales were derived from the distribution and everything was tokenised:

- `--cc-fs-lg: 0.9rem` added (a real 9-use cluster one step above body).
- Radius scale went 2 steps → 5: `xs/sm/md/lg/pill`, and **`--cc-radius-sm` was retuned `0.25rem`→`0.3rem`**
  because 4.8px is the modal radius (33 uses) — that halved the worst-case shift from 1.6px to 0.8px. Only
  one site consumed the old value.
- **~770 declarations tokenised** across 83 files. `findRawValues` + its test now fail on any literal.
- Exempt by rule: display type (>15px), pill radii, `0`, and `em` (container-relative by design —
  `ViewLegend` scales with the export). One inline-documented exception: `ChainQcNode` `.qc-bar`'s 1px
  radius, where the 3px token would round a 4px-wide bar into a blob.

Two blind spots surfaced while finishing, both now covered: the detector and the sweep only looked inside
`<style>` blocks, so four **inline `style="font-size:…"`** icon sizes were invisible; and the `muted`
matcher keyed on a *literal* size, so tokenising a rule would have silently un-flagged it —
`color: dim` + `font-size: var(--cc-fs-sm)` is still `.cc-muted` spelled longhand.

**Residual risk (needs eyes in a browser).** ~139 declarations shifted by ~0.48px, which is up to **4%
relative** at these sizes. Absolute shift is imperceptible, but a +4% label in a tightly-fitted fixed-width
element could newly wrap or ellipsize. Most likely spots: the chain whiteboard node footers (8px → 8.96px,
in a 120px-min node) and the dense table/readout rows.

Migrated on top of the earlier batch: the **three byte-identical overlay-empty triples** in
`ClusterHeatmapPanel` / `ClusterHmmStatesPanel` / `ClusterHmmTransitionsPanel` (the same rule as
`UmapView`'s, copy-pasted four times) · `ClusterHeatmapPanel` `.feat-empty` · `ClusterPlots` `.cp-empty` ·
`SpatialContactHeatmap` `.ch-empty`/`.ch-meta` · and the **six** hand-rolled eyebrows in `ChainModule`
(sizes 0.6/0.65/0.68rem, weights 600/700, tracking 0.06/0.07/0.08 — all one role). Those four files are
now fully clean. The eyebrow unification is the one visible change: weight and tracking normalise.

## Convention going forward

Each canonical primitive/utility gets a one-liner in `docs/UI.md` (the *when to use which*, in the
catalog) and a line in `INVENTORY.md` (the *where it lives*). A new hand-rolled copy of a primitive or
scenario that already has a canonical form is a bug, caught in review — same reflex as the
H5AD/zarr/`run_py` single-helper rules in `CLAUDE.md`.
