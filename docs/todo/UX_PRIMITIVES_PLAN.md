# UX primitives — unification tracker

**Purpose.** A living checklist of the app's UX primitives (buttons, toggles, sliders, empty states,
…) recording, for each: whether a **single canonical shared component/util** exists, where
**divergent hand-rolled copies** still live, and the unification status. This exists because the
"looks shared but is re-declared per file, and drifts" pattern (the [divergent re-implementation
trap](../../CLAUDE.md)) keeps recurring — a checkbox styled as a toggle in one place and a native box
in another; a `.btn-sm` that renders as a raw browser button on a page that forgot to copy the CSS.
The rule for every entry: **one canonical thing; the second copy is the bug.**

**How to use it.** Before adding a boolean/button/slider/empty-state/etc., check the "canonical" column
and use it. When you unify a category, tick it here and add the rule to `docs/UI.md`. Snapshot from the
2026-07 audit (counts are approximate and drift as files change — re-grep before starting a phase).

## Canonical components/utilities (the baseline — use these)

| Primitive | Canonical | Notes |
|-----------|-----------|-------|
| On/off toggle (option) | `components/CcToggle.vue` | `v-model`, `label` prop/slot, `disabled`. Rule: toggle = one immediate option; checkbox = selection from a list. |
| Chips / segmented select | `components/ChipSelect.vue` | pill/segmented, single/multi, reorderable. |
| Colour dropdown | `components/SwatchSelect.vue` | |
| Confirm / delete | `components/ConfirmButton.vue`, `ConfirmDeleteButton.vue` | arm→confirm; some dialogs still inline their own confirm flow. |
| Buttons | `.cc-btn` + `-primary`/`-ghost`/`-danger`/`-danger-ghost` (`style.css`) | never hand-roll `.btn-*` in scoped CSS. |
| Modal / dialog | `components/BaseModal.vue` | backdrop + panel + close; ~9 consumers. |
| Popover / dropdown | `components/TeleportPopover.vue` | ~11 consumers. |
| Tabs | `components/canvas/TabbedCanvas.vue` | + `ChipSelect` segmented. |
| Collapsible section | `components/CollapsibleSection.vue` | ~7 consumers; **but ~10 hand-rolled chevron toggles bypass it** (see *Remaining*). |
| Range (dual-thumb) | `components/RangeSlider.vue` | min+max only; no single-value wrapper (not recommended — see *Remaining*). |
| Plot-area spinner | `components/plots/PlotSpinner.vue` | plot area only. Inline busy spinners were long listed here as "ad-hoc" — **measured, and they aren't**: of 48 `pi-spin` usages only 13 carry a styled class, all token-sized or inheriting, and the rest are the shared `<i class="pi pi-spin pi-spinner" />` idiom inline in a button. Nothing to unify. |
| Severity colours | `lib/severity.ts` + `--cc-sev-*` | status colours should route through this. |
| Task/chain status | `lib/taskStatus.ts` (`TASK_STATUS`) | the ONE 5-state status→icon/colour map. |
| Semantic text/surface scenarios | `.cc-muted` · `.cc-empty` · `.cc-readout` · `.cc-eyebrow` · `.cc-card` (+ one modifier axis each) | full catalog in `docs/UI.md`. |
| Design tokens | `--cc-fs-3xs…md`, `--cc-radius-sm/md`, colours in `style.css` | guarded: `utils/cssTokens.test.ts` fails on an undeclared `--cc-*`. |

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

Remaining — incremental adoption only (no forced sweeps):
- [ ] **Collapsible section headers.** `CollapsibleSection` exists but **~10** chevron+heading toggles
  bypass it: `PlotOptions` ×4, `ParamRenderer` ×2, `ModuleLayout` ×2, `MetadataPanel`, `PopulationManager`.
  Migrate opportunistically, or extract `.cc-section-toggle`. (Recount: the earlier "~15" swept in six
  sites that are **not** sections and should stay as they are — `AnimationModule`'s two chevron-left/right
  *reorder* buttons, the dropdown carets in `SwatchSelect`/`GatePairsPanel`, and the whole-panel collapse
  in `CanvasPanel`/`PopulationPanelShell`. A seventh scenario is genuinely distinct and recurring:
  **per-row disclosure** in a list — `TaskList`, `ErrorConsole` — worth naming if a third site appears.)
- [ ] **Opportunistic muted-text / card / readout adoption.** Replace scoped `.*-empty` / `.*-val` /
  subtitle / surface blocks with the semantic utils as files are touched — NOT a dedicated sweep.
- [ ] **Not recommended as a sweep:** single-value range wrapper (base already accent-themed; readout now
  covered by `.cc-readout`; sliders are layout-entangled and some commit on release). Governed by the rule.
- [ ] **`.seg` segmented controls** — three **byte-identical** hand-rolled copies (`SummaryCanvas`,
  `ClusterPlots`, `GatingPlots`) of a `.seg button {…}` block that should be `ChipSelect`. Surfaced by
  the icon-button check and allow-listed there, because it's a component swap (`v-model`), not a class
  swap. The only remaining hand-rolled buttons in the app.

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
