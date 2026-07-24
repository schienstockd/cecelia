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
| Collapsible section | `components/CollapsibleSection.vue` | ~7 consumers; **but ~15 hand-rolled chevron toggles bypass it** (see Phase 4). |
| Range (dual-thumb) | `components/RangeSlider.vue` | min+max only; **no single-value wrapper yet** (see Phase 2). |
| Plot-area spinner | `components/plots/PlotSpinner.vue` | plot area only; inline busy spinners are ad-hoc (see Phase 6). |
| Severity colours | `lib/severity.ts` + `--cc-sev-*` | status colours should route through this. |

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

Done — the correctness item:
- [x] **Status/severity colours.** `lib/taskStatus.ts` (`TASK_STATUS`) — the ONE 5-state status map;
  `done`/`failed` route through the CVD-safe `--cc-sev-*`, `running` = new `--cc-active`, queued/cancelled
  neutral. Replaced the 3 drifted per-file maps (`TasksModule`, `TaskList`, `ChainLiveNode`);
  `ParamRenderer`'s QC flag already used `lib/severity.ts`. (PR #TBD)

Remaining — incremental adoption only (no forced sweeps):
- [ ] **Collapsible section headers.** `CollapsibleSection` exists but ~15 chevron+heading toggles
  bypass it (`PlotOptions` ×4, `MetadataPanel`, `ParamRenderer`, `PopulationManager`, …). Migrate
  opportunistically, or extract `.cc-section-toggle`.
- [ ] **Opportunistic muted-text / card / readout adoption.** Replace scoped `.*-empty` / `.*-val` /
  subtitle / surface blocks with the semantic utils as files are touched — NOT a dedicated sweep.
- [ ] **Not recommended as sweeps:** single-value range wrapper (base already accent-themed; readout now
  covered by `.cc-readout`; sliders are layout-entangled and some commit on release) and icon-only
  buttons (~90, mostly intentional — different sizes/hover-reveal/viewer-green). Governed by the rule.

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

**Roadblock — value readouts (`.cc-readout`) and eyebrows (`.cc-eyebrow`) NOT swept.** Their font sizes
are context-tuned (0.6–0.78rem: dense inline readouts, tiny node labels) and the neighbouring widths are
tuned to those sizes; the coarse 3-step scale (`--cc-fs-xs/sm`) would enlarge them and drift the layout.
Some readouts are intentionally accent-coloured (`.pt-val`) or `--cc-text` (not dim). So `.cc-readout` /
`.cc-eyebrow` are used only where the size already matches (e.g. `MoviesModule` zoom). Adopt the rest
opportunistically per-file where the values line up — not a sweep.

**Roadblock — card/surface (`.cc-card`) NOT swept.** ~29 panel/card blocks with radii spread 0.2–0.5rem
and mixed surface-1/2; normalising all to `--cc-radius-md` + surface-1 would visibly change many. Use
`.cc-card` for new containers; migrate existing ones only when a file is already being touched.

**Roadblocks — deliberately kept bespoke (do NOT force onto the utils):**
- **Rich empty state** — `ImageTable` `.empty-state` (+ `.empty-icon/-title/-hint/-cta`): a full icon +
  title + hint + CTA block with generous padding. `.cc-empty` is the plain column; a rich `<EmptyState>`
  variant could wrap it later, but it's the one genuinely rich case — leave it.
- **Absolute-positioned overlay empty** — `UmapView` `.uv-empty` (`position:absolute; inset:0`): not a
  flow block; `.cc-empty` doesn't apply.
- **Tiny / italic micro-empties** — `ChainQcNode` `.qc-empty` (9px italic), `PopulationManager`
  `.pm-chip-empty` (10px italic), `CropPanel`/`ChainQcNode` italic hints: sub-`--cc-fs-xs` sizes +
  italic are intentional dense-context chrome; `.cc-muted` (0.75rem, non-italic) would enlarge them.
- **`HintCallout.vue`** is a full component (icon + dismiss), not a text scenario — leave it.
- **`ChainModule`** empties/hints (`.live-empty`, `.canvas-empty`, `.no-chains-hint`, palette hints):
  several are richer (icons, multi-line CTAs) inside the whiteboard; adopt opportunistically, not swept.

## Convention going forward

Each canonical primitive/utility gets a one-liner in `docs/UI.md` (the *when to use which*, in the
catalog) and a line in `INVENTORY.md` (the *where it lives*). A new hand-rolled copy of a primitive or
scenario that already has a canonical form is a bug, caught in review — same reflex as the
H5AD/zarr/`run_py` single-helper rules in `CLAUDE.md`.
