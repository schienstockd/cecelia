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

## Phases (priority order — biggest divergence / highest bug-risk first)

- [x] **Phase 0 — Toggles.** `CcToggle` created; all immediate-option checkboxes migrated; multi-select
  lists kept as native checkboxes. (PR #341.)
- [x] **Phase 1 — Buttons.** Deleted 8 scoped `.btn-*` blocks (`PhysicalSizeDialog`, `ProjectPanel`,
  `SetBar`, `FileBrowser`, `ImageTable`, `MetadataPanel`, `GatingCopyDialog`, `AnimationModule`);
  migrated all markup to `.cc-btn` + modifier; added canonical `.cc-btn-danger` (solid) and
  `.cc-btn-danger-ghost` (subtle) to `style.css`, resolving the two drifted danger schemes. Also fixed
  the Movies refresh button (raw browser button — used the non-global `.btn-sm`).
- [ ] **Phase 2 — Range sliders.** ~26 raw `<input type="range">` across ~12 files, each with its own
  label/value/width markup (`.pt-slider`, `.cz-range`, `.anim-range`, `.movie-range`, `.po-row` ranges,
  …). Add a single-value `CcRange` (label + value readout + track); `RangeSlider` stays the dual-thumb
  specialist. High count; inconsistent value/step display.
- [ ] **Phase 3 — Empty states.** ~23 distinct scoped `*-empty` classes, no component (richest is
  `ImageTable`'s `.empty-state` — icon + title + hint + CTA). Add `<EmptyState>` (icon/title/hint +
  optional CTA slot). High visual divergence, low bug-risk.
- [ ] **Phase 4 — Collapsible section headers.** `CollapsibleSection` exists but ~15 chevron+heading
  toggles are hand-rolled (`PlotOptions` ×4 `.po-toggle`, `MetadataPanel`, `ParamRenderer`,
  `PopulationManager`, `CanvasPanel`, `TaskList`, `ErrorConsole`, `AppSidebar` nav groups). Migrate onto
  `CollapsibleSection` or a shared `.cc-section-toggle`.
- [ ] **Phase 5 — Status colours / badges.** Duplicated status→icon maps (`TasksModule`, `TaskList`,
  `ChainLiveNode`) and traffic-light colours not on `--cc-sev-*` (already flagged in `style.css`). Route
  through `severity.ts`; optional `<CcBadge>` for counts. Correctness/CVD-safety relevance.
- [ ] **Phase 6 — Inline spinners.** 40+ inline `pi-spin` busy icons, two spellings (`pi-cog` vs
  `pi-spinner`). Standardize the busy-icon idiom (or a tiny `<CcSpinner>`). Low bug-risk, high count.
- [ ] **Phase 7 — Card/panel chrome.** 200+ repeated surface+border+radius blocks; no `.cc-card` util.
  Add `.cc-card`/`.cc-surface`. Cosmetic, largest raw duplication, lowest priority.
- [ ] **Phase 8 (optional) — Icon-only buttons.** ~90 bespoke `*-btn`/`*-ico` classes; mostly
  intentional (different sizes, hover-reveal, viewer-green accents). Optional `.cc-icon-btn` base +
  per-component sizing; not the same trap as `.btn-*`.

**Healthy / no action:** Modals (`BaseModal`), popovers (`TeleportPopover`), tabs (`TabbedCanvas`),
chips (`ChipSelect`), colour dropdown (`SwatchSelect`), toggles (`CcToggle`).

## Convention going forward

Each unified primitive gets a one-liner in `docs/UI.md` (the *when to use which*) and a line in
`INVENTORY.md` (the *where it lives*). A new hand-rolled copy of a primitive that has a canonical form
is a bug, caught in review — same reflex as the H5AD/zarr/`run_py` single-helper rules in `CLAUDE.md`.
