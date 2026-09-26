> Part of the [Inventory index](../../INVENTORY.md). Add new shared components to the matching area file, and keep the index's one-liner accurate.

# Plots + analysis-board panels (`frontend/src/plots/`, `frontend/src/components/canvas/`, `frontend/src/modules/cluster/`)

- **Adding ANY plot — register it, don't hand-roll a panel or route.** The full reference is [`docs/PLOTS.md`](../PLOTS.md) (52 KB — slice it): chart types, encoding model, renderer spec, `SummaryCanvas` host, `INTERACTIVE_VIEWS` / `CLUSTER_PANELS` registration surfaces. Never a bespoke panel component or a bespoke `/plots/…` route.
- **`INTERACTIVE_VIEWS`** (`frontend/src/components/canvas/interactiveViews.ts`) — the registry for board-managed views (UMAP, gating strategy, filmstrip, track scheme, per-image panels, cards). One entry per view kind; `InteractivePanel.vue` reads from it. `interactiveViews.test.ts` enforces the shape.
- **`CLUSTER_PANELS`** (`frontend/src/modules/cluster/clusterPanels.ts`) — the registry for cluster-family panels (`cellCards`, plus siblings). `CardsPanelBase.vue` reads from it. Separate from `INTERACTIVE_VIEWS` because cluster panels ride a different rail (`docs/todo/BEHAVIOUR_CARDS_PLAN.md` covers the family design).
- **Board wiring lives in [`docs/ANALYSIS.md`](../ANALYSIS.md)** — analysis-board tabs, plate + plot-family registries, PDF/CSV export, `boardGroup`/`rail` semantics. Read alongside `docs/PLOTS.md` when adding a plot that belongs on the analysis board.
