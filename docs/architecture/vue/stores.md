# Vue — stores

> **Generated** by `scripts/architecture/vue_extract.py` — do not edit; run `pixi run arch-diagrams`.


The 36 Pinia stores in `frontend/src/stores/`. Arrow `a → b`: store a imports store b.

**Base stores** — imported by 5 or more other stores: `log`, `project`, `projectMeta`, `settings`.
Arrows into them are not drawn (they would hide everything else); each box says which base stores it
uses instead. Box also shows how many files import the store.

```mermaid
flowchart LR
  subgraph BASE["base stores"]
    log["log<br/><small>48 importers</small>"]
    project["project<br/><small>73 importers</small>"]
    projectMeta["projectMeta<br/><small>60 importers</small>"]
    settings["settings<br/><small>63 importers</small>"]
  end
  analysisLayout["analysisLayout<br/><small>5 importers</small><br/><small>uses log, projectMeta</small>"]
  analysisTabs["analysisTabs<br/><small>6 importers</small>"]
  animation["animation<br/><small>3 importers</small>"]
  appControl["appControl<br/><small>22 importers</small><br/><small>uses settings</small>"]
  blackboard["blackboard<br/><small>1 importers</small>"]
  canvasPanelExports["canvasPanelExports<br/><small>3 importers</small><br/><small>uses projectMeta</small>"]
  canvasPanels["canvasPanels<br/><small>7 importers</small><br/><small>uses projectMeta</small>"]
  captureReshow["captureReshow<br/><small>5 importers</small>"]
  correctionCockpit["correctionCockpit<br/><small>2 importers</small>"]
  customModules["customModules<br/><small>6 importers</small>"]
  gating["gating<br/><small>7 importers</small><br/><small>uses log, project, projectMeta</small>"]
  guide["guide<br/><small>7 importers</small><br/><small>uses project, projectMeta</small>"]
  guideMirror["guideMirror<br/><small>2 importers</small>"]
  kiwi["kiwi<br/><small>12 importers</small><br/><small>uses projectMeta, settings</small>"]
  labCapture["labCapture<br/><small>4 importers</small><br/><small>uses projectMeta, settings</small>"]
  labelOpsQueue["labelOpsQueue<br/><small>1 importers</small>"]
  layoutFold["layoutFold<br/><small>4 importers</small><br/><small>uses settings</small>"]
  linkedSelection["linkedSelection<br/><small>6 importers</small>"]
  observer["observer<br/><small>25 importers</small>"]
  paramHandoff["paramHandoff<br/><small>2 importers</small>"]
  plotRegistry["plotRegistry<br/><small>4 importers</small><br/><small>uses projectMeta</small>"]
  push["push<br/><small>7 importers</small>"]
  shareTarget["shareTarget<br/><small>3 importers</small>"]
  staleOutputs["staleOutputs<br/><small>2 importers</small><br/><small>uses project, projectMeta</small>"]
  taskDefs["taskDefs<br/><small>13 importers</small>"]
  taskDrafts["taskDrafts<br/><small>1 importers</small>"]
  taskPreview["taskPreview<br/><small>1 importers</small><br/><small>uses log</small>"]
  tasks["tasks<br/><small>25 importers</small>"]
  trackOpsQueue["trackOpsQueue<br/><small>2 importers</small>"]
  viewProfiles["viewProfiles<br/><small>5 importers</small><br/><small>uses settings</small>"]
  viewer["viewer<br/><small>39 importers</small>"]
  ws["ws<br/><small>30 importers</small><br/><small>uses log, project, projectMeta</small>"]
  analysisLayout --> analysisTabs
  analysisLayout --> ws
  canvasPanelExports --> ws
  gating --> ws
  guide --> appControl
  guide --> customModules
  guide --> layoutFold
  guide --> observer
  guide --> tasks
  guide --> viewProfiles
  kiwi --> observer
  kiwi --> ws
  project --> analysisLayout
  project --> analysisTabs
  project --> canvasPanels
  project --> settings
  projectMeta --> analysisLayout
  projectMeta --> analysisTabs
  projectMeta --> animation
  projectMeta --> canvasPanels
  projectMeta --> log
  projectMeta --> project
  settings --> appControl
  taskPreview --> viewer
  tasks --> appControl
  ws --> appControl
  ws --> blackboard
  ws --> labCapture
  ws --> linkedSelection
  ws --> push
  ws --> taskDefs
  ws --> tasks
  ws --> viewer
```

## Who imports each store

| Store | Importers | By group |
|---|---|---|
| `project` | 73 | components (flat) 22, modules 16, components/plots 9, composables 8, stores 5, components/canvas 4, utils (flat) 3, components (other) 2, tasks 2, app root 1, lib (flat) 1 |
| `settings` | 63 | components (flat) 15, modules 10, components/viewerWindow 9, composables 8, stores 6, components/canvas 5, components (other) 3, tasks 3, app root 2, components/plots 1, utils (flat) 1 |
| `projectMeta` | 60 | modules 19, components (flat) 17, stores 10, composables 4, components/canvas 3, tasks 3, components (other) 2, app root 1, utils (flat) 1 |
| `log` | 48 | modules 16, components (flat) 13, stores 5, app root 2, components/canvas 2, composables 2, tasks 2, utils (flat) 2, utils/viewer 2, components/plots 1, lib (flat) 1 |
| `viewer` | 39 | modules 11, components/plots 9, composables 5, components (flat) 4, utils/viewer 3, stores 2, app root 1, components (other) 1, components/canvas 1, components/viewerWindow 1, tasks 1 |
| `ws` | 30 | components (flat) 8, composables 6, modules 6, stores 4, tasks 2, utils (flat) 2, app root 1, lib (flat) 1 |
| `observer` | 25 | components (flat) 13, components (other) 4, components/canvas 2, composables 2, stores 2, app root 1, components/viewerWindow 1 |
| `tasks` | 25 | components (flat) 8, modules 5, tasks 3, utils (flat) 3, composables 2, stores 2, app root 1, lib (flat) 1 |
| `appControl` | 22 | components (flat) 9, stores 4, modules 3, app root 2, composables 2, components (other) 1, lib (flat) 1 |
| `taskDefs` | 13 | components (flat) 6, composables 2, modules 2, components (other) 1, stores 1, tasks 1 |
| `kiwi` | 12 | components (flat) 3, components (other) 3, composables 3, components/canvas 2, modules 1 |
| `canvasPanels` | 7 | components/canvas 2, composables 2, stores 2, modules 1 |
| `gating` | 7 | modules 4, components/canvas 2, components (flat) 1 |
| `guide` | 7 | components (flat) 6, app root 1 |
| `push` | 7 | composables 3, components (other) 1, components/plots 1, modules 1, stores 1 |
| `analysisTabs` | 6 | stores 3, components/canvas 1, modules 1, utils (flat) 1 |
| `customModules` | 6 | components (flat) 4, modules 1, stores 1 |
| `linkedSelection` | 6 | app root 1, components/canvas 1, composables 1, modules 1, stores 1, utils (flat) 1 |
| `analysisLayout` | 5 | components/canvas 2, stores 2, modules 1 |
| `captureReshow` | 5 | components/canvas 2, modules 2, composables 1 |
| `viewProfiles` | 5 | components (flat) 4, stores 1 |
| `labCapture` | 4 | components (flat) 2, app root 1, stores 1 |
| `layoutFold` | 4 | components (flat) 3, stores 1 |
| `plotRegistry` | 4 | composables 2, components (other) 1, components/canvas 1 |
| `animation` | 3 | modules 2, stores 1 |
| `canvasPanelExports` | 3 | composables 2, components/canvas 1 |
| `shareTarget` | 3 | composables 2, components (other) 1 |
| `correctionCockpit` | 2 | components (other) 1, components/plots 1 |
| `guideMirror` | 2 | app root 1, components (flat) 1 |
| `paramHandoff` | 2 | modules 1, tasks 1 |
| `staleOutputs` | 2 | components (flat) 1, tasks 1 |
| `trackOpsQueue` | 2 | components (other) 1, components/plots 1 |
| `blackboard` | 1 | stores 1 |
| `labelOpsQueue` | 1 | components (other) 1 |
| `taskDrafts` | 1 | tasks 1 |
| `taskPreview` | 1 | components (flat) 1 |
