# Vue — the viewer boundary

> **Generated** by `scripts/architecture/vue_extract.py` — do not edit; run `pixi run arch-diagrams`.


The viewer is `lib/webgpu`, `utils/viewer`, `components/viewerWindow`. Below: what it imports from the rest of the app
(the less, the more the viewer is a self-contained engine), and who imports it.

```mermaid
flowchart LR
  subgraph V["viewer"]
    components_viewerWindow["components/viewerWindow<br/><small>14 files</small>"]
    lib_webgpu["lib/webgpu<br/><small>15 files</small>"]
    utils_viewer["utils/viewer<br/><small>22 files</small>"]
  end
  utils_viewer -->|17| utils__flat_["utils (flat)"]
  lib_webgpu -->|14| utils__flat_["utils (flat)"]
  components_viewerWindow -->|12| utils__flat_["utils (flat)"]
  components_viewerWindow -->|9| components__flat_["components (flat)"]
  components_viewerWindow -->|6| composables["composables"]
  utils_viewer -->|6| stores["stores"]
  components_viewerWindow -->|3| plots["plots"]
  components_viewerWindow -->|3| stores["stores"]
  components_viewerWindow -->|2| lib__flat_["lib (flat)"]
  components_viewerWindow -->|1| components__other_["components (other)"]
  utils_viewer -->|1| lib__flat_["lib (flat)"]
  utils_viewer -->|1| plots["plots"]
  modules["modules"] -.->|39| V
  composables["composables"] -.->|7| V
  stores["stores"] -.->|5| V
  components__other_["components (other)"] -.->|3| V
  components_plots["components/plots"] -.->|3| V
  components__flat_["components (flat)"] -.->|2| V
  plots["plots"] -.->|2| V
  utils__flat_["utils (flat)"] -.->|2| V
  app_root["app root"] -.->|1| V
  components_canvas["components/canvas"] -.->|1| V
  tasks["tasks"] -.->|1| V
```

Solid: the viewer imports from outside (n files). Dotted: outside imports the viewer (n imports).

## Viewer → app stores

Each store the viewer reaches into — the coupling a self-contained viewer would replace with inputs.

| Viewer group | Stores imported |
|---|---|
| components/viewerWindow | `observer`, `settings`, `viewer` |
| lib/webgpu | — |
| utils/viewer | `linkedSelection`, `log`, `project`, `projectMeta`, `settings`, `viewer` |
