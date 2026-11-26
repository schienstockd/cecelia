# Vue — layers and who imports whom

> **Generated** by `scripts/architecture/vue_extract.py` — do not edit; run `pixi run arch-diagrams`.


Groups are the directories under `frontend/src/` (rule in the script header). `A → B (n)`: n import
statements from files in A to files in B. **A size map:** big directories import more, so the thick
arrows show where the code is. The strongest 30 arrows are drawn; the full matrix is below.

```mermaid
flowchart LR
  app_root["app root<br/><small>3 files · 0.5k lines</small>"]
  components__flat_["components (flat)<br/><small>134 files · 26.5k lines</small>"]
  components__other_["components (other)<br/><small>10 files · 2.5k lines</small>"]
  components_canvas["components/canvas<br/><small>24 files · 7.9k lines</small>"]
  components_lightPath["components/lightPath<br/><small>6 files · 0.3k lines</small>"]
  components_plots["components/plots<br/><small>33 files · 10.2k lines</small>"]
  components_viewerWindow["components/viewerWindow<br/><small>14 files · 1.9k lines</small>"]
  composables["composables<br/><small>58 files · 4.5k lines</small>"]
  directives["directives<br/><small>2 files · 0.1k lines</small>"]
  lib__flat_["lib (flat)<br/><small>40 files · 4.3k lines</small>"]
  lib_guides["lib/guides<br/><small>23 files · 4.3k lines</small>"]
  lib_webgpu["lib/webgpu<br/><small>15 files · 5.1k lines</small>"]
  modules["modules<br/><small>49 files · 21.3k lines</small>"]
  plots["plots<br/><small>51 files · 8.7k lines</small>"]
  stores["stores<br/><small>36 files · 7.9k lines</small>"]
  tasks["tasks<br/><small>18 files · 9.5k lines</small>"]
  utils__flat_["utils (flat)<br/><small>293 files · 35.7k lines</small>"]
  utils_viewer["utils/viewer<br/><small>22 files · 2.7k lines</small>"]
  components__flat_ -->|202| utils__flat_
  modules -->|153| utils__flat_
  components__flat_ -->|138| stores
  modules -->|125| components__flat_
  modules -->|103| stores
  components_plots -->|98| plots
  modules -->|67| composables
  stores -->|67| utils__flat_
  components__flat_ -->|66| lib__flat_
  components_canvas -->|62| utils__flat_
  composables -->|61| utils__flat_
  components__flat_ -->|60| composables
  composables -->|53| stores
  components_canvas -->|52| plots
  components_plots -->|50| composables
  tasks -->|47| utils__flat_
  components_plots -->|46| utils__flat_
  modules -->|45| plots
  modules -->|39| components_canvas
  components_canvas -->|37| components__flat_
  components_canvas -->|31| stores
  app_root -->|30| modules
  components_canvas -->|26| composables
  components_viewerWindow -->|26| components__flat_
  lib_webgpu -->|26| utils__flat_
  components__other_ -->|25| utils__flat_
  components__other_ -->|23| stores
  components_plots -->|23| stores
  utils_viewer -->|23| utils__flat_
  modules -->|22| utils_viewer
```

<details><summary>Full matrix (rows import columns)</summary>

| | app root | components (flat) | components (other) | components/canvas | components/lightPath | components/plots | components/viewerWindow | composables | directives | lib (flat) | lib/guides | lib/webgpu | modules | plots | stores | tasks | utils (flat) | utils/viewer |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| **app root** | · | 17 | 2 |  |  |  |  | 4 | 2 | 13 |  |  | 30 |  | 16 |  | 2 | 1 |
| **components (flat)** |  | · | 2 |  | 5 |  |  | 60 |  | 66 | 10 |  |  | 11 | 138 | 2 | 202 | 2 |
| **components (other)** |  | 17 | · |  |  |  |  | 12 |  | 8 |  |  |  |  | 23 |  | 25 | 3 |
| **components/canvas** |  | 37 | 1 | · |  | 16 |  | 26 |  | 3 |  |  | 2 | 52 | 31 |  | 62 | 1 |
| **components/lightPath** |  |  |  |  | · |  |  |  |  |  |  |  |  |  |  |  |  |  |
| **components/plots** |  | 21 |  | 1 |  | · |  | 50 |  | 4 |  |  |  | 98 | 23 |  | 46 | 3 |
| **components/viewerWindow** |  | 26 | 2 |  |  |  | · | 6 |  | 2 |  |  |  | 3 | 11 |  | 15 | 6 |
| **composables** |  |  |  |  |  |  |  | · |  | 3 |  |  |  | 7 | 53 |  | 61 | 7 |
| **directives** |  |  |  |  |  |  |  |  | · |  |  |  |  |  |  |  | 3 |  |
| **lib (flat)** |  |  |  |  |  |  |  |  |  | · |  |  |  |  | 5 |  | 6 |  |
| **lib/guides** |  |  |  | 1 |  |  |  |  |  | 4 | · |  | 1 |  |  |  | 4 |  |
| **lib/webgpu** |  |  |  |  |  |  |  |  |  |  |  | · |  |  |  |  | 26 |  |
| **modules** |  | 125 | 1 | 39 |  | 10 | 14 | 67 |  | 14 |  | 3 | · | 45 | 103 | 19 | 153 | 22 |
| **plots** |  |  |  |  |  |  |  |  |  | 1 |  |  |  | · |  |  | 14 | 2 |
| **stores** | 1 |  |  |  |  |  |  | 3 |  | 11 | 4 |  |  | 2 | · |  | 67 | 5 |
| **tasks** |  | 20 | 1 |  |  |  |  | 5 |  | 7 |  |  |  |  | 20 | · | 47 | 1 |
| **utils (flat)** |  |  |  |  |  |  |  | 1 |  | 18 | 13 | 1 |  | 18 | 10 | 8 | · | 1 |
| **utils/viewer** |  |  |  |  |  |  |  |  |  | 1 |  |  |  | 1 | 11 |  | 23 | · |

</details>

## Where the backend is called

Files that call the backend (a bare `fetch(` or a `utils/api.ts` shape), open a `WebSocket` or an `EventSource`: **121** of 831. HTTP calls go through `utils/api.ts` (status checked, failures logged); a bare `fetch(` is a call not moved onto it yet.

| Group | Files |
|---|---|
| utils (flat) | 28 |
| components (flat) | 21 |
| modules | 16 |
| stores | 15 |
| composables | 13 |
| components/plots | 11 |
| components/canvas | 4 |
| tasks | 4 |
| utils/viewer | 4 |
| lib (flat) | 2 |
| components (other) | 1 |
| components/viewerWindow | 1 |
| plots | 1 |
