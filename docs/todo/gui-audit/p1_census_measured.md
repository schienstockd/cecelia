# GUI capture (measured) — 2026-11-09T23:15:34.786Z

Base http://127.0.0.1:5223, viewport 1440x900, project `RFtn9f,4kS67f`. Counts are visible controls (buttons, links, inputs, selects, tabs, switches) at first paint, after the page settled; `page` = inside `.cc-main`, `chrome` = header, sidebar and rails.

| State | Route | Page controls | …above the fold | Chrome |
|---|---|---:|---:|---:|
| no-project | `/` | 0 | 0 | 30 |
| no-project | `/manage-images` | 23 | 23 | 32 |
| no-project | `/preprocess` | 22 | 22 | 32 |
| no-project | `/metadata` | 19 | 19 | 32 |
| no-project | `/cleanup` | 21 | 21 | 32 |
| no-project | `/model-training` | 29 | 24 | 32 |
| no-project | `/segment` | 25 | 24 | 32 |
| no-project | `/gate` | 2 | 2 | 31 |
| no-project | `/phenotype` | 2 | 2 | 29 |
| no-project | `/track` | 28 | 25 | 31 |
| no-project | `/behaviour` | 22 | 21 | 29 |
| no-project | `/clust-cells` | 23 | 17 | 31 |
| no-project | `/clust-tracks` | 24 | 23 | 31 |
| no-project | `/regions` | 21 | 20 | 31 |
| no-project | `/spatial` | 19 | 19 | 29 |
| no-project | `/analysis` | 2 | 2 | 32 |
| no-project | `/notebooks` | 0 | 0 | 32 |
| no-project | `/animation` | 3 | 3 | 32 |
| no-project | `/batch-movies` | 3 | 3 | 32 |
| no-project | `/movies` | 0 | 0 | 30 |
| no-project | `/tasks` | 10 | 10 | 26 |
| no-project | `/blackboard` | 0 | 0 | 30 |
| no-project | `/chain` | 11 | 11 | 28 |
| no-project | `/settings` | 51 | 17 | 26 |
| RFtn9f | `/` | 0 | 0 | 26 |
| RFtn9f | `/manage-images` | 37 | 37 | 32 |
| RFtn9f | `/preprocess` | 32 | 32 | 32 |
| RFtn9f | `/metadata` | 28 | 28 | 32 |
| RFtn9f | `/cleanup` | 33 | 32 | 32 |
| RFtn9f | `/model-training` | 42 | 37 | 32 |
| RFtn9f | `/segment` | 38 | 37 | 32 |
| RFtn9f | `/gate` | 14 | 14 | 31 |
| RFtn9f | `/phenotype` | 14 | 14 | 29 |
| RFtn9f | `/track` | 41 | 38 | 31 |
| RFtn9f | `/behaviour` | 35 | 34 | 29 |
| RFtn9f | `/clust-cells` | 36 | 30 | 31 |
| RFtn9f | `/clust-tracks` | 37 | 36 | 31 |
| RFtn9f | `/regions` | 34 | 33 | 31 |
| RFtn9f | `/spatial` | 31 | 31 | 29 |
| RFtn9f | `/analysis` | 18 | 18 | 32 |
| RFtn9f | `/notebooks` | 14 | 14 | 32 |
| RFtn9f | `/animation` | 14 | 14 | 32 |
| RFtn9f | `/batch-movies` | 13 | 13 | 32 |
| RFtn9f | `/movies` | 12 | 12 | 30 |
| RFtn9f | `/tasks` | 10 | 10 | 26 |
| RFtn9f | `/blackboard` | 21 | 21 | 30 |
| RFtn9f | `/chain` | 15 | 15 | 28 |
| RFtn9f | `/settings` | 56 | 21 | 26 |
| 4kS67f | `/` | 0 | 0 | 26 |
| 4kS67f | `/manage-images` | 62 | 62 | 32 |
| 4kS67f | `/preprocess` | 58 | 58 | 32 |
| 4kS67f | `/metadata` | 53 | 53 | 32 |
| 4kS67f | `/cleanup` | 57 | 57 | 32 |
| 4kS67f | `/model-training` | 68 | 63 | 32 |
| 4kS67f | `/segment` | 64 | 63 | 32 |
| 4kS67f | `/gate` | 40 | 40 | 31 |
| 4kS67f | `/phenotype` | 40 | 40 | 29 |
| 4kS67f | `/track` | 67 | 64 | 31 |
| 4kS67f | `/behaviour` | 61 | 60 | 29 |
| 4kS67f | `/clust-cells` | 62 | 56 | 31 |
| 4kS67f | `/clust-tracks` | 64 | 63 | 31 |
| 4kS67f | `/regions` | 60 | 59 | 31 |
| 4kS67f | `/spatial` | 57 | 57 | 29 |
| 4kS67f | `/analysis` | 49 | 49 | 32 |
| 4kS67f | `/notebooks` | 29 | 29 | 32 |
| 4kS67f | `/animation` | 40 | 40 | 32 |
| 4kS67f | `/batch-movies` | 39 | 39 | 32 |
| 4kS67f | `/movies` | 25 | 25 | 30 |
| 4kS67f | `/tasks` | 10 | 10 | 26 |
| 4kS67f | `/blackboard` | 23 | 23 | 30 |
| 4kS67f | `/chain` | 16 | 16 | 28 |
| 4kS67f | `/settings` | 56 | 21 | 26 |

Produced by `scripts/gui_capture.mjs` against an isolated instance (own config dir, port slot 5) with copies of projects RFtn9f and 4kS67f. Screenshots are not committed except the few in this folder. Image-table rows add per-row controls, so page counts grow with the number of images in the set.
