# Third-party acknowledgements

Cecelia Pineapple is licensed under **GPL-3.0-or-later** (see [`LICENSE`](LICENSE)). This file
acknowledges the third-party software it derives from, bundles, or depends on, together with the
license each is distributed under. It is informational; each component remains governed by its own
license.

## Derived from / ported

| Component | License | Relationship |
|-----------|---------|--------------|
| **cecelia** (`schienstockd/cecelia`, R) — Schienstock & Mueller, *Nature Communications* (2025) | GPL-3.0-or-later | Cecelia Pineapple is a port of this R package; the GPL-3-or-later license is inherited from it. |
| **celltrackR** (`ingewortel/celltrackR`, R) — Wortel et al. | GPL-2.0 | `app/src/tasks/tracking/track_measures.jl` is a from-scratch reimplementation of celltrackR's track-measure functions (path length, displacement, straightness, turning/overall angle, asphericity, …), cited inline against the reference. Credit and notice carried here. |
| **CytoMAP** (`DrStoltzfus/CytoMAP`, MATLAB) — Stoltzfus et al., *Cell Reports* 31(3):107523 (2020), [DOI 10.1016/j.celrep.2020.107523](https://doi.org/10.1016/j.celrep.2020.107523) | MIT | The spatial region-clustering / neighbourhood-composition readouts in `app/src/tasks/spatialAnalysis/` and `app/src/tasks/clustRegions/` are informed by CytoMAP's analytical outputs (raster-window composition, region SOM, density co-localization, region adjacency). Functions derived from a CytoMAP method are cited inline. Statistical tests use squidpy rather than porting CytoMAP's MATLAB code. |
| **CODEX i-niches** — Goltsev et al., *Cell* 174(4):968-981 (2018), [DOI 10.1016/j.cell.2018.07.010](https://doi.org/10.1016/j.cell.2018.07.010) | method (no code reused) | The pairwise cell-type contact **log-odds ratio** statistic (`spatial_utils.pairwise_contact_logodds`, `spatialAnalysis.neighbourStats`) is a from-scratch implementation of the CODEX observed-vs-expected Delaunay-contact metric; the neighbourhood-composition "i-niche" concept behind region clustering is theirs. Cited inline. |

## Bundled / dependencies

### Python analysis env (Pixi — see `pixi.toml`)

| Package | License |
|---------|---------|
| napari | BSD-3-Clause |
| Cellpose | BSD-3-Clause |
| btrack | MIT |
| scanpy | BSD-3-Clause |
| anndata | BSD-3-Clause |
| leidenalg | GPL-3.0-or-later |
| squidpy | BSD-3-Clause |
| scikit-image | BSD-3-Clause |
| NumPy / SciPy | BSD-3-Clause |
| pandas | BSD-3-Clause |
| zarr | MIT |
| dask | BSD-3-Clause |
| trimesh | MIT |
| ome-types | BSD-2-Clause |
| PyTorch / torchvision | BSD-3-Clause |
| PyQt5 | GPL-3.0 (Qt bindings) |
| websockets | BSD-3-Clause |

### Julia package (`app/Project.toml`)

| Package | License |
|---------|---------|
| DataFrames.jl | MIT |
| Distributions.jl | MIT |
| HiddenMarkovModels.jl | MIT |
| HDF5.jl | MIT |
| HTTP.jl | MIT |
| JSON3.jl | MIT |
| StructTypes.jl | MIT |
| StatsAPI.jl | MIT |
| DensityInterface.jl | MIT |

### Frontend (`frontend/package.json`)

| Package | License |
|---------|---------|
| Vue 3 / vue-router / Pinia | MIT |
| PrimeVue / PrimeIcons / @primevue/themes | MIT |
| Observable Plot (`@observablehq/plot`) | ISC |
| regl-scatterplot | MIT |
| Vue Flow (`@vue-flow/*`) | MIT |
| vega-embed | BSD-3-Clause |

---

License names are best-effort summaries of each project's published license at the time of writing;
the authoritative text ships with each package. If you spot an inaccuracy, please open an issue.
