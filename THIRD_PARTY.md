# Third-party acknowledgements

Cecelia Feijoa is licensed under **GPL-3.0-or-later** (see [`LICENSE`](LICENSE)). This file
acknowledges the third-party software it derives from, bundles, or depends on, together with the
license each is distributed under. It is informational; each component remains governed by its own
license.

## Derived from / ported

| Component | License | Relationship |
|-----------|---------|--------------|
| **cecelia** (`schienstockd/cecelia`, R) — Schienstock & Mueller, *Nature Communications* (2025) | GPL-3.0-or-later | Cecelia Feijoa is a port of this R package; the GPL-3-or-later license is inherited from it. |
| **celltrackR** (`ingewortel/celltrackR`, R) — Wortel et al. | GPL-2.0 | `app/src/tasks/tracking/track_measures.jl` is a from-scratch reimplementation of celltrackR's track-measure functions (path length, displacement, straightness, turning/overall angle, asphericity, …), cited inline against the reference. Credit and notice carried here. |
| **CytoMAP** (`DrStoltzfus/CytoMAP`, MATLAB) — Stoltzfus et al., *Cell Reports* 31(3):107523 (2020), [DOI 10.1016/j.celrep.2020.107523](https://doi.org/10.1016/j.celrep.2020.107523) | MIT | The spatial region-clustering / neighbourhood-composition readouts in `app/src/tasks/spatialAnalysis/` and `app/src/tasks/clustRegions/` are informed by CytoMAP's analytical outputs (raster-window composition, region SOM, density co-localization, region adjacency). Functions derived from a CytoMAP method are cited inline. Statistical tests use squidpy rather than porting CytoMAP's MATLAB code. |
| **CODEX i-niches** — Goltsev et al., *Cell* 174(4):968-981 (2018), [DOI 10.1016/j.cell.2018.07.010](https://doi.org/10.1016/j.cell.2018.07.010) | method (no code reused) | The pairwise cell-type contact **log-odds ratio** statistic (`spatial_utils.pairwise_contact_logodds`, `spatialAnalysis.neighbourStats`) is a from-scratch implementation of the CODEX observed-vs-expected Delaunay-contact metric; the neighbourhood-composition "i-niche" concept behind region clustering is theirs. Cited inline. |
| **ILEE_CSK** — Li et al., *Plant Cell* 35(2):371-397 (2023), [DOI 10.1093/plcell/koac290](https://doi.org/10.1093/plcell/koac290) | method (no code vendored) | The anisotropy path in `app/src/tasks/segment/branching_run.py` (`_anisotropy_2d/3d`) — local structure tensor at scale σ, mean-pooled over `box × box` windows, eigendecomposed per box → the same 5-array output ILEE_CSK produced for cytoskeleton anisotropy — is a from-scratch reimplementation against `skimage.feature.structure_tensor`. The upstream `ILEE_CSK` Python package is unmaintained (last commit 2024-04-22, `imp` unimportable on py3.12); no code from it is bundled. Cited inline. |
| **sitkibex** (`niaid/sitk-ibex`, v0.2.1) — Lowekamp / NIAID, [Zenodo 4632320](https://zenodo.org/record/4632320) | Apache-2.0 | The staining-cycle registration engine used by `editImages.register` (SimpleITK ITKv4 multi-modal affine + `resample`). Vendored under [`python/sitkibex/`](python/sitkibex/) rather than a PyPI dep (not on conda-forge). Only the `registration`/`resample`/utilities modules are kept; the upstream CLI + OME-XML reader (`cli.py`, `__main__.py`, `io.py`, `xml_info.py`) are dropped because we read OME-XML through `cecelia.utils.ome_xml_utils`. Source: [github.com/niaid/sitk-ibex](https://github.com/niaid/sitk-ibex). |
| **SUPPORT** (`NICALab/SUPPORT`) — Eom et al., *Nature Methods* 20:1581-1588 (2023), [DOI 10.1038/s41592-023-02005-8](https://doi.org/10.1038/s41592-023-02005-8) | GPL-3.0 (compatible with cecelia's GPL-3-or-later) | The self-supervised temporal blind-spot denoiser used by `cleanupImages.denoise` and trained by `opticalFlow.trainSupportDenoise`. Vendored under [`python/cecelia/vendor/support/`](python/cecelia/vendor/support/) — SUPPORT is not `pip install`-ready and upstream response times are slow ([issue #25](https://github.com/NICALab/SUPPORT/issues/25)), so we own the copy: `model/SUPPORT.py`, `model/convhole.py`, and a merged `dataset.py` (`DatasetSUPPORT`, `DatasetSUPPORT_test_stitch`, `random_transform`, `normalize`, `get_coordinate`); the upstream training CLI and dataloader factory are dropped because we drive training through `run_py`. Commit pin + local edits: [`python/cecelia/vendor/support/VENDORED.md`](python/cecelia/vendor/support/VENDORED.md). Verbatim third-party algorithm — cited inline in the runner. Source: [github.com/NICALab/SUPPORT](https://github.com/NICALab/SUPPORT). |

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
| skan | BSD-3-Clause |
| NumPy / SciPy | BSD-3-Clause |
| pandas | BSD-3-Clause |
| zarr | MIT |
| dask | BSD-3-Clause |
| trimesh | MIT |
| ome-types | BSD-2-Clause |
| PyTorch / torchvision | BSD-3-Clause |
| PyQt5 | GPL-3.0 (Qt bindings) |
| websockets | BSD-3-Clause |
| SimpleITK | Apache-2.0 |

> **Cellpose image restoration (denoise) — method lineage.** cecelia no longer runs a Cellpose
> denoiser: `cleanupImages.cellposeCorrect` was retired with the move to **cellpose 4**, which has no
> `DenoiseModel` (see docs/todo/CELLPOSE_V4_PLAN.md). `cleanupImages.smooth` uses coastal's
> model-free restorers instead. The lineage is kept here because the sibling **coastal** project
> still carries a reimplementation of that inference path, and because the restoration work it
> builds on is worth attributing:
> **CARE / CSBDeep** — Weigert et al., *Nature Methods* 2018,
> [DOI 10.1038/s41592-018-0216-7](https://doi.org/10.1038/s41592-018-0216-7); **Noise2Void** —
> Krull et al., CVPR 2019; **Noise2Self** — Batson & Royer, ICML 2019. The sibling **coastal**
> project reimplements this denoise inference path (CPnet) from scratch; see its `THIRD_PARTY.md`
> for the architecture-reimplementation attribution.

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
