# Python — inside `cecelia.utils`

> **Generated** by `scripts/architecture/python_extract.py` — do not edit; run `pixi run arch-diagrams`.


`cecelia.utils` is 61 modules, 22.4k lines. Which ones carry the weight, and how they lean on each other.

## The 20 most-imported modules

Box: module and how many repo files import it. Arrow `a → b`: a imports b. Only edges among these 20.

```mermaid
flowchart TB
  script_utils["script_utils<br/><small>imported by 63</small>"]
  atomic_io["atomic_io<br/><small>imported by 56</small>"]
  zarr_utils["zarr_utils<br/><small>imported by 41</small>"]
  ome_xml_utils["ome_xml_utils<br/><small>imported by 34</small>"]
  dim_utils["dim_utils<br/><small>imported by 29</small>"]
  label_props_utils["label_props_utils<br/><small>imported by 13</small>"]
  loopback["loopback<br/><small>imported by 7</small>"]
  cpu_utils["cpu_utils<br/><small>imported by 6</small>"]
  gpu_utils["gpu_utils<br/><small>imported by 6</small>"]
  spatial_utils["spatial_utils<br/><small>imported by 5</small>"]
  clustering_utils["clustering_utils<br/><small>imported by 4</small>"]
  correction_utils["correction_utils<br/><small>imported by 4</small>"]
  intensity_utils["intensity_utils<br/><small>imported by 4</small>"]
  segmentation_utils["segmentation_utils<br/><small>imported by 4</small>"]
  slice_utils["slice_utils<br/><small>imported by 4</small>"]
  units["units<br/><small>imported by 4</small>"]
  coastal_utils["coastal_utils<br/><small>imported by 3</small>"]
  ims_meta["ims_meta<br/><small>imported by 3</small>"]
  mesh_utils["mesh_utils<br/><small>imported by 3</small>"]
  title_card["title_card<br/><small>imported by 3</small>"]
  script_utils --> atomic_io
  script_utils --> cpu_utils
  zarr_utils --> ome_xml_utils
  zarr_utils --> slice_utils
  zarr_utils --> units
  dim_utils --> ome_xml_utils
  dim_utils --> units
  label_props_utils --> atomic_io
  spatial_utils --> atomic_io
  spatial_utils --> clustering_utils
  spatial_utils --> label_props_utils
  clustering_utils --> label_props_utils
  correction_utils --> cpu_utils
  correction_utils --> intensity_utils
  correction_utils --> script_utils
  correction_utils --> slice_utils
  correction_utils --> zarr_utils
  segmentation_utils --> cpu_utils
  segmentation_utils --> gpu_utils
  segmentation_utils --> intensity_utils
  segmentation_utils --> script_utils
  segmentation_utils --> zarr_utils
  coastal_utils --> cpu_utils
  coastal_utils --> gpu_utils
  coastal_utils --> script_utils
  coastal_utils --> segmentation_utils
```

## Import cycles

None.

## The de-facto API — most-imported functions and classes

`from cecelia.utils.x import y` counted per importing file, or `zarr_utils.open_as_zarr(…)` after a module import, counted per using file; dev tooling included. These are the interfaces a change ripples through.

| Name | Module | Imported by |
|---|---|---|
| `script_params` | `cecelia.utils.script_utils` | 58 |
| `get_logfile_utils` | `cecelia.utils.script_utils` | 55 |
| `write_json_atomic` | `cecelia.utils.atomic_io` | 48 |
| `read_axes` | `cecelia.utils.zarr_utils` | 33 |
| `open_as_zarr` | `cecelia.utils.zarr_utils` | 32 |
| `DimUtils` | `cecelia.utils.dim_utils` | 29 |
| `parse_meta` | `cecelia.utils.ome_xml_utils` | 28 |
| `staged_store` | `cecelia.utils.zarr_utils` | 21 |
| `open_multiscales_for_writing` | `cecelia.utils.zarr_utils` | 20 |
| `get_param` | `cecelia.utils.script_utils` | 17 |
| `save_meta_in_zarr` | `cecelia.utils.ome_xml_utils` | 15 |
| `write_calibration` | `cecelia.utils.zarr_utils` | 15 |
| `write_multiscale_pyramid` | `cecelia.utils.zarr_utils` | 15 |
| `read_timepoint` | `cecelia.utils.zarr_utils` | 12 |
| `LabelPropsView` | `cecelia.utils.label_props_utils` | 10 |
| `channel_indices` | `cecelia.utils.script_utils` | 9 |
| `carry_valid_box` | `cecelia.utils.zarr_utils` | 9 |
| `write_h5ad_atomic` | `cecelia.utils.atomic_io` | 6 |
| `torch_device` | `cecelia.utils.gpu_utils` | 5 |
| `write_label_pyramid` | `cecelia.utils.zarr_utils` | 5 |
| `atomic_path` | `cecelia.utils.atomic_io` | 4 |
| `default_base_url` | `cecelia.utils.loopback` | 4 |
| `channel_index` | `cecelia.utils.script_utils` | 4 |
| `fortify` | `cecelia.utils.zarr_utils` | 4 |
| `native_dtype` | `cecelia.utils.zarr_utils` | 4 |
| `store_axes` | `cecelia.utils.zarr_utils` | 4 |
| `split_back_and_write` | `cecelia.utils.clustering_utils` | 3 |
| `task_workers` | `cecelia.utils.cpu_utils` | 3 |
| `skimage_centroid_axis_names` | `cecelia.utils.label_props_utils` | 3 |
| `api_token` | `cecelia.utils.loopback` | 3 |

## Class hierarchies

Classes with a base class defined in the repo (exceptions and NamedTuples left out).

```mermaid
classDiagram
  FigureClient <|-- AutonomousClient
  GatingClient <|-- AutonomousClient
  FigureClient <|-- CeceliaClient
  GatingClient <|-- CeceliaClient
  SegmentationUtils <|-- CellposeUtils
  SegmentationUtils <|-- CoastalUtils
```

- `AutonomousClient` ← `FigureClient` — `mcp/cecelia_mcp/autonomous.py`
- `AutonomousClient` ← `GatingClient` — `mcp/cecelia_mcp/autonomous.py`
- `CeceliaClient` ← `FigureClient` — `mcp/cecelia_mcp/client.py`
- `CeceliaClient` ← `GatingClient` — `mcp/cecelia_mcp/client.py`
- `CellposeUtils` ← `SegmentationUtils` — `python/cecelia/utils/cellpose_utils.py`
- `CoastalUtils` ← `SegmentationUtils` — `python/cecelia/utils/coastal_utils.py`
