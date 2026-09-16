using JSON3
using Statistics: median

# OME-ZARR importer + metadata layer aggregator. The 1100-line file this replaces held five
# distinct responsibilities that grew together; splitting them mirrors what the codebase already
# does for chain.jl / task.jl. Nothing here `module`s the sub-files — the runtime is one namespace,
# and struct/const definitions load top-down, function bodies resolve at call time.
#
#   reader.jl      — pure metadata reads. `_delta_t_fallback` + `_recover_ims_time_increment!` (OME
#                    time-interval recovery), the NGFF/series structural readers (`series_base`,
#                    `bf2raw_series_subdir`, `ngff_attrs`, `ngff_multiscales`, `ngff_version`,
#                    `zarr_array_meta`), and `read_ome_metadata` — the single flat-Dict reader
#                    every consumer (api/image_geometry.jl, qc.jl, coastal, this task) shares.
#   calibration.jl — metadata WRITERS. `update_ome_scale!` (NGFF axis scale + unit) and
#                    `update_ome_xml_pixels!` (OME-XML `Pixels` attrs) — kept together because a
#                    calibration edit must land in BOTH halves or the store disagrees with itself.
#                    `sync_zarr_calibration!` is the single translator from ccid.json's meta shape
#                    to those two writers, used by the importer AND the metadata editor.
#   ccid_sync.jl   — ccid.json ⇄ zarr-metadata bridge. `_merge_channel_names!` / `_merge_zarr_meta_into_ccid!`
#                    reconcile a freshly-read `read_ome_metadata` result into an existing ccid.json;
#                    `resync_ome_meta!` is the entry point routes/importer call after a metadata edit.
#   staging.jl     — source copying — `_stage_source!` and the yielding chunked copy underneath, so
#                    a multi-GB import doesn't freeze the WS server. The COMPANION detector
#                    (`_companion_files`) also lives here since the staging step is what fans it out.
#   task.jl        — the `ImportOmezarr` task itself: struct, `ImportOmezarrParams`,
#                    `parse_import_omezarr_params`, and the `_run_task` that stitches everything above
#                    together.
include(joinpath(@__DIR__, "omezarr", "reader.jl"))
include(joinpath(@__DIR__, "omezarr", "calibration.jl"))
include(joinpath(@__DIR__, "omezarr", "ccid_sync.jl"))
include(joinpath(@__DIR__, "omezarr", "staging.jl"))
include(joinpath(@__DIR__, "omezarr", "task.jl"))
