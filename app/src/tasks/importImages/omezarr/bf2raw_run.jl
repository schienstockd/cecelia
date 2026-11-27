# Running bioformats2raw, and the one retry the import makes on a failed run: an Imaris `.ims` whose
# attribute heap Bio-Formats cannot read.
#
# Bio-Formats reads Imaris through netcdf-java. Its fractal-heap reader fails on the dense attribute
# storage some Imaris files carry — seen on an Imaris export of a Zeiss CZI whose "Original Metadata"
# group held ~11k attributes: `setId` dies with `java.io.IOException: Invalid argument` under
# `ucar.nc2.iosp.hdf5.FractalHeap.readIndirectBlock`. Writing the same attributes back through libhdf5
# lays out a heap netcdf-java can walk, so the retry rebuilds those groups in a LOCAL working copy
# (`cecelia.utils.ims_relink.rebuild_dense_attrs`) and converts that. Rebuilt, not dropped: the
# source metadata still reaches the OME-XML.

"""Stack-frame prefix of netcdf-java's fractal-heap reader — every trace of this failure passes it."""
const NETCDF_FRACTAL_HEAP = "ucar.nc2.iosp.hdf5.FractalHeap"

"""
    _ims_heap_failure(src_path, out_lines) -> Bool

Whether a failed bioformats2raw run is the netcdf-java attribute-heap failure on an Imaris source —
the one failure `_ims_heap_workcopy` fixes. Keyed on the stack frame, not the message (`Invalid
argument` and `should equal FRHP` are two faces of the same reader).
"""
_ims_heap_failure(src_path::AbstractString, out_lines::AbstractVector{<:AbstractString})::Bool =
    endswith(lowercase(src_path), ".ims") && any(l -> occursin(NETCDF_FRACTAL_HEAP, l), out_lines)

"""
    _ims_heap_workcopy(src_path, staged, stage_dir, run_dir; …) -> Union{String,Nothing}

A working copy of `src_path` with its dense-attribute groups rebuilt, or `nothing` when it could not
be made. `staged` is the copy the import already staged (`stageLocal`), reused as-is; otherwise the
source is staged into `stage_dir` first. Only ever writes under `stage_dir`.
"""
function _ims_heap_workcopy(src_path::AbstractString, staged::Union{AbstractString,Nothing},
                            stage_dir::AbstractString, run_dir::AbstractString;
                            on_log::Function = _ -> nothing, on_progress::Function = (_, _) -> nothing,
                            on_process::Function = _ -> nothing)::Union{String,Nothing}
    on_log("[WARN] Bio-Formats cannot read this Imaris file's metadata layout. Rebuilding it in a " *
           "local copy and converting again — the source file is not changed.")
    work = staged
    if isnothing(work)
        try
            work = _stage_source!(src_path, stage_dir; on_log = on_log, on_progress = on_progress)
        catch e
            on_log("[ERROR] Could not copy the source for the rebuild: $e")
            return nothing
        end
    end
    ok = run_py("tasks/importImages/ims_rebuild_attrs_run.py", (; imPath = work), run_dir;
                on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok ? String(work) : nothing
end

"""
    _run_bf2raw(cmd, zarr_out, src_size; …) -> (ok, lines)

Run one bioformats2raw conversion: stream its output to `on_log`, report progress as bytes written
over `src_size`, and return whether it exited cleanly together with every output line (for
`_ims_heap_failure`). Clean = exit code 0 AND no signal — a cancel can exit 0.
"""
function _run_bf2raw(cmd::Base.AbstractCmd, zarr_out::AbstractString, src_size::Integer;
                     on_log::Function = _ -> nothing, on_progress::Function = (_, _) -> nothing,
                     on_process::Function = _ -> nothing)::Tuple{Bool,Vector{String}}
    out_pipe = Pipe()
    proc = run(pipeline(cmd; stdout = out_pipe, stderr = out_pipe); wait = false)
    close(out_pipe.in)
    on_process(proc)
    monitor = @async begin
        while process_running(proc)
            if isdir(zarr_out) && src_size > 0
                on_progress(round(Int, min(_dir_bytes(zarr_out) / src_size, 0.98) * 100), 100)
            end
            sleep(2)
        end
    end
    lines = String[]
    for line in eachline(out_pipe)
        on_log(line)
        push!(lines, line)
    end
    wait(proc)
    wait(monitor)
    (proc.exitcode == 0 && proc.termsignal == 0, lines)
end
