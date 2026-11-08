function _companion_files(names::AbstractVector{<:AbstractString}, main::AbstractString)
    stem   = first(splitext(main))       # main name WITHOUT extension, e.g. "…-res_0001"
    prefix = stem * "_"
    filter(names) do n
        n == main && return true
        # Olympus OIR companions are `<main-stem>_<digits>` and are typically EXTENSIONLESS, e.g.
        # `…-res_0001.oir` (main) + `…-res_0001_00001`, `…-res_0001_00002`, … . So match a numeric
        # run after `<stem>_` (with an optional extension), not a fixed 5-digit + same-extension shape
        # — the earlier rule matched none of these, so only the main file staged and bioformats saw a
        # fraction of the timepoints. Literal stem prefix still avoids grabbing a sibling `…-res_0002`.
        startswith(n, prefix) || return false
        occursin(r"^[0-9]+(\.[^.]+)?$", chop(n; head = length(prefix), tail = 0))
    end
end

"""
Copy a source image (+ its companion file set) to a local scratch dir and return the path to the
copied main file. Reading a multi-file format like Olympus OIR directly over SMB is dominated by
per-read network latency (bioformats does many small random seeks); a bulk sequential copy is
throughput-bound and far faster — this automates the manual copy-to-tmp workaround.

`_companion_files` matches the main file + its companions by LITERAL stem prefix — never interpolate
the stem into a regex, `basal+NECA` would break it. Real Olympus naming: the registered file already
ends in `_NNNN.oir` and the companions are EXTENSIONLESS (`…-res_0001.oir` + `…-res_0001_00001`, …),
so the match is `<main-stem>_<digits>` with an OPTIONAL extension, not a fixed `_<5 digits><same-ext>`.
The first version matched none of the extensionless parts, so only the main file staged and bioformats
saw ~4 of 181 timepoints. The literal-stem prefix still excludes a sibling acquisition (`…-res_0002`).
"""
# Copy one file in chunks, yielding between blocks. Julia's `cp` is a single NON-yielding blocking
# call (`jl_fs_sendfile`); when the pool worker running it is scheduled onto the event-loop thread, a
# multi-GB copy freezes the WS server (and the whole GUI) until it finishes. A chunked loop with an
# explicit `yield()` keeps the scheduler/event loop responsive, and lets us report progress.
function _copy_file_yielding(src::AbstractString, dst::AbstractString;
                             chunk::Int = 8 * 1024 * 1024, on_bytes::Function = _ -> nothing)
    buf = Vector{UInt8}(undef, chunk)
    open(src, "r") do s
        open(dst, "w") do d
            while !eof(s)
                n = readbytes!(s, buf, chunk)
                write(d, view(buf, 1:n))
                on_bytes(n)
                yield()   # let the WS event loop + other tasks run between chunks
            end
        end
    end
end

function _stage_source!(src_path::AbstractString, stage_dir::AbstractString;
                        on_log::Function = _ -> nothing, on_progress::Function = (_, _) -> nothing)
    src_dir = dirname(src_path)
    files   = _companion_files(readdir(src_dir), basename(src_path))
    isempty(files) && (files = [basename(src_path)])
    mkpath(stage_dir)

    grand_total = sum(f -> filesize(joinpath(src_dir, f)), files; init = 0)
    on_log("[INFO] Staging $(length(files)) source file(s), $(round(grand_total / 1e9; digits = 1)) GB, to local scratch …")

    copied = 0
    for f in files
        _copy_file_yielding(joinpath(src_dir, f), joinpath(stage_dir, f);
                            on_bytes = n -> (copied += n; grand_total > 0 && on_progress(copied, grand_total)))
    end
    on_log("[INFO] Staged $(length(files)) file(s) ($(round(copied / 1e9; digits = 1)) GB).")
    joinpath(stage_dir, basename(src_path))
end


"""
    import_source_fingerprint(src_path) -> Dict{String,Any}

`source_fingerprint` of the file the import read, plus — for a multi-file format (Olympus OIR) — the
companions' `count` and total `bytes` from `stat` alone (no reads), since the bulk of the pixels live
in them and the main file's sample would not see them change. Banked as `meta.oriFingerprint`.
Empty (and a `@warn`) when it can't be taken — a record, so it never fails the import.
"""
function import_source_fingerprint(src_path::AbstractString)::Dict{String,Any}
    try
        fp = source_fingerprint(src_path)
        isempty(fp) && return fp
        dir, main = dirname(abspath(src_path)), basename(src_path)
        comps = filter(!=(main), _companion_files(readdir(dir), main))
        isempty(comps) || (fp["companions"] = Dict{String,Any}("count" => length(comps),
            "bytes" => sum(f -> filesize(joinpath(dir, f)), comps; init = 0)))
        fp
    catch e
        @warn "Could not fingerprint the import source" src_path exception = e
        Dict{String,Any}()
    end
end
