# `store_rev` — a stat-only change fingerprint for a store. Lives in the omezarr family because it
# names the raw metadata files (`.zattrs` / `.zarray` / `zarr.json`), which only this tier may do.

"""
A cheap "did this store's bytes change" fingerprint: inode + mtime + ctime of the store root and
its root/level-0 metadata files — a handful of `stat`s, never a walk (a drift-corrected store on
zolIMa is 300k chunk files). Every rewrite path either recreates the root (rmtree + `mode='w'`,
staging dir + rename swap) or rewrites the metadata, so one of these moves; the viewer compares it
across a task-done meta refetch and reloads only when a store it SHOWS changed. "" when absent.
A bioformats2raw import keeps its multiscales one level down (`<store>/0/`); `series_base` is the
canonical resolver, so both layouts fingerprint their real metadata — a calibration rewrite
(`set_ngff_axes`) moves it too.
"""
function store_rev(path::AbstractString)
    isdir(path) || return ""
    parts = String[]
    for p in _store_meta_paths(path)
        st = stat(p)
        ispath(st) && push!(parts, "$(st.inode):$(st.mtime):$(st.ctime)")
    end
    join(parts, "|")
end

# The store root plus its root/level-0 metadata files, in both layouts — what `store_rev` and
# `store_written_at` stat.
function _store_meta_paths(path::AbstractString)
    base = try series_base(path) catch; String(path) end
    unique([String(path); [joinpath(b, rel) for b in unique([String(path), base])
                           for rel in (".zattrs", ".zgroup", "zarr.json",
                                       joinpath("0", ".zarray"), joinpath("0", "zarr.json"))]])
end

"""
    store_written_at(path) -> Float64

The latest mtime (unix seconds) among the same stat-only set `store_rev` fingerprints — a lower bound
on when the store's current contents were written, and `0.0` when the store is absent. Every writer
fills a fresh staging dir and renames it into place, so these files are written by the run that
produced the store; the rename itself is not relied on, since a directory rename does not move the
directory's own mtime on every filesystem. Answers "is this store newer than that run's start?".
"""
function store_written_at(path::AbstractString)::Float64
    isdir(path) || return 0.0
    maximum(p -> (st = stat(p); ispath(st) ? Float64(st.mtime) : 0.0), _store_meta_paths(path))
end
