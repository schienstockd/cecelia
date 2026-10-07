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
    base = try series_base(path) catch; String(path) end
    paths = unique([path; [joinpath(b, rel) for b in unique([String(path), base])
                           for rel in (".zattrs", ".zgroup", "zarr.json",
                                       joinpath("0", ".zarray"), joinpath("0", "zarr.json"))]])
    parts = String[]
    for p in paths
        st = stat(p)
        ispath(st) && push!(parts, "$(st.inode):$(st.mtime):$(st.ctime)")
    end
    join(parts, "|")
end
