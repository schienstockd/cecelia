# ── Board export provenance (docs/ANALYSIS.md → Export) ───────────────────────────────────────────
#
# The record an exported board figure carries (PDF document info / SVG <metadata>): which code, which
# images, which populations (and the gate files that define them), and which runs made what the board
# shows. The figure is what leaves the lab, so it has to say where it came from without the project.
#
# The runs are the board-scoped parameter table's rows (`history_params_table` with the board's
# populations + cluster run) — the same cut the Parameters CSV uses, so the figure and its CSV can never
# name different runs. Read-only. The export time and the board titles are added by the client, which
# assembles one record across boards (`frontend/src/utils/boardProvenance.ts`).

"""
    board_export_provenance(proj, image_uids; pops, cluster_runs) -> Dict

What one board shows, for an exported figure: `ceceliaVersion`, `project` `{uid, name}`, `images`
`[{uid, name}]`, `populations` `[{key, popType, valueName, pop, gateFiles}]` (`gateFiles` = the
project-relative gating sidecars that define the population, one per image that has one; empty for a
`labels` key, which draws the objects themselves) and `runs` — the board-scoped parameter-table rows
as they are (`history_params_table`: step, task, output, relation, images, lastRun, taskIds,
changedFromDefault, conflicts, note), so the client can compare them with the board's frozen snapshot.
"""
function board_export_provenance(proj::CciaProject, image_uids::Vector{String};
                                 pops::AbstractVector = String[],
                                 cluster_runs::AbstractVector = String[])::Dict{String,Any}
    imgs = CciaImage[]
    for uid in image_uids
        img = try init_object(proj.uid, uid) catch; nothing end
        img isa CciaImage && push!(imgs, img)
    end
    populations = Any[]
    for k in unique(String.(pops))
        t = _parse_tkey(k)
        files = String[]
        if t.popType != "labels" && !isempty(t.valueName)
            for img in imgs
                p = gating_path(img._dir, t.valueName; pop_type = t.popType)
                isfile(p) && push!(files, replace(relpath(p, proj.root), '\\' => '/'))
            end
        end
        push!(populations, Dict{String,Any}("key" => k, "popType" => t.popType, "valueName" => t.valueName,
                                            "pop" => t.pop, "gateFiles" => files))
    end
    runs = history_params_table(proj, image_uids; pops = String.(pops), cluster_runs = String.(cluster_runs))
    Dict{String,Any}(
        "ceceliaVersion" => (try cecelia_version() catch; "" end),
        "project" => Dict{String,Any}("uid" => proj.uid, "name" => proj.name),
        "images" => Any[Dict{String,Any}("uid" => img.uid, "name" => img.name) for img in imgs],
        "populations" => populations,
        "runs" => runs)
end

"""
    gate_file_edits(proj | project_root, files, since; local_offset) -> Vector{Dict}

For each gating sidecar a board record names (`gateFiles`, project-relative), whether its gate changed
after `since` (a UTC `DateTime`: the figure's export time, or the board's frozen-parameters time). Read
from the append-only gate history beside the sidecar (`gating/popmanager/history.jl`), so a cosmetic
write (colour, show) does not count. One `{file, status, edits, lastAt}` per file, in order:

- `edited`: `edits` history entries after `since`; `lastAt` the newest (`iso_utc`).
- `unchanged`: no history entry after `since`, or no history and the sidecar not written since.
- `unknown`: no history, and the sidecar was written after `since` (a project from before gate
  history, or a cosmetic write: it can't tell which).
- `missing`: the sidecar is gone.
- `invalid`: not a gating sidecar inside this project; never read.

History `at` is local wall time; `local_offset` (local − UTC) converts it, the CURRENT offset, so an
edit across a DST change can be off by an hour.
"""
gate_file_edits(proj::CciaProject, files::AbstractVector, since::DateTime; kw...) =
    gate_file_edits(proj.root, files, since; kw...)
function gate_file_edits(project_root::AbstractString, files::AbstractVector, since::DateTime;
                         local_offset::Dates.Period = round(Dates.now() - Dates.now(Dates.UTC), Dates.Minute))
    root = normpath(project_root)
    out = Dict{String,Any}[]
    for f in String.(files)
        p = normpath(joinpath(root, f))
        ok = startswith(p, rstrip(root, ['/', '\\']) * Base.Filesystem.path_separator) &&
             endswith(p, ".json") && basename(dirname(p)) == "gating"
        if !ok
            push!(out, Dict{String,Any}("file" => f, "status" => "invalid", "edits" => 0, "lastAt" => ""))
            continue
        end
        if !isfile(p)
            push!(out, Dict{String,Any}("file" => f, "status" => "missing", "edits" => 0, "lastAt" => ""))
            continue
        end
        hist = gating_history_file(p)
        after = DateTime[]
        for e in read_jsonl(hist)
            at = try DateTime(string(get(e, "at", get(e, :at, "")))) - local_offset catch; nothing end
            at !== nothing && at > since && push!(after, at)
        end
        status = if !isempty(after)
            "edited"
        elseif isfile(hist) || Dates.unix2datetime(mtime(p)) <= since
            "unchanged"
        else
            "unknown"
        end
        push!(out, Dict{String,Any}("file" => f, "status" => status, "edits" => length(after),
                                    "lastAt" => isempty(after) ? "" : iso_utc(maximum(after))))
    end
    out
end
