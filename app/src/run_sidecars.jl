# Per-run manifest sidecars beside a label-props table: `{props}.{kind}.json`, a JSON object keyed by
# run (clustering suffix, motif value_name, HMM colName). Three families share the shape —
# `clustfeatures` (clustering), `motiffeatures` (motif discovery), `hmmfit` (HMM states) — and these
# helpers are the ONE reader / merge-writer / key-dropper for all of them. A family keeps its own
# key rules (clustfeatures' family-qualified and legacy keys) on top of the raw dict read here.

# `{dir}/{vn}.h5ad` → `{dir}/{vn}.{kind}.json`
props_sidecar_path(props_path::AbstractString, kind::AbstractString) =
    replace(String(props_path), r"\.h5ad$" => ".$(kind).json")

# The sidecar as a String-keyed Dict; empty when absent or unreadable (a corrupt manifest must not
# break the image's reads — the run's columns are still on the table).
function read_props_sidecar(sidecar::AbstractString)::Dict{String,Any}
    isfile(sidecar) || return Dict{String,Any}()
    try
        Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(sidecar, String), Dict{String,Any}))
    catch
        Dict{String,Any}()
    end
end

# Set (replace) one run's entry, keeping every other run's.
function put_props_sidecar_entry!(sidecar::AbstractString, key::AbstractString, entry)
    merged = read_props_sidecar(sidecar)
    merged[String(key)] = entry
    write_json_atomic(sidecar, merged)
end

# Drop keys from a sidecar already read by its owner (`raw`); remove the file once empty.
function drop_props_sidecar_keys!(sidecar::AbstractString, raw::AbstractDict, keys)
    isfile(sidecar) || return
    kept = Dict{String,Any}(String(k) => v for (k, v) in raw if !(String(k) in keys))
    length(kept) == length(raw) && return
    isempty(kept) ? rm(sidecar) : write_json_atomic(sidecar, kept)
end

# ── clustfeatures: the clustering run manifest's readers ─────────────────────────────────────────
# Clustering writes ONE shared `clusters.{suffix}` column + a `{props}.clustfeatures.json` sidecar into
# EVERY input segment (writer: `_write_clust_features!`, gating/popmanager/clustering_colour.jl). So
# the value_names that took part in a run = those whose clustfeatures sidecar carries `suffix` — the
# single source of truth for "which segmentations belong to this clustering".
_clustfeatures_path(props_path::AbstractString) = props_sidecar_path(props_path, "clustfeatures")

# The sidecar is keyed by the FULL obs column the run wrote — `{family}.{suffix}` (= the
# `_cluster_measure_prefix` family + the run suffix), so a `clusters.immune` cell clustering and a
# `regions.immune` region clustering coexist on one segmentation instead of clobbering each other.
_clustfeatures_key(suffix::AbstractString, family::AbstractString) = "$(family).$(suffix)"

# Split a sidecar key back into (suffix, family). A LEGACY key is a bare suffix with no family prefix
# (written before families existed); it returns `nothing` for the family and is then treated as matching
# ANY requested family — preserving the old single-namespace behaviour for existing sidecars, so this
# change is regression-free on data already on disk.
const _CLUSTFEATURES_FAMILIES = ("clusters", "regions")
function _clustfeatures_split_key(key::AbstractString)
    for fam in _CLUSTFEATURES_FAMILIES
        startswith(key, fam * ".") && return (String(key)[ncodeunits(fam)+2:end], fam)
    end
    (String(key), nothing)
end

_clustfeatures_raw(props_path::AbstractString) = read_props_sidecar(_clustfeatures_path(props_path))

# Suffixes recorded for `family` ("clusters" for clust/trackclust, "regions" for region). Legacy
# family-less entries match every family (see `_clustfeatures_split_key`).
function _clustfeatures_suffixes(props_path::AbstractString;
                                 family::AbstractString = "clusters")::Set{String}
    out = Set{String}()
    for k in keys(_clustfeatures_raw(props_path))
        sfx, fam = _clustfeatures_split_key(String(k))
        (fam === nothing || fam == String(family)) && push!(out, sfx)
    end
    out
end

"""
    _clustfeatures_entry(props_path, suffix; family="clusters") -> AbstractDict | nothing

THE reader for one run's clustfeatures entry — do not index the raw sidecar JSON anywhere else. Prefers
the family-qualified key (`{family}.{suffix}`) and falls back to the legacy bare-suffix key, normalising
the OLDEST format (`{suffix => [features]}`, a bare array with no membership) to the current shape. Every
consumer (the channels endpoint, the observer summaries) goes through this so there is one place that
knows the sidecar's three historical layouts.
"""
function _clustfeatures_entry(props_path::AbstractString, suffix::AbstractString;
                              family::AbstractString = "clusters")
    raw = _clustfeatures_raw(props_path)
    for k in (_clustfeatures_key(suffix, family), String(suffix))
        v = get(raw, k, nothing)
        v isa AbstractDict && return v
        v isa AbstractVector && return Dict{String,Any}(     # oldest format: features only
            "features" => String[string(x) for x in v], "partOf" => String[], "labels" => Dict{String,Any}())
    end
    nothing
end

# One run's `partOf` — the image uids clustered together — from a `_clustfeatures_entry`. A legacy entry
# records none: with `own_uid` that reads as a single-image run on that image, without it as `String[]`.
function _clustfeatures_part_of(entry, own_uid::Union{AbstractString,Nothing}=nothing)::Vector{String}
    v = entry === nothing ? nothing : get(entry, "partOf", get(entry, :partOf, nothing))
    part = v isa AbstractVector ? String[string(x) for x in v] : String[]
    (isempty(part) && own_uid !== nothing) ? [String(own_uid)] : part
end
