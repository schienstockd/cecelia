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
