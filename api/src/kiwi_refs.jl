# ── Kiwi refs — does a pointer name a real object? ───────────────────────────────────────────────────
#
# A `KiwiRef` is ONE typed pointer to an app object (`frontend/src/lib/kiwiRef.schema.json`), used as
# a prompt chip, as the evidence on a Kiwi claim, and as the target Kiwi points at. This file answers,
# per ref: is its SHAPE valid (the schema), and does the object EXIST — and how sure that answer is.
# docs/todo/KIWI_ASSISTANT_PLAN.md → *The reference type*, Decision 6 (validation is deterministic, in
# Cecelia, never a second model) and Decision 10 (the UI shows how far a chip was checked).
#
#   POST /api/kiwi/refs/resolve  {projectUid, refs:[KiwiRef…]}
#     → {results:[{ok, check, label, error}…]}   one per ref, same order
#
# `check` is how far the answer goes — the UI must not render these alike:
#   "exists"  verified against what is on disk (project, image, cells, capture, …)
#   "live"    verified against in-memory state the browser published (an open plot panel, a landscape
#             tile) — true NOW, gone when the panel closes or the server restarts
#   "format"  only the shape could be checked (a UI anchor: valid anchors are known only to the browser)
#
# READ-ONLY by construction: every lookup is an existing reader, and the one reader that can write
# (`load_pop_map`'s uid backfill) is called with `backfill_save = false`. Refs are project-relative;
# `projectUid` travels with the request.

# ── The schema — loaded once, the same file the TS side imports ──────────────────────────────────────
const _KIWI_REF_SCHEMA_PATH = normpath(joinpath(@__DIR__, "..", "..", "frontend", "src", "lib", "kiwiRef.schema.json"))
const _KIWI_REF_SCHEMA = JSON3.read(read(_KIWI_REF_SCHEMA_PATH, String), Dict{String,Any})
const _KIWI_REF_DEFS = _KIWI_REF_SCHEMA["definitions"]
# The kinds, in schema order — every `oneOf` branch is `{"$ref": "#/definitions/<kind>"}`.
const KIWI_REF_KINDS = String[split(String(b["\$ref"]), '/')[end] for b in _KIWI_REF_SCHEMA["oneOf"]]

_kiwi_def(d) = haskey(d, "\$ref") ? _KIWI_REF_DEFS[split(String(d["\$ref"]), '/')[end]] : d

# The subset of JSON Schema the file uses: type / const / minLength / maxLength / pattern / minimum /
# maximum / minItems / items. Returns "" when `v` fits `spec`, else a short reason. PURE → tested.
function _kiwi_value_error(v, spec)::String
    d = _kiwi_def(spec)
    haskey(d, "const") && return v == d["const"] ? "" : "must be \"$(d["const"])\""
    t = get(d, "type", "")
    if t == "string"
        v isa AbstractString || return "must be a string"
        length(v) >= get(d, "minLength", 0) || return "must not be empty"
        length(v) <= get(d, "maxLength", typemax(Int)) || return "is too long"
        haskey(d, "pattern") && !occursin(Regex(d["pattern"]), v) && return "has the wrong format"
    elseif t == "integer" || t == "number"
        (v isa Number && !(v isa Bool)) || return "must be a number"
        t == "integer" && !isinteger(v) && return "must be a whole number"
        v >= get(d, "minimum", -Inf) || return "must be ≥ $(d["minimum"])"
        v <= get(d, "maximum", Inf)  || return "must be ≤ $(d["maximum"])"
    elseif t == "array"
        v isa AbstractVector || return "must be a list"
        length(v) >= get(d, "minItems", 0) || return "must not be empty"
        for x in v
            e = _kiwi_value_error(x, d["items"]); isempty(e) || return "items $e"
        end
    end
    ""
end

"""
    kiwi_ref_shape_error(ref) -> String

"" when `ref` matches its kind's schema definition, else a reason naming the field. Unknown kinds,
missing required fields, unknown fields and wrong types all fail — the shape check the engine's own
`--json-schema` pass should already have made, repeated here because the resolver also serves the
frontend and any engine without native schema support.
"""
function kiwi_ref_shape_error(ref)::String
    ref isa AbstractDict || return "a ref must be an object"
    kind = string(something(_kiwi_get(ref, "kind"), ""))
    kind in KIWI_REF_KINDS || return isempty(kind) ? "missing kind" : "unknown kind \"$kind\""
    def   = _KIWI_REF_DEFS[kind]
    props = def["properties"]
    keys_ = Set(String(k) for k in keys(ref))
    for r in def["required"]
        String(r) in keys_ || return "$kind ref is missing $r"
    end
    for k in keys_
        haskey(props, k) || return "$kind ref has unknown field $k"
        e = _kiwi_value_error(_kiwi_get(ref, k), props[k]); isempty(e) || return "$k $e"
    end
    ""
end

_kiwi_get(ref, k::AbstractString, default = nothing) =
    haskey(ref, k) ? ref[k] : haskey(ref, Symbol(k)) ? ref[Symbol(k)] : default

_kiwi_ok(check, label)  = Dict{String,Any}("ok" => true,  "check" => check, "label" => String(label), "error" => "")
_kiwi_bad(check, error) = Dict{String,Any}("ok" => false, "check" => check, "label" => "", "error" => String(error))

# ── Per-kind resolvers — each built on the existing reader for that object ──────────────────────────

# The image named by the ref, or an error string. `init_object` reads only that object's ccid.json.
function _kiwi_image(puid::String, ref)::Union{CciaImage,String}
    iu = String(_kiwi_get(ref, "imageUid"))
    _valid_asset_id(iu) || return "invalid image id"
    isfile(state_file(joinpath(projects_dir(), puid), iu)) || return "no image $iu in this project"
    obj = try init_object(puid, iu) catch; return "image $iu could not be read" end
    obj isa CciaImage || return "$iu is a set, not an image"
    obj
end

function _kiwi_resolve_project(puid, ref)
    path = joinpath(projects_dir(), puid, "project.json")
    isfile(path) || return _kiwi_bad("exists", "no project $puid")
    # a one-field peek (the name), not a load: `load_project` reads every set's ccid.json
    name = try string(get(JSON3.read(read(path, String)), :name, puid)) catch; puid end
    _kiwi_ok("exists", name)
end

function _kiwi_resolve_set(puid, ref)
    su = String(_kiwi_get(ref, "setUid"))
    _valid_asset_id(su) || return _kiwi_bad("exists", "invalid set id")
    isfile(state_file(joinpath(projects_dir(), puid), su)) || return _kiwi_bad("exists", "no set $su in this project")
    obj = try init_object(puid, su) catch; return _kiwi_bad("exists", "set $su could not be read") end
    obj isa CciaSet || return _kiwi_bad("exists", "$su is an image, not a set")
    _kiwi_ok("exists", obj.name)
end

function _kiwi_resolve_image(puid, ref)
    img = _kiwi_image(puid, ref)
    img isa String ? _kiwi_bad("exists", img) : _kiwi_ok("exists", img.name)
end

# Which pop type holds `path` on this segmentation, and its display name — `nothing` when none does.
# The same shortcuts `resolve_pop_type` takes (a derived leaf like `_tracked`, the root), then a probe
# of every type's map — `resolve_pop_type` itself returns "flow" for an unknown path, so it can't answer
# "does this exist", and its probe may backfill-save; this one never writes.
function _kiwi_pop_type(img, vn::AbstractString, path::AbstractString)
    for seg in split(path, '/'; keepempty = false)
        d = get(Cecelia._DERIVED_POPS, String(seg), nothing)
        d === nothing || return (; popType = d.pop_type, name = path, fixed = true)
    end
    is_root(path) && return (; popType = "flow", name = "all cells", fixed = true)
    for pt in Cecelia._POP_TYPE_PROBE_ORDER
        m = try load_pop_map(img; value_name = vn, pop_type = pt, backfill_save = false) catch; continue end
        has_pop(m, path) && return (; popType = pt, name = pop_at(m, path).name, fixed = false)
    end
    nothing
end

function _kiwi_resolve_population(puid, ref)
    img = _kiwi_image(puid, ref); img isa String && return _kiwi_bad("exists", img)
    vn, path = String(_kiwi_get(ref, "valueName")), String(_kiwi_get(ref, "popPath"))
    p = _kiwi_pop_type(img, vn, path)
    p === nothing && return _kiwi_bad("exists", "no population $path on $(img.name) ($vn)")
    # the segmentation in the label: B's and T's "/qc" on one image read identically without it (the
    # first real turns, 4kS67f 2026-09-24 — every chip was "M1a…_005 · qc (flow)")
    p.fixed && return _kiwi_ok("exists", "$(img.name) · $vn · $(p.name)")     # root / derived: always exist
    # the type in the label: one name often exists under several types (a gate and a cluster both
    # called "Population 1" on 4kS67f, live check 2026-09-23) and a chip must say which
    _kiwi_ok("exists", "$(img.name) · $vn · $(p.name) ($(p.popType))")
end

# Integer label/track ids from an `obs/_index`-style column (strings in the h5ad, ints in a ref).
_kiwi_int_ids(col) = Set{Int}(x isa Integer ? Int(x) : parse(Int, string(x)) for x in col
                              if x isa Integer || tryparse(Int, string(x)) !== nothing)

function _kiwi_missing(want, have::Set{Int})::Vector{Int}
    sort!(Int[i for i in want if !(Int(i) in have)])
end

_kiwi_idlist(ids) = length(ids) <= 5 ? join(ids, ", ") : join(ids[1:5], ", ") * " … ($(length(ids)))"

function _kiwi_resolve_cells(puid, ref)
    img = _kiwi_image(puid, ref); img isa String && return _kiwi_bad("exists", img)
    vn, ids = String(_kiwi_get(ref, "valueName")), Int.(_kiwi_get(ref, "labelIds"))
    # only `obs/_index`: `select_cols(["label"])` keeps X, obsm and every other column unread
    df = try
        label_props(img; value_name = vn) |> lp -> filter_rows(lp, ids) |> lp -> select_cols(lp, ["label"]) |> as_df
    catch
        return _kiwi_bad("exists", "no segmentation $vn on $(img.name)")
    end
    miss = _kiwi_missing(ids, _kiwi_int_ids(df.label))
    isempty(miss) || return _kiwi_bad("exists", "cells $(_kiwi_idlist(miss)) not in $vn on $(img.name)")
    _kiwi_ok("exists", "$(length(ids)) cell$(length(ids) == 1 ? "" : "s") · $(img.name) · $vn")
end

function _kiwi_resolve_tracks(puid, ref)
    img = _kiwi_image(puid, ref); img isa String && return _kiwi_bad("exists", img)
    vn, ids = String(_kiwi_get(ref, "valueName")), Int.(_kiwi_get(ref, "trackIds"))
    have = try
        tp = img_track_props_path(img, vn)
        if isfile(tp)       # the per-track table: its `obs/_index` IS track_id — one small read
            _kiwi_int_ids(as_df(select_cols(label_props(tp), ["label"])).label)
        else                # tracked but not yet measured: the `track_id` column of the cell table
            is_tracked(img; value_name = vn) || return _kiwi_bad("exists", "$vn on $(img.name) is not tracked")
            Set{Int}(i for i in _kiwi_int_ids(as_df(select_cols(label_props(img; value_name = vn), ["track_id"])).track_id) if i > 0)
        end
    catch
        return _kiwi_bad("exists", "no segmentation $vn on $(img.name)")
    end
    miss = _kiwi_missing(ids, have)
    isempty(miss) || return _kiwi_bad("exists", "tracks $(_kiwi_idlist(miss)) not in $vn on $(img.name)")
    _kiwi_ok("exists", "$(length(ids)) track$(length(ids) == 1 ? "" : "s") · $(img.name) · $vn")
end

function _kiwi_resolve_viewer(puid, ref)
    img = _kiwi_image(puid, ref); img isa String && return _kiwi_bad("exists", img)
    t, z = _kiwi_get(ref, "t"), _kiwi_get(ref, "z")
    # Extents from the zarr's metadata (`image_geometry` — what GET /api/images/geometry uses; no
    # pixels read), else ccid.json's OME meta if the store can't be opened. If neither knows, the
    # position can't be range-checked and we say so (`format`) rather than pass it silently.
    # An image with no registered pixels (never converted) has nothing to view — a failure, not an
    # unchecked pass (2 of obWDNS's 5 images are like this).
    zp, _, zerr = resolve_image_version(puid, img.uid, nothing)
    zp === nothing && return _kiwi_bad("exists", "$(img.name) has no image data to view ($(something(zerr, "no zarr")))")
    geo = (t === nothing && z === nothing) ? nothing : try image_geometry(zp) catch; nothing end
    for (name, v, key, gkey) in (("t", t, "SizeT", :sizeT), ("z", z, "SizeZ", :sizeZ))
        v === nothing && continue
        n = geo === nothing ? meta_int(img.meta, key) : Int(getproperty(geo, gkey))
        n === nothing && return _kiwi_ok("format", "$(img.name) · $name=$v (extent unknown)")
        v < n || return _kiwi_bad("exists", "$name=$v is outside $(img.name) (0–$(n - 1))")
    end
    where_ = join(("$k=$v" for (k, v) in (("t", t), ("z", z)) if v !== nothing), " ")
    _kiwi_ok("exists", isempty(where_) ? img.name : "$(img.name) · $where_")
end

function _kiwi_resolve_plot(puid, ref)
    pid = String(_kiwi_get(ref, "plotId"))
    e = lock(_PLOTS_LOCK) do
        get(get(_PLOTS_BY_PROJECT, puid, Dict{String,PlotEntry}()), pid, nothing)
    end
    e === nothing && return _kiwi_bad("live", "that plot isn’t open any more — reopen its page to check it")
    c = e.content
    what = string(something(get(c, "yLabel", nothing), get(c, "measure", nothing), ""))
    label = join(filter(!isempty, [isempty(e.title) ? e.family : e.title, what]), " · ")
    r = _kiwi_ok("live", label)
    r["detail"] = _kiwi_plot_detail(puid, c)
    r["route"] = e.route          # where it lives — so a click can reopen its page once it has closed
    r
end

# What a plot shows, in a line: its series, its grouping and its images — from the `content` bag the
# panel publishes (`SummaryPanel` → `series`, `groupBy`, `setUid`/`imageUids`, `statUnit`). A panel
# that publishes less gets a shorter line; "" when it publishes none of these.
function _kiwi_plot_detail(puid, c::AbstractDict)::String
    parts = String[]
    series = get(c, "series", Any[])
    series isa AbstractVector && !isempty(series) && push!(parts, join(string.(series), ", "))
    gb = string(something(get(c, "groupBy", nothing), ""))
    isempty(gb) || push!(parts, "by $gb")
    su = string(something(get(c, "setUid", nothing), ""))
    if !isempty(su)
        set = _valid_asset_id(su) ? (try init_object(puid, su) catch; nothing end) : nothing
        uids = get(c, "imageUids", Any[])
        n = uids isa AbstractVector && !isempty(uids) ? length(uids) : set isa CciaSet ? length(set.image_uids) : 0
        name = set isa CciaSet ? set.name : su
        push!(parts, n > 0 ? "$name · $n image$(n == 1 ? "" : "s")" : name)
        get(c, "statUnit", "") == "image" && push!(parts, "one point per image")
    else
        iu = string(something(get(c, "imageUid", nothing), ""))
        img = isempty(iu) ? nothing : _kiwi_image(puid, Dict("imageUid" => iu))
        img isa CciaImage && push!(parts, img.name)
    end
    join(parts, " · ")
end

# What an open plot shows, as the panel published it (`PlotEntry.summary`) — "" when it isn't open or
# publishes none. Read by the context pack, NOT put in the resolve result: that result is stored per
# cited ref, and a claim table of eight refs would carry eight copies.
function kiwi_plot_summary(puid::AbstractString, plot_id::AbstractString)::String
    e = lock(_PLOTS_LOCK) do
        get(get(_PLOTS_BY_PROJECT, String(puid), Dict{String,PlotEntry}()), String(plot_id), nothing)
    end
    e === nothing ? "" : e.summary
end

function _kiwi_resolve_tile(puid, ref)
    img = _kiwi_image(puid, ref); img isa String && return _kiwi_bad("live", img)
    vn, cell = String(_kiwi_get(ref, "valueName")), String(_kiwi_get(ref, "cellId"))
    t, z = something(_kiwi_get(ref, "t"), -1), something(_kiwi_get(ref, "z"), -1)
    l = lock(_LANDSCAPE_LOCK) do
        get(_LANDSCAPE_BY_KEY, (puid, img.uid, vn, Int(t), Int(z)), nothing)
    end
    (l === nothing || !_landscape_alive(l)) &&
        return _kiwi_bad("live", "no landscape open for $(img.name) at that position")
    tiles = get(l.payload, "tiles", Any[])
    any(tl -> string(get(tl, "id", get(tl, :id, ""))) == cell, tiles) ||
        return _kiwi_bad("live", "no tile $cell in the $(img.name) landscape")
    _kiwi_ok("live", "tile $cell · $(img.name)")
end

function _kiwi_resolve_capture(puid, ref)
    id = String(_kiwi_get(ref, "captureId"))
    _valid_capture_id(id) || return _kiwi_bad("exists", "invalid capture id")
    meta = joinpath(_captures_dir_for_project(puid), id, "meta.json")
    isfile(meta) || return _kiwi_bad("exists", "no capture $id")
    m = try JSON3.read(read(meta, String)) catch; nothing end      # meta only — never the frame PNG
    surface = m === nothing ? "" : string(get(m, :surface, ""))
    _kiwi_ok("exists", isempty(surface) ? id : "$surface capture · $id")
end

function _kiwi_resolve_task(puid, ref)
    fn = String(_kiwi_get(ref, "funName"))
    task = try _task_from_fun_name(fn) catch; return _kiwi_bad("exists", "no task $fn") end
    spec = try Cecelia._task_spec(task) catch; Dict{String,Any}() end
    _kiwi_ok("exists", string(get(spec, "label", fn)))
end

# `<area>.<control>` (a data-guide id) or `nav:/<route>` — the two schemes `resolveAnchor`
# (frontend/src/utils/guideAnchor.ts) understands. Only the browser knows which anchors exist.
const _KIWI_UI_ANCHOR_RE = r"^(nav:/[^\s]*|[A-Za-z0-9_-]+(\.[A-Za-z0-9_-]+)+)$"

function _kiwi_resolve_ui(puid, ref)
    a = String(_kiwi_get(ref, "anchor"))
    occursin(_KIWI_UI_ANCHOR_RE, a) || return _kiwi_bad("format", "not a UI anchor (area.control or nav:/route)")
    _kiwi_ok("format", a)
end

function _kiwi_resolve_blackboard(puid, ref)
    id = String(_kiwi_get(ref, "entryId"))
    _valid_bb_entry_id(id) || return _kiwi_bad("exists", "invalid Blackboard entry id")
    meta = _read_bb_meta(puid, id)
    meta === nothing && return _kiwi_bad("exists", "no Blackboard entry $id")
    v = _kiwi_get(ref, "version")
    if v !== nothing
        current = Int(something(tryparse(Int, string(get(meta, "current", 0))), 0))
        (Int(v) == current || Int(v) in _bb_snapshot_versions(puid, id)) ||
            return _kiwi_bad("exists", "entry $id has no version $v")
    end
    title = string(get(meta, "title", id))
    _kiwi_ok("exists", v === nothing ? title : "$title (v$v)")
end

const _KIWI_RESOLVERS = Dict{String,Function}(
    "project" => _kiwi_resolve_project, "set" => _kiwi_resolve_set, "image" => _kiwi_resolve_image,
    "population" => _kiwi_resolve_population, "cells" => _kiwi_resolve_cells,
    "tracks" => _kiwi_resolve_tracks, "viewer" => _kiwi_resolve_viewer, "plot" => _kiwi_resolve_plot,
    "tile" => _kiwi_resolve_tile, "capture" => _kiwi_resolve_capture, "task" => _kiwi_resolve_task,
    "ui" => _kiwi_resolve_ui, "blackboard" => _kiwi_resolve_blackboard,
)
@assert Set(keys(_KIWI_RESOLVERS)) == Set(KIWI_REF_KINDS) "every schema kind needs a resolver"

"""
    resolve_kiwi_ref(project_uid, ref) -> Dict{ok, check, label, error}

Shape first (`kiwi_ref_shape_error`), then the kind's resolver. Never throws: an unexpected failure
in a reader is reported as that ref's error, so one bad ref can't sink a reply's other refs.
"""
function resolve_kiwi_ref(project_uid::AbstractString, ref)::Dict{String,Any}
    e = kiwi_ref_shape_error(ref)
    isempty(e) || return _kiwi_bad("shape", e)
    kind = string(_kiwi_get(ref, "kind"))
    try
        _KIWI_RESOLVERS[kind](String(project_uid), ref)
    catch err
        _kiwi_bad("exists", "could not check this $kind ref: $(sprint(showerror, err))")
    end
end

# ── A population's cells — what pointing at one shows ─────────────────────────────────────────────────
#
#   POST /api/kiwi/refs/cells  {projectUid, ref: <population KiwiRef>, limit?}
#     → {labelIds, total, truncated, popType}
#
# Clicking a population ref outlines its cells in the viewer (`PickHighlight`, the same outline the
# plots' brushing draws). It used to open the image's gating page, where no plot shows a given
# population — the user got a root scatter on arbitrary axes. Reads through `pop_df` (the sanctioned
# cell-data entry), at cell granularity, ids only.
const _KIWI_CELLS_LIMIT = 20000

function kiwi_population_cells(puid::AbstractString, ref; limit::Integer = _KIWI_CELLS_LIMIT)
    string(_kiwi_get(ref, "kind", "")) == "population" || return "not a population ref"
    img = _kiwi_image(String(puid), ref); img isa String && return img
    vn, path = String(_kiwi_get(ref, "valueName")), String(_kiwi_get(ref, "popPath"))
    p = _kiwi_pop_type(img, vn, path)
    p === nothing && return "no population $path on $(img.name) ($vn)"
    df = try
        pop_df(img, p.popType, [path]; value_name = vn, granularity = :cell, include_obs = false, include_x = false)
    catch e
        return "could not read $path: $(sprint(showerror, e))"
    end
    "label" in names(df) || return "no cell ids for $path"
    ids = sort!(unique(Int[Int(l) for l in df.label if !ismissing(l) && l > 0]))
    total = length(ids)
    (; labelIds = total > limit ? ids[1:limit] : ids, total, truncated = total > limit, popType = p.popType)
end

function api_kiwi_refs_cells(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    puid = _wstr(body, :projectUid)
    (isempty(puid) || !_valid_asset_id(puid)) && return 400, JSON3.write((; error = "projectUid required"))
    isfile(joinpath(projects_dir(), puid, "project.json")) ||
        return 404, JSON3.write((; error = "no project $puid"))
    ref = get(body, :ref, nothing)
    ref isa AbstractDict || return 400, JSON3.write((; error = "ref required"))
    err = kiwi_ref_shape_error(ref)
    isempty(err) || return 400, JSON3.write((; error = err))
    lim = get(body, :limit, _KIWI_CELLS_LIMIT)
    out = kiwi_population_cells(puid, ref; limit = lim isa Integer ? clamp(lim, 1, 200_000) : _KIWI_CELLS_LIMIT)
    out isa String && return 404, JSON3.write((; error = out))
    200, JSON3.write(out)
end

function api_kiwi_refs_resolve(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    puid = _wstr(body, :projectUid)
    (isempty(puid) || !_valid_asset_id(puid)) && return 400, JSON3.write((; error = "projectUid required"))
    isfile(joinpath(projects_dir(), puid, "project.json")) ||
        return 404, JSON3.write((; error = "no project $puid"))
    refs = get(body, :refs, nothing)
    refs isa AbstractVector || return 400, JSON3.write((; error = "refs must be a list"))
    200, JSON3.write((; results = [resolve_kiwi_ref(puid, r) for r in refs]))
end
