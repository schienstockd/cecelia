# Fixtures by property — `fixture_for(:cells, :calibrated; need = "<id>")`.
#
# test-data/fixtures.toml says what each committed fixture can prove; test-data/NEEDS.toml lists what
# the tests need and no fixture has. A test that depends on a property of the data asks for it here
# and gets a fixture that has it, or `nothing` — never the nearest stand-in. Shared by the package and
# API suites (included from both runtests.jl). Docs: docs/DEV.md → *Ask for properties, not a path*.
# The Python twin is python/cecelia/tests/fixture_registry.py; both read the same two files.

const _TEST_DATA_ROOT = normpath(joinpath(@__DIR__, "..", "..", "test-data"))

"""One registered fixture: absolute paths plus what fixtures.toml says it has."""
struct RegisteredFixture
    uid::String
    project::String                 # project dir (absolute), the first component of `dir`/`image_dir`
    dir::Union{Nothing,String}      # where ccid.json lives (absolute), nothing for an image-only fixture
    image_dir::Union{Nothing,String}
    value_name::Union{Nothing,String}
    technology::String
    properties::Set{Symbol}
    meta::Dict{String,Any}          # a derived fixture's ccid.json `meta` overrides; empty otherwise
end

_is_derived(fx::RegisteredFixture) = !isempty(fx.meta)

"""
    materialise(fx) -> RegisteredFixture

A derived fixture (`derive = "<uid>"` in fixtures.toml) as a fresh temp copy of its base's project
with the `meta` overrides written into the image's ccid.json — so a calibrated variant of real cells
costs no committed bytes. A fresh copy per call: the test may write into it. Anything else is
returned as is (committed, read-only: copy it before writing).
"""
function materialise(fx::RegisteredFixture)
    _is_derived(fx) || return fx
    root = mktempdir()
    proj = joinpath(root, basename(fx.project))
    cp(fx.project, proj)
    re(p) = p === nothing ? nothing : joinpath(proj, relpath(p, fx.project))
    dir = re(fx.dir)
    ccid_path = joinpath(dir, "ccid.json")
    ccid = JSON3.read(read(ccid_path, String), Dict{String,Any})
    meta = Dict{String,Any}(String(k) => v for (k, v) in get(ccid, "meta", Dict{String,Any}()))
    merge!(meta, fx.meta)
    ccid["meta"] = meta
    write(ccid_path, JSON3.write(ccid))
    RegisteredFixture(fx.uid, proj, dir, re(fx.image_dir), fx.value_name, fx.technology, fx.properties, fx.meta)
end

"""The cell table of a fixture with `:cells` (absolute path)."""
fixture_table(fx::RegisteredFixture) = joinpath(fx.dir, "labelProps", fx.value_name * ".h5ad")

_registry_projects_root() = get(ENV, "CECELIA_TEST_DATA", joinpath(_TEST_DATA_ROOT, "projects"))

fixture_vocabulary() = Dict(Symbol(k) => (checked = Bool(v[1]), meaning = String(v[2]))
                            for (k, v) in Cecelia.TOML.parsefile(joinpath(_TEST_DATA_ROOT, "fixtures.toml"))["vocabulary"])

function registered_fixtures()::Vector{RegisteredFixture}
    raw = Cecelia.TOML.parsefile(joinpath(_TEST_DATA_ROOT, "fixtures.toml"))["fixtures"]
    root = _registry_projects_root()
    abs(p) = p === nothing ? nothing : joinpath(root, split(p, '/')...)
    out = RegisteredFixture[]
    for uid in sort!(collect(keys(raw)))     # TOML tables are unordered; sort for a stable pick
        e = raw[uid]
        if haskey(e, "derive")               # paths, value name and technology come from the base
            b = raw[e["derive"]]
            e = merge(b, e)
        end
        rel = something(get(e, "dir", nothing), get(e, "image_dir", nothing))
        push!(out, RegisteredFixture(uid, joinpath(root, first(split(rel, '/'))),
            abs(get(e, "dir", nothing)), abs(get(e, "image_dir", nothing)),
            get(e, "value_name", nothing), e["technology"], Set(Symbol.(e["properties"])),
            haskey(e, "derive") ? Dict{String,Any}(String(k) => string(v) for (k, v) in e["meta"]) : Dict{String,Any}()))
    end
    out
end

fixture_needs() = get(Cecelia.TOML.parsefile(joinpath(_TEST_DATA_ROOT, "NEEDS.toml")), "needs", Dict{String,Any}())

const _WARNED_NEEDS = Set{String}()

"""
    fixture_for(props::Symbol...; technology = nothing, need = nothing) -> RegisteredFixture | nothing

A registered fixture with every property in `props` (and that technology, if given), or `nothing`.
The first match by uid wins; a derived fixture comes back as a fresh temp copy (`materialise`).
With no match, `need` must name an entry in test-data/NEEDS.toml: the test skips and the warning
says which request it waits on. No match and no `need` is an error — file the request, don't
reach for a fixture that lacks the property.
"""
function fixture_for(props::Symbol...; technology = nothing, need = nothing)
    vocab = fixture_vocabulary()
    bad = [p for p in props if !haskey(vocab, p)]
    isempty(bad) || error("fixture_for: unknown properties $(bad) — see test-data/fixtures.toml → [vocabulary]")
    need === nothing || haskey(fixture_needs(), need) ||
        error("fixture_for: need \"$need\" is not in test-data/NEEDS.toml — add it there")
    for fx in registered_fixtures()
        all(in(fx.properties), props) || continue
        technology === nothing || fx.technology == technology || continue
        # present on disk (a partial checkout); an absent one is skipped like any missing fixture
        isdir(something(fx.dir, fx.image_dir)) && return materialise(fx)
    end
    need === nothing && error("""fixture_for$(props): no registered fixture has these properties.
        Don't use one that lacks them: add a request to test-data/NEEDS.toml and pass `need = "<id>"`.""")
    if !(need in _WARNED_NEEDS)
        push!(_WARNED_NEEDS, need)
        @warn "FIXTURE NEEDED — tests waiting on test-data/NEEDS.toml → $need are SKIPPED " *
              "(no fixture has $(join(props, ", "))$(technology === nothing ? "" : ", technology $technology"))."
    end
    nothing
end

# ── fact checks: re-derive each `checked` property from the files ─────────────────────────────

# Axis name → (size, scale) of an OME-ZARR store's level 0, through the canonical NGFF readers
# (app/src/model/zarr_store.jl): flat or bf2raw-wrapped, zarr v2 or v3.
function _store_axes(store::AbstractString)
    g = Cecelia.series_base(store)
    ms = Cecelia.ngff_multiscales(g)
    ms === nothing && return nothing
    ds = ms[1][:datasets][1]
    meta = Cecelia.zarr_array_meta(joinpath(g, String(ds[:path])))
    meta === nothing && return nothing
    names = [String(a[:name]) for a in ms[1][:axes]]
    scale = Float64[]
    for t in get(ds, :coordinateTransformations, [])
        t[:type] == "scale" && (scale = Float64.(collect(t[:scale])))
    end
    Dict(n => (size = Int(meta[:shape][i]), scale = isempty(scale) ? 1.0 : scale[i]) for (i, n) in enumerate(names))
end

_distinct(df, col) = col in names(df) ? length(unique(skipmissing(df[!, col]))) : 0

"""
    fixture_facts(fx) -> Dict{Symbol,Bool}

Every `checked` property, re-derived from the fixture's files. A property it cannot judge on this
fixture (a cell-table property on an image-only fixture) is false.
"""
function fixture_facts(fx::RegisteredFixture)::Dict{Symbol,Bool}
    f = Dict{Symbol,Bool}()
    store = fx.image_dir === nothing ? nothing : joinpath(fx.image_dir, "ccidImage.ome.zarr")
    f[Symbol("image-store")] = store !== nothing && isdir(store)
    f[Symbol("label-store")] = fx.dir !== nothing && isdir(joinpath(fx.dir, "labels")) &&
                       any(endswith(".zarr"), readdir(joinpath(fx.dir, "labels")))
    img = fx.dir === nothing ? nothing : Cecelia._load_image(fx.dir)
    df = nothing
    if fx.dir !== nothing && fx.value_name !== nothing && isfile(fixture_table(fx))
        df = Cecelia.as_df(Cecelia.label_props(fixture_table(fx)))
    end
    f[:cells] = df !== nothing && size(df, 1) > 0
    axes = f[Symbol("image-store")] ? _store_axes(store) : nothing
    if df !== nothing
        # a z axis in the table makes it a 3D measurement, even if every cell sits on one plane
        nt, nz = _distinct(df, "centroid_t"), ("centroid_z" in names(df) ? 2 : 0)
    elseif axes !== nothing
        nt = haskey(axes, "t") ? axes["t"].size : 0
        nz = haskey(axes, "z") ? axes["z"].size : 0
    else
        nt = nz = 0
    end
    f[:movie] = nt > 1
    f[:still] = (df !== nothing || axes !== nothing) && nt <= 1
    f[Symbol("3d")] = nz > 1
    f[Symbol("2d")] = (df !== nothing || axes !== nothing) && nz <= 1
    f[:tracked] = df !== nothing && "track_id" in names(df)
    f[Symbol("track-table")] = fx.dir !== nothing && fx.value_name !== nothing &&
                       isfile(joinpath(fx.dir, "labelProps", fx.value_name * "__tracks.h5ad"))
    f[:regions] = df !== nothing && any(startswith("regions."), names(df))
    # calibration: ccid meta when there is one (what the app reads), else the store's NGFF scale
    # (not `img_is_calibrated`, which accepts 1.0: here 1.0 is exactly what can't show a missing conversion)
    if img !== nothing
        sx, sy = (tryparse(Float64, string(get(img.meta, k, ""))) for k in ("PhysicalSizeX", "PhysicalSizeY"))
        f[:calibrated] = sx !== nothing && sy !== nothing && sx != 1.0 && sy != 1.0
    else
        f[:calibrated] = axes !== nothing && haskey(axes, "x") && haskey(axes, "y") &&
                         axes["x"].scale != 1.0 && axes["y"].scale != 1.0
    end
    chans = img === nothing ? String[] : something(Cecelia.channel_names(img), String[])
    f[Symbol("marker-names")] = !isempty(chans) && !any(c -> occursin(r"^(ch|c|channel)[ _-]?\d+$"i, c), chans)
    f[:multiplex] = length(chans) >= 10    # MULTIPLEX_MIN in frontend/src/utils/setRecipe.ts
    # every image under the project that has a cell table / every cell table on this image
    tables = String[]
    for (d, _, files) in walkdir(fx.project), fn in files
        endswith(fn, ".h5ad") && basename(d) == "labelProps" && !occursin("__", fn) && push!(tables, joinpath(d, fn))
    end
    f[Symbol("multi-image")] = length(unique(basename(dirname(dirname(t))) for t in tables)) > 1
    f[Symbol("multi-segmentation")] = fx.dir !== nothing && count(t -> dirname(dirname(t)) == fx.dir, tables) > 1
    f
end
