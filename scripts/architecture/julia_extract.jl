# Generates docs/architecture/julia/*.md from the loaded `Cecelia` package — no hand-drawn boxes.
#
#   pixi run arch-diagrams          (runs this + the Python and Vue extractors)
#
# Two views:
#   interfaces.md — every abstract type in Cecelia, the functions that form its contract (a method on
#                   the abstract type and/or on a concrete subtype) and which subtypes override them;
#                   plus the Project/Set/Image data model from the structs' own fields.
#   areas.md      — which area of the code calls into which. The backend is one module, so the edges
#                   come from name references: a call `foo(` in a file of area A to a function whose
#                   methods are defined in area B is an A → B edge. Areas: julia_areas.toml.
#
# Introspection (subtypes, methods, fieldnames) is exact for what it sees. The name-reference graph
# is static and approximate: a local variable that shadows a package function and is called would
# count, and a call through a variable (`f = foo; f(x)`) would not. Good enough for a map, not for
# proofs.

using InteractiveUtils
using TOML
using Cecelia

const C = Cecelia
const REPO = normpath(joinpath(@__DIR__, "..", ".."))
const OUT = joinpath(REPO, "docs", "architecture", "julia")

rel(p) = relpath(normpath(string(p)), REPO)

# ── areas ─────────────────────────────────────────────────────────────────────

const AREA_SPEC = TOML.parsefile(joinpath(@__DIR__, "julia_areas.toml"))["areas"]
# TOML tables are unordered; first-match-wins needs the file order, so re-read keys in order.
const AREA_ORDER = let ks = String[]
    for ln in eachline(joinpath(@__DIR__, "julia_areas.toml"))
        m = match(r"^\"([^\"]+)\"\s*=", ln)
        m === nothing || push!(ks, m[1])
    end
    ks
end

function area_of(path::AbstractString)
    for a in AREA_ORDER, pat in AREA_SPEC[a]
        (endswith(pat, "/") || !endswith(pat, ".jl")) ? (startswith(path, pat) && return a) :
                                                        (path == pat && return a)
    end
    return nothing
end

source_files() = sort!(vcat(
    [rel(joinpath(r, f)) for d in ("app/src", "api/src") for (r, _, fs) in walkdir(joinpath(REPO, d))
     for f in fs if endswith(f, ".jl")]))

# ── what Cecelia defines, and where ───────────────────────────────────────────

owned(x) = try parentmodule(x) === C catch; false end

const FUNCS = Dict{Symbol,Function}()
const TYPES = Dict{Symbol,DataType}()
for n in names(C; all = true)
    isdefined(C, n) || continue
    x = getfield(C, n)
    startswith(string(n), "#") && continue
    if x isa Function && !(x isa Type) && owned(x)
        FUNCS[n] = x
    elseif x isa DataType && owned(x)
        TYPES[n] = x
    end
end

"Majority area of a function's methods (only methods defined in this repo)."
function def_area(f)
    counts = Dict{String,Int}()
    for m in methods(f)
        p = rel(m.file)
        a = area_of(p)
        a === nothing && continue
        counts[a] = get(counts, a, 0) + 1
    end
    isempty(counts) ? nothing : first(sort(collect(counts); by = kv -> -kv[2]))[1]
end

type_file(T) = (ms = methods(T); isempty(ms) ? nothing : rel(first(ms).file))

# ── area graph from name references ───────────────────────────────────────────

function strip_comments(src::String)
    src = replace(src, r"#=.*?=#"s => "")
    src = replace(src, r"\"\"\".*?\"\"\""s => "\"\"")
    join((replace(l, r"(^|\s)#.*$" => "") for l in split(src, '\n')), '\n')
end

function area_graph()
    fn_area = Dict{String,String}()
    for (n, f) in FUNCS
        length(string(n)) >= 4 || continue
        a = def_area(f); a === nothing || (fn_area[string(n)] = a)
    end
    ty_area = Dict{String,String}()
    for (n, T) in TYPES
        p = type_file(T); p === nothing && continue
        a = area_of(p); a === nothing || (ty_area[string(n)] = a)
    end
    edges = Dict{Tuple{String,String},Set{String}}()
    loc = Dict{String,Int}()
    nfiles = Dict{String,Int}()
    unmapped = String[]
    for p in source_files()
        a = area_of(p)
        if a === nothing
            push!(unmapped, p); continue
        end
        src = read(joinpath(REPO, p), String)
        loc[a] = get(loc, a, 0) + count(==('\n'), src)
        nfiles[a] = get(nfiles, a, 0) + 1
        code = strip_comments(src)
        for m in eachmatch(r"(?<![\w.:])(?:Cecelia\.)?([A-Za-z_][A-Za-z0-9_]*!?)(\.?\()?", code)
            name, called = m[1], m[2] !== nothing
            b = called ? get(fn_area, name, get(ty_area, name, nothing)) : get(ty_area, name, nothing)
            (b === nothing || b == a) && continue
            push!(get!(edges, (a, b), Set{String}()), name)
        end
    end
    return edges, loc, nfiles, unmapped
end

# ── interfaces ────────────────────────────────────────────────────────────────

sig_params(m) = (s = Base.unwrap_unionall(m.sig); s isa DataType ? collect(s.parameters[2:end]) : Any[])

function leaf_subtypes(A)
    out = DataType[]
    for S in subtypes(A)
        S isa DataType || continue
        isabstracttype(S) ? append!(out, leaf_subtypes(S)) : push!(out, S)
    end
    out
end

"For abstract type A: fn => (has_default, overriders::Set{DataType}, file of the default)"
function contract(A, leaves)
    leafset = Set(leaves)
    out = Dict{String,Tuple{Bool,Set{DataType},Union{String,Nothing}}}()
    for (n, f) in FUNCS
        f isa Type && continue
        dflt, deffile, ov = false, nothing, Set{DataType}()
        for m in methods(f)
            for P in sig_params(m)
                P = P isa Type ? P : continue
                if P === A
                    dflt = true; deffile = rel(m.file)
                elseif P isa DataType && P in leafset
                    push!(ov, P)
                end
            end
        end
        (dflt || !isempty(ov)) || continue
        out[string(n)] = (dflt, ov, deffile)
    end
    out
end

function abstract_file(A)
    pat = Regex("^\\s*abstract type \\Q$(nameof(A))\\E\\b")
    for p in source_files(), (i, l) in enumerate(eachline(joinpath(REPO, p)))
        occursin(pat, l) && return "$p:$i"
    end
    "?"
end

short(T) = replace(string(T), "Cecelia." => "")
mm(s) = replace(replace(replace(s, "{" => "~"), "}" => "~"), r"~([^~]*)~([^~]*)~" => s"~\1\2~")

function group_of(T)
    p = type_file(T)
    p === nothing && return "?"
    parts = split(p, '/')
    i = findfirst(==("tasks"), parts)
    (i !== nothing && i < length(parts) - 1) ? "tasks/" * parts[i+1] : dirname(p)
end

function interfaces_md(io)
    println(io, "# Julia — abstract types and their contracts\n")
    println(io, GENERATED)
    println(io, """
A Julia "interface" is an abstract type plus the functions its subtypes are expected to implement
(multiple dispatch — there are no classes). For each abstract type in `Cecelia`:

- **contract** — functions with a method specialised on a concrete subtype. *Kind*: **required** (no
  default, every subtype implements it), *optional* (a default on the abstract type, some subtypes
  override — note a default can itself raise, as `_run_task`'s does), or *partial* (no default and only
  some subtypes: a subtype-specific function, not a contract).
- **operations** — functions that take the abstract type but that no subtype overrides: the shared
  API every subtype gets for free.

Diagram rule: each subtype appears with the contract functions it overrides. More than 10 subtypes
are grouped by the directory they are defined in.
""")
    absts = sort([T for T in values(TYPES) if isabstracttype(T)]; by = T -> (-length(leaf_subtypes(T)), short(T)))
    for A in absts
        leaves = leaf_subtypes(A)
        con = contract(A, leaves)
        isempty(leaves) && isempty(con) && continue
        overridden = sort([k for (k, v) in con if !isempty(v[2])])
        shared = sort([k for (k, v) in con if isempty(v[2])])
        a = short(A)
        println(io, "## `$a`\n")
        println(io, "Defined in `$(abstract_file(A))` · $(length(leaves)) concrete subtypes · $(length(overridden)) contract functions · $(length(shared)) shared operations\n")
        println(io, "```mermaid\nclassDiagram")
        println(io, "  class $a {\n    <<abstract>>")
        for f in overridden; println(io, "    +$f()"); end
        println(io, "  }")
        if length(leaves) <= 10
            for S in sort(leaves; by = short)
                s = short(S)
                fs = sort([f for f in overridden if S in con[f][2]])
                println(io, "  class $s {"); for f in fs; println(io, "    +$f()"); end; println(io, "  }")
                println(io, "  $a <|-- $s")
            end
        else
            groups = Dict{String,Vector{DataType}}()
            for S in leaves; push!(get!(groups, group_of(S), DataType[]), S); end
            for (g, ss) in sort(collect(groups); by = first)
                id = replace(g, r"[^A-Za-z0-9]" => "_")
                println(io, "  class $id[\"$g ($(length(ss)))\"] {")
                for S in sort(ss; by = short); println(io, "    $(short(S))"); end
                println(io, "  }")
                println(io, "  $a <|-- $id")
            end
        end
        println(io, "```\n")
        if !isempty(overridden)
            println(io, "| Contract function | Kind | Implemented by |\n|---|---|---|")
            for f in overridden
                d, ov, _ = con[f]
                names_ = sort(short.(collect(ov)))
                shown = length(names_) > 8 ? join(names_[1:8], ", ") * ", … ($(length(names_)) of $(length(leaves)))" :
                                             join(names_, ", ") * (length(leaves) > 8 ? " ($(length(names_)) of $(length(leaves)))" : "")
                kind = d ? "optional — default on `$a`" :
                       length(ov) == length(leaves) ? "**required** — no default, every subtype has one" :
                       "partial — no default, only these subtypes (others: MethodError)"
                println(io, "| `$f` | $kind | $shown |")
            end
            println(io)
        end
        isempty(shared) || println(io, "Shared operations: ", join(("`$f`" for f in shared), ", "), "\n")
    end
end

function datamodel_md(io)
    println(io, "# Julia — the data model\n")
    println(io, GENERATED)
    println(io, """
The three persisted objects, from their struct definitions. Fields starting with `_` (and `root`)
are runtime-only, not written to `ccid.json` / `project.json`. On-disk shape and the versioned-field
convention: [`docs/OBJECTMODEL.md`](../../OBJECTMODEL.md).
""")
    roots = [C.CciaProject, C.CciaSet, C.CciaImage]
    println(io, "```mermaid\nclassDiagram")
    for T in roots
        println(io, "  class $(short(T)) {")
        for (f, FT) in zip(fieldnames(T), fieldtypes(T))
            t = short(FT)
            t = length(t) > 40 ? first(split(t, "{")) * "{…}" : t
            println(io, "    $(mm(t)) $f")
        end
        println(io, "  }")
    end
    for T in roots, (f, FT) in zip(fieldnames(T), fieldtypes(T)), R in roots
        R === T && continue
        (FT === R || FT === Vector{R}) && println(io, "  $(short(T)) \"1\" --> \"$(FT === R ? "1" : "*")\" $(short(R)) : $f")
    end
    println(io, "```\n")
    println(io, "Functions taking each object as their first argument, by area — where the code that works on it lives:\n")
    println(io, "| Area | " * join(("`$(short(T))`" for T in roots), " | ") * " |")
    println(io, "|---|" * repeat("---|", length(roots)))
    counts = Dict{Tuple{String,DataType},Int}()
    for (n, f) in FUNCS, m in methods(f)
        ps = sig_params(m); isempty(ps) && continue
        P = ps[1]
        P in roots || continue
        a = something(area_of(rel(m.file)), "?")
        counts[(a, P)] = get(counts, (a, P), 0) + 1
    end
    for a in sort(unique(first.(collect(keys(counts)))); by = a -> -sum(get(counts, (a, T), 0) for T in roots))
        println(io, "| $a | " * join((string(get(counts, (a, T), "")) for T in roots), " | ") * " |")
    end
    println(io)
end

function areas_md(io)
    edges, loc, nfiles, unmapped = area_graph()
    println(io, "# Julia — areas and who calls whom\n")
    println(io, GENERATED)
    println(io, """
The backend is **one module** (`Cecelia`, every file `include`d), so there is no module graph. These
areas are a curated grouping of files (`scripts/architecture/julia_areas.toml`); the arrows are from
the code: `A → B (n)` means files in A call `n` distinct functions (or name `n` types) defined in B.
Static name matching — approximate, see the script header.

Diagram rule: the strongest $(MAX_EDGES) arrows per diagram (ties at the cut-off included); every
count is in the full matrix below.
""")
    pkg = [a for a in AREA_ORDER if !startswith(a, "api:") && haskey(loc, a)]
    api = [a for a in AREA_ORDER if startswith(a, "api:") && haskey(loc, a)]
    id(a) = replace(a, r"[^A-Za-z0-9]" => "_")
    label(a) = "$a<br/><small>$(nfiles[a]) files · $(round(loc[a]/1000; digits=1))k lines</small>"
    for (title, srcs, dsts, note) in (
            ("Inside the package (`app/src`)", pkg, pkg, ""),
            ("API server → package (`api/src` → `app/src`)", api, pkg,
             "The API server's own files are grouped on the left; arrows into the package only."))
        println(io, "## $title\n")
        isempty(note) || println(io, note, "\n")
        println(io, "```mermaid\nflowchart LR")
        used = Set{String}()
        lines = String[]
        cand = sort([kv for kv in edges if kv[1][1] in srcs && kv[1][2] in dsts]; by = kv -> -length(kv[2]))
        cut = isempty(cand) ? 0 : length(cand[min(MAX_EDGES, length(cand))][2])
        for ((a, b), ns) in cand
            length(ns) >= cut || continue
            push!(lines, "  $(id(a)) -->|$(length(ns))| $(id(b))"); push!(used, a, b)
        end
        if srcs === api
            println(io, "  subgraph API[\"api/src\"]"); for a in api; a in used && println(io, "    $(id(a))[\"$(label(a))\"]"); end; println(io, "  end")
            println(io, "  subgraph PKG[\"app/src — Cecelia.jl\"]"); for a in pkg; a in used && println(io, "    $(id(a))[\"$(label(a))\"]"); end; println(io, "  end")
        else
            for a in pkg; a in used && println(io, "  $(id(a))[\"$(label(a))\"]"); end
        end
        foreach(l -> println(io, l), lines)
        println(io, "```\n")
    end
    println(io, "## Two-way dependencies between package areas\n")
    println(io, "Both directions present (any n). In one module these are legal, but each pair is two areas that cannot be understood apart.\n")
    println(io, "| Areas | → | ← |\n|---|---|---|")
    for (i, a) in enumerate(pkg), b in pkg[i+1:end]
        x, y = get(edges, (a, b), Set{String}()), get(edges, (b, a), Set{String}())
        (isempty(x) || isempty(y)) && continue
        ex(s) = "$(length(s)): " * join(("`$n`" for n in first(sort(collect(s)), 4)), ", ") * (length(s) > 4 ? ", …" : "")
        println(io, "| $a ↔ $b | $(ex(x)) | $(ex(y)) |")
    end
    println(io)
    println(io, "## Full matrix (rows call columns)\n")
    println(io, "| | " * join(pkg, " | ") * " |")
    println(io, "|---|" * repeat("---|", length(pkg)))
    for a in vcat(pkg, api)
        println(io, "| **$a** | " * join((a == b ? "·" : (n = length(get(edges, (a, b), Set{String}())); n == 0 ? "" : string(n)) for b in pkg), " | ") * " |")
    end
    println(io)
    if !isempty(unmapped)
        println(io, "## Unmapped files\n\nAdd these to `julia_areas.toml`:\n")
        foreach(p -> println(io, "- `$p`"), unmapped); println(io)
    end
    return unmapped
end

const MAX_EDGES = 25
const GENERATED = "> **Generated** by `scripts/architecture/julia_extract.jl` — do not edit; run `pixi run arch-diagrams`.\n"

mkpath(OUT)
open(io -> interfaces_md(io), joinpath(OUT, "interfaces.md"), "w")
open(io -> datamodel_md(io), joinpath(OUT, "datamodel.md"), "w")
unmapped = open(io -> areas_md(io), joinpath(OUT, "areas.md"), "w")
isempty(unmapped) || (println(stderr, "unmapped Julia files (add to julia_areas.toml):"); foreach(p -> println(stderr, "  ", p), unmapped))
println("wrote ", rel(OUT))
