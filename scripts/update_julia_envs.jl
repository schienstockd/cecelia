# scripts/update_julia_envs.jl — `Pkg.update()` the three Julia envs and print what moved, as markdown.
#
# WHY ONE SCRIPT FOR ALL THREE. `api/` and `pluto/` path-source `Cecelia` from `../app`
# (`[sources]`), so their manifests carry app's deps too. Updating them together, app first, keeps
# the three manifests agreeing. Dependabot can't do this: it handles each directory separately and
# opens one PR per dependency. See docs/todo/DEPS_UPDATES_PLAN.md (Decision 2).
#
# Moves versions within `[compat]` only. Raising a compat bound is a deliberate manual edit.
#
# Also lists what stays behind: direct deps `[compat]` holds back, and packages the three envs
# carry at different versions.
#
# Usage: `pixi run update-julia` (or `julia --startup-file=no scripts/update_julia_envs.jl`). The
# markdown goes to stdout and Pkg's own log goes to stderr, so `> julia-diff.md` captures just the
# summary. `.github/workflows/update-deps.yml` uses it as part of the PR body.

using Pkg, TOML

const REPO_ROOT = normpath(joinpath(@__DIR__, ".."))
const ENVS = ["app", "api", "pluto"]   # app first: the other two path-source it

"""
    manifest_versions(path) -> Dict{String,String}

Package name → version for every registry package in a Manifest.toml (format 2). Stdlibs and
path/repo deps carry no `version` key, or one that doesn't change through `Pkg.update`, so they're
skipped.
"""
function manifest_versions(path::AbstractString)
    out = Dict{String,String}()
    isfile(path) || return out
    for (name, entries) in get(TOML.parsefile(path), "deps", Dict())
        for e in entries
            haskey(e, "version") && !haskey(e, "path") && (out[name] = e["version"])
        end
    end
    out
end

"""
    breaking(a, b) -> Bool

Semver-breaking move: the major version changed, or, for a 0.x package, the minor did (0.12 → 0.13
may break, by the 0.x rule Julia's resolver itself uses).
"""
function breaking(a::AbstractString, b::AbstractString)
    va, vb = VersionNumber(a), VersionNumber(b)
    va.major != vb.major || (va.major == 0 && va.minor != vb.minor)
end

"""
    change_kind(a, b) -> String

"⬇ downgrade" when the resolver moved a package back (a newer version of something else holds it
down), "⚠ breaking" for a semver-breaking move up, "" otherwise. Both kinds are the ones to review.
"""
function change_kind(a::AbstractString, b::AbstractString)
    (isempty(a) || isempty(b)) && return ""
    VersionNumber(b) < VersionNumber(a) && return "⬇ downgrade"
    breaking(a, b) ? "⚠ breaking" : ""
end

"""
    diff_rows(old, new, direct) -> (rows, flagged)

Markdown table rows for every package added, removed, or changed between two version maps, plus the
downgrades and semver-breaking moves. Direct deps (listed in the env's Project.toml) are bolded so the
ones we chose stand out from transitive ones.
"""
function diff_rows(old::Dict, new::Dict, direct::Set{String})
    rows, flagged = String[], String[]
    for name in sort!(collect(union(keys(old), keys(new))))
        a, b = get(old, name, ""), get(new, name, "")
        a == b && continue
        label = name in direct ? "**$name**" : name
        kind = change_kind(a, b)
        isempty(kind) || push!(flagged, "$kind $label $a → $b")
        push!(rows, "| $label | $(isempty(a) ? "—" : a) | $(isempty(b) ? "—" : b) | $kind |")
    end
    rows, flagged
end

"""
    drift(versions) -> Vector{String}

Packages that more than one env carries at different versions. `api/` and `pluto/` load `app/`'s
code, so a package at a different version there runs Cecelia against a version `app/`'s tests never
saw.
"""
function drift(versions::Dict{String,Dict{String,String}})
    out = String[]
    for name in sort!(collect(keys(versions["app"])))
        seen = [(env, versions[env][name]) for env in ENVS if haskey(versions[env], name)]
        length(unique(last.(seen))) > 1 &&
            push!(out, "$name — " * join(("`$e/` $v" for (e, v) in seen), ", "))
    end
    out
end

function main()
    ENV["JULIA_PKG_PRECOMPILE_AUTO"] = "0"   # resolving only — nothing here loads the packages
    isempty(Pkg.Registry.reachable_registries()) && Pkg.Registry.add("General")

    sections, flagged, held = String[], String[], String[]
    versions = Dict{String,Dict{String,String}}()
    for env in ENVS
        dir = joinpath(REPO_ROOT, env)
        manifest = joinpath(dir, "Manifest.toml")
        old = manifest_versions(manifest)
        Pkg.activate(dir; io = stderr)
        Pkg.update(; io = stderr)
        new = versions[env] = manifest_versions(manifest)
        # Direct deps with a newer release the resolver couldn't take. ⌅ = blocked by [compat] —
        # ours or a dependency's. Only a deliberate compat edit moves these.
        buf = IOBuffer()
        Pkg.status(; outdated = true, io = buf)
        for line in eachline(IOBuffer(take!(buf)))
            occursin('⌅', line) && push!(held, "`$env/`: " * strip(replace(line, '⌅' => "")))
        end
        direct = Set(keys(get(TOML.parsefile(joinpath(dir, "Project.toml")), "deps", Dict())))
        rows, brk = diff_rows(old, new, direct)
        append!(flagged, ("`$env/`: $f" for f in brk))
        isempty(rows) && continue
        push!(sections, """
            ### Julia — `$env/` ($(length(rows)) changed)

            | Package | Old | New | Change |
            |---|---|---|---|
            $(join(rows, "\n"))
            """)
    end
    # Review items first: what moved oddly, then where the envs disagree. Bold = a direct dep.
    if !isempty(flagged)
        println("### Julia — downgrades and semver-breaking bumps\n")
        foreach(f -> println("- ", f), flagged)
        println()
    end
    skew = drift(versions)
    if !isempty(skew)
        println("### Julia — envs disagree after the update\n")
        foreach(f -> println("- ", f), skew)
        println()
    end
    if !isempty(held)
        println("### Julia — held back by `[compat]` (direct deps)\n")
        foreach(f -> println("- ", f), held)
        println()
    end
    println(isempty(sections) ? "### Julia\n\nNo manifest changes.\n" : join(sections, "\n"))
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
