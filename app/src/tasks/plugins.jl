# ── plugins.jl — distributable custom-module sets ─────────────────────────────────────────────────
#
# A PLUGIN is ONE directory: `<config_dir>/modules/plugins/<plugin>/`, holding a `plugin.json`
# manifest plus the same co-located `<category>/<name>.{jl,json,_run.py}` layout a hand-dropped
# module already uses. One directory = one git repo = one unit to install, update and remove.
#
#   <config_dir>/modules/<category>/<name>.json                    hand-dropped (unchanged)
#   <config_dir>/modules/plugins/<plugin>/plugin.json              the manifest
#   <config_dir>/modules/plugins/<plugin>/<category>/<name>.json   a plugin's task
#
# This file is the LAYOUT half (PLUGINS_PLAN P1): where plugins live, what the manifest says, and the
# ONE enumerator both API scans consume. Install/remove over the network is P2 and lives nowhere yet.
# Full design + locked decisions: docs/todo/PLUGINS_PLAN.md.
#
# **Why an enumerator and not a recursive walk.** `api_task_definitions` and
# `_custom_module_categories` each hand-rolled a one-level `readdir` plus its own copy of the legacy
# skip list — two implementations of one rule, which is how the two halves came to scan at different
# depths in the first place. Making them blindly recursive instead would turn any stray nested folder
# into a phantom category (PLUGINS_PLAN Decision 2), so the depth stays EXPLICIT: exactly the two
# shapes above, enumerated here, consumed by both.
#
# Trust model is unchanged and deliberately not softened: plugin code is arbitrary Julia `Base.include`d
# into `Cecelia` with full machine access, exactly like a hand-dropped module. There is no sandbox.
# What P2 adds is that the FETCH is pinned and user-confirmed — not that the code is contained.

using Dates: now

const PLUGINS_SUBDIR        = "plugins"
const PLUGIN_MANIFEST       = "plugin.json"
# A plugin's plot specs, mirroring the package's own `app/src/plotDefinitions/`. This is the half that
# makes a plugin more than "custom modules in a directory": a plugin ships a custom TASK *and* the
# custom MODULE PAGE that inspects its output. Both are declarative JSON — see `user_plot_specs`.
const PLOT_DEFS_SUBDIR      = "plotDefinitions"
# P2 writes the pinned install record (repo + ref) here, beside the manifest rather than INTO it:
# `plugin.json` ships from the plugin's own repo, so writing the resolved ref into it would dirty the
# checkout and be overwritten by the next update. Named here so both halves agree on the spelling.
const PLUGIN_INSTALL_RECORD = ".install.json"

# Directory names under a modules root (or a plugin root) that are NOT categories: leftovers from the
# old R split layout, which the shipped co-located loader no longer uses. `python` doubles as a
# plugin's shared-code dir (see `_custom_modules_pydirs` in py_runner.jl), which must likewise never
# be read as a category.
const LEGACY_LAYOUT_DIRS = ("sources", "inputDefinitions", "python")

# Precedence tiers for a `fun_name` clash — PLUGINS_PLAN Decision 3: built-in > hand-dropped > plugin.
# A user's own file outranks anything they installed, so a plugin can never silently take over a name
# they are already using.
const TIER_PLUGIN  = 1
const TIER_USER    = 2
const TIER_BUILTIN = 3

tier_name(t::Integer)::String =
    t == TIER_BUILTIN ? "built-in" : t == TIER_USER ? "hand-dropped" : "plugin"

"""
    plugins_dir([dev_dir]) -> String

The plugin root, `<config_dir>/modules/plugins` (see [`custom_modules_dir`](@ref)).
"""
plugins_dir(dev_dir::Union{String,Nothing} = nothing)::String =
    joinpath(custom_modules_dir(dev_dir), PLUGINS_SUBDIR)

"""
    plugin_roots(; dev_dir=nothing) -> Vector{String}

Absolute path of every installed plugin directory, **sorted**.

Sorted on purpose: precedence between two same-tier plugins is first-wins, and `readdir` yields
filesystem order — which differs between machines. Without the sort, which plugin won a `fun_name`
clash would depend on the disk.
"""
function plugin_roots(; dev_dir::Union{String,Nothing} = nothing)::Vector{String}
    root = plugins_dir(dev_dir)
    isdir(root) || return String[]
    sort!(String[e for e in readdir(root; join = true) if isdir(e)])
end

"""
    plugin_name_of(path; dev_dir=nothing) -> Union{String,Nothing}

The plugin a path belongs to, or `nothing` for a hand-dropped module or anything outside the tree.
Compares path COMPONENTS rather than string prefixes, so it behaves on Windows separators and cannot
match a sibling directory that merely shares a prefix.
"""
function plugin_name_of(path::AbstractString; dev_dir::Union{String,Nothing} = nothing)
    rootparts = splitpath(abspath(plugins_dir(dev_dir)))
    pathparts = splitpath(abspath(String(path)))
    length(pathparts) > length(rootparts)             || return nothing
    pathparts[1:length(rootparts)] == rootparts       || return nothing
    String(pathparts[length(rootparts) + 1])
end

"""
    read_plugin_manifest(dir) -> NamedTuple

Parse `<dir>/plugin.json` into
`(; name, version, description, homepage, requiresCecelia, categories, error)`.

**Never throws.** A missing or malformed manifest yields the directory name as `name` and a populated
`error`, because a plugin whose tasks nonetheless loaded must still be nameable in Settings — the
manifest is descriptive metadata, not the thing that makes a plugin work.
"""
function read_plugin_manifest(dir::AbstractString)
    fallback = basename(rstrip(String(dir), ['/', '\\']))
    path     = joinpath(String(dir), PLUGIN_MANIFEST)
    isfile(path) || return (; name = fallback, version = "", description = "", homepage = "",
                              requiresCecelia = "", categories = String[],
                              error = "no $PLUGIN_MANIFEST")
    try
        m    = JSON3.read(read(path, String), Dict{String,Any})
        _str(k) = string(get(m, k, ""))
        cats = get(m, "categories", nothing)
        (; name            = isempty(_str("name")) ? fallback : _str("name"),
           version         = _str("version"),
           description     = _str("description"),
           homepage        = _str("homepage"),
           requiresCecelia = _str("requiresCecelia"),
           categories      = cats isa AbstractVector ? String[string(c) for c in cats] : String[],
           error           = nothing)
    catch e
        (; name = fallback, version = "", description = "", homepage = "",
           requiresCecelia = "", categories = String[], error = sprint(showerror, e))
    end
end

"""
    plugin_version_warning(requires, running) -> Union{String,Nothing}

`nothing` when the plugin's `requiresCecelia` is satisfied, absent, or **unenforceable**; otherwise a
one-line message for the Settings panel.

Warn-only by design (PLUGINS_PLAN Decision 4): refusing to load on a version mismatch would make every
cecelia release break every plugin at once.

**Skipped entirely when `running` is `"dev"` or empty.** The running version is `"dev"` for every
source checkout — there is no `VERSION` file outside a release bundle (`api/src/update_api.jl`) — so
comparing would print a warning for every plugin on every developer's machine, forever. `running` is a
parameter rather than something read here because the version resolver lives in the API layer; keeping
this function pure is also what makes it testable in `test-pkg`.
"""
function plugin_version_warning(requires, running)
    req = strip(string(something(requires, "")))
    isempty(req) && return nothing
    run = strip(string(something(running, "")))
    (isempty(run) || run == "dev") && return nothing

    m = match(r"^(>=|>|==|=)?\s*v?(.+)$", req)
    m === nothing && return "unreadable requiresCecelia \"$req\""
    op   = something(m.captures[1], ">=")
    want = try VersionNumber(strip(m.captures[2])) catch; nothing end
    have = try VersionNumber(lstrip(run, 'v'))     catch; nothing end
    (want === nothing || have === nothing) && return "unreadable requiresCecelia \"$req\""

    ok = op in (">=",) ? have >= want :
         op == ">"     ? have >  want :
                         have == want
    ok ? nothing : "needs cecelia $req, running $run"
end

# Every user task spec on disk, in PRECEDENCE ORDER: hand-dropped first (sorted), then plugins
# (sorted by plugin, then category, then filename). Path-sorted throughout so nothing about the
# outcome depends on filesystem order. Depth is fixed at exactly the two documented shapes — a plugin
# manifest sits at the plugin ROOT and so is never mistaken for a task spec, and `python/` is skipped
# by the legacy list so a plugin's shared-code dir is never read as a category.
function _user_spec_files(dev_dir::Union{String,Nothing})
    T    = @NamedTuple{category::String, path::String, plugin::Union{String,Nothing}, tier::Int}
    out  = T[]
    root = custom_modules_dir(dev_dir)
    isdir(root) || return out

    _specs_in(dir, plugin, tier) = for cat_dir in sort(readdir(dir; join = true))
        isdir(cat_dir) || continue
        cat = basename(cat_dir)
        cat in LEGACY_LAYOUT_DIRS && continue
        cat == PLOT_DEFS_SUBDIR   && continue   # plot specs are not tasks — see `user_plot_specs`
        plugin === nothing && cat == PLUGINS_SUBDIR && continue   # the plugin root is not a category
        for f in sort(readdir(cat_dir; join = true))
            endswith(f, ".json") || continue
            push!(out, (; category = cat, path = f, plugin, tier))
        end
    end

    _specs_in(root, nothing, TIER_USER)
    for proot in plugin_roots(; dev_dir)
        _specs_in(proot, basename(proot), TIER_PLUGIN)
    end
    out
end

"""
    user_task_specs(; dev_dir=nothing, category="", exclude_funs=Set{String}()) -> Vector{NamedTuple}

Every user-supplied task spec — hand-dropped **and** plugin — as
`(; category, path, plugin, fun_name, tier, spec)`, already parsed and **deduped by `fun_name`** under
the precedence in PLUGINS_PLAN Decision 3: a strictly higher tier displaces the incumbent, and within
a tier the first (path-sorted) wins.

This is THE enumerator for the user modules tree; `api_task_definitions` and
`_custom_module_categories` both consume it rather than each walking the directory themselves. The
dedupe is why they agree with dispatch: two clashing specs used to render two forms on one page while
`_task_from_fun_name` resolved exactly one of them.

`exclude_funs` drops names owned by built-ins (built-ins win, and the API is the half that knows what
they are). A malformed spec is warned about and skipped, never fatal.
"""
function user_task_specs(; dev_dir::Union{String,Nothing} = nothing,
                           category::AbstractString = "",
                           exclude_funs = Set{String}())
    out  = Any[]
    seen = Dict{String,Int}()
    for e in _user_spec_files(dev_dir)
        (!isempty(category) && e.category != category) && continue
        spec = try
            JSON3.read(read(e.path, String), Dict{String,Any})
        catch err
            @warn "Skipping malformed custom task spec" path = e.path exception = err
            continue
        end
        fn = string(get(spec, "fun_name", ""))
        (isempty(fn) || fn ∈ exclude_funs) && continue
        rec = (; e.category, e.path, e.plugin, fun_name = fn, e.tier, spec)
        i   = get(seen, fn, 0)
        if i == 0
            push!(out, rec)
            seen[fn] = length(out)
        elseif rec.tier > out[i].tier
            out[i] = rec        # a higher tier displaces; same tier keeps the incumbent (first wins)
        end
    end
    out
end

"""
    user_plot_specs(; dev_dir=nothing, exclude_ids=Set{String}()) -> Vector{Dict{String,Any}}

Plot specs shipped by hand-dropped modules and plugins, from `<modules>/plotDefinitions/` and
`<modules>/plugins/<plugin>/plotDefinitions/` — the same shape and schema as the package's own
`app/src/plotDefinitions/`.

**This is what makes a plugin worth having.** The custom-module loader already gave a user a custom
TASK; on its own, packaging tasks into a directory is only distribution. A plugin also ships the
**module page** that inspects the task's output — and it does so with no Vue and no compiled code,
because both halves of a page are already declarative:

| Half of the page | Declared by | Rendered by |
|---|---|---|
| the task form | a task spec's `params` | `ParamRenderer` |
| the plot canvas | a plot spec here (`module: "<category>"`) | `SummaryCanvas` |

A plugin therefore does not ship Vue. That is a decision rather than a hard limit — a stable install
precompiles SFCs so a plugin's `.vue` could not be compiled there, but pre-compiled ESM would load
fine. It is excluded because shipping renderable code makes the frontend a **plugin ABI**: a component
contract that cannot be refactored freely, plus a loader and version skew between a plugin and the app
drawing it. See `docs/todo/PLUGINS_PLAN.md` for the full trade-off.

`exclude_ids` drops ids owned by the package registry — **built-ins win**, the same rule as tasks. A
malformed spec is warned about and skipped, never fatal.
"""
function user_plot_specs(; dev_dir::Union{String,Nothing} = nothing, exclude_ids = Set{String}())
    out  = Dict{String,Any}[]
    seen = Set{String}()
    root = custom_modules_dir(dev_dir)
    isdir(root) || return out
    # Hand-dropped first, then plugins (path-sorted) — the same precedence order tasks use, so a
    # plugin cannot displace a plot the user defined themselves.
    for dir in vcat([joinpath(root, PLOT_DEFS_SUBDIR)],
                    [joinpath(p, PLOT_DEFS_SUBDIR) for p in plugin_roots(; dev_dir)])
        isdir(dir) || continue
        for f in sort(readdir(dir; join = true))
            endswith(f, ".json") || continue
            spec = try
                JSON3.read(read(f, String), Dict{String,Any})
            catch e
                @warn "Skipping malformed plot spec" path = f exception = e
                continue
            end
            id = string(get(spec, "id", ""))
            (isempty(id) || id ∈ exclude_ids || id ∈ seen) && continue
            push!(seen, id)
            push!(out, spec)
        end
    end
    out
end

# ── P2: install / update / remove ─────────────────────────────────────────────────────────────────
#
# Source is a URL plus a **pinned ref**, fetched as a TARBALL — never `git`. An installed app has no
# git: both installers fetch tarballs over plain HTTP, and `_is_installed` is literally defined as
# "has a VERSION file and has NO `.git`". The download + unpack path here is the same one the in-app
# updater uses (`Downloads` stdlib + `Cecelia._run_tar`, the one tar runner, which registers the
# process so an extract can be cancelled and checks `termsignal` — a bare `run` reads a killed extract
# as success). See docs/todo/PLUGINS_PLAN.md → R1.

"""GitHub repo URL + ref → the archive tarball. Any other URL is returned unchanged (already a tarball)."""
function plugin_tarball_url(url::AbstractString, ref::AbstractString)::String
    u = strip(String(url))
    m = match(r"^https?://github\.com/([^/]+)/([^/#?]+?)(?:\.git)?/?$", u)
    m === nothing && return u
    r = isempty(strip(String(ref))) ? "HEAD" : strip(String(ref))
    "https://github.com/$(m.captures[1])/$(m.captures[2])/archive/$r.tar.gz"
end

"""Plugin directory name for a source URL — the repo name, so one repo maps to one directory."""
function plugin_name_from_url(url::AbstractString)::String
    u = strip(String(url))
    m = match(r"github\.com/[^/]+/([^/#?]+?)(?:\.git)?/?$", u)
    m !== nothing && return String(m.captures[1])
    safe_name_part(splitext(basename(rstrip(u, '/')))[1])
end

install_record_path(dir::AbstractString) = joinpath(String(dir), PLUGIN_INSTALL_RECORD)

"""
    read_install_record(dir) -> Dict{String,Any}

Where the plugin came from: `(url, ref, installedAt)`. `Dict()` when hand-installed — a `git clone`
into `plugins/` is a first-class way to install, and must not look broken for lacking a record.
"""
function read_install_record(dir::AbstractString)::Dict{String,Any}
    p = install_record_path(dir)
    isfile(p) || return Dict{String,Any}()
    try JSON3.read(read(p, String), Dict{String,Any}) catch; Dict{String,Any}() end
end

"""
    plugin_unpack!(tarball, url; ref="", dev_dir=nothing, job_id=…, on_log=…) -> NamedTuple

Unpack an already-downloaded plugin tarball into place: extract to a temp dir → verify it looks like a
plugin → **replace** the target directory → write `.install.json`. Returns `(; ok, name, dir, error)`.

Verification before the move is the point: extracting straight into `plugins/<name>/` would leave a
half-written directory that the loader walks on the next reload. A tarball with no `plugin.json` is
rejected here rather than becoming a directory that registers nothing and explains nothing.

The DOWNLOAD is the caller's job (`api/src/plugins_api.jl`) — `Downloads` is not an `app/` dependency
and adding one would mean re-resolving three manifests for an HTTP fetch that is an API-layer concern
anyway. This half is the part worth unit-testing, so it takes a local file and stays in the package.

Never throws: install is user-driven and every failure is a message, not a stacktrace.
"""
function plugin_unpack!(tarball::AbstractString, url::AbstractString;
                        ref::AbstractString = "",
                        dev_dir::Union{String,Nothing} = nothing,
                        job_id::AbstractString = "plugin-install",
                        on_log::Function = _ -> nothing)
    _tar_available() ||
        return (; ok = false, name = "", dir = "", error = "`tar` was not found on PATH")
    isfile(tarball) ||
        return (; ok = false, name = "", dir = "", error = "no such archive: $tarball")
    name = plugin_name_from_url(url)
    isempty(name) &&
        return (; ok = false, name = "", dir = "", error = "could not derive a plugin name from $url")
    target = joinpath(plugins_dir(dev_dir), name)
    tmp    = mktempdir()
    try
        payload = joinpath(tmp, "payload"); mkpath(payload)
        _run_tar(`tar -xzf $tarball -C $payload`, String(job_id)) ||
            return (; ok = false, name, dir = "",
                      error = "unpacking failed (tar exited non-zero or was cancelled)")

        # A GitHub archive wraps everything in one `<repo>-<ref>/` directory; a hand-rolled tarball may
        # not. Take the wrapper only when it IS the single entry, rather than assuming either shape.
        entries = readdir(payload; join = true)
        root = (length(entries) == 1 && isdir(entries[1])) ? entries[1] : payload
        isfile(joinpath(root, PLUGIN_MANIFEST)) ||
            return (; ok = false, name, dir = "",
                      error = "not a plugin: no $PLUGIN_MANIFEST at the archive root")

        mkpath(dirname(target))
        isdir(target) && rm(target; recursive = true, force = true)
        mv(root, target)
        # The record is a SIBLING of plugin.json, never inside it: plugin.json ships from the plugin's
        # own repo, so writing the resolved ref into it would dirty the checkout and be overwritten by
        # the next update. Decision 5.
        write_json_atomic(install_record_path(target),
                          Dict{String,Any}("url" => String(url), "ref" => String(ref),
                                           "installedAt" => string(now())))
        on_log("Installed $name")
        (; ok = true, name, dir = target, error = nothing)
    catch e
        (; ok = false, name, dir = "", error = sprint(showerror, e))
    finally
        rm(tmp; recursive = true, force = true)
    end
end

"""
    plugin_remove!(name; dev_dir=nothing) -> NamedTuple

Unregister the plugin's tasks, then delete its directory. Returns `(; ok, removed, error)`.

**Refuses while any of its tasks is running.** Deleting the directory under a live run pulls the
runner's own `_run.py` out from under a `run_py` subprocess, and `_unregister_task!` only drops the
registry entry — an in-flight `_run_task` already holds the instance. Decision 9.
"""
function plugin_remove!(name::AbstractString; dev_dir::Union{String,Nothing} = nothing)
    dir = joinpath(plugins_dir(dev_dir), String(name))
    isdir(dir) || return (; ok = false, removed = String[], error = "no such plugin: $name")

    mine = String[e.fun_name for e in user_task_specs(; dev_dir) if e.plugin == String(name)]
    busy = [t.fun_name for t in list_tasks() if t.fun_name ∈ mine]
    isempty(busy) ||
        return (; ok = false, removed = String[],
                  error = "still running: $(join(unique(busy), ", ")) — cancel it first")

    removed = String[]
    for fn in mine
        _unregister_task!(fn) && push!(removed, fn)
    end
    rm(dir; recursive = true, force = true)
    (; ok = true, removed, error = nothing)
end

"""
    plugin_registry() -> Vector{Dict{String,Any}}

The curated list of plugins we vouch for (`app/src/pluginRegistry.json`), each
`(name, url, description, categories, ref)`, with `installed` stamped on by `plugin_registry_status`.

**Curated, not a search index** (Decision 6): anything not listed installs by explicit URL, and cecelia
never browses GitHub. The list SHIPS with the app rather than being fetched, so an offline install
behaves like an online one and the catalogue cannot change under a running server — the trade is that
adding a plugin needs a release, which is the thing to revisit if the list grows.

A malformed registry yields an empty list rather than taking Settings down: the catalogue is a
convenience, and install-by-URL works without it.
"""
function plugin_registry()::Vector{Dict{String,Any}}
    path = joinpath(@__DIR__, "..", "pluginRegistry.json")
    isfile(path) || return Dict{String,Any}[]
    try
        doc = JSON3.read(read(path, String), Dict{String,Any})
        ps  = get(doc, "plugins", nothing)
        ps isa AbstractVector ? Dict{String,Any}[Dict{String,Any}(p) for p in ps] : Dict{String,Any}[]
    catch e
        @warn "Skipping malformed plugin registry" path exception = e
        Dict{String,Any}[]
    end
end

"""
    plugin_registry_status(; dev_dir=nothing) -> Vector{Dict{String,Any}}

The registry with `installed` set per entry, matched on the DIRECTORY the entry's url would install to
— not on the manifest `name`, which a plugin author controls and could set to anything. One repo maps
to one directory, which is what makes the check unambiguous.
"""
function plugin_registry_status(; dev_dir::Union{String,Nothing} = nothing)
    have = Set(basename.(plugin_roots(; dev_dir)))
    map(plugin_registry()) do e
        d = copy(e)
        d["installed"] = plugin_name_from_url(string(get(e, "url", ""))) ∈ have
        d
    end
end

"""
    plugins_report(; dev_dir=nothing, running_version="") -> Vector{NamedTuple}

One entry per installed plugin for the Settings panel: `(; name, dir, version, description, homepage,
categories, error, warning)`. `categories` is what the plugin actually ships on disk, not what its
manifest claims — the manifest is descriptive, the directory is the truth.
"""
function plugins_report(; dev_dir::Union{String,Nothing} = nothing,
                          running_version::AbstractString = "")
    specs = _user_spec_files(dev_dir)
    map(plugin_roots(; dev_dir)) do d
        name = basename(d)
        m    = read_plugin_manifest(d)
        (; name       = m.name,
           dir        = d,
           version    = m.version,
           description = m.description,
           homepage   = m.homepage,
           categories = sort(unique(String[e.category for e in specs if e.plugin == name])),
           error      = m.error,
           warning    = plugin_version_warning(m.requiresCecelia, running_version))
    end
end
