# ── Plugins: layout, enumeration, precedence (docs/todo/PLUGINS_PLAN.md P1) ───────────────────────
#
# A plugin is ONE directory under <config_dir>/modules/plugins/. The shipped blocker was that the two
# halves scanned at different depths — the Julia loader `walkdir`s (any depth) while both API scans
# did a one-level `readdir` — so a plugin's task registered and ran but had no form and no nav entry.
# These testsets pin the fix: one enumerator, explicit depth, and a precedence rule that no longer
# depends on the filesystem's ordering.

# Build a modules tree in a throwaway config dir and return its root.
function _mk_plugin_tree(cfg)
    mods = joinpath(cfg, "modules")
    spec(fun) = JSON3.write(Dict("fun_name" => fun, "label" => fun,
                                 "resource_pool" => "cpu", "scope" => "image", "params" => []))
    # hand-dropped: modules/<category>/<name>.json
    mkpath(joinpath(mods, "tracking"))
    write(joinpath(mods, "tracking", "handDropped.json"), spec("tracking.handDropped"))
    # plugin: modules/plugins/<plugin>/<category>/<name>.json  + a manifest at the plugin ROOT
    pdir = joinpath(mods, "plugins", "trackimport-smithlab")
    mkpath(joinpath(pdir, "tracking"))
    mkpath(joinpath(pdir, "python"))          # shared code — NOT a category
    write(joinpath(pdir, "plugin.json"), JSON3.write(Dict(
        "name" => "trackimport-smithlab", "version" => "0.2.0",
        "description" => "Import tracks from the Smith lab CSV export",
        "requiresCecelia" => ">=0.1.3", "categories" => ["tracking"])))
    write(joinpath(pdir, "tracking", "importSmith.json"), spec("tracking.importSmith"))
    write(joinpath(pdir, "python", "reader.json"), spec("should.neverAppear"))
    mods
end

@testset "Plugin layout + spec enumeration" begin
    cfg  = mktempdir()
    mods = _mk_plugin_tree(cfg)
    specs = Cecelia.user_task_specs(; dev_dir = cfg)
    byfun = Dict(e.fun_name => e for e in specs)

    # Both layouts are enumerated by the one scan — this is the whole P1 blocker.
    @test haskey(byfun, "tracking.handDropped")
    @test haskey(byfun, "tracking.importSmith")

    # CATEGORY comes from the directory BELOW the plugin root, never from the plugin name
    # (PLUGINS_PLAN Decision 2). `trackimport-smithlab` must not become a category.
    @test byfun["tracking.importSmith"].category == "tracking"
    @test byfun["tracking.importSmith"].plugin   == "trackimport-smithlab"
    @test !any(e -> e.category == "trackimport-smithlab", specs)
    @test !any(e -> e.category == "plugins", specs)          # the plugin ROOT is not a category

    # A hand-dropped module reports no plugin, and is the higher tier.
    @test byfun["tracking.handDropped"].plugin === nothing
    @test byfun["tracking.handDropped"].tier > byfun["tracking.importSmith"].tier

    # `python/` is a plugin's shared-code dir, not a category — a stray .json in it is NOT a task.
    @test !haskey(byfun, "should.neverAppear")
    # The manifest sits at the plugin root, so it can never be mistaken for a task spec.
    @test !any(e -> basename(e.path) == Cecelia.PLUGIN_MANIFEST, specs)

    # Depth stays EXPLICIT rather than recursive: a stray nested folder must not become a category
    # (an unbounded walkdir here is what Decision 2 rules out).
    mkpath(joinpath(mods, "tracking", "nested"))
    write(joinpath(mods, "tracking", "nested", "deep.json"),
          JSON3.write(Dict("fun_name" => "nested.deep", "label" => "d",
                           "resource_pool" => "cpu", "scope" => "image", "params" => [])))
    @test !any(e -> e.fun_name == "nested.deep", Cecelia.user_task_specs(; dev_dir = cfg))

    # category filter + built-in exclusion
    @test all(e -> e.category == "tracking", Cecelia.user_task_specs(; dev_dir = cfg, category = "tracking"))
    @test isempty(Cecelia.user_task_specs(; dev_dir = cfg, category = "nope"))
    excl = Cecelia.user_task_specs(; dev_dir = cfg, exclude_funs = Set(["tracking.importSmith"]))
    @test !any(e -> e.fun_name == "tracking.importSmith", excl)

    # plugin_name_of: components, not string prefixes
    @test Cecelia.plugin_name_of(joinpath(mods, "plugins", "trackimport-smithlab", "tracking", "x.jl");
                                 dev_dir = cfg) == "trackimport-smithlab"
    @test Cecelia.plugin_name_of(joinpath(mods, "tracking", "handDropped.jl"); dev_dir = cfg) === nothing
    @test Cecelia.plugin_name_of("/somewhere/else/x.jl"; dev_dir = cfg) === nothing
end

@testset "Plugin manifest + version warning" begin
    cfg = mktempdir(); _mk_plugin_tree(cfg)
    pdir = joinpath(cfg, "modules", "plugins", "trackimport-smithlab")

    m = Cecelia.read_plugin_manifest(pdir)
    @test m.name == "trackimport-smithlab" && m.version == "0.2.0" && m.error === nothing
    @test m.categories == ["tracking"]

    # Never throws: a missing or malformed manifest still yields a NAMEABLE plugin, because its tasks
    # may well have loaded and Settings has to be able to show it.
    empty_dir = joinpath(cfg, "modules", "plugins", "nomanifest"); mkpath(empty_dir)
    @test Cecelia.read_plugin_manifest(empty_dir).name  == "nomanifest"
    @test Cecelia.read_plugin_manifest(empty_dir).error !== nothing
    bad = joinpath(cfg, "modules", "plugins", "broken"); mkpath(bad)
    write(joinpath(bad, "plugin.json"), "{not json")
    @test Cecelia.read_plugin_manifest(bad).name  == "broken"
    @test Cecelia.read_plugin_manifest(bad).error !== nothing

    # requiresCecelia is WARN-ONLY (Decision 4) — and skipped outright on a dev checkout, where the
    # running version is the literal "dev". Without this every plugin warns on every dev machine.
    @test Cecelia.plugin_version_warning(">=0.1.3", "dev")     === nothing
    @test Cecelia.plugin_version_warning(">=0.1.3", "")        === nothing
    @test Cecelia.plugin_version_warning("",        "0.1.0")   === nothing   # no requirement
    @test Cecelia.plugin_version_warning(">=0.1.3", "0.1.3")   === nothing
    @test Cecelia.plugin_version_warning(">=0.1.3", "v0.2.0")  === nothing
    @test Cecelia.plugin_version_warning(">=0.1.3", "0.1.0")   !== nothing   # genuinely too old
    @test Cecelia.plugin_version_warning("garbage",  "0.1.3")  !== nothing   # unreadable, not fatal

    rep = Cecelia.plugins_report(; dev_dir = cfg, running_version = "dev")
    smith = only(filter(p -> p.name == "trackimport-smithlab", rep))
    @test smith.version == "0.2.0"
    @test smith.categories == ["tracking"]     # what it SHIPS, read off disk, not what it claims
    @test smith.warning === nothing            # dev → skipped
    @test length(rep) == 3                     # sorted, and the two broken ones still appear
end

@testset "A plugin's contributions: the layout desugars, the manifest is checked" begin
    # PLUGINS_PLAN Decision 10. The directory layout IS the contribution list; a `contributions` block
    # is optional and buys a CHECK, never a capability. The rule that matters most is the last one
    # here: declaring must not restrict, or a plugin author who adds a block and forgets a line
    # silently hides their own task.
    cfg  = mktempdir(); _mk_plugin_tree(cfg)
    pdir = joinpath(cfg, "modules", "plugins", "trackimport-smithlab")
    mkpath(joinpath(pdir, Cecelia.PLOT_DEFS_SUBDIR))
    write(joinpath(pdir, Cecelia.PLOT_DEFS_SUBDIR, "smith.json"),
          JSON3.write(Dict("id" => "smith_speed", "module" => "tracking", "title" => "Speed")))

    # ── no block at all: still fully described ────────────────────────────────────────────────────
    c = Cecelia.plugin_contributions(pdir)
    @test [t.funName for t in c.tasks] == ["tracking.importSmith"]
    @test only(c.tasks).category == "tracking"
    @test only(c.plots).id == "smith_speed"
    # Paths are relative to the plugin root and use `/` on every platform — they are compared against
    # what a manifest declares, and a manifest cannot spell a Windows separator.
    @test only(c.plots).spec == "plotDefinitions/smith.json"
    @test !occursin('\\', only(c.tasks).path)
    @test all(!t.declared for t in c.tasks) && isempty(c.problems)
    # `python/` is shared code, not a category — the same rule the task enumerator uses, because it is
    # literally the same function.
    @test !any(t -> t.funName == "should.neverAppear", c.tasks)

    _manifest(d) = write(joinpath(pdir, Cecelia.PLUGIN_MANIFEST), JSON3.write(d))
    base = Dict("name" => "trackimport-smithlab", "version" => "0.2.0")

    # ── a block that matches the disk ─────────────────────────────────────────────────────────────
    _manifest(merge(base, Dict("contributions" => Dict(
        "tasks" => [Dict("funName" => "tracking.importSmith")],
        "plots" => [Dict("spec" => "plotDefinitions/smith.json")]))))
    c = Cecelia.plugin_contributions(pdir)
    @test isempty(c.problems)
    @test only(c.tasks).declared && only(c.plots).declared

    # ── a block that does NOT ─────────────────────────────────────────────────────────────────────
    _manifest(merge(base, Dict("contributions" => Dict(
        "tasks" => [Dict("funName" => "tracking.renamedAwhileAgo")],
        "plots" => [Dict("spec" => "plotDefinitions/gone.json")]))))
    c = Cecelia.plugin_contributions(pdir)
    @test any(m -> occursin("tracking.renamedAwhileAgo", m), c.problems)
    @test any(m -> occursin("gone.json", m), c.problems)
    # …but the task on disk is STILL a task. Declaring is descriptive, never a filter.
    @test [t.funName for t in c.tasks] == ["tracking.importSmith"]
    @test only(c.plots).id == "smith_speed"

    # ── kinds that are understood but not acted on yet (Decisions 11/12) ──────────────────────────
    _manifest(merge(base, Dict("contributions" => Dict(
        "views"  => [Dict("module" => "tracking", "view" => "trackPaths", "label" => "Tracks")],
        "layers" => [Dict("fromTask" => "tracking.importSmith", "layerType" => "points",
                          "colorBy" => "speed")]))))
    c = Cecelia.plugin_contributions(pdir)
    @test only(c.views).view == "trackPaths" && only(c.views).moduleName == "tracking"
    @test only(c.layers).layerType == "points"
    # Extra keys survive as `options` — the vocabulary is Decision 12's job, not the parser's.
    @test only(c.layers).options["colorBy"] == "speed"
    # `views` IS acted on (Decision 11 — the custom module page hosts one), so it must NOT be
    # reported; `layers` is not, and declaring it says so. A blank panel with no explanation is the
    # failure mode this codebase keeps producing.
    @test !any(m -> occursin("views", m), c.problems)
    @test any(m -> occursin("layers", m) && occursin("does not act on", m), c.problems)

    # …and a declared view reaches the enumerator the API serves per category.
    vs = Cecelia.plugin_views(; dev_dir = cfg)
    @test only(filter(v -> v.plugin == "trackimport-smithlab", vs)).view == "trackPaths"
    @test only(filter(v -> v.plugin == "trackimport-smithlab", vs)).moduleName == "tracking"

    # ── malformed, never fatal ────────────────────────────────────────────────────────────────────
    _manifest(merge(base, Dict("contributions" => Dict(
        "tasks"   => "tracking.importSmith",                       # not a list
        "layers"  => [Dict("fromTask" => "x", "layerType" => "mesh")],
        "widgets" => [Dict("id" => "x")]))))                       # not a kind we know
    c = Cecelia.plugin_contributions(pdir)
    @test any(m -> occursin("must be a list", m), c.problems)
    # The layer type allow-list is the point (Decision 12): unchecked, `layerType` would make napari's
    # constructor signatures a plugin ABI by the back door.
    @test any(m -> occursin("mesh", m), c.problems)
    @test any(m -> occursin("widgets", m), c.problems)
    @test [t.funName for t in c.tasks] == ["tracking.importSmith"]   # still described

    # A malformed manifest cannot take the report down — same contract read_plugin_manifest has.
    write(joinpath(pdir, Cecelia.PLUGIN_MANIFEST), "{not json")
    @test [t.funName for t in Cecelia.plugin_contributions(pdir).tasks] == ["tracking.importSmith"]
    rep = Cecelia.plugins_report(; dev_dir = cfg, running_version = "dev")
    @test all(haskey(p, :problems) for p in rep)
end

@testset "A bundled example installs from the checkout, with no network" begin
    # `docs/examples/plugins/<name>/` is the SOURCE; the GitHub repo is a mirror published at release
    # time. So on a checkout the newest copy of a plugin is already on disk, and installing it "the
    # proper way" meant pushing to GitHub and pulling the same files back — with a window in which the
    # two disagree. That window is not hypothetical: an install from GitHub produced a task form three
    # commits stale while the fixed spec sat in the same worktree.
    have = Cecelia.bundled_plugins()
    @test !isempty(have)                                   # this repo HAS examples; empty means the path moved
    @test all(isfile(joinpath(p.dir, Cecelia.PLUGIN_MANIFEST)) for p in have)
    @test "ccia-trackMeasures" ∈ [p.name for p in have]

    cfg = mktempdir()
    res = Cecelia.plugin_install_local!("ccia-trackMeasures"; dev_dir = cfg)
    @test res.ok
    @test isfile(joinpath(res.dir, Cecelia.PLUGIN_MANIFEST))
    @test isfile(joinpath(res.dir, "trackTools", "cumulativeChange.jl"))
    # It COPIES: the source is the user's checkout, and a move would empty it.
    @test isfile(joinpath(Cecelia.bundled_plugins_dir(), "ccia-trackMeasures", Cecelia.PLUGIN_MANIFEST))

    # The record says where it really came from. Stamping the GitHub url would make an out-of-date
    # published mirror look like the installed source — the exact confusion this exists to end.
    rec = Cecelia.read_install_record(res.dir)
    @test rec["url"] == "bundled:ccia-trackMeasures"
    @test !occursin("github", rec["url"])

    # Re-installing REPLACES rather than merges — a file deleted upstream must not survive.
    stale = joinpath(res.dir, "trackTools", "gone.json")
    write(stale, "{}")
    @test Cecelia.plugin_install_local!("ccia-trackMeasures"; dev_dir = cfg).ok
    @test !isfile(stale)

    # A name outside the closed bundled list is refused — that list is read off disk, so there is no
    # path to sanitise and nothing a request can point at outside the checkout.
    for n in ("../../etc", "nope", "")
        @test !Cecelia.plugin_install_local!(n; dev_dir = cfg).ok
    end

    # …and the installed copy is enumerated like any other plugin.
    @test "ccia-trackMeasures" ∈ [p.name for p in Cecelia.plugins_report(; dev_dir = cfg)]
end

@testset "A module edited after loading reports STALE" begin
    # A `.jl` is `include`d once per session — Julia cannot redefine a struct — while a task's `.json`
    # spec is re-read on every request. So updating a plugin in place leaves the FORM new and the
    # HANDLER old, and the two then disagree about which params exist.
    #
    # That is exactly what the first update through the new checkout-install produced: a form asking
    # for track populations, and a handler still reading a `valueName` the form no longer sent, failing
    # with a message about a param the user could not see. Nothing said why, because nothing looked.
    # It cannot be REPAIRED (hence a report, not a fix) — only a restart picks the new code up.
    cfg  = mktempdir()
    mods = joinpath(cfg, "modules", "staleCat"); mkpath(mods)
    jl   = joinpath(mods, "staleMod.jl")
    write(jl, "# no registration; loading is all this test needs\n")
    write(joinpath(mods, "staleMod.json"),
          JSON3.write(Dict("fun_name" => "staleCat.staleMod", "label" => "s",
                           "resource_pool" => "cpu", "scope" => "image", "params" => [])))

    Cecelia.load_custom_modules!(; dev_dir = cfg)
    row() = only(filter(e -> e.path == jl, Cecelia.custom_modules_report()))
    @test row().status == "ok"
    @test row().stale == false

    # The mtime was RECORDED at load — that is the whole mechanism.
    @test haskey(Cecelia._CUSTOM_MODULES_MTIME, jl)

    # Edit it the way an install does. Then wind the RECORDED mtime back rather than trying to push the
    # file's forward: mtime resolution differs per filesystem (1 s on HFS+, ns on ext4), so asserting
    # on a fresh write would be a timing race in CI on one OS and not the others. This tests the rule
    # itself — "disk newer than what we recorded" — with no clock in it.
    write(jl, "# edited\n")
    Cecelia._CUSTOM_MODULES_MTIME[jl] -= 5
    @test row().stale == true

    # A reload does NOT clear it: the file was already loaded, so it is skipped and the running code is
    # still the old one. Reporting it as fresh here would be the lie the whole check exists to prevent.
    res = Cecelia.load_custom_modules!(; dev_dir = cfg)
    @test jl ∈ res.skipped
    @test row().stale == true

    # Deleting the file drops the row entirely rather than leaving a stale ghost.
    rm(jl)
    Cecelia.load_custom_modules!(; dev_dir = cfg)
    @test isempty(filter(e -> e.path == jl, Cecelia.custom_modules_report()))
end

@testset "Plugin fun_name precedence (built-in > hand-dropped > plugin)" begin
    # This is the rule that stops an installed plugin silently taking over a name the user's own
    # drop-in module already uses. Before it, `register_task!` overwrote unconditionally and the
    # winner was decided by `walkdir`'s filesystem order.
    cfg  = mktempdir()
    mods = joinpath(cfg, "modules")
    spec = joinpath(mods, "spec.json"); mkpath(mods)
    write(spec, JSON3.write(Dict("fun_name" => "x", "label" => "x",
                                 "resource_pool" => "cpu", "scope" => "image", "params" => [])))
    fn   = "plugPrec.shared$(rand(1000:9999))"
    user_path   = joinpath(mods, "tracking", "mine.jl")
    plugin_path = joinpath(mods, "plugins", "p-one", "tracking", "theirs.jl")

    # Simulate the loader's include context (what load_custom_modules! sets around Base.include).
    function _as(path, tier, plugin, task)
        Cecelia._LOADING_SOURCE[] = (; path, tier, plugin)
        try register_task!(fn, task; spec = spec) finally Cecelia._LOADING_SOURCE[] = nothing end
    end

    # The plugin registers FIRST, then the user's own module — the user must still win, so this
    # cannot be implemented as plain last-one-wins ordering.
    _as(plugin_path, Cecelia.TIER_PLUGIN, "p-one", _TestCustomTask())
    @test _task_from_fun_name(fn) isa _TestCustomTask
    _as(user_path, Cecelia.TIER_USER, nothing, _TestCustomTask2())
    @test _task_from_fun_name(fn) isa _TestCustomTask2          # hand-dropped displaced the plugin

    # …and the reverse order is stable too: the plugin does not take it back.
    _as(joinpath(mods, "plugins", "p-two", "tracking", "again.jl"),
        Cecelia.TIER_PLUGIN, "p-two", _TestCustomTask())
    @test _task_from_fun_name(fn) isa _TestCustomTask2

    # Every refusal is REPORTED, never silent — a clash is not a load failure, so it cannot show up
    # in custom_modules_report and would otherwise leave the task missing with no explanation.
    # custom_task_clashes() drops entries whose losing file is gone (same rule the load report uses),
    # so the placeholders have to exist for the clash to still be reportable.
    mkpath(dirname(user_path));   write(user_path,   "# placeholder\n")
    mkpath(dirname(plugin_path)); write(plugin_path, "# placeholder\n")
    clashes = filter(c -> c.funName == fn, custom_task_clashes())
    @test !isempty(clashes)
    @test any(c -> c.path == plugin_path && c.tier == "plugin", clashes)
    @test all(c -> c.winnerTier in ("hand-dropped", "built-in", "plugin"), clashes)

    # Built-ins outrank everything, as before — and now say so rather than failing silently.
    bfn = "importImages.remove"
    _as(joinpath(mods, "plugins", "p-one", "importImages", "remove.jl"),
        Cecelia.TIER_PLUGIN, "p-one", _TestCustomTask())
    @test _task_from_fun_name(bfn) isa RemoveImage
end

@testset "Custom module load order is deterministic (hand-dropped before plugins)" begin
    # Precedence is only meaningful if the ORDER is fixed: within a tier the first file loaded keeps
    # the name, and walkdir returns filesystem order, which differs between machines.
    cfg  = mktempdir()
    mods = _mk_plugin_tree(cfg)
    for (d, f) in (("tracking", "handDropped.jl"),)
        write(joinpath(mods, d, f), "# noop\n")
    end
    write(joinpath(mods, "plugins", "trackimport-smithlab", "tracking", "importSmith.jl"), "# noop\n")
    mkpath(joinpath(mods, "plugins", "a-earlier", "tracking"))
    write(joinpath(mods, "plugins", "a-earlier", "tracking", "z.jl"), "# noop\n")

    srcs = Cecelia._custom_module_sources(mods, cfg)
    isplug(p) = Cecelia.plugin_name_of(p; dev_dir = cfg) !== nothing
    @test !isempty(srcs)
    # every hand-dropped path precedes every plugin path
    @test findlast(!isplug, srcs) < findfirst(isplug, srcs)
    # and each group is path-sorted, so "first wins" is reproducible
    @test issorted(filter(!isplug, srcs)) && issorted(filter(isplug, srcs))
end

@testset "The shipped custom-module examples load end to end" begin
    # `docs/examples/custom-modules/` is the FIRST thing a beginner copies — the guide sends them
    # there before it mentions plugins — and until now nothing executed it. The copy ratchets read its
    # JSON, but its Julia was never `include`d by any test, so it could have rotted silently while
    # every suite stayed green. That is the wrong way round: the example with the least experienced
    # audience had the weakest guarantee.
    root = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "docs", "examples", "custom-modules")
    @test isdir(root)
    cfg = mktempdir()
    dst = joinpath(cfg, "modules")
    mkpath(dirname(dst)); cp(root, dst)
    rm(joinpath(dst, "README.md"); force = true)      # docs, not a module

    # Enumerated under the category DIRECTORY, exactly like a built-in — that is what puts a task on
    # an existing page (`behaviour`) or gives it a new one (`customExamples` → /custom/customExamples).
    byfun = Dict(e.fun_name => e for e in Cecelia.user_task_specs(; dev_dir = cfg))
    @test byfun["behaviour.exampleNormalise"].category == "behaviour"
    @test byfun["customExamples.trackContext"].category == "customExamples"
    # `nothing`, not "" — a loose drop-in belongs to no plugin, and the enumerator distinguishes
    # "no plugin" from "a plugin whose name is empty" rather than collapsing them.
    @test byfun["behaviour.exampleNormalise"].plugin === nothing

    # …and they actually LOAD. This `include`s the real Julia a reader is being told to copy.
    res = load_custom_modules!(; dev_dir = cfg)
    @test isempty(res.failed)
    @test length(res.loaded) == 2
    try
        @test _task_from_fun_name("behaviour.exampleNormalise") !== nothing
        @test _task_from_fun_name("customExamples.trackContext") !== nothing
        # The Python half is co-located and launched by absolute path, which is the layout rule the
        # guide states; a missing sibling would make the example unrunnable for whoever copied it.
        @test isfile(joinpath(dst, "customExamples", "trackContext_run.py"))
    finally
        Cecelia._unregister_task!("behaviour.exampleNormalise")
        Cecelia._unregister_task!("customExamples.trackContext")
    end
end

@testset "every example plugin is installable" begin
    # `ccia-trackMeasures` shipped in the repo, loaded in CI, and had **no GitHub page** — so the one
    # route a user actually has (Settings → Plugins) could not install it, and nothing failed. A
    # plugin that exists only as a directory in our own checkout is a plugin nobody else can run.
    reg  = Cecelia.plugin_registry()
    byname = Dict(String(get(e, "name", "")) => e for e in reg)
    root = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "docs", "examples", "plugins")
    for name in sort(readdir(root))
        isdir(joinpath(root, name)) || continue
        @test haskey(byname, name)
        e = get(byname, name, Dict{String,Any}())
        url = String(get(e, "url", ""))
        # The DIRECTORY an install creates is derived from the url, so a registry entry whose name
        # disagrees with its url would install under one name and be looked up under another.
        @test Cecelia.plugin_name_from_url(url) == name
        @test !isempty(String(get(e, "description", "")))

        # The published repo's landing page comes from the IN-REPO README — that copy is the source,
        # so a plugin without one publishes a repo that explains nothing.
        @test isfile(joinpath(root, name, "README.md"))
        # …and its manifest must point at its OWN repo, not cecelia's. Both examples shipped with
        # `homepage` pointing at schienstockd/cecelia, so "Homepage" in the plugin table sent the
        # user to the app rather than to the thing they were about to install.
        m = Cecelia.read_plugin_manifest(joinpath(root, name))
        @test m.error === nothing && String(m.homepage) == url

        # Whatever a shipped example DECLARES must resolve to something on disk. `ccia-trackMeasures`
        # carries a `contributions` block precisely so this ratchet is not vacuous: rename its
        # `fun_name` or its plot spec and CI says which line of the manifest disagrees.
        c = Cecelia.plugin_contributions(joinpath(root, name); manifest = m)
        @test isempty(c.problems)
        @test !isempty(c.tasks)
    end
    # …and at least one example must actually exercise the declared path, or the check above passes
    # for every plugin by declaring nothing.
    @test any(readdir(root)) do name
        isdir(joinpath(root, name)) &&
            any(t -> t.declared, Cecelia.plugin_contributions(joinpath(root, name)).tasks)
    end

    # The examples README is the landing page for "how do I write one of these", and it went stale
    # without anyone noticing: it described a single `tracktools-example` plugin, with an install
    # command naming a directory that no longer existed, long after the examples had been split in
    # two and published separately. Naming every directory is the cheapest check that catches it.
    readme = read(joinpath(root, "README.md"), String)
    for name in sort(readdir(root))
        isdir(joinpath(root, name)) && @test occursin(name, readme)
    end
    # Nothing in the curated list may point somewhere the name cannot be derived from.
    for e in reg
        @test Cecelia.plugin_name_from_url(String(get(e, "url", ""))) == String(get(e, "name", ""))
    end
end

@testset "The shipped example plugin loads end to end" begin
    # The example under docs/examples/plugins/ is the reference a user copies, so CI has to actually
    # LOAD it. (The custom-module examples are covered by the testset above — they were the older gap:
    # read by the copy ratchets, executed by nothing.) This installs the example into a throwaway
    # config dir exactly as a user would (`cp -r` into modules/plugins/) and asserts the whole chain.
    root = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "docs", "examples", "plugins")
    # TWO single-purpose example plugins, not one mixed bag: importing someone else's tracks and
    # measuring them are different capabilities, so they are different plugins.
    cfg = mktempdir()
    for name in ("ccia-importTracks", "ccia-trackMeasures")
        @test isdir(joinpath(root, name))
        dst = joinpath(cfg, "modules", Cecelia.PLUGINS_SUBDIR, name)
        mkpath(dirname(dst)); cp(joinpath(root, name), dst)
        m = Cecelia.read_plugin_manifest(dst)
        @test m.error === nothing && m.name == name
        @test occursin("-", name)   # the case that can never be a Python/Julia identifier
    end
    dst = joinpath(cfg, "modules", Cecelia.PLUGINS_SUBDIR, "ccia-importTracks")

    # 2) both TASKS are enumerated, each under the category dir it sits in — one on a built-in page
    #    (tracking), one in the plugin's own new category (trackTools)
    specs = Cecelia.user_task_specs(; dev_dir = cfg)
    byfun = Dict(e.fun_name => e for e in specs)
    @test byfun["tracking.importCsvTracks"].category    == "tracking"
    @test byfun["trackTools.cumulativeChange"].category == "trackTools"
    # each task comes from the plugin whose single purpose it is
    @test byfun["tracking.importCsvTracks"].plugin    == "ccia-importTracks"
    @test byfun["trackTools.cumulativeChange"].plugin == "ccia-trackMeasures"
    # neither the manifest, the shared python/, nor plotDefinitions/ is mistaken for a task or category
    @test !any(e -> e.category in ("python", Cecelia.PLOT_DEFS_SUBDIR), specs)

    # 3) the PAGE half — the plugin's plot spec is picked up and declares the plugin's own category,
    #    which is what makes /custom/trackTools a real module page rather than a bare task runner.
    plots = Cecelia.user_plot_specs(; dev_dir = cfg)
    cc = only(filter(p -> get(p, "id", "") == "tracktools_cumulative_change", plots))
    @test cc["module"] == "trackTools"
    @test "trackTools.cumulativeSpeed" ∈ cc["dataSource"]["measureOptions"]
    # built-ins win on an id clash — a plugin must not be able to replace a package plot
    @test isempty(Cecelia.user_plot_specs(; dev_dir = cfg,
                                            exclude_ids = Set(["tracktools_cumulative_change"])))

    # 4) the tasks actually LOAD and register (this `include`s the plugin's real Julia)
    res = load_custom_modules!(; dev_dir = cfg)
    @test isempty(res.failed)
    @test length(res.loaded) == 2
    try
        @test _task_from_fun_name("trackTools.cumulativeChange") !== nothing
        @test _task_from_fun_name("tracking.importCsvTracks")    !== nothing
        # the registered spec resolves, so the form renders and params validate like a built-in
        @test task_scope(_task_from_fun_name("trackTools.cumulativeChange")) == "image"
        # It takes TRACK POPULATIONS, not a segmentation — the house convention for anything measured
        # along tracks (docs/MODULES.md → *Derive the segmentation from the pops*). Pinned here
        # because the first version shipped a `valueNameSelection` labelled "Segmentation", which was
        # both the wrong picker and the wrong word for what it consumes.
        cc_spec = Cecelia._task_spec(_task_from_fun_name("trackTools.cumulativeChange"))
        cc_pops = only(filter(p -> get(p, "key", "") == "pops", cc_spec["params"]))
        @test cc_pops["type"] == "popSelection" && cc_pops["popScope"] == "tracks"
        @test !any(p -> get(p, "type", "") == "valueNameSelection", cc_spec["params"])

        ok_pops = Dict{String,Any}("pops" => ["default/_tracked"])
        @test_throws ParamValidationError validate_params(
            _task_from_fun_name("trackTools.cumulativeChange"), merge(ok_pops, Dict{String,Any}("gap" => 99)))
        # `pops` is required, so an empty selection is refused BEFORE the run rather than logged after
        @test_throws ParamValidationError validate_params(
            _task_from_fun_name("trackTools.cumulativeChange"), Dict{String,Any}("gap" => 3))
        @test validate_params(
            _task_from_fun_name("trackTools.cumulativeChange"), merge(ok_pops, Dict{String,Any}("gap" => 3))) === nothing

        # The window arithmetic, pinned. Verified against real spleen tracks (project 4kS67f, image
        # 3w4IY5): 482 tracked cells in 17 tracks at gap=3 produced exactly 482 - 17*3 = 431 windows,
        # i.e. each track loses precisely `gap` leading cells and no more. That off-by-one is the whole
        # correctness risk here, and it is invisible without either real data or this test.
        coords = [Float64[Float64(i), 0.0] for i in 1:10]        # straight line, unit steps
        d, s, st = Cecelia._cc_track(coords, 3)
        @test count(isfinite, d) == length(coords) - 3           # only the leading `gap` are NaN
        @test all(isnan, d[1:3]) && all(isfinite, d[4:end])
        @test all(≈(3.0), filter(isfinite, d))                   # 3 unit steps in a straight line
        @test all(≈(1.0), filter(isfinite, s))                   # speed = displacement / gap
        @test all(≈(1.0), filter(isfinite, st))                  # perfectly directed → straightness 1

        # A cell that never moves has zero path length: straightness is UNDEFINED, not 0 — reporting 0
        # would say "searching in place" about a cell that produced no evidence either way.
        still = [Float64[0.0, 0.0] for _ in 1:6]
        _, _, st0 = Cecelia._cc_track(still, 2)
        @test all(isnan, st0)
    finally
        # leave the global registry as we found it — later testsets enumerate available fun_names
        Cecelia._unregister_task!("trackTools.cumulativeChange")
        Cecelia._unregister_task!("tracking.importCsvTracks")
    end
end

@testset "importTracks hides what does not apply" begin
    # A param that cannot be answered from the form as it stands renders NOWHERE, rather than sitting
    # there empty. The report: picking a TrackMate track XML left "Column mapping" showing five empty
    # dropdowns — correct (that export has no columns) but indistinguishable from a failed load.
    cfg = Cecelia.config_dir()
    src = joinpath(dirname(dirname(dirname(pathof(Cecelia)))),
                   "docs", "examples", "plugins", "ccia-importTracks")
    dst = joinpath(cfg, "modules", Cecelia.PLUGINS_SUBDIR, "ccia-importTracks")
    mkpath(dirname(dst)); cp(src, dst; force = true)
    Cecelia.load_custom_modules!(; dev_dir = cfg)
    task = Cecelia._task_from_fun_name("tracking.importCsvTracks")
    @test task !== nothing

    tmp = mktempdir()
    xml = joinpath(tmp, "t.xml"); write(xml, "<Tracks><particle><detection t=\"0\" x=\"1\" y=\"2\" /></particle></Tracks>")
    csv = joinpath(tmp, "t.csv"); write(csv, "TRACK_ID,FRAME,X,Y\n1,0,3,4\n")

    # Walk params + one level of section sub-params — `hidden` has to be findable wherever it is set.
    function flat(spec)
        out = Dict{String,Any}()
        function go(ps)
            ps isa AbstractVector || return
            for p in ps
                p isa AbstractDict || continue
                out[string(get(p, "key", ""))] = p
                go(get(p, "params", nothing))
            end
        end
        go(get(spec, "params", nothing)); out
    end
    hidden(spec, k) = get(get(flat(spec), k, Dict{String,Any}()), "hidden", false) === true
    resolve(form) = Cecelia._inject_dynamic_options!(deepcopy(Cecelia._task_spec(task)), task, form)

    # The XML rule is NOT in the hook any more, and that is the point. "This export has no columns" is
    # decided by the file EXTENSION — a property of the string already in the form — so it is a
    # `showIf` on the spec, evaluated on every render. In the hook it was re-resolved only when the
    # user EDITED the path, so a form restored from a previous run, or redrawn once the run finished,
    # showed the column mapping for an XML import (reported on screen).
    spec_of(k) = get(flat(Cecelia._task_spec(task)), k, Dict{String,Any}())
    for k in ("columnMapping", "template")
        @test get(spec_of(k), "showIf", nothing) == Dict("csvPath" => Dict("notEndsWith" => ".xml"))
    end
    sx = resolve(Dict("csvPath" => xml, "mode" => "attach"))
    @test !hidden(sx, "columnMapping")      # the hook no longer has an opinion
    @test !hidden(sx, "csvPath")

    # …and the shared predicate agrees with the frontend's, which is what keeps `required` + `showIf`
    # from disagreeing between the Run button and the server.
    cm = spec_of("columnMapping")
    @test !Cecelia._show_if_satisfied(cm, Dict("csvPath" => xml))
    @test  Cecelia._show_if_satisfied(cm, Dict("csvPath" => csv))
    @test !Cecelia._show_if_satisfied(cm, Dict("csvPath" => uppercase(xml)))   # case-insensitive

    # What the hook still does need the file OPEN for: the column names themselves.
    sc = resolve(Dict("csvPath" => csv, "mode" => "attach"))
    @test [o["value"] for o in flat(sc)["trackColumn"]["options"]] == ["TRACK_ID", "FRAME", "X", "Y"]

    # The mode rules are NOT here any more — they moved into the spec as `showIf`, because they are
    # decidable from the form alone. The hook keeps only what the form cannot answer. Asserting the
    # split explicitly, so moving a rule back into Julia is a test failure and not a quiet regression.
    spec_of(k) = get(flat(Cecelia._task_spec(task)), k, Dict{String,Any}())
    @test get(spec_of("valueName"), "showIf", nothing) == Dict("mode" => "attach")
    @test get(spec_of("maxDistance"), "showIf", nothing) == Dict("mode" => "attach")
    @test get(spec_of("outputValueName"), "showIf", nothing) == Dict("mode" => "create")
    for m in ("attach", "create")      # the hook is now blind to mode
        @test !hidden(resolve(Dict("csvPath" => csv, "mode" => m)), "valueName")
        @test !hidden(resolve(Dict("csvPath" => csv, "mode" => m)), "outputValueName")
    end

    # Hiding must not make a param unsettable: the spec the RUNNER validates is untouched, so a value
    # carried over from before a mode switch still validates rather than erroring on a field nobody
    # can see. (`hidden` is presentation; `validate_params` reads the base spec.)
    @test !hidden(Cecelia._task_spec(task), "columnMapping")

    # No `hidden` survives an XML → CSV switch, because the hook sets none at all now.
    @test !hidden(resolve(Dict("csvPath" => csv, "mode" => "attach")), "columnMapping")
    # No teardown: the loader `include`s a plugin's .jl once, so unregistering here would leave the
    # task unrecoverable for the testset below — which installs the same plugin and tears it down.
end

@testset "Import tracks with NO segmentation (points)" begin
    # The driving case: tracks exported from another tool for an image cecelia has never segmented.
    # There is nothing to match against, so each detection becomes a cell. This works because
    # `img_value_names` lists ccid.json's `label_props` keys — NOT the label store — and `is_tracked`
    # asks only for a `track_id` obs column. No mask exists, so it is motility-only by construction.
    # Installed into the REAL (hermetic) config dir, not a side one: `run_py` resolves the plugin's
    # python/ dir through `config_dir()` with no dev_dir override, so a plugin loaded from elsewhere
    # would import nothing. That is only a test concern — an installed plugin always lives here.
    cfg = Cecelia.config_dir()
    src = joinpath(dirname(dirname(dirname(pathof(Cecelia)))),
                   "docs", "examples", "plugins", "ccia-importTracks")
    dst = joinpath(cfg, "modules", Cecelia.PLUGINS_SUBDIR, "ccia-importTracks")
    mkpath(dirname(dst)); cp(src, dst; force = true)
    xmldir = mktempdir()

    # A TrackMate "Export tracks to XML": <particle> IS a track, and it carries NO id — so the id is
    # the particle's position in the file. Two tracks, so an off-by-one is visible.
    xml = joinpath(xmldir, "tracks.xml")
    write(xml, """<?xml version="1.0" encoding="UTF-8"?>
    <Tracks nTracks="2" spaceUnits="micron" frameInterval="15.0" timeUnits="sec">
      <particle nSpots="2">
        <detection t="0" x="1.0" y="2.0" z="4.0" />
        <detection t="1" x="1.5" y="2.5" z="4.0" />
      </particle>
      <particle nSpots="2">
        <detection t="0" x="9.0" y="8.0" z="4.0" />
        <detection t="1" x="9.5" y="8.5" z="4.0" />
      </particle>
    </Tracks>""")

    proj = create_project!(name = "pointsimport")
    set  = add_set!(proj; name = "s1")
    img  = add_image!(set; name = "unsegmented")
    mkpath(img_label_props_dir(img))
    img.meta["PhysicalSizeX"] = 0.5; img.meta["PhysicalSizeY"] = 0.5; img.meta["PhysicalSizeZ"] = 2.0
    save!(img); save!(set); save!(proj)

    @test isempty(img_value_names(img))          # nothing segmented — the whole point
    load_custom_modules!(; dev_dir = cfg)
    try
        t = _task_from_fun_name("tracking.importCsvTracks")
        res = run_task(t, img, Dict{String,Any}(
            "mode" => "create", "outputValueName" => "tm",
            "csvPath" => xml, "template" => "trackmate_xml"))
        @test res !== nothing

        # REGISTERED, not merely written: an .h5ad nobody recorded is invisible to every picker.
        @test "tm" ∈ img_value_names(img)
        @test is_tracked(img; value_name = "tm")

        lp = label_props(img; value_name = "tm")
        @test centroid_columns(lp) == ["centroid_z", "centroid_y", "centroid_x"]
        @test temporal_columns(lp) == ["centroid_t"]
        df = lp |> select_cols(vcat(["track_id"], centroid_columns(lp), temporal_columns(lp))) |> as_df
        @test nrow(df) == 4

        # Track ids are 1-BASED. `track_props` keeps `track_id > 0`, so a 0-based ordinal silently
        # drops the whole first track — 314 in, 313 out on the real export that found this.
        @test sort(unique(df.track_id)) == [1, 2]
        @test nrow(track_props(img; value_name = "tm")) == 2

        # µm → pixels, using the image's own calibration: x 1.0µm ÷ 0.5 = 2px, z 4.0µm ÷ 2.0 = 2px.
        # Getting the Z,Y,X order wrong here would divide x by the z spacing.
        @test minimum(df[!, "centroid_x"]) ≈ 2.0f0
        @test all(≈(2.0f0), df[!, "centroid_z"])

        # RE-IMPORTING THE SAME NAME MUST WORK. The first version refused any existing name, which made
        # the ordinary case impossible — supplying a corrected file and updating the tracking you had
        # already named ("so i have no chance of updating the tracking"). Re-running a task
        # over its own output is what every other task does.
        @test run_task(t, img, Dict{String,Any}(
            "mode" => "create", "outputValueName" => "tm",
            "csvPath" => xml, "template" => "trackmate_xml")) !== nothing
        @test count(==( "tm"), img_value_names(img)) == 1      # replaced, not duplicated

        # What IS still refused: landing points on a name that has MASK PIXELS behind it. The mask
        # would stay while its cells were replaced by unrelated detections, and every population
        # scoped to that segmentation would silently start reading someone else's rows.
        img.labels["seg"] = ["seg.zarr"]; save!(img)
        @test run_task(t, img, Dict{String,Any}(
            "mode" => "create", "outputValueName" => "seg",
            "csvPath" => xml, "template" => "trackmate_xml")) === nothing
    finally
        Cecelia._unregister_task!("tracking.importCsvTracks")
        rm(joinpath(cfg, "modules", Cecelia.PLUGINS_SUBDIR); recursive = true, force = true)
    end
end

@testset "Plugin install / remove (P2)" begin
    # URL → tarball + directory name. GitHub serves any ref as an archive, which is why no `git`
    # binary is needed — an installed app has none (see PLUGINS_PLAN R1).
    @test Cecelia.plugin_tarball_url("https://github.com/schienstockd/ccia-importTracks", "v0.1.0") ==
          "https://github.com/schienstockd/ccia-importTracks/archive/v0.1.0.tar.gz"
    @test Cecelia.plugin_tarball_url("https://github.com/o/r.git", "") ==
          "https://github.com/o/r/archive/HEAD.tar.gz"
    # a direct tarball URL is passed through untouched
    @test Cecelia.plugin_tarball_url("https://example.org/p.tar.gz", "abc") == "https://example.org/p.tar.gz"
    # the DIRECTORY takes the repo name, so one repo maps to one directory with no lookup table
    @test Cecelia.plugin_name_from_url("https://github.com/schienstockd/ccia-importTracks") == "ccia-importTracks"
    @test Cecelia.plugin_name_from_url("https://github.com/o/r.git") == "r"

    cfg = mktempdir()
    src = joinpath(dirname(dirname(dirname(pathof(Cecelia)))),
                   "docs", "examples", "plugins", "ccia-importTracks")
    # Build a GitHub-shaped archive: everything wrapped in ONE `<repo>-<ref>/` directory, which the
    # unpacker has to see through without assuming it is always there.
    pack = mktempdir(); cp(src, joinpath(pack, "ccia-importTracks-main"))
    tarball = joinpath(mktempdir(), "p.tar.gz")
    # Packed through the canonical builder, not a raw `tar -czf $tarball`: an absolute `-f` on
    # Windows reads as `host:path` and tar tries a REMOTE archive, which is the one thing
    # `_tar_pack_cmd` exists to prevent. Building the fixture by hand reintroduced it.
    run(pipeline(Cecelia._tar_pack_cmd(tarball, joinpath(pack, "ccia-importTracks-main"));
                 stdout = devnull, stderr = devnull))

    res = Cecelia.plugin_unpack!(tarball, "https://github.com/schienstockd/ccia-importTracks";
                                 ref = "main", dev_dir = cfg)
    @test res.ok && res.name == "ccia-importTracks"
    @test isfile(joinpath(res.dir, Cecelia.PLUGIN_MANIFEST))     # wrapper dir seen through
    @test isfile(joinpath(res.dir, "tracking", "importCsvTracks.jl"))

    # The install record is a SIBLING of plugin.json, never inside it — plugin.json ships from the
    # plugin's own repo, so writing the ref into it would dirty the checkout and be overwritten by the
    # next update (Decision 5).
    rec = Cecelia.read_install_record(res.dir)
    @test rec["ref"] == "main" && occursin("ccia-importTracks", rec["url"])
    @test !occursin("url", read(joinpath(res.dir, Cecelia.PLUGIN_MANIFEST), String))
    @test Cecelia.PLUGIN_INSTALL_RECORD ∉ [e.category for e in Cecelia.user_task_specs(; dev_dir = cfg)]

    # A hand-cloned plugin has no record and must NOT look broken for it
    hand = joinpath(Cecelia.plugins_dir(cfg), "hand-rolled"); mkpath(hand)
    write(joinpath(hand, Cecelia.PLUGIN_MANIFEST), JSON3.write(Dict("name" => "hand-rolled")))
    @test isempty(Cecelia.read_install_record(hand))

    # An archive that is not a plugin is refused BEFORE anything is moved into place — otherwise the
    # loader would walk a half-written directory on the next reload.
    junk = mktempdir(); write(joinpath(junk, "readme.txt"), "nope")
    jt = joinpath(mktempdir(), "junk.tar.gz")
    # One FILE at the root — the hand-rolled shape with no `<repo>-<ref>/` wrapper to see through.
    run(pipeline(Cecelia._tar_pack_cmd(jt, joinpath(junk, "readme.txt"));
                 stdout = devnull, stderr = devnull))
    bad = Cecelia.plugin_unpack!(jt, "https://github.com/o/notaplugin"; dev_dir = cfg)
    @test !bad.ok && occursin("no plugin.json", replace(bad.error, "$(Cecelia.PLUGIN_MANIFEST)" => "plugin.json"))
    @test !isdir(joinpath(Cecelia.plugins_dir(cfg), "notaplugin"))   # nothing left behind

    # Remove unregisters the tasks and deletes the directory.
    Cecelia.load_custom_modules!(; dev_dir = cfg)
    try
        rm_res = Cecelia.plugin_remove!("ccia-importTracks"; dev_dir = cfg)
        @test rm_res.ok
        @test "tracking.importCsvTracks" ∈ rm_res.removed
        @test !isdir(res.dir)
        @test_throws Exception _task_from_fun_name("tracking.importCsvTracks")
        @test !Cecelia.plugin_remove!("ccia-importTracks"; dev_dir = cfg).ok   # gone → not ok
    finally
        Cecelia._unregister_task!("tracking.importCsvTracks")
        Cecelia._unregister_task!("trackTools.cumulativeChange")
    end
end

@testset "Resource pool mapping" begin
    # Pins the pool recategorisation (cpu/gpu/io/network). A task's pool comes from its JSON
    # resource_pool via _task_pool_name; a spec-less task falls back to "cpu".
    poolof(fn) = Cecelia._task_pool_name(_task_from_fun_name(fn))
    @test poolof("segment.cellpose")              == "gpu"
    @test poolof("segment.cellposeMeasure")       == "gpu"
    @test poolof("importImages.omezarr")          == "io"
    @test poolof("importImages.migrateLegacy")    == "io"
    @test poolof("editImages.cropImage")          == "io"
    @test poolof("editImages.copyImage")          == "io"
    @test poolof("cleanupImages.afCorrect")       == "cpu"
    @test poolof("tracking.bayesian_tracking")    == "cpu"
    @test poolof("segment.measureLabels")         == "cpu"
    # no strays: every built-in task resolves to one of the four canonical pools
    for fn in keys(Cecelia._fun_name_map())
        @test poolof(fn) in ("cpu", "gpu", "io", "network")
    end
end

@testset "Live pool limit (set_pool_limit!)" begin
    # resize a pool live AND persist to custom.toml. Redirect the config dir to a temp so the
    # real custom.toml is never touched (config_dir reads CECELIA_DEV_DIR live).
    prev = get(ENV, "CECELIA_DEV_DIR", nothing)
    tmp  = mktempdir()
    ENV["CECELIA_DEV_DIR"] = tmp
    try
        @test set_pool_limit!("io", 3) == 3
        @test any(p -> p.name == "io" && p.limit == 3, list_pools())   # applied live
        @test set_pool_limit!("io", 0)   == 1                           # clamped to ≥ 1
        @test set_pool_limit!("io", 999) == Cecelia.POOL_LIMIT_MAX      # clamped to max
        txt = read(joinpath(tmp, "custom.toml"), String)                # persisted under [pools]
        @test occursin("[pools]", txt) && occursin("io", txt)
    finally
        prev === nothing ? delete!(ENV, "CECELIA_DEV_DIR") : (ENV["CECELIA_DEV_DIR"] = prev)
        resize_pool!("io", 8)   # restore default so later tests are unaffected
    end
end

@testset "Pool status snapshot (pool_status)" begin
    # pool_status() joins the pool registry (limit + in-flight slots) with the task registry
    # (queued count). With no tasks in flight during the test, running/queued are 0 and the
    # limit reflects the live pool budget.
    resize_pool!("cpu", 7)
    st = pool_status()
    cpu = only(filter(p -> p.name == "cpu", st))
    @test cpu.limit   == 7          # live budget
    @test cpu.running == 0          # nothing executing in the test
    @test cpu.queued  == 0          # nothing queued
    # every configured pool is present with the same field set the UI consumes
    @test Set(keys(cpu)) == Set((:name, :limit, :running, :queued))
    @test Set(p.name for p in st) == Set(p.name for p in list_pools())
    resize_pool!("cpu", 20)         # restore default so later tests are unaffected
end

@testset "Custom module reload prunes deleted files" begin
    # load a module from a temp config dir, then delete its .jl and reload → the task must be
    # unregistered (no longer dispatchable) and dropped from the load report. Regression: the
    # report used to only accumulate, so a deleted module stayed "loaded"/green forever.
    tmp = mktempdir()
    srcdir = joinpath(tmp, "modules", "sources", "tmpcat");            mkpath(srcdir)
    defdir = joinpath(tmp, "modules", "inputDefinitions", "tmpcat");   mkpath(defdir)
    spec = joinpath(defdir, "pruneMe.json")
    write(spec, JSON3.write(Dict(
        "fun_name" => "tmpcat.pruneMe", "label" => "Prune me",
        "resource_pool" => "cpu", "scope" => "image", "params" => Any[])))
    jl = joinpath(srcdir, "pruneMe.jl")
    write(jl, """
        struct _PruneMeTask <: Cecelia.CciaTask end
        Cecelia.register_task!("tmpcat.pruneMe", _PruneMeTask(); spec = $(repr(spec)))
        """)

    load_custom_modules!(; dev_dir = tmp)
    @test _task_from_fun_name("tmpcat.pruneMe") isa CciaTask
    @test any(m -> m.path == jl, custom_modules_report())

    rm(jl)                                   # user deletes the module file
    res = load_custom_modules!(; dev_dir = tmp)
    @test jl in res.removed                  # reported as unloaded
    @test_throws Exception _task_from_fun_name("tmpcat.pruneMe")   # no longer dispatchable
    @test !any(m -> m.path == jl, custom_modules_report())         # gone from the report
end
