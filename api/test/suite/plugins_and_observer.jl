# Custom modules + plugins + observer status testsets — extracted from api/test/runtests.jl.
#
# Four testsets covering the plugin/custom-modules + observer-status surface:
#  - `API: custom modules status/reload` — /api/tasks/custom-modules read + reload.
#  - `API: a plugin task options can depend on the form` (optionsFrom/showIf pass-through).
#  - `API: a plugin task gets a form and a nav entry`.
#  - `API: observer status + feedback validation`.
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: custom modules status/reload" begin
    # Read-only status: shape is { dir, modules, plugins, clashes, categories }; dir is <config_dir>/modules.
    st, body = api_custom_modules_status(HTTP.Request("GET", "/api/tasks/custom-modules"))
    @test st == 200
    d = JSON3.read(body)
    @test endswith(String(d.dir), joinpath("modules"))
    @test haskey(d, :modules)
    @test haskey(d, :categories)   # drives the generic new-category page + "Custom" nav group
    @test haskey(d, :plugins)      # installed plugin sets — docs/todo/PLUGINS_PLAN.md
    @test haskey(d, :clashes)      # fun_names a module registered but did NOT get

    # Reload rescans; with no modules dir present it returns empty lists, never errors. It returns the
    # SAME payload as status plus the run's outcome, so the two can't drift.
    st2, body2 = api_custom_modules_reload(Vector{UInt8}("{}"))
    @test st2 == 200
    d2 = JSON3.read(body2)
    @test haskey(d2, :loaded) && haskey(d2, :failed) && haskey(d2, :categories)
    @test haskey(d2, :plugins) && haskey(d2, :clashes)
end

@testset "API: a plugin task's options can depend on the form" begin
    # The import builder (docs/todo/PLUGINS_PLAN.md → P1.5): the column fields offer the columns of
    # the file the user just picked. Two things this pins, both previously broken:
    #   1. dynamic options resolved through `_fun_name_map()` — BUILT-INS ONLY — so a plugin task
    #      overloading the hooks got its options at validation time (via `_task_spec`, which
    #      dispatches on the instance) but never in the served form. Picker and validator disagreed.
    #   2. options could only come from disk, never from what the user had typed.
    mods = joinpath(Cecelia.config_dir(), "modules")
    pdir = joinpath(mods, Cecelia.PLUGINS_SUBDIR, "ccia-importTracks")
    src  = joinpath(dirname(dirname(dirname(pathof(Cecelia)))),
                    "docs", "examples", "plugins", "ccia-importTracks")
    mkpath(dirname(pdir)); cp(src, pdir; force = true)
    csv = joinpath(mods, "spots.csv")
    write(csv, "Nb,Track n°,Slice n°,X,Y,Z\n1,7,1,10.0,20.0,3.0\n")
    Cecelia.load_custom_modules!()
    try
        _cols(body, key) = begin
            defs = JSON3.read(body)
            spec = only(filter(s -> String(get(s, :fun_name, "")) == "tracking.importCsvTracks",
                               collect(defs.tracking)))
            found = Ref{Any}(nothing)
            walk(ps) = for p in ps
                String(get(p, :key, "")) == key && (found[] = get(p, :options, nothing))
                haskey(p, :params) && walk(p.params)
            end
            walk(spec.params)
            found[]
        end

        # with the file in hand, the column fields offer ITS headers — including the non-ASCII `°`
        q = "/api/tasks/definitions?category=tracking&params=" *
            HTTP.escapeuri(JSON3.write(Dict("csvPath" => csv)))
        st, body = api_task_definitions(HTTP.Request("GET", q))
        @test st == 200
        opts = _cols(body, "trackColumn")
        @test opts !== nothing
        vals = String[String(o.value) for o in opts]
        @test "Track n°" ∈ vals && "Slice n°" ∈ vals && "X" ∈ vals
        @test String.(getproperty.(_cols(body, "yColumn"), :value)) == vals   # every column field

        # without form state there is nothing to offer, and the request still succeeds — a fresh page
        # load must render the form, just with an empty column picker until a file is chosen
        st2, body2 = api_task_definitions(HTTP.Request("GET", "/api/tasks/definitions?category=tracking"))
        @test st2 == 200
        @test isempty(something(_cols(body2, "trackColumn"), []))

        # malformed form state degrades to no options rather than 400-ing the page
        st3, body3 = api_task_definitions(
            HTTP.Request("GET", "/api/tasks/definitions?category=tracking&params=not-json"))
        @test st3 == 200
        @test isempty(something(_cols(body3, "trackColumn"), []))
    finally
        Cecelia._unregister_task!("tracking.importCsvTracks")
        rm(joinpath(mods, Cecelia.PLUGINS_SUBDIR); recursive = true, force = true)
        rm(csv; force = true)
    end
end

@testset "API: a plugin's task gets a form and a nav entry" begin
    # THE P1 blocker, end to end (docs/todo/PLUGINS_PLAN.md). The Julia loader walks the modules tree
    # recursively, but both API scans did a one-level readdir — so a plugin's task registered and ran
    # while its `.json` sat one level too deep to be seen: no form, no nav entry. Both scans now go
    # through Cecelia.user_task_specs, which knows the plugins/<plugin>/<category>/ shape explicitly.
    mods = joinpath(Cecelia.config_dir(), "modules")
    pdir = joinpath(mods, Cecelia.PLUGINS_SUBDIR, "trackimport-smithlab")
    mkpath(joinpath(pdir, "tracking"))
    write(joinpath(pdir, "plugin.json"),
          JSON3.write(Dict("name" => "trackimport-smithlab", "version" => "0.2.0")))
    write(joinpath(pdir, "tracking", "importSmith.json"),
          JSON3.write(Dict("fun_name" => "tracking.importSmith", "label" => "Import Smith tracks",
                           "resource_pool" => "cpu", "scope" => "image", "params" => [])))
    try
        # 1) the FORM: the spec is merged into the tracking category, not a category named after the plugin
        st, body = api_task_definitions(HTTP.Request("GET", "/api/tasks/definitions?category=tracking"))
        @test st == 200
        defs = JSON3.read(body)
        @test haskey(defs, :tracking)
        @test any(s -> String(get(s, :fun_name, "")) == "tracking.importSmith", defs.tracking)

        # 2) the NAV entry: the category is `tracking` (the dir BELOW the plugin root), never the
        #    plugin name — PLUGINS_PLAN Decision 2.
        cats = _custom_module_categories()
        byname = Dict(String(c.name) => c for c in cats)
        @test haskey(byname, "tracking")
        @test "tracking.importSmith" ∈ byname["tracking"].funNames
        @test byname["tracking"].builtin == true          # tracking is a built-in page; no generic page
        @test !haskey(byname, "trackimport-smithlab")     # the plugin name is not a category
        @test !haskey(byname, "plugins")                  # nor is the plugin root

        # 3) it shows up as an installed plugin, with what it actually ships on disk
        st3, body3 = api_custom_modules_status(HTTP.Request("GET", "/api/tasks/custom-modules"))
        plugs = JSON3.read(body3).plugins
        smith = only(filter(p -> String(p.name) == "trackimport-smithlab", collect(plugs)))
        @test String(smith.version) == "0.2.0"
        @test "tracking" ∈ String.(smith.categories)
        # …and what it CONTRIBUTES reaches the client, so Settings can show it without a second call
        # (PLUGINS_PLAN Decision 10). The layout desugars: this plugin declares no `contributions`
        # block at all, and its task is listed anyway.
        @test "tracking.importSmith" ∈ [String(t.funName) for t in smith.contributions.tasks]
        @test isempty(smith.problems)
    finally
        rm(joinpath(mods, Cecelia.PLUGINS_SUBDIR); recursive = true, force = true)
    end
end

# Observer (in-app AI assistant) — status shape + request validation. The actual agent spawn (a real
# billed CLI call) is NOT exercised here; only the guard rails around it. See
# docs/todo/OBSERVER_INTEGRATION_PLAN.md + app/src/ai/agent_runner.jl (pure pieces tested in app/test).
@testset "API: observer status + feedback validation" begin
    # status: availability is a bool (true/false depending on whether `claude` is on PATH — don't
    # assert which, so it passes both in CI and on a dev box with Claude Code installed).
    st, body = api_observer_status(HTTP.Request("GET", "/api/observer/status"))
    @test st == 200
    let s = JSON3.read(body)
        @test s.available isa Bool
        # the picker's choices + shipped default are exposed so the panel can populate the dropdown
        @test Set(String.(s.models)) == Set(["haiku", "sonnet", "opus"])
        @test String(s.defaultModel) in Set(["haiku", "sonnet", "opus"])
        # the MCP config is written on STATUS (not only on a feedback run) so the info panel can always
        # offer `claude --mcp-config <path>` — the user never hand-registers an MCP server
        @test isfile(String(s.mcpConfigPath))
        let cfg = JSON3.read(read(String(s.mcpConfigPath), String))
            @test haskey(cfg.mcpServers, Symbol("cecelia-observer"))
        end
        # terminal-setup detection: which button the lab-log toolbar shows (setup vs Chat to Claude).
        # Don't assert WHICH state — it depends on the dev machine's ~/.claude.json — but `ready` must
        # mean exactly "current", since the UI treats a stale entry as not set up.
        @test String(s.terminal.state) in Set(["missing", "stale", "shadowed", "current"])
        @test s.terminal.ready isa Bool
        @test s.terminal.ready == (String(s.terminal.state) == "current")
        # a per-folder (`local`-scope) entry overrides our user-scope one, so "registered correctly"
        # is not the same as "the user's terminal works" — `shadowed` names the folders that break it
        # Asserted as implications, not an equality: a shadow can coexist with a missing/stale user-scope
        # entry, and then THAT is the headline state (setup still fixes both).
        @test s.terminal.shadowedDirs isa JSON3.Array
        String(s.terminal.state) == "shadowed" && @test !isempty(s.terminal.shadowedDirs)
        isempty(s.terminal.shadowedDirs) || @test !s.terminal.ready
    end

    # feedback: validated before anything is spawned.
    @test _post(api_observer_feedback, Dict())[1] == 400                       # projectUid missing
    @test _post(api_observer_feedback, Dict("projectUid" => "nope"))[1] == 404 # unknown project

    # clear context: same validation, no spawn.
    @test _post(api_observer_clear, Dict())[1] == 400                          # projectUid missing
    @test _post(api_observer_clear, Dict("projectUid" => "nope"))[1] == 404    # unknown project

    # register (one-click terminal setup) is deliberately NOT called here: on a machine with Claude
    # Code installed it would rewrite the developer's own ~/.claude.json. Its command builders are
    # pure and covered in app/test/runtests.jl (`_build_mcp_register_cmd`/`_build_mcp_remove_cmd`).
end

