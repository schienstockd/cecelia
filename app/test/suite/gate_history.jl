# ── Gate history + content hashes: a run can be tied to the gate it read ───────────────────────────
# docs/todo/CECELIA_STRUCTURE_PLAN.md → Record (gates overwritten in place; GUI gate edits leave no
# record). `save_pop_map!` appends the previous state to `gating/{vn}.history.jsonl`; each population has
# a content hash (`pop_gate_hashes`); the scheduler stamps the hashes of the pops a run reads as the
# run-log entry's `gates`. See app/src/gating/popmanager/history.jl.

_gh_rect(x0, x1) = RectangleGate("x", "y", x0, x1, 0.0, 10.0)

@testset "Gate history: every definition change appends the previous state" begin
    td = mktempdir()
    @test read_gating_history(td, "B") == Any[]                 # no history = unknown, not an error

    m = PopulationMap(pop_type = "flow", value_name = "B")
    add_pop!(m, "cd4"; parent = ROOT, gate = _gh_rect(0.0, 10.0), colour = "#f00")
    add_pop!(m, "cd8"; parent = "/cd4", gate = _gh_rect(0.0, 5.0), colour = "#0f0")
    save_pop_map!(m, td)
    h = read_gating_history(td, "B")
    @test length(h) == 1 && h[1]["kind"] == "add" && h[1]["previous"] === nothing
    @test sort([String(c["path"]) for c in h[1]["changes"]]) == ["/cd4", "/cd4/cd8"]
    @test haskey(h[1], "author") && String(h[1]["valueName"]) == "B" && String(h[1]["popType"]) == "flow"
    h0 = pop_gate_hashes(load_pop_map(td, "B"))

    # cosmetic writes (colour, show) and a no-op save record nothing
    m.pops["/cd4"].colour = "#00f"; save_pop_map!(m, td)
    save_pop_map!(m, td)
    @test length(read_gating_history(td, "B")) == 1

    # a moved parent gate: one `change` for the parent, the previous tree kept whole; the child's own
    # record did not change, but its hash did (membership is ∩ parent)
    set_gate!(m, "/cd4", _gh_rect(1.0, 10.0)); save_pop_map!(m, td)
    h = read_gating_history(td, "B")
    @test length(h) == 2 && h[2]["kind"] == "change"
    c = only(h[2]["changes"])
    @test c["path"] == "/cd4" && c["previousHash"] == h0["/cd4"] && c["hash"] != h0["/cd4"]
    @test h[2]["previous"]["populations"][1]["gate"]["x_min"] == 0.0
    h1 = pop_gate_hashes(load_pop_map(td, "B"))
    @test h1["/cd4/cd8"] != h0["/cd4/cd8"]

    # a rename keeps the cells, so it keeps the hash: one `move`, not a delete plus an add
    rename_pop!(m, "/cd4/cd8", "cd8a"); save_pop_map!(m, td)
    c = only(read_gating_history(td, "B")[3]["changes"])
    @test c["kind"] == "move" && c["previousPath"] == "/cd4/cd8" && c["path"] == "/cd4/cd8a"
    @test c["hash"] == c["previousHash"] == h1["/cd4/cd8"]

    del_pop!(m, "/cd4/cd8a"); save_pop_map!(m, td)
    e = read_gating_history(td, "B")[4]
    @test e["kind"] == "delete" && only(e["changes"])["path"] == "/cd4/cd8a"

    # the hash names a gate however long ago it was replaced: current file first, then the history
    s = gate_state_for_hash(td, "B", h1["/cd4"])
    @test s.current && s.path == "/cd4" && s.at === nothing
    s = gate_state_for_hash(td, "B", h0["/cd4"])
    @test !s.current && s.path == "/cd4" && s.definition["gate"]["x_min"] == 0.0 && !isempty(s.at)
    s = gate_state_for_hash(td, "B", h0["/cd4/cd8"])
    @test s.path == "/cd4/cd8" && s.definition["gate"]["x_max"] == 5.0
    @test gate_state_for_hash(td, "B", "0000000000000000") === nothing

    # the history file is invisible to the sidecar readers: the map loads as before
    @test isfile(gating_history_path(td, "B")) && endswith(gating_history_path(td, "B"), "B.history.jsonl")
    @test pop_paths(load_pop_map(td, "B")) == ["/cd4"]

    # a torn last line (a kill mid-append) is skipped, and the next entry starts on a fresh line
    open(io -> write(io, "{\"at\":"), gating_history_path(td, "B"), "a")
    set_gate!(m, "/cd4", _gh_rect(2.0, 10.0)); save_pop_map!(m, td)
    @test length(read_gating_history(td, "B")) == 5
end

@testset "Gate hash: same definition, same hash; boolean terms and spatial unit count" begin
    mk(x1) = (m = PopulationMap(pop_type = "flow", value_name = "B");
              add_pop!(m, "a"; parent = ROOT, gate = _gh_rect(0.0, x1));
              add_pop!(m, "b"; parent = ROOT, gate = _gh_rect(5.0, 9.0));
              add_pop!(m, "a_not_b"; parent = ROOT); set_boolean!(m, "/a_not_b"; op = "and", pops = ["/a"], nots = ["/b"]);
              m)
    rt(m) = from_tree(JSON3.read(JSON3.write(to_tree(m)), Dict{String,Any}))
    ha, hb = pop_gate_hashes(rt(mk(10.0))), pop_gate_hashes(rt(mk(11.0)))
    @test ha["/b"] == hb["/b"]                                  # untouched gate
    @test ha["/a"] != hb["/a"] && ha["/a_not_b"] != hb["/a_not_b"]   # a term moved → the combination moved
    @test length(ha["/a"]) == 16
    # the unit is part of a POSITION gate, and only of one: an intensity gate is unit-free
    m = mk(10.0); m.spatial_unit = "um"
    @test pop_gate_hashes(rt(m))["/b"] == ha["/b"]
    pos(unit) = (m = PopulationMap(pop_type = "flow", value_name = "B", spatial_unit = unit);
                 add_pop!(m, "left"; parent = ROOT, gate = RectangleGate("centroid_x", "centroid_y", 0.0, 50.0, 0.0, 50.0));
                 pop_gate_hashes(rt(m))["/left"])
    @test pos("um") != pos("px")
    # a px → µm restamp of an intensity-only map (the image form of load does it) is no change: no entry
    td = mktempdir(); m = mk(10.0); save_pop_map!(m, td); n = length(read_gating_history(td, "B"))
    m.spatial_unit = "um"; save_pop_map!(m, td)
    @test length(read_gating_history(td, "B")) == n
    # a copy of the gate on another image is the same gate
    m2 = mk(10.0); m2.value_name = "C"
    @test pop_gate_hashes(rt(m2)) == ha
end

@testset "Run log stamps the hash of each gate a run read" begin
    have_fixture(fixture_path("testpr")) || return @test_skip "testpr fixture missing"
    dir = mktempdir(); cp(fixture_path("testpr"), joinpath(dir, "testpr"))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir   # in memory only — never set_projects_dir
        img = init_object("testpr", "KDIeEm")
        m = PopulationMap(pop_type = "flow", value_name = "B")
        add_pop!(m, "qc"; parent = ROOT, gate = _gh_rect(0.0, 10.0))
        save_pop_map!(m, img._dir)
        hq = pop_gate_hashes(load_pop_map(img._dir, "B"))["/qc"]

        cp_params = Dict{String,Any}("valueName" => "B", "popsA" => ["B/qc/_tracked"], "popsB" => ["B/"])
        p = run_provenance("spatialAnalysis.cellContacts", img, cp_params)
        @test p.gates == Any[Dict{String,Any}("valueName" => "B", "path" => "/qc", "hash" => hq)]
        # a TRACK analysis on `B/qc/_tracked` reads the cell gate `/qc` too; a track gate is found in
        # the track sidecar; a cluster pop (trackclust) is not a gate
        tm = PopulationMap(pop_type = "track", value_name = "B")
        add_pop!(tm, "fast"; parent = ROOT, gate = RectangleGate("speed", "y", 1.0, 9.0, 0.0, 10.0))
        save_pop_map!(tm, img._dir)
        hf = pop_gate_hashes(load_pop_map(img._dir, "B"; pop_type = "track"))["/fast"]
        g = run_gates("clustTracks.cluster", img,
                      Dict{String,Any}("valueName" => "B", "popsToCluster" => ["B/qc/_tracked", "B/fast", "B/Scanning"]))
        @test g == Any[Dict{String,Any}("valueName" => "B", "path" => "/qc", "hash" => hq),
                       Dict{String,Any}("valueName" => "B", "path" => "/fast", "hash" => hf, "popType" => "track")]
        @test gate_state_for_hash(img._dir, "B", hf; pop_type = "track").path == "/fast"
        # …and the edit guard counts that track run as a reader of `/qc`, as the record does
        tt = "ght-$(rand(1000:9999))"
        Cecelia.open_run_log!(img, "clustTracks.cluster", "B",
                              Dict{String,Any}("valueName" => "B", "popsToCluster" => ["B/qc/_tracked"]); task_id = tt)
        close_run_log!(img, tt, "done")
        @test "clustTracks.cluster" in get(run_pop_reads(img), ("B", "/qc"), String[])
        # a task that reads no population gets none
        @test run_provenance("cleanupImages.smooth", img, Dict{String,Any}("valueName" => "default")).gates == Any[]
        # a population that is not in the sidecar is left out rather than guessed
        @test run_gates("spatialAnalysis.cellContacts", img,
                        Dict{String,Any}("valueName" => "B", "popsA" => ["B/nope"], "popsB" => ["B/"])) == Any[]

        tid = "gh-$(rand(1000:9999))"
        Cecelia.open_run_log!(img, "spatialAnalysis.cellContacts", "B", cp_params; task_id = tid, gates = p.gates)
        e = only(filter(e -> String(get(e, "taskId", "")) == tid, read_run_log(img)))
        g = only(e["gates"])
        @test g["path"] == "/qc" && g["hash"] == hq

        # the gate is edited after the run: the entry still names the gate it read, via the history
        set_gate!(m, "/qc", _gh_rect(3.0, 10.0)); save_pop_map!(m, img._dir)
        s = gate_state_for_hash(img._dir, String(g["valueName"]), String(g["hash"]))
        @test !s.current && s.definition["gate"]["x_min"] == 0.0

        close_run_log!(img, tid, "done")
        # a label-set rename carries the history file and the run's `gates` with it
        Cecelia.rename_value_name!(img, "B", "B2")
        @test isfile(gating_history_path(img._dir, "B2")) && !isfile(gating_history_path(img._dir, "B"))
        @test last(read_gating_history(img._dir, "B2"))["kind"] == "rename"
        e = only(filter(e -> String(get(e, "taskId", "")) == tid, read_run_log(img)))
        @test only(e["gates"])["valueName"] == "B2"
        @test gate_state_for_hash(img._dir, "B2", hq) !== nothing
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
end

@testset "Scheduler stamps the gates a run read on its run-log entry" begin
    have_fixture(fixture_path("testpr")) || return @test_skip "testpr fixture missing"
    dir = mktempdir(); cp(fixture_path("testpr"), joinpath(dir, "testpr"))
    # the task measures in microns: give the image a pixel size so it passes its applicability check
    ccid = joinpath(dir, "testpr", "1", "KDIeEm", "ccid.json")
    raw = JSON3.read(read(ccid, String), Dict{String,Any})
    merge!(get!(raw, "meta", Dict{String,Any}()), Dict{String,Any}("PhysicalSizeX" => 0.5, "PhysicalSizeY" => 0.5,
                                                                   "PhysicalSizeZ" => 2.0, "PhysicalSizeUnit" => "µm"))
    write(ccid, JSON3.write(raw))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        img = init_object("testpr", "KDIeEm")
        m = PopulationMap(pop_type = "flow", value_name = "B")
        add_pop!(m, "qc"; parent = ROOT, gate = _gh_rect(0.0, 10.0))
        save_pop_map!(m, img._dir)
        hq = pop_gate_hashes(load_pop_map(img._dir, "B"))["/qc"]
        # a real task through `run_task`; whatever its outcome, the entry was opened (and stamped) first
        tid = "ghs-$(rand(1000:9999))"
        task = Cecelia._task_from_fun_name("spatialAnalysis.cellContacts")
        try
            run_task(task, img, Dict{String,Any}("valueName" => "B", "popsA" => ["B/qc"], "popsB" => ["B/qc"]); task_id = tid)
        catch      # the task's outcome is not under test (the fixture has no label store)
        end
        e = only(filter(e -> String(get(e, "taskId", "")) == tid, read_run_log(img)))
        @test only(e["gates"])["hash"] == hq
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
end
