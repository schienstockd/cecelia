# ── Replay reproduces: the record alone is enough to make the same result again ──
# docs/SCHEDULER.md → *Replay*. A real measure step (Python, CPU, deterministic) on the committed CONTMESH
# fixture, run with overrides the template does not hold; then the template moves on and a run of it
# changes the measures; a replay from the first run's record must put back the first run's measures,
# value for value. Exact equality: regionprops + a scipy gaussian on the CPU have no nondeterminism.
@testset "Chain replay reproduces a real step's measures (CONTMESH)" begin
    src = fixture_path("CONTMESH", "1", "CtMsh1", "labels", "seg.zarr")
    have_fixture(src) || return @test_skip "chain replay reproduction (CONTMESH fixture missing)"
    td = mktempdir()
    cp(fixture_path("CONTMESH"), joinpath(td, "CONTMESH"))
    write(joinpath(td, "CONTMESH", "project.json"),
          JSON3.write(Dict("class" => "CciaProject", "uid" => "CONTMESH", "name" => "replay", "set_uids" => [])))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = td   # in memory only — never set_projects_dir
        proj = load_project("CONTMESH")
        uid  = "CtMsh1"
        measures() = sort(label_props(init_object("CONTMESH", uid); value_name = "seg") |> as_df, :label)
        tpl(p...) = ChainTemplate("replay-measure",
            [ChainNode(id = "m", fn = "segment.measureLabels", scope = "image",
                       params = Dict{String,Any}("outputValueName" => "seg", "intensityValueName" => "default", p...))],
            ChainEdge[])
        logs = String[]
        save_chain_template!(proj, tpl())
        first = run_chain(proj, [uid]; chain = "replay-measure", on_log = l -> push!(logs, l),
                          overrides = Dict{String,Any}("m" => Dict("gaussianFilter" => 1.5, "intensityMeasure" => "median")))
        @test first.image_states[uid]["m"].status == NODE_DONE || error(join(logs, "\n"))
        made = measures()
        icols = [c for c in names(made) if occursin("intensity", c)]
        @test !isempty(icols) && nrow(made) == 6

        # the template moves on, and a run of it makes different measures
        save_chain_template!(proj, tpl("intensityMeasure" => "mean"))
        other = run_chain(proj, [uid]; chain = "replay-measure", on_log = l -> push!(logs, l))
        @test other.image_states[uid]["m"].status == NODE_DONE
        @test !isequal(measures(), made)

        # replay from the first run's record: same measures, every column, every label
        again = run_chain(proj, String[]; from_run = first.id, on_log = l -> push!(logs, l))
        @test again.from_run == first.id && again.image_states[uid]["m"].status == NODE_DONE
        @test again.node_params == first.node_params
        back = measures()
        @test names(back) == names(made)
        @test isequal(back, made)
        # and the image's run log says the two runs ran with the same params
        rl = [e for e in read_run_log(init_object("CONTMESH", uid)) if get(e, "fun", "") == "segment.measureLabels"]
        @test length(rl) == 3 && rl[3]["params"] == rl[1]["params"] && rl[2]["params"] != rl[1]["params"]
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
end
