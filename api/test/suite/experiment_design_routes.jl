# Experiment design routes (api/src/routes/experiment_design.jl): GET reads and writes nothing, POST
# saves and records history, preview shows the grid for unsaved values and writes nothing.

@testset "API: experiment design" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    post(f, b) = (r = f(Vector{UInt8}(JSON3.write(b))); (r[1], JSON3.read(r[2])))
    get_(uid, set) = (r = api_sets_experiment_design(HTTP.Request("GET", "/api/sets/experiment-design?projectUid=$uid&setUid=$set")); (r[1], JSON3.read(r[2])))
    try
        proj = create_project!(name = "api-xd")
        s    = add_set!(proj; name = "T cell types")
        a    = add_image!(s; name = "M1-1", attr = Dict("Panel" => "A", "Mouse" => "M1"))
        b    = add_image!(s; name = "M1-2", attr = Dict("Panel" => "B"))

        @test api_sets_experiment_design(HTTP.Request("GET", "/api/sets/experiment-design?projectUid=$(proj.uid)"))[1] == 400
        st, r = get_(proj.uid, s.uid)
        @test st == 200 && r.design === nothing && r.grid === nothing && r.version == 0   # no design yet

        design = Dict("question" => "Do the panels differ?", "group" => ["Panel"], "parents" => ["Mouse"])
        # preview: b placed by a value not written yet; nothing saved
        st, p = post(api_sets_experiment_design_preview, Dict("projectUid" => proj.uid, "setUid" => s.uid,
                     "design" => design, "attrs" => Dict(b.uid => Dict("Mouse" => "M2"))))
        @test st == 200 && isempty(p.grid.unassigned) && length(p.grid.cells) == 2 && p.version == 0
        @test get_(proj.uid, s.uid)[2].design === nothing
        @test post(api_sets_experiment_design_preview, Dict("projectUid" => proj.uid, "setUid" => s.uid))[1] == 400

        # save: version 1, b unassigned until its mouse is named
        st, r1 = post(api_sets_experiment_design_save, Dict("projectUid" => proj.uid, "setUid" => s.uid,
                      "design" => design, "change" => "suggested"))
        @test st == 200 && r1.version == 1 && r1.design.status == "inferred" && r1.history[1].change == "suggested"
        @test [u.name for u in r1.grid.unassigned] == ["M1-2"]
        st, r2 = get_(proj.uid, s.uid)
        @test r2.hash == r1.hash && r2.version == 1

        # a shape error is a 400 and writes nothing
        st, e = post(api_sets_experiment_design_save, Dict("projectUid" => proj.uid, "setUid" => s.uid,
                     "design" => Dict("group" => ["Panel"], "parents" => ["Panel"])))
        @test st == 400 && occursin("twice", e.error) && get_(proj.uid, s.uid)[2].version == 1

        # confirm, then clear
        st, r3 = post(api_sets_experiment_design_save, Dict("projectUid" => proj.uid, "setUid" => s.uid,
                      "design" => merge(design, Dict("status" => "confirmed")), "change" => "confirmed"))
        @test r3.version == 2 && r3.history[2].status == "confirmed"
        st, r4 = post(api_sets_experiment_design_save, Dict("projectUid" => proj.uid, "setUid" => s.uid, "design" => nothing))
        @test st == 200 && r4.design === nothing && r4.version == 3 && r4.history[3].mode === nothing
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
    end
end
