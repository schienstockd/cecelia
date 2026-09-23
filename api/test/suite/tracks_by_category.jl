# Category-source linked brushing endpoint — LINKED_BRUSHING_PLAN.md P2.
#
# Validation testset: the handler enumerates ~6 required fields, coerces limit, resolves each
# image via `_gating_image`, and dedupes across images. Semantic tests against a real fixture
# are handled where the underlying `pop_df` reader is unit-tested — this file pins the wire-
# level guards so bad client input never reaches the reader.
#
# The `_catkey_local` clone at the top of `tracks_by_category_api.jl` stays in sync with
# `_catkey` (`app/src/plotting/plot_data.jl` line 42) by inspection — a one-liner both sides;
# the endpoint's file docstring calls out the exact source line so a future edit lands here.

@testset "API: /api/tracks/by_category — validation guards" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        uid = "TESTTBC"; mkpath(joinpath(tmp, uid))
        w(b) = _post(api_tracks_by_category, b)

        # Missing / empty required fields — each step past the previous field's guard.
        @test w(Dict())[1] == 400                                                   # projectUid
        @test w(Dict("projectUid"=>uid))[1] == 400                                  # valueName
        @test w(Dict("projectUid"=>uid, "valueName"=>"B"))[1] == 400                # pop
        @test w(Dict("projectUid"=>uid, "valueName"=>"B", "pop"=>"/T"))[1] == 400   # measure
        @test w(Dict("projectUid"=>uid, "valueName"=>"B", "pop"=>"/T",
                     "measure"=>"live.cell.hmm.state.movement"))[1] == 400          # category
        @test w(Dict("projectUid"=>uid, "valueName"=>"B", "pop"=>"/T",
                     "measure"=>"live.cell.hmm.state.movement", "category"=>"1"))[1] == 400   # imageUids
        @test w(Dict("projectUid"=>uid, "valueName"=>"B", "pop"=>"/T",
                     "measure"=>"live.cell.hmm.state.movement", "category"=>"1",
                     "imageUids"=>[]))[1] == 400                                    # empty array
        @test w(Dict("projectUid"=>uid, "valueName"=>"B", "pop"=>"/T",
                     "measure"=>"live.cell.hmm.state.movement", "category"=>"1",
                     "imageUids"=>[""]))[1] == 400                                  # array of empties

        # Bad body (not JSON).
        @test api_tracks_by_category(Vector{UInt8}("{ not json"))[1] == 400

        # Unknown project — flagged by _gating_image at the first uid.
        st, body = w(Dict("projectUid"=>"NOPE", "valueName"=>"B", "pop"=>"/T",
                          "measure"=>"live.cell.hmm.state.movement", "category"=>"1",
                          "imageUids"=>["ANY"]))
        @test st == 404
        @test occursin("Project not found", String(body))
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
    end
end
