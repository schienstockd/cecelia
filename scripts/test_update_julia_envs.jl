# Unit test for the pure helpers in `scripts/update_julia_envs.jl`: what the monthly update PR flags
# for review. A wrong flag here hides a breaking bump or a downgrade from the reviewer.
#
# Run: `julia scripts/test_update_julia_envs.jl`. Zero deps beyond stdlib `Test` + `TOML`.

using Test

include(joinpath(@__DIR__, "update_julia_envs.jl"))  # PROGRAM_FILE guard skips main()

@testset "change_kind" begin
    @test change_kind("1.8.2", "2.0.1") == "⚠ breaking"
    @test change_kind("0.12.18", "0.13.4") == "⚠ breaking"      # 0.x minor is breaking
    @test change_kind("0.4.4", "0.4.9") == ""                    # 0.x patch is not
    @test change_kind("2.3.0", "2.8.0") == ""
    @test change_kind("2.1.2+0", "1.14.3+3") == "⬇ downgrade"   # jll build suffix parses
    @test change_kind("5.0.1+0", "5.0.2+0") == ""
    @test change_kind("", "1.1.0") == ""                         # added / removed: not flagged
    @test change_kind("0.1.10", "") == ""
end

@testset "diff_rows" begin
    rows, flagged = diff_rows(Dict("A" => "1.0.0", "B" => "1.0.0", "C" => "0.1.0"),
                              Dict("A" => "2.0.0", "B" => "1.0.0", "D" => "1.0.0"),
                              Set(["A"]))
    @test length(rows) == 3                                      # A changed, C removed, D added
    @test rows[1] == "| **A** | 1.0.0 | 2.0.0 | ⚠ breaking |"      # direct dep bolded
    @test flagged == ["⚠ breaking **A** 1.0.0 → 2.0.0"]
end

@testset "drift" begin
    v = Dict("app" => Dict("HTTP" => "2.8.0", "JSON" => "1.0.0"),
             "api" => Dict("HTTP" => "2.8.0", "JSON" => "1.0.0"),
             "pluto" => Dict("HTTP" => "1.11.0"))
    @test drift(v) == ["HTTP — `app/` 2.8.0, `api/` 2.8.0, `pluto/` 1.11.0"]
end
