# RESULT-REVIEW: result-4edc17f6
# A blank channel's filled-in name must never take a name a LATER channel already holds: saved gating
# columns, view states and task params refer to that name, meaning that later channel. Filling the blank
# first renamed the real holder to `GFP_2` and silently pointed every saved `GFP` at the blank channel.

@testset "a filled-in blank never takes a real channel's name" begin
    vars = ["mean_intensity_0", "mean_intensity_1"]
    @test unique_channel_names(["", "GFP"]; fallback = ["GFP"]) == ["GFP_2", "GFP"]
    @test unique_channel_names(["", "CH1"]) == ["CH1_2", "CH1"]
    for (names, fb, ref) in ((["", "GFP"], ["GFP"], "GFP"), (["", "CH1"], String[], "CH1"))
        @test Cecelia.resolve_channel_columns([ref], vars,
                  unique_channel_names(names; fallback = fb)).cols == ["mean_intensity_1"]
    end
end
