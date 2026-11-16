# RESULT-REVIEW: result-0d022543
# An unnamed channel's fallback name must not resolve to a DIFFERENT channel on another image. The name
# resolver compares case-insensitively, and bioformats2raw labels channel 0 `CH1`; a 0-based `Ch1`
# fallback on an unnamed image made a column picked as `CH1` (channel 0) resolve to channel 1 there, so
# a pooled plot silently put two different channels under one marker. Within one image, `ch1` and a
# fallback `Ch1` were two names to the dedup but one, ambiguous, to the resolver.

@testset "unnamed-channel fallback lines up with bioformats2raw labels" begin
    vars = ["mean_intensity_0", "mean_intensity_1", "mean_intensity_2"]
    b = unique_channel_names(["", "", ""])                     # an image whose store left labels blank
    @test Cecelia.resolve_channel_columns(["CH1"], vars, b).cols == ["mean_intensity_0"]
    # within one image: a fallback never collides case-insensitively with a real name
    names = unique_channel_names(["ch2", ""])
    @test length(unique(lowercase.(names))) == 2
    @test isempty(Cecelia.resolve_channel_columns(["ch2"], vars[1:2], names).problems)
end
