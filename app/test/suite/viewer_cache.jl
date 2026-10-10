# ── Viewer decoded-chunk cache budget: `[viewer].chunkCache` config helpers ──
# The cache itself lives in api/src/chunk_cache.jl (tested in api/test); this pins how the setting
# resolves to bytes. Hermetic — runtests.jl points CECELIA_DEV_DIR at a throwaway dir.

using Test
using Cecelia

@testset "viewer chunk cache budget — choices, auto sizing, roundtrip" begin
    # auto is sized from THIS machine's RAM and clamped, never a fixed number from the dev box
    auto = viewer_chunk_cache_auto_bytes()
    @test 256 * 2^20 <= auto <= 4 * 2^30
    @test auto == clamp(floor(Int, Sys.total_memory() / 16), 256 * 2^20, 4 * 2^30)
    @test viewer_chunk_cache_bytes("auto") == auto
    @test viewer_chunk_cache_bytes("off") == 0
    @test viewer_chunk_cache_bytes("512") == 512 * 2^20
    @test "auto" in VIEWER_CACHE_CHOICES && "off" in VIEWER_CACHE_CHOICES

    prior = viewer_chunk_cache_setting()
    try
        @test set_viewer_chunk_cache!("2048") == "2048"
        @test viewer_chunk_cache_setting() == "2048"
        @test viewer_chunk_cache_bytes() == 2048 * 2^20
        @test_throws ArgumentError set_viewer_chunk_cache!("lots")
        @test viewer_chunk_cache_setting() == "2048"          # a rejected value changes nothing
    finally
        set_viewer_chunk_cache!(prior)
    end
end
