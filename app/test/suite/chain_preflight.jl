# ── Chain pre-flight (tasks/chain/preflight.jl) ───────────────────────────
# The testpr fixture image (a temp copy): static and uncalibrated, channels ch0–ch3, one label set `B`,
# image version `default` — plus a `smoothed` version registered here so a re-smooth has one to replace.

@testset "Chain pre-flight — names what a run will hit on an image" begin
    have_fixture(fixture_path("testpr")) || return @test_skip "testpr fixture missing"
    dir = mktempdir(); cp(fixture_path("testpr"), joinpath(dir, "testpr"))
    ccid = joinpath(dir, "testpr", "1", "KDIeEm", "ccid.json")
    raw = JSON3.read(read(ccid, String), Dict{String,Any})
    raw["filepath"]["smoothed"] = "ccidImage.ome.zarr"
    write(ccid, JSON3.write(raw))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir   # in memory only — never set_projects_dir
        proj = load_project("testpr")
        node(id, fn, p...) = chain_node(fn; id, params = Dict{String,Any}(p...))
        tpl = ChainTemplate("pf", [
                node("af", "cleanupImages.afCorrect", "valueName" => "default", "targetChannel" => ["ch0"]),
                node("sm", "cleanupImages.smooth", "valueName" => "afCorrected", "channels" => ["SHG"]),
                node("gate", "gate.manual_gate", "valueName" => "T"),
                node("drift", "cleanupImages.driftCorrect", "valueName" => "default")],
            [ChainEdge("af", "sm")], String[])
        ps = chain_preflight(proj, tpl, ["KDIeEm"])
        by(id) = [p for p in ps if p["nodeId"] == id]
        # a channel the image lacks fails, by name
        @test any(p -> p["level"] == "fail" && occursin("SHG", p["message"]), by("sm"))
        # `afCorrected` is not on the image yet, but the step before makes it — no complaint about it
        @test !any(p -> occursin("afCorrected", p["message"]), by("sm"))
        # an input that is neither on the image nor made in the chain fails, by name
        @test any(p -> p["level"] == "fail" && occursin("'T'", p["message"]), by("gate"))
        # writing over an existing version warns (unless previous versions are kept)
        Cecelia.keep_previous_version() ||
            @test any(p -> p["level"] == "warn" && occursin("replaces the existing 'smoothed'", p["message"]), by("sm"))
        # a step the image cannot take (no T axis) is reported as skipped, not as a failure
        @test only(by("drift"))["level"] == "warn" && occursin("skipped", only(by("drift"))["message"])
        # nothing to say about a clean step
        clean = ChainTemplate("ok", [node("a", "cleanupImages.afCorrect", "valueName" => "default",
                                          "targetChannel" => ["ch1"])], ChainEdge[], String[])
        @test isempty(chain_preflight(proj, clean, ["KDIeEm"]))
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
end
