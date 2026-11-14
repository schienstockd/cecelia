# ── Task pure-helper testsets (calibration + copy) ────────────────────
# Eight sections covering pure task helpers that carry calibration/provenance metadata
# forward: CropImage inherits source calibration, ZProject inherits source calibration,
# TProject inherits source calibration, BinImage rescales calibration by the factor,
# ResampleZ rewrites SizeZ to match XY spacing, Register stacks channels across cycles,
# CopyImage carries calibration + provenance, and the CopyImage copy-tree helper
# (recursive, verbatim). Extracted from suite.jl to keep it small enough to merge without
# EOF conflicts on every append. The extracted file loads inside this file's aggregating
# testset scope, so any helpers defined earlier in suite.jl are still in scope (lexical
# include).
#
# No `@__DIR__` scans in the extracted range — no path rewrites needed.

@testset "CropImage inherits source calibration (pure helper)" begin
    # A crop must carry the source's physical calibration onto the new image (else the metadata
    # dialog shows "—" and the strip timestamp has no Δt) — see cropImage.jl.
    src = Dict{String,Any}(
        "SizeC" => 4, "SizeZ" => 20, "SizeT" => 181,
        "PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33, "PhysicalSizeZ" => 2.0,
        "PhysicalSizeUnit" => "micrometer", "TimeIncrement" => 15, "TimeIncrementUnit" => "second",
        "ori_path" => "/should/not/carry")                       # non-calibration keys stay behind
    # Z trimmed [2,8), T kept whole (-1) → SizeZ shrinks, SizeT & the scale/unit carry over unchanged
    m = Cecelia._crop_inherited_meta(src, (; x0=0, x1=100, y0=0, y1=100, z0=2, z1=8, t0=-1, t1=-1))
    @test m["SizeZ"] == 6                     # 8 - 2 (half-open)
    @test m["SizeT"] == 181                   # axis kept → source count
    @test m["SizeC"] == 4                     # channels invariant under crop
    @test m["PhysicalSizeX"] == 0.33 && m["TimeIncrement"] == 15
    @test m["TimeIncrementUnit"] == "second"
    @test !haskey(m, "ori_path")              # only calibration is inherited
    # T also trimmed [10,40); a source missing SizeZ → no SizeZ key invented
    m2 = Cecelia._crop_inherited_meta(Dict{String,Any}("SizeC" => 2),
                                      (; x0=0, x1=50, y0=0, y1=50, z0=-1, z1=-1, t0=10, t1=40))
    @test m2["SizeT"] == 30 && m2["SizeC"] == 2 && !haskey(m2, "SizeZ")
end

@testset "Crop/copy inherit source acquisition settings (pure helpers)" begin
    # Read back through JSON3 the way a ccid.json is: nested values arrive immutable, Symbol-keyed.
    src = Cecelia.JSON3.read(Cecelia.JSON3.write(Dict(
        "ori_path" => "/data/raw.oir",
        "acquisition" => Dict("system" => "FV4000", "zStack" => Dict("stepUm" => 2.0, "count" => 31),
            "timelapse" => Dict("intervalS" => 15.0, "count" => 181),
            "channels" => [Dict("detector" => "D1"), Dict("detector" => "D2")],
            "brightZ" => Dict("enabled" => true, "points" => [
                Dict("zFromStartUm" => 0.0, "transmissionPct" => Dict("MaiTai" => 10.0)),
                Dict("zFromStartUm" => 60.0, "transmissionPct" => Dict("MaiTai" => 30.0))])))))
    store  = Dict{String,Any}("SizeC" => 2, "SizeZ" => 31, "SizeT" => 181)   # the cropped version
    padded = Dict{String,Any}("SizeC" => 2, "SizeZ" => 41, "SizeT" => 181)   # drift-corrected: Z padded
    box(; z0 = -1, z1 = -1, t0 = -1, t1 = -1) = (; x0 = 0, x1 = 8, y0 = 0, y1 = 8, z0, z1, t0, t1)
    # XY only: the settings hold as they are
    a = Cecelia.crop_acquisition(src, box(), store)["acquisition"]
    @test a["zStack"]["count"] == 31 && !haskey(a["timelapse"], "acquired")
    @test [p["zFromStartUm"] for p in a["brightZ"]["points"]] == [0.0, 60.0]
    # Z [5,15) + T [10,40): plane count, ramp from the crop's first plane, frames kept
    a = Cecelia.crop_acquisition(src, box(z0 = 5, z1 = 15, t0 = 10, t1 = 40), store)["acquisition"]
    @test a["zStack"]["count"] == 10 && a["zStack"]["stepUm"] == 2.0
    @test [p["zFromStartUm"] for p in a["brightZ"]["points"]] == [-10.0, 50.0]
    @test a["timelapse"]["acquired"] == 30 && a["timelapse"]["count"] == 181
    # the source is not mutated
    @test src[:acquisition][:brightZ][:points][1][:zFromStartUm] == 0.0
    # a padded (drift-corrected) version is not the acquired stack: neither crop nor copy claims it
    for m in (Cecelia.crop_acquisition(src, box(z0 = 5, z1 = 15), padded), Cecelia.inherited_acquisition(src, padded))
        @test !haskey(m, "acquisition")
        @test m["acquisitionMissing"] == Dict{String,Any}("reason" => "other-stack", "ext" => ".oir")
    end
    @test Cecelia.inherited_acquisition(src, store)["acquisition"]["system"] == "FV4000"   # a copy
    # no settings: the source's reason carries; nothing read yet: nothing
    miss = Dict{String,Any}("acquisitionMissing" => Dict{String,Any}("reason" => "format", "ext" => ".lif"))
    @test Cecelia.crop_acquisition(miss, box(), store)["acquisitionMissing"]["reason"] == "format"
    @test isempty(Cecelia.inherited_acquisition(Dict{String,Any}("SizeC" => 2), store))
end

@testset "Bin keeps the source's acquisition settings" begin
    # As a ccid.json reads back: nested values immutable, Symbol-keyed.
    src = Dict{String,Any}(String(k) => v for (k, v) in Cecelia.JSON3.read(Cecelia.JSON3.write(Dict(
        "SizeC" => 2, "SizeZ" => 31, "SizeT" => 5, "SizeX" => 64, "SizeY" => 64, "ori_path" => "/data/raw.oir",
        "acquisition" => Dict("system" => "FV4000", "zStack" => Dict("stepUm" => 2.0, "count" => 31),
            "channels" => [Dict("detector" => "D1"), Dict("detector" => "D2")])))))
    p = Cecelia.BinImageParams(factorX = 2, factorY = 2)
    m = Cecelia._bin_meta(src, "srcuid", p, Dict{String,Any}("SizeC" => 2, "SizeZ" => 31, "SizeT" => 5))
    @test m["acquisition"]["system"] == "FV4000" && m["acquisition"]["zStack"]["count"] == 31
    @test m["ori_path"] == "/data/raw.oir" && m["SizeX"] == 32 && m["bin_source_uid"] == "srcuid"
    # a binned drift-corrected (Z-padded) version: the settings describe another stack
    m = Cecelia._bin_meta(src, "srcuid", p, Dict{String,Any}("SizeC" => 2, "SizeZ" => 41, "SizeT" => 5))
    @test !haskey(m, "acquisition") && m["acquisitionMissing"]["reason"] == "other-stack"
end


@testset "ZProject inherits source calibration (pure helper)" begin
    # A Z-projection collapses SizeZ to 1 while every other calibration field carries over
    # unchanged (X/Y pixel size + unit, T interval, channels). Same source→new pattern as crop's
    # `_crop_inherited_meta`; non-calibration keys (e.g. `ori_path`) stay behind — the handler
    # opts them in separately.
    src = Dict{String,Any}(
        "SizeC" => 4, "SizeZ" => 20, "SizeT" => 181,
        "PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33, "PhysicalSizeZ" => 2.0,
        "PhysicalSizeUnit" => "micrometer", "TimeIncrement" => 15, "TimeIncrementUnit" => "second",
        "ori_path" => "/should/not/carry")
    m = Cecelia._zproj_inherited_meta(src)
    @test m["SizeZ"] == 1                     # projected → single plane
    @test m["SizeT"] == 181 && m["SizeC"] == 4
    @test m["PhysicalSizeX"] == 0.33 && m["PhysicalSizeY"] == 0.33
    @test m["PhysicalSizeZ"] == 2.0           # slice thickness kept — describes the SOURCE stack
    @test m["TimeIncrement"] == 15 && m["TimeIncrementUnit"] == "second"
    @test !haskey(m, "ori_path")              # only calibration inherited; the handler adds it
    # a source with no SizeZ (a 2D image) still ends up with SizeZ=1 — the projection is a no-op
    m2 = Cecelia._zproj_inherited_meta(Dict{String,Any}("SizeC" => 2, "SizeT" => 10))
    @test m2["SizeZ"] == 1 && m2["SizeC"] == 2 && m2["SizeT"] == 10
end

@testset "TProject inherits source calibration (pure helper)" begin
    # A T-projection collapses SizeT to 1; every other calibration field carries over unchanged
    # (X/Y/Z pixel size + unit, frame interval, channels). Same source→new pattern as crop's and
    # ZProject's helpers.
    src = Dict{String,Any}(
        "SizeC" => 4, "SizeZ" => 20, "SizeT" => 181,
        "PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33, "PhysicalSizeZ" => 2.0,
        "PhysicalSizeUnit" => "micrometer", "TimeIncrement" => 15, "TimeIncrementUnit" => "second",
        "ori_path" => "/should/not/carry")
    m = Cecelia._tproj_inherited_meta(src)
    @test m["SizeT"] == 1                     # projected → single frame
    @test m["SizeZ"] == 20 && m["SizeC"] == 4
    @test m["PhysicalSizeX"] == 0.33 && m["PhysicalSizeZ"] == 2.0
    @test m["TimeIncrement"] == 15            # kept: describes the SOURCE's frame spacing
    @test m["TimeIncrementUnit"] == "second"
    @test !haskey(m, "ori_path")
    # a source with no SizeT (a still image) still ends up with SizeT=1 — the projection is a no-op
    m2 = Cecelia._tproj_inherited_meta(Dict{String,Any}("SizeC" => 2, "SizeZ" => 8))
    @test m2["SizeT"] == 1 && m2["SizeC"] == 2 && m2["SizeZ"] == 8
end

@testset "BinImage rescales calibration by the factor (pure helper)" begin
    # A bin shrinks SizeX/Y by integer floor and grows PhysicalSizeX/Y by the same factor — the
    # binned pixel PHYSICALLY covers `factor` source pixels. Z/T/C invariant. Same source→new
    # pattern as crop's / ZProject's / TProject's helpers.
    src = Dict{String,Any}(
        "SizeC" => 4, "SizeZ" => 20, "SizeT" => 181, "SizeX" => 1024, "SizeY" => 512,
        "PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.5, "PhysicalSizeZ" => 2.0,
        "PhysicalSizeUnit" => "micrometer",
        "TimeIncrement" => 15, "TimeIncrementUnit" => "second",
        "ori_path" => "/should/not/carry")
    m = Cecelia._bin_inherited_meta(src, 2, 4)
    @test m["SizeX"] == 512 && m["SizeY"] == 128
    @test m["PhysicalSizeX"] ≈ 0.66
    @test m["PhysicalSizeY"] ≈ 2.0
    @test m["SizeZ"] == 20 && m["SizeT"] == 181 && m["SizeC"] == 4
    @test m["PhysicalSizeZ"] == 2.0 && m["TimeIncrement"] == 15
    @test !haskey(m, "ori_path")
    # ragged remainder: integer floor matches `coarsen(trim_excess=True)` — a source of 1025 with
    # factor 2 gives 512, and the last source pixel is dropped so calibration stays honest
    m2 = Cecelia._bin_inherited_meta(Dict{String,Any}("SizeX" => 1025, "SizeY" => 513,
                                                       "PhysicalSizeX" => 1.0, "PhysicalSizeY" => 1.0), 2, 2)
    @test m2["SizeX"] == 512 && m2["SizeY"] == 256
    @test m2["PhysicalSizeX"] ≈ 2.0 && m2["PhysicalSizeY"] ≈ 2.0
end

@testset "ResampleZ rewrites SizeZ to match XY spacing (pure helper)" begin
    # A Z-resample rewrites SizeZ to make the output isotropic (PhysicalSizeZ = PhysicalSizeX). XY
    # stays put; T/C invariant.
    src = Dict{String,Any}(
        "SizeC" => 4, "SizeZ" => 20, "SizeT" => 5, "SizeX" => 1024, "SizeY" => 512,
        "PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33, "PhysicalSizeZ" => 2.0,
        "PhysicalSizeUnit" => "micrometer",
        "TimeIncrement" => 15, "TimeIncrementUnit" => "second",
        "ori_path" => "/should/not/carry")
    m = Cecelia._resample_z_inherited_meta(src)
    # ratio = 2.0 / 0.33 ≈ 6.06 → 20 * 6.06 ≈ 121 planes at 0.33 µm apart
    @test m["SizeZ"] == round(Int, 20 * (2.0 / 0.33))
    @test m["PhysicalSizeZ"] == 0.33          # isotropic → matches X
    @test m["PhysicalSizeX"] == 0.33 && m["SizeC"] == 4 && m["SizeT"] == 5
    @test !haskey(m, "ori_path")
    # a source already isotropic (px_x == px_z) is a no-op on SizeZ
    m2 = Cecelia._resample_z_inherited_meta(Dict{String,Any}(
        "SizeZ" => 32, "PhysicalSizeX" => 1.0, "PhysicalSizeZ" => 1.0))
    @test m2["SizeZ"] == 32 && m2["PhysicalSizeZ"] == 1.0
    # SizeZ never rounds down below 1 even if the ratio would zero it out
    m3 = Cecelia._resample_z_inherited_meta(Dict{String,Any}(
        "SizeZ" => 1, "PhysicalSizeX" => 1.0, "PhysicalSizeZ" => 0.1))
    @test m3["SizeZ"] == 1                    # round(1 * 0.1) = 0, floored to 1
    # mixed per-axis units: X in nm, Z in µm → the ratio is taken in µm, Z written back in its own unit
    m4 = Cecelia._resample_z_inherited_meta(Dict{String,Any}(
        "PhysicalSizeX" => 500.0, "PhysicalSizeUnit" => "nm", "PhysicalSizeZ" => 2.0,
        "PhysicalSizeZUnit" => "µm", "SizeZ" => 10))
    @test m4["SizeZ"] == 40                   # 2 µm / 0.5 µm = 4× planes
    @test m4["PhysicalSizeZ"] ≈ 0.5 && m4["PhysicalSizeZUnit"] == "µm"
end

@testset "Register stacks channels across cycles (pure helper)" begin
    # Registration keeps the reference's extent + calibration + timeline; only the C dimension grows.
    # The formula (ref_C + Σ(cycle_C - 1)) is decided by the handler, so the helper just receives it
    # and stamps SizeC onto the inherited meta. Other calibration flows through untouched.
    ref = Dict{String,Any}(
        "SizeC" => 4, "SizeZ" => 12, "SizeT" => 1, "SizeX" => 512, "SizeY" => 512,
        "PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33, "PhysicalSizeZ" => 2.0,
        "PhysicalSizeUnit" => "micrometer",
        "TimeIncrement" => 0, "TimeIncrementUnit" => "second",
        "ori_path" => "/should/not/carry — handler adds it separately")
    m = Cecelia._register_inherited_meta(ref, 4 + (5 - 1) + (3 - 1))  # ref + two moving cycles
    @test m["SizeC"] == 10
    @test m["SizeZ"] == 12 && m["SizeT"] == 1 && m["SizeX"] == 512 && m["SizeY"] == 512
    @test m["PhysicalSizeX"] == 0.33 && m["PhysicalSizeZ"] == 2.0
    @test m["PhysicalSizeUnit"] == "micrometer"
    @test !haskey(m, "ori_path")   # the handler carries it, not this helper
    # a total_c of 1 still writes SizeC=1 (a single-cycle "self-registration" is a valid no-op)
    m2 = Cecelia._register_inherited_meta(Dict{String,Any}("SizeX" => 100, "SizeY" => 100), 1)
    @test m2["SizeC"] == 1
    @test m2["SizeX"] == 100 && m2["SizeY"] == 100
end

@testset "CopyImage carries calibration + provenance (pure helper)" begin
    # A copy is a faithful duplicate of ONE version: every calibration field carries over UNCHANGED
    # (unlike a crop), plus ori_path and a copy_source_* breadcrumb; non-calibration keys stay behind.
    src = Dict{String,Any}(
        "SizeC" => 4, "SizeZ" => 20, "SizeT" => 181,
        "PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33, "PhysicalSizeZ" => 2.0,
        "PhysicalSizeUnit" => "micrometer", "TimeIncrement" => 15, "TimeIncrementUnit" => "second",
        "ori_path" => "/data/raw.czi", "crop_box" => Dict("x0" => 0))   # crop_box must NOT carry
    m = Cecelia._copied_meta(src, "srcUID", "driftCorrected")
    @test m["SizeC"] == 4 && m["SizeZ"] == 20 && m["SizeT"] == 181   # unchanged (full copy)
    @test m["PhysicalSizeX"] == 0.33 && m["PhysicalSizeZ"] == 2.0
    @test m["TimeIncrement"] == 15 && m["TimeIncrementUnit"] == "second"
    @test m["ori_path"] == "/data/raw.czi"                           # same acquisition → provenance carried
    @test m["copy_source_uid"] == "srcUID" && m["copy_source_value_name"] == "driftCorrected"
    @test !haskey(m, "crop_box")                                     # only calibration/provenance
end

@testset "CopyImage copy-tree helper (recursive, verbatim)" begin
    # The zarr copy is a byte-for-byte directory copy (preserves layout/levels/OME sidecar), NOT a
    # zarr re-encode — assert nested files land intact and progress reports the true file count.
    src = mktempdir()
    dst = joinpath(mktempdir(), "out.ome.zarr")
    mkpath(joinpath(src, "0", "sub"))
    write(joinpath(src, ".zattrs"), "{\"multiscales\":[]}")
    write(joinpath(src, "0", "chunk"), "abc")
    write(joinpath(src, "0", "sub", "deep"), "xyz")
    last = Ref((0, 0))
    n = Cecelia._copy_tree_with_progress(src, dst; on_progress = (a, b) -> (last[] = (a, b)))
    @test n == 3
    @test last[][2] == 3                                             # total reported = file count
    @test read(joinpath(dst, ".zattrs"), String) == "{\"multiscales\":[]}"
    @test read(joinpath(dst, "0", "chunk"), String) == "abc"
    @test read(joinpath(dst, "0", "sub", "deep"), String) == "xyz"  # nested tree preserved
end

@testset "Every derived image keeps the frame interval WITH its provenance (TIME_META_KEYS)" begin
    # A declared interval (Immunemap) records its source in `TimeIncrementDeclared`. Each derive
    # helper named `TimeIncrement*` by hand, so the source note was dropped while the value
    # travelled — a crop of an Immunemap video read as if its interval came from the file.
    src = Dict{String,Any}(
        "SizeC" => 2, "SizeZ" => 9, "SizeT" => 60, "SizeX" => 512, "SizeY" => 512,
        "PhysicalSizeX" => 1.8, "PhysicalSizeY" => 1.8, "PhysicalSizeZ" => 4.0,
        "PhysicalSizeUnit" => "micrometer",
        "TimeIncrement" => 30.0, "TimeIncrementUnit" => "second",
        "TimeIncrementDeclared" => "Immunemap acquisition 309 (timestamps)")
    derived = Dict(
        "copy"     => Cecelia._copied_meta(src, "abc", "default"),
        "crop"     => Cecelia._crop_inherited_meta(src, (; x0=0, x1=100, y0=0, y1=100, z0=-1, z1=-1, t0=-1, t1=-1)),
        "zProject" => Cecelia._zproj_inherited_meta(src),
        "tProject" => Cecelia._tproj_inherited_meta(src),
        "bin"      => Cecelia._bin_inherited_meta(src, 2, 2),
        "resample" => Cecelia._resample_z_inherited_meta(src),
        "register" => Cecelia._register_inherited_meta(src, 2))
    for (what, m) in derived
        @test (what, get(m, "TimeIncrementDeclared", nothing)) == (what, src["TimeIncrementDeclared"])
        @test (what, get(m, "TimeIncrement", nothing)) == (what, 30.0)
    end
    # a source with no declared interval gains no key
    plain = delete!(copy(src), "TimeIncrementDeclared")
    @test !haskey(Cecelia._bin_inherited_meta(plain, 2, 2), "TimeIncrementDeclared")
end

@testset "Register: channel plan, names, acquisition and per-cycle QC (pure helpers)" begin
    # three cycles, DAPI first in each; cycle 3 has three channels
    plan = Cecelia._register_channel_plan([4, 4, 3], [0, 0, 0], true)
    @test length(plan) == 11 && plan[5] == (2, 0) && plan[9] == (3, 0)
    plan_drop = Cecelia._register_channel_plan([4, 4, 3], [0, 0, 0], false)
    @test length(plan_drop) == 4 + 3 + 2 && !((2, 0) in plan_drop)
    names = [["DAPI", "IL-10", "NOD2", "IL1-B"], ["DAPI", "NOS2", "B-actin", "TNFa"], ["DAPI", "CD3", "IL-10"]]
    out = Cecelia._register_channel_names(names, plan)
    @test out[1:4] == ["DAPI (cycle 1)", "IL-10 (cycle 1)", "NOD2", "IL1-B"]
    @test out[5] == "DAPI (cycle 2)" && out[6] == "NOS2" && out[10] == "CD3" && out[11] == "IL-10 (cycle 3)"
    @test Cecelia._register_channel_names([["A"], String[]], [(1, 0), (2, 1)]) == ["A", "ch1"]

    # per-channel acquisition follows the channel; the scan's own cycle number is kept as scanCycle
    acq(n, sub) = Dict{String,Any}("system" => "PhenoImager Fusion", "modes" => ["tiled"],
        "channels" => [Dict{String,Any}("order" => i, "name" => "c$i", "cycle" => 1, "afSubtracted" => sub) for i in 1:n])
    a = Cecelia._register_acquisition([acq(4, true), acq(4, false), acq(3, true)], plan_drop)
    @test length(a["channels"]) == 9
    @test a["channels"][5]["cycle"] == 2 && a["channels"][5]["scanCycle"] == 1
    @test a["channels"][5]["afSubtracted"] === false && a["channels"][8]["afSubtracted"] === true
    # `order` is the position in the registered image (what the name list is indexed by), not the
    # scan's own count, which restarts at 1 every cycle
    @test [c["order"] for c in a["channels"]] == collect(1:9)
    @test a["channels"][5]["scanOrder"] == 2 && a["channels"][8]["scanOrder"] == 2   # plan_drop skips each later DAPI
    # Refresh metadata repairs settings registered before the renumbering: stored orders 1-4,2-4,2-3 →
    # 1…9, once (a second refresh changes nothing), and only settings marked registered — which a crop
    # of the stack inherits, so it is repaired too
    stale = Dict{String,Any}("cycles" => Dict{String,Any}("registered" => true),
                             "channels" => [Dict{String,Any}("order" => o) for o in [1, 2, 3, 4, 2, 3, 4, 2, 3]])
    reg = (; meta = Dict{String,Any}())
    zm = Dict{String,Any}()
    @test Cecelia._repair_registered_orders!(zm, reg, stale)
    @test [c["order"] for c in zm["acquisition"]["channels"]] == collect(1:9) && zm["acquisition"]["channels"][5]["scanOrder"] == 2
    @test stale["channels"][5]["order"] == 2                      # the stored settings are not mutated
    @test !Cecelia._repair_registered_orders!(Dict{String,Any}(), reg, zm["acquisition"])
    single = Dict{String,Any}("channels" => [Dict{String,Any}("order" => o) for o in [2, 1]])   # not registered
    @test !Cecelia._repair_registered_orders!(Dict{String,Any}(), reg, single)
    stack = (; meta = Dict{String,Any}("register_reference_uid" => "ref"))           # the stack itself, flag or not
    @test Cecelia._repair_registered_orders!(Dict{String,Any}(), stack, delete!(deepcopy(stale), "cycles"))
    @test "cyclic" in a["modes"] && a["cycles"]["count"] == 3
    @test !haskey(a["cycles"], "experiments")              # no scan had a run file beside it
    scan_run(n, x) = merge(acq(n, true), Dict{String,Any}("cycles" => Dict{String,Any}("count" => 1, "experiment" => x)))
    a = Cecelia._register_acquisition([scan_run(4, "Cycle1-Slide2"), acq(4, true), scan_run(3, "Cycle3-slide2")], plan_drop)
    @test a["cycles"]["experiments"] == ["Cycle1-Slide2", nothing, "Cycle3-slide2"]
    @test isnothing(Cecelia._register_acquisition([acq(4, true), nothing, acq(3, true)], plan_drop))
    @test isnothing(Cecelia._register_acquisition([acq(4, true), acq(2, true), acq(3, true)], plan_drop))

    # QC: one warn per failing check, numbers in detail; ratios banked, no finding
    good = Dict{String,Any}("cycle" => 2, "name" => "c2", "residualUm" => Dict("median" => 0.4),
                            "tissueCovered" => 0.99, "tissueRetained" => 0.95, "tissueOnlyInCycle" => 0.1,
                            "signalRatio" => 0.3)
    bad = Dict{String,Any}("cycle" => 3, "name" => "c3", "residualUm" => Dict("median" => 4.2),
                           "tissueCovered" => 0.7, "tissueRetained" => 0.5)
    @test isempty(Cecelia._register_qc_findings([good]))
    f = Cecelia._register_qc_findings([good, bad])
    @test sort([x["code"] for x in f]) == ["register.misaligned", "register.tissue_lost", "register.tissue_not_covered"]
    @test all(x -> x["level"] == "warn" && x["detail"]["cycle"] == 3, f)
    m = Cecelia._register_qc_metrics([good, bad], 3)
    @test m["nCycles"] == 3 && m["worstResidualUm"] == 4.2 && m["minTissueRetained"] == 0.5
    @test m["minTissueCovered"] == 0.7 && m["maxTissueOnlyInCycle"] == 0.1
    # the whole-plane path writes no per-cycle checks: the count is still the images registered
    @test Cecelia._register_qc_metrics(Any[], 4) == Dict{String,Any}("nCycles" => 4)
    # the order `register_run.output_plan` writes ([2, 3], [0, 1] there, images 0-based)
    @test Cecelia._register_channel_plan([2, 3], [0, 1], false) == [(1, 0), (1, 1), (2, 0), (2, 2)]
end

@testset "AF correction skips channels the scanner already AF-subtracted" begin
    acq = Dict{String,Any}("channels" => [Dict{String,Any}("afSubtracted" => true),
                                          Dict{String,Any}("afSubtracted" => false),
                                          Dict{String,Any}(), Dict{String,Any}("afSubtracted" => true)])
    @test Cecelia.scanner_af_subtracted(acq) == Set([0, 3])
    @test isempty(Cecelia.scanner_af_subtracted(nothing))
    combos = Dict{String,Any}("0" => Dict(), "1" => Dict(), "3" => Dict(), "x" => Dict())
    @test Cecelia.drop_scanner_af_targets!(combos, acq) == [0, 3]
    # the import probe reads the same flag through the same reader
    @test sort(collect(keys(Cecelia.photons_untestable(acq)))) == ["0", "3"]
    # a slide scan's tissue survives into meta.saturation (the data-path note reads it)
    sat = Cecelia._saturation_from_result(Dict{String,Any}("channels" => [Dict("index" => 0)],
        "tissue" => Dict("fraction" => 0.052, "level" => 5, "patches" => 14)))
    @test sat["tissue"]["patches"] == 14
    @test sort(collect(keys(combos))) == ["1", "x"]
end
