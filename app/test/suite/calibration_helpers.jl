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
