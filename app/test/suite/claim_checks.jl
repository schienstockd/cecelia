# Claim checks (app/src/claim_checks.jl) — S0 record gate, S1 consistency, S2 conventions over a synthetic set.
# Every seeded fault is paired with a clean twin where the check must stay quiet: "nothing raised" on
# a real project is not evidence the checks work, only these pairs are.

# One synthetic B/T image: track tables on disk, per-name seg + tracking params, a qc gate under each
# segmentation, banked motionDims. Keyword overrides seed one fault at a time.
function _cc_image(uid; interval = 30, included = true, mouse = "1", diameter = 10,
                   dims = (B = 2, T = 2), tracks = ("B", "T"), flat_only = false)
    img = CciaImage(; uid = uid, dir = mktempdir())
    img.included = included
    img.attr["Mouse"] = mouse
    img.meta["TimeIncrement"] = interval
    img.meta["PhysicalSizeX"] = 0.596
    img.meta["PhysicalSizeY"] = 0.596
    img.meta["PhysicalSizeUnit"] = "µm"
    img.meta["TimeIncrementUnit"] = "s"
    mkpath(Cecelia.img_label_props_dir(img))
    for vn in tracks
        write(Cecelia.img_track_props_path(img, vn), "")
    end
    seg(vn) = Dict{String,Any}("outputValueName" => vn, "valueName" => "default",
        "models" => Dict("0" => Dict("model" => "cyto2", "cellDiameter" => diameter,
                                     "cellChannels" => [vn == "B" ? "Channel 2" : "Channel 3"])))
    trk(vn) = Dict{String,Any}("valueName" => vn, "dims" => "auto", "maxSearchRadius" => 20)
    if flat_only
        # the flat blob keeps only the LAST run per task: B's segmentation, T's tracking
        img.meta["funParams"] = Dict{String,Any}("segment.cellposeMeasure" => seg("B"),
                                                 "tracking.bayesian_track_measures" => trk("T"))
    else
        img.meta["funParamsByName"] = Dict{String,Any}(
            "segment.cellposeMeasure" => Dict{String,Any}("B" => seg("B"), "T" => seg("T")),
            "tracking.bayesian_track_measures" => Dict{String,Any}("B" => trk("B"), "T" => trk("T")))
    end
    for vn in ("B", "T")
        mkpath(Cecelia.gating_dir(img._dir))
        Cecelia.write_json_atomic(Cecelia.gating_path(img._dir, vn), Dict{String,Any}(
            "value_name" => vn, "pop_type" => "flow", "populations" => Any[Dict{String,Any}(
                "name" => "qc", "children" => Any[],
                "gate" => Dict{String,Any}("kind" => "polygon", "x_channel" => "volume",
                                           "vertices" => [[0, 0], [1, 0], [1, 1]]))]))
        write_qc(img, "tracking.track_measures", vn, Dict{String,Any}[];
                 metrics = Dict{String,Any}("motionDims" => getfield(dims, Symbol(vn))))
    end
    img
end

function _cc_set(imgs)
    set = CciaSet(; dir = mktempdir())
    for img in imgs
        push!(set._images, img); push!(set.image_uids, img.uid)
    end
    set
end

_cc_run(imgs) = claim_checks(ClaimScope(_cc_set(imgs), ["B/qc", "T/qc"], "live.track.speed"; unit_attrs = ["Mouse"]))
_cc_codes(r) = sort!([f["code"] for f in r["findings"] if f["level"] == "warn"])
_cc_find(r, code) = filter(f -> f["code"] == code, r["findings"])
# seven mice, one image each: one over the smallest paired design whose rank-test floor is under 0.05
# (six), so a fault that drops one image doesn't also trip S2's `claim.few_units`
_cc_clean(; kw...) = [_cc_image(u; mouse = m, kw...) for (u, m) in zip(("a", "b", "c", "d", "e", "f", "g"), string.(1:7))]

@testset "claim checks: S0 + S1 + S2 over a synthetic set" begin
    # ── the clean twin: nothing to warn about, and it says what ran ──
    clean = _cc_run(_cc_clean())
    @test isempty(_cc_codes(clean))
    @test "S1.betweenGroups" in clean["ran"] && "S0.dims" in clean["ran"] && isempty(clean["skipped"])
    @test clean["nUsed"] == 14
    @test "S2.unit" in clean["ran"] && "S2.calibration" in clean["ran"]
    @test _cc_find(clean, "claim.unit_count")[1]["detail"]["evidence"]["n"] == 7

    # ── wrong interval on one image ──
    imgs = _cc_clean(); imgs[3] = _cc_image("c"; mouse = "3", interval = 60)
    r = _cc_run(imgs)
    f = _cc_find(r, "claim.differs_across_images")
    @test any(x -> x["detail"]["evidence"]["variable"] == "meta.TimeIncrement", f)
    @test all(x -> x["detail"]["stage"] == "S1" && x["detail"]["provenance"] == "inferred", f)
    # a sub-tolerance pixel-size difference is display only (info), not a warning
    imgs = _cc_clean(); imgs[3].meta["PhysicalSizeX"] = 0.5964
    r = _cc_run(imgs)
    @test isempty(_cc_codes(r))
    @test length(_cc_find(r, "claim.within_tolerance")) == 2          # once per population
    @test _cc_find(r, "claim.within_tolerance")[1]["detail"]["display"] == true

    # ── one image excluded: S0 says so, S1 runs without it ──
    imgs = _cc_clean(); imgs[2].included = false
    r = _cc_run(imgs)
    @test _cc_codes(r) == ["claim.image_excluded"]
    @test _cc_find(r, "claim.image_excluded")[1]["detail"]["evidence"]["images"] == ["b"]
    @test r["nUsed"] == 12

    # ── one param changed on one image ──
    imgs = _cc_clean(); imgs[1] = _cc_image("a"; mouse = "1", diameter = 15)
    r = _cc_run(imgs)
    f = _cc_find(r, "claim.differs_across_images")
    @test length(f) == 2                                                # B and T both segmented at 15
    ev = f[1]["detail"]["evidence"]
    @test ev["variable"] == "params.segment.cellposeMeasure.models.0.cellDiameter"
    @test ev["images"] == ["a"] && ev["mode"] == "10"

    # ── B 2D vs T 3D in every image: the variable splits the groups ──
    r = _cc_run(_cc_clean(; dims = (B = 2, T = 3)))
    @test _cc_codes(r) == ["claim.confounded"]
    @test _cc_find(r, "claim.confounded")[1]["detail"]["evidence"]["variable"] == "motionDims"
    # …in one image only: a within-image difference, not a confound
    imgs = _cc_clean(); imgs[2] = _cc_image("b"; mouse = "2", dims = (B = 2, T = 3))
    r = _cc_run(imgs)
    @test "claim.differs_between_groups" in _cc_codes(r)
    @test _cc_find(r, "claim.differs_between_groups")[1]["detail"]["evidence"]["imagesDiffering"] == ["b"]
    # the channel each population was segmented from differs by design: never a between-group finding
    @test !any(f -> occursin("cellChannels", get(f["detail"]["evidence"], "variable", "")), r["findings"])

    # ── blank unit attribute ──
    imgs = _cc_clean(); imgs[2].attr["Mouse"] = ""
    r = _cc_run(imgs)
    @test _cc_codes(r) == ["claim.unit_blank"]
    @test _cc_find(r, "claim.unit_blank")[1]["detail"]["evidence"]["images"] == ["b"]

    # ── no tracks for one population on one image ──
    imgs = _cc_clean(); imgs[1] = _cc_image("a"; mouse = "1", tracks = ("B",))
    r = _cc_run(imgs)
    @test _cc_codes(r) == ["claim.no_data"]
    @test _cc_find(r, "claim.no_data")[1]["detail"]["evidence"]["pop"] == "T/qc"
    @test r["nUsed"] == 13

    # ── params only in the flat last-run blob: the other population's run is gone ──
    r = _cc_run(_cc_clean(; flat_only = true))
    nr = _cc_find(r, "claim.params_not_recorded")
    @test Set((x["detail"]["evidence"]["pop"], x["detail"]["evidence"]["kind"]) for x in nr) ==
          Set([("B/qc", "tracking"), ("T/qc", "segment")])
    @test length(_cc_find(r, "claim.params_last_run_only")) == 2
    @test !isempty(_cc_find(r, "claim.not_compared"))                  # one group has no record

    # ── banked dims missing (a run from before track measures banked them) ──
    imgs = _cc_clean(); rm(qc_path(imgs[1], "tracking.track_measures", "T"))
    r = _cc_run(imgs)
    @test _cc_codes(r) == ["claim.dims_not_recorded"]

    # ── a setting present in one image's run and absent in another's is a difference ──
    imgs = _cc_clean()
    delete!(imgs[3].meta["funParamsByName"]["tracking.bayesian_track_measures"]["B"], "maxSearchRadius")
    r = _cc_run(imgs)
    f = _cc_find(r, "claim.differs_across_images")
    @test length(f) == 1 && f[1]["detail"]["evidence"]["images"] == ["c"]
    @test haskey(f[1]["detail"]["evidence"]["byValue"], "(not set)")

    # ── one population: S1 between groups is skipped and says why ──
    r1 = claim_checks(ClaimScope(_cc_set(_cc_clean()), ["B/qc"], "live.track.speed"))
    @test haskey(r1["skipped"], "S1.betweenGroups") && haskey(r1["skipped"], "S0.unit")

    # ── S2: no unit named ──
    r = claim_checks(ClaimScope(_cc_set(_cc_clean()), ["B/qc", "T/qc"], "live.track.speed"))
    @test _cc_codes(r) == ["claim.no_unit"]
    @test !("S2.unit" in r["ran"])

    # ── S2: four mice, paired — a rank test per mouse can't go below 2/16 ──
    r = _cc_run(_cc_clean()[1:4])
    @test _cc_codes(r) == ["claim.few_units"]
    ev = _cc_find(r, "claim.few_units")[1]["detail"]["evidence"]
    @test ev["design"] == "paired" && ev["n"] == 4 && ev["minP"] ≈ 0.125
    # 4kS67f's shape: six images, four mice (mouse 3 imaged three times)
    imgs = _cc_clean()[1:6]; imgs[4].attr["Mouse"] = "3"; imgs[5].attr["Mouse"] = "3"
    r = _cc_run(imgs)
    @test _cc_codes(r) == ["claim.few_units"]
    ipu = _cc_find(r, "claim.images_per_unit")
    @test length(ipu) == 1 && ipu[1]["detail"]["evidence"]["units"] == ["3"]
    @test isempty(_cc_find(_cc_run(_cc_clean()), "claim.images_per_unit"))   # clean twin: one image each
    # …the replicate as Mouse × Location: mouse 3's three images are three sites → six replicates,
    # one image each. Per mouse alone they'd still be four.
    for (img, loc) in zip(imgs, ("a", "a", "a", "b", "c", "a")); img.attr["Location"] = loc; end
    rml = claim_checks(ClaimScope(_cc_set(imgs), ["B/qc", "T/qc"], "live.track.speed"; unit_attrs = ["Mouse", "Location"]))
    ev = _cc_find(rml, "claim.unit_count")[1]["detail"]["evidence"]
    @test ev["n"] == 6 && ev["attrs"] == ["Mouse", "Location"] && haskey(ev["imagesPerUnit"], "3 × b")
    @test _cc_codes(rml) == [] && isempty(_cc_find(rml, "claim.images_per_unit"))
    @test claim_unit_attrs([" Mouse ", "", "Mouse", "Location"]) == ["Mouse", "Location"]
    @test claim_unit_attrs("Mouse") == ["Mouse"] && claim_unit_attrs(nothing) == String[]
    # …a site left blank makes that image's replicate uncountable — S0 names the combined attribute
    delete!(imgs[5].attr, "Location")
    rml = claim_checks(ClaimScope(_cc_set(imgs), ["B/qc", "T/qc"], "live.track.speed"; unit_attrs = ["Mouse", "Location"]))
    @test "claim.unit_blank" in _cc_codes(rml)
    @test occursin("Mouse × Location", _cc_find(rml, "claim.unit_blank")[1]["short"])

    # ── S2: unpaired floors (exact Mann–Whitney): 2 vs 2 → 1/3, 4 vs 4 → 2/70 ──
    @test claim_min_p_unpaired(2, 2) ≈ 1 / 3
    @test claim_min_p_unpaired(4, 4) ≈ 2 / 70
    @test claim_min_p_paired(5) > 0.05 && claim_min_p_paired(6) < 0.05

    # ── S2: a speed read on an image with no frame-interval unit ──
    imgs = _cc_clean(); delete!(imgs[2].meta, "TimeIncrementUnit")
    r = _cc_run(imgs)
    @test _cc_codes(r) == ["claim.uncalibrated"]
    @test collect(keys(_cc_find(r, "claim.uncalibrated")[1]["detail"]["evidence"]["images"])) == ["b"]
    # …an intensity claim doesn't need calibration: quiet, and says it skipped the check
    imgs = _cc_clean(); delete!(imgs[2].meta, "PhysicalSizeUnit")
    set = _cc_set(imgs)
    rows = claim_record_table(ClaimScope(set, ["B/qc", "T/qc"], "live.track.speed"; unit_attrs = ["Mouse"]))
    @test _cc_codes(claim_checks(rows, ClaimScope(set, ["B/qc", "T/qc"], "live.track.speed"; unit_attrs = ["Mouse"]))) ==
          ["claim.uncalibrated"]
    ri = claim_checks(rows, ClaimScope(set, ["B/qc", "T/qc"], "mean_intensity_1"; unit_attrs = ["Mouse"]))
    @test isempty(_cc_find(ri, "claim.uncalibrated")) && haskey(ri["skipped"], "S2.calibration")

    # every claim finding renders from the catalog (no unfilled placeholder)
    @test all(f -> !occursin("{", f["short"] * f["long"]), r["findings"])
end
