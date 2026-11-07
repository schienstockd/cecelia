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
                "gate" => Dict{String,Any}("kind" => "polygon", "x_channel" => "volume", "y_channel" => "area",
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
    @test length(_cc_find(r, "claim.within_tolerance")) == 1          # one fact, not one per population
    @test _cc_find(r, "claim.within_tolerance")[1]["detail"]["display"] == true

    # ── one image excluded: S0 says so, S1 runs without it ──
    imgs = _cc_clean(); imgs[2].included = false
    r = _cc_run(imgs)
    @test _cc_codes(r) == ["claim.image_excluded"]
    @test _cc_find(r, "claim.image_excluded")[1]["detail"]["evidence"]["images"] == ["b"]
    @test r["nUsed"] == 12
    @test _cc_find(r, "claim.image_excluded")[1]["short"] == "Excluded from the set: b"
    # the checks as facts: the one that found something is `warn`, the rest state what they confirmed
    st = Dict(l["id"] => l["state"] for l in r["checks"])
    @test st["S0.membership"] == "warn" && st["S0.data"] == "ok" && st["S2.calibration"] == "ok"
    @test only(l["text"] for l in r["checks"] if l["id"] == "S2.unit") == "Mouse: n = 6"
    # …grouped by what they are about: the excluded image makes Images a warning, the rest pass
    cats = Dict(c["id"] => c["state"] for c in r["categories"])
    @test [c["id"] for c in r["categories"]] == ["images", "record", "sameness", "replicates", "units"]
    @test cats["images"] == "warn" && cats["units"] == "ok" && r["categories"][1]["label"] == "Images"

    # ── one param changed on one image ──
    imgs = _cc_clean(); imgs[1] = _cc_image("a"; mouse = "1", diameter = 15)
    r = _cc_run(imgs)
    f = _cc_find(r, "claim.differs_across_images")
    @test length(f) == 1                                                # B and T both segmented at 15: one fact
    @test f[1]["short"] == "cellposeMeasure: models.0.cellDiameter differs across images for B and T"   # named, not keyed
    @test Cecelia._claim_variable_label("params.segment.cellposeMeasure.models.1.cellDiameter") !=
          Cecelia._claim_variable_label("params.segment.cellposeMeasure.models.0.cellDiameter")
    ev = f[1]["detail"]["evidence"]
    @test ev["pops"] == ["B", "T"] && f[1]["detail"]["check"] == "S1.acrossImages"
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
    nr = only(_cc_find(r, "claim.params_not_recorded"))                 # every gap in one fact
    @test Set((x["pop"], x["kind"]) for x in nr["detail"]["evidence"]["gaps"]) == Set([("B/qc", "tracking"), ("T/qc", "segment")])
    @test nr["short"] == "Processing params not on record: B tracking (7 images), T segment (7 images)"
    @test length(_cc_find(r, "claim.params_last_run_only")) == 1
    @test length(_cc_find(r, "claim.not_compared")) == 1                # one group has no record: one note

    # ── a FAILED re-run's params are not what produced the output ──
    # `funParamsByName` is written at dispatch whatever the outcome (a form default, so a failed run's
    # settings come back for a retry). Image a: B was made at diameter 10 (done), then a re-run at 15
    # failed — the by-name blob now holds 15, the run log says 10 made it. The record is the run log.
    imgs = _cc_clean()
    a = imgs[1]
    seg_b(d) = Dict{String,Any}("outputValueName" => "B", "valueName" => "default",
        "models" => Dict("0" => Dict("model" => "cyto2", "cellDiameter" => d, "cellChannels" => ["Channel 2"])))
    # the log records what `run_task` resolved (defaults filled, group order applied), not the form
    segtask = Cecelia._task_from_fun_name("segment.cellposeMeasure")
    append_run_log!(a, "segment.cellposeMeasure", "default", "done", Cecelia.resolve_run_params(segtask, seg_b(10), a))
    append_run_log!(a, "segment.cellposeMeasure", "default", "failed", seg_b(15))
    a.meta["funParamsByName"]["segment.cellposeMeasure"]["B"] = seg_b(15)
    # the other images read the form blob, which carries the form-only `modelsOrder` the run resolves
    # away: compared raw against a's run-log entry it would read as a difference nobody made
    for img in imgs[2:end]
        img.meta["funParamsByName"]["segment.cellposeMeasure"]["B"]["modelsOrder"] = ["0"]
    end
    r = _cc_run(imgs)
    @test isempty(_cc_codes(r))      # same processing everywhere, whichever source each image read
    row = only(filter(x -> x["image"] == "a" && x["pop"] == "B/qc",
                      claim_record_table(ClaimScope(_cc_set(imgs), ["B/qc", "T/qc"], "live.track.speed"))))
    @test row["paramsSource"]["segment"]["segment.cellposeMeasure"] == "runLog"
    @test row["vars"]["params.segment.cellposeMeasure.models.0.cellDiameter"] == 10
    # …and with no finished run in the log at all, the dispatch-time blob is shown as what it is
    imgs = _cc_clean()
    append_run_log!(imgs[1], "segment.cellposeMeasure", "default", "failed", seg_b(15))
    r = _cc_run(imgs)
    @test isempty(_cc_codes(r))
    ou = _cc_find(r, "claim.params_outcome_unrecorded")
    @test !isempty(ou) && all(x -> x["level"] == "info", ou)

    # ── mixed sources: run-log images are compared, a fallback image is a gap, not a difference ──
    # a–f have B's segmentation in the log; g only has the dispatch-time blob (its done run fell off
    # the log), and that blob says 15. The two shapes are not comparable, so g is named by S0 and
    # left out of S1 — never reported as segmented differently.
    imgs = _cc_clean()
    for img in imgs[1:6]
        append_run_log!(img, "segment.cellposeMeasure", "default", "done",
                        Cecelia.resolve_run_params(segtask, seg_b(10), img))
    end
    imgs[7].meta["funParamsByName"]["segment.cellposeMeasure"]["B"] = seg_b(15)
    r = _cc_run(imgs)
    @test !any(f -> occursin("cellDiameter", get(f["detail"]["evidence"], "variable", "")),
               filter(f -> f["level"] == "warn", r["findings"]))
    # one fact for every gap; B's segmentation gap is g alone
    ou = filter(x -> x["pop"] == "B/qc" && x["kind"] == "segment",
                only(_cc_find(r, "claim.params_outcome_unrecorded"))["detail"]["evidence"]["gaps"])
    @test length(ou) == 1 && ou[1]["images"] == ["g"]

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

    # ── a gate drawn differently on one image, for B and T: one fact, named plainly, gate advice ──
    # (on the record table — the rules are pure; the synthetic gates aren't what this exercises)
    sc = ClaimScope(_cc_set(_cc_clean()), ["B/qc", "T/qc"], "live.track.speed"; unit_attrs = ["Mouse"])
    rows = claim_record_table(sc)
    @test all(row -> row["hasGate"] && haskey(row["vars"], "gate.geometry"), rows)   # the fixture's gates load
    for row in rows; row["vars"]["gate.geometry"] = row["image"] == "b" ? "beef0001" : "cafe0002"; end
    r = claim_checks(rows, sc)
    gd = only(_cc_find(r, "claim.gate_differs"))
    @test gd["short"] == "Gate shape differs across images for B and T" && occursin("drawn per image", gd["long"])
    @test gd["detail"]["evidence"]["variable"] == "gate.geometry"          # evidence keeps the key
    @test isempty(_cc_find(r, "claim.differs_across_images"))

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
