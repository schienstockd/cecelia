# ── QC + AF correction + channel-index resolver testsets ──────────────
# Five sections covering: Smoothing QC (photon-limited fall of zeros, clipping detection),
# AF correction QC (retired-exemption catalog + per-channel plumbing), the "every QC finding
# carries the fields the GUI reads" ratchet (walkdir over app/src/*), the channel names →
# indices resolver (one resolver, 6-drift closure), and AF params being just channels.
# Extracted from suite.jl to keep it small enough to merge without EOF conflicts on every
# append. The extracted file loads inside this file's aggregating testset scope, so any
# helpers defined earlier in suite.jl are still in scope (lexical include).
#
# No `@__DIR__` scans in the extracted range — the two source scans (`every QC finding` walks
# `dirname(dirname(pathof(Cecelia)))/src`, and one channel-name test reads
# `dirname(pathof(Cecelia))`) are already pathof-anchored, so they resolve identically
# whether the file sits at app/test/ or app/test/suite/.

@testset "Smoothing QC" begin
    # Both findings key off the persisted python stats, so the helper is fed exactly what
    # smooth_run.py writes. Photon-limited input: zeros fall from ~90% to ~5%, no clipping.
    worked = Dict{String,Any}(
        "gain" => 2.4, "clippedVoxels" => 0,
        "zeroFracIn"  => Dict{String,Any}("0" => 0.91, "1" => 0.88),
        "zeroFracOut" => Dict{String,Any}("0" => 0.06, "1" => 0.05))
    @test isempty(Cecelia._smooth_qc_findings(worked))

    # Gain clipping — the bright end of every smoothed channel is now flat.
    clipped = merge(worked, Dict{String,Any}("clippedVoxels" => 1234))
    f = Cecelia._smooth_qc_findings(clipped)
    @test length(f) == 1 && f[1]["code"] == "smooth.gain_clipped" && f[1]["level"] == "warn"
    @test f[1]["short"] == "Dynamic-range gain clipped 1234 voxels"  # the count is IN the message
    @test occursin("Restore dynamic range", f[1]["long"]) # the action, imperative
    @test f[1]["detail"] isa AbstractDict
    # from the catalog, so it re-renders at read time like every other finding
    @test haskey(Cecelia.QC_TEXT, "smooth.gain_clipped")
    @test f[1]["key"] == "smooth.gain_clipped"

    # Dense input — nothing sparse to fill, so the step bought nothing. Info, not warn.
    dense = Dict{String,Any}(
        "gain" => 1.0, "clippedVoxels" => 0,
        "zeroFracIn"  => Dict{String,Any}("0" => 0.02),
        "zeroFracOut" => Dict{String,Any}("0" => 0.00))
    fd = Cecelia._smooth_qc_findings(dense)
    @test length(fd) == 1 && fd[1]["code"] == "smooth.no_effect" && fd[1]["level"] == "info"
    @test haskey(Cecelia.QC_TEXT, "smooth.no_effect")

    # advisory only, per docs/MODULES.md — never an error, never a gate
    @test all(x -> x["level"] in ("info", "warn"),
              vcat(Cecelia._smooth_qc_findings(clipped), fd))

    # Metrics reduce the per-channel dicts to the WORST channel — a step that filled one channel and
    # left another sparse is the case worth seeing.
    m = Cecelia._smooth_metrics(worked)
    @test m["zeroFracInMax"]  == 0.91
    @test m["zeroFracOutMax"] == 0.06
    @test m["gain"] == 2.4 && m["clippedVoxels"] == 0

    # Missing stats must not throw — the helper runs on whatever python managed to write.
    @test Cecelia._smooth_metrics(Dict{String,Any}())["gain"] == 1.0
    @test isempty(Cecelia._smooth_qc_findings(Dict{String,Any}()))

    # Deliberately NOT cohort: the input is the drift-corrected store, whose zero fraction includes
    # the canvas padding drift correction added, so the outlier detector would rank images by shake.
    @test !haskey(COHORT_METRICS, "cleanupImages.smooth")
end

@testset "AF correction QC — the exemption that got retired" begin
    # This task carried a QC-EXEMPT comment calling itself the weakest exemption in the codebase.
    # It now has exactly ONE finding: the correction has no free parameter left to land badly, so the
    # only objective signal is about the INPUT.
    ok = Dict{String,Any}("1" => Dict{String,Any}(
        "saturatedFrac" => 0.0001, "levelsUsed" => 200, "levelsAvailable" => 256))
    @test isempty(Cecelia.af_qc_findings(ok)[1])

    saturated = Dict{String,Any}("1" => Dict{String,Any}(
        "saturatedFrac" => 0.05, "levelsUsed" => 200, "levelsAvailable" => 256))
    f, w = Cecelia.af_qc_findings(saturated)
    @test length(f) == 1 && f[1]["code"] == "af.saturated_input" && f[1]["level"] == "warn"
    @test w.saturated == 0.05

    # THE BUG THIS REPLACED: the finding was hand-rolled with a `detail` STRING and no `long` at all,
    # so the QC panel rendered "Channel 1 saturated → undefined" — visible in the GUI from the day AF
    # QC shipped, because `lib/qc.ts` reads `f.long`. House convention (see drift_correct.jl):
    # short = problem, long = the action, FIGURES in `detail` as a Dict.
    @test f[1]["short"] == "Channel 1 saturated"
    @test !isempty(get(f[1], "long", ""))
    @test occursin("gain", f[1]["long"])                    # the action, imperative
    @test f[1]["detail"] isa AbstractDict                   # figures, NOT a string
    @test f[1]["detail"]["saturatedPct"] == 5.0
    # ...and it comes from the copy catalog, so it re-renders at read time like every other finding
    @test haskey(Cecelia.QC_TEXT, "af.saturated_input")
    @test f[1]["key"] == "af.saturated_input"

    # advisory only, per docs/MODULES.md — never an error, never a gate
    @test all(x -> x["level"] == "warn", f)

    # BLEEDTHROUGH — the diagnostic the audit said this task had never had. Not a failure: the
    # correction subtracts the leak, and the finding exists because a leak is a FILTER-SET property, so
    # one image of a set differing from its peers is a real signal about the optics.
    clean = Dict{String,Any}("1" => Dict{String,Any}(
        "saturatedFrac" => 0.0, "levelsUsed" => 200, "levelsAvailable" => 256,
        "bleedthrough" => Dict{String,Any}()))
    @test isempty(Cecelia.af_qc_findings(clean)[1])          # no leak detected → nothing to say
    @test Cecelia.af_qc_findings(clean)[2].leak == 0.0

    leaky = Dict{String,Any}("1" => Dict{String,Any}(
        "saturatedFrac" => 0.0, "levelsUsed" => 200, "levelsAvailable" => 256,
        # the real numbers measured on WIaUjL/p6t4mC: CH3 into CH2, and nothing else
        "bleedthrough" => Dict{String,Any}("2" => 0.0248)))
    lf, lw = Cecelia.af_qc_findings(leaky)
    @test length(lf) == 1 && lf[1]["code"] == "af.bleedthrough" && lf[1]["level"] == "warn"
    @test lw.leak == 0.0248
    @test haskey(Cecelia.QC_TEXT, "af.bleedthrough")
    @test lf[1]["detail"] isa AbstractDict                   # figures in `detail`, never a string
    @test lf[1]["detail"]["sourceChannel"] == "2"            # WHICH filter pair leaks is the point
    @test lf[1]["detail"]["alphaPct"] == 2.48
    @test !isempty(get(lf[1], "long", ""))

    # one finding PER SOURCE — collapsing them would hide which pair is leaking, which is the only
    # thing a user can act on
    two = Dict{String,Any}("1" => Dict{String,Any}(
        "saturatedFrac" => 0.0, "levelsUsed" => 200, "levelsAvailable" => 256,
        "bleedthrough" => Dict{String,Any}("2" => 0.02, "3" => 0.05)))
    tf, tw = Cecelia.af_qc_findings(two)
    @test length(tf) == 2 && tw.leak == 0.05

    # `af-low-range` IS GONE, and re-tuning it would be wrong. It warned when the output used <20% of
    # the dtype's levels — a real signal under the RATIO, whose output was stretched to fill the range
    # through a derived ceiling. The power weight outputs INPUT COUNTS, so a 16-bit channel with signal
    # in the low thousands legitimately occupies a sliver: measured on real runs, 735-3576 of 65536
    # levels (1.1-5.5%) on EVERY channel of EVERY image. The premise inverted with the mechanism.
    coarse = Dict{String,Any}("2" => Dict{String,Any}(
        "saturatedFrac" => 0.0, "levelsUsed" => 775, "levelsAvailable" => 65536))
    f2, w2 = Cecelia.af_qc_findings(coarse)
    @test isempty(f2)                              # 1.2% of the range is NORMAL now, not a warning
    @test w2.levels < 0.02                         # ...but the metric is still banked
    @test !any(x -> occursin("range", x["code"]), Cecelia.af_qc_findings(coarse)[1])

    # worst-case rollup across channels, since QC banks one number per image
    both = merge(saturated, coarse)
    _, w3 = Cecelia.af_qc_findings(both)
    @test w3.saturated == 0.05         # worst = most saturated
    @test w3.levels < 0.02             # worst = least range used

    # `levelsUsedFrac` stays a COHORT metric: an image far below its peers is informative even when the
    # absolute number is not. `saturatedFrac` describes the acquisition — measured across the nine
    # kSUFux movies it spanned 0.001%-0.018%, a 13x spread at identical settings. `maxBleedthrough` is
    # the most cohort-shaped of the three: a leak is a property of the FILTER SET, so it should be
    # identical across a set acquired the same way and one image differing is the whole signal.
    @test COHORT_METRICS["cleanupImages.afCorrect"] ==
          ["saturatedFrac", "levelsUsedFrac", "maxBleedthrough"]
    @test !("ceiling" in COHORT_METRICS["cleanupImages.afCorrect"])
    @test !("clippedFrac" in COHORT_METRICS["cleanupImages.afCorrect"])

    # ratio-era stats files are ignored, not warned on
    ceiling_era = Dict{String,Any}("1" => Dict{String,Any}(
        "clippedFrac" => 0.9, "levelsUsed" => 200, "levelsAvailable" => 256, "ceiling" => 999.0))
    @test isempty(Cecelia.af_qc_findings(ceiling_era)[1])

    # A stats file missing the key must read as 0.0, not throw.
    @test Cecelia.af_qc_findings(Dict{String,Any}("1" => Dict{String,Any}(
        "levelsUsed" => 200, "levelsAvailable" => 256)))[2].saturated == 0.0
end

@testset "every QC finding carries the fields the GUI reads" begin
    # `lib/qc.ts` renders `${f.short}\n→ ${f.long}`, so a finding without `long` displays the literal
    # string "undefined" to the user. AF shipped exactly that for months because it hand-rolled its
    # finding dict instead of calling `qc_finding`. Nothing checked, so nothing caught it.
    #
    # Enforced structurally: no producer may build a finding dict by hand. `qc_finding` is the one
    # constructor, and it cannot omit `long` or put a string in `detail`.
    src = String[]
    for (root, _, files) in walkdir(joinpath(dirname(dirname(pathof(Cecelia))), "src"))
        for f in files
            endswith(f, ".jl") || continue
            push!(src, joinpath(root, f))
        end
    end
    @test !isempty(src)
    offenders = String[]
    for path in src
        endswith(path, "qc.jl") && continue          # the constructor itself
        for (i, line) in enumerate(eachline(path))
            occursin(r"\"level\"\s*=>\s*\"(warn|info)\"", line) &&
                push!(offenders, "$(basename(path)):$i")
        end
    end
    @test offenders == []

    # and the constructor's own contract: `long` always present, `detail` only ever structured
    f = Cecelia.qc_finding("warn", "af.saturated_input"; channel = 2,
                           detail = Dict{String,Any}("saturatedPct" => 1.5))
    @test haskey(f, "long") && !isempty(f["long"])
    @test f["detail"] isa AbstractDict
    @test !(Cecelia.qc_finding("warn", "x.y", "s", "l")["long"] |> isempty)
end

@testset "one resolver turns channel names into indices" begin
    # SIX handlers had hand-rolled `findfirst(==(String(ch)), ch_names)` and drifted into three
    # different behaviours, all silently wrong: an already-resolved index crashed four of them, an
    # unmatched name was dropped by five, and `drift_correct` fell back to index 0 — which on a
    # resonance-scanner movie means registering the whole timelapse against SHG at 99.5% zeros.
    names = ["SHG", "nuc-GFP", "mem-TOM", "CD169-Kat"]

    @test Cecelia.channel_index("mem-TOM", names) == 2          # 0-BASED, for the Python side
    @test Cecelia.channel_index("SHG", names) == 0
    @test Cecelia.channel_indices(["CD169-Kat", "nuc-GFP"], names) == [3, 1]   # order preserved

    # idempotent: an index passes through, so translating a chain dict twice is a no-op
    @test Cecelia.channel_index(2, names) == 2
    @test Cecelia.channel_indices([2, "CD169-Kat"], names) == [2, 3]

    # a single value, not a vector — `channelSelection` with multiple=false still arrives as one
    @test Cecelia.channel_indices("mem-TOM", names) == [2]

    # deduped by default: a channel named twice would square its term into the AF denominator
    @test Cecelia.channel_indices(["mem-TOM", "mem-TOM"], names) == [2]
    @test Cecelia.channel_indices(["mem-TOM", "mem-TOM"], names; unique_only = false) == [2, 2]

    # "nothing selected" is a legitimate state each task judges for itself (branching only needs
    # fibreChannels for anisotropySource="channel") — not an error
    @test Cecelia.channel_indices(nothing, names) == Int[]
    @test Cecelia.channel_indices([], names) == Int[]

    # AN UNMATCHED NAME RAISES, and the message names what was available. This is the deliberate
    # behaviour change from silent-drop: a channel the user named and we cannot find is not a thing
    # to guess about.
    err = try; Cecelia.channel_index("CH3", names); nothing; catch e; e; end
    @test err isa ErrorException
    @test occursin("CH3", err.msg) && occursin("mem-TOM", err.msg)
    @test_throws ErrorException Cecelia.channel_indices(["nuc-GFP", "nope"], names)
    # ...including when the image registered no names at all, rather than silently indexing nothing
    @test_throws ErrorException Cecelia.channel_index("nuc-GFP", String[])

    # A case-only difference is the common real cause: two images from ONE experiment shipped
    # `mem-TOM` (zolIMa/eQRnwU) and `mem-Tom` (zolIMa/fXgbTl), so a chain built on one fails on the
    # other. Still an error — the match stays exact, guessing is what this resolver removes — but the
    # message names the near match so it is a five-second fix.
    cased = try; Cecelia.channel_index("mem-TOM", ["SHG", "nuc-GFP", "mem-Tom"]); nothing
            catch e; e end
    @test cased isa ErrorException
    @test occursin("mem-Tom", cased.msg) && occursin("case", cased.msg)

    # ccid_channel_names reads the versioned field; `nothing` asks for the ACTIVE version
    raw = Dict{String,Any}("imChannelNames" => Dict{String,Any}(
        "default" => names, "corrected" => ["a", "b"], "_active" => "corrected"))
    @test Cecelia.ccid_channel_names(raw) == names                  # default
    @test Cecelia.ccid_channel_names(raw, nothing) == ["a", "b"]    # active
    @test Cecelia.ccid_channel_names(Dict{String,Any}()) == String[]

    # NO SEVENTH COPY. The detector, not just the extraction — this is the second time this file has
    # had to count these sites, and grep-based guesses were wrong both times.
    src_root = dirname(pathof(Cecelia))
    offenders = String[]
    for (root, _, files) in walkdir(joinpath(src_root, "tasks"))
        for f in files
            endswith(f, ".jl") || continue
            body = read(joinpath(root, f), String)
            for line in split(body, '\n')
                startswith(strip(line), "#") && continue
                occursin(r"findfirst\(==\(String\(", line) &&
                    push!(offenders, relpath(joinpath(root, f), src_root))
            end
        end
    end
    @test isempty(offenders)
end

@testset "AF params are just channels" begin
    # The spec grew into a bag of ~20 numbers while fitting individual datasets and was never
    # revisited. A combination is now the two things it is actually about; everything else is derived
    # (`af_weight_stats`) or was a filter that belongs to a filtering task.
    spec = Cecelia._task_spec(Cecelia.AfCorrect())
    keys_top = [string(get(p, "key", "")) for p in get(spec, "params", [])]
    @test keys_top == ["valueName", "afCombinations", "backgroundMethod"]

    # `exclusive` is the ONE addition, and it is admitted on a rule the deleted twenty all failed:
    # it is a fact about the SPECIMEN that no amount of looking at the pixels can supply, not a number
    # to fit. Can one voxel carry both markers? The user knows; the estimator cannot. It selects
    # between the total slope and the envelope floor (`af_bleedthrough_alphas`), which on
    # `WIaUjL/p6t4mC` differ 5x — 0.113 against 0.024, the difference between a corrected channel and
    # one that visibly still carries the other's overspill.
    #
    # Anything proposed here later has to clear the same bar. `channelPercentile` and friends did not:
    # they were dials with no defensible value, fitted per dataset and never revisited.
    combo = only(p for p in get(spec, "params", []) if string(get(p, "key", "")) == "afCombinations")
    @test [string(get(p, "key", "")) for p in get(combo, "params", [])] ==
          ["targetChannel", "competingChannels", "exclusive"]

    # a statement, not a dial: boolean, and defaulting to the common case (distinct cell types)
    excl = only(p for p in get(combo, "params", []) if string(get(p, "key", "")) == "exclusive")
    @test string(get(excl, "type", "")) == "bool"
    @test get(excl, "default", nothing) === true

    # `none` is NOT offered: the weight is a ratio of intensities, so an unsubtracted pedestal makes
    # background voxels split evenly and survive. Measured on kSUFux/Or1L8a: 92.1% of background voxels
    # come out non-zero and cell-to-background contrast collapses to 6.8x.
    bg = only(p for p in get(spec, "params", []) if string(get(p, "key", "")) == "backgroundMethod")
    @test [string(get(o, "value", "")) for o in get(bg, "options", [])] == ["triangle", "otsu"]

    # No exponent param. This task deleted four numbers with no defensible value (channelPercentile,
    # correctionPercentile, correctionMin, correctionMax) and a user-facing sharpness dial is that same
    # thing returning — see `AF_WEIGHT_EXPONENT`.
    @test !("exponent" in keys_top)
    @test !("weightExponent" in keys_top)

    # the deleted ones, named so a future session doesn't reintroduce them one at a time
    gone = ["correctionMin", "correctionMax", "correctionGain", "channelPercentile",
            "correctionPercentile", "correctionMode", "summaryMode", "summaryPercentile",
            "generateInverse", "medianFilter", "topHatRadius", "rollingBallRadius",
            "rollingBallPadding", "denoiseFun", "waveletMethod", "waveletMode", "tvWeight",
            "applyGaussian", "applyGaussianToOthers"]
    flat = Set{String}(keys_top)
    union!(flat, Set(string(get(p, "key", "")) for p in get(combo, "params", [])))
    for k in gone
        @test !(k in flat)
    end
end
