# ── Model vaults + denoise + support-training testsets ────────────────
# 13 sections covering the cellpose / coastal / denoise model-vault + training-workflow
# pipeline: cellpose_model_path resolver, list_cellpose_models enumeration, coastal model
# vault, denoise model vault, denoise_model_names strips, denoise_model_target overwrite,
# opticalFlow.trainSupportDenoise task wiring, _support_short_movie_refusal actionable
# T-mismatch, _support_temporal_window_advisory form-time twin, param validator registry
# (SUPPORT registers), _support_train_qc_findings pure catalog, _denoise_qc_findings
# collapse detection (pooled runs), cleanupImages.denoise + CoastalSegment dynamic Model
# options. Extracted from suite.jl to keep it small enough to merge without EOF conflicts
# on every append. The extracted file loads inside this file's aggregating testset scope,
# so any helpers defined earlier in suite.jl are still in scope (lexical include).
#
# No `@__DIR__` scans in the extracted range — the only path-shaped comment (in the
# cellpose_model_path resolver testset) refers to the RESOLVER's own `@__DIR__` usage in
# app/src, not this file's.

# A user-placed checkpoint under `<config_dir>/models/cellposeModels/{name}` is picked up
# by the cellpose Julia handler and passed to Python as an absolute file path (which
# `cellpose_utils.py::_get_model` loads via `pretrained_model=…`). No shell-outs, no
# network — pure filesystem resolution.
@testset "cellpose_model_path resolver" begin
    # user-override slot (config_dir): missing file → nothing; empty/blank name → nothing.
    td = mktempdir()
    @test cellpose_model_path("__no_such_model__.pt", td) === nothing
    @test cellpose_model_path("", td) === nothing
    @test cellpose_model_path("   ", td) === nothing

    # place a file in the config-dir override slot, resolver returns its absolute path (the
    # bundled slot at <repo>/models/cellposeModels/ takes precedence when both exist — that
    # path is real in this repo and can't be safely mocked here).
    mkpath(joinpath(td, "models", "cellposeModels"))
    f = joinpath(td, "models", "cellposeModels", "__unique_test_model__.pt")
    open(io -> write(io, "stub"), f, "w")
    @test cellpose_model_path("__unique_test_model__.pt", td) == f

    # cellpose_models_dir is a pure path — no I/O, no side-effects
    @test cellpose_models_dir(td) == joinpath(td, "models", "cellposeModels")
end

# ── Runtime-enumerated cellpose model picker (drop-in convention) ────────────
# `list_cellpose_models` returns builtins + bundled + user drop-ins; the ordering + dedup
# rules are what the picker AND `validate_params` both see (see `_inject_dynamic_options!`
# in cellpose.jl + `_task_spec`'s dynamic hook in task.jl). Guards: builtins always present,
# dedup shadows a bundled file by the same-name user file, a user file that isn't a known
# built-in becomes selectable, and validate_params accepts it.
@testset "list_cellpose_models enumeration" begin
    # NOTE: only the USER dir is td-scoped here; the BUNDLED dir is `<repo>/models/
    # cellposeModels/` (hardcoded via @__DIR__ so it matches the resolver + install layout).
    # A dev running `pixi run models-fetch` populates the bundled dir with real checkpoints,
    # so tests can't assume it's empty — check invariants that hold either way.
    td = mktempdir()

    # Built-ins are always first, in a stable order. v4 rows always show (cpsam_v2/cpsam); v3
    # rows (cyto2/cyto3) are HIDDEN unless the opt-in `cellpose-v3` pixi env is installed — a
    # user picking one without the env would only get `run_py`'s missing-env error at Run time,
    # so dropping them upstream is the right filter (see docs/todo/CELLPOSE_V3_OPTIN_PLAN.md).
    # Whether the env is present depends on the machine running the tests; assert both branches
    # of the gate.
    names = [m.name for m in list_cellpose_models(td)]
    @test names[1:2] == ["cpsam_v2", "cpsam"]
    if Cecelia._cellpose_v3_env_installed()
        @test any(m.name == "cyto3" for m in list_cellpose_models(td))
        @test any(m.name == "cyto2" for m in list_cellpose_models(td))
    else
        @test !any(m.name in ("cyto2", "cyto3") for m in list_cellpose_models(td))
    end

    # No user drop-in → nothing tagged "user" for THIS td.
    base = list_cellpose_models(td)
    @test all(m.source != "user" for m in base)

    # Place a user drop-in checkpoint → appears with source="user"
    mkpath(joinpath(td, "models", "cellposeModels"))
    f = joinpath(td, "models", "cellposeModels", "myFluo.pt")
    open(io -> write(io, "stub"), f, "w")
    with_user = list_cellpose_models(td)
    idx = findfirst(m -> m.name == "myFluo.pt", with_user)
    @test !isnothing(idx)
    @test with_user[idx].source == "user"
    @test occursin("user", with_user[idx].label)

    # Dotfiles / subdirectories are skipped when scanning.
    open(io -> write(io, "hidden"), joinpath(td, "models", "cellposeModels", ".DS_Store"), "w")
    mkpath(joinpath(td, "models", "cellposeModels", "subdir"))
    clean = list_cellpose_models(td)
    @test all(m.name != ".DS_Store" for m in clean)
    @test all(m.name != "subdir"    for m in clean)
end

# ── Coastal (optical-flow) model vault ───────────────────────────────────────
# The same drop-in convention as cellpose, one directory over — but with no built-ins and no
# bundled slot, because coastal ships no models: a user trains their own on the Optical Flow
# page. The manifest is the load-bearing part. Inference MUST use the metric set a model was
# trained on, and coastal fails silently when it doesn't (a missing plane shifts every later
# channel), so `CoastalUtils` configures itself from the sidecar rather than from task params.
@testset "coastal model vault" begin
    td = mktempdir()
    @test coastal_models_dir(td) == joinpath(td, "models", "coastalModels")

    # Empty vault → empty picker. No built-in fallback to segment with by accident.
    @test isempty(list_coastal_models(td))
    @test coastal_model_path("anything.pt", td) === nothing
    @test coastal_model_path("", td) === nothing
    @test coastal_model_path("   ", td) === nothing

    dir = joinpath(td, "models", "coastalModels")
    mkpath(dir)
    pt = joinpath(dir, "gcMemTom.pt")
    open(io -> write(io, "stub"), pt, "w")
    @test coastal_model_path("gcMemTom.pt", td) == pt
    # Bare stem resolves too — picker shape (with or without `.pt`) roundtrips through the lookup.
    @test coastal_model_path("gcMemTom", td) == pt

    # A checkpoint with no manifest still lists — it just falls back to coastal's defaults.
    bare = list_coastal_models(td)
    @test length(bare) == 1
    @test bare[1].name == "gcMemTom.pt"
    @test bare[1].label == "gcMemTom"
    @test isempty(bare[1].manifest)
    @test isempty(coastal_model_manifest("gcMemTom.pt", td))

    # With a manifest, the picker label says what the model was trained on.
    write(joinpath(dir, "gcMemTom.json"),
          """{"channelName":"mem-TOM","temporalScales":[1,2,4,8],"cumulativeWindow":5}""")
    with_manifest = list_coastal_models(td)
    @test with_manifest[1].label == "gcMemTom (mem-TOM)"
    @test with_manifest[1].manifest["cumulativeWindow"] == 5
    @test with_manifest[1].manifest["temporalScales"] == [1, 2, 4, 8]

    # A corrupt manifest must not take the picker down with it.
    write(joinpath(dir, "gcMemTom.json"), "{not json")
    @test isempty(coastal_model_manifest("gcMemTom.pt", td))
    @test length(list_coastal_models(td)) == 1

    # The sidecar is not an entry of its own, and dotfiles/subdirs are skipped.
    open(io -> write(io, "hidden"), joinpath(dir, ".DS_Store"), "w")
    mkpath(joinpath(dir, "subdir.pt"))
    names = [m.name for m in list_coastal_models(td)]
    @test names == ["gcMemTom.pt"]
end

# ── Denoise (SUPPORT) model vault ───────────────────────────────────────────────
# Same shape as the coastal vault. Manifest is load-bearing here too — SUPPORT's architecture
# (input_frames, mid_channels, depth, blind_conv_channels, bs_size) is not encoded in the .pt, so
# without a sidecar the runner cannot rebuild the network.
@testset "denoise model vault" begin
    td = mktempdir()
    @test Cecelia.denoise_models_dir(td) == joinpath(td, "models", "denoiseModels")

    @test isempty(Cecelia.list_denoise_models(td))
    @test Cecelia.denoise_model_path("anything.pt", td) === nothing
    @test Cecelia.denoise_model_path("", td) === nothing

    dir = joinpath(td, "models", "denoiseModels")
    mkpath(dir)
    pt = joinpath(dir, "supMemTom.pt")
    open(io -> write(io, "stub"), pt, "w")
    @test Cecelia.denoise_model_path("supMemTom.pt", td) == pt
    # Bare stem resolves too — the denoise picker sends stems (#828); the segmentation runner passed
    # the stem straight to `denoise_model_path`, which used to look for `<dir>/supMemTom` and miss.
    @test Cecelia.denoise_model_path("supMemTom", td) == pt
    @test Cecelia.denoise_model_manifest("supMemTom", td) == Cecelia.denoise_model_manifest("supMemTom.pt", td)

    # A checkpoint with no manifest still lists — but the runner will refuse to load it.
    # `.name` is the on-disk name inside the vault directory — `<stem>.pt` for a pooled file,
    # `<stem>` (no extension) for a perChannel bundle folder (SUPPORT_PERCHANNEL_PLAN.md D2). Every
    # consumer that does `joinpath(dir, m.name)` (`vault_api.jl`, the `list_coastal_models` label
    # builder) then finds the right on-disk artifact without a kind-aware branch.
    bare = Cecelia.list_denoise_models(td)
    @test length(bare) == 1
    @test bare[1].name == "supMemTom.pt"
    @test bare[1].kind === :pooled
    @test bare[1].label == "supMemTom"
    @test isempty(bare[1].manifest)
    @test isempty(Cecelia.denoise_model_manifest("supMemTom.pt", td))

    # With a manifest, the picker label carries the acquisition it was trained on. A denoise model
    # pools N channels into one (DENOISE_INTEGRATION_PLAN.md D3 amendment, measured on fXgbTl
    # 2026-09-05); the label joins them with "+".
    write(joinpath(dir, "supMemTom.json"),
          """{"channels":["mem-TOM","nuc-GFP"],"arch":{"inputFrames":61,"midChannels":[64,128,256,512],"depth":4}}""")
    with_manifest = Cecelia.list_denoise_models(td)
    @test with_manifest[1].label == "supMemTom (mem-TOM+nuc-GFP)"
    @test with_manifest[1].manifest["arch"]["inputFrames"] == 61
    @test with_manifest[1].manifest["arch"]["midChannels"] == [64, 128, 256, 512]

    # A corrupt manifest must not take the picker down with it.
    write(joinpath(dir, "supMemTom.json"), "{not json")
    @test isempty(Cecelia.denoise_model_manifest("supMemTom.pt", td))
    @test length(Cecelia.list_denoise_models(td)) == 1

    # Dotfiles skipped, same as coastal. A directory named with a `.pt` suffix is a leftover mkdir
    # (not a bundle — a real bundle has no manifest.json marker), so also skip it.
    open(io -> write(io, "hidden"), joinpath(dir, ".DS_Store"), "w")
    mkpath(joinpath(dir, "subdir.pt"))
    names = [m.name for m in Cecelia.list_denoise_models(td)]
    @test names == ["supMemTom.pt"]

    @test Cecelia.denoise_model_names(td) == ["supMemTom"]

    # ── perChannel bundle (SUPPORT_PERCHANNEL_PLAN.md D2/D3) ──────────────────
    # A bundle is a folder in the vault with per-channel `.pt`s and a top-level `manifest.json`
    # listing them. `list_denoise_models` picks it up alongside pooled `.pt` files; the picker
    # label carries a `per-channel` hint so a user can tell them apart at a glance.
    bundle = joinpath(dir, "suppMerTK")
    mkpath(bundle)
    open(io -> write(io, "stub"), joinpath(bundle, "nuc-GFP.pt"), "w")
    open(io -> write(io, "stub"), joinpath(bundle, "CD169-Kat.pt"), "w")
    write(joinpath(bundle, "nuc-GFP.json"),
          """{"kind":"denoise-support","mode":"perChannel-sub","channels":["nuc-GFP"],"arch":{"inputFrames":21}}""")
    write(joinpath(bundle, "CD169-Kat.json"),
          """{"kind":"denoise-support","mode":"perChannel-sub","channels":["CD169-Kat"],"arch":{"inputFrames":21}}""")
    write(joinpath(bundle, "manifest.json"), """
        {"kind":"denoise-support","mode":"perChannel","channels":["nuc-GFP","CD169-Kat"],
         "perChannel":[{"index":1,"name":"nuc-GFP","slug":"nuc-GFP","pt":"nuc-GFP.pt"},
                       {"index":3,"name":"CD169-Kat","slug":"CD169-Kat","pt":"CD169-Kat.pt"}]}""")

    listed = Cecelia.list_denoise_models(td)
    @test length(listed) == 2
    bundle_entry = only(m for m in listed if m.name == "suppMerTK")
    @test bundle_entry.kind === :perChannel
    @test bundle_entry.label == "suppMerTK (nuc-GFP+CD169-Kat, per-channel)"

    resolved = Cecelia.denoise_model_resolve("suppMerTK", td)
    @test resolved.kind === :perChannel
    @test resolved.rootPath == bundle
    @test sort(collect(keys(resolved.perChannel))) == ["CD169-Kat", "nuc-GFP"]
    @test endswith(resolved.perChannel["CD169-Kat"].ptPath, "CD169-Kat.pt")
    @test resolved.perChannel["CD169-Kat"].manifest["arch"]["inputFrames"] == 21

    # Pooled resolver still works — `denoise_model_resolve` handles both kinds.
    pooled_resolved = Cecelia.denoise_model_resolve("supMemTom", td)
    @test pooled_resolved.kind === :pooled
    @test pooled_resolved.rootPath == pt
end

@testset "denoise_model_names strips only .pt, keeps internal dots in a bundle name" begin
    # A bundle folder `supp.small` is legal; `splitext` split it at the internal dot and returned
    # `"supp"`, which the picker sent to `denoise_model_resolve` — miss → "Model 'supp' not found".
    # Same rule applied in THREE places (config.jl's `denoise_model_names`, config.jl's
    # `list_denoise_models` pooled branch, task.jl's `_OPTION_SOURCES["denoiseModels"]`) — all three
    # now go through `vault_model_stem`.
    td = mktempdir()
    dir = Cecelia.denoise_models_dir(td)
    mkpath(dir)
    open(io -> write(io, "stub"), joinpath(dir, "supMemTom.pt"), "w")
    bundle = joinpath(dir, "supp.small")
    mkpath(bundle)
    open(io -> write(io, "stub"), joinpath(bundle, "a.pt"), "w")
    write(joinpath(bundle, "manifest.json"), """
        {"kind":"denoise-support","mode":"perChannel","channels":["a"],
         "perChannel":[{"index":0,"name":"a","slug":"a","pt":"a.pt"}]}""")

    @test sort(Cecelia.denoise_model_names(td)) == ["supMemTom", "supp.small"]

    # And the picker's chosen stem must round-trip through the resolver — the bug the user hit.
    resolved = Cecelia.denoise_model_resolve("supp.small", td)
    @test !isnothing(resolved)
    @test resolved.kind === :perChannel

    # The task-spec picker source runs off the same vault. Point config_dir at the fixture so the
    # denoiseModels source enumerates ours, not the dev vault.
    withenv("CECELIA_DEV_DIR" => td) do
        opts = Cecelia._OPTION_SOURCES["denoiseModels"]()
        values = [String(o.value) for o in opts]
        @test sort(values) == ["supMemTom", "supp.small"]   # NOT "supp"
    end
end

@testset "denoise_model_target clears both sibling shapes on overwrite" begin
    # Retraining `<name>` as perChannel used to leave the previous pooled `<name>.pt` orphaned next
    # to the new `<name>/` bundle, and the picker showed both rows under the same stem. `overwrite`
    # now means "one name is one model" — clear whichever shape existed at that stem.
    td = mktempdir()
    dir = Cecelia.denoise_models_dir(td)
    mkpath(dir)

    # 1) pooled → perChannel: `.pt` + `.json` cleared before the bundle is written.
    pt   = joinpath(dir, "supX.pt")
    json = joinpath(dir, "supX.json")
    open(io -> write(io, "stub"), pt, "w")
    write(json, """{"channels":["a"]}""")
    pt_target, bundle_target =
        Cecelia.denoise_model_target("supX"; overwrite = true, want_bundle = true, dev_dir = td)
    @test pt_target == pt
    @test bundle_target == joinpath(dir, "supX")
    @test !isfile(pt) && !isfile(json)   # old pooled pair gone
    @test !isdir(bundle_target)          # bundle path still available for the runner to create

    # 2) perChannel → pooled: bundle folder cleared before the .pt is written.
    bundle = joinpath(dir, "supY")
    mkpath(bundle)
    open(io -> write(io, "stub"), joinpath(bundle, "a.pt"), "w")
    write(joinpath(bundle, "manifest.json"), """{"kind":"denoise-support","mode":"perChannel"}""")
    Cecelia.denoise_model_target("supY"; overwrite = true, dev_dir = td)
    @test !isdir(bundle)                 # old bundle gone

    # 3) overwrite = false still refuses either colliding shape.
    open(io -> write(io, "stub"), joinpath(dir, "supZ.pt"), "w")
    @test_throws ErrorException Cecelia.denoise_model_target("supZ"; overwrite = false, dev_dir = td)
    mkpath(joinpath(dir, "supW"))
    @test_throws ErrorException Cecelia.denoise_model_target("supW"; overwrite = false, dev_dir = td)
end

# The denoise picker is entirely runtime-enumerated — cecelia ships no built-in denoise models. The
# spec declares one literal option ("None", value ""), the resolver appends the vault, dedup by value.
@testset "opticalFlow.trainSupportDenoise task wiring" begin
    # Fun-name namespace stays `opticalFlow.*` (stored in ccid.json chain state — a rename would
    # break every persisted chain). The display category was renamed to "Model training" in Phase C
    # so the task picker shows both training tasks under one honest heading.
    task = Cecelia._task_from_fun_name("opticalFlow.trainSupportDenoise")
    @test task isa Cecelia.TrainSupportDenoise
    spec = Cecelia._task_spec(task)
    @test !isnothing(spec)
    @test spec["category"] == "Model training"
    @test spec["scope"] == "set"
    @test spec["resource_pool"] == "gpu"

    # unetSize accepts the three sizes and nothing else; picking any of them must validate.
    # `trainChannels` is a multi-select — the pooled-channels amendment (D3, fXgbTl 2026-09-05).
    for size in ("small", "medium", "large")
        @test Cecelia.validate_params(task, Dict{String,Any}(
            "modelName" => "x", "trainChannels" => ["c1", "c2"], "unetSize" => size)) === nothing
    end

    # inputFrames stays odd — the centre-frame contract of a temporal blind-spot model. Non-odd is
    # a runtime error in the handler (not a spec constraint), so this is verified via the handler.
    #
    # The manifest keys the DENOISE runner reads back are enumerated here so the two files can't
    # drift silently: if this list changes, `denoise_run._build_model` reads a stale key.
    MANIFEST_ARCH_KEYS = ["inputFrames", "patchXY", "midChannels", "depth", "blindConvChannels",
                          "oneByOneChannels", "lastLayerChannels", "bsSize", "bp"]
    # Cheap sanity check on the CATALOG entry (its rendered short/long is asserted in the qc suite).
    @test haskey(Cecelia.QC_TEXT, "denoise.loss_flat")
end

@testset "_support_short_movie_refusal — actionable T-mismatch message" begin
    # No short movies → no refusal lines. The general-case "no usable images" fallback lives in
    # the handler alongside this, not here.
    @test isempty(Cecelia._support_short_movie_refusal(Tuple{String,Int}[], 61))

    # One short movie names the shortest T and the largest odd inputFrames that fits.
    lines = Cecelia._support_short_movie_refusal([("fXgbTl", 31)], 61)
    @test length(lines) == 2
    @test occursin("shortest is 31", lines[1])
    @test occursin("61+ timepoints", lines[1])
    @test occursin("31 (largest odd value ≤ 31)", lines[2])

    # Even shortest → next-lower odd. min(31-1) if T=30, but here also check the T=1 clamp.
    lines_even = Cecelia._support_short_movie_refusal([("aaa", 30), ("bbb", 45)], 61)
    @test occursin("shortest is 30", lines_even[1])
    @test occursin("29 (largest odd value ≤ 30)", lines_even[2])

    # T=1 is the pathological edge; max_odd is clamped to 1 rather than 0.
    lines_one = Cecelia._support_short_movie_refusal([("aaa", 1)], 5)
    @test occursin("Set Temporal window to 1", lines_one[2])
end

@testset "_support_temporal_window_advisory — form-time twin of the run-time refusal" begin
    # The registered validator emits a structured advisory that the frontend's `backendAdvisor`
    # fetches. Same rule as `_support_short_movie_refusal` above — pinned here so the two do not drift.
    mk(uid, t) = (img = CciaImage(; uid=uid, name=uid, dir="");
                  img.meta = Dict{String,Any}("SizeT" => t); img)
    empty_siblings = Dict{String,Any}()

    # Fits every movie → ok, message names the count + shortest.
    a = Cecelia._support_temporal_window_advisory(21, [mk("a", 31), mk("b", 60), mk("c", 45)], empty_siblings)
    @test a.severity == "ok"
    @test occursin("21f", a.message)
    @test occursin("3 movies", a.message)
    @test occursin("shortest 31f", a.message)

    # One movie too short → warn, tip suggests the odd cap = 31 (already odd).
    b = Cecelia._support_temporal_window_advisory(41, [mk("a", 31), mk("b", 60), mk("c", 45)], empty_siblings)
    @test b.severity == "warn"
    @test occursin("1 of 3", b.message)
    @test occursin("31", b.tip)

    # None fit → fail, tip suggests odd cap = 39 (largest odd ≤ 40), matching the run-time refusal.
    c = Cecelia._support_temporal_window_advisory(61, [mk("a", 31), mk("b", 40)], empty_siblings)
    @test c.severity == "fail"
    @test occursin("longest 40f", c.message)
    @test occursin("39", c.tip)
    @test occursin("largest odd", c.tip)

    # No images / no sizeT / bad value → nothing (silence beats a wrong readout).
    @test Cecelia._support_temporal_window_advisory(21, CciaImage[], empty_siblings) === nothing
    no_t = CciaImage(; uid="x", name="x", dir=""); no_t.meta = Dict{String,Any}()
    @test Cecelia._support_temporal_window_advisory(21, [no_t], empty_siblings) === nothing
    @test Cecelia._support_temporal_window_advisory(0, [mk("a", 31)], empty_siblings) === nothing
    @test Cecelia._support_temporal_window_advisory("nonsense", [mk("a", 31)], empty_siblings) === nothing
end

@testset "param validator registry — SUPPORT registers, the dispatcher rejects bad shapes" begin
    # Registered on include of train_support_denoise.jl.
    @test haskey(Cecelia.PARAM_VALIDATORS, ("opticalFlow.trainSupportDenoise", "inputFrames"))

    # Dispatcher returns nothing when nothing is registered.
    mk(uid, t) = (img = CciaImage(; uid=uid, name=uid, dir="");
                  img.meta = Dict{String,Any}("SizeT" => t); img)
    @test Cecelia.validate_param("nope.no_such_task", "someKey", 1, CciaImage[mk("a", 10)],
                                 Dict{String,Any}()) === nothing

    # Dispatcher round-trips a real validator.
    r = Cecelia.validate_param("opticalFlow.trainSupportDenoise", "inputFrames", 21,
                               CciaImage[mk("a", 31), mk("b", 60)], Dict{String,Any}())
    @test r !== nothing
    @test r.severity == "ok"

    # A validator that throws is swallowed and returns nothing — an advisory is not load-bearing.
    Cecelia.register_param_validator!("test.throws", "x", (_, _, _) -> error("oops"))
    @test Cecelia.validate_param("test.throws", "x", 1, CciaImage[], Dict{String,Any}()) === nothing

    # A validator that returns garbage is refused at the boundary (returns nothing, logs a warning).
    Cecelia.register_param_validator!("test.garbage", "x", (_, _, _) -> (; wrong = 1))
    @test Cecelia.validate_param("test.garbage", "x", 1, CciaImage[], Dict{String,Any}()) === nothing

    # Cleanup so the registry doesn't leak the test fixtures.
    delete!(Cecelia.PARAM_VALIDATORS, ("test.throws", "x"))
    delete!(Cecelia.PARAM_VALIDATORS, ("test.garbage", "x"))
end

@testset "_support_train_qc_findings — pure catalog" begin
    # Loss came down — no findings.
    @test isempty(Cecelia._support_train_qc_findings(Dict{String,Any}("lossDrop" => 2.5)))

    # Loss did not come down — one warn, code denoise.loss_flat.
    findings = Cecelia._support_train_qc_findings(Dict{String,Any}(
        "finalLoss" => 0.5, "lossDrop" => 0.9, "epochs" => 20))
    @test length(findings) == 1
    @test findings[1]["code"] == "denoise.loss_flat"
    @test findings[1]["level"] == "warn"

    # NaN drop — do not flag (the run wrote no history).
    @test isempty(Cecelia._support_train_qc_findings(Dict{String,Any}("lossDrop" => NaN)))
end

@testset "_denoise_qc_findings — collapse detection (pooled runs)" begin
    # No collapse — outMax/inMax ratios agree within ½× of the median. No finding.
    meta_ok = Dict{String,Any}(
        "mode" => "pooled",
        "perChannelMinMax" => Dict{String,Any}(
            "0" => Dict{String,Any}("inMin" => 0, "inMax" => 100.0, "outMin" => 0, "outMax" => 40.0),
            "1" => Dict{String,Any}("inMin" => 0, "inMax" => 200.0, "outMin" => 0, "outMax" => 70.0),
            "2" => Dict{String,Any}("inMin" => 0, "inMax" => 300.0, "outMin" => 0, "outMax" => 120.0)))
    @test isempty(Cecelia._denoise_qc_findings(meta_ok))

    # Ground-truth x4E5HU shape (2026-09-07): CD169-Kat at 0.13 vs the ~0.38 pooled cohort — well
    # under the 0.5× median threshold. One warn, code denoise.channel_collapsed, factor ≈ 3.
    meta_collapse = Dict{String,Any}(
        "mode" => "pooled",
        "perChannelMinMax" => Dict{String,Any}(
            "1" => Dict{String,Any}("inMin" => 0, "inMax" => 384.0, "outMin" => 0, "outMax" => 147.8),
            "2" => Dict{String,Any}("inMin" => 0, "inMax" => 520.0, "outMin" => 0, "outMax" => 186.1),
            "3" => Dict{String,Any}("inMin" => 0, "inMax" => 235.0, "outMin" => 0, "outMax" => 30.4)))
    findings = Cecelia._denoise_qc_findings(meta_collapse)
    @test length(findings) == 1
    @test findings[1]["code"] == "denoise.channel_collapsed"
    @test findings[1]["level"] == "warn"
    @test findings[1]["detail"]["channel"] == "3"

    # perChannel runs — within-run cohort comparison is meaningless (each channel has its own
    # model), so a collapsed-looking ratio must NOT fire the finding.
    meta_perch = merge(meta_collapse, Dict{String,Any}("mode" => "perChannel"))
    @test isempty(Cecelia._denoise_qc_findings(meta_perch))

    # Saturation branch is unchanged — a `channelsSkipped` list still fires the saturated finding.
    meta_sat = Dict{String,Any}("mode" => "pooled", "channelsSkipped" => [2])
    findings_sat = Cecelia._denoise_qc_findings(meta_sat)
    @test length(findings_sat) == 1
    @test findings_sat[1]["code"] == "denoise.channel_saturated"
end

@testset "cleanupImages.denoise spec dynamic Model options" begin
    spec = Cecelia._task_spec(Cecelia._task_from_fun_name("cleanupImages.denoise"))
    @test !isnothing(spec)
    model_sel = only(p for p in spec["params"] if get(p, "key", "") == "model")
    @test model_sel["optionsFrom"] == "denoiseModels"
    values = [string(o["value"]) for o in model_sel["options"]]
    @test first(values) == ""
    @test string(first(model_sel["options"])["label"]) == "None"
    # One entry per real model, plus "None". No duplicates.
    @test length(values) == length(unique(values))
    @test length(values) == 1 + length(Cecelia.list_denoise_models())
    # Option `value` is the bare stem (no `.pt`), matching `flowModels`. The training task strips
    # the extension when it writes, so a resolved `modelName` that carries `.pt` round-trips into
    # the picker's currently-selected label instead of matching an option.
    @test all(!endswith(v, ".pt") for v in values)
    @test values[2:end] == Cecelia.denoise_model_names()
end

# The coastal picker is ENTIRELY runtime-enumerated — coastal ships no built-in models, so on a
# fresh install the only option is "None". That empty state has to stay a legible choice rather than
# a select that rejects its own default, which is what the first version did.
@testset "CoastalSegment spec dynamic Model options" begin
    spec = Cecelia._task_spec(CoastalSegment())
    @test !isnothing(spec)
    models_group = only(p for p in spec["params"] if get(p, "key", "") == "models")
    model_sel    = only(p for p in models_group["params"] if get(p, "key", "") == "model")
    values = [string(o["value"]) for o in model_sel["options"]]

    @test first(values) == ""                       # "None" is always first and always present
    @test string(first(model_sel["options"])["label"]) == "None"
    @test validate_params(CoastalSegment(),
        Dict{String,Any}("models" => Dict{String,Any}(
            "0" => Dict{String,Any}("model" => "")))) === nothing

    # A name that is not in the vault is rejected — the enumeration is real, and a missing model
    # must never silently fall back to another one.
    @test_throws ParamValidationError validate_params(CoastalSegment(),
        Dict{String,Any}("models" => Dict{String,Any}(
            "0" => Dict{String,Any}("model" => "__not_in_the_vault__.pt"))))
end

# A `valueNameInput` is NOT free text, unlike the `text` type it replaced. Its value becomes a
# filename stem (`spatialGraph/{suffix}.h5ad`), a versioned-dict key (`labels[name]`) or a column
# suffix (`clusters.{suffix}`) — so a path separator writes somewhere else entirely and an empty
# name produces `labels[""]`. Both used to be accepted silently.
# See docs/todo/VALUE_NAME_INPUT_PLAN.md; the widget is `SuggestInput` (docs/UI.md).
@testset "valueNameInput rejects a name that is not usable as a key" begin
    ok(v) = validate_params(CellposeSegment(), Dict{String,Any}("outputValueName" => v))
    bad(v) = @test_throws ParamValidationError validate_params(
        CellposeSegment(), Dict{String,Any}("outputValueName" => v))

    @test ok("Tcell") === nothing
    @test ok("flow.cyto") === nothing        # dots are REAL names, not a path — never reject them
    @test ok("a b") === nothing              # a space is ugly, not dangerous; not ours to police

    # An EMPTY string is "unset", not a bad name: `_validate_params_against_spec` treats `""` the
    # same as absent for every param type, and the task then falls back to the spec default. Cleared
    # field → `default`, which is the behaviour the `text` type already had. Asserted so the guard
    # below is not "tightened" into rejecting it, which would fail a form the user merely blanked.
    @test ok("") === nothing

    bad("   ")                               # whitespace-only is NOT `== ""`, so it would become a real key
    bad("a/b")                               # → writes under a nested path
    bad("a\\b")                              # the Windows spelling of the same mistake
    bad(".")
    bad("..")
    bad(3)                                   # a number is not a name
end

# Selecting nothing must fail with an instruction, not with a stack trace deep in Python. The
# missing-model case is the shared one: a config-dir model does not travel with a `.ccbundle`, so
# opening someone else's project WILL name a model this machine does not have.
@testset "coastal_models_for_python resolution" begin
    raw = Dict{String,Any}("imChannelNames" => Dict{String,Any}(
        "default" => ["CH1", "CH2"], "_active" => "default"))

    @test isempty(Cecelia.coastal_models_for_python(Dict{String,Any}(), raw))

    no_model = Dict{String,Any}("models" => Dict{String,Any}(
        "0" => Dict{String,Any}("model" => "", "cellChannels" => ["CH2"])))
    err = try
        Cecelia.coastal_models_for_python(no_model, raw); nothing
    catch e; e end
    @test err isa ErrorException && occursin("Optical Flow page", err.msg)

    missing_model = Dict{String,Any}("models" => Dict{String,Any}(
        "0" => Dict{String,Any}("model" => "__absent__.pt", "cellChannels" => ["CH2"])))
    err2 = try
        Cecelia.coastal_models_for_python(missing_model, raw); nothing
    catch e; e end
    @test err2 isa ErrorException && occursin("not included in a project export", err2.msg)

    # Channel NAMES become 0-based indices — the translation the preview shares with the run.
    abs_model = Dict{String,Any}("models" => Dict{String,Any}(
        "0" => Dict{String,Any}("model" => @__FILE__, "cellChannels" => ["CH2"])))
    out = Cecelia.coastal_models_for_python(abs_model, raw)
    @test out["0"]["cellChannels"] == [1]
    @test out["0"]["model"] == @__FILE__
end
