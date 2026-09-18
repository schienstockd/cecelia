# Source-scan anchors for THIS file — the block source-scans `app/src/tasks/*.jl` and reads a Python
# helper + walks the repo root from below. `@__DIR__` used to be `app/test/`; this file sits one dir
# deeper (`app/test/suite/`), so the old `joinpath(@__DIR__, "..", "src", ...)` would resolve to
# `app/test/src` (doesn't exist) and `dirname(dirname(@__DIR__))` would resolve to `app/` (missing
# the /test level to the repo root). `pathof(Cecelia)` always points at `app/src/Cecelia.jl`,
# so backing up three dirs gives the repo root regardless of how many suite/ layers a future split adds.
const _SUITE_REPO    = dirname(dirname(dirname(pathof(Cecelia))))
const _SUITE_APP_SRC = joinpath(_SUITE_REPO, "app", "src")

# ── Segmentation label-store conventions + live outputs ────────────────────
# The algorithm-agnostic half of a segmentation task (app/src/segmentation.jl) and the
# `live_outputs` trait that makes a still-being-written store discoverable (task.jl).
#
# Declared out here because a `struct` can't be defined inside a @testset (it wraps its body in a
# local scope): a task whose live-output declaration throws, used below to assert the scheduler
# treats a preview as a convenience rather than a precondition for running.
struct _BadLiveTask <: Cecelia.CciaTask end
Cecelia.live_outputs(::_BadLiveTask, ::AbstractDict) = error("boom")

@testset "segmentation conventions and live outputs" begin
    @testset "segment_label_files mirrors the writer" begin
        # 'base' → {vn}.zarr, any other matchAs → an extra {vn}_{ma}.zarr (segmentation_utils._store_path)
        @test Cecelia.segment_label_files("X", Dict("0" => Dict("matchAs" => "base"))) == ["X.zarr"]
        @test Cecelia.segment_label_files("X", Dict("0" => Dict("matchAs" => "base"),
                                                   "1" => Dict("matchAs" => "nuc"))) ==
              ["X.zarr", "X_nuc.zarr"]
        # two models of the SAME type write one store between them, not two
        @test Cecelia.segment_label_files("X", Dict("0" => Dict("matchAs" => "base"),
                                                   "1" => Dict("matchAs" => "base"))) == ["X.zarr"]
        # a model with no matchAs is 'base' (the writer's own default)
        @test Cecelia.segment_label_files("X", Dict("0" => Dict{String,Any}())) == ["X.zarr"]
        # no models at all (REPL call, malformed params) still names the primary store
        @test Cecelia.segment_label_files("X", nothing) == ["X.zarr"]
        @test Cecelia.segment_label_files("X", Dict{String,Any}()) == ["X.zarr"]
    end

    @testset "preview params are translated the way the RUN translates them" begin
        # THE reported bug: `preview worker: ValueError: invalid literal for int() with base 10: 'CH3'`.
        # The frontend sends channel NAMES and a bare model name; Python wants 0-based indices and a
        # checkpoint PATH. `_run_task` translated them inline, so the preview — which sends the
        # frontend's params straight to the worker — sent names. Sharing `predict_slice` does not make
        # the params shared; preparing them is the task's job, hence `preview_params`.
        mktempdir() do dir
            img = CciaImage(; uid = "uid1", name = "n", dir = joinpath(dir, "1", "uid1"))
            mkpath(img._dir)
            img.filepath["default"] = "ccidImage.ome.zarr"
            img.im_channel_names["default"] = ["CH1", "CH2", "CH3", "CH4"]
            save!(img)
            raw = Cecelia.read_ccid_raw(Cecelia.state_file(img))

            params = Dict{String,Any}("models" => Dict("0" => Dict{String,Any}(
                "model" => "cpsam_v2", "matchAs" => "base",
                "cellChannels" => ["CH3"], "nucChannels" => String[])))

            m = Cecelia.cellpose_models_for_python(params, raw)["0"]
            @test m["cellChannels"] == [2]           # 0-based: CH3 is the third channel
            @test m["nucChannels"] == Int[]
            @test m["model"] == "cpsam_v2"           # a built-in name passes through untouched

            # Cellpose 3 zoo names that we DO NOT ship (`cyto`, `nuclei`, and the *torch_0 filenames)
            # are still REJECTED here — cellpose 4 answers an unknown `pretrained_model` with a log
            # warning and loads `cpsam_v2`, so forwarding one would return a DIFFERENT segmentation
            # with nothing in the run log to say so. `cyto2` and `cyto3` are no longer rejected: they
            # route to the opt-in `cellpose-v3` env (see docs/todo/CELLPOSE_V3_OPTIN_PLAN.md).
            for retired in ("cyto", "nuclei")
                p3 = Dict{String,Any}("models" => Dict("0" => Dict{String,Any}(
                    "model" => retired, "matchAs" => "base",
                    "cellChannels" => ["CH3"], "nucChannels" => String[])))
                err = try Cecelia.cellpose_models_for_python(p3, raw); nothing catch e; e end
                @test err isa ErrorException
                @test occursin("no longer exists", err.msg)
                @test occursin("cpsam_v2", err.msg)
            end

            # cyto2/cyto3 are now VALID model names (route to v3 env in the runner), so
            # `cellpose_models_for_python` accepts them and marks the backend accordingly.
            for v3name in ("cyto2", "cyto3")
                p3 = Dict{String,Any}("models" => Dict("0" => Dict{String,Any}(
                    "model" => v3name, "matchAs" => "base",
                    "cellChannels" => ["CH3"], "nucChannels" => String[])))
                m3 = Cecelia.cellpose_models_for_python(p3, raw)["0"]
                @test m3["model"] == v3name
                @test Cecelia.cellpose_model_backend(v3name) === :v3
            end
            @test Cecelia.cellpose_model_backend("cpsam_v2") === :v4
            @test Cecelia.cellpose_model_backend("unknown-custom.pt") === :v4

            # the hook the preview calls produces the same thing, and leaves other params alone
            got = Cecelia.preview_params(Cecelia.CellposeSegment(), params, img)
            @test got["models"]["0"]["cellChannels"] == [2]
            @test !haskey(params["models"]["0"], "cellChannels") ||
                  params["models"]["0"]["cellChannels"] == ["CH3"]   # input not mutated

            # idempotent: already-translated indices survive a second pass (a REPL/chain caller)
            @test Cecelia.cellpose_models_for_python(got, raw)["0"]["cellChannels"] == [2]

            # A channel the image does not have RAISES — it is never turned into a bogus index, and no
            # longer silently dropped either. Dropping satisfied the letter of "don't fabricate an
            # index" while still segmenting: on no channels, or on whichever of the pair survived. The
            # message names what was available, exactly like the missing-checkpoint case below; both are
            # "a param we cannot resolve", and this file already prefers raising for that.
            bad = Dict{String,Any}("models" => Dict("0" => Dict{String,Any}(
                "cellChannels" => ["CH9"], "nucChannels" => [])))
            bad_err = try; Cecelia.cellpose_models_for_python(bad, raw); nothing; catch e; e end
            @test bad_err isa ErrorException
            @test occursin("CH9", bad_err.msg) && occursin("CH1", bad_err.msg)

            # a missing CUSTOM checkpoint raises with a message worth showing, rather than failing
            # deep inside cellpose — the second translation the preview used to skip
            custom = Dict{String,Any}("models" => Dict("0" => Dict{String,Any}(
                "model" => "no-such-model.pth", "cellChannels" => ["CH1"], "nucChannels" => [])))
            err = try; Cecelia.cellpose_models_for_python(custom, raw); nothing
                  catch e; e end
            @test err isa ErrorException
            @test occursin("no-such-model.pth", err.msg)

            # and the composite delegates to its previewable step, since that is what the page runs
            composite = Cecelia._task_from_fun_name("segment.cellposeMeasure")
            @test Cecelia.preview_params(composite, params, img)["models"]["0"]["cellChannels"] == [2]

            # a task with no overload passes params through untouched
            @test Cecelia.preview_params(Cecelia.MeasureLabels(), params, img) === params

            # ── the SECOND half of "the way the RUN does it": section params must be lifted flat.
            # Reported live: "Run would tile this comes up all the time. i've set the tiling to
            # 4096 px. the image is not even 1000px." `blockSize` lives in the `imageTiling`
            # section, so the frontend sends it NESTED; `run_task` flattens, the preview did not,
            # and `SegmentationUtils` silently fell back to its own 512 default. Nothing errors —
            # which is why this needs a test rather than a fix.
            cellpose = Cecelia.CellposeSegment()
            nested = Dict{String,Any}(
                "models" => params["models"],
                "imageTiling" => Dict{String,Any}("blockSize" => 4096, "overlap" => 128))
            flat = Cecelia.preview_params_for_run(cellpose, nested, img)
            @test flat["blockSize"] == 4096            # NOT SegmentationUtils' 512 default
            @test flat["overlap"] == 128
            @test !haskey(flat, "imageTiling")         # lifted, not duplicated
            @test flat["models"]["0"]["cellChannels"] == [2]   # ...and still translated

            # idempotent, so an already-flat bag (REPL, chain, a re-translated dict) is safe
            @test Cecelia.preview_params_for_run(cellpose, flat, img)["blockSize"] == 4096

            # an explicit top-level value wins over a section entry of the same name — the preview
            # must not resurrect a stale nested copy
            both = Dict{String,Any}("models" => params["models"], "blockSize" => 1024,
                                    "imageTiling" => Dict{String,Any}("blockSize" => 4096))
            @test Cecelia.preview_params_for_run(cellpose, both, img)["blockSize"] == 1024

            # the composite goes through the same entry point, since that is what the page runs
            @test Cecelia.preview_params_for_run(composite, nested, img)["blockSize"] == 4096

            # ...and through the shape the API actually receives: a JSON body. Nested objects come
            # back as JSON3 values with SYMBOL keys, which is the standing trap (CLAUDE.md — a
            # `isa Dict` guard is false for `JSON3.Object`). If the lift missed those, this would be
            # the one path that regressed while every hand-built Dict above kept passing.
            body = """{"models":{"0":{"model":"cpsam_v2","matchAs":"base",
                       "cellChannels":["CH3"],"nucChannels":[]}},
                       "imageTiling":{"blockSize":4096,"overlap":128}}"""
            from_json = JSON3.read(body, Dict{String,Any})
            j = Cecelia.preview_params_for_run(cellpose, from_json, img)
            @test j["blockSize"] == 4096
            @test j["overlap"] == 128
            @test !haskey(j, "imageTiling")
            @test j["models"]["0"]["cellChannels"] == [2]

            # ── the THIRD half: `<group>Order` must be resolved, or the preview ignores the chips.
            # Reported live, on a two-pass coastal config: "is preview actually taking into account
            # the selected chips? it doesn't look like it". The order and the off switches live in a
            # SIBLING key that only `_apply_group_order` reads, so the preview neither reordered the
            # passes nor dropped the entries the user had unticked — while the chips said otherwise.
            two = Dict{String,Any}(
                "models" => Dict{String,Any}(
                    "0" => Dict{String,Any}("model" => "cpsam_v2", "matchAs" => "base",
                                            "cellChannels" => ["CH3"], "cellDiameter" => 10),
                    "1" => Dict{String,Any}("model" => "cpsam_v2", "matchAs" => "base",
                                            "cellChannels" => ["CH3"], "cellDiameter" => 3)),
                "modelsOrder" => ["1", "0"])
            rev = Cecelia.preview_params_for_run(cellpose, two, img)
            @test !haskey(rev, "modelsOrder")                       # consumed, not passed through
            # renumbered into RUN order: the chip-first entry becomes group "0"
            @test rev["models"]["0"]["cellDiameter"] == 3
            @test rev["models"]["1"]["cellDiameter"] == 10

            # an unticked entry is DROPPED, not merely moved — the off switch has to be real
            off = merge(two, Dict{String,Any}("modelsOrder" => ["0"]))
            one = Cecelia.preview_params_for_run(cellpose, off, img)
            @test length(one["models"]) == 1
            @test one["models"]["0"]["cellDiameter"] == 10

            # no order key at all keeps running everything — a chain node and a REPL call carry none
            @test length(Cecelia.preview_params_for_run(
                cellpose, Dict{String,Any}("models" => two["models"]), img)["models"]) == 2

            # ...and through JSON, where the order arrives as a JSON3 array of strings
            jbody = """{"models":{"0":{"model":"cpsam_v2","matchAs":"base",
                        "cellChannels":["CH3"],"cellDiameter":10},
                        "1":{"model":"cpsam_v2","matchAs":"base",
                        "cellChannels":["CH3"],"cellDiameter":3}},
                        "modelsOrder":["1","0"]}"""
            jr = Cecelia.preview_params_for_run(cellpose, JSON3.read(jbody, Dict{String,Any}), img)
            @test jr["models"]["0"]["cellDiameter"] == 3
            @test !haskey(jr, "modelsOrder")
        end
    end

    @testset "the preview prepares params with every step run_task uses" begin
        # `preview_params_for_run` mirrors `run_task`'s preparation, and each step it has ever been
        # missing was a silent bug: a nested `blockSize` defaulting to 512, then `<group>Order`
        # ignored so the order chips did nothing. Neither failed loudly. So the LIST is pinned by
        # reading both call sites, and this fails when `run_task` gains a step the preview does not.
        # `task.jl` is a small aggregator; `preview_params_for_run` and the flatten/order/defaults
        # steps live in the split fragments under `tasks/task/*.jl`. Read the whole family so a
        # future move within the family stays invisible to this test.
        task_dir = joinpath(_SUITE_APP_SRC, "tasks", "task")
        src      = read(joinpath(_SUITE_APP_SRC, "tasks", "task.jl"), String) * "\n" *
                   join([read(f, String) for f in
                         filter(f -> endswith(f, ".jl"), readdir(task_dir; join=true))], "\n")
        # Same shape as task.jl above: scheduler.jl is a small aggregator; run_task and the
        # flatten/order/defaults steps live in the split fragments under `tasks/scheduler/*.jl`.
        # Read the whole family so a future move within the family stays invisible to this test.
        sched_dir = joinpath(_SUITE_APP_SRC, "tasks", "scheduler")
        sched     = read(joinpath(_SUITE_APP_SRC, "tasks", "scheduler.jl"), String) * "\n" *
                    join([read(f, String) for f in
                          filter(f -> endswith(f, ".jl"), readdir(sched_dir; join=true))], "\n")

        prep = ["_flatten_sections", "_apply_group_order", "_apply_spec_defaults",
                "_apply_param_requires"]

        # what the preview entry point calls, taken from its body rather than from this list
        body = src[findfirst("function preview_params_for_run", src)[1]:end]
        body = body[1:findfirst("\nend", body)[1]]
        for step in prep
            occursin(step, body) || @error "preview_params_for_run is missing a run step" step
            @test occursin(step, body)
        end

        # and every step the scheduler applies before `validate_params` is in that list
        for step in prep
            @test occursin(step, sched)
        end
        run_steps = [m.match for m in eachmatch(r"_apply_[a-z_]+|_flatten_sections", sched)]
        for step in unique(run_steps)
            step in prep ||
                @error "run_task applies a preparation step the preview does not" step = step
            @test step in prep
        end
    end

    @testset "task_previewable is declared, and composites inherit it" begin
        # The trait replaced the frontend inferring previewability from a cellpose-shaped `models`
        # bag — right about cellpose, silently wrong about every other backend.
        @test Cecelia.task_previewable(Cecelia.CellposeSegment())

        # Default is FALSE: a task says nothing unless the worker can actually run it.
        for t in (Cecelia.MeasureLabels(), Cecelia.DriftCorrect(), Cecelia.ImportOmezarr())
            @test !Cecelia.task_previewable(t)
        end

        # THE overload that matters: the segmentation module page runs the composite, not
        # segment.cellpose. This is how the live preview shipped broken in #421.
        composite = Cecelia._task_from_fun_name("segment.cellposeMeasure")
        @test composite isa Cecelia.CompositeTask
        @test Cecelia.task_previewable(composite)
        # `any`, not `all` — measureLabels has nothing to preview but must not veto the segmentation
        @test any(Cecelia.task_previewable, Cecelia._composite_steps(composite))
        @test !all(Cecelia.task_previewable, Cecelia._composite_steps(composite))

        # every registered task answers without throwing — the definitions route stamps this onto
        # every spec, so one bad overload would otherwise break the whole task picker
        for (fun, task) in Cecelia._fun_name_map()
            @test Cecelia.task_previewable(task) isa Bool
        end
    end

    @testset "task_output_effect labels image-producing tasks" begin
        # editImages tasks all mint a NEW image (new uid via `add_image!`); the module-page picker
        # surfaces this so the user knows they're duplicating, not overwriting.
        for t in (Cecelia.CopyImage(), Cecelia.CropImage(), Cecelia.ZProject(), Cecelia.TProject(),
                  Cecelia.BinImage(), Cecelia.ResampleZ())
            @test Cecelia.task_output_effect(t) == "new-image"
        end
        # cleanupImages tasks write a NEW VERSION alongside `default` (via `_spec_output_value_name`).
        for t in (Cecelia.AfCorrect(), Cecelia.DriftCorrect(), Cecelia.Smooth(),
                  Cecelia.Flip(), Cecelia.DtypeConvert())
            @test Cecelia.task_output_effect(t) == "new-version"
        end
        # Default is `nothing` — a task whose output is not an image (measure/cluster/segment) MUST
        # NOT show a misleading line under the picker.
        for t in (Cecelia.MeasureLabels(), Cecelia.CellposeSegment(), Cecelia.ImportOmezarr())
            @test isnothing(Cecelia.task_output_effect(t))
        end
        # every registered task answers without throwing — same reason as previewable above; the
        # definitions route stamps this onto every spec.
        for (fun, task) in Cecelia._fun_name_map()
            v = Cecelia.task_output_effect(task)
            @test v === nothing || v in ("new-image", "new-version", "in-place")
        end
    end

    # The staging mechanism itself lives in `zarr_utils.staged_store` (Python, where the writers
    # are). Julia mirrors the two suffixes to name the in-progress store a preview watches and to
    # sweep debris a killed run leaves. Nothing connects the two at runtime, so pin them together
    # here — silent drift would aim the preview at a path no writer ever creates.
    @testset "store staging suffixes match the Python side" begin
        py = read(joinpath(_SUITE_REPO, "python", "cecelia", "utils",
                           "zarr_utils.py"), String)
        m_staging    = match(r"^STAGING_SUFFIX\s*=\s*'([^']+)'"m, py)
        m_superseded = match(r"^SUPERSEDED_SUFFIX\s*=\s*'([^']+)'"m, py)
        @test !isnothing(m_staging)
        @test !isnothing(m_superseded)
        @test m_staging.captures[1]    == Cecelia.STORE_STAGING_SUFFIX
        @test m_superseded.captures[1] == Cecelia.STORE_SUPERSEDED_SUFFIX
        @test Cecelia.staging_store_path(joinpath("labels", "X.zarr")) ==
              joinpath("labels", "X.zarr.partial")
    end

    @testset "live_outputs is opt-in per task" begin
        params = Dict{String,Any}("outputValueName" => "X",
                                  "models" => Dict("0" => Dict("matchAs" => "base"),
                                                   "1" => Dict("matchAs" => "nuc")))
        lo = Cecelia.live_outputs(Cecelia.CellposeSegment(), params)
        @test length(lo) == 1
        @test lo[1].kind == "labels"
        # value_name is the REGISTERED name, unsuffixed — the viewer names the layer `({vn})`
        # from it, and that prefix is what colour_labels and layer eviction match on.
        @test lo[1].value_name == "X"
        # ...but the files are the STAGING stores. A run writes through `staged_store`, so while
        # it is going the final path either doesn't exist or (on a re-run) still holds the
        # PREVIOUS segmentation — a preview aimed there would show stale labels. Asserted as
        # literals so changing the suffix has to be a deliberate edit here too.
        @test lo[1].files == ["X.zarr.partial", "X_nuc.zarr.partial"]
        @test lo[1].files ==
              Cecelia.staging_store_path.(Cecelia.segment_label_files("X", params["models"]))
        # falls back to the default value_name like the task itself does
        @test Cecelia.live_outputs(Cecelia.CellposeSegment(),
                                   Dict{String,Any}())[1].value_name == Cecelia.VERSIONED_DEFAULT_VAL

        # A task that assembles its output in RAM and writes it once has nothing to watch, and
        # must NOT claim otherwise — branching writes its store at the very end of the run.
        @test isempty(Cecelia.live_outputs(Cecelia.Branching(), params))
        @test isempty(Cecelia.live_outputs(Cecelia.MeasureLabels(), params))
        @test isempty(Cecelia.live_outputs(Cecelia.BayesianTracking(), params))
    end

    # A preview is a convenience: a task whose declaration throws must still run.
    @testset "a throwing live_outputs never blocks the task" begin
        @test isempty(Cecelia._live_outputs_for(_BadLiveTask(), Dict{String,Any}()))
    end

    # REGRESSION: the composite is what the segmentation module page actually runs, and its steps
    # execute via `_run_task` (no TaskRecord of their own), so the composite must answer for them.
    # This shipped broken — `segment.cellposeMeasure` declared nothing and no preview appeared.
    @testset "a composite declares its steps' live outputs" begin
        params = Dict{String,Any}("outputValueName" => "X",
                                  "models" => Dict("0" => Dict("matchAs" => "base")))
        lo = Cecelia.live_outputs(Cecelia.CompositeTask("segment.cellposeMeasure"), params)
        @test length(lo) == 1
        @test lo[1] == (kind = "labels", value_name = "X", files = ["X.zarr.partial"])

        # a composite of non-streaming steps still declares nothing
        @test isempty(Cecelia.live_outputs(Cecelia.CompositeTask("tracking.correct_measures"), params))
        # unknown composite / no spec → empty, never a throw
        @test isempty(Cecelia.live_outputs(Cecelia.CompositeTask("not.a.composite"), params))
    end

    # The preview worker's request shape. The worker owns the region DECISION (one z-plane,
    # clamping, the 2D fallback — tested in python/cecelia/tests/test_preview_region.py); Julia
    # only resolves which image version to read and where the scratch store goes. Both halves are
    # pinned so they can't drift into disagreeing about the contract.
    @testset "preview_request resolves the same image version a run would" begin
        mktempdir() do dir
            img = CciaImage(; uid = "uid1", name = "n", dir = joinpath(dir, "1", "uid1"))
            img.filepath["default"]   = "ccidImage.ome.zarr"
            img.filepath["corrected"] = "ccidDriftCorrected.ome.zarr"

            region = Dict("xy" => Dict("X" => [0, 512], "Y" => [0, 512]),
                          "z" => 8, "t" => 0, "ndisplay" => 2)

            # reads the version named by the task's OWN valueName — a preview of a corrected
            # image must not silently segment the original
            req = Cecelia.preview_request(
                img, Dict("valueName" => "corrected", "models" => Dict()), region)
            @test req["type"] == "preview"
            @test endswith(req["imPath"], joinpath("0", "uid1", "ccidDriftCorrected.ome.zarr"))
            @test req["taskDir"] == img._dir
            @test req["region"]["z"] == 8

            # default falls back to the primary version, like the task does
            req_default = Cecelia.preview_request(img, Dict("models" => Dict()), region)
            @test endswith(req_default["imPath"], "ccidImage.ome.zarr")

            # an unknown valueName is an error, not a silent segmentation of the wrong image
            @test_throws ErrorException Cecelia.preview_request(
                img, Dict("valueName" => "nope", "models" => Dict()), region)

            # the explicit-paths form: what the API uses, with the store the VIEWER has open, so
            # the pixels and the region can't come from differently-shaped versions
            direct = Cecelia.preview_request(
                "/somewhere/open.ome.zarr", "/somewhere/meta",
                Dict("valueName" => "corrected", "models" => Dict()), region;
                value_name = "B")
            @test direct["imPath"] == "/somewhere/open.ome.zarr"   # NOT re-resolved from ccid
            @test direct["taskDir"] == "/somewhere/meta"
            @test direct["outputValueName"] == "B"
            @test direct["region"]["z"] == 8

            # Channel DISPLAY names travel with the request, because `ccid.json` is the only
            # authoritative copy — the worker deriving them from the store's OME-XML instead is what
            # made every corrected AF layer render grey (its `source` named a layer that did not
            # exist). Sent only when known: an empty list would overwrite the fallback with nothing.
            @test !haskey(direct, "channelNames")
            named = Cecelia.preview_request(
                "/somewhere/open.ome.zarr", "/somewhere/meta",
                Dict("models" => Dict()), region; channel_names = ["SHG", "mem-TOM"])
            @test named["channelNames"] == ["SHG", "mem-TOM"]

            # and the image form fills them in from ccid, per the version being previewed
            img.im_channel_names["default"] = ["SHG", "nuc-GFP", "mem-TOM", "CD169-Kat"]
            from_img = Cecelia.preview_request(img, Dict("models" => Dict()), region)
            @test from_img["channelNames"] == ["SHG", "nuc-GFP", "mem-TOM", "CD169-Kat"]
        end
    end

    @testset "a preview reply is validated into a JSON payload for the browser viewer" begin
        # P7: labels layers carry `valueName`/`path` (labels store on disk) — the browser fetches
        # the mask through `/api/viewer/slab?labels=<vn>&preview=1`, so Julia's pass-through only
        # asserts the payload shape rather than decoding pixels. P7.1: an AF reply carries
        # `previewImages` (per-corrected-channel scratch image stores on disk) instead of an inline
        # block; both branches share this validator.
        layers = [
            Dict("kind" => "labels", "name" => "Preview",
                 "valueName" => "A", "path" => "/x/labels/A__preview.ome.zarr",
                 "shape" => [10, 5, 64, 64], "axes" => ["T", "Z", "Y", "X"]),
        ]
        reply = Dict("layers" => layers,
                     "region" => Dict("T" => [3, 4], "Z" => [1, 2],
                                      "Y" => [0, 4], "X" => [0, 4]),
                     "valueName" => "A", "counts" => Dict("base" => 7))
        payload = Cecelia.preview_reply_payload(reply)
        @test payload["layers"] === layers                  # not re-encoded, not copied
        @test payload["valueName"] == "A"
        @test payload["previewImages"] == Any[]              # labels-only reply → empty AF list
        # the value_name is the REAL one — an unsuffixed stem is what lets `({vn}) Preview` and
        # `({vn}) Labels` share a stem in the labels registry rather than diverge
        @test !occursin("__preview", payload["valueName"])

        # no layers AND no previewImages is a fault, not a viewer showing nothing
        @test_throws ErrorException Cecelia.preview_reply_payload(
            filter(p -> first(p) != "layers", reply))
        @test_throws ErrorException Cecelia.preview_reply_payload(
            merge(reply, Dict("layers" => Any[])))

        # a layer missing any of its geometry is a fault too — caught HERE rather than as a Python
        # traceback in the viewer, which is the point of validating in the pass-through
        for missing_key in ("kind", "name", "valueName", "path", "shape", "axes")
            broken_layer = filter(p -> first(p) != missing_key, layers[1])
            @test_throws ErrorException Cecelia.preview_reply_payload(
                merge(reply, Dict("layers" => Any[broken_layer])))
        end

        # an inline `block` field is protocol-12 and must not reach the browser — the API refuses to
        # answer a stale worker that still sends one
        @test_throws ErrorException Cecelia.preview_reply_payload(
            merge(reply, Dict("layers" => Any[merge(layers[1], Dict("block" => "AA=="))])))

        # an unknown kind is refused rather than passed on for the viewer to guess at
        @test_throws ErrorException Cecelia.preview_reply_payload(
            merge(reply, Dict("layers" => Any[merge(layers[1], Dict("kind" => "heatmap"))])))

        # AF preview: `previewImages` carries one entry per corrected channel, each with a source
        # channel index and disk path — no `kind` (the array IS `image`), no `block`.
        af_images = [
            Dict("sourceChannel" => 1, "name" => "mem-TOM AF",
                 "valueName" => "A", "path" => "/x/data/A__preview_af_ch1.ome.zarr",
                 "shape" => [10, 5, 64, 64], "axes" => ["T", "Z", "Y", "X"]),
            Dict("sourceChannel" => 2, "name" => "CD169 AF",
                 "valueName" => "A", "path" => "/x/data/A__preview_af_ch2.ome.zarr",
                 "shape" => [10, 5, 64, 64], "axes" => ["T", "Z", "Y", "X"]),
        ]
        af_reply = Dict("previewImages" => af_images,
                        "region" => Dict("T" => [3, 4], "Y" => [0, 4], "X" => [0, 4]),
                        "valueName" => "A")
        af_payload = Cecelia.preview_reply_payload(af_reply)
        @test af_payload["previewImages"] === af_images
        @test af_payload["layers"] == Any[]
        @test af_payload["valueName"] == "A"

        # each previewImage needs the same on-disk fields as a labels layer (minus `kind`/`name`,
        # which are labels' whole-list identity — an AF entry is one CHANNEL and identifies itself
        # by `sourceChannel` instead).
        for missing_key in ("sourceChannel", "valueName", "path", "shape", "axes")
            broken = filter(p -> first(p) != missing_key, af_images[1])
            @test_throws ErrorException Cecelia.preview_reply_payload(
                merge(af_reply, Dict("previewImages" => Any[broken])))
        end

        # An inline `block` on an AF entry is protocol-13 and must not reach the browser.
        @test_throws ErrorException Cecelia.preview_reply_payload(
            merge(af_reply,
                  Dict("previewImages" => Any[merge(af_images[1], Dict("block" => "AA=="))])))
    end

    @testset "a composite says which steps it does not preview" begin
        # `preview_params` delegates to the FIRST previewable step, so a composite previews one step
        # and the others silently do not happen. Correct — the alternative is previewing nothing — but
        # it has to be said, because a skipped step can change what the previewed one means:
        # cellposeMeasure previews cellpose and skips measureLabels, so the geometry on screen is
        # not the geometry the run produces.
        seg = Cecelia._task_from_fun_name("segment.cellposeMeasure")
        @test [x["fun"] for x in Cecelia.preview_steps_not_previewed(seg)] == ["segment.measureLabels"]

        # a plain task skips nothing, and neither does a non-composite previewable one
        for fn in ("segment.cellpose", "cleanupImages.afCorrect", "cleanupImages.driftCorrect")
            @test isempty(Cecelia.preview_steps_not_previewed(Cecelia._task_from_fun_name(fn)))
        end
    end

    @testset "AF correction is previewable, with its own param translation" begin
        @test Cecelia.task_previewable(Cecelia.AfCorrect())
        mktempdir() do dir
            img = CciaImage(; uid = "af1", name = "n", dir = joinpath(dir, "1", "af1"))
            mkpath(img._dir)
            img.filepath["default"] = "ccidImage.ome.zarr"
            img.im_channel_names["default"] = ["SHG", "nuc-GFP", "mem-TOM", "CD169-Kat"]
            save!(img)

            # A name this image does not have RAISES rather than dropping out of the competitor list.
            # Dropping looked harmless but changes the correction silently: the weight's denominator
            # loses a term, so every corrected voxel is wrong by an amount nothing reports. Naming a
            # channel that isn't there is a stale saved param, and the fix is to re-pick it.
            stale = Dict{String,Any}("afCombinations" => Dict("1" => Dict{String,Any}(
                "competingChannels" => ["CH4", "CD169-Kat"],
                "targetChannel"     => ["mem-TOM"])))
            stale_err = try
                Cecelia.preview_params_for_run(Cecelia.AfCorrect(), stale, img); nothing
            catch e; e end
            @test stale_err isa ErrorException
            @test occursin("CH4", stale_err.msg) && occursin("CD169-Kat", stale_err.msg)

            params = Dict{String,Any}("afCombinations" => Dict("1" => Dict{String,Any}(
                "competingChannels" => ["CD169-Kat"],
                "targetChannel"     => ["mem-TOM"])))
            out = Cecelia.preview_params_for_run(Cecelia.AfCorrect(), params, img)
            # targetChannel re-keys the combination to the channel being corrected (mem-TOM → 2)
            @test collect(keys(out["afCombinations"])) == ["2"]
            @test out["afCombinations"]["2"]["competingChannels"] == [3]
            @test !haskey(out["afCombinations"]["2"], "targetChannel")

            # idempotent: already-translated indices survive a second pass (a chain or REPL caller)
            again = Cecelia.preview_params_for_run(Cecelia.AfCorrect(), out, img)
            @test again["afCombinations"]["2"]["competingChannels"] == [3]

            # A target named inside its OWN competitor list is dropped, not squared into the denominator
            # a second time — that would quietly halve the channel's own output. Two separate widgets,
            # so picking the same channel in both is an easy slip with one obvious intent.
            self_ref = Dict{String,Any}("afCombinations" => Dict("1" => Dict{String,Any}(
                "competingChannels" => ["mem-TOM", "CD169-Kat"],
                "targetChannel"     => ["mem-TOM"])))
            selfed = Cecelia.preview_params_for_run(Cecelia.AfCorrect(), self_ref, img)
            @test selfed["afCombinations"]["2"]["competingChannels"] == [3]

            # ...and duplicates collapse, so a name listed twice cannot double its weight either
            dupes = Dict{String,Any}("afCombinations" => Dict("1" => Dict{String,Any}(
                "competingChannels" => ["CD169-Kat", "CD169-Kat"],
                "targetChannel"     => ["mem-TOM"])))
            @test Cecelia.preview_params_for_run(Cecelia.AfCorrect(), dupes,
                                                 img)["afCombinations"]["2"]["competingChannels"] == [3]
        end
    end

    @testset "the preview worker gets its own port" begin
        # must not collide with Pluto (7660), the historic napari bridge port (7655), the API
        # server (8080) or the frontend dev server (5173).
        @test Cecelia.PREVIEW_PORT ∉ (7655, 7660, 8080, 5173)
        # not alive until launched — `preview_alive` must never report true for a null process
        @test !Cecelia.preview_alive(Cecelia.PreviewWorker())
    end

    @testset "a stale preview worker is stopped by port, not by handle" begin
        # THE BUG THIS PINS. On a protocol mismatch the backend must remove the worker holding :7656.
        # It only ever PINGED that process, so the handle it has is a bare `PreviewWorker()` with no
        # `proc` — and `close!` on that is a silent no-op. Kill-by-handle therefore left the stale
        # worker listening, the replacement could not bind, and the replacement's readiness ping was
        # answered by the process being replaced: a relaunch loop serving the old code, strictly worse
        # than the mismatch. So the adoption path must kill by PORT, like `_ensure_viewer!` does.
        adopted = Cecelia.PreviewWorker()          # exactly what `_ensure_preview!` probes with
        @test adopted.proc === nothing
        Cecelia.close!(adopted)                    # no-op, and must not throw pretending otherwise
        @test adopted.proc === nothing

        # `_kill_listeners_on_port` is the one helper for this (never inline kill/lsof/taskkill), and
        # the mismatch branch in the API layer has to use it. Source-level because that branch needs a
        # live stale worker to exercise.
        api_src = read(joinpath(dirname(dirname(pathof(Cecelia))), "..", "api", "src",
                                "preview_api.jl"), String)
        @test occursin("_kill_listeners_on_port(PREVIEW_PORT)", api_src)
        # CODE only — the comment above that call names `close!(probe)` to say why it is wrong, and a
        # naive text search cannot tell an explanation from the thing it warns about.
        api_code = filter(l -> !startswith(strip(l), "#"), split(api_src, '\n'))
        @test !any(l -> occursin("close!(probe)", l), api_code)

        # And readiness is the protocol, not merely a reply — the other half of the same loop.
        preview_src = read(joinpath(dirname(pathof(Cecelia)), "preview.jl"), String)
        @test occursin("protocol == PREVIEW_PROTOCOL", preview_src)
    end

    @testset "language boundaries agree on their protocol" begin
        # THE PROBLEM THIS TABLE SOLVES. Julia and Python each hold their own copy of a version, and the
        # only thing keeping them equal is that someone remembers to change both. Measured record before
        # this test existed: the preview pair was bumped by hand three times and the fourth was nearly
        # missed; the napari bridge and the params contract had no version at all.
        #
        # A mismatch is never a clean failure. A stale peer answers the handshake perfectly and then
        # misreads the actual work — it has surfaced as `unexpected keyword argument 'mask'`, as a bare
        # "Preview failed", and as `invalid literal for int() with base 10: 'CH3'`, none of which name the
        # cause. So every boundary gets a version, and every version gets asserted here.
        #
        # A fourth boundary = one more row.
        repo = joinpath(dirname(dirname(pathof(Cecelia))), "..")
        boundaries = [
            # (what,            python file,                          python const,       julia value)
            ("preview worker",  "preview/preview_worker.py",           "PROTOCOL",         Cecelia.PREVIEW_PROTOCOL),
            ("params contract", "python/cecelia/utils/script_utils.py", "CONTRACT_VERSION", Cecelia.PY_CONTRACT_VERSION),
        ]
        for (what, rel, const_name, julia_value) in boundaries
            path = joinpath(repo, rel)
            @test isfile(path)
            m = match(Regex("^" * const_name * raw"\s*=\s*(\d+)", "m"), read(path, String))
            @test m !== nothing
            m === nothing && continue
            py = parse(Int, m.captures[1])
            @test py == julia_value
            py == julia_value ||
                @warn "$what: Python says $py, Julia says $julia_value — bump BOTH sides" rel
        end
    end

    @testset "resident python legs agree on their websocket frame cap" begin
        # The same failure shape as the protocol table above, in a number nobody thought of as a
        # version. Both ends of each leg cap the size of a frame they will accept, the caps are
        # independent, and the Python side had been raised to 64 MiB while the Julia side sat on
        # HTTP.jl's 16 MiB default. That made the backend the narrow leg: the flow-metrics sheet on a
        # 1050×1047 movie is 40–75 MB in one frame, and the read died with `1009: message too large` on
        # every image except the small one it was built against. Nothing named the cap.
        repo = joinpath(dirname(dirname(pathof(Cecelia))), "..")
        mib = Cecelia.WS_MAX_FRAME_SIZE ÷ (1024 * 1024)
        for (what, rel) in (("preview worker", "preview/preview_worker.py"),)
            path = joinpath(repo, rel)
            @test isfile(path)
            src = read(path, String)
            m = match(r"^WS_MAX_SIZE\s*=\s*(\d+)\s*\*\s*1024\s*\*\s*1024"m, src)
            @test m !== nothing
            m === nothing && continue
            py = parse(Int, m.captures[1])
            @test py == mib
            py == mib ||
                @warn "$what: Python caps at $(py) MiB, Julia at $(mib) MiB — set BOTH" rel
            # and it must actually reach the server, not merely be defined
            @test occursin("max_size=WS_MAX_SIZE", src)
        end
        # Julia is a CLIENT on this leg, so the cap is a keyword on the open — the default is what
        # was wrong, so an unqualified `WebSockets.open` here is the bug returning.
        for rel in ("preview.jl",)
            src = read(joinpath(dirname(pathof(Cecelia)), rel), String)
            @test occursin("maxframesize = WS_MAX_FRAME_SIZE", src)
        end
    end

    @testset "the params contract is checked where every runner already goes" begin
        # The guard lives in `script_params`, not in each runner, so a NEW runner is covered by writing
        # nothing. Asserted on the source because the check runs in a subprocess we do not spawn here.
        su = read(joinpath(dirname(dirname(pathof(Cecelia))), "..",
                           "python", "cecelia", "utils", "script_utils.py"), String)
        @test occursin("def check_contract_version", su)
        @test occursin(r"def script_params\(\):(?s).{0,600}check_contract_version\(\)", su)
        # ...and run_py is what supplies it, as an env var rather than a params field
        pr = read(joinpath(dirname(dirname(pathof(Cecelia))), "src", "py_runner.jl"), String)
        @test occursin("CECELIA_PY_CONTRACT", pr)
        @test occursin("PY_CONTRACT_VERSION", pr)
    end

    @testset "img_labels_path resolves registered and in-progress stores" begin
        mktempdir() do dir
            img = CciaImage(; uid = "uid1", name = "name1", dir = joinpath(dir, "1", "uid1"))
            img.labels["A"] = ["A.zarr", "A_nuc.zarr"]
            @test Cecelia.img_labels_dir(img) == joinpath(img._dir, "labels")
            # registered → the recorded filename (first of the set)
            @test Cecelia.img_labels_path(img, "A") == joinpath(img._dir, "labels", "A.zarr")
            # NOT registered → the convention. This is where a FINISHED store lands; a run in
            # progress writes to the staging sibling and is renamed here on completion.
            @test Cecelia.img_labels_path(img, "X") == joinpath(img._dir, "labels", "X.zarr")
            @test Cecelia.staging_store_path(Cecelia.img_labels_path(img, "X")) ==
                  joinpath(img._dir, "labels", "X.zarr.partial")
        end
    end

    # A patch with a bad script path fails only when a user clicks Apply, so resolve every
    # registered one the same way `run_py` does. Nothing else covers maintenance.jl.
    @testset "every maintenance patch resolves to a script on disk" begin
        repo = _SUITE_REPO
        patches = Cecelia.maintenance_patches()
        @test !isempty(patches)
        @test length(unique(p.id for p in patches)) == length(patches)
        for p in patches
            path = startswith(p.script, "tasks/") ?
                   joinpath(repo, "app", "src", p.script) :
                   joinpath(repo, "python", "cecelia", p.script)
            @test isfile(path)
            @test !isnothing(Cecelia.maintenance_patch(p.id))
            # Copy budget (docs/ui/COPY.md). This description sits in Settings and is read every
            # time, so it gets one line + the one caveat that matters — the store-debris entry had
            # grown to 674 characters explaining its own detection strategy, which belongs in the
            # runner. 160 leaves room for a caveat and none for an essay.
            @test length(p.description) <= 160
            @test !isempty(p.description)
            @test !endswith(p.title, ".")            # a title is a fragment, not a sentence
        end
    end
end

