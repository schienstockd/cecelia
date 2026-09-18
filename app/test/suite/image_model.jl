# ── Legacy `kind` on disk is silently ignored ────────────────────────────────
# Guards the on-disk contract: a pre-existing ccid.json/project.json with a `kind` key must load
# cleanly (no field on the struct) and the next save! must strip it. Project-wide static/live/flow
# distinction was dropped in favour of per-image axis gating (Cecelia.task_applies).
@testset "Legacy `kind` on disk — ignored + stripped" begin
    proj = create_project!(name="legacy-kind-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    # Inject legacy `kind` back into every ccid.json / project.json on disk
    for f in (joinpath(proj.root, "project.json"),
              joinpath(s._dir, "ccid.json"),
              joinpath(img._dir, "ccid.json"))
        raw = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(f, String)))
        raw["kind"] = "live"
        open(f, "w") do io; JSON3.pretty(io, raw); end
    end

    # Load — must not error, must not surface `kind` as a struct field
    loaded = load_project(proj.uid)
    @test !hasfield(typeof(loaded), :kind)
    r_img = init_object(proj.uid, img.uid)
    @test r_img isa CciaImage
    @test !hasfield(typeof(r_img), :kind)

    # save! strips `kind` from disk
    save!(loaded)
    for f in (joinpath(proj.root, "project.json"),
              joinpath(s._dir, "ccid.json"),
              joinpath(img._dir, "ccid.json"))
        raw = JSON3.read(read(f, String))
        @test !haskey(raw, :kind)
    end
    rm(proj.root; recursive=true)
end

# ── Image round-trip (status + attr) ────────────────────────────────────────
# Regression guard: save!(img) must persist status and attr, not silently drop them.
@testset "Image status/attr round-trip" begin
    proj = create_project!(name="rt-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    img.status = IMAGE_DONE
    img.attr["condition"] = "treated"
    save!(img)
    r = init_object(proj.uid, img.uid)
    @test r isa CciaImage
    @test r.status == IMAGE_DONE
    @test get(r.attr, "condition", "") == "treated"
    rm(proj.root; recursive=true)
end

# ── Task subdirs: none pre-created, dead ones cleared on load ───────────────
# An image folder holds only what has actually run — every writer makes its own directory, so nothing
# is created up front (the `[dirs.tasks]` pre-create table is gone). The eight names no writer in the
# codebase uses are shed when an old image loads — ONLY if empty, and never a name a live writer uses.
@testset "New image gets no task subdirs" begin
    proj = create_project!(name="nodirs-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    # ccid.json and nothing else — the image dir describes what has run, and nothing has
    @test readdir(img._dir) == ["ccid.json"]
    @test !isdir(joinpath(img._dir, "labels"))
    rm(proj.root; recursive=true)
end

@testset "Legacy task subdirs dropped on load" begin
    proj = create_project!(name="deaddir-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    live = ("logs", "labels", "labelProps", "data", "tasks")
    for sub in (Cecelia._DEAD_TASK_DIRS..., live...); mkpath(joinpath(img._dir, sub)); end
    init_object(proj.uid, img.uid)
    for sub in Cecelia._DEAD_TASK_DIRS
        @test !isdir(joinpath(img._dir, sub))
    end
    # a live name is never shed, even when empty — removing one would race the task that just made it
    for sub in live
        @test isdir(joinpath(img._dir, sub))
    end

    # a non-empty one is somebody's data, whatever we think wrote it
    mkpath(joinpath(img._dir, "mesh"))
    write(joinpath(img._dir, "mesh", "keep.txt"), "x")
    init_object(proj.uid, img.uid)
    @test isfile(joinpath(img._dir, "mesh", "keep.txt"))
    rm(proj.root; recursive=true)
end

# The one name that survived `[dirs.tasks]`, now a constant rather than a config lookup.
# Build the base with `joinpath` and reuse it on both sides: a literal "/x/y" keeps its forward
# slashes on Windows while `joinpath` appends a backslash, so hardcoding either separator asserts
# the platform rather than the behaviour.
@testset "task_run_dir is <base>/tasks" begin
    base = joinpath("x", "y")
    @test Cecelia.task_run_dir(base) == joinpath(base, "tasks")
    @test basename(Cecelia.task_run_dir(base)) == "tasks"
end

# ── Branch labels round-trip (BRANCHING_PLAN.md Decision 6) ──────────────────
# Skeleton (branch) label sets live in a dedicated `branch_labels` field, NOT in the generic
# `labels` dict, so the labels/measure/tracking pickers never see branch labels. Guards: the
# field survives save!/init_object, its accessor resolves the disk path from branchLabels/,
# and a legacy ccid.json without the key still loads (defaults to empty).
@testset "Branch labels round-trip" begin
    proj = create_project!(name="branch-rt-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    @test isempty(img.branch_labels)
    img.branch_labels["stroma"] = ["stroma.zarr"]
    save!(img)
    r = init_object(proj.uid, img.uid)
    @test r isa CciaImage
    @test r.branch_labels["stroma"] == ["stroma.zarr"]
    @test img_branch_labels_dir(r) == joinpath(r._dir, "branchLabels")
    @test img_branch_labels_path(r, "stroma") == joinpath(r._dir, "branchLabels", "stroma.zarr")
    # unregistered value_name falls back to {value_name}.zarr (write path)
    @test img_branch_labels_path(r, "shg") == joinpath(r._dir, "branchLabels", "shg.zarr")

    # legacy ccid.json (no branch_labels key) → empty
    ccid = joinpath(r._dir, "ccid.json")
    raw  = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
    delete!(raw, "branch_labels")
    open(ccid, "w") do io; JSON3.write(io, raw); end
    legacy = init_object(proj.uid, img.uid)
    @test isempty(legacy.branch_labels)
    rm(proj.root; recursive=true)
end

# ── resolve_version — inner-axis companion to resolve_value_name ─────────────
# Legacy shape (bare scalar / vector at the value_name key) always resolves to `v1`.
# Struct field types are not widened in P1b, so today every real project resolves this way;
# the new-shape branch is exercised via unit tests in labelprops.jl (`unversion_value`). End-to-end
# proof through a real struct happens in P2 when writers start producing versioned entries.
# See docs/todo/VN_VERSIONING_PLAN.md.
@testset "resolve_version — legacy always resolves to v1" begin
    proj = create_project!(name="resolve-ver-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    img.filepath["default"] = "ccidImage.ome.zarr"
    img.filepath[VERSIONED_ACTIVE_KEY] = "default"
    img.label_props["default"] = "default.h5ad"
    img.label_props[VERSIONED_ACTIVE_KEY] = "default"
    img.labels["default"] = ["labels.zarr"]

    @test resolve_version(img, :filepath) == LATEST_DEFAULT_VAL
    @test resolve_version(img, :label_props) == LATEST_DEFAULT_VAL
    @test resolve_version(img, :labels) == LATEST_DEFAULT_VAL
    @test resolve_version(img, :filepath, "default") == LATEST_DEFAULT_VAL
    # Missing value_name entry still resolves to v1 (the default), not an error.
    @test resolve_version(img, :filepath, "nonexistent") == LATEST_DEFAULT_VAL
    rm(proj.root; recursive=true)
end

# ── img_*_path helpers ignore the `version` kwarg on legacy shape ────────────
# Backward-compat verification: passing an explicit `version` on today's data must not change the
# result (the entry is a bare scalar, so `unversion_value` returns it unchanged either way).
@testset "img_*_path — legacy shape, version kwarg is a no-op" begin
    proj = create_project!(name="ver-noop-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    img.filepath["default"] = "ccidImage.ome.zarr"
    img.filepath[VERSIONED_ACTIVE_KEY] = "default"
    img.label_props["default"] = "default.h5ad"
    img.label_props[VERSIONED_ACTIVE_KEY] = "default"
    img.labels["default"] = ["labels.zarr"]
    img.branch_labels["stroma"] = ["stroma.zarr"]

    for ver in (nothing, "v1", "v99")
        @test img_filepath(img; version = ver) == joinpath(img_zero_dir(img), "ccidImage.ome.zarr")
        @test img_label_props_path(img, "default"; version = ver) ==
              joinpath(img_label_props_dir(img), "default.h5ad")
        @test img_labels_path(img, "default"; version = ver) ==
              joinpath(img_labels_dir(img), "labels.zarr")
        @test img_branch_labels_path(img, "stroma"; version = ver) ==
              joinpath(img_branch_labels_dir(img), "stroma.zarr")
    end
    rm(proj.root; recursive=true)
end

# ── Reserved value_name suffixes ─────────────────────────────────────────────
# __tracks and __branch are companion-table markers, not legal user segmentation names.
@testset "Reserved value_name suffixes" begin
    @test  is_reserved_value_name("stroma__tracks")
    @test  is_reserved_value_name("stroma__branch")
    @test !is_reserved_value_name("stroma")
    @test !is_reserved_value_name("stroma.branch")   # dot-suffix is the old R convention; not reserved
end

# ── Per-image user flags (included / note / starred) round-trip ──────────────
# Guards: new images default to included + unstarred; the flags survive save!/init_object; and a
# legacy ccid.json with none of the keys loads as included (the accessor never sees a missing field).
@testset "Image included/note/starred round-trip" begin
    proj = create_project!(name="incl-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    @test image_included(img)                 # default: included
    @test img.note == ""
    @test img.starred == false                # default: not starred

    img.included = false
    img.note = "bad drift reference channel"
    img.starred = true
    save!(img)
    r = init_object(proj.uid, img.uid)
    @test r isa CciaImage
    @test !image_included(r)
    @test r.note == "bad drift reference channel"
    @test r.starred

    # legacy file (none of the keys) → included, empty note, unstarred
    ccid = joinpath(r._dir, "ccid.json")
    raw  = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
    delete!(raw, "included"); delete!(raw, "note"); delete!(raw, "starred")
    open(ccid, "w") do io; JSON3.write(io, raw); end
    legacy = init_object(proj.uid, img.uid)
    @test image_included(legacy)
    @test legacy.note == ""
    @test legacy.starred == false
    rm(proj.root; recursive=true)
end

# ── Per-task param memory (funParams) — R moduleFunParams parity ─────────────
# Last-used params are remembered in ccid.json under meta["funParams"][fun], per image and per
# set. Guards: round-trips through save!/init_object, per-fun keys don't clobber, set-level too.
@testset "funParams per-object memory" begin
    proj = create_project!(name="fp-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    save!(img)

    @test read_module_fun_params(img._dir, "cleanupImages.driftCorrect") === nothing  # absent

    p = Dict{String,Any}("valueName" => "driftCorrected", "driftChannel" => ["DAPI"])
    write_module_fun_params!(img._dir, "cleanupImages.driftCorrect", p)
    got = read_module_fun_params(img._dir, "cleanupImages.driftCorrect")
    @test !isnothing(got)
    @test got["valueName"] == "driftCorrected"
    @test got["driftChannel"] == ["DAPI"]

    # init_object loads funParams into the object's meta; a load-modify-save then preserves them
    # (the loaded object carries funParams, so save! doesn't drop them — unlike a stale object).
    r = init_object(proj.uid, img.uid)
    @test haskey(r.meta, "funParams")
    r.status = IMAGE_DONE; save!(r)
    r2 = init_object(proj.uid, img.uid)
    @test r2.status == IMAGE_DONE
    @test read_module_fun_params(r2._dir, "cleanupImages.driftCorrect")["valueName"] == "driftCorrected"

    # a second task's params coexist under its own key (no clobber)
    write_module_fun_params!(img._dir, "cleanupImages.smooth",
                             Dict{String,Any}("valueName" => "default"))
    @test read_module_fun_params(img._dir, "cleanupImages.driftCorrect")["valueName"] == "driftCorrected"
    @test read_module_fun_params(img._dir, "cleanupImages.smooth")["valueName"] == "default"

    # set-level memory uses the same dir-based mechanism on the set's ccid.json
    write_module_fun_params!(s._dir, "cleanupImages.driftCorrect",
                             Dict{String,Any}("valueName" => "setDefault"))
    @test read_module_fun_params(s._dir, "cleanupImages.driftCorrect")["valueName"] == "setDefault"

    rm(proj.root; recursive=true)
end

# One blob per TASK is wrong the moment a task runs twice under different names: segmenting `Tcell`
# and then `Neutrophil` left the form showing Neutrophil's settings, so re-running Tcell meant
# re-entering every model parameter. Params are now ALSO banked per output name.
@testset "funParams remembered per output name" begin
    proj = create_project!(name="fpn-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    save!(img)
    fun = "segment.cellpose"

    tcell = Dict{String,Any}("outputValueName" => "Tcell", "cellDiameter" => 8)
    neutr = Dict{String,Any}("outputValueName" => "Neutrophil", "cellDiameter" => 15)
    write_module_fun_params!(img._dir, fun, tcell; value_name = "Tcell")
    write_module_fun_params!(img._dir, fun, neutr; value_name = "Neutrophil")

    # each name keeps its OWN params — the whole point
    @test read_module_fun_params(img._dir, fun; value_name = "Tcell")["cellDiameter"] == 8
    @test read_module_fun_params(img._dir, fun; value_name = "Neutrophil")["cellDiameter"] == 15

    # the flat blob still tracks the most recent run whatever it was called, because that is what a
    # NEW name falls back to — and falling back to bare task defaults would be a worse starting point
    @test read_module_fun_params(img._dir, fun)["cellDiameter"] == 15
    @test read_module_fun_params(img._dir, fun; value_name = "Macrophage")["cellDiameter"] == 15

    # …but the by-name reader does NOT fall back. The form needs the two answers distinguishable:
    # "nothing banked for this name" must not overwrite a form the user has just edited.
    @test read_module_fun_params_by_name(img._dir, fun, "Macrophage") === nothing
    @test read_module_fun_params_by_name(img._dir, fun, "Tcell")["cellDiameter"] == 8
    @test read_module_fun_params_by_name(img._dir, fun, "") === nothing

    # a task with no name banked is untouched by any of this (the pre-existing path)
    write_module_fun_params!(img._dir, "cleanupImages.smooth", Dict{String,Any}("spatialSigma" => 2))
    @test read_module_fun_params(img._dir, "cleanupImages.smooth")["spatialSigma"] == 2
    @test read_module_fun_params_by_name(img._dir, "cleanupImages.smooth", "Tcell") === nothing

    # survives the object round-trip, like funParams above
    r = init_object(proj.uid, img.uid)
    r.status = IMAGE_DONE; save!(r)
    @test read_module_fun_params_by_name(init_object(proj.uid, img.uid)._dir, fun, "Tcell")["cellDiameter"] == 8

    rm(proj.root; recursive=true)
end

# Banking params per name only helps runs made AFTER it existed — which on every real project is
# none of them, so every name resolved to "nothing banked" and the form restored nothing. The run log
# already held each run's params and the output name is recoverable from them, so names that predate
# the by-name record are answered from there.
@testset "funParams by name fall back to the run log" begin
    proj = create_project!(name="fpl-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    save!(img)
    fun = "segment.cellpose"

    # the history a project already has: two names segmented, nothing banked by name
    append_run_log!(img, fun, "corrected", "done",
                    Dict{String,Any}("outputValueName" => "Tcell", "cellDiameter" => 8))
    append_run_log!(img, fun, "corrected", "done",
                    Dict{String,Any}("outputValueName" => "Neutrophil", "cellDiameter" => 15))
    @test isnothing(read_module_fun_params_by_name(img._dir, fun, "Macrophage"))
    @test read_module_fun_params_by_name(img._dir, fun, "Tcell")["cellDiameter"] == 8
    @test read_module_fun_params_by_name(img._dir, fun, "Neutrophil")["cellDiameter"] == 15

    # the INPUT version comes back too. `_run_log_params` strips `valueName` (the entry carries it as
    # its own field), and a record restored without it silently resets which version the task runs on.
    @test read_module_fun_params_by_name(img._dir, fun, "Tcell")["valueName"] == "corrected"

    # the newest run under a name wins — this is "the settings it was last segmented with"
    append_run_log!(img, fun, "corrected", "done",
                    Dict{String,Any}("outputValueName" => "Tcell", "cellDiameter" => 11))
    @test read_module_fun_params_by_name(img._dir, fun, "Tcell")["cellDiameter"] == 11

    # …and only among runs that SUCCEEDED — a later failure does not become the name's settings
    append_run_log!(img, fun, "corrected", "failed",
                    Dict{String,Any}("outputValueName" => "Tcell", "cellDiameter" => 99))
    @test read_module_fun_params_by_name(img._dir, fun, "Tcell")["cellDiameter"] == 11

    # A name that ONLY ever failed restores nothing, rather than falling back to the failed run. The
    # two halves have to agree on one set of names: the picker offers what exists in the namespace,
    # and a failed run wrote nothing there — so restoring for it would answer for a name the list can
    # never offer. Same for a run still in flight, which has not written its output yet.
    append_run_log!(img, fun, "corrected", "failed",
                    Dict{String,Any}("outputValueName" => "Bcell", "cellDiameter" => 4))
    @test isnothing(read_module_fun_params_by_name(img._dir, fun, "Bcell"))
    append_run_log!(img, fun, "corrected", Cecelia.RUN_LOG_RUNNING,
                    Dict{String,Any}("outputValueName" => "Eos", "cellDiameter" => 5))
    @test isnothing(read_module_fun_params_by_name(img._dir, fun, "Eos"))

    # a name is matched through the SPEC's `namespace`, not by a key called `outputValueName` — the
    # run log records six different spellings depending on the task
    append_run_log!(img, "clustPops.cluster", "", "done",
                    Dict{String,Any}("valueNameSuffix" => "immune", "resolution" => 0.6))
    @test read_module_fun_params_by_name(img._dir, "clustPops.cluster", "immune")["resolution"] == 0.6

    # …and through a COMPOSITE's steps. This is what the segmentation page runs, so a log full of
    # `segment.cellposeMeasure` entries is the realistic case, not the plain-task one above.
    append_run_log!(img, "segment.cellposeMeasure", "afCorrected", "done",
                    Dict{String,Any}("outputValueName" => "Podocyte", "cellDiameter" => 6))
    got = read_module_fun_params_by_name(img._dir, "segment.cellposeMeasure", "Podocyte")
    @test !isnothing(got) && got["cellDiameter"] == 6 && got["valueName"] == "afCorrected"

    # another task's run under the same name is not this task's params
    @test isnothing(read_module_fun_params_by_name(img._dir, "cleanupImages.smooth", "Tcell"))

    # a banked record still wins — it is the exact answer, the log is the retroactive half
    write_module_fun_params!(img._dir, fun, Dict{String,Any}(
        "outputValueName" => "Tcell", "cellDiameter" => 3); value_name = "Tcell")
    @test read_module_fun_params_by_name(img._dir, fun, "Tcell")["cellDiameter"] == 3

    # the SET dir keeps no run log, so it answers from the banked record only — no crash, no guess
    @test isnothing(read_module_fun_params_by_name(s._dir, fun, "Tcell"))

    rm(proj.root; recursive=true)
end

# The namespaces a `valueNameInput` suggests from need an IMAGE-owned accessor, like the graph/track
# /branch ones — `INVENTORY.md`'s rule, and the reason `_clustfeatures_suffixes` (which takes a
# label-props PATH and owns the sidecar's three historical layouts) is WRAPPED here rather than moved.
@testset "image accessors for the suggestion namespaces" begin
    proj = create_project!(name="ns-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    save!(img)

    # nothing on disk → empty, never an error: an image with no stats/clustering is the normal case
    @test img_stats_suffixes(img) == String[]
    @test img_cluster_suffixes(img) == String[]

    # stats are listed from spatialStats/{suffix}.json, and the PATH helper is the one speller —
    # the writer (neighbourStats.jl) and reader (ai/spatial.jl) both used to join it inline
    mkpath(img_stats_dir(img))
    write(img_stats_path(img, "contacts"), "{}")
    write(img_stats_path(img, "aggregates"), "{}")
    write(joinpath(img_stats_dir(img), "notes.txt"), "x")     # not a .json → not a run
    @test img_stats_suffixes(img) == ["aggregates", "contacts"]

    # clustering runs come from the clustfeatures manifest beside the label props, per SEGMENTATION,
    # and the two families are listed separately so `clusters.immune` and `regions.immune` coexist
    mkpath(img_label_props_dir(img))
    props = img_label_props_path(img, "default")
    write(Cecelia._clustfeatures_path(props),
          """{"clusters.immune": {"features": ["a"]}, "regions.niches": {"features": ["b"]}}""")
    @test img_cluster_suffixes(img, "default"; family="clusters") == ["immune"]
    @test img_cluster_suffixes(img, "default"; family="regions")  == ["niches"]

    # a different segmentation has its own manifest — this is per (image, value_name), not per image
    @test img_cluster_suffixes(img, "other"; family="clusters") == String[]

    rm(proj.root; recursive=true)
end

# `task_output_name` is the Julia twin of `taskOutput` (frontend/src/utils/taskOutput.ts) — the name a
# run writes under, resolved from the spec's `namespace` because SIX different keys can carry it. The
# two cannot call each other, so the SPECS are the shared contract and each side is pinned against
# them (the TS half is `taskOutput.test.ts`). Same arrangement as the calibration writers.
@testset "task_output_name agrees with the frontend rule" begin
    # a `valueNameInput` param, found by its declared namespace and not by its key
    @test Cecelia.task_output_name("segment.cellpose",
                           Dict{String,Any}("outputValueName" => "Tcell")) == "Tcell"
    @test Cecelia.task_output_name("spatialAnalysis.cellNeighbours",
                           Dict{String,Any}("graphSuffix" => "pooled")) == "pooled"

    # falls back to the param's DEFAULT when the form has not set one — the same name the run will use
    @test Cecelia.task_output_name("segment.cellpose", Dict{String,Any}()) == "default"

    # whitespace is not a name; an unknown fun_name is not this function's error to raise
    @test Cecelia.task_output_name("segment.cellpose", Dict{String,Any}("outputValueName" => "  ")) == ""
    @test Cecelia.task_output_name("no.such.task", Dict{String,Any}("outputValueName" => "x")) == ""

    # a task that names no output of its own reports "", never a guess
    @test Cecelia.task_output_name("importImages.omezarr", Dict{String,Any}()) == ""

    # EVERY spec carrying a valueNameInput resolves through it — so flipping another task's param
    # (Phase 3: clustering, stats, models) cannot silently fail to be remembered per name
    for (fun, task) in Cecelia._fun_name_map()
        spec = Cecelia._task_spec(task)
        isnothing(spec) && continue
        vni = String[]
        each_spec_param(get(spec, "params", [])) do p, _gk
            String(something(spec_get(p, "type", ""), "")) == "valueNameInput" &&
                push!(vni, String(something(spec_get(p, "key", ""), "")))
        end
        isempty(vni) && continue
        k = first(vni)
        @test Cecelia.task_output_name(fun, Dict{String,Any}(k => "probe-name")) == "probe-name"
    end

    # …and every COMPOSITE resolves its steps' name. The loop above cannot see them: a composite spec
    # declares no params of its own, so it carries no `valueNameInput` to probe and was skipped —
    # while being what the module pages actually run (`segment.cellposeMeasure`, not `segment.cellpose`).
    # That is the trait-recursion trap task.jl warns about, and it shipped once: params banked per
    # output name keyed off this function, so no segmentation started from a module page was ever
    # remembered under its name. The frontend was fine, because `api_task_definitions` merges a
    # composite's step params before it ever sees them — which is what made it silent.
    composites = 0
    for (fun, task) in Cecelia._fun_name_map()
        task isa Cecelia.CompositeTask || continue
        keys_ = String[]
        for sub in Cecelia._composite_steps(task)
            spec = Cecelia._task_spec(sub)
            isnothing(spec) && continue
            each_spec_param(get(spec, "params", [])) do p, _gk
                String(something(spec_get(p, "type", ""), "")) == "valueNameInput" &&
                    push!(keys_, String(something(spec_get(p, "key", ""), "")))
            end
        end
        isempty(keys_) && continue
        composites += 1
        @test Cecelia.task_output_name(fun, Dict{String,Any}(first(keys_) => "probe-name")) == "probe-name"
    end
    @test composites > 0        # the loop above must not pass by finding nothing to check
end

# ── Channel names use the versioned convention ──────────────────────────────
# Regression guard: channel names were stored unversioned under meta, where the
# task/API readers (which use top-level versioned imChannelNames) never saw them.
@testset "Channel names versioned round-trip" begin
    proj = create_project!(name="cn-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    set_channel_names!(img, ["DAPI", "GFP"]; check_length=false)
    save!(img)

    # model accessor round-trips through save!/load
    r = init_object(proj.uid, img.uid)
    @test channel_names(r) == ["DAPI", "GFP"]

    # on-disk: top-level versioned imChannelNames (the shape tasks/API use), not under meta
    raw = JSON3.read(read(joinpath(img._dir, "ccid.json"), String), Dict{String,Any})
    @test haskey(raw, "imChannelNames")
    @test !haskey(Dict{String,Any}(get(raw, "meta", Dict())), "imChannelNames")
    @test versioned_active(raw["imChannelNames"]) == "default"
    # readable via the exact helper tasks/API use
    @test collect(String, versioned_get_field(raw, "imChannelNames")) == ["DAPI", "GFP"]

    rm(proj.root; recursive=true)
end

# read_ccid_raw is the one ccid.json read+Symbol-key-normalize helper (used by the api layer).
# versioned_get is the single active-value accessor for both String→String path dicts and the
# Any/JSON3 raw dicts (replaced the removed image.jl `active`).
@testset "read_ccid_raw + versioned_get on path dicts" begin
    mktempdir() do d
        p = joinpath(d, "ccid.json")
        write(p, """{"filepath":{"default":"x.ome.zarr","_active":"default"},"class":"CciaImage"}""")
        raw = read_ccid_raw(p)
        @test raw isa Dict{String,Any}
        @test all(k -> k isa String, keys(raw))
        @test raw["class"] == "CciaImage"
        # readable via the exact helper the api/tasks use (nothing → active entry)
        @test versioned_get_field(raw, "filepath") == "x.ome.zarr"
    end
    # versioned_get on a concrete String→String versioned dict (the img.filepath / img.label_props shape)
    d = Dict{String,String}("default" => "a.zarr", "v2" => "b.zarr", "_active" => "v2")
    @test versioned_get(d) == "b.zarr"                 # active entry
    @test sort(versioned_keys(d)) == ["default", "v2"] # excludes _active
end

# ── Destructive ops ──────────────────────────────────────────────────────────
@testset "delete_image! / delete_set!" begin
    proj = create_project!(name="del-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    a    = add_image!(s; name="a")
    b    = add_image!(s; name="b")

    delete_image!(s, b.uid)
    @test !(b.uid in s.image_uids)
    @test !isdir(joinpath(proj.root, "0", b.uid))
    @test !isdir(joinpath(proj.root, "1", b.uid))
    @test !(b.uid in init_object(proj.uid, s.uid).image_uids)   # persisted

    set_uid = s.uid
    delete_set!(proj, set_uid)
    @test !(set_uid in proj.set_uids)
    @test !isdir(joinpath(proj.root, "1", set_uid))
    @test !isdir(joinpath(proj.root, "1", a.uid))               # member removed too
    @test !(set_uid in load_project(proj.uid).set_uids)         # persisted

    rm(proj.root; recursive=true)
end

# ── Boundary contract: run a REAL module function end-to-end, no api/ ───────
# The whole suite loads only `using Cecelia` (api/ is not on the path). This runs
# an actual task (RemoveImage — real ccid.json + disk work, no external binary) to
# completion through the public `run_task` entrypoint. Catches coupling creeping back.
@testset "Boundary contract — real module fn end-to-end" begin
    proj = create_project!(name="bc-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    # register a real on-disk image version
    zarr = joinpath(img_zero_dir(img), "ccidImage.ome.zarr")
    mkpath(zarr)
    img.filepath["default"] = "ccidImage.ome.zarr"
    img.filepath["_active"] = "default"
    img.status = IMAGE_DONE
    save!(img)

    logs = String[]
    result = run_task(RemoveImage(), img,
        Dict{String,Any}("valueName"=>"default", "newDefault"=>"default");
        on_log = l -> push!(logs, l))

    @test result isa Dict
    @test result["removedValue"] == "default"
    @test result["cleared"] == true          # primary removal clears dims/status
    @test !isdir(zarr)                        # file actually deleted from disk
    @test !isempty(logs)                      # on_log callback fired (no WS needed)

    reloaded = init_object(proj.uid, img.uid)
    @test reloaded.status == IMAGE_PENDING        # primary removal reset status
    @test !haskey(reloaded.filepath, "default")  # version entry gone
    rm(proj.root; recursive=true)
end

# ── Storage reclaim — free every non-active image version, keep the active one ───────
@testset "Storage reclaim" begin
    # _path_bytes is the ONE "how big is this on disk" answer, shared by storage accounting, version
    # removal and the image-metadata modal — a directory is walked, a plain file is stat'd, and
    # anything absent is 0 rather than an error (a caller listing versions must not throw on a store
    # that isn't there).
    mktempdir() do d
        f = joinpath(d, "one.bin"); write(f, rand(UInt8, 4096))
        sub = joinpath(d, "store"); mkpath(joinpath(sub, "0"))
        write(joinpath(sub, "0", "chunk"), rand(UInt8, 8192))
        @test Cecelia._path_bytes(f) >= 4096                     # file: at least its bytes
        @test Cecelia._path_bytes(sub) >= 8192                   # dir: walked, not stat'd (a dir stat is ~4 KB)
        @test Cecelia._path_bytes(joinpath(d, "nope")) == 0      # absent, not an error
    end

    # pure policy: everything except the active version
    @test Set(Cecelia.reclaimable_versions(Dict{String,Any}(
        "default"=>"a", "afCorrected"=>"b", "driftCorrected"=>"c", "_active"=>"driftCorrected"))) ==
        Set(["default", "afCorrected"])
    @test isempty(Cecelia.reclaimable_versions(Dict{String,Any}(  # only the active version present
        "default"=>"a", "_active"=>"default")))
    # active is the original, but a leftover corrected variant is still freeable (NEW vs default-only)
    @test Cecelia.reclaimable_versions(Dict{String,Any}(
        "default"=>"a", "afCorrected"=>"b", "_active"=>"default")) == ["afCorrected"]

    _mk_ver!(img, fn) = (d = joinpath(img_zero_dir(img), fn); mkpath(d);
                         write(joinpath(d, "chunk"), rand(UInt8, 2048)); fn)

    proj = create_project!(name="stor-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")

    # imgA: original + af + drift, drift active → reclaim frees default AND af, keeps drift
    a = add_image!(s; name="a")
    _mk_ver!(a, "import.ome.zarr"); _mk_ver!(a, "af.ome.zarr"); _mk_ver!(a, "drift.ome.zarr")
    a.filepath = Dict("default"=>"import.ome.zarr", "afCorrected"=>"af.ome.zarr",
                      "driftCorrected"=>"drift.ome.zarr", "_active"=>"driftCorrected")
    a.im_channel_names = Dict{String,Union{Vector{String},String}}("default"=>["ch0","ch1"], "_active"=>"default")
    a.meta = Dict{String,Any}("SizeC"=>2, "SizeT"=>1, "SizeZ"=>5)
    a.status = IMAGE_DONE; save!(a)

    # imgB: original only, active default → nothing to reclaim
    b = add_image!(s; name="b")
    _mk_ver!(b, "import.ome.zarr")
    b.filepath = Dict("default"=>"import.ome.zarr", "_active"=>"default")
    b.status = IMAGE_DONE; save!(b)

    # safe-primary unit: removing default while other versions remain must NOT un-import
    freed, cleared = remove_image_version!(a, "default", "driftCorrected")
    @test freed > 0 && cleared == false
    # restore default for the batch reclaim below
    _mk_ver!(a, "import.ome.zarr")
    ra0 = init_object(proj.uid, a.uid); ra0.filepath["default"] = "import.ome.zarr"; save!(ra0)

    # reclaim_inactive! frees ALL non-active (default + af), keeps drift; imgB skipped
    tot, reclaimed = reclaim_inactive!(proj.uid, [a.uid, b.uid])
    @test reclaimed == [a.uid]
    @test tot > 0
    @test !isdir(joinpath(img_zero_dir(a), "import.ome.zarr"))    # original gone
    @test !isdir(joinpath(img_zero_dir(a), "af.ome.zarr"))        # intermediate gone
    @test  isdir(joinpath(img_zero_dir(a), "drift.ome.zarr"))     # active kept
    @test  isdir(joinpath(img_zero_dir(b), "import.ome.zarr"))    # b untouched

    ra = init_object(proj.uid, a.uid)
    @test ra.status == IMAGE_DONE                                     # NOT un-imported
    @test ra.filepath["_active"] == "driftCorrected"
    @test collect(keys(filter(kv -> kv.first != "_active", ra.filepath))) == ["driftCorrected"]
    @test ra.meta["SizeC"] == 2                                   # dims kept
    @test Cecelia.versioned_get(ra.im_channel_names, "default") == ["ch0","ch1"]  # channel names kept
    rm(proj.root; recursive=true)
end

# ── Analysis reset: drop everything derived, keep the image ────────────────────
# The other half of the delete story (docs/todo/IMAGE_DELETE_PLAN.md): `remove_image_version!` sheds
# STORES, `reset_image_analysis!` sheds NUMBERS, and neither may do the other's job. The keep-list is
# asserted by NAME rather than by count, so adding a new analysis dir to the image layout fails here
# until it is deliberately classified (Decision 7 — a delete-list would have leaked it silently).
@testset "Analysis reset keeps the image and drops the numbers" begin
    @test Cecelia.ANALYSIS_KEEP == Set(["ccid.json", "runlog.json", "gating"])

    proj = create_project!(name="reset-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="a")

    # an image store (must survive) …
    zdir = joinpath(img_zero_dir(img), "import.ome.zarr"); mkpath(zdir)
    write(joinpath(zdir, "chunk"), rand(UInt8, 2048))
    img.filepath = Dict("default"=>"import.ome.zarr", "_active"=>"default")
    img.im_channel_names = Dict{String,Union{Vector{String},String}}("default"=>["ch0"], "_active"=>"default")
    img.meta   = Dict{String,Any}("SizeC"=>1, "SizeT"=>1, "SizeZ"=>3)
    img.attr   = Dict{String,Any}("treatment"=>"CTRL")
    img.status = IMAGE_DONE
    img.labels = Dict("A"=>["A.zarr"])          # not versioned — a plain valueName → files map
    img.label_props = Dict("A"=>"A.h5ad")
    save!(img)

    # … and one file in every derived location, plus the things the keep-list protects
    for sub in ("labels", "labelProps", "populations", "stats", "mesh", "qc", "cl",
                "spatialGraph", "spatialStats", "branchLabels")
        mkpath(joinpath(img._dir, sub))
        write(joinpath(img._dir, sub, "x.bin"), rand(UInt8, 1024))
    end
    write(joinpath(img._dir, "runlog.json"), "[]")
    mkpath(joinpath(img._dir, "gating"))
    write(joinpath(img._dir, "gating", "A.json"), "{}")        # hand-drawn gates — must SURVIVE

    # the storage box's number IS what a reset would free — one accounting, so the box can't promise
    # bytes the reset doesn't deliver
    predicted = analysis_bytes_of(img)
    @test predicted > 0

    freed, dropped = reset_image_analysis!(img)

    @test freed == predicted
    @test analysis_bytes_of(img) == 0                              # nothing derived left to free
    @test freed > 0
    @test "labels" in dropped && "qc" in dropped && "spatialGraph" in dropped
    for sub in ("labels", "labelProps", "populations", "stats", "mesh", "qc", "cl",
                "spatialGraph", "spatialStats", "branchLabels")
        @test !ispath(joinpath(img._dir, sub))
    end
    # the keep-list survives, by name
    @test isfile(joinpath(img._dir, "ccid.json"))
    @test isfile(joinpath(img._dir, "runlog.json"))
    # gate polygons are user work, not output: a re-run under the same value_name reuses them
    @test isfile(joinpath(img._dir, "gating", "A.json"))
    @test !("ccid.json" in dropped) && !("runlog.json" in dropped) && !("gating" in dropped)

    # NO store is shed — that is remove_image_version!'s job (Decision 9)
    @test isdir(zdir)

    ri = init_object(proj.uid, img.uid)
    @test ri isa CciaImage
    @test Cecelia.versioned_get(ri.filepath, "default") == "import.ome.zarr"   # version untouched
    @test ri.filepath["_active"] == "default"
    @test ri.status == IMAGE_DONE                                                  # still imported
    @test ri.meta["SizeC"] == 1                                                # calibration/dims kept
    @test ri.attr["treatment"] == "CTRL"                                       # annotations kept
    # the analysis REGISTRATIONS are cleared, so nothing points at a deleted file
    @test isempty(ri.labels)
    @test isempty(ri.label_props)

    # idempotent: a second reset on an already-clean image is a no-op, not an error
    freed2, dropped2 = reset_image_analysis!(ri)
    @test freed2 == 0 && isempty(dropped2)

    rm(proj.root; recursive=true)
end

