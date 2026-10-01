# ── Pilot writer for VN versioning: ingest path (`filepath` field) ──
# Covers the shared writer helpers (`plan_versioned_target`, `versioned_filepath_write!`), the
# `as_new_version` branch of `_merge_zarr_meta_into_ccid!` (ccid.json write routing), and the
# `keep_previous_version()` / `set_keep_previous_version!` config helpers. Design:
# `docs/audit/vn-versioning-p4-design.md`.
#
# Runs hermetic — `runtests.jl` points `CECELIA_DEV_DIR` at a throwaway dir, so the config writes
# below don't touch the developer's real settings.

using Test
using Cecelia
using Cecelia: img_zero_dir, LATEST_ACTIVE_KEY, LATEST_DEFAULT_VAL, VERSIONED_ACTIVE_KEY,
               VERSIONED_DEFAULT_VAL

@testset "keep_previous_version toggle roundtrip" begin
    prior = keep_previous_version()   # restore afterward — hermetic dir, still be tidy
    try
        set_keep_previous_version!(true)
        @test keep_previous_version() === true
        set_keep_previous_version!(false)
        @test keep_previous_version() === false
        # default is off — a fresh install with an empty custom.toml gives false, so an untoggled
        # user gets the same overwrite semantics they had before this landed.
        @test Cecelia.KEEP_PREVIOUS_VERSION_DEFAULT === false
    finally
        set_keep_previous_version!(prior)
    end
end

@testset "plan_versioned_target — flat vs versioned" begin
    prior_toggle = keep_previous_version()
    try
        proj = create_project!(name = "pilot-plan-$(rand(1000:9999))")
        s    = add_set!(proj; name = "set")
        img  = add_image!(s; name = "img")

        # Fresh image, toggle OFF → flat legacy path, not a version-append.
        set_keep_previous_version!(false)
        abs, rel, as_new = plan_versioned_target(img, "default", "ccidImage.ome.zarr")
        @test abs == joinpath(img_zero_dir(img), "ccidImage.ome.zarr")
        @test rel == "ccidImage.ome.zarr"
        @test as_new === false

        # Fresh image, toggle ON → still flat. First-ever write always lands at the legacy path
        # (design: v1 stays flat for backward-compat; only v2+ nest under the vN subdir).
        set_keep_previous_version!(true)
        abs, rel, as_new = plan_versioned_target(img, "default", "ccidImage.ome.zarr")
        @test abs == joinpath(img_zero_dir(img), "ccidImage.ome.zarr")
        @test rel == "ccidImage.ome.zarr"
        @test as_new === false

        # Prior legacy scalar (a v1 already exists on disk), toggle OFF → still overwrite, still flat.
        img.filepath["default"] = "ccidImage.ome.zarr"
        img.filepath[VERSIONED_ACTIVE_KEY] = "default"
        set_keep_previous_version!(false)
        abs, rel, as_new = plan_versioned_target(img, "default", "ccidImage.ome.zarr")
        @test abs == joinpath(img_zero_dir(img), "ccidImage.ome.zarr")
        @test as_new === false

        # Prior legacy scalar, toggle ON → next `v2` under a versioned subdir.
        set_keep_previous_version!(true)
        abs, rel, as_new = plan_versioned_target(img, "default", "ccidImage.ome.zarr")
        @test abs == joinpath(img_zero_dir(img), "default", "v2", "ccidImage.ome.zarr")
        @test rel == joinpath("default", "v2", "ccidImage.ome.zarr")
        @test as_new === true

        # Prior versioned entry {v1, v2, _latest: v2}, toggle ON → next `v3`.
        img.filepath["default"] = Dict{String,Any}(
            "v1" => "ccidImage.ome.zarr",
            "v2" => "default/v2/ccidImage.ome.zarr",
            LATEST_ACTIVE_KEY => "v2",
        )
        abs, rel, as_new = plan_versioned_target(img, "default", "ccidImage.ome.zarr")
        @test abs == joinpath(img_zero_dir(img), "default", "v3", "ccidImage.ome.zarr")
        @test rel == joinpath("default", "v3", "ccidImage.ome.zarr")
        @test as_new === true

        # A non-active value_name (`corrected`) has its own version series independent of `default`.
        # Filename is task-specific so the returned path carries whatever the caller passes in.
        img.filepath["corrected"] = "ccidDriftCorrected.ome.zarr"
        abs, rel, as_new = plan_versioned_target(img, "corrected", "ccidDriftCorrected.ome.zarr")
        @test abs == joinpath(img_zero_dir(img), "corrected", "v2", "ccidDriftCorrected.ome.zarr")
        @test rel == joinpath("corrected", "v2", "ccidDriftCorrected.ome.zarr")
        @test as_new === true

        rm(proj.root; recursive = true)
    finally
        set_keep_previous_version!(prior_toggle)
    end
end

@testset "versioned_filepath_write! — direct API" begin
    proj = create_project!(name = "pilot-writer-$(rand(1000:9999))")
    s    = add_set!(proj; name = "set")
    img  = add_image!(s; name = "img")

    # Fresh raw dict, `as_new_version=false` → legacy scalar written under value_name.
    ccid = Cecelia.state_file(img)
    Cecelia.commit_state!(img) do raw
        versioned_filepath_write!(raw, "default", "ccidImage.ome.zarr"; as_new_version = false)
    end
    r = init_object(proj.uid, img.uid)
    @test r.filepath["default"] == "ccidImage.ome.zarr"

    # Second call with `as_new_version=true` upgrades and appends v2.
    Cecelia.commit_state!(r) do raw
        versioned_filepath_write!(raw, "default", "default/v2/ccidImage.ome.zarr";
                                    as_new_version = true)
    end
    r2 = init_object(proj.uid, img.uid)
    entry = r2.filepath["default"]
    @test entry isa AbstractDict
    @test entry["v1"] == "ccidImage.ome.zarr"
    @test entry["v2"] == "default/v2/ccidImage.ome.zarr"
    @test entry[LATEST_ACTIVE_KEY] == "v2"

    rm(proj.root; recursive = true)
end

# The segmentation writers (`register_label_files!`, measureLabels) used to rebuild the whole field
# with `string(v)` / `[string(v)]`, flattening any versioned entry they passed over into a string.
@testset "versioned_entry_overwrite! — keeps every entry's shape" begin
    v_lp  = Dict{String,Any}("v1" => "a.h5ad", "v2" => "a/v2/a.h5ad", LATEST_ACTIVE_KEY => "v2")
    raw   = Dict{String,Any}("label_props" => Dict{String,Any}(
                "default" => "default.h5ad", "a" => v_lp, VERSIONED_ACTIVE_KEY => "default"))

    # another value_name: the versioned entry and `_active` are left alone
    versioned_entry_overwrite!(raw, "label_props", "b", "b.h5ad")
    lp = raw["label_props"]
    @test lp["b"] == "b.h5ad"
    @test lp["a"] == v_lp
    @test lp["default"] == "default.h5ad"
    @test lp[VERSIONED_ACTIVE_KEY] == "default"

    # a versioned target: only its `_latest` leaf is replaced, earlier versions survive
    versioned_entry_overwrite!(raw, "label_props", "a", "a2.h5ad"; set_active = true)
    lp = raw["label_props"]
    @test lp["a"]["v1"] == "a.h5ad"
    @test lp["a"]["v2"] == "a2.h5ad"
    @test lp["a"][LATEST_ACTIVE_KEY] == "v2"
    @test lp[VERSIONED_ACTIVE_KEY] == "a"

    # absent field, and a bare legacy scalar field
    raw2 = Dict{String,Any}()
    versioned_entry_overwrite!(raw2, "labels", "default", ["default.zarr"])
    @test raw2["labels"] == Dict{String,Any}("default" => ["default.zarr"])
    raw3 = Dict{String,Any}("label_props" => "old.h5ad")
    versioned_entry_overwrite!(raw3, "label_props", "new", "new.h5ad")
    @test raw3["label_props"]["default"] == "old.h5ad"
    @test raw3["label_props"]["new"] == "new.h5ad"
    @test raw3["label_props"][VERSIONED_ACTIVE_KEY] == VERSIONED_DEFAULT_VAL

    # end to end through ccid.json: JSON3-parsed nested entries are normalised, not stringified
    proj = create_project!(name = "entry-overwrite-$(rand(1000:9999))")
    s    = add_set!(proj; name = "set")
    img  = add_image!(s; name = "img")
    Cecelia.commit_state!(img) do raw
        raw["labels"] = Dict{String,Any}("default" => Dict{String,Any}(
            "v1" => ["default.zarr"], "v2" => ["default/v2/default.zarr"], LATEST_ACTIVE_KEY => "v2"))
    end
    Cecelia.register_label_files!(img, "fresh", ["fresh.zarr"])
    r = init_object(proj.uid, img.uid)
    @test r.labels["fresh"] == ["fresh.zarr"]
    @test r.labels["default"] isa AbstractDict
    @test r.labels["default"]["v1"] == ["default.zarr"]
    @test r.labels["default"]["v2"] == ["default/v2/default.zarr"]

    # the measureLabels / SegmentCorrect readers unwrap the raw (JSON3) entry to its `_latest` leaf
    raw_labels = Cecelia.read_ccid_raw(Cecelia.state_file(img))["labels"]
    entry = get(raw_labels, "default", get(raw_labels, :default, nothing))
    @test collect(String, Cecelia.unversion_value(entry)) == ["default/v2/default.zarr"]

    # sizing / deleting a value_name walks every version's files, not just `_latest`
    @test [collect(String, l) for l in Cecelia.version_leaves(entry)] |> sort ==
          [["default.zarr"], ["default/v2/default.zarr"]]
    @test Cecelia.version_leaves(["fresh.zarr"]) == Any[["fresh.zarr"]]

    rm(proj.root; recursive = true)
end

@testset "_merge_zarr_meta_into_ccid! — as_new_version routing" begin
    prior_toggle = keep_previous_version()
    try
        set_keep_previous_version!(false)   # helper reads it — irrelevant here since we pass explicitly
        proj = create_project!(name = "pilot-merge-$(rand(1000:9999))")
        s    = add_set!(proj; name = "set")
        img  = add_image!(s; name = "img")

        # Fresh, `as_new_version=false` → legacy scalar (current behaviour). Reader-side unchanged.
        Cecelia._merge_zarr_meta_into_ccid!(img, Dict{String,Any}("SizeC" => 3);
            zarr_filename = "ccidImage.ome.zarr",
            value_name    = "default",
            as_new_version = false)
        r = init_object(proj.uid, img.uid)
        @test r.filepath["default"] == "ccidImage.ome.zarr"
        @test !(r.filepath["default"] isa AbstractDict)

        # A second write with `as_new_version=true`: wraps the scalar as v1 and appends v2.
        Cecelia._merge_zarr_meta_into_ccid!(r, Dict{String,Any}();
            zarr_filename = "default/v2/ccidImage.ome.zarr",
            value_name    = "default",
            as_new_version = true)
        r2 = init_object(proj.uid, img.uid)
        entry = r2.filepath["default"]
        @test entry isa AbstractDict
        @test entry["v1"] == "ccidImage.ome.zarr"                    # legacy scalar preserved as v1
        @test entry["v2"] == "default/v2/ccidImage.ome.zarr"         # new version at the subdir path
        @test entry[LATEST_ACTIVE_KEY] == "v2"

        # A third write with `as_new_version=true`: appends v3, moves `_latest` again.
        Cecelia._merge_zarr_meta_into_ccid!(r2, Dict{String,Any}();
            zarr_filename = "default/v3/ccidImage.ome.zarr",
            value_name    = "default",
            as_new_version = true)
        r3 = init_object(proj.uid, img.uid)
        entry3 = r3.filepath["default"]
        @test entry3["v1"] == "ccidImage.ome.zarr"
        @test entry3["v2"] == "default/v2/ccidImage.ome.zarr"
        @test entry3["v3"] == "default/v3/ccidImage.ome.zarr"
        @test entry3[LATEST_ACTIVE_KEY] == "v3"

        # Bucket A reader keeps working across shape changes — `img_filepath` unwraps versioned
        # entries via `unversion_value` (P1b), so the leaf reads without touching a caller.
        @test img_filepath(r3) == joinpath(img_zero_dir(r3), "default/v3/ccidImage.ome.zarr")

        # Attempting a version-append on a fresh (never-imported) value_name is a caller bug —
        # `plan_versioned_target` guarantees prior existence before flipping the flag. The
        # defensive `error(...)` inside `versioned_filepath_write!` is swallowed by
        # `_merge_zarr_meta_into_ccid!`'s outer `try` (metadata merges log-and-move-on by design),
        # so observe the side effect: nothing is created for the fresh value_name, and no partial
        # versioned shell leaks.
        proj2 = create_project!(name = "pilot-noprior-$(rand(1000:9999))")
        s2   = add_set!(proj2; name = "set")
        img2 = add_image!(s2; name = "img")
        Cecelia._merge_zarr_meta_into_ccid!(img2, Dict{String,Any}();
            zarr_filename = "default/v1/ccidImage.ome.zarr",
            value_name    = "default",
            as_new_version = true)
        r_fresh = init_object(proj2.uid, img2.uid)
        @test !haskey(r_fresh.filepath, "default")
        @test length(Cecelia.versioned_keys(r_fresh.filepath)) == 0   # nothing partial

        rm(proj.root;  recursive = true)
        rm(proj2.root; recursive = true)
    finally
        set_keep_previous_version!(prior_toggle)
    end
end

@testset "P3 pinning: versioned_get_field_at follows _latest OR a pinned vN" begin
    # The one reader helper task handlers use. `_latest` is the default follow-mode; passing
    # `version = "v1"` (or any concrete vN) pins the read regardless of what `_latest` points at.
    # This is what chain nodes exploit via `params["version"]` — the executor threads that param
    # into `versioned_get_field_at` inside each converted task's `_run_task`.
    raw = Dict{String,Any}(
        "filepath" => Dict{String,Any}(
            "default" => Dict{String,Any}(
                "v1"                => "old.zarr",
                "v2"                => "new.zarr",
                LATEST_ACTIVE_KEY   => "v2"),
            VERSIONED_ACTIVE_KEY => "default"))

    @test versioned_get_field_at(raw, "filepath", "default") == "new.zarr"                    # follows _latest
    @test versioned_get_field_at(raw, "filepath", "default"; version = nothing) == "new.zarr" # nothing == follow
    @test versioned_get_field_at(raw, "filepath", "default"; version = "v1") == "old.zarr"    # pin
    @test versioned_get_field_at(raw, "filepath", "default"; version = "v2") == "new.zarr"    # explicit latest
    @test isnothing(versioned_get_field_at(raw, "filepath", "default"; version = "v99"))       # missing → nothing

    # Legacy scalar entry stays a no-op regardless of the version kwarg — old projects don't have
    # a versioned entry to pin against, and every non-versioned reader has always followed the
    # scalar. This is what made the switch from `versioned_get_field` to `versioned_get_field_at`
    # safe: every task's reader accepts a version, but legacy entries silently ignore it.
    legacy = Dict{String,Any}(
        "filepath" => Dict{String,Any}(
            "default"             => "ccidImage.ome.zarr",
            VERSIONED_ACTIVE_KEY => "default"))
    @test versioned_get_field_at(legacy, "filepath", "default") == "ccidImage.ome.zarr"
    @test versioned_get_field_at(legacy, "filepath", "default"; version = "v2") == "ccidImage.ome.zarr"
end

@testset "P3 pinning ratchet: every task reader passes params.version through" begin
    # A grep-shaped ratchet — every task's `versioned_get_field_at(raw, "filepath", …)` MUST also
    # thread the chain node's `params["version"]` so pinning reaches the reader. If a new task
    # ships and forgets the kwarg, chain pinning silently no-ops for it. The invariant is simply
    # that the same line carries `version =` in the composer's kwargs; the value can be either
    # form (identical run-time semantics):
    #   • untyped:  `version = get(params, "version", nothing)`   ← the pre-#34 shape (baseline)
    #   • typed:    `version = p.version`                         ← post-#34 typed-params cleanup
    # Both keep chain pinning wired end-to-end. See `parse_version_pin` in `helpers.jl` for the
    # typed shape.
    task_root = joinpath(@__DIR__, "..", "..", "src", "tasks")
    all_reads = 0
    pin_reads = 0
    for (dir, _, files) in walkdir(task_root), f in files
        endswith(f, ".jl") || continue
        for line in eachline(joinpath(dir, f))
            occursin("versioned_get_field_at(raw, \"filepath\"", line) || continue
            all_reads += 1
            occursin("version =", line) && (pin_reads += 1)
        end
    end
    @test all_reads > 0                # sanity: the scan finds something
    @test pin_reads == all_reads       # every reader threads a version — no drift
end
