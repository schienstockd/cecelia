# Task log + history + attribute normalisation testsets — extracted from api/test/runtests.jl.
#
# Two testsets:
#  - `API: task log + history` — /api/tasks/log slicing per-run + history endpoint.
#  - `API: attribute normalisation on write` — image-attr write coerces types + strips
#    empties, so downstream _movie_basename never has to guard.
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: task log + history" begin
    # Redirect projects_dir() → a temp dir so we never touch the real dev projects dir.
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir()
    dirs["projects"] = tmp
    try
        proj = create_project!(name="api-tasklog")
        uid  = proj.uid
        s    = add_set!(proj; name="set-A")
        img1 = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
        img2 = add_image!(s; name="img-2", meta=Dict{String,Any}("ori_path"=>"/tmp/b.tif"))

        # ── image list (read-only, no lastOpenedAt bump) ──
        let r = JSON3.read(api_images_list(HTTP.Request("GET", "/api/images?projectUid=$uid"))[2])
            @test r.name == "api-tasklog" && r.count == 2
            @test length(r.sets) == 1 && r.sets[1].imageCount == 2
            names = [i.name for i in r.images]
            @test "img-1" in names && "img-2" in names
            @test all(i -> i.setName == "set-A", r.images)
            @test all(i -> i.included == true, r.images)          # included surfaced (default true)
        end
        # excluding an image surfaces as included:false (so the observer can see the silent member)
        img1.included = false; save!(img1)
        let r = JSON3.read(api_images_list(HTTP.Request("GET", "/api/images?projectUid=$uid"))[2])
            byname = Dict(i.name => i for i in r.images)
            @test byname["img-1"].included == false && byname["img-2"].included == true
        end
        img1.included = true; save!(img1)                          # restore for the rest of the testset
        # per-image attribute ASSIGNMENT — the observer needs it to size the groups of a cross-image
        # plot (the AXES come from /api/plots/attrs, which stays the one discovery route). An image
        # with no attributes must surface an empty map, not a missing key.
        let r = JSON3.read(api_images_list(HTTP.Request("GET", "/api/images?projectUid=$uid"))[2])
            @test all(i -> isempty(i.attr), r.images)
        end
        img1.attr = Dict{String,Any}("Mouse" => "3", "Location" => "b"); save!(img1)
        let r = JSON3.read(api_images_list(HTTP.Request("GET", "/api/images?projectUid=$uid"))[2])
            byname = Dict(i.name => i for i in r.images)
            @test byname["img-1"].attr.Mouse == "3" && byname["img-1"].attr.Location == "b"
            @test isempty(byname["img-2"].attr)
        end
        @test api_images_list(HTTP.Request("GET", "/api/images"))[1] == 400          # projectUid missing
        @test api_images_list(HTTP.Request("GET", "/api/images?projectUid=nope"))[1] == 404

        # ── image metadata payload: original file location (oriPath) + filtered extraMeta ──
        # The image-info dialog needs the source file path (kept in meta as ori_path) and a generic
        # bucket for any other scalar meta — but NOT keys already surfaced as fields, nor internal
        # bookkeeping (funParams) or nested display config (channel_colormaps).
        img2.meta["SizeC"]            = 3                              # → sizeC field, must NOT double into extraMeta
        img2.meta["Objective"]        = "40x/1.3"                      # arbitrary scalar → surfaced generically
        img2.meta["funParams"]        = Dict{String,Any}("x" => 1)    # internal nested dict → excluded
        img2.meta["channel_colormaps"] = ["red", "green"]             # nested/display → excluded
        save!(img2)
        let r = JSON3.read(api_images_meta(HTTP.Request("GET", "/api/images/meta?projectUid=$uid&imageUid=$(img2.uid)"))[2])
            @test r.image.oriPath == "/tmp/b.tif"
            @test r.image.sizeC == 3
            @test r.image.extraMeta.Objective == "40x/1.3"
            @test !haskey(r.image.extraMeta, :SizeC)              # already a first-class field
            @test !haskey(r.image.extraMeta, :ori_path)           # surfaced as oriPath
            @test !haskey(r.image.extraMeta, :funParams)          # internal nested dict
            @test !haskey(r.image.extraMeta, :channel_colormaps)  # nested display config
        end
        @test api_images_meta(HTTP.Request("GET", "/api/images/meta?projectUid=$uid"))[1] == 400           # imageUid missing
        @test api_images_meta(HTTP.Request("GET", "/api/images/meta?projectUid=$uid&imageUid=nope"))[1] == 404

        # ── task log ──
        tl(q) = api_images_tasklog(HTTP.Request("GET", "/api/images/tasklog?$q"))
        # no log yet → exists=false, empty content
        let r = JSON3.read(tl("projectUid=$uid&imageUid=$(img1.uid)&fun=segment.cellpose")[2])
            @test r.exists == false && r.content == ""
        end
        # write a log the way the scheduler's _wrap_log_with_file would, then read it back
        logdir = joinpath(img1._dir, "logs"); mkpath(logdir)
        write(joinpath(logdir, "segment.cellpose.log"), "[2026-07-15 10:00:00] running cellpose\n")
        let r = JSON3.read(tl("projectUid=$uid&imageUid=$(img1.uid)&fun=segment.cellpose")[2])
            @test r.exists == true && occursin("running cellpose", r.content)
        end
        # bad requests + path-traversal guard (%2F decodes to '/', so fun becomes "../secret")
        @test tl("")[1] == 400                                             # projectUid missing
        @test tl("projectUid=$uid&imageUid=$(img1.uid)")[1] == 400         # fun missing
        @test tl("projectUid=$uid&imageUid=$(img1.uid)&fun=..%2Fsecret")[1] == 400   # traversal blocked
        @test tl("projectUid=$uid&imageUid=nope&fun=x")[1] == 404          # image missing
        @test tl("projectUid=nope&imageUid=$(img1.uid)&fun=x")[1] == 404   # project missing

        # ── task history ──
        hist(q) = api_tasks_history(HTTP.Request("GET", "/api/tasks/history?$q"))
        # empty when no run-log activity
        let r = JSON3.read(hist("projectUid=$uid")[2])
            @test r.count == 0 && length(r.history) == 0
        end
        # activity across two images, aggregated — including a FAILED run (visible to the observer)
        append_run_log!(img1, "segment.cellpose", "default")
        append_run_log!(img2, "tracking.bayesian_tracking", "default", "failed",
                        Dict{String,Any}("maxSearchRadius" => 20, "maxLost" => 3))
        let r = JSON3.read(hist("projectUid=$uid")[2])
            @test r.count == 2
            funs = [h.fun for h in r.history]
            @test "segment.cellpose" in funs && "tracking.bayesian_tracking" in funs
            @test all(h -> h.imageUid in (img1.uid, img2.uid), r.history)
            # per-run outcome surfaced under runStatus (distinct from the image's `status`)
            byfun = Dict(String(h.fun) => h for h in r.history)
            @test String(byfun["segment.cellpose"].runStatus) == "done"          # default
            @test String(byfun["tracking.bayesian_tracking"].runStatus) == "failed"
            # the tuning trail rides along per row (Observer Phase 2 §1): the run's params
            @test byfun["tracking.bayesian_tracking"].params.maxSearchRadius == 20
            @test isempty(byfun["segment.cellpose"].params)                      # no params → {}
        end
        # limit caps rows
        @test JSON3.read(hist("projectUid=$uid&limit=1")[2]).count == 1
        # bad requests
        @test hist("")[1] == 400                     # projectUid missing
        @test hist("projectUid=nope")[1] == 404
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive=true, force=true)
    end
end

@testset "API: attribute normalisation on write" begin
    # The attr routes are the only place user-typed names/values enter the model, so they trim there.
    # Untrimmed, these pairs would each be TWO distinct values — two filter chips in the image table,
    # two segments in a generated movie name — or two separate attribute columns.
    @test _norm_attr("a") == "a"
    @test _norm_attr(" a ") == "a"
    @test _norm_attr("\tLocation\n") == "Location"
    @test _norm_attr(" a ") == _norm_attr("a")

    # Whitespace-only collapses to "" — the canonical UNSET that attr/create seeds a column with.
    # It must stay a value, not become a deletion: the key's presence is what makes the column exist.
    @test _norm_attr("") == ""
    @test _norm_attr("   ") == ""

    # interior whitespace is content, not padding
    @test _norm_attr(" day 3 ") == "day 3"

    # and the reason it matters downstream: a blank value is already dropped from movie names, so
    # normalising at the write is what keeps that defence from being needed in every consumer.
    @test _movie_basename(Dict("T" => _norm_attr("  ")), "u1", ["T"]) == "u1.mp4"
end

