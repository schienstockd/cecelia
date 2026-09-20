# Task form + preview + image geometry testsets — extracted from api/test/runtests.jl.
#
# Eight testsets covering the task-form / preview / geometry surface:
#  - `API: task definitions carry the previewable trait`
#  - `API: the served form resolves optionsFrom and defaultFrom`
#  - `API: task preview never guesses which image is open`
#  - `API: preview-labels slab resolves an UNREGISTERED vn`
#  - `API: preview stop sweeps the scratch labels store`
#  - `API: a built preview request is always sent`
#  - `API: the flow sheet renders a centred crop, not the whole frame`
#  - `API: image geometry (axis mapping + version resolution)`
#
# Two path expressions rewritten to use the shared API_TEST_DIR constant.
# Extracted so runtests.jl contains only include lines + section-header comments — same
# shape as app/test/suite/*.jl.

@testset "API: task definitions carry the previewable trait" begin
    # The trait is declared in Julia beside the task and STAMPED onto the spec here, rather than written
    # into the JSON (which is the param spec — a capability of the compute doesn't belong in it, and two
    # copies could disagree). The frontend reads this instead of sniffing the params for a
    # cellpose-shaped `models` bag.
    st, body = api_task_definitions(HTTP.Request("GET", "/api/tasks/definitions?category=segment"))
    @test st == 200
    specs = JSON3.read(body).segment
    by_fun = Dict(String(s.fun_name) => s for s in specs if haskey(s, :fun_name))

    # every spec gets the key, so the frontend never has to treat "absent" as a third state
    for s in specs
        haskey(s, :fun_name) && @test haskey(s, :previewable)
    end
    @test by_fun["segment.cellpose"].previewable == true
    # the composite the module page actually runs — the #421 trap
    @test by_fun["segment.cellposeMeasure"].previewable == true
    # and a task the worker can't run says so
    @test by_fun["segment.measureLabels"].previewable == false
end

@testset "API: the served form resolves optionsFrom and defaultFrom" begin
    # The picker the USER sees comes from this route, not from `_task_spec` — the route walks the spec
    # FILES (it must serve a category's forms whether or not every fun_name resolves to a registered
    # Julia task), so it never had a task instance to dispatch on and resolved only the
    # `_inject_dynamic_options!` hook. When the three model pickers moved from that hook to a
    # spec-declared `optionsFrom`, this route silently stopped filling them: the Optical Flow vault
    # manager listed every trained model and the segmentation form offered nothing but "None", both
    # reading the same `list_coastal_models`. `validate_params` resolved it fine, which is why the suite
    # was green — hence a test at the ROUTE, on the one thing the user can actually click.
    vault = Cecelia.coastal_models_dir()
    mkpath(vault)
    pt = joinpath(vault, "__api_defs_test_model__.pt")
    mf = joinpath(vault, "__api_defs_test_model__.json")
    try
        write(pt, "weights")
        write(mf, """{"channelName": "memTom"}""")

        # the walk the frontend does: `model` sits inside the repeatable `models` group
        function find_param(ps, key)
            for p in ps
                get(p, :key, "") == key && return p
                inner = get(p, :params, nothing)
                if inner !== nothing
                    hit = find_param(inner, key)
                    hit === nothing || return hit
                end
            end
            nothing
        end

        st, body = api_task_definitions(HTTP.Request("GET", "/api/tasks/definitions?category=segment"))
        @test st == 200
        specs = JSON3.read(body).segment
        coastal = only(s for s in specs if get(s, :fun_name, "") == "segment.coastal")
        opts = find_param(coastal.params, "model").options
        vals = [String(o.value) for o in opts]

        # the model is offerable at all — the whole bug
        @test "__api_defs_test_model__.pt" in vals
        # …and the label carries what it was trained on, so a user can tell whether it fits the image
        @test any(o -> String(o.value) == "__api_defs_test_model__.pt" &&
                       occursin("memTom", String(o.label)), opts)
        # the spec's own "None" survives and stays first: the vault is empty until you train something,
        # and that empty state must remain a legible choice rather than a select that rejects its default
        @test String(first(opts).value) == ""

        # picker == validator. The desync is the failure mode, not the empty list: `validate_params`
        # accepting a model the form cannot offer is how this stayed invisible.
        validated = Cecelia._task_spec(Cecelia._task_from_fun_name("segment.coastal"))
        vparams = JSON3.read(JSON3.write(validated)).params
        @test Set(String(o.value) for o in find_param(vparams, "model").options) == Set(vals)

        # the composite the module page actually runs pulls its params from the sub-spec, so it must see
        # the same resolved options — it is what the Segment page offers
        measure = only(s for s in specs if get(s, :fun_name, "") == "segment.coastalMeasure")
        @test "__api_defs_test_model__.pt" in
              [String(o.value) for o in find_param(measure.params, "model").options]

        # `defaultFrom` came in through the same door and had the same gap: the import form's store
        # layout must be the SETTING, not the literal in the JSON, or choosing zarr v3 in Settings and
        # importing from the form silently writes v2.
        #
        # Asserted against a setting FLIPPED to v3, not against `ngff_version()`. Comparing the served
        # default to the live setting looks like the tighter test and is in fact no test at all: the
        # spec literal is "0.4" and so is `NGFF_VERSION_DEFAULT`, so on any config that hasn't chosen
        # v3 — every CI run, since the suite isolates itself in an empty temp `CECELIA_DEV_DIR` — the
        # two sides agree whether or not the resolver ran. Only a setting that DISAGREES with the
        # literal can tell resolution from a passthrough.
        mktempdir() do tmp
            try
                withenv("CECELIA_DEV_DIR" => tmp) do
                    @test Cecelia.set_store_layout!("v3") == "v3"   # writes custom.toml + hot-reloads
                    @test Cecelia.ngff_version() == "0.5"           # the setting really did change
                    st2, body2 = api_task_definitions(
                        HTTP.Request("GET", "/api/tasks/definitions?category=importImages"))
                    @test st2 == 200
                    omezarr = only(s for s in JSON3.read(body2).importImages
                                   if get(s, :fun_name, "") == "importImages.omezarr")
                    # the form pre-fills v3 — and "0.4" here would be the spec literal winning
                    @test String(find_param(omezarr.params, "ngffVersion").default) == "0.5"
                end
            finally
                init_cecelia!()   # restore the real dev/prod config regardless of outcome
            end
        end
    finally
        rm(pt; force = true)
        rm(mf; force = true)
    end
end

@testset "API: task preview never guesses which image is open" begin
    # The property this whole route exists for. Under P7 the browser viewer is the source of truth: it
    # writes `useViewerStore().openImage` on the client side and body-carries `zarrPath`/`taskDir`/
    # `imageUid`/`region` into `/api/preview/run`. The API refuses when they're missing rather than
    # falling back to napari (deleted with the napari send calls in P7). Every branch is a REFUSAL,
    # because the alternative to refusing is acting on the wrong image.
    _region() = Dict("xy" => Dict("X" => [0, 32], "Y" => [0, 32]),
                     "z" => 0, "t" => 0, "ndisplay" => 2)
    # ── nothing body-carried → 409 no-viewer-open, and the status route reports null image fields
    #    (no server-side viewer tracking left after P9), so a transitional client can't read a
    #    plausible-looking default out of it
    st, body = api_preview_status(HTTP.Request("GET", "/api/preview/status"))
    @test st == 200
    d = JSON3.read(body)
    @test d.imageUid === nothing && d.zarrPath === nothing && d.taskDir === nothing
    @test d.port == 7656
    @test d.alive == false

    st, body = _post(api_preview_run, Dict("projectUid" => "p", "imageUid" => "i",
                                           "params" => Dict("models" => Dict()),
                                           "region" => _region()))
    @test st == 409
    @test JSON3.read(body).code == "no-viewer-open"

    # ── missing required fields are 400s, not silent defaults
    @test _post(api_preview_run, Dict("imageUid" => "i", "params" => Dict(),
                                      "region" => _region()))[1] == 400
    @test _post(api_preview_run, Dict("projectUid" => "p", "params" => Dict(),
                                      "region" => _region()))[1] == 400
    @test _post(api_preview_run, Dict("projectUid" => "p", "imageUid" => "i",
                                      "region" => _region()))[1] == 400
    # region itself is required — comes from the browser viewer body, not from a viewer WS callback
    st, body = _post(api_preview_run, Dict("projectUid" => "p", "imageUid" => "i",
                                           "params" => Dict("models" => Dict())))
    @test st == 400
    @test JSON3.read(body).code == "no-region"

    # ── the body's zarrPath vs the version the task would read → refuse and say which to open.
    #    Previewing anyway would either segment pixels the user can't see or pair the region with a
    #    differently-shaped store.
    mktempdir() do proj_root
        conf  = cecelia_conf()
        pdirs = get!(conf, "dirs", Dict{String,Any}())
        had   = haskey(pdirs, "projects"); prev = get(pdirs, "projects", nothing)
        pdirs["projects"] = proj_root
        try
            uid = "img9"
            meta = joinpath(proj_root, "p", "1", uid); mkpath(meta)
            write(joinpath(meta, "ccid.json"), JSON3.write(Dict(
                "uid" => uid,
                "filepath" => Dict("default" => "orig.ome.zarr",
                                   "corrected" => "drift.ome.zarr", "_active" => "default"))))
            open_zarr = joinpath(proj_root, "p", "0", uid, "orig.ome.zarr")

            st, body = _post(api_preview_run, Dict(
                "projectUid" => "p", "imageUid" => uid,
                "zarrPath" => open_zarr, "taskDir" => meta,
                "params" => Dict("valueName" => "corrected", "models" => Dict()),
                "region" => _region()))
            @test st == 409
            d = JSON3.read(body)
            @test d.code == "version-mismatch" && d.wantedValueName == "corrected"
            # The frontend renders `code` as the short amber label and this message as the tooltip
            # detail, so the message must carry the SPECIFICS rather than restate the problem. See
            # `previewNotice`/`ERROR_SHORT` in utils/taskPreview.ts.
            @test occursin("corrected", d.error) && occursin("orig.ome.zarr", d.error)

            # an unknown valueName is a 404, not a preview of the active version
            st, _ = _post(api_preview_run, Dict(
                "projectUid" => "p", "imageUid" => uid,
                "zarrPath" => open_zarr, "taskDir" => meta,
                "params" => Dict("valueName" => "nope", "models" => Dict()),
                "region" => _region()))
            @test st == 404
        finally
            had ? (pdirs["projects"] = prev) : delete!(pdirs, "projects")
        end
    end

    # ── a RUNNING segmentation puts the viewer on the staging store while ccid.json still resolves
    #    the final path. Same store mid-write, so this must NOT be a mismatch.
    @test _same_store("/a/b/X.ome.zarr", "/a/b/X.ome.zarr" * Cecelia.STORE_STAGING_SUFFIX)
    @test _same_store("/a/b/X.ome.zarr", "/a/b/X.ome.zarr")
    @test !_same_store("/a/b/X.ome.zarr", "/a/b/Y.ome.zarr")
end

@testset "API: preview-labels slab resolves an UNREGISTERED vn" begin
    # A first-time segmentation preview writes `<img_labels_dir>/<vn>__preview.ome.zarr` BEFORE any
    # ccid.json entry exists — registration only happens on a successful RUN. The `preview=1` slab
    # branch must therefore not go through `label_store_path`, which enforces `haskey(img.labels, vn)`
    # and would 404 the mask fetch for the exact case preview is most useful for. Regression:
    # `Viewer timepoint N: Mask failed: 404` on fXgbTl while previewing `flowKat` — a coastal-family
    # segmentation whose vn had never been registered on this image.
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "api-preview-slab-vn")
        s    = add_set!(proj; name = "s")
        img  = add_image!(s; name = "a")
        # labels registry is DELIBERATELY empty here — the preview lookup must not depend on it
        img.labels = Dict{String,Vector{String}}()
        save!(img)

        # Missing preview store on disk → err (the FE re-fetches; this is the "worker's promote hasn't
        # landed" race, and the finished-store path answers the same way).
        _, err = preview_labels_store_path(proj.uid, img.uid, "flowKat")
        @test err !== nothing && occursin("preview labels store not on disk", err)

        # Put the scratch store where the worker's convention says it goes — the labels DIR of the
        # image's meta dir. No ccid.json entry needed.
        labels_dir = joinpath(img._dir, "labels"); mkpath(labels_dir)
        scratch = joinpath(labels_dir, "flowKat__preview.ome.zarr"); mkpath(scratch)

        zp, err = preview_labels_store_path(proj.uid, img.uid, "flowKat")
        @test err === nothing
        @test zp == scratch

        # Belt-and-braces: `label_store_path` (the finished-store helper the non-preview branch uses)
        # still refuses this vn — the fix is that the preview branch takes a different route, not that
        # the registration guard was relaxed.
        _, lerr = label_store_path(proj.uid, img.uid, "flowKat")
        @test lerr !== nothing && occursin("no label store named", lerr)
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: preview stop sweeps the scratch labels store" begin
    # A preview writes `<vn>__preview.ome.zarr` under `{taskDir}/labels/`; a stop must clear it so
    # a subsequent session doesn't inherit stale bytes over the `preview=1` slab route. The worker
    # is not running here — the sweep is best-effort and the stop route must still succeed.
    mktempdir() do task_dir
        labels_dir = joinpath(task_dir, "labels")
        mkpath(labels_dir)
        stale = joinpath(labels_dir, "A__preview.ome.zarr")
        mkpath(stale); write(joinpath(stale, "junk"), "x")
        @test isdir(stale)

        st, body = _post(api_preview_stop, Dict("taskDir" => task_dir))
        @test st == 200
        d = JSON3.read(body)
        @test d.stopped == true && d.alive == false
        # the worker was down, so this is a no-op on the disk from the API side — the sweep only
        # runs when the worker is up (it owns the file handles that might still be holding it
        # open on Windows). Left as debris here; a later run wipes it on entry via `_stage_labels_store`.
    end
end

@testset "API: a built preview request is always sent" begin
    # `preview_request` BUILDS a request; `send(w, …)` runs it. Returning the request instead is a
    # 200 full of plausible-looking JSON — right imPath, right params, right funName — that simply
    # has no result in it, so the caller renders an empty panel and nothing anywhere reports an
    # error. `api_optical_flow_inspect` shipped exactly that: reviewed, type-checked, and dead.
    #
    # Building one without sending it has no other use, so the rule is total: every call site is an
    # argument to `send`.
    for file in filter(f -> endswith(f, ".jl"), readdir(joinpath(API_TEST_DIR, "..", "src"); join = true))
        src = read(file, String)
        for m in eachmatch(r"preview_request\(", src)
            # the enclosing call — `send(w, preview_request(…))` — sits just before it; allow for the
            # keyword-heavy wrapping the real call sites use
            before = src[max(1, m.offset - 240):m.offset]
            @test occursin("send(", before) ||
                  occursin("function preview_request", before)   # the definition itself
        end
    end
end

@testset "API: the flow sheet renders a centred crop, not the whole frame" begin
    # The panel used to send the FULL XY extent. On the one small image it was built against that was
    # ~1.5 MB; on a real 1044×1102 movie it is 36.3 MB in one websocket frame, and the reply died on the
    # 16 MiB frame cap with `1009: message too large` — see `FLOW_INSPECT_MAX_PX` for the measurement.
    #
    # Centred, because the corner of an intravital frame is routinely outside the specimen. These two
    # are the bounds the worker actually returned for that image at the default size.
    @test _centre_span(1044, 512) == (266, 778)
    @test _centre_span(1102, 512) == (295, 807)
    # ...and the span is exactly the cap, wherever it lands
    for (len, cap) in ((1044, 512), (1102, 512), (1023, 256), (1024, 768))
        lo, hi = _centre_span(len, cap)
        @test hi - lo == cap
        @test lo >= 0 && hi <= len
        # off-centre by at most the odd pixel
        @test abs(lo - (len - hi)) <= 1
    end

    # An image already smaller than the cap is shown whole — no crop, no padding. `fXgbTl` (418×434)
    # is that case, and it is the image the panel was developed against, so this must stay a no-op.
    @test _centre_span(418, 512) == (0, 418)
    @test _centre_span(434, 512) == (0, 434)
    @test _centre_span(512, 512) == (0, 512)

    # A missing/degenerate size means the whole axis rather than an empty region: an empty XY region is
    # rejected downstream ("empty preview region"), which would read as a broken panel.
    @test _centre_span(600, 0) == (0, 600)
    @test _centre_span(600, -1) == (0, 600)

    # The server's own fallback is what the panel's default mirrors (utils/flowRegion.ts).
    @test FLOW_INSPECT_MAX_PX == 512
    ts = read(joinpath(API_TEST_DIR, "..", "..", "frontend", "src", "utils", "flowRegion.ts"), String)
    @test occursin("DEFAULT_FLOW_REGION_PX = $(FLOW_INSPECT_MAX_PX)", ts)
end

@testset "API: image geometry (axis mapping + version resolution)" begin
    # Pure parts of image_geometry.jl — no zarr, no IO. These were `_crop_*` privates until a second
    # consumer showed none of it was crop-specific (docs: the anisotropy grid advisory).

    # Zarr.jl is column-major and presents the array REVERSED, so the C-order axis at position i sits
    # at Julia dim ndims-i+1. Getting this backwards silently swaps x and y — and a square frame
    # would hide it, so assert with a NON-square rank-5 layout.
    d = axis_dims(["t", "c", "z", "y", "x"], 5)
    @test d["t"] == 5 && d["c"] == 4 && d["z"] == 3 && d["y"] == 2 && d["x"] == 1

    d3 = axis_dims(["c", "y", "x"], 3)
    @test d3["x"] == 1 && d3["y"] == 2 && d3["c"] == 3

    # no axes in .zattrs → fall back to the conventional order for that rank, not an error
    @test axis_dims(String[], 5)["x"] == 1
    @test axis_dims(String[], 2)["y"] == 2
    @test !haskey(axis_dims(String[], 2), "z")

    # absent .zattrs → empty, and the caller falls back rather than throwing
    mktempdir() do dir
        @test read_ngff_axes(dir) == String[]
    end
    # malformed .zattrs must not take the request down
    mktempdir() do dir
        write(joinpath(dir, ".zattrs"), "{not json")
        @test read_ngff_axes(dir) == String[]
    end
    mktempdir() do dir
        write(joinpath(dir, ".zattrs"),
              """{"multiscales":[{"axes":[{"name":"T"},{"name":"Y"},{"name":"X"}]}]}""")
        @test read_ngff_axes(dir) == ["t", "y", "x"]     # lowercased
    end

    # version resolution reports WHY it failed instead of throwing — the route maps it to a status
    _, _, e1 = resolve_image_version("", "", nothing)
    @test e1 == "projectUid + imageUid required"
    _, _, e2 = resolve_image_version("no-such-project", "no-such-image", nothing)
    @test e2 == "Image not found"
end

