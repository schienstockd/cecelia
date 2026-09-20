# Viewer overlays + label stores + meta + props testsets — extracted from api/test/runtests.jl.
#
# Five testsets covering the viewer payload contract:
#  - `API: viewer overlays (one request for the whole movie, in µm)` — P3 payload contract.
#  - `API: viewer label stores (P4 — masks through the same reader)`.
#  - `API: viewer meta names the versions and which one it resolved`.
#  - `API: viewer overlays on an image with no cell table`.
#  - `API: viewer props round-trip (save then load)`.
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: viewer overlays (one request for the whole movie, in µm)" begin
    # P3's payload contract. What can go wrong here is silent: a coordinate in pixels instead of µm
    # lands the overlay in the corner of the image at 1/3 scale and still LOOKS like data, and a
    # `null` in a coordinate array becomes 0 through `Float32Array.from` — a cell drawn at the origin
    # rather than not drawn.
    h5 = api_fixture("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !api_have_fixture(h5)
        @test_skip "labelProps fixture missing"
    else
        dir = mktempdir()
        proj = joinpath(dir, "testpr")
        cp(api_fixture("testpr"), proj)
        old = Cecelia.cecelia_conf()["dirs"]["projects"]
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dir
            ask(qs) = JSON3.read(api_viewer_overlays(HTTP.Request("GET",
                "/api/viewer/overlays?projectUid=testpr&imageUid=KDIeEm&valueName=B" * qs))[2])
            st, body = api_viewer_overlays(HTTP.Request("GET",
                "/api/viewer/overlays?projectUid=testpr&imageUid=KDIeEm&valueName=B"))
            @test st == 200
            d = JSON3.read(body)

            # ── the table ────────────────────────────────────────────────────────────
            @test d.nCells > 0
            # Every coordinate finite, always. JSON has no NaN literal (JSON3 refuses to write one) and
            # `null` becomes 0 through `Float32Array.from`, so an undrawable cell is DROPPED rather than
            # encoded — and `nDropped` says how many, since shipping fewer cells than the table holds
            # would otherwise read as a segmentation problem.
            @test d.nDropped >= 0
            @test d.nCells + d.nDropped == length(JSON3.read(String(JSON3.write(d.cells.label))))  ||
                  d.nDropped == 0
            for a in ("x", "y", "z", "t")
                @test all(isfinite, Float64.(getproperty(d.cells, Symbol(a))))
            end
            @test length(d.cells.label) == d.nCells
            @test length(d.cells.x) == d.nCells && length(d.cells.y) == d.nCells
            @test Set(String.(d.axes)) ⊆ Set(["x", "y", "z"])
            @test "x" in d.axes && "y" in d.axes
            # every declared axis actually carries values, and every absent one is empty — the client
            # reads these arrays positionally, so a declared-but-missing axis is a wrong picture
            for a in ("x", "y", "z")
                col = getproperty(d.cells, Symbol(a))
                @test (a in d.axes) == (length(col) == d.nCells)
            end

            # ── µm, not pixels ───────────────────────────────────────────────────────
            # The route promises the same space as `extentUm`. Compare against the raw file: with a
            # real calibration the two MUST differ, and by exactly the axis resolution.
            img, _ = _gating_image("testpr", "KDIeEm")
            sizes, _ = img_physical_sizes(img)              # [sz, sy, sx] µm/px
            lp = label_props(joinpath(proj, "1", "KDIeEm", "labelProps", "B.h5ad"))
            view_centroid_cols(lp; order = [:x, :y, :z])
            raw = as_df(lp)
            if sizes[3] != 1.0                             # x resolution is a real measurement
                @test !(Float64(raw[1, :centroid_x]) ≈ Float64(d.cells.x[1]))
            end
            @test Float64(raw[1, :centroid_x]) * sizes[3] ≈ Float64(d.cells.x[1])
            @test Float64(raw[1, :centroid_y]) * sizes[2] ≈ Float64(d.cells.y[1])
            # t stays a FRAME index — scaling it would silently redefine every frame-counted
            # parameter, the same choice `scale_centroids!` makes on disk.
            if d.hasT
                @test Float64(raw[1, :centroid_t]) ≈ Float64(d.cells.t[1])
            end

            # ── tracks ───────────────────────────────────────────────────────────────
            # -1 for "not tracked", never 0 and never null: one sentinel the client tests against.
            if !isempty(d.cells.track)
                @test length(d.cells.track) == d.nCells
                @test all(t -> t == -1 || t > 0, d.cells.track)
                @test any(t -> t > 0, d.cells.track)        # the fixture IS tracked
            end

            # ── colour-by ────────────────────────────────────────────────────────────
            @test d.colourBy === nothing && d.values === nothing
            if !isempty(d.colourColumns)
                c = String(first(d.colourColumns))
                got = ask("&colourBy=" * HTTP.escapeuri(c))
                @test got.colourBy == c
                @test got.values !== nothing && length(got.values) == got.nCells
                # WHICH KIND of scale is the server's answer, through the same `_is_categorical_col`
                # rule the plots use — so a column that plots as a code set shades as one in the
                # viewer. Re-deriving it in TypeScript would be a second answer about one column.
                @test String(got.valueKind) in ("categorical", "numeric")
                if got.valueKind == "numeric"
                    @test got.valueRange !== nothing && length(got.valueRange) == 2
                    @test got.valueRange[1] <= got.valueRange[2]
                    @test got.valueLevels === nothing
                else
                    @test got.valueLevels !== nothing && !isempty(got.valueLevels)
                    @test got.valueRange === nothing
                    # the levels must COVER the values, else the client greys a cell it can colour
                    lv = Set(string.(got.valueLevels))
                    @test all(v -> v === nothing || string(v) in lv, got.values)
                end
                # every column the route offers must answer both questions — a column that came back
                # with no kind would silently fall through to the population colour
                for col in got.colourColumns
                    one = ask("&colourBy=" * HTTP.escapeuri(String(col)))
                    @test String(one.valueKind) in ("categorical", "numeric")
                end
            end
            # no colour-by → no kind, no levels, no range: three fields that must not linger
            @test get(d, :valueKind, nothing) === nothing
            @test get(d, :valueLevels, nothing) === nothing
            @test get(d, :valueRange, nothing) === nothing
            # an unknown column is ignored rather than fatal — a stale column name from a saved view
            # must not take the overlay down with it
            bad = ask("&colourBy=does_not_exist")
            @test bad.colourBy === nothing && bad.values === nothing && bad.nCells == d.nCells

            # ── populations ──────────────────────────────────────────────────────────
            # Membership comes from `resolve_pops`, so an ungated image answers an empty list. Never an
            # error: unsegmented and ungated are normal states for an image, not failures.
            @test d.pops isa JSON3.Array
            for p in d.pops
                @test !isempty(String(p.path)) && !isempty(String(p.colour))
                @test all(l -> l isa Integer, p.labels)
            end
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end

@testset "API: viewer label stores (P4 — masks through the same reader)" begin
    # A mask is another zarr of the same geometry, which is what makes P4 cheap: the same `read_slab`,
    # the same headers, the same shape guard. What must NOT be re-derived is where a store lives —
    # `img_labels_path` is the image-owned accessor the tasks write through, so resolving a path by hand
    # here would drift the day a filename convention changes.
    dirs = Cecelia.cecelia_conf()["dirs"]
    old  = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        proj = create_project!(name = "api-viewer-labels")
        img  = add_image!(add_set!(proj; name = "s"); name = "a")
        mkpath(joinpath(img._dir, "labels"))
        img.labels = Dict("seg" => ["seg.zarr"], "ghost" => ["ghost.zarr"])
        save!(img)

        # registered AND on disk → resolves
        mkpath(joinpath(img._dir, "labels", "seg.zarr"))
        p, e = label_store_path(proj.uid, img.uid, "seg")
        @test e === nothing
        @test p == joinpath(img._dir, "labels", "seg.zarr")

        # registered but NOT on disk → a message, not a path. `labels` and `label_props` are
        # independent registries and a store can be registered before it is written.
        p2, e2 = label_store_path(proj.uid, img.uid, "ghost")
        @test p2 === nothing && occursin("not on disk", e2)

        # never registered, and no image at all — both normal states, both a message
        @test label_store_path(proj.uid, img.uid, "nope")[2] == "no label store named 'nope'"
        @test label_store_path(proj.uid, img.uid, "")[2] == "no label store named ''"
        @test label_store_path(proj.uid, "NOSUCH", "seg")[2] == "image not found"
    finally
        old === nothing ? delete!(dirs, "projects") : (dirs["projects"] = old)
    end
end

@testset "API: viewer meta names the versions and which one it resolved" begin
    # The viewer window is a pop-out with no project open, so it cannot look up either the list of
    # image versions or which one it is showing. Without the SECOND field a version picker opens on an
    # empty box and the first change is a no-op; without the first there is nothing to pick from.
    #
    # "Active" here must be the ccid's `_active` — the version a task would run against — and NOT
    # "default", which is merely one of the names. The two differ on every image that has been through
    # a correction step, which is most of them.
    dirs = Cecelia.cecelia_conf()["dirs"]
    old  = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        proj = create_project!(name = "api-viewer-meta")
        img  = add_image!(add_set!(proj; name = "s"); name = "a")
        img.filepath = Dict("default"    => "ccidImage.ome.zarr",
                            "smoothed"   => "ccidSmoothed.ome.zarr",
                            "_active"    => "smoothed")
        save!(img)

        # A minimal (t,c,z,y,x) store per version, so `open_level0` has something real to measure.
        proj_dir = dirname(dirname(img._dir))
        for fn in ["ccidImage.ome.zarr", "ccidSmoothed.ome.zarr"]
            dir = joinpath(proj_dir, "0", img.uid, fn)
            g = zgroup(Zarr.DirectoryStore(dir);
                       attrs = Dict("multiscales" => [Dict("axes" =>
                           [Dict("name" => n) for n in ["t", "c", "z", "y", "x"]])]))
            a = zcreate(UInt16, g, "0", 5, 4, 3, 1, 2; chunks = (5, 4, 3, 1, 2))
            a[:, :, :, :, :] = zeros(UInt16, 5, 4, 3, 1, 2)
        end

        ask(q) = JSON3.read(api_viewer_meta(HTTP.Request("GET", "/api/viewer/meta?" * q))[2])

        # No version asked for → the ACTIVE one, named back.
        m = ask("projectUid=$(proj.uid)&imageUid=$(img.uid)")
        @test m.valueName == "smoothed"
        @test Set(m.valueNames) == Set(["default", "smoothed"])
        # `_active` is bookkeeping, not a version anyone can pick.
        @test !("_active" in m.valueNames)

        @test m.activeValueName == "smoothed"

        # A version asked for → that one, echoed rather than re-resolved. `activeValueName` must NOT
        # follow it: the whole point is that a picker can then say "this is not the active version",
        # which is impossible if the only field echoes the request.
        m2 = ask("projectUid=$(proj.uid)&imageUid=$(img.uid)&valueName=default")
        @test m2.valueName == "default"
        @test m2.activeValueName == "smoothed"
        @test Set(m2.valueNames) == Set(["default", "smoothed"])
    finally
        old === nothing ? delete!(dirs, "projects") : (dirs["projects"] = old)
    end
end

@testset "API: viewer overlays on an image with no cell table" begin
    # An unsegmented image is the FIRST thing the viewer opens for most users. It must answer an empty
    # overlay, not a 500 — the panel asks unconditionally.
    dirs = Cecelia.cecelia_conf()["dirs"]
    old  = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        proj = create_project!(name = "api-overlay-empty")
        img  = add_image!(add_set!(proj; name = "s"); name = "a")
        save!(img)
        st, body = api_viewer_overlays(HTTP.Request("GET",
            "/api/viewer/overlays?projectUid=$(proj.uid)&imageUid=$(img.uid)"))
        d = JSON3.read(body)
        @test st == 200
        @test d.nCells == 0 && isempty(d.pops) && d.values === nothing
        @test d.note == "not segmented"        # the reason, so the panel can say so rather than guess
    finally
        old === nothing ? delete!(dirs, "projects") : (dirs["projects"] = old)
    end
end

@testset "API: viewer props round-trip (save then load)" begin
    # PY — the WebGPU viewer autosaves per-image view state (contrast/colormap/T-Z/camera) to the
    # SAME on-disk file napari's autosave uses, so an animation-card snapshot is portable across
    # viewers. Missing file answers 404 (a normal state — the image was never saved), a saved file
    # comes back exactly.
    dirs = Cecelia.cecelia_conf()["dirs"]
    old  = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        proj = create_project!(name = "api-viewer-props")
        img  = add_image!(add_set!(proj; name = "s"); name = "a")
        img.filepath = Dict("default" => "ccidImage.ome.zarr", "_active" => "default")
        save!(img)
        # A minimal store, so `resolve_image_version` finds a directory that exists.
        mkpath(joinpath(dirname(dirname(img._dir)), "0", img.uid, "ccidImage.ome.zarr"))

        base = "projectUid=$(proj.uid)&imageUid=$(img.uid)"

        # No file yet → 404 with a message (a normal state, not a failure).
        st, _ = api_viewer_props_get(HTTP.Request("GET", "/api/viewer/props?" * base))
        @test st == 404

        vs = Dict("webgpu" => Dict("channels" => [Dict("hex"=>"#ff0000","lo"=>1,"hi"=>2,"visible"=>true)],
                                    "cam" => Dict("yaw"=>0.1,"pitch"=>0.2,"dist"=>3.0,"panX"=>0.0,"panY"=>0.0),
                                    "mode"=>"plane","zPlane"=>0,"zRange"=>[0,0],"t"=>0,"valueName"=>""),
                  "layers" => Dict("Channel 0" => Dict("contrast_limits"=>[1,2], "visible"=>true)))
        body = JSON3.write(Dict("projectUid" => proj.uid, "imageUid" => img.uid, "viewState" => vs))
        st, _ = api_viewer_props_post(Vector{UInt8}(body))
        @test st == 200

        st, out = api_viewer_props_get(HTTP.Request("GET", "/api/viewer/props?" * base))
        @test st == 200
        d = JSON3.read(out)
        @test d.webgpu.channels[1].hex == "#ff0000"
        @test d.webgpu.mode == "plane"
    finally
        old === nothing ? delete!(dirs, "projects") : (dirs["projects"] = old)
    end
end
