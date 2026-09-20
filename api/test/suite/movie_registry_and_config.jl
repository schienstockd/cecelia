# Movie registry + config + frame range testsets — extracted from api/test/runtests.jl.
#
# Four testsets covering the /api/movies/* configuration surface:
#  - `API: movie registry` (settings/movies.json, big — save/list/delete/rename/dedup/
#    default_movie/…).
#  - `API: movie config banks what the edit page reads` (contract: only what edit reads is
#    what the batch runner sees).
#  - `MovieConfig JSON round-trip preserves the on-disk shape`.
#  - `API: movie frame range` (_t_range parser).
#
# One path expression rewritten to use API_TEST_DIR. Extracted so runtests.jl contains
# only include lines + section-header comments — same shape as app/test/suite/*.jl.

@testset "API: movie registry" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        uid = "TESTMOV"
        mdir = joinpath(tmp, uid, "movies"); mkpath(mdir)
        write(joinpath(mdir, "a.mp4"), "a")
        write(joinpath(mdir, "b.mp4"), "b")

        # ── a project with no registry reads exactly as it did before one existed
        ms = movies_with_meta(uid)
        @test Set(m.name for m in ms) == Set(["a.mp4", "b.mp4"])
        @test all(m -> m.displayName == "" && !m.starred && isempty(m.tags) &&
                       m.producedBy == "" && !m.hasConfig && !m.configStale, ms)

        # ── the user-owned fields patch INDEPENDENTLY: setting tags must not clear a star
        @test _post(api_movies_meta_set, Dict("projectUid"=>uid, "name"=>"a.mp4", "starred"=>true))[1] == 200
        @test _post(api_movies_meta_set,
                    Dict("projectUid"=>uid, "name"=>"a.mp4", "tags"=>["figure 2", "figure 2", " "]))[1] == 200
        a = only(filter(m -> m.name == "a.mp4", movies_with_meta(uid)))
        @test a.starred && a.tags == ["figure 2"]        # deduped, blanks dropped

        # a display name never touches the file
        _post(api_movies_meta_set, Dict("projectUid"=>uid, "name"=>"a.mp4", "displayName"=>"  Day 3  CNO "))
        a = only(filter(m -> m.name == "a.mp4", movies_with_meta(uid)))
        @test a.displayName == "Day 3 CNO"               # trimmed, inner whitespace collapsed
        @test isfile(joinpath(mdir, "a.mp4"))

        # ── guards: traversal and a movie that doesn't exist are both "no such movie", and the
        #    offending names come BACK, so a caller can say which of a selection it could not touch
        st, body = _post(api_movies_meta_set, Dict("projectUid"=>uid, "name"=>"../x.mp4"))
        @test st == 404 && JSON3.read(body).rejected == ["../x.mp4"]
        @test _post(api_movies_meta_set, Dict("projectUid"=>uid, "name"=>"nope.mp4"))[1] == 404
        @test _post(api_movies_delete,   Dict("projectUid"=>uid, "name"=>"../a.mp4"))[1] == 404

        # ── BULK: one call, one read-modify-write. A client looping N requests would rewrite the
        #    registry N times, and two in flight at once lose one side's edit.
        write(joinpath(mdir, "c.mp4"), "c")
        # addTags is a set operation — it must not wipe tags a movie already carries ("figure 2" on a)
        st, body = _post(api_movies_meta_set,
                         Dict("projectUid"=>uid, "names"=>["a.mp4", "c.mp4"], "addTags"=>["cohort 1"]))
        @test st == 200
        byname = Dict(m.name => m for m in movies_with_meta(uid))
        @test byname["a.mp4"].tags == ["figure 2", "cohort 1"]
        @test byname["c.mp4"].tags == ["cohort 1"]
        # …and removeTags takes one back out without touching the others
        _post(api_movies_meta_set, Dict("projectUid"=>uid, "names"=>["a.mp4"], "removeTags"=>["figure 2"]))
        @test only(filter(m -> m.name == "a.mp4", movies_with_meta(uid))).tags == ["cohort 1"]
        # a bulk call carries the valid names through and reports the rest
        st, body = _post(api_movies_meta_set,
                         Dict("projectUid"=>uid, "names"=>["c.mp4", "ghost.mp4"], "starred"=>true))
        @test st == 200
        d = JSON3.read(body)
        @test d.names == ["c.mp4"] && d.rejected == ["ghost.mp4"]
        # a display name identifies ONE movie, so it is not applied across a selection
        _post(api_movies_meta_set, Dict("projectUid"=>uid, "names"=>["a.mp4","c.mp4"], "displayName"=>"X"))
        @test all(m -> m.displayName != "X", movies_with_meta(uid))
        # bulk delete removes every named file in one pass
        st, body = _post(api_movies_delete, Dict("projectUid"=>uid, "names"=>["c.mp4", "ghost.mp4"]))
        @test st == 200 && JSON3.read(body).deleted == ["c.mp4"]
        @test !isfile(joinpath(mdir, "c.mp4"))

        # ── the user's "name" suffix, banked so the next recording can offer it back
        # It is NOT recoverable from the filename: that carries uid and attribute parts too, with
        # nothing marking where the suffix begins, so parsing it back would mean encoding three
        # recorders' naming conventions in a fourth place. Stored instead.
        register_movie!(uid, "a.mp4"; produced_by = "viewer", suffix = "afCorrected")
        @test only(filter(m -> m.name == "a.mp4", movies_with_meta(uid))).suffix == "afCorrected"

        # The RAW suffix, not the `_sanitised` fragment that goes in the filename — offering `_af`
        # back would re-prefix it to `__af` on the next recording.
        @test !startswith(only(filter(m -> m.name == "a.mp4", movies_with_meta(uid))).suffix, "_")

        # A recorder that passes none leaves a banked one standing, like `imageUid`/`channels` — a
        # re-record must not blank what the previous one knew.
        register_movie!(uid, "a.mp4"; produced_by = "viewer")
        @test only(filter(m -> m.name == "a.mp4", movies_with_meta(uid))).suffix == "afCorrected"

        # A movie recorded before the field existed reports "", never `nothing` — the frontend filters
        # blanks out of the suggestion list rather than rendering an empty row.
        @test only(filter(m -> m.name == "b.mp4", movies_with_meta(uid))).suffix == ""

        # ── config banking + the stale rule
        register_movie!(uid, "b.mp4"; produced_by = "batch",
                        config = Dict("fps" => 15), config_kind = "look")
        b = only(filter(m -> m.name == "b.mp4", movies_with_meta(uid)))
        @test b.producedBy == "batch" && b.hasConfig && b.configKind == "look" && !b.configStale
        # Re-recording replaces the bytes under an entry that stays put, and the config then no longer
        # describes the file. Aged by rewinding the ENTRY rather than the file's mtime — setting an
        # mtime portably is not something Julia offers, and the rule is a comparison either way.
        let reg = _read_movies_registry(uid)
            reg["b.mp4"]["recordedAt"] = time() - 3600
            _write_movies_registry!(uid, reg)
        end
        @test only(filter(m -> m.name == "b.mp4", movies_with_meta(uid))).configStale

        # …and a stamp that is not unix seconds (absent, or an ISO string) cannot be vouched for. The
        # units matter: `string(Dates.now())` is naive LOCAL time, which `datetime2unix` would read as
        # UTC and put hours in the FUTURE — on UTC+10 nothing would ever read as stale.
        for bad in (nothing, "2020-01-01T00:00:00")
            let reg = _read_movies_registry(uid)
                bad === nothing ? delete!(reg["b.mp4"], "recordedAt") : (reg["b.mp4"]["recordedAt"] = bad)
                _write_movies_registry!(uid, reg)
            end
            @test only(filter(m -> m.name == "b.mp4", movies_with_meta(uid))).configStale
        end
        register_movie!(uid, "b.mp4"; produced_by = "batch",
                        config = Dict("fps" => 15), config_kind = "look")   # re-stamp → fresh again
        @test !only(filter(m -> m.name == "b.mp4", movies_with_meta(uid))).configStale

        # a re-record MERGES: the user's name/star/tags outlive the new bytes. Asserted as SURVIVAL
        # against whatever they are now — a literal here would just re-encode the edits made above and
        # break every time one of them changes, which is not what this is pinning.
        before = only(filter(m -> m.name == "a.mp4", movies_with_meta(uid)))
        register_movie!(uid, "a.mp4"; produced_by = "viewer",
                        config = Dict("look" => Dict("channels" => Dict("CD3" => "green"))),
                        config_kind = "look")
        a = only(filter(m -> m.name == "a.mp4", movies_with_meta(uid)))
        @test a.displayName == before.displayName && !isempty(a.displayName)
        @test a.starred == before.starred && a.tags == before.tags && !isempty(a.tags)
        @test a.producedBy == "viewer" && a.configKind == "look"

        # ── which image, and what it shows: the two the Movies page joins against the project's images.
        # The BACK-FILL is the point of the fallbacks — neither field existed when the movies already on
        # disk were recorded, and the page still has to answer for them.
        register_movie!(uid, "a.mp4"; produced_by = "viewer", config_kind = "look",
                        config = Dict("imageUid" => "imgA",
                                      "look" => Dict("channels" => Dict("CD3"  => "green",
                                                                        "B220" => "magenta"))))
        a = only(filter(m -> m.name == "a.mp4", movies_with_meta(uid)))
        # the single recorder has banked the uid inside its config since Phase 4, so a viewer movie
        # answers with no migration; the channels come out of the `look` it read off the live view
        @test a.imageUid == "imgA" && a.channels == ["B220", "CD3"]   # sorted — a JSON object has no order
        # a BATCH banks the authored config one level in, under `config`
        register_movie!(uid, "b.mp4"; produced_by = "batch", config_kind = "look",
                        config = Dict("imageUids" => ["img1", "img2"],
                                      "config" => Dict("channels" => Dict("DAPI" => "blue"))))
        bm = only(filter(m -> m.name == "b.mp4", movies_with_meta(uid)))
        @test bm.channels == ["DAPI"]
        # …but its `imageUids` is the whole SELECTION, not this file's image. Reading it would label
        # every movie in the batch with the same wrong one, so it is deliberately not a fallback — the
        # filename is what identifies a batch movie, and that is resolved client-side.
        @test bm.imageUid == ""
        # Banked explicitly, both win — and the channel ORDER survives, which the config fallback cannot
        # give: the recorder lists them in the image's order, a JSON object has none.
        register_movie!(uid, "b.mp4"; produced_by = "batch", image_uid = "img2",
                        channels = ["CD8", "DAPI"], config_kind = "look",
                        config = Dict("config" => Dict("channels" => Dict("DAPI" => "blue"))))
        bm = only(filter(m -> m.name == "b.mp4", movies_with_meta(uid)))
        @test bm.imageUid == "img2" && bm.channels == ["CD8", "DAPI"]
        # A re-record by a producer that cannot say (an animation shows whatever its keyframes do) leaves
        # the banked answer standing rather than blanking it
        register_movie!(uid, "b.mp4"; produced_by = "batch", config_kind = "look",
                        config = Dict("fps" => 15))
        bm = only(filter(m -> m.name == "b.mp4", movies_with_meta(uid)))
        @test bm.imageUid == "img2" && bm.channels == ["CD8", "DAPI"]

        # the full entry (with the config the list omits) comes from the meta GET
        st, body = api_movies_meta_get(HTTP.Request("GET", "/api/movies/meta?projectUid=$uid&name=a.mp4"))
        @test st == 200
        @test haskey(JSON3.read(body).entry, :config)

        # ── the EDIT side (Phase 6): the config comes back VERBATIM, nesting and all.
        # `frontend/src/utils/movieRestore.ts` reads it field by field, so anything this route flattens,
        # renames or drops is a config that reopens wrong rather than one that fails to open.
        register_movie!(uid, "b.mp4"; produced_by = "animation", config_kind = "keyframes",
                        config = Dict("imageUid" => "img1", "fps" => 20,
                                      "keyframes"    => [Dict("viewState" => Dict("camera" => Dict("zoom" => 2)),
                                                              "steps" => 40)],
                                      "keyframeMeta" => [Dict("assetId" => "a1", "duration" => 2)]))
        st, body = api_movies_meta_get(HTTP.Request("GET", "/api/movies/meta?projectUid=$uid&name=b.mp4"))
        cfg = JSON3.read(body).entry.config
        @test st == 200
        @test cfg.imageUid == "img1" && cfg.fps == 20
        @test cfg.keyframes[1].viewState.camera.zoom == 2 && cfg.keyframes[1].steps == 40
        @test cfg.keyframeMeta[1].assetId == "a1" && cfg.keyframeMeta[1].duration == 2

        # ── delete removes the file AND the entry
        @test _post(api_movies_delete, Dict("projectUid"=>uid, "name"=>"a.mp4"))[1] == 200
        @test !isfile(joinpath(mdir, "a.mp4"))
        @test Set(m.name for m in movies_with_meta(uid)) == Set(["b.mp4"])

        # ── an entry whose file vanished outside the app (a manual rm, a moved folder) is PRUNED by
        #    the listing pass, not rendered as a row that plays nothing
        write(joinpath(mdir, "ghost.mp4"), "g")
        register_movie!(uid, "ghost.mp4"; produced_by = "batch")
        @test haskey(_read_movies_registry(uid), "ghost.mp4")
        rm(joinpath(mdir, "ghost.mp4"))
        @test Set(m.name for m in movies_with_meta(uid)) == Set(["b.mp4"])
        @test !haskey(_read_movies_registry(uid), "ghost.mp4")

        # ── a corrupt registry degrades to "no metadata", never to a broken page
        write(joinpath(tmp, uid, "settings", "movies.json"), "{not json")
        @test Set(m.name for m in movies_with_meta(uid)) == Set(["b.mp4"])
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

# ── The banked movie config is a CONTRACT with the edit page ──────────────────
# `movie_config` is assembled in sockets.jl and read, field by field, by
# `frontend/src/utils/movieRestore.ts`. Nothing type-checks across that boundary and nothing fails when
# a key goes missing — the page just quietly restores less and says so in a note nobody wrote. So the
# keys the edit path cannot work without are pinned here, at the one place that names them.
#
# `MovieRecordConfig` / `MovieBatchConfig` (`api/src/movie_config.jl`) are the ONE place the shape is
# named; the socket handlers hand a live napari through the struct as a kwarg bundle. The struct field
# name IS the on-disk JSON key (StructTypes.Struct()), so a rename here surfaces as a broken restore
# on `movieRestore.ts` — this test breaks first. Handler-source is still checked for `_t_range(data)`
# because that lives at the boundary, not on the config.
@testset "API: movie config banks what the edit page reads" begin
    src = read(joinpath(API_TEST_DIR, "..", "src", "sockets.jl"), String)
    single = src[findfirst("function handle_movie_record", src)[1]:end]
    single = single[1:findfirst("\nend", single)[1]]

    # WHICH IMAGE. A movie is named after its image, but nothing can turn that name back into a uid, so
    # without this an edited look has no idea what it was recorded on.
    @test :imageUid  in fieldnames(MovieRecordConfig)
    @test :imageUids in fieldnames(MovieBatchConfig)
    # The editor's half of a keyframe — thumbnail, title, seconds. `keyframes` alone is the RENDER
    # payload, which restores a timeline with no strip and durations rounded to whole frames.
    @test :keyframeMeta in fieldnames(MovieRecordConfig)
    # The look itself, and the kinds it is filed under (MOVIE_MANAGEMENT_PLAN Decision 7).
    @test :look      in fieldnames(MovieRecordConfig)
    @test :keyframes in fieldnames(MovieRecordConfig)
    @test :config    in fieldnames(MovieBatchConfig)
    @test :fileAttrs in fieldnames(MovieBatchConfig)
    # The output half both kinds share — restoring a look at the wrong size or fps is not restoring it.
    for k in (:fps, :sizeX, :sizeY, :suffix)
        @test k in fieldnames(MovieRecordConfig)
        @test k in fieldnames(MovieBatchConfig)
    end
    # The frame range: banked at the top level for a viewer recording, and inside the authored config
    # for a batch (`buildBatchMovieConfig` always emits the pair). A recreate that silently records the
    # whole timelapse is not a recreate.
    @test :tStart in fieldnames(MovieRecordConfig)
    @test :tEnd   in fieldnames(MovieRecordConfig)
    @test occursin("_t_range(data)", single)
end

# ── MovieConfig on-disk shape: JSON round-trip preserves every field ──────────
# `settings/movies.json` is written verbatim from `register_movie!(config = <the struct>)` — the file
# a user has after this PR must be readable by a Cecelia server that predates it (the on-disk shape
# hasn't changed), AND by every future one (the shape is now pinned to the struct fields via
# StructTypes.Struct()). Both directions matter: a silent field rename would be a Phase-6-edit
# regression that only surfaces later when someone opens their old movie in the animation editor.
@testset "MovieConfig JSON round-trip preserves the on-disk shape" begin
    rec = MovieRecordConfig(;
        imageUid = "img-abc",
        keyframeMeta = Dict{String,Any}("dur" => [1, 2, 3]),
        fps = 30, sizeX = 1024, sizeY = 768, suffix = "-a",
        titleCard = Dict{String,Any}("enabled" => true, "title" => "T"),
        valueNames = ["default", "smoothed"],
        labelValueNames = ["Tcell"], branchValueNames = nothing,
        labelContour = 2, show3D = true, zSlice = nothing,
        tStart = 5, tEnd = 20,
        compareLayout = "grid", compareContrast = "shared",
        showTimestamp = false, showScaleBar = true,
        look = Dict{String,Any}("colourBy" => "cluster"),
        keyframes = Any[Dict{String,Any}("t" => 1.0)])
    round = JSON3.read(JSON3.write(rec))
    # Every declared field survives a JSON round-trip under the SAME wire name — the frontend restore
    # path reads by these exact keys (movieRestore.ts).
    for f in fieldnames(MovieRecordConfig)
        @test haskey(round, f)
    end
    @test String(get(round, :imageUid, "")) == "img-abc"
    @test Int(get(round, :fps, 0)) == 30
    @test Bool(get(round, :show3D, false)) === true
    @test Bool(get(round, :showTimestamp, true)) === false
    @test collect(String, get(round, :valueNames, String[])) == ["default", "smoothed"]

    # `nothing` fields survive as JSON `null`, not as an absent key — the edit page needs to tell "no
    # value on record" apart from "the writer forgot", which is why `_wstr`/`_wbool` were built.
    rec_min = MovieRecordConfig(imageUid = "img-null")
    round_min = JSON3.read(JSON3.write(rec_min))
    @test haskey(round_min, :zSlice) && get(round_min, :zSlice, "sentinel") === nothing
    @test haskey(round_min, :look)   && get(round_min, :look,   "sentinel") === nothing

    # Batch flavour — every declared field survives round-trip too. `imageUids` is the whole selection
    # (banked on EVERY movie in the batch — see `handle_movie_batch`), NOT `imageUid`; conflating the
    # two is what the enum wall in `_entry_image_uid` (movies_api.jl) exists to close.
    batch = MovieBatchConfig(;
        config = Dict{String,Any}("valueNames" => ["default"], "fps" => 15),
        fileAttrs = ["date"], fps = 15, sizeX = nothing, sizeY = nothing,
        suffix = "", imageUids = ["a", "b", "c"])
    bround = JSON3.read(JSON3.write(batch))
    for f in fieldnames(MovieBatchConfig)
        @test haskey(bround, f)
    end
    @test collect(String, get(bround, :imageUids, String[])) == ["a", "b", "c"]
    @test !haskey(bround, :imageUid)  # batch has NO singular imageUid — flavour is disjoint
end

# Which stretch of the timelapse a movie sweeps. ONE reader for both entry points — the viewer's
# recorder puts the pair on the request, the batch page puts it in its authored config — because they
# mean the same thing and a second parse is where the two would drift.
@testset "API: movie frame range" begin
    # absent = the whole thing, which is what every recording did before the control existed
    @test _t_range(Dict{Symbol,Any}()) == (0, nothing)
    # `nothing` for the end MEANS "the last frame" and must survive as `nothing` — clamping it to a
    # number here would truncate the same config the moment it ran on a longer image, which is exactly
    # what a batch does.
    @test _t_range(Dict(:tStart => 10, :tEnd => nothing)) == (10, nothing)
    @test _t_range(Dict(:tStart => 10, :tEnd => 60)) == (10, 60)
    # a negative start is a bad value, not a request — clamp rather than fail a render
    @test _t_range(Dict(:tStart => -5, :tEnd => 20)) == (0, 20)
    # an inverted range would sweep nothing at all; the end gives way to the start
    @test _t_range(Dict(:tStart => 30, :tEnd => 5)) == (30, 30)
    # the wire carries JSON numbers, which arrive as Float64 for a fractional value
    @test _t_range(Dict(:tStart => 2.0, :tEnd => 7.0)) == (2, 7)
    # and it reads a JSON3 object, not only a Dict — that is what a real request is
    @test _t_range(JSON3.read("""{"tStart":3,"tEnd":9}""")) == (3, 9)
    @test _t_range(JSON3.read("""{"tStart":3,"tEnd":null}""")) == (3, nothing)
end
