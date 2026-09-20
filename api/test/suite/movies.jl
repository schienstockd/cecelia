# Movie naming + version comparison + comparison grid testsets — extracted from api/test/runtests.jl.
#
# Six testsets covering the movies API surface:
#  - `API: batch-movie output naming`
#  - `API: single-image movie naming`
#  - `API: 3D detail level from a movie config`
#  - `API: filename fragments are sanitised one way`
#  - `API: movie version comparison`
#  - `API: movie comparison grid`
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: batch-movie output naming" begin
    attr = Dict("Day" => "3", "Treatment" => "CNO", "Blank" => "  ")
    # attrs joined in the requested order, uid always terminates → unique name
    @test _movie_basename(attr, "AbC123", ["Day", "Treatment"]) == "3_CNO_AbC123.mp4"
    # no attrs → just the uid
    @test _movie_basename(attr, "AbC123", String[]) == "AbC123.mp4"
    # blank / missing attr values are dropped (never leaves a dangling separator)
    @test _movie_basename(attr, "AbC123", ["Blank", "Missing", "Day"]) == "3_AbC123.mp4"
    # unsafe characters in an attr value are sanitised to underscores
    @test _movie_basename(Dict("T" => "a/b c:d"), "u1", ["T"]) == "a_b_c_d_u1.mp4"
    # channels token expands to the shown channel names joined by '-', positioned by its order
    chans = ["CD3", "CD8"]
    @test _movie_basename(attr, "AbC123", ["Day", MOVIE_CHANNELS_TOKEN], chans) == "3_CD3-CD8_AbC123.mp4"
    @test _movie_basename(attr, "AbC123", [MOVIE_CHANNELS_TOKEN, "Treatment"], chans) == "CD3-CD8_CNO_AbC123.mp4"
    # token with no shown channels drops out cleanly (no dangling separator)
    @test _movie_basename(attr, "AbC123", ["Day", MOVIE_CHANNELS_TOKEN], String[]) == "3_AbC123.mp4"

    # What TERMINATES the name is a choice: a single viewer recording is named after the IMAGE
    # (`_movie_named_path`), a batch after the uid — so regenerating a restored viewer config wrote a
    # uid-named twin beside the original. `name` ends it with the image instead.
    @test _movie_basename(attr, "AbC123", String[]; name = "M2b-MERTK_KAT (cropped)") ==
          "M2b-MERTK_KAT_cropped.mp4"
    @test _movie_basename(attr, "AbC123", ["Day"]; name = "my image") == "3_my_image.mp4"
    # …and this is exactly what the single-image recorder produces for the same image, which is the
    # whole point — the two namers must agree once the batch is asked to name by image
    @test _movie_basename(Dict{String,String}(), "AbC123", String[]; name = "My Image") ==
          basename(_movie_named_path((; name = "My Image", _dir = joinpath("p", "1", "u")), "AbC123"))
    # a name of pure punctuation sanitises to nothing, and a file still has to be written
    @test _movie_basename(attr, "AbC123", String[]; name = "()") == "AbC123.mp4"
    @test _movie_basename(attr, "AbC123", String[]; name = "   ") == "AbC123.mp4"
    # blank `name` is the default and keeps the uid — the safe one, since two images CAN share a name
    @test _movie_basename(attr, "AbC123", String[]) == "AbC123.mp4"
end

# The single-image recorders (timelapse / animation) name by IMAGE via the shared _movies_dir +
# _movie_named_path (img._dir = {proj}/1/{uid} → {proj}/movies/). Mock img with a NamedTuple.
@testset "API: single-image movie naming" begin
    mktempdir() do tmp
        img = (; _dir = joinpath(tmp, "proj", "1", "uid7"), name = "My Image")
        @test _movie_named_path(img, "uid7") == joinpath(tmp, "proj", "movies", "My_Image.mp4")
        @test _movie_named_path(img, "uid7"; suffix = "_animation") ==
              joinpath(tmp, "proj", "movies", "My_Image_animation.mp4")
        @test isdir(joinpath(tmp, "proj", "movies"))   # _movies_dir created it
        # blank / unsafe name falls back to the uid
        blank = (; _dir = joinpath(tmp, "proj", "1", "uid7"), name = "   ")
        @test _movie_named_path(blank, "uid7") == joinpath(tmp, "proj", "movies", "uid7.mp4")
        # A name ENDING in a character a filename can't hold — the crop task's "(cropped)" is the one
        # that showed up in the movies list — must not leave the separator it collapses to. It used to,
        # and the animation variant then doubled it ("…_cropped__animation.mp4").
        cropped = (; _dir = joinpath(tmp, "proj", "1", "uid7"),
                     name = "M2b-MERTK_KAT-SWHL-GFP-Tom-res (cropped)")
        @test _movie_named_path(cropped, "uid7") ==
              joinpath(tmp, "proj", "movies", "M2b-MERTK_KAT-SWHL-GFP-Tom-res_cropped.mp4")
        @test _movie_named_path(cropped, "uid7"; suffix = "_animation") ==
              joinpath(tmp, "proj", "movies", "M2b-MERTK_KAT-SWHL-GFP-Tom-res_cropped_animation.mp4")
        # a name with nothing usable left also falls back to the uid
        parens = (; _dir = joinpath(tmp, "proj", "1", "uid7"), name = "()")
        @test _movie_named_path(parens, "uid7") == joinpath(tmp, "proj", "movies", "uid7.mp4")
    end
end

# ONE sanitiser behind the image name, the user's suffix and the attr-composed basename — they used to
# be three near-copies and only one of them stripped edge separators. Mirrored in the frontend by
# `safeNamePart` (frontend/src/utils/batchMovie.ts), whose testset asserts the same cases.
# The 3D detail level an authored/batch movie config asks for. Absent means FULL RESOLUTION, not
# napari's automatic choice: napari picks the coarsest level in 3D, which erases a strided label
# pyramid, and a config written before this control existed still wants visible masks.
@testset "API: 3D detail level from a movie config" begin
    @test _detail_3d(Dict(:detail3d => 0)) == 0
    @test _detail_3d(Dict(:detail3d => 2)) == 2
    @test _detail_3d(Dict(:detail3d => "3")) == 3          # JSON numbers may arrive as strings
    @test _detail_3d(Dict{Symbol,Any}()) == 0              # absent → full resolution
    @test _detail_3d(Dict(:detail3d => nothing)) === nothing   # explicit null → leave it to napari
    @test _detail_3d(Dict(:detail3d => -1)) == 0           # never a negative index
end

@testset "API: filename fragments are sanitised one way" begin
    @test _safe_name_part("M2b-MERTK_KAT-SWHL-GFP-Tom-res (cropped)") ==
          "M2b-MERTK_KAT-SWHL-GFP-Tom-res_cropped"
    @test _safe_name_part("a/b c:d") == "a_b_c_d"
    @test _safe_name_part("Day 3.v2-final") == "Day_3.v2-final"
    @test _safe_name_part("../../etc/passwd") == "etc_passwd"
    @test _safe_name_part("__x__") == "x"
    @test _safe_name_part("   ") == ""
    @test _safe_name_part("()") == ""
    @test _safe_name_part(nothing) == ""
end

# Side-by-side version comparison (docs/todo/MOVIE_COMPARE_PLAN.md). The pure parts: which versions a
# config asks for, what each column is, how the contrast toggle reads, and the frame arithmetic behind
# the single progress bar. The recording loop itself needs a live viewer and is not exercised here.
@testset "API: movie version comparison" begin
    # Which versions to record. A config from before comparisons existed carries one `valueName`, and
    # "" (the active version) is a perfectly good single column — the list is never empty, so the
    # caller always has something to record.
    @test _config_value_names(Dict(:valueNames => ["default", "af"])) == ["default", "af"]
    @test _config_value_names(Dict(:valueName => "af"))               == ["af"]
    @test _config_value_names(Dict{Symbol,Any}())                     == [""]
    @test _config_value_names(Dict(:valueNames => String[], :valueName => "af")) == ["af"]

    # Each column carries the WHOLE authored config with its own version pinned — the overlays and
    # channels must not differ between columns, only the version does.
    cols = _version_columns(Dict(:channels => Dict("CD3" => "green"), :showTracks => true),
                            ["default", " af_corrected ", ""])
    @test [c.label for c in cols] == ["default", "af_corrected", "active"]   # "" captions as "active"
    @test [String(c.config[:valueName]) for c in cols] == ["default", "af_corrected", ""]
    @test all(c -> c.config[:showTracks] === true, cols)
    @test all(c -> haskey(c.config, :channels), cols)
    # Symbol keys, because that is how every config reader here addresses them
    @test get(cols[1].config, :valueName, "MISSING") == "default"

    # D4 — the contrast toggle. Anything unrecognised reads as the default rather than failing a batch.
    @test _share_contrast("reference")
    @test _share_contrast("")
    @test _share_contrast("nonsense")
    @test !_share_contrast("version")

    # Frame arithmetic for ONE progress bar across the passes + the compose. Mirrors the bridge's own
    # range maths: one frame per timepoint, both ends inclusive.
    img20 = (; meta = Dict("SizeT" => 20))
    @test _t_sweep_frames(img20, 0, nothing) == 20
    @test _t_sweep_frames(img20, 5, nothing) == 15
    @test _t_sweep_frames(img20, 0, 9)       == 10
    @test _t_sweep_frames(img20, 0, 99)      == 20      # clamped to the stack
    @test _t_sweep_frames(img20, 8, 8)       == 0       # empty range
    @test _t_sweep_frames((; meta = Dict("SizeT" => 1)), 0, nothing) == 0
    @test _t_sweep_frames((; meta = Dict{String,Any}()), 0, nothing) == 0   # image doesn't say
end

# The comparison GRID (docs/todo/MOVIE_COMPARE_PLAN.md, generalised). `_record_grid!` and everything
# under it is blind to what made two cells differ, so what needs pinning is the layer above: what shape
# a pair of selections means, what each cell pins, and what a cell says about the masks it draws.
@testset "API: movie comparison grid" begin
    base = Dict{Symbol,Any}(:channels => Dict("CD3" => "green"))

    # 2+ of BOTH → the cross-product. One row per MASK; that row's cells are the VERSIONS, so versions
    # read across and masks read down.
    grid = _compare_grid(merge(base, Dict{Symbol,Any}(
        :valueNames => ["default", "af"], :labelValueNames => ["cellpose", "coastal"])))
    @test length(grid) == 2
    @test [r.label for r in grid] == ["cellpose", "coastal"]
    @test [c.label for c in grid[1].columns] == ["default", "af"]
    @test [String(c.config[:valueName]) for c in grid[1].columns] == ["default", "af"]
    # every cell of a row draws THAT row's mask, and only it
    @test all(c -> c.config[:labelValueNames] == ["cellpose"], grid[1].columns)
    @test all(c -> c.config[:labelValueNames] == ["coastal"],  grid[2].columns)
    @test all(r -> all(c -> haskey(c.config, :channels), r.columns), grid)   # the config rides along
    # rectangular: 2 x 2 is FOUR renders, not two — the cost is multiplicative
    @test sum(length(r.columns) for r in grid) == 4

    # 2+ of ONE only → a single row, side by side, whichever list it came from.
    vonly = _compare_grid(Dict{Symbol,Any}(:valueNames => ["default", "af"]))
    @test length(vonly) == 1
    @test [c.label for c in vonly[1].columns] == ["default", "af"]
    @test vonly[1].label == ""                       # no outer compose → nothing to caption

    monly = _compare_grid(Dict{Symbol,Any}(:valueNames => ["af"], :labelValueNames => ["a", "b"]))
    @test length(monly) == 1
    @test [c.label for c in monly[1].columns] == ["a", "b"]
    # …all on the ONE selected version, which is what keeps a mask row comparable
    @test all(c -> String(c.config[:valueName]) == "af", monly[1].columns)
    @test [c.config[:labelValueNames] for c in monly[1].columns] == [["a"], ["b"]]

    # A single mask is drawn in the one cell rather than becoming a row of its own.
    one = _compare_grid(Dict{Symbol,Any}(:labelValueNames => ["a"]))
    @test length(one) == 1 && length(one[1].columns) == 1
    @test one[1].columns[1].config[:labelValueNames] == ["a"]

    # Nothing selected is still ONE cell — a plain movie is a 1x1 grid, not a special case.
    plain = _compare_grid(Dict{Symbol,Any}())
    @test length(plain) == 1 && length(plain[1].columns) == 1
    @test String(plain[1].columns[1].config[:valueName]) == ""

    # `grid` layout — one row of N folded into the squarest rectangle that holds them. A cosmetic
    # rearrangement of the SAME cells, handed on as the same `Vector{MovieRow}` the cross-product
    # builds, so nothing downstream needs to know it happened.
    four = _compare_grid(Dict{Symbol,Any}(:valueNames => ["a", "b", "c", "d"]))
    wrapped = _wrap_grid(four, "grid")
    @test [length(r.columns) for r in wrapped] == [2, 2]                 # 4 → 2x2
    @test [c.label for r in wrapped for c in r.columns] == ["a", "b", "c", "d"]   # order preserved
    @test all(r -> isempty(r.label), wrapped)    # the cells carry the captions; a wrapped row is nothing
    # a short last row is allowed — the compositor centres it on black rather than us padding the grid
    @test [length(r.columns) for r in
           _wrap_grid(_compare_grid(Dict{Symbol,Any}(:valueNames => ["a","b","c","d","e"])), "grid")] == [3, 2]
    @test [length(r.columns) for r in
           _wrap_grid(_compare_grid(Dict{Symbol,Any}(:valueNames => ["a","b","c","d","e","f"])), "grid")] == [3, 3]
    # two cells wrap to the row they already are, so `_record_grid!` needs no small-count guard
    @test length(_wrap_grid(_compare_grid(Dict{Symbol,Any}(:valueNames => ["a", "b"])), "grid")) == 1
    @test length(_wrap_grid(_compare_grid(Dict{Symbol,Any}()), "grid")) == 1
    # the other layouts, and a CROSS-PRODUCT under any layout, are left exactly as they were: picking
    # from both lists already fixes both directions
    @test _wrap_grid(four, "row") === four && _wrap_grid(four, "column") === four
    @test _wrap_grid(grid, "grid") === grid
    # …and a wrap costs no extra RENDERS, only one more compose: 4 cells + 2 row composes + 1 stack,
    # against 4 + 1 in a single row
    @test _grid_frame_total(wrapped, 20) == 140
    @test _grid_frame_total(four, 20) == 100

    # Per-cell max_px scales UP to the cell's native long side. `render_view_frame` treats
    # `max_px` as a stride cap (`step = cld(max(H,W), max_px)`), so a cell whose native canvas
    # EXCEEDS the grid-wide cap gets stride-subsampled while smaller siblings render at native —
    # the two land at different µm/output-pixel and a compare-grid reader sees them at different
    # scales. Regression cover for `d5vw7z/c91ICQ` where driftCorrected (598 wide) rendered at
    # half-size next to default and stackAligned (both 512) with a viewer canvas of 512.
    @test _grid_cell_max_px(512, 512) == 512                     # native fits: no change
    @test _grid_cell_max_px(512, 598) == 598                     # native exceeds cap: scaled up
    @test _grid_cell_max_px(512, 300) == 512                     # native under cap: cap wins
    @test _grid_cell_max_px(0,   598) == 0                       # explicit "no cap" stays a no-op
    @test _grid_cell_max_px(0,   0)   == 0
    # And the stride-cost check the fix is defending against: a 598-wide native at cap 512 gets
    # `step = cld(598, 512) = 2`, which subsamples by 2×; at the scaled cap 598 it stays at step 1.
    @test cld(598, 512) == 2 && cld(598, _grid_cell_max_px(512, 598)) == 1

    # D4 — the contrast toggle. Anything unrecognised reads as the default rather than failing a batch.
    @test _share_contrast("reference") && _share_contrast("") && _share_contrast("nonsense")
    @test !_share_contrast("version")

    # Frame arithmetic for ONE progress bar across every pass AND every compose.
    @test _grid_frame_total(1, 2, 20) == 60          # 2 passes + the compose = the old 1-D case
    @test _grid_frame_total(1, 3, 20) == 80
    @test _grid_frame_total(2, 2, 20) == 140         # 4 cells + 2 row composes + 1 stack
    @test _grid_frame_total(3, 1, 20) == 80          # a column of 3 = 3 cells + the stack (no row composes)
    @test _grid_frame_total(1, 1, 20) == 0           # one cell = a plain record, own total
    @test _grid_frame_total(2, 2, 0)  == 0           # unknown T → let each pass report its own

    # The count `_record_grid!` actually uses is read off the grid, so it cannot drift from the loop
    # (which walks it with a running counter). It must agree with the rectangular form above on every
    # rectangular grid — that agreement IS the contract between the progress bar and the pass loop.
    for (vs, ms) in ((["a", "b"], ["s1", "s2"]), (["a", "b", "c"], ["s1", "s2"]),
                     (["a", "b"], String[]), (["a"], ["s1", "s2"]), (String[], String[]))
        g = _compare_grid(Dict{Symbol,Any}(:valueNames => vs, :labelValueNames => ms))
        @test _grid_frame_total(g, 20) == _grid_frame_total(length(g), length(g[1].columns), 20)
    end
    # …and a RAGGED grid (which `_compare_grid` never builds, but the loop tolerates) is counted by
    # what is in it, not by its widest row: 2 + 1 cells, one row compose, one stack = 5 units.
    ragged = MovieRow[(; label = "r1", columns = _version_columns(Dict{Symbol,Any}(), ["a", "b"])),
                      (; label = "r2", columns = _version_columns(Dict{Symbol,Any}(), ["a"]))]
    @test _grid_frame_total(ragged, 20) == 100

    # The column list is authored ONCE for a whole batch, so it does not vary per image — unlike the
    # per-image mask list below, which drops what an image hasn't got.
    @test _config_compare_segmentations(Dict(:labelValueNames => ["a", " b ", "a", ""])) == ["a", "b"]
    @test _config_compare_segmentations(Dict{Symbol,Any}()) == String[]

    # Mask OUTLINE width. Clamped, not validated: a bad value is a display nicety and must not fail a
    # whole batch, and a negative contour is meaningless to the renderer.
    @test _label_contour(Dict{Symbol,Any}())              == 0      # absent → filled, what it always was
    @test _label_contour(Dict(:labelContour => 3))        == 3
    @test _label_contour(Dict(:labelContour => -2))       == 0
    @test _label_contour(Dict(:labelContour => 999))      == LABEL_CONTOUR_MAX
    @test _label_contour(Dict(:labelContour => 2.7))      == 3      # _to_int rounds

    # How much of the z stack a movie shows. `show3D` WINS over a z index: the index is a leftover from
    # the last time 2D was chosen, and dropping it silently would lose the user's slice.
    @test !_show_3d(Dict{Symbol,Any}())
    @test _show_3d(Dict(:show3D => true))
    @test _z_slice(Dict{Symbol,Any}())                       === nothing   # "whatever is showing"
    @test _z_slice(Dict(:zSlice => 4))                       == 4
    @test _z_slice(Dict(:zSlice => -1))                      == 0          # floored, clamped again bridge-side
    @test _z_slice(Dict(:show3D => true, :zSlice => 4))      === nothing   # 3D ignores the index…
    @test _z_slice(Dict(:show3D => false, :zSlice => 4))     == 4          # …and keeps it for next time
end

# Observer (mcp/) event broadcasts — Slice B. Capture WS frames by registering a private queue in
# `_ws_clients` (broadcast_ws puts a serialised frame per client). These frames drive the observer's
# 10-attempts pattern + note/lab-log surfacing (docs/ai-assist/OBSERVER.md §4-5).
