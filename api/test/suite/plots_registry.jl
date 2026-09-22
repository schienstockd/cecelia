# Live plot registry testset (BIDIR PR #8) — extracted from api/test/runtests.jl.
#
# Covers `api/src/plots_registry_api.jl` + the WS disconnect hook wired in `api/src/server.jl`:
#  - register puts an entry into the bag; list returns it
#  - same plotId + different clientId → last-writer-wins (only one entry)
#  - deregister with a matching (plotId, clientId) removes; a stale deregister does not
#  - WS disconnect hook drops all entries for that clientId; entries for other clientIds survive
#  - guards: empty plotId / missing projectUid / non-existent project return 400/404
#
# No `viewer:hello` frame is exercised here (that path is covered by the enum round-trip in
# `observer_taskframes.jl`); this file drives `set_ws_client_id!` + the hook directly, which is
# what the finally block would do on a real disconnect.

@testset "API: viewer/plots — register / deregister / list + WS-disconnect cleanup" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _reset_plots_registry_for_test!()
    try
        uid = "TESTPLOTS"; mkpath(joinpath(tmp, uid))

        # ── register: guards ────────────────────────────────────────────────
        r(b) = _post(api_viewer_plots_register, b)
        @test r(Dict("plotId"=>"pk"))[1] == 400                                          # projectUid required
        @test r(Dict("projectUid"=>"NOPE", "clientId"=>"c", "plotId"=>"pk", "family"=>"umap"))[1] == 404
        @test r(Dict("projectUid"=>uid, "clientId"=>"c", "family"=>"umap"))[1] == 400   # plotId required
        @test r(Dict("projectUid"=>uid, "plotId"=>"pk", "family"=>"umap"))[1] == 400    # clientId required
        @test r(Dict("projectUid"=>uid, "clientId"=>"c", "plotId"=>"pk"))[1] == 400     # family required

        # ── register: happy path lands in the bag, list returns it ──────────
        st, body = r(Dict("projectUid"=>uid, "clientId"=>"tabA", "plotId"=>"panel:7",
                          "family"=>"umap", "title"=>"UMAP", "route"=>"/analysis",
                          "cellKeys"=>["A1", "B2"],
                          "bboxScreen"=>Dict("x"=>10, "y"=>20, "w"=>400, "h"=>300)))
        @test st == 200
        @test JSON3.read(body).ok == true

        gl(qs) = api_viewer_plots_list(HTTP.Request("GET", "/api/viewer/plots?" * qs))
        @test gl("")[1] == 400
        @test gl("projectUid=NOPE")[1] == 404
        st2, body2 = gl("projectUid=$uid")
        @test st2 == 200
        items = JSON3.read(body2).items
        @test length(items) == 1
        row = items[1]
        @test String(row.plotId) == "panel:7"
        @test String(row.clientId) == "tabA"
        @test String(row.family) == "umap"
        @test String(row.title) == "UMAP"
        @test String(row.route) == "/analysis"
        @test collect(String, row.cellKeys) == ["A1", "B2"]
        @test Float64(row.bboxScreen.x) == 10.0 && Float64(row.bboxScreen.w) == 400.0

        # ── same plotId, different clientId → last-writer-wins ──────────────
        st3, _ = r(Dict("projectUid"=>uid, "clientId"=>"tabB", "plotId"=>"panel:7",
                        "family"=>"umap", "title"=>"UMAP (tabB)", "route"=>"/analysis"))
        @test st3 == 200
        items2 = JSON3.read(gl("projectUid=$uid")[2]).items
        @test length(items2) == 1
        @test String(items2[1].clientId) == "tabB"
        @test String(items2[1].title) == "UMAP (tabB)"

        # ── deregister: stale (wrong clientId) does NOT remove ──────────────
        d(b) = _post(api_viewer_plots_deregister, b)
        @test d(Dict("projectUid"=>uid, "clientId"=>"tabA", "plotId"=>"panel:7"))[1] == 200
        items3 = JSON3.read(gl("projectUid=$uid")[2]).items
        @test length(items3) == 1
        @test String(items3[1].clientId) == "tabB"

        # ── deregister: matching (plotId, clientId) removes ─────────────────
        @test d(Dict("projectUid"=>uid, "clientId"=>"tabB", "plotId"=>"panel:7"))[1] == 200
        items4 = JSON3.read(gl("projectUid=$uid")[2]).items
        @test isempty(items4)
        # Idempotent — a second matching deregister is still 200 (entry already gone).
        @test d(Dict("projectUid"=>uid, "clientId"=>"tabB", "plotId"=>"panel:7"))[1] == 200

        # ── deregister: guards ──────────────────────────────────────────────
        @test d(Dict("projectUid"=>uid, "plotId"=>"pk"))[1] == 400                    # clientId required
        @test d(Dict("clientId"=>"c", "plotId"=>"pk"))[1] == 400                       # projectUid required
        @test d(Dict("projectUid"=>uid, "clientId"=>"c"))[1] == 400                    # plotId required
        @test d(Dict("projectUid"=>"NOPE", "clientId"=>"c", "plotId"=>"pk"))[1] == 404

        # ── WS-disconnect hook drops THIS clientId's entries only ───────────
        _reset_plots_registry_for_test!()
        # Two panels registered by different tabs; both live.
        r(Dict("projectUid"=>uid, "clientId"=>"tabA", "plotId"=>"pA",
               "family"=>"umap", "title"=>"A", "route"=>"/analysis"))
        r(Dict("projectUid"=>uid, "clientId"=>"tabB", "plotId"=>"pB",
               "family"=>"umap", "title"=>"B", "route"=>"/analysis"))
        r(Dict("projectUid"=>uid, "clientId"=>"tabA", "plotId"=>"pA2",
               "family"=>"gate-scatter", "title"=>"A2", "route"=>"/gating"))
        @test length(JSON3.read(gl("projectUid=$uid")[2]).items) == 3

        # A real disconnect stamps the id via `set_ws_client_id!` and later the finally block
        # snapshots it. Here we call the registered hook directly with (dummy_ws, "tabA").
        _plots_registry_ws_disconnect_hook(gensym("dummy-ws"), "tabA")
        after = JSON3.read(gl("projectUid=$uid")[2]).items
        @test length(after) == 1
        @test String(after[1].plotId) == "pB"

        # A hook fire with a `nothing` client id is a no-op (a tab that never sent
        # `viewer:hello`); no entries lost.
        _plots_registry_ws_disconnect_hook(gensym("dummy-ws"), nothing)
        @test length(JSON3.read(gl("projectUid=$uid")[2]).items) == 1

        # ── set_ws_client_id! round-trips through ws_client_id ──────────────
        ws_key = gensym("test-hello-ws")
        set_ws_client_id!(ws_key, "tabX")
        @test ws_client_id(ws_key) == "tabX"

        # ── bboxScreen malformed → dropped, entry still registers ───────────
        _reset_plots_registry_for_test!()
        st_bb, _ = r(Dict("projectUid"=>uid, "clientId"=>"tabC", "plotId"=>"pBB",
                          "family"=>"umap", "title"=>"", "route"=>"/analysis",
                          "bboxScreen"=>Dict("x"=>"nope", "y"=>0, "w"=>10, "h"=>10)))
        @test st_bb == 200
        row_bb = JSON3.read(gl("projectUid=$uid")[2]).items[1]
        @test !haskey(row_bb, :bboxScreen)

        # ── content: registered without `content` → GET returns `{}` (stable shape) ─
        _reset_plots_registry_for_test!()
        r(Dict("projectUid"=>uid, "clientId"=>"tabD", "plotId"=>"pNoC",
               "family"=>"summary", "title"=>"", "route"=>"/analysis"))
        row_nc = JSON3.read(gl("projectUid=$uid")[2]).items[1]
        @test haskey(row_nc, :content)
        @test isempty(row_nc.content)

        # ── content: primitive round-trip lands verbatim ────────────────────
        _reset_plots_registry_for_test!()
        r(Dict("projectUid"=>uid, "clientId"=>"tabE", "plotId"=>"pC",
               "family"=>"summary", "title"=>"Track measures", "route"=>"/analysis",
               "content"=>Dict("measure"=>"speed", "chartType"=>"box",
                               "statsEnabled"=>true, "valueNames"=>["default","flowTom"])))
        row_c = JSON3.read(gl("projectUid=$uid")[2]).items[1]
        @test String(row_c.content.measure) == "speed"
        @test String(row_c.content.chartType) == "box"
        @test row_c.content.statsEnabled == true
        @test collect(String, row_c.content.valueNames) == ["default", "flowTom"]

        # ── content: oversized single value → dropped, other keys survive ──
        _reset_plots_registry_for_test!()
        big = repeat("x", 4096)                                         # > 2 KiB after JSON quotes
        r(Dict("projectUid"=>uid, "clientId"=>"tabF", "plotId"=>"pBig",
               "family"=>"summary", "title"=>"", "route"=>"/analysis",
               "content"=>Dict("measure"=>"speed", "huge"=>big)))
        row_big = JSON3.read(gl("projectUid=$uid")[2]).items[1]
        @test String(row_big.content.measure) == "speed"
        @test !haskey(row_big.content, :huge)

        # ── content: oversized whole bag → dropped wholesale ────────────────
        _reset_plots_registry_for_test!()
        halfk = repeat("y", 1024)                                       # ~1 KiB each, 10× > 8 KiB
        bag = Dict("k$i"=>halfk for i in 1:10)
        r(Dict("projectUid"=>uid, "clientId"=>"tabG", "plotId"=>"pFat",
               "family"=>"summary", "title"=>"", "route"=>"/analysis",
               "content"=>bag))
        row_fat = JSON3.read(gl("projectUid=$uid")[2]).items[1]
        @test isempty(row_fat.content)

        # ── content: non-primitive garbage → skipped silently; other keys kept ──
        _reset_plots_registry_for_test!()
        # a Dict at depth 3 (over the depth cap) drops; a mixed vector drops; a good primitive stays
        r(Dict("projectUid"=>uid, "clientId"=>"tabH", "plotId"=>"pMix",
               "family"=>"summary", "title"=>"", "route"=>"/analysis",
               "content"=>Dict(
                    "measure"=>"speed",
                    "deep"=>Dict("a"=>Dict("b"=>Dict("c"=>1))),         # depth 3, over cap
                    "mixed"=>Any[1, Dict("x"=>1)],                      # vector containing a dict
               )))
        row_mix = JSON3.read(gl("projectUid=$uid")[2]).items[1]
        @test String(row_mix.content.measure) == "speed"
        @test !haskey(row_mix.content, :mixed)
        # a depth-3 nested dict is trimmed at the depth cap — the outer `deep` key survives with
        # its innermost dict discarded (the `a` value is over-depth so it drops out). The exact
        # shape after the depth cap can be a `{}` or a leaf that lost its overdeep value; what
        # matters is that no primitive smuggled its way through past the cap.
        @test !haskey(row_mix.content, :deep) || isempty(row_mix.content.deep)

        # ── content: re-register same plotId → last-writer wins on content ──
        _reset_plots_registry_for_test!()
        r(Dict("projectUid"=>uid, "clientId"=>"tabI", "plotId"=>"pRe",
               "family"=>"summary", "title"=>"", "route"=>"/analysis",
               "content"=>Dict("measure"=>"speed")))
        r(Dict("projectUid"=>uid, "clientId"=>"tabI", "plotId"=>"pRe",
               "family"=>"summary", "title"=>"", "route"=>"/analysis",
               "content"=>Dict("measure"=>"displacement")))
        row_re = JSON3.read(gl("projectUid=$uid")[2]).items[1]
        @test String(row_re.content.measure) == "displacement"

        # ── content: non-dict body value → silently coerced to `{}` (garbage in, empty out) ─
        _reset_plots_registry_for_test!()
        r(Dict("projectUid"=>uid, "clientId"=>"tabJ", "plotId"=>"pStr",
               "family"=>"summary", "title"=>"", "route"=>"/analysis",
               "content"=>"not a dict"))
        row_str = JSON3.read(gl("projectUid=$uid")[2]).items[1]
        @test isempty(row_str.content)
    finally
        _reset_plots_registry_for_test!()
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end
