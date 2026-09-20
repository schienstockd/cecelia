# Gating mutation + undo/redo testsets — extracted from api/test/runtests.jl.
#
# Four testsets covering /api/gating/* mutations + hand-drawn gating history:
#  - `API: pop/move re-parents, pop/delete childrenOnly prunes`.
#  - `API: gating plotdata carries a colour-by measure` (COLOUR BY a third measure).
#  - `API: boolean populations combine, update and block an orphaning delete` (Decision 16).
#  - `API: gating undo/redo steps through the population tree` (ring of snapshots at the
#    single _persist_and_broadcast! chokepoint).
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

# ── Undo / redo for hand-drawn gating ────────────────────────────────────────────────────────
# The whole population tree is one serialisable document, so history is a ring of snapshots taken at
# the one choke point every mutation already goes through (`_persist_and_broadcast!`). What this
# pins is the contract that makes that safe: a step must not record ITSELF as an edit (or undo would
# only ever toggle the last change), a fresh edit must drop the redo branch, and the pop types whose
# edit is a re-tickable filter must not get history at all.
# ── pop/move + pop/delete childrenOnly ───────────────────────────────────────────────────────────
# The population manager's ⋯ menu: re-parent a population (gate kept, membership re-derived) and prune
# the subtree under one without deleting it. Both are ordinary mutations, so they persist, broadcast
# and record undo history like any other — asserted here on the ON-DISK document, not just the reply.
@testset "API: pop/move re-parents, pop/delete childrenOnly prunes" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "testpr fixture missing"
  else
    dir = mktempdir()
    proj = joinpath(dir, "testpr")
    cp(api_fixture("testpr"), proj)
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    empty!(_GATING_HISTORY)
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        vn = "B"
        base = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                "valueName" => vn, "popType" => "flow")
        post(h, extra) = h(Vector{UInt8}(JSON3.write(merge(base, extra))))
        gate(xmax) = Dict{String,Any}("kind" => "rectangle", "x_channel" => "c1", "y_channel" => "c2",
                                      "x_min" => 0.0, "x_max" => xmax, "y_min" => 0.0, "y_max" => 1.0)
        onDisk() = Set(pop_paths(load_pop_map(joinpath(proj, "1", "KDIeEm"), vn; pop_type = "flow")))

        post(api_gating_pop_add, Dict{String,Any}("name" => "qc", "gate" => gate(1.0)))
        post(api_gating_pop_add, Dict{String,Any}("name" => "B", "parent" => "/qc", "gate" => gate(2.0)))
        post(api_gating_pop_add, Dict{String,Any}("name" => "mem", "parent" => "/qc/B", "gate" => gate(3.0)))
        @test onDisk() == Set(["/qc", "/qc/B", "/qc/B/mem"])

        # lift B out of qc — the whole subtree comes with it, and the reply names the new path
        st, b = post(api_gating_pop_move, Dict{String,Any}("path" => "/qc/B", "parent" => "root"))
        @test st == 200 && String(JSON3.read(b).path) == "/B"
        @test onDisk() == Set(["/qc", "/B", "/B/mem"])

        # rejected moves leave the document alone
        st, _ = post(api_gating_pop_move, Dict{String,Any}("path" => "/B", "parent" => "/B/mem"))
        @test st == 400                                             # into its own subtree = a cycle
        st, _ = post(api_gating_pop_move, Dict{String,Any}("path" => "/nope", "parent" => "root"))
        @test st == 404
        st, _ = post(api_gating_pop_move, Dict{String,Any}("path" => "/B"))
        @test st == 400                                             # parent is required
        @test onDisk() == Set(["/qc", "/B", "/B/mem"])

        # a move is undoable like any other edit (history rides on _persist_and_broadcast!)
        st, _ = post(api_gating_undo, Dict{String,Any}())
        @test st == 200 && onDisk() == Set(["/qc", "/qc/B", "/qc/B/mem"])
        st, _ = post(api_gating_redo, Dict{String,Any}())
        @test st == 200 && onDisk() == Set(["/qc", "/B", "/B/mem"])

        # childrenOnly prunes BELOW the pop; the pop itself survives (plain delete takes it along)
        st, _ = post(api_gating_pop_delete, Dict{String,Any}("path" => "/B", "childrenOnly" => true))
        @test st == 200 && onDisk() == Set(["/qc", "/B"])
        st, _ = post(api_gating_pop_delete, Dict{String,Any}("path" => "/B"))
        @test st == 200 && onDisk() == Set(["/qc"])
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        empty!(_GATING_HISTORY)
    end
  end
end

# ── COLOUR BY a third measure ────────────────────────────────────────────────────
# The dots keep their positions and gain a value: `plotdata?z=…` answers TRIPLES instead of pairs,
# read in one pass so `z[i]` is the same cell as `(x[i], y[i])`. A stride slip here mis-colours every
# dot with a plausible-looking picture, so pin the pair half against the no-z response, and pin every
# value inside the ramp `plotmeta` hands the legend.
@testset "API: gating plotdata carries a colour-by measure" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "testpr fixture missing"
  else
    dir = mktempdir(); cp(api_fixture("testpr"), joinpath(dir, "testpr"))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        common = "projectUid=testpr&imageUid=KDIeEm&valueName=B&popType=flow"
        st, chb = api_gating_channels(HTTP.Request("GET", "/api/gating/channels?" * common))
        @test st == 200
        cols = String.(JSON3.read(chb).columns)
        @test length(cols) >= 3
        x, y, z = cols[1], cols[2], cols[3]
        base = "$common&x=$(HTTP.escapeuri(x))&y=$(HTTP.escapeuri(y))"
        data(t) = (r = api_gating_plotdata(HTTP.Request("GET", "/api/gating/plotdata?" * t));
                   (r[1], reinterpret(Float32, UInt8.(r[2]))))
        st1, xy = data(base)
        st2, xyz = data("$base&z=$(HTTP.escapeuri(z))")
        n = length(xy) ÷ 2
        @test st1 == 200 && st2 == 200 && n > 0
        @test length(xyz) == 3n                                  # triples, not pairs
        # asking for a colour does not move a single dot
        @test all(i -> xyz[3i-2] == xy[2i-1] && xyz[3i-1] == xy[2i], 1:n)
        meta = JSON3.read(api_gating_plotmeta(HTTP.Request("GET", "/api/gating/plotmeta?$base&z=$(HTTP.escapeuri(z))"))[2])
        @test meta.zExtent !== nothing && length(meta.zTicks) == 3 && meta.usedZ == "linear"
        lo, hi = Float32(meta.zExtent[1]), Float32(meta.zExtent[2])
        # The ramp is a CONTRAST setting, not an axis: a 2–98 percentile clip, so it sits INSIDE the
        # data range and holds the bulk of it. (Full min…max spent ~70% of the colour scale on outliers
        # on real data — see `_ramp_range`.) Outliers are not dropped; they clamp to the ramp's ends.
        zs = Float32[xyz[3i] for i in 1:n]
        @test minimum(zs) <= lo && hi <= maximum(zs)
        @test count(v -> lo <= v <= hi, zs) >= 0.9 * n
        @test hi > lo
        # no colour measure asked for → the response says nothing about a ramp (the client falls back
        # to the density pseudocolour rather than inventing a range)
        @test JSON3.read(api_gating_plotmeta(HTTP.Request("GET", "/api/gating/plotmeta?" * base))[2]).zExtent === nothing
        # An UNREADABLE colour measure (a stale panel naming a column this table doesn't have) tints
        # nothing — it must not blank the cloud it was only supposed to colour. Triples still (the client
        # chose the stride), values NaN, and no ramp to describe.
        st3, bogus = data("$base&z=live.track.nope")
        @test st3 == 200 && length(bogus) == 3n
        @test all(i -> bogus[3i-2] == xy[2i-1] && bogus[3i-1] == xy[2i] && isnan(bogus[3i]), 1:n)
        @test JSON3.read(api_gating_plotmeta(HTTP.Request("GET", "/api/gating/plotmeta?$base&z=live.track.nope"))[2]).zExtent === nothing
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
  end
end

# ── boolean populations over the API (Decision 16) ───────────────────────────────────────────────
# "positive for nuc-GFP OR mem-TOM", "double positive but NOT CD169" — a population defined by
# combining others, created through pop/add and rewritten through pop/update like any other. The one
# thing only the API can enforce: a delete that would leave a combination pointing at nothing.
@testset "API: boolean populations combine, update and block an orphaning delete" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "testpr fixture missing"
  else
    dir = mktempdir()
    proj = joinpath(dir, "testpr")
    cp(api_fixture("testpr"), proj)
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    empty!(_GATING_HISTORY)
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        vn = "B"
        base = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                "valueName" => vn, "popType" => "flow")
        post(h, extra) = h(Vector{UInt8}(JSON3.write(merge(base, extra))))
        gate(xmax) = Dict{String,Any}("kind" => "rectangle", "x_channel" => "c1", "y_channel" => "c2",
                                      "x_min" => 0.0, "x_max" => xmax, "y_min" => 0.0, "y_max" => 1.0)
        loaded() = load_pop_map(joinpath(proj, "1", "KDIeEm"), vn; pop_type = "flow")

        post(api_gating_pop_add, Dict{String,Any}("name" => "gfp+", "gate" => gate(1.0)))
        post(api_gating_pop_add, Dict{String,Any}("name" => "tom+", "gate" => gate(2.0)))
        st, _ = post(api_gating_pop_add,
                     Dict{String,Any}("name" => "either", "colour" => "#abc",
                                      "boolean" => Dict{String,Any}("op" => "or",
                                                                    "pops" => ["/gfp+", "/tom+"])))
        @test st == 200
        p = pop_at(loaded(), "/either")
        @test p.boolean_op == Cecelia.BOOL_OR && p.boolean_pops == ["/gfp+", "/tom+"] && p.gate === nothing

        # rewritten wholesale by pop/update — including an exclusion ("but not …")
        st, _ = post(api_gating_pop_update,
                     Dict{String,Any}("path" => "/either",
                                      "boolean" => Dict{String,Any}("op" => "and", "pops" => ["/gfp+"],
                                                                    "not" => ["/tom+"])))
        @test st == 200
        p = pop_at(loaded(), "/either")
        @test p.boolean_op == Cecelia.BOOL_AND && p.boolean_pops == ["/gfp+"] && p.boolean_not == ["/tom+"]

        # a reference that isn't a population, a loop, and an empty term list are all 400s
        st, _ = post(api_gating_pop_add,
                     Dict{String,Any}("name" => "bad",
                                      "boolean" => Dict{String,Any}("op" => "or", "pops" => ["/nope"])))
        @test st == 400
        st, _ = post(api_gating_pop_update,
                     Dict{String,Any}("path" => "/either",
                                      "boolean" => Dict{String,Any}("op" => "or", "pops" => ["/either"])))
        @test st == 400
        st, _ = post(api_gating_pop_add,
                     Dict{String,Any}("name" => "bad2",
                                      "boolean" => Dict{String,Any}("op" => "or", "pops" => [])))
        @test st == 400

        # deleting a combined population would leave "either" pointing at nothing → refused, by name
        st, b = post(api_gating_pop_delete, Dict{String,Any}("path" => "/tom+"))
        @test st == 400 && occursin("either", String(b))
        @test has_pop(loaded(), "/tom+")
        # …but renaming it is fine: the reference is rewritten with the path
        st, _ = post(api_gating_pop_rename, Dict{String,Any}("path" => "/tom+", "newName" => "TOM+"))
        @test st == 200 && pop_at(loaded(), "/either").boolean_not == ["/TOM+"]
        # clearing the combination releases the hold, and then the delete goes through
        st, _ = post(api_gating_pop_update, Dict{String,Any}("path" => "/either", "boolean" => nothing))
        @test st == 200 && pop_at(loaded(), "/either").boolean_op === nothing
        st, _ = post(api_gating_pop_delete, Dict{String,Any}("path" => "/TOM+"))
        @test st == 200 && !has_pop(loaded(), "/TOM+")
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        empty!(_GATING_HISTORY)
    end
  end
end

@testset "API: gating undo/redo steps through the population tree" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "testpr fixture missing"
  else
    dir = mktempdir()
    proj = joinpath(dir, "testpr")
    cp(api_fixture("testpr"), proj)
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    empty!(_GATING_HISTORY)
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        # `_resolve_vn` falls back to the image's ACTIVE segmentation when the requested value_name
        # is not one of its label_props — so use the real one, or every write lands under a different
        # name than the one read back here.
        vn = "B"
        base = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                "valueName" => vn, "popType" => "flow")
        post(h, extra) = h(Vector{UInt8}(JSON3.write(merge(base, extra))))
        gate(xmax) = Dict{String,Any}("kind" => "rectangle", "x_channel" => "c1", "y_channel" => "c2",
                                      "x_min" => 0.0, "x_max" => xmax, "y_min" => 0.0, "y_max" => 1.0)
        names(body) = [String(p.name) for p in JSON3.read(body).tree.populations]

        # nothing done yet → nothing to undo, and saying so is a 409, not a crash
        st, _ = post(api_gating_undo, Dict{String,Any}())
        @test st == 409

        st, b1 = post(api_gating_pop_add, Dict{String,Any}("name" => "cd4", "gate" => gate(1.0)))
        @test st == 200 && names(b1) == ["cd4"]
        @test JSON3.read(b1).canUndo && !JSON3.read(b1).canRedo   # the edit's own response says so
        st, b2 = post(api_gating_pop_add, Dict{String,Any}("name" => "cd8", "gate" => gate(2.0)))
        @test st == 200 && Set(names(b2)) == Set(["cd4", "cd8"])

        # one step back = the state before the LAST edit, not a toggle: undo twice reaches empty
        st, b3 = post(api_gating_undo, Dict{String,Any}())
        @test st == 200 && names(b3) == ["cd4"]
        @test JSON3.read(b3).canRedo
        st, b4 = post(api_gating_undo, Dict{String,Any}())
        @test st == 200 && isempty(names(b4))
        @test !JSON3.read(b4).canUndo
        st, _ = post(api_gating_undo, Dict{String,Any}())
        @test st == 409                                            # exhausted, not wrapped around

        # forward again, and it is the same tree — history restores, it does not re-run the edit
        st, b5 = post(api_gating_redo, Dict{String,Any}())
        @test st == 200 && names(b5) == ["cd4"]
        st, b6 = post(api_gating_redo, Dict{String,Any}())
        @test st == 200 && Set(names(b6)) == Set(["cd4", "cd8"])
        @test !JSON3.read(b6).canRedo

        # it is the ON-DISK document that moved, not just the response
        m = load_pop_map(joinpath(proj, "1", "KDIeEm"), vn; pop_type = "flow")
        @test Set(pop_name.(pop_paths(m))) == Set(["cd4", "cd8"])

        # a NEW edit after an undo drops the redo branch — the future you did not take is gone
        st, _  = post(api_gating_undo, Dict{String,Any}())
        st, b7 = post(api_gating_pop_add, Dict{String,Any}("name" => "cd19", "gate" => gate(3.0)))
        @test st == 200 && Set(names(b7)) == Set(["cd4", "cd19"])
        @test !JSON3.read(b7).canRedo
        st, _ = post(api_gating_redo, Dict{String,Any}())
        @test st == 409

        # a gate EDIT is undoable the same way a structural one is (the case this feature exists for)
        st, _  = post(api_gating_pop_set_gate, Dict{String,Any}("path" => "/cd4", "gate" => gate(9.0)))
        m2 = load_pop_map(joinpath(proj, "1", "KDIeEm"), vn; pop_type = "flow")
        @test pop_at(m2, "/cd4").gate.x_max == 9.0
        st, _  = post(api_gating_undo, Dict{String,Any}())
        m3 = load_pop_map(joinpath(proj, "1", "KDIeEm"), vn; pop_type = "flow")
        @test st == 200 && pop_at(m3, "/cd4").gate.x_max == 1.0

        # …including a change of gate KIND, which is what the panel's rectangle ⇄ polygon convert
        # does (same `pop/set-gate` route). Worth its own step: undoing it has to bring back a
        # RectangleGate through the snapshot, not just different numbers in the same struct.
        st, _  = post(api_gating_pop_set_gate, Dict{String,Any}("path" => "/cd4",
            "gate" => Dict{String,Any}("kind" => "polygon", "x_channel" => "c1", "y_channel" => "c2",
                                       "vertices" => [[0.0, 0.0], [1.0, 0.0], [1.0, 1.0]])))
        m4 = load_pop_map(joinpath(proj, "1", "KDIeEm"), vn; pop_type = "flow")
        @test st == 200 && pop_at(m4, "/cd4").gate isa PolygonGate
        st, _  = post(api_gating_undo, Dict{String,Any}())
        m5 = load_pop_map(joinpath(proj, "1", "KDIeEm"), vn; pop_type = "flow")
        @test st == 200 && pop_at(m5, "/cd4").gate isa RectangleGate
        @test pop_at(m5, "/cd4").gate.x_max == 1.0

        # filter pops (cluster / region) are OUT of scope: their edit is a tick you can un-tick, and
        # they mirror set-wide, so there is nothing coherent to step back on one image
        st, body = post(api_gating_undo, Dict{String,Any}("popType" => "clust"))
        @test st == 400 && occursin("flow/track", String(body))
        # …and a cluster edit records no history it could later claim to undo
        st, bc = post(api_gating_pop_add, Dict{String,Any}("popType" => "clust", "name" => "c1",
            "filter" => Dict{String,Any}("measure" => "clusters.default", "fun" => "in", "values" => [0])))
        @test st == 200 && !JSON3.read(bc).canUndo
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        empty!(_GATING_HISTORY)
    end
  end
end

