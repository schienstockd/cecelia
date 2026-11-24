# RESULT-REVIEW: result-4425ab6a
# Claude moving one region of its own slice / quadrant with set-own-gate: the region keeps the cells on its
# side. A slice drawn just under 45° ([[0,0],[1,0.9]]) and re-sent just over it ([[0,0],[0.9,1]]) runs the
# same way, so nothing flips — the `+`/`-` naming rule's 45° switch is not a property of the cells. Re-sent
# reversed, it is turned back. A quadrant re-sent with arms from +y is rotated back to start at +x.
# Expected values from the geometry (side = left of the vertex direction; sector q = arm q → q+1).
@testset "API: set-own-gate keeps a split region on its side (re-sent dividers re-oriented)" begin
    lin = Dict("kind" => "linear")
    slice(v; side = 1) = gate_from_spec(Dict{String,Any}("kind" => "slice", "x_channel" => "a", "y_channel" => "b",
        "x_transform" => lin, "y_transform" => lin, "vertices" => v, "side" => side, "group" => "g"))
    cur = slice([[0.0, 0.0], [1.0, 0.9]])                 # side 1 = left of (1, 0.9) = up-left
    at(g, x, y) = Cecelia.inside(g, [x], [y])[1]
    upleft(g) = at(g, -1.0, 1.0) && !at(g, 1.0, -1.0)
    @test upleft(cur)
    g, err = _keep_split_region(cur, slice([[0.0, 0.0], [0.9, 1.0]]), "/sl+")
    @test err === nothing && g.side == 1 && g.group == "g" && upleft(g)
    g, _ = _keep_split_region(cur, slice([[0.9, 1.0], [0.0, 0.0]]), "/sl+")     # reversed → turned back
    @test upleft(g) && first(g.vertices) == (0.0, 0.0)

    quad(arms; q = 1) = gate_from_spec(Dict{String,Any}("kind" => "quadrant", "x_channel" => "a", "y_channel" => "b",
        "x_transform" => lin, "y_transform" => lin, "centre" => [0.0, 0.0], "arms" => arms, "quadrant" => q, "group" => "g"))
    qc = quad([[1, 0], [0, 1], [-1, 0], [0, -1]])          # sector 1 = x+ y+
    g, _ = _keep_split_region(qc, quad([[0, 1], [-1, 0], [0, -1], [1, 0]]), "/q++")
    @test at(g, 1.0, 1.0) && !at(g, -1.0, 1.0)
end
