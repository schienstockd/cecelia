# RESULT-REVIEW: result-be985135
# A kNN neighbour graph is DIRECTED: a cell's k nearest need not list it back, so squidpy's CSR holds
# many edges one way only. Reading the upper triangle alone dropped them silently. Fixture: the real
# `cellNeighbours` builder, kNN k = 3 per frame over B.h5ad frames 0–2 (`make_spatial_graph_fixture.py`):
# 594 stored entries, 369 pairs linked in either direction (scipy's `(A + Aᵀ > 0)`, nnz / 2).
@testset "neighbour-graph edges: a directed kNN graph keeps every edge" begin
    knn = fixture_path("testpr", "1", "KDIeEm", "spatialGraph", "knn.h5ad")
    if !have_fixture(knn)
        @test_skip "spatialGraph/knn fixture (missing)"
    else
        g = spatial_graph_edges(knn)
        @test length(g.i) == 369
        @test all(g.i .< g.j) && length(Set(zip(g.i, g.j))) == 369
        # k = 3 out-edges per cell: every cell has at least 3 neighbours once both directions count
        deg = zeros(Int, length(g.label))
        for (a, b) in zip(g.i, g.j); deg[a] += 1; deg[b] += 1; end
        @test minimum(deg) >= 3
    end
end
