# RESULT-REVIEW: result-090b69ae
# A column written back into a cell table (clustRegions' `regions.*`, contacts, clusters, gating) leaves
# every label naming the same cell, so the neighbour graph built from it is NOT stale — even though the
# table's file is now newer than the graph. Staleness is the mask store being rewritten (re-segmented),
# never the table's mtime: clustRegions reads this very graph and then writes the table, so an mtime
# rule would flag every graph it was used with.

@testset "API: a neighbour graph is not stale after a write-back to its cell table" begin
    src = api_fixture("testpr", "1", "KDIeEm")
    if !api_have_fixture(joinpath(src, "spatialGraph", "net.h5ad"))
        @test_skip "spatialGraph fixtures (missing)"
    else
        conf = cecelia_conf()
        dirs = get!(conf, "dirs", Dict{String,Any}())
        had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
        dirs["projects"] = mktempdir()
        try
            proj = create_project!(name = "graph-stale")
            img = add_image!(add_set!(proj; name = "s"); name = "im", uid = "grphS")
            mkpath(img_label_props_dir(img)); mkpath(img_spatial_graph_dir(img))
            # a mask and its table, then the graph built from them
            mkpath(img_labels_path(img, "B")); write(joinpath(img_labels_path(img, "B"), ".zgroup"), "{}")
            cp(joinpath(src, "labelProps", "B.h5ad"), img_label_props_path(img, "B"))
            sleep(1.1)
            cp(joinpath(src, "spatialGraph", "net.h5ad"), img_spatial_graph_path(img, "net"))
            # a task writes a column back into the table — the same labels, the same cells
            sleep(1.1)
            label_props(img_label_props_path(img, "B")) |>
                v -> add_obs(v, Cecelia.DataFrames.DataFrame(label = [0], regions_r = [1.0])) |> save!
            @test mtime(img_label_props_path(img, "B")) > mtime(img_spatial_graph_path(img, "net"))
            @test !spatial_graph_segments(img, "net").stale
        finally
            had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        end
    end
end
