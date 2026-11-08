# ── Pop-type registry (app/src/gating/popmanager/registry.jl) ──────────────────────────────────────
# Every pop-type list derives from POP_TYPE_REGISTRY. These pin the derived lists to the literals they
# replaced (behaviour unchanged), the frontend's JSON copy to the registry, and the two disagreements
# the registry fixed (the observer walk lacked `branch`; the overlay legend lacked `region`, in
# api/test/suite/movie_rail_translators.jl).

@testset "pop-type registry — derived lists match the lists they replaced" begin
    @test STORED_POP_TYPES == ("flow", "track", "clust", "trackclust", "region", "branch")
    @test GATING_POP_TYPES == ("flow", "track")
    @test CLUSTER_POP_TYPES == ("clust", "trackclust", "region")
    @test TRACK_FAMILY_POP_TYPES == ("live", "track", "trackclust")
    @test Cecelia._CLUST_POP_TYPES == ("clust", "trackclust")
    @test POP_TYPE_PROBE_ORDER == ("flow", "clust", "region", "track", "trackclust", "branch")
    @test Set(Cecelia.ACCEPT_TOKENS) == Set(["live", "flow", "clust", "trackclust", "region", "track", "branch"])
    @test Cecelia.POP_MAP_SUFFIX == Dict("track" => "__tracks", "clust" => "__clust",
        "trackclust" => "__trackclust", "region" => "__region", "branch" => "__branch")
    @test Cecelia._scope_accepts("cells", true) == ["live", "clust", "region"]
    @test Cecelia._scope_accepts("tracks", true) == ["track", "trackclust"]
    @test Cecelia._scope_accepts("cells", false) == ["live"]
    # predicates, including the "unknown string → no" answer the `in (...)` forms gave
    @test [pt for pt in string.(instances(PopType)) if is_track_grained(pt)] == ["track", "trackclust"]
    @test !is_track_grained("bogus") && !is_gating_pop_type("bogus") && !Cecelia._is_cluster_pop_type("bogus")
    @test is_track_grained(POP_TRACKCLUST) && is_gating_pop_type(POP_FLOW)
    @test Cecelia._cluster_measure_prefix("region") == "regions."
    @test Cecelia._cluster_measure_prefix("trackclust") == "clusters."
    @test Cecelia._cluster_measure_prefix("flow") == "clusters."
    @test [pop_category(pt, "/a") for pt in ("flow", "live", "track", "clust", "trackclust", "region", "branch", "labels", "bogus")] ==
          ["gated", "gated", "gated", "clustered", "clustered", "region", "gated", "gated", "gated"]
    @test Cecelia._category_of_pop_type("region") == "Clustering"
    @test Cecelia._category_of_pop_type("track") == Cecelia.CATEGORY_GATING
    @test pop_map_type("live") == "flow" && pop_map_type("labels") === nothing && pop_map_type("x") === nothing
    # every enum value has exactly one row
    @test sort([string(i.type) for i in POP_TYPE_REGISTRY]) == sort(collect(string.(instances(PopType))))
end

@testset "pop-type registry — the frontend JSON is the registry written out" begin
    committed = JSON3.read(read(Cecelia.POP_TYPE_REGISTRY_JSON, String))
    wire = JSON3.read(JSON3.write(Cecelia.pop_type_registry_wire()))
    same = committed == wire
    same || @error "frontend/src/plots/popTypeRegistry.json is stale — regenerate it: " *
        "pixi run julia --project=app -e 'using Cecelia; Cecelia.write_pop_type_registry_json()'"
    @test same
end

@testset "pop-type registry — the observer walk covers branch pops, measures skip them" begin
    proj = CciaProject(; uid = "ptrP", name = "ptr"); proj.root = mktempdir()
    s = CciaSet(; uid = "ptrS", dir = mktempdir())
    push!(proj._sets, s); push!(proj.set_uids, s.uid)
    img = CciaImage(; uid = "i1", dir = mktempdir())
    img.label_props = Dict("A" => "A.h5ad")
    mb = PopulationMap(; pop_type = "branch", value_name = "A")
    add_pop!(mb, "endpoints"; filter_measure = "branch-type", filter_fun = "eq", filter_values = 0)
    save_pop_map!(mb, img)
    push!(s._images, img); push!(s.image_uids, img.uid)
    pops = populations_summary(proj).images[1].populations
    @test any(p -> p.name == "endpoints" && p.popType == "branch", pops)
    # a branch-only segmentation still gets its base-population measure summary (no branch target)
    @test !haskey(Cecelia._gated_by_value_name(img), "A")
end
