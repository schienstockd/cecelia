# RESULT-REVIEW: result-4f7d8a2c
# The overlay's µm z must divide back to the cell's voxel z within `zPlaneOf`'s 1e-3 snap
# (frontend/src/utils/viewerOverlays.ts) — otherwise a cell on plane k draws on plane k-1 in the viewer
# while the movie (which floors native voxel z) draws it on plane k, and the channel lens counts it on
# the wrong plane. Rounding µm to 0.01 to shrink the payload broke exactly this: up to 0.005 µm off,
# which is 0.0025 planes on this fixture's 2 µm step and ~0.007 on a 0.6918 µm one. The wire value is
# the µm value at Float32 precision, nothing coarser.

@testset "API: viewer overlay z divides back to the voxel plane (Float32, not rounded)" begin
    fx = fixture_for(:cells, Symbol("3d"), :calibrated)    # z ≠ x calibration, so a plane can be missed
    if fx === nothing
        @test_skip "calibrated cell-table fixture missing"
    else
        old = Cecelia.cecelia_conf()["dirs"]["projects"]
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dirname(fx.project)
            pu, iu = basename(fx.project), basename(fx.dir)
            st, body = api_viewer_overlays(HTTP.Request("GET",
                "/api/viewer/overlays?projectUid=$pu&imageUid=$iu&valueName=$(fx.value_name)"))
            @test st == 200
            d = JSON3.read(body)
            img, _ = _gating_image(pu, iu)
            sizes, _ = img_physical_sizes(img)              # [sz, sy, sx] µm/px
            lp = label_props(fixture_table(fx))
            view_centroid_cols(lp; order = [:x, :y, :z])
            raw = as_df(lp)
            rawz = Dict(Int(raw[i, :label]) => Float64(raw[i, :centroid_z]) for i in 1:size(raw, 1))
            @test "z" in d.axes
            zs = Float64.(d.cells.z); labs = Int.(d.cells.label)
            # every cell: µm z / sz lands within the client's plane snap of its voxel z
            @test maximum(abs(zs[i] / sizes[1] - rawz[labs[i]]) for i in eachindex(zs)) < 1e-3
            # and the wire value is the µm value to Float32 precision — nothing coarser
            @test all(isapprox(zs[i], rawz[labs[i]] * sizes[1]; rtol = 1e-6) for i in eachindex(zs))
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end
