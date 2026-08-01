# Build the FULL (release) sysimage for the notebook Playground: deps + Cecelia + CeceliaNb baked in,
# for near-instant first plot AND first `pop_df` on a shipped install.
#
#   pixi run notebooks-sysimage-full
#
# Difference from the deps-only build (build_sysimage.jl): that one deliberately EXCLUDES Cecelia, so
# workers load it from source and see app/src edits. This one bakes Cecelia + CeceliaNb in too, which
# FREEZES them at build time — correct for a release, and wrong for dev (the stamp records which, so
# launch.jl can warn). Wire this into the packaging flow (docs/SHIPPING.md); output is git-ignored.
import Pkg
Pkg.activate(@__DIR__)
using PackageCompiler
include(joinpath(@__DIR__, "sysimage_stamp.jl"))

create_sysimage(
    ["CairoMakie", "AlgebraOfGraphics", "DataFrames", "HDF5", "HTTP", "CSV", "Cecelia", "CeceliaNb"];
    sysimage_path = joinpath(@__DIR__, "deps.so"),
    precompile_execution_file = joinpath(@__DIR__, "precompile_workload_full.jl"),
)
write_sysimage_stamp(@__DIR__, "full")   # "full" so launch.jl can say Cecelia is baked in (frozen)
@info "full sysimage built" path = joinpath(@__DIR__, "deps.so")
