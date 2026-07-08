# Build the FULL (release) sysimage for the notebook Playground: deps + Cecelia + CeceliaNb baked in,
# for near-instant first plot AND first `pop_df` on a shipped install.
#
#   pixi run notebooks-sysimage-full
#
# Difference from the deps-only build (build_sysimage.jl): that one deliberately EXCLUDES Cecelia so
# Revise can hot-reload it in dev. This one bakes Cecelia + CeceliaNb in too — correct for a release
# where the code is frozen. Wire this into the packaging flow (docs/SHIPPING.md); output is git-ignored.
import Pkg
Pkg.activate(@__DIR__)
using PackageCompiler

create_sysimage(
    ["CairoMakie", "AlgebraOfGraphics", "DataFrames", "HDF5", "HTTP", "CSV", "Cecelia", "CeceliaNb"];
    sysimage_path = joinpath(@__DIR__, "deps.so"),
    precompile_execution_file = joinpath(@__DIR__, "precompile_workload_full.jl"),
)
@info "full sysimage built" path = joinpath(@__DIR__, "deps.so")
