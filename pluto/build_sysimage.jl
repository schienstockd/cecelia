# Build the DEPS-ONLY sysimage for the notebook Playground (pluto/deps.so).
#
#   pixi run notebooks-sysimage
#
# Deps-only (Locked #6): bakes the stable heavy stack — Makie/CairoMakie + AoG + DataFrames + HDF5 +
# HTTP — but NOT Cecelia, so Revise still hot-reloads Cecelia in dev. For RELEASE, a full sysimage
# (Cecelia included) is built in the packaging flow (docs/SHIPPING.md).
#
# Cost (Phase 0, Linux): ~10 min to build, ~1.4 GB on disk. One-time. deps.so is git-ignored.
import Pkg
Pkg.activate(@__DIR__)
using PackageCompiler

create_sysimage(
    ["CairoMakie", "AlgebraOfGraphics", "DataFrames", "HDF5", "HTTP"];
    sysimage_path = joinpath(@__DIR__, "deps.so"),
    precompile_execution_file = joinpath(@__DIR__, "precompile_workload.jl"),
)
@info "sysimage built" path = joinpath(@__DIR__, "deps.so")
