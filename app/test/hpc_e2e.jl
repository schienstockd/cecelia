# HPC P1 tests on their own: `pixi run test-hpc`. The same file the package suite includes
# (`suite/hpc.jl`), run standalone so the SLURM-container workflow (`.github/workflows/verify-hpc.yml`)
# does not pay for the whole package suite. Its live testset runs only when `CECELIA_HPC_TEST_HOST`
# is set; see the header of `suite/hpc.jl`.

using Test
using Cecelia
using JSON3

# Hermetic, exactly as `runtests.jl` does it: never the user's config dir.
if !haskey(ENV, "CECELIA_DEV_DIR")
    let cfg = mktempdir(), proj = mktempdir()
        write(joinpath(cfg, "custom.toml"), "[dirs]\nprojects = '" * proj * "'\n")
        ENV["CECELIA_DEV_DIR"] = cfg
    end
end
init_cecelia!()

@testset "HPC" begin
    include(joinpath(@__DIR__, "suite", "hpc.jl"))
    include(joinpath(@__DIR__, "suite", "hpc_remote.jl"))
    include(joinpath(@__DIR__, "suite", "hpc_pbs.jl"))
    include(joinpath(@__DIR__, "suite", "label_import.jl"))    # its fixture project, for dispatch
    include(joinpath(@__DIR__, "suite", "hpc_dispatch.jl"))
    include(joinpath(@__DIR__, "suite", "hpc_job.jl"))
end
