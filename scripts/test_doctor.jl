# Unit test for `scripts/doctor.jl :: missing_julia_pkgs`.
#
# Why it exists: doctor used to pass any env whose Manifest.toml merely EXISTED, so a branch that
# changed the Manifest (a monthly lock refresh) got a green row and `pixi run dev` then died with
# "Package Zarr … is required but does not seem to be installed". The probe must name a Manifest
# entry the depot lacks, and must not flag stdlibs or path deps (no `git-tree-sha1`).
#
# Run: `julia scripts/test_doctor.jl`. Zero deps beyond stdlib.

using Test

include(joinpath(@__DIR__, "doctor.jl"))  # PROGRAM_FILE guard skips main()

function _fake_env(manifest_deps::String)
    dir = mktempdir()
    write(joinpath(dir, "Project.toml"), "name = \"DoctorProbe\"\n")
    write(joinpath(dir, "Manifest.toml"), "julia_version = \"$(VERSION)\"\nmanifest_format = \"2.0\"\n\n" * manifest_deps)
    dir
end

@testset "missing_julia_pkgs" begin
    # A registry entry no depot can hold (random uuid + tree hash) is reported by name.
    absent = _fake_env("""
        [[deps.NotARealPackageXyz]]
        git-tree-sha1 = "0123456789abcdef0123456789abcdef01234567"
        uuid = "8c1e7a52-3b0d-4b7e-9f0a-2d4c6e8f1a3b"
        version = "1.0.0"
        """)
    @test missing_julia_pkgs(absent) == ["NotARealPackageXyz"]

    # A stdlib entry and a path dep carry no git-tree-sha1: never "not installed".
    clean = _fake_env("""
        [[deps.Dates]]
        uuid = "ade2ca70-3891-5945-98fb-dc099432e06a"
        version = "1.11.0"

        [[deps.LocalThing]]
        path = "../nowhere"
        uuid = "5d0b9e3c-1f2a-4c6b-8e7d-9a0b1c2d3e4f"
        version = "0.1.0"
        """)
    @test isempty(missing_julia_pkgs(clean))
end

println("doctor unit tests OK")
