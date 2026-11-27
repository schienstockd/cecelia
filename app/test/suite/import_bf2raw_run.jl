# bioformats2raw runner + the Imaris attribute-heap retry (`tasks/importImages/omezarr/bf2raw_run.jl`).
# Included by suite.jl — no outer @testset wrapper.

@testset "bioformats2raw runner and the Imaris heap retry" begin
    # Lines copied from real bioformats2raw 0.12.1 output on an Imaris file whose dense attribute
    # storage netcdf-java cannot read — the two faces of the same reader failure.
    invalid_arg = ["java.io.IOException: Invalid argument",
                   "    at java.base/java.io.RandomAccessFile.seek0(Native Method)",
                   "    at ucar.nc2.iosp.hdf5.FractalHeap.readIndirectBlock(FractalHeap.java:453)",
                   "    at loci.formats.in.ImarisHDFReader.initFile(ImarisHDFReader.java:260)"]
    frhp = ["java.lang.IllegalStateException:      should equal FRHP",
            "\tat ucar.nc2.iosp.hdf5.FractalHeap.<init>(FractalHeap.java:115)"]
    # A different Imaris failure (the soft-link one, `ims_relink`) must NOT take this retry.
    softlink = ["loci.formats.FormatException: Unknown pixel type: null",
                "    at loci.formats.in.ImarisHDFReader.initFile(ImarisHDFReader.java:339)"]

    @testset "_ims_heap_failure keys on the netcdf-java frame, Imaris only" begin
        @test Cecelia._ims_heap_failure("/data/a.ims", invalid_arg)
        @test Cecelia._ims_heap_failure("/data/A.IMS", frhp)
        @test !Cecelia._ims_heap_failure("/data/a.ims", softlink)
        @test !Cecelia._ims_heap_failure("/data/a.ims", String[])
        @test !Cecelia._ims_heap_failure("/data/a.czi", invalid_arg)   # not an Imaris source
    end

    @testset "_run_bf2raw streams every line and reports a clean exit" begin
        out = mktempdir()
        logged = String[]
        ok, lines = Cecelia._run_bf2raw(`$(Base.julia_cmd()) --startup-file=no -e 'println("a"); println(stderr, "b")'`,
                                         joinpath(out, "none.zarr"), 0; on_log = l -> push!(logged, l))
        @test ok
        @test Set(lines) == Set(["a", "b"])          # stdout AND stderr, for `_ims_heap_failure`
        @test logged == lines
        ok, _ = Cecelia._run_bf2raw(`$(Base.julia_cmd()) --startup-file=no -e 'exit(3)'`,
                                     joinpath(out, "none.zarr"), 0)
        @test !ok
    end

    @testset "_run_bf2raw progress = bytes written / source size, only once output exists" begin
        out = mktempdir()
        zarr = joinpath(out, "p.zarr")
        # Child writes 4 MiB into the store (MiB so du block rounding stays under 1 %), then stays alive past one 2 s monitor tick.
        # Path passed via ENV so Windows backslashes never land inside a Julia string literal.
        write_then_wait = addenv(`$(Base.julia_cmd()) --startup-file=no -e 'z = ENV["BF2RAW_TEST_Z"]; mkpath(z); write(joinpath(z, "x"), zeros(UInt8, 4 << 20)); sleep(5)'`,
                                 "BF2RAW_TEST_Z" => zarr)
        seen = Tuple{Int,Int}[]
        Cecelia._run_bf2raw(write_then_wait, zarr, 8 << 20; on_progress = (a, b) -> push!(seen, (a, b)))
        @test !isempty(seen)
        @test last(seen) == (50, 100)                  # 4 MiB / 8 MiB
        # Unknown source size: no progress at all (no divide-by-zero estimate).
        seen0 = Tuple{Int,Int}[]
        Cecelia._run_bf2raw(write_then_wait, zarr, 0; on_progress = (a, b) -> push!(seen0, (a, b)))
        @test isempty(seen0)
        # Store never created: no progress, even with a known size.
        seen_none = Tuple{Int,Int}[]
        Cecelia._run_bf2raw(`$(Base.julia_cmd()) --startup-file=no -e 'sleep(3)'`, joinpath(out, "absent.zarr"), 1000;
                            on_progress = (a, b) -> push!(seen_none, (a, b)))
        @test isempty(seen_none)
    end
end
