# RESULT-REVIEW: result-5c9c8913
#
# A declared test's assertions are about what the run WROTE. The measure test's fixture already ships
# the cell table the task writes, and that committed table satisfies every assertion — so a measure
# that returned success without writing would still have passed. The runner now stamps each declared
# output before and after the run (`spec_output_stamp` / `spec_output_written`) and fails an output
# the run left untouched. This pins that decision on the exact case: the committed table, unwritten.
@testset "declared spec tests — an output the run did not write fails" begin
    fx = fixture_for(:cells, Symbol("image-store"), Symbol("label-store"), Symbol("marker-names"); technology = "synthetic")
    root = mktempdir(); proj = joinpath(root, basename(fx.project)); cp(fx.project, proj)
    img = Cecelia._load_image(joinpath(proj, relpath(fx.dir, fx.project)))
    path = Cecelia.spec_output_path(img, "cells", "seg")
    before = Cecelia.spec_output_stamp(path)
    @test before !== nothing                                     # the fixture already holds the table

    # the committed table passes the declared assertions as it stands — the reviewer's repro
    a = Cecelia.SpecAssertion("n_labels", "cells", "seg", 6.0, 0.0, nothing, nothing, nothing)
    @test Cecelia.evaluate_spec_assertion(img, a)[1]
    # …but nothing wrote it: no run, same stamp → not written
    @test !Cecelia.spec_output_written(before, Cecelia.spec_output_stamp(path))
    # an output still absent after the run is not written either
    @test !Cecelia.spec_output_written(nothing, Cecelia.spec_output_stamp(joinpath(root, "absent.h5ad")))

    # a replacing write (how every writer lands its output) changes the stamp
    tmp = path * ".new"; cp(path, tmp); mv(tmp, path; force = true)
    @test Cecelia.spec_output_written(before, Cecelia.spec_output_stamp(path))
end
