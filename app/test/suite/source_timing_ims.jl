# The source-timing tiers through the REAL Imaris reader (`_recover_ims_time_increment!` → run_py →
# ims_meta.time_increment), not an injected stand-in: what it hands back decides whether the declared
# interval may apply. Miniature `.ims` files come from the Python tests' own writer
# (`python/cecelia/tests/test_ims_meta.py` → `_ims`). Why: docs/todo/TEST_VALUE_PLAN.md → P3.

const _IMS_FIXTURE_PY = raw"""
import cecelia.utils.script_utils as script_utils
from cecelia.tests.test_ims_meta import _ims

p = script_utils.script_params()
s = lambda secs: ['2026-07-29 14:%02d:%02d.000' % divmod(t, 60) for t in secs]
_ims(p['uniform'], stamps=s([0, 30, 60, 90]))
_ims(p['paused'], stamps=s([0, 30, 60, 600, 630]))
_ims(p['nominal'], oir_step='12.5')
_ims(p['none'])
"""

@testset "source timing through the real Imaris reader" begin
    dir = mktempdir()
    ims = (uniform = joinpath(dir, "uniform.ims"), paused = joinpath(dir, "paused.ims"),
           nominal = joinpath(dir, "nominal.ims"), none = joinpath(dir, "none.ims"))
    script = joinpath(dir, "make_ims.py"); write(script, _IMS_FIXTURE_PY)
    log = String[]
    Cecelia.run_py(script, ims, joinpath(dir, "fixture_tasks"); on_log = l -> push!(log, l)) ||
        error("ims fixture failed:\n" * join(log, "\n"))

    declared(v) = Dict{String,Any}("declaredTimeIncrement" => Dict{String,Any}(
        "value" => v, "unit" => "second", "source" => "declared at registration"))
    function fill(src; dt = declared(99.0))
        m = Dict{String,Any}("SizeT" => 5); lines = String[]
        run_dir = mkpath(joinpath(dir, "run", basename(src)))
        ok = Cecelia._fill_source_time_increment!(m, dt, src, run_dir; on_log = l -> push!(lines, l))
        (ok, m, lines, run_dir)
    end

    # measured in the file: it wins over the declared value, in seconds
    ok, m, lines, run_dir = fill(ims.uniform)
    @test ok && m["TimeIncrement"] == 30.0 && m["TimeIncrementUnit"] == "second"
    @test !haskey(m, "TimeIncrementDeclared")
    @test isempty(filter(f -> endswith(f, ".result.json"), readdir(run_dir)))   # the result file is removed

    # paused: the file's answer is "no single interval" — nothing set, the declared one vetoed and said so
    ok, m, lines, _ = fill(ims.paused)
    @test !ok && !haskey(m, "TimeIncrement")
    @test any(contains("Declared frame interval not applied"), lines)
    # … and with nothing declared there is nothing to veto, so no such warning
    ok, m, lines, _ = fill(ims.paused; dt = Dict{String,Any}())
    @test !ok && !any(contains("Declared frame interval not applied"), lines)

    # only the vendor's nominal step: used, and the log says it is nominal
    ok, m, lines, _ = fill(ims.nominal)
    @test ok && m["TimeIncrement"] == 12.5
    @test any(l -> contains(l, "recovered from the Imaris") && contains(l, ", nominal"), lines)
    ok, m, lines, _ = fill(ims.uniform)
    @test !any(contains(", nominal"), lines)

    # no timing in the file → the declared tier
    ok, m, _, _ = fill(ims.none)
    @test ok && m["TimeIncrement"] == 99.0 && haskey(m, "TimeIncrementDeclared")
    # not an .ims, or an .ims that isn't there: the file tier has nothing, the declared tier applies
    tif = joinpath(dir, "movie.tif"); write(tif, "x")
    for src in (tif, joinpath(dir, "gone.ims"))
        ok, m, _, _ = fill(src)
        @test ok && m["TimeIncrement"] == 99.0
    end
    # a declared interval under a second is still an interval
    ok, m, _, _ = fill(tif; dt = declared(0.5))
    @test ok && m["TimeIncrement"] == 0.5
end
