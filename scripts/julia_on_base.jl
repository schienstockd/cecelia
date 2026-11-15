# Run test files against the code as it was before a change, in one process (no second package build):
# load the suite preamble, delete every method the changed source files define, include each file's
# BASE text under its own name, then run the tests. TEST_VALUE_PLAN.md → P4(a).
#
#   pixi run julia --project=app scripts/julia_on_base.jl OUT.txt SRC=BASE … -- TEST.jl …
#
# BASE is a file holding SRC's text at the base commit; an empty BASE is a file the change added.
# Writes OUT.txt: `fail <file>:<line>` per failed or errored @test, `threw <file>` per test file that
# threw outside one, `base-error <src>: <msg>` when a BASE text would not load (the change's own types
# or constants are in the way: the new tests cannot be judged on it). Nothing written = all passed.
# Approximate by design: a const or struct the change added stays defined (Julia can't remove one),
# and a method the change added to another module's function (`Base.show(::NewType)`) stays too.

using Test

function main(args)
    sep = findfirst(==("--"), args)
    out, pairs, tests = args[1], args[2:sep-1], abspath.(args[sep+1:end])
    rt = joinpath(dirname(Base.active_project()), "test", "runtests.jl")   # its @__DIR__ must resolve
    Base.include_string(Main, split(read(rt, String), "@testset verbose=true")[1], rt)
    lines = String[]
    Base.invokelatest() do
        C = Main.Cecelia
        for pr in pairs
            src, base = abspath.(split(pr, '='; limit = 2))
            # every method this file defines, on Cecelia's own functions and types
            for n in names(C; all = true)
                isdefined(C, n) || continue
                f = getfield(C, n)
                (f isa Function || f isa Type) || continue
                for m in collect(methods(f))
                    m.module === C && String(m.file) == src && Base.delete_method(m)
                end
            end
            try include_string(C, read(base, String), src)
            catch e; push!(lines, "base-error $src: " * first(split(sprint(showerror, e), '\n'))) end
        end
    end
    fails(ts) = foldl((acc, r) -> r isa Union{Test.Fail,Test.Error} ? push!(acc, "fail $(r.source.file):$(r.source.line)") :
                                  r isa Test.DefaultTestSet ? append!(acc, fails(r)) : acc, ts.results; init = String[])
    ts = Test.DefaultTestSet("on base"); Test.push_testset(ts)
    try
        for f in tests
            try redirect_stdio(() -> Base.invokelatest(Base.include, Main, f); stdout = devnull, stderr = devnull)
            catch; push!(lines, "threw $f") end
        end
    finally Test.pop_testset() end
    append!(lines, fails(ts))
    write(out, join(lines, '\n') * (isempty(lines) ? "" : "\n"))
end

main(ARGS)
