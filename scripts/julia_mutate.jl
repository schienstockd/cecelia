# Line-based mutation testing for one Cecelia source file against chosen test files.
#
#   julia --project=app scripts/julia_mutate.jl SRC OUT.tsv TEST.jl... [--timeout 120] [--list]
#                                               [--sample N --seed S] [--lines 12-30,45] [--known-failures]
#
# Applies ONE mutation per mutant to SRC (a file `include`d into module Cecelia), re-includes it into
# the loaded package (no restart), runs the TEST files in a throwaway testset and records
# killed / survived / build-error, then restores the original. A worker process does the runs; this
# parent restarts it past a mutant that exceeds --timeout (recorded `timeout`). Mutant 0 is the
# unmutated file and must survive, or the harness is wrong. Survivors are only "not killed by THESE
# tests" — confirm a survivor against the whole suite before calling it a gap.
# Skips comments, string literals and docstrings. Rows: id, line, op, original, mutated, result, secs.
# --sample N runs N mutants drawn with a fixed --seed (default 1), so a rerun draws the same ones.
# --lines keeps only mutants on those lines of SRC (a diff's changed lines; ids stay those of --list).
# --known-failures takes what fails on the unmutated SRC (an order-dependent check that needs a file
# suite.jl ran earlier) as the baseline: a mutant is killed only by a failure outside it. Listed on
# stderr and in OUT.tsv.known. Without it, any failure on the unmutated SRC stops the run.
# The suite preamble is the active project's test/runtests.jl, so a copy of this script works on any
# checkout: `julia --project=<checkout>/app /elsewhere/julia_mutate.jl …`.

using Random

const OPS = [  # (name, regex on the masked line, replacement)
    ("<=>",  r"(?<![<=!>])<=(?!=)", "<"),   ("<=<",  r"(?<![<-])<(?![=:<])", "<="),
    (">=>",  r"(?<![>=!<])>=(?!=)", ">"),   (">=>=", r"(?<![-=>])>(?![=>])", ">="),
    ("==",   r"(?<![=!<>])==(?!=)", "!="),  ("!=",   r"!=(?!=)", "=="),
    ("&&",   r"&&", "||"),                  ("||",   r"\|\|", "&&"),
    ("!x",   r"(?<![\w)\]!])!(?=[\w(])", ""),
    ("+",    r"(?<=\s)\+(?=\s)", "-"),      ("-",    r"(?<=\s)-(?=\s)", "+"),
    ("*",    r"(?<=\s)\*(?=\s)", "/"),
    ("0→1",  r"(?<![\w.])0(?![\w.])", "1"), ("1→0",  r"(?<![\w.])1(?![\w.])", "0"),
    ("min",  r"\bmin(?=\()", "max"),        ("max",  r"\bmax(?=\()", "min"),
    ("minimum", r"\bminimum\b", "maximum"), ("maximum", r"\bmaximum\b", "minimum"),
    ("true", r"\btrue\b", "false"),         ("false", r"\bfalse\b", "true"),
]

# blank out comments and string contents so operators inside them are never mutated
function masked_lines(src::Vector{String})
    out = String[]; indoc = false
    for ln in src
        if occursin("\"\"\"", ln)
            push!(out, " "^ncodeunits(ln)); isodd(count("\"\"\"", ln)) && (indoc = !indoc); continue
        end
        indoc && (push!(out, " "^ncodeunits(ln)); continue)
        m = IOBuffer(); instr = false; prev = ' '
        for c in ln
            if instr
                print(m, c == '"' && prev != '\\' ? (instr = false; "\"") : " "^ncodeunits(c))
            elseif c == '"'; instr = true; print(m, c)
            elseif c == '#'; print(m, " "^(ncodeunits(ln) - position(m))); break
            else print(m, c) end
            prev = c
        end
        push!(out, String(take!(m)))
    end
    out
end

function mutants(path)
    src = readlines(path); mk = masked_lines(src); ms = Any[]
    for (i, (ln, k)) in enumerate(zip(src, mk)), (name, re, rep) in OPS
        for mt in eachmatch(re, k)
            a = mt.offset; b = a + ncodeunits(mt.match) - 1
            isvalid(ln, a) && isvalid(ln, b) || continue
            push!(ms, (line = i, op = name, orig = strip(ln), new = ln[1:prevind(ln, a)] * rep * ln[nextind(ln, b):end]))
        end
    end
    src, ms
end

function worker(srcpath, out, tests, ids, known_failures)
    src, ms = mutants(srcpath)
    # every failed or errored @test, by where it sits; a file that throws outside a @test counts once
    fails(ts) = foldl((acc, r) -> r isa Union{Test.Fail,Test.Error} ? push!(acc, "$(r.source.file):$(r.source.line)") :
                                  r isa Test.DefaultTestSet ? union!(acc, fails(r)) : acc, ts.results; init = Set{String}())
    function run()
        ts = Test.DefaultTestSet("mutant"); Test.push_testset(ts); bad = Set{String}()
        try
            for f in tests
                try redirect_stdio(() -> Base.include(Main, f); stdout = devnull, stderr = devnull)
                catch; push!(bad, "threw:$f") end
            end
        finally Test.pop_testset() end
        union!(bad, fails(ts))
    end
    # what already fails unmutated (an order-dependent check that needs a file suite.jl ran earlier):
    # a mutant is killed only by a failure outside this set
    known = Set{String}()
    if known_failures && first(ids) != 0 && isfile(out * ".known")   # a restarted worker
        known = Set(readlines(out * ".known"))
    end
    for id in ids
        t0 = time(); lines = copy(src)
        id > 0 && (lines[ms[id].line] = ms[id].new)
        # under SRC's own name, so its `@__DIR__` / `@__FILE__` still point where they should
        res = try
            include_string(Cecelia, join(lines, "\n") * "\n", srcpath)
            bad = run()
            if id == 0 && known_failures
                known = bad
                open(io -> foreach(k -> println(io, k), sort!(collect(known))), out * ".known", "w")
                foreach(k -> println(stderr, "julia_mutate: fails unmutated, ignored: ", k), sort!(collect(known)))
            end
            isempty(setdiff(bad, known)) ? "survived" : "killed"
        catch; "build-error" end
        Base.include(Cecelia, srcpath)                                   # restore
        m = id == 0 ? (line = 0, op = "none", orig = "", new = "") : ms[id]
        open(out, "a") do io
            println(io, join((id, m.line, m.op, m.orig, strip(m.new), res, round(time() - t0; digits = 1)), '\t'))
        end
        id == 0 && res != "survived" && exit(3)   # the harness is wrong; no mutant result means anything
    end
end

# a hung worker may have spawned children (a test's run_py): stop the whole tree, the canonical way
function kill_tree(p)
    @eval Main using Cecelia
    Base.invokelatest(q -> Main.Cecelia._kill_proc_tree(q), p)   # Cecelia is newer than this world
end

function main(args)
    if get(args, 1, "") == "--worker"   # load the suite preamble only, then run in the new world
        rt = joinpath(dirname(Base.active_project()), "test", "runtests.jl")   # its @__DIR__ must resolve
        Base.include_string(Main, split(read(rt, String), "@testset verbose=true")[1], rt)
        kf = args[end] == "--known-failures"
        return Base.invokelatest(worker, args[2], args[3], args[5:end-kf], parse.(Int, split(args[4], ',')), kf)
    end
    opt(k, d) = (i = findfirst(==(k), args); i === nothing ? d : args[i+1])
    pos = filter(a -> !startswith(a, "--"), args[1:something(findfirst(startswith("--"), args), length(args) + 1) - 1])
    srcpath, out, tests = abspath(pos[1]), pos[2], abspath.(pos[3:end])
    _, ms = mutants(srcpath)
    if "--list" in args
        foreach(((i, m),) -> println(join((i, m.line, m.op, m.orig, strip(m.new)), '\t')), enumerate(ms)); return
    end
    timeout = parse(Float64, opt("--timeout", "120")); done = 0
    n_sample = parse(Int, opt("--sample", "0"))
    pool = collect(1:length(ms))
    if (spec = opt("--lines", nothing)) !== nothing
        keep = Set{Int}()
        for r in split(spec, ',', keepempty = false)
            a, b = occursin('-', r) ? parse.(Int, split(r, '-')) : (parse(Int, r), parse(Int, r))
            union!(keep, a:b)
        end
        filter!(i -> ms[i].line in keep, pool)
    end
    ids = 0 < n_sample < length(pool) ?
        [0; sort(Random.shuffle(Random.MersenneTwister(parse(Int, opt("--seed", "1"))), pool)[1:n_sample])] :
        [0; pool]
    kf = "--known-failures" in args ? ["--known-failures"] : String[]
    isfile(out) && (done = maximum(parse(Int, split(l, '\t')[1]) for l in readlines(out); init = -1) + 1)
    while any(>=(done), ids)
        todo = join(filter(>=(done), ids), ',')
        cmd = `$(Base.julia_cmd()) --project=$(Base.active_project()) -O0 $(@__FILE__) --worker $srcpath $out $todo $tests $kf`
        p = run(pipeline(cmd; stdout = devnull, stderr = stderr); wait = false); last = time()
        seen = isfile(out) ? countlines(out) : 0
        while process_running(p)
            sleep(1)
            n = isfile(out) ? countlines(out) : 0
            n != seen && (seen = n; last = time())
            # the first row also pays for loading the package: give it 10× the budget
            time() - last > (seen == 0 ? 10timeout : timeout) && (kill_tree(p); break)
        end
        wait(p); p.exitcode == 3 && error("the unmutated run did not survive — see $out")
        n = isfile(out) ? maximum(parse(Int, split(l, '\t')[1]) for l in readlines(out); init = -1) + 1 : 0
        if n == done   # no progress: the next mutant hung or crashed the worker
            done = ids[findfirst(>=(done), ids)]
            open(io -> println(io, join((done, ms[max(done, 1)].line, "-", "", "", done == 0 ? "harness-failed" : "timeout", timeout), '\t')), out, "a")
            done == 0 && error("unmutated run did not complete — see stderr")
            n = done + 1
        end
        done = n
    end
end

main(ARGS)
