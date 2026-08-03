using Cecelia
using Test
using JSON3
using Dates
import DataFrames: DataFrame, nrow   # only the symbols the tests construct/measure with

# ── Smoke tests — no API, no WebSocket, no Vue ────────────────────────────────
# Run from the app/ directory:  julia -O0 --project test/runtests.jl   (or `pixi run test-pkg`)
#
# THIS FILE IS THE PREAMBLE ONLY — the suite body lives in `suite.jl`, which the single aggregating
# @testset at the bottom includes. Add testsets THERE, not here. (The split is a compile-time fix,
# not organisation; `suite.jl`'s header explains why, and why it must not be re-wrapped.)

# HERMETIC BY DEFAULT: unless `CECELIA_DEV_DIR` is already set, point config at a throwaway dir with a
# throwaway projects dir. The suite CREATES projects, and with no `custom.toml` anywhere `projects_dir()`
# is the shipped placeholder `/path/to/projects` — so `create_project!` did `mkdir("/path")` and ~30
# testsets errored with EACCES/EROFS. That is invisible on a dev machine (a real `.env` → `custom.toml`
# makes it pass) and is why the suite could not run in CI at all.
#
# Set `CECELIA_DEV_DIR` yourself to run against a specific config instead. Julia deletes both temp dirs
# at exit. Paths go in TOML *literal* strings (single quotes) so Windows backslashes are not escapes.
if !haskey(ENV, "CECELIA_DEV_DIR")
    let cfg = mktempdir(), proj = mktempdir()
        write(joinpath(cfg, "custom.toml"), "[dirs]\nprojects = '" * proj * "'\n")
        ENV["CECELIA_DEV_DIR"] = cfg
        @info "Tests: hermetic config" config_dir=cfg projects_dir=proj
    end
end

init_cecelia!()

# ── Test data fixtures ────────────────────────────────────────────────────────
# Version-controlled fixtures live at <workspace-root>/test-data/projects, independent
# of the deletable dev projects dir (`projects_dir()`). Override with CECELIA_TEST_DATA.
# (@__DIR__ = app/test → ../../.. = workspace root.)  See test-data/README.md.
test_projects_dir() = get(ENV, "CECELIA_TEST_DATA",
    normpath(joinpath(@__DIR__, "..", "..", "test-data", "projects")))   # <repo>/test-data, IN git

"""Absolute path to a fixture under test-data/projects (no existence check)."""
fixture_path(relparts...) = joinpath(test_projects_dir(), relparts...)

const _WARNED_FIXTURES = Set{String}()

"""
    have_fixture(path) -> Bool

True if the fixture exists. If not, emit a single strong warning (once per path) and
return false so the caller can `@test_skip`. Generic — works for any fixture file.
"""
function have_fixture(path::AbstractString)::Bool
    isfile(path) && return true
    if !(path in _WARNED_FIXTURES)
        push!(_WARNED_FIXTURES, path)
        @warn """
        ╔══════════════════════════════════════════════════════════════════════════╗
        ║  TEST FIXTURE MISSING — dependent tests will be SKIPPED.                   ║
        ╚══════════════════════════════════════════════════════════════════════════╝
        Expected: $path
        Tests that assert against real data are skipped without it, leaving that path
        unverified. Fixtures are committed under <repo>/test-data/, so this normally means a
        partial checkout — restore it with `git checkout -- test-data` or set CECELIA_TEST_DATA
        to a projects dir containing the file above."""
    end
    false
end

# Custom drop-in task fixture — structs must live at module top level, not inside a @testset block.
struct _TestCustomTask <: CciaTask end

# A task whose _run_task always throws — used to assert the scheduler tees a crash into the task log.
struct _CrashTask <: CciaTask end
Cecelia._run_task(::_CrashTask, ::CciaImage, ::Dict{String,Any};
                  on_log::Function = _ -> nothing, on_progress::Function = (_, _) -> nothing,
                  on_process::Function = _ -> nothing) = error("boom in _run_task (test)")

# A task that stays running until it's told to stop, so a test can inspect the LIVE registry — the
# scheduler's timestamps and `list_tasks()` only exist while the task is in flight (the record is
# deregistered the instant it finishes).
const _HOLD_TASK_GO = Ref{Channel{Nothing}}(Channel{Nothing}(1))
struct _HoldTask <: CciaTask end
Cecelia._run_task(::_HoldTask, ::CciaImage, ::Dict{String,Any};
                  on_log::Function = _ -> nothing, on_progress::Function = (_, _) -> nothing,
                  on_process::Function = _ -> nothing) = (take!(_HOLD_TASK_GO[]); true)

# ── Fault injection: make the scheduler's OWN error path throw ────────────────────
# `_execute_job!` must post to `job.done` no matter what, because `run_task` is blocked in `take!` and
# a throw escaping into the dispatcher's `Threads.@spawn` is silent — the cost is a submitter blocked
# forever and a TaskRecord stranded at `:running` (a task the scheduler keeps reporting as in-flight
# long after it finished). A logger is just the injection handle: it puts the throw *inside* the
# `catch` block, which is the one window the inline handling can't cover. `catch_exceptions = false`
# is what makes `@warn` propagate instead of reporting; only the scheduler's crash message throws, so
# Test's own output is untouched.
using Logging
struct _ThrowingLogger <: Logging.AbstractLogger
    inner::Logging.AbstractLogger
end
Logging.min_enabled_level(l::_ThrowingLogger) = Logging.min_enabled_level(l.inner)
Logging.shouldlog(l::_ThrowingLogger, level, _module, group, id) =
    Logging.shouldlog(l.inner, level, _module, group, id)
Logging.catch_exceptions(::_ThrowingLogger) = false
function Logging.handle_message(l::_ThrowingLogger, level, message, _module, group, id, file, line; kwargs...)
    occursin("Unhandled error in task", string(message)) && error("logger exploded (test)")
    Logging.handle_message(l.inner, level, message, _module, group, id, file, line; kwargs...)
end

# ── ONE walk over a task spec's params ────────────────────────────────────────────
# Five testsets walk task-spec `params` — the tip budget, tip coverage, the copy sweep, numeric
# ranges, and param validation — and each had hand-rolled the same recursive descent. They agree on
# the hard parts by luck rather than construction, and the hard parts are real: `params` nests
# through `section`/`group` containers, and the key type depends on WHERE the spec came from — a spec
# read from disk (`JSON3.read`) has SYMBOL keys, one from `Cecelia._task_spec` has STRING keys (see
# CLAUDE.md → *JSON3 gotcha*). A walker that handles only one of those silently sees no params, which
# reads as "nothing to report" rather than as a broken walk.

"""A spec param's field, whichever key type the spec was parsed with (Symbol or String)."""
spec_get(p, key, default = nothing) = get(p, Symbol(key), get(p, String(key), default))

"""
    each_spec_param(f, params; group = "")

Depth-first over a spec's `params`, calling `f(param, group_key)` for EVERY param — containers
included (callers that only want settable inputs filter on `type` themselves), children descended.
`group_key` is the enclosing `group` param's key, or `""` at the top level; only `group` sets it,
because a `section` is pure layout while a `group`'s values live nested under its key.
"""
function each_spec_param(f, params; group = "")
    params isa AbstractVector || return nothing
    for p in params
        p isa AbstractDict || continue
        f(p, group)
        inner = spec_get(p, "params", nothing)
        each_spec_param(f, inner;
                        group = String(something(spec_get(p, "type", ""), "")) == "group" ?
                                String(something(spec_get(p, "key", ""), "")) : group)
    end
    nothing
end

@testset verbose=true "Cecelia package smoke tests" begin
    include("suite.jl")   # see suite.jl — inlining it here costs ~90s of thunk compilation
end
