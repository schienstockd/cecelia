# Unit test for `scripts/bootstrap_worktree.jl :: sibling_dst`.
#
# The whole reason this file exists: the bootstrap task landed one new tree INSIDE the
# source worktree instead of as its sibling. Root cause was `dirname(REPO_ROOT)` on a
# trailing-slash path (`normpath("scripts/..")` returns `.../<repo>/` on Unix, and
# `dirname(".../<repo>/")` strips only the slash — same directory, not the parent). The
# fix computes the sibling via `joinpath(REPO_ROOT, "..", ...)` + `normpath`, which is
# trailing-slash-safe. Pin that behaviour so a future refactor can't reintroduce it.
#
# Run: `julia scripts/test_bootstrap_worktree.jl`. Zero deps beyond stdlib `Test`.

using Test

include(joinpath(@__DIR__, "bootstrap_worktree.jl"))  # PROGRAM_FILE guard skips main()

@testset "sibling_dst" begin
    # The failing case — the exact shape `normpath(joinpath(@__DIR__, ".."))` produces on
    # Unix. Trailing slash MUST NOT keep the result nested inside the source.
    @test sibling_dst("/home/u/ws/cecelia/cecelia-source/", "target") ==
        "/home/u/ws/cecelia/cecelia-target"

    # No trailing slash — the same answer.
    @test sibling_dst("/home/u/ws/cecelia/cecelia-source", "target") ==
        "/home/u/ws/cecelia/cecelia-target"

    # A relative repo root also lands relative-to-parent, not nested inside itself.
    @test sibling_dst("cecelia-source", "target") == "cecelia-target"

    # Windows-style separators — normpath uses OS-native, but the parent-hop math must
    # hold regardless of trailing-separator normalisation.
    @test !occursin("cecelia-source", sibling_dst("/repo/cecelia-source/", "target"))
end

println("bootstrap_worktree unit tests OK")
