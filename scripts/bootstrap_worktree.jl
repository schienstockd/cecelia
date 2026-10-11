# scripts/bootstrap_worktree.jl — create a fully bootstrapped sibling cecelia worktree.
#
# WHAT IT IS. `git worktree add` + `.env` copy + a fresh per-worktree `pixi install`, then defers
# `.env`/frontend/deps VERIFICATION to `pixi run doctor` in the new tree. Bootstrap only owns the
# steps that doctor cannot do (there is no tree yet to audit, no source to copy `.env` from, no
# per-worktree `.pixi` yet). Every check that survives past creation lives in doctor.
#
# WHY ONE COMMAND. The four setup steps are all required together but reading three separate memory
# bullets left at least one skipped every time — dev drops a bare `git worktree add` and finds the
# failure looks like a code bug (a shared `.pixi` silently loads another checkout's `python/`;
# missing `.env` resolves projects_dir to the placeholder; missing `node_modules` dies at Vite
# start). See docs/DEV.md → Creating a new worktree.
#
# WHY JULIA (not bash). Root CLAUDE.md: "Launcher logic lives in pixi.toml tasks, not shell
# scripts." A bash-only script does not run on Windows. Julia is available in the pixi env and
# matches the pattern already set by `scripts/doctor.jl` and `api/portkill.jl`.

const REPO_ROOT = normpath(joinpath(@__DIR__, ".."))

"""
    sibling_dst(repo_root::AbstractString, name::AbstractString) -> String

The sibling worktree path: `<repo_root>/../cecelia-<name>`, absolute + normalised.

Extracted (and factored via `joinpath(..., "..", ...)` instead of `dirname(repo_root)`)
after the bug where a bootstrapped tree landed INSIDE the source worktree instead of as
its sibling. Root cause: `normpath(joinpath(@__DIR__, ".."))` on Unix ends in a trailing
`/`, and `dirname("/…/cecelia-source/")` strips only the slash — it returns the SAME
directory, not its parent. `joinpath(REPO_ROOT, "..")` + `normpath` is trailing-slash-safe
and reads as what we want: "go up one, then into the sibling name".

Pure — no side effects, no `git` calls — so the test suite drives it with synthetic paths.
"""
sibling_dst(repo_root::AbstractString, name::AbstractString) =
    normpath(joinpath(repo_root, "..", "cecelia-$name"))

# Above this many worktrees (the main checkout included) the nudge fires: a dozen is the usual
# working set of parallel sessions, so more than that is mostly merged work left standing.
const NUDGE_AT = 15

"""
    cleanup_nudge(porcelain::AbstractString) -> Union{String,Nothing}

One line suggesting `pixi run prune-worktrees` when `git worktree list --porcelain` shows more than
`NUDGE_AT` worktrees or any dead entry (`prunable`); `nothing` otherwise. Counting only — the
merged/clean/in-use judgement is the prune tool's, so this stays instant. Agents relay the line
(root CLAUDE.md → *remind Dominik about worktree cleanup*).
"""
function cleanup_nudge(porcelain::AbstractString)
    blocks = filter(b -> startswith(b, "worktree "), split(strip(porcelain), r"\n\n+"))
    dead = count(b -> occursin(r"^prunable"m, b), blocks)
    n = length(blocks)
    (n <= NUDGE_AT && dead == 0) && return nothing
    "$n worktrees" * (dead > 0 ? " ($dead dead)" : "") *
        " — run `pixi run prune-worktrees` to see which can go"
end

function usage()
    println(stderr, "usage: pixi run bootstrap-worktree <name> [<branch>] [<ref>]")
    println(stderr, "  name    — path suffix; the worktree lives at ../cecelia-<name>")
    println(stderr, "  branch  — new branch name (default: <name>)")
    println(stderr, "  ref     — starting point (default: origin/main)")
end

function main()
    if length(ARGS) < 1
        usage()
        exit(2)
    end
    name   = ARGS[1]
    branch = length(ARGS) >= 2 ? ARGS[2] : name
    ref    = length(ARGS) >= 3 ? ARGS[3] : "origin/main"

    dst = sibling_dst(REPO_ROOT, name)
    if ispath(dst)
        println(stderr, "error: $dst already exists")
        exit(1)
    end

    println("→ fetching origin")
    run(Cmd(`git -C $REPO_ROOT fetch origin --quiet`))

    println("→ git worktree add $dst (branch $branch, off $ref)")
    run(Cmd(`git -C $REPO_ROOT worktree add -b $branch $dst $ref`))

    src_env = joinpath(REPO_ROOT, ".env")
    if isfile(src_env)
        println("→ copying .env from $REPO_ROOT")
        cp(src_env, joinpath(dst, ".env"))
    else
        # doctor will amber-flag this in the follow-up. Not fatal — a fresh clone has no .env
        # to copy either, and the user is prompted with the right command.
        println("!  no .env at $src_env — skipped (doctor will report it)")
    end

    println("→ pixi install in $dst (fresh env — pixi env is not relocatable)")
    run(Cmd(`pixi install --manifest-path $(joinpath(dst, "pixi.toml"))`))

    # Delegate .env sanity, frontend deps (npm ci is auto-fixed), julia deps, credentials, and
    # optional envs to the sanctioned dispatcher rather than re-implementing them here.
    println("→ pixi run doctor in $dst")
    run(Cmd(`pixi run --manifest-path $(joinpath(dst, "pixi.toml")) doctor`))

    println()
    println("✓ worktree ready at $dst")
    println("  branch: $branch (tracking $ref)")
    println()
    println("next: cd $dst && pixi run dev")

    nudge = cleanup_nudge(read(Cmd(`git -C $REPO_ROOT worktree list --porcelain`), String))
    nudge === nothing || (println(); println("tidy: ", nudge))
end

# Only fire `main()` when the script is the process entrypoint. `include`-ing it from a
# test (or another script) then must not spawn `git worktree add`. Matches the pattern used
# by `api/task_console.jl` at the bottom.
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
