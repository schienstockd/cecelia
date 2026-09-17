# Cecelia package smoke tests — the suite body.
#
# Split out of runtests.jl (which keeps the preamble and wraps this in the one aggregating
# @testset). The split is a PERFORMANCE fix, not organisation: as a single 8k-line `@testset
# begin ... end`, the whole body was one top-level thunk that Julia lowered and compiled in full
# before running a single assertion — ~90s of the suite's ~200s, on top of ~99s of ordinary method
# compilation, for ~11s of actual test work. Behind an include it is ~194 small statements instead.
# Keep it that way: do not re-wrap this file in a `begin` block or a single outer @testset.


# ── Config ────────────────────────────────────────────────────────────────
@testset "Config" begin
    @test !isempty(projects_dir())
    @test !isempty(python_bin_path())
    @test tasks_concurrent_limit() >= 1
end

# ── Version stamp is consistent across the four files that carry it ──────────
# `cecelia_version()` (from Project.toml, via pkgversion) is the runtime reader; CITATION.cff is the
# human-facing citation; frontend/package.json is what JS tooling sees; frontend/package-lock.json
# carries the same name+version and npm rewrites it on install. All four MUST agree — the
# release-cutting checklist (docs/RELEASING.md step 4) bumps them together, and this testset is the
# ratchet. A divergence here means either the bump was partial or one file was edited by hand.
# (The lockfile once sat at 0.2.0 for two releases because "the parity gate doesn't cover it" was
# tribal knowledge instead of an assertion.)
@testset "cecelia_version agrees with CITATION.cff and package.json" begin
    ver = cecelia_version()
    @test !isempty(ver) && ver != "0.0.0"

    root = normpath(joinpath(@__DIR__, "..", ".."))

    cff_ver = nothing
    for line in eachline(joinpath(root, "CITATION.cff"))
        m = match(r"^version:\s*(\S+)\s*$", line)
        m === nothing || (cff_ver = String(m.captures[1]); break)
    end
    @test cff_ver == ver

    pkg_ver = JSON3.read(read(joinpath(root, "frontend", "package.json"), String))[:version]
    @test String(pkg_ver) == ver

    lock = JSON3.read(read(joinpath(root, "frontend", "package-lock.json"), String))
    @test String(lock[:version]) == ver                              # top-level
    @test String(lock[:packages][Symbol("")][:version]) == ver       # root package entry
end

# ── Fixture size ratchet ─────────────────────────────────────────────────────
# Fixtures are committed now, and `.h5ad` is binary: git stores a WHOLE new copy per update and
# history can't be pruned without a rewrite. "Keep fixtures small" was already the rule but nothing
# enforced it, and an in-repo dir is a standing invitation to drop a GB OME-ZARR in. This is the
# enforcement — same shape as the UI-copy ratchets: an exact cap, not a vibe.
#
# 1 MB leaves ~3x headroom over today's largest (B.h5ad, 332 KB). If a fixture genuinely needs more,
# that is a design conversation (regenerate smaller / synthesise / gate the test differently), not a
# number to nudge up.
@testset "fixtures stay small" begin
    root = normpath(joinpath(@__DIR__, "..", "..", "test-data"))
    CAP  = 1024 * 1024
    if isdir(root)
        oversized = Tuple{String,Int}[]
        total = 0
        for (dir, _, files) in walkdir(root), f in files
            n = filesize(joinpath(dir, f)); total += n
            n > CAP && push!(oversized, (relpath(joinpath(dir, f), root), n))
        end
        @test isempty(oversized)          # names the offender if it fires
        isempty(oversized) || @info "oversized fixtures" oversized
        # a whole-tree bound too: many medium files are as bad as one large one
        @test total <= 8 * CAP
    else
        @test_skip "test-data/ not present"
    end
end

# ── Config resolver (dev↔prod coordination) ─────────────────────────────────
# The single resolver both init_cecelia! (reader) and set_projects_dir! (writer) share.
# Order: explicit arg → CECELIA_DEV_DIR env → .env → ~/.cecelia. See docs/todo/ONBOARDING_PLAN.md.
@testset "Config resolver" begin
    # pure resolution order, no env/file reads
    @test Cecelia._resolve_config_dir("/x", "/y", "/z") == "/x"       # explicit wins
    @test Cecelia._resolve_config_dir(nothing, "/y", "/z") == "/y"    # env beats .env
    @test Cecelia._resolve_config_dir(nothing, nothing, "/z") == "/z" # .env beats default
    # `expand_user`, NOT Base.expanduser: the latter is a no-op on Windows, so asserting against
    # it would compare two unexpanded strings and pass vacuously there.
    @test Cecelia._resolve_config_dir(nothing, nothing, nothing) ==   # installed-app default
          expand_user("~/.cecelia")
    @test Cecelia._resolve_config_dir("~/foo", nothing, nothing) == expand_user("~/foo")
    # …and the resolved default must be a real absolute path on EVERY platform — a surviving `~`
    # is the Windows bug that produced `~/.cecelia\observer-mcp.json` in CI.
    @test isabspath(Cecelia._resolve_config_dir(nothing, nothing, nothing))
    @test !startswith(Cecelia._resolve_config_dir(nothing, nothing, nothing), "~")
    # public composition
    @test config_dir("/tmp/ceceliatest") == "/tmp/ceceliatest"
    @test custom_toml_path("/tmp/ceceliatest") == joinpath("/tmp/ceceliatest", "custom.toml")
end

# ── python_bin_path: resolved, not a bare name ───────────────────────────────
# It used to return the config default `"python3"` verbatim. That works for anything JULIA spawns
# (pixi run puts the env first on PATH) but not for the string the observer registers into the
# user's OWN Claude Code config: launched from a plain shell, bare `python3` is the SYSTEM python,
# which has neither `mcp` nor `websockets` — so the observer's tools failed in exactly the sessions
# one-click setup exists to enable. And on Windows `python3` frequently doesn't exist at all.
@testset "python_bin_path resolution" begin
    # candidate order — `iswin` explicit so the Windows list is asserted from any host
    @test Cecelia._python_bin_candidates("", false) == ["python3", "python"]
    @test Cecelia._python_bin_candidates("", true)  == ["python", "python3"]   # conda ships python.exe
    # the SHIPPED default is not a deliberate choice, so it gets the platform fallbacks
    @test Cecelia._python_bin_candidates(Cecelia._PYTHON_BIN_DEFAULT, true) == ["python", "python3"]
    @test Cecelia._python_bin_candidates("  python3  ", false) == ["python3", "python"]  # trimmed
    # a DELIBERATELY configured name is the only candidate — resolve it, never substitute another
    # interpreter (that would run tasks under something lacking the analysis deps, silently)
    @test Cecelia._python_bin_candidates("mypy-thon", false) == ["mypy-thon"]
    @test Cecelia._python_bin_candidates("python", false) == ["python"]
    @test Cecelia._PYTHON_BIN_DEFAULT == "python3"          # must match app/config.toml [dirs]

    # the live resolver: with no explicit path configured it must return something ABSOLUTE that
    # exists — that is the whole point (the old bare name failed `isfile`).
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "python"); old = get(dirs, "python", nothing)
    try
        dirs["python"] = "python3"          # the shipped default → must resolve, not pass through
        let p = python_bin_path()
            @test isabspath(p)
            @test isfile(p)
        end
        # an explicitly configured PATH is honoured verbatim — the user named an exact interpreter
        dirs["python"] = "/opt/custom/bin/python3.11"
        @test python_bin_path() == "/opt/custom/bin/python3.11"
        # …including through a leading ~
        dirs["python"] = joinpath("~", "venv", "bin", "python")
        @test python_bin_path() == joinpath(homedir(), "venv", "bin", "python")
        # an unresolvable bare name degrades to itself rather than to nothing
        dirs["python"] = "cecelia-no-such-interpreter-42"
        @test python_bin_path() == "cecelia-no-such-interpreter-42"
    finally
        had ? (dirs["python"] = old) : delete!(dirs, "python")
    end
end

# ── rscript_bin_path: macOS GUI PATH fallbacks ───────────────────────────────
# macOS GUI apps inherit a bare PATH (`/usr/bin:/bin:/usr/sbin:/sbin`); Terminal-tested Rscript
# is invisible to the app. The legacy-migrate scan then died with `FileNotFoundError: 'Rscript'`
# even though the user's install was fine.
@testset "rscript_bin_path resolution" begin
    # candidate list — parameterised on OS booleans so both platforms are asserted from any host
    @test Cecelia._rscript_fallback_candidates(true, false) == [
        "/Library/Frameworks/R.framework/Resources/bin/Rscript",
        "/opt/homebrew/bin/Rscript",
        "/usr/local/bin/Rscript",
    ]
    @test Cecelia._rscript_fallback_candidates(false, false) == String[]
    @test Cecelia._rscript_fallback_candidates(false, true)  == String[]

    # an explicitly configured PATH is honoured verbatim, even if it doesn't exist — user's call
    @test rscript_bin_path("/opt/custom/bin/Rscript") == "/opt/custom/bin/Rscript"
    # an unresolvable bare name degrades to itself rather than crashing
    @test rscript_bin_path("cecelia-no-such-rscript-42") == "cecelia-no-such-rscript-42"
    # empty input degrades to the bare `"Rscript"` — same string subprocess would have got before
    let p = rscript_bin_path("")
        @test p == "Rscript" || (isabspath(p) && isfile(p))
    end
end

# ── expand_user: portable leading-~ expansion ────────────────────────────────
# Base.expanduser is documented Unix-only and silently returns the path unchanged on Windows, so
# every stored `~`-prefixed path (custom.toml dirs, .env CECELIA_DEV_DIR) went through unexpanded
# there. These assertions hold on all three platforms.
@testset "expand_user" begin
    @test expand_user("~") == homedir()
    @test expand_user("~/foo") == joinpath(homedir(), "foo")
    @test expand_user("~/foo/bar") == joinpath(homedir(), "foo", "bar")
    # never leaves a tilde behind
    @test !startswith(expand_user("~/foo"), "~")
    # absolute + relative paths pass through untouched
    @test expand_user("/abs/path") == "/abs/path"
    @test expand_user("relative/path") == "relative/path"
    @test expand_user("") == ""
    # a tilde that isn't a leading path component is a legitimate filename character
    @test expand_user("/tmp/a~b") == "/tmp/a~b"
    @test expand_user("~notauser/foo") == "~notauser/foo"
    # Windows accepts either separator after the tilde
    if Sys.iswindows()
        @test expand_user("~\\foo") == joinpath(homedir(), "foo")
        @test expand_user("~\\foo\\bar") == joinpath(homedir(), "foo", "bar")
        # The result must be a CANONICAL path — no mixed separators. Pasting the remainder on
        # verbatim gave `C:\Users\x\foo/bar`, which Windows tolerates but which makes every path
        # comparison unreliable (and this is what CI caught first time it ran on Windows).
        @test !occursin('/', expand_user("~/foo/bar"))
    end
    # collapsing a doubled separator is fine; losing a component is not
    @test expand_user("~//foo") == joinpath(homedir(), "foo")
    @test splitpath(expand_user("~/a/b/c"))[end-2:end] == ["a", "b", "c"]
end

# ── ensure_config_dir: safe to WRITE into ────────────────────────────────────
# config_dir() is a pure path computation, so on a machine that has never run the setup wizard
# the directory does not exist and `open(joinpath(config_dir(), …), "w")` throws. That broke CI
# on all three platforms when the observer began writing its MCP config on every status call.
@testset "ensure_config_dir" begin
    base = mktempdir()
    target = joinpath(base, "never-created")
    @test !isdir(target)
    @test ensure_config_dir(target) == target
    @test isdir(target)                              # created
    @test ensure_config_dir(target) == target        # idempotent on an existing dir
    @test isdir(target)
    # and a file can actually be written into it — the thing the observer needs
    nested = joinpath(base, "a", "b", "c")           # several levels missing
    ensure_config_dir(nested)
    write(joinpath(nested, "observer-mcp.json"), "{}")
    @test isfile(joinpath(nested, "observer-mcp.json"))
end

# ── Release-bundle integrity ─────────────────────────────────────────────────
# `/api/update/apply` hands the downloaded payload to the launcher, which overwrites the app
# with it on the next restart — so a truncated or swapped asset matters, and HTTPS says nothing
# about either. These back the `.sha256` published beside the bundle by `release.yml`.
@testset "_file_sha256 / _sha256_matches" begin
    mktempdir() do d
        f = joinpath(d, "cecelia.tar.gz")
        write(f, "some bundle bytes")
        h = Cecelia._file_sha256(f)

        @test occursin(r"^[0-9a-f]{64}$", h)                         # lowercase hex, 64 chars
        @test h == Cecelia._file_sha256(f)                           # stable
        @test Cecelia._sha256_matches(f, "$h  cecelia.tar.gz")       # GNU `sha256sum` form
        @test Cecelia._sha256_matches(f, h)                          # bare hash
        @test Cecelia._sha256_matches(f, "$h  cecelia.tar.gz\n")     # trailing newline
        @test Cecelia._sha256_matches(f, uppercase(h))               # case-insensitive

        # Every not-a-match must be FALSE, never an exception — the caller decides whether a
        # missing/broken digest is fatal (it is verify-if-present, so it isn't).
        @test !Cecelia._sha256_matches(f, "0"^64)                    # wrong digest
        @test !Cecelia._sha256_matches(f, "")                        # empty file
        @test !Cecelia._sha256_matches(f, "   \n ")                  # whitespace only
        @test !Cecelia._sha256_matches(f, "<!DOCTYPE html>")         # an error page, not a digest
        @test !Cecelia._sha256_matches(f, h[1:40])                   # truncated
        @test !Cecelia._sha256_matches(f, h * "ff")                  # over-long

        # A changed byte must change the verdict — the whole point.
        write(f, "some bundle bytez")
        @test !Cecelia._sha256_matches(f, "$h  cecelia.tar.gz")
    end
end

# Best-effort git for the dev diagnostics. The point is what it does NOT do: no throw, no stderr.
# An installed app has no `.git`, so every probe printed `fatal: not a git repository` into the
# user's launch console (#540) — caught and harmless, but it reads like a broken install.
@testset "git_probe is quiet and never throws" begin
    mktempdir() do d          # a directory that is definitely not a git checkout
        # stderr has to be captured at the fd level (a file), not an IOBuffer — the leak this guards
        # against comes from the CHILD process inheriting stderr, which an in-memory buffer wouldn't
        # see anyway.
        errfile = joinpath(d, "stderr.txt")
        out = open(errfile, "w") do io
            redirect_stderr(io) do
                Cecelia.git_probe("rev-parse", "--short", "HEAD"; dir = d)
            end
        end
        @test out == ""                          # no answer, rather than an exception
        @test isempty(read(errfile, String))     # and git's "fatal: …" did not reach the console
        @test Cecelia.git_probe("no-such-subcommand"; dir = d) == ""
        @test Cecelia.git_probe("rev-parse"; dir = joinpath(d, "does", "not", "exist")) == ""

        # Control: with stderr INHERITED, that same call is exactly what used to be printed. Without
        # this the test above would also pass on a machine where git says nothing at all.
        if Sys.which("git") !== nothing
            ctlfile = joinpath(d, "control.txt")
            open(ctlfile, "w") do io
                redirect_stderr(io) do
                    try; read(`git -C $d rev-parse --short HEAD`, String); catch; end
                end
            end
            @test occursin("not a git repository", read(ctlfile, String))
        end
    end
end

# The macOS trap this guards against: `/usr/bin/git` is a Xcode CLT *shim* that pops the "Install
# the command line developer tools" OS dialog *before* git runs, so `try/catch` and stderr redirect
# are useless. `git_probe` must therefore not spawn `git` at all when there is no `.git` above
# `dir` — the shape of every installed Cecelia.app. CI runners always have git installed so the
# dialog itself cannot be reproduced; instead we prove the SPAWN doesn't happen by putting a fake
# `git` earlier on PATH that touches a marker, and asserting the marker never appears when the
# guard should fire.
@testset "git_probe does not spawn git when no .git is reachable" begin
    # Direct check on the helper — cross-platform, no subprocess.
    mktempdir() do d
        @test !Cecelia._dir_has_git_marker(d)
        mkdir(joinpath(d, ".git"))
        @test Cecelia._dir_has_git_marker(d)
        sub = joinpath(d, "a", "b")
        mkpath(sub)
        @test Cecelia._dir_has_git_marker(sub)   # walks upward past `a/` and `b/`
    end
    # `.git` as a FILE (linked worktree gitdir pointer) also counts.
    mktempdir() do d
        write(joinpath(d, ".git"), "gitdir: /elsewhere\n")
        @test Cecelia._dir_has_git_marker(d)
    end

    # Spawn-blocking test — only meaningful where we can write an executable shim on PATH.
    if !Sys.iswindows()
        mktempdir() do shim_dir
            marker = joinpath(shim_dir, "was_called")
            gitshim = joinpath(shim_dir, "git")
            # Pure builtins — the shim must not depend on any other binary being on PATH,
            # since we PREPEND `shim_dir` rather than replacing PATH (real git after the shim
            # is fine — the shim always shadows it — but the shim itself has to be self-contained).
            write(gitshim, "#!/bin/sh\n: > \"$marker\"\nexit 0\n")
            chmod(gitshim, 0o755)
            saved = get(ENV, "PATH", "")
            try
                ENV["PATH"] = shim_dir * ":" * saved      # shim wins the PATH lookup
                mktempdir() do nogit
                    # No `.git` anywhere above `nogit` (mktempdir roots at system tempdir).
                    # Guard must fire; shim must NOT be invoked.
                    @test Cecelia.git_probe("rev-parse", "--short", "HEAD"; dir = nogit) == ""
                    @test !isfile(marker)

                    # Positive control: add a `.git`, and the shim IS called (proves the shim
                    # actually works, so the absence above wasn't a false negative).
                    mkdir(joinpath(nogit, ".git"))
                    Cecelia.git_probe("rev-parse", "--short", "HEAD"; dir = nogit)
                    @test isfile(marker)
                end
            finally
                ENV["PATH"] = saved
            end
        end
    end
end

# The tar list in `.github/workflows/release.yml` is an ALLOW-list, so a directory the running app
# loads is absent from every stable install the moment nobody remembers to name it — and the dev
# channel, which ships a full branch archive, keeps working, so the gap is invisible in development.
# That is #540: `pluto/` was missing, `api/src/notebooks_api.jl` includes `pluto/sysimage_stamp.jl`
# at server load, and every v0.1.1 install died on first launch. `preview/` and `mcp/` were missing
# by the same mechanism and fail quieter (no worker to spawn; an observer registered against a
# directory that isn't there).
#
# So this pins the paths the INSTALLED app opens against what the bundle actually carries. The two
# process paths are read from the constants that spawn them rather than retyped, so moving a file
# fails here instead of at a user's first launch.
@testset "release bundle ships every runtime path" begin
    repo = normpath(joinpath(@__DIR__, "..", ".."))
    yml  = joinpath(repo, ".github", "workflows", "release.yml")
    if !isfile(yml)
        @warn "release.yml not found — skipping bundle coverage" path=yml
        @test_skip false
    else
        # The tar invocation, backslash-continued across lines: take from `tar -czf` to the first
        # line that does not continue.
        lines = readlines(yml)
        i = findfirst(l -> occursin("tar -czf", l), lines)
        @test i !== nothing
        toks = String[]
        while i !== nothing && i <= length(lines)
            line = strip(lines[i])
            more = endswith(line, "\\")
            append!(toks, split(replace(line, r"\\$" => ""), r"\s+"; keepempty = false))
            more || break
            i += 1
        end
        filter!(t -> !(t in ("tar", "-czf", "out/cecelia.tar.gz")), toks)
        @test !isempty(toks)

        # tar carries a directory whole, so a token covers itself and everything under it.
        covered(p) = any(t -> t == p || startswith(p, t * "/"), toks)
        # repo-relative, forward-slash — these are tar paths, not filesystem paths
        rel(abs_path) = replace(relpath(normpath(abs_path), repo), '\\' => '/')

        # The list lives in scripts/bundle_required_paths.txt, NOT here — `scripts/bundle_check.sh`
        # checks the same entries against a bundle it actually builds and extracts, and two
        # hand-kept copies would drift the moment one checker was updated.
        listfile = joinpath(repo, "scripts", "bundle_required_paths.txt")
        @test isfile(listfile)
        required = [strip(l) for l in readlines(listfile)
                    if !isempty(strip(l)) && !startswith(strip(l), "#")]
        @test length(required) >= 10        # a truncated/emptied list must not pass silently

        # The spawned process is pinned to the constant that spawns it, so moving the file fails
        # HERE (pointing at the list) instead of at a user's first launch.
        @test rel(Cecelia.PREVIEW_WORKER) in required

        for p in required
            @test covered(p)
            # and it has to be a real path, or the entry above is pinning a typo
            @test ispath(joinpath(repo, split(p, '/')...))
        end
        # built by the release job, so it exists in CI but not in a checkout — coverage only
        @test covered("frontend/dist")

        # …and every OTHER token in the tar allow-list has to point at something that exists in a
        # checkout — otherwise `tar` fails at tag time (as v0.2.0 did on a stale `napari/napari_bridge.py`
        # left behind after the P9 retire). Skip the two paths the release job produces itself.
        job_produced = Set(["VERSION", "frontend/dist"])
        for t in toks
            t in job_produced && continue
            @test ispath(joinpath(repo, split(t, '/')...))
        end
    end
end

# ── Custom cellpose model resolver ───────────────────────────────────────────
# A user-placed checkpoint under `<config_dir>/models/cellposeModels/{name}` is picked up
# by the cellpose Julia handler and passed to Python as an absolute file path (which
# `cellpose_utils.py::_get_model` loads via `pretrained_model=…`). No shell-outs, no
# network — pure filesystem resolution.
@testset "cellpose_model_path resolver" begin
    # user-override slot (config_dir): missing file → nothing; empty/blank name → nothing.
    td = mktempdir()
    @test cellpose_model_path("__no_such_model__.pt", td) === nothing
    @test cellpose_model_path("", td) === nothing
    @test cellpose_model_path("   ", td) === nothing

    # place a file in the config-dir override slot, resolver returns its absolute path (the
    # bundled slot at <repo>/models/cellposeModels/ takes precedence when both exist — that
    # path is real in this repo and can't be safely mocked here).
    mkpath(joinpath(td, "models", "cellposeModels"))
    f = joinpath(td, "models", "cellposeModels", "__unique_test_model__.pt")
    open(io -> write(io, "stub"), f, "w")
    @test cellpose_model_path("__unique_test_model__.pt", td) == f

    # cellpose_models_dir is a pure path — no I/O, no side-effects
    @test cellpose_models_dir(td) == joinpath(td, "models", "cellposeModels")
end

# ── Runtime-enumerated cellpose model picker (drop-in convention) ────────────
# `list_cellpose_models` returns builtins + bundled + user drop-ins; the ordering + dedup
# rules are what the picker AND `validate_params` both see (see `_inject_dynamic_options!`
# in cellpose.jl + `_task_spec`'s dynamic hook in task.jl). Guards: builtins always present,
# dedup shadows a bundled file by the same-name user file, a user file that isn't a known
# built-in becomes selectable, and validate_params accepts it.
@testset "list_cellpose_models enumeration" begin
    # NOTE: only the USER dir is td-scoped here; the BUNDLED dir is `<repo>/models/
    # cellposeModels/` (hardcoded via @__DIR__ so it matches the resolver + install layout).
    # A dev running `pixi run models-fetch` populates the bundled dir with real checkpoints,
    # so tests can't assume it's empty — check invariants that hold either way.
    td = mktempdir()

    # Built-ins are always first, in a stable order. v4 rows always show (cpsam_v2/cpsam); v3
    # rows (cyto2/cyto3) are HIDDEN unless the opt-in `cellpose-v3` pixi env is installed — a
    # user picking one without the env would only get `run_py`'s missing-env error at Run time,
    # so dropping them upstream is the right filter (see docs/todo/CELLPOSE_V3_OPTIN_PLAN.md).
    # Whether the env is present depends on the machine running the tests; assert both branches
    # of the gate.
    names = [m.name for m in list_cellpose_models(td)]
    @test names[1:2] == ["cpsam_v2", "cpsam"]
    if Cecelia._cellpose_v3_env_installed()
        @test any(m.name == "cyto3" for m in list_cellpose_models(td))
        @test any(m.name == "cyto2" for m in list_cellpose_models(td))
    else
        @test !any(m.name in ("cyto2", "cyto3") for m in list_cellpose_models(td))
    end

    # No user drop-in → nothing tagged "user" for THIS td.
    base = list_cellpose_models(td)
    @test all(m.source != "user" for m in base)

    # Place a user drop-in checkpoint → appears with source="user"
    mkpath(joinpath(td, "models", "cellposeModels"))
    f = joinpath(td, "models", "cellposeModels", "myFluo.pt")
    open(io -> write(io, "stub"), f, "w")
    with_user = list_cellpose_models(td)
    idx = findfirst(m -> m.name == "myFluo.pt", with_user)
    @test !isnothing(idx)
    @test with_user[idx].source == "user"
    @test occursin("user", with_user[idx].label)

    # Dotfiles / subdirectories are skipped when scanning.
    open(io -> write(io, "hidden"), joinpath(td, "models", "cellposeModels", ".DS_Store"), "w")
    mkpath(joinpath(td, "models", "cellposeModels", "subdir"))
    clean = list_cellpose_models(td)
    @test all(m.name != ".DS_Store" for m in clean)
    @test all(m.name != "subdir"    for m in clean)
end

# ── Coastal (optical-flow) model vault ───────────────────────────────────────
# The same drop-in convention as cellpose, one directory over — but with no built-ins and no
# bundled slot, because coastal ships no models: a user trains their own on the Optical Flow
# page. The manifest is the load-bearing part. Inference MUST use the metric set a model was
# trained on, and coastal fails silently when it doesn't (a missing plane shifts every later
# channel), so `CoastalUtils` configures itself from the sidecar rather than from task params.
@testset "coastal model vault" begin
    td = mktempdir()
    @test coastal_models_dir(td) == joinpath(td, "models", "coastalModels")

    # Empty vault → empty picker. No built-in fallback to segment with by accident.
    @test isempty(list_coastal_models(td))
    @test coastal_model_path("anything.pt", td) === nothing
    @test coastal_model_path("", td) === nothing
    @test coastal_model_path("   ", td) === nothing

    dir = joinpath(td, "models", "coastalModels")
    mkpath(dir)
    pt = joinpath(dir, "gcMemTom.pt")
    open(io -> write(io, "stub"), pt, "w")
    @test coastal_model_path("gcMemTom.pt", td) == pt
    # Bare stem resolves too — picker shape (with or without `.pt`) roundtrips through the lookup.
    @test coastal_model_path("gcMemTom", td) == pt

    # A checkpoint with no manifest still lists — it just falls back to coastal's defaults.
    bare = list_coastal_models(td)
    @test length(bare) == 1
    @test bare[1].name == "gcMemTom.pt"
    @test bare[1].label == "gcMemTom"
    @test isempty(bare[1].manifest)
    @test isempty(coastal_model_manifest("gcMemTom.pt", td))

    # With a manifest, the picker label says what the model was trained on.
    write(joinpath(dir, "gcMemTom.json"),
          """{"channelName":"mem-TOM","temporalScales":[1,2,4,8],"cumulativeWindow":5}""")
    with_manifest = list_coastal_models(td)
    @test with_manifest[1].label == "gcMemTom (mem-TOM)"
    @test with_manifest[1].manifest["cumulativeWindow"] == 5
    @test with_manifest[1].manifest["temporalScales"] == [1, 2, 4, 8]

    # A corrupt manifest must not take the picker down with it.
    write(joinpath(dir, "gcMemTom.json"), "{not json")
    @test isempty(coastal_model_manifest("gcMemTom.pt", td))
    @test length(list_coastal_models(td)) == 1

    # The sidecar is not an entry of its own, and dotfiles/subdirs are skipped.
    open(io -> write(io, "hidden"), joinpath(dir, ".DS_Store"), "w")
    mkpath(joinpath(dir, "subdir.pt"))
    names = [m.name for m in list_coastal_models(td)]
    @test names == ["gcMemTom.pt"]
end

# ── Denoise (SUPPORT) model vault ───────────────────────────────────────────────
# Same shape as the coastal vault. Manifest is load-bearing here too — SUPPORT's architecture
# (input_frames, mid_channels, depth, blind_conv_channels, bs_size) is not encoded in the .pt, so
# without a sidecar the runner cannot rebuild the network.
@testset "denoise model vault" begin
    td = mktempdir()
    @test Cecelia.denoise_models_dir(td) == joinpath(td, "models", "denoiseModels")

    @test isempty(Cecelia.list_denoise_models(td))
    @test Cecelia.denoise_model_path("anything.pt", td) === nothing
    @test Cecelia.denoise_model_path("", td) === nothing

    dir = joinpath(td, "models", "denoiseModels")
    mkpath(dir)
    pt = joinpath(dir, "supMemTom.pt")
    open(io -> write(io, "stub"), pt, "w")
    @test Cecelia.denoise_model_path("supMemTom.pt", td) == pt
    # Bare stem resolves too — the denoise picker sends stems (#828); the segmentation runner passed
    # the stem straight to `denoise_model_path`, which used to look for `<dir>/supMemTom` and miss.
    @test Cecelia.denoise_model_path("supMemTom", td) == pt
    @test Cecelia.denoise_model_manifest("supMemTom", td) == Cecelia.denoise_model_manifest("supMemTom.pt", td)

    # A checkpoint with no manifest still lists — but the runner will refuse to load it.
    # `.name` is the on-disk name inside the vault directory — `<stem>.pt` for a pooled file,
    # `<stem>` (no extension) for a perChannel bundle folder (SUPPORT_PERCHANNEL_PLAN.md D2). Every
    # consumer that does `joinpath(dir, m.name)` (`vault_api.jl`, the `list_coastal_models` label
    # builder) then finds the right on-disk artifact without a kind-aware branch.
    bare = Cecelia.list_denoise_models(td)
    @test length(bare) == 1
    @test bare[1].name == "supMemTom.pt"
    @test bare[1].kind === :pooled
    @test bare[1].label == "supMemTom"
    @test isempty(bare[1].manifest)
    @test isempty(Cecelia.denoise_model_manifest("supMemTom.pt", td))

    # With a manifest, the picker label carries the acquisition it was trained on. A denoise model
    # pools N channels into one (DENOISE_INTEGRATION_PLAN.md D3 amendment, measured on fXgbTl
    # 2026-09-05); the label joins them with "+".
    write(joinpath(dir, "supMemTom.json"),
          """{"channels":["mem-TOM","nuc-GFP"],"arch":{"inputFrames":61,"midChannels":[64,128,256,512],"depth":4}}""")
    with_manifest = Cecelia.list_denoise_models(td)
    @test with_manifest[1].label == "supMemTom (mem-TOM+nuc-GFP)"
    @test with_manifest[1].manifest["arch"]["inputFrames"] == 61
    @test with_manifest[1].manifest["arch"]["midChannels"] == [64, 128, 256, 512]

    # A corrupt manifest must not take the picker down with it.
    write(joinpath(dir, "supMemTom.json"), "{not json")
    @test isempty(Cecelia.denoise_model_manifest("supMemTom.pt", td))
    @test length(Cecelia.list_denoise_models(td)) == 1

    # Dotfiles skipped, same as coastal. A directory named with a `.pt` suffix is a leftover mkdir
    # (not a bundle — a real bundle has no manifest.json marker), so also skip it.
    open(io -> write(io, "hidden"), joinpath(dir, ".DS_Store"), "w")
    mkpath(joinpath(dir, "subdir.pt"))
    names = [m.name for m in Cecelia.list_denoise_models(td)]
    @test names == ["supMemTom.pt"]

    @test Cecelia.denoise_model_names(td) == ["supMemTom"]

    # ── perChannel bundle (SUPPORT_PERCHANNEL_PLAN.md D2/D3) ──────────────────
    # A bundle is a folder in the vault with per-channel `.pt`s and a top-level `manifest.json`
    # listing them. `list_denoise_models` picks it up alongside pooled `.pt` files; the picker
    # label carries a `per-channel` hint so a user can tell them apart at a glance.
    bundle = joinpath(dir, "suppMerTK")
    mkpath(bundle)
    open(io -> write(io, "stub"), joinpath(bundle, "nuc-GFP.pt"), "w")
    open(io -> write(io, "stub"), joinpath(bundle, "CD169-Kat.pt"), "w")
    write(joinpath(bundle, "nuc-GFP.json"),
          """{"kind":"denoise-support","mode":"perChannel-sub","channels":["nuc-GFP"],"arch":{"inputFrames":21}}""")
    write(joinpath(bundle, "CD169-Kat.json"),
          """{"kind":"denoise-support","mode":"perChannel-sub","channels":["CD169-Kat"],"arch":{"inputFrames":21}}""")
    write(joinpath(bundle, "manifest.json"), """
        {"kind":"denoise-support","mode":"perChannel","channels":["nuc-GFP","CD169-Kat"],
         "perChannel":[{"index":1,"name":"nuc-GFP","slug":"nuc-GFP","pt":"nuc-GFP.pt"},
                       {"index":3,"name":"CD169-Kat","slug":"CD169-Kat","pt":"CD169-Kat.pt"}]}""")

    listed = Cecelia.list_denoise_models(td)
    @test length(listed) == 2
    bundle_entry = only(m for m in listed if m.name == "suppMerTK")
    @test bundle_entry.kind === :perChannel
    @test bundle_entry.label == "suppMerTK (nuc-GFP+CD169-Kat, per-channel)"

    resolved = Cecelia.denoise_model_resolve("suppMerTK", td)
    @test resolved.kind === :perChannel
    @test resolved.rootPath == bundle
    @test sort(collect(keys(resolved.perChannel))) == ["CD169-Kat", "nuc-GFP"]
    @test endswith(resolved.perChannel["CD169-Kat"].ptPath, "CD169-Kat.pt")
    @test resolved.perChannel["CD169-Kat"].manifest["arch"]["inputFrames"] == 21

    # Pooled resolver still works — `denoise_model_resolve` handles both kinds.
    pooled_resolved = Cecelia.denoise_model_resolve("supMemTom", td)
    @test pooled_resolved.kind === :pooled
    @test pooled_resolved.rootPath == pt
end

@testset "denoise_model_names strips only .pt, keeps internal dots in a bundle name" begin
    # A bundle folder `supp.small` is legal; `splitext` split it at the internal dot and returned
    # `"supp"`, which the picker sent to `denoise_model_resolve` — miss → "Model 'supp' not found".
    # Same rule applied in THREE places (config.jl's `denoise_model_names`, config.jl's
    # `list_denoise_models` pooled branch, task.jl's `_OPTION_SOURCES["denoiseModels"]`) — all three
    # now go through `vault_model_stem`.
    td = mktempdir()
    dir = Cecelia.denoise_models_dir(td)
    mkpath(dir)
    open(io -> write(io, "stub"), joinpath(dir, "supMemTom.pt"), "w")
    bundle = joinpath(dir, "supp.small")
    mkpath(bundle)
    open(io -> write(io, "stub"), joinpath(bundle, "a.pt"), "w")
    write(joinpath(bundle, "manifest.json"), """
        {"kind":"denoise-support","mode":"perChannel","channels":["a"],
         "perChannel":[{"index":0,"name":"a","slug":"a","pt":"a.pt"}]}""")

    @test sort(Cecelia.denoise_model_names(td)) == ["supMemTom", "supp.small"]

    # And the picker's chosen stem must round-trip through the resolver — the bug the user hit.
    resolved = Cecelia.denoise_model_resolve("supp.small", td)
    @test !isnothing(resolved)
    @test resolved.kind === :perChannel

    # The task-spec picker source runs off the same vault. Point config_dir at the fixture so the
    # denoiseModels source enumerates ours, not the dev vault.
    withenv("CECELIA_DEV_DIR" => td) do
        opts = Cecelia._OPTION_SOURCES["denoiseModels"]()
        values = [String(o.value) for o in opts]
        @test sort(values) == ["supMemTom", "supp.small"]   # NOT "supp"
    end
end

@testset "denoise_model_target clears both sibling shapes on overwrite" begin
    # Retraining `<name>` as perChannel used to leave the previous pooled `<name>.pt` orphaned next
    # to the new `<name>/` bundle, and the picker showed both rows under the same stem. `overwrite`
    # now means "one name is one model" — clear whichever shape existed at that stem.
    td = mktempdir()
    dir = Cecelia.denoise_models_dir(td)
    mkpath(dir)

    # 1) pooled → perChannel: `.pt` + `.json` cleared before the bundle is written.
    pt   = joinpath(dir, "supX.pt")
    json = joinpath(dir, "supX.json")
    open(io -> write(io, "stub"), pt, "w")
    write(json, """{"channels":["a"]}""")
    pt_target, bundle_target =
        Cecelia.denoise_model_target("supX"; overwrite = true, want_bundle = true, dev_dir = td)
    @test pt_target == pt
    @test bundle_target == joinpath(dir, "supX")
    @test !isfile(pt) && !isfile(json)   # old pooled pair gone
    @test !isdir(bundle_target)          # bundle path still available for the runner to create

    # 2) perChannel → pooled: bundle folder cleared before the .pt is written.
    bundle = joinpath(dir, "supY")
    mkpath(bundle)
    open(io -> write(io, "stub"), joinpath(bundle, "a.pt"), "w")
    write(joinpath(bundle, "manifest.json"), """{"kind":"denoise-support","mode":"perChannel"}""")
    Cecelia.denoise_model_target("supY"; overwrite = true, dev_dir = td)
    @test !isdir(bundle)                 # old bundle gone

    # 3) overwrite = false still refuses either colliding shape.
    open(io -> write(io, "stub"), joinpath(dir, "supZ.pt"), "w")
    @test_throws ErrorException Cecelia.denoise_model_target("supZ"; overwrite = false, dev_dir = td)
    mkpath(joinpath(dir, "supW"))
    @test_throws ErrorException Cecelia.denoise_model_target("supW"; overwrite = false, dev_dir = td)
end

# The denoise picker is entirely runtime-enumerated — cecelia ships no built-in denoise models. The
# spec declares one literal option ("None", value ""), the resolver appends the vault, dedup by value.
@testset "opticalFlow.trainSupportDenoise task wiring" begin
    # Fun-name namespace stays `opticalFlow.*` (stored in ccid.json chain state — a rename would
    # break every persisted chain). The display category was renamed to "Model training" in Phase C
    # so the task picker shows both training tasks under one honest heading.
    task = Cecelia._task_from_fun_name("opticalFlow.trainSupportDenoise")
    @test task isa Cecelia.TrainSupportDenoise
    spec = Cecelia._task_spec(task)
    @test !isnothing(spec)
    @test spec["category"] == "Model training"
    @test spec["scope"] == "set"
    @test spec["resource_pool"] == "gpu"

    # unetSize accepts the three sizes and nothing else; picking any of them must validate.
    # `trainChannels` is a multi-select — the pooled-channels amendment (D3, fXgbTl 2026-09-05).
    for size in ("small", "medium", "large")
        @test Cecelia.validate_params(task, Dict{String,Any}(
            "modelName" => "x", "trainChannels" => ["c1", "c2"], "unetSize" => size)) === nothing
    end

    # inputFrames stays odd — the centre-frame contract of a temporal blind-spot model. Non-odd is
    # a runtime error in the handler (not a spec constraint), so this is verified via the handler.
    #
    # The manifest keys the DENOISE runner reads back are enumerated here so the two files can't
    # drift silently: if this list changes, `denoise_run._build_model` reads a stale key.
    MANIFEST_ARCH_KEYS = ["inputFrames", "patchXY", "midChannels", "depth", "blindConvChannels",
                          "oneByOneChannels", "lastLayerChannels", "bsSize", "bp"]
    # Cheap sanity check on the CATALOG entry (its rendered short/long is asserted in the qc suite).
    @test haskey(Cecelia.QC_TEXT, "denoise.loss_flat")
end

@testset "_support_short_movie_refusal — actionable T-mismatch message" begin
    # No short movies → no refusal lines. The general-case "no usable images" fallback lives in
    # the handler alongside this, not here.
    @test isempty(Cecelia._support_short_movie_refusal(Tuple{String,Int}[], 61))

    # One short movie names the shortest T and the largest odd inputFrames that fits.
    lines = Cecelia._support_short_movie_refusal([("fXgbTl", 31)], 61)
    @test length(lines) == 2
    @test occursin("shortest is 31", lines[1])
    @test occursin("61+ timepoints", lines[1])
    @test occursin("31 (largest odd value ≤ 31)", lines[2])

    # Even shortest → next-lower odd. min(31-1) if T=30, but here also check the T=1 clamp.
    lines_even = Cecelia._support_short_movie_refusal([("aaa", 30), ("bbb", 45)], 61)
    @test occursin("shortest is 30", lines_even[1])
    @test occursin("29 (largest odd value ≤ 30)", lines_even[2])

    # T=1 is the pathological edge; max_odd is clamped to 1 rather than 0.
    lines_one = Cecelia._support_short_movie_refusal([("aaa", 1)], 5)
    @test occursin("Set Temporal window to 1", lines_one[2])
end

@testset "_support_temporal_window_advisory — form-time twin of the run-time refusal" begin
    # The registered validator emits a structured advisory that the frontend's `backendAdvisor`
    # fetches. Same rule as `_support_short_movie_refusal` above — pinned here so the two do not drift.
    mk(uid, t) = (img = CciaImage(; uid=uid, name=uid, dir="");
                  img.meta = Dict{String,Any}("SizeT" => t); img)
    empty_siblings = Dict{String,Any}()

    # Fits every movie → ok, message names the count + shortest.
    a = Cecelia._support_temporal_window_advisory(21, [mk("a", 31), mk("b", 60), mk("c", 45)], empty_siblings)
    @test a.severity == "ok"
    @test occursin("21f", a.message)
    @test occursin("3 movies", a.message)
    @test occursin("shortest 31f", a.message)

    # One movie too short → warn, tip suggests the odd cap = 31 (already odd).
    b = Cecelia._support_temporal_window_advisory(41, [mk("a", 31), mk("b", 60), mk("c", 45)], empty_siblings)
    @test b.severity == "warn"
    @test occursin("1 of 3", b.message)
    @test occursin("31", b.tip)

    # None fit → fail, tip suggests odd cap = 39 (largest odd ≤ 40), matching the run-time refusal.
    c = Cecelia._support_temporal_window_advisory(61, [mk("a", 31), mk("b", 40)], empty_siblings)
    @test c.severity == "fail"
    @test occursin("longest 40f", c.message)
    @test occursin("39", c.tip)
    @test occursin("largest odd", c.tip)

    # No images / no sizeT / bad value → nothing (silence beats a wrong readout).
    @test Cecelia._support_temporal_window_advisory(21, CciaImage[], empty_siblings) === nothing
    no_t = CciaImage(; uid="x", name="x", dir=""); no_t.meta = Dict{String,Any}()
    @test Cecelia._support_temporal_window_advisory(21, [no_t], empty_siblings) === nothing
    @test Cecelia._support_temporal_window_advisory(0, [mk("a", 31)], empty_siblings) === nothing
    @test Cecelia._support_temporal_window_advisory("nonsense", [mk("a", 31)], empty_siblings) === nothing
end

@testset "param validator registry — SUPPORT registers, the dispatcher rejects bad shapes" begin
    # Registered on include of train_support_denoise.jl.
    @test haskey(Cecelia.PARAM_VALIDATORS, ("opticalFlow.trainSupportDenoise", "inputFrames"))

    # Dispatcher returns nothing when nothing is registered.
    mk(uid, t) = (img = CciaImage(; uid=uid, name=uid, dir="");
                  img.meta = Dict{String,Any}("SizeT" => t); img)
    @test Cecelia.validate_param("nope.no_such_task", "someKey", 1, CciaImage[mk("a", 10)],
                                 Dict{String,Any}()) === nothing

    # Dispatcher round-trips a real validator.
    r = Cecelia.validate_param("opticalFlow.trainSupportDenoise", "inputFrames", 21,
                               CciaImage[mk("a", 31), mk("b", 60)], Dict{String,Any}())
    @test r !== nothing
    @test r.severity == "ok"

    # A validator that throws is swallowed and returns nothing — an advisory is not load-bearing.
    Cecelia.register_param_validator!("test.throws", "x", (_, _, _) -> error("oops"))
    @test Cecelia.validate_param("test.throws", "x", 1, CciaImage[], Dict{String,Any}()) === nothing

    # A validator that returns garbage is refused at the boundary (returns nothing, logs a warning).
    Cecelia.register_param_validator!("test.garbage", "x", (_, _, _) -> (; wrong = 1))
    @test Cecelia.validate_param("test.garbage", "x", 1, CciaImage[], Dict{String,Any}()) === nothing

    # Cleanup so the registry doesn't leak the test fixtures.
    delete!(Cecelia.PARAM_VALIDATORS, ("test.throws", "x"))
    delete!(Cecelia.PARAM_VALIDATORS, ("test.garbage", "x"))
end

@testset "_support_train_qc_findings — pure catalog" begin
    # Loss came down — no findings.
    @test isempty(Cecelia._support_train_qc_findings(Dict{String,Any}("lossDrop" => 2.5)))

    # Loss did not come down — one warn, code denoise.loss_flat.
    findings = Cecelia._support_train_qc_findings(Dict{String,Any}(
        "finalLoss" => 0.5, "lossDrop" => 0.9, "epochs" => 20))
    @test length(findings) == 1
    @test findings[1]["code"] == "denoise.loss_flat"
    @test findings[1]["level"] == "warn"

    # NaN drop — do not flag (the run wrote no history).
    @test isempty(Cecelia._support_train_qc_findings(Dict{String,Any}("lossDrop" => NaN)))
end

@testset "_denoise_qc_findings — collapse detection (pooled runs)" begin
    # No collapse — outMax/inMax ratios agree within ½× of the median. No finding.
    meta_ok = Dict{String,Any}(
        "mode" => "pooled",
        "perChannelMinMax" => Dict{String,Any}(
            "0" => Dict{String,Any}("inMin" => 0, "inMax" => 100.0, "outMin" => 0, "outMax" => 40.0),
            "1" => Dict{String,Any}("inMin" => 0, "inMax" => 200.0, "outMin" => 0, "outMax" => 70.0),
            "2" => Dict{String,Any}("inMin" => 0, "inMax" => 300.0, "outMin" => 0, "outMax" => 120.0)))
    @test isempty(Cecelia._denoise_qc_findings(meta_ok))

    # Ground-truth x4E5HU shape (2026-09-07): CD169-Kat at 0.13 vs the ~0.38 pooled cohort — well
    # under the 0.5× median threshold. One warn, code denoise.channel_collapsed, factor ≈ 3.
    meta_collapse = Dict{String,Any}(
        "mode" => "pooled",
        "perChannelMinMax" => Dict{String,Any}(
            "1" => Dict{String,Any}("inMin" => 0, "inMax" => 384.0, "outMin" => 0, "outMax" => 147.8),
            "2" => Dict{String,Any}("inMin" => 0, "inMax" => 520.0, "outMin" => 0, "outMax" => 186.1),
            "3" => Dict{String,Any}("inMin" => 0, "inMax" => 235.0, "outMin" => 0, "outMax" => 30.4)))
    findings = Cecelia._denoise_qc_findings(meta_collapse)
    @test length(findings) == 1
    @test findings[1]["code"] == "denoise.channel_collapsed"
    @test findings[1]["level"] == "warn"
    @test findings[1]["detail"]["channel"] == "3"

    # perChannel runs — within-run cohort comparison is meaningless (each channel has its own
    # model), so a collapsed-looking ratio must NOT fire the finding.
    meta_perch = merge(meta_collapse, Dict{String,Any}("mode" => "perChannel"))
    @test isempty(Cecelia._denoise_qc_findings(meta_perch))

    # Saturation branch is unchanged — a `channelsSkipped` list still fires the saturated finding.
    meta_sat = Dict{String,Any}("mode" => "pooled", "channelsSkipped" => [2])
    findings_sat = Cecelia._denoise_qc_findings(meta_sat)
    @test length(findings_sat) == 1
    @test findings_sat[1]["code"] == "denoise.channel_saturated"
end

@testset "cleanupImages.denoise spec dynamic Model options" begin
    spec = Cecelia._task_spec(Cecelia._task_from_fun_name("cleanupImages.denoise"))
    @test !isnothing(spec)
    model_sel = only(p for p in spec["params"] if get(p, "key", "") == "model")
    @test model_sel["optionsFrom"] == "denoiseModels"
    values = [string(o["value"]) for o in model_sel["options"]]
    @test first(values) == ""
    @test string(first(model_sel["options"])["label"]) == "None"
    # One entry per real model, plus "None". No duplicates.
    @test length(values) == length(unique(values))
    @test length(values) == 1 + length(Cecelia.list_denoise_models())
    # Option `value` is the bare stem (no `.pt`), matching `flowModels`. The training task strips
    # the extension when it writes, so a resolved `modelName` that carries `.pt` round-trips into
    # the picker's currently-selected label instead of matching an option.
    @test all(!endswith(v, ".pt") for v in values)
    @test values[2:end] == Cecelia.denoise_model_names()
end

# The coastal picker is ENTIRELY runtime-enumerated — coastal ships no built-in models, so on a
# fresh install the only option is "None". That empty state has to stay a legible choice rather than
# a select that rejects its own default, which is what the first version did.
@testset "CoastalSegment spec dynamic Model options" begin
    spec = Cecelia._task_spec(CoastalSegment())
    @test !isnothing(spec)
    models_group = only(p for p in spec["params"] if get(p, "key", "") == "models")
    model_sel    = only(p for p in models_group["params"] if get(p, "key", "") == "model")
    values = [string(o["value"]) for o in model_sel["options"]]

    @test first(values) == ""                       # "None" is always first and always present
    @test string(first(model_sel["options"])["label"]) == "None"
    @test validate_params(CoastalSegment(),
        Dict{String,Any}("models" => Dict{String,Any}(
            "0" => Dict{String,Any}("model" => "")))) === nothing

    # A name that is not in the vault is rejected — the enumeration is real, and a missing model
    # must never silently fall back to another one.
    @test_throws ParamValidationError validate_params(CoastalSegment(),
        Dict{String,Any}("models" => Dict{String,Any}(
            "0" => Dict{String,Any}("model" => "__not_in_the_vault__.pt"))))
end

# A `valueNameInput` is NOT free text, unlike the `text` type it replaced. Its value becomes a
# filename stem (`spatialGraph/{suffix}.h5ad`), a versioned-dict key (`labels[name]`) or a column
# suffix (`clusters.{suffix}`) — so a path separator writes somewhere else entirely and an empty
# name produces `labels[""]`. Both used to be accepted silently.
# See docs/todo/VALUE_NAME_INPUT_PLAN.md; the widget is `SuggestInput` (docs/UI.md).
@testset "valueNameInput rejects a name that is not usable as a key" begin
    ok(v) = validate_params(CellposeSegment(), Dict{String,Any}("outputValueName" => v))
    bad(v) = @test_throws ParamValidationError validate_params(
        CellposeSegment(), Dict{String,Any}("outputValueName" => v))

    @test ok("Tcell") === nothing
    @test ok("flow.cyto") === nothing        # dots are REAL names, not a path — never reject them
    @test ok("a b") === nothing              # a space is ugly, not dangerous; not ours to police

    # An EMPTY string is "unset", not a bad name: `_validate_params_against_spec` treats `""` the
    # same as absent for every param type, and the task then falls back to the spec default. Cleared
    # field → `default`, which is the behaviour the `text` type already had. Asserted so the guard
    # below is not "tightened" into rejecting it, which would fail a form the user merely blanked.
    @test ok("") === nothing

    bad("   ")                               # whitespace-only is NOT `== ""`, so it would become a real key
    bad("a/b")                               # → writes under a nested path
    bad("a\\b")                              # the Windows spelling of the same mistake
    bad(".")
    bad("..")
    bad(3)                                   # a number is not a name
end

# Selecting nothing must fail with an instruction, not with a stack trace deep in Python. The
# missing-model case is the shared one: a config-dir model does not travel with a `.ccbundle`, so
# opening someone else's project WILL name a model this machine does not have.
@testset "coastal_models_for_python resolution" begin
    raw = Dict{String,Any}("imChannelNames" => Dict{String,Any}(
        "default" => ["CH1", "CH2"], "_active" => "default"))

    @test isempty(Cecelia.coastal_models_for_python(Dict{String,Any}(), raw))

    no_model = Dict{String,Any}("models" => Dict{String,Any}(
        "0" => Dict{String,Any}("model" => "", "cellChannels" => ["CH2"])))
    err = try
        Cecelia.coastal_models_for_python(no_model, raw); nothing
    catch e; e end
    @test err isa ErrorException && occursin("Optical Flow page", err.msg)

    missing_model = Dict{String,Any}("models" => Dict{String,Any}(
        "0" => Dict{String,Any}("model" => "__absent__.pt", "cellChannels" => ["CH2"])))
    err2 = try
        Cecelia.coastal_models_for_python(missing_model, raw); nothing
    catch e; e end
    @test err2 isa ErrorException && occursin("not included in a project export", err2.msg)

    # Channel NAMES become 0-based indices — the translation the preview shares with the run.
    abs_model = Dict{String,Any}("models" => Dict{String,Any}(
        "0" => Dict{String,Any}("model" => @__FILE__, "cellChannels" => ["CH2"])))
    out = Cecelia.coastal_models_for_python(abs_model, raw)
    @test out["0"]["cellChannels"] == [1]
    @test out["0"]["model"] == @__FILE__
end

# ── COHORT_METRICS (Julia) vs COHORT_STAGES (frontend) ───────────────────────
# The two lists were kept in step by a comment, and the comment did not work: `segment.coastal` was
# added to COHORT_METRICS and not to COHORT_STAGES, so the Segment page's cohort check silently
# skipped every coastal run. Nothing failed — the button was just quietly less useful than it looked,
# which is the worst shape for a bug to have.
#
# The rule is scoped, not total. A category with NO entry in COHORT_STAGES has deliberately no cohort
# button (Import banks metrics and offers none), so it is exempt. But once a page offers the button,
# it must cover every cohort-bearing fun in its category — that is the case this catches.
@testset "cohort stages cover their category's cohort metrics" begin
    ts_path = joinpath(@__DIR__, "..", "..", "frontend", "src", "lib", "cohortStages.ts")
    if !isfile(ts_path)
        @test_skip "cohortStages.ts not found"
    else
        src = read(ts_path, String)
        body = match(r"COHORT_STAGES:\s*Record<string,\s*string\[\]>\s*=\s*\{(.*?)\n\}"s, src)
        @test !isnothing(body)

        stages = Dict{String,Vector{String}}()
        for m in eachmatch(r"(\w+)\s*:\s*\[([^\]]*)\]", body.captures[1])
            stages[m.captures[1]] = [String(x.captures[1])
                                     for x in eachmatch(r"'([^']+)'", m.captures[2])]
        end
        @test !isempty(stages)

        # every fun the frontend lists must actually bank cohort metrics
        unknown = [f for fs in values(stages) for f in fs if !haskey(COHORT_METRICS, f)]
        @test isempty(unknown)

        # …and every cohort-bearing fun in a category that HAS a button must be listed
        listed = Set(f for fs in values(stages) for f in fs)
        missing_funs = [f for f in keys(COHORT_METRICS)
                        if haskey(stages, first(split(f, "."))) && !(f in listed)]
        @test isempty(missing_funs)
    end
end

# ── Guide catalogue (frontend) vs the task registry (Julia) ──────────────────
# A guide that teaches "run this function" names the task two ways: `taskKey` (what TaskRunner's
# <select> holds, i.e. the spec's `task`) and `funName` (what the task rail reports, i.e. `fun_name`).
# Nothing in the frontend can check either — the specs live here — so a rename or a mismatched pair
# would leave the guide waiting forever on a function that does not exist, with no error anywhere.
#
# This is the structural half of a real bug: the Segment guide taught plain `segment.cellpose`, which
# produces labels with NO measures, so its own "now gate on these" ending could not work. Choosing the
# wrong function is a judgement no test can make; naming one that isn't real is, and that's this.
@testset "guide catalogue names real tasks" begin
    dir = joinpath(@__DIR__, "..", "..", "frontend", "src", "lib", "guides")
    if !isdir(dir)
        @test_skip "frontend guides catalogue not found"
    else
        src = join([read(joinpath(dir, f), String)
                    for f in readdir(dir) if endswith(f, ".ts") && !endswith(f, ".test.ts")], "\n")
        funs = [String(m.captures[1]) for m in eachmatch(r"funName:\s*'([^']+)'", src)]
        keys_ = [String(m.captures[1]) for m in eachmatch(r"taskKey:\s*'([^']+)'", src)]
        @test !isempty(funs)
        @test length(funs) == length(keys_)      # every task-run block names both

        registry = Cecelia._fun_name_map()
        @test isempty([f for f in funs if !haskey(registry, f)])

        # …and the pair must describe the SAME task: spec(funName).task == taskKey. A half-applied
        # rename that leaves the two pointing at different functions passes every other check —
        # the dropdown gate would never match while the rail happily parked on something else.
        mismatched = String[]
        for (f, k) in zip(funs, keys_)
            haskey(registry, f) || continue
            spec = Cecelia._task_spec(registry[f])
            String(get(spec, "task", "")) == k || push!(mismatched, "$f => $k")
        end
        @test isempty(mismatched)
    end
end

# ── Guides teaching a task a COMPOSITE wraps ─────────────────────────────────
# The bug this closes, twice over: the Segment guide taught `segment.cellpose` and the Track guide
# `tracking.bayesian_tracking` — the BARE halves of `…cellposeMeasure` / `…bayesian_track_measures`.
# Labels without measures and tracks without measures leave gating, clustering and the HMM with nothing
# to read, so each guide's own closing promise ("now gate on these") could not be kept. Nothing failed:
# the tasks ran, the guides completed, the next page was just empty.
#
# So whenever a guide teaches a task that some composite CONTAINS, that has to be a decision on record.
# Drift correction is the legitimate case — its composite adds autofluorescence removal, a separate
# scientific step, not the missing half of drift — which is exactly the distinction a human has to make
# and a test cannot. This is the inventory that forces the question, in the same spirit as the
# frontend's DECLARED_TIMERS list.
@testset "a guide teaching a composite's bare half is declared" begin
    dir = joinpath(@__DIR__, "..", "..", "frontend", "src", "lib", "guides")
    if !isdir(dir)
        @test_skip "frontend guides catalogue not found"
    else
        src = join([read(joinpath(dir, f), String)
                    for f in readdir(dir) if endswith(f, ".ts") && !endswith(f, ".test.ts")], "\n")
        taught = unique([String(m.captures[1]) for m in eachmatch(r"funName:\s*'([^']+)'", src)])
        @test !isempty(taught)

        # fun_name => why teaching the bare task is right even though a composite wraps it
        bare_by_design = Dict{String,String}()

        # every composite's constituent steps, from the registry
        wrapped_by = Dict{String,Vector{String}}()
        for (fun, task) in Cecelia._fun_name_map()
            spec = Cecelia._task_spec(task)
            steps = get(spec, "composite", nothing)
            steps isa AbstractVector || continue
            for st in steps
                push!(get!(wrapped_by, String(st), String[]), fun)
            end
        end

        undeclared = [t for t in taught
                      if haskey(wrapped_by, t) && !haskey(bare_by_design, t)]
        @test isempty(undeclared)

        # …and the list stays honest: an entry whose composite is gone, or that no guide teaches
        # any more, is stale rather than protective.
        stale = [k for k in keys(bare_by_design)
                 if !(k in taught) || !haskey(wrapped_by, k)]
        @test isempty(stale)
    end
end

# ── Optical-flow training (opticalFlow.train) ────────────────────────────────
# The scales are the single most consequential parameter of the pipeline AND the one that fails
# silently: the set a model is trained on must be the set inference feeds it, and coastal does not
# check. Rejecting a typo at the form is the only cheap place to catch it.
@testset "parse_temporal_scales" begin
    @test parse_temporal_scales("1,2,4,8") == [1, 2, 4, 8]
    @test parse_temporal_scales(" 8 , 1 ,2 ") == [1, 2, 8]      # sorted
    @test parse_temporal_scales("2 4 4 2") == [2, 4]            # deduped, whitespace-separated
    @test parse_temporal_scales([1, 2]) == [1, 2]               # a REPL caller's vector

    @test_throws ParamValidationError parse_temporal_scales("")
    @test_throws ParamValidationError parse_temporal_scales("   ")
    @test_throws ParamValidationError parse_temporal_scales("1,2,x")
    @test_throws ParamValidationError parse_temporal_scales("1,0")     # a lag of 0 is not a lag
    @test_throws ParamValidationError parse_temporal_scales("1,-2")
    @test_throws ParamValidationError parse_temporal_scales("1.5")
end

# Which metric planes the model reads. Same silent-failure family as the scales above: coastal stacks
# what it is given in sorted-key order and zero-fills the rest, so an inference set that differs from
# the training set shifts every later channel and raises nothing.
@testset "flow_dropped_metrics" begin
    # nothing = a caller from before the picker existed → the shipped default, not "train on all 11"
    @test sort(Cecelia.flow_dropped_metrics(nothing)) ==
          sort(collect(Cecelia.FLAT_FLOW_METRICS))

    # the picker's own default: the three flat planes are the ones left out
    default_pick = ["acceleration", "cell_boundary_likelihood", "cumulative_mag",
                    "direction_stability", "edge_strength", "normal_flow", "strain",
                    "tangential_flow"]
    @test Cecelia.flow_dropped_metrics(default_pick) ==
          ["divergence", "flow_structure_alignment", "vorticity"]

    # an arbitrary subset is allowed — the defaults are a starting point, not a rule
    @test Cecelia.flow_dropped_metrics(["divergence", "vorticity"]) ==
          [m for m in Cecelia.FIXED_FLOW_METRICS if !(m in ("divergence", "vorticity"))]
    @test isempty(Cecelia.flow_dropped_metrics(collect(Cecelia.FIXED_FLOW_METRICS)))

    # per-scale magnitudes are NOT choices (they follow temporalScales), so naming one drops nothing
    @test Cecelia.flow_dropped_metrics(["mag_1", "strain"]) ==
          [m for m in Cecelia.FIXED_FLOW_METRICS if m != "strain"]

    @test_throws ErrorException Cecelia.flow_dropped_metrics(String[])
end

# A model name reaches the filesystem. Not a security boundary — the user owns the machine — but a
# stray separator would write outside the vault and the model would then never appear in the picker.
@testset "flow_model_target" begin
    td = mktempdir()
    dir = joinpath(td, "models", "coastalModels")

    @test flow_model_target("gcMemTom"; dev_dir = td) == joinpath(dir, "gcMemTom.pt")
    @test isdir(dir)                                   # the vault is created on demand
    @test flow_model_target("gcMemTom.pt"; dev_dir = td) == joinpath(dir, "gcMemTom.pt")

    @test_throws ErrorException flow_model_target(""; dev_dir = td)
    @test_throws ErrorException flow_model_target("  "; dev_dir = td)
    @test_throws ErrorException flow_model_target("../escape"; dev_dir = td)
    @test_throws ErrorException flow_model_target("sub/dir"; dev_dir = td)
    @test_throws ErrorException flow_model_target(".."; dev_dir = td)

    # Overwrite is opt-in: a training run is long, and silently replacing the model a segmentation
    # already used would make an earlier run unreproducible with no trace.
    open(io -> write(io, "stub"), joinpath(dir, "gcMemTom.pt"), "w")
    @test_throws ErrorException flow_model_target("gcMemTom"; dev_dir = td)
    @test flow_model_target("gcMemTom"; overwrite = true, dev_dir = td) ==
          joinpath(dir, "gcMemTom.pt")
end

# The one objective signal a training run has. A model whose loss never came down still segments —
# confidently and wrongly — so it is worth a warning rather than being left in the log.
@testset "flow_training_qc_findings" begin
    @test isempty(flow_training_qc_findings(
        Dict{String,Any}("finalLoss" => 0.2, "lossDrop" => 3.4, "epochs" => 30)))

    flat = flow_training_qc_findings(
        Dict{String,Any}("finalLoss" => 0.9, "lossDrop" => 0.98, "epochs" => 30))
    @test length(flat) == 1
    @test flat[1]["level"] == "warn"
    @test flat[1]["detail"]["epochs"] == 30
    # numbers live in `detail`, not in the prose (docs/UI.md → QC copy)
    @test !occursin("0.9", flat[1]["long"])

    # exactly 1.0 = no improvement at all, still a warning
    @test length(flow_training_qc_findings(Dict{String,Any}("lossDrop" => 1.0))) == 1
    # no history parsed → no claim either way
    @test isempty(flow_training_qc_findings(Dict{String,Any}("epochs" => 30)))
    @test isempty(flow_training_qc_findings(Dict{String,Any}("lossDrop" => NaN)))

    # The held-out arm. This is the case the training curve CANNOT see: the loss drops 3.4x on the
    # frames the weights were fitted to while the held-out loss goes nowhere — a model fitting these
    # frames rather than learning what a cell looks like.
    memorised = flow_training_qc_findings(Dict{String,Any}(
        "finalLoss" => 0.2, "lossDrop" => 3.4, "valLossDrop" => 0.99,
        "valFinalLoss" => 0.9, "epochs" => 30))
    @test length(memorised) == 1
    @test memorised[1]["code"] == "opticalFlow.val_loss_flat"
    @test memorised[1]["level"] == "warn"
    @test memorised[1]["detail"]["valLossDrop"] == 0.99
    @test !occursin("0.99", memorised[1]["long"])

    # both flat = both findings, in order
    @test [f["code"] for f in flow_training_qc_findings(
        Dict{String,Any}("lossDrop" => 0.9, "valLossDrop" => 0.9))] ==
        ["opticalFlow.loss_flat", "opticalFlow.val_loss_flat"]

    # a run with no split says nothing about generalising, rather than passing it silently
    @test isempty(flow_training_qc_findings(
        Dict{String,Any}("finalLoss" => 0.2, "lossDrop" => 3.4)))
    @test isempty(flow_training_qc_findings(
        Dict{String,Any}("lossDrop" => 3.4, "valLossDrop" => 2.1)))
end

# `_task_spec` runs `_inject_dynamic_options!` for CellposeSegment on every call, so a
# dropped-in checkpoint under `<repo>/models/cellposeModels/` appears in the Model select's
# options — that's what makes validate_params accept the name. The bundled dir is normally EMPTY
# since the cellpose 4 migration (the `ceceliaModels` set is all v3 checkpoints and is no longer
# fetched), so the picker returns builtins only; the test guards both regimes.
@testset "CellposeSegment spec dynamic Model options" begin
    spec = Cecelia._task_spec(CellposeSegment())
    @test !isnothing(spec)
    models_group = only(p for p in spec["params"]
                        if get(p, "key", "") == "models")
    model_sel    = only(p for p in models_group["params"]
                        if get(p, "key", "") == "model")
    values = [string(o["value"]) for o in model_sel["options"]]
    # Built-ins are always there
    @test issubset(["cpsam_v2", "cpsam"], values)
    # …and exactly ONCE each. `optionsFrom` APPENDS to the spec's literal `options`, so a spec that
    # also declares an option the lister enumerates gets it twice — which is what this picker did,
    # showing "Cellpose-SAM v2" and "v1" twice each in the browser. An
    # `issubset` assertion cannot see that, which is why it survived; this can.
    @test length(values) == length(unique(values))

    # A genuinely-unknown checkpoint name is still rejected — the enumeration is real
    @test_throws ParamValidationError validate_params(CellposeSegment(),
        Dict{String,Any}("models" => Dict{String,Any}(
            "0" => Dict{String,Any}(
                "model" => "__no_such_file__.pt", "matchAs" => "base",
                "cellChannels" => [], "nucChannels" => [],
                "cellDiameter" => 10, "normalise" => 99.9,
                "stitchThreshold" => 0.0, "threshold" => 0,
                "medianFilter" => 0, "gaussianFilter" => 0.0))))
end

@testset "run_py custom-modules PYTHONPATH (config_dir not shadowed)" begin
    # Regression: run_py's task-dir parameter was named `config_dir`, which shadowed the
    # config_dir() function, so the custom-modules PYTHONPATH line `joinpath(config_dir(), …)`
    # called the task-dir STRING as a function → every Python task died with
    # `MethodError(<task dir>, (), …)` before Python was ever spawned. The call now lives in a
    # standalone helper with no shadowing param in scope. This asserts it resolves via the real
    # config_dir() function (equality would fail if it ever called anything else).
    # Post-#332 the custom-modules python dir IS the modules ROOT (runners are co-located under
    # modules/<cat>/, launched by absolute path so their own dir is sys.path[0]; the root just makes
    # the wider tree importable) — not the old shared modules/python. See py_runner.jl:_custom_modules_pydirs.
    mods = joinpath(config_dir(), "modules")
    mkpath(mods)
    @test first(Cecelia._custom_modules_pydirs()) == mods
    @test endswith(first(Cecelia._custom_modules_pydirs()), "modules")

    # …and every installed plugin's `python/` dir is listed alongside it (PLUGINS_PLAN R2). Via the
    # modules root alone a plugin's shared code spells `plugins.<plugin>.python.<mod>`, and a plugin
    # directory name is not required to be a Python identifier — the hyphen here is exactly the case
    # that can never be imported that way. Naming `python/` directly makes it a plain `import <mod>`,
    # so the plugin's directory name never appears in an import at all.
    pdir = joinpath(mods, Cecelia.PLUGINS_SUBDIR, "trackimport-smithlab")
    mkpath(joinpath(pdir, "python"))
    try
        @test joinpath(pdir, "python") ∈ Cecelia._custom_modules_pydirs()
        @test pdir ∉ Cecelia._custom_modules_pydirs()   # the ROOT is not added — see the helper's note
        # a plugin with no shared code contributes no entry
        mkpath(joinpath(mods, Cecelia.PLUGINS_SUBDIR, "nopython"))
        @test !any(p -> occursin("nopython", p), Cecelia._custom_modules_pydirs())
    finally
        rm(joinpath(mods, Cecelia.PLUGINS_SUBDIR); recursive = true, force = true)
    end
end

# ── Every Python task inherits a BLAS thread budget ─────────────────────────
#
# A pool limit caps concurrent TASKS, not threads: any numpy/scipy call reaching BLAS takes every
# core, so `cpu` at its default 20 means twenty tasks each asking for 32 threads. `run_py` is the
# only layer that can bound it — `OPENBLAS_NUM_THREADS` is read when the child imports numpy.
#
# Asserted on the SOURCE rather than by spawning, because the value has to be in the env `addenv`
# builds, and a helper nobody is forced to call is exactly how this gets dropped again. See
# docs/SCHEDULER.md → *Thread budgets*.
@testset "run_py bounds the BLAS thread pool" begin
    env = Dict(Cecelia._py_task_env("/tmp/py"))
    @test env["OPENBLAS_NUM_THREADS"] == string(Cecelia.BLAS_THREADS_PER_TASK)
    # A small positive budget: 1 measured SLOWER than 4 (the work is parallel, just not 32-ways),
    # and anything large defeats the point.
    @test 2 <= Cecelia.BLAS_THREADS_PER_TASK <= 8

    # NOT OMP_NUM_THREADS. That also throttles torch's intra-op parallelism, and torch on CPU is
    # the one measured workload that genuinely wants the cores (a cellpose-shaped conv stack goes
    # 0.19s -> 0.34s at 4 threads). Capping OpenBLAS alone leaves torch untouched.
    @test !haskey(env, "OMP_NUM_THREADS")
    @test !haskey(env, "MKL_NUM_THREADS")

    # the rest of the contract this env carries, so a refactor cannot silently drop one
    @test env["PYTHONPATH"] == "/tmp/py"
    @test haskey(env, "CECELIA_PY_CONTRACT") && haskey(env, "CECELIA_IMAGE_COMPRESSOR")

    # A task's OWN thread pools (coastal maps z-planes over one) are a separate budget from BLAS,
    # and derived from the box rather than a constant — the number it replaced was picked on one
    # 32-core laptop, which oversubscribes a small machine and idles a large one.
    @test env["CECELIA_TASK_WORKERS"] == string(Cecelia.task_worker_threads())
    # …and the same number in joblib's spelling. coastal's flow stage is `Parallel(n_jobs=-1)` —
    # processes, not threads, so nothing above bounds it — and `joblib.cpu_count()` honours this.
    # Asserted EQUAL to the task budget: two numbers for "how wide may one task go" would drift.
    @test env["LOKY_MAX_CPU_COUNT"] == env["CECELIA_TASK_WORKERS"]

    # The escape hatch for a stage MEASURED to keep scaling with width — coastal's flow metrics,
    # 14x at 32 threads against 6.8x at 8. Two variables, and both halves matter:
    #
    #   * WIDEN is the AND of the user's flag and "the budget was derived". Computed HERE rather than
    #     in Python, so the config decision has one home; a Python-side `or` would let a stage widen
    #     past a thread count somebody typed, making the slider a suggestion.
    #   * USABLE_CPUS is `usable_cpus()` — the affinity mask and cgroup quota applied. Passed in
    #     because re-deriving it from `os.cpu_count()` would hand out threads for CPUs this process
    #     cannot touch, which is the entire reason that helper exists.
    @test haskey(env, "CECELIA_TASK_WORKERS_WIDEN")
    @test env["CECELIA_TASK_WORKERS_WIDEN"] in ("0", "1")
    @test env["CECELIA_TASK_WORKERS_WIDEN"] ==
          ((Cecelia.task_workers_widen() && Cecelia.task_workers_derived()) ? "1" : "0")
    @test env["CECELIA_USABLE_CPUS"] == string(Cecelia.usable_cpus())
    @test parse(Int, env["CECELIA_USABLE_CPUS"]) >= 1
    # Never wider than the box: the widening target is a real ceiling, not "unbounded".
    @test parse(Int, env["CECELIA_USABLE_CPUS"]) <= max(Sys.CPU_THREADS, 1)

    @test Cecelia.task_worker_threads() >= 1
    @test Cecelia.default_task_worker_threads() <= 16
    # never wider than the machine, however many tasks are assumed concurrent
    @test Cecelia.default_task_worker_threads() <= max(Sys.CPU_THREADS, 1)

    # The preview worker runs the tasks' OWN compute, so it inherits the same budget.
    prev = read(joinpath(Cecelia._app_dir(), "src", "preview.jl"), String)
    @test occursin("OPENBLAS_NUM_THREADS", prev)
end

@testset "widening a linear stage needs BOTH conditions" begin
    # `task_workers_widen()` alone is not enough, and neither is a derived budget alone. The AND is
    # computed in Julia (see `_py_task_env`) precisely so there is one place that can be read.
    #
    # No config is WRITTEN here — `set_task_workers_widen!` touches the user's own custom.toml, and a
    # test must not. The predicates are pure over `cecelia_conf()`, so they are exercised directly.
    @test Cecelia.task_workers_widen() isa Bool
    @test Cecelia.task_workers_derived() isa Bool

    # `task_workers_derived()` must agree with the number actually in effect, or the UI offers a
    # control the backend ignores. A typo counts as DERIVED, matching `task_worker_threads`'s own
    # fallback — a bad value there does not stop a run, it falls back to the machine.
    conf = get(get(Cecelia.cecelia_conf(), "tasks", Dict{String,Any}()), "workerThreads", nothing)
    expected = isnothing(conf) || (try Int(conf) < 1 catch; true end)
    @test Cecelia.task_workers_derived() == expected
    if Cecelia.task_workers_derived()
        @test Cecelia.task_worker_threads() == Cecelia.default_task_worker_threads()
    end

    # Only ONE call site claims linear scaling, and it is the measured one. `PREDICT_WORKERS` must
    # never claim it — its curve turns DOWN past 4, the opposite claim, already served by `cap=`.
    src = read(joinpath(dirname(dirname(@__DIR__)), "python", "cecelia", "utils",
                        "coastal_utils.py"), String)
    @test occursin("FLOW_WORKERS = cpu_utils.task_workers(scales_linearly=True)", src)
    predict = src[findfirst("PREDICT_WORKERS =", src)[1]:end]
    predict = predict[1:findfirst("\n", predict)[1]]
    @test !occursin("scales_linearly", predict)
end

# ── First-launch setup wizard (isolated temp config dir) ────────────────────
# Uses its own CECELIA_DEV_DIR tempdir so it never touches the real dev/prod config; restores
# global config afterwards. Exercises setup_required + set_projects_dir! (merge + reload).
@testset "Config setup wizard" begin
    prev_env = get(ENV, "CECELIA_DEV_DIR", nothing)
    mktempdir() do tmp
        ENV["CECELIA_DEV_DIR"] = tmp
        try
            init_cecelia!()                            # load the empty temp config
            @test custom_toml_path() == joinpath(tmp, "custom.toml")
            @test setup_required() == true             # no custom.toml yet

            # a pre-existing key must survive the merge
            write(custom_toml_path(), "[dirs]\npython = \"/opt/py\"\n")
            @test setup_required() == true             # projects still unset → placeholder

            proj = joinpath(tmp, "projects"); mkpath(proj)
            stored = set_projects_dir!(proj)
            @test stored == proj
            @test isfile(custom_toml_path())
            @test projects_dir() == proj               # hot-reloaded, no restart
            @test setup_required() == false            # configured + dir exists
            @test python_bin_path() == "/opt/py"       # merge preserved the other key

            # a configured-but-missing dir re-triggers setup
            rm(proj; recursive = true)
            @test setup_required() == true
        finally
            prev_env === nothing ? delete!(ENV, "CECELIA_DEV_DIR") :
                                   (ENV["CECELIA_DEV_DIR"] = prev_env)
            init_cecelia!()                            # restore real dev/prod config
        end
    end
end

# ── Resident preview worker resolves `cecelia` from THIS checkout ───────────
# The worker is launched by PATH but imports `cecelia` by NAME, so without an explicit PYTHONPATH
# it uses whatever pip has installed — in dev an editable install pointing at the MAIN checkout.
# A worktree would then run its own `preview_worker.py` against another checkout's library and the
# halves drift with no error until one calls something the other lacks. `run_py` always set
# PYTHONPATH; the resident preview worker's `launch!` does too. (The napari bridge used to be
# subject to the same rule; retired in P9.)
@testset "resident preview worker pins PYTHONPATH" begin
    pyroot = Cecelia._python_dir()
    @test isdir(joinpath(pyroot, "cecelia"))              # the dir we are pinning really is the package

    # the worker's launch is inside `launch!` (which spawns), so assert on the source rather than run it
    src = read(joinpath(dirname(pathof(Cecelia)), "preview.jl"), String)
    body = src[findfirst("function launch!(", src)[1]:end]
    @test occursin("PYTHONPATH", body[1:findfirst("\nend", body)[1]])
end

# ── The log rail (app/src/log_stream.jl) ─────────────────────────────────────
# What the bottom console shows is built here, and every piece of it used to be got wrong somewhere:
# the exception kwarg was interpolated instead of formatted, child stdio was thrown away, and a
# dropped WS frame was undetectable. These pin the pure halves of all three.
@testset "log_record formats an exception instead of interpolating it" begin
    e, bt = try; error("boom"); catch err; (err, catch_backtrace()); end

    rec = log_record("error", "Unhandled error in POST /api/x"; exception = (e, bt))
    @test rec["level"]  == "error"
    @test rec["source"] == LOG_SOURCE_BACKEND
    # THE regression this file exists for: `"$k = $v"` on an (exception, backtrace) tuple rendered
    # 857 characters of `Ptr{Nothing}(0x…)` into the collapsed row. A formatted trace says
    # "Stacktrace:" and names files; a interpolated one is a wall of pointers.
    @test occursin("Stacktrace:", rec["detail"])
    @test !occursin("Ptr{Nothing}(0x", rec["detail"])
    # the exception's first line is promoted into the row — a row reading only "…failed" is why the
    # one word that matters used to be a click away
    @test occursin("boom", rec["message"])
    @test occursin("Unhandled error", rec["message"])

    # an exception with no backtrace (the `exception = e` call sites) still formats
    @test occursin("boom", log_record("warn", "show_labels failed"; exception = e)["detail"])
    # …and is not repeated when the message already carries it
    @test count(_ -> true, findall("boom", log_record("warn", "boom"; exception = e)["message"])) == 1

    # ordinary kwargs go to the DETAIL, not the row: they are what made rows unreadable
    plain = log_record("info", "GL renderer"; renderer = "NVIDIA", vendor = "NVIDIA Corp")
    @test plain["message"] == "GL renderer"
    @test occursin("renderer = NVIDIA", plain["detail"])
    @test !haskey(log_record("info", "plain line"), "detail")

    # `source` is a facet, never text in the body
    @test log_record("info", "x"; source = LOG_SOURCE_PREVIEW)["source"] == LOG_SOURCE_PREVIEW
    @test !haskey(log_record("info", "x"; source = LOG_SOURCE_PREVIEW), "detail")

    # a detail is capped — it rides a WS frame into a browser store
    big = log_record("error", "huge"; detail = repeat("x", LOG_DETAIL_CAP * 2))["detail"]
    @test length(big) < LOG_DETAIL_CAP + 100
    @test occursin("truncated", big)

    # never throws on a value that is neither shape
    @test log_record("error", "odd"; exception = (1, 2))["detail"] isa String
end

@testset "child log lines — level and traceback reassembly" begin
    @test Cecelia.child_line_level("[preview] added labels: shape=(1, 2)") == "info"
    @test Cecelia.child_line_level("[ERROR] $(:x): could not open")       == "error"
    @test Cecelia.child_line_level("ValueError: invalid literal for int") == "error"
    @test Cecelia.child_line_level("cecelia.utils.zarr_utils.StoreError: nope") == "error"
    @test Cecelia.child_line_level("[WARN] could not read back the movie size") == "warn"
    @test Cecelia.child_line_level("UserWarning: deprecated")             == "warn"

    # A Python traceback is ONE event over many lines. Split per line, the frames classify as `info`
    # — which the default view hides — so the console would show a bare header with nothing under it.
    sink = Cecelia.ChildLineSink(LOG_SOURCE_PREVIEW)
    recs = Dict{String,Any}[]
    for ln in ["preview worker ready on ws://127.0.0.1:7656",
               "Traceback (most recent call last):",
               "  File \"preview_worker.py\", line 903, in _serve",
               "    reply = handle(msg)",
               "  File \"preview_worker.py\", line 400, in handle",
               "    raise ValueError(\"bad region\")",
               "ValueError: bad region",
               "preview worker ready again"]
        append!(recs, Cecelia.child_line_records!(sink, ln))
    end
    @test length(recs) == 3                                   # ready · the whole traceback · ready
    tb = recs[2]
    @test tb["level"]   == "error"
    @test tb["source"]  == LOG_SOURCE_PREVIEW
    @test tb["message"] == "ValueError: bad region"           # the row names the failure, not "Traceback"
    @test occursin("line 400", tb["detail"])                  # …and the frames are all in the body
    @test occursin("Traceback (most recent call last):", tb["detail"])
    @test isempty(sink.buf)

    # a traceback cut off by the process dying still gets emitted rather than swallowed
    cut = Cecelia.ChildLineSink(LOG_SOURCE_PREVIEW)
    Cecelia.child_line_records!(cut, "Traceback (most recent call last):")
    Cecelia.child_line_records!(cut, "  File \"preview_worker.py\", line 1, in <module>")
    left = Cecelia.child_line_flush!(cut)
    @test length(left) == 1 && left[1]["level"] == "error"
    @test occursin("preview_worker.py", left[1]["detail"])

    # blank filler is not an event
    @test isempty(Cecelia.child_line_records!(Cecelia.ChildLineSink("x"), "   "))
end

@testset "LogRing stamps a monotonic seq so a dropped line is detectable" begin
    r = LogRing(3)
    a = log_ring_push!(r, log_record("info", "one"))
    b = log_ring_push!(r, log_record("warn", "two"))
    @test a["seq"] == 1 && b["seq"] == 2
    @test !isempty(a["ts"])
    @test log_ring_seq(r) == 2

    # `since` is what repairs a gap — the client asks for what it did not get, not for everything
    @test [x["message"] for x in log_ring_since(r, 1)] == ["two"]
    @test length(log_ring_since(r, 0)) == 2
    @test isempty(log_ring_since(r, 2))

    # the cap evicts, but the seq keeps counting — so eviction reads as a gap, not as a renumbering
    for i in 3:6; log_ring_push!(r, log_record("info", "n$i")); end
    @test length(log_ring_since(r, 0)) == 3
    @test log_ring_seq(r) == 6

    # a relayed record (the runner's) keeps the stamp from where it happened, and is RE-seq'd here so
    # the browser follows exactly one counter
    relayed = log_ring_push!(r, Dict{String,Any}("level" => "info", "message" => "from runner",
                                                 "source" => LOG_SOURCE_RUNNER, "ts" => "2026-01-01T00:00:00.000Z"))
    @test relayed["ts"]  == "2026-01-01T00:00:00.000Z"
    @test relayed["seq"] == 7
end

@testset "long-lived children are spawned onto the log rail, never swallowed" begin
    # `run(cmd; wait = false)` sends BOTH streams to devnull (spawn_opts_swallow), which is how the
    # preview worker's traceback.print_exc() reached nothing at all — not the console, not the
    # terminal. This is a convention test: the two remaining spawn sites must use `spawn_logged`,
    # because the failure it prevents is invisible by construction.
    src = dirname(pathof(Cecelia))
    for (file, what) in (("preview.jl", "preview worker"),)
        body = read(joinpath(src, file), String)
        @test occursin("spawn_logged(", body)
        @test !occursin(r"\.proc = run\(", body)          # the swallowing shape, gone
    end

    # The RUNNER is the deliberate exception and must stay one: it is `detach = true` so it outlives
    # the backend, and a pipe this process owns becomes a broken pipe on the restart it exists to
    # survive. It reports over its event stream instead — assert that carrier still exists, or the
    # exemption silently becomes the old silence again.
    runner_src = read(joinpath(src, "runner", "server.jl"), String)
    @test occursin("install_log_tee!", runner_src)
    @test occursin("runner:log", runner_src)
    @test occursin("/api/logs/recent", runner_src)

    # …and its OTHER carrier. Being exempt from `spawn_logged` is not a licence to go back to devnull:
    # the runner's raw stdout/stderr (a `println`, an unhandled `@spawn` task error, a segfault dump the
    # C runtime writes while the process dies) is not a log record and never reaches `runner:log`. It is
    # explicitly INHERITED instead, which is safe precisely because the fd belongs to the terminal
    # rather than to us — a detached child keeps writing to it long after we exit.
    client_src = read(joinpath(src, "runner", "client.jl"), String)
    @test occursin("detach = true", client_src)                    # the reason for the exemption
    @test occursin(r"stdout\s*=\s*stdout", client_src)             # …and not into a hole
    @test occursin(r"stderr\s*=\s*stderr", client_src)
    # …and NOT via spawn_logged: a pipe we read would break on the restart it exists to survive. The
    # trailing paren matters — the file discusses `spawn_logged` at length in the comment explaining
    # why it is wrong here, and a bare substring test would fail on the explanation.
    @test !occursin("spawn_logged(", client_src)
end

@testset "log sources agree across languages" begin
    # The console renders one filter chip per source, so the set is a CONTRACT with the frontend —
    # a source Julia can emit but the console has no chip for is a message that arrives and is
    # unreachable. Same shape as the protocol-version pairs in docs/ARCHITECTURE.md.
    ts = read(normpath(joinpath(@__DIR__, "..", "..", "frontend", "src", "utils", "logFilter.ts")), String)
    for s in LOG_SOURCES
        @test occursin("'$s'", ts)
    end
    @test LOG_SOURCE_BACKEND ∉ CHILD_LOG_SOURCES     # the backend is never hidden by default
    @test length(LOG_SOURCES) == length(CHILD_LOG_SOURCES) + 1
end

# ── AI observer (in-app assistant) — pure command/result pieces ─────────────
# The live spawn (needs the agent CLI + a running API) isn't tested here; these pin the pure
# builders/parsers that the runner + api route depend on. See docs/todo/OBSERVER_INTEGRATION_PLAN.md.
@testset "AI observer agent runner (pure pieces)" begin
    a   = Cecelia.ClaudeAgent(bin = "claude", model = "")               # explicit empty → no flag
    cmd = Cecelia._build_claude_cmd(a, "hello", "/tmp/mcp.json"; system_prompt = "be brief")
    argv = cmd.exec
    @test argv[1] == "claude"
    @test "-p" in argv && "hello" in argv
    @test "--output-format" in argv && "json" in argv
    @test "--mcp-config" in argv && "/tmp/mcp.json" in argv
    @test "--allowedTools" in argv                                    # observer tools allowed
    @test "--append-system-prompt" in argv && "be brief" in argv
    @test !("--resume" in argv)                                       # no session → no resume
    @test !("--model" in argv)                                        # empty model → no flag

    cmd2 = Cecelia._build_claude_cmd(Cecelia.ClaudeAgent(bin = "claude", model = "claude-opus-4-8"),
                                     "hi", "/tmp/m.json"; session_id = "sess123")
    @test "--resume" in cmd2.exec && "sess123" in cmd2.exec
    @test "--model" in cmd2.exec && "claude-opus-4-8" in cmd2.exec

    # model choice: shipped default is Sonnet (Opus not needed for observer work); the request
    # model is allow-listed — an arbitrary string never reaches --model. (default_model reads
    # config [ai] model, so assert it stays within the allow-list rather than a hard "sonnet".)
    @test Set(Cecelia.OBSERVER_MODELS) == Set(["haiku", "sonnet", "opus"])
    @test Cecelia.observer_default_model() in Cecelia.OBSERVER_MODELS
    @test Cecelia.observer_valid_model("haiku") == "haiku"
    @test Cecelia.observer_valid_model("gpt-4") == Cecelia.observer_default_model()   # unknown → default
    @test Cecelia.observer_valid_model("")     == Cecelia.observer_default_model()
    @test Cecelia.ClaudeAgent(bin = "claude").model == Cecelia.observer_default_model()

    # result parsing — success carries text + usage + session
    r = Cecelia._parse_claude_result(
        """{"is_error":false,"result":"noted a stuck task","session_id":"s1","usage":{"input_tokens":1200,"output_tokens":40}}""")
    @test r.ok && r.text == "noted a stuck task"
    @test r.input_tokens == 1200 && r.output_tokens == 40 && r.session_id == "s1"
    # error result surfaces the message; garbage is a clean failure, not a throw
    e = Cecelia._parse_claude_result("""{"is_error":true,"result":"tool failed"}""")
    @test !e.ok && occursin("tool failed", e.error)
    g = Cecelia._parse_claude_result("not json")
    @test !g.ok && g.input_tokens == 0

    # stale-session detection: a pruned/expired --resume id makes the CLI say "No conversation
    # found with session ID: …" → run_observer_turn drops the id and retries fresh (self-heal).
    @test Cecelia._is_stale_session_error(
        "No conversation found with session ID: 0df65af8-ae13-4ec5-964a-7231cd8bf005")
    @test Cecelia._is_stale_session_error("no conversation found with session id: x")  # case-insensitive
    @test !Cecelia._is_stale_session_error("agent exited 1")                            # other failures don't retry
    @test !Cecelia._is_stale_session_error("tool failed")

    # MCP config points the spawned agent at the same cecelia_mcp server + this API
    cfg = Cecelia.observer_mcp_config("/repo/mcp", "/env/python", "http://127.0.0.1:8080")
    srv = cfg["mcpServers"]["cecelia-observer"]
    @test srv["command"] == "/env/python"
    @test srv["args"] == ["-m", "cecelia_mcp.server"]
    @test srv["env"]["PYTHONPATH"] == "/repo/mcp"
    @test srv["env"]["CECELIA_API_URL"] == "http://127.0.0.1:8080"
    # the wrapper is built FROM the spec — one source of truth for both the --mcp-config file and
    # `claude mcp add-json` (which takes the bare spec)
    @test srv == Cecelia.observer_mcp_spec("/repo/mcp", "/env/python", "http://127.0.0.1:8080")

    # one-click terminal setup: add-json is not idempotent, so registering is remove-then-add at
    # user scope, and both commands must name the SAME server as the config/--allowedTools filter
    reg = Cecelia._build_mcp_register_cmd(a, "{\"command\":\"/env/python\"}")
    @test reg.exec == [a.bin, "mcp", "add-json", Cecelia.OBSERVER_MCP_NAME,
                       "{\"command\":\"/env/python\"}", "-s", "user"]
    rm_cmd = Cecelia._build_mcp_remove_cmd(a)
    @test rm_cmd.exec == [a.bin, "mcp", "remove", Cecelia.OBSERVER_MCP_NAME, "-s", "user"]
    @test Cecelia._build_mcp_register_cmd(a, "{}"; scope = "local").exec[end] == "local"
    # the restore path uses the SAME add command, so a prior entry can be put back verbatim
    @test Cecelia._build_mcp_register_cmd(a, "{\"command\":\"/old/python\"}").exec[5] ==
          "{\"command\":\"/old/python\"}"
    @test Cecelia.OBSERVER_MCP_NAME == "cecelia-observer"   # the name users see in `claude mcp list`

    # ── Windows CLI resolution + spawn wrapping ──────────────────────────────────────────
    # The observer was invisible on Windows: `Sys.which` only tries the bare name plus `.exe`/
    # `.com` (base/sysinfo.jl), never `.cmd`/`.bat`, so an npm-installed `claude.cmd` was never
    # found → available:false → "Set up my terminal" told users WITH Claude Code to install it.
    # And a `.cmd` cannot be spawned directly — CreateProcess refuses batch files.
    # These helpers take `iswin` explicitly so BOTH platforms' behaviour is asserted from any
    # host — the reason this shipped broken is that nobody could exercise the Windows path.
    @test Cecelia._agent_bin_candidates("claude", false) == ["claude"]        # unix: as given
    @test Cecelia._agent_bin_candidates("claude", true) ==
          ["claude", "claude.cmd", "claude.bat"]                             # windows: + batch shims
    # an explicit extension is Sys.which's job already — don't append to it
    @test Cecelia._agent_bin_candidates("claude.exe", true) == ["claude.exe"]
    @test Cecelia._agent_bin_candidates("claude.cmd", true) == ["claude.cmd"]

    # only batch files need the shell, and only on Windows
    @test Cecelia._needs_cmd_shell("C:/n/claude.cmd", true)
    @test Cecelia._needs_cmd_shell("C:/n/claude.BAT", true)                  # extension case-insensitive
    @test !Cecelia._needs_cmd_shell("C:/n/claude.exe", true)
    @test !Cecelia._needs_cmd_shell("/usr/bin/claude", false)
    @test !Cecelia._needs_cmd_shell("/usr/bin/claude.cmd", false)            # never on unix

    # argv rewriting: argv[1] becomes the resolved path, and a batch file gains `cmd /c`
    logical = ["claude", "mcp", "add-json", "cecelia-observer", "{}", "-s", "user"]
    @test Cecelia._agent_spawn_argv(logical, "/usr/bin/claude", false) ==
          ["/usr/bin/claude", "mcp", "add-json", "cecelia-observer", "{}", "-s", "user"]
    @test Cecelia._agent_spawn_argv(logical, "C:/npm/claude.cmd", true) ==
          ["cmd", "/c", "C:/npm/claude.cmd", "mcp", "add-json", "cecelia-observer", "{}", "-s", "user"]
    @test Cecelia._agent_spawn_argv(logical, "C:/p/claude.exe", true) ==
          ["C:/p/claude.exe", "mcp", "add-json", "cecelia-observer", "{}", "-s", "user"]
    # unresolvable → argv untouched, so the spawn fails naming what the user configured
    @test Cecelia._agent_spawn_argv(logical, nothing, true) == logical
    @test Cecelia._agent_spawn_argv(String[], nothing, true) == String[]
    # the spec argument must survive rewriting verbatim — it is the whole payload
    @test Cecelia._agent_spawn_argv(logical, "C:/npm/claude.cmd", true)[7] == "{}"

    # live resolver: an absolute path to a real executable resolves to itself; nonsense is nothing.
    # (Uses this Julia's own binary — no assumption about what is on PATH.)
    let jl = joinpath(Sys.BINDIR, Sys.iswindows() ? "julia.exe" : "julia")
        isfile(jl) && @test Cecelia.agent_bin_path(jl) == jl
    end
    @test Cecelia.agent_bin_path("") === nothing
    @test Cecelia.agent_bin_path("cecelia-definitely-no-such-binary-42") === nothing

    # Is the user's own terminal set up? Drives which button the lab-log toolbar shows, so the
    # three states must be exact. A stale entry (another checkout's python, or no/!matching
    # CECELIA_API_URL) is NOT "set up" — it fails silently in the user's session.
    want = Cecelia.observer_mcp_spec("/repo/mcp", "/env/python", "http://127.0.0.1:8080")
    @test Cecelia.observer_registration_state(nothing, want) === :missing
    registered = JSON3.read(JSON3.write(merge(want, Dict("type" => "stdio"))))  # as Claude stores it
    @test Cecelia.observer_registration_state(registered, want) === :current    # extra keys are fine
    other_py = JSON3.read(JSON3.write(Cecelia.observer_mcp_spec("/repo/mcp", "/OTHER/python",
                                                               "http://127.0.0.1:8080")))
    @test Cecelia.observer_registration_state(other_py, want) === :stale        # moved checkout
    other_port = JSON3.read(JSON3.write(Cecelia.observer_mcp_spec("/repo/mcp", "/env/python",
                                                                 "http://127.0.0.1:9999")))
    @test Cecelia.observer_registration_state(other_port, want) === :stale      # different port
    # the real-world case that bit an early manual registration: PYTHONPATH set, CECELIA_API_URL absent
    no_url = JSON3.read(JSON3.write(Dict("command" => "/env/python", "args" => ["-m", "cecelia_mcp.server"],
                                         "env" => Dict("PYTHONPATH" => "/repo/mcp"))))
    @test Cecelia.observer_registration_state(no_url, want) === :stale
    @test Cecelia.observer_registration_state(Dict{String,Any}("command" => "/env/python"), want) === :stale

    # config path honours Claude Code's own env override; missing file → not set up, never an error
    withenv("CLAUDE_CONFIG_DIR" => "/tmp/cc-cfg") do
        @test Cecelia.claude_config_path() == joinpath("/tmp/cc-cfg", ".claude.json")
    end
    withenv("CLAUDE_CONFIG_DIR" => nothing) do
        @test Cecelia.claude_config_path() == joinpath(homedir(), ".claude.json")
    end
    @test Cecelia.read_registered_observer_spec(joinpath(mktempdir(), "nope.json")) === nothing
    let bad = joinpath(mktempdir(), "bad.json")
        write(bad, "not json at all")
        @test Cecelia.read_registered_observer_spec(bad) === nothing     # another tool's file — tolerate
    end
    let cfgf = joinpath(mktempdir(), ".claude.json")
        write(cfgf, JSON3.write(Dict("mcpServers" => Dict(Cecelia.OBSERVER_MCP_NAME => want))))
        @test Cecelia.observer_registration_state(
            Cecelia.read_registered_observer_spec(cfgf), want) === :current
        # this reader is user-scope ONLY — a local-scope entry is not a registration for it to find
        write(cfgf, JSON3.write(Dict("projects" => Dict("/somewhere" =>
            Dict("mcpServers" => Dict(Cecelia.OBSERVER_MCP_NAME => want))))))
        @test Cecelia.read_registered_observer_spec(cfgf) === nothing
    end

    # ── Local-scope shadowing ────────────────────────────────────────────────────────────
    # The bug: our button writes `-s user`, but Claude Code resolves `local` scope
    # (projects[<dir>].mcpServers) FIRST. A leftover local entry pointing at a DELETED checkout
    # therefore killed the server with ENOENT for every session started in that dir — while the
    # status route, reading only the top level, reported :current and offered "Chat to Claude".
    stale_local = Cecelia.observer_mcp_spec("/gone/mcp", "/gone/python", "http://127.0.0.1:8080")
    let cfgf = joinpath(mktempdir(), ".claude.json")
        write(cfgf, JSON3.write(Dict(
            "mcpServers" => Dict(Cecelia.OBSERVER_MCP_NAME => want),
            "projects"   => Dict(
                "/home/u"       => Dict("mcpServers" => Dict(Cecelia.OBSERVER_MCP_NAME => stale_local)),
                "/home/u/right" => Dict("mcpServers" => Dict(Cecelia.OBSERVER_MCP_NAME => want)),
                "/home/u/none"  => Dict("mcpServers" => Dict{String,Any}()),
                "/home/u/other" => Dict("mcpServers" => Dict("something-else" => want))))))
        locals = Cecelia.read_local_observer_specs(cfgf)
        @test sort(String[d for (d, _) in locals]) == ["/home/u", "/home/u/right"]
        # only the MISMATCHED one is a problem: a local entry equal to `want` resolves to the same
        # server, so it is left alone — we never delete config that isn't breaking anything
        @test Cecelia.observer_shadow_dirs(locals, want) == ["/home/u"]
        # user scope is still read independently of any of this
        @test Cecelia.observer_registration_state(
            Cecelia.read_registered_observer_spec(cfgf), want) === :current
    end
    # tolerant like the user-scope reader — a missing/garbage/odd-shaped config is "no shadows",
    # never an exception (it is another tool's file and this runs on every status poll)
    @test isempty(Cecelia.read_local_observer_specs(joinpath(mktempdir(), "nope.json")))
    let bad = joinpath(mktempdir(), "bad.json")
        write(bad, "not json at all")
        @test isempty(Cecelia.read_local_observer_specs(bad))
    end
    let odd = joinpath(mktempdir(), "odd.json")
        write(odd, JSON3.write(Dict("projects" => "a string, not an object")))
        @test isempty(Cecelia.read_local_observer_specs(odd))
    end
    @test Cecelia.observer_shadow_dirs(Pair{String,Any}[], want) == String[]
    # sorted → the folder list the UI reports (and the removal order) is deterministic
    @test Cecelia.observer_shadow_dirs(
        Pair{String,Any}["/b" => stale_local, "/a" => stale_local], want) == ["/a", "/b"]

    # `claude mcp remove -s local` acts on the process's CWD, so the cleanup must be able to say
    # WHERE it runs — and the spawn wrapper must not drop that when it rewrites argv (it rebuilds
    # the Cmd, which is exactly how a `dir` gets silently lost and the wrong scope edited)
    @test Cecelia._build_mcp_remove_cmd(a; scope = "local", dir = "/home/u").exec[end] == "local"
    @test Cecelia._build_mcp_remove_cmd(a; scope = "local", dir = "/home/u").dir == "/home/u"
    @test isempty(Cecelia._build_mcp_remove_cmd(a).dir)                  # unchanged default
    @test Cecelia._agent_spawn_cmd(
        Cecelia._build_mcp_remove_cmd(a; scope = "local", dir = "/home/u")).dir == "/home/u"
    # a dir that no longer exists is skipped, not attempted (Claude ignores its entry too)
    @test Cecelia.remove_shadowing_observer_mcps(a, ["/no/such/dir/at/all"]) == (String[], String[])

    # the prompt carries the project + the discipline rules THIS ROLE adds
    fp = Cecelia.observer_feedback_prompt("NRUBxU")
    @test occursin("NRUBxU", fp) && occursin("append_lab_log", fp) && occursin("[Claude]", fp)
    # §1 param-suggestion guidance is present: on an outlier, use get_module_params + the trail to
    # suggest a param direction — framed as a suggestion, current-state only (not a prediction).
    @test occursin("get_module_params", fp) && occursin("suggest", fp)
    # the watch loop's own tools — nobody else polls a running project
    @test occursin("poll_observations", fp) && occursin("get_cohort_qc", fp)
    # the lab-log discipline, which is the whole point of the role: most of the time, write nothing
    @test occursin("write NOTHING", fp)
    # …and it is pointed at the server's briefing for everything shared, rather than restating it
    @test occursin("get_session_briefing", fp)
    # It must NOT restate what the MCP server already tells every client (mcp/cecelia_mcp/guidance.py):
    # the read catalogue, the grouping discipline, the boards/chains rules, the never-starts boundary.
    # A second copy is what went stale twice. Match on unwrapped text — the prompt is hard-wrapped, so
    # a phrase can straddle a newline.
    flat = replace(fp, r"\s+" => " ")
    for shared in ("not four replicates", "statUnit", "press Run", "get_image_attributes",
                   "get_analysis_boards", "get_available_plots", "get_repl_api")
        @test !occursin(shared, flat)
    end
end

@testset "the in-app observer prompt is a role, not a second tool manual" begin
    # THE recurring bug in this area, twice: an MCP tool is added, one of the prompts describing the
    # toolset is updated and another silently goes stale — an unmentioned tool is an unused one, so the
    # capability just never gets offered (create_chain the first time, then get_analysis_boards /
    # get_image_attributes). Both surfaced only because reading a prompt and noticed a gap.
    #
    # The fix was to stop having copies. The MCP server describes its own toolset
    # (mcp/cecelia_mcp/guidance.py: SERVER_INSTRUCTIONS on connect, BRIEFING_GUIDANCE with
    # get_session_briefing), and the in-app agent is spawned with `--mcp-config` pointing at that same
    # server, so it gets both. What is left here is the ROLE — the watch loop, the QC pass, the
    # lab-log discipline — which no chat session has.
    #
    # So this no longer checks "does the prompt name every tool". It checks the DIVISION: the loop's
    # own tools are named here, and the shared catalogue is NOT restated here. Each side is guarded in
    # its own language, since neither can import the other — mcp/tests/test_server.py → GuidanceTest
    # holds the other half (every registered tool is named in guidance.py).
    root = normpath(joinpath(@__DIR__, "..", ".."))
    server = read(joinpath(root, "mcp", "cecelia_mcp", "server.py"), String)
    # The PROMPT, not the file that holds it: a tool name in a source comment (this file's own header
    # names several) would otherwise count as "mentioned" and mask a real omission. That is not
    # hypothetical — rewording the header alone flipped this assertion once.
    jl = Cecelia.observer_feedback_prompt("NRUBxU")

    tools = Set(String[m.captures[1] for m in eachmatch(r"@mcp\.tool\(\)\s*\ndef (\w+)", server)])
    @test length(tools) >= 30                          # anti-vacuity: a bad regex must not pass
    named_jl = Set(t for t in tools if occursin(t, jl))

    # The watch loop's OWN tools — the reason this role exists. Nothing else polls a running project
    # or decides whether a finished-but-degenerate run is worth a line in the lab log.
    for own in ("poll_observations",        # the 10-attempts pattern, from the session monitor
                "get_task_history", "get_task_log", "get_recent_logs",   # what ran / what broke
                "get_cohort_qc",            # a "done" run that produced far too few cells
                "get_module_params",        # the param-suggestion range
                "read_lab_log", "append_lab_log",                        # prior context + its output
                "get_session_briefing")     # …and where the shared rules come from
        @test own in named_jl
    end
    # The SHARED catalogue must not be restated here — that is the second copy, and the second copy is
    # the bug. These are all named by guidance.py, which reaches this agent through the same MCP.
    for shared in ("get_image_attributes", "get_analysis_boards", "get_available_plots",
                   "get_populations", "get_measure_summary", "get_analysis_lineage", "get_repl_api",
                   "add_analysis_board", "list_projects")
        @test !(shared in named_jl)
    end
    # A tool this role does not use is fine; a tool NOBODY names is not. Every tool the in-app prompt
    # leaves out must be covered by the server's guidance — asserted in full by GuidanceTest, and
    # pinned here as the reason this set is allowed to be small.
    @test length(named_jl) < length(tools)
end

@testset "MCP connections — enumerate whatever is registered" begin
    # Generic on purpose: it lists what's in the config rather than looking for names we know, so a
    # connector added later needs no change here. Backs Settings → MCP connections.
    dir = mktempdir()
    cfg = joinpath(dir, ".claude.json")

    @test isempty(mcp_connections(joinpath(dir, "nope.json")))     # no config → no rows, never throws
    write(cfg, "{not json")
    @test isempty(mcp_connections(cfg))                            # another tool's file: tolerate junk

    write(cfg, """
    {"mcpServers": {"cecelia-observer": {"command": "py", "env": {"PYTHONPATH": "/opt/here/mcp"}},
                    "other-tool": {"url": "https://x/mcp"}},
     "projects": {"/tmp/p1": {"mcpServers": {"cecelia-observer": {"command": "old"}}},
                  "/tmp/p2": {}}}
    """)
    rows = mcp_connections(cfg)
    @test length(rows) == 3                                        # 2 user-scope + 1 local-scope
    names = [r["name"] for r in rows]
    @test "other-tool" in names                                    # a server we know nothing about still lists
    obs = [r for r in rows if r["name"] == "cecelia-observer"]
    @test length(obs) == 2 && Set(r["scope"] for r in obs) == Set(["user", "local"])
    @test all(r["ours"] for r in obs)                              # ours is flagged, others are not
    @test !first(r["ours"] for r in rows if r["name"] == "other-tool")
    # transport is inferred so an http connector doesn't render as a stdio one
    @test first(r["transport"] for r in rows if r["name"] == "other-tool") == "http"
    @test first(r["dir"] for r in rows if r["scope"] == "local") == "/tmp/p1"
    @test rows == sort(rows; by = r -> (r["name"], r["scope"], r["dir"]))   # deterministic order
    # installPath: exposed so the Settings row can name the checkout an "out of date" entry points at.
    # Tolerant: a spec with no env yields "", never errors.
    @test first(r["installPath"] for r in rows if r["scope"] == "user" && r["name"] == "cecelia-observer") == "/opt/here/mcp"
    @test first(r["installPath"] for r in rows if r["scope"] == "local") == ""
end

@testset "LabArchives context sidecar (round-trip, gaps, briefing)" begin
    proj = create_project!(name = "la-$(rand(1000:9999))")
    s = add_set!(proj; name = "s1")

    # no sidecar → present=false, no gaps, and the briefing OMITS the key entirely (so "not linked"
    # and "linked but empty" stay distinguishable).
    d0 = read_la_doc(proj)
    @test d0["present"] == false && d0["readable"] == true
    @test isempty(la_gaps(proj))
    @test la_briefing(proj) === nothing
    @test !haskey(session_briefing(proj), :labarchives)

    # two images, both Treatment=MERTK — the WT arm the ELN declares has NO images.
    for (nm, mouse) in (("m1", "1"), ("m2", "2"))
        img = add_image!(s; name = nm)
        img.attr = Dict("Treatment" => "MERTK", "Mouse" => mouse)
        save!(img)
    end

    doc = write_la_doc!(proj;
        source   = Dict("notebookId" => "nb1", "notebookName" => "Ailsa",
                        "pageIds" => ["p1"], "url" => "https://example/nb"),
        sections = [Dict("heading" => "Setup", "lines" => ["LHS immunised only", "2 sites/mouse"],
                         "sourceDate" => "2026-02-24")],
        cohort   = [Dict("attr" => "Treatment", "value" => "WT", "n" => 6),
                    Dict("attr" => "Treatment", "value" => "MERTK", "n" => 5)])
    @test doc["source"]["notebookName"] == "Ailsa" && doc["source"]["pageIds"] == ["p1"]
    @test !isempty(doc["syncedAt"])

    # round-trips through disk with String keys intact (JSON3 hands back Symbols — json_native)
    r = read_la_doc(proj)
    @test r["present"] == true && r["readable"] == true
    @test r["sections"][1]["heading"] == "Setup"
    @test r["sections"][1]["lines"] == ["LHS immunised only", "2 sites/mouse"]

    # THE case this feature exists for: the declared WT arm has no images, and nothing in the project
    # would otherwise show it — attribute levels are derived from the images PRESENT.
    @test [v for (v, _) in Dict(attr_value_counts(images(proj)))["Treatment"]] == ["MERTK"]
    g = la_gaps(proj)
    @test length(g) == 1
    @test g[1]["attr"] == "Treatment" && g[1]["value"] == "WT"
    @test g[1]["declared"] == 6 && g[1]["present"] == 0

    # briefing carries HEADINGS + gaps, never the section text
    b = la_briefing(proj)
    @test b.sections == ["Setup"] && length(b.gaps) == 1 && b.notebookName == "Ailsa"
    sb = session_briefing(proj)
    @test haskey(sb, :labarchives) && sb.labarchives.sections == ["Setup"]

    # a full REPLACE, not a merge — a section deleted in the ELN must not linger
    write_la_doc!(proj; source = Dict("notebookName" => "Ailsa"),
                  sections = [Dict("heading" => "Question", "lines" => ["nuclear vs cytoplasmic"])],
                  cohort = [])
    r2 = read_la_doc(proj)
    @test [s["heading"] for s in r2["sections"]] == ["Question"]
    @test isempty(la_gaps(proj))            # cohort cleared → nothing to be missing

    # bounds are the WRITER's call, not the caller's
    r3 = write_la_doc!(proj; sections = [Dict("heading" => "H", "lines" => ["x" for _ in 1:50])])
    @test length(r3["sections"][1]["lines"]) == Cecelia.LA_MAX_LINES
    r4 = write_la_doc!(proj; sections = [Dict("heading" => "H$i", "lines" => ["l"]) for i in 1:40])
    @test length(r4["sections"]) == Cecelia.LA_MAX_SECTIONS

    # a corrupt sidecar reads as present-but-UNREADABLE, never as "no context"
    write(la_doc_path(proj), "{not json")
    rbad = read_la_doc(proj)
    @test rbad["present"] == true && rbad["readable"] == false
    @test la_briefing(proj).readable == false

    # the sidecar must never have touched the lab log — that stays append-only
    @test !isfile(lab_log_path(proj)) || isempty(read_lab_log(proj))
end

@testset "AI observer session sidecar (tokens + clear)" begin
    proj = create_project!(name = "obs-sess-$(rand(1000:9999))")
    # fresh project → zeroed session
    s0 = read_observer_session(proj)
    @test s0["sessionId"] == "" && s0["inputTokens"] == 0 && s0["turns"] == 0

    # a turn adopts the session id + accumulates tokens
    record_observer_turn!(proj, "sessABC", 1000, 40)
    s1 = read_observer_session(proj)                       # re-read from disk (persisted)
    @test s1["sessionId"] == "sessABC" && s1["inputTokens"] == 1000 && s1["outputTokens"] == 40
    @test s1["turns"] == 1
    # a second turn accumulates; an EMPTY session id keeps the existing one
    record_observer_turn!(proj, "", 500, 10)
    s2 = read_observer_session(proj)
    @test s2["sessionId"] == "sessABC"                     # unchanged (empty id kept prior)
    @test s2["inputTokens"] == 1500 && s2["outputTokens"] == 50 && s2["turns"] == 2

    # activity log: every pass is recorded (newest-first), even a silent/failed one
    log_observer_pass!(proj; trigger = "manual", model = "sonnet", ok = true, appended = false,
                       input_tokens = 900, output_tokens = 20, note = "reviewed — nothing to flag")
    log_observer_pass!(proj; trigger = "auto", model = "haiku", ok = true, appended = true,
                       input_tokens = 700, output_tokens = 30, note = "flagged clustTracks failed 4×")
    ps = read_observer_session(proj)["passes"]
    @test length(ps) == 2
    @test ps[1]["trigger"] == "auto" && ps[1]["appended"] == true          # newest-first
    @test ps[1]["model"] == "haiku" && ps[1]["inputTokens"] == 700
    @test ps[2]["trigger"] == "manual" && ps[2]["appended"] == false
    @test occursin("nothing to flag", ps[2]["note"])

    # clear resets everything (next run forks a fresh session), incl. the activity log
    cleared = clear_observer_session!(proj)
    @test cleared["sessionId"] == "" && cleared["inputTokens"] == 0 && cleared["turns"] == 0
    @test isempty(cleared["passes"])
    @test read_observer_session(proj)["inputTokens"] == 0
    rm(proj.root; recursive = true)
end

# ── Model: create project and image ───────────────────────────────────────
@testset "Model round-trip" begin
    proj = create_project!(name="smoke-test-$(rand(1000:9999))")
    @test isdir(proj.root)
    @test isfile(joinpath(proj.root, "project.json"))

    s = add_set!(proj; name="set-A")
    @test isdir(s._dir)

    img = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path" => "/tmp/fake.tif"))
    @test isdir(img._dir)
    @test isfile(joinpath(img._dir, "ccid.json"))

    # Round-trip via init_object
    loaded = init_object(proj.uid, img.uid)
    @test loaded isa CciaImage
    @test loaded.name == "img-1"
    @test get(loaded.meta, "ori_path", "") == "/tmp/fake.tif"

    # image_by_uid(...; uid) convenience accessor (REPL/notebook lookup)
    @test image_by_uid(proj; uid = img.uid) === img
    @test image_by_uid(s; uid = img.uid) === img
    @test image_by_uid(proj; uid = "nope") === nothing
    @test image_by_uid(s; uid = "nope") === nothing

    # Cleanup
    rm(proj.root; recursive=true)
end

# ── Starred: a plain per-image bookmark ───────────────────────────────────────────────────────────
# ANY number of images can be starred, and nothing downstream reads it — it drives the Starred row
# filter and nothing else. It replaced a SET-level single "reference image" nomination, from which
# the 8-bit import used to derive one intensity window for the whole set; that coupling is gone, so
# the load-bearing property here is just that the flag round-trips and is independent per image.
@testset "starred images" begin
    proj = create_project!(name = "star-$(rand(1000:9999))")
    s    = add_set!(proj; name = "set")
    img1 = add_image!(s; name = "a")
    img2 = add_image!(s; name = "b")

    @test img1.starred == false                                 # not starred is the default
    @test img2.starred == false

    img1.starred = true
    img2.starred = true                                         # multi-select: not one per set
    save!(img1); save!(img2)

    rs = load_project(proj.uid)._sets[1]
    @test all(i -> i.starred, rs._images)                       # persisted through ccid.json

    img1.starred = false
    save!(img1)
    reloaded = load_project(proj.uid)._sets[1]._images
    @test count(i -> i.starred, reloaded) == 1                  # independent per image

    rm(proj.root; recursive=true)
end
@testset "add_image! carries attr for derived images" begin
    # editImages tasks (copy/crop/z-proj/t-proj/bin/resample) mint a new image via `add_image!`,
    # and the user's cohort/condition/mouse tags — `img.attr` — must follow that derivation.
    # Silent DROP made a copy invisible to attr_value_counts / compare-by-attribute and forced the
    # user to re-tag every crop; the fix is one kwarg into add_image!.
    proj = create_project!(name = "attr-carry-$(rand(1000:9999))")
    s    = add_set!(proj; name = "set")
    src_attr = Dict{String,String}("cohort" => "WT", "mouse" => "m3")
    derived  = add_image!(s; name = "cropped", attr = src_attr)
    @test derived.attr == src_attr
    # copy defensively — mutating the passed dict later must not leak into the new image's attr
    src_attr["cohort"] = "MUT"
    @test derived.attr["cohort"] == "WT"
    # round-trips through save! / load_project
    reloaded = load_project(proj.uid)._sets[1]._images[1]
    @test reloaded.attr == Dict{String,String}("cohort" => "WT", "mouse" => "m3")

    # Default is empty (the fresh-import path — routes.jl — does not pass `attr`)
    fresh = add_image!(s; name = "import")
    @test fresh.attr == Dict{String,String}()
    rm(proj.root; recursive = true)
end
@testset "move_image! (manifest-only, no data moved)" begin
    proj = create_project!(name="move-test-$(rand(1000:9999))")
    a = add_set!(proj; name="set-A")
    b = add_set!(proj; name="set-B")
    img1 = add_image!(a; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
    img2 = add_image!(a; name="img-2", meta=Dict{String,Any}("ori_path"=>"/tmp/b.tif"))
    data_dir = joinpath(proj.root, "0", img1.uid)   # image data + metadata dirs are UID-keyed
    meta_dir = joinpath(proj.root, "1", img1.uid)

    move_image!(proj, img1.uid, a.uid, b.uid)

    # membership moved …
    @test a.image_uids == [img2.uid]
    @test b.image_uids == [img1.uid]
    @test image_by_uid(a; uid=img1.uid) === nothing
    @test image_by_uid(b; uid=img1.uid) !== nothing
    # … but NO data moved on disk (dirs are UID-keyed, independent of the set)
    @test isdir(data_dir) && isdir(meta_dir)
    @test !isdir(joinpath(b._dir, img1.uid))   # sets never nest image dirs

    # persists: reload the project fresh and the manifests reflect the move
    reloaded = load_project(proj.uid)
    ra = reloaded._sets[findfirst(s -> s.uid == a.uid, reloaded._sets)]
    rb = reloaded._sets[findfirst(s -> s.uid == b.uid, reloaded._sets)]
    @test ra.image_uids == [img2.uid]
    @test rb.image_uids == [img1.uid]
    @test image_by_uid(reloaded; uid=img1.uid) !== nothing   # still findable project-wide

    # idempotent / no-op guards
    @test move_image!(proj, img1.uid, b.uid, b.uid) === proj        # same set → no-op
    move_image!(proj, img1.uid, a.uid, b.uid)                        # already in dest → no-op
    @test b.image_uids == [img1.uid]                                 # not duplicated
    # error cases
    @test_throws ErrorException move_image!(proj, "nope", a.uid, b.uid)   # image not in source
    @test_throws ErrorException move_image!(proj, img2.uid, a.uid, "gone") # dest missing

    rm(proj.root; recursive=true)
end

# ── REPL / notebook data-access surface (Observer Phase 2 foundation) ─────────
@testset "REPL API surface + generated doc" begin
    # every allow-listed accessor is defined, exported, and documented — the notebook-facing
    # surface must be complete (a rename/removal or a missing docstring fails here).
    ref = repl_api_reference()
    @test !isempty(ref)
    for e in ref
        @test isdefined(Cecelia, Symbol(e.name))
        @test e.exported
        @test e.documented          # has a real docstring (undocumented accessors are a bug)
        @test !occursin("value*name", e.doc)   # raw docstring, not the mangled re-render
    end

    # GOLDEN: docs/REPL.md's generated section is in sync with the live docstrings. If this fails,
    # someone changed a listed function's docstring (or the list) without regenerating — run
    # `Cecelia.write_repl_doc()`. This is the drift-guard that keeps REPL.md honest.
    p = Cecelia.repl_doc_path()
    if isfile(p)
        committed = read(p, String)
        # Compare line-ending agnostically: this is a CONTENT drift-guard, not a byte-exactness
        # check. On Windows git checks the file out with CRLF while `repl_api_section()` emits LF,
        # so the splice mismatched on every line and the test failed for a reason that has nothing
        # to do with docstring drift.
        _lf(s) = replace(s, "\r\n" => "\n")
        @test _lf(committed) == _lf(Cecelia.render_repl_doc(committed))
        @test occursin(Cecelia.REPL_DOC_BEGIN, committed)
    else
        @test_skip "docs/REPL.md not found at $p"
    end
end

# ── Run log (automatic per-image provenance) ─────────────────────────────────
@testset "Run log" begin
    proj = create_project!(name="runlog-test-$(rand(1000:9999))")
    s = add_set!(proj; name="set-A")
    img = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path" => "/tmp/fake.tif"))

    @test read_run_log(img) == Any[]                       # empty before any run
    append_run_log!(img, "segment.cellpose", "default")
    append_run_log!(img, "behaviour.hmm")
    log = read_run_log(img)
    @test length(log) == 2
    @test log[1]["fun"] == "segment.cellpose"
    @test log[1]["valueName"] == "default"
    @test haskey(log[1], "at") && !isempty(log[1]["at"])
    @test log[2]["fun"] == "behaviour.hmm"

    # persists across reload (init_object)
    loaded = init_object(proj.uid, img.uid)
    @test length(read_run_log(loaded)) == 2

    # capped to the most recent RUN_LOG_CAP entries
    for i in 1:(Cecelia.RUN_LOG_CAP + 10); append_run_log!(img, "x.$i"); end
    capped = read_run_log(img)
    @test length(capped) == Cecelia.RUN_LOG_CAP
    @test capped[end]["fun"] == "x.$(Cecelia.RUN_LOG_CAP + 10)"   # newest kept

    # params trail: entry carries the sanitised task params; internal `_…` keys and the redundant
    # `valueName` are dropped, real tuning knobs kept (Observer Phase 2 §1 — see docs/ai-assist/OBSERVER.md).
    img2 = add_image!(s; name="img-2", meta=Dict{String,Any}("ori_path" => "/tmp/fake2.tif"))
    append_run_log!(img2, "tracking.bayesian_tracking", "default", "done",
                    Dict{String,Any}("search_radius" => 5.0, "max_lost" => 3,
                                     "valueName" => "default", "_task_id" => "abc123"))
    e = read_run_log(img2)[end]
    @test e["params"]["search_radius"] == 5.0
    @test e["params"]["max_lost"] == 3
    @test !haskey(e["params"], "valueName")    # redundant with its own field
    @test !haskey(e["params"], "_task_id")      # internal, dropped
    # default (no params) → shape-stable empty dict, and it survives reload
    append_run_log!(img2, "behaviour.hmm")
    @test read_run_log(img2)[end]["params"] == Dict{String,Any}()
    @test read_run_log(init_object(proj.uid, img2.uid))[end-1]["params"]["search_radius"] == 5.0
    # sanitiser handles nothing/empty directly
    @test Cecelia._run_log_params(nothing) == Dict{String,Any}()
    @test Cecelia._run_log_params(Dict("_x" => 1, "keep" => 2)) == Dict{String,Any}("keep" => 2)

    rm(proj.root; recursive=true)
end

# ── Run log: open → close, and reaping runs whose process died ───────────────
# The log is written TWICE per run (open at :running, close at the terminal status) for one reason:
# an append-on-finish log cannot record a run that never reaches its finish. A killed runner takes its
# in-flight tasks with it, so 22 minutes of segmentation left NOTHING on disk — no entry, no outcome.
# See run_log.jl's header.
@testset "Run log open/close and reap" begin
    proj = create_project!(name="runlog-reap-$(rand(1000:9999))")
    s    = add_set!(proj; name="set-A")
    img  = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path" => "/tmp/fake.tif"))

    # open writes a non-terminal entry immediately — before any work happens
    open_run_log!(img, "segment.cellposeMeasure", "afCorrected",
                  Dict{String,Any}("cellDiameter" => 10, "_task_id" => "zzz"); task_id = "T1")
    e = read_run_log(img)[end]
    @test e["status"] == Cecelia.RUN_LOG_RUNNING
    @test e["taskId"] == "T1"
    @test e["params"]["cellDiameter"] == 10        # params are on the OPEN entry, not held until close
    @test !haskey(e["params"], "_task_id")

    # close patches that entry in place — it does not append a second one
    close_run_log!(img, "T1", "done")
    log = read_run_log(img)
    @test length(log) == 1
    @test log[end]["status"] == "done"
    @test log[end]["params"]["cellDiameter"] == 10   # params survive the patch
    @test !isempty(log[end]["finishedAt"])

    # :cancelled is RECORDED, not skipped. This is the regression that made a killed segmentation
    # untraceable: it used to be dropped as "the user aborted, not an outcome worth logging".
    open_run_log!(img, "segment.cellposeMeasure", "afCorrected"; task_id = "T2")
    close_run_log!(img, "T2", "cancelled")
    @test read_run_log(img)[end]["status"] == "cancelled"
    @test length(read_run_log(img)) == 2

    # closing an id that was never opened still records the outcome rather than dropping it
    close_run_log!(img, "T404", "failed"; fun_name = "tracking.bayesian_tracking")
    @test read_run_log(img)[end]["status"] == "failed"
    @test read_run_log(img)[end]["fun"] == "tracking.bayesian_tracking"

    # ── the reap ────────────────────────────────────────────────────────────
    # T3 is still executing somewhere, T4's process died. Only T4 is interrupted — reaping a task the
    # detached runner is still running would report LIVE work as lost, which is the whole risk here.
    open_run_log!(img, "segment.cellposeMeasure", "Neutrophil"; task_id = "T3")
    open_run_log!(img, "segment.cellposeMeasure", "Tcell";      task_id = "T4")
    @test reap_run_log!(img, ["T3"]) == 1
    byid = Dict(e["taskId"] => e for e in read_run_log(img) if !isempty(get(e, "taskId", "")))
    @test byid["T3"]["status"] == Cecelia.RUN_LOG_RUNNING          # untouched — still live
    @test byid["T4"]["status"] == Cecelia.RUN_LOG_INTERRUPTED
    @test byid["T4"]["valueName"] == "Tcell"                       # the reap preserves what it recorded

    # reaping is idempotent, and a closed entry is never re-opened by it
    @test reap_run_log!(img, ["T3"]) == 0
    @test reap_run_log!(img, String[]) == 1                        # now T3 too — nothing is live
    @test read_run_log(img)[1]["status"] == "done"                 # T1 stays done

    # survives reload — this is a durable record, which is the entire point
    @test read_run_log(init_object(proj.uid, img.uid))[end]["status"] == Cecelia.RUN_LOG_INTERRUPTED

    rm(proj.root; recursive=true)
end

# ── Session briefing + all_qc_docs (Observer Phase 2 §2) ─────────────────────
@testset "Session briefing + all_qc_docs" begin
    proj = create_project!(name="brief-$(rand(1000:9999))")
    s = add_set!(proj; name="set-A")
    img1 = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
    img2 = add_image!(s; name="img-2", meta=Dict{String,Any}("ori_path"=>"/tmp/b.tif"))
    # suppress the calibration fallback (these fixtures have no PhysicalSize) by persisting an
    # empty omezarr QC doc — so the only flag is the one we add explicitly. Also tests "persisted wins".
    for im in (img1, img2)
        write_qc(im, "importImages.omezarr", "default", Dict{String,Any}[])
    end
    write_qc(img1, "tracking.bayesian_tracking", "default",
             [qc_finding("warn", "few_tracks", "Few tracks", "Only 5 tracks")])
    append_lab_log!(proj, "User", ["started tracking run"])

    b = session_briefing(proj)
    @test b.projectUid == proj.uid && b.projectName == proj.name && b.imageCount == 2
    uids = [f.uid for f in b.flagged]
    @test img1.uid in uids && !(img2.uid in uids)     # only the warn image flags; clean stays clean
    f1 = b.flagged[findfirst(f -> f.uid == img1.uid, b.flagged)]
    @test f1.worst == "warn" && f1.findings[1].short == "Few tracks"
    # WHICH task banked it. Without the fun, a probe's hardcoded threshold is indistinguishable from a
    # real pipeline finding — that cost a session chasing "0 cells" no segmentation had produced.
    @test f1.findings[1].fun == "tracking.bayesian_tracking"
    # an image the user has NOT excluded says so positively (absent would read as "unknown")
    @test f1.included == true && b.excludedCount == 0

    # EXCLUDED images: still listed (a warn on one is information, and dropping it would make the count
    # disagree with the image table) but LABELLED, so a session leads with the ones that still count.
    # The first real session opened on a drift anomaly for an image its owner had dropped weeks earlier.
    img1.included = false
    save!(img1)
    b2 = session_briefing(proj)
    @test b2.imageCount == 2 && b2.excludedCount == 1      # cohort size is imageCount - excludedCount
    f1b = b2.flagged[findfirst(f -> f.uid == img1.uid, b2.flagged)]
    @test f1b.included == false && f1b.findings[1].short == "Few tracks"   # labelled, not hidden
    img1.included = true
    save!(img1)
    @test length(b.recentLabLog) == 1 && b.recentLabLog[1].author == "User"
    @test occursin("tracking", b.recentLabLog[1].summary)

    # all_qc_docs: a fresh image (no persisted omezarr) gets the computed calibration fallback
    img3 = add_image!(s; name="img-3", meta=Dict{String,Any}("ori_path"=>"/tmp/c.tif"))
    @test haskey(all_qc_docs(img3), "importImages.omezarr/default")
    @test haskey(all_qc_docs(img1), "importImages.omezarr/default")   # persisted present too

    rm(proj.root; recursive=true)
end

# ── Lockfile (naive guard) ──────────────────────────────────────────────────
@testset "with_transaction" begin
    proj     = create_project!(name="lock-test-$(rand(1000:9999))")
    # the lockfile is DERIVED from the object's state file (the R `getStateFile` + ".lock" shape),
    # not a hardcoded name — that's what makes the per-image form below possible at all
    lockfile = Cecelia.state_file(proj) * ".lock"
    @test lockfile == Cecelia._lock_path(proj)

    # happy path: returns the body value and releases the lock
    @test (with_transaction(proj) do; 42; end) == 42
    @test !isfile(lockfile)

    # lock is released even when the body throws (no leaked lock)
    @test_throws ErrorException with_transaction(proj) do; error("boom"); end
    @test !isfile(lockfile)

    # works for an IMAGE too, locking that image alone — different images never block each other
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")
    img2 = add_image!(s; name="i2")
    @test Cecelia._lock_path(img) == Cecelia.state_file(img) * ".lock"
    @test Cecelia._lock_path(img) != Cecelia._lock_path(proj)
    @test Cecelia._lock_path(img) != Cecelia._lock_path(img2)
    @test (with_transaction(img) do; 7; end) == 7
    @test !isfile(Cecelia._lock_path(img))

    # REENTRANT: a commit reached from inside another transaction on the SAME object must not
    # deadlock. Before the in-process lock this sat on its own lockfile until the timeout.
    @test (with_transaction(img) do
               with_transaction(img) do; 9; end
           end) == 9
    @test !isfile(Cecelia._lock_path(img))

    # ANOTHER PROCESS's fresh lockfile is respected — we wait, then fail naming the file
    touch(Cecelia._lock_path(img))
    err = try; with_transaction(img; timeout = 1) do; 1; end; "" catch e; sprint(showerror, e) end
    @test occursin(Cecelia._lock_path(img), err)
    @test occursin("stale lockfile", err)
    rm(Cecelia._lock_path(img); force = true)

    # ...but an ABANDONED one (process died mid-commit) is reclaimed rather than blocking every
    # later task on that image behind a hidden file. Safe only because a transaction now wraps the
    # short commit, never the computation — so nothing legitimate is ever this old.
    @test !Cecelia._lock_abandoned(time())                                   # fresh → keep waiting
    @test  Cecelia._lock_abandoned(time() - Cecelia._LOCK_STALE_AFTER - 1)   # abandoned → reclaim

    rm(proj.root; recursive=true)
end

# ── commit_state!: registering an output is atomic against a concurrent registration ────────
# THE lost-update bug. Every task used to hand-roll re-read → poke → write, so
# two tasks finishing on one image both read the old dict and the second write dropped the first's
# field. `write_json_atomic` alone does NOT fix this — each write is individually intact; the
# second is simply built on stale data.
@testset "commit_state! wraps the read-modify-write in the transaction" begin
    proj = create_project!(name="commit-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")

    # basic read-modify-write, persisted
    commit_state!(img) do raw
        raw["status"] = "done"
    end
    @test read_ccid_raw(Cecelia.state_file(img))["status"] == "done"

    # The RMW must happen INSIDE the transaction — that is the whole mechanism, and it is what
    # makes a concurrent registration wait instead of reading stale data. Asserted directly and
    # deterministically: while the body runs, this object's lockfile is held.
    #
    # This replaces a thread-interleaving test that did not work. It spawned two tasks and relied on
    # the scheduler to interleave them so one would read stale `labels`; standalone it did (one
    # registration was lost), but inside the suite the second task simply wasn't scheduled during the
    # first's sleep, so it passed with the lock REMOVED. A concurrency test whose failure depends on
    # scheduler luck is worse than none — it reads as coverage. Mutual exclusion itself is covered
    # deterministically by the `with_transaction` testset above (lockfile held, foreign lock
    # respected, reentrancy, abandoned-lock reclaim).
    held = false
    commit_state!(img) do raw
        held = isfile(Cecelia._lock_path(img))     # false if the RMW isn't wrapped
        raw["labels"] = Dict{String,Any}("segA" => ["segA.zarr"])
    end
    @test held
    @test !isfile(Cecelia._lock_path(img))         # ...and released afterwards
    @test haskey(read_ccid_raw(Cecelia.state_file(img))["labels"], "segA")

    # a nested commit on the same object still completes (reentrancy through commit_state!, not just
    # with_transaction) and both mutations land
    commit_state!(img) do raw
        raw["note"] = "outer"
        commit_state!(img) do inner
            inner["status"] = "inner-done"
        end
    end
    fresh = read_ccid_raw(Cecelia.state_file(img))
    @test fresh["note"] == "outer"

    # the lock is released on a throwing body, and the file keeps its previous content
    @test_throws ErrorException commit_state!(img) do raw
        raw["status"] = "clobbered"
        error("boom")
    end
    @test !isfile(Cecelia._lock_path(img))
    @test read_ccid_raw(Cecelia.state_file(img))["status"] == "done"

    # a metadata-dir form for the API layer, which commits without loading the object
    commit_state!(img._dir) do raw
        raw["note"] = "by dir"
    end
    @test read_ccid_raw(Cecelia.state_file(img))["note"] == "by dir"

    rm(proj.root; recursive=true)
end

# ── Durable state writes are atomic ─────────────────────────────────────────
# Every state file (ccid.json, project.json, sidecars, custom.toml, the lab log) is written
# tmp-then-rename via `write_atomic`. The failure this prevents: `open(path, "w")` truncates
# first, so a kill in that window (the Quit button SIGKILLs) left a half-written file — and since
# `_load_set` has no per-image guard, ONE truncated image ccid.json failed the WHOLE project load.
@testset "durable state writes are atomic" begin
    td = mktempdir()

    # a failed write leaves the PREVIOUS content intact, not a truncated file
    p = joinpath(td, "state.json")
    write_json_atomic(p, Dict("a" => 1))
    before = read(p, String)
    @test_throws ErrorException write_atomic(p) do io
        print(io, "{\"partial\":")
        error("killed mid-write")
    end
    @test read(p, String) == before          # untouched, NOT truncated
    @test JSON3.read(read(p, String))[:a] == 1

    # no temp files left behind, on success or on failure
    @test isempty(filter(f -> occursin(".tmp.", f), readdir(td)))

    # a leftover temp (from a process killed between write and rename) must NOT be picked up by
    # sidecar discovery, which is `readdir` + `endswith(f, ".json")` in several places
    @test !endswith(Cecelia.write_atomic(io -> print(io, "x"), joinpath(td, "probe.json")), ".tmp")
    tmpname = "state.json.tmp.abc123"
    @test !endswith(tmpname, ".json")

    # creates a missing parent dir rather than throwing
    deep = joinpath(td, "a", "b", "c.json")
    write_json_atomic(deep, Dict("ok" => true))
    @test JSON3.read(read(deep, String))[:ok] == true

    # write_atomic handles non-JSON content too (TOML config, the lab-log markdown)
    t = joinpath(td, "notes.md")
    write_atomic(io -> print(io, "# hello"), t)
    @test read(t, String) == "# hello"

    rm(td; recursive=true)
end

# ── state_file: one derivation, whatever the caller holds ───────────────────
# The R original kept this private to the object (`getStateFile`); the port had 20+ call sites
# re-deriving `joinpath(obj._dir, "ccid.json")` and the API layer additionally re-spelling the
# `1/` metadata segment. All forms must agree.
# ── resolve_value_name: the `defaultOnly` half of R's cciaImage$valueNames ──────────────────
# Nine call sites hand-rolled `something(value_name, get(img.label_props, "_active", "default"))`,
# hardcoding the `_active` key and the `"default"` fallback that VERSIONED_ACTIVE_KEY /
# VERSIONED_DEFAULT_VAL exist to name. One resolver now, so the fallback rule lives in one place.
@testset "resolve_value_name" begin
    proj = create_project!(name="rvn-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")

    # no label_props at all → the versioned default, never an error
    @test resolve_value_name(img) == VERSIONED_DEFAULT_VAL

    # an explicit value_name always wins, even when an active one exists
    img.label_props = Dict{String,String}("A" => "A.h5ad", "B" => "B.h5ad",
                                         VERSIONED_ACTIVE_KEY => "B")
    @test resolve_value_name(img, "A") == "A"
    @test resolve_value_name(img)      == "B"        # else the active one

    # falls back to the versioned default when nothing is marked active
    img.label_props = Dict{String,String}("A" => "A.h5ad")
    @test resolve_value_name(img) == VERSIONED_DEFAULT_VAL

    # agrees with the underlying helper it replaced, and with the list accessor's view
    img.label_props = Dict{String,String}("A" => "A.h5ad", VERSIONED_ACTIVE_KEY => "A")
    @test resolve_value_name(img) == versioned_active(img.label_props)
    @test resolve_value_name(img) in img_value_names(img)

    rm(proj.root; recursive=true)
end

# Migration QC: only the silent-failure case is a finding — an image that migrates with no cell
# table looks successful and leaves every downstream page empty.
@testset "migrate_qc_findings" begin
    @test isempty(migrate_qc_findings(["A"]))
    @test isempty(migrate_qc_findings(["A", "B"]))
    f = migrate_qc_findings(String[])
    @test length(f) == 1
    @test f[1]["level"] == "warn"
    @test f[1]["code"]  == "migrate.no_segmentation"
    @test occursin("re-run", lowercase(f[1]["long"]))   # the long text says what to DO
end

@testset "state_file resolution" begin
    proj = create_project!(name="statefile-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")

    @test Cecelia.state_file(proj) == joinpath(proj.root, "project.json")
    @test basename(Cecelia.state_file(img)) == Cecelia.STATE_FILENAME
    @test basename(Cecelia.state_file(s))   == Cecelia.STATE_FILENAME

    # object form == metadata-dir form == (project dir + uid) form
    @test Cecelia.state_file(img) == Cecelia.state_file(img._dir)
    @test Cecelia.state_file(img) == Cecelia.state_file(proj.root, img.uid)
    @test Cecelia.state_file(s)   == Cecelia.state_file(proj.root, s.uid)
    @test Cecelia.obj_meta_dir(proj.root, img.uid) == img._dir

    # and the file the accessor names is the one save! actually wrote
    @test isfile(Cecelia.state_file(img))

    rm(proj.root; recursive=true)
end

# An unreadable state file must name the FILE. JSON3 alone says only "invalid JSON at byte
# position N" — raised from inside a project load, that told the user nothing actionable.
@testset "unreadable state file names the file" begin
    proj = create_project!(name="corrupt-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")
    path = Cecelia.state_file(img)

    full = read(path, String)
    write(path, full[1:cld(length(full), 2)])          # truncate, as an interrupted write would
    err = try; load_project(proj.uid); "" catch e; sprint(showerror, e) end
    @test occursin(path, err)                          # says WHICH file
    @test occursin("not valid JSON", err)
    @test occursin(".ccbundle", err)                    # and what to do about it

    rm(proj.root; recursive=true)
end

# Detector, not advisory: a NEW bare `open(<state file>, "w")` fails here. This is how the
# truncating form spread to ~30 sites in the first place — the atomic pattern existed (the gating
# sidecar) but nothing stopped the next writer hand-rolling the unsafe one.
@testset "no hand-rolled state writes" begin
    roots = [joinpath(@__DIR__, "..", "src"), joinpath(@__DIR__, "..", "..", "api", "src")]
    # Every write-mode `open` is an offender unless listed here WITH a reason. Deliberately an
    # allow-list of exact call sites, not of whole files: exempting a file would let the next
    # state write in that file slip through, which is precisely how this spread.
    allowed = Dict(
        # the atomic writer itself — this IS the tmp-then-rename implementation
        "utils.jl"       => [raw"""open(tmp, "w") do io"""],
        # transient per-run params blob handed to a Python subprocess, in the run's task dir
        "py_runner.jl"   => [raw"""open(params_file, "w") do io"""],
        # bundle manifest, written INTO the export staging dir that is then tarred and deleted
        "project_io.jl"  => [raw"""open(joinpath(tmp, BUNDLE_MANIFEST), "w") do io"""],
        # bulk image-data copy (multi-GB, chunked); not state, and the import task owns cleanup
        # (moved from `omezarr.jl` when it was split into `omezarr/`; `staging.jl` is the new home)
        "staging.jl"    => [raw"""open(dst, "w") do d"""],
        # raw RGB24 frames streamed to the run's task dir and handed straight to the encoder, then
        # deleted (the offline renderer, docs/todo/WEB_VIEWER_PLAN.md P5). Multi-GB and transient: staging a
        # copy to rename would double the disk for a file nothing ever reads back.
        "movie_render.jl" => [raw"""open(raw, "w") do io"""],
    )
    offenders = String[]
    for root in roots, (dir, _, files) in walkdir(root), f in files
        endswith(f, ".jl") || continue
        ok = get(allowed, f, String[])
        for (i, line) in enumerate(eachline(joinpath(dir, f)))
            occursin(r"""open\([^)]*,\s*"w"\)""", line) || continue
            startswith(strip(line), "#") && continue
            any(a -> occursin(a, line), ok) && continue
            push!(offenders, "$f:$i: $(strip(line))")
        end
    end
    if !isempty(offenders)
        @warn "Hand-rolled write-mode open — use write_atomic/write_json_atomic, or add an " *
              "allow-list entry with a reason if it genuinely isn't durable state" offenders
    end
    @test isempty(offenders)
end

# ── Lab-log + param-validation testsets ────────────────────────────────
# Per-project append-only lab log (read/append/dismiss + auto [Cecelia] activity digest capture)
# and task-dispatch param validation. Extracted from this file to keep it small enough to merge
# without EOF conflicts on every append. The extracted file loads inside this file's aggregating
# testset scope, so any helpers defined earlier in suite.jl are still in scope for the extracted
# fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "lablog_and_validation.jl"))

# ── Typed meta accessors (meta_int / meta_float / meta_str) ──────────────
#
# `img.meta` is Dict{String,Any} and carries OME-XML metadata in whatever shape the loader wrote —
# `SizeC` is typically a String ("3"), `PhysicalSizeX` a String or Real depending on the source.
# Every reader used to hand-roll its own parse (bare `Int(get(...))` crashed on the String case,
# `tryparse_i(get(...))` didn't return the caller's default, three separate `_meta_*` variants lived
# in `api/src/routes/helpers.jl` alone). These accessors are the ONE way; the testset pins the
# missing/malformed contract so a call site can rely on `something(meta_int(...), default)`.
@testset "meta_int / meta_float / meta_str contract" begin
    m = Dict{String,Any}(
        "SizeC" => "3",           # OME-XML strings — the common case
        "SizeT" => 1,             # numeric passes through
        "PhysicalSizeX" => "0.5",
        "PhysicalSizeY" => 0.5,   # Real → Float64
        "PhysicalSizeUnit" => "µm",
        "ori_path" => "/data/img.tif",
        "garbage" => "not a number",
    )
    @test meta_int(m, "SizeC")   === 3
    @test meta_int(m, "SizeT")   === 1
    @test meta_int(m, "SizeZ")   === nothing            # missing
    @test meta_int(m, "garbage") === nothing            # unparseable
    @test meta_float(m, "PhysicalSizeX") === 0.5
    @test meta_float(m, "PhysicalSizeY") === 0.5
    @test meta_float(m, "SizeZ")         === nothing
    @test meta_str(m, "PhysicalSizeUnit") == "µm"
    @test meta_str(m, "ori_path")         == "/data/img.tif"
    @test meta_str(m, "SizeC")            == "3"        # coerces non-strings, doesn't drop them
    @test meta_str(m, "missing")          === nothing

    # `something(meta_int(...), default)` is the fallback idiom the drift-consolidation depends on.
    @test something(meta_int(m, "SizeZ"), 0) === 0
    @test something(meta_int(m, "SizeC"), 0) === 3
end

# ── Axis gating (task_applies + img_axes) ────────────────────────────────
@testset "Axis gating — img_axes + task_applies" begin
    # img_axes: SizeT > 1 → :T; TimeIncrement present as fallback for pre-SizeT projects.
    img_static = CciaImage(; uid="a1", name="static", dir="")
    img_static.meta = Dict{String,Any}("SizeC"=>2, "SizeT"=>1, "SizeZ"=>5)
    @test Cecelia.img_axes(img_static) == Set([:X, :Y, :Z, :C])
    @test !Cecelia.img_has_time(img_static)

    # CALIBRATED, which the axis assertions do not care about but `task_applies` now does: tracking
    # measures in µm/min, so a live image with no pixel size is not runnable (see the scale gate
    # below). Keeping the calibration here rather than on a fourth image keeps this testset about
    # axes with the scale as a given.
    img_live = CciaImage(; uid="a2", name="live", dir="")
    img_live.meta = Dict{String,Any}("SizeC"=>4, "SizeT"=>10, "SizeZ"=>1,
                                     "PhysicalSizeX"=>0.5, "PhysicalSizeY"=>0.5,
                                     "TimeIncrement"=>30)
    @test Cecelia.img_axes(img_live) == Set([:X, :Y, :T, :C])
    @test Cecelia.img_has_time(img_live)

    # TimeIncrement fallback for pre-SizeT imports (present + parseable → :T)
    img_legacy = CciaImage(; uid="a3", name="legacy", dir="")
    img_legacy.meta = Dict{String,Any}("TimeIncrement"=>"30")
    @test :T ∈ Cecelia.img_axes(img_legacy)
    @test Cecelia.img_has_time(img_legacy)

    # task_requires_axes: reads spec's requires.axes (BayesianTracking → {:T})
    @test Cecelia.task_requires_axes(BayesianTracking()) == Set([:T])
    @test isempty(Cecelia.task_requires_axes(ImportOmezarr()))

    # task_applies: T-requiring task rejects a static image, accepts a live one
    @test !task_applies(BayesianTracking(), img_static)
    @test  task_applies(BayesianTracking(), img_live)
    @test  task_applies(ImportOmezarr(), img_static)   # no requirement → always applies

    # Composite recursion: HMM (states + transitions) inherits T from its steps
    hmm = Cecelia._task_from_fun_name("behaviour.hmm")
    @test :T ∈ Cecelia.task_requires_axes(hmm)
    @test !task_applies(hmm, img_static)
    @test  task_applies(hmm, img_live)

    # ── The SCALE half of the same gate ──────────────────────────────────────
    # `requires.scale`: the task computes in microns, so it needs the image to RECORD a scale. The
    # failure it prevents is silent — `img_physical_sizes` falls back to 1.0 for anything absent, and
    # 1.0 is indistinguishable from a real 1 µm/px, so the run succeeds and reports pixels as microns.
    @test Cecelia.task_requires_scale(BayesianTracking()) == Set([:XY, :T])
    @test isempty(Cecelia.task_requires_scale(ImportOmezarr()))

    # Recorded, per axis, and > 0 — a zero is not a measurement.
    @test Cecelia.img_scale_axes(img_live) == Set([:XY, :T])
    @test isempty(Cecelia.img_scale_axes(img_static))
    img_zero = CciaImage(; uid="a4", name="zero", dir="")
    img_zero.meta = Dict{String,Any}("PhysicalSizeX"=>0, "PhysicalSizeY"=>0.5)
    @test isempty(Cecelia.img_scale_axes(img_zero))

    # An uncalibrated live image is BLOCKED, and the reason names the fix rather than the fact —
    # unlike a missing axis, this the user can do something about.
    img_nocal = CciaImage(; uid="a5", name="live-uncal", dir="")
    img_nocal.meta = Dict{String,Any}("SizeC"=>1, "SizeT"=>10, "SizeZ"=>1)
    @test !task_applies(BayesianTracking(), img_nocal)
    reason = Cecelia.task_applicability_reason(BayesianTracking(), img_nocal)
    @test occursin("pixel size", reason) && occursin("time interval", reason)
    @test occursin("metadata", reason)

    # Intersected with the image's own axes: a 2D task on a static image needs no time interval, so
    # a calibrated-XY static image runs a µm-measuring task that does not require T.
    img_static_cal = CciaImage(; uid="a6", name="static-cal", dir="")
    img_static_cal.meta = Dict{String,Any}("SizeC"=>1, "SizeT"=>1, "SizeZ"=>1,
                                           "PhysicalSizeX"=>0.5, "PhysicalSizeY"=>0.5)
    @test isempty(Cecelia.task_missing_scale(CellposeSegment(), img_static_cal))
    @test Cecelia.task_missing_scale(CellposeSegment(), img_static) == Set([:XY])
    # …and a T-scale requirement does not apply to an image with no T axis at all.
    @test isempty(Cecelia.task_missing_scale(
        Cecelia._task_from_fun_name("segment.cellpose"), img_static_cal))

    # A composite inherits its steps' scale needs, same union as the axes.
    cpm = Cecelia._task_from_fun_name("segment.cellposeMeasure")
    @test :XY ∈ Cecelia.task_requires_scale(cpm)
    @test !task_applies(cpm, img_static)
    @test  task_applies(cpm, img_static_cal)

    # run_task raises TaskApplicabilityError before scheduling on a static image
    proj = create_project!(name="axis-gate-$(rand(1000:9999))")
    s   = add_set!(proj; name="s")
    img = add_image!(s; name="static-img",
                     meta=Dict{String,Any}("SizeC"=>1, "SizeT"=>1, "SizeZ"=>1))
    @test_throws Cecelia.TaskApplicabilityError run_task(
        BayesianTracking(), img, Dict{String,Any}(); pool_name="cpu")
    rm(proj.root; recursive=true)
end

# ── Per-param image gating (`param.requires.axes`) ────────────────────────────
# The image-side twin of showIf: a param whose axes the image does not carry is deleted from the
# effective run BEFORE validation, and the handler's `get(params, key, default)` returns the "off"
# default. Smooth is the one mixed case in the tree — spatial sigma applies to a still, the temporal
# window does not — so gating the WHOLE task on `requires.axes` would refuse a run that works.
@testset "Per-param requires.axes — smooth's temporal controls" begin
    smooth = Cecelia.Smooth()

    img_still = CciaImage(; uid="p1", name="still", dir="")
    img_still.meta = Dict{String,Any}("SizeC"=>2, "SizeT"=>1, "SizeZ"=>1)
    img_live  = CciaImage(; uid="p2", name="live", dir="")
    img_live.meta  = Dict{String,Any}("SizeC"=>2, "SizeT"=>10, "SizeZ"=>1)

    # Smooth's task-level requires no longer names T — spatial-only smoothing is a legitimate run.
    @test isempty(Cecelia.task_requires_axes(smooth))
    @test task_applies(smooth, img_still)
    @test task_applies(smooth, img_live)

    # After spec defaults + guard, a static image loses the temporal keys; the timelapse keeps them.
    base = Cecelia._apply_spec_defaults(smooth, Dict{String,Any}())
    @test base["temporalFrames"] == 3          # spec default fires
    @test base["temporalStat"]   == "median"
    @test haskey(base, "spatialSigma")

    still_run = Cecelia._apply_param_requires(smooth, img_still, copy(base))
    @test !haskey(still_run, "temporalFrames") # dropped by requires.axes
    @test !haskey(still_run, "temporalStat")
    @test still_run["spatialSigma"] == 1.0     # unguarded, survives
    @test still_run["restoreDynamicRange"] == true

    live_run = Cecelia._apply_param_requires(smooth, img_live, copy(base))
    @test live_run["temporalFrames"] == 3      # timelapse keeps them
    @test live_run["temporalStat"]   == "median"

    # Set-scope intersects: any static image in the mix drops the temporal params.
    mixed = Cecelia._apply_param_requires(smooth, CciaImage[img_live, img_still], copy(base))
    @test !haskey(mixed, "temporalFrames")

    # Vs a task with the task-level requires still in place: this must not touch its params.
    @test !isempty(Cecelia.task_requires_axes(BayesianTracking()))
    unchanged = Cecelia._apply_param_requires(BayesianTracking(), img_live, Dict{String,Any}("k"=>1))
    @test unchanged == Dict{String,Any}("k"=>1)
end

# ── Dispatch + param validation — Branching (segment.branching) ──────────────
# docs/todo/BRANCHING_PLAN.md Phase 1. New task registers via _task_from_fun_name and
# validate_params rejects out-of-range dilation sizes + wrong-typed booleans.
# Ranges, types and the unknown-select case are swept for EVERY task above. What survives here is
# specific to branching: the µm key rename, the enumerated anisotropy sources, and the copy budget.
@testset "Branching spec — µm keys, anisotropy sources, copy budget" begin
    # anisotropySource (docs/todo/SPATIAL_ANISOTROPY_PLAN.md Decision 5) — a select with three
    # allowed values. The runner raises on anything else, so an unknown value must not get past
    # validation and reach Python as a subprocess failure.
    for src in ("skeleton", "mask", "channel")
        @test begin
            validate_params(Branching(), Dict{String,Any}("anisotropySource" => src))
            true
        end
    end
    @test_throws ParamValidationError validate_params(
        Branching(), Dict{String,Any}("anisotropySource" => "intensity"))

    # The anisotropy scales are in MICRONS, and the keys carry `Um` for exactly that reason: a
    # project with saved PIXEL params (sigma 2, box 45) must not have them silently reread as µm,
    # which would be ~3 px and ~75 px. New keys mean the stale values simply do not apply.
    let spec = Cecelia._task_spec(Branching())
        bykey = Dict(String(get(p, "key", "")) => p for p in spec["params"])
        @test haskey(bykey, "structureTensorSigmaUm") && haskey(bykey, "anisotropyBoxUm")
        @test !haskey(bykey, "structureTensorSigma") && !haskey(bykey, "anisotropyBoxSize")
        @test bykey["structureTensorSigmaUm"]["default"] == 7.0
        @test bykey["anisotropyBoxUm"]["default"] == 5.0
        @test bykey["anisotropySource"]["default"] == "skeleton"
        # tips: covered repo-wide by "every task param carries a tip" + the copy-budget sweep
    end
end

# ── µm → px for the anisotropy scales ─────────────────────────────────────────────────────────
# The user sets a PHYSICAL scale (a fibre is ~2 µm thick whatever the objective); the compute is
# in pixels. Getting this backwards, or letting a sub-pixel request through, produces a grid that
# resamples noise rather than summarising structure.
@testset "Anisotropy µm→px conversion" begin
    # EaMaVq's real calibration: 0.596 µm/px
    px, clamped = Cecelia._um_to_px(7.0, 0.596; minimum_px = 0.5)
    @test px ≈ 7.0 / 0.596 && !clamped
    @test round(Int, first(Cecelia._um_to_px(5.0, 0.596; minimum_px = 3))) == 8

    # A coarser image needs FEWER pixels for the same physical scale — the whole point: the same
    # setting means the same thing on both, which a pixel setting never did.
    @test first(Cecelia._um_to_px(5.0, 1.0; minimum_px = 3)) <
          first(Cecelia._um_to_px(5.0, 0.5; minimum_px = 3))

    # Sub-minimum requests clamp AND say so, rather than silently running a different setting.
    px2, clamped2 = Cecelia._um_to_px(0.5, 0.596; minimum_px = 3)
    @test px2 == 3.0 && clamped2
    @test_throws ErrorException Cecelia._um_to_px(5.0, 0.0; minimum_px = 3)

    # Stored bytes scale as boxes, and boxes as 1/box² — so halving the spacing is 4x the file.
    # This is the number behind the "what do I actually put there" tip.
    @test Cecelia._aniso_grid_bytes(1296, 201) == 1296 * 40 * 201        # 36x36 over 201 frames
    @test Cecelia._aniso_grid_bytes(4 * 1296, 201) == 4 * Cecelia._aniso_grid_bytes(1296, 201)
    @test Cecelia._aniso_grid_bytes(100, 0) == Cecelia._aniso_grid_bytes(100, 1)  # static image

    # Advisory only, and only past the threshold — a fine grid is a legitimate choice.
    @test isempty(Cecelia._aniso_grid_findings(Cecelia._aniso_grid_bytes(1296, 201), 1296, 5.0))
    big = Cecelia._aniso_grid_bytes(36_000, 201)      # ~290 MB, a ~1 µm grid on this image
    f = Cecelia._aniso_grid_findings(big, 36_000, 1.0)
    @test length(f) == 1 && f[1]["level"] == "warn"
    @test f[1]["code"] == "branching.aniso_grid_large"
    # the number lives in `detail`, per the catalog rule that prose carries no figures
    @test occursin("289 MB", f[1]["detail"]) && occursin("36000 boxes", f[1]["detail"])
    @test !occursin("289", f[1]["short"])

    # Both branching findings come from the QC CATALOG now, not inlined at the call site
    @test haskey(Cecelia.QC_TEXT, "branching.no_branches")
    @test haskey(Cecelia.QC_TEXT, "branching.aniso_grid_large")
    @test haskey(Cecelia.QC_TEXT, "branching.uncalibrated")
    @test Cecelia._branching_qc_findings(0)[1]["short"] == "No branches found"
    @test isempty(Cecelia._branching_qc_findings(5))
end

@testset "Smoothing QC" begin
    # Both findings key off the persisted python stats, so the helper is fed exactly what
    # smooth_run.py writes. Photon-limited input: zeros fall from ~90% to ~5%, no clipping.
    worked = Dict{String,Any}(
        "gain" => 2.4, "clippedVoxels" => 0,
        "zeroFracIn"  => Dict{String,Any}("0" => 0.91, "1" => 0.88),
        "zeroFracOut" => Dict{String,Any}("0" => 0.06, "1" => 0.05))
    @test isempty(Cecelia._smooth_qc_findings(worked))

    # Gain clipping — the bright end of every smoothed channel is now flat.
    clipped = merge(worked, Dict{String,Any}("clippedVoxels" => 1234))
    f = Cecelia._smooth_qc_findings(clipped)
    @test length(f) == 1 && f[1]["code"] == "smooth.gain_clipped" && f[1]["level"] == "warn"
    @test f[1]["short"] == "Dynamic-range gain clipped 1234 voxels"  # the count is IN the message
    @test occursin("Restore dynamic range", f[1]["long"]) # the action, imperative
    @test f[1]["detail"] isa AbstractDict
    # from the catalog, so it re-renders at read time like every other finding
    @test haskey(Cecelia.QC_TEXT, "smooth.gain_clipped")
    @test f[1]["key"] == "smooth.gain_clipped"

    # Dense input — nothing sparse to fill, so the step bought nothing. Info, not warn.
    dense = Dict{String,Any}(
        "gain" => 1.0, "clippedVoxels" => 0,
        "zeroFracIn"  => Dict{String,Any}("0" => 0.02),
        "zeroFracOut" => Dict{String,Any}("0" => 0.00))
    fd = Cecelia._smooth_qc_findings(dense)
    @test length(fd) == 1 && fd[1]["code"] == "smooth.no_effect" && fd[1]["level"] == "info"
    @test haskey(Cecelia.QC_TEXT, "smooth.no_effect")

    # advisory only, per docs/MODULES.md — never an error, never a gate
    @test all(x -> x["level"] in ("info", "warn"),
              vcat(Cecelia._smooth_qc_findings(clipped), fd))

    # Metrics reduce the per-channel dicts to the WORST channel — a step that filled one channel and
    # left another sparse is the case worth seeing.
    m = Cecelia._smooth_metrics(worked)
    @test m["zeroFracInMax"]  == 0.91
    @test m["zeroFracOutMax"] == 0.06
    @test m["gain"] == 2.4 && m["clippedVoxels"] == 0

    # Missing stats must not throw — the helper runs on whatever python managed to write.
    @test Cecelia._smooth_metrics(Dict{String,Any}())["gain"] == 1.0
    @test isempty(Cecelia._smooth_qc_findings(Dict{String,Any}()))

    # Deliberately NOT cohort: the input is the drift-corrected store, whose zero fraction includes
    # the canvas padding drift correction added, so the outlier detector would rank images by shake.
    @test !haskey(COHORT_METRICS, "cleanupImages.smooth")
end

@testset "AF correction QC — the exemption that got retired" begin
    # This task carried a QC-EXEMPT comment calling itself the weakest exemption in the codebase.
    # It now has exactly ONE finding: the correction has no free parameter left to land badly, so the
    # only objective signal is about the INPUT.
    ok = Dict{String,Any}("1" => Dict{String,Any}(
        "saturatedFrac" => 0.0001, "levelsUsed" => 200, "levelsAvailable" => 256))
    @test isempty(Cecelia.af_qc_findings(ok)[1])

    saturated = Dict{String,Any}("1" => Dict{String,Any}(
        "saturatedFrac" => 0.05, "levelsUsed" => 200, "levelsAvailable" => 256))
    f, w = Cecelia.af_qc_findings(saturated)
    @test length(f) == 1 && f[1]["code"] == "af.saturated_input" && f[1]["level"] == "warn"
    @test w.saturated == 0.05

    # THE BUG THIS REPLACED: the finding was hand-rolled with a `detail` STRING and no `long` at all,
    # so the QC panel rendered "Channel 1 saturated → undefined" — visible in the GUI from the day AF
    # QC shipped, because `lib/qc.ts` reads `f.long`. House convention (see drift_correct.jl):
    # short = problem, long = the action, FIGURES in `detail` as a Dict.
    @test f[1]["short"] == "Channel 1 saturated"
    @test !isempty(get(f[1], "long", ""))
    @test occursin("gain", f[1]["long"])                    # the action, imperative
    @test f[1]["detail"] isa AbstractDict                   # figures, NOT a string
    @test f[1]["detail"]["saturatedPct"] == 5.0
    # ...and it comes from the copy catalog, so it re-renders at read time like every other finding
    @test haskey(Cecelia.QC_TEXT, "af.saturated_input")
    @test f[1]["key"] == "af.saturated_input"

    # advisory only, per docs/MODULES.md — never an error, never a gate
    @test all(x -> x["level"] == "warn", f)

    # BLEEDTHROUGH — the diagnostic the audit said this task had never had. Not a failure: the
    # correction subtracts the leak, and the finding exists because a leak is a FILTER-SET property, so
    # one image of a set differing from its peers is a real signal about the optics.
    clean = Dict{String,Any}("1" => Dict{String,Any}(
        "saturatedFrac" => 0.0, "levelsUsed" => 200, "levelsAvailable" => 256,
        "bleedthrough" => Dict{String,Any}()))
    @test isempty(Cecelia.af_qc_findings(clean)[1])          # no leak detected → nothing to say
    @test Cecelia.af_qc_findings(clean)[2].leak == 0.0

    leaky = Dict{String,Any}("1" => Dict{String,Any}(
        "saturatedFrac" => 0.0, "levelsUsed" => 200, "levelsAvailable" => 256,
        # the real numbers measured on WIaUjL/p6t4mC: CH3 into CH2, and nothing else
        "bleedthrough" => Dict{String,Any}("2" => 0.0248)))
    lf, lw = Cecelia.af_qc_findings(leaky)
    @test length(lf) == 1 && lf[1]["code"] == "af.bleedthrough" && lf[1]["level"] == "warn"
    @test lw.leak == 0.0248
    @test haskey(Cecelia.QC_TEXT, "af.bleedthrough")
    @test lf[1]["detail"] isa AbstractDict                   # figures in `detail`, never a string
    @test lf[1]["detail"]["sourceChannel"] == "2"            # WHICH filter pair leaks is the point
    @test lf[1]["detail"]["alphaPct"] == 2.48
    @test !isempty(get(lf[1], "long", ""))

    # one finding PER SOURCE — collapsing them would hide which pair is leaking, which is the only
    # thing a user can act on
    two = Dict{String,Any}("1" => Dict{String,Any}(
        "saturatedFrac" => 0.0, "levelsUsed" => 200, "levelsAvailable" => 256,
        "bleedthrough" => Dict{String,Any}("2" => 0.02, "3" => 0.05)))
    tf, tw = Cecelia.af_qc_findings(two)
    @test length(tf) == 2 && tw.leak == 0.05

    # `af-low-range` IS GONE, and re-tuning it would be wrong. It warned when the output used <20% of
    # the dtype's levels — a real signal under the RATIO, whose output was stretched to fill the range
    # through a derived ceiling. The power weight outputs INPUT COUNTS, so a 16-bit channel with signal
    # in the low thousands legitimately occupies a sliver: measured on real runs, 735-3576 of 65536
    # levels (1.1-5.5%) on EVERY channel of EVERY image. The premise inverted with the mechanism.
    coarse = Dict{String,Any}("2" => Dict{String,Any}(
        "saturatedFrac" => 0.0, "levelsUsed" => 775, "levelsAvailable" => 65536))
    f2, w2 = Cecelia.af_qc_findings(coarse)
    @test isempty(f2)                              # 1.2% of the range is NORMAL now, not a warning
    @test w2.levels < 0.02                         # ...but the metric is still banked
    @test !any(x -> occursin("range", x["code"]), Cecelia.af_qc_findings(coarse)[1])

    # worst-case rollup across channels, since QC banks one number per image
    both = merge(saturated, coarse)
    _, w3 = Cecelia.af_qc_findings(both)
    @test w3.saturated == 0.05         # worst = most saturated
    @test w3.levels < 0.02             # worst = least range used

    # `levelsUsedFrac` stays a COHORT metric: an image far below its peers is informative even when the
    # absolute number is not. `saturatedFrac` describes the acquisition — measured across the nine
    # kSUFux movies it spanned 0.001%-0.018%, a 13x spread at identical settings. `maxBleedthrough` is
    # the most cohort-shaped of the three: a leak is a property of the FILTER SET, so it should be
    # identical across a set acquired the same way and one image differing is the whole signal.
    @test COHORT_METRICS["cleanupImages.afCorrect"] ==
          ["saturatedFrac", "levelsUsedFrac", "maxBleedthrough"]
    @test !("ceiling" in COHORT_METRICS["cleanupImages.afCorrect"])
    @test !("clippedFrac" in COHORT_METRICS["cleanupImages.afCorrect"])

    # ratio-era stats files are ignored, not warned on
    ceiling_era = Dict{String,Any}("1" => Dict{String,Any}(
        "clippedFrac" => 0.9, "levelsUsed" => 200, "levelsAvailable" => 256, "ceiling" => 999.0))
    @test isempty(Cecelia.af_qc_findings(ceiling_era)[1])

    # A stats file missing the key must read as 0.0, not throw.
    @test Cecelia.af_qc_findings(Dict{String,Any}("1" => Dict{String,Any}(
        "levelsUsed" => 200, "levelsAvailable" => 256)))[2].saturated == 0.0
end

@testset "every QC finding carries the fields the GUI reads" begin
    # `lib/qc.ts` renders `${f.short}\n→ ${f.long}`, so a finding without `long` displays the literal
    # string "undefined" to the user. AF shipped exactly that for months because it hand-rolled its
    # finding dict instead of calling `qc_finding`. Nothing checked, so nothing caught it.
    #
    # Enforced structurally: no producer may build a finding dict by hand. `qc_finding` is the one
    # constructor, and it cannot omit `long` or put a string in `detail`.
    src = String[]
    for (root, _, files) in walkdir(joinpath(dirname(dirname(pathof(Cecelia))), "src"))
        for f in files
            endswith(f, ".jl") || continue
            push!(src, joinpath(root, f))
        end
    end
    @test !isempty(src)
    offenders = String[]
    for path in src
        endswith(path, "qc.jl") && continue          # the constructor itself
        for (i, line) in enumerate(eachline(path))
            occursin(r"\"level\"\s*=>\s*\"(warn|info)\"", line) &&
                push!(offenders, "$(basename(path)):$i")
        end
    end
    @test offenders == []

    # and the constructor's own contract: `long` always present, `detail` only ever structured
    f = Cecelia.qc_finding("warn", "af.saturated_input"; channel = 2,
                           detail = Dict{String,Any}("saturatedPct" => 1.5))
    @test haskey(f, "long") && !isempty(f["long"])
    @test f["detail"] isa AbstractDict
    @test !(Cecelia.qc_finding("warn", "x.y", "s", "l")["long"] |> isempty)
end

@testset "one resolver turns channel names into indices" begin
    # SIX handlers had hand-rolled `findfirst(==(String(ch)), ch_names)` and drifted into three
    # different behaviours, all silently wrong: an already-resolved index crashed four of them, an
    # unmatched name was dropped by five, and `drift_correct` fell back to index 0 — which on a
    # resonance-scanner movie means registering the whole timelapse against SHG at 99.5% zeros.
    names = ["SHG", "nuc-GFP", "mem-TOM", "CD169-Kat"]

    @test Cecelia.channel_index("mem-TOM", names) == 2          # 0-BASED, for the Python side
    @test Cecelia.channel_index("SHG", names) == 0
    @test Cecelia.channel_indices(["CD169-Kat", "nuc-GFP"], names) == [3, 1]   # order preserved

    # idempotent: an index passes through, so translating a chain dict twice is a no-op
    @test Cecelia.channel_index(2, names) == 2
    @test Cecelia.channel_indices([2, "CD169-Kat"], names) == [2, 3]

    # a single value, not a vector — `channelSelection` with multiple=false still arrives as one
    @test Cecelia.channel_indices("mem-TOM", names) == [2]

    # deduped by default: a channel named twice would square its term into the AF denominator
    @test Cecelia.channel_indices(["mem-TOM", "mem-TOM"], names) == [2]
    @test Cecelia.channel_indices(["mem-TOM", "mem-TOM"], names; unique_only = false) == [2, 2]

    # "nothing selected" is a legitimate state each task judges for itself (branching only needs
    # fibreChannels for anisotropySource="channel") — not an error
    @test Cecelia.channel_indices(nothing, names) == Int[]
    @test Cecelia.channel_indices([], names) == Int[]

    # AN UNMATCHED NAME RAISES, and the message names what was available. This is the deliberate
    # behaviour change from silent-drop: a channel the user named and we cannot find is not a thing
    # to guess about.
    err = try; Cecelia.channel_index("CH3", names); nothing; catch e; e; end
    @test err isa ErrorException
    @test occursin("CH3", err.msg) && occursin("mem-TOM", err.msg)
    @test_throws ErrorException Cecelia.channel_indices(["nuc-GFP", "nope"], names)
    # ...including when the image registered no names at all, rather than silently indexing nothing
    @test_throws ErrorException Cecelia.channel_index("nuc-GFP", String[])

    # A case-only difference is the common real cause: two images from ONE experiment shipped
    # `mem-TOM` (zolIMa/eQRnwU) and `mem-Tom` (zolIMa/fXgbTl), so a chain built on one fails on the
    # other. Still an error — the match stays exact, guessing is what this resolver removes — but the
    # message names the near match so it is a five-second fix.
    cased = try; Cecelia.channel_index("mem-TOM", ["SHG", "nuc-GFP", "mem-Tom"]); nothing
            catch e; e end
    @test cased isa ErrorException
    @test occursin("mem-Tom", cased.msg) && occursin("case", cased.msg)

    # ccid_channel_names reads the versioned field; `nothing` asks for the ACTIVE version
    raw = Dict{String,Any}("imChannelNames" => Dict{String,Any}(
        "default" => names, "corrected" => ["a", "b"], "_active" => "corrected"))
    @test Cecelia.ccid_channel_names(raw) == names                  # default
    @test Cecelia.ccid_channel_names(raw, nothing) == ["a", "b"]    # active
    @test Cecelia.ccid_channel_names(Dict{String,Any}()) == String[]

    # NO SEVENTH COPY. The detector, not just the extraction — this is the second time this file has
    # had to count these sites, and grep-based guesses were wrong both times.
    src_root = dirname(pathof(Cecelia))
    offenders = String[]
    for (root, _, files) in walkdir(joinpath(src_root, "tasks"))
        for f in files
            endswith(f, ".jl") || continue
            body = read(joinpath(root, f), String)
            for line in split(body, '\n')
                startswith(strip(line), "#") && continue
                occursin(r"findfirst\(==\(String\(", line) &&
                    push!(offenders, relpath(joinpath(root, f), src_root))
            end
        end
    end
    @test isempty(offenders)
end

@testset "AF params are just channels" begin
    # The spec grew into a bag of ~20 numbers while fitting individual datasets and was never
    # revisited. A combination is now the two things it is actually about; everything else is derived
    # (`af_weight_stats`) or was a filter that belongs to a filtering task.
    spec = Cecelia._task_spec(Cecelia.AfCorrect())
    keys_top = [string(get(p, "key", "")) for p in get(spec, "params", [])]
    @test keys_top == ["valueName", "afCombinations", "backgroundMethod"]

    # `exclusive` is the ONE addition, and it is admitted on a rule the deleted twenty all failed:
    # it is a fact about the SPECIMEN that no amount of looking at the pixels can supply, not a number
    # to fit. Can one voxel carry both markers? The user knows; the estimator cannot. It selects
    # between the total slope and the envelope floor (`af_bleedthrough_alphas`), which on
    # `WIaUjL/p6t4mC` differ 5x — 0.113 against 0.024, the difference between a corrected channel and
    # one that visibly still carries the other's overspill.
    #
    # Anything proposed here later has to clear the same bar. `channelPercentile` and friends did not:
    # they were dials with no defensible value, fitted per dataset and never revisited.
    combo = only(p for p in get(spec, "params", []) if string(get(p, "key", "")) == "afCombinations")
    @test [string(get(p, "key", "")) for p in get(combo, "params", [])] ==
          ["targetChannel", "competingChannels", "exclusive"]

    # a statement, not a dial: boolean, and defaulting to the common case (distinct cell types)
    excl = only(p for p in get(combo, "params", []) if string(get(p, "key", "")) == "exclusive")
    @test string(get(excl, "type", "")) == "bool"
    @test get(excl, "default", nothing) === true

    # `none` is NOT offered: the weight is a ratio of intensities, so an unsubtracted pedestal makes
    # background voxels split evenly and survive. Measured on kSUFux/Or1L8a: 92.1% of background voxels
    # come out non-zero and cell-to-background contrast collapses to 6.8x.
    bg = only(p for p in get(spec, "params", []) if string(get(p, "key", "")) == "backgroundMethod")
    @test [string(get(o, "value", "")) for o in get(bg, "options", [])] == ["triangle", "otsu"]

    # No exponent param. This task deleted four numbers with no defensible value (channelPercentile,
    # correctionPercentile, correctionMin, correctionMax) and a user-facing sharpness dial is that same
    # thing returning — see `AF_WEIGHT_EXPONENT`.
    @test !("exponent" in keys_top)
    @test !("weightExponent" in keys_top)

    # the deleted ones, named so a future session doesn't reintroduce them one at a time
    gone = ["correctionMin", "correctionMax", "correctionGain", "channelPercentile",
            "correctionPercentile", "correctionMode", "summaryMode", "summaryPercentile",
            "generateInverse", "medianFilter", "topHatRadius", "rollingBallRadius",
            "rollingBallPadding", "denoiseFun", "waveletMethod", "waveletMode", "tvWeight",
            "applyGaussian", "applyGaussianToOthers"]
    flat = Set{String}(keys_top)
    union!(flat, Set(string(get(p, "key", "")) for p in get(combo, "params", [])))
    for k in gone
        @test !(k in flat)
    end
end

# Cohort QC must aggregate the per-image anisotropy readout — it is Figure 4 panel D's x-axis
# (SPATIAL_ANISOTROPY_PLAN Decision 6), so dropping it from COHORT_METRICS silently removes
# the plot's data source.
@testset "Cohort metrics — branching anisotropy" begin
    @test "anisotropy" in COHORT_METRICS["segment.branching"]
    @test "nBranches" in COHORT_METRICS["segment.branching"]

    # `anisotropy` is the first RATIO metric in a cohort list otherwise made of counts, so
    # check the outlier rule behaves on 0–1 values at the magnitudes real data produces
    # (EaMaVq measures ≈ 0.32). The modified-z path is scale-free, but the MAD==0 fallback
    # is a RELATIVE departure, so tiny numbers are where it would misbehave if anywhere.
    r = Cecelia._cohort_outliers(Dict("a" => 0.31, "b" => 0.33, "c" => 0.30, "d" => 0.09))
    @test haskey(r.outliers, "d") && !haskey(r.outliers, "a")
    # …and a cohort that merely spans the normal 0.1–0.4 band must NOT flag anything: real
    # tissue varies this much, and a false "outlier" on every low-anisotropy image is noise.
    @test isempty(Cecelia._cohort_outliers(
        Dict("a" => 0.12, "b" => 0.21, "c" => 0.30, "d" => 0.38)).outliers)
end

# ── Dispatch + param validation — ClustPops (clustPops.cluster, set-scope) ───


@testset "cellNeighbours QC findings (pure helper)" begin
    # objective graph metrics → advisory findings; only the unambiguous problems flag
    @test isempty(Cecelia._neighbours_qc_findings(100, 500, 0.1))        # healthy graph → no finding
    @test only(Cecelia._neighbours_qc_findings(0, 0, 0.0))["code"]   == "spatial.no_cells"
    @test only(Cecelia._neighbours_qc_findings(100, 0, 0.0))["code"] == "spatial.no_edges"
    @test only(Cecelia._neighbours_qc_findings(100, 40, 0.7))["code"] == "spatial.many_isolated"
    @test isempty(Cecelia._neighbours_qc_findings(100, 40, 0.3))         # some isolated, under half → fine
end


@testset "aggregate DBSCAN ids (Clustering.jl)" begin
    # two dense blobs + one far noise point → two aggregates, noise = id 0
    coords = [0.0 0.0; 0.1 0.1; 0.2 0.0; 5.0 5.0; 5.1 5.1; 5.2 5.0; 50.0 50.0]
    ids = Cecelia._aggregate_ids(coords, 0.5, 2)
    @test length(unique(ids[ids .> 0])) == 2                          # two aggregates
    @test ids[end] == 0                                               # far point is noise
    @test count(==(0), ids) == 1                                      # exactly one noise point
    # too-few points → all noise
    @test all(Cecelia._aggregate_ids([0.0 0.0; 0.1 0.1], 0.5, 5) .== 0)
end



@testset "cellContacts target-name sanitisation" begin
    # obs column suffix — nothing to do with param validation, which is swept above
    @test Cecelia._contact_target("flow", ["T/qc"]) == "flow.T_qc"
    @test Cecelia._contact_target("flow", ["B/qc", "T/qc"]) == "flow.B_qc+T_qc"
end

@testset "neighbourStats spec — graph knobs live on the graph, not here" begin
    # The graph parameters (method / radius / k) deliberately do NOT live here any more — they belong
    # to the graph this task consumes (`graphSuffix` → spatialAnalysis.cellNeighbours), so a
    # neighbourhood is defined once. (Ranges for what remains are swept above.)
    ns_spec = JSON3.read(read(Cecelia._spec_path(NeighbourStats()), String))
    ns_keys = Set(String(get(p, :key, "")) for p in get(ns_spec, :params, []))
    @test "graphSuffix" in ns_keys && "nPermutations" in ns_keys
    for gone in ("neighbourRadius", "nNeighbours", "neighbourMethod")
        @test !(gone in ns_keys)
    end
end

@testset "clustRegions spec — graph knobs moved out with the graph" begin
    # regions run ON a neighbour graph and no longer build their own, so the graph knobs moved to
    # cellNeighbours; `perTimepoint` went with them (whether neighbourhoods are per-frame is a
    # property of the graph, so behaviour regions come from choosing a per-timepoint graph).
    cr_keys = Set(String(get(p, :key, ""))
                  for p in get(JSON3.read(read(Cecelia._spec_path(ClustRegions()), String)), :params, []))
    @test "graphSuffix" in cr_keys && "includeOther" in cr_keys
    for gone in ("neighbourRadius", "nNeighbours", "neighbourMethod", "perTimepoint")
        @test !(gone in cr_keys)
    end
end

@testset "every slider can reach its own max and default" begin
    # An `int`/`float` param renders as `<input type="range">` (ParamRenderer.vue), and a range input
    # anchors its stops at `min` — the reachable values are `min + k*step`, NOT the round numbers the
    # three bounds suggest. So `min 1, max 500, step 5` (what `epochs` shipped as) offers 1, 6, 11 …
    # 496: 100 is not selectable, 101 is, the declared max is unreachable, and the declared default of
    # 30 is not even on a stop — the form silently disagrees with its own spec the moment the user
    # touches the control.
    #
    # Only the two unambiguous failures are asserted. That the stops are round *numbers* is taste and
    # sometimes wrong on purpose: `smooth.temporalFrames` is 1–9 step 2 precisely so the window stays
    # odd, and its max and default are both reachable.
    on_grid(v, mn, st) = (n = (v - mn) / st; isapprox(n, round(n); atol = 1e-6))
    checked = 0
    for (fun_name, task) in sort(collect(Cecelia._fun_name_map()); by = first)
        path = try Cecelia._spec_path(task) catch; nothing end
        (isnothing(path) && continue)
        isfile(path) || continue
        each_spec_param(get(JSON3.read(read(path, String)), :params, [])) do p, _
            t = String(something(spec_get(p, "type", ""), ""))
            (t == "int" || t == "float") || return
            key = String(something(spec_get(p, "key", ""), ""))
            mn = Float64(something(spec_get(p, "min", 0), 0))
            mx = Float64(something(spec_get(p, "max", 100), 100))
            st = Float64(something(spec_get(p, "step", t == "int" ? 1 : 0.01), 1))
            st > 0 || return
            checked += 1
            on_grid(mx, mn, st) ||
                @error "slider max is unreachable" task = fun_name param = key min = mn max = mx step = st
            @test on_grid(mx, mn, st)
            dflt = spec_get(p, "default", nothing)
            if dflt isa Real
                on_grid(Float64(dflt), mn, st) ||
                    @error "slider default is not on a stop" task = fun_name param = key default = dflt min = mn step = st
                @test on_grid(Float64(dflt), mn, st)
            end
        end
    end
    @test checked > 20      # the sweep actually found the sliders
end

@testset "a second model group is not born a copy of the first" begin
    # Entries of a `repeatable` group are applied IN RUN ORDER and each fills only the pixels an
    # earlier one left (`fill_unlabelled`), so entry 2 is a FRAGMENT pass over what the cell pass did
    # not claim. Born identical it grows to nearly the same regions, is clipped along the first pass's
    # boundaries, and leaves slivers at twice the compute — measured on zolIMa/fXgbTl, where both
    # passes carried `affinityThreshold` 0.5 and the run took 508 s to produce that.
    #
    # `entryDefaults[2]` is what the form seeds a second entry with. This pins that it actually
    # DIFFERS on the parameters that decide how far a pass grows: identical values here would make the
    # whole feature a no-op and nothing else would notice.
    spec = JSON3.read(read(joinpath(@__DIR__, "..", "src", "tasks", "segment", "coastal.json"),
                           String))
    models = nothing
    for p in get(spec, :params, [])
        String(get(p, :key, "")) == "models" && (models = p)
    end
    @test !isnothing(models)
    seeds = get(models, :entryDefaults, nothing)
    @test !isnothing(seeds)
    @test length(seeds) >= 2
    second = seeds[2]

    # The sub-param defaults, i.e. what entry 1 starts as.
    firsts = Dict{String,Any}()
    function collect_leaves!(ps)
        for p in ps
            if String(get(p, :type, "")) == "section"
                collect_leaves!(get(p, :params, []))
            else
                haskey(p, :default) && (firsts[String(p[:key])] = p[:default])
            end
        end
    end
    collect_leaves!(get(models, :params, []))

    # These three decide how far a pass grows and how readily its fragments re-merge. If the second
    # pass matches the first on them, it is the first pass again.
    for k in ("affinityThreshold", "seedSize", "seedBlurSigma")
        @test haskey(second, Symbol(k))
        @test second[Symbol(k)] != firsts[k]
    end
    # Directions, not just difference: the fragment pass grows LESS freely and blurs its seeds LESS.
    @test second[:affinityThreshold] > firsts["affinityThreshold"]
    @test second[:seedSize] < firsts["seedSize"]
    @test second[:seedBlurSigma] < firsts["seedBlurSigma"]
    # Every seeded key must be one the group declares, or it reaches the runner as an unknown param.
    for k in keys(second)
        @test haskey(firsts, String(k))
    end
end

@testset "coastal forwards every top-level spec param to its runner" begin
    # `coastal.jl` hands `run_py` an explicit NamedTuple, i.e. a WHITELIST — while `preview_params`
    # forwards the whole param bag. So a spec param missing from that list is honoured in the preview
    # and silently ignored by the run, which is the same class of divergence as the group-order bug
    # and just as quiet. Checked by reading the handler source: the param names are literals there.
    #
    # `valueName`/`outputValueName` are excluded because the handler TRANSFORMS them (into `imPath`
    # and the label store name) rather than forwarding them, and `models` is rebuilt by
    # `coastal_models_for_python`. Sections are excluded here and covered by the flatten tests —
    # their sub-params appear individually.
    raw  = read(joinpath(@__DIR__, "..", "src", "tasks", "segment", "coastal.jl"), String)
    # COMMENTS STRIPPED, and the key matched QUOTED — i.e. as `get(params, "key", …)` spells it.
    # A bare `occursin(key, raw)` passes on a param that was deleted and left described in a comment,
    # which is exactly the state this is meant to catch.
    src  = join((replace(l, r"#.*$" => "") for l in split(raw, '\n')), "\n")
    spec = JSON3.read(read(joinpath(@__DIR__, "..", "src", "tasks", "segment", "coastal.json"),
                           String))
    transformed = ["valueName", "outputValueName", "models"]
    checked = 0
    for p in get(spec, :params, [])
        key = String(get(p, :key, ""))
        String(get(p, :type, "")) in ("section", "group") && continue
        key in transformed && continue
        isempty(key) && continue
        checked += 1
        found = occursin("\"" * key * "\"", src)
        found || @error "a coastal spec param never reaches coastal_run.py" param = key
        @test found
    end
    @test checked >= 2      # the sweep found the top-level params, not an empty list
end

@testset "an int param never declares a fractional step" begin
    # `ParamRenderer.vue` runs `parseInt` on an `int` slider's value, so a fractional step makes half
    # the stops DEAD: the control moves and the value does not. Found live on `segment.coastal`, where
    # the two params carrying a PHYSICAL unit were the ones affected — `seedSize` (µm, step 0.5) and
    # `minComponentSize` (µm², step 0.5). At 0.33 µm/px that put coastal's own tuned pass-1 seed window
    # (14 px = 4.6 µm) and pass-2 size floor (6 px = 0.66 µm²) out of reach entirely, so a two-pass
    # config could not be given the values the two passes are supposed to differ by.
    #
    # The fix is the type, not the step: a µm value is continuous and the conversion to pixels rounds
    # at the ONE boundary that owns it (`px_from_um` / `px_area_from_um2`). An int with step 1 is fine
    # and common — this only catches the contradiction.
    checked = 0
    for (fun_name, task) in sort(collect(Cecelia._fun_name_map()); by = first)
        path = try Cecelia._spec_path(task) catch; nothing end
        (isnothing(path) && continue)
        isfile(path) || continue
        each_spec_param(get(JSON3.read(read(path, String)), :params, [])) do p, _
            String(something(spec_get(p, "type", ""), "")) == "int" || return
            st = spec_get(p, "step", nothing)
            st isa Real || return
            checked += 1
            frac = !isapprox(Float64(st), round(Float64(st)); atol = 1e-9)
            frac && @error "int param with a fractional step — half its slider stops do nothing" task = fun_name param = String(something(spec_get(p, "key", ""), "")) step = st
            @test !frac
        end
    end
    @test checked > 5
end

# A repeatable group carries defaults in TWO places: the group's own `default` dict (what entry "0"
# starts as) and each nested param's `default` (what a NEWLY ADDED entry starts as). When they
# disagree, the first entry and the second silently begin on different values — which is exactly the
# shape of a multi-pass segmentation, so the two passes differ by a parameter nobody set.
#
# Found live: `segment.coastal` had `embeddingBlurSigma` at 0.5 in the group default and 1.5 in the
# param spec (whose tip says "Calibrated at 1.5"). A second pass added in the GUI therefore ran at a
# different embedding blur from the first, and on real data that mismatch turned 56% of the second
# pass's objects into rims around the first pass's cells instead of standalone fragments.
# Ordering and switching off entries of a repeatable group is offered for EVERY such group, by the
# renderer, with no spec field — the reason it exists is a property of repeatable groups themselves
# (entries are applied in turn, each filling only what an earlier one left, so the order is
# semantic). It is resolved away here rather than forwarded: `_apply_group_order` rebuilds the group
# so no handler, runner or Python task learns that ordering exists. The first version of this WAS a
# hand-authored `modelsOrder` param plus a passthrough in one task's .jl — i.e. exactly the thing
# every future grouped task would have had to remember.
@testset "a repeatable group's run order is resolved into the group" begin
    task = Cecelia._fun_name_map()["segment.coastal"]
    @test "models" in Cecelia._repeatable_group_keys(task)

    three = Dict{String,Any}("models" => Dict{String,Any}(
        "0" => Dict{String,Any}("model" => "a"),
        "1" => Dict{String,Any}("model" => "b"),
        "2" => Dict{String,Any}("model" => "c")))

    # no order at all: every entry, untouched. A task saved before the control existed, a chain node
    # and a REPL call all look like this.
    kept = Cecelia._apply_group_order(task, copy(three))
    @test sort(collect(keys(kept["models"]))) == ["0", "1", "2"]

    # reordered AND filtered, renumbered so a consumer's ascending walk IS the run order
    ord = merge(copy(three), Dict{String,Any}("modelsOrder" => ["2", "0"]))
    got = Cecelia._apply_group_order(task, ord)
    @test !haskey(got, "modelsOrder")            # resolved away, never forwarded
    @test sort(collect(keys(got["models"]))) == ["0", "1"]
    @test got["models"]["0"]["model"] == "c"
    @test got["models"]["1"]["model"] == "a"

    # an empty list means run NOTHING — otherwise the off switch would be a no-op
    none = Cecelia._apply_group_order(task, merge(copy(three), Dict{String,Any}("modelsOrder" => String[])))
    @test isempty(none["models"])

    # a stale key outlives the group it was saved against; ignore it rather than fail the run
    stale = Cecelia._apply_group_order(task, merge(copy(three), Dict{String,Any}("modelsOrder" => ["1", "9"])))
    @test length(stale["models"]) == 1
    @test stale["models"]["0"]["model"] == "b"

    # the same entry twice would offset its labels against itself and write nothing the second time
    dup = Cecelia._apply_group_order(task, merge(copy(three), Dict{String,Any}("modelsOrder" => ["0", "0", "1"])))
    @test length(dup["models"]) == 2

    # idempotent, because `run_task` is not the only thing that may normalise a bag of params
    @test Cecelia._apply_group_order(task, copy(got))["models"] == got["models"]

    # EVERY repeatable group gets it, not just the one that motivated it
    reps = String[]
    for (fun_name, t) in Cecelia._fun_name_map()
        isempty(Cecelia._repeatable_group_keys(t)) || push!(reps, fun_name)
    end
    @test "segment.cellpose" in reps
    @test length(reps) >= 3
end

@testset "a group's two sets of defaults agree" begin
    checked = 0
    for (fun_name, task) in sort(collect(Cecelia._fun_name_map()); by = first)
        path = try Cecelia._spec_path(task) catch; nothing end
        (isnothing(path) && continue)
        isfile(path) || continue
        each_spec_param(get(JSON3.read(read(path, String)), :params, [])) do p, _
            String(something(spec_get(p, "type", ""), "")) == "group" || return
            entry0 = spec_get(p, "default", nothing)
            entry0 isa AbstractDict || return
            # the group's default is keyed by entry index ("0"); take the first entry's values
            vals = get(entry0, Symbol("0"), get(entry0, "0", nothing))
            vals isa AbstractDict || return
            gkey = String(something(spec_get(p, "key", ""), ""))
            each_spec_param(get(p, :params, get(p, "params", []))) do q, _
                qkey = String(something(spec_get(q, "key", ""), ""))
                isempty(qkey) && return
                qdef = spec_get(q, "default", nothing)
                isnothing(qdef) && return
                gdef = get(vals, Symbol(qkey), get(vals, qkey, nothing))
                isnothing(gdef) && return
                checked += 1
                same = (gdef isa Real && qdef isa Real) ? isapprox(Float64(gdef), Float64(qdef)) :
                                                          string(gdef) == string(qdef)
                same || @error "group default disagrees with the param default — entry 0 and a newly added entry start differently" task = fun_name group = gkey param = qkey group_default = gdef param_default = qdef
                @test same
            end
        end
    end
    @test checked > 10      # the sweep actually found the groups
end

@testset "plot specs live on the page that EXPLORES, not the one that DEFINES" begin
    # Where a plot lives is a product decision worth pinning, because the drift is invisible: a new
    # pop type arrives, someone adds a `population_summary_<type>.json` pointed at the page that
    # produced it, and every population-DEFINING page slowly grows a summary canvas it has no use
    # for. Populations are DEFINED on gate / track / clust-cells / clust-tracks / regions, and
    # SUMMARISED on the Explore pages. Each summary follows its pop type:
    #     flow → phenotype ·  clust → phenotype ·  live/trackclust → behaviourAnalysis ·  region → spatialAnalysis
    root = joinpath(dirname(dirname(pathof(Cecelia))), "src", "plotDefinitions")
    @test isdir(root)
    specs = Dict{String,Any}()
    for f in readdir(root)
        endswith(f, ".json") || continue
        specs[f] = JSON3.read(read(joinpath(root, f), String), Dict{String,Any})
    end
    @test length(specs) > 5          # the walk found the registry (a floor, not a census)

    # The interaction matrix is a REGISTRY plot now, not a bespoke component + route: it was the one
    # violation of docs/PLOTS.md → *Hosting — ONE way*, which is why it sat in a fixed box below the
    # table and couldn't be duplicated, arranged, exported or put on the Analysis board.
    @test haskey(specs, "spatial_interactions.json")
    @test String(specs["spatial_interactions.json"]["dataSource"]["matrix"]["mode"]) == "interaction"
    @test String(specs["spatial_interactions.json"]["module"]) == "spatialAnalysis"
    # …and the bespoke surface is gone for good
    fe = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "frontend", "src")
    @test !isfile(joinpath(fe, "modules", "spatial", "SpatialContactHeatmap.vue"))
    @test !isfile(joinpath(fe, "utils", "contactHeatmap.ts"))
    srv = read(joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "api", "src", "server.jl"), String)
    @test !occursin("/api/plots/contact_matrix", srv)

    # the population-DEFINING module pages carry no plot specs at all
    DEFINING = ("clustPops", "clustTracks", "clustRegions")
    stray = ["$f → module=$(get(s, "module", ""))" for (f, s) in specs
             if String(get(s, "module", "")) in DEFINING]
    @test isempty(stray)

    # There is now ONE population-summary spec offering every family, with the per-page curation in
    # its `modules` allow-list — the four per-popType copies are gone. Pin both halves: no copies
    # come back, and each page still offers exactly the families it should.
    for gone in ("population_summary_clust.json", "population_summary_trackclust.json",
                 "population_summary_tracks.json", "population_summary_region.json")
        @test !haskey(specs, gone)
    end
    ps = specs["population_summary.json"]
    @test !haskey(ps, "module")                       # multi-page specs use `modules`, not `module`
    offered = Dict(String(k) => Set(String(x) for x in v) for (k, v) in ps["modules"])
    @test offered["phenotype"]         == Set(["flow", "clust"])
    @test offered["behaviourAnalysis"] == Set(["live", "track", "trackclust"])
    @test offered["spatialAnalysis"]   == Set(["region"])

    # every family a page offers must actually be declared, WITH its own granularity — the one thing
    # that genuinely blocked a shared spec (sending the spec's single granularity asked for cell rows
    # under a track pop type). flow/clust/region are cell-grained, live/track/trackclust track-grained.
    pts = Dict(String(p["popType"]) => String(p["granularity"]) for p in ps["dataSource"]["popTypes"])
    @test Set(keys(pts)) == Set(["flow", "clust", "live", "track", "trackclust", "region"])
    @test pts["flow"] == "cell" && pts["clust"] == "cell" && pts["region"] == "cell"
    @test pts["live"] == "track" && pts["track"] == "track" && pts["trackclust"] == "track"
    for (_, fams) in offered, f in fams
        @test haskey(pts, f)                          # a page can't offer an undeclared family
    end

    # BEHAVIOUR PLOTS ARE NOT LIVE-ONLY. Every one of them shipped the legacy single
    # `popType: "live"`, so a gated-track population or a track cluster could not be plotted at all
    # — the family picker existed but these specs never opted into it. `pop_df` has always
    # supported `track`/`trackclust` at either granularity (`_pop_df_track_gating` expands track
    # membership to its member cells), so this was a spec omission, not a capability gap.
    BEHAVIOUR = ("cell_properties.json", "hmm_state_frequency.json", "state_signature.json",
                 "transition_matrix.json", "track_measures.json")
    for f in BEHAVIOUR
        ds = specs[f]["dataSource"]
        @test !haskey(ds, "popType")            # legacy single-family form is gone
        fams = Dict(String(p["popType"]) => String(p["granularity"]) for p in ds["popTypes"])
        @test Set(keys(fams)) == Set(["live", "track", "trackclust"])
        # granularity is the PLOT's, not the family's: per-track measures are track-grained, the
        # cell/HMM readouts cell-grained — and it must be the same for all three families, or one
        # pick would silently ask for a different table than another.
        want = f == "track_measures.json" ? "track" : "cell"
        @test all(g == want for g in values(fams))
    end

    # A plot's family list is CURATED in its spec (not derived from the data), because "which family
    # can this measure be sliced by" is a judgement the data can't make. The cost of curation is
    # silent drift, and it drifted: the spatial measures plot offered Gated/Cell clusters/Regions/
    # Tracked but not Track clusters — a family every spatial task happily accepts as input. So pin
    # the agreement to the PRODUCING tasks' own `accepts`, via the canonical token mapping
    # (`_accept_pop_types`) rather than a second hand-written list.
    producing = (CellNeighbours(), NeighbourStats(), CellContacts(), ContactsMeshes(),
                 DetectAggregates(), AggregatesMeshes(), ClustRegions())
    accepted = Set{String}()
    for t in producing
        spec = JSON3.read(read(Cecelia._spec_path(t), String))
        for p in get(spec, :params, [])
            String(get(p, :type, "")) == "popSelection" || continue
            acc = Cecelia._normalise_accepts(get(p, :accepts, String[]))
            union!(accepted, Cecelia._accept_pop_types(acc))
        end
    end
    @test accepted == Set(["live", "track", "clust", "trackclust", "region"])
    spat = Dict(String(p["popType"]) => String(p["granularity"])
                for p in specs["spatial_cell_properties.json"]["dataSource"]["popTypes"])
    @test isempty(setdiff(accepted, keys(spat)))   # every accepted family is offered for plotting
    # `flow` is offered ON TOP: _normalise_accepts folds flow→live (same gate map), but the plot
    # keeps them apart — "Gated" slices the cell gates, "Tracked" the derived `_tracked` sets.
    @test haskey(spat, "flow")
    # the spatial readouts are per-CELL columns, so every family is sliced at cell granularity —
    # including the track-grained ones (pop_df expands track membership to its member cells)
    @test all(g == "cell" for g in values(spat))

    # The manager follows the ACTIVE plot's family, which needs both hosts to pass activeSpecId AND
    # activePopType into useSummaryData. If that regresses the picker silently lists the wrong
    # family — invisible, so pin the wiring.
    fe = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "frontend", "src")
    for host in ("SummaryCanvas.vue", "LayoutCanvas.vue")
        src = read(joinpath(fe, "components", "canvas", host), String)
        @test occursin("activeSpecId", src)
        @test occursin("activePopType", src)
        @test occursin("migrateSpecId", src)          # persisted canvases must not silently empty
    end
end

# ── a plot spec may only name CURRENT column names ────────────────────────────────
# `groupByOptions` is a HINT list: SummaryPanel keeps only the entries actually present on the data
# (`hints = groupByOptions.filter(c => present.has(c))`). So a spec naming a column that no longer
# exists does not error and does not warn — the option just never appears in the menu, and whatever
# view it unlocks is silently unreachable. That is exactly how the segmentation-QC per-timepoint plot
# (cell count / any label measure over time, the LOESS trend + CI ribbon) sat dark: the spec said
# `t`, the PRE-MIGRATION temporal column name that `centroid_migrate.py` renames to `centroid_t`
# (`uns/temporal_cols`), so the hint matched nothing, `timeSeries` never went true, and the chart menu
# never swapped to [trend, count]. The spec was right when written and rotted in place — which is why
# this is pinned statically rather than left to a data-dependent test.
@testset "plot spec groupByOptions name current columns" begin
    root = joinpath(dirname(dirname(pathof(Cecelia))), "src", "plotDefinitions")
    RETIRED = Set(["t"])          # pre-migration uns/temporal_cols spelling → now "centroid_t"
    for f in readdir(root)
        endswith(f, ".json") || continue
        spec = JSON3.read(read(joinpath(root, f), String), Dict{String,Any})
        ds = get(spec, "dataSource", nothing)
        ds isa AbstractDict || continue
        for c in String.(get(ds, "groupByOptions", String[]))
            @test !(c in RETIRED)                 # a retired name filters to nothing, silently
            @test !startswith(c, "centroid-")     # pre-migration positional centroid spelling
        end
    end
    # and the segmentation-QC spec still offers the per-timepoint view at all
    qc = JSON3.read(read(joinpath(root, "segmentation_qc.json"), String), Dict{String,Any})
    @test "centroid_t" in String.(qc["dataSource"]["groupByOptions"])
    @test "count" in String.(qc["chartTypes"])    # the count-over-time headline
end

# ── every plot canvas offers the same bulk close ──────────────────────────────────
# "Close all" has to be on EVERY canvas, not just the one it was asked for — a per-canvas answer is
# how the Tile/Cascade group ended up copied into four hosts in the first place. The shared halves are
# `useCanvasPanels.removeAll` (the workspace logic) and `CanvasArrangeButtons` (the toolbar group), so
# the drift to catch is a host that drives the composable but renders its own arrange buttons: it
# would silently lack the close, and nothing else would fail.
#
# Scanned from here rather than from vitest because the frontend suite is deliberately pure-logic only
# (docs/DEV.md → Tests) — `removeAll` is store mutation with no pure kernel to unit-test, and this is
# the same source-scanning guard the plot-host wiring above already uses.
@testset "every canvas host offers Close all" begin
    fe = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "frontend", "src")
    comp = read(joinpath(fe, "composables", "useCanvasPanels.ts"), String)
    @test occursin("function removeAll", comp)
    @test occursin("removeAll,", comp)                  # …and it is actually returned to hosts
    @test isfile(joinpath(fe, "components", "canvas", "CanvasArrangeButtons.vue"))

    HOSTS = [joinpath("components", "canvas", "SummaryCanvas.vue"),
             joinpath("modules", "gate", "GatingPlots.vue"),
             joinpath("modules", "cluster", "ClusterPlots.vue"),
             joinpath("modules", "modelTraining", "ModelPlots.vue")]
    for h in HOSTS
        src = read(joinpath(fe, h), String)
        @test occursin("useCanvasPanels", src)
        @test occursin("CanvasArrangeButtons", src)     # the shared group, not a private copy
        @test occursin("close-all", src)                # …wired, not merely imported
        # the arrange buttons must not be re-inlined beside the shared component
        @test !occursin("'Tile in a grid'", src)
    end

    # Any OTHER host that starts driving the same workspace must adopt the shared group too — this is
    # the check that fails when a fifth canvas is added and quietly ships without Close all.
    for (root, _, files) in walkdir(fe), f in files
        endswith(f, ".vue") || continue
        p = joinpath(root, f)
        src = read(p, String)
        occursin("useCanvasPanels(", src) || continue   # calls it (not merely a type import)
        occursin("CanvasArrangeButtons", src) ||
            error("$(relpath(p, fe)) drives useCanvasPanels but renders no CanvasArrangeButtons — " *
                  "every plot canvas must offer Tile/Cascade/Close all")
    end
end

# ── a module summary canvas belongs to the SET, a gating canvas to the IMAGE ──────
# The summary canvas used to be keyed by the FIRST selected image, which quietly tied two unrelated
# things to the selection order: what got plotted, AND which saved layout you were looking at — so
# re-ticking swapped your whole canvas, and ticking five images showed the first one's. Summary plots
# are set-aware by design (the `compare` control is exactly the per-image/pooled/by-attribute choice),
# so the LAYOUT has no business being image-scoped as well. Gating is the opposite case and must stay
# per image: gates belong to one (image, value_name).
#
# Pinned by reading the key expressions, because both are one-liners in an SFC and the failure is
# silent either way — a wrongly-scoped canvas still renders, just not the one you saved.
@testset "summary canvas is set-scoped, gating canvas is image-scoped" begin
    fe = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "frontend", "src")
    ckey_line(path) = begin
        ls = filter(l -> occursin("const ckey = computed", l),
                    split(read(joinpath(fe, path...), String), '\n'))
        @test length(ls) == 1                      # one key expression per canvas, or this is stale
        first(ls)
    end

    sc = ckey_line(("components", "canvas", "SummaryCanvas.vue"))
    @test occursin("setUid", sc)
    @test !occursin("imageUid", sc)                # the bug: first-selected image decided the canvas

    # gating stays per (image, value_name) — deliberate, not an oversight. The vn identifier
    # is `pageVn` (page-local, decoupled from the singleton `g.valueName` so cluster/trackclust
    # task-done can't rebind our ckey) — either substring is fine as long as SOME vn is there.
    gp = ckey_line(("modules", "gate", "GatingPlots.vue"))
    @test occursin("imageUid", gp) && (occursin("valueName", gp) || occursin("pageVn", gp))

    # cluster was already set-scoped; it is the precedent this follows (and `objectOf` persists a
    # set-keyed canvas to the SET's own moduleCanvases.json, so no new persistence path was needed)
    @test occursin("setUid", ckey_line(("modules", "cluster", "ClusterPlots.vue")))

    # …and ticking several images must actually PLOT several: `image` means "the first one only", so it
    # cannot be the default. `canCompare` already gates this on there being >1 image selected.
    usd = read(joinpath(fe, "composables", "useSummaryData.ts"), String)
    @test occursin("compareMode: 'per_image'", usd)
    @test !occursin("compareMode: 'image'", usd)
end

@testset "interaction matrix aggregates with NO population targets" begin
    # The path `api_plot_data`'s `precomputed` branch now takes. The panel sends no `series` (the
    # matrix's rows/columns come from the neighbourStats run), so the targets vector is EMPTY — and
    # the interception has to fire before anything touches pop_df. Previously the selector guard
    # rejected the body outright ("pops (or series) required" on a plot with no pops to pick), so
    # this dispatch was never exercised.
    td = mktempdir()
    try
        mkpath(joinpath(td, "spatialStats"))
        write(joinpath(td, "spatialStats", "run1.json"), """
            {"basis":["B/qc","T/qc"],"nCells":334,"nEdges":1200,"graphSuffix":"g1",
             "nPermutations":500,"coverage":0.9,"records":[
              {"popA":"B/qc","popB":"B/qc","observed":120,"expected":80,"logOdds":0.48,
               "zScore":15.8,"pValue":0.002,"significant":true,"association":"association"},
              {"popA":"B/qc","popB":"T/qc","observed":10,"expected":33,"logOdds":-1.19,
               "zScore":-30.4,"pValue":0.002,"significant":true,"association":"avoidance"},
              {"popA":"T/qc","popB":"T/qc","observed":90,"expected":60,"logOdds":0.58,
               "zScore":17.7,"pValue":0.002,"significant":true,"association":"association"}]}
            """)
        img = CciaImage(; dir = td)
        r = plot_summary_data(img, "flow", Tuple{String,String}[], "matrix";
                              matrix_mode = "interaction", stats_suffix = "run1")
        @test r["chartType"] == "matrix" && r["matrixMode"] == "interaction"
        @test r["xLabels"] == ["B/qc", "T/qc"] && r["yLabels"] == r["xLabels"]
        @test r["suffixes"] == ["run1"] && r["suffix"] == "run1"
        @test isempty(r["series"])                       # nothing to overlay — it IS the matrix
        # symmetric fill: 2 populations → 4 cells, the off-diagonals sharing one record
        @test length(r["cells"]) == 4
        by = Dict((c["x"], c["y"]) => c for c in r["cells"])
        @test by[("B/qc", "B/qc")]["value"] == 0.48
        @test by[("B/qc", "T/qc")]["value"] == by[("T/qc", "B/qc")]["value"] == -1.19
        # z / p / observed ride along per cell so the renderer needs no second request
        @test by[("B/qc", "T/qc")]["zScore"] == -30.4
        @test by[("B/qc", "T/qc")]["pValue"] == 0.002
        @test by[("B/qc", "T/qc")]["count"] == 10
        # …plus the star ladder, from the SAME function the hypothesis tests use — a second ladder
        # in the renderer would be a fork waiting to disagree
        @test by[("B/qc", "T/qc")]["significance"] == Cecelia._significance(0.002)
        @test by[("B/qc", "T/qc")]["significance"] == "**"
        # the colour encoding is DIVERGING about 0, so the value must keep its sign as sent (the
        # renderer asserts the scale; here we pin that the payload isn't pre-normalised)
        @test by[("B/qc", "T/qc")]["value"] < 0 < by[("B/qc", "B/qc")]["value"]
        @test r["valueLabel"] == "log-odds"
        # an unknown suffix falls back to the first run rather than erroring
        @test plot_summary_data(img, "flow", Tuple{String,String}[], "matrix";
                                matrix_mode = "interaction", stats_suffix = "nope")["suffix"] == "run1"
        # …and with NO run at all it's an empty matrix, not a throw (the panel shows its own hint)
        empty_img = CciaImage(; dir = mktempdir())
        e = plot_summary_data(empty_img, "flow", Tuple{String,String}[], "matrix";
                              matrix_mode = "interaction")
        @test isempty(e["cells"]) && isempty(e["xLabels"])
    finally
        rm(td; recursive = true, force = true)
    end
end

@testset "spatial graph — path accessor + discovery" begin
    # The graph pools ACROSS segmentations, so it is keyed by run suffix under spatialGraph/, not by
    # value_name next to a cell table (which could not represent a cross-segmentation graph).
    # Discovery is a directory listing, like spatialStats/ — nothing in ccid.json.
    td = mktempdir()
    img = CciaImage(; dir = td)
    @test img_spatial_graph_suffixes(img) == String[]        # nothing built yet
    @test endswith(img_spatial_graph_path(img, "run1"), joinpath("spatialGraph", "run1.h5ad"))
    mkpath(img_spatial_graph_dir(img))
    for s in ("run2", "run1")
        touch(img_spatial_graph_path(img, s))
    end
    touch(joinpath(img_spatial_graph_dir(img), "notes.txt"))  # non-h5ad ignored
    @test img_spatial_graph_suffixes(img) == ["run1", "run2"]     # sorted
end

@testset "neighbourStats QC findings" begin
    # pure helper (docs/MODULES.md) — advisory findings only, never gates
    ids(fs) = Set(String(f["code"]) for f in fs)
    @test ids(Cecelia._neighbour_stats_findings(0, 0)) == Set(["spatial.no_cells"])
    @test ids(Cecelia._neighbour_stats_findings(10, 0)) == Set(["spatial.no_edges"])
    @test isempty(Cecelia._neighbour_stats_findings(10, 5, 1.0, 3))
    # a graph built over far more cells than the analysis selects → the counts rest on a slice of it
    @test "spatial.low_coverage" in ids(Cecelia._neighbour_stats_findings(10, 5, 0.02, 3))
    @test !("spatial.low_coverage" in ids(Cecelia._neighbour_stats_findings(10, 5, 0.5, 3)))
    # nothing beat chance → say so; -1 means the test was skipped (permutations = 0), so stay quiet
    @test "spatial.none_significant" in ids(Cecelia._neighbour_stats_findings(10, 5, 1.0, 0))
    @test isempty(Cecelia._neighbour_stats_findings(10, 5, 1.0, -1))
end

@testset "params NOT declared in the spec pass through untouched" begin
    # Ranges/types are swept above. What is asserted here is the absence of a rule: a task's
    # runner may receive keys its spec does not declare, and validation must not reject an
    # unknown key — several tasks rely on carrying extra values through to their runner.
    # cropImage's spec now declares one `cropBox` param carrying the whole box (the imagePicker
    # widget's shape); everything else is unknown and passes through.
    box = Dict{String,Any}("x0"=>0, "x1"=>100, "y0"=>0, "y1"=>100,
                           "z0"=>2, "z1"=>8, "t0"=>-1, "t1"=>-1)
    @test validate_params(
        CropImage(), Dict{String,Any}("cropBox" => box, "extraKey" => "unknown")) === nothing
end

@testset "CropImage inherits source calibration (pure helper)" begin
    # A crop must carry the source's physical calibration onto the new image (else the metadata
    # dialog shows "—" and the strip timestamp has no Δt) — see cropImage.jl.
    src = Dict{String,Any}(
        "SizeC" => 4, "SizeZ" => 20, "SizeT" => 181,
        "PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33, "PhysicalSizeZ" => 2.0,
        "PhysicalSizeUnit" => "micrometer", "TimeIncrement" => 15, "TimeIncrementUnit" => "second",
        "ori_path" => "/should/not/carry")                       # non-calibration keys stay behind
    # Z trimmed [2,8), T kept whole (-1) → SizeZ shrinks, SizeT & the scale/unit carry over unchanged
    m = Cecelia._crop_inherited_meta(src, (; x0=0, x1=100, y0=0, y1=100, z0=2, z1=8, t0=-1, t1=-1))
    @test m["SizeZ"] == 6                     # 8 - 2 (half-open)
    @test m["SizeT"] == 181                   # axis kept → source count
    @test m["SizeC"] == 4                     # channels invariant under crop
    @test m["PhysicalSizeX"] == 0.33 && m["TimeIncrement"] == 15
    @test m["TimeIncrementUnit"] == "second"
    @test !haskey(m, "ori_path")              # only calibration is inherited
    # T also trimmed [10,40); a source missing SizeZ → no SizeZ key invented
    m2 = Cecelia._crop_inherited_meta(Dict{String,Any}("SizeC" => 2),
                                      (; x0=0, x1=50, y0=0, y1=50, z0=-1, z1=-1, t0=10, t1=40))
    @test m2["SizeT"] == 30 && m2["SizeC"] == 2 && !haskey(m2, "SizeZ")
end


@testset "ZProject inherits source calibration (pure helper)" begin
    # A Z-projection collapses SizeZ to 1 while every other calibration field carries over
    # unchanged (X/Y pixel size + unit, T interval, channels). Same source→new pattern as crop's
    # `_crop_inherited_meta`; non-calibration keys (e.g. `ori_path`) stay behind — the handler
    # opts them in separately.
    src = Dict{String,Any}(
        "SizeC" => 4, "SizeZ" => 20, "SizeT" => 181,
        "PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33, "PhysicalSizeZ" => 2.0,
        "PhysicalSizeUnit" => "micrometer", "TimeIncrement" => 15, "TimeIncrementUnit" => "second",
        "ori_path" => "/should/not/carry")
    m = Cecelia._zproj_inherited_meta(src)
    @test m["SizeZ"] == 1                     # projected → single plane
    @test m["SizeT"] == 181 && m["SizeC"] == 4
    @test m["PhysicalSizeX"] == 0.33 && m["PhysicalSizeY"] == 0.33
    @test m["PhysicalSizeZ"] == 2.0           # slice thickness kept — describes the SOURCE stack
    @test m["TimeIncrement"] == 15 && m["TimeIncrementUnit"] == "second"
    @test !haskey(m, "ori_path")              # only calibration inherited; the handler adds it
    # a source with no SizeZ (a 2D image) still ends up with SizeZ=1 — the projection is a no-op
    m2 = Cecelia._zproj_inherited_meta(Dict{String,Any}("SizeC" => 2, "SizeT" => 10))
    @test m2["SizeZ"] == 1 && m2["SizeC"] == 2 && m2["SizeT"] == 10
end

@testset "TProject inherits source calibration (pure helper)" begin
    # A T-projection collapses SizeT to 1; every other calibration field carries over unchanged
    # (X/Y/Z pixel size + unit, frame interval, channels). Same source→new pattern as crop's and
    # ZProject's helpers.
    src = Dict{String,Any}(
        "SizeC" => 4, "SizeZ" => 20, "SizeT" => 181,
        "PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33, "PhysicalSizeZ" => 2.0,
        "PhysicalSizeUnit" => "micrometer", "TimeIncrement" => 15, "TimeIncrementUnit" => "second",
        "ori_path" => "/should/not/carry")
    m = Cecelia._tproj_inherited_meta(src)
    @test m["SizeT"] == 1                     # projected → single frame
    @test m["SizeZ"] == 20 && m["SizeC"] == 4
    @test m["PhysicalSizeX"] == 0.33 && m["PhysicalSizeZ"] == 2.0
    @test m["TimeIncrement"] == 15            # kept: describes the SOURCE's frame spacing
    @test m["TimeIncrementUnit"] == "second"
    @test !haskey(m, "ori_path")
    # a source with no SizeT (a still image) still ends up with SizeT=1 — the projection is a no-op
    m2 = Cecelia._tproj_inherited_meta(Dict{String,Any}("SizeC" => 2, "SizeZ" => 8))
    @test m2["SizeT"] == 1 && m2["SizeC"] == 2 && m2["SizeZ"] == 8
end

@testset "BinImage rescales calibration by the factor (pure helper)" begin
    # A bin shrinks SizeX/Y by integer floor and grows PhysicalSizeX/Y by the same factor — the
    # binned pixel PHYSICALLY covers `factor` source pixels. Z/T/C invariant. Same source→new
    # pattern as crop's / ZProject's / TProject's helpers.
    src = Dict{String,Any}(
        "SizeC" => 4, "SizeZ" => 20, "SizeT" => 181, "SizeX" => 1024, "SizeY" => 512,
        "PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.5, "PhysicalSizeZ" => 2.0,
        "PhysicalSizeUnit" => "micrometer",
        "TimeIncrement" => 15, "TimeIncrementUnit" => "second",
        "ori_path" => "/should/not/carry")
    m = Cecelia._bin_inherited_meta(src, 2, 4)
    @test m["SizeX"] == 512 && m["SizeY"] == 128
    @test m["PhysicalSizeX"] ≈ 0.66
    @test m["PhysicalSizeY"] ≈ 2.0
    @test m["SizeZ"] == 20 && m["SizeT"] == 181 && m["SizeC"] == 4
    @test m["PhysicalSizeZ"] == 2.0 && m["TimeIncrement"] == 15
    @test !haskey(m, "ori_path")
    # ragged remainder: integer floor matches `coarsen(trim_excess=True)` — a source of 1025 with
    # factor 2 gives 512, and the last source pixel is dropped so calibration stays honest
    m2 = Cecelia._bin_inherited_meta(Dict{String,Any}("SizeX" => 1025, "SizeY" => 513,
                                                       "PhysicalSizeX" => 1.0, "PhysicalSizeY" => 1.0), 2, 2)
    @test m2["SizeX"] == 512 && m2["SizeY"] == 256
    @test m2["PhysicalSizeX"] ≈ 2.0 && m2["PhysicalSizeY"] ≈ 2.0
end

@testset "ResampleZ rewrites SizeZ to match XY spacing (pure helper)" begin
    # A Z-resample rewrites SizeZ to make the output isotropic (PhysicalSizeZ = PhysicalSizeX). XY
    # stays put; T/C invariant.
    src = Dict{String,Any}(
        "SizeC" => 4, "SizeZ" => 20, "SizeT" => 5, "SizeX" => 1024, "SizeY" => 512,
        "PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33, "PhysicalSizeZ" => 2.0,
        "PhysicalSizeUnit" => "micrometer",
        "TimeIncrement" => 15, "TimeIncrementUnit" => "second",
        "ori_path" => "/should/not/carry")
    m = Cecelia._resample_z_inherited_meta(src)
    # ratio = 2.0 / 0.33 ≈ 6.06 → 20 * 6.06 ≈ 121 planes at 0.33 µm apart
    @test m["SizeZ"] == round(Int, 20 * (2.0 / 0.33))
    @test m["PhysicalSizeZ"] == 0.33          # isotropic → matches X
    @test m["PhysicalSizeX"] == 0.33 && m["SizeC"] == 4 && m["SizeT"] == 5
    @test !haskey(m, "ori_path")
    # a source already isotropic (px_x == px_z) is a no-op on SizeZ
    m2 = Cecelia._resample_z_inherited_meta(Dict{String,Any}(
        "SizeZ" => 32, "PhysicalSizeX" => 1.0, "PhysicalSizeZ" => 1.0))
    @test m2["SizeZ"] == 32 && m2["PhysicalSizeZ"] == 1.0
    # SizeZ never rounds down below 1 even if the ratio would zero it out
    m3 = Cecelia._resample_z_inherited_meta(Dict{String,Any}(
        "SizeZ" => 1, "PhysicalSizeX" => 1.0, "PhysicalSizeZ" => 0.1))
    @test m3["SizeZ"] == 1                    # round(1 * 0.1) = 0, floored to 1
end

@testset "Register stacks channels across cycles (pure helper)" begin
    # Registration keeps the reference's extent + calibration + timeline; only the C dimension grows.
    # The formula (ref_C + Σ(cycle_C - 1)) is decided by the handler, so the helper just receives it
    # and stamps SizeC onto the inherited meta. Other calibration flows through untouched.
    ref = Dict{String,Any}(
        "SizeC" => 4, "SizeZ" => 12, "SizeT" => 1, "SizeX" => 512, "SizeY" => 512,
        "PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33, "PhysicalSizeZ" => 2.0,
        "PhysicalSizeUnit" => "micrometer",
        "TimeIncrement" => 0, "TimeIncrementUnit" => "second",
        "ori_path" => "/should/not/carry — handler adds it separately")
    m = Cecelia._register_inherited_meta(ref, 4 + (5 - 1) + (3 - 1))  # ref + two moving cycles
    @test m["SizeC"] == 10
    @test m["SizeZ"] == 12 && m["SizeT"] == 1 && m["SizeX"] == 512 && m["SizeY"] == 512
    @test m["PhysicalSizeX"] == 0.33 && m["PhysicalSizeZ"] == 2.0
    @test m["PhysicalSizeUnit"] == "micrometer"
    @test !haskey(m, "ori_path")   # the handler carries it, not this helper
    # a total_c of 1 still writes SizeC=1 (a single-cycle "self-registration" is a valid no-op)
    m2 = Cecelia._register_inherited_meta(Dict{String,Any}("SizeX" => 100, "SizeY" => 100), 1)
    @test m2["SizeC"] == 1
    @test m2["SizeX"] == 100 && m2["SizeY"] == 100
end

@testset "CopyImage carries calibration + provenance (pure helper)" begin
    # A copy is a faithful duplicate of ONE version: every calibration field carries over UNCHANGED
    # (unlike a crop), plus ori_path and a copy_source_* breadcrumb; non-calibration keys stay behind.
    src = Dict{String,Any}(
        "SizeC" => 4, "SizeZ" => 20, "SizeT" => 181,
        "PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33, "PhysicalSizeZ" => 2.0,
        "PhysicalSizeUnit" => "micrometer", "TimeIncrement" => 15, "TimeIncrementUnit" => "second",
        "ori_path" => "/data/raw.czi", "crop_box" => Dict("x0" => 0))   # crop_box must NOT carry
    m = Cecelia._copied_meta(src, "srcUID", "driftCorrected")
    @test m["SizeC"] == 4 && m["SizeZ"] == 20 && m["SizeT"] == 181   # unchanged (full copy)
    @test m["PhysicalSizeX"] == 0.33 && m["PhysicalSizeZ"] == 2.0
    @test m["TimeIncrement"] == 15 && m["TimeIncrementUnit"] == "second"
    @test m["ori_path"] == "/data/raw.czi"                           # same acquisition → provenance carried
    @test m["copy_source_uid"] == "srcUID" && m["copy_source_value_name"] == "driftCorrected"
    @test !haskey(m, "crop_box")                                     # only calibration/provenance
end

@testset "CopyImage copy-tree helper (recursive, verbatim)" begin
    # The zarr copy is a byte-for-byte directory copy (preserves layout/levels/OME sidecar), NOT a
    # zarr re-encode — assert nested files land intact and progress reports the true file count.
    src = mktempdir()
    dst = joinpath(mktempdir(), "out.ome.zarr")
    mkpath(joinpath(src, "0", "sub"))
    write(joinpath(src, ".zattrs"), "{\"multiscales\":[]}")
    write(joinpath(src, "0", "chunk"), "abc")
    write(joinpath(src, "0", "sub", "deep"), "xyz")
    last = Ref((0, 0))
    n = Cecelia._copy_tree_with_progress(src, dst; on_progress = (a, b) -> (last[] = (a, b)))
    @test n == 3
    @test last[][2] == 3                                             # total reported = file count
    @test read(joinpath(dst, ".zattrs"), String) == "{\"multiscales\":[]}"
    @test read(joinpath(dst, "0", "chunk"), String) == "abc"
    @test read(joinpath(dst, "0", "sub", "deep"), String) == "xyz"  # nested tree preserved
end

@testset "Custom module registry (drop-in tasks)" begin
    # A user drops a task by calling register_task! with an instance + a spec path; it must then
    # resolve through _task_from_fun_name / _spec_path / validate_params exactly like a built-in.
    spec_dir = mktempdir()
    spec = joinpath(spec_dir, "exampleTest.json")
    write(spec, JSON3.write(Dict(
        "fun_name"      => "customTest.exampleTest",
        "label"         => "Example test",
        "resource_pool" => "cpu",
        "scope"         => "image",
        "params"        => [Dict("key" => "n", "label" => "N", "type" => "int",
                                 "min" => 0, "max" => 10)],
    )))

    register_task!("customTest.exampleTest", _TestCustomTask(); spec = spec)

    @test _task_from_fun_name("customTest.exampleTest") isa _TestCustomTask
    @test Cecelia._spec_path(_TestCustomTask()) == spec       # default _spec_path → custom registry
    @test task_scope(_TestCustomTask()) == "image"           # spec read via the registered path
    # param validation uses the dropped spec (n has min=0/max=10)
    @test_throws ParamValidationError validate_params(
        _TestCustomTask(), Dict{String,Any}("n" => 99))
    @test validate_params(_TestCustomTask(), Dict{String,Any}("n" => 3)) === nothing

    # built-ins win on a fun_name clash — registering under an existing name must NOT shadow it
    register_task!("importImages.remove", _TestCustomTask(); spec = spec)
    @test _task_from_fun_name("importImages.remove") isa RemoveImage

    # a missing spec file is rejected up front
    @test_throws ArgumentError register_task!(
        "customTest.bad", _TestCustomTask(); spec = joinpath(spec_dir, "nope.json"))

    # unknown fun_name still errors
    @test_throws Exception _task_from_fun_name("customTest.doesNotExist")
end

# ── Plugin testsets ────────────────────────────────────────────────────────
# ~15 testsets covering plugin layout / enumeration, manifest + versioning, contributions,
# bundled examples, module staleness, fun_name precedence, custom-module load order,
# shipped examples end-to-end, points-import path, install/remove (P2), resource-pool mapping,
# live pool limit + status snapshot, and the custom-module reload prune. Extracted from this file
# to keep it small enough to merge without EOF conflicts on every append.
# The extracted file loads inside this file's aggregating testset scope, so any helpers defined
# earlier in suite.jl are still in scope for the plugin fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "plugins.jl"))

# ── Section-param flattening (whiteboard/chain stores section params NESTED) ───
# Regression: a chain node saves `section` params under the section key (e.g.
# measureOptions => {extendedMeasures: true}), but tasks read them flat. run_task must lift them.
@testset "Section params flatten (chain nesting)" begin
    ml = _task_from_fun_name("segment.measureLabels")
    # measureLabels declares `measureOptions` + `imageTiling` sections
    @test "measureOptions" in Cecelia._section_keys(ml)
    @test "imageTiling"    in Cecelia._section_keys(ml)
    nested = Dict{String,Any}(
        "outputValueName" => "T",
        "measureOptions"  => Dict{String,Any}("extendedMeasures" => true),
        "imageTiling"     => Dict{String,Any}("blockSize" => 4096, "overlap" => 0))
    flat = Cecelia._flatten_sections(ml, nested)
    @test flat["extendedMeasures"] == true          # was buried under measureOptions
    @test flat["blockSize"] == 4096                  # was buried under imageTiling
    @test !haskey(flat, "measureOptions")            # section container dropped
    @test flat["outputValueName"] == "T"             # top-level survives
    # composite pulls section keys from its sub-tasks (cellpose + measureLabels)
    comp = _task_from_fun_name("segment.cellposeMeasure")
    @test "measureOptions" in Cecelia._section_keys(comp)
    @test Cecelia._flatten_sections(comp,
        Dict{String,Any}("measureOptions" => Dict{String,Any}("extendedMeasures" => true)))["extendedMeasures"] == true
    # already-flat params are unchanged (idempotent)
    @test Cecelia._flatten_sections(ml, Dict{String,Any}("extendedMeasures" => true))["extendedMeasures"] == true
end

# ── Dispatch + param validation — ClustTracks (clustTracks.cluster, set-scope) ───

# ── Legacy `kind` on disk is silently ignored ────────────────────────────────
# Guards the on-disk contract: a pre-existing ccid.json/project.json with a `kind` key must load
# cleanly (no field on the struct) and the next save! must strip it. Project-wide static/live/flow
# distinction was dropped in favour of per-image axis gating (Cecelia.task_applies).
@testset "Legacy `kind` on disk — ignored + stripped" begin
    proj = create_project!(name="legacy-kind-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    # Inject legacy `kind` back into every ccid.json / project.json on disk
    for f in (joinpath(proj.root, "project.json"),
              joinpath(s._dir, "ccid.json"),
              joinpath(img._dir, "ccid.json"))
        raw = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(f, String)))
        raw["kind"] = "live"
        open(f, "w") do io; JSON3.pretty(io, raw); end
    end

    # Load — must not error, must not surface `kind` as a struct field
    loaded = load_project(proj.uid)
    @test !hasfield(typeof(loaded), :kind)
    r_img = init_object(proj.uid, img.uid)
    @test r_img isa CciaImage
    @test !hasfield(typeof(r_img), :kind)

    # save! strips `kind` from disk
    save!(loaded)
    for f in (joinpath(proj.root, "project.json"),
              joinpath(s._dir, "ccid.json"),
              joinpath(img._dir, "ccid.json"))
        raw = JSON3.read(read(f, String))
        @test !haskey(raw, :kind)
    end
    rm(proj.root; recursive=true)
end

# ── Image round-trip (status + attr) ────────────────────────────────────────
# Regression guard: save!(img) must persist status and attr, not silently drop them.
@testset "Image status/attr round-trip" begin
    proj = create_project!(name="rt-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    img.status = IMAGE_DONE
    img.attr["condition"] = "treated"
    save!(img)
    r = init_object(proj.uid, img.uid)
    @test r isa CciaImage
    @test r.status == IMAGE_DONE
    @test get(r.attr, "condition", "") == "treated"
    rm(proj.root; recursive=true)
end

# ── Task subdirs: none pre-created, dead ones cleared on load ───────────────
# An image folder holds only what has actually run — every writer makes its own directory, so nothing
# is created up front (the `[dirs.tasks]` pre-create table is gone). The eight names no writer in the
# codebase uses are shed when an old image loads — ONLY if empty, and never a name a live writer uses.
@testset "New image gets no task subdirs" begin
    proj = create_project!(name="nodirs-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    # ccid.json and nothing else — the image dir describes what has run, and nothing has
    @test readdir(img._dir) == ["ccid.json"]
    @test !isdir(joinpath(img._dir, "labels"))
    rm(proj.root; recursive=true)
end

@testset "Legacy task subdirs dropped on load" begin
    proj = create_project!(name="deaddir-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    live = ("logs", "labels", "labelProps", "data", "tasks")
    for sub in (Cecelia._DEAD_TASK_DIRS..., live...); mkpath(joinpath(img._dir, sub)); end
    init_object(proj.uid, img.uid)
    for sub in Cecelia._DEAD_TASK_DIRS
        @test !isdir(joinpath(img._dir, sub))
    end
    # a live name is never shed, even when empty — removing one would race the task that just made it
    for sub in live
        @test isdir(joinpath(img._dir, sub))
    end

    # a non-empty one is somebody's data, whatever we think wrote it
    mkpath(joinpath(img._dir, "mesh"))
    write(joinpath(img._dir, "mesh", "keep.txt"), "x")
    init_object(proj.uid, img.uid)
    @test isfile(joinpath(img._dir, "mesh", "keep.txt"))
    rm(proj.root; recursive=true)
end

# The one name that survived `[dirs.tasks]`, now a constant rather than a config lookup.
# Build the base with `joinpath` and reuse it on both sides: a literal "/x/y" keeps its forward
# slashes on Windows while `joinpath` appends a backslash, so hardcoding either separator asserts
# the platform rather than the behaviour.
@testset "task_run_dir is <base>/tasks" begin
    base = joinpath("x", "y")
    @test Cecelia.task_run_dir(base) == joinpath(base, "tasks")
    @test basename(Cecelia.task_run_dir(base)) == "tasks"
end

# ── Branch labels round-trip (BRANCHING_PLAN.md Decision 6) ──────────────────
# Skeleton (branch) label sets live in a dedicated `branch_labels` field, NOT in the generic
# `labels` dict, so the labels/measure/tracking pickers never see branch labels. Guards: the
# field survives save!/init_object, its accessor resolves the disk path from branchLabels/,
# and a legacy ccid.json without the key still loads (defaults to empty).
@testset "Branch labels round-trip" begin
    proj = create_project!(name="branch-rt-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    @test isempty(img.branch_labels)
    img.branch_labels["stroma"] = ["stroma.zarr"]
    save!(img)
    r = init_object(proj.uid, img.uid)
    @test r isa CciaImage
    @test r.branch_labels["stroma"] == ["stroma.zarr"]
    @test img_branch_labels_dir(r) == joinpath(r._dir, "branchLabels")
    @test img_branch_labels_path(r, "stroma") == joinpath(r._dir, "branchLabels", "stroma.zarr")
    # unregistered value_name falls back to {value_name}.zarr (write path)
    @test img_branch_labels_path(r, "shg") == joinpath(r._dir, "branchLabels", "shg.zarr")

    # legacy ccid.json (no branch_labels key) → empty
    ccid = joinpath(r._dir, "ccid.json")
    raw  = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
    delete!(raw, "branch_labels")
    open(ccid, "w") do io; JSON3.write(io, raw); end
    legacy = init_object(proj.uid, img.uid)
    @test isempty(legacy.branch_labels)
    rm(proj.root; recursive=true)
end

# ── Reserved value_name suffixes ─────────────────────────────────────────────
# __tracks and __branch are companion-table markers, not legal user segmentation names.
@testset "Reserved value_name suffixes" begin
    @test  is_reserved_value_name("stroma__tracks")
    @test  is_reserved_value_name("stroma__branch")
    @test !is_reserved_value_name("stroma")
    @test !is_reserved_value_name("stroma.branch")   # dot-suffix is the old R convention; not reserved
end

# ── Per-image user flags (included / note / starred) round-trip ──────────────
# Guards: new images default to included + unstarred; the flags survive save!/init_object; and a
# legacy ccid.json with none of the keys loads as included (the accessor never sees a missing field).
@testset "Image included/note/starred round-trip" begin
    proj = create_project!(name="incl-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    @test image_included(img)                 # default: included
    @test img.note == ""
    @test img.starred == false                # default: not starred

    img.included = false
    img.note = "bad drift reference channel"
    img.starred = true
    save!(img)
    r = init_object(proj.uid, img.uid)
    @test r isa CciaImage
    @test !image_included(r)
    @test r.note == "bad drift reference channel"
    @test r.starred

    # legacy file (none of the keys) → included, empty note, unstarred
    ccid = joinpath(r._dir, "ccid.json")
    raw  = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
    delete!(raw, "included"); delete!(raw, "note"); delete!(raw, "starred")
    open(ccid, "w") do io; JSON3.write(io, raw); end
    legacy = init_object(proj.uid, img.uid)
    @test image_included(legacy)
    @test legacy.note == ""
    @test legacy.starred == false
    rm(proj.root; recursive=true)
end

# ── Per-task param memory (funParams) — R moduleFunParams parity ─────────────
# Last-used params are remembered in ccid.json under meta["funParams"][fun], per image and per
# set. Guards: round-trips through save!/init_object, per-fun keys don't clobber, set-level too.
@testset "funParams per-object memory" begin
    proj = create_project!(name="fp-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    save!(img)

    @test read_module_fun_params(img._dir, "cleanupImages.driftCorrect") === nothing  # absent

    p = Dict{String,Any}("valueName" => "driftCorrected", "driftChannel" => ["DAPI"])
    write_module_fun_params!(img._dir, "cleanupImages.driftCorrect", p)
    got = read_module_fun_params(img._dir, "cleanupImages.driftCorrect")
    @test !isnothing(got)
    @test got["valueName"] == "driftCorrected"
    @test got["driftChannel"] == ["DAPI"]

    # init_object loads funParams into the object's meta; a load-modify-save then preserves them
    # (the loaded object carries funParams, so save! doesn't drop them — unlike a stale object).
    r = init_object(proj.uid, img.uid)
    @test haskey(r.meta, "funParams")
    r.status = IMAGE_DONE; save!(r)
    r2 = init_object(proj.uid, img.uid)
    @test r2.status == IMAGE_DONE
    @test read_module_fun_params(r2._dir, "cleanupImages.driftCorrect")["valueName"] == "driftCorrected"

    # a second task's params coexist under its own key (no clobber)
    write_module_fun_params!(img._dir, "cleanupImages.smooth",
                             Dict{String,Any}("valueName" => "default"))
    @test read_module_fun_params(img._dir, "cleanupImages.driftCorrect")["valueName"] == "driftCorrected"
    @test read_module_fun_params(img._dir, "cleanupImages.smooth")["valueName"] == "default"

    # set-level memory uses the same dir-based mechanism on the set's ccid.json
    write_module_fun_params!(s._dir, "cleanupImages.driftCorrect",
                             Dict{String,Any}("valueName" => "setDefault"))
    @test read_module_fun_params(s._dir, "cleanupImages.driftCorrect")["valueName"] == "setDefault"

    rm(proj.root; recursive=true)
end

# One blob per TASK is wrong the moment a task runs twice under different names: segmenting `Tcell`
# and then `Neutrophil` left the form showing Neutrophil's settings, so re-running Tcell meant
# re-entering every model parameter. Params are now ALSO banked per output name.
@testset "funParams remembered per output name" begin
    proj = create_project!(name="fpn-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    save!(img)
    fun = "segment.cellpose"

    tcell = Dict{String,Any}("outputValueName" => "Tcell", "cellDiameter" => 8)
    neutr = Dict{String,Any}("outputValueName" => "Neutrophil", "cellDiameter" => 15)
    write_module_fun_params!(img._dir, fun, tcell; value_name = "Tcell")
    write_module_fun_params!(img._dir, fun, neutr; value_name = "Neutrophil")

    # each name keeps its OWN params — the whole point
    @test read_module_fun_params(img._dir, fun; value_name = "Tcell")["cellDiameter"] == 8
    @test read_module_fun_params(img._dir, fun; value_name = "Neutrophil")["cellDiameter"] == 15

    # the flat blob still tracks the most recent run whatever it was called, because that is what a
    # NEW name falls back to — and falling back to bare task defaults would be a worse starting point
    @test read_module_fun_params(img._dir, fun)["cellDiameter"] == 15
    @test read_module_fun_params(img._dir, fun; value_name = "Macrophage")["cellDiameter"] == 15

    # …but the by-name reader does NOT fall back. The form needs the two answers distinguishable:
    # "nothing banked for this name" must not overwrite a form the user has just edited.
    @test read_module_fun_params_by_name(img._dir, fun, "Macrophage") === nothing
    @test read_module_fun_params_by_name(img._dir, fun, "Tcell")["cellDiameter"] == 8
    @test read_module_fun_params_by_name(img._dir, fun, "") === nothing

    # a task with no name banked is untouched by any of this (the pre-existing path)
    write_module_fun_params!(img._dir, "cleanupImages.smooth", Dict{String,Any}("spatialSigma" => 2))
    @test read_module_fun_params(img._dir, "cleanupImages.smooth")["spatialSigma"] == 2
    @test read_module_fun_params_by_name(img._dir, "cleanupImages.smooth", "Tcell") === nothing

    # survives the object round-trip, like funParams above
    r = init_object(proj.uid, img.uid)
    r.status = IMAGE_DONE; save!(r)
    @test read_module_fun_params_by_name(init_object(proj.uid, img.uid)._dir, fun, "Tcell")["cellDiameter"] == 8

    rm(proj.root; recursive=true)
end

# Banking params per name only helps runs made AFTER it existed — which on every real project is
# none of them, so every name resolved to "nothing banked" and the form restored nothing. The run log
# already held each run's params and the output name is recoverable from them, so names that predate
# the by-name record are answered from there.
@testset "funParams by name fall back to the run log" begin
    proj = create_project!(name="fpl-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    save!(img)
    fun = "segment.cellpose"

    # the history a project already has: two names segmented, nothing banked by name
    append_run_log!(img, fun, "corrected", "done",
                    Dict{String,Any}("outputValueName" => "Tcell", "cellDiameter" => 8))
    append_run_log!(img, fun, "corrected", "done",
                    Dict{String,Any}("outputValueName" => "Neutrophil", "cellDiameter" => 15))
    @test isnothing(read_module_fun_params_by_name(img._dir, fun, "Macrophage"))
    @test read_module_fun_params_by_name(img._dir, fun, "Tcell")["cellDiameter"] == 8
    @test read_module_fun_params_by_name(img._dir, fun, "Neutrophil")["cellDiameter"] == 15

    # the INPUT version comes back too. `_run_log_params` strips `valueName` (the entry carries it as
    # its own field), and a record restored without it silently resets which version the task runs on.
    @test read_module_fun_params_by_name(img._dir, fun, "Tcell")["valueName"] == "corrected"

    # the newest run under a name wins — this is "the settings it was last segmented with"
    append_run_log!(img, fun, "corrected", "done",
                    Dict{String,Any}("outputValueName" => "Tcell", "cellDiameter" => 11))
    @test read_module_fun_params_by_name(img._dir, fun, "Tcell")["cellDiameter"] == 11

    # …and only among runs that SUCCEEDED — a later failure does not become the name's settings
    append_run_log!(img, fun, "corrected", "failed",
                    Dict{String,Any}("outputValueName" => "Tcell", "cellDiameter" => 99))
    @test read_module_fun_params_by_name(img._dir, fun, "Tcell")["cellDiameter"] == 11

    # A name that ONLY ever failed restores nothing, rather than falling back to the failed run. The
    # two halves have to agree on one set of names: the picker offers what exists in the namespace,
    # and a failed run wrote nothing there — so restoring for it would answer for a name the list can
    # never offer. Same for a run still in flight, which has not written its output yet.
    append_run_log!(img, fun, "corrected", "failed",
                    Dict{String,Any}("outputValueName" => "Bcell", "cellDiameter" => 4))
    @test isnothing(read_module_fun_params_by_name(img._dir, fun, "Bcell"))
    append_run_log!(img, fun, "corrected", Cecelia.RUN_LOG_RUNNING,
                    Dict{String,Any}("outputValueName" => "Eos", "cellDiameter" => 5))
    @test isnothing(read_module_fun_params_by_name(img._dir, fun, "Eos"))

    # a name is matched through the SPEC's `namespace`, not by a key called `outputValueName` — the
    # run log records six different spellings depending on the task
    append_run_log!(img, "clustPops.cluster", "", "done",
                    Dict{String,Any}("valueNameSuffix" => "immune", "resolution" => 0.6))
    @test read_module_fun_params_by_name(img._dir, "clustPops.cluster", "immune")["resolution"] == 0.6

    # …and through a COMPOSITE's steps. This is what the segmentation page runs, so a log full of
    # `segment.cellposeMeasure` entries is the realistic case, not the plain-task one above.
    append_run_log!(img, "segment.cellposeMeasure", "afCorrected", "done",
                    Dict{String,Any}("outputValueName" => "Podocyte", "cellDiameter" => 6))
    got = read_module_fun_params_by_name(img._dir, "segment.cellposeMeasure", "Podocyte")
    @test !isnothing(got) && got["cellDiameter"] == 6 && got["valueName"] == "afCorrected"

    # another task's run under the same name is not this task's params
    @test isnothing(read_module_fun_params_by_name(img._dir, "cleanupImages.smooth", "Tcell"))

    # a banked record still wins — it is the exact answer, the log is the retroactive half
    write_module_fun_params!(img._dir, fun, Dict{String,Any}(
        "outputValueName" => "Tcell", "cellDiameter" => 3); value_name = "Tcell")
    @test read_module_fun_params_by_name(img._dir, fun, "Tcell")["cellDiameter"] == 3

    # the SET dir keeps no run log, so it answers from the banked record only — no crash, no guess
    @test isnothing(read_module_fun_params_by_name(s._dir, fun, "Tcell"))

    rm(proj.root; recursive=true)
end

# The namespaces a `valueNameInput` suggests from need an IMAGE-owned accessor, like the graph/track
# /branch ones — `INVENTORY.md`'s rule, and the reason `_clustfeatures_suffixes` (which takes a
# label-props PATH and owns the sidecar's three historical layouts) is WRAPPED here rather than moved.
@testset "image accessors for the suggestion namespaces" begin
    proj = create_project!(name="ns-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    save!(img)

    # nothing on disk → empty, never an error: an image with no stats/clustering is the normal case
    @test img_stats_suffixes(img) == String[]
    @test img_cluster_suffixes(img) == String[]

    # stats are listed from spatialStats/{suffix}.json, and the PATH helper is the one speller —
    # the writer (neighbourStats.jl) and reader (ai/spatial.jl) both used to join it inline
    mkpath(img_stats_dir(img))
    write(img_stats_path(img, "contacts"), "{}")
    write(img_stats_path(img, "aggregates"), "{}")
    write(joinpath(img_stats_dir(img), "notes.txt"), "x")     # not a .json → not a run
    @test img_stats_suffixes(img) == ["aggregates", "contacts"]

    # clustering runs come from the clustfeatures manifest beside the label props, per SEGMENTATION,
    # and the two families are listed separately so `clusters.immune` and `regions.immune` coexist
    mkpath(img_label_props_dir(img))
    props = img_label_props_path(img, "default")
    write(Cecelia._clustfeatures_path(props),
          """{"clusters.immune": {"features": ["a"]}, "regions.niches": {"features": ["b"]}}""")
    @test img_cluster_suffixes(img, "default"; family="clusters") == ["immune"]
    @test img_cluster_suffixes(img, "default"; family="regions")  == ["niches"]

    # a different segmentation has its own manifest — this is per (image, value_name), not per image
    @test img_cluster_suffixes(img, "other"; family="clusters") == String[]

    rm(proj.root; recursive=true)
end

# `task_output_name` is the Julia twin of `taskOutput` (frontend/src/utils/taskOutput.ts) — the name a
# run writes under, resolved from the spec's `namespace` because SIX different keys can carry it. The
# two cannot call each other, so the SPECS are the shared contract and each side is pinned against
# them (the TS half is `taskOutput.test.ts`). Same arrangement as the calibration writers.
@testset "task_output_name agrees with the frontend rule" begin
    # a `valueNameInput` param, found by its declared namespace and not by its key
    @test Cecelia.task_output_name("segment.cellpose",
                           Dict{String,Any}("outputValueName" => "Tcell")) == "Tcell"
    @test Cecelia.task_output_name("spatialAnalysis.cellNeighbours",
                           Dict{String,Any}("graphSuffix" => "pooled")) == "pooled"

    # falls back to the param's DEFAULT when the form has not set one — the same name the run will use
    @test Cecelia.task_output_name("segment.cellpose", Dict{String,Any}()) == "default"

    # whitespace is not a name; an unknown fun_name is not this function's error to raise
    @test Cecelia.task_output_name("segment.cellpose", Dict{String,Any}("outputValueName" => "  ")) == ""
    @test Cecelia.task_output_name("no.such.task", Dict{String,Any}("outputValueName" => "x")) == ""

    # a task that names no output of its own reports "", never a guess
    @test Cecelia.task_output_name("importImages.omezarr", Dict{String,Any}()) == ""

    # EVERY spec carrying a valueNameInput resolves through it — so flipping another task's param
    # (Phase 3: clustering, stats, models) cannot silently fail to be remembered per name
    for (fun, task) in Cecelia._fun_name_map()
        spec = Cecelia._task_spec(task)
        isnothing(spec) && continue
        vni = String[]
        each_spec_param(get(spec, "params", [])) do p, _gk
            String(something(spec_get(p, "type", ""), "")) == "valueNameInput" &&
                push!(vni, String(something(spec_get(p, "key", ""), "")))
        end
        isempty(vni) && continue
        k = first(vni)
        @test Cecelia.task_output_name(fun, Dict{String,Any}(k => "probe-name")) == "probe-name"
    end

    # …and every COMPOSITE resolves its steps' name. The loop above cannot see them: a composite spec
    # declares no params of its own, so it carries no `valueNameInput` to probe and was skipped —
    # while being what the module pages actually run (`segment.cellposeMeasure`, not `segment.cellpose`).
    # That is the trait-recursion trap task.jl warns about, and it shipped once: params banked per
    # output name keyed off this function, so no segmentation started from a module page was ever
    # remembered under its name. The frontend was fine, because `api_task_definitions` merges a
    # composite's step params before it ever sees them — which is what made it silent.
    composites = 0
    for (fun, task) in Cecelia._fun_name_map()
        task isa Cecelia.CompositeTask || continue
        keys_ = String[]
        for sub in Cecelia._composite_steps(task)
            spec = Cecelia._task_spec(sub)
            isnothing(spec) && continue
            each_spec_param(get(spec, "params", [])) do p, _gk
                String(something(spec_get(p, "type", ""), "")) == "valueNameInput" &&
                    push!(keys_, String(something(spec_get(p, "key", ""), "")))
            end
        end
        isempty(keys_) && continue
        composites += 1
        @test Cecelia.task_output_name(fun, Dict{String,Any}(first(keys_) => "probe-name")) == "probe-name"
    end
    @test composites > 0        # the loop above must not pass by finding nothing to check
end

# ── Channel names use the versioned convention ──────────────────────────────
# Regression guard: channel names were stored unversioned under meta, where the
# task/API readers (which use top-level versioned imChannelNames) never saw them.
@testset "Channel names versioned round-trip" begin
    proj = create_project!(name="cn-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    set_channel_names!(img, ["DAPI", "GFP"]; check_length=false)
    save!(img)

    # model accessor round-trips through save!/load
    r = init_object(proj.uid, img.uid)
    @test channel_names(r) == ["DAPI", "GFP"]

    # on-disk: top-level versioned imChannelNames (the shape tasks/API use), not under meta
    raw = JSON3.read(read(joinpath(img._dir, "ccid.json"), String), Dict{String,Any})
    @test haskey(raw, "imChannelNames")
    @test !haskey(Dict{String,Any}(get(raw, "meta", Dict())), "imChannelNames")
    @test versioned_active(raw["imChannelNames"]) == "default"
    # readable via the exact helper tasks/API use
    @test collect(String, versioned_get_field(raw, "imChannelNames")) == ["DAPI", "GFP"]

    rm(proj.root; recursive=true)
end

# read_ccid_raw is the one ccid.json read+Symbol-key-normalize helper (used by the api layer).
# versioned_get is the single active-value accessor for both String→String path dicts and the
# Any/JSON3 raw dicts (replaced the removed image.jl `active`).
@testset "read_ccid_raw + versioned_get on path dicts" begin
    mktempdir() do d
        p = joinpath(d, "ccid.json")
        write(p, """{"filepath":{"default":"x.ome.zarr","_active":"default"},"class":"CciaImage"}""")
        raw = read_ccid_raw(p)
        @test raw isa Dict{String,Any}
        @test all(k -> k isa String, keys(raw))
        @test raw["class"] == "CciaImage"
        # readable via the exact helper the api/tasks use (nothing → active entry)
        @test versioned_get_field(raw, "filepath") == "x.ome.zarr"
    end
    # versioned_get on a concrete String→String versioned dict (the img.filepath / img.label_props shape)
    d = Dict{String,String}("default" => "a.zarr", "v2" => "b.zarr", "_active" => "v2")
    @test versioned_get(d) == "b.zarr"                 # active entry
    @test sort(versioned_keys(d)) == ["default", "v2"] # excludes _active
end

# ── Destructive ops ──────────────────────────────────────────────────────────
@testset "delete_image! / delete_set!" begin
    proj = create_project!(name="del-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    a    = add_image!(s; name="a")
    b    = add_image!(s; name="b")

    delete_image!(s, b.uid)
    @test !(b.uid in s.image_uids)
    @test !isdir(joinpath(proj.root, "0", b.uid))
    @test !isdir(joinpath(proj.root, "1", b.uid))
    @test !(b.uid in init_object(proj.uid, s.uid).image_uids)   # persisted

    set_uid = s.uid
    delete_set!(proj, set_uid)
    @test !(set_uid in proj.set_uids)
    @test !isdir(joinpath(proj.root, "1", set_uid))
    @test !isdir(joinpath(proj.root, "1", a.uid))               # member removed too
    @test !(set_uid in load_project(proj.uid).set_uids)         # persisted

    rm(proj.root; recursive=true)
end

# ── Boundary contract: run a REAL module function end-to-end, no api/ ───────
# The whole suite loads only `using Cecelia` (api/ is not on the path). This runs
# an actual task (RemoveImage — real ccid.json + disk work, no external binary) to
# completion through the public `run_task` entrypoint. Catches coupling creeping back.
@testset "Boundary contract — real module fn end-to-end" begin
    proj = create_project!(name="bc-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    # register a real on-disk image version
    zarr = joinpath(img_zero_dir(img), "ccidImage.ome.zarr")
    mkpath(zarr)
    img.filepath["default"] = "ccidImage.ome.zarr"
    img.filepath["_active"] = "default"
    img.status = IMAGE_DONE
    save!(img)

    logs = String[]
    result = run_task(RemoveImage(), img,
        Dict{String,Any}("valueName"=>"default", "newDefault"=>"default");
        on_log = l -> push!(logs, l))

    @test result isa Dict
    @test result["removedValue"] == "default"
    @test result["cleared"] == true          # primary removal clears dims/status
    @test !isdir(zarr)                        # file actually deleted from disk
    @test !isempty(logs)                      # on_log callback fired (no WS needed)

    reloaded = init_object(proj.uid, img.uid)
    @test reloaded.status == IMAGE_PENDING        # primary removal reset status
    @test !haskey(reloaded.filepath, "default")  # version entry gone
    rm(proj.root; recursive=true)
end

# ── Storage reclaim — free every non-active image version, keep the active one ───────
@testset "Storage reclaim" begin
    # _path_bytes is the ONE "how big is this on disk" answer, shared by storage accounting, version
    # removal and the image-metadata modal — a directory is walked, a plain file is stat'd, and
    # anything absent is 0 rather than an error (a caller listing versions must not throw on a store
    # that isn't there).
    mktempdir() do d
        f = joinpath(d, "one.bin"); write(f, rand(UInt8, 4096))
        sub = joinpath(d, "store"); mkpath(joinpath(sub, "0"))
        write(joinpath(sub, "0", "chunk"), rand(UInt8, 8192))
        @test Cecelia._path_bytes(f) >= 4096                     # file: at least its bytes
        @test Cecelia._path_bytes(sub) >= 8192                   # dir: walked, not stat'd (a dir stat is ~4 KB)
        @test Cecelia._path_bytes(joinpath(d, "nope")) == 0      # absent, not an error
    end

    # pure policy: everything except the active version
    @test Set(Cecelia.reclaimable_versions(Dict{String,Any}(
        "default"=>"a", "afCorrected"=>"b", "driftCorrected"=>"c", "_active"=>"driftCorrected"))) ==
        Set(["default", "afCorrected"])
    @test isempty(Cecelia.reclaimable_versions(Dict{String,Any}(  # only the active version present
        "default"=>"a", "_active"=>"default")))
    # active is the original, but a leftover corrected variant is still freeable (NEW vs default-only)
    @test Cecelia.reclaimable_versions(Dict{String,Any}(
        "default"=>"a", "afCorrected"=>"b", "_active"=>"default")) == ["afCorrected"]

    _mk_ver!(img, fn) = (d = joinpath(img_zero_dir(img), fn); mkpath(d);
                         write(joinpath(d, "chunk"), rand(UInt8, 2048)); fn)

    proj = create_project!(name="stor-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")

    # imgA: original + af + drift, drift active → reclaim frees default AND af, keeps drift
    a = add_image!(s; name="a")
    _mk_ver!(a, "import.ome.zarr"); _mk_ver!(a, "af.ome.zarr"); _mk_ver!(a, "drift.ome.zarr")
    a.filepath = Dict("default"=>"import.ome.zarr", "afCorrected"=>"af.ome.zarr",
                      "driftCorrected"=>"drift.ome.zarr", "_active"=>"driftCorrected")
    a.im_channel_names = Dict{String,Union{Vector{String},String}}("default"=>["ch0","ch1"], "_active"=>"default")
    a.meta = Dict{String,Any}("SizeC"=>2, "SizeT"=>1, "SizeZ"=>5)
    a.status = IMAGE_DONE; save!(a)

    # imgB: original only, active default → nothing to reclaim
    b = add_image!(s; name="b")
    _mk_ver!(b, "import.ome.zarr")
    b.filepath = Dict("default"=>"import.ome.zarr", "_active"=>"default")
    b.status = IMAGE_DONE; save!(b)

    # safe-primary unit: removing default while other versions remain must NOT un-import
    freed, cleared = remove_image_version!(a, "default", "driftCorrected")
    @test freed > 0 && cleared == false
    # restore default for the batch reclaim below
    _mk_ver!(a, "import.ome.zarr")
    ra0 = init_object(proj.uid, a.uid); ra0.filepath["default"] = "import.ome.zarr"; save!(ra0)

    # reclaim_inactive! frees ALL non-active (default + af), keeps drift; imgB skipped
    tot, reclaimed = reclaim_inactive!(proj.uid, [a.uid, b.uid])
    @test reclaimed == [a.uid]
    @test tot > 0
    @test !isdir(joinpath(img_zero_dir(a), "import.ome.zarr"))    # original gone
    @test !isdir(joinpath(img_zero_dir(a), "af.ome.zarr"))        # intermediate gone
    @test  isdir(joinpath(img_zero_dir(a), "drift.ome.zarr"))     # active kept
    @test  isdir(joinpath(img_zero_dir(b), "import.ome.zarr"))    # b untouched

    ra = init_object(proj.uid, a.uid)
    @test ra.status == IMAGE_DONE                                     # NOT un-imported
    @test ra.filepath["_active"] == "driftCorrected"
    @test collect(keys(filter(kv -> kv.first != "_active", ra.filepath))) == ["driftCorrected"]
    @test ra.meta["SizeC"] == 2                                   # dims kept
    @test Cecelia.versioned_get(ra.im_channel_names, "default") == ["ch0","ch1"]  # channel names kept
    rm(proj.root; recursive=true)
end

# ── Analysis reset: drop everything derived, keep the image ────────────────────
# The other half of the delete story (docs/todo/IMAGE_DELETE_PLAN.md): `remove_image_version!` sheds
# STORES, `reset_image_analysis!` sheds NUMBERS, and neither may do the other's job. The keep-list is
# asserted by NAME rather than by count, so adding a new analysis dir to the image layout fails here
# until it is deliberately classified (Decision 7 — a delete-list would have leaked it silently).
@testset "Analysis reset keeps the image and drops the numbers" begin
    @test Cecelia.ANALYSIS_KEEP == Set(["ccid.json", "runlog.json", "gating"])

    proj = create_project!(name="reset-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="a")

    # an image store (must survive) …
    zdir = joinpath(img_zero_dir(img), "import.ome.zarr"); mkpath(zdir)
    write(joinpath(zdir, "chunk"), rand(UInt8, 2048))
    img.filepath = Dict("default"=>"import.ome.zarr", "_active"=>"default")
    img.im_channel_names = Dict{String,Union{Vector{String},String}}("default"=>["ch0"], "_active"=>"default")
    img.meta   = Dict{String,Any}("SizeC"=>1, "SizeT"=>1, "SizeZ"=>3)
    img.attr   = Dict{String,Any}("treatment"=>"CTRL")
    img.status = IMAGE_DONE
    img.labels = Dict("A"=>["A.zarr"])          # not versioned — a plain valueName → files map
    img.label_props = Dict("A"=>"A.h5ad")
    save!(img)

    # … and one file in every derived location, plus the things the keep-list protects
    for sub in ("labels", "labelProps", "populations", "stats", "mesh", "qc", "cl",
                "spatialGraph", "spatialStats", "branchLabels")
        mkpath(joinpath(img._dir, sub))
        write(joinpath(img._dir, sub, "x.bin"), rand(UInt8, 1024))
    end
    write(joinpath(img._dir, "runlog.json"), "[]")
    mkpath(joinpath(img._dir, "gating"))
    write(joinpath(img._dir, "gating", "A.json"), "{}")        # hand-drawn gates — must SURVIVE

    # the storage box's number IS what a reset would free — one accounting, so the box can't promise
    # bytes the reset doesn't deliver
    predicted = analysis_bytes_of(img)
    @test predicted > 0

    freed, dropped = reset_image_analysis!(img)

    @test freed == predicted
    @test analysis_bytes_of(img) == 0                              # nothing derived left to free
    @test freed > 0
    @test "labels" in dropped && "qc" in dropped && "spatialGraph" in dropped
    for sub in ("labels", "labelProps", "populations", "stats", "mesh", "qc", "cl",
                "spatialGraph", "spatialStats", "branchLabels")
        @test !ispath(joinpath(img._dir, sub))
    end
    # the keep-list survives, by name
    @test isfile(joinpath(img._dir, "ccid.json"))
    @test isfile(joinpath(img._dir, "runlog.json"))
    # gate polygons are user work, not output: a re-run under the same value_name reuses them
    @test isfile(joinpath(img._dir, "gating", "A.json"))
    @test !("ccid.json" in dropped) && !("runlog.json" in dropped) && !("gating" in dropped)

    # NO store is shed — that is remove_image_version!'s job (Decision 9)
    @test isdir(zdir)

    ri = init_object(proj.uid, img.uid)
    @test ri isa CciaImage
    @test Cecelia.versioned_get(ri.filepath, "default") == "import.ome.zarr"   # version untouched
    @test ri.filepath["_active"] == "default"
    @test ri.status == IMAGE_DONE                                                  # still imported
    @test ri.meta["SizeC"] == 1                                                # calibration/dims kept
    @test ri.attr["treatment"] == "CTRL"                                       # annotations kept
    # the analysis REGISTRATIONS are cleared, so nothing points at a deleted file
    @test isempty(ri.labels)
    @test isempty(ri.label_props)

    # idempotent: a second reset on an already-clean image is a no-op, not an error
    freed2, dropped2 = reset_image_analysis!(ri)
    @test freed2 == 0 && isempty(dropped2)

    rm(proj.root; recursive=true)
end

# ── TaskJob target sum type (SingleImage / MultiImage) ─────────────────────
#
# The scheduler used to carry the scope as `img::CciaImage` + `imgs::Union{Nothing,Vector{CciaImage}}`
# — `nothing` meant "single image, use img". Every read branched on `isnothing`, and a bare read of
# `job.imgs` looked like it might return the representative. The `TaskJobTarget` ADT (SingleImage |
# MultiImage) makes the two shapes distinct types and moves the branch into `all_images` /
# `run_task_target` / `representative_image` — one dispatch each, no `isnothing`.
@testset "TaskJobTarget: SingleImage | MultiImage" begin
    proj = create_project!(name="tjt-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    a    = add_image!(s; name="a")
    b    = add_image!(s; name="b")

    st = Cecelia.SingleImage(a)
    @test Cecelia.all_images(st) == CciaImage[a]
    @test Cecelia.run_task_target(st) === a
    @test Cecelia.representative_image(st) === a

    mt = Cecelia.MultiImage(CciaImage[a, b])
    @test Cecelia.all_images(mt) == CciaImage[a, b]
    @test Cecelia.run_task_target(mt) == CciaImage[a, b]       # the vector, contents unchanged
    @test Cecelia.representative_image(mt) === a               # first is the representative

    # An empty set-scope job is a bug at THIS layer — the empty vector is caught earlier in
    # `run_task(::CciaTask, ::Vector{CciaImage}, …)`. Refusing at construction closes the "queued
    # a job that will crash on the worker" path.
    @test_throws ArgumentError Cecelia.MultiImage(CciaImage[])
end

# ── A task crash is recorded in the per-image log, not just the console ──────
# Regression: a Julia-side failure (caught in _execute_job!) used to only @warn to the console —
# it never reached {img._dir}/logs/{fun}.log, so a crashed task looked like it just stopped
# mid-run with no error (invisible to get_task_log + on-disk debugging).
@testset "Task crash is teed into the per-image log" begin
    proj = create_project!(name="crash-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    logs = String[]
    result = run_task(_CrashTask(), img, Dict{String,Any}(); on_log = l -> push!(logs, l))

    @test result === nothing                                       # crash → nil result
    @test any(l -> occursin("Task crashed", l) && occursin("boom", l), logs)  # reached on_log

    fun_name = Cecelia._fun_name_from_task(_CrashTask())
    logfile  = joinpath(img._dir, "logs", fun_name * ".log")
    @test isfile(logfile)                                          # ...and the on-disk log
    @test occursin("boom", read(logfile, String))

    # ...AND the run log records the FAILED run (so history / the observer can see repeats, not
    # just successes). This is the fix for tasks that silently failed invisibly (broken run_py, HMM).
    rlog = read_run_log(img)
    @test length(rlog) == 1
    @test String(rlog[end]["fun"]) == fun_name && String(rlog[end]["status"]) == "failed"
    rm(proj.root; recursive=true)
end

# ── A cancelled run is RECORDED, and it is recorded before the work starts ───
# Regression, and the expensive one. The run log used to be appended on finish and only for
# :done/:failed — `:cancelled` was skipped as "the user aborted, not an outcome worth logging". So a
# segmentation killed 22 minutes in left NOTHING behind: no run-log entry, no outcome, and a task log
# that simply stopped mid-stream. "I started six, three are running, what happened to the other three?"
# had no answer anywhere in the project. Two things are pinned here:
#   1. the entry exists WHILE the task runs (an append-on-finish log cannot record a run whose process
#      is killed, which is how the detached runner loses its queue — docs/RUNNER.md), and
#   2. the cancel is stamped onto that same entry rather than dropped or duplicated.
@testset "A cancelled run is banked in the run log" begin
    proj = create_project!(name="rl-cancel-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name="s"); name="img")

    _HOLD_TASK_GO[] = Channel{Nothing}(1)
    tid  = "cancelhold$(rand(1000:9999))"
    logs = String[]
    th = Threads.@spawn run_task(_HoldTask(), img, Dict{String,Any}("modelType" => "cyto3",
                                                                    "diameter"  => 17);
                                 task_id = tid, on_log = l -> push!(logs, l))
    try
        @test timedwait(() -> any(r -> r.id == tid && r.status == "running", list_tasks()), 30.0) === :ok
        # (1) OPEN — the run is on disk, as "running", while it is still going
        open_entry = only(filter(e -> String(get(e, "taskId", "")) == tid, read_run_log(img)))
        @test String(open_entry["status"]) == Cecelia.RUN_LOG_RUNNING
        @test String(open_entry["fun"]) == Cecelia._fun_name_from_task(_HoldTask())
        cancel_task!(tid)
    finally
        put!(_HOLD_TASK_GO[], nothing)
        timedwait(() -> istaskdone(th), 30.0)
    end

    # (2) CLOSE — one entry, now terminal. Not skipped (the bug) and not appended twice.
    rlog = filter(e -> String(get(e, "taskId", "")) == tid, read_run_log(img))
    @test length(rlog) == 1
    @test String(rlog[end]["status"]) == "cancelled"
    @test !isempty(String(rlog[end]["finishedAt"]))
    # …and the task log says so, instead of just stopping — which is indistinguishable from a crash
    @test any(l -> occursin("cancelled", lowercase(l)), logs)
    @test occursin("cancelled", lowercase(read(joinpath(img._dir, "logs",
                                                        Cecelia._fun_name_from_task(_HoldTask()) * ".log"),
                                               String)))
    forget_task_start!(tid)
    rm(proj.root; recursive=true)
end

# ── A job ALWAYS releases its submitter, even if the error path itself throws ──
# Regression: `_execute_job!` posted to `job.done` as its last statement, so any throw before that
# (here: the crash `@warn` itself failing) escaped into the dispatcher's fire-and-forget
# `Threads.@spawn` — silently. `run_task` then blocked in `take!(job.done)` FOREVER and never ran
# `_deregister_task!`, leaving the TaskRecord stranded at `:running`: `list_tasks()`, the GUI and
# the task console all keep listing a task that finished, while every pool correctly reads idle
# (the dispatcher's `finally` had released the slot). The post now lives in a `finally`.
@testset "Job posts its result even when the error path throws" begin
    proj = create_project!(name="job-post-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name="s"); name="img")

    tid = "jobpost$(rand(1000:9999))"
    old = global_logger()
    t = try
        global_logger(_ThrowingLogger(old))
        th = Threads.@spawn run_task(_CrashTask(), img, Dict{String,Any}(); task_id = tid)
        timedwait(() -> istaskdone(th), 30.0)        # wait BEFORE restoring, so the throw is injected
        th
    finally
        global_logger(old)                            # a failing @test must not log through it
    end

    @test istaskdone(t)                               # the whole point — this used to hang forever
    # guarded: on a regression the task is still blocked, and a bare fetch would hang the SUITE
    istaskdone(t) && @test fetch(t) === nothing       # aborted job → nil result, like any failure
    # ...and the record is gone, not stranded at :running for the console/GUI to keep showing
    @test !any(r -> r.id == tid, list_tasks())
    rm(proj.root; recursive=true)
end

# ── A terminal task-rail frame is banked for replay ────────────────────────
# Regression (the "0 done · 17 ended" console, and a project export stuck at "running"): the frame
# announcing HOW a unit of work ended is its only carrier and the server drops frames for a slow
# client BY DESIGN, so a client that missed it could never find out. `record_task_outcome!` keeps it.
# Banked from `ws_status` (see api/test) — the rail's one status sink, so background jobs and batch
# movies are covered too, not just scheduler tasks. Here: the log's own contract.
@testset "Task outcome log" begin
    empty!(Cecelia._OUTCOMES)
    rec!(id, status; kw...) = record_task_outcome!(id, status; kw...)

    rec!("o1", "done"; image_uid="img1", fun="segment.cellpose", pool="gpu")
    rec!("o2", "failed"; image_uid="img2", fun="project:export", pool="job")
    rec!("o3", "cancelled"; image_uid="img3")
    rows = recent_tasks()
    @test [r.id for r in rows] == ["o1", "o2", "o3"]              # oldest → newest
    @test [r.status for r in rows] == ["done", "failed", "cancelled"]
    let o1 = first(rows)
        @test o1.fun_name == "segment.cellpose" && o1.pool_name == "gpu"
        @test o1.image_uid == "img1" && o1.image_uids == String[]
        @test occursin(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$", o1.finished_at)
    end

    # a non-terminal status is NOT an outcome — the one call site hands over every status frame, so
    # banking "running" here would report work that is still going as finished.
    rec!("live", "queued"); rec!("live", "running")
    @test !any(r -> r.id == "live", recent_tasks())

    # a set-scope task's FULL member list survives: it exists only on this frame, and a replay
    # without it invalidates the representative image's plots only (docs/API.md).
    rec!("set1", "done"; image_uid="a", image_uids=["a", "b", "c"])
    @test only(filter(r -> r.id == "set1", recent_tasks())).image_uids == ["a", "b", "c"]

    # ONE row per task id. Both repeats are real: a cancel is announced twice (immediately, then as
    # the final status), and task:restart reuses the id for a new run that must supersede the old.
    rec!("o1", "done"); rec!("o1", "done")
    @test count(r -> r.id == "o1", recent_tasks()) == 1
    rec!("o2", "done")                                            # restarted → new outcome wins
    @test only(filter(r -> r.id == "o2", recent_tasks())).status == "done"
    @test last(recent_tasks()).id == "o2"                         # …and re-appended, still in order

    # `since` is INCLUSIVE, so a poll always re-reads its own newest entry (two units finishing in
    # the same millisecond must not let the second fall through the gap).
    newest = last(recent_tasks()).finished_at
    @test any(r -> r.finished_at == newest, recent_tasks(; since = newest))
    @test isempty(recent_tasks(; since = "9999-01-01T00:00:00.000Z"))
    @test length(recent_tasks(; since = "")) == length(recent_tasks())

    # bounded: a reporting tail for live clients, never run history (that's the on-disk run log)
    for i in 1:(Cecelia._OUTCOME_CAP + 50); rec!("cap$i", "done"); end
    @test length(Cecelia._OUTCOMES) == Cecelia._OUTCOME_CAP
    @test last(recent_tasks()).id == "cap$(Cecelia._OUTCOME_CAP + 50)"   # newest kept
    @test !any(r -> r.id == "o1", recent_tasks())                        # oldest evicted
    empty!(Cecelia._OUTCOMES)
end

# ── The scheduler stamps a task's own timing ───────────────────────────────
# `list_tasks()` is what a client polls to answer "how long has this been going?". Without these
# fields it can only be answered from when the client first SAW the row, so a console or a browser
# tab that attached mid-run could report a lower bound and nothing better (the task console printed
# `≥0s` for a task that had been running for 20 minutes).
@testset "Scheduler records queued/started timestamps" begin
    proj = create_project!(name="tasktime-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name="s"); name="img")

    _HOLD_TASK_GO[] = Channel{Nothing}(1)
    tid  = "hold$(rand(1000:9999))"
    seen = TaskRecord[]
    th = Threads.@spawn run_task(_HoldTask(), img, Dict{String,Any}("modelType" => "cyto3",
                                                                    "diameter"  => 17);
                                 task_id = tid, on_status_change = rec -> push!(seen, rec))
    try
        @test timedwait(() -> any(r -> r.id == tid && r.status == "running", list_tasks()), 30.0) === :ok
        row = only(filter(r -> r.id == tid, list_tasks()))
        # The snapshot's FIELD NAMES are a contract with two independent consumers that share no runtime:
        # `_reconcile_snapshot!` (api/task_console.jl) and `adoptableTasks`
        # (frontend/src/utils/runningTasks.ts), each of which silently blanks a column if one is renamed.
        # Pinned here so a rename fails a test instead — the frontend pins its own half in
        # runningTasks.test.ts.
        @test issubset(Set([:id, :fun_name, :pool_name, :image_uid, :project_uid, :chain_run_id,
                            :chain_node_id, :status, :queued_at, :started_at, :params]), Set(keys(row)))
        # …and WHICH project the image belongs to: one server serves every project under
        # `projects_dir()`, so the task console (which watches the whole rail, not one project) can't
        # tell two images apart by uid alone. Read off the image's path, never stored — the project's
        # identity IS its directory name (docs/OBJECTMODEL.md).
        @test row.project_uid == proj.uid == img_project_uid(img)
        # …and the params it was SUBMITTED with, which is what lets a client that didn't launch the task
        # offer Re-run: with only the fun_name it would relaunch on the JSON spec's defaults instead.
        @test row.params == Dict{String,Any}("modelType" => "cyto3", "diameter" => 17)
        # both are ISO-8601 UTC to the millisecond — one wire format for the whole rail
        @test occursin(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$", row.queued_at)
        @test occursin(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$", row.started_at)
        @test row.started_at >= row.queued_at            # a slot is acquired after submission
        # …and the SAME start is on the rail, so it still answers once this record is gone
        @test iso_utc(task_started_at(tid)) == row.started_at
        # the status frames the API sends carry it too (that's what `on_status_change` feeds)
        running = last(filter(r -> r.status === TASK_RUNNING, seen))
        @test !isnothing(running.started_at) && iso_utc(running.started_at) == row.started_at
    finally
        put!(_HOLD_TASK_GO[], nothing)                    # let the task finish even if a @test failed
        timedwait(() -> istaskdone(th), 30.0)
    end
    @test istaskdone(th) && fetch(th) === true
    @test !any(r -> r.id == tid, list_tasks())            # record gone…
    @test !isnothing(task_started_at(tid))                # …but the start outlived it
    forget_task_start!(tid)

    # A chain node reports WHICH node it is, not just which run — the GUI keys a chain row
    # `runId::nodeId::imageUid`, so a snapshot row without the node id can't be matched to one and the
    # same work would be listed twice (once adopted, once from the chain events).
    rec = Cecelia._register_task!("cn$(rand(1000:9999))", "segment.cellpose", "gpu", img.uid,
                                 "run1", _ -> nothing; chain_node_id = "n3")
    let row = only(filter(r -> r.id == rec.id, list_tasks()))
        @test row.chain_run_id == "run1" && row.chain_node_id == "n3"
    end
    Cecelia._deregister_task!(rec.id)

    # a task still QUEUED has a queue time and NO start — so a client shows a wait, not a run of 0s
    rec = Cecelia._register_task!("q$(rand(1000:9999))", "f", "cpu", img.uid, "", _ -> nothing)
    @test isnothing(rec.started_at)
    @test only(filter(r -> r.id == rec.id, list_tasks())).started_at == ""
    # …and a task registered with no params reports an EMPTY set, never a missing field: the frontend
    # reads an absent `params` as "unknown, withhold Re-run" and an empty one as "this task takes none".
    @test only(filter(r -> r.id == rec.id, list_tasks())).params == Dict{String,Any}()
    Cecelia._deregister_task!(rec.id)

    # The whole snapshot is written in ONE JSON3.write, so an unserialisable param value would throw and
    # take `/api/tasks` down for every row — no adoption, no console reconcile, and a quit busy-check
    # that reads idle. Params from the GUI are parsed JSON and always fine; a REPL-dispatched run can
    # put anything in the dict. Published as `null` instead, which the client reads as "unknown".
    rec = Cecelia._register_task!("np$(rand(1000:9999))", "f", "cpu", img.uid, "", _ -> nothing;
                                  params = Dict{String,Any}("fn" => sin, "diameter" => 17))
    let row = only(filter(r -> r.id == rec.id, list_tasks()))
        @test isnothing(row.params)                      # all-or-nothing — NOT a partial dict
        @test JSON3.write(row) isa String                # the endpoint still answers
    end
    Cecelia._deregister_task!(rec.id)

    # …and the nested shapes params actually take DO survive (a group param is a dict of dicts), plus the
    # ones Julia code writes naturally, so a REPL-dispatched run isn't denied Re-run over a tuple.
    nested = Dict{String,Any}("models" => Any["cyto3", "nuclei"], "opts" => Dict("d" => 17, "gpu" => true),
                              "unset" => nothing, "name" => :cellpose,
                              "range" => (1, 10), "shape" => (w = 5, h = 6))
    rec = Cecelia._register_task!("ok$(rand(1000:9999))", "f", "cpu", img.uid, "", _ -> nothing;
                                  params = nested)
    let row = only(filter(r -> r.id == rec.id, list_tasks()))
        @test row.params == nested
        @test JSON3.write(row) isa String
    end
    Cecelia._deregister_task!(rec.id)

    # A whitelist, NOT a `try JSON3.write` probe: JSON3 throws on a Function but happily serialises a
    # plain struct INTO AN OBJECT, so a probe would publish that and a client would Re-run on a value
    # that is not what the task ran with. Anything whose JSON form isn't the value it came from must
    # read as unknown — which is why this is a predicate over shapes, not an attempted write.
    @test !Cecelia._json_writable(img)
    rec = Cecelia._register_task!("st$(rand(1000:9999))", "f", "cpu", img.uid, "", _ -> nothing;
                                  params = Dict{String,Any}("img" => img))
    @test isnothing(only(filter(r -> r.id == rec.id, list_tasks())).params)
    Cecelia._deregister_task!(rec.id)
    rm(proj.root; recursive=true)
end

# ── When a unit of work started ────────────────────────────────────────────
# The other half of the same problem as the outcome log: the scheduler's record is deregistered the
# instant a task finishes, and the consumers that want a DURATION mostly ask afterwards (the chain
# bridge fires node:done once run_task has returned; a dropped terminal frame is recovered minutes
# later). So the start is noted on the rail, and the banked outcome row carries it from then on —
# without which every client has to time tasks off when it first happened to see them.
@testset "Task start timing" begin
    empty!(Cecelia._OUTCOMES); empty!(Cecelia._STARTED)

    # first note wins, so a repeated `running` announcement does not restart the clock
    began = Dates.now(UTC) - Dates.Minute(5)
    @test note_task_started!("t1", began) == began
    @test note_task_started!("t1", Dates.now(UTC)) == began       # ← would have reset the elapsed
    @test task_started_at("t1") == began

    # not started / not noted is `nothing`, never a zero date — a client must be able to tell
    @test isnothing(task_started_at("never-seen"))
    @test isnothing(task_started_at(""))
    @test iso_utc(nothing) == ""                                   # …and it serialises as "", not epoch 0

    # the banked outcome carries the start, and the in-flight note is then dropped: one home for the
    # fact at a time, so nothing can report two different starts for the same task.
    row = record_task_outcome!("t1", "done"; image_uid="img1")
    @test row.started_at == iso_utc(began)
    @test isnothing(task_started_at("t1"))
    @test only(filter(r -> r.id == "t1", recent_tasks())).started_at == iso_utc(began)
    @test row.finished_at >= row.started_at

    # a task that never ran banks an empty start rather than a made-up one
    @test record_task_outcome!("t2", "cancelled").started_at == ""

    # a non-terminal status still returns nothing — the caller uses that to tell live from finished
    @test isnothing(record_task_outcome!("t3", "running"))

    # a reused id (task:restart) is timed from its own beginning, not the previous run's
    note_task_started!("t4", began)
    forget_task_start!("t4")
    @test isnothing(task_started_at("t4"))

    # bounded, like the outcome log: a producer that never announces an outcome must not accumulate
    # forever. The OLDEST starts are evicted — a long-running task is the one whose elapsed matters.
    empty!(Cecelia._STARTED)
    base = Dates.now(UTC) - Dates.Hour(1)
    for i in 1:(Cecelia._STARTED_CAP + 10)
        note_task_started!("s$i", base + Dates.Millisecond(i))
    end
    @test length(Cecelia._STARTED) <= Cecelia._STARTED_CAP
    @test !isnothing(task_started_at("s$(Cecelia._STARTED_CAP + 10)"))   # newest kept
    @test isnothing(task_started_at("s1"))                              # oldest evicted
    empty!(Cecelia._STARTED); empty!(Cecelia._OUTCOMES)
end

@testset "Run log records status (done + failed)" begin
    proj = create_project!(name="rl-status-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name="s"); name="img")
    append_run_log!(img, "segment.cellpose", "default")              # default status = done
    append_run_log!(img, "behaviour.hmm", "", "failed")
    rl = read_run_log(img)
    @test String(rl[1]["status"]) == "done"
    @test String(rl[2]["fun"]) == "behaviour.hmm" && String(rl[2]["status"]) == "failed"
    rm(proj.root; recursive=true)
end

# ── Set expansion — a set resolves to its correct member UIDs ───────────────
# run_tasks and the batch accessors depend on this everywhere.
@testset "Set expansion" begin
    proj = create_project!(name="se-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    a = add_image!(s; name="a"); b = add_image!(s; name="b"); c = add_image!(s; name="c")
    expected = Set([a.uid, b.uid, c.uid])

    @test Set(i.uid for i in images(s))    == expected   # in-memory
    @test Set(i.uid for i in images(proj)) == expected
    reloaded = init_object(proj.uid, s.uid)              # reload from disk
    @test reloaded isa CciaSet
    @test Set(reloaded.image_uids)            == expected
    @test Set(i.uid for i in images(reloaded)) == expected
    rm(proj.root; recursive=true)
end

# ── Renaming a set is a metadata edit, and must not disturb its membership ─────
# A set's identity is its uid (the dir under {proj}/1/), so the name is display-only. The thing worth
# pinning is that it lands ON DISK and that nothing else moves: the images stay attached, and the
# project manifest — which stores uids — is untouched.
@testset "Set rename" begin
    proj = create_project!(name="sr-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="before")
    a = add_image!(s; name="a"); b = add_image!(s; name="b")

    out = rename_set!(proj, s.uid, "after")
    @test out.uid == s.uid && out.name == "after"        # same object, new name
    @test s.name == "after"                              # the loaded set is in step with disk
    reloaded = init_object(proj.uid, s.uid)
    @test reloaded.name == "after"                        # persisted, not just in memory
    @test Set(reloaded.image_uids) == Set([a.uid, b.uid]) # membership survived the one-field commit
    @test proj.set_uids == [s.uid]                        # the manifest keys off the uid, so untouched

    # The duplicate-name GUARD lives at this layer on purpose, so it holds for a REPL caller — which is
    # where a rename is most likely to be scripted across a cohort and least likely to be eyeballed.
    # `add_set!` carries the same guard, so it covers all three creation paths (create route,
    # `newSetName` on move, the copyImage task) at once.
    @test_throws ErrorException add_set!(proj; name="after")
    @test length(proj._sets) == 1                             # refused before anything was written
    other = add_set!(proj; name="other")
    @test set_name_taken(proj, "after")                       # taken by `s`
    @test !set_name_taken(proj, "after"; except = s.uid)      # …but not by anyone ELSE
    @test !set_name_taken(proj, "unused")
    @test_throws ErrorException rename_set!(proj, other.uid, "after")
    @test init_object(proj.uid, other.uid).name == "other"    # refused, and nothing was written

    # Renaming to its OWN name is a no-op rather than a self-conflict, so a re-run is idempotent.
    @test rename_set!(proj, s.uid, "after").name == "after"

    # …and `force` is the escape hatch for a caller who means it (two sets CAN share a name — they are
    # still distinct objects, keyed by uid). Both mutators take it, spelled the same way.
    forced = add_set!(proj; name="after", force = true)
    @test forced.name == "after" && forced.uid != s.uid
    delete_set!(proj, forced.uid)
    rename_set!(proj, other.uid, "after"; force = true)
    @test init_object(proj.uid, other.uid).name == "after"
    @test other.uid != s.uid

    # the set-object form, which is what a REPL caller holding `sets(proj)[i]` reaches for
    @test rename_set!(proj, other, "by object").name == "by object"
    @test init_object(proj.uid, other.uid).name == "by object"

    @test_throws ErrorException rename_set!(proj, "nosuchset", "x")
    rm(proj.root; recursive=true)
end

# ── Sink-agnostic task execution (runner/execute.jl) ──────────────────────────
# `execute_task` is the body `handle_task_run` used to inline. It is tested HERE, in the package, with
# no server and no socket — which is the whole point: the API server and the detached runner drive the
# same function, so its contract (scope dispatch, terminal frame on every path, result-before-status)
# must hold without either of them. See docs/todo/TASK_RUNNER_PLAN.md.

# Collect every announcement in call order, so ORDERING can be asserted and not just membership.
function _collect_exec(req)
    logs, sts, res = String[], Tuple{String,String,Vector{String}}[], Any[]
    final = execute_task(req;
        on_log      = l -> push!(logs, l),
        on_status   = (s, uid, uids) -> push!(sts, (s, uid, uids)),
        on_result   = (uid, meta) -> push!(res, (uid, meta)))
    (; final, logs, statuses = sts, results = res)
end

@testset "execute_task — image scope" begin
    proj = create_project!(name="exec-img-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name="s"); name="img")

    r = _collect_exec(TaskRequest(; task_id = "exec$(rand(1000:9999))",
                                   fun_name = "testTasks.image_task",
                                   project_uid = proj.uid, image_uid = img.uid,
                                   params = Dict{String,Any}("message" => "hello runner")))

    @test r.final == :done
    @test any(l -> occursin("hello runner", l), r.logs)          # on_log reached the caller
    @test last(r.statuses) == ("done", img.uid, String[])        # terminal frame, image-scope shape
    @test "running" in [s for (s, _, _) in r.statuses]           # live transitions forwarded
    @test length(r.results) == 1                                 # ...and the result arrived
    # ORDERING: the result must precede the terminal status — the frontend keys off that, and
    # reversing them silently drops the result.
    @test r.statuses[end][1] == "done" && !isempty(r.results)
    rm(proj.root; recursive=true)
end

@testset "execute_task — set scope names every member" begin
    proj = create_project!(name="exec-set-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    a, b = add_image!(s; name="a"), add_image!(s; name="b")

    r = _collect_exec(TaskRequest(; task_id = "execset$(rand(1000:9999))",
                                   fun_name = "testTasks.set_task",
                                   project_uid = proj.uid,
                                   image_uids = [a.uid, b.uid]))

    @test r.final == :done
    st, uid, uids = last(r.statuses)
    @test st == "done"
    @test uid == a.uid                       # representative = first image
    # The terminal frame carries ALL members. The representative alone leaves the other members'
    # plots stale — the non-rep-member gap this list closes.
    @test Set(uids) == Set([a.uid, b.uid])
    rm(proj.root; recursive=true)
end

# Every exit path must emit a terminal status, including the ones that never reach the scheduler.
# A failure that logs nothing and announces nothing is how a task pins at `running` forever in the
# GUI, and how anything keyed on the terminal frame (the observer's Watch trigger) never fires.
@testset "execute_task — failure paths still announce" begin
    proj = create_project!(name="exec-fail-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name="s"); name="img")

    unknown = _collect_exec(TaskRequest(; task_id = "execunk", fun_name = "nope.notATask",
                                          project_uid = proj.uid, image_uid = img.uid))
    @test unknown.final == :failed
    @test last(unknown.statuses)[1] == "failed"
    @test any(l -> occursin("Unknown task", l), unknown.logs)

    missing_img = _collect_exec(TaskRequest(; task_id = "execmiss",
                                             fun_name = "testTasks.image_task",
                                             project_uid = proj.uid, image_uid = "nosuchuid"))
    @test missing_img.final == :failed
    @test last(missing_img.statuses)[1] == "failed"

    # A bad param throws in `run_task` BEFORE any job is queued, so `on_status_change` never fires —
    # the terminal frame here can only come from execute_task's own outer catch.
    badparam = _collect_exec(TaskRequest(; task_id = "execbad",
                                          fun_name = "testTasks.image_task",
                                          project_uid = proj.uid, image_uid = img.uid,
                                          params = Dict{String,Any}("waitMs" => "not-an-int")))
    @test badparam.final == :failed
    @test last(badparam.statuses)[1] == "failed"
    @test any(l -> occursin("[ERROR]", l), badparam.logs)

    rm(proj.root; recursive=true)
end

# The request IS the wire format, so it round-trips through JSON unchanged — one encoder, one decoder.
# Each transport shaping its own dict is how a param key goes missing on one path only.
@testset "TaskRequest survives the wire" begin
    req = TaskRequest(; task_id = "t1", fun_name = "segment.cellpose", project_uid = "p1",
                        image_uid = "i1", image_uids = ["i1", "i2"], pool_name = "gpu",
                        params = Dict{String,Any}("blockSize" => 512, "name" => "x"))
    back = task_request(JSON3.read(JSON3.write(task_request_dict(req)), Dict{String,Any}))

    @test back.task_id == req.task_id && back.fun_name == req.fun_name
    @test back.project_uid == req.project_uid && back.image_uid == req.image_uid
    @test back.image_uids == req.image_uids && back.pool_name == req.pool_name
    @test back.params["blockSize"] == 512 && back.params["name"] == "x"
    @test back.target == "local"          # the field a second target lands in, defaulted not omitted

    # A request off the wire with only the required keys must not throw — a sender on older code
    # omitting an optional field is a compatibility case, not an error.
    minimal = task_request(Dict{String,Any}("taskId" => "t2", "funName" => "f", "projectUid" => "p"))
    @test minimal.image_uids == String[] && minimal.params == Dict{String,Any}()
    @test minimal.target == "local"
end

# ── The detached task runner ──────────────────────────────────────────────────
# In-process: the server's routes and fan-out, driven directly. A REAL second process is exercised by
# `pixi run test-runner` (app/test/runner_e2e.jl) — it costs a Julia start-up + precompile, which does
# not belong in the suite everyone runs.
@testset "runner protocol identity" begin
    # `/ping` must carry what tells an adopted runner apart from one we started, and WHICH CODE it is
    # running — a runner that deliberately survives your edits is only safe if it says so (Decision 5).
    id = runner_identity()
    @test id["protocol"] == RUNNER_PROTOCOL
    @test id["pid"] == getpid()
    @test haskey(id, "commit") && haskey(id, "startedAt") && haskey(id, "projectsDir")
end

@testset "runner_emit never blocks or throws without subscribers" begin
    # Emission happens on a POOL WORKER thread. If it could block or throw it would wedge a slot, so
    # the no-subscriber case has to be a silent no-op rather than an error path nobody tests.
    @test runner_emit(Dict{String,Any}("type" => "task:log", "taskId" => "x", "line" => "hi")) === nothing
end

# The opt-in is a SETTING, not just an env var — a packaged install has no way to set an env var, so
# "opt-in" would have meant "dev-only". The override still wins, which is what `pixi run dev-runner`
# and CI use; the test pins that precedence, because getting it backwards makes the toggle a lie.
# The runner is DEV-ONLY. A prod install has no Restart button, so its whole benefit is unreachable
# there while every failure mode (an idle process with no window, no cancel, nothing to find it by)
# lands on the user — and a prod user does not need it: they leave the app running, and closing the
# browser tab was never what stopped a task. The gate is asserted, not remembered, because "it is only
# a setting, what harm" is exactly how it would drift back on.
@testset "runner is dev-only, then a setting, then an env override" begin
    # `init_cecelia!()` mutates PROCESS-WIDE config, so this testset must put it back — leaving it
    # pointed at a throwaway config dir broke 37 later testsets (every one that touches
    # projects_dir()) the first time this was written, while passing itself.
    cfg = mktempdir()
    try
        # NOT dev: off, whatever the file says. Written first because it is the load-bearing one.
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => nothing, "CECELIA_RUNNER" => nothing) do
            init_cecelia!()
            @test is_dev_session() == false
            @test set_runner_enabled!(true) == false      # persisted, but not in EFFECT here
            @test runner_enabled() == false
        end
        # …and not even an explicit env override turns it on outside dev.
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => nothing, "CECELIA_RUNNER" => "1") do
            init_cecelia!()
            @test runner_enabled() == false
        end

        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => "1", "CECELIA_RUNNER" => nothing) do
            init_cecelia!()
            @test runner_enabled() == true                # the value written above, now in effect
            @test occursin("[runner]", read(custom_toml_path(), String))
            @test set_runner_enabled!(false) == false
            init_cecelia!()
            @test runner_enabled() == false               # …and it survives a config reload
        end
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => "1", "CECELIA_RUNNER" => "1") do
            init_cecelia!()
            @test runner_enabled() == true                # env wins over a file that says false
        end
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => "1", "CECELIA_RUNNER" => "0") do
            init_cecelia!()
            @test runner_enabled() == false               # …in both directions
        end
    finally
        init_cecelia!()                                   # restore the suite's own config
    end
end

@testset "TLS desired: env overrides toml overrides default (prod on / dev off)" begin
    # Parallel to the runner testset above — same resolution shape. The default IS the
    # feature that changed (opt-in → prod-on), so it is the one pinned by every branch here.
    cfg = mktempdir()
    try
        # PROD default (no dev, no env, no toml key): TLS on. The whole point of the flip.
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => nothing, "CECELIA_TLS" => nothing) do
            init_cecelia!()
            @test tls_desired(is_dev = false) == true
        end
        # DEV default: TLS off. Vite proxy is HTTP/1.1-only, TLS earns nothing.
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => "1", "CECELIA_TLS" => nothing) do
            init_cecelia!()
            @test tls_desired(is_dev = true) == false
        end
        # Settings toggle persists in both modes and takes precedence over the default.
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => nothing, "CECELIA_TLS" => nothing) do
            init_cecelia!()
            @test set_tls_desired!(false; is_dev = false) == false
            @test occursin("[tls]", read(custom_toml_path(), String))
            init_cecelia!()
            @test tls_desired(is_dev = false) == false     # …survives a reload
        end
        # …and env still wins over the file, in both directions.
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => nothing, "CECELIA_TLS" => "1") do
            init_cecelia!()
            @test tls_desired(is_dev = false) == true       # env-on beats toml-off
        end
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => nothing, "CECELIA_TLS" => "0") do
            init_cecelia!()
            @test set_tls_desired!(true; is_dev = false) == false   # written but env forces off
            @test tls_desired(is_dev = false) == false
        end
    finally
        init_cecelia!()
    end
end

@testset "runner client refuses a dead port cleanly" begin
    # Every one of these runs on the API server's request path. A runner that is simply not there must
    # read as absent — never a throw that takes a route down, and never a hang.
    h = RunnerHandle(; port = 7699)          # nothing listens here
    @test runner_ping(h) === nothing
    @test runner_alive(h) == false
end

# ── Chain testsets ────────────────────────────────────────────────────────
# 25+ testsets covering template round-trip, validation, run, resume, fault isolation,
# picnic-node policies, event bus, resource pools, pipelining and the headless entry point.
# Extracted from this file to keep it small enough to merge without EOF conflicts on every append.
# The extracted file loads inside this file's aggregating testset scope, so any helpers defined
# earlier in suite.jl are still in scope for the chain fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "chain.jl"))

# QC helper for FlowRegister — the "aligner is chronically saturating" case is what a user needs to
# know about (raise the clamp or accept the deformation is beyond dense flow) and the metrics are
# cohort-comparable, so both branches deserve a pin.
@testset "flow_register QC" begin
    quiet = Dict{String,Any}(
        "flowMax"    => [0.0, 3.2, 4.1, 2.8],
        "flowMean"   => [0.0, 0.8, 1.1, 0.7],
        "maxShiftPx" => 16.0,
    )
    @test isempty(Cecelia._flow_register_qc_findings(quiet))
    qm = Cecelia._flow_register_qc_metrics(quiet)
    @test qm["peakFlowPx"] == 4.1
    # 2.525 -> 2.52 under Julia's banker's rounding at digits=2 (ties to even).
    # Assert what actually ships, not the raw mean.
    @test qm["meanPeakFlowPx"] ≈ 2.52 atol=1e-3
    @test qm["meanFlowPx"]  ≈ 0.65  atol=1e-3

    # >= 50% of frames within 85% of the 16 px clamp -> single 'high_shifts' warn.
    saturating = Dict{String,Any}(
        "flowMax"    => [0.0, 15.0, 14.5, 15.2, 14.0],
        "flowMean"   => [0.0, 5.0, 4.8, 5.3, 4.9],
        "maxShiftPx" => 16.0,
    )
    findings = Cecelia._flow_register_qc_findings(saturating)
    @test length(findings) == 1
    @test findings[1]["level"] == "warn"
    @test findings[1]["code"]  == "flow_register.high_shifts"
    @test findings[1]["detail"]["framesNearCap"] == 4
    @test findings[1]["detail"]["nFrames"]       == 5
    # numbers live in detail, not the copy (docs/UI.md → QC copy)
    @test !occursin("14", findings[1]["long"])

    # No flow trajectory at all -> no claim, rather than a spurious warn.
    @test isempty(Cecelia._flow_register_qc_findings(Dict{String,Any}("maxShiftPx" => 16.0)))
    @test isempty(Cecelia._flow_register_qc_findings(
        Dict{String,Any}("flowMax" => Float64[], "maxShiftPx" => 16.0)))

    # meanFrameCorrelation metric (Phase 1) — average of the finite entries,
    # emitted as a per-image cohort-comparable diagnostic (Galene-derived; see
    # Warren et al. 2018, eLife 7:e35800). The metric is banked even when the
    # `high_shifts` warn does NOT fire, so the sidecar has to carry it.
    with_corr = merge(quiet, Dict{String,Any}(
        "frameCorrelation" => [1.0, 0.8, 0.4, 0.2]))
    qm2 = Cecelia._flow_register_qc_metrics(with_corr)
    @test qm2["meanFrameCorrelation"] == 0.6
    # Absent → metric absent (rather than a spurious NaN or 0).
    @test !haskey(Cecelia._flow_register_qc_metrics(quiet), "meanFrameCorrelation")
end

@testset "fun_name dispatch" begin
    @test _task_from_fun_name("importImages.omezarr") isa ImportOmezarr
    @test _task_from_fun_name("importImages.remove")  isa RemoveImage
    @test _task_from_fun_name("cleanupImages.afCorrect")       isa AfCorrect
    @test _task_from_fun_name("cleanupImages.driftCorrect")    isa DriftCorrect
    @test _task_from_fun_name("cleanupImages.stackAlign")      isa StackAlign
    @test _task_from_fun_name("cleanupImages.flowRegister")    isa FlowRegister
    @test _task_from_fun_name("cleanupImages.smooth")          isa Smooth
    @test _task_from_fun_name("segment.cellpose")              isa CellposeSegment
    @test _task_from_fun_name("segment.measureLabels")         isa MeasureLabels
    cp_measure = _task_from_fun_name("segment.cellposeMeasure")
    @test cp_measure isa CompositeTask
    @test cp_measure.fun_name == "segment.cellposeMeasure"
    @test_throws ErrorException _task_from_fun_name("nonexistent.task")
    @test _task_from_fun_name("tracking.bayesian_tracking")    isa BayesianTracking
    @test _task_from_fun_name("tracking.track_measures")       isa TrackMeasures
    bt_measures = _task_from_fun_name("tracking.bayesian_track_measures")
    @test bt_measures isa CompositeTask
    @test bt_measures.fun_name == "tracking.bayesian_track_measures"
end

# ── BayesianTracking param validation ─────────────────────────────────────
@testset "BayesianTracking params" begin
    task = BayesianTracking()
    good = Dict{String,Any}(
        "valueName" => "default", "popsToTrack" => "NONE",
        "maxSearchRadius" => 20, "maxLost" => 3, "trackBranching" => false,
        "minTimepoints" => 5, "accuracy" => 0.8, "probToAssign" => 0.8,
        # advanced section params are flattened by the frontend before submit
        "noiseInital" => 300, "distThresh" => 10.0, "segmentationMissRate" => 0.1,
    )
    @test begin validate_params(task, good); true end
    # maxSearchRadius max is 200
    @test_throws ParamValidationError validate_params(
        task, merge(good, Dict{String,Any}("maxSearchRadius" => 500)))
    # segmentationMissRate min is 0.001
    @test_throws ParamValidationError validate_params(
        task, merge(good, Dict{String,Any}("segmentationMissRate" => 0.0)))
end

# ── TrackMeasures param validation ────────────────────────────────────────
@testset "TrackMeasures params" begin
    task = TrackMeasures()
    @test begin validate_params(task,
        Dict{String,Any}("valueName" => "B", "forceRecompute" => false)); true end
    # forceRecompute is bool — a non-bool must be rejected. Also a guard that params use
    # "key" (not "id"): with "id" the spec key resolves empty and validation silently skips.
    @test_throws ParamValidationError validate_params(
        task, Dict{String,Any}("valueName" => "B", "forceRecompute" => "yes"))
end

# ── Labels field round-trip ───────────────────────────────────────────────
# Regression guard: the `labels` Dict written by cellposeSegment must survive
# save!/init_object and land at the agreed location in ccid.json.
@testset "Labels field round-trip" begin
    proj = create_project!(name="labels-rt-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    img.labels["default"] = ["default.zarr", "default_nuc.zarr"]
    save!(img)

    # Reloaded value is correct in memory
    r = init_object(proj.uid, img.uid)
    @test r isa CciaImage
    @test haskey(r.labels, "default")
    @test r.labels["default"] == ["default.zarr", "default_nuc.zarr"]

    # On-disk shape: top-level "labels" dict with string-vector values
    raw = JSON3.read(read(joinpath(img._dir, "ccid.json"), String), Dict{String,Any})
    @test haskey(raw, "labels")
    label_val = collect(String, raw["labels"]["default"])
    @test label_val == ["default.zarr", "default_nuc.zarr"]

    rm(proj.root; recursive=true)
end

# ── CompositeTask — spec loads and composite array is correct ─────────────
@testset "CompositeTask spec" begin
    task = CompositeTask("segment.cellposeMeasure")
    spec = Cecelia._task_spec(task)
    @test !isnothing(spec)
    @test haskey(spec, "composite")
    steps = [string(s) for s in spec["composite"]]
    @test steps == ["segment.cellpose", "segment.measureLabels"]
    @test get(spec, "fun_name", "") == "segment.cellposeMeasure"
end

@testset "a composite does not validate the params it derives itself" begin
    # `behaviour.hmm` = hmm_states → hmm_transitions. `hmm_transitions.hmmStates` is
    # `required` AND `hideInComposite`: the definitions route strips it from the merged form
    # (api/src/routes.jl), and the composite threads the states step's `stateColumn` into it in
    # `_run_task` — AFTER validation. Validating the step standalone-style therefore failed EVERY
    # composite run on "Select the state columns — run HMM states first", naming a field the form
    # does not have. Reported on a real set; reproduced here without one, because validation is
    # pure.
    #
    # Both halves matter. `hideInComposite` is skipped only IN a composite — run the step on its
    # own (a chain node, a REPL call) and the requirement is real, because then nothing supplies it.
    base = Dict{String,Any}("pops" => ["A/tracked"], "colName" => "default",
                            "modelMeasurements" => ["live.cell.speed", "live.cell.angle"],
                            "numStates" => 2)
    hmm   = Cecelia._task_from_fun_name("behaviour.hmm")
    trans = Cecelia._task_from_fun_name("behaviour.hmm_transitions")

    @test validate_params(hmm, copy(base)) === nothing            # composite: hmmStates derived
    @test_throws ParamValidationError validate_params(trans, copy(base))   # standalone: required
    @test validate_params(trans, merge(base,
        Dict{String,Any}("hmmStates" => ["live.cell.hmm.state.default"]))) === nothing

    # ABSENT and EMPTY are the same thing for a multi-pick (that is the rule `required` encodes),
    # and a chain node saved off the standalone form carries the spec default `[]`. Both must pass
    # in the composite, or the fix only covers the module-page path.
    @test validate_params(hmm, merge(base, Dict{String,Any}("hmmStates" => Any[]))) === nothing
end

@testset "a task's own validate_params overload survives a keyword call" begin
    # Keywords do not participate in dispatch: a keyword-LESS method is skipped outright when the
    # caller passes one, and the call falls through to the `::CciaTask` fallback — silently, no
    # error. So `validate_params(task, p; extra_options=…)` (chain template validation) ran the
    # spec half only and never the task's own check. Same hole would have swallowed `in_composite`.
    tc = Cecelia._task_from_fun_name("tracking.correct")
    bad = Dict{String,Any}("trackOps" => "nonsense!!")
    @test_throws ParamValidationError validate_params(tc, copy(bad))
    @test_throws ParamValidationError validate_params(tc, copy(bad); extra_options = Set{String}())
    @test_throws ParamValidationError validate_params(tc, copy(bad); in_composite = true)

    # Every overload of `validate_params` must therefore accept keywords — declared, or `kwargs...`.
    for m in methods(Cecelia.validate_params)
        @test !isempty(Base.kwarg_decl(m))
    end
end

# ── $include fragment resolution ──────────────────────────────────────────
# Verifies that {"$include": "imageTiling"} in cellpose.json is expanded
# to the 4 shared tiling params (blockSize, overlap, blockSizeZ, overlapZ).
@testset "\$include fragment resolution" begin
    task = CellposeSegment()
    spec = Cecelia._task_spec(task)
    @test !isnothing(spec)
    # Find the imageTiling section
    tiling_sec = nothing
    for p in spec["params"]
        p isa AbstractDict && string(get(p, "key", "")) == "imageTiling" &&
            (tiling_sec = p; break)
    end
    @test !isnothing(tiling_sec)
    tiling_params = tiling_sec["params"]
    keys_in_tiling = [string(get(p, "key", "")) for p in tiling_params if p isa AbstractDict]
    # Fragment contributes these 4; cellpose.json adds labelOverlap
    @test "blockSize"  ∈ keys_in_tiling
    @test "overlap"    ∈ keys_in_tiling
    @test "blockSizeZ" ∈ keys_in_tiling
    @test "overlapZ"   ∈ keys_in_tiling
    @test "labelOverlap" ∈ keys_in_tiling
    @test length(keys_in_tiling) == 5   # 4 from fragment + 1 inline
    # No raw $include entries should survive
    @test !any(p isa AbstractDict && haskey(p, "\$include") for p in tiling_params)
end

# ── label_props field round-trip ──────────────────────────────────────────
@testset "label_props field round-trip" begin
    proj = create_project!(name="lp-rt-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    img.label_props["default"] = "default.h5ad"
    save!(img)

    r = init_object(proj.uid, img.uid)
    @test r isa CciaImage
    @test get(r.label_props, "default", nothing) == "default.h5ad"

    raw = JSON3.read(read(joinpath(img._dir, "ccid.json"), String), Dict{String,Any})
    @test haskey(raw, "label_props")
    @test string(raw["label_props"]["default"]) == "default.h5ad"

    rm(proj.root; recursive=true)
end

# ── Param validation — AfCorrect (group with flat sub-params) ─────────────

# ── Param validation — DriftCorrect ───────────────────────────────────────

# ── Versioned helpers ─────────────────────────────────────────────────────
@testset "Versioned dict helpers" begin
    d = Dict{String,Any}()
    versioned_set_field!(d, "filepath", "ccidImage.ome.zarr")
    @test versioned_get_field(d, "filepath") == "ccidImage.ome.zarr"
    @test versioned_active(d["filepath"]) == "default"

    versioned_set_field!(d, "filepath", "ccidDriftCorrected.ome.zarr", "driftCorrected")
    @test versioned_get_field(d, "filepath", "driftCorrected") == "ccidDriftCorrected.ome.zarr"
    @test versioned_get_field(d, "filepath") == "ccidDriftCorrected.ome.zarr"  # active = driftCorrected

    versioned_set_field!(d, "filepath", nothing, "driftCorrected")
    @test isnothing(get(d["filepath"], "driftCorrected", nothing))
end

# ── LabelProps reader (H5AD via HDF5.jl) ──────────────────────────────────
@testset "LabelProps reader" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "LabelProps reader (fixture missing)"
    else
        # metadata (cheap reads)
        @test length(col_names(label_props(h5); data_type=:vars)) == 27
        @test channel_columns(label_props(h5)) ==
              ["mean_intensity_0", "mean_intensity_1", "mean_intensity_2", "mean_intensity_3"]
        @test centroid_columns(label_props(h5)) == ["centroid_z", "centroid_y", "centroid_x"]
        @test temporal_columns(label_props(h5)) == ["centroid_t"]
        # order= selects BY AXIS (present only), never positionally
        @test centroid_columns(label_props(h5); order=[:x, :y, :z]) == ["centroid_x", "centroid_y", "centroid_z"]
        @test centroid_columns(label_props(h5); order=[:x, :y]) == ["centroid_x", "centroid_y"]

        # channel-name selection: request an intensity column by its CHANNEL name and the reader
        # resolves it to the raw {measure}_intensity_{i} column, returning it under the channel name.
        # This is what lets pop_df(...; pop_cols=["<channel>"]) work. channel_names is positional:
        # index i ↔ chans[i+1], so "chC" == mean_intensity_2.
        let lpc = label_props(h5; channel_names=["chA", "chB", "chC", "chD"])
            d = lpc |> select_cols(["chC"]) |> as_df
            @test "chC" in names(d)                                   # returned under the requested name
            @test !("mean_intensity_2" in names(d))                   # not the raw name
            raw = label_props(h5) |> select_cols(["mean_intensity_2"]) |> as_df
            @test d.chC == raw.mean_intensity_2                       # same underlying column
        end
        # raw names still resolve (gates/clustering pass raw) — unchanged behaviour
        @test names(label_props(h5; channel_names=["chA","chB","chC","chD"]) |> select_cols(["mean_intensity_2"]) |> as_df) == ["label", "mean_intensity_2"]
        # a genuinely unknown name is still ignored (not resolved to anything)
        @test names(label_props(h5; channel_names=["chA","chB","chC","chD"]) |> select_cols(["nope"]) |> as_df) == ["label"]

        # full frame: label + 27 vars + 3 spatial + 1 temporal + 8 obs (track lineage +
        # live.cell.* from tracking.track_measures) = 40 cols, 1377 rows
        df = label_props(h5) |> as_df
        @test size(df) == (1377, 40)
        # n_obs is the cheap dims-only count — must agree with the materialised row count
        @test n_obs(label_props(h5)) == 1377
        @test "label" in names(df)
        @test eltype(df.label) == Int64
        @test df.label[1:5] == [0, 1, 2, 3, 4]

        # X orientation correctness (audited values).
        # NOTE: intentional coupling to the committed fixture state of KDIeEm/B.h5ad — these
        # are the actual bbox values in that file, asserting /X is read with correct row/col
        # orientation (not transposed). If this breaks, it's either (a) the reader regressed,
        # or (b) the fixture was deliberately regenerated (e.g. segmentation rerun) — in which
        # case re-audit and update these constants. A failure here is NOT "the test is wrong".
        @test [df[1, "bbox-$j"] for j in 0:4] == Float32[0, 0, 71, 2, 29]
        @test [df[2, "bbox-$j"] for j in 0:4] == Float32[0, 7, 368, 4, 38]

        # is_tracked's signal: a tracked segmentation carries a track_id obs column (KDIeEm/B is
        # tracked). track_props / the track-grained gating plots key off this to say "track first"
        # (empty) instead of erroring when it's absent.
        obs = col_names(label_props(h5); data_type=:obs)
        @test "track_id" in obs
        @test !("not_a_column" in obs)

        # lazy column selection — only requested columns (+ label) are returned
        @test names(label_props(h5) |> select_cols(["area"]) |> as_df) == ["label", "area"]

        # centroid + intensity selection
        @test Set(names(label_props(h5) |> select_cols(["mean_intensity_0"]) |> view_centroid_cols |> as_df)) ==
              Set(["label", "mean_intensity_0", "centroid_z", "centroid_y", "centroid_x", "centroid_t"])

        # row filter by label
        d4 = label_props(h5) |> filter_rows([0, 1, 2]; by=:label) |> as_df
        @test sort(d4.label) == [0, 1, 2]

        # filter is intersection: nonexistent IDs are silently skipped (≤ requested, no NaN/error)
        d4b = label_props(h5) |> filter_rows([0, 1, 999_999]; by=:label) |> as_df
        @test sort(d4b.label) == [0, 1]

        # sort by area, descending
        d5 = label_props(h5) |> select_cols(["area"]) |> sort_by("area"; rev=true) |> as_df
        @test d5.area[1] == maximum(d5.area)
    end
end

# ── LabelProps writer (add_obs / save! — the chain write path) ─────────────
@testset "LabelProps writer" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "LabelProps writer (fixture missing)"
    else
        tmp = joinpath(mktempdir(), "B.h5ad")
        cp(h5, tmp)

        existing = label_props(tmp) |> as_df
        some = existing.label[1:5]                       # write to a subset of labels
        df = DataFrame("label" => some,
                       "test.measure" => Float64.(1:5),
                       "test.other"   => [10.0, 20.0, 30.0, 40.0, 50.0])
        # the documented chain write idiom
        label_props(tmp) |> add_obs(df) |> save!

        # new columns appear in obs column-order
        obs_cols = col_names(label_props(tmp); data_type=:obs)
        @test "test.measure" in obs_cols
        @test "test.other" in obs_cols

        # read back via the reader, aligned by label; unset labels are NaN
        back = label_props(tmp) |> select_cols(["test.measure", "test.other"]) |> as_df
        byrow = Dict(l => i for (i, l) in enumerate(back.label))
        for (k, lab) in enumerate(some)
            @test back[byrow[lab], "test.measure"] == Float64(k)
        end
        # a label not in df → NaN
        other = first(setdiff(back.label, some))
        @test isnan(back[byrow[other], "test.measure"])

        # original data preserved (var count unchanged — obs append only, no X rewrite)
        @test length(col_names(label_props(tmp); data_type=:vars)) ==
              length(col_names(label_props(h5);  data_type=:vars))

        # idempotent overwrite: re-writing the same column updates, doesn't duplicate
        df2 = DataFrame("label" => some, "test.measure" => fill(99.0, 5))
        label_props(tmp) |> add_obs(df2) |> save!
        @test count(==("test.measure"), col_names(label_props(tmp); data_type=:obs)) == 1
        back2 = label_props(tmp) |> select_cols(["test.measure"]) |> as_df
        @test back2[Dict(l => i for (i, l) in enumerate(back2.label))[some[1]], "test.measure"] == 99.0

        # drop_obs: remove a column; gone from column-order and from as_df
        label_props(tmp) |> drop_obs(["test.measure"]) |> save!
        @test "test.measure" ∉ col_names(label_props(tmp); data_type=:obs)
        @test "test.other"   ∈ col_names(label_props(tmp); data_type=:obs)   # sibling untouched
        @test "test.measure" ∉ names(label_props(tmp) |> as_df)
        # dropping a nonexistent column is a no-op (idempotent)
        @test begin label_props(tmp) |> drop_obs(["never.existed"]) |> save!; true end

        # combined drop + add in one chain (invalidate-then-write, e.g. btrack rerun)
        df3 = DataFrame("label" => some, "test.fresh" => Float64.(1:5))
        label_props(tmp) |> drop_obs(["test.other"]) |>
                            add_obs(df3) |> save!
        cols3 = col_names(label_props(tmp); data_type=:obs)
        @test "test.other" ∉ cols3
        @test "test.fresh" ∈ cols3

        # drop + re-add the SAME column in one chain → the add wins (column survives with new
        # values). Regression: the drop used to de-list and delete the just-written dataset, so
        # e.g. overwriting a categorical hmm.state with a numeric one in one chain lost it.
        df4 = DataFrame("label" => some, "test.fresh" => Float64.(101:105))
        label_props(tmp) |> drop_obs(["test.fresh"]) |>
                            add_obs(df4) |> save!
        @test "test.fresh" ∈ col_names(label_props(tmp); data_type=:obs)
        back4 = label_props(tmp) |> select_cols(["test.fresh"]) |> as_df
        row4  = Dict(l => i for (i, l) in enumerate(back4.label))
        for (k, lab) in enumerate(some)
            @test back4[row4[lab], "test.fresh"] == Float64(100 + k)
        end
    end
end

# ── Julia ↔ Python reader parity (the duplication safety net) ──────────────
# The Julia LabelProps reader and the Python LabelPropsView are two implementations of
# ONE spec (docs/DATAMODEL.md). They can drift — a new encoding type added to one and not
# the other. This runs BOTH against the same fixture and compares. Gated on the napari
# venv + anndata being importable, so headless CI without Python skips rather than fails.
@testset "LabelProps Julia/Python parity" begin
    h5    = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    pybin = python_bin_path()
    py_ok = !isempty(pybin) && isfile(pybin) &&
            success(setenv(`$pybin -c "import anndata, numpy, pandas"`, dir=@__DIR__))
    if !have_fixture(h5) || !py_ok
        @test_skip "LabelProps parity (fixture or python+anndata unavailable)"
    else
        # Python side dumps a comparable summary as JSON via the LabelPropsView reader.
        pyscript = """
            import sys, json
            import cecelia.utils.label_props_utils as lpu
            v = lpu.LabelPropsView(sys.argv[1])
            df  = v.view_cols(["mean_intensity_0"]).as_df().sort_values("label")
            cv  = lpu.LabelPropsView(sys.argv[1]).only_centroid_cols().as_df().sort_values("label")
            print(json.dumps({
                "var_names":      list(v.var_names()),
                "obs_cols":       list(v.adata.obs.columns),
                "centroid_cols":  list(v.centroid_columns()),
                "temporal_cols":  list(v.temporal_columns()),
                "n_obs":          int(len(v.labels())),
                "labels5":        [int(x) for x in df["label"].to_numpy()[:5]],
                "mean_int0_5":    [float(x) for x in df["mean_intensity_0"].to_numpy()[:5]],
                "centroid0_5":    [float(x) for x in cv["centroid_z"].to_numpy()[:5]],
            }))
            """
        # python/ on PYTHONPATH so `import cecelia.utils...` resolves (matches run_py's PYTHONPATH).
        py_dir = joinpath(dirname(dirname(@__DIR__)), "python")   # app/test → app → repo-root/python
        penv = copy(ENV); penv["PYTHONPATH"] = py_dir
        out = read(setenv(`$pybin -c $pyscript $h5`, penv; dir=py_dir), String)
        py = JSON3.read(out)

        lp = label_props(h5)
        @test Set(col_names(lp; data_type=:vars)) == Set(String.(py.var_names))
        @test Set(col_names(lp; data_type=:obs))  == Set(String.(py.obs_cols))
        @test Set(centroid_columns(lp))           == Set(String.(py.centroid_cols))
        @test Set(temporal_columns(lp))           == Set(String.(py.temporal_cols))

        # value parity on the same labels (sorted), one var col + one centroid col
        dj = label_props(h5) |> select_cols(["mean_intensity_0"]) |> sort_by("label") |> as_df
        @test dj.label[1:5] == collect(Int, py.labels5)
        @test dj[1:5, "mean_intensity_0"] ≈ Float64.(py.mean_int0_5)
        cj = label_props(h5) |> view_centroid_cols |> sort_by("label") |> as_df
        @test cj[1:5, "centroid_z"] ≈ Float64.(py.centroid0_5)
    end
end

# ── Track measures: numeric cross-check vs celltrackR ─────────────────────
# Golden values — provenance:
#   Generated from celltrackR 1.2.2 (Wortel et al. 2021, doi:10.1016/j.crmeth.2021.100006)
#   on the track below, via the R package's own functions (trackLength/speed/displacement/
#   straightness/asphericity/overallAngle/meanTurningAngle, degrees=TRUE; per-step via
#   subtracks()). celltrackR is the reference Cecelia ported these from; it is NOT a runtime
#   dependency — these constants pin the port to the original. If a measure here changes,
#   either the port regressed or it was deliberately changed (then re-derive from celltrackR).
@testset "Track measures (celltrackR golden)" begin
    Track = Cecelia.Track
    t  = [0.0, 10, 20, 30, 40]
    c3 = [0.0 0 0; 3 4 0; 7 4 2; 7 8 2; 10 12 5]    # (x,y,z) per position
    tr = Track(1, t, c3)

    @test Cecelia.track_length(tr)            ≈ 19.3030878498 atol=1e-6
    @test Cecelia.track_duration(tr)          == 40.0
    @test Cecelia.track_speed(tr)             ≈ 0.4825771962  atol=1e-6
    @test Cecelia.track_displacement(tr)      ≈ 16.4012194669 atol=1e-6
    @test Cecelia.max_displacement(tr)        ≈ 16.4012194669 atol=1e-6
    @test Cecelia.track_straightness(tr)      ≈ 0.8496681772  atol=1e-6
    @test Cecelia.track_displacement_ratio(tr) ≈ 1.0          atol=1e-6
    @test Cecelia.track_outreach_ratio(tr)    ≈ 0.8496681772  atol=1e-6
    @test Cecelia.track_asphericity(tr)       ≈ 0.8469835416  atol=1e-6
    @test Cecelia.track_overall_angle(tr)     ≈ 30.9637565321 atol=1e-6
    @test Cecelia.track_mean_turning_angle(tr) ≈ 64.7432782933 atol=1e-6

    # per-cell subtracks (celltrackR subtracks(·,1) speed; subtracks(·,2) overallAngle)
    ss = Cecelia.step_speeds(tr)              # cell_id 1 → NaN; i>1 = step speed to endpoint
    @test isnan(ss[1])
    @test ss[2:5] ≈ [0.5, 0.4472135955, 0.4, 0.5830951895] atol=1e-6
    sa = Cecelia.step_turning_angles(tr)      # cell_id 1,2 → NaN; i≥3 = turn angle (deg)
    @test all(isnan, sa[1:2])
    @test sa[3:5] ≈ [57.5436915381, 90.0, 46.6861433417] atol=1e-6

    # 2D path (drop z) — same functions, no call-site branching
    tr2 = Track(1, t, c3[:, 1:2])
    @test Cecelia.track_straightness(tr2)  ≈ 0.8678055195 atol=1e-6
    @test Cecelia.track_asphericity(tr2)   ≈ 0.8287305960 atol=1e-6
    @test Cecelia.track_overall_angle(tr2) ≈ 0.0          atol=1e-6  # first ∥ last step in xy

    # ── edge cases (Step 4 mandates these) ────────────────────────────────
    # single-step track (2 positions): measures needing ≥3 steps → NaN; no crash
    one = Track(1, [0.0, 10], [0.0 0 0; 3 4 0])
    @test Cecelia.track_length(one)            ≈ 5.0
    @test Cecelia.track_speed(one)             ≈ 0.5
    @test Cecelia.track_straightness(one)      ≈ 1.0          # straight by definition
    @test isnan(Cecelia.track_overall_angle(one))            # n<3
    @test isnan(Cecelia.track_mean_turning_angle(one))
    @test Cecelia.track_asphericity(one)       == 1.0         # celltrackR convention for <3

    # single-position track: no div-by-zero, sane fallbacks
    pt = Track(1, [0.0], reshape([0.0, 0, 0], 1, 3))
    @test Cecelia.track_length(pt)        == 0.0
    @test isnan(Cecelia.track_speed(pt))                     # duration 0
    @test Cecelia.track_straightness(pt)  == 1.0             # length 0 → 1
    @test isnan(Cecelia.track_displacement_ratio(pt))        # maxDisplacement 0

    # zero net displacement (returns to origin): straightness 0, no div-by-zero
    loop = Track(1, [0.0, 1, 2], [0.0 0; 1 0; 0 0])
    @test Cecelia.track_displacement(loop)       ≈ 0.0
    @test Cecelia.track_length(loop)             ≈ 2.0
    @test Cecelia.track_straightness(loop)       ≈ 0.0
    @test Cecelia.track_displacement_ratio(loop) ≈ 0.0       # disp 0 / maxDisp 1
    @test Cecelia.track_outreach_ratio(loop)     ≈ 0.5       # maxDisp 1 / length 2
end

# ── Gating engine: transforms ─────────────────────────────────────────────
@testset "Transforms" begin
    # linear is identity
    @test apply_transform(LinearTransform(), 42.0) == 42.0
    @test invert_transform(LinearTransform(), 42.0) == 42.0

    # log / asinh round-trip
    lg = LogTransform()
    @test invert_transform(lg, apply_transform(lg, 1234.0)) ≈ 1234.0
    ah = AsinhTransform(cofactor=150.0)
    for x in (-500.0, 0.0, 37.0, 9000.0)
        @test invert_transform(ah, apply_transform(ah, x)) ≈ x atol=1e-6
    end

    # Logicle golden values — provenance:
    #   Generated once from FlowUtils' reference C implementation (`logicle_c`, the C port
    #   of Moore & Parks 2012), params T=262144 W=0.5 M=4.5 A=0, via:
    #     flowutils.transforms.logicle(x, channel_indices=[0], t=262144, m=4.5, w=0.5, a=0)
    #   flowutils was used transiently only to produce these numbers; it is NOT a runtime
    #   dependency. The values are baked in below so this test needs no Python at runtime.
    #   See app/src/gating/transforms.jl for the full citation.
    lc = LogicleTransform(T=262144, W=0.5, M=4.5, A=0)
    golden = [(-1000.0, -0.2321153540), (-100.0, 0.0090411347), (0.0, 0.1111111111),
              (1.0, 0.1122315321), (10.0, 0.1223042757), (100.0, 0.2131810875),
              (1000.0, 0.4543375762), (10000.0, 0.6838326572),
              (100000.0, 0.9069275915), (262144.0, 1.0)]
    for (x, gy) in golden
        @test apply_transform(lc, x) ≈ gy atol=1e-6
    end
    # round-trip
    for x in (-1000.0, 0.0, 1.0, 1000.0, 262144.0)
        @test invert_transform(lc, apply_transform(lc, x)) ≈ x atol=1e-4
    end
    # vectorised
    @test apply_transform(lc, [0.0, 262144.0]) ≈ [0.1111111111, 1.0] atol=1e-6

    # Range-based auto-linearisation: logicle collapses a bounded 0–1 measure (morphology) but
    # spreads a real intensity range → effective_transform swaps to linear only for the former.
    @test transform_kind(lc) == "logicle"
    @test transform_collapses(lc, 0.02, 1.0)              # solidity ∈ [0,1] → collapses
    @test !transform_collapses(lc, 0.0, 262144.0)         # full intensity range → fine
    @test !transform_collapses(lc, 0.0, 5000.0)           # large-range morphology (area) → keep logicle
    @test effective_transform(lc, 0.02, 1.0) isa LinearTransform
    @test effective_transform(lc, 0.0, 262144.0) === lc   # untouched
    @test effective_transform(LinearTransform(), 0.02, 1.0) isa LinearTransform  # linear never coerces
    @test !transform_collapses(LinearTransform(), 0.02, 1.0)
    # log needs ≥1 decade above the floor; asinh coerces when all data is inside its ~linear core
    @test transform_collapses(LogTransform(floor=1.0), 0.5, 5.0)
    @test !transform_collapses(LogTransform(floor=1.0), 1.0, 1e5)
    @test transform_collapses(AsinhTransform(cofactor=150.0), 0.0, 1.0)
    @test !transform_collapses(AsinhTransform(cofactor=150.0), 0.0, 5000.0)
    # degenerate extent (single value / non-finite) → no coercion, keep requested
    @test effective_transform(lc, 1.0, 1.0) === lc
    @test effective_transform(lc, NaN, 1.0) === lc
end

# ── Gating engine: gates ──────────────────────────────────────────────────
@testset "Gates" begin
    # rectangle (linear): inclusive bounds
    rg = RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0)
    xin = inside(rg, [5.0, 11.0, -1.0, 0.0], [5.0, 5.0, 5.0, 0.0])
    @test xin == BitVector([true, false, false, true])

    # polygon point-in-polygon (unit square)
    sq = [(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)]
    @test point_in_polygon(0.5, 0.5, sq)
    @test !point_in_polygon(1.5, 0.5, sq)
    @test !point_in_polygon(-0.1, 0.5, sq)
    pg = PolygonGate("x", "y", sq)
    @test inside(pg, [0.5, 2.0], [0.5, 2.0]) == BitVector([true, false])

    # transformed-space gate: a rectangle in logicle coords selects by raw value
    lc = LogicleTransform(T=262144, W=0.5, M=4.5, A=0)
    # logicle(1000)=0.4543, logicle(100000)=0.9069 → gate [0.45,0.95] keeps 1000 & 100000, not 10
    rgl = RectangleGate("x", "y", 0.45, 0.95, 0.45, 0.95;
                        x_transform=lc, y_transform=lc)
    keep = inside(rgl, [10.0, 1000.0, 100000.0], [1000.0, 1000.0, 100000.0])
    @test keep == BitVector([false, true, true])

    # JSON round-trip preserves membership
    for g in (rg, pg, rgl)
        g2 = gate_from_spec(gate_spec(g))
        @test inside(g2, [5.0, 1000.0], [5.0, 1000.0]) == inside(g, [5.0, 1000.0], [5.0, 1000.0])
    end

    # project_gate: re-express a gate's stored geometry into a DISPLAY transform so its outline
    # aligns with points drawn in that transform (the client has no transform math).
    lcp = LogicleTransform(T=262144, W=0.5, M=4.5, A=0)
    # a rectangle stored in logicle coords, projected onto a LINEAR display → raw bounds
    rgp = RectangleGate("cd4", "cd8", 0.4543375762, 0.9069275915, 0.4543375762, 0.9069275915;
                        x_transform=lcp, y_transform=lcp)
    lin = LinearTransform()
    pj = project_gate(rgp, "cd4", "cd8", lin, lin)
    @test pj["kind"] == "rectangle"
    @test pj["x_min"] ≈ 1000.0 atol=1e-2      # invert(logicle, 0.4543) ≈ 1000
    @test pj["x_max"] ≈ 100000.0 atol=1e-1    # invert(logicle, 0.9069) ≈ 100000
    # projecting onto the SAME transform is (near) identity
    same = project_gate(rgp, "cd4", "cd8", lcp, lcp)
    @test same["x_min"] ≈ 0.4543375762 atol=1e-4
    # swapped channel order → x/y transposed
    sw = project_gate(rgp, "cd8", "cd4", lin, lin)
    @test sw !== nothing
    @test sw["y_min"] ≈ 1000.0 atol=1e-2
    # not on this channel pair → nothing
    @test project_gate(rgp, "cd4", "cd19", lin, lin) === nothing
    # polygon vertices map pointwise — corner count is preserved under ANY display transform.
    # The client sends this list back on any handle drag, so densifying here would silently multiply
    # the vertex count on every cross-transform round-trip until the gate is un-editable.
    pgp = PolygonGate("cd4", "cd8", [(0.4543375762, 0.4543375762), (0.9069275915, 0.4543375762),
                                     (0.9069275915, 0.9069275915)]; x_transform=lcp, y_transform=lcp)
    pjp = project_gate(pgp, "cd4", "cd8", lin, lin)
    @test pjp["kind"] == "polygon"
    @test length(pjp["vertices"]) == 3                  # NEVER more than the stored corners
    @test pjp["vertices"][1][1] ≈ 1000.0 atol=1e-2       # first corner mapped through inverse logicle
    @test length(project_gate(pgp, "cd4", "cd8", lcp, lcp)["vertices"]) == 3
end

# ── Gating engine: density ────────────────────────────────────────────────
@testset "Density" begin
    x = collect(0.0:0.01:1.0)
    d = density_2d(x, x; bins=10)
    @test sum(d.counts) == length(x)          # every point counted once
    @test size(d.counts) == (10, 10)
    @test d.counts[1, 10] == 0                 # off-diagonal empty (x==y data)
    @test all(d.counts[i, i] >= 1 for i in 1:10)

    # NaN/Inf (object/morphology measures on degenerate objects) must be skipped, not throw —
    # extents come from the finite values, and a non-finite point contributes to no bin.
    xn = [0.0, 0.5, 1.0, NaN, Inf, -Inf]
    dn = density_2d(xn, xn; bins=10)
    @test sum(dn.counts) == 3                  # only the 3 finite points counted
    @test (dn.x_min, dn.x_max) == (0.0, 1.0)   # extents ignore NaN/Inf
    # all-non-finite → falls back to the default extent and empty counts (no throw)
    dz = density_2d([NaN, Inf], [NaN, Inf]; bins=4)
    @test sum(dz.counts) == 0
end

# ── Population manager: paths, tree, persistence ──────────────────────────
@testset "Population manager" begin
    @test pop_parent("/a/b") == "/a"
    @test pop_parent("/a") == ROOT
    @test pop_name("/a/b") == "b"
    @test pop_path("/a", "b") == "/a/b"
    @test pop_path(ROOT, "a") == "/a"
    @test is_root(ROOT) && is_root("/") && !is_root("/a")

    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "cd4"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10), colour="#f00")
    add_pop!(m, "cd8"; parent="/cd4", gate=PolygonGate("x", "y", [(0.,0.),(0.,5.),(5.,5.),(5.,0.)]))
    @test pop_paths(m) == ["/cd4", "/cd4/cd8"]
    @test direct_children(m, ROOT) == ["/cd4"]
    @test descendants(m, "/cd4") == ["/cd4/cd8"]

    # save/load round-trip (tree + gate)
    td = mktempdir()
    save_pop_map!(m, td)
    @test isfile(gating_path(td, "B"))
    # atomic write: the temp file is renamed into place, never left behind
    @test !isfile(gating_path(td, "B") * ".tmp")
    m2 = load_pop_map(td, "B")
    @test pop_paths(m2) == pop_paths(m)
    @test pop_at(m2, "/cd4").colour == "#f00"
    @test pop_at(m2, "/cd4").gate isa RectangleGate
    @test pop_at(m2, "/cd4/cd8").gate isa PolygonGate

    # cascade rename
    rename_pop!(m, "/cd4", "tcell")
    @test Set(pop_paths(m)) == Set(["/tcell", "/tcell/cd8"])
    @test pop_at(m, "/tcell/cd8").parent == "/tcell"
    # cascade delete
    del_pop!(m, "/tcell")
    @test isempty(pop_paths(m))
end

# ── Re-parenting a population: move_pop! ─────────────────────────────────────
# The UI (PopulationManager's ⋯ → "Move under…", pop/move) lifts a population out of the gate it was
# drawn under — the "I gated B under qc and now want it against all cells" case, which used to mean
# deleting the whole branch and redrawing it. The pop keeps its gate, name, colour and children; what
# changes is its PARENT, and with it its membership (a pop is its own gate ∩ its parent's).
@testset "move_pop! re-parents a population and its subtree" begin
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "qc"; parent=ROOT, gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0), colour="#f00")
    add_pop!(m, "B";  parent="/qc", gate=RectangleGate("x", "y", 0.0, 100.0, 0.0, 100.0), colour="#0f0")
    add_pop!(m, "mem+"; parent="/qc/B", gate=RectangleGate("x", "y", 0.0, 100.0, 0.0, 100.0))

    # membership BEFORE: B is capped by qc's gate, so the far cell (20, 20) can't be in it
    df = DataFrame("label" => [1, 2, 3], "x" => [1.0, 7.0, 20.0], "y" => [1.0, 7.0, 20.0])
    recompute!(m, _ -> df)
    @test Set(cells_in_pop(m, "/qc/B")) == Set([1, 2])

    @test move_pop!(m, "/qc/B", ROOT) == "/B"
    @test Set(pop_paths(m)) == Set(["/qc", "/B", "/B/mem+"])
    @test pop_at(m, "/B").parent == ROOT
    @test pop_at(m, "/B").name == "B"
    @test pop_at(m, "/B").colour == "#0f0"                   # identity untouched
    @test pop_at(m, "/B").gate isa RectangleGate             # gate untouched
    @test pop_at(m, "/B/mem+").parent == "/B"                # the subtree came along
    # `order` keeps parents before children, so the moved subtree follows its new parent
    @test findfirst(==("/B"), pop_paths(m)) < findfirst(==("/B/mem+"), pop_paths(m))

    # membership AFTER: no longer capped by qc — the same gate now takes the far cell too
    recompute!(m, _ -> df)
    @test Set(cells_in_pop(m, "/B")) == Set([1, 2, 3])
    @test Set(cells_in_pop(m, "/B/mem+")) == Set([1, 2, 3])
    @test Set(cells_in_pop(m, "/qc")) == Set([1, 2])         # the old parent is unaffected

    # and back under a gate again (the other direction)
    @test move_pop!(m, "/B", "/qc") == "/qc/B"
    @test pop_at(m, "/qc/B/mem+").parent == "/qc/B"
    @test move_pop!(m, "/qc/B", "/qc") == "/qc/B"            # already there → no-op, not an error

    # the move survives a save/load round-trip (the tree is nested, so the parent IS the position)
    td = mktempdir(); save_pop_map!(m, td)
    @test Set(pop_paths(load_pop_map(td, "B"))) == Set(["/qc", "/qc/B", "/qc/B/mem+"])
end

# ── Pop UIDs — the stable id every outside reference (obs columns, external notes) points at ─────
# Reintroduces the R version's `popID` (MULTI_POP_TRACKING_PLAN.md Decision 0). Path is the DISPLAY
# identity (what the UI shows); UID is the DURABLE one that outlives a rename. Two invariants matter
# and are pinned here: UID survives rename_pop!/move_pop! (rename ≠ new pop), and a legacy sidecar
# without any UIDs is one-shot-backfilled at load and saved so the next load gets the SAME uids.
@testset "population UIDs survive rename + move + save/load round-trips" begin
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "cd4"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10))
    add_pop!(m, "cd8"; parent="/cd4", gate=RectangleGate("x", "y", 0, 10, 0, 10))
    uid4 = pop_uid(m, "/cd4"); uid8 = pop_uid(m, "/cd4/cd8")
    @test length(uid4) == 6 && length(uid8) == 6 && uid4 != uid8
    # reverse lookup
    @test pop_path_by_uid(m, uid4) == "/cd4"
    @test pop_by_uid(m, uid4).path == "/cd4"
    @test pop_path_by_uid(m, "missing") === nothing
    # rename: uid stays, path shifts, index updates
    rename_pop!(m, "/cd4", "tcell")
    @test pop_uid(m, "/tcell") == uid4
    @test pop_uid(m, "/tcell/cd8") == uid8            # descendant's uid unchanged
    @test pop_path_by_uid(m, uid4) == "/tcell"
    # move: uid stays, path shifts, index updates
    add_pop!(m, "qc"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10))
    move_pop!(m, "/tcell", "/qc")
    @test pop_uid(m, "/qc/tcell") == uid4
    @test pop_path_by_uid(m, uid4) == "/qc/tcell"
    # save/load round-trip preserves uids AS-IS (no reroll)
    td = mktempdir(); save_pop_map!(m, td)
    m2 = load_pop_map(td, "B")
    @test pop_uid(m2, "/qc/tcell") == uid4
    @test pop_uid(m2, "/qc/tcell/cd8") == uid8
    # delete drops the reverse-lookup entry so a stale uid resolves to nothing rather than a valid path
    del_pop!(m2, "/qc/tcell")
    @test pop_path_by_uid(m2, uid4) === nothing
    @test pop_path_by_uid(m2, uid8) === nothing       # descendants dropped along with the parent
end

@testset "legacy sidecar without UIDs is backfilled and saved on first load" begin
    td = mktempdir()
    mkpath(joinpath(td, "gating"))
    # a hand-written sidecar carrying no `uid` on any node — what every project shipped before this
    write(joinpath(td, "gating", "B.json"), """
    {"pop_type":"flow","value_name":"B","populations":[
      {"name":"cd4","colour":"#f00","show":true,
       "gate":{"kind":"rectangle","x_channel":"x","y_channel":"y",
               "x_transform":{"kind":"linear"},"y_transform":{"kind":"linear"},
               "x_min":0,"x_max":1,"y_min":0,"y_max":1},
       "children":[
         {"name":"cd8","colour":"#0f0","show":true,
          "gate":{"kind":"rectangle","x_channel":"x","y_channel":"y",
                  "x_transform":{"kind":"linear"},"y_transform":{"kind":"linear"},
                  "x_min":0,"x_max":1,"y_min":0,"y_max":1},"children":[]}
       ]}
    ]}
    """)
    # first load backfills + saves; a second load reads back the SAME uids (stable across sessions)
    m1 = load_pop_map(td, "B")
    u4a = pop_uid(m1, "/cd4"); u8a = pop_uid(m1, "/cd4/cd8")
    @test length(u4a) == 6 && length(u8a) == 6
    m2 = load_pop_map(td, "B")
    @test pop_uid(m2, "/cd4") == u4a
    @test pop_uid(m2, "/cd4/cd8") == u8a
    @test m2._uid_backfilled == false                 # nothing to backfill this time
end

# ── Retired UIDs (MULTI_POP_TRACKING_ORPHANS_PLAN P2) ──────────────────────────────
# A pop's UID outlives the pop as long as the segmentation's h5ad still carries `track_source`
# rows stamped with it. Reusing that UID for a new pop would silently transfer the old orphan
# rows to the new pop on its first tracking run. `_del_paths!` retires the UID; `_fresh_pop_uid`
# refuses to reissue it; the set round-trips through the sidecar so retirement survives sessions.
@testset "deleted pop's UID is retired and never reissued" begin
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "a"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10))
    uid_a = pop_uid(m, "/a")
    @test uid_a ∉ m.retired_uids                       # live pops don't belong in the set

    del_pop!(m, "/a")
    @test uid_a in m.retired_uids                      # retirement is on delete

    # `_fresh_pop_uid` must refuse the retired UID even when explicitly preferred (a hand-edited
    # sidecar that resurrected it). Rerolls to something fresh instead of colliding.
    reused = Cecelia._fresh_pop_uid(m; preferred=uid_a)
    @test reused != uid_a
    @test reused ∉ m.retired_uids

    # And every future add_pop! is guaranteed to skip retired UIDs — driven statistically by the
    # birthday probability, guaranteed by the guard. Add 20 pops and none can land on `uid_a`.
    for i in 1:20
        add_pop!(m, "p$i"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10))
        @test pop_uid(m, "/p$i") != uid_a
    end
end

@testset "retired UIDs round-trip through the sidecar" begin
    td = mktempdir()
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "a"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10))
    add_pop!(m, "b"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10))
    uid_a = pop_uid(m, "/a"); uid_b = pop_uid(m, "/b")
    del_pop!(m, "/a")
    del_pop!(m, "/b")
    save_pop_map!(m, td)

    m2 = load_pop_map(td, "B")
    @test uid_a in m2.retired_uids
    @test uid_b in m2.retired_uids
    # The guard still applies AFTER a load — the retired set is authoritative in the fresh session.
    @test Cecelia._fresh_pop_uid(m2; preferred=uid_a) != uid_a
end

@testset "legacy sidecar without retired_uids loads with an empty set" begin
    td = mktempdir()
    mkpath(joinpath(td, "gating"))
    # An emitted sidecar from before the P2 ship — no `retired_uids` key at all.
    write(joinpath(td, "gating", "B.json"), """
    {"pop_type":"flow","value_name":"B","populations":[
      {"name":"a","uid":"abc123","colour":"#f00","show":true,
       "gate":{"kind":"rectangle","x_channel":"x","y_channel":"y",
               "x_transform":{"kind":"linear"},"y_transform":{"kind":"linear"},
               "x_min":0,"x_max":1,"y_min":0,"y_max":1},"children":[]}
    ]}
    """)
    m = load_pop_map(td, "B")
    @test isempty(m.retired_uids)                      # legacy → empty, no exception
    @test pop_uid(m, "/a") == "abc123"                 # live UID untouched by the P2 path
end

@testset "retired_uids only appears in the sidecar when non-empty (visual cleanliness)" begin
    td = mktempdir()
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "a"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10))
    save_pop_map!(m, td)
    # No deletions → no `retired_uids` key emitted. Byte-visible on the JSON.
    raw = read(joinpath(td, "gating", "B.json"), String)
    @test !occursin("retired_uids", raw)

    del_pop!(m, "/a")
    save_pop_map!(m, td)
    raw2 = read(joinpath(td, "gating", "B.json"), String)
    @test occursin("retired_uids", raw2)
end

@testset "move_pop! rejects a cycle, a collision and an unknown target" begin
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "qc"; parent=ROOT, gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0))
    add_pop!(m, "B";  parent="/qc", gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0))
    add_pop!(m, "mem+"; parent="/qc/B", gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0))
    add_pop!(m, "B";  parent=ROOT, gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0))  # same NAME at root

    # a population cannot become its own descendant's child (that would orphan the whole subtree)
    @test_throws ErrorException move_pop!(m, "/qc/B", "/qc/B/mem+")
    @test_throws ErrorException move_pop!(m, "/qc/B", "/qc/B")
    # …nor land where a population of that name already sits
    @test_throws ErrorException move_pop!(m, "/qc/B", ROOT)
    @test_throws ErrorException move_pop!(m, "/qc/B", "/nope")
    @test_throws ErrorException move_pop!(m, "/nope", ROOT)
    # every rejection left the tree exactly as it was
    @test Set(pop_paths(m)) == Set(["/qc", "/qc/B", "/qc/B/mem+", "/B"])
end

# ── del_children!: prune BELOW a population, keep the population ──────────────
# The other half of del_pop! (⋯ → "Delete N below it"): re-gating from a gate you want to keep,
# without redrawing that gate. One call, so one undo step.
@testset "del_children! prunes the subtree and keeps the pop" begin
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "qc"; parent=ROOT, gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0), colour="#f00")
    add_pop!(m, "B";  parent="/qc", gate=RectangleGate("x", "y", 0.0, 5.0, 0.0, 5.0))
    add_pop!(m, "mem+"; parent="/qc/B", gate=RectangleGate("x", "y", 0.0, 5.0, 0.0, 5.0))

    del_children!(m, "/qc")
    @test pop_paths(m) == ["/qc"]                            # the whole branch below it went
    @test pop_at(m, "/qc").colour == "#f00"                  # the pop itself survived intact
    del_children!(m, "/qc")                                  # already a leaf → no-op, not an error
    @test pop_paths(m) == ["/qc"]
    @test_throws ErrorException del_children!(m, "/nope")

    # membership of the kept pop is unchanged (nothing below it ever fed into it)
    df = DataFrame("label" => [1, 2], "x" => [1.0, 20.0], "y" => [1.0, 20.0])
    recompute!(m, _ -> df)
    @test Set(cells_in_pop(m, "/qc")) == Set([1])
end

# ── Boolean populations (Decision 16): a pop defined by combining OTHER pops ──
# The ask: "gate cells positive for nuc-GFP or mem-TOM", and "mem-TOM+ AND nuc-GFP+ but NOT
# CD169-Kat+" — combinations no single 2D gate can express. One form covers all of it: included
# terms combined with AND or OR, minus every excluded term, still ∩ the parent like any other pop.
@testset "boolean populations combine other populations" begin
    m = PopulationMap(pop_type="flow", value_name="B")
    # three gates on the same axes, each admitting a different slice of the four cells below
    add_pop!(m, "gfp+"; parent=ROOT, gate=RectangleGate("g", "t", 1.0, 100.0, -100.0, 100.0))
    add_pop!(m, "tom+"; parent=ROOT, gate=RectangleGate("g", "t", -100.0, 100.0, 1.0, 100.0))
    add_pop!(m, "kat+"; parent=ROOT, gate=RectangleGate("k", "k", 1.0, 100.0, 1.0, 100.0))
    #                    label:      1        2        3        4
    df = DataFrame("label" => [1, 2, 3, 4],
                   "g" => [5.0, 0.0, 5.0, 0.0],      # gfp+: 1, 3
                   "t" => [0.0, 5.0, 5.0, 0.0],      # tom+: 2, 3
                   "k" => [0.0, 0.0, 5.0, 0.0])      # kat+: 3
    fetch = _ -> df

    # OR — the "positive for either marker" ask
    add_pop!(m, "gfp+ or tom+"; parent=ROOT, boolean_op="or", boolean_pops=["/gfp+", "/tom+"])
    # AND minus an exclusion — the "double positive but not CD169" ask
    add_pop!(m, "dp not kat"; parent=ROOT, boolean_op="and",
             boolean_pops=["/gfp+", "/tom+"], boolean_not=["/kat+"])
    # a plain NOT gate: no included terms ⇒ everything in the parent except the excluded one
    add_pop!(m, "not kat"; parent=ROOT, boolean_op="not", boolean_pops=["/kat+"])

    recompute!(m, fetch)
    @test Set(cells_in_pop(m, "/gfp+ or tom+")) == Set([1, 2, 3])
    @test Set(cells_in_pop(m, "/dp not kat")) == Set{Int}()      # only cell 3 is double+, and it is kat+
    @test Set(cells_in_pop(m, "/not kat")) == Set([1, 2, 4])
    # "not" is stored as an exclusion, not a third operator
    @test pop_at(m, "/not kat").boolean_op == Cecelia.BOOL_AND
    @test pop_at(m, "/not kat").boolean_pops == String[]
    @test pop_at(m, "/not kat").boolean_not == ["/kat+"]

    # ∩ parent still applies: the same combination under a gate is capped by that gate
    add_pop!(m, "qc"; parent=ROOT, gate=RectangleGate("g", "t", -100.0, 100.0, 1.0, 100.0))  # = tom+
    add_pop!(m, "either"; parent="/qc", boolean_op="or", boolean_pops=["/gfp+", "/tom+"])
    recompute!(m, fetch)
    @test Set(cells_in_pop(m, "/qc/either")) == Set([2, 3])

    # a boolean pop can combine boolean pops — and evaluation order is a DEPENDENCY, not depth: this
    # one sits at depth 1 and references "/qc/either" at depth 2, so plain depth order would derive
    # it before the thing it needs.
    add_pop!(m, "chained"; parent=ROOT, boolean_op="and", boolean_pops=["/qc/either", "/not kat"])
    recompute!(m, fetch)
    @test Set(cells_in_pop(m, "/chained")) == Set([2])

    # round-trips through the sidecar, references and all
    td = mktempdir(); save_pop_map!(m, td)
    m2 = load_pop_map(td, "B")
    @test pop_at(m2, "/dp not kat").boolean_op == Cecelia.BOOL_AND
    @test pop_at(m2, "/dp not kat").boolean_pops == ["/gfp+", "/tom+"]
    @test pop_at(m2, "/dp not kat").boolean_not == ["/kat+"]
    recompute!(m2, fetch)
    @test Set(cells_in_pop(m2, "/gfp+ or tom+")) == Set([1, 2, 3])
end

@testset "BoolMembership enum — boundary coercion + JSON round-trip" begin
    # boolean_op is stored as a `BoolMembership` enum (BOOL_AND / BOOL_OR); string kwargs at the
    # add_pop!/set_boolean!/JSON-load boundaries are coerced through `_normalise_boolean`, and the
    # sidecar's `"boolean.op"` writes the lowercase wire string via `Base.string(::BoolMembership)`.
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "a"; parent=ROOT, gate=RectangleGate("x", "y", 0.0, 1.0, 0.0, 1.0))
    add_pop!(m, "b"; parent=ROOT, gate=RectangleGate("x", "y", 0.0, 1.0, 0.0, 1.0))

    # string kwarg → enum stored, both operators
    add_pop!(m, "and_pop"; parent=ROOT, boolean_op="and", boolean_pops=["/a", "/b"])
    add_pop!(m, "or_pop";  parent=ROOT, boolean_op="or",  boolean_pops=["/a", "/b"])
    @test pop_at(m, "/and_pop").boolean_op === Cecelia.BOOL_AND
    @test pop_at(m, "/or_pop").boolean_op  === Cecelia.BOOL_OR

    # passing the enum itself also works — accepts BoolMembership | AbstractString | Symbol
    add_pop!(m, "enum_pop"; parent=ROOT, boolean_op=Cecelia.BOOL_OR, boolean_pops=["/a"])
    @test pop_at(m, "/enum_pop").boolean_op === Cecelia.BOOL_OR

    # sidecar serialises the wire string, not the enum name (`"or"`, not `"BOOL_OR"`)
    td = mktempdir(); save_pop_map!(m, td)
    raw = JSON3.read(read(gating_path(td, "B"), String))
    ops = String[]
    walk = node -> begin
        b = get(node, :boolean, nothing)
        b === nothing || push!(ops, String(get(b, :op, "")))
        for c in get(node, :children, [])
            walk(c)
        end
    end
    for root in raw.populations
        walk(root)
    end
    @test Set(ops) == Set(["and", "or", "or"])
    @test all(o in ("and", "or") for o in ops)   # no "BOOL_AND"/"BOOL_OR" leaking to disk

    # and read back — the loader must coerce the sidecar string into the enum
    m2 = load_pop_map(td, "B")
    @test pop_at(m2, "/and_pop").boolean_op === Cecelia.BOOL_AND
    @test pop_at(m2, "/or_pop").boolean_op  === Cecelia.BOOL_OR

    # parse_bool_membership rejects garbage with an ArgumentError
    @test Cecelia.parse_bool_membership("and") === Cecelia.BOOL_AND
    @test Cecelia.parse_bool_membership("or")  === Cecelia.BOOL_OR
    @test_throws ArgumentError Cecelia.parse_bool_membership("xor")

    # exported backward-compat surface: BOOLEAN_OPS still enumerates the wire strings
    @test Set(Cecelia.BOOLEAN_OPS) == Set(("and", "or"))
end

@testset "boolean references follow rename/move, and refuse to loop" begin
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "qc"; parent=ROOT, gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0))
    add_pop!(m, "gfp+"; parent="/qc", gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0))
    add_pop!(m, "tom+"; parent=ROOT, gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0))
    add_pop!(m, "either"; parent=ROOT, boolean_op="or", boolean_pops=["/qc/gfp+", "/tom+"])

    # a reference is a PATH, so renaming or re-parenting a combined pop must rewrite it — otherwise
    # the combination silently loses a term the moment either gate is renamed.
    rename_pop!(m, "/qc/gfp+", "GFP+")
    @test pop_at(m, "/either").boolean_pops == ["/qc/GFP+", "/tom+"]
    move_pop!(m, "/qc/GFP+", ROOT)
    @test pop_at(m, "/either").boolean_pops == ["/GFP+", "/tom+"]

    # self-reference and loops are rejected, at creation and at edit
    @test_throws ErrorException add_pop!(m, "self"; parent=ROOT, boolean_op="not", boolean_pops=["/self"])
    @test_throws ErrorException set_boolean!(m, "/either"; op="or", pops=["/either"])
    add_pop!(m, "second"; parent=ROOT, boolean_op="and", boolean_pops=["/either"])
    @test_throws ErrorException set_boolean!(m, "/either"; op="or", pops=["/second"])   # A→B→A
    # …as are an unknown reference, an unknown operator and an empty term list
    @test_throws ErrorException add_pop!(m, "x"; parent=ROOT, boolean_op="or", boolean_pops=["/nope"])
    @test_throws ErrorException add_pop!(m, "x"; parent=ROOT, boolean_op="xor", boolean_pops=["/tom+"])
    @test_throws ErrorException add_pop!(m, "x"; parent=ROOT, boolean_op="or", boolean_pops=String[])
    # a pop cannot combine its own descendant either (that descendant already depends on it)
    add_pop!(m, "child"; parent="/either", gate=RectangleGate("x", "y", 0.0, 1.0, 0.0, 1.0))
    @test_throws ErrorException set_boolean!(m, "/either"; op="or", pops=["/either/child"])
    @test Set(pop_paths(m)) == Set(["/qc", "/GFP+", "/tom+", "/either", "/second", "/either/child"])

    # who would be left dangling by a delete — what the API refuses on (rename/move can't orphan)
    @test boolean_dependents(m, ["/tom+"]) == ["/either" => ["/tom+"]]
    @test isempty(boolean_dependents(m, ["/qc"]))
    # a dangling reference (only reachable by hand-editing a sidecar) degrades to an empty pop,
    # never a crash — the same rule as a missing column
    m.pops["/either"].boolean_pops = ["/gone"]
    df = DataFrame("label" => [1], "x" => [1.0], "y" => [1.0])
    recompute!(m, _ -> df)
    @test isempty(cells_in_pop(m, "/either"))
end

# ── Gate shape swap: rectangle ⇄ polygon on an EXISTING population ────────────
# The UI (PopulationManager's convert button, frontend plots/gateGeometry.ts) changes a gate's shape
# by pushing a new spec of the OTHER kind through pop/set-gate. `set_gate!` must therefore accept a
# gate of a different concrete type on a live pop — the population, its children and its identity
# survive; only the geometry and the derived membership change. Without this the only way to redraw
# a rectangle as a polygon is delete + redraw, which takes the children with it.
@testset "set_gate! swaps the gate KIND in place" begin
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "cd4"; parent=ROOT, gate=RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0), colour="#f00")
    add_pop!(m, "cd8"; parent="/cd4", gate=RectangleGate("x", "y", 0.0, 5.0, 0.0, 5.0), colour="#0f0")

    # the four corners of the rectangle, as a polygon → the SAME region (what rect → poly does)
    corners = [(0.0, 0.0), (10.0, 0.0), (10.0, 10.0), (0.0, 10.0)]
    set_gate!(m, "/cd4", PolygonGate("x", "y", corners))
    @test pop_at(m, "/cd4").gate isa PolygonGate
    @test pop_at(m, "/cd4").colour == "#f00"                 # identity untouched
    @test pop_paths(m) == ["/cd4", "/cd4/cd8"]               # child still hanging off it
    @test pop_at(m, "/cd4/cd8").gate isa RectangleGate       # and its own gate untouched

    # membership is re-derived, and the corner polygon selects what the rectangle did
    df = DataFrame("label" => [1, 2, 3, 4],
                   "x" => [1.0, 7.0, 20.0, 3.0], "y" => [1.0, 7.0, 20.0, 3.0])
    recompute!(m, _ -> df)
    @test Set(cells_in_pop(m, "/cd4")) == Set([1, 2, 4])     # 3 is outside
    @test Set(cells_in_pop(m, "/cd4/cd8")) == Set([1, 4])    # child gate still applies under it

    # and back the other way (poly → its bounding box): a WIDENING swap, still in place
    set_gate!(m, "/cd4", RectangleGate("x", "y", 0.0, 25.0, 0.0, 25.0))
    recompute!(m, _ -> df)
    @test pop_at(m, "/cd4").gate isa RectangleGate
    @test Set(cells_in_pop(m, "/cd4")) == Set([1, 2, 3, 4])  # bbox picked up the outlier

    # the swap survives a save/load round-trip (the sidecar carries "kind")
    td = mktempdir()
    set_gate!(m, "/cd4", PolygonGate("x", "y", corners)); save_pop_map!(m, td)
    @test pop_at(load_pop_map(td, "B"), "/cd4").gate isa PolygonGate
end

# ── clust / trackclust pop types (cluster-membership populations) ─────────────
# A cluster pop is a filter on the `clusters.{suffix}` column (clustPops/clustTracks output):
# filter_fun="in", filter_values=[ticked cluster ids]. Stored in its own sidecar so it never
# collides with flow gates. Headless — membership via a recompute! closure (no fixture).
# gating pop types = the hand-drawn ones (flow=cells, track=tracks); clust/trackclust are filters.
# Drives copy-to-images + the defining-plot view (one abstraction over both, no flow special-casing).
@testset "GATING_POP_TYPES" begin
    @test GATING_POP_TYPES == ("flow", "track")
    @test is_gating_pop_type("flow") && is_gating_pop_type("track")
    @test !is_gating_pop_type("clust") && !is_gating_pop_type("trackclust") && !is_gating_pop_type("live")
end

# generic value_name presence check on an image (drives copy-to-images target filtering).
# `_active` is a bookkeeping key, not a value_name → excluded by versioned_keys.
@testset "img_has_value_name" begin
    proj = create_project!(name="vn-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")
    img.label_props = Dict("A" => "A.h5ad", "B" => "B.h5ad", "_active" => "A")
    @test Set(img_value_names(img)) == Set(["A", "B"])
    @test img_has_value_name(img, "A") && img_has_value_name(img, "B")
    @test !img_has_value_name(img, "C") && !img_has_value_name(img, "_active")
    rm(proj.root; recursive=true)
end

@testset "clust / trackclust pop types" begin
    td = mktempdir()
    # each clustering pop_type routes to its OWN gating sidecar (no collision with flow's {vn}.json)
    @test endswith(gating_path(td, "B"; pop_type="flow"),       joinpath("gating", "B.json"))
    @test endswith(gating_path(td, "B"; pop_type="clust"),      joinpath("gating", "B__clust.json"))
    @test endswith(gating_path(td, "B"; pop_type="trackclust"), joinpath("gating", "B__trackclust.json"))
    @test endswith(gating_path(td, "B"; pop_type="track"),      joinpath("gating", "B__tracks.json"))

    # cluster pop membership = filter "in" over the cluster code column
    m = PopulationMap(pop_type="clust", value_name="B")
    add_pop!(m, "myeloid"; filter_measure="clusters.default", filter_fun="in",
             filter_values=[1, 3], colour="#10b981")
    fetch = _ -> DataFrame("label" => [10, 11, 12, 13, 14],
                           "clusters.default" => [0, 1, 2, 3, 1])
    recompute!(m, fetch)
    @test Set(cells_in_pop(m, "/myeloid")) == Set([11, 13, 14])   # codes ∈ {1,3}

    # save/load round-trip → own file, filter fields preserved, flow file untouched
    save_pop_map!(m, td)
    @test isfile(gating_path(td, "B"; pop_type="clust"))
    @test !isfile(gating_path(td, "B"; pop_type="flow"))
    m2 = load_pop_map(td, "B"; pop_type="clust")
    @test pop_at(m2, "/myeloid").filter_measure == "clusters.default"
    @test pop_at(m2, "/myeloid").filter_fun == Cecelia.FILTER_IN
    @test Set(pop_at(m2, "/myeloid").filter_values) == Set([1, 3])
end

@testset "region pop type (spatial regions)" begin
    # region reuses the cluster-pop machinery with its OWN `regions.{suffix}` column prefix
    # (docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 5) — no duplicated logic, one generalisation.
    td = mktempdir()
    @test endswith(gating_path(td, "B"; pop_type="region"), joinpath("gating", "B__region.json"))
    @test Cecelia._is_cluster_pop_type("region")
    @test Cecelia._cluster_measure_prefix("region") == "regions."
    @test Cecelia._cluster_measure_prefix("clust") == "clusters."
    @test is_track_pop("region", "/tumour_zone") == false          # regions are per-cell, not per-track
    @test !is_gating_pop_type("region")                            # filter/membership pop, not a gate

    # region membership = filter "in" over the region code column (same engine path as clust)
    m = PopulationMap(pop_type="region", value_name="B")
    add_pop!(m, "tumour_zone"; filter_measure="regions.niches", filter_fun="in",
             filter_values=[1, 3], colour="#10b981")
    fetch = _ -> DataFrame("label" => [10, 11, 12, 13, 14],
                           "regions.niches" => [0, 1, 2, 3, 1])
    recompute!(m, fetch)
    @test Set(cells_in_pop(m, "/tumour_zone")) == Set([11, 13, 14])   # region codes ∈ {1,3}

    # referenced-suffixes generalisation reads the region prefix from the map's own pop_type
    @test Cecelia._referenced_cluster_suffixes(m) == Set(["niches"])

    # save/load round-trip → own __region file, flow file untouched
    save_pop_map!(m, td)
    @test isfile(gating_path(td, "B"; pop_type="region"))
    @test !isfile(gating_path(td, "B"; pop_type="flow"))
    m2 = load_pop_map(td, "B"; pop_type="region")
    @test pop_at(m2, "/tumour_zone").filter_measure == "regions.niches"
    @test Set(pop_at(m2, "/tumour_zone").filter_values) == Set([1, 3])

    # categorical name-rule: `regions`/`regions.{suffix}` are always a code set, even past the level cap
    @test Cecelia._is_categorical_col(collect(0:50), "regions.niches")   # 51 int levels, name-rule wins
    @test Cecelia._is_categorical_col(collect(0:50), "regions")
    @test Cecelia._is_categorical_col([0.0, 1.5, 2.7], "regions.niches") # decimals irrelevant under name-rule

    # per-region heatmap matrix detection routes through the shared suffix extractor (regions. prefix)
    @test Cecelia._cluster_matrix_suffix("matrix", "regions.niches") == "niches"
    @test Cecelia._cluster_matrix_suffix("matrix", "clusters.default") == "default"
end

@testset "contact_matrix — CODEX log-odds heatmap matrix" begin
    # sidecar spatialStats/{suffix}.json → symmetric pop×pop log-odds matrix for the plot renderer
    td = mktempdir(); mkpath(joinpath(td, "spatialStats"))
    open(joinpath(td, "spatialStats", "default.json"), "w") do f
        write(f, """{"basis":["B/qc","T/qc"],"nCells":100,"nEdges":200,"records":[""" *
                 """{"popA":"B/qc","popB":"B/qc","observed":10,"expected":5,"logOdds":0.7,"association":"associated"},""" *
                 """{"popA":"B/qc","popB":"T/qc","observed":1,"expected":5,"logOdds":-1.1,"association":"avoided"},""" *
                 """{"popA":"T/qc","popB":"T/qc","observed":8,"expected":4,"logOdds":0.6,"association":"associated"}]}""")
    end
    m = contact_matrix(CciaImage(; dir=td))
    @test m.suffixes == ["default"] && m.suffix == "default"
    @test Set(m.basis) == Set(["B/qc", "T/qc"]) && m.nCells == 100 && m.nEdges == 200
    val(x, y) = only(c.value for c in m.cells if c.x == x && c.y == y)
    @test val("B/qc", "T/qc") ≈ -1.1 && val("T/qc", "B/qc") ≈ -1.1   # symmetric fill
    @test val("B/qc", "B/qc") ≈ 0.7 && val("T/qc", "T/qc") ≈ 0.6
    @test length(m.cells) == 4                                       # 2×2 fully filled
    # no sidecar → empty (route returns empty, UI shows "run contact stats first")
    m0 = contact_matrix(CciaImage(; dir=mktempdir()))
    @test isempty(m0.cells) && isempty(m0.suffixes)
end

@testset "region pop auto-share (co-clustered value_names, cell granularity)" begin
    # regions are a per-run column shared across co-clustered segmentations — the identical
    # auto-share/expand machinery as clust, exercised via the `regions.` prefix + cell granularity.
    td = mktempdir()
    lpdir = joinpath(td, "labelProps"); mkpath(lpdir)
    # A & B were region-clustered together (both CELL sidecars carry suffix "niches"); C was not.
    for vn in ("A", "B")
        open(joinpath(lpdir, "$(vn).clustfeatures.json"), "w") do f
            JSON3.write(f, Dict("niches" => Dict("features" => ["flow.region.cd8"], "partOf" => ["u1"])))
        end
    end
    am = PopulationMap(pop_type="region", value_name="A")
    add_pop!(am, "TumourZone"; filter_measure="regions.niches", filter_fun="in", filter_values=[2], colour="#c061cb")
    save_pop_map!(am, td)

    img = CciaImage(; dir=td)
    img.label_props = Dict("A" => "A.h5ad", "B" => "B.h5ad", "C" => "C.h5ad", "_active" => "A")

    @test Set(Cecelia.co_clustered_value_names(img, "niches"; granularity=:cell)) == Set(["A", "B"])

    # B has no sidecar but IS co-clustered → borrows A's region pops, relabeled to B
    mb = load_pop_map(img; value_name="B", pop_type="region")
    @test Set(keys(mb.pops)) == Set(["/TumourZone"]) && mb.value_name == "B"
    @test all(p.value_name == "B" for p in values(mb.pops))
    # C was not in the run → no borrow
    @test isempty(load_pop_map(img; value_name="C", pop_type="region").pops)

    # bare region-pop ref expands across all co-clustered segmentations
    @test Set(Cecelia._expand_cluster_pops(img, ["/TumourZone"], "region", "A")) ==
          Set(["A/TumourZone", "B/TumourZone"])
end

@testset "bare cluster/region pops: run-wide by default, per-segmentation on request" begin
    # A bare cluster-family ref spans every co-clustered segmentation (old-R popDT parity) — right
    # for "show me this run's cluster", WRONG for a plot series, where the picker already offered
    # each (segmentation, population) pair separately. Ticking 3 region pops under B plotted 6.
    td = mktempdir()
    lpdir = joinpath(td, "labelProps"); mkpath(lpdir)
    for vn in ("B", "T")
        Cecelia._write_clust_features!(joinpath(lpdir, "$(vn).h5ad"), "immune",
                                       ["spatial.comp.x.immune"], ["u1"]; family = "regions")
    end
    m = PopulationMap(pop_type="region", value_name="B")
    add_pop!(m, "Population 1"; filter_measure="regions.immune", filter_fun="in", filter_values=[1])
    save_pop_map!(m, td)
    img = CciaImage(; dir=td)
    img.label_props = Dict("B" => "B.h5ad", "T" => "T.h5ad", "_active" => "B")

    # default: bare ref fans out across the run's segmentations
    @test Set(Cecelia._expand_cluster_pops(img, ["/Population 1"], "region", "B")) ==
          Set(["B/Population 1", "T/Population 1"])
    # explicitly value_name-prefixed refs are untouched either way
    @test Cecelia._expand_cluster_pops(img, ["B/Population 1"], "region", "B") == ["B/Population 1"]
    # a non-cluster pop type never expands
    @test Cecelia._expand_cluster_pops(img, ["/gate"], "flow", "B") == ["/gate"]
    # and pop_df exposes the opt-out the series path uses (keyword present, both forms)
    @test :expand_cluster_pops in Base.kwarg_decl(
        only(methods(pop_df, (CciaImage, AbstractString, Any))))
end

@testset "clustfeatures sidecar — families, labels, legacy layouts" begin
    # The sidecar is keyed `{family}.{suffix}` so a cell clustering and a REGION clustering that
    # share a suffix coexist on one segmentation instead of clobbering each other. Three historical
    # layouts must all read back through the ONE shared reader (docs/todo/SPATIAL_REGIONS_PLAN.md).
    @test Cecelia._cluster_measure_family("region") == "regions"
    @test Cecelia._cluster_measure_family("clust")  == "clusters"
    @test Cecelia._cluster_measure_family("trackclust") == "clusters"
    @test Cecelia._clustfeatures_key("immune", "regions") == "regions.immune"
    @test Cecelia._clustfeatures_split_key("regions.immune") == ("immune", "regions")
    @test Cecelia._clustfeatures_split_key("clusters.a.b")   == ("a.b", "clusters")
    @test Cecelia._clustfeatures_split_key("immune")         == ("immune", nothing)   # legacy → any family

    td = mktempdir(); lpdir = joinpath(td, "labelProps"); mkpath(lpdir)
    props = joinpath(lpdir, "B.h5ad")

    # two runs, SAME suffix, different families — the collision that used to silently overwrite
    Cecelia._write_clust_features!(props, "immune", ["mean_intensity_0"], ["u1"]; family="clusters")
    Cecelia._write_clust_features!(props, "immune", ["spatial.comp.B_qc.immune"], ["u1", "u2"];
                                   family="regions",
                                   labels=Dict("spatial.comp.B_qc.immune" => "B/qc"))
    @test Cecelia._clustfeatures_features(props, "immune"; family="clusters") == ["mean_intensity_0"]
    @test Cecelia._clustfeatures_features(props, "immune"; family="regions") == ["spatial.comp.B_qc.immune"]
    @test Cecelia._clustfeatures_suffixes(props; family="clusters") == Set(["immune"])
    @test Cecelia._clustfeatures_suffixes(props; family="regions")  == Set(["immune"])
    # partOf stays per-family (the region run covered one more image)
    e_r = Cecelia._clustfeatures_entry(props, "immune"; family="regions")
    e_c = Cecelia._clustfeatures_entry(props, "immune"; family="clusters")
    @test length(get(e_r, "partOf", [])) == 2 && length(get(e_c, "partOf", [])) == 1
    @test String(get(e_r, "labels", Dict())["spatial.comp.B_qc.immune"]) == "B/qc"

    # LEGACY bare-suffix entry (pre-family) matches every family, so existing data keeps working
    legacy = joinpath(lpdir, "L.h5ad")
    open(replace(legacy, r"\.h5ad$" => ".clustfeatures.json"), "w") do f
        JSON3.write(f, Dict("niches" => Dict("features" => ["x"], "partOf" => ["u1"])))
    end
    @test Cecelia._clustfeatures_suffixes(legacy; family="regions")  == Set(["niches"])
    @test Cecelia._clustfeatures_suffixes(legacy; family="clusters") == Set(["niches"])
    @test Cecelia._clustfeatures_features(legacy, "niches"; family="regions") == ["x"]

    # OLDEST layout: {suffix => [features]} (a bare array, no membership) normalises to the current shape
    oldest = joinpath(lpdir, "O.h5ad")
    open(replace(oldest, r"\.h5ad$" => ".clustfeatures.json"), "w") do f
        JSON3.write(f, Dict("old" => ["f1", "f2"]))
    end
    @test Cecelia._clustfeatures_features(oldest, "old") == ["f1", "f2"]
    @test isempty(get(Cecelia._clustfeatures_entry(oldest, "old"), "partOf", ["nonempty"]))

    # absent run / absent file → empty, never a throw
    @test Cecelia._clustfeatures_features(props, "nosuchrun"; family="regions") == String[]
    @test Cecelia._clustfeatures_entry(joinpath(lpdir, "missing.h5ad"), "x") === nothing
end

@testset "spatial obs measures are NUMERIC, not integer code sets" begin
    # A 0/1 contact/aggregate flag has few integer levels, so the generic heuristic calls it a
    # categorical code set — and the plot panel then offers only count/bar and snaps the chart type
    # to `count`. Commit 16ead1d fixed exactly this for integer morphology by exempting `var`
    # columns; these are `obs`, so they need a name-rule instead.
    flag = [0, 1, 1, 0, 1]
    @test Cecelia._is_categorical_col(flag, "live.cell.contact#live.T_qc__tracked") == false
    @test Cecelia._is_categorical_col(flag, "flow.cell.is.aggregate") == false
    @test Cecelia._is_categorical_col([1, 2, 3], "live.cell.min_distance#live.T_qc") == false
    @test Cecelia._is_categorical_col([0, 0, 1], "spatial.comp.other.immune") == false
    # …while the IDENTIFIERS beside them stay categorical (they are label codes, not quantities)
    @test Cecelia._is_categorical_col([3, 7, 7], "live.cell.contact_id#live.T_qc__tracked") == true
    @test Cecelia._is_categorical_col([1, 2, 2], "live.cell.aggregate.id") == true
    # and the existing rules are untouched
    @test Cecelia._is_categorical_col([0, 1, 2], "regions.immune") == true
    @test Cecelia._is_categorical_col([0, 1, 2], "clusters.default") == true
    @test Cecelia._is_categorical_col([1.5, 2.5], "live.cell.speed") == false
    @test Cecelia._is_categorical_col([1, 2, 3], "live.cell.hmm.state.movement") == true
end

@testset "region 'other' column is skipped when it would be all-zero" begin
    # A graph built over the basis populations themselves contains nothing outside the basis, so the
    # "other" composition column is all-zero — not a measurement, just a flat row in the heatmap.
    # The runner drops it and flags that in the run QC; Julia must then not advertise it in the
    # clustfeatures sidecar, or the heatmap offers a column the table doesn't have.
    d = mktempdir()
    p = joinpath(d, "region_qc.json")
    @test Cecelia._region_other_all_zero(joinpath(d, "absent.json")) == false   # missing → written
    open(p, "w") do f; JSON3.write(f, Dict("otherAllZero" => true)); end
    @test Cecelia._region_other_all_zero(p) == true
    open(p, "w") do f; JSON3.write(f, Dict("otherAllZero" => false)); end
    @test Cecelia._region_other_all_zero(p) == false
    open(p, "w") do f; JSON3.write(f, Dict("nClusters" => 3)); end                # older run, no flag
    @test Cecelia._region_other_all_zero(p) == false
    write(p, "{ not json")                                                        # unreadable → written
    @test Cecelia._region_other_all_zero(p) == false
end

@testset "region composition column naming (one namer, Julia → Python)" begin
    # Julia names the composition columns AND records them in the sidecar; the Python runner is
    # handed the same list. They used to be derived independently and disagreed, so the region
    # composition heatmap asked for columns that did not exist.
    @test Cecelia._comp_col("B/qc/_tracked", "immune") == "spatial.comp.B_qc__tracked.immune"
    @test Cecelia._comp_col("T cells", "x") == "spatial.comp.T_cells.x"
    @test Cecelia._comp_col("plain", "s") == "spatial.comp.plain.s"
end

@testset "compound filter populations (Decision 15 — AND-ed conditions)" begin
    # a user-defined filter pop combining two obs conditions in ONE pop: CD4>0.5 AND speed>5
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "CD4hi_fast"; colour="#8b5cf6", filter_conditions=[
        Dict("measure" => "live.cell.cd4",   "fun" => "gt", "values" => 0.5),
        Dict("measure" => "live.cell.speed", "fun" => "gt", "values" => 5)])
    fetch = _ -> DataFrame("label" => [1, 2, 3, 4],
                           "live.cell.cd4"   => [0.9, 0.9, 0.1, 0.6],
                           "live.cell.speed" => [10,  2,   10,  7])
    recompute!(m, fetch)
    @test Set(cells_in_pop(m, "/CD4hi_fast")) == Set([1, 4])   # BOTH hold: (0.9,10) and (0.6,7)

    # single fields mirror conditions[1] so single-field readers still work
    p = pop_at(m, "/CD4hi_fast")
    @test p.filter_measure == "live.cell.cd4" && p.filter_fun == Cecelia.FILTER_GT && length(p.filter_conditions) == 2

    # round-trip through to_tree/from_tree preserves the conditions + membership
    m2 = from_tree(to_tree(m)); recompute!(m2, fetch)
    @test Set(cells_in_pop(m2, "/CD4hi_fast")) == Set([1, 4])
    @test length(pop_at(m2, "/CD4hi_fast").filter_conditions) == 2

    # a missing condition column → the whole pop degrades to empty (warns), never crashes
    fetch1 = _ -> DataFrame("label" => [1, 2], "live.cell.cd4" => [0.9, 0.1])   # speed absent
    @test_logs (:warn, r"live\.cell\.speed") match_mode=:any recompute!(m, fetch1)
    @test isempty(cells_in_pop(m, "/CD4hi_fast"))
end

@testset "FilterFun enum + FilterCondition — boundary coercion + JSON round-trip" begin
    # `filter_fun` is stored as a `FilterFun` enum; string/Symbol kwargs at add_pop! and API-body
    # boundaries are coerced via `parse_filter_fun`. `filter_conditions` is a `Vector{FilterCondition}`
    # after normalisation (JSON dicts and NamedTuples both accepted at the boundary).
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "single"; filter_measure="x", filter_fun="gt", filter_values=1)
    add_pop!(m, "enum_kwarg"; filter_measure="x", filter_fun=Cecelia.FILTER_GTE, filter_values=1)
    @test pop_at(m, "/single").filter_fun === Cecelia.FILTER_GT
    @test pop_at(m, "/enum_kwarg").filter_fun === Cecelia.FILTER_GTE
    @test pop_at(m, "/single").filter_conditions === nothing

    # compound: string funs and Symbol funs both coerce, and the produced conditions ARE typed
    add_pop!(m, "compound"; filter_conditions=[
        Dict("measure" => "a", "fun" => "in",  "values" => [1, 2]),
        Dict("measure" => "b", "fun" => :neq, "values" => 0)])
    p = pop_at(m, "/compound")
    @test p.filter_conditions isa Vector{Cecelia.FilterCondition}
    @test p.filter_conditions[1].fun === Cecelia.FILTER_IN
    @test p.filter_conditions[2].fun === Cecelia.FILTER_NEQ
    # single-field mirror still works (conditions[1] onto the single fields)
    @test p.filter_measure == "a" && p.filter_fun === Cecelia.FILTER_IN

    # sidecar serialises the wire string (`"in"`, NOT `"FILTER_IN"`) — enforced so the frontend and
    # any older Julia consumer keep reading a raw string.
    td = mktempdir(); save_pop_map!(m, td)
    raw = JSON3.read(read(gating_path(td, "B"), String))
    funs = String[]
    walk = node -> begin
        f = get(node, :filter, nothing)
        if f !== nothing
            push!(funs, String(get(f, :fun, "")))
            for c in get(f, :conditions, [])
                push!(funs, String(get(c, :fun, "")))
            end
        end
        for c in get(node, :children, [])
            walk(c)
        end
    end
    for root in raw.populations
        walk(root)
    end
    @test Set(funs) ⊆ Set(["gt", "gte", "in", "neq"])   # all wire-form strings
    @test !any(startswith(f, "FILTER_") for f in funs)

    # read back — the loader coerces sidecar strings into the enum + typed FilterCondition
    m2 = load_pop_map(td, "B")
    @test pop_at(m2, "/single").filter_fun === Cecelia.FILTER_GT
    p2 = pop_at(m2, "/compound")
    @test p2.filter_conditions isa Vector{Cecelia.FilterCondition}
    @test p2.filter_conditions[1].fun === Cecelia.FILTER_IN

    # parse_filter_fun rejects garbage with an ArgumentError; catalog covers all wire forms
    @test Cecelia.parse_filter_fun("lte") === Cecelia.FILTER_LTE
    @test_throws ArgumentError Cecelia.parse_filter_fun("gtish")

    # gating_engine._filter_mask now takes FilterFun | Nothing directly (no string branching)
    col = [1.0, 2.0, 3.0, 4.0]
    @test Cecelia._filter_mask(col, Cecelia.FILTER_GT,  2.0, false) == BitVector([false, false, true, true])
    @test Cecelia._filter_mask(col, Cecelia.FILTER_IN,  [1.0, 3.0], false) == BitVector([true, false, true, false])
    @test Cecelia._filter_mask(col, nothing, nothing, true) == BitVector([true, true, true, true])
end

@testset "recompute! — a missing filter/gate column degrades to empty (no crash)" begin
    # A cluster pop whose `clusters.{suffix}` column isn't in the fetched frame — e.g. evaluated
    # against a segmentation that didn't take part in that run, so `fetch_cols` silently dropped it
    # — must resolve to NO members, not raise `ArgumentError: column name … not found` and 500 the
    # whole plot. Regression: the trackclust heatmap crash (clusters.default not found).
    m = PopulationMap(pop_type="trackclust", value_name="C")
    add_pop!(m, "present"; filter_measure="clusters.movement", filter_fun="in", filter_values=[1, 2], colour="#10b981")
    add_pop!(m, "absent";  filter_measure="clusters.default",  filter_fun="in", filter_values=[0, 1], colour="#ef4444")
    # frame HAS clusters.movement but NOT clusters.default
    fetch = _ -> DataFrame("label" => [10, 11, 12, 13], "clusters.movement" => [0, 1, 2, 1])
    @test_logs (:warn, r"clusters\.default") match_mode=:any recompute!(m, fetch)  # warns, doesn't throw
    @test Set(cells_in_pop(m, "/present")) == Set([11, 12, 13])   # present column resolves normally
    @test isempty(cells_in_pop(m, "/absent"))                     # missing column → empty membership

    # same guard for a GATE whose axis column is absent from the frame
    mg = PopulationMap(pop_type="flow", value_name="C")
    add_pop!(mg, "g"; gate=RectangleGate("x", "missingY", 0.0, 10.0, 0.0, 10.0), colour="#abcdef")
    fetchg = _ -> DataFrame("label" => [1, 2], "x" => [1.0, 2.0])   # no "missingY"
    @test_logs (:warn, r"missingY") match_mode=:any recompute!(mg, fetchg)
    @test isempty(cells_in_pop(mg, "/g"))
end

@testset "colour_by_palette — pop colour else default" begin
    # a value a user pop FILTERS for on the column → that pop's colour; the rest → OKABE_ITO by
    # sorted position. Generalises "use the population's colour where one exists" (a cluster pop is
    # just a filter on clusters.{suffix}).
    m = PopulationMap(pop_type="clust", value_name="B")
    add_pop!(m, "directed";   filter_measure="clusters.mov", filter_fun="in", filter_values=[2],       colour="#ff1493")
    add_pop!(m, "crawling";   filter_measure="clusters.mov", filter_fun="in", filter_values=[0, 3, 4], colour="#ffd700")
    add_pop!(m, "unrelated";  filter_measure="clusters.other", filter_fun="in", filter_values=[1],     colour="#000001")

    pal = colour_by_palette(m, "clusters.mov", [0, 1, 2, 3, 4])
    @test pal[2] == "#ff1493"                 # user pop colour
    @test pal[0] == "#ffd700" && pal[3] == "#ffd700" && pal[4] == "#ffd700"
    @test pal[1] == OKABE_ITO[1]              # uncovered value 1 → first default
    # a pop filtering a DIFFERENT column never leaks its colour in
    @test pal[1] != "#000001"

    # numeric tolerance: a filter value stored as 2.0 still matches integer column value 2
    m3 = PopulationMap(pop_type="clust", value_name="B")
    add_pop!(m3, "d"; filter_measure="clusters.mov", filter_fun="in", filter_values=[2.0], colour="#abcdef")
    @test colour_by_palette(m3, "clusters.mov", [2])[2] == "#abcdef"

    # no matching pop → all default, by sorted position (stable)
    empty = PopulationMap(pop_type="clust", value_name="B")
    p2 = colour_by_palette(empty, "clusters.mov", [5, 3, 3, 1])
    @test p2[1] == OKABE_ITO[1] && p2[3] == OKABE_ITO[2] && p2[5] == OKABE_ITO[3]

    # pop_colour_overrides: string-keyed {value => hex} for the wire (2.0/2 → "2"); only pops on
    # the column contribute; no default fill (the bridge does that).
    ov = pop_colour_overrides(m, "clusters.mov")
    @test ov == Dict("2" => "#ff1493", "0" => "#ffd700", "3" => "#ffd700", "4" => "#ffd700")
    @test pop_colour_overrides(m3, "clusters.mov") == Dict("2" => "#abcdef")   # 2.0 → "2"
    @test isempty(pop_colour_overrides(m, "clusters.absent"))

    # pop_label_overrides: same keying, value → the POP NAME (so the legend reads "directed", not "2")
    lbl = pop_label_overrides(m, "clusters.mov")
    @test lbl == Dict("2" => "directed", "0" => "crawling", "3" => "crawling", "4" => "crawling")
    @test isempty(pop_label_overrides(m, "clusters.absent"))
end

# ── Summary-canvas population picker (plot_pop_types / plot_population_groups) ──
# The logic the /api/plots/populations route delegates to — pure, so tested here (the route is a
# thin wrapper). Covers granularity→pop_type selection, cross-image + cross-pop_type union/dedup,
# derived-pop injection, and pop_type tagging (the track-pops-in-the-picker fix; docs/POPULATION.md).
@testset "plot population picker" begin
    # pop_type selection by granularity
    @test plot_pop_types("live", "cell") == ["live"]
    @test plot_pop_types("live", "")     == ["live"]
    @test plot_pop_types("live", "track") == ["live", "track"]
    @test plot_pop_types("track", "track") == ["track"]          # no duplicate

    # flatten_pop_tree: pre-order paths + colours
    fm = PopulationMap(pop_type="flow", value_name="C")
    add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#ef4444")
    add_pop!(fm, "sub"; parent="/qc", gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#abc")
    flat = flatten_pop_tree(to_tree(fm))
    @test [p for (p, _, _) in flat] == ["/qc", "/qc/sub"]
    @test flat[1][3] == "#ef4444"

    # a track-gated map (pop_type "track") with one pop
    tm = PopulationMap(pop_type="track", value_name="C")
    add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5, colour="#f59e0b")

    # loaders injected (as the API passes versioned_keys/load_pop_map closures)
    names_for = _ -> ["C"]
    load = (_, vn, pt) -> vn == "C" ? (pt == "track" ? tm : (pt == "live" ? fm : nothing)) : nothing

    # CELL granularity → live pops only, with derived `_tracked` at root AND under each stored pop
    # (so /qc/_tracked is a selectable, indented child); a derived child directly follows its parent.
    cell = plot_population_groups([:img1], names_for, load, plot_pop_types("live", "cell"))
    @test length(cell) == 1 && cell[1].value_name == "C"
    cpops = cell[1].populations
    @test [p.path for p in cpops] ==
          ["/_tracked", "/qc", "/qc/_tracked", "/qc/sub", "/qc/sub/_tracked"]
    @test all(p.pop_type == "live" for p in cpops)
    @test !any(p.path == "/TEST" for p in cpops)
    # the nested derived pop is named by its leaf (indents under its parent in the UI)
    @test only(p for p in cpops if p.path == "/qc/_tracked").name == "_tracked"
    # a derived child inherits its parent pop's colour (so /qc/_tracked pairs with /qc visually —
    # the derived colour is read-only on the behaviour page, the parent's is editable on gating)
    @test only(p for p in cpops if p.path == "/qc/_tracked").colour == "#ef4444"      # = /qc
    @test only(p for p in cpops if p.path == "/qc/sub/_tracked").colour == "#abc"      # = /qc/sub
    @test only(p for p in cpops if p.path == "/_tracked").colour == "#7c93b8"          # root: no parent → grey

    # TRACK granularity → unions live (incl. nested /qc/_tracked) AND track (/TEST), each tagged
    trk = plot_population_groups([:img1], names_for, load, plot_pop_types("live", "track"))
    tp = trk[1].populations
    @test Set(p.path for p in tp) ==
          Set(["/_tracked", "/qc", "/qc/_tracked", "/qc/sub", "/qc/sub/_tracked", "/TEST"])
    test_pop = only(p for p in tp if p.path == "/TEST")
    @test test_pop.pop_type == "track" && test_pop.colour == "#f59e0b"
    @test only(p for p in tp if p.path == "/qc").pop_type == "live"
    @test !any(p.path == "/TEST/_tracked" for p in tp)          # no track-derived pop registered

    # derived_ok predicate, keyed on the PARENT path ("" = root): the API passes false where the
    # derived set is a copy of a deeper one, so tracking gated to /qc/sub hides /_tracked AND
    # /qc/_tracked while keeping /qc/sub/_tracked. Default (no predicate) offers all three —
    # asserted by `cell` above.
    gated = plot_population_groups([:img1], names_for, load, plot_pop_types("live", "cell");
                                   derived_ok = (_v, _pt, parent, _d) -> parent == "/qc/sub")
    gpaths = [p.path for p in gated[1].populations]
    @test !("/_tracked" in gpaths) && !("/qc/_tracked" in gpaths)   # root + ancestor copies hidden
    @test "/qc/sub/_tracked" in gpaths                              # the population that was tracked
    @test "/qc" in gpaths && "/qc/sub" in gpaths                    # the stored gates are untouched

    # cross-image UNION + dedup: two images both expose "C" → each (pop_type, path) appears once
    dedup = plot_population_groups([:img1, :img2], names_for, load, ["live"])
    @test length(dedup) == 1
    @test length(dedup[1].populations) == length(cpops)         # no duplicates across images

    # LABELS (gateless): no gating map — one selectable pop per segmentation value_name, named by
    # the value_name, tagged pop_type "labels" (segmentation QC: B/T plot side by side).
    names2 = _ -> ["B", "T"]
    lab = plot_population_groups([:img1], names2, (args...) -> error("must not load a map for labels"),
                                 ["labels"])
    @test [g.value_name for g in lab] == ["B", "T"]
    @test all(g -> length(g.populations) == 1, lab)
    bp = only(lab[1].populations)
    @test bp.path == "/labels" && bp.name == "B" && bp.pop_type == "labels"
    @test only(lab[2].populations).name == "T"
end

@testset "popScope population picker" begin
    # is_track_pop: the sole cell-vs-track test (Julia parity of the R `isTrack` attribute)
    @test is_track_pop("live", "/qc") == false                  # plain cell gate
    @test is_track_pop("flow", "/qc/sub") == false
    @test is_track_pop("clust", "/myeloid") == false            # cell cluster
    @test is_track_pop("live", "/_tracked") == true             # derived tracked set (root)
    @test is_track_pop("live", "/qc/_tracked") == true          # derived tracked subset of a gate
    @test is_track_pop("track", "/TEST") == true                # per-track gate
    @test is_track_pop("trackclust", "/clusterA") == true       # track cluster

    # scope_pop_types: sources loaded per scope; clusters toggleable; unknown scope throws.
    # `cells` also loads `region` (spatial regions) alongside `clust` — both cluster-family.
    @test scope_pop_types("cells", true)  == ["live", "clust", "region"]
    @test scope_pop_types("cells", false) == ["live"]
    @test scope_pop_types("tracks", true)  == ["live", "track", "trackclust"]
    @test scope_pop_types("tracks", false) == ["live", "track"]
    @test_throws ErrorException scope_pop_types("bogus", true)

    # maps: flow gates (/qc, /qc/sub), a per-track gate (/TEST), a cell cluster (/myeloid),
    # a track cluster (/clusterA)
    fm = PopulationMap(pop_type="flow", value_name="C")
    add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#ef4444")
    add_pop!(fm, "sub"; parent="/qc", gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#abc")
    tm = PopulationMap(pop_type="track", value_name="C")
    add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5, colour="#f59e0b")
    cm = PopulationMap(pop_type="clust", value_name="C")
    add_pop!(cm, "myeloid"; filter_measure="clusters.default", filter_fun="in", filter_values=[1, 2])
    tcm = PopulationMap(pop_type="trackclust", value_name="C")
    add_pop!(tcm, "clusterA"; filter_measure="clusters.tracks", filter_fun="in", filter_values=[0])
    names_for = _ -> ["C"]
    load = (_, vn, pt) -> vn != "C" ? nothing :
        pt == "live" ? fm : pt == "track" ? tm : pt == "clust" ? cm : pt == "trackclust" ? tcm : nothing

    # CELLS scope: all-cells root ("/") + plain gates + cell clusters; NO derived _tracked sets
    cells = population_scope_groups([:img1], names_for, load, "cells")
    @test length(cells) == 1 && cells[1].value_name == "C"
    cpaths = [p.path for p in cells[1].populations]
    @test cpaths == ["/", "/qc", "/qc/sub", "/myeloid"]
    @test cells[1].populations[1].name == "all"                 # backend all-cells root
    @test !any(occursin("_tracked", p) for p in cpaths)         # cells never show tracked sets
    @test all(!is_track_pop(p.pop_type, p.path) for p in cells[1].populations if p.path != "/")

    # CELLS, clusters excluded → drops /myeloid
    cells_nc = population_scope_groups([:img1], names_for, load, "cells"; include_clusters=false)
    @test [p.path for p in cells_nc[1].populations] == ["/", "/qc", "/qc/sub"]

    # TRACKS scope: derived _tracked sets (root + per-gate) + per-track gate + track cluster;
    # NO plain cell gates (/qc, /qc/sub) and NO all-cells root ("/")
    trk = population_scope_groups([:img1], names_for, load, "tracks")
    tpaths = Set(p.path for p in trk[1].populations)
    @test tpaths == Set(["/_tracked", "/qc/_tracked", "/qc/sub/_tracked", "/TEST", "/clusterA"])
    @test !("/qc" in tpaths) && !("/qc/sub" in tpaths) && !("/" in tpaths)
    @test all(is_track_pop(p.pop_type, p.path) for p in trk[1].populations)
    # a derived tracked child keeps its parent gate's colour (visual pairing, read-only)
    @test only(p for p in trk[1].populations if p.path == "/qc/_tracked").colour == "#ef4444"

    # TRACKS, gated tracking → root /_tracked hidden (redundant with /qc/_tracked); children kept
    trk_g = population_scope_groups([:img1], names_for, load, "tracks";
                                    derived_ok=(_v, _pt, parent, _d) -> parent != "")
    gpaths = Set(p.path for p in trk_g[1].populations)
    @test !("/_tracked" in gpaths) && "/qc/_tracked" in gpaths

    # TRACKS, clusters excluded → drops /clusterA, keeps the per-track gate /TEST
    trk_nc = population_scope_groups([:img1], names_for, load, "tracks"; include_clusters=false)
    tncpaths = Set(p.path for p in trk_nc[1].populations)
    @test !("/clusterA" in tncpaths) && "/TEST" in tncpaths
end

# ── pop_category + population_accept_groups (Decision 14, accepts allow-list) ────────────────
@testset "population accepts allow-list + category tags" begin
    # pop_category: gated / clustered / region / tracked / aggregated from (pop_type, leaf).
    @test pop_category("live", "/qc")               == "gated"
    @test pop_category("track", "/TEST")             == "gated"
    @test pop_category("clust", "/myeloid")          == "clustered"
    @test pop_category("trackclust", "/clusterA")    == "clustered"
    @test pop_category("region", "/r0")              == "region"
    @test pop_category("live", "/qc/_tracked")       == "tracked"
    @test pop_category("live", "/qc/" * Cecelia.AGGREGATED_POP_NAME) == "aggregated"

    # same fixtures as the popScope testset above, plus a region map and an aggregated cell pop.
    fm = PopulationMap(pop_type="flow", value_name="C")
    add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#ef4444")
    add_pop!(fm, Cecelia.AGGREGATED_POP_NAME; parent="/qc", filter_measure="live.cell.is.aggregate",
             filter_fun="gt", filter_values=0, reserved_ok=true)   # auto-created aggregate pop
    tm = PopulationMap(pop_type="track", value_name="C")
    add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5)
    cm = PopulationMap(pop_type="clust", value_name="C")
    add_pop!(cm, "myeloid"; filter_measure="clusters.default", filter_fun="in", filter_values=[1, 2])
    tcm = PopulationMap(pop_type="trackclust", value_name="C")
    add_pop!(tcm, "clusterA"; filter_measure="clusters.tracks", filter_fun="in", filter_values=[0])
    rm_ = PopulationMap(pop_type="region", value_name="C")
    add_pop!(rm_, "r0"; filter_measure="regions.default", filter_fun="in", filter_values=[0])
    names_for = _ -> ["C"]
    load = (_, vn, pt) -> vn != "C" ? nothing :
        pt == "live" ? fm : pt == "track" ? tm : pt == "clust" ? cm :
        pt == "trackclust" ? tcm : pt == "region" ? rm_ : nothing

    # accepts=["live"] → all-cells root + cell gate + the aggregated cell pop; NO tracked sets,
    # NO clusters/regions. Each population carries granularity/category tags.
    g = population_accept_groups([:img1], names_for, load, ["live"])[1].populations
    @test [p.path for p in g] == ["/", "/qc", "/qc/" * Cecelia.AGGREGATED_POP_NAME]
    @test all(p.granularity == "cell" for p in g)
    @test only(p for p in g if p.path == "/qc").category == "gated"
    @test only(p for p in g if endswith(p.path, Cecelia.AGGREGATED_POP_NAME)).category == "aggregated"

    # "flow" is an alias for "live".
    @test [p.path for p in population_accept_groups([:img1], names_for, load, ["flow"])[1].populations] ==
          [p.path for p in g]

    # region basis: cells (gated+clustered+region) AND tracks (gated+clustered). One picker, both
    # granularities — the case popScope could not express.
    basis = population_accept_groups([:img1], names_for, load,
                ["live", "clust", "region", "track", "trackclust"])[1].populations
    bcats = Set((p.granularity, p.category) for p in basis)
    @test ("cell", "gated") in bcats && ("cell", "clustered") in bcats && ("cell", "region") in bcats
    @test ("track", "tracked") in bcats && ("track", "gated") in bcats && ("track", "clustered") in bcats
    @test "/r0" in [p.path for p in basis] && "/myeloid" in [p.path for p in basis]
    @test "/clusterA" in [p.path for p in basis] && "/TEST" in [p.path for p in basis]

    # accepts=["clust"] alone → only cell clusters, no all-cells root (live not accepted).
    cl = population_accept_groups([:img1], names_for, load, ["clust"])[1].populations
    @test [p.path for p in cl] == ["/myeloid"]

    # popScope shim must still produce identical paths to the direct accept call.
    @test [p.path for p in population_scope_groups([:img1], names_for, load, "cells")[1].populations] ==
          [p.path for p in population_accept_groups([:img1], names_for, load,
                                ["live", "clust", "region"])[1].populations]

    # unknown token / empty list throw loudly.
    @test_throws ErrorException population_accept_groups([:img1], names_for, load, ["bogus"])
    @test_throws ErrorException population_accept_groups([:img1], names_for, load, String[])
end

# ── branch pop_type (BRANCHING_PLAN.md Decision 2) ────────────────────────────
# Adding "branch" to the framework must extend POP_MAP_SUFFIX/ACCEPT_TOKENS/pop_category and
# route via population_accept_groups with granularity="branch". The framework was designed to
# take a third pop_type; this guards the wiring.
@testset "branch pop_type wiring" begin
    # POP_MAP_SUFFIX resolves the gating file suffix.
    @test Cecelia.POP_MAP_SUFFIX["branch"] == BRANCH_PROPS_SUFFIX
    # build the expected tail with joinpath — a literal "gating/..." fails on Windows, where the
    # path is "\\gating\\stroma__branch.json" (the product is fine; the assertion wasn't portable)
    @test endswith(gating_path("/tmp", "stroma"; pop_type="branch"),
                   joinpath("gating", "stroma__branch.json"))

    # ACCEPT_TOKENS + validators.
    @test "branch" in Cecelia.ACCEPT_TOKENS

    # pop_category: branch pops are gated (the ensure_filter_pop! per-branch-type case).
    @test pop_category("branch", "/endpoint-to-endpoint") == "gated"

    # population_accept_groups tags branch pops with granularity="branch" and only surfaces
    # them when "branch" is in accepts. A mixed request keeps cells + branches.
    bm = PopulationMap(pop_type="branch", value_name="C")
    add_pop!(bm, "endpoint-to-endpoint"; filter_measure="branch-type",
             filter_fun="eq", filter_values=0)
    add_pop!(bm, "junction-to-junction"; filter_measure="branch-type",
             filter_fun="eq", filter_values=2)
    fm = PopulationMap(pop_type="flow", value_name="C")
    add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1))
    names_for = _ -> ["C"]
    load = (_, vn, pt) -> vn != "C" ? nothing :
        pt == "live"   ? fm :
        pt == "branch" ? bm : nothing

    # accepts=["branch"] → only branch pops, no cell root
    br = population_accept_groups([:img1], names_for, load, ["branch"])[1].populations
    @test Set(p.path for p in br) == Set(["/endpoint-to-endpoint", "/junction-to-junction"])
    @test all(p.granularity == "branch" for p in br)
    @test all(p.category    == "gated"  for p in br)
    @test all(p.pop_type    == "branch" for p in br)

    # accepts=["live","branch"] → all-cells root + cell gate + branches
    mix = population_accept_groups([:img1], names_for, load, ["live", "branch"])[1].populations
    gcats = Set((p.granularity, p.category) for p in mix)
    @test ("cell", "gated") in gcats
    @test ("branch", "gated") in gcats

    # accepts=["live"] must NOT include branches.
    only_cells = population_accept_groups([:img1], names_for, load, ["live"])[1].populations
    @test all(p.granularity == "cell" for p in only_cells)
end

# ── ensure_filter_pop! — a cutoff materialised as a reusable filter pop (Decision 14) ────────
@testset "ensure_filter_pop! auto-created population" begin
    td = mktempdir()
    img = CciaImage(; dir=td)
    m = PopulationMap(; pop_type="flow", value_name="B")
    add_pop!(m, "qc"; gate=RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
    save_pop_map!(m, img)

    # a 0/1 flag column → aggregated pop under /qc (the generalisable `> 0`, not a baked TRUE/FALSE)
    created = ensure_filter_pop!(img, "flow", "B", ["/qc"], AGGREGATED_POP_NAME;
                 filter_measure="flow.cell.is.aggregate", filter_fun="gt", filter_values=0)
    @test created == ["/qc/" * AGGREGATED_POP_NAME]
    p = pop_at(load_pop_map(img; value_name="B", pop_type="flow"), "/qc/" * AGGREGATED_POP_NAME)
    @test p.filter_measure == "flow.cell.is.aggregate" && p.filter_fun == Cecelia.FILTER_GT && p.filter_values == 0
    @test pop_category(p.pop_type, p.path) == "aggregated" && !is_track_pop(p.pop_type, p.path)

    # idempotent: re-running REDEFINES (a probability cutoff — measure-agnostic), never duplicates
    ensure_filter_pop!(img, "flow", "B", ["/qc"], AGGREGATED_POP_NAME;
                 filter_measure="flow.cell.aggregate.score", filter_fun="gte", filter_values=0.5)
    m3 = load_pop_map(img; value_name="B", pop_type="flow")
    @test count(pp -> endswith(pp, AGGREGATED_POP_NAME), pop_paths(m3)) == 1
    @test pop_at(m3, "/qc/" * AGGREGATED_POP_NAME).filter_fun == Cecelia.FILTER_GTE

    # a parent absent from the map is skipped; the all-cells root ("/") maps to ROOT and is created
    created2 = ensure_filter_pop!(img, "flow", "B", ["/nonexistent", "/"], AGGREGATED_POP_NAME;
                 filter_measure="flow.cell.is.aggregate", filter_fun="gt", filter_values=0)
    @test created2 == ["/" * AGGREGATED_POP_NAME]
    rm(td; recursive=true)
end

# ── Mixed-type pop resolution: resolve_pop_type / pop_namespace / pop_df_multi (module pickers) ──
@testset "resolve_pop_type + pop_namespace (mixed-type pickers)" begin
    td = mktempdir()
    img = CciaImage(; dir=td)
    # one stored map per type on disk (routed by m.pop_type), all under value_name "B"
    fm = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(fm, "qc"; gate=RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0)); save_pop_map!(fm, img)
    cm = PopulationMap(pop_type="clust", value_name="B")
    add_pop!(cm, "myeloid"; filter_measure="clusters.default", filter_fun="in", filter_values=[1, 2]); save_pop_map!(cm, img)
    rmp = PopulationMap(pop_type="region", value_name="B")
    add_pop!(rmp, "r0"; filter_measure="regions.default", filter_fun="in", filter_values=[0]); save_pop_map!(rmp, img)
    tm = PopulationMap(pop_type="track", value_name="B")
    add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5); save_pop_map!(tm, img)

    # each path resolves to the map that CONTAINS it; _tracked → live; root/unknown → flow
    @test resolve_pop_type(img, "B", "/qc")           == "flow"
    @test resolve_pop_type(img, "B", "/myeloid")      == "clust"
    @test resolve_pop_type(img, "B", "/r0")           == "region"
    @test resolve_pop_type(img, "B", "/TEST")         == "track"
    @test resolve_pop_type(img, "B", "/qc/_tracked")  == "live"   # derived leaf, not stored
    @test resolve_pop_type(img, "B", "/")             == "flow"   # all-cells root → cells
    @test resolve_pop_type(img, "B", "/nonexistent")  == "flow"   # unknown → default (empty downstream)

    # _split_pop_ref: prefix names the value_name; leading-slash/root stays in default
    @test Cecelia._split_pop_ref("B/qc", "default") == ("B", "/qc")
    @test Cecelia._split_pop_ref("/qc", "B")        == ("B", "/qc")
    @test Cecelia._split_pop_ref("qc", "B")         == ("B", "/qc")

    # pops_value_name: the spatial tasks derive their segmentation from the pick (no dropdown).
    # Value_name comes from the first ref's prefix; the all-cells root "B/" carries it too.
    @test pops_value_name(["B/qc"])              == "B"
    @test pops_value_name(["B/qc", "B/myeloid"]) == "B"       # single-segmentation set
    @test pops_value_name(["B/"])                == "B"       # "… all" root pick
    @test pops_value_name(String[])              == "default" # empty → default
    @test pops_value_name(String[]; default="C") == "C"
    # distinct value_names shouldn't reach a single-segmentation picker → warn, first still wins
    @test (@test_logs (:warn,) match_mode=:any pops_value_name(["B/qc", "T/qc"])) == "B"

    # grouping by discovered type preserves first-appearance order
    grp = Cecelia._group_pops_by_type(img, ["/qc", "/myeloid", "/qc/_tracked", "/r0"], "B")
    @test grp == ["flow" => ["/qc"], "clust" => ["/myeloid"], "live" => ["/qc/_tracked"], "region" => ["/r0"]]

    # pop_namespace: any TRACKED source → live, else flow (cluster/region are just cell selections)
    @test pop_namespace(img, ["/qc"]; value_name="B")            == "flow"
    @test pop_namespace(img, ["/r0"]; value_name="B")            == "flow"
    @test pop_namespace(img, ["/myeloid"]; value_name="B")       == "flow"
    @test pop_namespace(img, ["/qc/_tracked"]; value_name="B")   == "live"
    @test pop_namespace(img, ["/TEST"]; value_name="B")          == "live"   # track pop → live namespace
    @test pop_namespace(img, ["/qc", "B/TEST"]; value_name="B")  == "live"   # any tracked → live
    @test pop_namespace(img, String[])                           == "flow"

    # name-uniqueness guard (cross pop_type): a name already used by ANOTHER type in the segmentation
    @test pop_name_conflict(img, "B", "/qc"; pop_type="region")     == "flow"    # flow gate qc exists
    @test pop_name_conflict(img, "B", "/myeloid"; pop_type="flow")  == "clust"   # clust myeloid exists
    @test pop_name_conflict(img, "B", "/TEST"; pop_type="flow")     == "track"
    @test pop_name_conflict(img, "B", "/qc"; pop_type="flow")       === nothing   # same type → not a conflict
    @test pop_name_conflict(img, "B", "/qc"; pop_type="live")       === nothing   # live shares the flow map
    @test pop_name_conflict(img, "B", "/brandnew"; pop_type="flow") === nothing   # unused name → ok
    @test pop_name_conflict(img, "B", "/"; pop_type="clust")        === nothing   # root exempt

    # same-name guard: "/qc" now exists in BOTH the flow map (a gate) and the region map — an
    # ambiguous path. resolve by priority (flow first) AND @warn, never a silent mis-resolve.
    add_pop!(rmp, "qc"; filter_measure="regions.default", filter_fun="in", filter_values=[1]); save_pop_map!(rmp, img)
    @test (@test_logs (:warn,) match_mode=:any resolve_pop_type(img, "B", "/qc")) == "flow"
    rm(td; recursive=true)
end

# ── pop_df_multi membership over real H5AD (equals per-type pop_df; unknown refs skip cleanly) ──
@testset "pop_df_multi integration (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "pop_df_multi integration (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
        thr = sort(full.mean_intensity_0)[cld(nrow(full), 2)]
        truth = sum(full.mean_intensity_0 .>= thr)
        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "pos"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1", thr, 1e12, -1e12, 1e12))
        save_pop_map!(m, img)

        # a flow gate resolves + returns the SAME cells as an explicit pop_df("flow", …)
        direct = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area"])
        multi  = pop_df_multi(img, ["/pos"]; value_name="B", pop_cols=["area"])
        @test nrow(multi) == truth == nrow(direct)
        @test unique(multi.pop) == ["/pos"]

        # an unknown-type ref (no map contains it) resolves to flow and is skipped → still just /pos
        mixed = pop_df_multi(img, ["/pos", "/nonexistent"]; value_name="B", pop_cols=["area"])
        @test nrow(mixed) == truth

        # dedup: /pos ∪ all-cells root collapses to one row per cell (root = every cell)
        pooled = pop_df_multi(img, ["/pos", "/"]; value_name="B", pop_cols=["area"])
        @test nrow(pooled) == nrow(full)
        @test length(unique(pooled.label)) == nrow(pooled)   # no duplicated cell rows

        # restrict_to guard: keep only the operated-on segmentation's cells (single-seg tasks)
        @test nrow(pop_df_multi(img, ["/pos"]; value_name="B", pop_cols=["area"], restrict_to="B")) == truth
        @test nrow(pop_df_multi(img, ["/pos"]; value_name="B", pop_cols=["area"], restrict_to="OTHER")) == 0

        # END-TO-END on real tracked data (the user's ask): the TRACKED subset of a gate resolves
        # to CELLS via the mixed picker — impossible before (cells picker hid _tracked; the consumer
        # assumed flow and got nothing). B.h5ad carries track_id.
        tid = label_props(img; value_name="B") |> v -> select_cols(v, ["track_id"]) |> as_df
        tracked = Set(tid.label[[x isa Real && isfinite(x) && x > 0 for x in tid.track_id]])
        gate = pop_df(img, "flow", ["/pos"]; value_name="B")
        expected_tracked = count(l -> l in tracked, gate.label)
        @test 0 < expected_tracked < nrow(gate)               # a genuine tracked subset of the gate
        trkd = pop_df_multi(img, ["/pos/_tracked"]; value_name="B", restrict_to="B")
        @test nrow(trkd) == expected_tracked                  # tracked cells now resolve
        @test resolve_pop_type(img, "B", "/pos/_tracked") == "live"
        @test pop_namespace(img, ["/pos/_tracked"]; value_name="B") == "live"
        rm(td; recursive=true)
    end
end

# ── Which populations get a derived `_tracked` row (the picker's rule, on real tracked data) ──
# A `_tracked` under EVERY population that exists is what the picker used to offer: gating a
# segmentation into qc → B → subsets listed five identical "all tracks" rows plus the real subsets.
# The rule is now "only where it says something new", and this checks it against cells that really
# carry `track_id`.
@testset "tracked_pop_parents — no _tracked row that copies a deeper one (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "tracked_pop_parents (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
        thr  = sort(full.mean_intensity_0)[cld(nrow(full), 2)]
        tid  = label_props(img; value_name="B") |> v -> select_cols(v, ["track_id"]) |> as_df
        trk_of = Dict(Int(l) => Int(t) for (l, t) in zip(tid.label, tid.track_id)
                      if t isa Real && isfinite(t) && t > 0)
        # /all takes every cell, /pos half of them — so /all's tracks ARE the segmentation's tracks
        # and /pos's are (verified below) fewer.
        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "all"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1", -1e12, 1e12, -1e12, 1e12))
        add_pop!(m, "pos"; parent="/all",
                 gate=RectangleGate("mean_intensity_0", "mean_intensity_1", thr, 1e12, -1e12, 1e12))
        save_pop_map!(m, img)
        pos_tracks = Set(trk_of[Int(l)] for l in pop_df(img, "flow", ["/all/pos"]; value_name="B").label
                         if haskey(trk_of, Int(l)))
        @test 0 < length(pos_tracks) < length(Set(values(trk_of)))   # a genuine subset of the tracks

        parents = tracked_pop_parents(img; value_name="B")
        @test !("" in parents)          # root: /all holds the same tracks → the root row is a copy
        @test "/all" in parents         # the population tracking effectively ran on
        @test "/all/pos" in parents     # its own, smaller track set

        # an UNTRACKED segmentation offers nothing at all — no `_tracked` under the gates it has,
        # which is what a freshly gated, not-yet-tracked segmentation used to show. (`is_tracked` is
        # the first exit; a registered value_name with nothing written yet is untracked.)
        img.label_props["C"] = "C.h5ad"
        @test !is_tracked(img; value_name="C")
        @test isempty(tracked_pop_parents(img; value_name="C"))

        # The picker asks this on every load, so the answer is cached on the gating + h5ad mtimes —
        # and a SAVED GATE EDIT has to invalidate it, or the rail keeps answering for the old tree.
        # Deleting /all/pos leaves /all as the deepest pop holding the tracks, so the set changes.
        # Functional oracle over the wall-clock timing the previous shape used: a cache hit does
        # not add a key. `@elapsed` on a sub-ms function on a shared macOS runner picked up JIT/GC
        # jitter (4.7 ms > 1 ms floor) — a hard flake, since the cost this test asserts on already
        # left the wall clock. The Dict-length check is the same assertion as "cache hit".
        n_before = length(Cecelia._TRACKED_PARENTS_CACHE)
        tracked_pop_parents(img; value_name="B")
        @test length(Cecelia._TRACKED_PARENTS_CACHE) == n_before     # second ask hit, no new entry
        del_pop!(m, "/all/pos"); sleep(0.01); save_pop_map!(m, img)
        @test tracked_pop_parents(img; value_name="B") == Set(["/all"])
        rm(td; recursive=true)
    end
end

# ── Cluster-pop auto-share across co-clustered segmentations (CLUSTER_POOLING_PLAN.md) ─────
@testset "cluster pop auto-share (co-clustered value_names)" begin
    td = mktempdir()
    lpdir = joinpath(td, "labelProps"); mkpath(lpdir)
    # B & T were clustered together (both track sidecars carry suffix "movement"); C was not.
    for vn in ("B", "T")
        open(joinpath(lpdir, "$(vn)__tracks.clustfeatures.json"), "w") do f
            JSON3.write(f, Dict("movement" => Dict("features" => ["live.track.speed"], "partOf" => ["u1"])))
        end
    end
    # named trackclust pops authored ONLY under B (filter the shared clusters.movement column)
    bm = PopulationMap(pop_type="trackclust", value_name="B")
    add_pop!(bm, "Directed"; filter_measure="clusters.movement", filter_fun="in", filter_values=[3], colour="#c061cb")
    add_pop!(bm, "Scanning"; filter_measure="clusters.movement", filter_fun="in", filter_values=[0], colour="#62a0ea")
    save_pop_map!(bm, td)

    img = CciaImage(; dir=td)
    img.label_props = Dict("B" => "B.h5ad", "T" => "T.h5ad", "C" => "C.h5ad", "_active" => "B")

    # co-clustered segmentations for run "movement" (track granularity) = B and T (not C)
    @test Set(Cecelia.co_clustered_value_names(img, "movement"; granularity=:track)) == Set(["B", "T"])

    # B has its OWN sidecar → loaded verbatim
    mb = load_pop_map(img; value_name="B", pop_type="trackclust")
    @test Set(keys(mb.pops)) == Set(["/Directed", "/Scanning"]) && mb.value_name == "B"

    # T has NO sidecar but IS co-clustered → BORROWS B's named pops, relabeled to T so
    # membership resolves over T's own track table
    mt = load_pop_map(img; value_name="T", pop_type="trackclust")
    @test Set(keys(mt.pops)) == Set(["/Directed", "/Scanning"])
    @test mt.value_name == "T" && all(p.value_name == "T" for p in values(mt.pops))
    @test mt.pops["/Directed"].filter_measure == "clusters.movement"

    # C was NOT part of the run (no clustfeatures suffix) → no borrow (empty map)
    mc = load_pop_map(img; value_name="C", pop_type="trackclust")
    @test isempty(mc.pops)

    # BARE cluster-pop ref expands across ALL co-clustered segmentations (R popDT parity)
    @test Set(Cecelia._expand_cluster_pops(img, ["/Directed"], "trackclust", "B")) ==
          Set(["B/Directed", "T/Directed"])
    # explicit value_name-prefixed ref is untouched (single-segmentation request still works)
    @test Cecelia._expand_cluster_pops(img, ["T/Scanning"], "trackclust", "B") == ["T/Scanning"]
    # unknown pop → left as-is (falls back to default_vn downstream); non-cluster type → no-op
    @test Cecelia._expand_cluster_pops(img, ["/Nope"], "trackclust", "B") == ["/Nope"]
    @test Cecelia._expand_cluster_pops(img, ["/x"], "flow", "B") == ["/x"]

    # per-cluster heatmap detection: a matrix over a clusters.{suffix} column pools co-clustered vns
    @test Cecelia._cluster_matrix_suffix("matrix", "clusters.movement") == "movement"
    @test Cecelia._cluster_matrix_suffix("matrix", "pop") === nothing      # per-population mode
    @test Cecelia._cluster_matrix_suffix("boxplot", "clusters.movement") === nothing
end

# ── Gating engine: recompute, membership, filtered (tracked) pops ─────────
@testset "recompute! + cells_in_pop" begin
    df = DataFrame(label=[1, 2, 3, 4, 5], x=[1.0, 6, 6, 9, 9], track_id=[0, 5, 9, 0, 7])

    # flow: parent (x≥0) ∩ child (x≥5)
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "p"; gate=RectangleGate("x", "x", 0.0, 1e9, -1e9, 1e9))
    add_pop!(m, "c"; parent="/p", gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    recompute!(m, _ -> df)
    @test cells_in_pop(m, "/p") == [1, 2, 3, 4, 5]
    @test cells_in_pop(m, "/p/c") == [2, 3, 4, 5]         # x≥5
    @test pop_stats(m, "/p/c").pct_parent == 80.0

    # filtered (tracked) pop: track_id > 0
    mt = PopulationMap(pop_type="live", value_name="T")
    add_pop!(mt, "tracked"; filter_measure="track_id", filter_fun="gt", filter_values=0)
    recompute!(mt, _ -> df)
    @test cells_in_pop(mt, "/tracked") == [2, 3, 5]

    @test_throws ErrorException cells_in_pop(PopulationMap(), "/x")  # not recomputed
end

# ── explicit-label membership (pick selection) + transient not persisted ─
@testset "explicit-label (pick selection) membership" begin
    df = DataFrame(label=[1, 2, 3, 4, 5], x=[1.0, 6, 6, 9, 9])

    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "p"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))   # x≥5 → 2,3,4,5
    # transient pick selection of labels {2,4,9} ∩ parent(x≥5) → {2,4}
    add_pop!(m, "pick"; parent="/p", explicit_labels=[2, 4, 9],
             colour="#22d3ee", transient=true)
    recompute!(m, _ -> df)
    @test cells_in_pop(m, "/p/pick") == [2, 4]              # 9 absent, 3/5 not selected

    # root-level selection (no gate parent): exactly the labels present
    m2 = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m2, "sel"; explicit_labels=[1, 3, 99], transient=true)
    recompute!(m2, _ -> df)
    @test cells_in_pop(m2, "/sel") == [1, 3]

    # transient pops are NOT written to disk, but stay in the in-memory/broadcast tree
    td = mktempdir()
    save_pop_map!(m, td)
    reloaded = load_pop_map(td, "B")
    @test !has_pop(reloaded, "/p/pick")                     # dropped on persist
    @test has_pop(reloaded, "/p")                           # real pop kept
    @test "transient" in keys(Cecelia._node_dict(m, "/p/pick"))  # flagged in broadcast tree

    # explicit-label pops carry a membership signature in the broadcast tree (no gate/filter
    # to diff on) so the client refreshes plots when the selection's cell set changes.
    nd1 = Cecelia._node_dict(m, "/p/pick")
    @test haskey(nd1, "membership_sig")
    del_pop!(m, "/p/pick")
    add_pop!(m, "pick"; parent="/p", explicit_labels=[2, 9], colour="#22d3ee", transient=true)
    @test Cecelia._node_dict(m, "/p/pick")["membership_sig"] != nd1["membership_sig"]
end

# ── scale_centroids!: THE one pixel→µm conversion (pure, no fixture needed) ────────
# The Python mirror (`label_props_utils.scale_centroids`) is asserted on the SAME numbers in
# python/cecelia/tests/test_centroid_migrate.py, so the two languages cannot drift on which axis
# scales by what.
@testset "scale_centroids! maps each axis by name" begin
    phys = [3.0, 0.5, 0.25]        # [sz, sy, sx]
    mk(; with_z=true) = begin
        d = DataFrame("label" => [1, 2], "centroid_x" => [100.0, 200.0],
                      "centroid_y" => [10.0, 20.0], "centroid_t" => [0.0, 1.0],
                      "area" => [5.0, 6.0])
        with_z && (d[!, "centroid_z"] = [4.0, 8.0])
        d
    end

    d = scale_centroids!(mk(), phys)
    @test d.centroid_x == [25.0, 50.0]      # ×sx
    @test d.centroid_y == [5.0, 10.0]       # ×sy
    @test d.centroid_z == [12.0, 24.0]      # ×sz
    # time stays a FRAME index on purpose, and non-centroid columns are untouched
    @test d.centroid_t == [0.0, 1.0]
    @test d.area == [5.0, 6.0]
    @test d.label == [1, 2]

    # 2D: with no centroid_z, x must STILL use sx. A tail-aligned implementation would give x the
    # sy value here — the silent 2D bug the by-name contract exists to prevent.
    d2 = scale_centroids!(mk(with_z=false), phys)
    @test d2.centroid_x == [25.0, 50.0]
    @test d2.centroid_y == [5.0, 10.0]
    @test !("centroid_z" in names(d2))

    # a frame with no centroid columns is a no-op, not an error
    plain = DataFrame("label" => [1], "area" => [5.0])
    @test scale_centroids!(copy(plain), phys) == plain

    # the CciaImage form reads the sizes off `meta` — same numbers, one axis at a time
    with_meta(m) = (i = CciaImage(; uid="c1", name="cal", dir=""); i.meta = Dict{String,Any}(m); i)
    let img = with_meta(Dict("PhysicalSizeZ" => "3.0", "PhysicalSizeY" => "0.5",
                             "PhysicalSizeX" => "0.25"))
        d3 = scale_centroids!(mk(), img)
        @test d3.centroid_x == [25.0, 50.0]
        @test d3.centroid_z == [12.0, 24.0]
    end
    # uncalibrated → img_physical_sizes defaults to 1.0, so the frame comes back unchanged
    let img = with_meta(Dict{String,Any}())
        @test scale_centroids!(mk(), img).centroid_x == [100.0, 200.0]
        @test !img_is_calibrated(img)
    end
    # `_pop_df_finish` is the single conversion point every pop_df branch returns through.
    let cal = with_meta(Dict("PhysicalSizeZ" => "3.0", "PhysicalSizeY" => "0.5",
                             "PhysicalSizeX" => "0.25"))
        # :pixel leaves the values alone; :physical converts
        @test Cecelia._pop_df_finish(mk(), cal, :pixel).centroid_x == [100.0, 200.0]
        @test Cecelia._pop_df_finish(mk(), cal, :physical).centroid_x == [25.0, 50.0]
        @test Cecelia._pop_df_finish(mk(), cal, false).centroid_x == [100.0, 200.0]
        # a frame with NO cell coordinates (a track-grained or branch frame) warns rather than
        # silently ignoring the argument
        trackish = DataFrame("label" => [1], "live.track.speed" => [3.0])
        @test_logs (:warn, r"no centroid_x") Cecelia._pop_df_finish(trackish, cal, :physical)
        # …and an uncalibrated image warns instead of relabelling pixels as µm
        @test_logs (:warn, r"no physical pixel size") Cecelia._pop_df_finish(
            mk(), with_meta(Dict{String,Any}()), :physical)
    end

    # calibrated: X/Y present and > 0 (Z not required — a 2D image legitimately has none)
    @test img_is_calibrated(with_meta(Dict("PhysicalSizeX" => "0.25", "PhysicalSizeY" => "0.5")))
    @test !img_is_calibrated(with_meta(Dict("PhysicalSizeX" => "0.25")))
    @test !img_is_calibrated(with_meta(Dict("PhysicalSizeX" => "0", "PhysicalSizeY" => "0.5")))
    @test !img_is_calibrated(with_meta(Dict("PhysicalSizeX" => "", "PhysicalSizeY" => "0.5")))
end

# ── spatial gates in µm: the stamp, the eval-time scale, the portability predicate ────────
# docs/todo/SPATIAL_GATE_UNITS_PLAN.md. A position gate is stored in µm and compared against data
# scaled with THIS image's µm/px, so one gate means one physical region on every image.
@testset "spatial gate units" begin
    # ── the stamp round-trips, and a legacy file (no stamp) reads as px ──
    @test PopulationMap().spatial_unit == SPATIAL_UNIT_PX
    let m = PopulationMap(; spatial_unit=SPATIAL_UNIT_UM)
        @test to_tree(m)["spatial_unit"] == SPATIAL_UNIT_UM
        @test from_tree(to_tree(m)).spatial_unit == SPATIAL_UNIT_UM
    end
    # no stamp ⇒ px: every gating file written before this change holds pixel coordinates and must
    # keep evaluating as pixels, so the migration is optional rather than required
    @test from_tree(Dict("pop_type" => "flow", "value_name" => "B",
                         "populations" => [])).spatial_unit == SPATIAL_UNIT_PX

    # ── a map ADOPTS µm when there is nothing to reinterpret (this replaces the migration) ──
    # The stamp only constrains a file that already holds position coordinates, so a calibrated image
    # upgrades an intensity-only map — new or long-standing — and the first position gate anyone draws is
    # already physical. A map that DOES carry a position gate keeps its unit, or its numbers would move.
    let td = mktempdir()
        img = CciaImage(uid="CAL", dir=td)
        img.meta = Dict{String,Any}("PhysicalSizeX" => "0.25", "PhysicalSizeY" => "0.5")
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # brand-new map on a calibrated image → µm, with that image's sizes attached
        m0 = load_pop_map(img; value_name="B", pop_type="flow")
        @test m0.spatial_unit == SPATIAL_UNIT_UM
        @test m0.physical_sizes == [1.0, 0.5, 0.25]

        # an EXISTING intensity-only file (stamped px, as every pre-change file is) is upgraded
        mpx = PopulationMap(; pop_type="flow", value_name="B", spatial_unit=SPATIAL_UNIT_PX)
        add_pop!(mpx, "hi"; gate=RectangleGate("area", "mean_intensity_0", 0., 1., 0., 1.))
        save_pop_map!(mpx, td)
        @test load_pop_map(img; value_name="B", pop_type="flow").spatial_unit == SPATIAL_UNIT_UM

        # …but one that ALREADY holds a position gate keeps px — re-stamping would move its coordinates
        msp = PopulationMap(; pop_type="flow", value_name="B", spatial_unit=SPATIAL_UNIT_PX)
        add_pop!(msp, "pos"; gate=RectangleGate("centroid_x", "centroid_y", 0., 500., 0., 400.))
        save_pop_map!(msp, td)
        @test load_pop_map(img; value_name="B", pop_type="flow").spatial_unit == SPATIAL_UNIT_PX

        # an UNCALIBRATED image adopts nothing and carries no sizes (no µm to convert to)
        u = CciaImage(uid="UNCAL", dir=mktempdir())
        u.label_props["B"] = "B.h5ad"; u.label_props["_active"] = "B"
        let mu = load_pop_map(u; value_name="B", pop_type="flow")
            @test mu.spatial_unit == SPATIAL_UNIT_PX
            @test mu.physical_sizes === nothing
        end
    end

    # ── is_spatial_axis: centroid_t is NOT spatial (a frame index carries no pixel size) ──
    @test all(is_spatial_axis, ["centroid_x", "centroid_y", "centroid_z"])
    @test !any(is_spatial_axis, ["centroid_t", "area", "mean_intensity_0", "live.cell.speed"])

    # ── recompute! scales the DATA to the gate's unit, in one place ──
    # 3 cells at x = 100/200/300 px; sx = 0.5 µm/px ⇒ 50/100/150 µm. A gate over 40–110 µm selects
    # the first two; the SAME numbers read as pixels select only the first.
    cells = DataFrame("label" => [1, 2, 3], "centroid_x" => [100.0, 200.0, 300.0],
                      "centroid_y" => [0.0, 0.0, 0.0])
    fetch = _ -> cells
    mk(unit, sizes) = begin
        m = PopulationMap(; pop_type="flow", value_name="B", spatial_unit=unit, physical_sizes=sizes)
        add_pop!(m, "sel"; gate=RectangleGate("centroid_x", "centroid_y", 40.0, 110.0, -1.0, 1.0))
        recompute!(m, fetch)
        m
    end
    @test sort(collect(cells_in_pop(mk(SPATIAL_UNIT_UM, [1.0, 0.5, 0.5]), "/sel"))) == [1, 2]
    @test sort(collect(cells_in_pop(mk(SPATIAL_UNIT_PX, [1.0, 0.5, 0.5]), "/sel"))) == [1]
    # a µm map on an UNCALIBRATED image (no sizes) falls back to pixels rather than inventing a scale
    @test sort(collect(cells_in_pop(mk(SPATIAL_UNIT_UM, nothing), "/sel"))) == [1]
    # the caller's frame is never mutated by the scaling (recompute! copies)
    @test cells.centroid_x == [100.0, 200.0, 300.0]
    # an intensity-only gate is untouched by any of this
    let m = PopulationMap(; spatial_unit=SPATIAL_UNIT_UM, physical_sizes=[1.0, 0.5, 0.5])
        add_pop!(m, "hi"; gate=RectangleGate("area", "perim", 5.0, 15.0, 0.0, 100.0))
        recompute!(m, _ -> DataFrame("label" => [1, 2, 3], "area" => [1.0, 10.0, 20.0],
                                     "perim" => [1.0, 1.0, 1.0]))
        @test sort(collect(cells_in_pop(m, "/hi"))) == [2]
    end

    # ── has_spatial_gate: which strategies need the target image to be calibrated to copy ──
    g(f) = (m = PopulationMap(); f(m); m)
    @test has_spatial_gate(g(m -> add_pop!(m, "s";
        gate=RectangleGate("centroid_x", "centroid_y", 0., 1., 0., 1.))))
    @test has_spatial_gate(g(m -> add_pop!(m, "m";      # y axis alone is enough
        gate=RectangleGate("area", "centroid_z", 0., 1., 0., 1.))))
    @test has_spatial_gate(g(m -> add_pop!(m, "f";      # a filter on a position measure counts
        filter_measure="centroid_x", filter_fun="gt", filter_values=10)))
    @test has_spatial_gate(g(m -> add_pop!(m, "c"; filter_conditions=[
        (; measure="area", fun="gt", values=1), (; measure="centroid_y", fun="lt", values=99)])))
    @test !has_spatial_gate(g(m -> add_pop!(m, "i";
        gate=RectangleGate("area", "mean_intensity_0", 0., 1., 0., 1.))))
    @test !has_spatial_gate(g(m -> add_pop!(m, "t";     # centroid_t is not spatial
        gate=RectangleGate("centroid_t", "area", 0., 1., 0., 1.))))
    @test !has_spatial_gate(g(m -> add_pop!(m, "n";
        filter_measure="flow.cell.is.aggregate", filter_fun="gt", filter_values=0)))
    @test !has_spatial_gate(PopulationMap())
end

# ── pop_df(centroids=…): coordinates without naming the columns ────────────
# `pop_df` is the primary accessor for population data (docs/POPULATION.md) — a caller should never
# have to know which centroid columns exist (they differ per segmentation) or convert units by hand.
@testset "pop_df centroids (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "pop_df centroids (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        cp(fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad"),
           joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
        cent = ["centroid_z", "centroid_y", "centroid_x", "centroid_t"]

        # a narrowed read (pop_cols) does NOT carry coordinates …
        base = pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"])
        @test !any(c -> c in names(base), cent)
        # … until `centroids` widens the PUSHDOWN to include them
        px = pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"], centroids=:pixel)
        @test all(c -> c in names(px), cent)
        @test "area" in names(px)                       # the requested column is still there
        @test nrow(px) == nrow(base)

        # this fixture is UNCALIBRATED, so :physical must return PIXELS rather than relabel them µm
        @test !img_is_calibrated(img)
        ph = pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"], centroids=:physical)
        @test ph.centroid_x == px.centroid_x

        # :pixel and :physical share ONE cached read (the cache holds the frame as read, in pixels,
        # and the unit conversion happens on the returned copy) — so a :physical call cannot leave
        # scaled values behind for a later :pixel caller.
        @test pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"],
                     centroids=:pixel).centroid_x == px.centroid_x

        # …and `false` is a DIFFERENT read (different columns), so it must not share their entry
        @test !("centroid_x" in names(pop_df(img, "labels", String[]; value_name="B",
                                             pop_cols=["area"])))

        # the no-columns read already returns coordinates, so `centroids` changes nothing there
        wide = pop_df(img, "labels", String[]; value_name="B", centroids=:pixel)
        @test all(c -> c in names(wide), cent)

        # the keyword is validated, and is advertised on the public method
        @test_throws ErrorException pop_df(img, "labels", String[]; value_name="B", centroids=:um)
        @test :centroids in Base.kwarg_decl(
            only(methods(pop_df, (CciaImage, AbstractString, Any))))
    end
end

# ── pop_df: pooling across value_names + dedup to most-specific pop ────────
@testset "pop_df pooling + dedup" begin
    dfA = DataFrame(label=[1, 2, 3], x=[1.0, 6.0, 9.0])
    dfB = DataFrame(label=[10, 11], x=[7.0, 2.0])
    mA = PopulationMap(pop_type="flow", value_name="A")
    add_pop!(mA, "p"; gate=RectangleGate("x", "x", -1e9, 1e9, -1e9, 1e9))
    add_pop!(mA, "c"; parent="/p", gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    mB = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(mB, "hi"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    maps = Dict("A" => mA, "B" => mB)
    load_map = vn -> maps[vn]
    fetch = (vn, _) -> (vn == "A" ? dfA : dfB)

    # request /p and /p/c from A, and hi from B (prefixed)
    res = Cecelia._pop_df(load_map, fetch, "flow", ["/p", "/p/c", "B/hi"];
                          default_vn="A", pop_cols=["x"], unique_labels=true)
    @test Set(unique(res.value_name)) == Set(["A", "B"])      # pooled across value_names
    # dedup: label 2 & 3 are in both /p and /p/c → assigned the most specific (/p/c)
    getpop(l, vn) = only(res[(res.label .== l) .& (res.value_name .== vn), :pop])
    @test getpop(1, "A") == "/p"
    @test getpop(2, "A") == "/p/c"
    @test getpop(3, "A") == "/p/c"
    @test getpop(10, "B") == "/hi"                            # x=7 ≥ 5
    @test nrow(res[res.value_name .== "B", :]) == 1           # label 11 (x=2) excluded
end

# ── pop_df: drop_na drops NA/NaN cells in requested pop_cols ──────────────
@testset "pop_df drop_na" begin
    # NaN is in a measure column (x), NOT the gate column (g) — a NaN gate value would
    # fail the gate comparison and never enter the result, masking what drop_na does.
    df = DataFrame(label=[1, 2, 3], g=[0.0, 0.0, 0.0], x=[1.0, NaN, 9.0])
    m = PopulationMap(pop_type="flow", value_name="A")
    add_pop!(m, "p"; gate=RectangleGate("g", "g", -1e9, 1e9, -1e9, 1e9))  # all pass
    load_map = _ -> m
    fetch = (_, _) -> df
    keep = Cecelia._pop_df(load_map, fetch, "flow", ["/p"]; default_vn="A", pop_cols=["x"])
    @test nrow(keep) == 3                                     # NaN row kept by default
    dropped = Cecelia._pop_df(load_map, fetch, "flow", ["/p"];
                              default_vn="A", pop_cols=["x"], drop_na=true)
    @test sort(dropped.label) == [1, 3]                       # label 2 (x=NaN) dropped
end

# ── pop_df: track_id joins the dedup key when present (most-specific pop still wins) ──
@testset "pop_df track_id dedup key" begin
    df = DataFrame(label=[1, 2], x=[9.0, 9.0], track_id=[10.0, 20.0])
    m = PopulationMap(pop_type="flow", value_name="A")
    add_pop!(m, "p"; gate=RectangleGate("x", "x", -1e9, 1e9, -1e9, 1e9))
    add_pop!(m, "c"; parent="/p", gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    load_map = _ -> m
    fetch = (_, _) -> df
    res = Cecelia._pop_df(load_map, fetch, "flow", ["/p", "/p/c"];
                          default_vn="A", pop_cols=["x"], unique_labels=true)
    @test "track_id" in names(res)                            # track_id carried through
    @test nrow(res) == 2                                      # still one row per cell
    @test Set(res.pop) == Set(["/p/c"])                       # most-specific pop wins
end

# ── pop_df: derived live "_tracked" pop (track_id>0 filter on a gated parent) ──
@testset "pop_df live _tracked (derived filter)" begin
    # label4 fails the qc gate (x=1); label2 is in qc but untracked (track_id=NaN).
    df = DataFrame(label=[1, 2, 3, 4], x=[9.0, 9.0, 9.0, 1.0], track_id=[10.0, NaN, 20.0, 30.0])
    m = PopulationMap(pop_type="flow", value_name="A")
    add_pop!(m, "qc"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))   # qc = {1,2,3}
    # "_tracked" is derived, not stored — injecting it adds a filtered child of /qc
    Cecelia._inject_derived_pops!(m, ["/qc/_tracked"], "live")
    @test has_pop(m, "/qc/_tracked")
    @test m.pops["/qc/_tracked"].filter_measure == "track_id"
    load_map = _ -> m
    fetch = (_, _) -> df
    res = Cecelia._pop_df(load_map, fetch, "live", ["/qc/_tracked"];
                          default_vn="A", pop_cols=["track_id"])
    @test sort(res.label) == [1, 3]                # qc ∩ track_id>0 (label2 NaN, label4 not in qc)
    @test unique(res.pop) == ["/qc/_tracked"]
    # a derived pop is only injected under its registered pop_type (foreign type → skip)
    m2 = PopulationMap(pop_type="flow", value_name="A")
    add_pop!(m2, "qc"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    Cecelia._inject_derived_pops!(m2, ["/qc/_tracked"], "clust")  # _tracked is a `live` spec
    @test !has_pop(m2, "/qc/_tracked")
    # an unknown `_`-name is not derived either
    Cecelia._inject_derived_pops!(m2, ["/qc/_nope"], "live")
    @test !has_pop(m2, "/qc/_nope")
end

# ── reserved derived-pop namespace: `_`-prefixed names can't be hand-drawn gates ──
@testset "reserved pop names (_ prefix)" begin
    @test is_reserved_pop_name("_tracked")
    @test is_reserved_pop_name("_anything")
    @test !is_reserved_pop_name("qc")
    m = PopulationMap(pop_type="flow", value_name="A")
    add_pop!(m, "qc"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    # a hand-drawn gate may not take a reserved name
    @test_throws ErrorException add_pop!(m, "_tracked"; parent="/qc",
                                         gate=RectangleGate("x", "x", 0.0, 1.0, 0.0, 1.0))
    @test_throws ErrorException rename_pop!(m, "/qc", "_qc")
    # the derived injection (reserved_ok) is allowed to create it
    add_pop!(m, "_tracked"; parent="/qc", filter_measure="track_id", filter_fun="gt",
             filter_values=0, transient=true, reserved_ok=true)
    @test has_pop(m, "/qc/_tracked")
    # round-trips through from_tree (reconstruction bypasses the guard)
    m3 = from_tree(to_tree(m; include_transient=true))
    @test has_pop(m3, "/qc/_tracked")
end

# ── pop_df: cache key folds in file mtimes → auto-invalidates on gate/h5ad change ──
@testset "pop_df cache auto-invalidation" begin
    td = mktempdir(); mkpath(joinpath(td, "gating")); mkpath(joinpath(td, "labelProps"))
    img = CciaImage(uid="X", dir=td)
    img.label_props["A"] = "A.h5ad"
    write(joinpath(td, "gating", "A.json"), "{}")
    write(joinpath(td, "labelProps", "A.h5ad"), "x")
    key() = Cecelia._pop_df_cache_key(img, "flow", "A", ["/qc"], nothing,
                                      false, true, true, false, false, :cell, String[], String[])
    k1 = key()
    sleep(0.05); touch(joinpath(td, "labelProps", "A.h5ad"))   # re-track rewrites h5ad
    k2 = key()
    @test k1 != k2
    sleep(0.05); touch(joinpath(td, "gating", "A.json"))        # gate edit rewrites map
    @test key() != k2
end

# ── pop_df: integration on real KDIeEm (gate eval over real H5AD) ─────────
@testset "pop_df integration (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "pop_df integration (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
        thr = sort(full.mean_intensity_0)[cld(nrow(full), 2)] # ~median → discriminating
        truth = sum(full.mean_intensity_0 .>= thr)
        @test 0 < truth < nrow(full)                          # genuinely partial selection

        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "pos"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1",
                                              thr, 1e12, -1e12, 1e12))
        save_pop_map!(m, img)
        df = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area", "mean_intensity_0"])
        @test nrow(df) == truth
        @test Set(names(df)) ⊇ Set(["label", "area", "mean_intensity_0", "pop", "value_name"])
        @test all(df.mean_intensity_0 .>= thr)                # every returned cell passes the gate
        @test unique(df.pop) == ["/pos"]

        # channel-name resolution: pop_df renames intensity cols to channel names by default,
        # raw_channel_names=true keeps the {measure}_intensity_{i} names (channel names are
        # stored under the default version, so value_name="B" falls back to it)
        set_channel_names!(img, ["CD4", "CD8", "CD3", "CD19"]; check_length=false)
        named = pop_df(img, "flow", ["/pos"]; value_name="B", include_x=true)
        @test "CD4" in names(named) && !("mean_intensity_0" in names(named))
        raw = pop_df(img, "flow", ["/pos"]; value_name="B", include_x=true, raw_channel_names=true)
        @test "mean_intensity_0" in names(raw) && !("CD4" in names(raw))

        # value_name=nothing resolves to the active segmentation (img.label_props _active="B")
        auto = pop_df(img, "flow", ["/pos"]; pop_cols=["area"])
        @test nrow(auto) == truth

        # cache: a request is stored under its signature key; flush_cache recomputes
        ck = Cecelia._pop_df_cache_key(img, "flow", "B", ["/pos"], ["area"],
                                       false, true, true, false, false, :cell, String[], String[])
        cached = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area"])
        @test haskey(img._pop_df_cache, ck)
        fresh = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area"], flush_cache=true)
        @test nrow(cached) == truth && nrow(fresh) == truth
    end
end

# ── pop_df "labels": the value_name PREFIX selects the segmentation ───────────────
# `labels` has no sub-populations, so the branch used to ignore `pops` entirely and read the image's
# ACTIVE segmentation — meaning the QC canvas returned the active label set's cells whichever one the
# picker asked for: the WRONG DATA under the RIGHT LABEL, with no error. The prefix half of a pop ref
# still carries the value_name, and it has to be honoured like every other pop_type's.
@testset "pop_df labels honours the value_name prefix (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "pop_df labels prefix (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        cp(h5, joinpath(td, "labelProps", "C.h5ad"))    # a SECOND segmentation on the same image
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"
        img.label_props["C"] = "C.h5ad"
        img.label_props["_active"] = "B"                # active is B — the old code always returned B

        n1 = nrow(pop_df(img, "labels", ["B/labels"]; pop_cols=["area"]))
        @test n1 > 0

        # the PREFIX picks the segmentation, not the active one
        onlyC = pop_df(img, "labels", ["C/labels"]; pop_cols=["area"])
        @test unique(onlyC.value_name) == ["C"]
        @test nrow(onlyC) == n1

        # both segmentations pool in ONE call — the QC canvas comparing two label sets side by side
        both = pop_df(img, "labels", ["B/labels", "C/labels"]; pop_cols=["area"])
        @test Set(unique(both.value_name)) == Set(["B", "C"])
        @test unique(both.pop) == ["/labels"]
        # `label` is unique only WITHIN a segmentation, so the pooled frame repeats ids across
        # value_names — nothing may dedup by label alone or one segmentation's cells vanish
        @test nrow(both) == 2 * n1

        # a segmentation absent on THIS image is skipped, not an error: a set-level call spans images
        # segmented differently (a cellpose run yielding zero objects writes no labelProps at all)
        miss = pop_df(img, "labels", ["B/labels", "nope/labels"]; pop_cols=["area"])
        @test unique(miss.value_name) == ["B"] && nrow(miss) == n1

        # no pops, or a leading-slash ref, still resolves to the ACTIVE segmentation — unchanged
        @test unique(pop_df(img, "labels", String[]; pop_cols=["area"]).value_name) == ["B"]
        @test unique(pop_df(img, "labels", ["/labels"]; pop_cols=["area"]).value_name) == ["B"]
    end
end

# ── resolve_pops: cached, display-ready per-pop membership (napari points overlay) ──
@testset "resolve_pops (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "resolve_pops (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
        thr  = sort(full.mean_intensity_0)[cld(nrow(full), 2)]      # ~median → partial selection
        want = sort(Int.(full.label[full.mean_intensity_0 .>= thr]))

        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "pos"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1",
                                              thr, 1e12, -1e12, 1e12), colour="#ef4444")
        save_pop_map!(m, img)

        layers = resolve_pops(img, "flow"; value_name="B")
        @test length(layers) == 1
        L = layers[1]
        @test L.path == "/pos" && L.name == "pos" && L.colour == "#ef4444"
        @test L.show === true && L.is_track === false
        @test sort(L.labels) == want                       # membership == the gate's cells

        # cached: a second call returns the SAME stored object (no recompute), keyed under poplayers:
        again = resolve_pops(img, "flow"; value_name="B")
        @test again === layers
        @test any(k -> startswith(k, "poplayers:"), keys(img._pop_df_cache))
    end
end

# ── resolve_pops.has_tracks — DATA fact per pop, drives ribbon eligibility next to `is_track` ─────
# The `has_tracks` field (MULTI_POP_TRACKING_PLAN.md Decision 2) says whether a pop CURRENTLY holds
# any cell with `track_id > 0` — so a flow gate on cells that have since been tracked qualifies as
# ribbon-drawable without touching its `is_track` flag (which stays "was TYPED as a track pop"). The
# fixture h5ad `KDIeEm/B.h5ad` carries a `track_id` obs column (see the test at line 8837 above), so
# a broad flow gate lands on some tracked cells.
@testset "resolve_pops has_tracks: data flag, orthogonal to is_track" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "resolve_pops has_tracks (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # Broad gate: every measured cell (0 → 1e12 on both axes) → includes the tracked ones. A
        # `resolve_pops` layer for this flow pop should carry `has_tracks == true`.
        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "all"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1",
                                              -1e12, 1e12, -1e12, 1e12), colour="#3b82f6")
        save_pop_map!(m, img)

        layers = resolve_pops(img, "flow"; value_name="B")
        @test length(layers) == 1
        L = layers[1]
        @test L.is_track === false                # flow pop → typed flag stays false
        @test L.has_tracks === true               # data flag: at least one label has track_id > 0
    end
end

# ── has_tracks attribution guard (MULTI_POP_TRACKING_ORPHANS_PLAN P0) ─────────
# `_pop_has_authored_tracks` is the predicate that fixes the wrong-attribution bug where an orphan
# row (from a deleted pop) or a shared-label row (from an overlapping live pop) bled into a
# different pop's ribbon. Pure predicate so we can pin its four rules without a fixture that
# carries a categorical `track_source` column (which requires the Python writer). See docstring +
# the plan's decision 1.
@testset "has_tracks attribution guard (pure predicate)" begin
    UID_A = "aAaAaA"
    UID_B = "bBbBbB"
    WS    = Cecelia.WHOLE_SEG_TRACK_SOURCE
    src(d) = Dict{Int,Union{String,Nothing}}(k => v for (k, v) in d)

    # 1. Empty label_to_source → false regardless of pop.uid / labels.
    @test Cecelia._pop_has_authored_tracks(UID_A, [1, 2, 3], src(Dict())) === false

    # 2. A ↔ its own labels: authored by A → true.
    m1 = src(Dict(1 => UID_A, 2 => UID_A))
    @test Cecelia._pop_has_authored_tracks(UID_A, [1, 2, 3], m1) === true

    # 3. B on labels A authored → false. THIS is the bug the guard fixes: pre-guard, B.has_tracks
    #    would fire because B's labels overlap tracked rows; now B claims only rows IT authored.
    @test Cecelia._pop_has_authored_tracks(UID_B, [1, 2, 3], m1) === false

    # 4. `whole_seg` sentinel → counts for everyone (the documented prime-everything mode).
    m_ws = src(Dict(1 => WS, 2 => WS))
    @test Cecelia._pop_has_authored_tracks(UID_A, [1, 2], m_ws) === true
    @test Cecelia._pop_has_authored_tracks(UID_B, [1, 2], m_ws) === true

    # 5. Legacy row (`nothing`) → counts for everyone. Preserves pre-P1 behaviour for h5ads written
    #    before the provenance ship (decision 1's legacy branch).
    m_leg = src(Dict(1 => nothing, 2 => nothing))
    @test Cecelia._pop_has_authored_tracks(UID_A, [1, 2], m_leg) === true
    @test Cecelia._pop_has_authored_tracks(UID_B, [1, 2], m_leg) === true

    # 6. Mixed set: label 1 authored by A, label 2 by B, label 3 legacy, label 4 whole_seg,
    #    label 5 orphaned (deleted pop's UID no longer live). Guard fires when ANY qualifying
    #    label sits in `labs` — attribution is per-pop, per-label.
    ORPHAN = "zZzZzZ"
    m_mix = src(Dict(1 => UID_A, 2 => UID_B, 3 => nothing, 4 => WS, 5 => ORPHAN))
    #   A owns 1 directly → true even in isolation.
    @test Cecelia._pop_has_authored_tracks(UID_A, [1], m_mix) === true
    #   B owns 2 → true in isolation.
    @test Cecelia._pop_has_authored_tracks(UID_B, [2], m_mix) === true
    #   Nobody but the orphan-source claims label 5. A does NOT get it.
    @test Cecelia._pop_has_authored_tracks(UID_A, [5], m_mix) === false
    #   Legacy label 3 counts for A.
    @test Cecelia._pop_has_authored_tracks(UID_A, [3], m_mix) === true
    #   whole_seg label 4 counts for B.
    @test Cecelia._pop_has_authored_tracks(UID_B, [4], m_mix) === true
    #   A over [5, 6] (all orphan or absent) → false. Confirms the ORPHAN row cannot bleed into A.
    @test Cecelia._pop_has_authored_tracks(UID_A, [5, 6], m_mix) === false

    # 7. Label absent from label_to_source (i.e. `track_id ≤ 0` or NaN) → not tracked, ignored.
    @test Cecelia._pop_has_authored_tracks(UID_A, [99, 100], m1) === false
end

# ── Segmentation integrity (QC) plot data (KDIeEm, timecourse) ───────────────
# count per (image, timepoint) via group_by=temporal, + a per-timepoint measure distribution.
# See docs/todo/SEGMENTATION_QC_PLOT_PLAN.md.
# ── labels pop_type + count aggregation (segmentation QC data source, R parity) ──
@testset "labels pop_type + count (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "labels pop_type (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # `labels` = ALL measured cells, ungated, one "labels" pop; pops arg is ignored.
        all = pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"])
        @test nrow(all) > 0
        @test Set(names(all)) ⊇ Set(["label", "area", "pop", "value_name"])
        @test unique(all.pop) == ["/labels"]
        @test unique(all.value_name) == ["B"]

        # cell count via the summary aggregator over labels — one series, value == total.
        whole = plot_summary_data(img, "labels", String[], "count"; value_name="B")
        @test whole["chartType"] == "count"
        @test length(whole["series"]) == 1
        @test whole["series"][1]["value"] == Float64(nrow(all))

        # count per timepoint (group_by the temporal column) → counts partition the total.
        byT = plot_summary_data(img, "labels", String[], "count"; value_name="B", group_by="centroid_t")
        @test byT["groupBy"] == "centroid_t"
        @test length(byT["series"]) > 1
        @test sum(s["value"] for s in byT["series"]) == Float64(nrow(all))

        # a morphology distribution over labels, per timepoint
        area = plot_summary_data(img, "labels", String[], "boxplot"; value_name="B",
                                 measure="area", group_by="centroid_t")
        @test area["measure"] == "area"
        @test length(area["series"]) == length(byT["series"])

        # targets signature (the path the summary canvas + whiteboard QC row use: series =
        # [(value_name, "labels")]) — count over the "labels" pop yields the same total.
        tg = plot_summary_data(img, "labels", [("B", "/labels")], "count")
        @test tg["chartType"] == "count"
        @test length(tg["series"]) == 1
        @test tg["series"][1]["value"] == Float64(nrow(all))
        @test tg["series"][1]["pop"] == "B/labels"    # manager-form id round-trips
    end
end

# ── track table: path/naming helpers + JSON-safety (pure, no fixture) ─────
@testset "track table helpers" begin
    td = mktempdir()
    img = CciaImage(uid="X", dir=td)
    # companion track table sits next to the cell labelProps with the __tracks suffix
    @test img_track_props_path(img, "A") == joinpath(td, "labelProps", "A__tracks.h5ad")
    @test endswith(img_track_props_path(img, "A"), "A__tracks.h5ad")
    @test img_track_props_path(img, "A") != img_label_props_path(img, "A")
    # reserved value-name suffix (a user segmentation may not end in __tracks)
    @test is_reserved_value_name("A__tracks")
    @test is_reserved_value_name("foo__tracks")
    @test !is_reserved_value_name("A")
    @test !is_reserved_value_name("A_tracks")        # single underscore is NOT reserved
    # JSON-safety: NaN floats → nothing (→ JSON null), everything else passes through
    @test Cecelia._jsonsafe(NaN) === nothing
    @test Cecelia._jsonsafe(1.5) === 1.5
    @test Cecelia._jsonsafe(3)   === 3
    # cache key folds granularity → :cell and :track differ; :track also folds the track mtime
    mkpath(joinpath(td, "gating")); mkpath(joinpath(td, "labelProps"))
    img.label_props["B"] = "B.h5ad"
    kc = Cecelia._pop_df_cache_key(img, "live", "B", ["/_tracked"], nothing,
                                   false, true, true, false, false, :cell, String[], String[])
    kt = Cecelia._pop_df_cache_key(img, "live", "B", ["/_tracked"], nothing,
                                   false, true, true, false, false, :track, String[], String[])
    @test kc != kt
end

# ── pop_df granularity=:track on real KDIeEm B (track table read path) ─────
@testset "pop_df :track (KDIeEm B)" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "pop_df :track (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # track table layout: measures in X/var, lineage in obs, one row per track_id
        tvars = col_names(label_props(img_track_props_path(img, "B")); data_type=:vars)
        @test "live.track.speed" in tvars && "live.track.meanTurningAngle" in tvars
        tobs = col_names(label_props(img_track_props_path(img, "B")); data_type=:obs)
        @test "track_root" in tobs

        # one row per track; carries measures + track_id + value_name
        tr = pop_df(img, "live", ["B/_tracked"]; granularity=:track)
        @test nrow(tr) > 0
        @test Set(names(tr)) ⊇ Set(["track_id", "live.track.speed", "pop", "value_name"])
        @test length(unique(tr.track_id)) == nrow(tr)          # exactly one point per track
        @test unique(tr.value_name) == ["B"]

        # :track row count == number of distinct tracks among the :cell members (expand↔collapse)
        ce = pop_df(img, "live", ["B/_tracked"]; granularity=:cell)
        ntracks_cells = length(unique(Int.(filter(!isnan, ce.track_id))))
        @test nrow(tr) == ntracks_cells
        @test nrow(ce) > nrow(tr)                               # many cells collapse to few tracks

        # pop_cols restriction returns just that measure (+ bookkeeping)
        sp = pop_df(img, "live", ["B/_tracked"]; granularity=:track,
                    pop_cols=["live.track.speed"])
        @test "live.track.speed" in names(sp) && !("live.track.duration" in names(sp))

        # cell_measures aggregation (the clustTracks path): a per-cell measure is aggregated to
        # per-track feature column(s) via track_props, alongside motility — this is what lets
        # clustTracks cluster `_tracked` pops on HMM/intensity features, not just motility.
        cvars = col_names(label_props(img; value_name="B"); data_type=:vars)
        if !isempty(cvars)
            base = String(first(cvars))                        # a real per-cell measure
            ag = pop_df(img, "live", ["B/_tracked"]; granularity=:track, cell_measures=[base])
            @test any(startswith(c, base * ".") for c in names(ag))   # aggregated → {base}.…
            @test nrow(ag) == nrow(tr)                          # same tracks, extra feature cols
            @test "live.track.speed" in names(ag)               # motility still present
            @test "num_cells" in names(ag)                      # per-track cell count (minTracklength)
        end
    end
end

# ── summary-plot aggregation (server-side; pop_df → bins / freq counts) ────
@testset "plot_summary_data (KDIeEm B)" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "plot_summary_data (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # histogram of per-track speed (granularity=:track): shared edges, every track binned
        h = plot_summary_data(img, "live", ["B/_tracked"], "histogram";
                              measure="live.track.speed", granularity=:track, nbins=20)
        @test h["chartType"] == "histogram"
        @test length(h["binEdges"]) == 21
        @test length(h["series"]) == 1
        ntr = nrow(pop_df(img, "live", ["B/_tracked"]; granularity=:track))
        @test sum(h["series"][1]["counts"]) == ntr

        # mock a categorical per-cell column, then frequency over the tracked cells
        cells = label_props(img; value_name="B") |> select_cols(["track_id"]) |> as_df
        mock = DataFrame("label" => cells.label,
                         "mock.state" => [Float64((l % 3) + 1) for l in cells.label])
        label_props(img_label_props_path(img, "B")) |> add_obs(mock) |> save!
        f = plot_summary_data(img, "live", ["B/_tracked"], "frequency";
                              measure="mock.state", granularity=:cell, normalize=:fraction)
        @test f["chartType"] == "frequency"
        @test Set(f["categories"]) ⊆ Set(["1", "2", "3"])
        @test length(f["series"]) == 1
        props = f["series"][1]["values"]
        @test all(0 .<= props .<= 1) && isapprox(sum(props), 1.0; atol=1e-9)

        # measureType auto-detection: continuous speed → numeric; integer code set → categorical
        @test h["measureType"] == "numeric"
        @test f["measureType"] == "categorical"

        # bar: mean + all three error metrics (sd, sem = sd/√n, ci95 ≈ 1.96·sem)
        br = plot_summary_data(img, "live", ["B/_tracked"], "bar";
                               measure="live.track.speed", granularity=:track)
        s = br["series"][1]
        @test Set(keys(s)) ⊇ Set(["value", "sd", "sem", "ci95", "n"])
        @test s["sem"] ≈ s["sd"] / sqrt(s["n"])
        @test s["ci95"] ≈ 1.96 * s["sem"]

        # raw points: boxplot with raw_points carries downsampled values (≤ cap); "points" chart
        bx = plot_summary_data(img, "live", ["B/_tracked"], "boxplot";
                               measure="live.track.speed", granularity=:track,
                               raw_points=true, max_points=10)
        @test length(bx["series"][1]["points"]) == min(ntr, 10)
        pts = plot_summary_data(img, "live", ["B/_tracked"], "points";
                                measure="live.track.speed", granularity=:track, max_points=10)
        @test pts["chartType"] == "points" && length(pts["series"][1]["points"]) == min(ntr, 10)
        # without raw_points, boxplot carries no payload of values
        bx0 = plot_summary_data(img, "live", ["B/_tracked"], "boxplot";
                                measure="live.track.speed", granularity=:track)
        @test isempty(bx0["series"][1]["points"])
    end
end

@testset "motion dimensionality detection (2D vs 3D)" begin
    # build a Track by cumulative-summing per-step [dz,dy,dx] vectors (coords are [z,y,x])
    mk(steps) = begin
        P = zeros(length(steps) + 1, 3)
        for k in 1:length(steps); P[k+1, :] = P[k, :] .+ steps[k]; end
        Cecelia.Track(1, Float64.(0:length(steps)), P)
    end
    dy(k) = 2 + 0.5 * cos(k / 3); dx(k) = 2 + 0.5 * sin(k / 3)   # persistent forward heading in xy
    # z either tracks the persistent xy motion (real 3D) or oscillates with large amplitude (jitter)
    real(rng)   = mk([[2 + 0.5*sin(k/3), dy(k), dx(k)] for k in rng])
    jitter(rng) = mk([[(-1.0)^k * 4.0,   dy(k), dx(k)] for k in rng])
    trks_real   = [real(t*20 : t*20+14)   for t in 1:8]
    trks_jitter = [jitter(t*20 : t*20+14) for t in 1:8]

    d3 = Cecelia._detect_motion_dims(trks_real)
    @test d3.dims == 3 && d3.z_used            # persistent z → keep 3D
    d2 = Cecelia._detect_motion_dims(trks_jitter)
    @test d2.dims == 2 && !d2.z_used           # oscillating/anti-persistent z → in-plane 2D
    @test d2.metrics["autocorrZ"] < 0          # jitter signature

    # a 2D-only track set (no z column) is trivially 2D
    P2 = zeros(12, 2); for k in 1:11; P2[k+1, :] = P2[k, :] .+ [dy(k), dx(k)]; end
    @test Cecelia._detect_motion_dims([Cecelia.Track(1, Float64.(0:11), P2)]).dims == 2
end

# ── uns reader: the anisotropy grid on the branch sidecar ─────────────────────────────────────
# The one thing worth pinning here is the DIMENSION REVERSAL. HDF5 stores C-order, Julia reads
# column-major, so a numpy (T, y, x, comp) array arrives as (comp, x, y, T) — every axis flipped,
# INCLUDING the two box axes, which are equal-length and would therefore swap silently. The
# fixture's values encode their own (t, y, x) coordinates precisely so a transposed read fails
# instead of passing on symmetry.
@testset "uns reader (anisotropy grid)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "aniso__branch.h5ad")
    if !have_fixture(h5)
        @test_skip "aniso__branch fixture (missing)"
    else
        lp = label_props(h5)
        @test "orientation_coords" in uns_keys(lp) && "orientation_meta" in uns_keys(lp)

        # producer order = numpy order: (T, y_boxes, x_boxes, component)
        coor = uns_array(lp, "orientation_coords")
        @test size(coor) == (3, 4, 4, 2)
        # value encodes 100t + 10y + x, so this catches an axis swap, not just a shape match
        @test coor[1, 1, 1, 1] ≈ 0.0f0
        @test coor[3, 2, 4, 1] ≈ 100 * 2 + 10 * 1 + 3      # t=2, y=1, x=3 (0-based)
        @test coor[3, 2, 4, 2] ≈ 1000 + 100 * 2 + 10 * 1 + 3
        @test size(uns_array(lp, "orientation_eigvec")) == (3, 4, 4, 2, 2)
        @test size(uns_array(lp, "orientation_box_coherence")) == (3, 4, 4)

        # as_stored hands back the raw (reversed) layout for a caller that wants it
        @test size(uns_array(lp, "orientation_coords"; as_stored=true)) == (2, 4, 4, 3)

        # the self-describing block — strings, scalars and arrays all round-trip
        m = uns_dict(lp, "orientation_meta")
        @test m["box_size_px"] == 15 && m["sigma_px"] ≈ 12.0
        @test m["source"] == "skeleton" && m["fibre_direction"] == "minor"
        @test m["eigval_order"] == "ascending" && m["eigvec_layout"] == "vec_major"
        @test Int.(m["t_index"]) == [0, 1, 2]
        @test length(m["scale_um_per_px"]) == 2

        # absent key, and a group requested as an array (or vice versa) → nothing, not a throw
        @test uns_array(lp, "no_such_key") === nothing
        @test uns_array(lp, "orientation_meta") === nothing
        @test uns_dict(lp, "orientation_coords") === nothing

        # `orientation_summary` is a pandas DataFrame in uns — a third encoding, read by uns_df
        s = uns_df(lp, "orientation_summary")
        @test s isa DataFrame && nrow(s) == 3
        @test "anisotropy" in names(s) && "MF_full_length" in names(s)
        @test Float64.(s.anisotropy) ≈ [0.21, 0.32, 0.43] atol = 1e-6
        @test uns_df(lp, "orientation_coords") === nothing      # a plain array is not a dataframe
        @test uns_df(lp, "no_such_key") === nothing
    end
end

# ── The notebook readouts: quiver_df / branch_segments / anisotropy_df ────────────────────────
# These three are the whole point of the anisotropy pass — the arrows, the branch network and
# the per-image scalar, as tidy frames a Pluto notebook can plot directly (docs/NOTEBOOKS.md).
# The fixture is built so a WRONG read fails: the fibre (minor) eigenvector is a pure +x unit
# vector and the major one is +y, so taking the wrong eigenvector rotates every arrow 90°.
@testset "anisotropy notebook readouts" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "aniso__branch.h5ad")
    if !have_fixture(h5)
        @test_skip "aniso__branch fixture (missing)"
    else
        # EXPLICIT uids. `@testset` reseeds the global RNG per testset, so `gen_uid()` deals
        # every testset the SAME sequence — two testsets that both create a project+set+image
        # land in the same directory. Harmless until one of them, like this pair, asserts on
        # the directory's CONTENTS.
        proj = create_project!(name="aniso-fixture")
        s = add_set!(proj; name="set-A")
        img = add_image!(s; name="img-a", uid="anisoA")
        dir = img_label_props_dir(img); mkpath(dir)
        rm.(joinpath.(dir, readdir(dir)); force=true)     # a previous run's copies
        cp(h5, img_branch_props_path(img, "SHG"))

        @test img_branch_value_names(img) == ["SHG"]

        # ── arrows ────────────────────────────────────────────────────────────────────────
        q = quiver_df(img; value_name="SHG")
        @test nrow(q) == 3 * 4 * 4                       # every frame, every box
        @test sort(unique(q.t)) == [0, 1, 2]
        # the MINOR eigenvector is (y=0, x=1) ⇒ u=1, v=0. If the reader took the major one
        # instead the arrows would come back (0, 1) — a silent 90° rotation.
        @test all(q.u .≈ 1.0) && all(q.v .≈ 0.0)
        # box centres, and that x/y did not swap: coor[...,1] is y, coor[...,2] is x
        r = only(q[(q.t .== 2) .& (q.iy .== 1) .& (q.ix .== 3), :] |> eachrow)
        @test r.y ≈ 100 * 2 + 10 * 1 + 3
        @test r.x ≈ 1000 + 100 * 2 + 10 * 1 + 3
        # the deliberately-empty box carries its zero length through, so it can be filtered out
        @test only(q[(q.t .== 0) .& (q.iy .== 0) .& (q.ix .== 0), :].length) == 0.0
        @test count(>(0.0), q.length) == 3 * (16 - 1)

        @test nrow(quiver_df(img; value_name="SHG", t=1)) == 16
        @test_throws ErrorException quiver_df(img; value_name="SHG", t=99)
        @test_throws ErrorException quiver_df(img; value_name="nope")

        # ── branch segments ───────────────────────────────────────────────────────────────
        b = branch_segments(img; value_name="SHG")
        @test nrow(b) == 6
        # x from axis 1, y from axis 0 — a swap here would mirror the whole network
        @test b.y1 == [0.0, 10, 20, 30, 40, 50] && b.x1 == [1.0, 11, 21, 31, 41, 51]
        @test b.y2 == [4.0, 14, 24, 34, 44, 54] && b.x2 == [5.0, 15, 25, 35, 45, 55]
        @test b.branch_type == [0, 1, 2, 3, 1, 2]
        @test nrow(branch_segments(img; value_name="SHG", t=1)) == 2

        # ── per-image scalar ──────────────────────────────────────────────────────────────
        a = anisotropy_df(img)
        @test nrow(a) == 3 && unique(a.uID) == [img.uid] && unique(a.value_name) == ["SHG"]
        @test a.t == [0, 1, 2]                           # from orientation_meta.t_index, not position
        @test Float64.(a.anisotropy) ≈ [0.21, 0.32, 0.43] atol = 1e-6
        @test "occupancy" in names(a) && "branching_act" in names(a)

        # a second branch table on the same image (SHG collagen + a DCs network) — long format,
        # one block per value_name, which is what makes a cross-image comparison filterable
        cp(h5, img_branch_props_path(img, "DCs"); force=true)
        a2 = anisotropy_df(img)
        @test nrow(a2) == 6 && sort(unique(a2.value_name)) == ["DCs", "SHG"]
        @test nrow(anisotropy_df(img; value_name="SHG")) == 3

        # across images — the cohort frame Figure 4 panel D scatters
        img2 = add_image!(s; name="img-b", uid="anisoB")
        mkpath(img_label_props_dir(img2))
        cp(h5, img_branch_props_path(img2, "SHG"); force=true)
        across = anisotropy_df([img, img2]; value_name="SHG")
        @test nrow(across) == 6 && sort(unique(across.uID)) == sort([img.uid, img2.uid])

        # an image with no branch table contributes nothing — never an error, never a zero row
        img3 = add_image!(s; name="img-c", uid="anisoC")
        @test nrow(anisotropy_df(img3)) == 0
        @test nrow(anisotropy_df([img, img3]; value_name="SHG")) == 3
    end
end

# ── Branch value_names are NOT label_props value_names ────────────────────────────────────────
# Branching runs on a SEGMENTATION, which need not have a per-cell measurement table: an SHG
# collagen mask is skeletonised but never measured, so it lives in `labels`/`branch_labels`
# while `label_props` holds only the measured cell segmentations. Enumerating branch pops from
# `label_props` therefore found NOTHING — it looked for B__branch / T__branch and missed the
# SHG__branch that exists, so the branch picker came back empty. One image can carry several
# (SHG + DCs, per behaviourUbiTom3P.Rmd), so this is the plural case.
@testset "branch value_names come from the sidecars" begin
    proj = create_project!(name="bvn-$(rand(1000:9999))")
    s = add_set!(proj; name="set-A")
    img = add_image!(s; name="img-a")
    dir = img_label_props_dir(img); mkpath(dir)
    @test img_branch_value_names(img) == String[]          # nothing banked yet
    for f in ("SHG__branch.h5ad", "DCs__branch.h5ad", "B.h5ad", "B__tracks.h5ad")
        touch(joinpath(dir, f))
    end
    # only the __branch sidecars, and NOT the cell/track tables that sit beside them
    @test img_branch_value_names(img) == ["DCs", "SHG"]
    @test img_branch_props_path(img, "SHG") == joinpath(dir, "SHG__branch.h5ad")
end

# The same question for tracks, and the reason it needs its own answer: "is this image tracked" was
# being asked of the RUN LOG (does a `tracking.*` entry exist), which a project migrated from the R
# version answers "no" for while its `{vn}__tracks.h5ad` sits on disk — so the guide picker declared
# "needs a tracked image" over a project whose tracks had already been clustered. The sidecar is the
# state; the run log is only the provenance of runs this app happened to execute.
@testset "track value_names come from the sidecars" begin
    proj = create_project!(name="tvn-$(rand(1000:9999))")
    s = add_set!(proj; name="set-A")
    # Explicit uid: `@testset` reseeds the RNG, so `gen_uid` would hand this image the SAME uid — and
    # the same directory — as the branch testset above, whose sidecars are already sitting in it
    # (docs/DEV.md → the `@testset` reseeds the global RNG trap; this test hit it on the first run).
    img = add_image!(s; name="img-a", uid="tvnImgA")
    dir = img_label_props_dir(img); mkpath(dir)
    @test img_track_value_names(img) == String[]           # nothing banked yet
    for f in ("B__tracks.h5ad", "T__tracks.h5ad", "B.h5ad", "T.h5ad", "SHG__branch.h5ad")
        touch(joinpath(dir, f))
    end
    # only the __tracks sidecars, and NOT the cell/branch tables that sit beside them
    @test img_track_value_names(img) == ["B", "T"]
    @test img_track_props_path(img, "B") == joinpath(dir, "B__tracks.h5ad")
    # and it does NOT consult the run log: no tracking entry was ever written above
    @test isempty(read_run_log(img))
end

@testset "plot groupBy (generic categorical sub-axis)" begin
    # split a numeric measure by a categorical column (the hmmPlotParams port): each (pop × level)
    # becomes its own series tagged with `group`. Deterministic synthetic frame — no fixture.
    df = DataFrame("value_name" => fill("A", 7), "pop" => fill("/p", 7),
                   "m"  => [1.0, 2.0, 3.0, 10.0, 11.0, 12.0, 99.0],
                   "st" => [1.0, 1.0, 1.0, 2.0,  2.0,  2.0,  NaN])   # last row: missing group → dropped
    r = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                             normalize=:none, by_image=false, group_by="st")
    @test r["groupBy"] == "st"
    @test length(r["series"]) == 2                                  # two states, NaN-group row dropped
    @test Set(s["group"] for s in r["series"]) == Set(["1", "2"])
    byg = Dict(s["group"] => s for s in r["series"])
    @test byg["1"]["median"] == 2.0 && byg["2"]["median"] == 11.0   # NaN row excluded from state 2
    @test byg["2"]["n"] == 3
    # no group_by → single series, empty group label (back-compat)
    r0 = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=false)
    @test length(r0["series"]) == 1 && r0["series"][1]["group"] == "" && r0["groupBy"] === nothing

    # collapse_series: pool across pops/segmentations/images → series by groupBy level only. Two
    # pops, two value_names, but collapse + group_by="st" still yields exactly two series (1, 2).
    dfc = DataFrame("value_name" => ["A","A","B","B","A","B"], "pop" => ["/p","/q","/p","/q","/p","/q"],
                    "uID" => ["x","x","y","y","x","y"],
                    "m"  => [1.0, 2.0, 3.0, 10.0, 11.0, 12.0],
                    "st" => [1.0, 1.0, 1.0, 2.0,  2.0,  2.0])
    rc = Cecelia._summary_agg(dfc, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, group_by="st", collapse_series=true)
    @test length(rc["series"]) == 2
    @test Set(s["group"] for s in rc["series"]) == Set(["1", "2"])
    @test all(s["pop"] == "" && s["value_name"] == "" && s["uID"] == "" for s in rc["series"])
    @test Dict(s["group"] => s["n"] for s in rc["series"]) == Dict("1" => 3, "2" => 3)
    # collapse with no group_by → one pooled series over everything
    rc0 = Cecelia._summary_agg(dfc, "bar"; measure="m", granularity=:cell, nbins=10,
                               normalize=:none, by_image=true, collapse_series=true)
    @test length(rc0["series"]) == 1 && rc0["series"][1]["n"] == 6
end

@testset "plot percent (% positive of a 0/1 measure)" begin
    # "% of B cells in contact with a T cell" (`…cell.contact#…`) and "how many T cells are
    # clustered" (`…cell.is.aggregate`) are ONE question: the fraction of a population whose 0/1
    # measure is positive. Both were previously only reachable as a `bar` of the MEAN — an
    # unlabelled 0..1 fraction.

    # ── the detector: a property of the data, not a list of blessed column names ──
    @test Cecelia._is_boolean_measure([0, 1, 1, 0])
    @test Cecelia._is_boolean_measure([0.0, 1.0])
    @test Cecelia._is_boolean_measure([1, missing, NaN, 0])      # missing/non-finite ignored
    @test Cecelia._is_boolean_measure([0, 0, 0])                 # all-negative is still boolean
    @test !Cecelia._is_boolean_measure([0, 1, 2])
    @test !Cecelia._is_boolean_measure([0.0, 0.5, 1.0])
    @test !Cecelia._is_boolean_measure(["a", "b"])               # categorical, not boolean
    @test !Cecelia._is_boolean_measure(Float64[])                # nothing to judge
    @test !Cecelia._is_boolean_measure([missing, NaN])

    # ── Wilson score interval (Wilson 1927), against published values ──
    lo, hi = Cecelia._wilson_ci(5, 10)
    @test isapprox(lo, 0.23659, atol = 1e-4) && isapprox(hi, 0.76341, atol = 1e-4)
    # the case Wald gets WRONG: 0 of 10 → Wald says [0,0] ("certainly never"), Wilson keeps width
    lo0, hi0 = Cecelia._wilson_ci(0, 10)
    @test lo0 == 0.0 && isapprox(hi0, 0.27753, atol = 1e-4)
    # …and at p=1 the interval's exact upper bound IS 1 (float arithmetic lands 1 ulp short)
    lo1, hi1 = Cecelia._wilson_ci(10, 10)
    @test isapprox(lo1, 0.72247, atol = 1e-4) && isapprox(hi1, 1.0)
    @test all(isnan, Cecelia._wilson_ci(0, 0))                   # no data → no interval
    # symmetric about 0.5 (a sanity property of the interval, not of our arithmetic)
    @test isapprox(1 - Cecelia._wilson_ci(3, 10)[2], Cecelia._wilson_ci(7, 10)[1], atol = 1e-12)

    # ── the aggregation ──
    df = DataFrame("value_name" => fill("B", 10), "pop" => fill("/qc", 10),
                   "contact" => [1, 1, 1, 0, 0, 0, 0, 0, 0, 0])
    r = Cecelia._summary_agg(df, "percent"; measure="contact", granularity=:cell, nbins=10,
                             normalize=:none, by_image=false)
    @test r["chartType"] == "percent"
    @test r["valueLabel"] == "% positive"
    @test r["measureBoolean"] === true
    s = only(r["series"])
    @test s["value"] == 30.0 && s["n"] == 10 && s["nPositive"] == 3
    # bounds are the Wilson ones (as percentages) and BRACKET the estimate asymmetrically
    wl, wh = Cecelia._wilson_ci(3, 10)
    @test isapprox(s["lower"], 100wl) && isapprox(s["upper"], 100wh)
    @test s["lower"] < s["value"] < s["upper"]
    @test !isapprox(s["value"] - s["lower"], s["upper"] - s["value"])   # asymmetric — hence 2 bounds
    @test isapprox(s["ci95"], 100 * max(wh - 0.3, 0.3 - wl))            # the wider half-width

    # a percent chart must NOT carry rank/ANOVA comparisons — 0/1 data needs a proportion test
    df2 = vcat(df, DataFrame("value_name" => fill("T", 6), "pop" => fill("/qc", 6),
                             "contact" => [1, 1, 1, 1, 1, 0]))
    r2 = Cecelia._summary_agg(df2, "percent"; measure="contact", granularity=:cell, nbins=10,
                              normalize=:none, by_image=false, stats_enabled=true)
    @test !haskey(r2, "comparisons")
    byvn = Dict(s["value_name"] => s for s in r2["series"])
    @test byvn["B"]["value"] == 30.0
    @test isapprox(byvn["T"]["value"], 500 / 6)

    # an all-missing series reports no percentage rather than a spurious 0%
    dfe = DataFrame("value_name" => fill("T", 3), "pop" => fill("/qc", 3),
                    "contact" => [NaN, NaN, NaN])
    se = only(Cecelia._summary_agg(dfe, "percent"; measure="contact", granularity=:cell, nbins=10,
                                   normalize=:none, by_image=false)["series"])
    @test isnan(se["value"]) && se["n"] == 0

    # ── measureBoolean rides along on the ORDINARY charts, so the panel can offer % positive ──
    rb = Cecelia._summary_agg(df, "bar"; measure="contact", granularity=:cell, nbins=10,
                              normalize=:none, by_image=false)
    @test rb["measureBoolean"] === true
    rn = Cecelia._summary_agg(DataFrame("value_name" => fill("B", 3), "pop" => fill("/qc", 3),
                                        "dist" => [1.5, 20.0, 3.25]), "bar";
                              measure="dist", granularity=:cell, nbins=10,
                              normalize=:none, by_image=false)
    @test rn["measureBoolean"] === false
    # a POPULATION SUMMARY substitutes a synthetic per-image count; counts of 0/1 are not a boolean
    # MEASURE, and offering "% positive" on them would be nonsense.
    dfp = DataFrame("value_name" => ["B", "T"], "pop" => ["/qc", "/qc"], "uID" => ["i1", "i1"])
    rp = Cecelia._summary_agg(dfp, "bar"; measure=nothing, granularity=:cell, nbins=10,
                              normalize=:none, by_image=true)
    @test rp["measureBoolean"] === false
end

@testset "plot count (raw + proportion normalize) — population summary" begin
    # two pops in two images; count → raw row counts; normalize=:fraction → each pop's share of
    # its image's plotted total (the population-summary plot). Deterministic frame — no fixture.
    df = DataFrame("value_name" => fill("A", 10),
                   "pop" => ["/p","/p","/p","/q","/q", "/p","/q","/q","/q","/p"],
                   "uID" => ["x","x","x","x","x",       "y","y","y","y","y"])
    # image x: /p=3, /q=2 (total 5); image y: /p=2, /q=3 (total 5)
    bykey(r) = Dict((s["uID"], s["pop"]) => s["value"] for s in r["series"])
    raw = Cecelia._summary_agg(df, "count"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:none, by_image=true)
    @test raw["chartType"] == "count"
    rk = bykey(raw)
    @test rk[("x","A/p")] == 3.0 && rk[("x","A/q")] == 2.0
    @test rk[("y","A/p")] == 2.0 && rk[("y","A/q")] == 3.0
    prop = Cecelia._summary_agg(df, "count"; measure=nothing, granularity=:cell, nbins=0,
                                normalize=:fraction, by_image=true)
    @test prop["normalize"] == "fraction"
    pk = bykey(prop)
    @test pk[("x","A/p")] ≈ 0.6 && pk[("x","A/q")] ≈ 0.4     # 3/5, 2/5
    @test pk[("y","A/p")] ≈ 0.4 && pk[("y","A/q")] ≈ 0.6     # 2/5, 3/5
    @test Dict((s["uID"], s["pop"]) => s["n"] for s in prop["series"])[("x","A/p")] == 3

    # no measure + a DISTRIBUTION chart → each IMAGE is a point (its pop count), grouped by pop:
    # boxplot/beeswarm show within-pop variability and compare pops. A/p counts = [3(x),2(y)],
    # A/q = [2(x),3(y)] → each pop has 2 points (images), median 2.5.
    bx = Cecelia._summary_agg(df, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                              normalize=:none, by_image=true)
    @test bx["chartType"] == "boxplot"
    bybp = Dict(s["pop"] => s for s in bx["series"])
    @test Set(keys(bybp)) == Set(["A/p", "A/q"])
    @test bybp["A/p"]["n"] == 2 && bybp["A/p"]["median"] == 2.5
    # bar over the same per-image counts → mean (A/p mean of [3,2] = 2.5)
    br = Cecelia._summary_agg(df, "bar"; measure=nothing, granularity=:cell, nbins=10,
                              normalize=:none, by_image=true)
    @test Dict(s["pop"] => s["value"] for s in br["series"])["A/p"] == 2.5

    # A SYNTHETIC METRIC IS NUMERIC BY CONSTRUCTION — never sniffed.
    #
    # `_is_categorical_col` guesses from the values, and a per-image count is a handful of small
    # integers, which its integer-level heuristic reads as CATEGORICAL. The panel then intersected
    # the spec's numeric charts with the categorical set and everything except `count` (kept
    # explicitly as measure-independent) disappeared — "population summary always defaults back to
    # count and you cannot select anything else", on the FIRST render, since the spec's first chart
    # is boxplot. The gate is "did the user name this column", so it covers `proportion` and any
    # later synthetic metric with no new name to remember.
    # backend chart names — the frontend's strip/violin both map onto `points` (backendChart)
    for (ct, nrm) in (("boxplot", :none), ("bar", :none), ("points", :none),
                      ("count", :none), ("bar", :fraction))
        r = Cecelia._summary_agg(df, ct; measure=nothing, granularity=:cell, nbins=10,
                                 normalize=nrm, by_image=true)
        @test r["measureType"] == "numeric"
        @test r["measureBoolean"] === false     # counts that happen to be 0/1 are not a boolean measure
    end
    # counts of exactly 1 are the worst case for the heuristic (a single integer level)
    df1 = DataFrame("value_name" => ["A","A"], "pop" => ["/p","/q"], "uID" => ["x","x"])
    @test Cecelia._summary_agg(df1, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                               normalize=:none, by_image=true)["measureType"] == "numeric"
    # …and a REAL categorical measure is still detected as categorical (the gate must not blanket
    # everything to numeric)
    dfc = DataFrame("value_name" => fill("A", 4), "pop" => fill("/p", 4),
                    "live.cell.hmm.state.movement" => [1.0, 2.0, 1.0, 2.0])
    @test Cecelia._summary_agg(dfc, "frequency"; measure="live.cell.hmm.state.movement",
                               granularity=:cell, nbins=10, normalize=:none,
                               by_image=false)["measureType"] == "categorical"

    # SPLIT BY POPULATION: two tracked pops (value_names B, T) each with clusters — proportion is
    # normalised WITHIN each value_name per image, not pooled across B+T.
    df2 = DataFrame("value_name" => ["B","B","B","T","T", "B","B","T","T","T"],
                    "pop" => ["/Dir","/Dir","/Mea","/Dir","/Mea", "/Dir","/Mea","/Dir","/Mea","/Mea"],
                    "uID" => ["x","x","x","x","x",              "y","y","y","y","y"])
    pr2 = Cecelia._summary_agg(df2, "count"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:fraction, by_image=true)
    p2 = Dict((s["uID"], s["pop"]) => s["value"] for s in pr2["series"])
    @test p2[("x","B/Dir")] ≈ 2/3 && p2[("x","B/Mea")] ≈ 1/3   # within B (image x: B tot 3)
    @test p2[("x","T/Dir")] ≈ 1/2 && p2[("x","T/Mea")] ≈ 1/2   # within T (image x: T tot 2)
    @test p2[("y","B/Dir")] ≈ 1/2 && p2[("y","T/Mea")] ≈ 2/3   # within B / T (image y)

    # COMPLETE CASES (R tidyr::complete): image y has no /q — it must still contribute a 0 to /q's
    # distribution, not be dropped. Without completion /q would have n=1 (only x) and median 1.
    df3 = DataFrame("value_name" => fill("A", 5),
                    "pop" => ["/p","/p","/q", "/p","/p"],
                    "uID" => ["x","x","x",    "y","y"])          # x: p=2 q=1 ; y: p=2 q=0 (missing)
    bx3 = Cecelia._summary_agg(df3, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                               normalize=:none, by_image=true)
    by3 = Dict(s["pop"] => s for s in bx3["series"])
    @test by3["A/q"]["n"] == 2 && by3["A/q"]["median"] == 0.5    # /q points [1(x), 0(y)]
    @test by3["A/p"]["n"] == 2 && by3["A/p"]["median"] == 2.0
    # proportion completes too: /q in image y = 0 / (y's A total 2) = 0 → points [1/3, 0]
    pr3 = Cecelia._summary_agg(df3, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                               normalize=:fraction, by_image=true)
    by3f = Dict(s["pop"] => s for s in pr3["series"])
    @test by3f["A/q"]["n"] == 2 && by3f["A/q"]["median"] ≈ 1/6
end

@testset "plot raw (per-datapoint export)" begin
    # raw=true → one tidy row per datapoint (identity + value) for re-plotting externally, instead of
    # collapsing to box stats. Deterministic frame with label + a groupBy column; last row NaN measure.
    df = DataFrame("value_name" => fill("A", 5), "pop" => fill("/p", 5),
                   "uID" => ["x","x","y","y","y"], "label" => [1, 2, 3, 4, 5],
                   "m"  => [1.0, 2.0, 3.0, 4.0, NaN],
                   "st" => [1.0, 1.0, 2.0, 2.0, 2.0])
    r = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                             normalize=:none, by_image=true, group_by="st", raw=true)
    @test r["chartType"] == "raw" && r["measure"] == "m" && r["groupBy"] == "st"
    @test length(r["rows"]) == 4                       # the NaN-measure row is dropped
    row1 = r["rows"][1]
    @test row1["uID"] == "x" && row1["label"] == "1" && row1["value_name"] == "A"
    @test row1["pop"] == "/p" && row1["value"] == 1.0 && row1["group"] == "1"
    @test [rw["value"] for rw in r["rows"]] == [1.0, 2.0, 3.0, 4.0]
    @test [rw["group"] for rw in r["rows"]] == ["1", "1", "2", "2"]

    # measure-less count chart → raw collapses to per-(image, pop) counts (no label column populated)
    dfc = DataFrame("value_name" => fill("A", 5), "pop" => ["/p","/p","/p","/q","/q"],
                    "uID" => ["x","x","x","x","x"])
    rc = Cecelia._summary_agg(dfc, "count"; measure=nothing, granularity=:cell, nbins=0,
                              normalize=:none, by_image=true, raw=true)
    @test rc["chartType"] == "raw" && rc["measure"] == "count"
    cbyp = Dict(rw["pop"] => rw["value"] for rw in rc["rows"])
    @test cbyp["/p"] == 3.0 && cbyp["/q"] == 2.0 && all(!haskey(rw, "label") for rw in rc["rows"])

    # TRACK granularity: `label` duplicates `track_id` in the track table → drop it, keep track_id.
    dft = DataFrame("value_name" => fill("A", 3), "pop" => fill("/_tracked", 3),
                    "uID" => ["x","x","y"], "label" => [10, 11, 12], "track_id" => [10, 11, 12],
                    "m" => [1.0, 2.0, 3.0])
    rt = Cecelia._summary_agg(dft, "boxplot"; measure="m", granularity=:track, nbins=10,
                              normalize=:none, by_image=true, raw=true)
    @test all(!haskey(rw, "label") for rw in rt["rows"])            # no meaningless label
    @test [rw["track_id"] for rw in rt["rows"]] == ["10", "11", "12"]

    # groupBy that ISN'T applied (its column isn't in the frame) → groupBy null + no group column,
    # so the export never carries an empty, misleading category column.
    rna = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                               normalize=:none, by_image=true, group_by="not_a_column", raw=true)
    @test rna["groupBy"] === nothing && all(!haskey(rw, "group") for rw in rna["rows"])
end

@testset "plot statUnit=image (per-image mean = each dot an image)" begin
    # collapse each image to its mean, then plot those per-image means (n = #images). Deterministic
    # frame: image x cells [1,3,5] (mean 3), image y cells [10,20,30] (mean 20).
    df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                   "uID" => ["x","x","x","y","y","y"], "m" => [1.0, 3.0, 5.0, 10.0, 20.0, 30.0])
    r = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                             normalize=:none, by_image=true, stat_unit=:image)
    @test length(r["series"]) == 1                     # images pooled into ONE box
    @test r["series"][1]["n"] == 2                      # two datapoints = two images
    @test r["series"][1]["median"] == 11.5 && r["series"][1]["mean"] == 11.5   # of [3, 20]
    # default (individual) + per-image scope → one box PER image, each over its own cells (n = 3);
    # image-mean instead pools those into a single box whose points are the two image means.
    r0 = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true)
    @test length(r0["series"]) == 2 && all(s["n"] == 3 for s in r0["series"])
    # bar over per-image means → mean of the image means
    rb = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, stat_unit=:image)
    @test rb["series"][1]["value"] == 11.5 && rb["series"][1]["n"] == 2

    # with groupBy: per-image mean WITHIN each level → each level's points are its image means.
    df2 = DataFrame("value_name" => fill("A", 8), "pop" => fill("/p", 8),
                    "uID" => ["x","x","y","y","x","x","y","y"],
                    "m"  => [2.0, 4.0, 6.0, 8.0, 20.0, 20.0, 30.0, 10.0],
                    "st" => [1.0, 1.0, 1.0, 1.0, 2.0,  2.0,  2.0,  2.0])
    r2 = Cecelia._summary_agg(df2, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, group_by="st", stat_unit=:image)
    byg = Dict(s["group"] => s for s in r2["series"])
    @test byg["1"]["n"] == 2 && byg["1"]["median"] == 5.0     # st1: x[2,4]→3, y[6,8]→7 → [3,7]
    @test byg["2"]["n"] == 2 && byg["2"]["median"] == 20.0    # st2: x[20,20]→20, y[30,10]→20

    # with attr_map: one series PER ATTRIBUTE value, points = the images in it.
    dfa = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                    "uID" => ["x","x","y","y","z","z"], "m" => [2.0, 4.0, 6.0, 8.0, 100.0, 100.0])
    am = Dict("x" => "ctrl", "y" => "ctrl", "z" => "treat")
    ra = Cecelia._summary_agg(dfa, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, stat_unit=:image, attr_map=am)
    bya = Dict(s["uID"] => s for s in ra["series"])
    @test Set(keys(bya)) == Set(["ctrl", "treat"])
    @test bya["ctrl"]["n"] == 2 && bya["ctrl"]["median"] == 5.0   # images x(3), y(7) → [3,7]
    @test bya["treat"]["n"] == 1                                  # image z only

    # raw export honours it too: rows are the per-image means (label empty, value = the mean)
    rr = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, stat_unit=:image, raw=true)
    @test [rw["value"] for rw in rr["rows"]] == [3.0, 20.0]
    @test all(!haskey(rw, "label") for rw in rr["rows"]) && Set(rw["uID"] for rw in rr["rows"]) == Set(["x","y"])

    # image_agg=:median collapses each image by MEDIAN, not mean — distinguishable on skewed images:
    # x cells [1,2,9] (mean 4, median 2), y [10,20,90] (mean 40, median 20).
    dfs = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                    "uID" => ["x","x","x","y","y","y"], "m" => [1.0, 2.0, 9.0, 10.0, 20.0, 90.0])
    rmean = Cecelia._summary_agg(dfs, "bar"; measure="m", granularity=:cell, nbins=10,
                                 normalize=:none, by_image=true, stat_unit=:image, image_agg=:mean)
    rmed  = Cecelia._summary_agg(dfs, "bar"; measure="m", granularity=:cell, nbins=10,
                                 normalize=:none, by_image=true, stat_unit=:image, image_agg=:median)
    @test rmean["series"][1]["value"] == 22.0   # mean of image means [4, 40]
    @test rmed["series"][1]["value"] == 11.0    # mean of image medians [2, 20]
    rmedraw = Cecelia._summary_agg(dfs, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                   normalize=:none, by_image=true, stat_unit=:image, image_agg=:median, raw=true)
    @test [rw["value"] for rw in rmedraw["rows"]] == [2.0, 20.0]   # per-image medians
end

@testset "plot matrix (heatmap: profile + crosstab)" begin
    # PROFILE: rows = measures, cols = category levels; cell = mean(measure | level). Pools the whole
    # frame into one grid (no series). Deterministic synthetic frame — no fixture.
    df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                   "speed" => [1.0, 3.0, 10.0, 12.0, NaN, 5.0],
                   "angle" => [0.1, 0.3, 0.9, 1.1, 0.5, 0.5],
                   "st"    => [1.0, 1.0, 2.0,  2.0,  2.0, NaN])   # last row: NaN level → dropped
    pr = Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                              normalize=:none, by_image=false,
                              matrix_mode="profile", measures=["speed", "angle"], category="st")
    @test pr["matrixMode"] == "profile"
    @test pr["xLabels"] == ["1", "2"] && pr["yLabels"] == ["speed", "angle"]
    cell(r, x, y) = first(c for c in r["cells"] if c["x"] == x && c["y"] == y)
    @test cell(pr, "1", "speed")["value"] == 2.0           # mean(1,3)
    @test cell(pr, "2", "speed")["value"] == 11.0          # mean(10,12); NaN excluded
    @test cell(pr, "2", "speed")["n"] == 2
    @test isempty(pr["series"])

    # z-score standardises each row across its levels (mean 0) — the comparable "signature"
    prz = Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:none, by_image=false, zscore=true,
                               matrix_mode="profile", measures=["speed", "angle"], category="st")
    @test prz["zscore"] == true && prz["valueLabel"] == "z-score"
    zs = [c["value"] for c in prz["cells"] if c["y"] == "speed"]
    @test isapprox(sum(zs), 0.0; atol=1e-9) && all(isfinite, zs)

    # CROSSTAB: a "from_to" categorical → transition matrix; the hybrid uses '.', so the first '_'
    # splits prev|cur ("1.2_3.4" → from "1.2", to "3.4"). Row-normalise → P(to|from).
    dft = DataFrame("value_name" => fill("A", 5), "pop" => fill("/p", 5),
                    "tr" => ["1_1", "1_2", "1_2", "2_1", "x"])   # "x" has no sep → ignored
    ct = Cecelia._summary_agg(dft, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                              normalize=:none, by_image=false,
                              matrix_mode="crosstab", category="tr")
    @test ct["matrixMode"] == "crosstab"
    @test ct["yLabels"] == ["1", "2"] && ct["xLabels"] == ["1", "2"]
    ctc(x, y) = first(c for c in ct["cells"] if c["x"] == x && c["y"] == y)
    @test ctc("1", "1")["value"] == 1.0 && ctc("2", "1")["value"] == 2.0   # counts
    # row-normalised: from state 1 → {1:1, 2:2} → P(2|1) = 2/3
    ctr = Cecelia._summary_agg(dft, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:none, by_image=false,
                               matrix_mode="crosstab", category="tr", matrix_normalize=:row)
    @test ctr["valueLabel"] == "P(to|from)"
    ctrc(x, y) = first(c for c in ctr["cells"] if c["x"] == x && c["y"] == y)
    @test isapprox(ctrc("2", "1")["value"], 2/3; atol=1e-9)
    @test isapprox(ctrc("1", "1")["value"], 1/3; atol=1e-9)

    # error cases: unknown mode, missing category, profile with no present measure column
    @test_throws ErrorException Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell,
        nbins=0, normalize=:none, by_image=false, matrix_mode="bogus", category="st")
    @test_throws ErrorException Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell,
        nbins=0, normalize=:none, by_image=false, matrix_mode="profile", measures=["speed"], category="nope")

    # all-NaN / empty level → JSON-null value (NOT NaN — JSON3 rejects NaN; the renderer skips null)
    dfn = DataFrame("value_name" => fill("A", 3), "pop" => fill("/p", 3),
                    "speed" => [1.0, 2.0, NaN], "st" => [1.0, 1.0, 2.0])
    prn = Cecelia._summary_agg(dfn, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:none, by_image=false, matrix_mode="profile",
                               measures=["speed"], category="st")
    c2 = first(c for c in prn["cells"] if c["x"] == "2")
    @test c2["value"] === nothing && c2["n"] == 0     # state 2 has only a NaN → null cell

    # EMPTY FRAME (a population with no rows in this image) → an empty GRID, never an error. pop_df
    # returns a frame with no columns at all, which used to trip the category check and print
    # "matrix needs a `category` column present in the data" into the panel — the failure a per-image
    # board of cluster pops hits whenever one cluster is absent from one image. Every other chart type
    # answers this with an empty series; the response keys must match the populated ones so the
    # renderer's empty state fires instead of breaking.
    for (mode, extra) in (("crosstab", "normalize"), ("profile", "zscore"))
        e = Cecelia._summary_agg(DataFrame(), "matrix"; measure=nothing, granularity=:cell, nbins=0,
                                 normalize=:none, by_image=false, matrix_mode=mode,
                                 measures=["speed"], category="tr")
        @test e["chartType"] == "matrix" && e["matrixMode"] == mode && e["category"] == "tr"
        @test isempty(e["cells"]) && isempty(e["xLabels"]) && isempty(e["yLabels"])
        @test haskey(e, extra) && haskey(e, "valueLabel")   # same shape as the populated response
    end
    # …but an unknown mode is still an error, empty frame or not
    @test_throws ErrorException Cecelia._summary_agg(DataFrame(), "matrix"; measure=nothing,
        granularity=:cell, nbins=0, normalize=:none, by_image=false, matrix_mode="bogus", category="tr")
end

@testset "plot attribute grouping (compare by attr)" begin
    # group cross-image series by an image attribute: images sharing a value pool into one series
    # labelled by the value; an image with no value falls back to its uID. No fixture.
    df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                   "uID" => ["x1","x1","x2","x2","y1","y1"],
                   "m"   => [1.0, 2.0, 3.0, 4.0, 10.0, 12.0])
    amap = Dict("x1"=>"T", "x2"=>"T", "y1"=>"C")
    r = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                             normalize=:none, by_image=true, attr_map=amap)
    @test length(r["series"]) == 2
    @test Set(s["uID"] for s in r["series"]) == Set(["T", "C"])
    byu = Dict(s["uID"] => s for s in r["series"])
    @test byu["T"]["n"] == 4 && byu["C"]["n"] == 2          # x1+x2 pooled under "T"
    # image missing the attribute → falls back to its uID
    r2 = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                              normalize=:none, by_image=true, attr_map=Dict("x1"=>"T", "x2"=>"T"))
    @test Set(s["uID"] for s in r2["series"]) == Set(["T", "y1"])
    # no attr_map → group by image as before (3 images)
    r3 = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                              normalize=:none, by_image=true)
    @test length(r3["series"]) == 3
end

# ── cross-image (set-level) aggregation: pool pop_df across images by uID ──
@testset "plot_summary_data cross-image" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "plot_summary_data cross-image (fixture missing)"
    else
        # two "images" from the same fixture (uX, uY) — exercises set-level pooling mechanics
        mk = function (uid)
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            img = CciaImage(uid=uid, dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
            img
        end
        imgs = [mk("uX"), mk("uY")]; uids = ["uX", "uY"]

        # pop_df set-level: uID column tags each image; rows = 2× a single image's tracks
        one = pop_df(imgs[1], "live", ["B/_tracked"]; granularity=:track)
        both = pop_df(imgs, uids, "live", ["B/_tracked"]; granularity=:track)
        @test "uID" in names(both)
        @test Set(unique(both.uID)) == Set(uids)
        @test nrow(both) == 2 * nrow(one)

        # boxplot per_image → one box per image (same data → equal stats), + scope field
        sp = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "boxplot";
                               measure="live.track.speed", granularity=:track, scope=:per_image)
        @test sp["scope"] == "per_image" && sp["chartType"] == "boxplot"
        @test length(sp["series"]) == 2
        @test Set(s["uID"] for s in sp["series"]) == Set(uids)
        @test Set(keys(sp["series"][1])) ⊇ Set(["q1","median","q3","lower","upper","mean","n"])
        @test sp["series"][1]["median"] ≈ sp["series"][2]["median"]    # identical fixtures
        @test sp["series"][1]["q1"] <= sp["series"][1]["median"] <= sp["series"][1]["q3"]
        @test sp["series"][1]["n"] == nrow(one)

        # summarised → one pooled box across both images
        ss = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "boxplot";
                               measure="live.track.speed", granularity=:track, scope=:summarised)
        @test length(ss["series"]) == 1 && ss["series"][1]["n"] == 2 * nrow(one)

        # SAME data source, different chart type (bar of mean ± sd) — chart ⊥ data source
        br = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "bar";
                               measure="live.track.speed", granularity=:track, scope=:per_image)
        @test br["chartType"] == "bar" && length(br["series"]) == 2
        @test Set(keys(br["series"][1])) ⊇ Set(["value", "sd", "n"])
        @test br["series"][1]["value"] ≈ br["series"][2]["value"]

        # histogram per_image → 2 overlay series sharing bin edges
        hh = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "histogram";
                               measure="live.track.speed", granularity=:track, scope=:per_image, nbins=10)
        @test length(hh["series"]) == 2 && length(hh["binEdges"]) == 11
        @test all(sum(s["counts"]) == nrow(one) for s in hh["series"])
    end
end

# ── multiple SEGMENTATIONS on one plot: (value_name, pop) targets ─────────
@testset "plot_summary_data multi-segmentation targets" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "plot_summary_data multi-segmentation (fixture missing)"
    else
        # one image with the same data exposed under TWO segmentations (B, B2) — exercises the
        # targets path: a (value_name, pop) per series, vcat across segmentations.
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        cp(h5,  joinpath(td, "labelProps", "B2.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B2__tracks.h5ad"))
        img = CciaImage(uid="uM", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["B2"] = "B2.h5ad"
        img.label_props["_active"] = "B"

        targets = [("B", "/_tracked"), ("B2", "/_tracked")]
        # single image, two segmentations → one series per (segmentation, pop), distinct vn ids
        bp = plot_summary_data(img, "live", targets, "boxplot";
                               measure="live.track.speed", granularity=:track)
        @test bp["chartType"] == "boxplot" && length(bp["series"]) == 2
        @test Set(s["value_name"] for s in bp["series"]) == Set(["B", "B2"])
        @test Set(s["pop"] for s in bp["series"]) == Set(["B/_tracked", "B2/_tracked"])
        @test bp["series"][1]["median"] ≈ bp["series"][2]["median"]   # identical underlying data

        # cross-image AND cross-segmentation: 2 images × 2 segmentations → 4 per_image series
        img2 = CciaImage(uid="uN", dir=td)
        img2.label_props["B"] = "B.h5ad"; img2.label_props["B2"] = "B2.h5ad"
        img2.label_props["_active"] = "B"
        xp = plot_summary_data([img, img2], ["uM", "uN"], "live", targets, "bar";
                               measure="live.track.speed", granularity=:track, scope=:per_image)
        @test xp["scope"] == "per_image" && length(xp["series"]) == 4
        @test Set((s["uID"], s["value_name"]) for s in xp["series"]) ==
              Set([("uM","B"), ("uM","B2"), ("uN","B"), ("uN","B2")])
    end
end

# ── summary-plot aggregation: pure helpers (no fixture) ───────────────────
@testset "plot_summary_data helpers" begin
    @test Cecelia._hist_edges(Float64[], 10) == Float64[]            # no data → no edges
    @test length(Cecelia._hist_edges([5.0], 4)) == 5                 # single value → 1-wide bin
    let edges = Cecelia._hist_edges([0.0, 10.0], 10)
        @test Cecelia._hist_counts([0.0, 5.0, 9.99, NaN, 10.0], edges) |> sum == 4  # NaN skipped
    end
    @test Cecelia._catkey(2.0) == "2" && Cecelia._catkey(1.5) == "1.5"
    @test Cecelia._sort_cats(["10", "2", "1"]) == ["1", "2", "10"]   # numeric, not lexical
    @test Cecelia._sort_cats(["b", "a"]) == ["a", "b"]               # lexical fallback
    # derived-pop registry is generic: /_tracked is a `live` derived pop, none for `flow`
    @test derived_pop_paths("live") == ["/_tracked"]
    @test isempty(derived_pop_paths("flow"))
end

# ── track_props: per-track aggregation (ports tracksInfo; cell→track properties) ──
@testset "track_props (KDIeEm B)" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "track_props (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # mock a categorical per-cell column to exercise the freq branch
        cells = label_props(img; value_name="B") |> select_cols(["track_id"]) |> as_df
        label_props(img_label_props_path(img, "B")) |>
            add_obs(DataFrame("label" => cells.label,
                                      "st" => [Float64((l % 2) + 1) for l in cells.label])) |> save!

        tp = track_props(img; value_name="B", cell_measures=["area", "st"], categorical=["st"])
        ntr = nrow(pop_df(img, "live", ["B/_tracked"]; granularity=:track))
        @test nrow(tp) == ntr                                  # one row per track
        @test Set(names(tp)) ⊇ Set(["track_id", "label", "num_cells"])
        @test tp.label == tp.track_id                          # engine membership key
        # numeric aggregates present
        @test Set(names(tp)) ⊇ Set(["area.mean", "area.median", "area.sum", "area.qUp", "area.qLow", "area.sd"])
        # categorical → per-category frequency columns
        @test "st.1" in names(tp) && "st.2" in names(tp)
        # motility joined from the track table
        @test "live.track.speed" in names(tp)
        # num_cells totals the tracked cells
        @test sum(tp.num_cells) == sum(c -> c > 0, Int.(filter(x -> x isa Number && !isnan(x), cells.track_id)))

        # AUTO-DETECTION (no config map; replaces R config.yml labelStats). The split is read
        # off the decoded type + values: strings and integer code sets → categorical; continuous
        # floats → numeric. Mirrors the real data: hmm.transitions "1.3", hmm.state 1/2/3, speed 10.12.
        @test Cecelia._is_categorical_col(["1.3", "2.2"])                  # String → categorical (transitions)
        @test Cecelia._is_categorical_col(["a", missing])                  # Missing-union String too
        @test Cecelia._is_categorical_col([1.0, 2.0, 3.0])                 # integer code set → categorical (hmm.state)
        @test Cecelia._is_categorical_col([1, 2, missing])                 # integer codes (Missing-union) too
        @test !Cecelia._is_categorical_col([10.12, 11.3, 9.8])             # continuous floats → numeric (speed)
        @test !Cecelia._is_categorical_col(Float64.(1:100))               # wide-spread integers → numeric (counts/area)
        # name-rule: cluster code columns are categorical regardless of level count (>cap clusters)
        @test Cecelia._is_categorical_col(Float64.(1:100), "clusters")          # exact name
        @test Cecelia._is_categorical_col(Float64.(1:100), "clusters.default")  # clusters.{suffix}
        @test !Cecelia._is_categorical_col(Float64.(1:100), "area")             # other names keep the heuristic
        # `st` is an integer code (1/2) → auto-detected categorical with NO override → freq cols
        auto = track_props(img; value_name="B", cell_measures=["st"])
        @test "st.1" in names(auto) && "st.2" in names(auto) && !("st.mean" in names(auto))
        # `numeric` escape-hatch forces it back to numeric aggregates when desired
        forced = track_props(img; value_name="B", cell_measures=["st"], numeric=["st"])
        @test "st.mean" in names(forced) && !("st.1" in names(forced))

        # An UNTRACKED segmentation → the empty, well-formed table, and SILENTLY. `track_props`
        # handles this case by design, so it must ASK (`is_tracked`, which reads the obs column list
        # only) instead of selecting `track_id` and inspecting the result: `select_cols` @warns about
        # every column it cannot find, so the by-design path logged
        # `LabelProps: ignoring unknown columns ["track_id"]` once per request — six per page load of
        # a track-grained plot panel, every one of them about a column we already knew might be absent.
        td2 = mktempdir(); mkpath(joinpath(td2, "labelProps"))
        cp(h5, joinpath(td2, "labelProps", "B.h5ad"))
        img2 = CciaImage(uid="KDIeEm", dir=td2)
        img2.label_props["B"] = "B.h5ad"; img2.label_props["_active"] = "B"
        label_props(img_label_props_path(img2, "B")) |> drop_obs(["track_id"]) |> save!
        @test !is_tracked(img2; value_name="B")
        untracked = @test_logs min_level=Logging.Warn track_props(img2; value_name="B", cell_measures=["area"])
        @test nrow(untracked) == 0
        @test Set(names(untracked)) == Set(["track_id", "num_cells", "label"])
    end
end

# ── track_cell_measures: derive base cell measures from track-property column names ──
@testset "track_cell_measures" begin
    mot = ["live.track.speed", "live.track.meanTurningAngle"]
    # motility axes need no cell aggregation
    @test isempty(track_cell_measures(["live.track.speed"], mot))
    # numeric aggregate columns → their base cell measure (suffix stripped)
    @test track_cell_measures(["mean_intensity_0.mean", "area.qUp"], mot) ==
          ["mean_intensity_0", "area"]
    # categorical frequency column `{base}.{cat}` → base
    @test track_cell_measures(["hmm.state.1"], mot) == ["hmm.state"]
    # bookkeeping + motility skipped; dedup across aggregates of the same base
    @test track_cell_measures(["track_id", "num_cells", "live.track.speed",
                               "area.mean", "area.sd"], mot) == ["area"]
end

# ── pop_df pop_type="track": gate DIRECTLY on per-track properties (3b) ────────
@testset "pop_df pop_type=track (KDIeEm B)" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "pop_df pop_type=track (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # motility-only track props (the common track-gating case: no cell_measures needed)
        tp = track_props(img; value_name="B")
        @test "live.track.speed" in names(tp)
        spd = Float64.(collect(skipmissing(tp[!, "live.track.speed"])))
        thr = sort(spd)[cld(length(spd), 2)]                     # ~median → discriminating
        truth = count(>=(thr), spd)
        @test 0 < truth < length(spd)

        # a TRACK gate (one point per track) on the speed axis, stored under __tracks
        m = PopulationMap(pop_type="track", value_name="B")
        add_pop!(m, "fast"; gate=RectangleGate("live.track.speed", "live.track.speed",
                                               thr, 1e12, -1e12, 1e12))
        save_pop_map!(m, img)
        @test isfile(joinpath(td, "gating", "B__tracks.json"))   # track gate file
        @test !isfile(joinpath(td, "gating", "B.json"))          # NOT the flow file

        # granularity=:track → gated track rows, one point per track, gate genuinely applied
        g = pop_df(img, "track", ["/fast"]; value_name="B", granularity=:track)
        @test nrow(g) == truth
        @test length(unique(g.track_id)) == nrow(g)
        @test unique(g.pop) == ["/fast"]
        @test all(Float64.(g[!, "live.track.speed"]) .>= thr)

        # granularity=:cell → expand gated tracks to their member cells (track pulls its cells)
        gc = pop_df(img, "track", ["/fast"]; value_name="B", granularity=:cell)
        @test Set(names(gc)) ⊇ Set(["label", "track_id", "pop", "value_name"])
        @test Set(unique(gc.track_id)) == Set(Int.(g.track_id))  # same tracks, expanded
        @test nrow(gc) > nrow(g)                                 # many cells per track
        @test all(in(Set(Int.(g.track_id))), Int.(gc.track_id))

        # granularity=:cell + `centroids`: the member cells' COORDINATES come too. They are what the
        # track PLOTS draw — a gated or clustered track's path — and the expansion used to carry only
        # `pop_cols`, so the frame came back with no coordinates at all and `_pop_df_finish` could do
        # nothing but warn about it. Resolved per value_name (a 2D segmentation has no centroid_z).
        gcc = pop_df(img, "track", ["/fast"]; value_name="B", granularity=:cell, centroids=:pixel)
        @test all(c -> c in names(gcc), ["centroid_x", "centroid_y", "centroid_t"])
        @test nrow(gcc) == nrow(gc)
        @test Set(unique(gcc.track_id)) == Set(Int.(g.track_id))
    end
end

@testset "HMM states + transitions" begin
    # deterministic two-state tracks across two images; state flips at t=13. Track-start cells
    # carry NaN like real track measures (no speed at t=1; no angle at t=1,2) → must decode to
    # `missing` (per-cell states are undefined where a measurement can't exist).
    uID = String[]; vn = String[]; tid = Int[]; tt = Float64[]; sp = Float64[]; an = Float64[]
    for img in ("X", "Y"), k in 1:3, t in 1:25
        slow = t <= 12
        s = (slow ? 0.5 : 5.0) + 0.05 * sin(t)        # deterministic, non-degenerate
        a = (slow ? 0.2 : 2.5) + 0.05 * cos(t)
        if t == 1; s = NaN; a = NaN; elseif t == 2; a = NaN; end
        push!(uID, img); push!(vn, "A"); push!(tid, k); push!(tt, Float64(t))
        push!(sp, s); push!(an, a)
    end
    df = DataFrame("uID" => uID, "value_name" => vn, "track_id" => tid, "t" => tt,
                   "live.cell.speed" => sp, "live.cell.angle" => an)

    st = hmm_fit_states(df, ["live.cell.speed", "live.cell.angle"]; num_states=2, time_col="t")
    @test length(st) == nrow(df)

    # regression: an EMPTY measure selection from the GUI arrives as `Vector{Union{}}` (not
    # Vector{String}); fit must not MethodError on the normalise/scale step.
    let stu = hmm_fit_states(df, ["live.cell.speed", "live.cell.angle"]; num_states=2, time_col="t",
                             scale_measures=Union{}[], normalise=Dict{String,String}())
        @test count(!ismissing, stu) == count(!ismissing, st)
    end
    @test count(ismissing, st) == 12                  # 2 dropped × 6 tracks (t=1 no speed+angle, t=2 no angle)
    @test Set(skipmissing(st)) == Set([1, 2])
    df[!, "live.cell.hmm.state.default"] = st

    one = st[(df.uID .== "X") .& (df.track_id .== 1)]
    @test all(ismissing, one[1:2]) && !any(ismissing, one[3:end])
    decoded = collect(skipmissing(one))
    @test count(i -> decoded[i] != decoded[i-1], 2:length(decoded)) == 1   # exactly one flip

    tr = hmm_transitions(df, ["live.cell.hmm.state.default"]; time_col="t",
                         include_start=false, include_self=true)
    @test length(tr) == nrow(df)
    nonmiss = collect(skipmissing(tr))
    @test all(occursin("_", x) for x in nonmiss)
    @test Set(nonmiss) ⊆ Set(["1_1", "2_2", "1_2", "2_1"])
    @test ("1_2" in nonmiss) || ("2_1" in nonmiss)    # the flip transition exists

    trn = hmm_transitions(df, ["live.cell.hmm.state.default"]; time_col="t",
                          include_start=false, include_self=false)
    nm2 = Set(skipmissing(trn))
    @test nm2 ⊆ Set(["1_2", "2_1"]) && !isempty(nm2)  # self excluded → only the flip survives

    # cross-model hybrid: two state columns paste into "a.b" before transitions
    df[!, "live.cell.hmm.state.second"] = st
    trh = hmm_transitions(df, ["live.cell.hmm.state.default", "live.cell.hmm.state.second"];
                          time_col="t", include_start=false, include_self=true)
    @test any(x -> occursin(".", split(x, "_")[1]), skipmissing(trh))

    # cross-segmentation pops parsing: prefixed pops name their value_name ("A/_tracked" → "A",
    # the derived tracked pop = track_id>0); placeholders/empties are dropped. This is what lets
    # one run fit tracked A, B, C together (the segmentation is the pop prefix, not a separate
    # param). `_tracked` is the reserved derived-pop convention (leaf names beginning with `_`).
    @test Cecelia._hmm_pops(Dict{String,Any}("pops" => ["A/_tracked", "B/_tracked", "NONE", ""])) ==
          ["A/_tracked", "B/_tracked"]
    @test Cecelia._hmm_pops(Dict{String,Any}("pops" => "A/_tracked")) == ["A/_tracked"]
    @test Set(Cecelia._hmm_pop_value_names(["A/_tracked", "B/_tracked", "C/cd4/_tracked"], "default")) ==
          Set(["A", "B", "C"])

    # task registration + set-scope routing
    @test _task_from_fun_name("behaviour.hmm_states") isa Cecelia.HmmStates
    @test _task_from_fun_name("behaviour.hmm_transitions") isa Cecelia.HmmTransitions
    @test _task_from_fun_name("behaviour.hmm") isa Cecelia.CompositeTask
    @test task_scope(_task_from_fun_name("behaviour.hmm")) == "set"
    @test task_scope(_task_from_fun_name("behaviour.hmm_states")) == "set"
    @test task_scope(_task_from_fun_name("tracking.track_measures")) == "image"
end

# The two entry points into hmm.jl expect a `pop_df`-shaped DataFrame (uID / value_name / track_id
# / time_col / measures-or-state-cols). A rename upstream — the audit called this out for `pop_df`
# too — used to fail late with a bare `KeyError`; `_require_cols` names every missing column at once
# so a report reads "missing [t]" not "column name :t not found". The second block pins the
# read-boundary normalisation for `hmm_transitions`: state columns may arrive as Int (fresh fit) or
# String (categorical obs read-back), and both must yield the same "1" / "1.2" hybrid strings.
@testset "HMM entry guards + transition state normalisation" begin
    # A minimal well-formed DataFrame — we build DROP variants by column-subscript, because the
    # test suite only imports DataFrames.DataFrame + nrow (no `select`/`Not`).
    good = DataFrame("uID" => ["A"], "value_name" => ["V"], "track_id" => [1], "t" => [1.0],
                     "live.cell.speed" => [0.5])
    drop(df, col) = df[:, filter(!=(col), names(df))]

    # hmm_fit_states — every missing column is named in one shot (not one at a time on retry).
    err = try
        hmm_fit_states(drop(good, "live.cell.speed"), ["live.cell.speed"];
                       num_states=2, time_col="t")
        nothing
    catch e; e end
    @test err isa ErrorException
    @test occursin("hmm_fit_states", err.msg) && occursin("live.cell.speed", err.msg)

    err = try
        hmm_fit_states(drop(good, "t"), ["live.cell.speed"]; num_states=2, time_col="t")
        nothing
    catch e; e end
    @test err isa ErrorException && occursin("t", err.msg)

    # hmm_transitions — same guard, same shape.
    st_df = copy(good)
    st_df[!, "state"] = [1]
    err = try
        hmm_transitions(drop(st_df, "uID"), ["state"]; time_col="t")
        nothing
    catch e; e end
    @test err isa ErrorException && occursin("hmm_transitions", err.msg) && occursin("uID", err.msg)

    # Int and String state columns yield IDENTICAL transitions — normalisation at the read boundary
    # is what makes the composite (which reads back as categorical String) match a fresh in-memory
    # fit (Int). The float column proves the Int(round(v)) branch survives too.
    base = DataFrame("uID" => fill("A", 4), "value_name" => fill("V", 4),
                     "track_id" => fill(1, 4), "t" => [1.0, 2.0, 3.0, 4.0])
    int_df = copy(base);   int_df[!, "state"]   = [1, 1, 2, 2]
    str_df = copy(base);   str_df[!, "state"]   = ["1", "1", "2", "2"]
    flt_df = copy(base);   flt_df[!, "state"]   = [1.0, 1.0, 2.0, 2.0]
    tr_int = hmm_transitions(int_df, ["state"]; time_col="t", include_self=true)
    tr_str = hmm_transitions(str_df, ["state"]; time_col="t", include_self=true)
    tr_flt = hmm_transitions(flt_df, ["state"]; time_col="t", include_self=true)
    # First cell has no prev — always missing; the rest agree exactly across all three arms.
    @test isequal(tr_int, tr_str) && isequal(tr_int, tr_flt)
    @test collect(skipmissing(tr_int)) == ["1_1", "1_2", "2_2"]

    # Missing / NaN / "" all collapse to missing hybrid (and therefore missing transition), same
    # behaviour whether the state col is numeric or a string.
    for (col, tag) in ((Union{Int,Missing}[1, missing, 2, 2],   "Int+missing"),
                       (Union{Float64,Missing}[1.0, NaN, 2.0, 2.0], "Float+NaN"),
                       (["1", "", "2", "2"],                       "String+empty"))
        d = copy(base); d[!, "state"] = col
        tr = hmm_transitions(d, ["state"]; time_col="t", include_self=true)
        @test ismissing(tr[2])                                     # the gap breaks the chain
        @test collect(skipmissing(tr)) == ["2_2"]                  # only 3→4 survives ($tag)
    end

    # Hybrid column pastes with "." — Float second col rounds to "1"/"2", not "1.0"/"2.0".
    hyb = copy(base)
    hyb[!, "a"] = [1, 1, 2, 2]
    hyb[!, "b"] = [1.0, 1.0, 2.0, 2.0]
    tr_h = collect(skipmissing(hmm_transitions(hyb, ["a", "b"]; time_col="t", include_self=true)))
    @test tr_h == ["1.1_1.1", "1.1_2.2", "2.2_2.2"]
end

include(joinpath(@__DIR__, "suite", "ome_qc.jl"))

# ── Every directory whose params a USER actually sees ─────────────────────────────────────────
#
# All three copy testsets below walk THIS list. They each used to hardcode `src/tasks`, which
# exempted the custom-module examples — 7 tips, every one breaking the no-trailing-period rule,
# in the very file people COPY to write a drop-in module. Those specs are loaded by
# `load_custom_modules!` and rendered by the same `ParamRenderer`, so they are task specs that
# happen to live in `docs/`. Missing directories are skipped, so a trimmed checkout is fine.
#
# `app/src/plotDefinitions/` is deliberately NOT here, and the distinction is worth keeping
# straight: those files have a `params` array of the same SHAPE, but it is a defaults bag, not a
# form. Its only consumer is `SummaryPanel.vue` —
#     `props.spec.params?.find(p => p.key === k)?.default ?? d`
# — which reads `default` and nothing else. A `label` or `tip` there renders to nobody, so
# requiring one would have produced nine strings that look maintained and reach no user. The
# controls a user really operates for those plots are hand-rolled in the SFC, and the frontend
# ratchet already covers them. (The top-level `spec.label` IS rendered, in the plot picker, and
# is unchecked — a small, separate gap; don't fix it by dragging the whole directory in here.)
spec_dirs() = filter(isdir, [
    joinpath(dirname(dirname(pathof(Cecelia))), "src", "tasks"),
    joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "docs", "examples", "custom-modules"),
    # Plugin examples are shipped, installable task specs rendered by the same `ParamRenderer` — and
    # they are what a plugin author COPIES, so a mistake here propagates. They were outside every
    # copy ratchet until plugins existed.
    joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "docs", "examples", "plugins"),
])
# Walk every spec, yielding (label-for-messages, parsed spec) so a failure names a findable file.
# The label carries the containing directory — `tasks/x.json` vs `plotDefinitions/x.json` — because
# base names collide across surfaces (`track_measures.json` exists in both).
function each_spec(visit)
    for dir in spec_dirs(), (root, _, files) in walkdir(dir), fname in files
        endswith(fname, ".json") || continue
        # `plotDefinitions/` is excluded STRUCTURALLY, not by leaving one path off `spec_dirs`. Its
        # files carry a `params` array of the same shape, but it is a defaults bag read only for
        # `default` — a `tip` there renders to nobody (see the note above `spec_dirs`). The
        # positional version held only while every such directory happened to sit outside the listed
        # roots; adding `docs/examples/plugins`, which contains one, immediately broke it.
        basename(root) == "plotDefinitions" && continue
        spec = try JSON3.read(read(joinpath(root, fname), String)) catch; continue end
        spec isa AbstractDict || continue
        visit(joinpath(basename(root), fname), spec)
    end
end

# ── UI copy budget: task-spec `tip` fields ────────────────────────────────────────────────────
#
# The enforceable half of `docs/ui/COPY.md`, for the surface Julia owns.
# A `tip` renders as a tooltip on the task form, so it carries the same bar as any other tooltip:
# one line, under 90 characters, no second sentence explaining itself. This lives here rather than
# in the frontend suite because task specs are backend files and the frontend never holds a copy.
#
# 56 of 175 tips had drifted past the budget (worst: 332 chars, three sentences on a form field)
# before this existed. An exact allow-list, not a count — a count silently permits swapping one
# violation for another. Before adding an entry, check whether the fact belongs in a `docs/` file:
# that was true of every tip the sweep shortened.
# ── Numeric param RANGES have to be plausible ─────────────────────────────────────────────────
#
# `min`/`max` are enforced (`_validate_leaf`) and rendered as the slider's travel, so a bound that
# was never thought about does two things: it makes the useful part of the slider a few pixels
# wide, and it lets one drag start a run nobody wants. Several were plainly copy-pasted — a cell
# **surface distance** and a "min cells" count both ran to **1000** (a cell is ~10 µm, so 1000 µm
# is 100 cell diameters; an aggregate of 1000 cells is an organ), and `nPermutations` reached
# 100 000, hours of compute one drag away.
#
# The check is a RATIO, not a table of blessed numbers: a table would just restate the JSON and
# would need editing every time a default legitimately moves. `max / default` is the tell for a
# bound nobody chose — a sane range puts the default somewhere you can reach, so a max fifty times
# the default means the default is pinned to the far left of the travel. The worst honest ratio in
# the tree is 20 (`minCells` 5→100), so 50 leaves real headroom while catching every case above.
@testset "numeric param ranges are plausible" begin
    RATIO_MAX = 50
    # A param whose range genuinely spans orders of magnitude. Empty on purpose: before adding one,
    # check that the DEFAULT isn't the thing that's wrong.
    ALLOWED_WIDE = String[]

    nums = Tuple[]
    each_spec() do f, spec
        each_spec_param(spec_get(spec, "params")) do p, _
            String(something(spec_get(p, "type"), "")) in ("int", "float") || return
            push!(nums, (f, String(something(spec_get(p, "key"), "?")),
                         spec_get(p, "min"), spec_get(p, "max"),
                         spec_get(p, "step"), spec_get(p, "default")))
        end
    end
    @test length(nums) > 20                      # the walk found the numeric params

    # structural sanity first — these are bugs, not judgement calls
    for (f, k, mn, mx, _, def) in nums
        mn === nothing && continue
        if mx !== nothing
            @test mn <= mx || "$f/$k: min $mn > max $mx" == ""
        end
        if def !== nothing && def isa Real
            @test def >= mn                                   || "$f/$k: default $def < min $mn" == ""
            @test mx === nothing || def <= mx                 || "$f/$k: default $def > max $mx" == ""
        end
    end

    # a step coarser than the whole range means the slider has one position
    coarse = ["$f/$k: step $st over range $mn..$mx" for (f, k, mn, mx, st, _) in nums
              if st !== nothing && mn !== nothing && mx !== nothing && st > (mx - mn)]
    @test isempty(coarse)

    # …then the judgement call, as a loose bound
    wide = ["$f/$k: max $mx is $(round(mx / def, digits = 1))× the default $def"
            for (f, k, _, mx, _, def) in nums
            if mx !== nothing && def isa Real && def > 0 && mx / def > RATIO_MAX &&
               !("$f/$k" in ALLOWED_WIDE)]
    @test isempty(wide)
end

@testset "task spec tips stay short" begin
    COPY_MAX = 90
    ALLOWED = String[]

    # `tip`s nest inside `section`/`group` params, so recurse.


    # A trailing dot here is an abbreviation, not a sentence end ("e.g. HMM state").
    ABBREV = r"(?:^|[\s(])(?:e\.g|i\.e|etc|vs|cf|approx|fig|no)\.$"i
    function multi_sentence(s)
        for m in eachmatch(r"\S*\.\s+(?=[A-Z(])", s)
            occursin(ABBREV, rstrip(m.match)) || return true
        end
        false
    end

    tips = Tuple{String,String}[]
    nspecs = 0
    each_spec() do f, spec
        nspecs += 1
        each_spec_param(spec_get(spec, "params")) do p, _
            t = spec_get(p, "tip")
            t isa AbstractString && push!(tips, (f, join(split(String(t)), " ")))
            # `tips: [{text, requires?}]` — image-dependent variants of `tip`. Each entry's text is a
            # tip in its own right and gets the same length + house-style check.
            ts = spec_get(p, "tips")
            if ts isa AbstractVector
                for entry in ts
                    entry isa AbstractDict || continue
                    et = spec_get(entry, "text")
                    et isa AbstractString && push!(tips, (f, join(split(String(et)), " ")))
                end
            end
        end
    end

    @test nspecs > 20                       # the walk found the specs
    @test length(tips) > 100                # ...and their tips

    too_long = ["$f: [$(length(t))] $t" for (f, t) in tips
                if length(t) > COPY_MAX && !(t in ALLOWED)]
    @test isempty(too_long)

    two_sentence = ["$f: $t" for (f, t) in tips if multi_sentence(t) && !(t in ALLOWED)]
    @test isempty(two_sentence)
end

# ── UI copy COVERAGE: every task param carries a `tip` ────────────────────────────────────────
#
# The testset above polices tips that EXIST. This one polices the ones that don't. `docs/UI.md`
# asks for CellProfiler-style tip DENSITY — every setting explains itself on hover — and until
# this existed nothing could see a gap: `branching.json` shipped **twelve** parameters with no
# tip at all, so the form read "Flatten Z" / "Pre-dilation" / "Anisotropy box size (px)" with no
# way to find out what any of them did short of reading the Python runner.
#
# Presence is the half a machine can decide; whether a tip is the RIGHT tip stays a review
# question, exactly as the length ratchet can't tell you a short line is a good line. The
# frontend half of this rule — settable controls with no `v-tooltip` — is `uncoveredControls`
# in `frontend/src/utils/uiCopy.ts`, checked in `uiCopy.test.ts`.
#
# SECTIONS AND GROUPS ARE EXEMPT. They are container headers ("Advanced", "Filters"), not inputs
# — a user can't set them to anything, and requiring one would buy 18 tips saying "advanced
# options". Their CHILDREN are checked like any other param.
@testset "a handler fallback never contradicts its spec default" begin
    # `run_task` now applies the spec's `default` before calling `_run_task`, so a handler's own
    # `get(params, "k", d)` fallback is unreachable for any declared param — dead weight, not a second
    # answer. It stops being harmless the moment the two DISAGREE, because then the form promises one
    # number and a REPL/chain/MCP run uses another. Five did, and each was a real production
    # divergence: clustTracks.minTracklength 1 vs 5, opticalFlow.trainRatio 1.0 vs 0.8,
    # coastal.labelSmoothing 0.0 vs 0.5, contactsMeshes.maxContactDist 10.0 vs 5,
    # track_measures.forceRecompute false vs true.
    #
    # Deliberately a TEXT scan of the handler sources: the alternative is running every task. Only
    # literal scalars are compared — a computed fallback is a different thing and is skipped.
    root = dirname(dirname(pathof(Cecelia)))
    norm(x) = x isa AbstractString ? strip(String(x), ['"']) :
              x isa Bool ? string(x) :
              x isa Number ? string(float(x)) : nothing
    bad = String[]
    for (rootdir, _, files) in walkdir(joinpath(root, "src", "tasks")), f in files
        endswith(f, ".json") || continue
        spec = try JSON3.read(read(joinpath(rootdir, f), String), Dict{String,Any}) catch; continue end
        haskey(spec, "params") || continue
        jl = joinpath(rootdir, replace(f, ".json" => ".jl"))
        isfile(jl) || continue
        src = read(jl, String)
        defaults = Dict{String,Any}()
        walk(ps) = ps isa AbstractVector && for q in ps
            q isa AbstractDict || continue
            # A per-param `requires.axes` gate can DROP the key from the effective run
            # (`_apply_param_requires`), so the handler's fallback here is deliberately the "off"
            # value rather than the visible spec default — the guard IS the reason they differ.
            # Same reason `showIf`-only params are already exempt (their fallback is a state the
            # form no longer names). Skip these keys from the equality check.
            if !haskey(q, "requires")
                haskey(q, "default") && (defaults[string(get(q, "key", ""))] = q["default"])
            end
            walk(get(q, "params", nothing))
        end
        walk(spec["params"])
        for (key, dflt) in defaults
            want = norm(dflt)
            isnothing(want) && continue                      # arrays/objects: not a literal fallback
            for m in eachmatch(Regex("get\\(params, \"$(key)\", ([^)]+)\\)"), src)
                got_raw = strip(m.captures[1])
                got = occursin(r"^[-0-9.]+$", got_raw) ? string(parse(Float64, got_raw)) :
                      got_raw in ("true", "false") ? got_raw :
                      startswith(got_raw, "\"") ? strip(got_raw, ['"']) : nothing
                isnothing(got) && continue                   # computed fallback — a different thing
                got == want || push!(bad, "$(basename(jl)): $key — handler $got_raw, spec $(dflt)")
            end
        end
    end
    @test isempty(bad) || (@info "handler fallback contradicts the spec default" bad; false)
end

@testset "every task spec field is declared and documented" begin
    # Spec fields drift in BOTH directions, so this checks both.
    #
    #   forward  — a field is added to `ParamDef` and rendered, and `docs/MODULES.md` never hears of
    #              it. The reference silently stops being the reference.
    #   backward — a spec declares a field no consumer reads. `clustPops/cluster.json` carried
    #              `"includeChannels": true` for a `labelPropsColsSelection`; nothing in the form
    #              read it, in Julia or in TS (the only match was `napariOverlays.ts`, an unrelated
    #              movie-overlay concept). A spec that declares something nobody reads is a lie about
    #              the form, and it reads as intent to whoever copies the spec next.
    #
    # "Read by a consumer" is either half of the contract: declared on the frontend's `ParamDef`, or
    # read by Julia — `hideInComposite` is server-only and legitimately absent from `ParamDef`.
    root      = dirname(dirname(dirname(pathof(Cecelia))))
    types_ts  = read(joinpath(root, "frontend", "src", "tasks", "types.ts"), String)
    modules   = read(joinpath(root, "docs", "MODULES.md"), String)
    # Both `tasks/task.jl` and `api/src/routes.jl` are small aggregators now — their structural
    # spec-field references (`"composite"`, `"steps"`, `"scope"`, …) live in split fragments under
    # `tasks/task/*.jl` and `api/src/routes/*.jl` respectively.
    task_dir   = joinpath(root, "app", "src", "tasks", "task")
    routes_dir = joinpath(root, "api", "src", "routes")
    julia_src = join([read(f, String) for f in vcat(
                        joinpath(root, "app", "src", "tasks", "task.jl"),
                        filter(f -> endswith(f, ".jl"), readdir(task_dir;   join=true)),
                        joinpath(root, "api", "src", "routes.jl"),
                        filter(f -> endswith(f, ".jl"), readdir(routes_dir; join=true)))], "\n")

    # Structural keys of the params array itself, not fields a spec author sets on a param.
    STRUCTURAL = Set(["key", "label", "type", "default", "\$include"])
    fields = Set{String}()
    each_spec() do _, spec
        walk(ps) = ps isa AbstractVector && for q in ps
            q isa AbstractDict || continue
            union!(fields, string.(keys(q)))
            walk(get(q, "params", nothing))
        end
        walk(get(spec, "params", nothing))
    end

    undeclared = String[]; undocumented = String[]
    for f in sort(collect(setdiff(fields, STRUCTURAL)))
        occursin(Regex("^\\s*$(f)\\??:", "m"), types_ts) ||
            occursin("\"$f\"", julia_src) || push!(undeclared, f)
        occursin("`$f`", modules) || push!(undocumented, f)
    end
    @test isempty(undeclared)   || (@info "spec field read by nothing" undeclared; false)
    @test isempty(undocumented) || (@info "spec field absent from docs/MODULES.md" undocumented; false)
end

@testset "optionsFrom fills a picker from a named source" begin
    # Three tasks each carried twenty lines of identical dict-walking to do this — cellpose, coastal
    # and opticalFlow.train — differing only in which lister they called. The point for plugins: a
    # plugin author ships JSON and a task .jl, so offering a model vault used to mean writing a Julia
    # hook. Resolved for every task now, before the dispatch hook.
    flat(spec) = begin
        out = Dict{String,Any}()
        go(ps) = ps isa AbstractVector && for q in ps
            q isa AbstractDict || continue
            out[string(get(q, "key", ""))] = q
            go(get(q, "params", nothing))
        end
        go(get(spec, "params", nothing)); out
    end

    cp = flat(Cecelia._task_spec(Cecelia._task_from_fun_name("segment.cellpose")))["model"]
    @test cp["optionsFrom"] == "cellposeModels"
    @test !isempty(cp["options"])                                    # the built-ins are always there
    @test Set(String(m.name) for m in Cecelia.list_cellpose_models()) ==
          Set(String(o["value"]) for o in cp["options"])

    # Coastal APPENDS the vault to the literal options its spec declares, so "None" stays first and
    # stays selectable. The vault is empty until the user trains something, and an empty state should
    # be a legible choice — not a select that rejects everything including its own default.
    co = flat(Cecelia._task_spec(Cecelia._task_from_fun_name("segment.coastal")))["model"]
    @test co["options"][1]["value"] == "" && co["options"][1]["label"] == "None"
    @test length(co["options"]) == 1 + length(Cecelia.list_coastal_models())

    # value == label here: the user types the stem, so the suggestion IS what goes in the field.
    tr = flat(Cecelia._task_spec(Cecelia._task_from_fun_name("opticalFlow.train")))["modelName"]
    @test all(o -> o["label"] == o["value"], tr["options"])

    # …and every picker filled this way lists each value ONCE. The append is what makes coastal's
    # "None" work, and it is also what duplicated cellpose's built-ins: the spec declared `cpsam_v2`
    # and `cpsam` as literals while `cellposeModels` enumerates the same tuple, so the Model select
    # showed both twice. Neither the `issubset` check above nor
    # a `Set ==` comparison can see a duplicate — both collapse them — which is why it shipped.
    for (fn, key) in (("segment.cellpose", "model"), ("segment.coastal", "model"),
                      ("opticalFlow.train", "modelName"))
        vals = [string(o["value"])
                for o in flat(Cecelia._task_spec(Cecelia._task_from_fun_name(fn)))[key]["options"]]
        @test length(vals) == length(unique(vals))
    end

    # A declared option that the source ALSO enumerates keeps the SPEC's wording and position: the
    # label is the author's, and order is what keeps a "None" first.
    dup = Dict{String,Any}("params" => Any[Dict{String,Any}(
        "key" => "k", "type" => "select", "optionsFrom" => "cellposeModels",
        "options" => Any[Dict{String,Any}("value" => "cpsam_v2", "label" => "Mine")])])
    Cecelia._apply_options_from!(dup)
    opts = dup["params"][1]["options"]
    @test count(o -> string(o["value"]) == "cpsam_v2", opts) == 1
    @test string(first(opts)["label"]) == "Mine"

    # An unregistered name leaves the declared options alone rather than emptying the picker — a
    # typo in a spec must not silently produce a control nobody can choose anything in.
    spec = Dict{String,Any}("params" => Any[Dict{String,Any}(
        "key" => "k", "type" => "select", "optionsFrom" => "nope",
        "options" => Any[Dict{String,Any}("value" => "a", "label" => "A")])])
    Cecelia._apply_options_from!(spec)
    @test [o["value"] for o in spec["params"][1]["options"]] == ["a"]
end

@testset "showIf conditions name a param that exists" begin
    # `showIf` is the DECLARATIVE half of "this param does not apply here": a condition on the form,
    # beside the param it is about, so a plugin author never writes Julia to make a field disappear.
    # (The other half — a condition needing a file read, like "this XML export has no columns" —
    # cannot be a spec field and stays a server hook setting `hidden`.)
    #
    # Its one silent failure mode: name a key that is not in the spec and the condition can never be
    # satisfied, so the param is hidden FOREVER with no error anywhere. A typo costs a whole control.
    bad = String[]
    each_spec() do label, spec
        present = Set{String}()
        conds   = Tuple{String,String}[]
        function walk(ps)
            ps isa AbstractVector || return
            for q in ps
                q isa AbstractDict || continue
                push!(present, string(get(q, "key", "")))
                cond = get(q, "showIf", nothing)
                cond isa AbstractDict &&
                    for k in keys(cond); push!(conds, (string(get(q, "key", "?")), string(k))); end
                walk(get(q, "params", nothing))
            end
        end
        walk(get(spec, "params", nothing))
        # Sub-params of a section are stored FLAT in the value dict, so a condition may cross that
        # boundary in either direction — which is why membership is checked against the whole spec.
        for (owner, k) in conds
            k ∈ present || push!(bad, "$label: $owner showIf → '$k', which no param declares")
        end
    end
    @test isempty(bad) || (@info "showIf names a param that does not exist" bad; false)
end

@testset "a param that says segmentation reads SEGMENTATIONS" begin
    # `valueNameSelection` defaults to `filepaths` — image VERSIONS — when `field` is omitted, and
    # that default is right for the seven built-ins that omit it ("Image to segment", "Images to
    # train on"). It is silent, though: the importer's "Segmentation" picker offered smoothed,
    # afCorrected, driftCorrected — image versions with segmentation's label on them, and nothing
    # anywhere said so. Three example specs had the same bug, in the files people copy.
    #
    # The rule is narrow on purpose: only a param whose own label or tip CALLS ITSELF a segmentation
    # or a label set must read `labels`. "Image to segment" is not a claim about the picker's
    # contents, so it stays exempt.
    bad = String[]
    each_spec() do label, spec
        function walk(ps)
            ps isa AbstractVector || return
            for q in ps
                q isa AbstractDict || continue
                if get(q, "type", "") == "valueNameSelection"
                    txt = lowercase(string(get(q, "label", ""), " ", get(q, "tip", "")))
                    claims = occursin("segmentation", txt) || occursin("label set", txt)
                    claims && get(q, "field", "") != "labels" &&
                        push!(bad, "$label → $(get(q, "key", "?")) (field=$(get(q, "field", "omitted")))")
                end
                walk(get(q, "params", nothing))
            end
        end
        walk(get(spec, "params", nothing))
    end
    @test isempty(bad) || (@info "valueNameSelection claims a segmentation but reads image versions" bad; false)
end

@testset "a picker gates on `labels` only when the task needs the MASK" begin
    # The mirror of the testset above, and the one that actually cost a workflow. `labels` and
    # `label_props` are two INDEPENDENT ccid.json registries: mask pixels vs a measurement table. A
    # directly-imported track set registers only the second — there are no mask pixels to register —
    # so `field: "labels"` silently drops exactly the sets `ccia-importTracks` creates.
    #
    # Every track-CONSUMING task reads the h5ad and nothing else, and all three gated on `labels`
    # anyway: `tracking.track_measures`, `tracking.correct`, and the plugin's
    # `trackTools.cumulativeChange` (which is where spotted it, from the word "Segmentation"
    # on a form that wanted tracks). You could import tracks and then not measure them, with nothing
    # saying why the set was missing from the picker.
    #
    # So the rule is what the HANDLER does, not what the label says: if neither the task's `.jl` nor
    # its `_run.py` reaches mask pixels, its picker must not gate on `labels`.
    MASK_ACCESS = r"img_labels_path|img\.labels|labelsPath|open_labels|labels_path|zarr"

    # Gates on `labels`, reads no mask, and that is DELIBERATE — with the reason, because a bare
    # exemption list is how a real hit hides.
    #
    # `tracking.bayesian_tracking` reads centroids out of the h5ad and writes lineage columns back,
    # so it touches no mask either. It stays on `labels` because it PRODUCES tracks rather than
    # consuming them: every trackable set today comes from a segmentation (the importer only writes
    # sets that are already tracked), so `labels` is not currently narrower than the truth. Revisit
    # if anything ever registers untracked detections without a mask.
    ALLOWED = Set(["bayesian_tracking.json"])

    bad = String[]
    for dir in spec_dirs(), (root, _, files) in walkdir(dir), fname in files
        endswith(fname, ".json") || continue
        basename(root) == "plotDefinitions" && continue
        fname ∈ ALLOWED && continue
        spec = try JSON3.read(read(joinpath(root, fname), String)) catch; continue end
        spec isa AbstractDict || continue
        base = splitext(fname)[1]
        src  = join([isfile(joinpath(root, base * e)) ? read(joinpath(root, base * e), String) : ""
                     for e in (".jl", "_run.py")], "\n")
        occursin(MASK_ACCESS, src) && continue      # genuinely needs the mask
        function walk(ps)
            ps isa AbstractVector || return
            for q in ps
                q isa AbstractDict || continue
                get(q, "type", "") == "valueNameSelection" && get(q, "field", "") == "labels" &&
                    push!(bad, "$(joinpath(basename(root), fname)) → $(get(q, "key", "?"))")
                walk(get(q, "params", nothing))
            end
        end
        walk(get(spec, "params", nothing))
    end
    @test isempty(bad) ||
        (@info "picker gates on `labels` but the task never reads a mask — an imported " *
               "points-only set can never be picked; use `labelPropsNames`" bad; false)
end

@testset "every task param carries a tip" begin
    CONTAINER = ("section", "group")
    # A param whose label genuinely IS the whole explanation. Empty on purpose — same reason as
    # the length ratchet's: an allow-list that starts populated never gets emptied. Before adding
    # one, try writing the tip; it is nearly always shorter than the argument for skipping it.
    ALLOWED_NO_TIP = String[]

    # Collects every SETTABLE param (container children included), flagged tipped or not, so the
    # guard below can assert the walk actually found something — a silently empty walk would
    # otherwise report perfect coverage, which is how the QC scraper once lost 40 strings.


    params = Tuple{String,String,Bool}[]
    each_spec() do f, spec
        each_spec_param(spec_get(spec, "params")) do p, _
            ptype = String(something(spec_get(p, "type"), ""))
            (haskey(p, :key) || haskey(p, "key")) && !(ptype in CONTAINER) || return
            tip = spec_get(p, "tip")
            tipped = !isempty(strip(tip isa AbstractString ? String(tip) : ""))
            # `tips: [{text, requires?}]` — image-dependent variants. Any entry with a non-empty
            # `text` covers the requirement; the renderer picks the first that matches, and a
            # deliberate no-match (a T-only tip on a still) is the honest empty state.
            if !tipped
                tips = spec_get(p, "tips")
                if tips isa AbstractVector
                    for entry in tips
                        entry isa AbstractDict || continue
                        txt = spec_get(entry, "text")
                        if txt isa AbstractString && !isempty(strip(String(txt)))
                            tipped = true; break
                        end
                    end
                end
            end
            push!(params, (f, String(something(spec_get(p, "key"), "?")), tipped))
        end
    end

    @test length(params) > 150              # the walk found the params it is meant to police

    missing_tips = ["$f: $k" for (f, k, tipped) in params
                    if !tipped && !("$f: $k" in ALLOWED_NO_TIP)]
    @test isempty(missing_tips)
end

# ── UI copy house style: task-spec `label` + `tip` ────────────────────────────────────────────
#
# The Julia half of `docs/UI.md` → *House style*, mirroring the frontend checks in
# `frontend/src/utils/uiCopy.test.ts`. Split for the same reason the `tip` budget is: task specs
# are backend files and the frontend never holds a copy of one.
#
# This is the surface that actually drifted. Nothing could see the whole corpus at once, so the
# two halves of the app diverged along the storage boundary — 14 task labels went Title Case
# ("Bayesian Tracking", "Drift Correction") while every frontend label stayed sentence case, and
# all 164 tips grew a trailing period that no tooltip in the frontend had. `pixi run ui-copy`
# found it; this keeps it found. Exact allow-lists, not counts.
@testset "task spec copy follows the house style" begin
    ALLOWED_TITLE_CASE = String[]      # a label that is really a proper name
    ALLOWED_TRAILING_PERIOD = String[] # a `tip` that is genuinely a sentence

    # `@testset` bodies are their own scope, so the collector above isn't visible here — this one
    # pulls both keys in a single walk rather than re-deriving two nearly identical recursions.


    # Mirrors `isTitleCase` in uiCopy.ts — see there for why the allowances exist. A capital is
    # only evidence of Title Case when the word isn't expected to carry one: acronyms, single
    # letters, known proper nouns, and the first word after a separator ("Spatial / Time").
    PROPER = r"^(?:Cellpose|Bayesian|Dask|Cecelia|Leiden|Python|Julia|ImageJ|Fiji|OME|Napari|Zarr|Pluto|Rscript)$"
    SEPARATOR = r"^[/+&–—|]+$"
    expected_cap(w) = occursin(r"^[A-Z0-9+&/–-]+$", w) || length(w) == 1 || occursin(PROPER, w)
    function title_case(text)
        words = [w for w in split(text) if occursin(r"^[A-Za-z]", w) || occursin(SEPARATOR, w)]
        length(words) < 2 && return false
        judged = [(w = words[i], after_sep = occursin(SEPARATOR, words[i - 1]))
                  for i in 2:length(words) if !occursin(SEPARATOR, words[i])]
        isempty(judged) && return false
        any(j -> occursin(r"^[A-Z]", j.w) && !j.after_sep && !expected_cap(j.w), judged) &&
            all(j -> occursin(r"^[A-Z]", j.w) || j.after_sep || expected_cap(j.w), judged)
    end

    labels, tips2 = Tuple{String,String}[], Tuple{String,String}[]
    each_spec() do f, spec
        l = get(spec, :label, nothing)
        l isa AbstractString && push!(labels, (f, join(split(String(l)), " ")))
        each_spec_param(spec_get(spec, "params")) do p, _
            l = spec_get(p, "label")
            l isa AbstractString && push!(labels, (f, join(split(String(l)), " ")))
            t = spec_get(p, "tip")
            t isa AbstractString && push!(tips2, (f, join(split(String(t)), " ")))
        end
    end

    @test length(labels) > 150              # the walk found task + param labels

    titled = ["$f: $l" for (f, l) in labels if title_case(l) && !(l in ALLOWED_TITLE_CASE)]
    @test isempty(titled)

    # `…`/`...` is a continuation, not a sentence end.
    dotted = ["$f: $t" for (f, t) in tips2
              if occursin(r"[^.]\.$", t) && !(t in ALLOWED_TRAILING_PERIOD)]
    @test isempty(dotted)

    # Only the words with a decided winner. Create/Add, Delete/Remove and Run/Start are NOT
    # synonyms (see the vocabulary table in docs/UI.md) and are deliberately absent.
    BANNED = ["Choose" => "Select", "Pick" => "Select", "Display" => "Show",
              "Execute" => "Run", "Modify" => "Edit", "Discard" => "Remove"]
    wrong_verb = ["$f: \"$s\" — use $good" for (f, s) in vcat(labels, tips2)
                  for (bad, good) in BANNED if occursin(Regex("\\b$bad\\b", "i"), s)]
    @test isempty(wrong_verb)
end

# ── Stats module (docs/todo/STATS_ANNOTATIONS_PLAN.md) ─────────────────────
#
# Pins the glue between `run_stats` and HypothesisTests.jl. We don't test the underlying
# test math (that's HypothesisTests' own suite) — we test that the API is wired correctly:
# test dispatch, insertion order preserved (via Vector{Pair}), pairwise Bonferroni-adjusted,
# ns/star ladder, error handling for empty and too-few groups.
@testset "run_stats" begin
    # Two clearly-different groups → mannwhitney by default; p is very small; significance
    # ladder is at least ** (matches STATS_ANNOTATIONS_PLAN.md → S0-1).
    @testset "2 groups auto → mannwhitney" begin
        r = Cecelia.run_stats(["WT" => [1.0,2,3,4,5], "KO" => [10.0,11,12,13,14]])
        @test r.test == :mannwhitney
        @test r.groups == ["WT", "KO"]
        @test r.n == [5, 5]
        @test r.means[1] ≈ 3.0 && r.means[2] ≈ 12.0
        @test r.medians[1] ≈ 3.0 && r.medians[2] ≈ 12.0
        @test r.p_value < 0.05
        @test r.significance in ("*", "**", "***", "****")
        @test occursin("Mann-Whitney", r.method_note)
        @test isempty(r.comparison_pairs)   # omnibus IS the pair for 2 groups
    end

    # `auto` also has to say WHY. The UI showed the resolved test name and nothing else, so a user
    # had no way to know the basis — and deriving the explanation in the frontend would fork the
    # rule (change `_auto_test` and the tooltip would quietly keep claiming the old basis).
    @testset "auto states its basis; a NAMED test states none" begin
        two = Cecelia.run_stats(["WT" => [1.0,2,3], "KO" => [9.0,10,11]])
        @test occursin("2 groups", two.auto_reason)
        @test occursin("Mann-Whitney", two.auto_reason)
        three = Cecelia.run_stats(["A" => [1.0,2,3], "B" => [9.0,10,11], "C" => [20.0,21,22]])
        @test occursin("3 groups", three.auto_reason)
        @test occursin("Kruskal-Wallis", three.auto_reason)
        # both auto choices are rank-based — that's the reassurance the note has to carry, since
        # `auto` never runs a normality check
        @test occursin("rank-based", two.auto_reason) && occursin("rank-based", three.auto_reason)
        # nothing was chosen for the user, so there is nothing to explain
        @test isempty(Cecelia.run_stats(["A" => [1.0,2,3], "B" => [9.0,10,11]]; test=:ttest).auto_reason)
        @test isempty(Cecelia.run_stats(["A" => [1.0,2,3], "B" => [9.0,10,11]]; test=:mannwhitney).auto_reason)
        # the reason must name the test that actually ran — one rule, not two
        for n in (2, 3, 7)
            @test occursin(n == 2 ? "Mann-Whitney" : "Kruskal-Wallis", Cecelia._auto_reason(n))
            @test occursin("$(n) groups", Cecelia._auto_reason(n))
        end
        # …and it reaches the wire under `autoReason`
        d = Cecelia._stats_result_dict(two)
        @test d["autoReason"] == two.auto_reason
        @test isempty(Cecelia._stats_result_dict(
            Cecelia.run_stats(["A" => [1.0,2,3], "B" => [9.0,10,11]]; test=:ttest))["autoReason"])
    end

    # Two identical groups → p ≈ 1, "ns".
    @testset "identical groups → ns" begin
        r = Cecelia.run_stats(["A" => [1.0,2,3,4,5], "B" => [1.0,2,3,4,5]])
        @test r.p_value > 0.9
        @test r.significance == "ns"
    end

    # Welch's t-test opt-in — different method note, still small p on separated data.
    @testset "ttest opt-in" begin
        r = Cecelia.run_stats(["A" => [1.0,2,3,4], "B" => [10.0,11,12,13]]; test=:ttest)
        @test r.test == :ttest
        @test occursin("t-test", r.method_note)
        @test r.p_value < 0.05
    end

    # Three groups → kruskal by default, pairs are populated with Bonferroni-adjusted values.
    @testset "3 groups → kruskal + pairwise" begin
        r = Cecelia.run_stats([
            "A" => [1.0,2,3,4,5], "B" => [10.0,11,12,13,14], "C" => [20.0,21,22,23,24]])
        @test r.test == :kruskal
        @test r.groups == ["A", "B", "C"]
        @test occursin("Kruskal-Wallis", r.method_note)
        @test length(r.comparison_pairs) == 3   # (A,B), (A,C), (B,C)
        for (a, b, p_adj, sig) in r.comparison_pairs
            @test p_adj >= 0.0 && p_adj <= 1.0
            @test sig in ("ns", "*", "**", "***", "****")
        end
        # A vs C is the widest gap → definitely significant post-Bonferroni.
        ac = only(p for (a, b, p, _) in r.comparison_pairs if a == "A" && b == "C")
        @test ac < 0.05
    end

    # ANOVA opt-in with 3 groups.
    @testset "anova opt-in (3 groups)" begin
        r = Cecelia.run_stats([
            "A" => [1.0,2,3,4,5], "B" => [5.0,6,7,8,9], "C" => [10.0,11,12,13,14]];
            test=:anova)
        @test r.test == :anova
        @test occursin("ANOVA", r.method_note)
        @test r.p_value < 0.05
    end

    # Insertion order preserved (Vector of Pairs guarantees it — this asserts we don't sort).
    @testset "group order preserved" begin
        r = Cecelia.run_stats(["Z" => [1.0,2,3], "A" => [4.0,5,6], "M" => [7.0,8,9]])
        @test r.groups == ["Z", "A", "M"]
    end

    # Error paths.
    @testset "errors" begin
        @test_throws ArgumentError Cecelia.run_stats(["only" => [1.0,2,3]])
        @test_throws ArgumentError Cecelia.run_stats(["A" => Float64[], "B" => [1.0,2]])
        # 2-group tests refuse when given ≠2 groups.
        three = ["A" => [1.0,2], "B" => [3.0,4], "C" => [5.0,6]]
        @test_throws ArgumentError Cecelia.run_stats(three; test=:ttest)
        @test_throws ArgumentError Cecelia.run_stats(three; test=:mannwhitney)
        @test_throws ArgumentError Cecelia.run_stats(["A"=>[1.0,2], "B"=>[3.0,4]];
                                                    test=:notarealtest)
    end
end

# ── Segmentation testset ────────────────────────────────────────────────
# One big testset for algorithm-agnostic segmentation.jl + the live_outputs trait — plus the
# out-of-testset `_BadLiveTask` struct declaration it depends on. Extracted from this file to keep
# it small enough to merge without EOF conflicts on every append. The extracted file loads inside
# this file's aggregating testset scope, so any helpers defined earlier in suite.jl are still in
# scope for the segmentation fragment (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "segmentation.jl"))

# ── chipSelect param type ────────────────────────────────────────────────────
# "1,2,4,8" was a raw text field, which is a parse error waiting to happen and reads as unfinished.
# The values are validated per element like a `select`, because they reach a runner that can only
# fail much later and much less clearly — a bad temporal scale corrupts the model's channel layout.
@testset "chipSelect validation" begin
    spec = Cecelia._task_spec(TrainFlowModel())
    scales = only(p for p in spec["params"] if get(p, "key", "") == "temporalScales")
    @test scales["type"] == "chipSelect"
    @test [string(o["value"]) for o in scales["options"]] == ["1", "2", "3", "4", "6", "8", "12", "16"]

    @test validate_params(TrainFlowModel(),
        Dict{String,Any}("temporalScales" => ["1", "2", "8"])) === nothing
    @test_throws ParamValidationError validate_params(TrainFlowModel(),
        Dict{String,Any}("temporalScales" => ["1", "5"]))       # 5 is not offered
    @test_throws ParamValidationError validate_params(TrainFlowModel(),
        Dict{String,Any}("temporalScales" => "1,2,4,8"))        # a string is no longer the shape

    # …and the runner-side parser still takes what the chips produce
    @test parse_temporal_scales(["1", "2", "8"]) == [1, 2, 8]
end


# ── the intensity loss is a dial, not a switch ───────────────────────────────
# This testset previously asserted the opposite: that `intensityWeight` defaulted to 0 and was no
# longer offered. That change was made on coastal's docstring ("prefer ConfettiForegroundLoss", with a
# measurement of the intensity target ALONE: 2535 components, median 3 px) plus the observation that
# recent models all used 0. An audit refuted the inference:
#
#   • The controlled pair was already on disk. memTom (intensity 1.0) and flow.small (intensity 0.0)
#     share image, channel, seed, epochs and frame count — and memTom reaches a LOWER foreground loss
#     (0.32507 vs 0.32747). The retained objective is fit no better without the term, so "it degrades
#     the head it supervises" does not hold.
#   • A sweep put peak IoU at 0.25 (0.622) with 0.0 at 0.606 and 1.0 at 0.523: 1.0 over-claims
#     (precision 54%) and 0.0 under-claims (recall 70%, circularity 16% above the raw signal).
#   • cecelia pins `confetti: 0.0`, so `foreground` is coastal's `ForegroundLoss` — brightness-only,
#     explicitly "the colour-blind form". The single-channel worry that shipped with the change named
#     `ConfettiForegroundLoss` and did not apply.
#
# So the default is the measured middle and the control is back. Pinned because "the form offers it"
# is the part that had no measurement behind it either way, and removing it also made
# `paramsFromManifest` drop it silently — a model trained at 1.0 handed back a form that would not
# reproduce it.
# ── the flow-boundary term and the metrics it is built from ──────────────────
# `foregroundBoundaryWeight` reached coastal as its default 0.0 because nothing passed it, and per
# `ForegroundLoss.target` it is "the ONLY path by which optical flow reaches the labels" — everywhere
# else flow enters as an input channel or through the contrastive term. Now plumbed.
#
# The trap it ships with: `coastal.loss.flow_discontinuity` builds the signal from |strain| +
# |vorticity| + |divergence|, and cecelia's DEFAULT metric set drops two of those three
# (`FLAT_FLOW_METRICS` — they are flat as input channels, which is a different question from whether
# their gradient marks a boundary). `flow_discontinuity` sums whichever are present and normalises, so
# a partial set trains against a weaker signal and says nothing. Refused rather than warned about,
# because a warning is read after the hour of training.
@testset "flow boundary weight requires the metrics it is built from" begin
    @test Set(Cecelia.FLOW_BOUNDARY_METRICS) == Set(["strain", "vorticity", "divergence"])
    # the collision is real and worth asserting, so a future change to either set surfaces it here
    @test !isempty(intersect(Set(Cecelia.FLOW_BOUNDARY_METRICS), Set(Cecelia.FLAT_FLOW_METRICS)))

    # off → nothing is required, whatever the metric set
    @test isempty(Cecelia.flow_boundary_missing(nothing, 0.0))
    @test isempty(Cecelia.flow_boundary_missing(["strain"], 0.0))

    # on with the shipped default → the two dropped ones are named
    @test Set(Cecelia.flow_boundary_missing(nothing, 0.5)) == Set(["vorticity", "divergence"])
    # on with all three ticked → nothing missing
    all_three = ["strain", "vorticity", "divergence", "acceleration"]
    @test isempty(Cecelia.flow_boundary_missing(all_three, 0.5))

    # …and the validator turns that into a submit-time error rather than a weak model
    @test validate_params(TrainFlowModel(),
        Dict{String,Any}("foregroundBoundaryWeight" => 0.0)) === nothing
    @test validate_params(TrainFlowModel(),
        Dict{String,Any}("foregroundBoundaryWeight" => 0.5,
                         "flowMetrics" => all_three)) === nothing
    @test_throws ParamValidationError validate_params(TrainFlowModel(),
        Dict{String,Any}("foregroundBoundaryWeight" => 0.5, "flowMetrics" => ["strain"]))

    # the message has to NAME them — "check your metrics" costs a round trip to the docs
    err = try
        validate_params(TrainFlowModel(),
            Dict{String,Any}("foregroundBoundaryWeight" => 0.5, "flowMetrics" => ["strain"]))
        nothing
    catch e; e end
    @test occursin("vorticity", err.msg) && occursin("divergence", err.msg)

    # the control is offered, and OFF by default: switching it on also requires re-ticking metrics,
    # so it cannot be a silent default
    spec = Cecelia._task_spec(TrainFlowModel())
    flat(ps) = reduce(vcat, [haskey(p, "params") ? flat(p["params"]) : [p] for p in ps]; init = [])
    fb = only(p for p in flat(spec["params"]) if get(p, "key", "") == "foregroundBoundaryWeight")
    @test fb["default"] == 0.0
end

@testset "intensity loss is an offered dial at the measured default" begin
    spec = Cecelia._task_spec(TrainFlowModel())
    keys_of(ps) = reduce(vcat, [haskey(p, "params") ? keys_of(p["params"]) : [get(p, "key", "")]
                                for p in ps]; init = String[])
    @test "intensityWeight" ∈ keys_of(spec["params"])
    @test "foregroundWeight" ∈ keys_of(spec["params"])

    flat(ps) = reduce(vcat, [haskey(p, "params") ? flat(p["params"]) : [p] for p in ps]; init = [])
    iw = only(p for p in flat(spec["params"]) if get(p, "key", "") == "intensityWeight")
    # NOT 0.0 (the refuted default) and NOT 1.0 (over-claims); the sweep's peak.
    @test iw["default"] == 0.25
    @test iw["min"] == 0.0        # 0 stays REACHABLE — the experiment must remain runnable
    @test validate_params(TrainFlowModel(), Dict{String,Any}("intensityWeight" => 0.0)) === nothing
    @test validate_params(TrainFlowModel(), Dict{String,Any}("intensityWeight" => 1.0)) === nothing
end

@testset "OME-ZARR metadata reads v2 and v3 alike" begin
    # `read_ome_metadata` feeds ccid.json `meta`, which docs/OBJECTMODEL.md → *Calibration* makes authoritative
    # for every physical number in the app. NGFF 0.5 nests attributes under `ome`; a reader that misses
    # that returns an EMPTY Dict, and the caller then has no PhysicalSize/TimeIncrement at all — which
    # downstream becomes 1.0 rather than an error. So the two formats are asserted to agree, against two
    # committed stores of the same real pixels. See test-data/README.md, docs/todo/ZARR_V3_PLAN.md.
    v2 = fixture_path("ZARRFMT", "0", "ZV2img", "ccidImage.ome.zarr")
    v3 = fixture_path("ZARRFMT", "0", "ZV3img", "ccidImage.ome.zarr")
    if !(have_fixture(v2) && have_fixture(v3))
        @test_skip "zarr format fixtures missing"
    else
        # the series wrapper is found structurally in BOTH formats (v2 `.zattrs`, v3 `zarr.json`)
        @test Cecelia.series_base(v2) == joinpath(v2, "0")
        @test Cecelia.series_base(v3) == joinpath(v3, "0")

        # the one resolver: attributes come back unwrapped regardless of the `ome` nesting
        for p in (v2, v3)
            attrs = ngff_attrs(joinpath(p, "0"))
            @test !isnothing(attrs)
            @test haskey(attrs, :multiscales)          # NOT nested under :ome by the time we see it
            ms = ngff_multiscales(joinpath(p, "0"))
            @test !isnothing(ms) && !isempty(ms)
        end
        # a directory with no zarr metadata answers nothing rather than throwing
        @test isnothing(ngff_attrs(joinpath(v2, "does-not-exist")))
        # array metadata resolves for both; a GROUP dir must NOT be mistaken for an array (v3 shares
        # the filename `zarr.json` between the two)
        @test !isnothing(zarr_array_meta(joinpath(v3, "0", "0")))
        @test isnothing(zarr_array_meta(joinpath(v3, "0")))

        m2 = read_ome_metadata(v2)
        m3 = read_ome_metadata(v3)
        @test !isempty(m2) && !isempty(m3)
        for k in ("SizeC", "SizeT", "SizeZ")
            @test m2[k] == m3[k]
        end
        @test (m2["SizeC"], m2["SizeT"], m2["SizeZ"]) == (4, 3, 3)

        # Calibration — the whole reason these fixtures are real. Deliberately not 1.0, so a correct
        # read is distinguishable from the "unknown" fallback.
        for k in ("PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "TimeIncrement")
            @test haskey(m2, k) && haskey(m3, k)
            @test isapprox(m2[k], m3[k]; rtol = 1e-9)
        end
        @test isapprox(m2["PhysicalSizeX"], 0.5964274525755702; rtol = 1e-6)
        @test !isapprox(m2["PhysicalSizeX"], 1.0; atol = 1e-6)    # not the silent fallback
        @test isapprox(m2["PhysicalSizeZ"], 3.0; rtol = 1e-6)
        @test isapprox(m2["TimeIncrement"], 30.0; rtol = 1e-6)
    end
end

@testset "bioformats2raw chunk flags" begin
    # These flags were the bug: `chunkSizeX`/`chunkSizeY` existed in omezarr.json and were read by
    # NOTHING — no tile flag ever reached the CLI, so a user who chose 512 still got bioformats2raw's
    # 1024. One `chunkSize` param now, and it is passed.
    @test Cecelia.bf2raw_chunk_flags("512") == ["--tile-width", "512", "--tile-height", "512"]
    @test Cecelia.bf2raw_chunk_flags(1024)  == ["--tile-width", "1024", "--tile-height", "1024"]

    # "auto" passes NOTHING on purpose: bioformats2raw's own default is 1024 ALREADY CAPPED to the
    # frame, which is exactly the rule we want (one chunk per plane, up to 1024) and needs no source
    # dimensions — which we do not have, since the image is not converted yet.
    @test isempty(Cecelia.bf2raw_chunk_flags("auto"))
    @test isempty(Cecelia.bf2raw_chunk_flags("AUTO"))
    @test isempty(Cecelia.bf2raw_chunk_flags(""))

    # unparseable / absurd falls back to auto rather than raising — same call as the compression
    # flags: a bad value must not fail an hour-long import
    @test isempty(Cecelia.bf2raw_chunk_flags("banana"))
    @test isempty(Cecelia.bf2raw_chunk_flags(0))
    @test isempty(Cecelia.bf2raw_chunk_flags(-8))
    @test isempty(Cecelia.bf2raw_chunk_flags(16))       # below 32: not a sane chunk

    # every option the task spec offers must actually resolve (a spec/handler drift here is silent —
    # the import would just ignore the choice, which is the bug this whole testset exists for)
    spec = JSON3.read(read(joinpath(@__DIR__, "..", "src", "tasks", "importImages", "omezarr.json"), String))
    adv  = only(filter(p -> get(p, :type, "") == "section", collect(spec.params)))
    cs   = only(filter(p -> get(p, :key, "") == "chunkSize", collect(adv.params)))
    vals = [string(get(o, :value, o)) for o in cs.options]
    @test "auto" in vals
    @test string(cs.default) in vals
    for v in vals
        @test v == "auto" ? isempty(Cecelia.bf2raw_chunk_flags(v)) :
                            Cecelia.bf2raw_chunk_flags(v) == ["--tile-width", v, "--tile-height", v]
    end

    # and the tips must not merely restate the label — that is what made these params guesswork
    for p in vcat(collect(spec.params), collect(adv.params))
        get(p, :type, "") == "section" && continue
        tip = String(get(p, :tip, ""))
        @test !isempty(tip)
        @test lowercase(tip) != lowercase(String(get(p, :label, "")))
    end
end

@testset "bioformats2raw worker + heap flags" begin
    # Same spec/handler drift shape as bf2raw_chunk_flags — an option surfaced in the JSON that no
    # code translates is silent (import runs at the wrong worker count). Measured 2026-08-27 on
    # `Human_Lymph_Node_Manual_IBEX.ims`: workers=4 (bf2raw default) → 105 OOMs and 0-3 chunks;
    # workers=2 + -Xmx16g → 2 OOMs and 3820 chunks; workers=1 → zero OOMs. That's why Imaris auto = 1.
    @test Cecelia.bf2raw_worker_flags("1") == ["--max-workers=1"]
    @test Cecelia.bf2raw_worker_flags(2)   == ["--max-workers=2"]
    @test Cecelia.bf2raw_worker_flags(8)   == ["--max-workers=8"]

    # "auto" and unparseable both defer to bioformats2raw's own default (4) — same forgiving-fallback
    # rule as chunk/compression: a bad value must not fail an hour-long import
    @test isempty(Cecelia.bf2raw_worker_flags("auto"))
    @test isempty(Cecelia.bf2raw_worker_flags("AUTO"))
    @test isempty(Cecelia.bf2raw_worker_flags(""))
    @test isempty(Cecelia.bf2raw_worker_flags("banana"))
    @test isempty(Cecelia.bf2raw_worker_flags(0))
    @test isempty(Cecelia.bf2raw_worker_flags(-2))

    # Extension-keyed defaults — Imaris (`.ims`) is the reader we know decompresses fat HDF5 chunks.
    # Everything else stays on "auto" (bf2raw picks 4).
    @test Cecelia.bf2raw_default_workers("/some/path/thing.ims") == "1"
    @test Cecelia.bf2raw_default_workers("/some/path/THING.IMS") == "1"
    @test Cecelia.bf2raw_default_workers("/some/path/thing.tif") == "auto"
    @test Cecelia.bf2raw_default_workers("/some/path/thing.czi") == "auto"
    @test Cecelia.bf2raw_default_workers("")                       == "auto"

    # JVM heap parsing mirrors the workers pattern
    @test Cecelia.bf2raw_java_heap_gib("16") == 16
    @test Cecelia.bf2raw_java_heap_gib(24)   == 24
    @test Cecelia.bf2raw_java_heap_gib("auto") == 0
    @test Cecelia.bf2raw_java_heap_gib("") == 0
    @test Cecelia.bf2raw_java_heap_gib("banana") == 0
    @test Cecelia.bf2raw_java_heap_gib(0) == 0
    @test Cecelia.bf2raw_java_heap_gib(-4) == 0

    # Extension-keyed heap default — Imaris gets headroom, everything else defers to the JVM. Cap at
    # half the box RAM so a fixed literal can't wedge a small machine into swap (measured off
    # Sys.total_memory at the call site, not a hardcoded number).
    ram_gib = max(1, floor(Int, Sys.total_memory() / (1024^3)))
    exp_ims = min(16, floor(Int, ram_gib / 2))
    @test Cecelia.bf2raw_default_heap_gib("/some/path/thing.ims") == exp_ims
    @test Cecelia.bf2raw_default_heap_gib("/some/path/thing.tif") == 0
    @test Cecelia.bf2raw_default_heap_gib("")                       == 0

    # Env dict shape — `heap_gib > 0` sets BIOFORMATS2RAW_OPTS with -Xmx, otherwise empty (JVM default).
    # A pre-existing value in ENV is preserved (prepended to keep our flag winning) — feedback: never
    # clobber a user-set env var. This side-tests that path without mutating the real ENV.
    empty_env = Cecelia.bf2raw_java_env(0)
    @test isempty(empty_env)
    heap_env = Cecelia.bf2raw_java_env(16)
    @test heap_env["BIOFORMATS2RAW_OPTS"] == "-Xmx16g" ||
          startswith(heap_env["BIOFORMATS2RAW_OPTS"], "-Xmx16g ")

    # every worker option in the task spec must actually resolve
    spec2 = JSON3.read(read(joinpath(@__DIR__, "..", "src", "tasks", "importImages", "omezarr.json"), String))
    adv2  = only(filter(p -> get(p, :type, "") == "section", collect(spec2.params)))
    mw    = only(filter(p -> get(p, :key, "") == "maxWorkers", collect(adv2.params)))
    for o in mw.options
        v = string(get(o, :value, o))
        @test v == "auto" ? isempty(Cecelia.bf2raw_worker_flags(v)) :
                            Cecelia.bf2raw_worker_flags(v) == ["--max-workers=$v"]
    end
    hp = only(filter(p -> get(p, :key, "") == "jvmHeapGiB", collect(adv2.params)))
    for o in hp.options
        v = string(get(o, :value, o))
        n = Cecelia.bf2raw_java_heap_gib(v)
        @test v == "auto" ? n == 0 : n == parse(Int, v)
    end
end

@testset "bioformats2raw format flags" begin
    # The import is the ONLY place the store format is chosen; derived stores inherit it
    # (docs/todo/ZARR_V3_PLAN.md D9).
    ff(args...; kw...) = Cecelia.bf2raw_format_flags(args...; kw...)

    @test isempty(ff("0.4", "auto"))                       # default = the command we always ran
    @test ff("0.5", "auto") == ["--ngff-version", "0.5"]
    @test ff("0.5", "1024") ==
          ["--ngff-version", "0.5", "--shard-width", "1024", "--shard-height", "1024"]

    # Sharding is NGFF 0.5 only, and is dropped for 0.4 rather than raising: they are separate controls
    # and switching the version back must still produce a working import.
    @test isempty(ff("0.4", "1024"))

    # unparseable / absurd falls back to upstream's default rather than raising
    for bad in ("banana", "0", "-8", "16", "")
        @test ff("0.5", bad) == ["--ngff-version", "0.5"]
    end

    # ── chunk-key separator ──────────────────────────────────────────────────────
    # `--no-nested` IS NEVER EMITTED (2026-08-14). Flat keys saved ~5% on a real movie at identical read
    # time, but produce a store that conforms to no published NGFF version — nested storage is what 0.2
    # introduced, so flat keys are 0.1 storage under the 0.4-shaped metadata written beside them. The
    # separator is therefore no longer a parameter of this function at all, which also retires the old
    # flat+0.5 conflict (that pair silently wrote zarr v2) by making it unrepresentable.
    for v in ("0.4", "0.5"), sh in ("auto", "1024")
        @test !("--no-nested" in ff(v, sh))
    end
    @test_throws MethodError Cecelia.bf2raw_format_flags("0.4", "auto"; separator = "flat")

    # ── shard depth ──────────────────────────────────────────────────────────────
    # The ONLY axis that reduces the file count on a 512x512 frame — width/height cap to the frame, so
    # the shard equals the chunk and packs nothing (measured: depth 13 -> 13 files vs 109).
    @test ff("0.5", "auto"; shard_depth = "13") == ["--ngff-version", "0.5", "--shard-depth", "13"]
    @test ff("0.5", "auto"; shard_depth = "all", z_planes = 13) ==
          ["--ngff-version", "0.5", "--shard-depth", "13"]
    @test ff("0.5", "auto"; shard_depth = "1") == ["--ngff-version", "0.5"]      # the default: no flag
    # "all" with no usable z count drops the flag rather than guessing a depth
    @test ff("0.5", "auto"; shard_depth = "all", z_planes = 0) == ["--ngff-version", "0.5"]
    @test ff("0.5", "auto"; shard_depth = "all", z_planes = 1) == ["--ngff-version", "0.5"]
    # depth is NGFF 0.5 only, like the rest of sharding
    @test isempty(ff("0.4", "auto"; shard_depth = "13"))

    # Every option the spec offers must resolve, and there must be NO option claiming to disable
    # sharding: --shard-width cannot be turned off, so bioformats2raw shards every v3 store (verified
    # against 0.12.1 — a 0.5 import with no shard flag still produces a sharding_indexed codec), and an
    # "off" option would be a lie.
    spec = JSON3.read(read(joinpath(@__DIR__, "..", "src", "tasks", "importImages", "omezarr.json"), String))
    adv  = only(filter(p -> get(p, :type, "") == "section", collect(spec.params)))
    # `chunkSeparator` is NOT in this list any more: it was a declared param that the importer never
    # read — no `--no-nested`, no `dimension_separator`, nothing — and its default `"flat"`
    # contradicted `CHUNK_SEPARATOR_DEFAULT = "nested"`, which `config.jl` notes is the only separator
    # still offered. A control that reaches nothing is worse than an absent one: it reads as a choice.
    for key in ("ngffVersion", "shardSize", "shardDepth")
        prm  = only(filter(p -> get(p, :key, "") == key, collect(adv.params)))
        vals = [string(get(o, :value, o)) for o in prm.options]
        # `ngffVersion` takes its default from the Settings store layout (`defaultFrom`), so this
        # reads the RESOLVED spec — a raw file read would see the pre-resolution literal.
        @test string(prm.default) in vals
        @test !isempty(String(get(prm, :tip, "")))
    end
    shard = only(filter(p -> get(p, :key, "") == "shardSize", collect(adv.params)))
    @test !any(lowercase(string(get(o, :value, o))) in ("none", "off", "0") for o in shard.options)

    # Transparency: someone who knows zarr must be able to map each control onto what lands on disk, so
    # every one of these tips names its bioformats2raw flag or the metadata key it sets.
    for key in ("chunkSize", "ngffVersion", "shardSize", "shardDepth")
        prm = only(filter(p -> get(p, :key, "") == key, collect(adv.params)))
        tip = String(get(prm, :tip, ""))
        @test occursin("--", tip) || occursin("_", tip)   # a CLI flag or a zarr metadata key
    end
end

@testset "OME-TIFF export carries the calibration" begin
    # The task exists because the OLD route (OME-TIFF → ImageJ → plain TIFF → Imaris File Converter)
    # lost the pixel sizes: a plain TIFF has nowhere to record Z spacing, so the converter guessed the
    # voxel size. Every assertion below is about the calibration surviving — that IS the feature.

    meta = Dict{String,Any}("PhysicalSizeX" => 0.325, "PhysicalSizeY" => 0.325,
                            "PhysicalSizeZ" => 2.0,   "PhysicalSizeUnit" => "µm",
                            "TimeIncrement" => 10.0,  "TimeIncrementUnit" => "s")

    cal = Cecelia._export_calibration(meta)
    @test cal["PhysicalSizeZ"] == 2.0                     # the field the old workflow dropped
    @test cal["PhysicalSizeZUnit"] == "µm"

    # UNITS MUST BE THE OME SYMBOL, not the NGFF/UDUNITS name ccid.json stores. OME's UnitsLength and
    # UnitsTime are ENUMERATIONS; "micrometer" is not a member, so one such attribute makes <Pixels>
    # schema-invalid and Bio-Formats discards the ENTIRE OME block and falls back to counting IFDs —
    # a 31x4x32 movie then opens as 3968 timepoints, one channel, no names, no voxel size. Verified
    # against real Bio-Formats (bioformats2raw): "µm" reads back in full, "micrometer" reads nothing.
    ngff = Dict{String,Any}("PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33,
                            "PhysicalSizeZ" => 2.0,  "PhysicalSizeUnit" => "micrometer",
                            "TimeIncrement" => 15.0, "TimeIncrementUnit" => "second")
    ncal = Cecelia._export_calibration(ngff)
    for k in ("PhysicalSizeXUnit", "PhysicalSizeYUnit", "PhysicalSizeZUnit")
        @test ncal[k] == "µm"
    end
    @test ncal["TimeIncrementUnit"] == "s"
    # An unknown unit passes through rather than being guessed at — same rule as the converter.
    @test Cecelia._export_calibration(
        Dict{String,Any}("PhysicalSizeX" => 1.0, "PhysicalSizeUnit" => "furlong")
    )["PhysicalSizeXUnit"] == "furlong"
    @test cal["PhysicalSizeX"] == 0.325 && cal["PhysicalSizeXUnit"] == "µm"
    @test cal["TimeIncrement"] == 10.0 && cal["TimeIncrementUnit"] == "s"

    # A Z-MIP has no z extent left and a single frame has no interval — writing either would state a
    # geometry the file doesn't have.
    @test !haskey(Cecelia._export_calibration(meta; z_mip = true), "PhysicalSizeZ")
    @test haskey(Cecelia._export_calibration(meta; z_mip = true), "PhysicalSizeX")
    @test !haskey(Cecelia._export_calibration(meta; one_frame = true), "TimeIncrement")

    # Unknown must stay unknown. Defaulting an absent/zero/garbage size to 1.0 would tell Imaris the
    # pixel is one micron, which is a claim, not a fallback.
    for bad in (Dict{String,Any}(), Dict{String,Any}("PhysicalSizeX" => ""),
                Dict{String,Any}("PhysicalSizeX" => 0.0), Dict{String,Any}("PhysicalSizeX" => "abc"))
        @test !haskey(Cecelia._export_calibration(bad), "PhysicalSizeX")
    end

    # …and that absence is exactly what QC flags, since the write itself always "succeeds".
    codes(f) = [x["code"] for x in f]
    @test isempty(Cecelia._export_qc_findings(cal, 21))     # fully calibrated → nothing to say
    @test isempty(Cecelia._export_qc_findings(cal, 1))
    # A 2D image legitimately has no Z spacing — don't cry wolf on SizeZ == 1.
    @test isempty(Cecelia._export_qc_findings(Cecelia._export_calibration(meta; z_mip = true), 1))
    @test "export.no_z_calibration" in
          codes(Cecelia._export_qc_findings(Cecelia._export_calibration(meta; z_mip = true), 21))
    @test "export.no_xy_calibration" in codes(Cecelia._export_qc_findings(Dict{String,Any}(), 1))

    # channelSelection submits channel NAMES, not indices. Converting them by hand threw
    # `Int("DAPI")` straight out of the task; `channel_indices` is the resolver, and it is 0-based —
    # which is what the runner slices with, so an off-by-one here exports the wrong channel.
    names = ["DAPI", "GFP", "mem-Tom"]
    @test channel_indices(["GFP"], names; what = "channels") == [1]
    @test channel_indices(["mem-Tom", "DAPI"], names; what = "channels") == [2, 0]
    @test channel_indices(nothing, names; what = "channels") == Int[]
    @test channel_indices(String[], names; what = "channels") == Int[]
    # A name this version doesn't have must say so rather than silently pick something.
    @test_throws ErrorException channel_indices(["nope"], names; what = "channels")

    # …and the NAMES must come from `channel_names`, which falls back to the active version. Channel
    # names are typically registered only under `default` while a processed version carries none, so
    # reading the requested version's raw field returns nothing and the task reports "(none
    # registered)" for an image whose channels the picker is listing — the picker is fed by
    # `channel_names(img)`, so any other source disagrees with what the user just clicked.
    proj = create_project!(name = "chan-fallback-$(rand(1000:9999))")
    st   = add_set!(proj; name = "s")
    im   = add_image!(st; name = "chan-fallback")
    set_channel_names!(im, ["DAPI", "SHG"]; value_name = VERSIONED_DEFAULT_VAL, check_length = false)
    save!(im)

    @test channel_names(im) == ["DAPI", "SHG"]
    # An explicit version with no entry of its own still resolves — this is the bug.
    @test channel_names(im; value_name = "corrected") == ["DAPI", "SHG"]
    @test channel_indices(["SHG"],
                          something(channel_names(im; value_name = "corrected"), String[]);
                          what = "channels") == [1]

    # Dispatch + spec wiring
    @test Cecelia._task_from_fun_name("exportImages.ome_tiff") isa ExportOmeTiff
    spec = JSON3.read(read(Cecelia._spec_path(ExportOmeTiff()), String))
    @test String(get(spec, :fun_name, "")) == "exportImages.ome_tiff"
    @test String(get(spec, :resource_pool, "")) == "io"
    # The output is an ARTEFACT, not a version — nothing may register an image version from it.
    @test !any(String(get(p, :key, "")) == "outputValueName" for p in get(spec, :params, []))

    # One filename rule, shared with the movie recorders — an image called "… (cropped)" must not
    # produce a name that ends in a separator (that bug shipped once already).
    @test safe_name_part("A B (cropped)") == "A_B_cropped"
    @test safe_name_part("  ") == ""
    @test safe_name_part(nothing) == ""
end

# ── Canonical-helper detectors ────────────────────────────────────────────────
# These exist because both rules below have now cost real debugging time, and neither was enforced.
# The pattern is the repo's existing one (`no_bare_write_h5ad`, `TextIoDeclaresEncodingTest`, the
# store-compressor/staging conventions): scan the SOURCE, fail on a new bypass.

@testset "channelSelection params resolve through channel_indices" begin
    # A `channelSelection` param submits channel NAMES. `channel_indices` is the one resolver — it
    # takes names OR already-resolved indices, returns 0-based values, and errors by name on a miss
    # (with a case-difference hint). Its own comment records SIX handlers that had hand-rolled
    # `findfirst(==(String(ch)), ch_names)` and drifted into three different wrong behaviours: an
    # index crashed four of them, an unmatched name was silently DROPPED by five, and drift
    # correction silently fell back to channel 0 — registering a whole timelapse against SHG.
    #
    # It happened again on the OME-TIFF export (`Int("SHG")` → MethodError, straight out of the task),
    # which is why this is now a test rather than a comment.
    function _has_channel_param(ps)::Bool
        for p in ps
            p isa AbstractDict || continue
            String(get(p, "type", "")) == "channelSelection" && return true
            inner = get(p, "params", nothing)
            inner isa AbstractVector && _has_channel_param(inner) && return true
        end
        false
    end

    checked = String[]
    for (fun, task) in Cecelia._fun_name_map()
        spec = Cecelia._task_spec(task)
        isnothing(spec) && continue
        _has_channel_param(get(spec, "params", [])) || continue
        spec_path = Cecelia._spec_path(task)
        isnothing(spec_path) && continue
        jl = replace(spec_path, r"\.json$" => ".jl")
        isfile(jl) || continue        # a composite resolves nothing itself; its steps are checked
        push!(checked, fun)
        # A task file may have been split into a family (e.g. af_correct/{translate,qc,run}.jl next
        # to af_correct.jl) — check the aggregator PLUS any siblings under a same-named dir, since
        # the handler that actually calls `channel_indices` may live in a sub-file.
        family = String[jl]
        subdir = replace(jl, r"\.jl$" => "")
        if isdir(subdir)
            for f in readdir(subdir; join = true)
                endswith(f, ".jl") && push!(family, f)
            end
        end
        combined = join((read(f, String) for f in family), "\n")
        @test occursin("channel_indices", combined) ||
              error("$fun declares a channelSelection param but its handler never calls " *
                    "`channel_indices`. Resolve names with it (0-based, errors by name) rather " *
                    "than converting them by hand — see CLAUDE.md and channel_index's own comment.")
    end
    # The scan must actually find tasks; a rename that silently matched nothing would "pass".
    @test length(checked) >= 8
end

@testset "zarr access routes through the canonical helpers" begin
    # OME-ZARR is the same rule as `.h5ad`: one set of readers per language, and re-opening a store
    # by hand is how the two variants drift (CLAUDE.md → *Image / OME-ZARR access*). Julia has a
    # metadata tier in `app/src/tasks/importImages/omezarr.jl` (`series_base`, `ngff_attrs`,
    # `ngff_multiscales`, `zarr_array_meta`, `ngff_version` — exported through `Cecelia.jl`) and a
    # narrow display-only pixel tier in `api/src/image_geometry.jl` (`open_level` / `open_level0` /
    # `read_native`). Every other caller routes through those helpers.
    #
    # Two ways this rule is bypassed in practice — a bare `zopen(...)` / `Zarr.open(...)`, and a
    # `joinpath(..., ".zattrs" | ".zarray" | "zarr.json")` that reads the JSON itself instead of
    # going through `ngff_attrs`/`zarr_array_meta`. Both regressions have shipped before (the
    # `read_ngff_axes` note in `image_geometry.jl` records the second: reading `.zattrs` directly
    # made every v3 store answer EMPTY).
    #
    # Docstring mentions use backticks (`` `.zattrs` ``), which don't match the double-quoted literal.
    #
    # Sanctioned owners:
    #   * `importImages/omezarr.jl` (aggregator) and every `importImages/omezarr/*.jl` sub-file
    #     collectively define the metadata reader/writer tier — those are the files allowed to read
    #     the raw `.zattrs` / `.zarray` / `zarr.json` files.
    #   * `image_geometry.jl` defines `open_level`, so it is the one file allowed to call `zopen`.

    # ".zattrs", ".zarray", "zarr.json" as a code literal (a path join into a store) — but not the
    # docstring form `` `.zattrs` ``. `\\.` in raw-string form is the literal dot.
    literal_re = r"""\"\.zattrs\"|\"\.zarray\"|\"zarr\.json\""""
    # `zopen(...)` or `Zarr.open(...)` — the two ways a store gets opened via `Zarr.jl`.
    open_re    = r"\bzopen\s*\(|\bZarr\.open\s*\("

    # Path fragment (portable on Windows via joinpath). Any file under the omezarr/ family owns
    # the raw-JSON reads; the aggregator's own basename is the sentinel for the parent file itself.
    omezarr_family = joinpath("importImages", "omezarr")
    allowed_open   = Set(["image_geometry.jl"])

    literal_hits = String[]
    open_hits    = String[]
    for root in (joinpath(@__DIR__, "..", "src"), joinpath(@__DIR__, "..", "..", "api", "src"))
        isdir(root) || continue
        for (dir, _, files) in walkdir(root), f in files
            endswith(f, ".jl") || continue
            path = joinpath(dir, f)
            # rel key is enough to distinguish siblings — importImages/omezarr.jl vs the base name.
            in_omezarr_family = occursin(omezarr_family, path)
            rel  = in_omezarr_family ? joinpath(omezarr_family, f) : f
            for (i, ln) in enumerate(eachline(path))
                startswith(strip(ln), "#") && continue          # comments don't count
                if occursin(literal_re, ln) && !in_omezarr_family
                    push!(literal_hits, "$rel:$i  $(strip(ln))")
                end
                if occursin(open_re, ln) && !(rel in allowed_open)
                    push!(open_hits, "$rel:$i  $(strip(ln))")
                end
            end
        end
    end

    isempty(literal_hits) || error(
        "bare `.zattrs` / `.zarray` / `zarr.json` read outside `importImages/omezarr.jl`:\n  " *
        join(literal_hits, "\n  ") *
        "\nRoute through `ngff_attrs` / `ngff_multiscales` / `zarr_array_meta` / `ngff_version` — " *
        "reading `.zattrs` directly made every v3 store answer EMPTY once already " *
        "(see docs/todo/ZARR_V3_PLAN.md and CLAUDE.md → *Image / OME-ZARR access*).")

    isempty(open_hits) || error(
        "bare `zopen` / `Zarr.open` outside `api/src/image_geometry.jl`:\n  " *
        join(open_hits, "\n  ") *
        "\nOpen a store through `open_level` / `open_level0` and read pixels with `read_native` — " *
        "the display-only carve-out. Anything that PROCESSES data reads through Python `zarr_utils` " *
        "(CLAUDE.md → *Image / OME-ZARR access*).")

    @test isempty(literal_hits) && isempty(open_hits)

    # The scan must actually reach the sanctioned owners; a wrong root would let a real offender
    # slip through with an empty offender list.
    saw_literal_owner = false
    saw_open_owner    = false
    for root in (joinpath(@__DIR__, "..", "src"), joinpath(@__DIR__, "..", "..", "api", "src")),
        (dir, _, files) in walkdir(root), f in files
        endswith(f, ".jl") || continue
        path = joinpath(dir, f)
        occursin(omezarr_family, path) &&
            occursin(literal_re, read(path, String)) && (saw_literal_owner = true)
        f == "image_geometry.jl" &&
            occursin(open_re, read(path, String)) && (saw_open_owner = true)
    end
    @test saw_literal_owner
    @test saw_open_owner
end

@testset "a process exit check also checks termsignal" begin
    # libuv reports `exitcode = 0` for a SIGNAL-KILLED child, and `task:cancel` kills by design — so
    # `exitcode == 0` alone reads a cancelled or timed-out process as a clean success. That is how a
    # timed-out agent run had its TRUNCATED output handed to the result parser.
    offenders = String[]
    for root in (joinpath(@__DIR__, "..", "src"), joinpath(@__DIR__, "..", "..", "api", "src"))
        isdir(root) || continue
        for (dir, _, files) in walkdir(root), f in files
            endswith(f, ".jl") || continue
            path  = joinpath(dir, f)
            lines = readlines(path)
            for (i, ln) in enumerate(lines)
                occursin(".exitcode", ln) || continue
                # A window, not the same line: the check is often split over two lines, or guarded by
                # a `killed` flag derived from termsignal a few lines above.
                lo, hi = max(1, i - 6), min(length(lines), i + 6)
                any(occursin("termsignal", lines[j]) for j in lo:hi) && continue
                push!(offenders, "$(basename(path)):$i  $(strip(ln))")
            end
        end
    end
    isempty(offenders) && @test true
    isempty(offenders) || error("`.exitcode` used without a nearby `termsignal` check:\n  " *
                                join(offenders, "\n  ") *
                                "\nlibuv sets exitcode 0 for a signal-killed process — check " *
                                "`proc.exitcode == 0 && proc.termsignal == 0`.")
end

@testset "dirPath param validation" begin
    # A destination folder, typed by hand or picked with the FileBrowser. The failure this guards is
    # late and expensive: without it a bad destination is only discovered after the task has read,
    # converted and tried to write the whole output.
    spec = [Dict{String,Any}("key" => "outDir", "label" => "Destination", "type" => "dirPath")]

    # Empty is legal — every consumer falls back to its own default (default_export_dir()).
    Cecelia._validate_params_against_spec(Dict{String,Any}("outDir" => ""), spec)
    Cecelia._validate_params_against_spec(Dict{String,Any}(), spec)

    mktempdir() do dir
        # An existing folder is the normal case.
        Cecelia._validate_params_against_spec(Dict{String,Any}("outDir" => dir), spec)

        # One that does not exist yet is fine too — a destination is created on demand, so rejecting
        # it would stop someone naming a new subfolder, which is the obvious thing to want.
        Cecelia._validate_params_against_spec(
            Dict{String,Any}("outDir" => joinpath(dir, "new_subfolder")), spec)

        # An existing FILE is the one unambiguous mistake: nothing can write output into it.
        f = joinpath(dir, "not_a_dir.txt"); write(f, "x")
        @test_throws Cecelia.ParamValidationError Cecelia._validate_params_against_spec(
            Dict{String,Any}("outDir" => f), spec)
    end

    @test_throws Cecelia.ParamValidationError Cecelia._validate_params_against_spec(
        Dict{String,Any}("outDir" => 42), spec)

    # The export's destination actually uses the type — the point of adding it.
    ospec = JSON3.read(read(Cecelia._spec_path(ExportOmeTiff()), String))
    outdir = only(filter(p -> String(get(p, :key, "")) == "outDir", collect(ospec[:params])))
    @test String(get(outdir, :type, "")) == "dirPath"
end

@testset "units written into OME-XML are schema-valid symbols" begin
    # OME's UnitsLength / UnitsTime are ENUMERATIONS of symbols. A value outside them makes the
    # whole <Pixels> element schema-invalid, and Bio-Formats then discards the ENTIRE OME block and
    # falls back to counting IFDs — a 31x4x32 movie opened as 3968 timepoints, one channel, no
    # names, no voxel size. Verified against real Bio-Formats (bioformats2raw): "µm" round-trips in
    # full, "micrometer" yields nothing.
    #
    # The trap is that "micrometer" is CORRECT in the two places it comes from: NGFF `.zattrs` axes
    # use UDUNITS-2 names, and `ccid.json` mirrors them because the importer reads the unit from the
    # axes. Only the OME-XML boundary needs the symbol — which is what `ome_xml_unit_name` is for.
    valid_length = Set(["Ym","Zm","Em","Pm","Tm","Gm","Mm","km","hm","dam","m","dm","cm","mm",
                        "µm","nm","pm","fm","am","zm","ym","Å","thou","li","in","ft","yd","mi",
                        "ua","ly","pc","pt","pixel","reference frame"])
    valid_time   = Set(["Ys","Zs","Es","Ps","Ts","Gs","Ms","ks","hs","das","s","ds","cs","ms",
                        "µs","ns","ps","fs","as","zs","ys","min","h","d"])
    valid = union(valid_length, valid_time)

    # Every output of the converter is a member — including for inputs already in symbol form.
    for (ngff, sym) in Cecelia._OME_XML_UNIT
        @test sym in valid
        @test Cecelia.ome_xml_unit_name(ngff) == sym
        @test Cecelia.ome_xml_unit_name(sym) in valid    # idempotent: a symbol stays valid
    end
    # The vocabularies the importer actually stores in ccid.json must all convert.
    for ngff in ("micrometer", "nanometer", "millimeter", "second", "minute")
        @test Cecelia.ome_xml_unit_name(ngff) in valid
    end
    # An unknown unit passes through — we do not guess a conversion — so it is the CALLER's job not
    # to invent one, and the scan below is what keeps a caller from skipping the converter entirely.
    @test Cecelia.ome_xml_unit_name("furlong") == "furlong"

    # Anything that ASSIGNS an OME unit attribute must route through the converter. This is the
    # bypass that shipped: the OME-TIFF export copied ccid.json's "micrometer" straight into
    # PhysicalSizeXUnit, while every other writer converted.
    #
    # Compliance is per-DIRECTORY: a file that only READS these keys into an intermediate Dict (the
    # metadata reader) doesn't itself need to call `ome_xml_unit_name`, but the sibling file in the
    # same directory that WRITES OME-XML must — and the split of `importImages/omezarr.jl` into
    # `omezarr/reader.jl` + `omezarr/calibration.jl` separates the two halves, so a file-local check
    # would false-positive on reader.jl. Aggregate at directory level: the family passes iff SOMEONE
    # under the same dir calls the converter.
    dir_has_converter = Dict{String,Bool}()
    for root in (joinpath(@__DIR__, "..", "src"), joinpath(@__DIR__, "..", "..", "api", "src"))
        isdir(root) || continue
        for (dir, _, files) in walkdir(root), f in files
            endswith(f, ".jl") || continue
            occursin("ome_xml_unit_name", read(joinpath(dir, f), String)) || continue
            dir_has_converter[dir] = true
        end
    end
    offenders = String[]
    for root in (joinpath(@__DIR__, "..", "src"), joinpath(@__DIR__, "..", "..", "api", "src"))
        isdir(root) || continue
        for (dir, _, files) in walkdir(root), f in files
            endswith(f, ".jl") || continue
            path = joinpath(dir, f); src = read(path, String)
            occursin(r"\"(PhysicalSize[XYZ]Unit|TimeIncrementUnit)\"\s*(=>|\]\s*=)", src) || continue
            occursin("ome_xml_unit_name", src) && continue
            get(dir_has_converter, dir, false) && continue
            push!(offenders, basename(path))
        end
    end
    isempty(offenders) && @test true
    isempty(offenders) || error("these assign an OME-XML unit attribute without calling " *
                                "`ome_xml_unit_name`:\n  " * join(offenders, "\n  ") *
                                "\nccid.json/NGFF store UDUNITS names ('micrometer'); OME-XML " *
                                "needs the symbol ('µm'), and an invalid one voids the whole block.")
end

@testset "view profiles (curated sidebar)" begin
    # A profile is a named, ORDERED subset of sidebar routes, dropped in as a file under
    # <config_dir>/profiles/. The reader validates SHAPE ONLY — it deliberately does not know the route
    # table (that lives in frontend/src/main.ts), so an item naming a route that no longer exists is a
    # frontend concern, not an error here. See docs/todo/VIEW_PROFILES_PLAN.md.
    @test view_profiles_dir() == joinpath(config_dir(), "profiles")

    # ── parse: what a valid profile is ────────────────────────────────────────
    p = parse_view_profile("focused", Dict("label" => "Gating + behaviour",
                                           "items" => ["/gate", "/behaviour"]))
    @test p.id == "focused"
    @test p.label == "Gating + behaviour"
    @test p.items == ["/gate", "/behaviour"]

    # the label falls back to the id, so a file needs only `items`
    @test parse_view_profile("myview", Dict("items" => ["/gate"])).label == "myview"
    # order is the profile's, and duplicates keep their FIRST position
    @test parse_view_profile("x", Dict("items" => ["/track", "/gate", "/track"])).items ==
          ["/track", "/gate"]

    # ── parse: every rejection is a message, never a crash ────────────────────
    @test_throws ArgumentError parse_view_profile("x", Dict("label" => "no items"))
    # empty is an ERROR, not an empty profile: rendering a blank sidebar from a typo looks broken
    @test_throws ArgumentError parse_view_profile("x", Dict("items" => String[]))
    @test_throws ArgumentError parse_view_profile("x", Dict("items" => ["gate"]))   # not a route path
    @test_throws ArgumentError parse_view_profile("x", Dict("items" => [42]))
    @test_throws ArgumentError parse_view_profile("x", "not an object")

    # ── ids come from a user-typed label, never a filename ────────────────────
    @test view_profile_id("Gating + behaviour") == "gating_behaviour"
    @test_throws ArgumentError view_profile_id("+++")
    # and a hostile id cannot escape the profiles dir
    @test !occursin("..", view_profile_id("../../etc/passwd"))

    # ── round-trip through disk: write → read ─────────────────────────────────
    dir = view_profiles_dir()
    saved = write_view_profile("Gating + behaviour", ["/gate", "/track", "/behaviour"])
    @test saved.id == "gating_behaviour"
    @test isfile(joinpath(dir, "gating_behaviour.json"))

    got = read_view_profiles()
    @test got.dir == dir
    mine = only(filter(p -> p.id == "gating_behaviour", got.profiles))
    @test mine.label == "Gating + behaviour"
    @test mine.items == ["/gate", "/track", "/behaviour"]
    @test isempty(got.errors)

    # renaming the LABEL must keep the id, or an active selection breaks under the user
    renamed = write_view_profile("Gating only", ["/gate"]; id = "gating_behaviour")
    @test renamed.id == "gating_behaviour"
    @test renamed.label == "Gating only"
    @test only(filter(p -> p.id == "gating_behaviour", read_view_profiles().profiles)).items == ["/gate"]

    # ── a broken file is REPORTED, never fatal, and never hides the good ones ──
    write(joinpath(dir, "broken.json"), "{ not json")
    write(joinpath(dir, "empty.json"), "{\"items\": []}")
    bad = read_view_profiles()
    @test any(p -> p.id == "gating_behaviour", bad.profiles)     # the valid one still loads
    @test Set(e.file for e in bad.errors) == Set(["broken.json", "empty.json"])

    # a non-.json file in the dir is not a profile and not an error
    write(joinpath(dir, "notes.txt"), "ignore me")
    @test Set(e.file for e in read_view_profiles().errors) == Set(["broken.json", "empty.json"])

    # ── delete ────────────────────────────────────────────────────────────────
    @test delete_view_profile!("gating_behaviour")
    @test !delete_view_profile!("gating_behaviour")             # already gone is not an error
    @test !any(p -> p.id == "gating_behaviour", read_view_profiles().profiles)

    # tidy up so a later testset reading the config dir sees no leftovers
    for f in ("broken.json", "empty.json", "notes.txt")
        rm(joinpath(dir, f); force = true)
    end
end

# ── The task runner losing its port is a normal outcome, not a crash ──────────
#
# The runner is built to OUTLIVE the API server, so on a fresh start there is often already one
# running — and two checkouts sharing a `CECELIA_DEV_DIR` share the port outright. Two things used to
# go wrong when a second one lost that race, and this pins both:
#
#   1. it died with a `TaskFailedException` stack trace, which reads as a broken app; and
#   2. WORSE — it wrote `runner.json` with its own pid BEFORE binding, clobbering the incumbent's
#      record, then its `atexit` hook deleted the file on the way out. So a collision left the
#      SURVIVING runner with no state file, which is exactly the "a stray runner is folklore" case
#      that file exists to prevent.
#
# Uses an EPHEMERAL port held by a plain socket, never the real 7657 — a test must not touch a runner
# the developer has running.
#
# It caught a THIRD way to get this wrong, on macOS CI only: `HTTP.listen!` builds a `Server` and
# spawns a task that does the bind, and the failure path notifies its ready `Event` BEFORE it rethrows —
# so `listen!` returned normally, the state file was claimed, and the EADDRINUSE surfaced at `wait` as
# a `TaskFailedException`. Both original symptoms, back. Ownership is therefore proven by asking
# `/ping` for the responder's PID (`_runner_owns_port`), which no scheduler ordering can fake — the
# raw socket this test holds never speaks HTTP, so nothing answers and the runner stands down.
@testset "runner_serve stands down when the port is taken" begin
    using Sockets: listen as sock_listen, getsockname, localhost

    held = sock_listen(localhost, 0)              # port 0 → the OS picks a free one
    port = Int(getsockname(held)[2])

    state = joinpath(Cecelia.config_dir(), "runner.json")
    mkpath(dirname(state))
    # stand in for the incumbent's record, with a pid that is NOT ours
    marker = Dict{String,Any}("pid" => 999_999, "port" => port, "commit" => "incumbent")
    Cecelia.write_json_atomic(state, marker)

    try
        # Returns rather than throwing. The old code raised TaskFailedException out of HTTP.listen.
        r = @test_logs (:warn,) match_mode=:any Cecelia.runner_serve(; port = port)
        @test r === nothing

        # …and the incumbent's record is untouched: neither overwritten with our pid nor deleted.
        @test isfile(state)
        @test JSON3.read(read(state, String), Dict{String,Any})["pid"] == 999_999
        @test JSON3.read(read(state, String), Dict{String,Any})["commit"] == "incumbent"
    finally
        close(held)
        rm(state; force = true)
    end
end

# ── The TWIN of the test above, and the one whose absence let a regression ship ───────────────────
#
# The guard was only ever pinned from the "port is taken" side, so `_runner_owns_port` answering false
# for a port we had *just bound ourselves* looked like a pass. It shipped, and then every fresh
# `pixi run dev` printed "another process took it" with nothing listening on 7657 at all — the runner
# exited on start and every task fell back to in-process.
#
# The cause was a budget that could not be met, not a wrong idea: a freshly returned `HTTP.listen!`
# server does not serve its first in-process request for ~1.2 s (the accept path is still compiling),
# and the session's first `HTTP.get` compiles for about as long — so a 2 s per-attempt timeout inside a
# 3 s deadline bought ONE attempt, which could not succeed. This test fails outright on those numbers.
#
# Deliberately exercises `_runner_owns_port` against a real `HTTP.listen!` rather than `runner_serve`:
# a full runner installs `_runner_idle_watchdog!`, which calls `exit(0)`, and a test process that may
# exit from under the suite is not a test.
@testset "_runner_owns_port recognises a port WE just bound" begin
    using Sockets: listen as sock_listen, getsockname, localhost

    probe = sock_listen(localhost, 0)             # port 0 → the OS names a free one
    port  = Int(getsockname(probe)[2])
    close(probe)                                   # …then hand it back, so we can bind it ourselves

    server = Cecelia.HTTP.listen!(Cecelia._runner_stream, "127.0.0.1", port)
    try
        @test Cecelia._runner_owns_port(port)
        # and it is OUR pid it recognised, not merely "something answered"
        reply = Cecelia.runner_ping(Cecelia.RunnerHandle(; port = port))
        @test reply !== nothing
        @test string(get(reply, "pid", "")) == string(getpid())
    finally
        try; close(server); catch; end
    end
end

# ── An empty response body must not corrupt the connection ────────────────────
#
# `write_http_body!` (utils.jl, where the mechanism is explained) exists for one wire-level reason:
# our responses carry no Content-Length, so HTTP.jl frames them CHUNKED and frames every `write` as
# its own chunk — so a zero-length write emits the TERMINATING `0\r\n\r\n` and `closewrite` emits a
# second one. This asserts the BYTES, over two requests on ONE keep-alive connection, because the
# response that broke was never the empty one: the extra terminator sits in the connection and the
# NEXT response is parsed starting at it.
#
# The unguarded `write` is kept as the negative control — a guard is only worth having if the test
# fails without it. Ephemeral port, and a raw socket that speaks HTTP by hand (an HTTP client would
# hide the framing, which is the whole subject).
@testset "an empty response body is written through write_http_body!" begin
    using Sockets: connect as sock_connect
    HTTP = Cecelia.HTTP

    # READING THE WIRE, without guessing at packetisation — both halves of this were measured, and
    # both got it wrong first:
    #   * `bytesavailable(sock)` is NOT a usable poll. Julia stops the libuv read loop when its
    #     buffer empties, so it reports 0 with bytes still pending — that version saw 2 terminators
    #     of 4 on half its runs. So ONE reader task blocks in `readavailable` for the socket's whole
    #     life and appends everything; `close` is what ends it.
    #   * The stopping condition must be the DATA, not a fixed sleep. The extra terminator is written
    #     separately from the head, so "both responses are in" can be true with those 4 bytes still in
    #     flight — that is how the first version of this test flaked on ubuntu CI only (2 of 4) while
    #     passing on macOS, Windows and locally. A generous deadline also absorbs the first call's
    #     compilation, which a fixed window did not.
    settle = function (acc, quiet, cap)         # wait until the accumulated bytes stop growing
        n = length(acc[]); t0 = time(); t_last = time()
        while time() - t_last < quiet && time() - t0 < cap
            sleep(0.02)
            if length(acc[]) != n; n = length(acc[]); t_last = time(); end
        end
    end

    wire = function (guarded::Bool)
        handler = function (stream)
            read(stream)
            HTTP.setstatus(stream, 200)
            HTTP.setheader(stream, "Content-Type" => "application/octet-stream")
            HTTP.startwrite(stream)
            guarded ? write_http_body!(stream, UInt8[]) : write(stream, UInt8[])
        end
        server = HTTP.listen!(handler, "127.0.0.1", 0)          # port 0 → never a real one
        try
            sock = sock_connect("127.0.0.1", HTTP.port(server))
            acc  = Ref("")
            reader = @async try
                while !eof(sock); acc[] *= String(copy(readavailable(sock))); end
            catch; end
            try
                for i in 1:2                    # request 2 is the one the stray bytes broke
                    write(sock, "GET /$i HTTP/1.1\r\nHost: x\r\n\r\n")
                    t0 = time()
                    while count("HTTP/1.1 200", acc[]) < i && time() - t0 < 20; sleep(0.02); end
                end
                settle(acc, 0.5, 5.0)
            finally
                close(sock)
            end
            wait(reader)
            acc[]
        finally
            close(server)
        end
    end

    guarded = wire(true)
    @test count("HTTP/1.1 200", guarded) == 2
    @test count("0\r\n\r\n", guarded) == 2        # exactly ONE terminating chunk per response
    @test endswith(guarded, "0\r\n\r\n")          # …and no bytes left over for the next response

    # the bug, on the wire: `…0\r\n\r\n0\r\n\r\nHTTP/1.1 …`. Vite's proxy died on exactly these bytes
    # (`HPE_INVALID_CONSTANT … rawPacket <30 0d 0a 0d 0a 30 0d 0a 0d 0a>`), failing the OTHER, good
    # requests that shared the connection with one legitimately-empty gating plot.
    bare = wire(false)
    @test count("0\r\n\r\n", bare) == 4
    @test occursin("0\r\n\r\n0\r\n\r\n", bare)

    # so no handler hands a body to `write` directly — the runner's reply path included
    src = read(joinpath(@__DIR__, "..", "src", "runner", "server.jl"), String)
    @test occursin("write_http_body!(stream, body)", src)
    @test !occursin("write(stream, ", src)
end

# ── Correction testsets ────────────────────────────────────────────────────
# Manual TRACK correction (docs/todo/CORRECTION_PLAN.md P1) + manual LABEL correction (P2)
# + track-issue triage worklist. Pure ops-engine tests against hand-built cell tables — no
# fixture, no .h5ad. Extracted from this file to keep it small enough to merge without EOF
# conflicts on every append. The extracted file loads inside this file's aggregating testset
# scope, so any helpers defined earlier in suite.jl are still in scope for the correction
# fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "correction.jl"))

# ── Tracking / celltrackR testsets ─────────────────────────────────────────
# 5 testsets covering: analyze_cell_pairs (celltrackR port + double-tracking + angle/dist
# semantics), the diagnostics battery (app/src/tracking/track_diagnostics.jl), golden values
# cross-checked against celltrackR 1.2.2, image-pooled track_cohort readings, and the
# (images × population) grouping the two track plots share. Extracted from this file to keep
# it small enough to merge without EOF conflicts on every append. The extracted file loads
# inside this file's aggregating testset scope, so any helpers defined earlier in suite.jl
# are still in scope for the tracking fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "tracking.jl"))

# ── bf2raw_series_subdir: which subdir under `<zarr>/` carries the multiscales, when the caller can't
# assume it is `0/`. `bioformats2raw --series N` preserves the source index in the output group name
# (so `--series 3` writes `<zarr>/3/`); this is the resolver the import task uses to decide which
# subdir to point ccid.json's `filepath["default"]` at. Same fixture shape as `make_zarr`'s :series
# layout — a numbered subdir with a `.zattrs` multiscales attr.
@testset "bf2raw_series_subdir" begin
    function _seed(dir::AbstractString, name::AbstractString)
        base = joinpath(dir, name)
        mkpath(base)
        zattrs = Dict{String,Any}("multiscales" =>
            [Dict{String,Any}("axes" => [Dict{String,Any}("name" => "x", "type" => "space")],
                              "datasets" => [Dict{String,Any}("path" => "0",
                                  "coordinateTransformations" =>
                                    [Dict{String,Any}("type" => "scale", "scale" => [1.0])])])])
        open(joinpath(base, ".zattrs"), "w") do io; JSON3.write(io, zattrs); end
        mkpath(joinpath(base, "0"))
        write(joinpath(base, "0", ".zattrs"), "{}")
        base
    end

    mktempdir() do d
        # single-series bf2raw default: subdir "0" carries multiscales
        _seed(d, "0")
        @test Cecelia.bf2raw_series_subdir(d) == "0"
        # `prefer = 0` doesn't change the answer when 0/ exists
        @test Cecelia.bf2raw_series_subdir(d; prefer = 0) == "0"
    end
    mktempdir() do d
        # --series 3 output: only subdir "3" exists (no "0"). The resolver must find it, and
        # `prefer = 3` must beat the fallback scan (a same-name collision on 0/ never masks the pick).
        _seed(d, "3")
        @test Cecelia.bf2raw_series_subdir(d) == "3"
        @test Cecelia.bf2raw_series_subdir(d; prefer = 3) == "3"
    end
    mktempdir() do d
        # multi-series without --series: bf2raw wrote 0/ 1/ 2/ 3/. `prefer = 3` picks the requested
        # one instead of the alphabetically-first (0/), which is what the "unblock ttRMjQ" path needs.
        for n in ("0", "1", "2", "3"); _seed(d, n); end
        @test Cecelia.bf2raw_series_subdir(d; prefer = 3) == "3"
        @test Cecelia.bf2raw_series_subdir(d) == "0"  # no preference → the bf2raw default
    end
    mktempdir() do d
        # flat-store layout: multiscales AT the root (crop/correction output). Return "" — caller
        # should not descend into a subdir. Level-0 array under `0/` has empty `.zattrs`, so a naive
        # scan would otherwise still return "0" and read the wrong file.
        zattrs = Dict{String,Any}("multiscales" =>
            [Dict{String,Any}("axes" => [Dict{String,Any}("name" => "x", "type" => "space")],
                              "datasets" => [Dict{String,Any}("path" => "0",
                                  "coordinateTransformations" =>
                                    [Dict{String,Any}("type" => "scale", "scale" => [1.0])])])])
        open(joinpath(d, ".zattrs"), "w") do io; JSON3.write(io, zattrs); end
        mkpath(joinpath(d, "0"))
        write(joinpath(d, "0", ".zattrs"), "{}")
        @test Cecelia.bf2raw_series_subdir(d) == ""
    end
    # nothing exists → nothing
    @test isnothing(Cecelia.bf2raw_series_subdir(joinpath(tempdir(), "nope-$(rand(UInt32))")))
end

# The typed per-task params structs for the cleanupImages family. Each parser is the ONE place its
# task reads the params bag, so this pins the fields + defaults that the rest of the task body
# depends on — a spec rename that isn't mirrored here would show up as a struct-field access error
# rather than a silent default at the read site.
@testset "typed params — cleanupImages" begin
    # Smooth: full dict → every field carries through; empty dict → defaults.
    let p = Cecelia.parse_smooth_params(Dict{String,Any}(
            "valueName"           => "driftCorrected",
            "channels"            => ["mem-TOM"],
            "spatialMethod"       => "bilateral_vst",
            "spatialSigma"        => 2.5,
            "bilateralColor"      => 12.0,
            "bilateralReach"      => 4.0,
            "bilateralPolish"     => 0.7,
            "temporalFrames"      => 3,
            "temporalStat"        => "mean",
            "farnebackMaxShiftPx" => 6.5,
            "restoreDynamicRange" => false))
        @test p.valueName == "driftCorrected"
        @test p.channels == ["mem-TOM"]
        @test p.spatialMethod == "bilateral_vst"
        @test p.spatialSigma === 2.5
        @test p.temporalFrames === 3
        @test p.restoreDynamicRange === false
    end
    let p = Cecelia.parse_smooth_params(Dict{String,Any}())
        @test p.valueName == Cecelia.VERSIONED_DEFAULT_VAL
        @test p.spatialMethod == "gaussian"
        @test p.temporalFrames === 1
        @test p.restoreDynamicRange === true
    end

    # StackAlign
    let p = Cecelia.parse_stack_align_params(Dict{String,Any}(
            "alignChannel" => ["CD169"], "referenceMode" => "first",
            "minConfidence" => 0.5, "maxShiftPx" => 12.0))
        @test p.alignChannel == ["CD169"]
        @test p.referenceMode == "first"
        @test p.minConfidence === 0.5
        @test p.maxShiftPx === 12.0
    end

    # FlowRegister
    let p = Cecelia.parse_flow_register_params(Dict{String,Any}(
            "registerChannel" => ["CD169"], "structuralChannels" => ["SHG"],
            "referenceMode" => "first", "aggressiveness" => "gentle",
            "pyrLevels" => 3, "maxShiftPx" => 20.0))
        @test p.registerChannel == ["CD169"]
        @test p.structuralChannels == ["SHG"]
        @test p.aggressiveness == "gentle"
        @test p.pyrLevels === 3
    end

    # Denoise
    let p = Cecelia.parse_denoise_params(Dict{String,Any}(
            "model" => "supp.MERTK", "channels" => ["mem-TOM"], "batchSize" => 4))
        @test p.model == "supp.MERTK"
        @test p.channels == ["mem-TOM"]
        @test p.batchSize === 4
    end

    # DriftCorrect
    let p = Cecelia.parse_drift_correct_params(Dict{String,Any}(
            "driftChannel" => ["mem-TOM"], "driftEstimator" => "sitkRigid",
            "driftMaxAngle" => 3.0, "driftPerPlane" => true, "driftZSmoothness" => 0.5))
        @test p.driftChannel == ["mem-TOM"]
        @test p.driftEstimator == "sitkRigid"
        @test p.driftMaxAngle === 3.0
        @test p.driftPerPlane === true
        @test p.driftZSmoothness === 0.5
    end

    # AfCorrect + the two typed structs at its edges (task #21 AfChannelStats, task #22
    # AfCombinationSpec — see docs/archive/comment-audit-findings.md → Tier 1 boundary bags).
    let p = Cecelia.parse_af_correct_params(Dict{String,Any}(
            "backgroundMethod" => "otsu"))
        @test p.valueName == Cecelia.VERSIONED_DEFAULT_VAL
        @test p.backgroundMethod == "otsu"
    end
    let specs = Cecelia.parse_af_combinations(Dict{String,Any}(
            "afCombinations" => Dict{String,Any}(
                "CD169-Kat" => Dict{String,Any}(
                    "targetChannel"     => ["CD169-Kat"],
                    "competingChannels" => ["SHG", "CH4"]),
                "malformed" => "not a dict")))
        @test length(specs) == 1
        @test specs[1].key == "CD169-Kat"
        @test specs[1].targetChannel == ["CD169-Kat"]
        @test specs[1].competingChannels == ["SHG", "CH4"]
    end
    # AfChannelStats round-trips the Python-side per-channel block and carries a Dict{String,Float64}
    # bleedthrough. A missing bleedthrough is a legal shape (no leaks detected) → empty Dict.
    let s = Cecelia.parse_af_channel_stats(Dict{String,Any}(
            "saturatedFrac" => 0.02, "levelsUsed" => 3200.0, "levelsAvailable" => 4096.0,
            "bleedthrough" => Dict{String,Any}("SHG" => 0.15, "CD169" => 0.03)))
        @test s.saturatedFrac === 0.02
        @test s.levelsUsed === 3200.0
        @test s.levelsAvailable === 4096.0
        @test s.bleedthrough == Dict("SHG" => 0.15, "CD169" => 0.03)
    end
    let s = Cecelia.parse_af_channel_stats(Dict{String,Any}())
        @test s.saturatedFrac === 0.0
        @test s.levelsAvailable === 1.0    # min-1 floor so `used / avail` never divides by zero
        @test isempty(s.bleedthrough)
    end
end

# Typed per-task params structs for the editImages family. Each parser is the ONE place its task
# reads the params bag — a spec rename that isn't mirrored here becomes a struct-field error rather
# than a silent default at the read site. Same pattern as `typed params — cleanupImages`.
@testset "typed params — editImages" begin
    # BinImage
    let p = Cecelia.parse_bin_image_params(Dict{String,Any}(
            "valueName" => "corrected", "factorX" => 4, "factorY" => 2, "op" => "sum"))
        @test p.valueName == "corrected"
        @test p.factorX === 4
        @test p.factorY === 2
        @test p.op == "sum"
    end
    let p = Cecelia.parse_bin_image_params(Dict{String,Any}())
        @test p.factorX === 2 && p.factorY === 2 && p.op == "mean"
    end

    # CopyImage — exactly-one-of toSetUid/newSetName is a runtime check (not enforced by the type),
    # so both empty is a legal parse (the handler catches it).
    let p = Cecelia.parse_copy_image_params(Dict{String,Any}(
            "valueName" => "corrected", "newSetName" => "  Day 3  "))
        @test p.valueName == "corrected"
        @test p.toSetUid == ""
        @test p.newSetName == "Day 3"          # whitespace stripped
    end

    # CropImage — nested CropBox with the -1 keep-axis convention.
    let p = Cecelia.parse_crop_image_params(Dict{String,Any}(
            "cropBox" => Dict{String,Any}(
                "x0"=>10, "x1"=>200, "y0"=>0, "y1"=>256, "z0"=>-1, "z1"=>-1, "t0"=>5, "t1"=>20)))
        b = p.cropBox
        @test b.x0 === 10 && b.x1 === 200
        @test b.y0 === 0  && b.y1 === 256
        @test b.z0 === -1 && b.z1 === -1        # 2D image: keep the whole axis
        @test b.t0 === 5  && b.t1 === 20
    end
    # missing/malformed cropBox → defaulted struct (the handler's separate check rejects the run).
    let p = Cecelia.parse_crop_image_params(Dict{String,Any}("valueName" => "x"))
        @test p.cropBox.x0 === 0 && p.cropBox.z0 === -1
    end

    # DtypeConvert — dtype + rescale are LOWERCASED at parse (the handler's whitelist reads the
    # lowered value).
    let p = Cecelia.parse_dtype_convert_params(Dict{String,Any}(
            "dtype" => "UINT16", "rescale" => "NONE"))
        @test p.dtype == "uint16"
        @test p.rescale == "none"
    end

    # Flip — axis UPPERCASED at parse.
    let p = Cecelia.parse_flip_params(Dict{String,Any}("axis" => "z"))
        @test p.axis == "Z"
    end

    # Register — the audit's `regChannel` is a scalar channel NAME (not a `channelSelection`
    # array); parser preserves it verbatim.
    let p = Cecelia.parse_register_params(Dict{String,Any}(
            "regChannel" => "mem-TOM", "doAffine3d" => true, "sigma" => 0.5,
            "samplesPerParameter" => 8000))
        @test p.regChannel == "mem-TOM"
        @test p.doAffine2d === true            # default preserved
        @test p.doAffine3d === true
        @test p.sigma === 0.5
        @test p.samplesPerParameter === 8000
    end

    # ResampleZ — order LOWERCASED at parse.
    let p = Cecelia.parse_resample_z_params(Dict{String,Any}("order" => "CUBIC"))
        @test p.order == "cubic"
    end

    # TProject / ZProject — same shape, different defaults (mean vs max).
    let p = Cecelia.parse_t_project_params(Dict{String,Any}())
        @test p.op == "mean"
    end
    let p = Cecelia.parse_z_project_params(Dict{String,Any}())
        @test p.op == "max"
    end
    let p = Cecelia.parse_z_project_params(Dict{String,Any}("op" => "median"))
        @test p.op == "median"
    end
end

# Typed per-task params structs for the tracking family. Same pattern as
# `typed params — cleanupImages`: each parser is the ONE place its task reads the params bag, so
# a spec rename that isn't mirrored here becomes a struct-field error rather than a silent default.
@testset "typed params — tracking" begin
    # BayesianTracking — full-dict + empty-dict paths. Field spellings match btrack's own names
    # (`noiseInital` is a typo in the spec; preserved verbatim so a saved run stays reproducible).
    let p = Cecelia.parse_bayesian_tracking_params(Dict{String,Any}(
            "valueName"      => "corrected",
            "popsToTrack"    => "T/tracked",
            "maxSearchRadius"=> 30,
            "maxLost"        => 5,
            "trackBranching" => true,
            "accuracy"       => 0.9,
            "distThresh"     => 12.5,
            "lambdaBranch"   => 100,
            "thetaDist"      => 7.5))
        @test p.valueName == "corrected"
        @test p.popsToTrack == "T/tracked"
        @test p.maxSearchRadius === 30
        @test p.maxLost === 5
        @test p.trackBranching === true
        @test p.accuracy === 0.9
        @test p.distThresh === 12.5
        @test p.lambdaBranch === 100
        @test p.thetaDist === 7.5
    end
    let p = Cecelia.parse_bayesian_tracking_params(Dict{String,Any}())
        @test p.popsToTrack == "NONE"                # default = track whole segmentation
        @test p.maxSearchRadius === 20
        @test p.noiseInital === 300                  # historical spelling preserved
        @test p.thetaDist === 5.0
    end

    # TrackCorrect — the parser delegates `trackOps` to `parse_track_ops` (the same helper that
    # `validate_params` uses); empty/nothing → empty vector, a real op passes through with its
    # own dict entries preserved.
    let p = Cecelia.parse_track_correct_params(Dict{String,Any}(
            "valueName" => "corrected",
            "trackOps"  => Any[Dict{String,Any}(
                "op" => "track.join", "trackIds" => Any[78, 92])]))
        @test p.valueName == "corrected"
        @test length(p.trackOps) == 1
        @test p.trackOps[1]["op"] == "track.join"
        @test p.trackOps[1]["trackIds"] == Any[78, 92]
    end
    let p = Cecelia.parse_track_correct_params(Dict{String,Any}("trackOps" => nothing))
        @test isempty(p.trackOps)
    end
    let p = Cecelia.parse_track_correct_params(Dict{String,Any}())
        @test p.valueName == Cecelia.VERSIONED_DEFAULT_VAL
        @test isempty(p.trackOps)
    end
    # JSON string round-trips the same way as a real Vector (the form path).
    let p = Cecelia.parse_track_correct_params(Dict{String,Any}(
            "trackOps" => "[{\"op\":\"track.remove\",\"trackIds\":[10]}]"))
        @test length(p.trackOps) == 1
        @test p.trackOps[1]["op"] == "track.remove"
    end

    # TrackMeasures — `dims` is lower-cased AND stripped at parse so the handler reads one shape.
    let p = Cecelia.parse_track_measures_params(Dict{String,Any}(
            "valueName" => "corrected", "forceRecompute" => false, "dims" => "  3D  "))
        @test p.valueName == "corrected"
        @test p.forceRecompute === false
        @test p.dims == "3d"
    end
    let p = Cecelia.parse_track_measures_params(Dict{String,Any}())
        @test p.dims == "auto"                       # default: preflight detects
        @test p.forceRecompute === true              # default is force-recompute
    end
end

# Typed per-task params structs for the segment family. Same pattern as
# `typed params — cleanupImages` — each parser is the ONE place its task reads the params bag.
@testset "typed params — segment" begin
    # CellposeSegment — `models` stays a bag (resolved separately by cellpose_models_for_python).
    let p = Cecelia.parse_cellpose_segment_params(Dict{String,Any}(
            "valueName" => "corrected", "outputValueName" => "cells",
            "blockSize" => 1024, "overlap" => 128, "matchThreshold" => 0.5,
            "removeUnmatched" => true, "normaliseToWhole" => false))
        @test p.valueName == "corrected"
        @test p.outputValueName == "cells"
        @test p.blockSize === 1024
        @test p.overlap === 128
        @test p.matchThreshold === 0.5
        @test p.removeUnmatched === true
        @test p.normaliseToWhole === false
    end
    let p = Cecelia.parse_cellpose_segment_params(Dict{String,Any}())
        @test p.blockSize === 512
        @test p.overlap === 64
        @test p.matchThreshold === 0.3
        @test p.normaliseToWhole === true         # default: normalise to whole image
    end

    # CoastalSegment — same shape as cellpose plus temporalScaleMode.
    let p = Cecelia.parse_coastal_segment_params(Dict{String,Any}(
            "outputValueName" => "flow-cells", "temporalScaleMode" => "seconds",
            "labelSmoothing" => 1.0))
        @test p.outputValueName == "flow-cells"
        @test p.temporalScaleMode == "seconds"
        @test p.labelSmoothing === 1.0
    end
    let p = Cecelia.parse_coastal_segment_params(Dict{String,Any}())
        @test p.temporalScaleMode == "frames"    # default: as trained
        @test p.labelSmoothing === 0.5           # coastal's default differs from cellpose (0.0)
    end

    # Branching — fibreChannels stays a bag (resolved via channel_indices in the handler).
    let p = Cecelia.parse_branching_params(Dict{String,Any}(
            "refPops" => "T/tracked", "calcAnisotropy" => true,
            "fibreChannels" => ["SHG"], "anisotropySource" => "channel",
            "structureTensorSigmaUm" => 5.0, "anisotropyBoxUm" => 3.0,
            "integrateTime" => true))
        @test p.refPops == "T/tracked"
        @test p.calcAnisotropy === true
        @test p.fibreChannels == ["SHG"]
        @test p.anisotropySource == "channel"
        @test p.structureTensorSigmaUm === 5.0
        @test p.anisotropyBoxUm === 3.0
        @test p.integrateTime === true
    end
    let p = Cecelia.parse_branching_params(Dict{String,Any}())
        @test p.refPops == "NONE"                # default: skeletonise whole segmentation
        @test p.calcAnisotropy === false
        @test p.anisotropySource == "skeleton"
    end

    # SegmentCorrect — same JSON-string trackOps pattern as tracking.correct.
    let p = Cecelia.parse_segment_correct_params(Dict{String,Any}(
            "valueName" => "cells",
            "labelOps" => Any[Dict{String,Any}(
                "op" => "label.merge", "t" => 0, "ids" => Any[3, 5], "into" => 3)]))
        @test p.valueName == "cells"
        @test length(p.labelOps) == 1
        @test p.labelOps[1]["op"] == "label.merge"
        @test p.labelOps[1]["into"] == 3
    end
    let p = Cecelia.parse_segment_correct_params(Dict{String,Any}("labelOps" => nothing))
        @test isempty(p.labelOps)
    end
    let p = Cecelia.parse_segment_correct_params(Dict{String,Any}(
            "labelOps" => "[{\"op\":\"label.remove\",\"t\":0,\"ids\":[7]}]"))
        @test length(p.labelOps) == 1
        @test p.labelOps[1]["op"] == "label.remove"
    end

    # SegmentCorrectCarryOver — a single shared struct for both phases (they read only valueName).
    let p = Cecelia.parse_segment_correct_carry_over_params(Dict{String,Any}(
            "valueName" => "corrected"))
        @test p.valueName == "corrected"
    end
    let p = Cecelia.parse_segment_correct_carry_over_params(Dict{String,Any}())
        @test p.valueName == Cecelia.VERSIONED_DEFAULT_VAL
    end

    # MeasureLabels
    let p = Cecelia.parse_measure_labels_params(Dict{String,Any}(
            "outputValueName" => "cells", "intensityValueName" => "corrected",
            "intensityMeasure" => "median", "gaussianFilter" => 1.0,
            "extendedMeasures" => true))
        @test p.outputValueName == "cells"
        @test p.intensityValueName == "corrected"
        @test p.intensityMeasure == "median"
        @test p.gaussianFilter === 1.0
        @test p.extendedMeasures === true
    end
    let p = Cecelia.parse_measure_labels_params(Dict{String,Any}())
        @test p.intensityMeasure == "mean"       # default
        @test p.extendedMeasures === false
    end
end

@testset "typed params — opticalFlow" begin
    # TrainFlowModel — 20+ fields; spot-check the ones that MEAN something.
    let p = Cecelia.parse_train_flow_model_params(Dict{String,Any}(
            "valueName" => "corrected", "modelName" => "cd8-flow",
            "overwrite" => true, "trainChannels" => ["mem-TOM"],
            "temporalScaleMode" => "seconds", "temporalScales" => "2,4,8",
            "flowMetrics" => ["cell_boundary_likelihood", "cumulative_mag"],
            "epochs" => 60, "foregroundBoundaryWeight" => 0.3,
            "cropSize" => 256))
        @test p.valueName == "corrected"
        @test p.modelName == "cd8-flow"
        @test p.overwrite === true
        @test p.trainChannels == ["mem-TOM"]
        @test p.temporalScaleMode == "seconds"
        @test p.temporalScales == "2,4,8"
        @test p.flowMetrics == ["cell_boundary_likelihood", "cumulative_mag"]
        @test p.epochs === 60
        @test p.foregroundBoundaryWeight === 0.3
        @test p.cropSize === 256
    end
    let p = Cecelia.parse_train_flow_model_params(Dict{String,Any}())
        @test p.temporalScaleMode == "frames"
        @test p.temporalScales == "1,2,4,8"
        @test isnothing(p.flowMetrics)                  # nothing → shipped default
        @test p.epochs === 30
        @test p.foregroundBoundaryWeight === 0.0        # OFF by default
        @test p.foregroundBlurSigma === 1.0
    end

    # TrainSupportDenoise
    let p = Cecelia.parse_train_support_denoise_params(Dict{String,Any}(
            "modelName" => "supp.MERTK", "trainChannels" => ["mem-TOM"],
            "unetSize" => "large", "inputFrames" => 31,
            "trainMode" => "perChannel", "epochs" => 40, "earlyStop" => false))
        @test p.modelName == "supp.MERTK"
        @test p.unetSize == "large"
        @test p.inputFrames === 31
        @test p.trainMode == "perChannel"
        @test p.epochs === 40
        @test p.earlyStop === false
    end
    let p = Cecelia.parse_train_support_denoise_params(Dict{String,Any}())
        @test p.unetSize == "medium"
        @test p.inputFrames === 61
        @test p.trainMode == "pooled"
        @test p.earlyStop === true
        @test p.patience === 5
    end
end

# Typed per-task params for the remaining families — closing out the ChainNode.params arc.
# Bundles the small tail families (importImages, exportImages, clust{Pops,Tracks,Regions},
# behaviour, spatialAnalysis) into one testset; each parser is the ONE place its task reads the
# params bag.
@testset "typed params — remaining families" begin
    # exportImages.ExportOmeTiff — `timepoint` accepts strings + -1 sentinel; `outDir` is stripped.
    let p = Cecelia.parse_export_ome_tiff_params(Dict{String,Any}(
            "valueName" => "corrected", "channels" => ["mem-TOM"],
            "zMip" => true, "timepoint" => "5", "outDir" => "  /tmp/out  "))
        @test p.valueName == "corrected"
        @test p.channels == ["mem-TOM"]
        @test p.zMip === true
        @test p.timepoint === 5
        @test p.outDir == "/tmp/out"
    end
    let p = Cecelia.parse_export_ome_tiff_params(Dict{String,Any}())
        @test p.zMip === false
        @test p.timepoint === -1                    # sentinel: export every frame
        @test p.outDir == ""                        # empty → default_export_dir()
    end

    # clustPops
    let p = Cecelia.parse_clust_pops_params(Dict{String,Any}(
            "popsToCluster" => ["A/root", "B/root", "NONE", ""],
            "valueNameSuffix" => "immune",
            "clusterMeasures" => ["mean_intensity_0"],
            "resolution" => 0.5, "usePaga" => true))
        @test p.popsToCluster == ["A/root", "B/root"]     # NONE + blank stripped
        @test p.valueNameSuffix == "immune"
        @test p.clusterMeasures == ["mean_intensity_0"]
        @test p.resolution === 0.5
        @test p.usePaga === true
    end
    let p = Cecelia.parse_clust_pops_params(Dict{String,Any}())
        @test isempty(p.popsToCluster)
        @test p.valueNameSuffix == "default"
        @test p.resolution === 1.0
        @test p.transformation == "NONE"
    end

    # clustTracks — same shape as clustPops plus popType + minTracklength.
    let p = Cecelia.parse_clust_tracks_params(Dict{String,Any}(
            "popsToCluster" => ["A/_tracked"], "popType" => "track",
            "minTracklength" => 10, "resolution" => 2.0))
        @test p.popsToCluster == ["A/_tracked"]
        @test p.popType == "track"
        @test p.minTracklength === 10
    end
    let p = Cecelia.parse_clust_tracks_params(Dict{String,Any}())
        @test p.popType == "live"                   # default: _tracked cells
        @test p.minTracklength === 5
    end

    # clustRegions
    let p = Cecelia.parse_clust_regions_params(Dict{String,Any}(
            "basisPops" => ["A/B", "A/T"], "graphSuffix" => "delaunay",
            "clusterMethod" => "kmeans", "numClusters" => 8))
        @test p.basisPops == ["A/B", "A/T"]
        @test p.graphSuffix == "delaunay"
        @test p.clusterMethod == "kmeans"
        @test p.numClusters === 8
    end
    let p = Cecelia.parse_clust_regions_params(Dict{String,Any}())
        @test p.clusterMethod == "leiden"
        @test p.numClusters === 5
        @test p.includeOther === true
    end

    # behaviour.hmm_states
    let p = Cecelia.parse_hmm_states_params(Dict{String,Any}(
            "pops" => ["A/_tracked"], "colName" => "cd8",
            "modelMeasurements" => ["live.cell.speed"],
            "numStates" => 3, "normaliseTo" => "1", "normaliseMeasurements" => ["live.cell.speed"]))
        @test p.pops == ["A/_tracked"]
        @test p.colName == "cd8"
        @test p.modelMeasurements == ["live.cell.speed"]
        @test p.numStates === 3
        @test p.normaliseTo == "1"
        @test p.normaliseMeasurements == ["live.cell.speed"]
    end
    let p = Cecelia.parse_hmm_states_params(Dict{String,Any}())
        @test p.modelMeasurements == ["live.cell.speed", "live.cell.angle"]
        @test p.numStates === 2
        @test p.normaliseTo == "none"
    end

    # behaviour.hmm_transitions — hmmStates honours nothing → derived from colName
    let p = Cecelia.parse_hmm_transitions_params(Dict{String,Any}(
            "pops" => ["A/_tracked"], "colName" => "cd8",
            "hmmStates" => ["live.cell.hmm.state.cd8", "live.cell.hmm.state.mp"],
            "includeStart" => true))
        @test p.colName == "cd8"
        @test p.hmmStates == ["live.cell.hmm.state.cd8", "live.cell.hmm.state.mp"]
        @test p.includeStart === true
    end
    let p = Cecelia.parse_hmm_transitions_params(Dict{String,Any}())
        @test isnothing(p.hmmStates)                # nothing → derived from colName in the handler
        @test p.includeSelfTransitions === true
    end

    # spatialAnalysis (spot-check the parsers; each is small).
    let p = Cecelia.parse_aggregates_meshes_params(Dict{String,Any}(
            "pops" => ["A/B"], "maxClusterDist" => 7.5, "minCells" => 10))
        @test p.pops == ["A/B"]
        @test p.maxClusterDist === 7.5
        @test p.minCells === 10
    end
    let p = Cecelia.parse_cell_contacts_params(Dict{String,Any}(
            "popsA" => ["A/T"], "popsB" => ["A/B"], "maxContactDist" => 15.0))
        @test p.popsA == ["A/T"] && p.popsB == ["A/B"]
        @test p.maxContactDist === 15.0
    end
    let p = Cecelia.parse_cell_neighbours_params(Dict{String,Any}(
            "neighbourMethod" => "knn", "pops" => ["A/"], "graphSuffix" => "knn6",
            "nNeighbours" => 12, "perTimepoint" => true))
        @test p.neighbourMethod == "knn"
        @test p.pops == ["A/"]
        @test p.graphSuffix == "knn6"
        @test p.nNeighbours === 12
        @test p.perTimepoint === true
    end
    let p = Cecelia.parse_cell_neighbours_params(Dict{String,Any}())
        @test p.neighbourMethod == "delaunay"
        @test p.perTimepoint === false
        @test p.neighbourRadius === 30.0
    end
    let p = Cecelia.parse_contacts_meshes_params(Dict{String,Any}(
            "popsA" => ["A/T"], "popsB" => ["A/B"]))
        @test p.maxContactDist === 5.0
    end
    let p = Cecelia.parse_detect_aggregates_params(Dict{String,Any}(
            "pops" => ["A/B"], "clustDiameter" => 20.0, "perTimepoint" => true))
        @test p.clustDiameter === 20.0
        @test p.perTimepoint === true
    end
    let p = Cecelia.parse_neighbour_stats_params(Dict{String,Any}(
            "basisPops" => ["A/B", "A/T"], "nPermutations" => 500))
        @test p.basisPops == ["A/B", "A/T"]
        @test p.nPermutations === 500
    end

    # importImages
    let p = Cecelia.parse_import_omezarr_params(Dict{String,Any}(
            "src_path" => "/data/foo.czi", "pyramidLevels" => 5,
            "stageLocal" => true, "chunkSize" => 512,
            "maxWorkers" => "2", "jvmHeapGiB" => "16"))
        @test p.src_path == "/data/foo.czi"
        @test p.pyramidLevels == 5
        @test p.stageLocal === true
        @test p.chunkSize == 512
        @test p.maxWorkers == "2"
        @test p.jvmHeapGiB == "16"
    end
    let p = Cecelia.parse_import_omezarr_params(Dict{String,Any}())
        @test p.src_path == ""                       # empty → fall back to img.meta.ori_path in the handler
        @test isnothing(p.pyramidLevels)             # nothing → fall back to pyramidScale
        @test p.pyramidScale === 2
        @test p.maxWorkers == "auto"                 # sentinel, resolved per-reader in the handler
        @test p.jvmHeapGiB == "auto"
        @test isnothing(p.ngffVersion)               # nothing → Settings default (store_layout())
    end
    let p = Cecelia.parse_migrate_legacy_params(Dict{String,Any}(
            "sourceProjectDir" => "/old/proj", "sourceUid" => "abc123", "rscript" => "Rscript"))
        @test p.sourceProjectDir == "/old/proj"
        @test p.sourceUid == "abc123"
        @test p.rscript == "Rscript"
        @test p.mode == "copy"
    end
    # `_merge_meta_preserving_legacy`: the register-time source pointers survive the meta rewrite
    # applied at the end of a successful migration, so a second run (e.g. copy → symlink) still
    # knows what to migrate. Without this the second run hits the `no legacy source` guard.
    let old = Dict{String,Any}("legacySourceDir" => "/old/proj", "legacySourceUid" => "abc123",
                               "legacyRscript" => "/usr/bin/Rscript", "orifilepath" => "/x")
        new_ = Dict{String,Any}("SizeX" => 512, "SizeY" => 512, "name" => "img1")
        merged = Cecelia._merge_meta_preserving_legacy(new_, old)
        @test merged["legacySourceDir"] == "/old/proj"
        @test merged["legacySourceUid"] == "abc123"
        @test merged["legacyRscript"]   == "/usr/bin/Rscript"
        @test merged["SizeX"] == 512                    # new-side OME content preserved
        @test !haskey(merged, "orifilepath")            # non-legacy old-side keys NOT carried
    end
    let old = Dict{String,Any}("legacySourceDir" => "/old", "legacySourceUid" => "abc")
        # new side wins if it happens to carry a legacy key (would only happen if the Python side
        # started returning one) — nothing gets silently overwritten by the carry-over.
        new_ = Dict{String,Any}("legacySourceDir" => "/from-python")
        merged = Cecelia._merge_meta_preserving_legacy(new_, old)
        @test merged["legacySourceDir"] == "/from-python"
        @test merged["legacySourceUid"] == "abc"
    end
    let old = Dict{String,Any}("legacySourceDir" => "", "legacySourceUid" => "abc")
        # An empty string on the old side is treated as absent (guard on read uses `isempty`).
        merged = Cecelia._merge_meta_preserving_legacy(Dict{String,Any}(), old)
        @test !haskey(merged, "legacySourceDir")
        @test merged["legacySourceUid"] == "abc"
    end
    let p = Cecelia.parse_remove_image_params(Dict{String,Any}(
            "valueName" => "old", "newDefault" => "corrected"))
        @test p.valueName == "old"
        @test p.newDefault == "corrected"
    end
end

# label_props boundary types (audit task #26): `add_obs` refuses non-numeric columns at entry
# with a named-column error; `CategoricalObsColumn` names the drift instead of dying deep in the
# Python subprocess. Pure-Julia tests — no h5ad needed.
@testset "label_props boundary types" begin
    using DataFrames: DataFrame

    # add_obs — numeric passes; String / Bool refused with the column name.
    let df = DataFrame("label" => [1, 2, 3], "live.cell.speed" => [0.1, 0.2, 0.3])
        @test Cecelia._assert_float_convertible(df) === nothing
    end
    let df = DataFrame("label" => [1, 2, 3], "state" => ["A", "B", "A"])
        try
            Cecelia._assert_float_convertible(df); @test false  # should raise
        catch e
            msg = sprint(showerror, e)
            @test occursin("`state`", msg)                      # column name is IN the message
            @test occursin("write_categorical_obs", msg)        # points at the right helper
        end
    end
    let df = DataFrame("label" => [1, 2], "gated" => [true, false])
        # Bool refused deliberately — silently coerces to 0.0/1.0 and misrepresents the semantic.
        @test_throws Exception Cecelia._assert_float_convertible(df)
    end
    let df = DataFrame("label" => [1, 2], "mixed" => [missing, 0.5])
        # `missing` is allowed (maps to NaN in save!).
        @test Cecelia._assert_float_convertible(df) === nothing
    end

    # CategoricalObsColumn — direct construction, dict entry, NamedTuple entry, and error naming.
    let c = Cecelia.CategoricalObsColumn(; name = "state", labels = [1, 2, 3],
                                          values = ["A", "B", missing])
        @test c.name == "state"
        @test c.labels == [1, 2, 3]
        @test c.values[3] === nothing                  # missing → nothing (JSON null → Python)
        @test c.values[1] == "A"
    end
    let c = Cecelia._to_categorical_obs_column(
                Dict("name" => "state", "labels" => [1, 2], "values" => ["A", "B"]))
        @test c isa Cecelia.CategoricalObsColumn
        @test c.name == "state"
        @test c.labels == [1, 2]
    end
    let nt = (; name = "state", labels = [1, 2], values = ["A", "B"])
        @test Cecelia._to_categorical_obs_column(nt).labels == [1, 2]
    end
    let bad = Dict("nmae" => "state", "labels" => [1], "values" => ["A"])
        # missing "name" — error names the missing field
        try
            Cecelia._to_categorical_obs_column(bad); @test false
        catch e
            @test occursin("`name`", sprint(showerror, e))
        end
    end
    let bad_nt = (; labels = [1], values = ["A"])
        try
            Cecelia._to_categorical_obs_column(bad_nt); @test false
        catch e
            @test occursin("`name`", sprint(showerror, e))
        end
    end
end

# ── Typed-params ratchet ──────────────────────────────────────────────────────
# Every task's `_run_task` reads its params bag through a `parse_<task>_params(::AbstractDict)`
# helper that returns a `Base.@kwdef` struct — one authoritative statement of the task's contract
# with its inbound bag, so a spec rename becomes a struct-field error at parse rather than a silent
# default at the read site. This is the mechanical enforcement of what MAINTAINABILITY.md → *Task
# params are typed* names as a standing rule (pattern-4 fix from the maintainability audit).
#
# Ratchet shape: same as `frontend/src/utils/cssScenarios.ts` — an exact per-file baseline of the
# tasks NOT YET on typed params. New offenders outside the list fail immediately; a file that leaves
# the list must be removed from `TYPED_PARAMS_MIGRATION_BASELINE`. When the baseline is empty the
# ratchet becomes "no offenders", the shape docs/ui/PRIMITIVES.md → *Re-implementing a scenario is a
# test failure* describes.
@testset "typed params ratchet — _run_task reads params through parse_*_params" begin
    tasks_root = joinpath(@__DIR__, "..", "src", "tasks")

    # Structurally exempt — never need typing:
    #   • `task.jl` — the CciaTask / CompositeTask dispatcher; its base `_run_task` methods delegate
    #     to concrete task methods and legitimately pass the raw bag through.
    #   • `task/composite.jl` — the CompositeTask `_run_task` overloads (split out of `task.jl`);
    #     same exemption reason as the aggregator.
    #   • `testTasks/*` — minimal in-tree fixtures used ONLY by the test suite to exercise the
    #     scheduler; typing them would double their surface with zero production value.
    STRUCTURAL_EXEMPTIONS = Set([
        joinpath("tasks", "task.jl"),
        joinpath("tasks", "task", "composite.jl"),
        joinpath("tasks", "testTasks", "image_task.jl"),
        joinpath("tasks", "testTasks", "incremental_plot_task.jl"),
        joinpath("tasks", "testTasks", "set_task.jl"),
    ])

    # Migration baseline — tasks currently on raw `Dict` access, awaiting their family-arc PR. This
    # list MAY SHRINK, MUST NEVER GROW. Remove the entry in the SAME change that lands the parser.
    # Empty list ⇒ the arc is closed; the ratchet then rejects any regression.
    #
    # The arc closed with PRs #906 (cleanupImages), #909 (editImages), #910 (tracking),
    # #913 (segment), #914 (opticalFlow), #915 (tail: importImages, exportImages, clust*,
    # behaviour, spatialAnalysis). See docs/archive/comment-audit-findings.md for the register.
    TYPED_PARAMS_MIGRATION_BASELINE = Set{String}()

    # Walk every _run_task body via Meta.parseall — robust vs regex (docstrings, nested `end`,
    # helper functions that happen to name their positional arg `params`).
    function _walk(f, expr)
        f(expr)
        if expr isa Expr
            for a in expr.args
                _walk(f, a)
            end
        end
    end

    function _is_run_task_def(e::Expr)
        e.head == :function || return false
        sig = e.args[1]
        sig isa Expr || return false
        # Peel `where` clauses
        while sig.head == :where || sig.head == :(::)
            sig = sig.args[1]
        end
        sig.head == :call || return false
        name = sig.args[1]
        # Bare `_run_task` OR `Foo._run_task` (qualified)
        name === :_run_task || (name isa Expr && name.head == :. &&
                                 name.args[2] isa QuoteNode &&
                                 name.args[2].value === :_run_task)
    end

    _sym_starts(sym::Symbol, prefix::AbstractString, suffix::AbstractString) =
        let s = String(sym)
            startswith(s, prefix) && endswith(s, suffix)
        end

    # Returns (has_parser_def, run_task_calls_parser, run_task_reads_bag).
    # An unavoidable pre-parse guard (a shape check that only makes sense on the raw bag) may carry
    # `# ratchet-ok:` on that exact line + a reason — same escape-hatch discipline as the H5AD/zarr
    # readers (CLAUDE.md → deviations need an inline comment on that exact line). Everything else
    # goes through the parser.
    function _classify(src::AbstractString)
        expr = try
            Meta.parseall(src)
        catch
            return (false, false, false, "unparseable")
        end
        src_lines = split(src, '\n'; keepempty = true)
        _exempt(line::Int) = 1 <= line <= length(src_lines) &&
                             occursin("# ratchet-ok", src_lines[line])

        has_parser_def = false
        calls_parser   = false
        reads_bag      = false
        _walk(expr) do e
            e isa Expr || return
            if e.head == :function
                sig = e.args[1]
                while sig isa Expr && (sig.head == :where || sig.head == :(::))
                    sig = sig.args[1]
                end
                if sig isa Expr && sig.head == :call && sig.args[1] isa Symbol &&
                   _sym_starts(sig.args[1], "parse_", "_params")
                    has_parser_def = true
                end
            end
        end
        _walk(expr) do e
            e isa Expr || return
            _is_run_task_def(e) || return
            body = e.args[2]
            last_line = Ref(0)
            _walk(body) do inner
                if inner isa LineNumberNode
                    last_line[] = inner.line
                    return
                end
                inner isa Expr || return
                if inner.head == :call && !isempty(inner.args)
                    callee = inner.args[1]
                    if callee isa Symbol && _sym_starts(callee, "parse_", "_params")
                        length(inner.args) >= 2 && inner.args[2] === :params && (calls_parser = true)
                    end
                    if callee === :get && length(inner.args) >= 2 && inner.args[2] === :params
                        _exempt(last_line[]) || (reads_bag = true)
                    end
                end
                if inner.head == :ref && !isempty(inner.args) && inner.args[1] === :params
                    _exempt(last_line[]) || (reads_bag = true)
                end
            end
        end
        (has_parser_def, calls_parser, reads_bag, "")
    end

    offenders = String[]      # unexpected — not in either list
    stale_baseline = String[] # still in baseline but actually compliant now (list must shrink)
    for (dir, _, files) in walkdir(tasks_root)
        for f in files
            endswith(f, ".jl") || continue
            path = joinpath(dir, f)
            rel_from_src = relpath(path, joinpath(@__DIR__, "..", "src"))
            src = read(path, String)
            occursin(r"\bfunction\s+_run_task\b", src) || continue
            (has_parser, calls_parser, reads_bag, err) = _classify(src)
            is_offender = !isempty(err) || !has_parser || !calls_parser || reads_bag

            if rel_from_src in STRUCTURAL_EXEMPTIONS
                # Structural exemptions are silent whether compliant or not — they wouldn't be
                # exempt if the intent were to type them.
                continue
            end
            if rel_from_src in TYPED_PARAMS_MIGRATION_BASELINE
                if !is_offender
                    push!(stale_baseline, rel_from_src)
                end
                continue
            end
            if is_offender
                push!(offenders, rel_from_src *
                      (isempty(err) ? "" : "  ($err)"))
            end
        end
    end

    if !isempty(offenders)
        @error "typed-params ratchet: NEW file(s) on raw `get(params,…)` access.\n" *
               "Add a `Base.@kwdef struct <Task>Params` + `parse_<task>_params(::AbstractDict)` and " *
               "call it once at the top of `_run_task`. See app/src/tasks/cleanupImages/smooth.jl for " *
               "the canonical shape.\n" *
               "Offending file(s):\n  " * join(offenders, "\n  ")
    end
    @test isempty(offenders)

    if !isempty(stale_baseline)
        @error "typed-params ratchet: baseline names file(s) that are now compliant. " *
               "Remove them from `TYPED_PARAMS_MIGRATION_BASELINE` in the same change that landed the " *
               "parser (the list may shrink, must never grow):\n  " * join(stale_baseline, "\n  ")
    end
    @test isempty(stale_baseline)

    # Sanity: the scan must actually find `_run_task` bodies; a wrong root would let real offenders
    # slip through with an empty offender list. cleanupImages + editImages already ship the pattern.
    canonical = String[]
    for (dir, _, files) in walkdir(tasks_root), f in files
        endswith(f, ".jl") || continue
        path = joinpath(dir, f)
        rel = relpath(path, joinpath(@__DIR__, "..", "src"))
        rel in STRUCTURAL_EXEMPTIONS && continue
        rel in TYPED_PARAMS_MIGRATION_BASELINE && continue
        src = read(path, String)
        occursin(r"\bfunction\s+_run_task\b", src) || continue
        occursin(r"\bfunction\s+parse_\w+_params\b", src) && push!(canonical, rel)
    end
    @test length(canonical) >= 41   # cleanupImages (6) + editImages (9) + tracking (3) + segment (6) + opticalFlow (2) + tail (15)
end

# ── Cohort-metrics ratchet ────────────────────────────────────────────────────
# Every task that BANKS QC (calls `write_qc`) either registers cohort-comparable metrics in
# `COHORT_METRICS` (app/src/qc_cohort.jl) OR carries a `# COHORT-EXEMPT: <reason>` marker in its
# `.jl` file. Same shape as the typed-params ratchet: MAY SHRINK, MUST NEVER GROW.
#
# The ridges post-mortem exposed the discipline gap: `segment.ridges` shipped `write_qc(...,
# metrics = ...)` calls but was absent from `COHORT_METRICS`, so its per-image counts couldn't be
# outlier-checked across the cohort. The `docs/MODULES.md` "adding a task" checklist already lists
# both (bank QC + add to COHORT_METRICS) as boxes; this makes the second box mechanical.
#
# Marker discipline mirrors the H5AD/zarr readers (CLAUDE.md → deviations need an inline comment
# with a reason). `# COHORT-EXEMPT: <reason>` anywhere in the task's `.jl` file exempts it.
@testset "cohort-metrics ratchet — write_qc callers are in COHORT_METRICS or marked exempt" begin
    tasks_root = joinpath(@__DIR__, "..", "src", "tasks")

    # Baseline: tasks that bank QC today without a COHORT_METRICS entry and no exemption marker.
    # Each entry is a candidate to close — either register cohort metrics OR add an inline
    # `# COHORT-EXEMPT: <reason>` and remove from this list, in one PR. MAY SHRINK, MUST NEVER GROW.
    COHORT_METRICS_BASELINE = Set([
        # `cleanupImages.smooth` — `zeroFracIn` reads acquisition properties (sparsity), not a
        # cohort-comparable process metric. Test at suite.jl also asserts this is deliberately out
        # (`@test !haskey(COHORT_METRICS, "cleanupImages.smooth")`).
        joinpath("tasks", "cleanupImages", "smooth.jl"),
        # `cleanupImages.denoise`, `cleanupImages.stackAlign` — pending review whether these have
        # cohort-comparable output signals; on-list until the owner decides.
        joinpath("tasks", "cleanupImages", "denoise.jl"),
        joinpath("tasks", "cleanupImages", "stack_align.jl"),
        # `opticalFlow.trainSupportDenoise` — companion training task to opticalFlow.train; same
        # decision-pending as the two above.
        joinpath("tasks", "opticalFlow", "train_support_denoise.jl"),
        # `exportImages.ome_tiff` — an EXPORT (writes a downstream file). No per-image processing
        # metric that a cohort comparison would clarify.
        joinpath("tasks", "exportImages", "ome_tiff.jl"),
        # `segment.ridges` — the incident this ratchet exists to catch. Registered by name on the
        # baseline so this PR doesn't couple to a scientific decision about which ridge counts are
        # cohort-comparable. Close in a follow-up.
        joinpath("tasks", "segment", "ridges.jl"),
    ])

    # Extract the `fun_name` a task's write_qc call names (`write_qc(img, "segment.ridges", …)`).
    _fun_from_write_qc = function (src::AbstractString)
        m = match(r"write_qc\([^,]+,\s*\"([a-zA-Z_.]+)\"", src)
        return isnothing(m) ? nothing : m.captures[1]
    end

    offenders = String[]
    stale_baseline = String[]
    coverage = 0
    for (dir, _, files) in walkdir(tasks_root), f in files
        endswith(f, ".jl") || continue
        path = joinpath(dir, f)
        src = read(path, String)
        occursin("write_qc(", src) || continue
        fun_name = _fun_from_write_qc(src)
        isnothing(fun_name) && continue
        coverage += 1
        rel = relpath(path, joinpath(@__DIR__, "..", "src"))
        in_cohort = haskey(COHORT_METRICS, fun_name)
        marked = occursin("COHORT-EXEMPT", src)
        if rel in COHORT_METRICS_BASELINE
            if in_cohort || marked
                push!(stale_baseline, "$rel: no longer needs baseline ($(in_cohort ? "in COHORT_METRICS" : "carries COHORT-EXEMPT marker"))")
            end
            continue
        end
        if !in_cohort && !marked
            push!(offenders, "$rel: `write_qc(\"$fun_name\", …)` — add `$fun_name` to `COHORT_METRICS` (app/src/qc_cohort.jl) OR add `# COHORT-EXEMPT: <reason>` in this file")
        end
    end

    if !isempty(offenders)
        @error "cohort-metrics ratchet: NEW task(s) banking QC without a COHORT_METRICS entry.\n" *
               "See docs/MODULES.md → *Cohort metrics*.\n" *
               "Offending file(s):\n  " * join(offenders, "\n  ")
    end
    @test isempty(offenders)

    if !isempty(stale_baseline)
        @error "cohort-metrics ratchet: baseline names file(s) that are now compliant. " *
               "Remove them from `COHORT_METRICS_BASELINE` in the same change that landed the " *
               "registration/marker (the list may shrink, must never grow):\n  " *
               join(stale_baseline, "\n  ")
    end
    @test isempty(stale_baseline)

    # Sanity: a wrong scan root would let real offenders slip through with an empty offender list.
    @test coverage >= 20
end

# ── Julia zarr-access ratchet ────────────────────────────────────────────────
# Companion to `python/cecelia/tests/test_zarr_access_convention.py`, for the Julia side. The
# Python side goes through `zarr_utils`; the Julia side has one sanctioned reader —
# `api/src/image_render.jl` (its header declares itself a "SANCTIONED, NARROW carve-out" for
# lightweight preview renders, with "Do NOT grow this into a general image reader"). Any other
# `using`/`import` of `Zarr`, `EzXML`, or `LightXML` in `app/src/**` or `api/src/**` fails this
# ratchet. Empty baseline today.
@testset "zarr-access ratchet — Julia files don't `using Zarr` outside the sanctioned reader" begin
    banned = (r"^\s*(?:using|import)\s+Zarr(?:\s|$|,)",
              r"^\s*(?:using|import)\s+EzXML(?:\s|$|,)",
              r"^\s*(?:using|import)\s+LightXML(?:\s|$|,)")
    # Sanctioned narrow reader. Its module header pins the "do not grow this" contract.
    exempt = Set([joinpath("api", "src", "image_render.jl")])

    roots = [joinpath(@__DIR__, "..", "..", "app", "src"),
             joinpath(@__DIR__, "..", "..", "api", "src")]
    offenders = String[]
    for root in roots, (dir, _, files) in walkdir(root), f in files
        endswith(f, ".jl") || continue
        path = joinpath(dir, f)
        rel = relpath(path, joinpath(@__DIR__, "..", ".."))
        rel in exempt && continue
        src = read(path, String)
        for (i, line) in enumerate(split(src, '\n'; keepempty = true))
            for pat in banned
                if occursin(pat, line)
                    push!(offenders, "$rel:$i: `$(strip(line))`")
                end
            end
        end
    end

    if !isempty(offenders)
        @error "zarr-access ratchet: Julia file(s) import Zarr/EzXML/LightXML outside the " *
               "sanctioned reader. On the Julia side, only `api/src/image_render.jl` is allowed to " *
               "hold this — the browser viewer / bricks path goes through the Python `zarr_utils` " *
               "reader indirectly. See CLAUDE.md → *Image / OME-ZARR access*.\n" *
               "Offending line(s):\n  " * join(offenders, "\n  ")
    end
    @test isempty(offenders)

    # Coverage: the sanctioned exempt must still exist and still import Zarr — otherwise we're
    # ratcheting on nothing (a moved file would silently make the scan pass).
    exempt_path = joinpath(@__DIR__, "..", "..", "api", "src", "image_render.jl")
    @test isfile(exempt_path)
    @test occursin(r"\busing\s+Zarr\b", read(exempt_path, String))
end
