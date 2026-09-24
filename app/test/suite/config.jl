# ── Config ────────────────────────────────────────────────────────────────
@testset "Config" begin
    @test !isempty(projects_dir())
    @test !isempty(python_bin_path())
    @test tasks_concurrent_limit() >= 1
end

# ── Single-instance lock (LOGIN_CREDENTIAL_ISOLATION_PLAN D7 / P5) ────────
# Pure pieces: stale-detection predicate, message-building, acquire/release round-trip. The LIVE
# "second `pixi run dev` produces the friendly error" case is a manual check documented in the plan.
@testset "Single-instance lock" begin
    # `_lock_is_stale`: no file / no PID / dead PID → stale (reclaim). Own PID → alive (refuse).
    @test Cecelia._lock_is_stale(nothing) === true
    @test Cecelia._lock_is_stale(Dict{String,Any}()) === true                      # no pid field → treat as stale
    @test Cecelia._lock_is_stale(Dict{String,Any}("pid" => 0)) === true            # bad pid
    @test Cecelia._lock_is_stale(Dict{String,Any}("pid" => "not an int")) === true # wrong shape
    # Own PID is always alive — bulletproof "not stale" check across OS + UID.
    @test Cecelia._lock_is_stale(Dict{String,Any}("pid" => getpid())) === false
    @test Cecelia._lock_is_stale(Dict{String,Any}("pid" => 99999999)) === true     # very unlikely to exist

    # Message-building — pure. Must name the PID, start time, and port so the remote user has
    # something to act on. The `pixi run stop` hint is deliberate — that IS the escape hatch.
    msg = Cecelia._already_running_message(
        Dict{String,Any}("pid" => 4242, "startedAt" => "2026-09-24 09:00:00", "api_port" => 8080))
    @test occursin("Cecelia is already running", msg)
    @test occursin("4242", msg)
    @test occursin("2026-09-24 09:00:00", msg)
    @test occursin("8080", msg)
    @test occursin("pixi run stop", msg)
    # A record missing optional fields still produces a usable one-liner (defensive against a
    # pre-P5 lock format if this ever gets one).
    @test occursin("Cecelia is already running",
                   Cecelia._already_running_message(Dict{String,Any}("pid" => 4242)))

    # Round-trip: acquire → lock file exists with our PID → release → gone. Uses a temp config dir
    # via CECELIA_DEV_DIR so the real config_dir() is untouched (memory rule: never write the
    # shared dev config).
    mktempdir() do tmp
        withenv("CECELIA_DEV_DIR" => tmp) do
            Cecelia.init_cecelia!()   # rebind config_dir()
            Cecelia.release_single_instance!()   # ensure clean start (idempotent)
            @test !isfile(Cecelia.single_instance_lock_path())
            Cecelia.acquire_single_instance!("127.0.0.1", 8080)
            @test isfile(Cecelia.single_instance_lock_path())
            data = Cecelia._read_lock()
            @test data !== nothing
            @test data["pid"] == getpid()
            @test data["api_port"] == 8080
            @test data["host"] == "127.0.0.1"
            @test haskey(data, "startedAt")
            # Idempotent within one process — a second acquire is a no-op, not an error.
            Cecelia.acquire_single_instance!("127.0.0.1", 8080)
            @test Cecelia._read_lock()["pid"] == getpid()
            # Re-acquire refuses when the lock names a DIFFERENT live PID.
            Cecelia.release_single_instance!()
            other = Dict{String,Any}("pid" => getpid(),   # our own pid stands in for "live"
                                     "startedAt" => "x", "host" => "127.0.0.1", "api_port" => 8080)
            write(Cecelia.single_instance_lock_path(), JSON3.write(other))
            # Force _SINGLE_INSTANCE_HELD to false so this is a real re-check, not a re-entry
            Cecelia._SINGLE_INSTANCE_HELD[] = false
            @test_throws Cecelia.AlreadyRunningError Cecelia.acquire_single_instance!("127.0.0.1", 8080)
            # A lock with a dead PID silently reclaims — no throw.
            write(Cecelia.single_instance_lock_path(),
                  JSON3.write(Dict{String,Any}("pid" => 99999999, "startedAt" => "x",
                                               "host" => "127.0.0.1", "api_port" => 8080)))
            Cecelia._SINGLE_INSTANCE_HELD[] = false
            Cecelia.acquire_single_instance!("127.0.0.1", 8080)
            @test Cecelia._read_lock()["pid"] == getpid()
            Cecelia.release_single_instance!()
            @test !isfile(Cecelia.single_instance_lock_path())
        end
        Cecelia.init_cecelia!()   # restore the real config_dir() for the rest of the suite
    end
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

    root = normpath(dirname(dirname(dirname(pathof(Cecelia)))))

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
    root = normpath(joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "test-data"))
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
    repo = normpath(dirname(dirname(dirname(pathof(Cecelia)))))
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
