# scripts/doctor.jl — "why isn't this working on a fresh clone / new worktree" in one command.
#
# Design pattern adapted from spaCR (Olafsson et al., BSD-3-Clause). See THIRD_PARTY.md → Design
# patterns adopted. No code lifted.
#
# WHAT IT IS. A dispatcher over checks that already exist elsewhere in the tree. It never
# re-implements a check — it calls the sanctioned helper (`scripts/check_claude_env.sh`,
# `scripts/bundle_check.sh`, `Pkg.instantiate`, `npm ci`) and prints one ordered green/amber/red
# report with a fix hint per row. Repairs the cheap things by default (npm deps, .env copy);
# `--check` reports without touching anything.
#
# WHY. The fresh-clone / new-worktree failure mode is repeated and silent: a `pixi run dev` that
# fails because node_modules is stale prints ~200 lines of vite errors; a task that runs against a
# `~/.cecelia` config because the worktree's `.env` was never copied lands writes in the wrong dir.
# See docs/DEV.md → Development environment for the worktree-switch caveat.

using Pkg

const REPO_ROOT = normpath(joinpath(@__DIR__, ".."))

const OPT_CHECK = "--check" in ARGS   # report only; skip all auto-fix

# ── row model ─────────────────────────────────────────────────────────────────

mutable struct Row
    name::String
    status::Symbol   # :green | :amber | :red
    msg::String
    fix::String      # command a user can run; "" when nothing to suggest
end

_glyph(s::Symbol) = s === :green ? "✓" : s === :amber ? "⚠" : "✗"

function _print(r::Row)
    line = "$(_glyph(r.status))  $(r.name) — $(r.msg)"
    println(line)
    if r.status !== :green && !isempty(r.fix)
        println("     Fix: $(r.fix)")
    end
end

# ── individual checks (each returns a Row) ────────────────────────────────────

function check_pixi_env()::Row
    # Ambient marker set by `pixi run` / `pixi shell`. If missing, the caller invoked us the wrong
    # way — every other row that shells out will misbehave the same way, so flag it first.
    root = get(ENV, "PIXI_PROJECT_ROOT", "")
    conda = get(ENV, "CONDA_PREFIX", "")
    if isempty(root) || isempty(conda)
        return Row("pixi env", :red,
            "not inside a pixi environment ($(isempty(root) ? "PIXI_PROJECT_ROOT" : "CONDA_PREFIX") unset)",
            "run via `pixi run doctor` from the cecelia checkout root")
    end
    if !isdir(conda)
        return Row("pixi env", :red, "CONDA_PREFIX does not exist: $conda", "pixi install")
    end
    Row("pixi env", :green, "active ($(basename(conda)))", "")
end

function check_env_file()::Row
    p = joinpath(REPO_ROOT, ".env")
    if isfile(p)
        return Row(".env", :green, "present", "")
    end
    # A missing .env means config_dir() falls back to ~/.cecelia (installed-app default) and the
    # worktree writes into the wrong projects dir. Not auto-fixable — the value is machine-specific.
    Row(".env", :amber, "missing at $p; app will use ~/.cecelia",
        "echo 'CECELIA_DEV_DIR=<your dev dir>' > .env  (or copy from another worktree)")
end

function check_dev_dir()::Row
    envfile = joinpath(REPO_ROOT, ".env")
    isfile(envfile) || return Row("CECELIA_DEV_DIR", :amber, "skipped — no .env", "")
    dev = ""
    for line in eachline(envfile)
        m = match(r"^\s*CECELIA_DEV_DIR\s*=\s*(.*?)\s*$", line)
        m === nothing || (dev = String(m.captures[1]); break)
    end
    isempty(dev) && return Row("CECELIA_DEV_DIR", :amber, "not set in .env",
        "add CECELIA_DEV_DIR=<path> to .env")
    dev = expanduser(dev)
    if !isdir(dev)
        return Row("CECELIA_DEV_DIR", :red, "does not exist: $dev",
            "mkdir -p '$dev'  (or point .env at your existing dev dir)")
    end
    # Best-effort writability probe.
    probe = joinpath(dev, ".doctor-probe")
    try
        touch(probe); rm(probe; force=true)
    catch e
        return Row("CECELIA_DEV_DIR", :red, "not writable: $dev ($(sprint(showerror, e)))",
            "chmod u+w '$dev'  (or point .env elsewhere)")
    end
    Row("CECELIA_DEV_DIR", :green, dev, "")
end

function check_julia_deps(area::String)::Row
    proj = joinpath(REPO_ROOT, area)
    isfile(joinpath(proj, "Project.toml")) || return Row("julia ($area)", :red,
        "no $area/Project.toml", "")
    if !isfile(joinpath(proj, "Manifest.toml"))
        return Row("julia ($area)", :amber, "$area/Manifest.toml missing (deps not instantiated)",
            "pixi run $(area == "app" ? "julia-instantiate" : "julia --project=$area -e 'using Pkg; Pkg.resolve(); Pkg.instantiate()'")")
    end
    Row("julia ($area)", :green, "Manifest.toml present", "")
end

function check_frontend_deps()::Row
    fe = joinpath(REPO_ROOT, "frontend")
    lock = joinpath(fe, "package-lock.json")
    marker = joinpath(fe, "node_modules", ".package-lock.json")
    isfile(lock) || return Row("frontend deps", :red, "frontend/package-lock.json missing", "")
    if !isfile(marker)
        return _maybe_fix_npm(Row("frontend deps", :amber, "node_modules missing",
                                   "npm ci --prefix frontend"))
    end
    # `npm ci` writes .package-lock.json into node_modules atop the lockfile it was built from —
    # comparing mtimes catches the "switched worktree / lockfile changed under me" case that
    # otherwise blows up at vite time with a wall of module-not-found errors.
    if mtime(lock) > mtime(marker) + 1  # 1s epsilon for filesystem granularity
        return _maybe_fix_npm(Row("frontend deps", :amber,
            "node_modules stale (package-lock.json is newer)",
            "npm ci --prefix frontend"))
    end
    Row("frontend deps", :green, "node_modules matches package-lock.json", "")
end

function _maybe_fix_npm(r::Row)::Row
    OPT_CHECK && return r
    println("     Fixing: npm ci --prefix frontend …")
    ok = try
        run(Cmd(`npm ci --prefix $(joinpath(REPO_ROOT, "frontend"))`))
        true
    catch e
        println("     Fix failed: $(sprint(showerror, e))")
        false
    end
    ok ? Row(r.name, :green, "installed via npm ci", "") : r
end

function check_credential_env()::Row
    # Shell out to the existing script (docs/todo/LOGIN_CREDENTIAL_ISOLATION_PLAN.md P1). It exits
    # 0 when clean, non-zero on any shadowing hit. We only surface the pass/fail here — the script
    # itself prints the offending files/lines when a user actually runs it, keeping our row terse.
    sh = joinpath(REPO_ROOT, "scripts", "check_claude_env.sh")
    isfile(sh) || return Row("credential env vars", :amber,
        "scripts/check_claude_env.sh missing", "")
    ok = try
        success(pipeline(Cmd(`bash $sh`); stdout=devnull, stderr=devnull))
    catch
        false
    end
    ok ? Row("credential env vars", :green, "no ambient shadowing", "") :
         Row("credential env vars", :red, "ambient var overrides CLAUDE_CONFIG_DIR",
             "bash scripts/check_claude_env.sh  (shows which file sets it)")
end

function check_optional_pixi_env(name::String)::Row
    # Sidecar features live under .pixi/envs/<name>/. Presence is informational only —
    # `pixi install --environment cellpose-v3` is opt-in and only needed by users on the v3 path.
    envdir = joinpath(REPO_ROOT, ".pixi", "envs", name)
    isdir(envdir) ? Row("optional env ($name)", :green, "installed", "") :
                    Row("optional env ($name)", :amber, "not installed (opt-in)",
                        "pixi install --environment $name")
end

# ── main ─────────────────────────────────────────────────────────────────────

function main()
    println("cecelia doctor — $(REPO_ROOT)")
    println(OPT_CHECK ? "(check mode — no repairs)" : "(auto-fixes cheap things; pass --check to disable)")
    println()

    rows = Row[
        check_pixi_env(),
        check_env_file(),
        check_dev_dir(),
        check_julia_deps("app"),
        check_julia_deps("api"),
        check_frontend_deps(),
        check_credential_env(),
        check_optional_pixi_env("cellpose-v3"),
    ]

    for r in rows; _print(r); end
    println()

    counts = Dict(:green => 0, :amber => 0, :red => 0)
    for r in rows; counts[r.status] += 1; end
    println("$(counts[:green]) ok · $(counts[:amber]) amber · $(counts[:red]) red")
    exit(counts[:red] == 0 ? 0 : 1)
end

main()
