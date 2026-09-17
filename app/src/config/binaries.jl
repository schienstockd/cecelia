# External binary resolvers — bioformats2raw, python, Rscript. Split out of config.jl.
# Names live in the top-level `Cecelia` module; included from config.jl after the bootstrap.

# Resolve the bioformats2raw launcher: explicit config override → the copy the installer fetched
# alongside the app (`<install>/bioformats2raw/`; ~190 MB, so downloaded at install time rather than
# shipped in the release bundle — Java comes from the Pixi env) → PATH → the (likely-missing)
# default. Run via `pixi run` so the `bioformats2raw` script finds `java`. See docs/SHIPPING.md.
function bioformats2raw_bin()::String
    exe = Sys.iswindows() ? "bioformats2raw.bat" : "bioformats2raw"
    d   = get(get(cecelia_conf(), "dirs", Dict{String,Any}()), "bioformats2raw", "")
    if !isempty(string(d)) && string(d) != "/path/to/bioformats2raw"
        return joinpath(expand_user(string(d)), "bin", exe)
    end
    # `@__DIR__` = `<repo>/app/src/config` → `..`/`..`/`..` = repo (install) root.
    bundled = joinpath(@__DIR__, "..", "..", "..", "bioformats2raw", "bin", exe)
    isfile(bundled) && return bundled
    found = Sys.which(exe)
    found === nothing || return string(found)
    joinpath(_cfg_dir("bioformats2raw", "/path/to/bioformats2raw"), "bin", exe)
end

# Resolve `showinf` (Bio-Formats CLI) — the pyramid-levels advisor uses it to peek dims for formats
# without a fast Python reader (.czi/.nd2/.oir/.lsm/.oib/...). Same resolution shape as
# `bioformats2raw_bin()`: bundled next to the app (`<install>/bftools/`) → PATH → empty on miss (the
# caller then falls back to skipping the peek). Explicit override via `[dirs] bftools`. Fetched by
# `install.sh`; ~30 MB, so downloaded at install time (like bf2raw) rather than shipped in the bundle.
function showinf_bin()::String
    exe = Sys.iswindows() ? "showinf.bat" : "showinf"
    d   = get(get(cecelia_conf(), "dirs", Dict{String,Any}()), "bftools", "")
    if !isempty(string(d)) && string(d) != "/path/to/bftools"
        return joinpath(expand_user(string(d)), exe)
    end
    bundled = joinpath(@__DIR__, "..", "..", "..", "bftools", exe)
    isfile(bundled) && return bundled
    found = Sys.which(exe)
    found === nothing || return string(found)
    ""  # empty ⇒ caller skips the JVM peek path silently
end

# The shipped `[dirs] python` value. Must match `app/config.toml` — it is the sentinel for "nobody
# chose this", the same role `_PROJECTS_DIR_PLACEHOLDER` plays for the projects dir.
const _PYTHON_BIN_DEFAULT = "python3"

# Interpreter names to try, in order. PURE and parameterised on `iswin` so BOTH platforms' behaviour is
# testable from any host. Windows conda/pixi envs ship `python.exe` and frequently no `python3` at all,
# so `python` must be tried there; on Unix `python3` is the unambiguous one.
#
# A name the user DELIBERATELY configured is the only candidate: resolve it to an absolute path if we
# can, but never silently substitute a different interpreter. Falling back would run tasks under an
# interpreter that lacks the analysis deps and report nothing about why — worse than failing on the
# name they asked for. The shipped default is not a deliberate choice, so it does get the fallbacks.
_python_bin_candidates(configured::AbstractString, iswin::Bool)::Vector{String} =
    let c = String(strip(String(configured)))
        (isempty(c) || c == _PYTHON_BIN_DEFAULT) ?
            (iswin ? String["python", "python3"] : String["python3", "python"]) :
            String[c]
    end

"""
    python_bin_path() -> String

The Python interpreter the engine's subprocesses run — **resolved to an absolute path** whenever it
can be found on `PATH`.

Absolute, not the bare `"python3"` it used to return, because the string escapes the activated
environment. `pixi run` puts the Pixi env first on `PATH`, so a bare name resolves correctly for
anything *Julia* spawns (`run_py`, the napari bridge) — but the observer's MCP spec registers this
value into the user's **own** Claude Code config, where it is launched from a plain shell with no Pixi
activation. There, a bare `python3` is the *system* python, which has neither `mcp` nor `websockets`,
so the observer's tools failed to start in exactly the sessions the one-click setup was meant to
enable. It also could not work on Windows at all, where `python3` frequently does not exist.

Resolution: an explicitly configured `dirs.python` **path** (anything with a directory component) is
used verbatim — the user has said precisely which interpreter. A bare *name* (including the shipped
default `"python3"`) is resolved through `PATH`, falling back to the platform's other spellings. If
nothing resolves, the configured/legacy bare name is returned unchanged, so behaviour never gets
worse than before.
"""
function python_bin_path()::String
    raw  = strip(string(get(get(cecelia_conf(), "dirs", Dict{String,Any}()), "python", "")))
    conf = isempty(raw) ? "" : expand_user(String(raw))
    # An explicit PATH wins verbatim; a bare NAME falls through to resolution below.
    isempty(conf) || isempty(dirname(conf)) || return conf
    for cand in _python_bin_candidates(conf, Sys.iswindows())
        p = Sys.which(cand)
        isnothing(p) || return String(p)
    end
    isempty(conf) ? "python3" : conf
end

# Standard Rscript locations to try when neither the caller nor `PATH` supplies one. macOS GUI apps
# inherit a bare PATH (`/usr/bin:/bin:/usr/sbin:/sbin`) that omits both the CRAN framework and
# Homebrew, so a user whose Terminal happily runs `Rscript` still hits `FileNotFoundError: 'Rscript'`
# from the legacy-migrate scan. PURE and parameterised on OS booleans so both platforms' behaviour
# is testable from any host.
_rscript_fallback_candidates(isapple::Bool, iswin::Bool)::Vector{String} =
    isapple ? String["/Library/Frameworks/R.framework/Resources/bin/Rscript",
                     "/opt/homebrew/bin/Rscript",
                     "/usr/local/bin/Rscript"] :
    iswin   ? String[] :   # PATH covers the Linux distro-package case; no version-agnostic Win path
              String[]

"""
    rscript_bin_path(configured::AbstractString = "") -> String

Resolve the Rscript to spawn for the legacy-migrate scan and task. An explicitly configured PATH
(anything with a directory component) is used verbatim; a bare name — or empty — falls through to
`Sys.which` and then to the platform's standard install locations. Returns the caller's
value/`"Rscript"` unchanged if nothing resolves, so behaviour is never worse than passing the bare
name straight to `subprocess`.
"""
function rscript_bin_path(configured::AbstractString = "")::String
    conf = strip(String(configured))
    isempty(conf) || isempty(dirname(conf)) || return String(conf)
    name = isempty(conf) ? "Rscript" : String(conf)
    p = Sys.which(name)
    isnothing(p) || return String(p)
    for cand in _rscript_fallback_candidates(Sys.isapple(), Sys.iswindows())
        isfile(cand) && return cand
    end
    name
end
