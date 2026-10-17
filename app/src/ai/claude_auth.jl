# ── Claude login per profile — who is a profile logged in as, log it in, log it out ──────────────
#
# Each profile's Claude credentials live in its own `CLAUDE_CONFIG_DIR` (`active_profile_dir`;
# `""` = the default profile = the CLI's own `~/.claude`). These read and clear them through the
# CLI's own non-interactive verbs — `claude auth status --json` and `claude auth logout` — never by
# touching the credential store ourselves: on macOS it is the Keychain, not a file.
#
# Measured on Linux, CLI 2.1.301: both verbs honour `CLAUDE_CONFIG_DIR` and take ~1 s; logout removes
# only that dir's `.credentials.json` (the home login was untouched), is idempotent, and exits 0 on an
# already-logged-out dir. `auth status` exits 1 when logged out but still prints the JSON.

"""
    ClaudeAuth

One profile's login as `claude auth status --json` reports it. `state` is `:loggedIn`,
`:loggedOut`, or `:unknown` (CLI missing, timed out, or output not parseable — never shown as
"logged out", which would invite a pointless re-login).
"""
struct ClaudeAuth
    state::Symbol
    email::String
    method::String     # "claude.ai", "console", "none", … — the CLI's `authMethod`
    org::String
end

const CLAUDE_AUTH_UNKNOWN = ClaudeAuth(:unknown, "", "", "")
const CLAUDE_DEFAULT_LOGOUT_REFUSED = "Peanut's Claude login is the terminal's — log out there"

"""
    parse_claude_auth_status(output) -> ClaudeAuth

Parse `claude auth status --json`. Tolerates noise around the JSON object (a CLI update banner);
anything without a Bool `loggedIn` is `:unknown`. PURE → unit-tested.
"""
function parse_claude_auth_status(output::AbstractString)::ClaudeAuth
    i, j = findfirst('{', output), findlast('}', output)
    (isnothing(i) || isnothing(j) || j < i) && return CLAUDE_AUTH_UNKNOWN
    d = try
        JSON3.read(output[i:j])
    catch
        return CLAUDE_AUTH_UNKNOWN
    end
    d isa AbstractDict || return CLAUDE_AUTH_UNKNOWN
    li = get(d, :loggedIn, nothing)
    li isa Bool || return CLAUDE_AUTH_UNKNOWN
    _s(k) = (v = get(d, k, ""); v isa AbstractString ? String(v) : "")
    ClaudeAuth(li ? :loggedIn : :loggedOut, _s(:email), _s(:authMethod), _s(:orgName))
end

# Status is a ~1 s CLI spawn and Kiwi + Preferences ask on open, so it is cached per profile dir.
# Short TTL: a login done in a terminal shows up within it. Logout/select invalidate explicitly.
const _CLAUDE_AUTH_TTL_S = 15.0
const _CLAUDE_AUTH_CACHE = Dict{String,Tuple{Float64,ClaudeAuth}}()
const _CLAUDE_AUTH_LOCK  = ReentrantLock()

_forget_claude_auth!(profile_dir::AbstractString) =
    lock(() -> delete!(_CLAUDE_AUTH_CACHE, String(profile_dir)), _CLAUDE_AUTH_LOCK)

"""
    claude_auth_status(profile_dir; fresh = false) -> ClaudeAuth

Who `profile_dir`'s Claude login is (`""` = the default profile). Cached for a few seconds;
`fresh = true` bypasses the cache. LIVE path (spawns the CLI) — `parse_claude_auth_status` is the
tested surface.
"""
function claude_auth_status(profile_dir::AbstractString; fresh::Bool = false)::ClaudeAuth
    key = String(profile_dir)
    if !fresh
        hit = lock(() -> get(_CLAUDE_AUTH_CACHE, key, nothing), _CLAUDE_AUTH_LOCK)
        !isnothing(hit) && time() - hit[1] < _CLAUDE_AUTH_TTL_S && return hit[2]
    end
    isnothing(agent_bin_path(observer_agent_bin())) && return CLAUDE_AUTH_UNKNOWN
    _, output = _run_claude_cli(`$(observer_agent_bin()) auth status --json`;
                                profile_dir = key, timeout_s = 20)
    auth = parse_claude_auth_status(output)
    auth.state === :unknown || lock(() -> (_CLAUDE_AUTH_CACHE[key] = (time(), auth)), _CLAUDE_AUTH_LOCK)
    auth
end

"""
    claude_auth_logout!(profile_dir; timeout_s = 30) -> (ok, message)

Log `profile_dir`'s Claude login out. Idempotent. LIVE path. NEVER the default profile (`""`, i.e.
peanut = `~/.claude`): that login is also every terminal `claude` on this OS account, so Cecelia
refuses rather than end it (plan D21) — every logout path funnels through here.
"""
function claude_auth_logout!(profile_dir::AbstractString; timeout_s::Real = 30)::Tuple{Bool,String}
    isempty(profile_dir) && return (false, CLAUDE_DEFAULT_LOGOUT_REFUSED)
    isnothing(agent_bin_path(observer_agent_bin())) &&
        return (false, "No assistant CLI found.")
    ok, output = _run_claude_cli(`$(observer_agent_bin()) auth logout`;
                                 profile_dir = String(profile_dir), timeout_s)
    _forget_claude_auth!(profile_dir)
    (ok, String(strip(output)))
end

# ── Logging in — Cecelia runs `claude auth login` itself (LOGIN_CREDENTIAL_ISOLATION_PLAN P9) ──────
#
# Why the login runs here and not in a terminal: docs/todo/LOGIN_CREDENTIAL_ISOLATION_PLAN.md → P9.
# What the CLI does, measured (CLI 2.1.302):
#
# - `claude auth login` is a non-REPL verb: it exits once the login finishes (exit 1 + `Login failed:
#   …` on a bad code). It works with stdin as a pipe and reads a pasted code from it.
# - It opens the browser through `$BROWSER` with a localhost-callback URL, AND prints a fallback URL
#   whose page shows a code to paste. Either finishes the login, so both are offered.
# - `$BROWSER` is one executable (the CLI does not split it on spaces), so the private-window flag
#   goes in a per-login shim script. On POSIX Cecelia NEVER lets the CLI open a normal window: with no
#   private-capable browser found the shim does nothing and the user gets the link.
# - Windows: `$BROWSER` handling is unmeasured, so no shim — the CLI opens its default browser.

"""
    PrivateBrowser

A browser Cecelia can open in a private window: the command prefix to run before the URL and a
label for the UI (`"Firefox"`).
"""
struct PrivateBrowser
    argv::Vector{String}
    label::String
end

# (substring of the default-browser desktop id, executables to look for, private flag, label).
# Order matters: the first installed one wins when the default is unknown or not private-capable.
# `chromium` sits before `chrome` only for readability — neither id contains the other's key.
const _PRIVATE_BROWSERS = (
    ("firefox",  ("firefox",),                                 "--private-window", "Firefox"),
    ("chrome",   ("google-chrome", "google-chrome-stable"),    "--incognito",      "Chrome"),
    ("chromium", ("chromium", "chromium-browser"),             "--incognito",      "Chromium"),
    ("brave",    ("brave-browser", "brave"),                   "--incognito",      "Brave"),
    ("edge",     ("microsoft-edge", "microsoft-edge-stable"),  "--inprivate",      "Edge"),
)

# macOS: `.app` bundle name per label, launched through `open -na <app> --args <flag> <url>`.
const _MAC_BROWSER_APPS = Dict("Firefox" => "Firefox", "Chrome" => "Google Chrome",
                               "Chromium" => "Chromium", "Brave" => "Brave Browser",
                               "Edge" => "Microsoft Edge")

"""
    resolve_private_browser(default_id; which = Sys.which, mac = Sys.isapple(), app_exists) -> Union{PrivateBrowser,Nothing}

The browser to open the login in, privately. Linux: the desktop's default browser
(`xdg-settings get default-web-browser`, e.g. `firefox_firefox.desktop`) if it is one of
`_PRIVATE_BROWSERS` and installed, else the first installed one. macOS: the first installed
`/Applications` bundle (the default browser is not looked up — UNVERIFIED, no Mac to test on).
`nothing` when none is found. PURE given its injected lookups → unit-tested.
"""
function resolve_private_browser(default_id::AbstractString;
                                 which::Function = Sys.which, mac::Bool = Sys.isapple(),
                                 app_exists::Function = app -> isdir("/Applications/$(app).app"))::Union{PrivateBrowser,Nothing}
    if mac
        for (_, _, flag, label) in _PRIVATE_BROWSERS
            app = _MAC_BROWSER_APPS[label]
            app_exists(app) && return PrivateBrowser(["open", "-na", app, "--args", flag], label)
        end
        return nothing
    end
    function found(bins)
        for x in bins
            b = which(x)
            isnothing(b) || return String(b)
        end
        nothing
    end
    id = lowercase(String(default_id))
    order = sort(collect(_PRIVATE_BROWSERS); by = e -> !occursin(e[1], id))   # default first (stable)
    for (_, bins, flag, label) in order
        bin = found(bins)
        isnothing(bin) || return PrivateBrowser([bin, flag], label)
    end
    nothing
end

# The desktop's default browser id on Linux ("" when unknown). Best-effort, never throws.
function _default_browser_id()::String
    Sys.islinux() || return ""
    isnothing(Sys.which("xdg-settings")) && return ""
    try
        String(strip(read(pipeline(ignorestatus(`xdg-settings get default-web-browser`);
                                   stderr = devnull), String)))
    catch
        ""
    end
end

"""
    private_browser_shim(browser) -> String

The `\$BROWSER` script for one login: opens the URL (`\$1`) in `browser`'s private window, or does
nothing when `browser` is `nothing` (the user gets the link instead). POSIX `sh`. PURE → tested.
"""
function private_browser_shim(browser::Union{PrivateBrowser,Nothing})::String
    isnothing(browser) && return "#!/bin/sh\nexit 0\n"
    q(s) = "'" * replace(s, "'" => "'\\''") * "'"
    "#!/bin/sh\nexec " * join(q.(browser.argv), " ") * " \"\$1\" >/dev/null 2>&1\n"
end

"""
    parse_login_url(output) -> String

The paste-code URL `claude auth login` prints after `visit:` (ANSI + OSC-8 hyperlink escapes
stripped — the CLI adds them on a terminal). `""` until it has been printed. PURE → tested.
"""
function parse_login_url(output::AbstractString)::String
    s = replace(String(output), r"\e\]8;[^\a\e]*(\a|\e\\)" => "", r"\e\[[0-9;?]*[A-Za-z]" => "")
    m = match(r"visit:\s*(https://\S+)", s)
    isnothing(m) ? "" : String(m.captures[1])
end

# Last non-empty output line — the CLI's own verdict (`Login successful.` / `Login failed: …`), minus
# the paste prompt it shares a line with (stdin is a pipe, so no newline follows the prompt).
function _last_line(output::AbstractString)::String
    for l in Iterators.reverse(split(output, '\n'))
        isempty(strip(l)) || return String(strip(replace(l, r"^.*prompted\s*>" => "")))
    end
    ""
end

"""
    ClaudeLogin

One `claude auth login` in flight (or just finished) for a profile dir. `state` is `:waiting`,
`:loggedIn` or `:failed`; `url` is the paste-code fallback link (`""` until printed); `browser` names
the private window opened (`""` = none — the link is the way).
"""
mutable struct ClaudeLogin
    proc::Union{Base.Process,Nothing}
    input::Union{Pipe,Nothing}
    output::String
    state::Symbol
    browser::String
    tmp::String
end

const _CLAUDE_LOGINS = Dict{String,ClaudeLogin}()
const _CLAUDE_LOGIN_LOCK = ReentrantLock()
const _CLAUDE_LOGIN_TIMEOUT_S = 600   # an abandoned login is killed after 10 min

"""
    claude_login_status(profile_dir) -> NamedTuple

`(state, url, browser, message)` for the profile's current/last login; `state = "none"` when none
has been started. `message` is the CLI's last line (the failure reason on `failed`).
"""
function claude_login_status(profile_dir::AbstractString)
    lock(_CLAUDE_LOGIN_LOCK) do
        l = get(_CLAUDE_LOGINS, String(profile_dir), nothing)
        isnothing(l) && return (; state = "none", url = "", browser = "", message = "")
        (; state = String(l.state), url = parse_login_url(l.output), browser = l.browser,
           message = l.state === :waiting ? "" : _last_line(l.output))
    end
end

"""
    claude_login_start!(profile_dir) -> (ok, message)

Start `claude auth login` under `profile_dir` (`""` = the default profile, `~/.claude`), with
`\$BROWSER` set to a private-window shim on POSIX. A login already running for that dir is killed
first (one login in flight per profile). Returns immediately; poll `claude_login_status`. LIVE path.
"""
function claude_login_start!(profile_dir::AbstractString)::Tuple{Bool,String}
    bin = observer_agent_bin()
    isnothing(agent_bin_path(bin)) && return (false, "No assistant CLI found.")
    key = String(profile_dir)
    claude_login_cancel!(key)
    isempty(key) || isdir(key) || mkpath(key)
    tmp = mktempdir(; cleanup = false)
    cmd = _apply_claude_env(_agent_spawn_cmd(`$bin auth login`), key)
    browser = ""
    if !Sys.iswindows()
        pb = resolve_private_browser(_default_browser_id())
        shim = joinpath(tmp, "open-private")
        write(shim, private_browser_shim(pb))
        chmod(shim, 0o700)
        cmd = addenv(cmd, "BROWSER" => shim)
        browser = isnothing(pb) ? "" : pb.label
    end
    input, out = Pipe(), Pipe()
    proc = try
        run(pipeline(cmd; stdin = input, stdout = out, stderr = out); wait = false)
    catch e
        rm(tmp; recursive = true, force = true)
        return (false, sprint(showerror, e))
    end
    close(input.out); close(out.in)
    login = ClaudeLogin(proc, input, "", :waiting, browser, tmp)
    lock(() -> (_CLAUDE_LOGINS[key] = login), _CLAUDE_LOGIN_LOCK)
    Threads.@spawn _watch_claude_login!(login, key, out)
    (true, "")
end

# Collect the CLI's output as it arrives (the URL is printed before it waits), then record how it
# ended. Signal-killed (cancel / timeout) reads as failed — libuv reports exitcode 0 for it.
function _watch_claude_login!(login::ClaudeLogin, key::String, out::Pipe)
    timer = Timer(_ -> (try; _kill_proc_tree(login.proc); catch; end), _CLAUDE_LOGIN_TIMEOUT_S)
    try
        while !eof(out)
            chunk = String(readavailable(out))
            lock(() -> (login.output *= chunk), _CLAUDE_LOGIN_LOCK)
        end
        wait(login.proc)
    catch
    end
    close(timer)
    ok = login.proc.exitcode == 0 && login.proc.termsignal == 0
    lock(_CLAUDE_LOGIN_LOCK) do
        login.state = ok ? :loggedIn : :failed
        try; close(login.input); catch; end
    end
    rm(login.tmp; recursive = true, force = true)
    _forget_claude_auth!(key)
    # A cancel (dialog closed) has no verdict line — only a real failure is worth a console line.
    msg = _last_line(login.output)
    ok || isempty(msg) || @warn "Claude login did not complete" message = msg
    nothing
end

"""
    claude_login_code!(profile_dir, code) -> (ok, message)

Hand the code from the fallback page to the waiting `claude auth login`. LIVE path.
"""
function claude_login_code!(profile_dir::AbstractString, code::AbstractString)::Tuple{Bool,String}
    c = strip(String(code))
    isempty(c) && return (false, "Paste the code from the sign-in page.")
    lock(_CLAUDE_LOGIN_LOCK) do
        l = get(_CLAUDE_LOGINS, String(profile_dir), nothing)
        (isnothing(l) || l.state !== :waiting) && return (false, "No login is waiting for a code.")
        try
            write(l.input, c, "\n"); flush(l.input)
            (true, "")
        catch e
            (false, sprint(showerror, e))
        end
    end
end

"""
    claude_login_cancel!(profile_dir) -> Bool

Kill a waiting login for the profile (true if one was running). The watcher marks it failed.
"""
function claude_login_cancel!(profile_dir::AbstractString)::Bool
    l = lock(() -> get(_CLAUDE_LOGINS, String(profile_dir), nothing), _CLAUDE_LOGIN_LOCK)
    (isnothing(l) || l.state !== :waiting || isnothing(l.proc)) && return false
    try; _kill_proc_tree(l.proc); catch; end
    true
end
