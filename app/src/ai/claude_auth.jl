# ── Claude login per profile — who is a profile logged in as, and log it out ────────────────────
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
    claude_auth_logout!(profile_dir) -> (ok, message)

Log `profile_dir`'s Claude login out (`""` = the default profile — which is also the login every
terminal `claude` on this OS account uses). Idempotent. LIVE path.
"""
function claude_auth_logout!(profile_dir::AbstractString)::Tuple{Bool,String}
    isnothing(agent_bin_path(observer_agent_bin())) &&
        return (false, "No assistant CLI found.")
    ok, output = _run_claude_cli(`$(observer_agent_bin()) auth logout`;
                                 profile_dir = String(profile_dir), timeout_s = 30)
    _forget_claude_auth!(profile_dir)
    (ok, String(strip(output)))
end
