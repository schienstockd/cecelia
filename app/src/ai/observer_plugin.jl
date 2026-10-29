# ── The Cecelia Claude Code plugin — how a user's OWN terminal gets the observer tools ─────────────
#
# A terminal session needs two things from us: the observer MCP server, and the caller hook that
# records WHICH model made each call (`mcp/cecelia_mcp/caller_hook.py`). An MCP server can be
# registered through `claude mcp add-json`; a hook cannot — hooks live in the user's settings.json,
# which Cecelia never edits itself. A Claude Code PLUGIN carries both and is installed by Claude
# Code's own CLI (`claude plugin marketplace add` + `claude plugin install`), so the rule holds: every
# write to the user's Claude config goes through the tool that owns it.
#
# The plugin is `mcp/claude-plugin/` (a one-plugin local marketplace). Its manifest is STATIC: every
# machine-specific value — the interpreter, the mcp/ dir, the API URL, the token FILE — is a plugin
# option set at install (`--config`), substituted by Claude Code as `${user_config.*}`. So nothing in
# the plugin can go stale against the install it points at; a moved install or a changed port is a
# changed option, detected below and fixed by the same Set-up click.
#
# Tool names change shape inside a plugin: `mcp__plugin_cecelia_cecelia-observer__<tool>`. Kiwi's own
# turns keep `--mcp-config` + `--strict-mcp-config` (which ignores plugin servers), so their
# allow-lists are unchanged; the hook matcher (`OBSERVER_HOOK_MATCHER`) covers both namings.

const OBSERVER_PLUGIN_NAME      = "cecelia"
const OBSERVER_MARKETPLACE_NAME = "cecelia-local"
const OBSERVER_PLUGIN_ID        = "$(OBSERVER_PLUGIN_NAME)@$(OBSERVER_MARKETPLACE_NAME)"

observer_marketplace_dir(mcp_dir::AbstractString)::String = joinpath(String(mcp_dir), "claude-plugin")

_observer_plugin_manifest(mcp_dir::AbstractString)::String =
    joinpath(observer_marketplace_dir(mcp_dir), OBSERVER_PLUGIN_NAME, ".claude-plugin", "plugin.json")

"""
    observer_plugin_options(mcp_dir, python_bin, api_url; token_file) -> Dict{String,String}

The plugin's option values for THIS install — the same resolution as `observer_mcp_spec`, so the
plugin's server and the `--mcp-config` one can never disagree. Keys are the manifest's `userConfig`.
"""
observer_plugin_options(mcp_dir::AbstractString, python_bin::AbstractString, api_url::AbstractString;
                        token_file::AbstractString = api_token_path())::Dict{String,String} =
    Dict{String,String}("python" => String(python_bin), "mcp_dir" => String(mcp_dir),
                        "pythonpath" => _observer_pythonpath(mcp_dir), "api_url" => String(api_url),
                        "token_file" => String(token_file))

"""
    shipped_plugin_version(mcp_dir) -> String

The `version` of the plugin this install ships, `""` if its manifest can't be read. A bump means the
manifest changed, so an installed copy at an older version is re-installed by Set up.
"""
function shipped_plugin_version(mcp_dir::AbstractString)::String
    m = _read_json_tolerant(_observer_plugin_manifest(mcp_dir))
    m === nothing ? "" : string(something(get(m, :version, nothing), ""))
end

"""
    claude_settings_path([profile_dir]) -> String

The active profile's Claude Code user settings — where `enabledPlugins`, `pluginConfigs` and
`extraKnownMarketplaces` live: the profile's own dir, `~/.claude` for the default profile. Driven by
the ACTIVE PROFILE, never an ambient `CLAUDE_CONFIG_DIR` — the same rule as `claude_config_path`, so
the plugin and legacy-registration reads always look at one profile.
"""
claude_settings_path(profile_dir::AbstractString = active_profile_dir())::String =
    joinpath(isempty(profile_dir) ? joinpath(homedir(), ".claude") : String(profile_dir), "settings.json")

claude_installed_plugins_path(profile_dir::AbstractString = active_profile_dir())::String =
    joinpath(dirname(claude_settings_path(profile_dir)), "plugins", "installed_plugins.json")

_jget(d, keys...) = foldl((x, k) -> x isa AbstractDict ? get(x, Symbol(k), nothing) : nothing, keys; init = d)

"""
    read_observer_plugin([settings_path, installed_path]) -> NamedTuple

What Claude Code has for our plugin: `(marketplace, installed, enabled, version, options)` —
`marketplace` is the registered marketplace dir (`""` if none), `options` the stored option values.
"""
function read_observer_plugin(settings_path::AbstractString = claude_settings_path(),
                              installed_path::AbstractString = claude_installed_plugins_path())
    s = _read_json_tolerant(settings_path)
    inst = _jget(_read_json_tolerant(installed_path), "plugins", OBSERVER_PLUGIN_ID)
    rec = inst isa AbstractVector && !isempty(inst) ? first(inst) : nothing
    opts = _jget(s, "pluginConfigs", OBSERVER_PLUGIN_ID, "options")
    (marketplace = string(something(_jget(s, "extraKnownMarketplaces", OBSERVER_MARKETPLACE_NAME,
                                          "source", "path"), "")),
     installed   = rec !== nothing,
     enabled     = _jget(s, "enabledPlugins", OBSERVER_PLUGIN_ID) === true,
     version     = string(something(_jget(rec, "version"), "")),
     options     = opts isa AbstractDict ?
                   Dict{String,String}(String(k) => string(v) for (k, v) in opts) : Dict{String,String}())
end

"""
    observer_plugin_state(have, want_options, want_marketplace, want_version; legacy = false) -> Symbol

PURE. `:current` — installed, enabled, from this install's marketplace, at its version, with these
options. `:missing` — not installed (or switched off). `:stale` — anything else, including a leftover
`claude mcp add-json` registration (`legacy`): it gives the tools without the caller hook, and beside
the plugin it would put every tool in the session twice.
"""
function observer_plugin_state(have, want_options::AbstractDict, want_marketplace::AbstractString,
                               want_version::AbstractString; legacy::Bool = false)::Symbol
    legacy && return :stale
    (have.installed && have.enabled) || return :missing
    (have.marketplace == want_marketplace && have.version == want_version &&
     have.options == want_options) ? :current : :stale
end

# ── CLI builders (PURE → unit-tested) ──────────────────────────────────────────────────────────────
_build_marketplace_add_cmd(a::ClaudeAgent, dir::AbstractString)::Cmd =
    Cmd(String[a.bin, "plugin", "marketplace", "add", String(dir)])
_build_marketplace_remove_cmd(a::ClaudeAgent)::Cmd =
    Cmd(String[a.bin, "plugin", "marketplace", "remove", OBSERVER_MARKETPLACE_NAME])
_build_plugin_uninstall_cmd(a::ClaudeAgent)::Cmd =
    Cmd(String[a.bin, "plugin", "uninstall", OBSERVER_PLUGIN_ID])
function _build_plugin_install_cmd(a::ClaudeAgent, options::AbstractDict)::Cmd
    args = String[a.bin, "plugin", "install", OBSERVER_PLUGIN_ID]
    for k in sort!(collect(keys(options)))   # sorted: one argv per input, for the tests
        append!(args, ["--config", string(k, "=", options[k])])
    end
    Cmd(args)
end

"""
    install_observer_plugin(a, mcp_dir, options; have, legacy, timeout_s) -> (ok, message)

LIVE (spawns the CLI). Bring the plugin to `:current`, then remove the legacy `mcp add-json` entry.

- A marketplace registered at ANOTHER install's folder is removed first (that also uninstalls the
  plugin — measured) and re-added at ours.
- `plugin install` on an installed plugin is a no-op that does NOT update its options, so a re-sync is
  uninstall + install. If that install fails, the previous options are put back (best-effort, reported):
  the user is never left with less than before the click.
- The legacy registration is removed ONLY after the plugin is in: removing it first and then failing
  would leave the user with no tools at all.
"""
function install_observer_plugin(a::ClaudeAgent, mcp_dir::AbstractString, options::AbstractDict;
                                 have = read_observer_plugin(), legacy::Bool = false,
                                 timeout_s::Int = 60)
    agent_available(a) || return (false, "No assistant CLI found. Install Claude Code to enable this.")
    run1(cmd) = _run_claude_cli(cmd; timeout_s)
    dir = observer_marketplace_dir(mcp_dir)
    log = String[]
    if have.marketplace != dir
        isempty(have.marketplace) || run1(_build_marketplace_remove_cmd(a))
        ok, out = run1(_build_marketplace_add_cmd(a, dir))
        ok || return (false, strip(out))
        have = read_observer_plugin()
    end
    want_version = shipped_plugin_version(mcp_dir)
    if observer_plugin_state(have, options, dir, want_version) !== :current
        prior = have.installed ? have.options : nothing
        have.installed && run1(_build_plugin_uninstall_cmd(a))
        ok, out = run1(_build_plugin_install_cmd(a, options))
        if !ok
            restored = prior !== nothing && !isempty(prior) && first(run1(_build_plugin_install_cmd(a, prior)))
            return (false, string(strip(out), prior === nothing ? "" :
                restored ? "\n(your previous Cecelia plugin settings were restored)" :
                           "\n(could not restore your previous Cecelia plugin settings)"))
        end
        push!(log, "Installed the Cecelia plugin")
    end
    if legacy
        ok, out = run1(_build_mcp_remove_cmd(a; scope = "user"))
        push!(log, ok ? "Replaced the older $(OBSERVER_MCP_NAME) registration" :
                        "Could not remove the older $(OBSERVER_MCP_NAME) registration: $(strip(out))")
    end
    (true, isempty(log) ? "Already set up" : join(log, "\n"))
end
