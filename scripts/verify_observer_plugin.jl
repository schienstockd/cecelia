# Live check of the terminal plugin's setup path on THIS OS, against the real `claude` CLI —
# `install_observer_plugin` (app/src/ai/observer_plugin.jl), the code behind Kiwi's Set up button.
#
# What the unit tests can't reach: the CLI's own behaviour per platform (on Windows an npm-installed
# `claude.cmd` runs through `cmd /c`, which re-parses every argument), named profiles with their own
# CLAUDE_CONFIG_DIR — the shared-workstation case — and a shared install the user can't write to.
# Needs the CLI, NOT a login: plugin install/uninstall never talk to the API.
#
#   CECELIA_DEV_DIR=<empty temp dir> pixi run julia --project=app scripts/verify_observer_plugin.jl
#
# Refuses to run unless CECELIA_DEV_DIR is set (all config lands there) and only ever acts on NAMED
# profiles, whose Claude config lives under it — never the default profile, i.e. never `~/.claude`.
# Run by .github/workflows/verify-observer-plugin.yml on all three OSes.
using Cecelia, JSON3

dev = get(ENV, "CECELIA_DEV_DIR", "")
isempty(dev) && error("set CECELIA_DEV_DIR to an empty temp dir — this script installs into it")
init_cecelia!()
startswith(abspath(config_dir()), abspath(dev)) || error("config_dir() is not under CECELIA_DEV_DIR")

fails = String[]
check(ok, what) = (println(ok ? "  ok   " : "  FAIL ", what); ok || push!(fails, what))

a = ClaudeAgent()
agent_available(a) || error("no `claude` CLI found")
py = python_bin_path()

# The install as a shared one: copied under a path WITH A SPACE (quoting through `cmd /c`), read-only
# where the OS lets a non-admin say so (a system install isn't the user's to write).
src = joinpath(dirname(@__DIR__), "mcp")
mcp_dir = joinpath(dev, "shared install", "mcp")
mkpath(mcp_dir)
for d in ("cecelia_mcp", "claude-plugin")
    cp(joinpath(src, d), joinpath(mcp_dir, d); force = true)
end
Sys.iswindows() || run(`chmod -R a-w $(dirname(mcp_dir))`)

dir = observer_marketplace_dir(mcp_dir)
ver = shipped_plugin_version(mcp_dir)
opts(port) = observer_plugin_options(mcp_dir, py, "http://127.0.0.1:$port")
state(o) = observer_plugin_state(read_observer_plugin(), o, dir, ver;
                                 legacy = read_registered_observer_spec() !== nothing)
use(name) = (set_active_profile!(name); mkpath(active_profile_dir()); println("profile $name → ", active_profile_dir()))

use("alice")
check(!isempty(active_profile_dir()), "a named profile has its own Claude config dir")
# an older install's registration, made the way Set up used to (`claude mcp add-json`)
spec = observer_mcp_spec(mcp_dir, py, "http://127.0.0.1:8080")
ok, out = Cecelia._run_claude_cli(Cmd(String[a.bin, "mcp", "add-json", OBSERVER_MCP_NAME, JSON3.write(spec), "-s", "user"]))
check(ok && read_registered_observer_spec() !== nothing, "legacy add-json registration in place ($(strip(out)))")
check(state(opts(8080)) === :stale, "a legacy registration reads as stale")
ok, msg = install_observer_plugin(a, mcp_dir, opts(8080); legacy = true)
println("    ", replace(msg, "\n" => " | "))
check(ok && state(opts(8080)) === :current, "Set up installs the plugin")
check(read_registered_observer_spec() === nothing, "…and removes the legacy registration")
check(read_observer_plugin().options == opts(8080), "every option stored verbatim (spaces, separators)")
ok, msg = install_observer_plugin(a, mcp_dir, opts(8080))
check(ok && msg == "Already set up", "a second click is a no-op")
check(state(opts(8081)) === :stale, "a moved port reads as stale")
ok, _ = install_observer_plugin(a, mcp_dir, opts(8081))
check(ok && state(opts(8081)) === :current, "re-sync updates the options")

use("bob")
check(state(opts(8080)) === :missing, "a second profile starts without the plugin")
ok, _ = install_observer_plugin(a, mcp_dir, opts(8080))
check(ok && state(opts(8080)) === :current, "…and gets its own install")
use("alice")
check(state(opts(8081)) === :current, "bob's install leaves alice's alone")

# The hook as the plugin runs it: the stored `python` option + the stored mcp_dir — on Windows that is
# `python.exe` and a backslash path, joined with `/cecelia_mcp/caller_hook.py` by the manifest.
o = read_observer_plugin().options
tp = joinpath(dev, "t.jsonl")
write(tp, """{"type":"assistant","message":{"role":"assistant","model":"claude-test-1","content":[]}}\n""")
ev = JSON3.write(Dict("session_id" => "s-1", "transcript_path" => tp, "tool_input" => Dict("x" => 1)))
hook = Cmd([o["python"], "-I", o["mcp_dir"] * "/cecelia_mcp/caller_hook.py"])
res = try JSON3.read(read(pipeline(hook; stdin = IOBuffer(ev)), String)) catch e; nothing end
check(res !== nothing && res.hookSpecificOutput.updatedInput.cecelia_caller_model == "claude-test-1",
      "the hook runs from the stored options and reads the model")

println("\nCLI view of alice's plugin:")
println(Cecelia._run_claude_cli(Cmd(String[a.bin, "plugin", "details", "cecelia@cecelia-local"]))[2])
isempty(fails) || error("$(length(fails)) check(s) failed:\n  " * join(fails, "\n  "))
println("all checks passed")
