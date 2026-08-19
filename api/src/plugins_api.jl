# ── Plugins API — install / remove a distributable module set ─────────────────────────────────────
#
# POST /api/plugins/install  {url, ref?}  → fetch a PINNED plugin and put it in place
# POST /api/plugins/remove   {name}       → unregister its tasks and delete its directory
#
# Listing lives on the existing `/api/tasks/custom-modules` payload (`plugins`), not a second endpoint:
# a plugin IS a custom module set, and the Settings panel that shows one shows the other.
# See docs/todo/PLUGINS_PLAN.md → P2.
#
# **The fetch is a tarball, never `git`.** An installed app has no git — both installers pull tarballs
# over plain HTTP, and `_is_installed` is defined as "has a VERSION file and has NO `.git`". Download
# is `Downloads` (stdlib, already used by the updater); unpack + verify + place is
# `Cecelia.plugin_unpack!`, which keeps the testable half in the package.
#
# **No auto-update, no background fetch, no install-on-startup.** Every install is one explicit user
# action against one explicit ref. The confirm dialog is the frontend's job; this layer refuses nothing
# on the user's behalf beyond validating the request, because the trust decision is theirs to make and
# a plugin is NOT sandboxed either way.

# Installing a NEW plugin takes effect on a reload; UPDATING one that is already loaded does not.
# Re-`include`ing a Julia struct errors, so a replaced `.jl` cannot be swapped into a running session
# (`load_custom_modules!` skips files it has already loaded). Say so in the response rather than
# letting the user wonder why their edit did nothing — Decision 7.
function _plugin_restart_needed(name::AbstractString)::Bool
    any(m -> m.status == "ok" && m.plugin == String(name), Cecelia.custom_modules_report())
end

function api_plugins_install(body::Vector{UInt8})
    req = try
        JSON3.read(String(body), Dict{String,Any})
    catch
        return 400, JSON3.write((; error = "invalid JSON body"))
    end
    url = strip(string(get(req, "url", "")))
    ref = strip(string(get(req, "ref", "")))
    isempty(url) && return 400, JSON3.write((; error = "url is required"))
    startswith(url, "http://") || startswith(url, "https://") ||
        return 400, JSON3.write((; error = "url must be http(s)"))

    name = Cecelia.plugin_name_from_url(url)
    was_loaded = _plugin_restart_needed(name)

    tmp = mktempdir()
    try
        tarball = joinpath(tmp, "plugin.tar.gz")
        src     = Cecelia.plugin_tarball_url(url, ref)
        try
            # No total timeout, matching the updater: Downloads.jl already aborts on a stalled
            # connection, and a wall-clock cap would kill a legitimately slow large download.
            Downloads.download(src, tarball)
        catch e
            return 502, JSON3.write((; error = "download failed: $(sprint(showerror, e))", source = src))
        end
        res = Cecelia.plugin_unpack!(tarball, url; ref = ref)
        res.ok || return 400, JSON3.write((; error = res.error, source = src))

        # Pick up the newly-dropped .jl immediately — installing a plugin whose tasks only appear after
        # a manual Reload would read as a failed install.
        load = Cecelia.load_custom_modules!()
        200, JSON3.write((; ok = true, name = res.name, dir = res.dir, source = src, ref = ref,
                            loaded = load.loaded, failed = [(; path = p, error = m) for (p, m) in load.failed],
                            restartRequired = was_loaded,
                            _custom_modules_payload()...))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    finally
        rm(tmp; recursive = true, force = true)
    end
end

"""
POST /api/plugins/install-local {name} — install an example plugin from THIS checkout.

`docs/examples/plugins/<name>/` is the source and the GitHub repo is a mirror published at release
time, so on a checkout the newest copy is already on disk. Without this, updating a plugin you are
editing meant pushing to GitHub and pulling the same files back — and the window where the two
disagree is exactly how a stale form reached the screen while the fix sat in the worktree.

No network, no tarball, no `gh`. Offers nothing when `docs/examples/plugins` is absent, which is the
structural test for "is this a checkout".
"""
function api_plugins_install_local(body::Vector{UInt8})
    req = try
        JSON3.read(String(body), Dict{String,Any})
    catch
        return 400, JSON3.write((; error = "invalid JSON body"))
    end
    name = strip(string(get(req, "name", "")))
    isempty(name) && return 400, JSON3.write((; error = "name is required"))

    was_loaded = _plugin_restart_needed(name)
    res = Cecelia.plugin_install_local!(name)
    res.ok || return 400, JSON3.write((; error = res.error))

    load = Cecelia.load_custom_modules!()
    200, JSON3.write((; ok = true, name = res.name, dir = res.dir, source = "bundled:$name", ref = "",
                        loaded = load.loaded,
                        failed = [(; path = p, error = m) for (p, m) in load.failed],
                        restartRequired = was_loaded,
                        _custom_modules_payload()...))
end

function api_plugins_remove(body::Vector{UInt8})
    req = try
        JSON3.read(String(body), Dict{String,Any})
    catch
        return 400, JSON3.write((; error = "invalid JSON body"))
    end
    name = strip(string(get(req, "name", "")))
    isempty(name) && return 400, JSON3.write((; error = "name is required"))
    # The name indexes a directory under the plugins root, so it must not be able to escape it.
    (occursin('/', name) || occursin('\\', name) || name in (".", "..")) &&
        return 400, JSON3.write((; error = "invalid plugin name"))

    res = Cecelia.plugin_remove!(name)
    # 409, not 400: "still running" is a state that will pass later, not a malformed request.
    res.ok || return (occursin("still running", something(res.error, "")) ? 409 : 404),
                     JSON3.write((; error = res.error))
    200, JSON3.write((; ok = true, name = name, unregistered = res.removed,
                        _custom_modules_payload()...))
end
