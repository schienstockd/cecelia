# In-app update.
#
# Safety model: the running server NEVER overwrites its own files. `/api/update/apply` only
# DOWNLOADS + STAGES the new release bundle; the launcher (app.py) applies the staged update on the
# next restart, when nothing is using the files. Apply is refused in a dev/git checkout so it can
# never clobber source. See docs/SHIPPING.md.
#
# Testing knobs (env): CECELIA_VERSION overrides the reported running version;
# CECELIA_UPDATE_INCLUDE_PRERELEASE=1 makes the check also consider prereleases.

using Downloads

const _UPDATE_REPO = "schienstockd/cecelia"
const _APP_ROOT    = abspath(joinpath(@__DIR__, "..", ".."))   # api/src → repo / install root

# Running version: env override (testing) → VERSION file (written into release bundles) → "dev".
function _running_version()::String
    v = get(ENV, "CECELIA_VERSION", "")
    !isempty(v) && return strip(v)
    vf = joinpath(_APP_ROOT, "VERSION")
    isfile(vf) && return strip(read(vf, String))
    "dev"
end

# Installed bundle (safe to self-update) vs dev checkout (must not be clobbered).
_is_installed()::Bool = isfile(joinpath(_APP_ROOT, "VERSION")) && !isdir(joinpath(_APP_ROOT, ".git"))

# "v0.1.0-rc1" → VersionNumber; nothing if unparseable (e.g. "dev").
function _parse_ver(tag::AbstractString)
    try VersionNumber(lstrip(strip(tag), ['v', 'V'])) catch; nothing end
end

function api_version(::HTTP.Request)
    200, JSON3.write((; version=_running_version(), installed=_is_installed()))
end

# GET /api/update/check — compare the running version to the newest GitHub release.
# Stable releases only, unless CECELIA_UPDATE_INCLUDE_PRERELEASE=1.
function api_update_check(::HTTP.Request)
    current = _running_version()
    include_pre = lowercase(get(ENV, "CECELIA_UPDATE_INCLUDE_PRERELEASE", "")) in ("1", "true", "yes")
    releases = try
        resp = HTTP.get("https://api.github.com/repos/$_UPDATE_REPO/releases?per_page=20";
                        headers = ["Accept" => "application/vnd.github+json", "User-Agent" => "cecelia"],
                        readtimeout = 15, retry = false)
        JSON3.read(resp.body)
    catch e
        return 200, JSON3.write((; current, latest = nothing, updateAvailable = false,
                                   error = "could not reach GitHub: $(sprint(showerror, e))"))
    end
    best_tag = nothing; best_ver = nothing; best_url = ""
    for r in releases
        get(r, :draft, false) === true && continue
        (get(r, :prerelease, false) === true && !include_pre) && continue
        v = _parse_ver(String(get(r, :tag_name, "")))
        v === nothing && continue
        if best_ver === nothing || v > best_ver
            best_ver = v; best_tag = String(r.tag_name); best_url = String(get(r, :html_url, ""))
        end
    end
    cur = _parse_ver(current)
    avail = best_ver !== nothing && cur !== nothing && best_ver > cur
    200, JSON3.write((; current, latest = best_tag, updateAvailable = avail, url = best_url))
end

# POST /api/update/apply {version} — download + stage the target release bundle. The launcher
# applies it on the next restart. Refused in a dev/git checkout.
function api_update_apply(body_bytes::Vector{UInt8})
    _is_installed() || return 400, JSON3.write((;
        error = "Updates apply only to an installed copy, not a dev/git checkout."))
    body = try JSON3.read(String(body_bytes)) catch; Dict{Symbol,Any}() end
    tag  = String(get(body, :version, ""))
    isempty(tag) && return 400, JSON3.write((; error = "version (tag) required"))

    url     = "https://github.com/$_UPDATE_REPO/releases/download/$tag/cecelia.tar.gz"
    staging = joinpath(_APP_ROOT, ".update-staging")
    try
        rm(staging; recursive = true, force = true)
        payload = joinpath(staging, "payload"); mkpath(payload)
        tarball = joinpath(staging, "cecelia.tar.gz")
        Downloads.download(url, tarball)
        run(`tar -xzf $tarball -C $payload`)
        write(joinpath(_APP_ROOT, ".pending-update"), tag)   # marker the launcher looks for
        broadcast_ws(Dict("type" => "update:staged", "version" => tag))
        200, JSON3.write((; staged = tag,
            message = "Update $tag downloaded. Restart Cecelia to finish installing."))
    catch e
        rm(staging; recursive = true, force = true)
        500, JSON3.write((; error = "staging failed: $(sprint(showerror, e))"))
    end
end
