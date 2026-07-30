# In-app update.
#
# Safety model: the running server NEVER overwrites its own files. `/api/update/apply` only
# DOWNLOADS + STAGES the new release bundle; the launcher (app.py) applies the staged update on the
# next restart, when nothing is using the files. Apply is refused in a dev/git checkout so it can
# never clobber source. See docs/SHIPPING.md.
#
# Testing knobs (env): CECELIA_VERSION overrides the reported running version.
#
# Prereleases are treated as releases: Cecelia ships RC tags as its actual releases (the install
# script defaults to the newest RC — see docs/SHIPPING.md), and major/stable releases are rare.
# So the check considers ALL non-draft releases; there's no separate "stable-only" track.

using Downloads

const _UPDATE_REPO = "schienstockd/cecelia"

# GitHub release notes come as markdown. The `releaseNotes` field is passed through raw; the
# frontend renders it with `marked` for full GitHub-flavoured markdown parity (task lists,
# strikethrough, tables). Julia's Markdown stdlib is incomplete for GFM so we don't use it.
const _APP_ROOT    = abspath(joinpath(@__DIR__, "..", ".."))   # api/src → repo / install root

# Running version: env override (testing) → VERSION file (written into release bundles) → "dev".
function _running_version()::String
    v = get(ENV, "CECELIA_VERSION", "")
    !isempty(v) && return strip(v)
    vf = joinpath(_APP_ROOT, "VERSION")
    isfile(vf) && return strip(read(vf, String))
    "dev"
end

# Installed bundle (safe to self-update) vs dev checkout (must not be clobbered). `root` param is for
# tests; production always uses the real install root.
_is_installed(root::AbstractString = _APP_ROOT)::Bool =
    isfile(joinpath(root, "VERSION")) && !isdir(joinpath(root, ".git"))

# Install scope for update purposes:
#   "dev"    — git/source checkout: never self-update (would clobber source).
#   "system" — shared system-wide install (/opt, /Applications, Program Files): the app files are
#              admin-owned/read-only, so in-app apply is refused — an admin re-runs install-system.
#   "user"   — per-user install: self-update is fine.
# The installer writes `.cecelia-scope` ("system"/"user") at the install root; absent → "user".
# (Config location does NOT use this — it's always per-user ~/.cecelia; see docs/todo/ONBOARDING_PLAN.md D1.)
function _install_scope(root::AbstractString = _APP_ROOT)::String
    _is_installed(root) || return "dev"
    f = joinpath(root, ".cecelia-scope")
    (isfile(f) && strip(read(f, String)) == "system") ? "system" : "user"
end

# A prerelease identifier that is LETTERS IMMEDIATELY FOLLOWED BY DIGITS (`rc10`) is one opaque
# string to `VersionNumber`, so it compares LEXICOGRAPHICALLY: `"rc10" < "rc9"`. Splitting it into
# `rc.10` makes the number a numeric identifier, which compares numerically — the semver rule.
const _RC_SPLIT = r"-([A-Za-z]+)(\d+)" => s"-\1.\2"

"""
    _parse_ver(tag) -> VersionNumber | nothing

`"v0.1.0-rc1"` → a `VersionNumber`; `nothing` if unparseable (e.g. `"dev"`).

**Why the rewrite.** Julia parses `v"0.1.0-rc10"`'s prerelease as the single STRING `("rc10",)`, not
`("rc", 10)`, and strings compare lexicographically — so `"rc10" < "rc9"` and **rc10 sorted BELOW
rc9**. `api_update_check` picks the max release, so once rc10 existed the max stayed rc9: every
client was told rc9 was the newest release, anyone on rc9 saw "up to date", and anyone older was
updated *to* rc9 and then stuck there permanently. Silent — no error anywhere. Latent from rc1,
triggered at the 9→10 boundary.

Rewriting `-rc10` → `-rc.10` before parsing makes the digits a numeric identifier:

    v"0.1.0-rc.10" > v"0.1.0-rc.9"   # true — what we want
    v"0.1.0-rc10"  > v"0.1.0-rc9"    # false — the bug

Both sides of every comparison go through here, so the normalisation is self-consistent; it changes
relative order only where the lexicographic result was already wrong. A prerelease still sorts below
its release (`0.1.0-rc.9 < 0.1.0`), which is what lets a stable tag supersede any rc.

NOTE this cannot rescue clients ALREADY running an affected build — they compare with their own copy
of this function. The next tag has to outrank `v0.1.0-rc9` under the OLD comparator (`v0.1.0`,
`v0.2.0`, or `v0.1.0-rc9.N`) to reach them at all.
"""
function _parse_ver(tag::AbstractString)
    s = lstrip(strip(tag), ['v', 'V'])
    try VersionNumber(replace(s, _RC_SPLIT)) catch; nothing end
end

function api_version(::HTTP.Request)
    200, JSON3.write((; version=_running_version(), installed=_is_installed()))
end

# A release tag, and nothing else. `tag` arrives in the POST body and is interpolated into the
# download URL and written to `.pending-update` for the launcher, so it must not be free-form: a
# value containing `../` or a query string would point the download at some other path in the repo.
# Anchored, so it matches the WHOLE string.
const _TAG_RE = r"^v?\d+\.\d+\.\d+(?:-[A-Za-z0-9.]+)?$"

_valid_tag(tag::AbstractString)::Bool = occursin(_TAG_RE, tag)

"""
    _apply_precheck(tag; scope, installed) -> nothing | (status, error_message)

The guard rails on `/api/update/apply`, separated from the download so they can be tested without a
network or an installed root — the endpoint itself refuses to run in a git checkout, which is
exactly where the tests live, so nothing could reach the body otherwise.

`nothing` means "cleared to download".
"""
function _apply_precheck(tag::AbstractString; scope::AbstractString, installed::Bool)
    scope == "system" && return (403, "This is a shared (system-wide) installation — updates must be run by an administrator (re-run the install-system script).")
    installed || return (400, "Updates apply only to an installed copy, not a dev/git checkout.")
    isempty(tag) && return (400, "version (tag) required")
    _valid_tag(tag) || return (400, "not a valid release tag: $(repr(tag))")
    nothing
end

# GET /api/update/check — compare the running version to the newest GitHub release.
# All non-draft releases considered (RCs are shipped as releases — see header comment).
function api_update_check(::HTTP.Request)
    current = _running_version()
    releases = try
        # 100 is GitHub's per-page maximum. The winner is the max BY VERSION, but the page is
        # ordered by DATE, so a bounded page can hide a higher version that was published earlier —
        # at per_page=20 that was ~20 releases away, i.e. reachable. 100 is years of heartbeat tags;
        # if this repo ever passes it, this needs real pagination rather than a bigger number.
        resp = HTTP.get("https://api.github.com/repos/$_UPDATE_REPO/releases?per_page=100";
                        headers = ["Accept" => "application/vnd.github+json", "User-Agent" => "cecelia"],
                        read_idle_timeout = 15, retry = false)
        JSON3.read(resp.body)
    catch e
        return 200, JSON3.write((; current, latest = nothing, updateAvailable = false,
                                   error = "could not reach GitHub: $(sprint(showerror, e))"))
    end
    best_tag = nothing; best_ver = nothing; best_url = ""; best_body = ""; best_at = ""
    for r in releases
        get(r, :draft, false) === true && continue
        v = _parse_ver(String(get(r, :tag_name, "")))
        v === nothing && continue
        if best_ver === nothing || v > best_ver
            best_ver  = v
            best_tag  = String(r.tag_name)
            best_url  = String(get(r, :html_url,      ""))
            best_body = String(get(r, :body,          ""))
            best_at   = String(get(r, :published_at,  ""))
        end
    end
    cur = _parse_ver(current)
    avail = best_ver !== nothing && cur !== nothing && best_ver > cur
    # scope tells the UI whether the user can apply in-app: only "user" installs self-update; a
    # "system" install shows an admin note, a "dev" checkout hides the control entirely.
    # releaseNotes/publishedAt are shown in the What's New modal (WHATS_NEW_PLAN.md) — the older
    # header badge/Settings surfaces ignore them.
    200, JSON3.write((; current, latest = best_tag, updateAvailable = avail, url = best_url,
                        releaseNotes = best_body, publishedAt = best_at,
                        scope = _install_scope()))
end

# POST /api/update/apply {version} — download + stage the target release bundle. The launcher
# applies it on the next restart. Refused in a dev/git checkout.
function api_update_apply(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch; Dict{Symbol,Any}() end
    tag  = String(get(body, :version, ""))

    pre = _apply_precheck(tag; scope = _install_scope(), installed = _is_installed())
    pre === nothing || return pre[1], JSON3.write((; error = pre[2]))

    # `tar` is not guaranteed present (it is on Windows 10+ as bsdtar, but guard rather than emit a
    # cryptic spawn failure) — same check the project export/import path makes before packing.
    Cecelia._tar_available() || return 500, JSON3.write((;
        error = "`tar` was not found on PATH — cannot unpack the update bundle."))

    url     = "https://github.com/$_UPDATE_REPO/releases/download/$tag/cecelia.tar.gz"
    staging = joinpath(_APP_ROOT, ".update-staging")
    job_id  = "update-apply"
    try
        rm(staging; recursive = true, force = true)
        payload = joinpath(staging, "payload"); mkpath(payload)
        tarball = joinpath(staging, "cecelia.tar.gz")
        # NO total `timeout` on purpose. Downloads.jl already aborts after 20s with NO DATA
        # RECEIVED, which is the actual hang we care about; a total cap would instead kill a
        # legitimately slow download of a large bundle on a poor connection. Don't "fix" this.
        Downloads.download(url, tarball)

        # Integrity: check the bundle against the `.sha256` published beside it. HTTPS covers the
        # transport; this covers a TRUNCATED or SWAPPED asset, which transport security says nothing
        # about — and we are about to hand this payload to the launcher to overwrite the app with.
        #
        # VERIFY-IF-PRESENT, deliberately. Releases up to and including v0.1.0-rc9 have no digest
        # asset, so REQUIRING one would make every existing release uninstallable from a client that
        # has this code. Absent digest → proceed (and say so in the message); present digest that
        # does NOT match → refuse. Tighten to mandatory once no supported release predates it.
        digest = try
            String(take!(Downloads.download("$url.sha256", IOBuffer())))
        catch
            ""   # 404 on a pre-digest release, or the fetch failed — not fatal, see above
        end
        verified = false
        if !isempty(strip(digest))
            if !Cecelia._sha256_matches(tarball, digest)
                rm(staging; recursive = true, force = true)
                return 500, JSON3.write((;
                    error = "the downloaded bundle does not match its published SHA-256 — refusing to stage it."))
            end
            verified = true
        end
        # Go through `_run_tar` (app/src/project_io.jl) rather than a bare `run` — it is the one tar
        # runner. A bare `run` skipped `track_job!` (so the extract could not be cancelled) and, more
        # importantly, missed the `termsignal` check: libuv reports `exitcode == 0` for a
        # SIGNAL-KILLED process, so a killed extract read as SUCCESS and we would stage a
        # half-unpacked payload and mark it pending. See CLAUDE.md → Windows compatibility.
        if !Cecelia._run_tar(`tar -xzf $tarball -C $payload`, job_id)
            # Clear the half-unpacked payload BEFORE returning. `.pending-update` is deliberately
            # not written, so the launcher has nothing to apply on the next restart.
            rm(staging; recursive = true, force = true)
            return 500, JSON3.write((;
                error = "unpacking the update bundle failed (tar exited non-zero or was cancelled)."))
        end
        write(joinpath(_APP_ROOT, ".pending-update"), tag)   # marker the launcher looks for
        broadcast_ws(Dict("type" => "update:staged", "version" => tag))
        200, JSON3.write((; staged = tag, verified,
            message = "Update $tag downloaded. Restart Cecelia to finish installing."))
    catch e
        rm(staging; recursive = true, force = true)
        500, JSON3.write((; error = "staging failed: $(sprint(showerror, e))"))
    finally
        # `track_job!` auto-creates the registry entry, so drop it however we leave — otherwise
        # every apply leaks one and a later `cancel_job!("update-apply")` would target dead procs.
        # Staging is NOT removed here: on success the launcher needs the payload it points at.
        finish_job!(job_id)
    end
end
