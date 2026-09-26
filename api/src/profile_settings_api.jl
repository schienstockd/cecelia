# ── Per-profile settings API — USER_PROFILE_PLAN Phase 4 ─────────────────────────
#
# The frontend hydrates its per-profile Pinia store from GET /api/profile/settings on launch
# and coalesces changes into PATCH /api/profile/settings. The bag is opaque strings/numbers/
# booleans — the frontend owns key names + defaults. See docs/audit/user-profile-field-audit.md
# for what belongs here (~25 keys, working preferences that follow the person; per-machine
# renderer knobs stay in localStorage, per-image/per-set bags stay in the project data).
#
# Server writes go to `<config_dir>/user-profiles/<name>/settings.toml`. See
# `app/src/config/profile_settings.jl` for the reader/writer.
#
# Routes (see server.jl for wiring):
#
#   GET  /api/profile/settings          → { profile, settings }
#   POST /api/profile/settings/patch    → { profile, settings }  (body: partial dict)
#
# Route path is `/api/profile/settings` (not `/api/kiwi/...`) — this endpoint is not
# Kiwi-scoped, and it's new so no URL contract to preserve. POST for the write to match
# every other state-changing endpoint in this codebase (no PATCH verb wired in the router).

import JSON3

function api_profile_settings_get(::HTTP.Request)
    profile = active_profile_name()
    body = Dict{String,Any}("profile"  => profile,
                            "settings" => read_profile_settings(profile))
    200, JSON3.write(body)
end

function api_profile_settings_patch(body_bytes::Vector{UInt8})
    parsed = try; JSON3.read(String(body_bytes)); catch; nothing; end
    parsed isa AbstractDict ||
        return 400, JSON3.write((; ok = false, error = "Body must be a JSON object."))
    # JSON3 objects come back with Symbol keys; the writer normalises to Strings.
    patch = Dict{String,Any}(String(k) => v for (k, v) in parsed)
    profile = active_profile_name()
    merged  = patch_profile_settings!(patch, profile)
    body = Dict{String,Any}("ok"       => true,
                            "profile"  => profile,
                            "settings" => merged)
    200, JSON3.write(body)
end
