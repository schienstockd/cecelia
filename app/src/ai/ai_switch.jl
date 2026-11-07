# ── The AI switch — is model-backed work allowed for this profile? ───────────────────────────────
#
# Two layers, the stricter wins (docs/todo/AI_TOGGLE_PLAN.md D2/D3):
#   - the workstation ceiling: `[sharedWorkstation] aiAllowed` in custom.toml, set by peanut
#     (`default`) in Settings → Shared workstation. A visibility gate like the rest of that section,
#     not security — the switch is a policy for the people using the box (plan D1).
#   - the profile's own preference: `aiEnabled` in `user-profiles/<name>/settings.toml`, set in
#     Preferences.
#
# Enforced server-side at three points (AI_SURFACES_AUDIT.md §8.7): `kiwi_start_turn` (every in-app
# model turn), the stream handler (every request marked `X-Cecelia-Client: claude`, i.e. the MCP
# observer and autonomous servers), and the routes that hand out a model path. Hiding buttons is the
# frontend's job and never the enforcement.

"""Which layer turned AI off — `ai_on` when nothing did."""
@enum AIOffBy ai_on ai_off_workstation ai_off_profile

"""The profile preference's default: OFF — AI is opt-in, turned on through the dialog that says what
is sent (AI_TOGGLE_PLAN P3, audit §8.3). A Ref only so a test suite written before the switch can run
its AI paths without opting every profile in; nothing in the app writes it."""
const AI_PROFILE_DEFAULT = Ref(false)

"""What every refusal says. The MCP surfaces it verbatim, so a Claude Code session reads it as-is."""
const AI_OFF_MESSAGE = "AI is off in Cecelia"

"""
    ai_off_by(name = active_profile_name()) -> AIOffBy

The effective state for profile `name`: the workstation ceiling first, then the profile's own
`aiEnabled` (a non-Bool value — a hand-edit typo — reads as the default, like `shared_workstation`).
"""
function ai_off_by(name::AbstractString = active_profile_name())::AIOffBy
    shared_workstation("aiAllowed") || return ai_off_workstation
    v = get(read_profile_settings(name), "aiEnabled", AI_PROFILE_DEFAULT[])
    (v isa Bool ? v : AI_PROFILE_DEFAULT[]) ? ai_on : ai_off_profile
end

"""`ai_enabled(name)` — true when neither layer turned AI off."""
ai_enabled(name::AbstractString = active_profile_name())::Bool = ai_off_by(name) == ai_on

const _AI_SET_BY = Dict(ai_off_workstation => "workstation", ai_off_profile => "profile")

"""
    ai_state(name = active_profile_name()) -> Dict{String,Any}

`{enabled, setBy}` for the wire (`/api/observer/status` and every refusal). `setBy` is
`"workstation"`, `"profile"`, or `nothing` when AI is on.
"""
function ai_state(name::AbstractString = active_profile_name())::Dict{String,Any}
    by = ai_off_by(name)
    Dict{String,Any}("enabled" => by == ai_on, "setBy" => get(_AI_SET_BY, by, nothing))
end

"""`ai_refusal(name)` — the `{error, setBy}` body every refusal sends, or `nothing` when AI is on."""
function ai_refusal(name::AbstractString = active_profile_name())::Union{Nothing,Dict{String,Any}}
    st = ai_state(name)
    st["enabled"] ? nothing : Dict{String,Any}("error" => AI_OFF_MESSAGE, "setBy" => st["setBy"])
end
