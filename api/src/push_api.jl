# ── Bidirectional context — Part 5 (push delivery, PR #1: pairing infra) ──────
# `docs/todo/BIDIR_PUSH_PLAN.md` PR #1. This file holds ONLY the pairing record — no socket
# writer yet. The writer lands in PR #2 alongside the fallback wiring.
#
# A pairing record ties a Cecelia project to a Claude Code session's inbox socket + auth token,
# so PR #2's Julia writer can POST a plain-text capture notification directly to the paired
# session over its Unix socket (macOS/Linux) or named pipe (native Windows). Same-machine only.
#
# Storage. Per-project `<proj>/settings/push_target.json`, one file per project, following the
# shape of `settings/notebooks.json` / `settings/captures.json`. Overwrites on re-pair; the plan
# specifies auto-refresh via MCP middleware (Decision 1 amendment), so most re-pairs are silent.
#
# What the frontend sees vs what the Julia writer sees. The GET response NEVER includes the
# token — it's not needed for the "am I paired?" chip and it doesn't belong in a browser
# response. The writer (PR #2) reads the file directly on disk.
#
# MCP surface (`mcp/cecelia_mcp/server.py`): `register_push_target(project_uid, session_label?)`
# for explicit re-pair. The MCP client also calls this route implicitly through middleware on
# any tool with a `project_uid`, so a fresh Claude Code session auto-pairs on its first tool
# call without the user typing anything (Decision 1 amendment).
using Dates

const _PUSH_TARGET_FILE = "push_target.json"

_push_target_path(project_uid::AbstractString) =
    joinpath(_settings_dir_for_project(String(project_uid)), _PUSH_TARGET_FILE)

# The stored record. `socketPath` + `token` are the delivery credentials; `sessionLabel` is a
# human label the user might set with `claude --name foo`; `pairedFromPid` is captured so a
# later debug session can trace which Claude PID authored the pairing (see Decision 13). Not
# used for authentication — the socket + token are the only auth.
const _PUSH_TARGET_STR_KEYS = ("socketPath", "token", "sessionLabel", "pairedAt", "pairedFromPid")

# GET response leaks nothing sensitive — the token stays on disk. Frontend gets what's needed
# for a "paired ✓ / not paired" chip: paired-or-not, when, which session label (if any), and
# the (non-secret) socket path so an explicit debug tooltip can show it if we want.
function _push_target_public(record::AbstractDict)::Dict{String,Any}
    Dict{String,Any}(
        "paired"        => true,
        "socketPath"    => String(get(record, "socketPath", "")),
        "sessionLabel"  => String(get(record, "sessionLabel", "")),
        "pairedAt"      => String(get(record, "pairedAt", "")),
        "pairedFromPid" => String(get(record, "pairedFromPid", "")),
    )
end

"""
    GET /api/push/target?projectUid=…

Reply: `{paired: bool, socketPath?, sessionLabel?, pairedAt?, pairedFromPid?}`. The token is
NEVER returned — it's a delivery credential that lives only on disk and in the Julia writer's
memory (PR #2).
"""
function api_push_target_get(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    uid = get(query, "projectUid", "")
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))

    path = _push_target_path(uid)
    isfile(path) || return 200, JSON3.write((; paired = false))
    record = try
        JSON3.read(read(path, String), Dict{String,Any})
    catch e
        # An unreadable record is treated as unpaired (self-heal via the next auto-pair) rather
        # than a 500 — a corrupt sidecar shouldn't break the chip render, and it'll be
        # overwritten the next time an MCP tool auto-pairs.
        @warn "Unreadable push_target.json; treating as unpaired" path = path exception = e
        return 200, JSON3.write((; paired = false))
    end
    200, JSON3.write(_push_target_public(record))
end

"""
    POST /api/push/target

Body: `{projectUid, socketPath, token, sessionLabel?, pairedFromPid?}`. Writes / overwrites
`<proj>/settings/push_target.json`. Additive-only in the "same shape as its neighbours" sense —
never touches project data, images, or analysis output; only its own sidecar.

Called from the MCP client's auto-pair middleware on every fresh (project, socket, token) tuple
(so the user never types a pairing command) and from the explicit `register_push_target` MCP
tool.
"""
function api_push_target_post(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid = _wstr(body, :projectUid)
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))

    socket_path = _wstr(body, :socketPath)
    token       = _wstr(body, :token)
    # A pairing without one of these is meaningless — the writer needs both. Reject rather than
    # store half a record; the auto-pair middleware only sends complete tuples.
    isempty(socket_path) && return 400, JSON3.write((; error = "socketPath required"))
    isempty(token)       && return 400, JSON3.write((; error = "token required"))

    record = Dict{String,Any}(
        "socketPath"    => socket_path,
        "token"         => token,
        "sessionLabel"  => _wstr(body, :sessionLabel),
        "pairedAt"      => string(Dates.now()),
        "pairedFromPid" => _wstr(body, :pairedFromPid),
    )
    mkpath(dirname(_push_target_path(uid)))
    write_json_atomic(_push_target_path(uid), record)
    200, JSON3.write((; ok = true))
end
