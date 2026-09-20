# ── Bidirectional context — Part 5 (push delivery, PR #2: writer + fallback) ──
# `docs/todo/BIDIR_PUSH_PLAN.md` PR #2. Sends a plain-text notification directly to the paired
# Claude Code session's inbox socket when the user shares a viewer capture. Fires from
# `captures_api.jl::api_viewer_capture` after the capture is on disk. Every failure is a
# silent `:fallback` — the frontend keeps the existing `#1040` clipboard/toast path in that
# case, so a broken push never blocks a share.
#
# Wire protocol (verified against Claude Code v2.1.278 by extracting the runtime example
# embedded in the binary's `[uds-messaging]` help string):
#
#     {"type":"auth","token":"<token>"}\n
#     {"type":"user","message":{"role":"user","content":"<content>"}}\n
#
# Two newline-terminated JSON lines, close the connection. Line 1 authenticates; line 2 is the
# message. `message.content` is a string; `.role` is `"user"`. The receiving Claude reads it
# between tool calls (or starts a new turn if idle) — for a normal prompting-mode receiver the
# message is DELIVERED without approval (per the cross-session-messaging docs' inbound-default
# rules). Bypass-mode receivers hold for approval; either way the sender's `write()` returns
# fine and Cecelia treats it as `:sent`.
#
# Native Windows: the socket becomes a named pipe (`\\.\pipe\<name>`); Julia's
# `Sockets.connect(pipe_name)` handles both transports transparently. Not verified on a Windows
# box in this PR — `PUSH_PLAN.md` Open Item 2. If a Windows user hits it and it doesn't work,
# the fallback keeps the share flow going while the branch gets a real test.
using Sockets
using JSON3

# ── Message wording ──────────────────────────────────────────────────────────
# One line, addresses the capture by (project, id) and tells Claude which MCP tool to call.
# Locked here as the ONE format so a chat-side grep for `"[cecelia] shared capture"` finds
# every incident, past and future. The `content` string travels as-is inside the JSON message
# frame — the socket writer wraps it, the string itself is human-readable prose.
function format_capture_message(project_uid::AbstractString, capture_id::AbstractString,
                                address, notes::AbstractString = "")::String
    surface  = address isa AbstractDict ? String(get(address, "surface",   "")) : ""
    image    = address isa AbstractDict ? String(get(address, "imageUid",  "")) : ""
    t_val    = address isa AbstractDict ? get(address, "t", nothing) : nothing
    z_val    = address isa AbstractDict ? get(address, "z", nothing) : nothing
    # Compact parenthetical carrying whatever coords the capture actually names; each part is
    # skipped when empty so a UI-surface capture (no image / t / z) reads cleanly.
    parts = String[]
    !isempty(surface) && push!(parts, surface)
    !isempty(image)   && push!(parts, string("image ", image))
    t_val === nothing || push!(parts, string("t=", t_val))
    z_val === nothing || push!(parts, string("z=", z_val))
    coords = isempty(parts) ? "" : string(" (", join(parts, ", "), ")")
    # Free-text notes the user typed on DrawSurface (BIDIR follow-up 2026-09-20). Included INLINE
    # in the push text so the receiving Claude reads them immediately — the whole point of the
    # notes field is that it travels alongside the pixels, not that it's discoverable via a
    # follow-up `get_capture` call. Full envelope is still available via that call for the
    # overlay / landscape / view-state bits the notes don't cover.
    notes_clean = strip(String(notes))
    notes_line = isempty(notes_clean) ? "" : string("\nUser said: ", notes_clean, "\n")
    string("[cecelia] shared capture ", capture_id, " from project ", project_uid, coords,
           ".", notes_line,
           " Read it with get_capture(\"", project_uid, "\", \"", capture_id, "\").")
end

# ── The writer ───────────────────────────────────────────────────────────────
# Reads `push_target.json`, connects to the socket, sends auth line + message line, closes.
# Any exception ⇒ `:fallback` (also clears a stale record so the next share doesn't re-attempt
# a dead socket — the frontend then shows "not paired" until the user's next MCP tool call
# auto-repairs). No retry, no queue — a missed push is not a crash-severity event; the
# clipboard/toast is right there.
#
# Return value: `(:sent | :fallback | :not_paired, message_or_nothing)`. The message string
# is returned so the caller can log/log-tag it consistently; the frontend only sees the
# outcome symbol via the JSON response.
function push_capture_notification(project_uid::AbstractString, capture_id::AbstractString,
                                   address, notes::AbstractString = ""
                                   )::Tuple{Symbol,Union{String,Nothing}}
    record = _read_push_target(project_uid)
    record === nothing && return (:not_paired, nothing)
    socket_path = String(get(record, "socketPath", ""))
    token       = String(get(record, "token", ""))
    if isempty(socket_path) || isempty(token)
        return (:not_paired, nothing)
    end
    content = format_capture_message(project_uid, capture_id, address, notes)
    ok = try
        _write_uds_message(socket_path, token, content)
    catch e
        @warn "push_capture_notification: socket write failed, clearing stale target" project = project_uid path = socket_path exception = e
        false
    end
    if !ok
        _clear_push_target_silent(project_uid)
        # BIDIR Part 5 PR #3: broadcast so the chip flips from "paired ✓" → "not paired"
        # without waiting for the next Share click. Same shape as the pair-write broadcast
        # (push_api.jl), just with `paired: false`.
        broadcast_ws(Dict{String,Any}(
            "type" => "push_target:changed", "projectUid" => String(project_uid),
            "paired" => false,
        ))
        return (:fallback, content)
    end
    # Push landed. Announce it so the frontend chip can show a transient "sent ✓" state.
    # Include capture_id so a viewer showing that capture can react specifically. No user
    # content — this is a delivery signal, not a data channel.
    broadcast_ws(Dict{String,Any}(
        "type" => "push:sent", "projectUid" => String(project_uid),
        "captureId" => String(capture_id),
    ))
    (:sent, content)
end

# Read the paired record straight off disk. Nothing (return `nothing`) if unpaired or unreadable
# — the caller distinguishes not-paired from an active-failure so the log says the right thing.
function _read_push_target(project_uid::AbstractString)::Union{Dict{String,Any},Nothing}
    path = _push_target_path(project_uid)
    isfile(path) || return nothing
    try
        JSON3.read(read(path, String), Dict{String,Any})
    catch e
        @warn "_read_push_target: unreadable record" path = path exception = e
        nothing
    end
end

# Delete a stale record so future GETs read "not paired" and the frontend chip flips. Silent
# on any error — the record is best-effort state, and the fallback path is what matters.
function _clear_push_target_silent(project_uid::AbstractString)::Nothing
    path = _push_target_path(project_uid)
    isfile(path) || return nothing
    try
        rm(path; force = true)
    catch e
        @warn "_clear_push_target_silent: rm failed (harmless)" path = path exception = e
    end
    nothing
end

# ── Socket write ─────────────────────────────────────────────────────────────
# Two lines. Close the connection to signal "message complete" (Claude Code's line reader
# hands off between tool calls; a lingering connection would eventually hit its own 30 s
# silent-connection timeout on the server side). Newline is required inside each line
# (per docs: "the first line of its connection" implies line-delimited framing throughout).
#
# `write` returns the byte count; a partial write on a Unix socket is unusual for < 1 KB
# payloads but we check `bytes == length(payload)` anyway so a truncated write reads as
# `false` (⇒ `:fallback`) instead of silent success.
function _write_uds_message(socket_path::AbstractString, token::AbstractString,
                            content::AbstractString)::Bool
    auth = string(JSON3.write(Dict("type" => "auth", "token" => String(token))), "\n")
    msg  = string(JSON3.write(Dict(
        "type"    => "user",
        "message" => Dict("role" => "user", "content" => String(content)),
    )), "\n")
    payload = codeunits(string(auth, msg))
    sock = Sockets.connect(socket_path)
    try
        written = write(sock, payload)
        # `write` returns Int on success; anything short is a partial write.
        return written == length(payload)
    finally
        close(sock)
    end
end
