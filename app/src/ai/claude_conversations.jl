# ── Deleting a profile's Claude conversations — shared-workstation hygiene ───────────────────────────
#
# A named profile's Claude config dir (`<config_dir>/user-profiles/<name>/`, `CLAUDE_CONFIG_DIR` for
# every Kiwi turn and the profile's terminal `claude`) collects that person's conversations: full
# transcripts, every prompt they typed, pasted text, snapshots of files Claude edited. On a box where
# several people share one OS login, those stay readable by the next person until the CLI's own
# age-based cleanup (`cleanupPeriodDays`, 30 days by default) removes them.
#
# Wanted when the workstation policy (`[sharedWorkstation].purgeClaudeConversations`) OR the
# profile's own preference (`purgeClaudeConversations` in its settings.toml) is on — the policy is
# the floor, a person can opt in on top. Hygiene, not access control: same OS login, same files.
#
# What goes is an ALLOWLIST of the CLI's conversation stores, never "everything but": the same dir
# also holds Cecelia's own `settings.toml` / `recent-projects.toml`, the login, the plugin + MCP
# registration (`.claude.json`, `plugins/`), and Claude's per-project memory (`projects/*/memory/`),
# which the person curated deliberately and keeps.
#
# Never `default` (peanut = `~/.claude`, every terminal `claude` on the OS account — plan D21), and
# never while a `claude` process is running under the profile: deleting a live transcript breaks
# that session. The CLI registers each running session as `sessions/<pid>.json`; a live pid there
# means in use. A reused pid can only make us SKIP a purge (the next trigger retries), never delete
# under a live session.

# Top-level entries under a Claude config dir that hold conversation content (CLI 2.1.312, measured
# on Linux). `projects/` is handled separately so `memory/` survives.
const CLAUDE_CONVERSATION_ENTRIES = (
    "history.jsonl",     # every prompt typed, across sessions
    "paste-cache",       # pasted text
    "file-history",      # pre-edit snapshots of files Claude changed
    "session-env",       # per-session environment captures
    "shell-snapshots",   # shell state captured per session
    "todos",             # per-session task lists (older CLIs)
    "tasks",             # per-session task lists
    "plans",             # plan-mode drafts
    "debug",             # debug logs — carry prompts and tool output
)

# Inside each `projects/<encoded-cwd>/`: everything but these.
const CLAUDE_PROJECT_KEEP = ("memory",)

"""
    claude_profile_in_use(profile_dir) -> Bool

Is a `claude` process running under this config dir right now? True when any `sessions/<pid>.json`
names a live pid (`_pid_alive`). False for a dir with no `sessions/`.
"""
function claude_profile_in_use(profile_dir::AbstractString)::Bool
    sdir = joinpath(profile_dir, "sessions")
    isdir(sdir) || return false
    for f in readdir(sdir)
        endswith(f, ".json") || continue
        pid = tryparse(Int, first(splitext(f)))
        !isnothing(pid) && _pid_alive(pid) && return true
    end
    false
end

"""
    purge_claude_conversations!(profile_dir) -> (ok, message)

Delete the conversation stores (`CLAUDE_CONVERSATION_ENTRIES` + every `projects/*/` entry except
`memory/`) under a named profile's Claude config dir. Refuses `""` (peanut, D21) and a profile with a
running `claude` (`claude_profile_in_use`). Idempotent; a missing entry is fine. `ok = false` with a
message on refusal or on the first entry that could not be removed (the rest are still attempted).
"""
function purge_claude_conversations!(profile_dir::AbstractString)::Tuple{Bool,String}
    isempty(profile_dir) && return (false, CLAUDE_DEFAULT_PURGE_REFUSED)
    isdir(profile_dir) || return (true, "")
    claude_profile_in_use(profile_dir) && return (false, CLAUDE_PURGE_IN_USE)
    targets = String[joinpath(profile_dir, e) for e in CLAUDE_CONVERSATION_ENTRIES]
    pdir = joinpath(profile_dir, "projects")
    if isdir(pdir)
        for proj in readdir(pdir; join = true)
            isdir(proj) || (push!(targets, proj); continue)
            for e in readdir(proj)
                e in CLAUDE_PROJECT_KEEP || push!(targets, joinpath(proj, e))
            end
        end
    end
    failed = ""
    for t in targets
        ispath(t) || continue
        try
            rm(t; recursive = true, force = true)
        catch e
            isempty(failed) && (failed = "Could not delete $(basename(t)): $(sprint(showerror, e))")
        end
    end
    (isempty(failed), failed)
end

const CLAUDE_DEFAULT_PURGE_REFUSED =
    "Peanut's conversations are terminal claude's too — Cecelia does not delete them."
const CLAUDE_PURGE_IN_USE = "Claude is still running for this profile — close it, then log out again."

"""
    purge_claude_conversations_wanted(name) -> Bool

Should profile `name`'s conversations be deleted when its session ends? The workstation policy OR
the profile's own `purgeClaudeConversations` preference. Always false for `default` (D21).
"""
function purge_claude_conversations_wanted(name::AbstractString)::Bool
    name == "default" && return false
    shared_workstation("purgeClaudeConversations") && return true
    get(read_profile_settings(name), "purgeClaudeConversations", false) === true
end
