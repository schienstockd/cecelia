# HPC settings — the split of docs/todo/HPC_PLAN.md → Decision 9 (revised 2026-10-13):
#
#   cluster   `custom.toml` `[hpc]`                       scheduler, host, port — machine-wide: which
#                                                          cluster this machine reaches is not personal
#   personal  `user-profiles/<name>/settings.toml`        everything else: username + SSH key
#             (`hpcUser`, `hpcKeyFile`, `hpc` table)       (`hpcUser`/`hpcKeyFile`), and the remote folder,
#                                                          email and CPU/GPU queue/account/QoS/directives/
#                                                          preamble (`hpc`). A remote folder is usually
#                                                          `/scratch/<user>/…`, and accounts and queues
#                                                          follow a person's group, so two profiles on one
#                                                          workstation must not share them
#   project   `<project>/settings/hpc.json` `account`     billing follows the grant, which is per project
#
# `HpcSite` is the EFFECTIVE view — the shared cluster + this profile's part — so nothing downstream
# needs to know which file a field came from.
#
# Readers and validated writers only. Nothing here spawns a process: that is `hpc/transport.jl`.
# Included from Cecelia.jl after `config/profile_settings.jl` (identity reads the profile bag).

"""
The batch systems the Settings select offers, served to the frontend so the list is not duplicated
in Vue. `probe` is the binary the connection test looks for; `available = false` is shown as coming
and refused on save (the slot for the next backend). PBS means PBS Pro / OpenPBS (backends/pbs.jl).
"""
const HPC_SCHEDULERS = (
    (; name = "slurm", label = "SLURM", probe = "sbatch", available = true),
    (; name = "pbs",   label = "PBS",   probe = "qstat",  available = true),
)

"""Job classes; the class picks queue, account and QoS (HPC_PLAN D8). Defaults from `resource_pool`."""
const HPC_CLASSES = ("cpu", "gpu")

"""Per-class site settings (`[hpc.cpu]` / `[hpc.gpu]`). Empty = the site's default."""
struct HpcClass
    queue::String
    account::String
    qos::String
    directives::String   # extra scheduler lines, verbatim
    preamble::String     # shell lines run before the task, verbatim
end

"""The machine-wide site settings, `custom.toml` `[hpc]` merged over `app/config.toml`."""
struct HpcSite
    scheduler::String
    host::String
    port::Int
    remote_dir::String
    email::String
    cpu::HpcClass
    gpu::HpcClass
end

"""A profile's cluster login. `key_file` is stored as typed (a leading `~` stays portable)."""
struct HpcIdentity
    user::String
    key_file::String
end

const _HPC_CLASS_KEYS = ("queue", "account", "qos", "directives", "preamble")
const _HPC_SITE_KEYS  = ("scheduler", "host", "port", "remoteDir", "email")
# Which of the site's keys are shared by every profile (custom.toml); the rest are the profile's own.
const _HPC_SHARED_KEYS   = ("scheduler", "host", "port")
const _HPC_PERSONAL_KEYS = ("remoteDir", "email", HPC_CLASSES...)
const _HPC_PROFILE_KEY   = "hpc"
const _HPC_DEFAULT_REMOTE_DIR = "~/cecelia"

"""
    hpc_available() -> Bool

Whether this OS can use the HPC tier. Windows is deferred (HPC_PLAN Q8: no rsync, no ControlMaster),
so Settings says so instead of offering controls that cannot work.
"""
hpc_available()::Bool = !Sys.iswindows()

_hstr(d::AbstractDict, k::String, default::String = "")::String = strip(string(get(d, k, default)))

function _hpc_class(d)::HpcClass
    d isa AbstractDict || (d = Dict{String,Any}())
    # Directives and preamble are multi-line and used verbatim: strip only the outer whitespace.
    HpcClass(_hstr(d, "queue"), _hstr(d, "account"), _hstr(d, "qos"),
             _hstr(d, "directives"), _hstr(d, "preamble"))
end

function _hpc_site(d::AbstractDict)::HpcSite
    port = tryparse(Int, string(get(d, "port", 22)))
    HpcSite(lowercase(_hstr(d, "scheduler", "slurm")), _hstr(d, "host"),
            isnothing(port) ? 22 : port, _hstr(d, "remoteDir", _HPC_DEFAULT_REMOTE_DIR),
            _hstr(d, "email"), _hpc_class(get(d, "cpu", nothing)), _hpc_class(get(d, "gpu", nothing)))
end

"""
    hpc_site(name = active_profile_name()) -> HpcSite

The settings in effect for profile `name`: the shared cluster from `custom.toml` `[hpc]`, with the
profile's own part on top. Never throws: anything missing reads as unconfigured.

A personal key still in `custom.toml` (written there before the split) is read as a fallback for a
profile that has not set its own; the profile's first save moves it out (`set_hpc_site!`).
"""
function hpc_site(name::AbstractString = active_profile_name();
                  config_root::AbstractString = config_dir())::HpcSite
    shared = get(cecelia_conf(), "hpc", Dict{String,Any}())
    shared isa AbstractDict || (shared = Dict{String,Any}())
    mine = _hpc_profile_part(name; config_root)
    merged = Dict{String,Any}(String(k) => v for (k, v) in shared)
    merge!(merged, mine)
    _hpc_site(merged)
end

function _hpc_profile_part(name::AbstractString; config_root::AbstractString = config_dir())::Dict{String,Any}
    bag = read_profile_settings(name; config_root = config_root)
    d = get(bag, _HPC_PROFILE_KEY, nothing)
    d isa AbstractDict ? Dict{String,Any}(String(k) => v for (k, v) in d if String(k) in _HPC_PERSONAL_KEYS) :
                         Dict{String,Any}()
end

hpc_class_dict(c::HpcClass)::Dict{String,Any} =
    Dict{String,Any}("queue" => c.queue, "account" => c.account, "qos" => c.qos,
                     "directives" => c.directives, "preamble" => c.preamble)

"""`custom.toml` / JSON shape of a site — the same keys `set_hpc_site!` accepts."""
hpc_site_dict(s::HpcSite)::Dict{String,Any} =
    Dict{String,Any}("scheduler" => s.scheduler, "host" => s.host, "port" => s.port,
                     "remoteDir" => s.remote_dir, "email" => s.email,
                     "cpu" => hpc_class_dict(s.cpu), "gpu" => hpc_class_dict(s.gpu))

# ── Validation ─────────────────────────────────────────────────────────────────────────────────────
#
# Host, user, queue, account and QoS end up as ssh arguments or scheduler flags. A value with
# whitespace is a typo, and one starting with `-` would be read by ssh as an OPTION
# (`-oProxyCommand=…` runs a local command). Refused at save, so nothing downstream has to re-check.

function _hpc_token!(errs::Vector{String}, what::String, v::AbstractString; required::Bool = false)
    if isempty(v)
        required && push!(errs, "$what is required")
    elseif startswith(v, "-") || any(c -> isspace(c) || iscntrl(c), v)
        push!(errs, "$what must be one word, not starting with '-'")
    end
    errs
end

"""
    hpc_site_errors(site) -> Vector{String}

Every problem with `site`, as brief messages for the Settings page; empty when valid. An empty host
is valid (the site is simply not configured yet).
"""
function hpc_site_errors(s::HpcSite)::Vector{String}
    errs = String[]
    sched = findfirst(x -> x.name == s.scheduler, HPC_SCHEDULERS)
    if isnothing(sched)
        push!(errs, "Unknown scheduler '$(s.scheduler)'")
    elseif !HPC_SCHEDULERS[sched].available
        push!(errs, "$(HPC_SCHEDULERS[sched].label) support is coming; use SLURM")
    end
    _hpc_token!(errs, "Host", s.host)
    1 <= s.port <= 65535 || push!(errs, "Port must be 1–65535")
    isempty(s.remote_dir) && push!(errs, "Remote folder is required")
    any(c -> c in ('\n', '\r', '\0'), s.remote_dir) && push!(errs, "Remote folder must be one line")
    _hpc_token!(errs, "Email", s.email)
    for (cls, c) in (("CPU", s.cpu), ("GPU", s.gpu))
        _hpc_token!(errs, "$cls queue", c.queue)
        _hpc_token!(errs, "$cls account", c.account)
        _hpc_token!(errs, "$cls QoS", c.qos)
    end
    errs
end

"""
    set_hpc_site!(patch; name = active_profile_name()) -> HpcSite

Merge `patch` (any subset of `scheduler, host, port, remoteDir, email, cpu{…}, gpu{…}`) into the
settings in effect, validate the RESULT, then write each part where it lives: scheduler/host/port to
`custom.toml` `[hpc]` (hot-reloaded), the rest to profile `name`. Throws `ArgumentError` naming every
problem; nothing is written then. Unknown keys are refused rather than stored, so a typo cannot
accumulate (the same rule as `POST /api/pools/set`).
"""
function set_hpc_site!(patch::AbstractDict; name::AbstractString = active_profile_name(),
                       config_root::AbstractString = config_dir())::HpcSite
    p = Dict{String,Any}(String(k) => v for (k, v) in patch)
    for k in keys(p)
        (k in _HPC_SITE_KEYS || k in HPC_CLASSES) || throw(ArgumentError("Unknown HPC setting '$k'"))
    end
    merged = hpc_site_dict(hpc_site(name; config_root))
    for (k, v) in p
        if k in HPC_CLASSES
            v isa AbstractDict || throw(ArgumentError("'$k' must be an object"))
            for (ck, cv) in v
                String(ck) in _HPC_CLASS_KEYS || throw(ArgumentError("Unknown HPC setting '$k.$ck'"))
                merged[k][String(ck)] = string(cv)
            end
        elseif k == "port"
            n = v isa Integer ? Int(v) : tryparse(Int, string(v))
            isnothing(n) && throw(ArgumentError("Port must be a number"))
            merged[k] = n
        else
            merged[k] = string(v)
        end
    end
    site = _hpc_site(merged)
    errs = hpc_site_errors(site)
    isempty(errs) || throw(ArgumentError(join(errs, "; ")))
    full = hpc_site_dict(site)
    patch_profile_settings!(Dict{String,Any}(_HPC_PROFILE_KEY =>
        Dict{String,Any}(k => full[k] for k in _HPC_PERSONAL_KEYS)), name; config_root = config_root)
    ensure_config_dir()
    cfg_path = custom_toml_path()
    cfg = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    # Only the shared part; a personal key left from before the split is dropped (now in the profile).
    cfg["hpc"] = Dict{String,Any}(k => full[k] for k in _HPC_SHARED_KEYS)
    write_atomic(io -> TOML.print(io, cfg), cfg_path)
    init_cecelia!()
    hpc_site(name; config_root)
end

# ── Identity (per profile) ─────────────────────────────────────────────────────────────────────────

const _HPC_USER_KEY = "hpcUser"
const _HPC_KEY_FILE_KEY = "hpcKeyFile"

"""
    hpc_identity(name = active_profile_name()) -> HpcIdentity

This profile's cluster username and SSH key path; empty strings when unset.
"""
function hpc_identity(name::AbstractString = active_profile_name();
                      config_root::AbstractString = config_dir())::HpcIdentity
    bag = read_profile_settings(name; config_root = config_root)
    HpcIdentity(_hstr(bag, _HPC_USER_KEY), _hstr(bag, _HPC_KEY_FILE_KEY))
end

"""
    hpc_key_file_error(path) -> String

Why `path` cannot be the SSH key, or `""`. Picking the `.pub` half is the common slip, so it is named.
"""
function hpc_key_file_error(path::AbstractString)::String
    isempty(path) && return ""
    p = expand_user(String(path))
    endswith(lowercase(p), ".pub") && return "Select the private key, not the .pub file"
    isfile(p) || return "Key file not found"
    ""
end

"""
    set_hpc_identity!(; user, key_file, name, config_root) -> HpcIdentity

Validate and store this profile's login. Either keyword may be omitted (left unchanged); `""` clears
it. Throws `ArgumentError` with every problem; nothing is written then.
"""
function set_hpc_identity!(; user::Union{Nothing,AbstractString} = nothing,
                           key_file::Union{Nothing,AbstractString} = nothing,
                           name::AbstractString = active_profile_name(),
                           config_root::AbstractString = config_dir())::HpcIdentity
    errs = String[]
    patch = Dict{String,Any}()
    if !isnothing(user)
        u = strip(String(user))
        _hpc_token!(errs, "Username", u)
        occursin('@', u) && push!(errs, "Username only, without @host")
        patch[_HPC_USER_KEY] = isempty(u) ? nothing : u
    end
    if !isnothing(key_file)
        k = strip(String(key_file))
        e = hpc_key_file_error(k)
        isempty(e) || push!(errs, e)
        patch[_HPC_KEY_FILE_KEY] = isempty(k) ? nothing : k
    end
    isempty(errs) || throw(ArgumentError(join(errs, "; ")))
    isempty(patch) || patch_profile_settings!(patch, name; config_root = config_root)
    hpc_identity(name; config_root = config_root)
end

# ── Project override ───────────────────────────────────────────────────────────────────────────────
#
# `<project>/settings/hpc.json` holds `{"account": "…"}` and nothing else (D9). Project-relative and
# uid-free, so it travels with an exported project (docs/OBJECTMODEL.md → Project identity).

hpc_project_settings_path(root::AbstractString)::String = joinpath(root, "settings", "hpc.json")

"""
    read_hpc_project_settings(root) -> Dict{String,Any}

`{"account" => …}` for the project at `root`; `account` is `""` when there is no override.
"""
function read_hpc_project_settings(root::AbstractString)::Dict{String,Any}
    p = hpc_project_settings_path(root)
    d = isfile(p) ? (try json_native(JSON3.read(read(p, String))) catch; nothing end) : nothing
    d isa AbstractDict || (d = Dict{String,Any}())
    Dict{String,Any}("account" => _hstr(d, "account"))
end

"""
    set_hpc_project_account!(root, account) -> Dict{String,Any}

Store the project's account override; `""` removes it (and the file). Throws `ArgumentError` on a
malformed account.
"""
function set_hpc_project_account!(root::AbstractString, account::AbstractString)::Dict{String,Any}
    a = strip(String(account))
    errs = _hpc_token!(String[], "Account", a)
    isempty(errs) || throw(ArgumentError(join(errs, "; ")))
    p = hpc_project_settings_path(root)
    if isempty(a)
        isfile(p) && rm(p)
    else
        write_json_atomic(p, Dict("account" => a))
    end
    read_hpc_project_settings(root)
end
