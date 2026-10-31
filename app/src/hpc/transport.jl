# HPC transport — the ONE place the HPC tier spawns ssh (and, from P3, rsync). docs/todo/HPC_PLAN.md
# → Decision 4: everything goes over SSH with the user's key, nothing long-lived on the login node,
# no passwords stored. Binaries come from `ssh_bin()`/`rsync_bin()` (config/binaries.jl), never a
# bare name; the `ssh/rsync spawn ratchet` in app/test/suite/ratchets.jl holds that.
#
# Two layers: the Settings connection test (P1), and the connection every later call goes through
# (`HpcConn`: run a shell script, rsync a tree). Host keys (HPC_PLAN Q10): the test is the one call
# that may trust a NEW host key (`accept-new`), and it reports the fingerprint so the user can compare
# it with what their site publishes. Every other call is `BatchMode=yes` + `StrictHostKeyChecking=yes`.

const HPC_CONNECT_TIMEOUT_S = 10
const HPC_TEST_TIMEOUT_S = 30

"""
    hpc_known_hosts_path(; config_root) -> String

Cecelia's own known-hosts file. Listed FIRST in `UserKnownHostsFile`, so a key trusted by the
connection test is written here and never into the user's `~/.ssh/known_hosts`, which is still read
second (a host the user already trusts is not re-asked).
"""
hpc_known_hosts_path(; config_root::AbstractString = config_dir())::String =
    joinpath(config_root, "hpc", "known_hosts")

_ssh_opt_quote(p::AbstractString)::String = "\"" * replace(String(p), "\"" => "\\\"") * "\""

"""
    hpc_ssh_args(site, ident; accept_new = false, known_hosts, verbose = false) -> Vector{String}

The ssh arguments for `ident@site.host` (everything except the binary and the remote command).
PURE, so the security-relevant flags are unit-tested: `BatchMode` always (never a prompt),
`IdentitiesOnly` (only THIS profile's key, never another identity the agent happens to hold) and
strict host-key checking unless `accept_new`.
"""
function hpc_ssh_args(site::HpcSite, ident::HpcIdentity; accept_new::Bool = false,
                      known_hosts::AbstractVector{<:AbstractString} =
                          [hpc_known_hosts_path(), "~/.ssh/known_hosts"],
                      verbose::Bool = false, control_path::AbstractString = "")::Vector{String}
    push!(hpc_ssh_opts(site, ident; accept_new, known_hosts, verbose, control_path), site.host)
end

"""
    hpc_ssh_opts(site, ident; ...) -> Vector{String}

`hpc_ssh_args` without the host: the form rsync's `-e` takes (rsync appends the host itself).
`control_path` non-empty reuses a master connection opened by `_hpc_open_master` (P2: one TCP+auth
handshake serves every call of a poll loop); it is never a master itself, so no call can leave a
background ssh holding its output pipes open.
"""
function hpc_ssh_opts(site::HpcSite, ident::HpcIdentity; accept_new::Bool = false,
                      known_hosts::AbstractVector{<:AbstractString} =
                          [hpc_known_hosts_path(), "~/.ssh/known_hosts"],
                      verbose::Bool = false, control_path::AbstractString = "")::Vector{String}
    args = String[]
    verbose && push!(args, "-v")
    append!(args, ["-o", "BatchMode=yes",
                   "-o", "ConnectTimeout=$(HPC_CONNECT_TIMEOUT_S)",
                   "-o", "StrictHostKeyChecking=" * (accept_new ? "accept-new" : "yes"),
                   "-o", "UserKnownHostsFile=" * join(_ssh_opt_quote.(known_hosts), " "),
                   "-o", "IdentitiesOnly=yes",
                   "-i", expand_user(ident.key_file),
                   "-p", string(site.port),
                   "-l", ident.user])
    isempty(control_path) || append!(args, ["-o", "ControlMaster=no", "-o", "ControlPath=" * control_path])
    args
end

# POSIX single-quote, for a value spliced into the probe script.
_sh_quote(s::AbstractString)::String = "'" * replace(String(s), "'" => "'\\''") * "'"

# A remote path as a shell word. `~` means the REMOTE home, so it becomes `"$HOME"` — quoting the
# path as a whole would stop the remote shell expanding it.
function _remote_path_word(p::AbstractString)::String
    p == "~" && return "\"\$HOME\""
    startswith(p, "~/") && return "\"\$HOME\"/" * _sh_quote(p[3:end])
    _sh_quote(p)
end

const _PROBE_TAG = "CECELIA-HPC"

"""
    hpc_probe_script(site) -> String

The `sh` script the connection test sends on stdin: is the remote folder writable, and is the
scheduler's submit binary on `PATH`. Sent to `sh -s` rather than as an ssh command line, so it runs
the same under a csh/tcsh login shell. Sites that put the scheduler behind `module load` get one
retry after sourcing `/etc/profile`, which is where Lmod/Environment Modules initialise.
"""
function hpc_probe_script(site::HpcSite)::String
    probe = HPC_SCHEDULERS[something(findfirst(x -> x.name == site.scheduler, HPC_SCHEDULERS), 1)].probe
    t = _PROBE_TAG
    """
    echo "$t ok"
    d=$(_remote_path_word(site.remote_dir))
    if mkdir -p "\$d" 2>/dev/null && : > "\$d/.cecelia-probe.\$\$" 2>/dev/null; then
      rm -f "\$d/.cecelia-probe.\$\$"; echo "$t dir ok \$d"
    else
      echo "$t dir fail \$d"
    fi
    command -v $probe >/dev/null 2>&1 || . /etc/profile >/dev/null 2>&1
    if command -v $probe >/dev/null 2>&1; then
      echo "$t sched ok \$($probe --version 2>&1 | head -n 1)"
    else
      echo "$t sched missing"
    fi
    """
end

"""Outcome of a connection test. `error` is one brief line for the Settings page, `""` when ok."""
struct HpcTestResult
    ok::Bool
    reachable::Bool
    fingerprint::String     # "ED25519 SHA256:…", "" if the handshake never got that far
    new_host_key::Bool      # this test trusted the key (it was not known before)
    remote_dir::String      # as the remote shell resolved it
    dir_ok::Bool
    scheduler::String       # first line of `<probe> --version`, "" when missing
    error::String
end

hpc_test_dict(r::HpcTestResult)::Dict{String,Any} =
    Dict{String,Any}("ok" => r.ok, "reachable" => r.reachable, "fingerprint" => r.fingerprint,
                     "newHostKey" => r.new_host_key, "remoteDir" => r.remote_dir,
                     "dirOk" => r.dir_ok, "scheduler" => r.scheduler, "error" => r.error)

# ssh failure → one brief line. Ordered: the first match wins, most specific first.
const _SSH_ERRORS = (
    ("REMOTE HOST IDENTIFICATION HAS CHANGED", "Host key changed. Check with your site before trusting it"),
    ("Host key verification failed",           "Host key not trusted. Run Test connection in Settings"),
    ("UNPROTECTED PRIVATE KEY FILE",           "Key file is readable by others. Run chmod 600 on it"),
    ("Could not resolve hostname",             "Host not found"),
    ("Connection refused",                     "Connection refused. Check host and port"),
    ("Connection reset",                       "Connection dropped. Check the port (SSH is usually 22)"),
    ("timed out",                              "No response from host"),
    ("No route to host",                       "No response from host"),
    ("Network is unreachable",                 "No response from host"),
    ("Permission denied",                      "Key rejected. A passphrase key must be loaded in ssh-agent"),
    ("No such file",                           "Key file not readable"),
)

function _ssh_error_line(stderr::AbstractString)::String
    for (needle, msg) in _SSH_ERRORS
        occursin(needle, stderr) && return msg
    end
    # Fall back to ssh's own last word, minus `-v` debug noise.
    lines = filter(l -> !isempty(strip(l)) && !startswith(l, "debug") && !startswith(l, "OpenSSH"),
                   split(stderr, '\n'))
    isempty(lines) ? "SSH failed" : String(strip(last(lines)))
end

"""
    parse_hpc_test(code, stdout, stderr; timed_out = false, scheduler_label = "SLURM") -> HpcTestResult

Turn one probe run into a result. PURE — this is where every brief error message comes from, so it
is tested on canned ssh output without a network.
"""
function parse_hpc_test(code::Integer, out::AbstractString, err::AbstractString;
                        timed_out::Bool = false, scheduler_label::AbstractString = "SLURM")::HpcTestResult
    m  = match(r"Server host key: (\S+) (SHA256:\S+)", err)
    fp = isnothing(m) ? "" : uppercase(replace(m.captures[1], r"^ssh-" => "")) * " " * m.captures[2]
    fp = replace(fp, r"^ECDSA-SHA2-NISTP\d+" => "ECDSA")
    new_key = occursin("Permanently added", err)
    tagged = Dict{String,String}()
    for l in split(out, '\n')
        mm = match(Regex("^$(_PROBE_TAG) (ok|dir|sched)\\b ?(.*)\$"), strip(l))
        isnothing(mm) || (tagged[mm.captures[1]] = String(mm.captures[2]))
    end
    reachable = haskey(tagged, "ok")
    if !reachable
        e = timed_out ? "No reply within $(HPC_TEST_TIMEOUT_S) s" :
            code == 0  ? "Unexpected reply from host" : _ssh_error_line(err)
        return HpcTestResult(false, false, fp, new_key, "", false, "", e)
    end
    dir = get(tagged, "dir", "fail ")
    dir_ok = startswith(dir, "ok")
    remote_dir = strip(replace(dir, r"^(ok|fail) ?" => ""))
    sched = get(tagged, "sched", "missing")
    sched_ok = startswith(sched, "ok")
    sched_ver = sched_ok ? String(strip(sched[3:end])) : ""
    e = !dir_ok   ? "Remote folder not writable" :
        !sched_ok ? "$(scheduler_label) not found on the login node" : ""
    HpcTestResult(dir_ok && sched_ok, true, fp, new_key, remote_dir, dir_ok, sched_ver, e)
end

# Run `cmd` with `input` on stdin, capturing stdout and stderr separately; kill the tree after
# `timeout_s`. Returns `(code, out, err, timed_out)`.
function _hpc_capture(cmd::Cmd, input::AbstractString; timeout_s::Real)
    out, err = Pipe(), Pipe()
    proc = run(pipeline(cmd; stdin = IOBuffer(String(input)), stdout = out, stderr = err); wait = false)
    close(out.in); close(err.in)
    rd_out = @async read(out, String)
    rd_err = @async read(err, String)
    timed_out = Ref(false)
    timer = Timer(timeout_s) do _
        process_running(proc) && (timed_out[] = true; _kill_proc_tree(proc))
    end
    wait(proc)
    close(timer)
    # libuv reports exitcode 0 for a signal-killed child, so a kill reads as -1, never as success.
    code = proc.termsignal == 0 ? Int(proc.exitcode) : -1
    (; code = code, out = fetch(rd_out), err = fetch(rd_err), timed_out = timed_out[])
end

"""
    hpc_test_connection(site = hpc_site(), ident = hpc_identity(); known_hosts) -> HpcTestResult

The Settings "Test connection": one ssh call that may trust a new host key (and says so), then
checks the remote folder is writable and the scheduler is installed. Never throws; every failure is
a result with a brief `error`.
"""
function hpc_test_connection(site::HpcSite = hpc_site(), ident::HpcIdentity = hpc_identity();
                             known_hosts::AbstractVector{<:AbstractString} =
                                 [hpc_known_hosts_path(), "~/.ssh/known_hosts"])::HpcTestResult
    fail(e) = HpcTestResult(false, false, "", false, "", false, "", e)
    hpc_available() || return fail("Not available on Windows yet")
    isempty(site.host)     && return fail("Set the cluster host in Settings → HPC")
    isempty(ident.user)    && return fail("Set your username in Preferences → HPC")
    isempty(ident.key_file) && return fail("Select your SSH key in Preferences → HPC")
    errs = hpc_site_errors(site)
    isempty(errs) || return fail(first(errs))
    ke = hpc_key_file_error(ident.key_file)
    isempty(ke) || return fail(ke)
    ssh = ssh_bin()
    isempty(ssh) && return fail("ssh not found on this machine")
    # ssh does not create the directory of a known-hosts file it is asked to write.
    mkpath(dirname(expand_user(first(known_hosts))))
    args = hpc_ssh_args(site, ident; accept_new = true, known_hosts = known_hosts, verbose = true)
    cmd = Cmd([ssh; args; "sh"; "-s"])
    label = HPC_SCHEDULERS[something(findfirst(x -> x.name == site.scheduler, HPC_SCHEDULERS), 1)].label
    r = try
        _hpc_capture(cmd, hpc_probe_script(site); timeout_s = HPC_TEST_TIMEOUT_S)
    catch e
        return fail("Could not run ssh: " * sprint(showerror, e))
    end
    parse_hpc_test(r.code, r.out, r.err; timed_out = r.timed_out, scheduler_label = label)
end


# ── The connection every later call goes through ───────────────────────────────────────────────────
#
# Two kinds, one interface (`hpc_exec`, `hpc_rsync`, `hpc_remote_root`):
#   HpcSshConn    the cluster, over ssh with the profile's key (D4)
#   HpcLocalConn  THIS machine as the "remote": the tests, and `LocalShellBackend` (HPC_PLAN Q7), so
#                 the mirror, the env install and dispatch run end-to-end with no sshd and no cluster

"""A brief, user-facing HPC failure (`msg` is shown as is: Settings, the task log)."""
struct HpcError <: Exception
    msg::String
end
Base.showerror(io::IO, e::HpcError) = print(io, e.msg)

abstract type HpcConn end

struct HpcSshConn <: HpcConn
    site::HpcSite
    ident::HpcIdentity
    known_hosts::Vector{String}
end

struct HpcLocalConn <: HpcConn end

"""
    hpc_conn(site = hpc_site(), ident = hpc_identity(); known_hosts) -> HpcSshConn

The configured cluster connection. Throws `HpcError` naming the first missing or invalid setting, so a
caller never spawns ssh on a half-filled form.
"""
function hpc_conn(site::HpcSite = hpc_site(), ident::HpcIdentity = hpc_identity();
                  known_hosts::AbstractVector{<:AbstractString} =
                      [hpc_known_hosts_path(), "~/.ssh/known_hosts"])::HpcSshConn
    hpc_available() || throw(HpcError("HPC is not available on Windows yet"))
    isempty(site.host)      && throw(HpcError("Set the HPC host in Settings → HPC"))
    isempty(ident.user)     && throw(HpcError("Set your HPC username in Preferences → HPC"))
    isempty(ident.key_file) && throw(HpcError("Select your SSH key in Preferences → HPC"))
    errs = hpc_site_errors(site)
    isempty(errs) || throw(HpcError(first(errs)))
    ke = hpc_key_file_error(ident.key_file)
    isempty(ke) || throw(HpcError(ke))
    isempty(ssh_bin()) && throw(HpcError("ssh not found on this machine"))
    HpcSshConn(site, ident, String.(known_hosts))
end

# Control sockets live beside our known_hosts. A unix socket path is capped at ~104 bytes (macOS), and
# `%C` expands to 40 hex chars, so a long config dir simply goes without multiplexing.
function _hpc_control_path(c::HpcSshConn)::String
    p = joinpath(dirname(expand_user(first(c.known_hosts))), "cm-%C")
    length(p) + 40 <= 100 ? p : ""
end

const _HPC_MASTERS_LOCK = ReentrantLock()

# Open (once per host+user, kept 5 min after the last use) the master the other calls multiplex over.
# A master started from a call whose output we capture would hold that pipe open until it exits, so it
# is started on its own with its output discarded (`-f` backgrounds it once authenticated). Best
# effort: on any failure every call simply opens its own connection.
function _hpc_open_master(c::HpcSshConn)::String
    cp = _hpc_control_path(c)
    isempty(cp) && return ""
    lock(_HPC_MASTERS_LOCK) do
        mkpath(dirname(cp))
        base = hpc_ssh_opts(c.site, c.ident; known_hosts = c.known_hosts)
        check = Cmd([ssh_bin(); base; "-o"; "ControlPath=" * cp; "-O"; "check"; c.site.host])
        success(pipeline(check; stdout = devnull, stderr = devnull)) && return cp
        start = Cmd([ssh_bin(); base; "-o"; "ControlMaster=yes"; "-o"; "ControlPath=" * cp;
                     "-o"; "ControlPersist=300"; "-f"; "-N"; c.site.host])
        try
            run(pipeline(start; stdin = devnull, stdout = devnull, stderr = devnull); wait = true)
        catch
            return ""
        end
        cp
    end
end

_hpc_cmd(c::HpcSshConn) =
    Cmd([ssh_bin(); hpc_ssh_args(c.site, c.ident; known_hosts = c.known_hosts,
                                 control_path = _hpc_open_master(c)); "sh"; "-s"])
_hpc_cmd(::HpcLocalConn) = `sh -s`

"""
    hpc_exec(conn, script; timeout_s = 60) -> (; code, out, err, timed_out)

Run `script` with `sh -s` on the remote (the script travels on stdin, so a csh login shell does not
matter, as for the connection test). Never throws for a failing script; see `hpc_exec!`.
"""
hpc_exec(c::HpcConn, script::AbstractString; timeout_s::Real = 60) =
    _hpc_capture(_hpc_cmd(c), script; timeout_s = timeout_s)

"""
    hpc_exec!(conn, script; what, timeout_s = 60) -> String

`hpc_exec`, returning stdout and throwing `HpcError("<what>: <reason>")` on a non-zero exit. The
reason is ssh's own failure in brief when ssh failed (exit 255), else the script's last stderr line.
"""
function hpc_exec!(c::HpcConn, script::AbstractString; what::AbstractString, timeout_s::Real = 60)::String
    r = hpc_exec(c, script; timeout_s = timeout_s)
    r.code == 0 && return r.out
    why = r.timed_out ? "the cluster did not answer within $(round(Int, timeout_s)) s. It may be busy; try again" :
          (c isa HpcSshConn && r.code == 255) ? _ssh_error_line(r.err) :
          r.code == 127 ? "not installed on the cluster (" * _last_line(r.err, "command not found") * ")" :
          _last_line(r.err, "exit code $(r.code)")
    throw(HpcError("$what: $why"))
end

function _last_line(s::AbstractString, default::AbstractString)::String
    ls = filter(l -> !isempty(strip(l)), split(s, '\n'))
    isempty(ls) ? String(default) : String(strip(last(ls)))
end

const _HPC_ROOTS = Dict{Tuple{String,String,Int,String},String}()

"""
    hpc_remote_root(conn, remote_dir = hpc_site().remote_dir) -> String

The remote folder as an absolute path (created if missing), so every later path is a plain prefix
and nothing depends on a remote shell expanding `~`. Cached per host, user and setting.
"""
function hpc_remote_root(c::HpcSshConn, remote_dir::AbstractString = c.site.remote_dir)::String
    key = (c.site.host, c.ident.user, c.site.port, String(remote_dir))
    haskey(_HPC_ROOTS, key) && return _HPC_ROOTS[key]
    out = hpc_exec!(c, "d=$(_remote_path_word(remote_dir))\nmkdir -p \"\$d\" && cd \"\$d\" && pwd -P\n";
                    what = "Remote folder")
    root = _last_line(out, "")
    startswith(root, "/") || throw(HpcError("Remote folder: unexpected reply"))
    _HPC_ROOTS[key] = root
end
hpc_remote_root(::HpcLocalConn, remote_dir::AbstractString)::String = (mkpath(remote_dir); realpath(remote_dir))

# rsync's `-e` is split by rsync itself: single quotes protect spaces, and a quote is doubled.
_rsync_e_word(s::AbstractString)::String = "'" * replace(String(s), "'" => "''") * "'"

_rsync_remote(c::HpcSshConn, path::AbstractString) = c.site.host * ":" * path
_rsync_remote(::HpcLocalConn, path::AbstractString) = String(path)

function _rsync_transport(c::HpcSshConn)::Vector{String}
    opts = hpc_ssh_opts(c.site, c.ident; known_hosts = c.known_hosts, control_path = _hpc_open_master(c))
    ["-e", join(_rsync_e_word.([ssh_bin(); opts]), " ")]
end
_rsync_transport(::HpcLocalConn) = String[]

"""Transfer totals from rsync's `--stats` (what the rerun-skips-upload tests assert on)."""
struct HpcRsyncStats
    files_transferred::Int
    bytes_sent::Int        # literal (non-matched) data, the bytes a rerun must not re-send
end

function _rsync_stats(out::AbstractString)::HpcRsyncStats
    num(re) = (m = match(re, out); isnothing(m) ? 0 : parse(Int, replace(m.captures[1], r"[,.]" => "")))
    HpcRsyncStats(num(r"Number of regular files transferred: ([\d,.]+)"),
                  num(r"Literal data: ([\d,.]+)"))
end

"""
    hpc_rsync(conn, src, dest; up = true, args = String[], files_from = nothing, what, timeout_s) -> HpcRsyncStats

One rsync between this machine and the remote: `up` copies the local `src` to the remote `dest`,
otherwise the remote `src` to the local `dest`. `args` are extra rsync flags (filters, `--delete`);
`files_from` is a list of paths relative to `src`, copied and nothing else. Throws `HpcError`.
`-s` keeps paths with spaces whole on the remote; `--partial-dir` keeps an interrupted file out of
the destination, so a half-sent chunk is never mistaken for a whole one.
"""
hpc_rsync(c::HpcConn, src::AbstractString, dest::AbstractString; kw...)::HpcRsyncStats =
    _rsync_stats(_hpc_rsync_run(c, src, dest; kw...))

"""
    hpc_rsync_itemize(conn, src, dest; up = false, args) -> Vector{String}

The files a pull of the remote `src` WOULD change locally (a dry run: nothing is written), relative to
`src`. What the undeclared-change check (dispatch.jl) asks.
"""
function hpc_rsync_itemize(c::HpcConn, src::AbstractString, dest::AbstractString; up::Bool = false,
                           args::AbstractVector{<:AbstractString} = String[])::Vector{String}
    out = _hpc_rsync_run(c, src, dest; up, args = vcat(["-n", "-i"], args), what = "Check")
    String[m[1] for m in eachmatch(r"(?m)^[<>]f\S* (.+)$", out)]
end

function _hpc_rsync_run(c::HpcConn, src::AbstractString, dest::AbstractString; up::Bool = true,
                        args::AbstractVector{<:AbstractString} = String[],
                        files_from::Union{Nothing,AbstractVector{<:AbstractString}} = nothing,
                        what::AbstractString = "Sync", timeout_s::Real = 24 * 3600)::String
    rsync = rsync_bin()
    isempty(rsync) && throw(HpcError("rsync not found on this machine"))
    list = nothing
    flags = ["-a", "-s", "--stats", "--partial-dir=.rsync-partial"]
    if !isnothing(files_from)
        list = tempname()
        write(list, join(files_from, "\n") * "\n")
        push!(flags, "--files-from=" * list)
    end
    from = up ? String(src) : _rsync_remote(c, src)
    to   = up ? _rsync_remote(c, dest) : String(dest)
    cmd = Cmd([rsync; flags; _rsync_transport(c); args; from; to])
    try
        r = _hpc_capture(cmd, ""; timeout_s = timeout_s)
        r.code == 0 && return r.out
        why = r.timed_out ? "timed out" :
              occursin("remote command not found", r.err) ? "rsync is not installed on the cluster" :
              (c isa HpcSshConn && occursin("connection unexpectedly closed", r.err)) ? _ssh_error_line(r.err) :
              _last_line(r.err, "rsync exit code $(r.code)")
        throw(HpcError("$what: $why"))
    finally
        isnothing(list) || rm(list; force = true)
    end
end
