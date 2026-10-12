# HPC transport — the ONE place the HPC tier spawns ssh (and, from P3, rsync). docs/todo/HPC_PLAN.md
# → Decision 4: everything goes over SSH with the user's key, nothing long-lived on the login node,
# no passwords stored. Binaries come from `ssh_bin()`/`rsync_bin()` (config/binaries.jl), never a
# bare name; the `ssh/rsync spawn ratchet` in app/test/suite/ratchets.jl holds that.
#
# P1 builds only the connection test. Host keys (HPC_PLAN Q10): the test is the one call that may
# trust a NEW host key (`accept-new`), and it reports the fingerprint so the user can compare it
# with what their site publishes. Every other call is `BatchMode=yes` + `StrictHostKeyChecking=yes`.

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
                      verbose::Bool = false)::Vector{String}
    args = String[]
    verbose && push!(args, "-v")
    append!(args, ["-o", "BatchMode=yes",
                   "-o", "ConnectTimeout=$(HPC_CONNECT_TIMEOUT_S)",
                   "-o", "StrictHostKeyChecking=" * (accept_new ? "accept-new" : "yes"),
                   "-o", "UserKnownHostsFile=" * join(_ssh_opt_quote.(known_hosts), " "),
                   "-o", "IdentitiesOnly=yes",
                   "-i", expand_user(ident.key_file),
                   "-p", string(site.port),
                   "-l", ident.user,
                   site.host])
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
    isempty(site.host)     && return fail("Set the host first")
    isempty(ident.user)    && return fail("Set your username first")
    isempty(ident.key_file) && return fail("Select your SSH key first")
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
