# HPC P1 — settings, migration, resolvers and the connection test (docs/todo/HPC_PLAN.md → P1).
#
# Everything here writes into temp dirs: `CECELIA_DEV_DIR` is pointed at a fresh dir for each
# config write and restored after, and profile/project settings take an explicit root.
#
# The LAST testset talks to a real sshd. It runs only when `CECELIA_HPC_TEST_HOST` is set — CI's
# SLURM container (`.github/workflows/verify-hpc.yml`) sets it; a dev machine skips it. Also run
# standalone by `pixi run test-hpc` (app/test/hpc_e2e.jl), which is what that workflow calls.

# Run `f` with config pointed at a fresh dir holding `custom` as custom.toml, then restore.
function _with_temp_config(f, custom::AbstractString = "")
    prev = get(ENV, "CECELIA_DEV_DIR", nothing)
    tmp = mktempdir()
    isempty(custom) || write(joinpath(tmp, "custom.toml"), custom)
    ENV["CECELIA_DEV_DIR"] = tmp
    try
        init_cecelia!()
        f(tmp)
    finally
        prev === nothing ? delete!(ENV, "CECELIA_DEV_DIR") : (ENV["CECELIA_DEV_DIR"] = prev)
        init_cecelia!()
    end
end

@testset "HPC: custom.toml migration (network pool → hpc)" begin
    d = Dict{String,Any}("pools" => Dict{String,Any}("network" => 3, "io" => 2))
    @test Cecelia.migrate_custom_conf!(d) == ["[pools] network renamed to hpc"]
    @test d["pools"] == Dict{String,Any}("hpc" => 3, "io" => 2)
    @test isempty(Cecelia.migrate_custom_conf!(d))                       # idempotent
    both = Dict{String,Any}("pools" => Dict{String,Any}("network" => 3, "hpc" => 5))
    Cecelia.migrate_custom_conf!(both)
    @test both["pools"] == Dict{String,Any}("hpc" => 5)                  # the new name wins
    @test isempty(Cecelia.migrate_custom_conf!(Dict{String,Any}()))      # no [pools] at all

    # Through init: the stored file is rewritten once, and the merged config has the new name.
    _with_temp_config("[dirs]\nprojects = '/tmp'\n\n[pools]\nnetwork = 3\n") do tmp
        txt = read(joinpath(tmp, "custom.toml"), String)
        @test !occursin("network", txt)
        @test Cecelia.TOML.parse(txt)["pools"]["hpc"] == 3
        @test Cecelia.TOML.parse(txt)["dirs"]["projects"] == "/tmp"        # other keys survive
        @test cecelia_conf()["pools"]["hpc"] == 3
        @test !haskey(cecelia_conf()["pools"], "network")
    end
    # A pool name stored outside custom.toml (a saved chain node, a drop-in spec) reads as the new one.
    @test Cecelia._renamed_pool("network") == "hpc" && Cecelia._renamed_pool("gpu") == "gpu"
    @test Cecelia._pool("network").name == "hpc"                          # an explicit request too
    @test Cecelia._node_from_dict(Dict("id" => "a", "fn" => "segment.cellpose",
                                       "resource_pool" => "network")).resource_pool == "hpc"
    # The shipped defaults carry the new name only.
    @test haskey(cecelia_conf()["pools"], "hpc") && !haskey(cecelia_conf()["pools"], "network")
end

@testset "HPC: every construction path renames the old pool" begin
    # The keyword constructor, the REPL helper and the deserialiser all land on the one inner
    # constructor, so none of them can build a node the validator refuses for naming `network`.
    @test Cecelia.ChainNode(; id = "a", fn = "segment.cellpose", resource_pool = "network").resource_pool == "hpc"
    @test Cecelia.chain_node("segment.cellpose"; resource_pool = "network").resource_pool == "hpc"
    @test Cecelia.chain_node("segment.cellpose"; resource_pool = "gpu").resource_pool == "gpu"
    @test Cecelia.chain_node("segment.cellpose").resource_pool == ""          # "" still means "from the spec"

    # Raw template documents (served verbatim): only a renamed node pool changes.
    doc = Dict{String,Any}("nodes" => Any[Dict{String,Any}("id" => "a", "resource_pool" => "network"),
                                           Dict{String,Any}("id" => "b", "resource_pool" => "gpu"),
                                           Dict{String,Any}("id" => "c")])
    @test Cecelia.rename_template_pools!(doc)
    @test [get(n, "resource_pool", nothing) for n in doc["nodes"]] == ["hpc", "gpu", nothing]
    @test !Cecelia.rename_template_pools!(doc)                             # nothing left to rename
    @test !Cecelia.rename_template_pools!(Dict{String,Any}())

    # The REPL pool setters too: an old name resizes `hpc` and is never written back to custom.toml.
    _with_temp_config() do tmp
        before = Cecelia._pool("hpc").limit
        try
            @test set_pool_limit!("network", 3) == 3
            @test Cecelia._pool("hpc").limit == 3
            @test !any(p -> p.name == "network", Cecelia.list_pools())
            pools = Cecelia.TOML.parsefile(joinpath(tmp, "custom.toml"))["pools"]
            @test pools == Dict{String,Any}("hpc" => 3)
        finally
            resize_pool!("hpc", before)
        end
    end

    # A drop-in module whose spec predates the rename: the scheduler sees `hpc`, never `network`.
    tmp = mktempdir()
    srcdir = joinpath(tmp, "modules", "sources", "hpcrename");          mkpath(srcdir)
    defdir = joinpath(tmp, "modules", "inputDefinitions", "hpcrename"); mkpath(defdir)
    spec = joinpath(defdir, "oldPool.json")
    write(spec, """{"fun_name": "hpcrename.oldPool", "label": "Old pool", "resource_pool": "network",
                    "scope": "image", "params": []}""")
    write(joinpath(srcdir, "oldPool.jl"), """
        struct _HpcOldPoolTask <: Cecelia.CciaTask end
        Cecelia.register_task!("hpcrename.oldPool", _HpcOldPoolTask(); spec = $(repr(spec)))
        """)
    try
        Cecelia.load_custom_modules!(; dev_dir = tmp)
        t = Cecelia._task_from_fun_name("hpcrename.oldPool")
        @test Cecelia._task_spec(t)["resource_pool"] == "hpc"
        @test Cecelia._task_pool_name(t) == "hpc"
        @test Cecelia.effective_pool_name(t) == "hpc"
    finally
        Cecelia._unregister_task!("hpcrename.oldPool")
    end
end

@testset "HPC: ssh/rsync resolvers" begin
    @test Cecelia._resolve_tool_bin("/opt/ssh/bin/ssh", "ssh"; which = _ -> nothing) == "/opt/ssh/bin/ssh"
    @test Cecelia._resolve_tool_bin("", "ssh"; which = _ -> "/usr/bin/ssh") == "/usr/bin/ssh"
    @test Cecelia._resolve_tool_bin("  ", "rsync"; which = _ -> nothing) == ""     # a miss is "", not "rsync"
    @test Cecelia._resolve_tool_bin("~/bin/rsync", "rsync"; which = _ -> nothing) ==
          joinpath(homedir(), "bin", "rsync")
    _with_temp_config("[dirs]\nssh = '/custom/ssh'\n") do _
        @test ssh_bin() == "/custom/ssh"
    end
    @test ssh_bin() isa String && rsync_bin() isa String
end

@testset "HPC: site settings round-trip + refusals" begin
    _with_temp_config("[dirs]\nprojects = '/tmp'\n") do tmp
        s = hpc_site()
        @test s.scheduler == "slurm" && s.port == 22 && s.remote_dir == "~/cecelia" && isempty(s.host)
        s = set_hpc_site!(Dict(:host => "login.example.org", :port => "2222",
                               :cpu => Dict(:queue => "normal", :preamble => "module load cuda\nmodule load x")))
        @test s.host == "login.example.org" && s.port == 2222
        @test s.cpu.queue == "normal" && s.cpu.preamble == "module load cuda\nmodule load x"
        @test s.gpu.queue == ""
        cfg = Cecelia.TOML.parsefile(joinpath(tmp, "custom.toml"))
        @test cfg["dirs"]["projects"] == "/tmp"                             # merged, not replaced
        @test cfg["hpc"] == Dict("scheduler" => "slurm", "host" => "login.example.org", "port" => 2222,
                                 "netQueue" => "")
        @test hpc_site() == s                                               # hot-reloaded
        # Which queue reaches the internet is a fact about the cluster: shared, like the host.
        s = set_hpc_site!(Dict("netQueue" => "copyq"))
        @test s.net_queue == "copyq" && hpc_site("bob").net_queue == "copyq"
        @test Cecelia.TOML.parsefile(joinpath(tmp, "custom.toml"))["hpc"]["netQueue"] == "copyq"
        s = set_hpc_site!(Dict("netQueue" => ""))

        # Only the cluster is shared: another profile sees the host, not this one's queue or folder.
        set_hpc_site!(Dict("remoteDir" => "/scratch/alice/cecelia"))
        @test Cecelia.read_profile_settings(Cecelia.active_profile_name())["hpc"]["cpu"]["queue"] == "normal"
        bob = hpc_site("bob")
        @test bob.host == "login.example.org" && bob.port == 2222
        @test bob.cpu.queue == "" && bob.remote_dir == "~/cecelia"
        b2 = set_hpc_site!(Dict("email" => "bob@uni.edu", "gpu" => Dict("account" => "lab2")); name = "bob")
        @test b2.email == "bob@uni.edu" && hpc_site().email == "" && hpc_site().remote_dir == "/scratch/alice/cecelia"
        @test set_hpc_site!(Dict("host" => "new.example.org"); name = "bob").host == "new.example.org"
        @test hpc_site().host == "new.example.org"                         # the cluster is shared

        # A personal key written to custom.toml before the split: read as the fallback, moved on save.
        cfg = Cecelia.TOML.parsefile(joinpath(tmp, "custom.toml"))
        cfg["hpc"]["email"] = "legacy@uni.edu"
        open(io -> Cecelia.TOML.print(io, cfg), joinpath(tmp, "custom.toml"), "w"); init_cecelia!()
        @test hpc_site("carol").email == "legacy@uni.edu"
        set_hpc_site!(Dict("port" => 22); name = "carol")
        @test !haskey(Cecelia.TOML.parsefile(joinpath(tmp, "custom.toml"))["hpc"], "email")
        @test hpc_site("carol").email == "legacy@uni.edu"                 # kept, now in carol's profile
        set_hpc_site!(Dict("host" => "login.example.org", "port" => 2222))

        before = read(joinpath(tmp, "custom.toml"), String)
        refused(p) = try set_hpc_site!(p); "" catch e; e isa ArgumentError ? e.msg : rethrow() end
        @test occursin("Host", refused(Dict("host" => "-oProxyCommand=touch /tmp/x")))
        @test occursin("Host", refused(Dict("host" => "two words")))
        @test occursin("Unknown scheduler", refused(Dict("scheduler" => "lsf")))
        @test occursin("Port", refused(Dict("port" => 0)))
        @test occursin("Port", refused(Dict("port" => "abc")))
        @test occursin("Unknown HPC setting", refused(Dict("hostname" => "x")))
        @test occursin("Unknown HPC setting", refused(Dict("cpu" => Dict("partition" => "x"))))
        @test occursin("GPU queue", refused(Dict("gpu" => Dict("queue" => "a b"))))
        @test occursin("Internet queue", refused(Dict("netQueue" => "-q x")))
        @test occursin("remote folder in Preferences", refused(Dict("remoteDir" => "")))
        @test read(joinpath(tmp, "custom.toml"), String) == before          # nothing written
    end
end

@testset "HPC: identity per profile" begin
    root = mktempdir()
    key = joinpath(root, "id_ed25519"); write(key, "k"); write(key * ".pub", "p")
    id = set_hpc_identity!(; user = "alice", key_file = key, name = "alice", config_root = root)
    @test id == HpcIdentity("alice", key)
    @test hpc_identity("alice"; config_root = root) == id
    @test hpc_identity("bob"; config_root = root) == HpcIdentity("", "")   # another profile: own login
    # Stored in the profile bag next to the frontend's keys, not in custom.toml.
    @test read_profile_settings("alice"; config_root = root)["hpcUser"] == "alice"
    bad(; kw...) = try set_hpc_identity!(; name = "alice", config_root = root, kw...); "" catch e; e.msg end
    @test occursin(".pub", bad(; key_file = key * ".pub"))
    @test occursin("not found", bad(; key_file = joinpath(root, "nope")))
    @test occursin("@host", bad(; user = "alice@login"))
    @test occursin("Username", bad(; user = "-l root"))
    @test hpc_identity("alice"; config_root = root) == id                  # refusals wrote nothing
    @test set_hpc_identity!(; user = "", name = "alice", config_root = root) == HpcIdentity("", key)
    @test !haskey(read_profile_settings("alice"; config_root = root), "hpcUser")   # cleared, not ""
end

@testset "HPC: project account override" begin
    root = mktempdir()
    @test read_hpc_project_settings(root) == Dict{String,Any}("account" => "")
    @test set_hpc_project_account!(root, " grant-42 ")["account"] == "grant-42"
    @test isfile(joinpath(root, "settings", "hpc.json"))
    @test !occursin(basename(root), read(joinpath(root, "settings", "hpc.json"), String))  # no identity baked in
    @test_throws ArgumentError set_hpc_project_account!(root, "a b")
    @test set_hpc_project_account!(root, "")["account"] == ""
    @test !isfile(joinpath(root, "settings", "hpc.json"))
end

@testset "HPC: ssh arguments + probe script" begin
    site = HpcSite("slurm", "login.example.org", 2222, "~/cecelia", "", HpcClass("", "", "", "", ""),
                   HpcClass("", "", "", "", ""))
    ident = HpcIdentity("alice", "/keys/id")
    a = Cecelia.hpc_ssh_args(site, ident; known_hosts = ["/kh"])
    opt(k) = (i = findfirst(==(k), a); isnothing(i) ? "" : a[i + 1])
    @test "BatchMode=yes" in a && "IdentitiesOnly=yes" in a
    @test "StrictHostKeyChecking=yes" in a                                 # strict unless asked
    @test !any(x -> occursin("accept-new", x), a)
    @test "StrictHostKeyChecking=accept-new" in Cecelia.hpc_ssh_args(site, ident; accept_new = true, known_hosts = ["/kh"])
    @test opt("-i") == "/keys/id" && opt("-p") == "2222" && opt("-l") == "alice"
    @test last(a) == "login.example.org"
    @test "UserKnownHostsFile=\"/kh\"" in a
    @test !("-v" in a)

    @test Cecelia._remote_path_word("~") == "\"\$HOME\""
    @test Cecelia._remote_path_word("~/a b") == "\"\$HOME\"/'a b'"
    @test Cecelia._remote_path_word("/scratch/it's") == "'/scratch/it'\\''s'"
    sc = Cecelia.hpc_probe_script(site)
    @test occursin("command -v sbatch", sc) && occursin("sbatch --version", sc)
end

@testset "HPC: connection test parsing" begin
    P(code, out, err; kw...) = Cecelia.parse_hpc_test(code, out, err; kw...)
    hk = "debug1: Server host key: ssh-ed25519 SHA256:abcDEF123\n"
    ok_out = "motd noise\nCECELIA-HPC ok\nCECELIA-HPC dir ok /home/alice/cecelia\nCECELIA-HPC sched ok slurm 23.11.4\n"
    r = P(0, ok_out, hk * "Warning: Permanently added '[h]:22' (ED25519) to the list of known hosts.\n")
    @test r.ok && r.reachable && r.new_host_key && r.dir_ok
    @test r.fingerprint == "ED25519 SHA256:abcDEF123"
    @test r.remote_dir == "/home/alice/cecelia" && r.scheduler == "slurm 23.11.4" && r.error == ""
    @test !P(0, ok_out, hk).new_host_key
    @test P(0, ok_out, "debug1: Server host key: ecdsa-sha2-nistp256 SHA256:x\n").fingerprint == "ECDSA SHA256:x"

    r = P(0, "CECELIA-HPC ok\nCECELIA-HPC dir fail /readonly\nCECELIA-HPC sched ok slurm 1\n", hk)
    @test !r.ok && r.reachable && !r.dir_ok && r.error == "Remote folder not writable"
    r = P(0, "CECELIA-HPC ok\nCECELIA-HPC dir ok /x\nCECELIA-HPC sched missing\n", hk; scheduler_label = "SLURM")
    @test !r.ok && r.error == "SLURM not found on the login node"

    err(code, e) = P(code, "", e).error
    @test err(255, hk * "alice@h: Permission denied (publickey).") ==
          "Key rejected. A passphrase key must be loaded in ssh-agent"
    @test startswith(err(255, "@@@\n@    WARNING: REMOTE HOST IDENTIFICATION HAS CHANGED!     @\nHost key verification failed."),
                     "Host key changed")
    @test startswith(err(255, "Host key verification failed."), "Host key not trusted")
    @test err(255, "ssh: connect to host h port 22: Connection refused") == "Connection refused. Check host and port"
    # A non-SSH port (Gadi on 20) accepts the TCP connect, then resets it.
    @test err(255, "kex_exchange_identification: read: Connection reset by peer\nConnection reset by 203.0.19.85 port 20") ==
          "Connection dropped. Check the port (SSH is usually 22)"
    @test err(255, "ssh: connect to host h port 22: Connection timed out") == "No response from host"
    @test err(255, "ssh: Could not resolve hostname nope: Name or service not known") == "Host not found"
    @test err(255, "debug1: x\nsomething odd happened") == "something odd happened"
    @test P(-1, "", ""; timed_out = true).error == "No reply within $(Cecelia.HPC_TEST_TIMEOUT_S) s"
    @test P(0, "no tags", "").error == "Unexpected reply from host"
    @test !P(255, "", "").reachable
end

@testset "HPC: connection test refuses before spawning" begin
    site = HpcSite("slurm", "", 22, "~/c", "", HpcClass("", "", "", "", ""), HpcClass("", "", "", "", ""))
    site2 = HpcSite("slurm", "h", 22, "~/c", "", site.cpu, site.gpu)
    cases = [(site,  HpcIdentity("a", "/k"),                    "Set the cluster host in Settings → HPC"),
             (site2, HpcIdentity("", "/k"),                     "Set your username in Preferences → HPC"),
             (site2, HpcIdentity("a", ""),                      "Select your SSH key in Preferences → HPC"),
             (site2, HpcIdentity("a", "/definitely/not/here"),  "Key file not found")]
    for (s, id, msg) in cases
        # Windows is refused before anything else (HPC_PLAN Q8), and says so.
        @test hpc_test_connection(s, id).error == (hpc_available() ? msg : "Not available on Windows yet")
    end
end

# ── Against a real sshd (+ SLURM): CI's container, or any sshd you point it at ─────────────────────
#
#   CECELIA_HPC_TEST_HOST / _PORT / _USER / _KEY   the target and a key it accepts
#   CECELIA_HPC_TEST_REMOTE_DIR                    where the probe may write (default ~/cecelia-test)
#   CECELIA_HPC_TEST_SCHEDULER_VERSION             substring `sbatch --version` must contain (optional)
const _HPC_LIVE = !isempty(get(ENV, "CECELIA_HPC_TEST_HOST", ""))
@testset "HPC: connection test against a live sshd" begin
    if !_HPC_LIVE
        @info "HPC live connection test skipped: CECELIA_HPC_TEST_HOST is not set (CI runs it in verify-hpc.yml)"
        @test_skip false
    elseif !hpc_available()
        @test_skip false
    else
        empty = HpcClass("", "", "", "", "")
        site = HpcSite("slurm", ENV["CECELIA_HPC_TEST_HOST"], parse(Int, get(ENV, "CECELIA_HPC_TEST_PORT", "22")),
                       get(ENV, "CECELIA_HPC_TEST_REMOTE_DIR", "~/cecelia-test"), "", empty, empty)
        ident = HpcIdentity(ENV["CECELIA_HPC_TEST_USER"], ENV["CECELIA_HPC_TEST_KEY"])
        kh = joinpath(mktempdir(), "hpc", "known_hosts")    # never the user's ~/.ssh/known_hosts

        r1 = hpc_test_connection(site, ident; known_hosts = [kh])
        @test r1.ok
        r1.ok || @error "Live HPC test failed" result = r1
        @test r1.new_host_key                               # first contact: trusted here, and said so
        @test startswith(r1.fingerprint, "ED25519 SHA256:") || startswith(r1.fingerprint, "ECDSA SHA256:") ||
              startswith(r1.fingerprint, "RSA SHA256:")
        @test isfile(kh)                                    # written to OUR known_hosts
        @test r1.dir_ok && !isempty(r1.remote_dir) && !occursin("~", r1.remote_dir)
        want = get(ENV, "CECELIA_HPC_TEST_SCHEDULER_VERSION", "")
        @test isempty(want) ? !isempty(r1.scheduler) : occursin(want, r1.scheduler)

        r2 = hpc_test_connection(site, ident; known_hosts = [kh])
        @test r2.ok && !r2.new_host_key && r2.fingerprint == r1.fingerprint

        # Every call but the test is strict: an unknown host is refused, not silently trusted.
        fresh = joinpath(mktempdir(), "known_hosts")
        strict = Cmd([ssh_bin(); Cecelia.hpc_ssh_args(site, ident; known_hosts = [fresh]); "true"])
        r3 = Cecelia._hpc_capture(strict, ""; timeout_s = 20)
        @test r3.code != 0 && occursin("Host key verification failed", r3.err)
        @test !isfile(fresh) || isempty(read(fresh, String))

        # A changed host key is refused even by the test, with the brief message.
        bogus = joinpath(mktempdir(), "known_hosts")
        hostpat = site.port == 22 ? site.host : "[$(site.host)]:$(site.port)"
        write(bogus, "$hostpat ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl\n")
        r4 = hpc_test_connection(site, ident; known_hosts = [bogus])
        @test !r4.ok && startswith(r4.error, "Host key changed")

        # A key the server does not accept.
        other = joinpath(mktempdir(), "other_key")
        run(pipeline(`ssh-keygen -q -t ed25519 -N "" -f $other`; stdout = devnull))
        r5 = hpc_test_connection(site, HpcIdentity(ident.user, other); known_hosts = [kh])
        @test !r5.ok && !r5.reachable && startswith(r5.error, "Key rejected")
    end
end
