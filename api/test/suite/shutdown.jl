# App shutdown / stop testsets — extracted from api/test/runtests.jl.
#
# Two large source-level testsets pinning the shutdown surface:
#  - `API: shutdown stops EVERY resident child` — every port in api_diagnostics is freed,
#    Quit stops the runner by default but Restart / worktree-switch do not, PROD supervisor
#    (app.py) mirrors the same rule, dev supervisor CRASH_LIMIT loop breaker.
#  - `API: stopping the app actually stops it` — Julia-1.12 signal/exit hazards: exit()
#    tearing down JIT while workers compile, exit_on_sigint under non-interactive julia,
#    thread safepoints under signal-driven exit.
#
# Both are source-level on purpose: exercising them would kill the developer’s own running
# napari / preview worker / task runner (fixed ports).
#
# Path expressions rewritten to reach files from api/test/suite/*.jl via the shared
# API_TEST_DIR constant hoisted into runtests.jl. Extracted so runtests.jl contains only
# include lines + section-header comments — same shape as app/test/suite/*.jl.

@testset "API: shutdown stops EVERY resident child" begin
    # The preview worker was added and this function was not updated, so Quit left it alive on :7656
    # holding a warm cellpose model's VRAM that nothing could reach. Zombie children are the reason
    # the start/stop logic exists at all, so the coverage is asserted rather than remembered.
    #
    # Source-level on purpose: actually exercising it would kill the developer's own running napari
    # and preview worker, since the ports are fixed and these tests share the machine.
    src  = read(joinpath(API_TEST_DIR, "..", "src", "app_api.jl"), String)
    body = src[findfirst("function _stop_children_for_exit(", src)[1]:end]
    body = body[1:findfirst("\nend", body)[1]]

    # `api_diagnostics` is the de-facto registry of resident children — it reports one `*Port` per
    # child for the System panel. Anything it lists must be freed on exit, so ADDING a child to
    # diagnostics without adding it here fails this test rather than shipping another zombie.
    diag  = JSON3.read(api_diagnostics(HTTP.Request("GET", "/api/diagnostics"))[2])
    child = [k for k in keys(diag) if endswith(String(k), "Port") && String(k) != "port"]
    @test length(child) == 3            # preview, notebooks, runner — deliberately, not silently
    # None is freed by an unconditional `_kill_listeners_on_port`: another Cecelia's child can sit on
    # these ports (the sticky slot checks API + frontend only), so each goes through a helper that
    # skips a foreign one — and the runner only when `stop_runner` is true.
    @test !occursin("_kill_listeners_on_port(", body)
    for helper in ("_free_preview_port!()", "_free_notebooks_port!()", "runner_stop!(")
        @test occursin(helper, body)
    end

    # ── The runner's asymmetry, which is the whole point of it ──────────────────
    # Quit stops it; Restart and the worktree switch must NOT — that is what stops a backend restart
    # costing a running segmentation. Source-level like the rest of this testset: the alternative is
    # killing a runner that may be mid-task on the developer's own machine.
    @test occursin("stop_runner::Bool = true", src)          # Quit gets the stop by DEFAULT…
    @test occursin("stop_runner && try; Cecelia.runner_stop!", body)
    # …but the IN-PROCESS task cancel must NOT be gated on it. Every exit path here ends in `exit`, so
    # a task running in this process (the fallback whenever the runner is down) dies either way; the
    # gate only chose whether it died tidily. Gated, a Restart or worktree switch orphaned the Python
    # child to keep burning GPU and left the run with no terminal status at all — it simply vanished.
    # The runner's tasks are not in `_TASKS`, so this loop never reached them and the gate was never
    # what protected them. Asserted on the loop itself, since the bug was one `if` around it.
    cancel_at = findfirst("for t in list_tasks(); cancel_task!(", body)
    @test cancel_at !== nothing
    @test !occursin("if stop_runner", body[1:cancel_at[1]])
    # …and both restart paths opt out. Two call sites, asserted separately: a single count would pass
    # if one of them were changed to stop it. The restart route stops the runner only when the body
    # asks (`stopRunner`, the "restart to finish an update" path); `_wbool` defaults that to false.
    restart = src[findfirst("function api_app_restart(", src)[1]:end]
    restart = restart[1:findfirst("\nend", restart)[1]]
    @test occursin("stop_runner = _wbool(body, :stopRunner) || _pending_restart()", restart)
    @test occursin("_stop_children_for_exit(; stop_runner)", restart)
    @test _wbool(Dict{String,Any}(), :stopRunner) === false
    switch = src[findfirst("function api_app_switch_worktree(", src)[1]:end]
    @test occursin("_stop_children_for_exit(; stop_runner = false)",
                   switch[1:findfirst("\nend", switch)[1]])
    # Quit itself must NOT opt out — the failure this guards is a copy-paste that quietly leaves an
    # unreachable runner holding the GPU after the user asked for everything to stop.
    shutdown = src[findfirst("function api_app_shutdown(", src)[1]:end]
    @test !occursin("stop_runner = false", shutdown[1:findfirst("\nend", shutdown)[1]])
    # and the graceful stop, not only the port-level kill, for each child that has a handle
    @test occursin("_shutdown_notebook_server!()", body)
    @test occursin("_stop_preview_worker!()", body)

    # the shared stop must be reachable from the toggle-off route too — one meaning of "stop the
    # worker", however it is reached
    @test occursin("_stop_preview_worker!()",
                   read(joinpath(API_TEST_DIR, "..", "src", "preview_api.jl"), String))
    @test isdefined(Main, :_stop_preview_worker!)

    # The dev supervisor frees the same children on Ctrl-C / crash, where nothing runs the route above.
    # It names them as services (mapped through the same `service_port` the package uses) — assert the
    # list covers every child, because a child only ONE side knows about is a silent zombie.
    dev = read(joinpath(API_TEST_DIR, "..", "dev.jl"), String)
    m   = match(r"const CHILD_SERVICES = \(([^)]*)\)", dev)
    @test m !== nothing
    @test sort(Symbol.(lstrip.(strip.(split(m.captures[1], ",")), ':'))) ==
          sort(filter(s -> s ∉ (:backend, :frontend), collect(keys(Cecelia.SERVICE_PORT_BASE))))
    @test occursin("for svc in CHILD_SERVICES", dev)     # …and they are actually freed, not just listed
    # Both LONG-LIVED children this supervisor spawns must wire their streams EXPLICITLY. A
    # non-blocking `run` defaults to devnull, NOT to inheritance, and the Vite launch got that wrong for
    # as long as it existed — under a comment claiming the opposite, so its missing output was read as
    # buffering rather than as the discard it was. Asserted per spawn rather than by counting
    # `wait = false`, because the fire-and-forget `taskkill` on the Windows path legitimately wants
    # devnull and would make a count meaningless.
    @test occursin(r"run\(pipeline\(Cmd\(cmd; dir = fe\); stdout = stdout, stderr = stderr\)", dev)  # Vite
    # backend — its stdin is its lifeline (app/src/lifeline.jl), not the terminal
    @test occursin(r"run\(pipeline\(addenv\(bcmd, LIFELINE_ENV => \"1\"\); stdin = lifeline\[\], stdout = stdout,\s+stderr = stderr\)", dev)
    # …but a CRASH must not reach that teardown, or the runner it is supposed to protect dies with the
    # server that crashed. The relaunch decision is real code, loaded and called here rather than
    # grepped for: whether SIGTERM counts as a fault is exactly the kind of thing a text assertion
    # cannot see. `dev.jl` guards its `supervise()` call on being the script, so this include is inert.
    @test occursin("_crash_death(backend[])", dev)
    let switch = get(ENV, "CECELIA_SWITCH_FILE", nothing)
        # `@eval module` evaluates in a fresh module scope where `API_TEST_DIR` (defined in
        # Main by runtests.jl) is not visible — reach through Main explicitly.
        @eval module DevSupervisor; include(joinpath(Main.API_TEST_DIR, "..", "dev.jl")); end
        switch === nothing ? delete!(ENV, "CECELIA_SWITCH_FILE") : (ENV["CECELIA_SWITCH_FILE"] = switch)
    end
    # A fault → relaunch. `exitcode` is 0 for a signalled process (libuv), so the signal must be read
    # FIRST — checking the code first would read every crash as a clean exit and stop supervising.
    crashed(code, sig) = DevSupervisor._crash_death(code, sig)   # (exitcode, termsignal), as libuv reports
    @test crashed(0, 11)         # SIGSEGV — the crash this guard came from, and exitcode 0 with it
    @test crashed(0, 6)          # SIGABRT
    @test crashed(1, 0)          # a nonzero exit (a Windows fault arrives as one of these)
    # Asked to stop → stay stopped. Relaunching on SIGTERM/SIGKILL would make `pixi run stop` unable to
    # stop the app: the supervisor would keep bringing the backend back.
    @test !crashed(0, 15)        # SIGTERM — `pixi run stop`
    @test !crashed(0, 9)         # SIGKILL — a forced kill
    @test !crashed(0, 2)         # SIGINT  — Ctrl-C
    @test !crashed(0, 0)         # in-app Quit
    @test !crashed(42, 0)        # the restart sentinel — the loop's own path, not a crash
    @test DevSupervisor._crash_why(0, 11) == "signal 11"
    @test DevSupervisor._crash_why(3, 0)  == "exit code 3"
    # The loop breaker: CRASH_LIMIT faults inside the window stops it, and an old fault is forgotten so
    # a server that crashes once a day still self-heals forever.
    times = Float64[]
    @test DevSupervisor._note_crash!(times)                   # 1st
    @test DevSupervisor._note_crash!(times)                   # 2nd
    @test !DevSupervisor._note_crash!(times)                  # 3rd inside the window → give up
    stale = [time() - DevSupervisor.CRASH_WINDOW - 1, time() - DevSupervisor.CRASH_WINDOW - 2]
    @test DevSupervisor._note_crash!(stale) && length(stale) == 1   # both aged out; only the new one

    # PROD's supervisor carries the same rule (it cannot call the Julia one), so assert the mirror
    # exists — `_crashed` there reads a NEGATIVE rc as the signal, which is Popen's convention.
    app_src = read(joinpath(API_TEST_DIR, "..", "..", "app.py"), String)
    @test occursin("def _crashed(", app_src) && occursin("if _crashed(rc):", app_src)
    @test occursin("_FAULT_SIGNALS", app_src)

    # PROD's supervisor (`app.py`) had the same hole: `proc.terminate()` kills the Julia server and
    # leaves its three grandchildren running. It closes it by REUSING the route above rather than
    # carrying a third copy of platform port-killing — so assert the reuse and, crucially, the ORDER:
    # attempting a graceful stop AFTER terminate would be pointless, and the diff that introduces that
    # mistake looks almost identical to the correct one.
    app = read(joinpath(API_TEST_DIR, "..", "..", "app.py"), String)
    @test occursin("/api/app/shutdown", app)
    @test occursin("_stop_gracefully(proc)", app)
    # Assert the order inside the teardown block itself: the graceful attempt must come before the
    # terminate it is meant to avoid. Comparing positions in the whole file would pass even if the two
    # were in unrelated places, which is exactly the bug being guarded against.
    let tail = app[findlast("finally", app)[1]:end]
        i_graceful = findfirst("_stop_gracefully(proc)", tail)
        i_term     = findfirst("proc.terminate()", tail)
        @test i_graceful !== nothing
        @test i_term !== nothing
        @test i_graceful[1] < i_term[1]
    end
end

# ── Stopping the app actually stops it ────────────────────────────────────────
# Three separate defects made "stop" not stop, all of them Julia-1.12 signal/exit behaviour rather
# than anything visible in the app's own logic — which is why they are pinned here:
#
#  1. `exit()` tears down the JIT and the thread pool while worker threads are still live. With a
#     worker mid-compile — the normal state, since every HTTP handler runs on the pool — it SEGFAULTS
#     (measured: 3 of 5 runs). `dev.jl` reads a fault signal as a crash and RELAUNCHES, so an in-app
#     Quit came straight back up.
#  2. `exit_on_sigint` is TRUE for a non-interactive julia: SIGINT calls `jl_exit` from the handler,
#     so nothing unwinds. The supervisor's whole `finally` teardown, and the server's own child
#     cleanup, were dead code under Ctrl-C.
#  3. A signal-driven exit needs every thread at a safepoint; one inside codegen or a blocking ccall
#     never gets there, so the process prints every thread's backtrace and KEEPS RUNNING. A single
#     SIGTERM — all `pixi run stop` sent — therefore could not stop the backend, while the task
#     printed "stopped …" regardless.
#
# Source-level, like the shutdown testset above and for the same reason: exercising any of it would
# kill the developer's own running napari, preview worker and task runner (the ports are fixed).
@testset "API: stopping the app actually stops it" begin
    app_api = read(joinpath(API_TEST_DIR, "..", "src", "app_api.jl"), String)
    server  = read(joinpath(API_TEST_DIR, "..", "src", "server.jl"),  String)
    dev     = read(joinpath(API_TEST_DIR, "..", "dev.jl"),            String)

    # (1) No exit path may call `exit()`. `_exit_now` is POSIX `_exit`: no atexit, no thread
    # rendezvous, no JIT teardown — and it delivers the exact status, which is the ONLY channel
    # carrying intent to the supervisor (0 = quit, 42 = restart). A `exit(0)` that faults delivers a
    # signal instead, and the supervisor cannot then tell a Quit from a crash.
    @test occursin("ccall(:_exit, Cvoid, (Cint,), code)", app_api)
    for route in ("api_app_shutdown(", "api_app_restart(", "api_app_switch_worktree(")
        body = app_api[findfirst("function $route", app_api)[1]:end]
        body = body[1:findfirst("\nend", body)[1]]
        @test occursin("_exit_now(", body)
        # the bare `exit(` this replaced — the fallback inside `_exit_now` itself is the only one left
        @test match(r"(?<![_\w])exit\(", body) === nothing
    end

    # (2) Ctrl-C must run each process's teardown — but the two need OPPOSITE mechanisms, and using
    # the supervisor's in the server was measured to be fatal.
    #
    # Supervisor: `exit_on_sigint(false)`, so SIGINT throws and the `catch`/`finally` below it are
    # reachable at all — without it they are dead code and every child is orphaned.
    @test occursin("Base.exit_on_sigint(false)", dev)
    @test occursin("Base.disable_sigint()", dev)          # a 2nd Ctrl-C can't abort teardown half way
    # The backend runs in its OWN process group, so Ctrl-C reaches only the supervisor. Same group and
    # it is a race: the backend dies first with Julia's unhandled-InterruptException status (exit 1),
    # `wait` returns normally, and the crash classifier RELAUNCHES the app the user just stopped.
    @test occursin("detach = true", dev)
    # …which in turn means a hard-killed supervisor can no longer take the backend down, so the server
    # watches for it — through its lifeline, which the supervisor must hand it. Without this pairing,
    # `detach` trades one orphan for another. (The lifeline itself, against a SIGKILLed launcher, is
    # app/test/suite/logging.jl.)
    @test occursin("_watch_supervisor!", server) && occursin("exit_with_parent!", server)
    @test occursin("LIFELINE_ENV => \"1\"", dev) && occursin("stdin = lifeline[]", dev)
    # Server: an `atexit` hook, NEVER `exit_on_sigint(false)`. Under `-t auto` the InterruptException
    # goes to whichever task is at a safepoint — routinely an idle worker inside the scheduler's own
    # `poptask`, which has no handler — so the process dies with `fatal: error thrown and no exception
    # handler available` and skips the teardown entirely. `jl_exit` runs atexit hooks, so the hook
    # works on Ctrl-C *and* SIGTERM. This assertion is the one that keeps the "obvious" fix out.
    # Checked against the CODE, not the file text — the comment above `start` names the trap in order
    # to warn about it, and must not itself trip the guard.
    let code = join(filter(l -> !startswith(strip(l), "#"), split(server, '\n')), '\n')
        @test !occursin("exit_on_sigint", code)
    end
    let body = server[findfirst("function start(", server)[1]:end]
        body = body[1:findfirst("\nend", body)[1]]
        @test occursin("atexit() do", body)
        @test occursin("_stop_children_for_exit()", body)
    end
    # The Quit/Restart routes leave via `_exit_now` (POSIX `_exit`), which SKIPS atexit — that is what
    # stops the teardown running twice, and stops a Restart's `stop_runner = false` being undone by a
    # hook that does not know about it. So they must each still call the teardown THEMSELVES.
    for route in ("api_app_shutdown(", "api_app_restart(", "api_app_switch_worktree(")
        body = app_api[findfirst("function $route", app_api)[1]:end]
        body = body[1:findfirst("\nend", body)[1]]
        @test occursin("_stop_children_for_exit(", body)
    end
    # The supervisor's thread count must NOT be pinned. `-t 1` reads as harmless (it only spawns and
    # waits) and breaks Ctrl-C outright: at one thread Julia 1.12 does not deliver SIGINT to a process
    # blocked in `wait` at all, so the teardown never runs. Measured; this keeps the pin from coming
    # back as a "tidy-up".
    let pixi = read(joinpath(API_TEST_DIR, "..", "..", "pixi.toml"), String)
        for task in ("dev", "dev-runner")
            line = only(filter(l -> startswith(l, task * " ") || startswith(l, task * "  "),
                               split(pixi, '\n')))
            @test occursin("dev.jl", line) && !occursin("-t ", line)
        end
    end

    # (3) SIGTERM is not enough — the stop path must escalate to SIGKILL and CONFIRM.
    # `_stop_backend!` is real code, loaded and called below rather than grepped for: "does it return
    # only once the process is actually dead" is not something a text assertion can see.
    # NOTE the `port` on every call: `_stop_backend!` ASKS before it signals, and its default port is
    # :8080. Left at the default these tests would POST /api/app/shutdown to the developer's own
    # running server and quit it. `port = 1` can never be listening, so the ask always fails through.
    @test occursin("Base.SIGKILL", dev)
    let p = run(pipeline(`bash -c 'trap "" TERM INT; sleep 60'`; stdout = devnull, stderr = devnull);
                wait = false)
        sleep(0.3)
        DevSupervisor._stop_backend!(p; port = 1, quit_grace = 0.5, term_grace = 0.8)
        @test !process_running(p)                    # deaf to SIGTERM, so only SIGKILL ended it
    end
    # …and a backend that goes away on its own inside the grace window is never signalled at all —
    # the orderly path (`/api/app/shutdown` → `_exit_now(0)`) must not be cut short by a SIGTERM.
    let p = run(pipeline(`bash -c 'sleep 1'`); wait = false)
        DevSupervisor._stop_backend!(p; port = 1, quit_grace = 6.0)
        @test !process_running(p)
        @test p.termsignal == 0 && p.exitcode == 0   # it exited on its own terms
    end
    # Asking is tried BEFORE signalling — the ordering is the whole point (SIGTERM prints every
    # thread's backtrace; SIGKILL orphans the children outright), and a diff that reverses it looks
    # almost identical to the correct one. Same assertion prod's `app.py` already carries.
    let body = dev[findfirst("function _stop_backend!(", dev)[1]:end]
        body = body[1:findfirst("\nend", body)[1]]
        @test findfirst("_ask_backend_to_quit(", body)[1] < findfirst("kill(p)", body)[1]
        @test findfirst("kill(p)", body)[1] < findfirst("Base.SIGKILL", body)[1]
    end

    # portkill.jl is the ONE port→kill implementation, shared by `dev.jl` and every `pixi run stop*`
    # task (which used to carry three per-OS shell variants of it). Its parser must agree with the
    # package's own — they cannot share code across the api/app boundary, so pin them to each other.
    @test occursin("include(joinpath(@__DIR__, \"portkill.jl\"))", dev)
    let raw = "LISTEN 0 128 127.0.0.1:8080 0.0.0.0:* users:((\"julia\",pid=4242,fd=24))\n" *
              "LISTEN 0 128    [::1]:8080    [::]:* users:((\"julia\",pid=4242,fd=25))\n"
        @test DevSupervisor.PortKill.pids_from_ss(raw) == ["4242"]     # deduped across IPv4/IPv6
        @test string.(Cecelia._listener_pids_from_ss(raw)) == DevSupervisor.PortKill.pids_from_ss(raw)
        @test isempty(DevSupervisor.PortKill.pids_from_ss(""))
    end
    # The port slot is resolved before ANY include: `_RUNNER` (runner_api.jl) captures its port at
    # include time, and one captured before the slot is known points at slot 0 — so a slot-1 instance's
    # Quit killed the slot-0 session's runner. Measured, not hypothetical.
    let src = read(joinpath(API_TEST_DIR, "..", "src", "server.jl"), String)
        @test findfirst("Cecelia.resolve_port_slot!()", src)[1] < findfirst("\ninclude(", src)[1]
    end
    # `stop` names every service, by name — portkill.jl maps them to this user's port slot. A literal
    # port here would aim at slot 0, i.e. possibly another user's Cecelia.
    let pixi = read(joinpath(API_TEST_DIR, "..", "..", "pixi.toml"), String)
        stop_line = only(filter(l -> startswith(l, "stop  "), split(pixi, '\n')))
        m = match(r"portkill\.jl ([a-z ]+)\"", stop_line)
        @test m !== nothing
        @test sort(Symbol.(split(m.captures[1]))) == sort(collect(keys(Cecelia.SERVICE_PORT_BASE)))
        @test !occursin(r"\b\d{4}\b", stop_line)
        @test occursin("portkill.jl", stop_line)
        # Base-only, deliberately: `stop` has to work when a manifest is broken, which is when you
        # reach for it. `--project` here would make the emergency stop depend on the thing that broke.
        @test !occursin("--project", stop_line)
    end
end

@testset "API: a stop never kills another config dir's child" begin
    # Kill-by-port trusts the port, and the child ports are the ones the sticky slot does not check —
    # so the installed app's runner can be mid-task on our slot's runner port (or its Pluto / preview
    # worker orphaned on ours) while `pixi run dev` runs against CECELIA_DEV_DIR. In-process stops
    # skip a foreign one; the dev supervisor's teardown and `pixi run stop*` cannot load Cecelia and
    # ask the same questions through PortKill.
    PK = DevSupervisor.PortKill
    @test PK.ping_config_dir("HTTP/1.0 200 OK\r\n\r\n{\"pid\":1,\"configDir\":\"C:\\\\Users\\\\a b\\/c \\u00e9\"}") ==
          "C:\\Users\\a b/c é"                       # JSON-unescaped, so a Windows path compares equal
    @test PK.ping_config_dir("{\"pid\":1}") === nothing   # a runner too old to report one…
    @test PK.foreign_runner_dir(1, "/x") === nothing      # …and nothing listening both read as ours
    @test PK.http_status("HTTP/1.1 403 Forbidden\r\n\r\n") == 403
    @test PK.http_status("") === nothing
    @test PK.pluto_is_foreign(1, "x") == false          # nothing listening → not foreign
    @test PK.preview_is_foreign(1, "x") == false
    @test PK.preview_is_foreign(1, nothing) == false      # no token of ours proves nothing

    # A stand-in child in its OWN process (a stop that wrongly fires must not take this suite down
    # with it). `reply(request)` → the raw HTTP response; spliced into the child's source.
    function fake_child(reply::AbstractString)
        port = let (p, s) = PK.Sockets.listenany(PK.Sockets.localhost, 30000); close(s); Int(p); end
        code = """
            using Sockets
            reply = $reply
            srv = listen(Sockets.localhost, $port)
            while true
                s = accept(srv)
                @async try
                    write(s, reply(readuntil(s, "\\r\\n\\r\\n")))
                finally
                    close(s)
                end
            end"""
        proc = run(pipeline(`$(Base.julia_cmd()) --startup-file=no -e $code`;
                            stdout = devnull, stderr = devnull); wait = false)
        for _ in 1:600
            isempty(PK._http_get_raw(port, "/")) || break
            sleep(0.1)
        end
        proc, port
    end
    resp(status, body = "") = "HTTP/1.0 $status\r\nContent-Length: $(sizeof(body))\r\n\r\n$body"
    # A runner `/ping` reporting `dir`; a Pluto that 403s unless the request carries ITS secret (Pluto's
    # auth middleware); a preview worker that 401s without ITS token and 426s a non-upgrade GET with it.
    fake_runner(dir)  = fake_child("_ -> $(repr(resp("200 OK", JSON3.write(Dict("protocol" => 1, "configDir" => dir)))))")
    fake_pluto(sec)   = fake_child("req -> occursin($(repr("secret=" * sec * " ")), req) ? " *
                                   "$(repr(resp("200 OK"))) : $(repr(resp("403 Forbidden")))")
    fake_preview(tok) = fake_child("req -> $(repr("Authorization: Bearer " * tok)) in split(req, \"\\r\\n\") ? " *
                                   "$(repr(resp("426 Upgrade Required"))) : $(repr(resp("401 Unauthorized")))")

    gone(proc) = (for _ in 1:50; process_running(proc) || break; sleep(0.1); end; !process_running(proc))
    stop_script(svc) = read(Cmd(`$(Base.julia_cmd()) --startup-file=no portkill.jl $svc`;
                                dir = joinpath(API_TEST_DIR, "..")), String)   # `pixi run stop-<svc>`

    # Each child, foreign then ours: dev.jl's Ctrl-C teardown (`_free_child`), the `pixi run stop-*`
    # script and the in-app stop (`runner_stop!`, the Quit/Restart backstops) must all leave another
    # Cecelia's running, and must still stop ours (the check must not make teardown a no-op). One
    # stand-in for both sides pins each in-process check to its Base-only PortKill twin.
    function check(svc, env, (theirs, ours); in_app = nothing)
        proc, port = theirs()
        try
            withenv(env => string(port)) do
                @test PK.foreign_owner(svc, port, DevSupervisor.config_dir(); token = read_api_token(), secret = read_pluto_secret()) !== nothing
                DevSupervisor._free_child(svc)
                @test occursin("left $svc($port) running", stop_script(svc))
                in_app === nothing || @test_logs (:warn, r"belongs to another Cecelia") match_mode = :any in_app(port)
            end
            sleep(0.5)
            @test process_running(proc)
        finally
            kill(proc, Base.SIGKILL)
        end
        for stop in (port -> withenv(() -> DevSupervisor._free_child(svc), env => string(port)),
                     (in_app === nothing ? () : (in_app,))...)
            proc, port = ours()
            try
                @test PK.foreign_owner(svc, port, DevSupervisor.config_dir(); token = read_api_token(), secret = read_pluto_secret()) === nothing
                stop(port)
                @test gone(proc)
            finally
                process_running(proc) && kill(proc, Base.SIGKILL)
            end
        end
    end

    cfg = DevSupervisor.config_dir()
    @test cfg == Cecelia.config_dir()                  # the scripts resolve the dir the runner reports
    check(:runner, "CECELIA_RUNNER_PORT",
          (() -> fake_runner(joinpath(mktempdir(), "another cecelia")), () -> fake_runner(cfg));
          in_app = port -> Cecelia.runner_stop!(Cecelia.RunnerHandle(; port)))

    # Ours = the secret this install published (`pluto_secret_path`, read by the API and by PortKill alike).
    # Reuse a live one rather than overwrite it — a running Pluto of this checkout would lose its links.
    created = !isfile(pluto_secret_path())
    created && (mkpath(dirname(pluto_secret_path())); write(pluto_secret_path(), "oursecret" * string(rand(UInt32); base = 16)))
    try
        check(:notebooks, "CECELIA_PLUTO_PORT",
              (() -> fake_pluto("theirsecret456"), () -> fake_pluto(_notebook_secret()));
              in_app = port -> withenv(_free_notebooks_port!, "CECELIA_PLUTO_PORT" => string(port)))
        # Shut down (the Notebooks page): ours, adopted with no handle, is stopped by its port; another
        # Cecelia's only on the user's explicit `force` — the page's confirmed "Stop it".
        for (mk, force) in ((() -> fake_pluto(_notebook_secret()), false), (() -> fake_pluto("theirsecret456"), true))
            proc, port = mk()
            try
                withenv("CECELIA_PLUTO_PORT" => string(port)) do
                    if force
                        @test first(_shutdown_notebook_server!()) == false
                        sleep(0.5)
                        @test process_running(proc)
                    end
                    @test _shutdown_notebook_server!(; force) == (true, "stopped")
                end
                @test gone(proc)
            finally
                process_running(proc) && kill(proc, Base.SIGKILL)
            end
        end
    finally
        created && rm(pluto_secret_path(); force = true)
    end

    # The token is handed in env, as the backend hands it to its children; the script inherits it.
    withenv("CECELIA_API_TOKEN" => "ourtoken" * string(rand(UInt32); base = 16)) do
        check(:preview, "CECELIA_PREVIEW_PORT",
              (() -> fake_preview("theirtoken"), () -> fake_preview(read_api_token()));
              in_app = port -> _free_preview_port!(port))
        # Settings → "Stop it" on another Cecelia's worker: the user's explicit, confirmed `force`.
        proc, port = fake_preview("theirtoken")
        try
            withenv("CECELIA_PREVIEW_PORT" => string(port)) do
                @test first(api_preview_stop(Vector{UInt8}(JSON3.write(Dict("force" => true))))) == 200
            end
            @test gone(proc)
        finally
            process_running(proc) && kill(proc, Base.SIGKILL)
        end
    end
end
