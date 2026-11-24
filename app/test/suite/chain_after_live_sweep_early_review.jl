# RESULT-REVIEW: result-6e5c9a1e
# A sequential sweep has finished its current value on `a` but is still running it on `b` when the user
# clicks Queue after: `a` has no live row, so the browser sends nothing for it in `waitFor`. Its wait must
# still hold while the fn is going on `b` — the sweep's next value lands on `a` too, and an image-scope
# follow-up run now would read only the current value's output. Helpers from chain_after_live.jl.

@testset "Queue after — an image with nothing live of its own still holds while a peer runs" begin
    proj, (a, b) = _al_project(2)
    id_b, live_b = _al_start_live(proj, b; wait_ms = 5000)
    tmpl = ChainTemplate("Queue after",
        [ChainNode(id = "wait", fn = _AL_FN, scope = "image",
                   params = Dict{String,Any}("waitFor" => Dict{String,Any}(b => [id_b])), after_live = true),
         ChainNode(id = "next", fn = _AL_FN, scope = "image", params = Dict{String,Any}(),
                   barrier_policy = "successful_only")],
        [ChainEdge("wait", "next")])
    t = Threads.@spawn run_chain(proj, [a, b]; template = tmpl, on_log = _ -> nothing)
    run_id = _al_run_id(proj)
    sleep(Cecelia._AFTER_LIVE_GRACE + 0.5)
    @test _al_live(b)
    mid = load_chain_run(proj, run_id)
    @test _al_state(mid, a, "wait") != NODE_DONE        # a must not be released while b still runs
    @test _al_state(mid, a, "next") == NODE_PENDING     # so its follow-up has not run on the early output
    run = fetch(t)
    fetch(live_b)
    @test all(_al_state(run, u, n) == NODE_DONE for u in (a, b), n in ("wait", "next"))
    rm(proj.root; recursive = true)
end
