# RESULT-REVIEW: result-0c2a358b
# Chain from history leaves out, by default, only the versions of a sweep the exemplars moved past: an
# HMM on the same tracks as the one clustering read. An HMM on OTHER populations that nothing has read
# yet is a separate analysis, not an alternative to it, and stays in. (Uses `_history_project` from
# chain_from_history.jl.)
@testset "Chain from history — an unread HMM on other populations is not a sweep sibling" begin
    proj, uids = _history_project("t" => "RFtn9f_TKufLV")
    img = init_object(proj.uid, uids["t"])
    log = read_run_log(img)
    hmm = deepcopy(last(e for e in log if e["fun"] == "behaviour.hmm"))
    hmm["params"]["pops"] = ["Bcells/_tracked"]
    hmm["params"]["colName"] = "bcells"
    hmm["at"] = "2026-11-15T15:30:00"
    push!(log, hmm)
    write(Cecelia.run_log_path(img._dir), JSON3.write(log))
    r = chain_from_history(proj, [uids["t"]])
    fs = Dict(f["detail"] => f for f in r["finalOutputs"] if f["fn"] == "behaviour.hmm")
    @test !fs["bcells"]["siblingRead"]
    @test all(d -> fs[d]["siblingRead"], ["movement_2", "movement_4", "movement_5", "movement_6"])
    rm(proj.root; recursive = true)
end
