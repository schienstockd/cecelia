# Kiwi eval harness — KIWI_ASSISTANT_PLAN.md Phase 3 exit gate (Open decision 8) and the regression
# check for prompt changes / a second engine. Runs every prompt in scripts/kiwi_eval_prompts.json under
# each schema variant, `--reps` times, through the real `run_kiwi_turn`, and writes:
#
#   results.jsonl  one line per turn: automatic metrics + the full reply
#   summary.md     per-variant and per-prompt aggregates of the automatic metrics
#   scoring.md     BLINDED hand-scoring sheet: every claim, shuffled, variant hidden
#   key.json       blinded id → variant, to unblind after scoring
#
# The automatic metrics are cheap proxies, not the verdict: the gate is the hand score (does each claim
# say one thing its refs actually back?). Flags are keyword heuristics and will misfire — read them.
#
# COST: each turn is a real engine call on the SAME seat window this Claude Code session uses (~30 s and
# ~2.8k output tokens a turn, more with a re-ask; 2026-09-23 a 50-turn run hit the session limit at
# turn 29). Start with --reps 1, state the turn count before running, and watch the running spend the
# harness prints. It stops at the first session/usage-limit error rather than burning the rest as fails.
#
# SAFETY: hermetic config (a temp dir); the real projects dir is set IN MEMORY only; the MCP config is
# the headless one (`CECELIA_OBSERVER_NO_PAIR`), so no turn can re-pair a project; Kiwi's tool
# allow-list is read-only; this process's Claude Code messaging env is stripped before any spawn.
#
#   julia --project=api scripts/kiwi_eval.jl --projects-dir ~/cecelia-feijoa/projects \
#         --out /tmp/kiwi-eval --reps 5 [--variants bare,reasoning] [--prompts id,id] [--model sonnet]
#         [--python <analysis-env python>]      # default: python_bin_path()
#         [--slice k/n]                         # k-th of n parts of the same shuffled job list

using Random, Statistics

function _args()
    a = Dict{String,String}("reps" => "1", "variants" => "bare,reasoning", "model" => "sonnet",
                            "prompts" => "", "python" => "", "seed" => "1", "slice" => "1/1")
    i = 1
    while i <= length(ARGS)
        k = replace(ARGS[i], "--" => ""); a[k] = ARGS[i + 1]; i += 2
    end
    haskey(a, "projects-dir") || error("--projects-dir is required (the real projects dir — set in memory only)")
    haskey(a, "out") || error("--out is required")
    a
end
const A = _args()

ENV["CECELIA_NO_SERVE"] = "1"
let cfg = mktempdir(); write(joinpath(cfg, "custom.toml"), "[dirs]\nprojects = '" * mktempdir() * "'\n"); ENV["CECELIA_DEV_DIR"] = cfg end
include(joinpath(@__DIR__, "..", "api", "src", "server.jl"))
using JSON3
for k in collect(keys(ENV))
    (startswith(k, "CLAUDE_CODE_") || k in ("CLAUDECODE", "CLAUDE_PID")) && delete!(ENV, k)
end

const OUT = mkpath(expanduser(A["out"]))
const PROMPTS = let all_ = JSON3.read(read(joinpath(@__DIR__, "kiwi_eval_prompts.json"), String))["prompts"]
    want = filter(!isempty, split(A["prompts"], ","))
    isempty(want) ? collect(all_) : [p for p in all_ if String(p["id"]) in want]
end
const VARIANTS = String.(split(A["variants"], ","))
const REPS = parse(Int, A["reps"])

# the headless MCP config, written fresh into the temp config dir
const MCP_CFG = let
    py = isempty(A["python"]) ? python_bin_path() : A["python"]
    cfg = observer_mcp_config(_observer_mcp_dir(), py, _observer_api_url(); headless = true)
    path = joinpath(mktempdir(), "kiwi-eval-mcp.json"); write(path, JSON3.write(cfg)); path
end

# ── automatic metrics (proxies — the hand score is the gate) ────────────────────────────────────────
const COARSE_KINDS = ("project", "set", "image", "viewer")
const MULTI_FACT_RE = r"(;|, and |\band also\b|: .*,.*,)"i           # several facts in one claim
const REASSURE_RE   = r"\b(that's (fine|normal|ok)|nothing to worry|looks (fine|good)|is expected|no (issue|problem))\b"i
const LIMIT_RE      = r"(session|usage|rate) limit|hit your .*limit"i   # the seat window is spent — stop
const RECOMMEND_RE  = r"\b(you should|i recommend|i'd recommend|consider (excluding|removing|using)|exclude|discard)\b"i

# the check behind one validation error — which checks drive re-asks (and their cost) is the number to watch
_reask_reason(e) = occursin("more than one fact", e) ? "bundled" : occursin("cite it as", e) ? "underspecified" :
                   occursin("not in any tool result", e) ? "unseen" : "unresolved/other"

function metrics(out)
    claims = out["claims"]
    refs = [r["ref"] for c in claims for r in c["refs"]]
    kinds = [string(_kiwi_get(r, "kind")) for r in refs]
    texts = [String(c["text"]) for c in claims]
    Dict{String,Any}(
        "ok" => out["ok"], "reasked" => out["reasked"], "abstain" => out["abstain"],
        "claims" => length(claims), "refs" => length(refs),
        # depends on the prompt: an image-only prompt is RIGHT to cite images — compare per prompt
        "specificRefFrac" => isempty(kinds) ? missing : count(k -> !(k in COARSE_KINDS), kinds) / length(kinds),
        "multiFactClaims" => count(t -> occursin(MULTI_FACT_RE, t), texts),
        "reassureFlags" => count(t -> occursin(REASSURE_RE, t), texts),
        "recommendFlags" => count(t -> occursin(RECOMMEND_RE, t), texts),
        "interpretations" => count(c -> c["kind"] == "interpretation", claims),
        "questions" => count(c -> c["kind"] == "question", claims),
        "reaskReasons" => [_reask_reason(e) for e in get(out, "reaskErrors", String[])],
        "seconds" => out["seconds"], "toolCalls" => out["toolCalls"], "outputTokens" => out["usage"]["output"])
end

# ── run, interleaved so drift over the session doesn't land on one variant ─────────────────────────
jobs = [(p, v, r) for r in 1:REPS for p in PROMPTS for v in VARIANTS]
shuffle!(MersenneTwister(parse(Int, A["seed"])), jobs)
# `--slice k/n`: run the k-th of n parts of the SAME shuffled job list (same --seed) — so a comparison
# too big for one seat window runs across windows without repeating or dropping a turn
let (k, n) = parse.(Int, split(A["slice"], '/'))
    global jobs = jobs[k:n:end]
end
println("kiwi eval: $(length(jobs)) turns → $OUT  (model $(A["model"]), mcp $(MCP_CFG))")

old = Cecelia.cecelia_conf()["dirs"]["projects"]
rows = Dict{String,Any}[]
spent = Ref(0); t_start = time()
try
    Cecelia.cecelia_conf()["dirs"]["projects"] = expanduser(A["projects-dir"])
    open(joinpath(OUT, "results.jsonl"), "a") do io
        for (n, (p, v, r)) in enumerate(jobs)
            refs = [Dict(String(k) => x for (k, x) in pairs(ref)) for ref in p["refs"]]
            out = try
                run_kiwi_turn(String(p["project"]), String(p["prompt"]); refs,
                              agent = ClaudeAgent(; model = A["model"]), reasoning = (v == "reasoning"),
                              mcp_config_path = MCP_CFG)
            catch e
                Dict{String,Any}("ok" => false, "reasked" => false, "abstain" => false, "claims" => Any[],
                                 "errors" => [sprint(showerror, e)], "reasoning" => "", "seconds" => 0.0,
                                 "toolCalls" => 0, "usage" => Dict("input" => 0, "output" => 0))
            end
            row = Dict{String,Any}("prompt" => String(p["id"]), "variant" => v, "rep" => r,
                                   "metrics" => metrics(out), "reply" => out)
            push!(rows, row); println(io, JSON3.write(row)); flush(io)
            m = row["metrics"]; spent[] += m["outputTokens"]
            println("[$n/$(length(jobs))] $(p["id"]) · $v · rep $r — ok=$(m["ok"]) claims=$(m["claims"]) ",
                    "reasked=$(m["reasked"]) $(isempty(m["reaskReasons"]) ? "" : join(unique(m["reaskReasons"]), ",") * " ")",
                    "$(m["seconds"])s · $(m["outputTokens"]) out · running $(spent[]) out, $(round(Int, time() - t_start))s")
            if any(e -> occursin(LIMIT_RE, e), out["errors"])
                println("STOPPED: seat limit hit at turn $n — $(first(filter(e -> occursin(LIMIT_RE, e), out["errors"])))")
                break
            end
        end
    end
finally
    Cecelia.cecelia_conf()["dirs"]["projects"] = old
end

# ── summary ──────────────────────────────────────────────────────────────────────────────────────────
fmt(xs) = (ys = collect(skipmissing(xs)); isempty(ys) ? "–" :
           length(ys) == 1 ? string(round(ys[1]; digits = 2)) :
           string(round(mean(ys); digits = 2), " ± ", round(std(ys); digits = 2)))
cols = ["ok", "reasked", "claims", "specificRefFrac", "multiFactClaims", "reassureFlags", "recommendFlags",
        "questions", "seconds", "toolCalls", "outputTokens"]
function table(io, groups)
    println(io, "| group | n | ", join(cols, " | "), " |"); println(io, "|", repeat("---|", length(cols) + 2))
    for (name, rs) in groups
        vals = [fmt([r["metrics"][c] === missing ? missing : Float64(r["metrics"][c]) for r in rs]) for c in cols]
        println(io, "| $name | $(length(rs)) | ", join(vals, " | "), " |")
    end
end
open(joinpath(OUT, "summary.md"), "w") do io
    println(io, "# Kiwi eval — $(length(rows)) turns, model $(A["model"])\n")
    println(io, "Automatic proxies only; the gate is the hand score in `scoring.md`. `multiFactClaims`, ",
            "`reassureFlags`, `recommendFlags` are keyword heuristics.\n\n## By variant\n")
    table(io, [(v, filter(r -> r["variant"] == v, rows)) for v in VARIANTS])
    println(io, "\n## By prompt × variant\n")
    table(io, [("$(p["id"]) · $v", filter(r -> r["prompt"] == p["id"] && r["variant"] == v, rows))
               for p in PROMPTS for v in VARIANTS])
end

# ── blinded hand-scoring sheet ───────────────────────────────────────────────────────────────────────
items = [(r, i, c) for r in rows for (i, c) in enumerate(r["reply"]["claims"])]
shuffle!(MersenneTwister(parse(Int, A["seed"]) + 1), items)
key = Dict{String,Any}()
open(joinpath(OUT, "scoring.md"), "w") do io
    println(io, "# Kiwi claims — blinded hand scoring\n\nScore each claim: **S** its refs back exactly this ",
            "claim · **P** partly (true, but the refs are too coarse or it says more than they show) · ",
            "**N** not backed · **J** it judges/reassures/recommends. Variants are hidden; `key.json` unblinds.\n")
    for (n, (r, i, c)) in enumerate(items)
        id = "c$(lpad(n, 3, '0'))"
        key[id] = Dict("variant" => r["variant"], "prompt" => r["prompt"], "rep" => r["rep"], "claim" => i)
        refs = join(("`" * JSON3.write(x["ref"]) * "` → " * (x["result"]["ok"] ? x["result"]["label"] : "FAIL")
                     for x in c["refs"]), "<br>")
        println(io, "### $id · $(r["prompt"])\n**$(c["kind"])** — $(c["text"])\n\n$refs\n\nScore: ___\n")
    end
end
write(joinpath(OUT, "key.json"), JSON3.write(key))
println("wrote results.jsonl, summary.md, scoring.md ($(length(items)) claims), key.json")
