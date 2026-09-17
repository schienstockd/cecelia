# ── Lab log (per-project append-only markdown) ──────────────────────────────
@testset "Lab log" begin
    proj = create_project!(name="lablog-test-$(rand(1000:9999))")

    @test read_lab_log(proj) == ""                     # empty before any entry
    @test parse_lab_log("") == Dict{String,Any}[]

    append_lab_log!(proj, "User", "CD4 gate lower bound ~0.25 for this tissue prep")
    append_lab_log!(proj, "Claude", ["Image 7 gated to 23 cells (cohort mean 187)",
                                     "User excluded image"])
    content = read_lab_log(proj)
    @test occursin("[User]", content) && occursin("[Claude]", content)

    # parsed NEWEST-FIRST, date+author injected, bullets captured
    entries = parse_lab_log(content)
    @test length(entries) == 2
    @test entries[1]["author"] == "Claude"             # newest first
    @test entries[1]["lines"] == ["Image 7 gated to 23 cells (cohort mean 187)",
                                  "User excluded image"]
    @test entries[2]["author"] == "User"
    @test occursin(r"^\d{4}-\d{2}-\d{2}$", entries[1]["date"])

    # APPEND-ONLY: a later write never rewrites earlier bytes
    before = read_lab_log(proj)
    append_lab_log!(proj, "User — correction",
                    "Corrects above: image 7 low count is real biology — keep it")
    after = read_lab_log(proj)
    @test startswith(after, before)
    after_entries = parse_lab_log(after)
    @test length(after_entries) == 3
    @test after_entries[1]["author"] == "User — correction"

    # persists across reload
    @test length(parse_lab_log(read_lab_log(load_project(proj.uid)))) == 3

    # a non-entry `## ` header (version boundary) is not parsed as an entry, and doesn't
    # swallow the following real entry
    marked = after * "\n## [Version boundary: v1 → v2, 2026-07-15]\n---\n"
    append_lab_log!(proj, "User", "post-boundary note")
    # (the boundary line lives in the file only if a user adds it; here we assert the parser)
    @test length(parse_lab_log(marked)) == 3           # boundary line adds no entry

    # empty / whitespace-only / no-author entries are rejected
    @test_throws ErrorException append_lab_log!(proj, "User", ["   "])
    @test_throws ErrorException append_lab_log!(proj, "Claude", String[])
    @test_throws ErrorException append_lab_log!(proj, "   ", ["x"])

    # returned block is a well-formed, header-injected markdown block
    blk = append_lab_log!(proj, "Claude", "another note")
    @test startswith(blk, "## ") && occursin("[Claude]", blk) && occursin("- another note", blk)

    rm(proj.root; recursive=true)
end

# ── Lab log context (auto [Cecelia] activity digest) ────────────────────────
# ROLLING DAILY block: one [Cecelia] block per day, regenerated from source and rewritten in place
# as activity accrues (append-only preserved for human entries). Dates are pinned so day rollover is
# deterministic; run-log `at` is stamped explicitly (the `at` kwarg) so a task lands on a given day.
@testset "Lab log context — rolling daily block" begin
    proj = create_project!(name="labctx-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="set-A")
    img1 = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
    img2 = add_image!(s; name="img-2", meta=Dict{String,Any}("ori_path"=>"/tmp/b.tif"))
    d1, d2 = Date(2026, 7, 20), Date(2026, 7, 21)

    # nothing run yet → no digest, no block on disk
    @test capture_context!(proj; date=d1) === nothing
    @test isempty(parse_lab_log(read_lab_log(proj)))

    # day-1 activity (run logs dated on d1) → ONE [Cecelia] block for the day
    append_run_log!(img1, "segment.cellpose", "default"; at="2026-07-20T09:00:00")
    append_run_log!(img2, "segment.cellpose", "default"; at="2026-07-20T09:05:00")
    append_run_log!(img1, "tracking.bayesian_tracking", ""; at="2026-07-20T10:00:00")

    block = capture_context!(proj; date=d1)
    @test block !== nothing
    @test occursin("## 2026-07-20 [Cecelia]", block)
    @test occursin("Segment — cellpose on 2 images", block)      # grouped by category, prefix dropped
    @test occursin("Tracking — bayesian_tracking on 1 image", block)   # singular
    @test occursin("✅ Segment", block) && occursin("✅ Tracking", block)   # all ok → ✅
    entries = parse_lab_log(read_lab_log(proj))
    @test length(entries) == 1 && entries[1]["author"] == "Cecelia"

    # idempotent: same day, no new activity → block unchanged → nothing, still ONE block
    @test capture_context!(proj; date=d1) === nothing
    @test length(parse_lab_log(read_lab_log(proj))) == 1

    # MORE activity the SAME day → the day's block is rewritten IN PLACE (cumulative), not a 2nd block
    append_run_log!(img2, "behaviour.hmm", ""; at="2026-07-20T14:00:00")
    block1b = capture_context!(proj; date=d1)
    @test block1b !== nothing
    @test occursin("Behaviour — hmm on 1 image", block1b)
    @test occursin("cellpose on 2 images", block1b)              # earlier activity still present
    @test length(parse_lab_log(read_lab_log(proj))) == 1         # ONE block, rewritten — not appended

    # NEXT day → a NEW block; the previous day's block is untouched and holds only its own activity
    append_run_log!(img1, "segment.measureLabels", "default"; at="2026-07-21T08:00:00")
    block2 = capture_context!(proj; date=d2)
    @test block2 !== nothing
    @test occursin("## 2026-07-21 [Cecelia]", block2)
    @test occursin("measureLabels", block2)
    @test !occursin("cellpose", block2)                          # day-2 block ≠ day-1 activity
    @test length(parse_lab_log(read_lab_log(proj))) == 2
    full = read_lab_log(proj)
    @test occursin("cellpose on 2 images", full) && occursin("Behaviour — hmm", full)   # d1 intact

    rm(proj.root; recursive=true)
end

# severity symbols + the COLLAPSED QC-detail lines (per-image and per-channel repetition folded away)
@testset "Lab log context — severity + collapsed QC details" begin
    projS = create_project!(name="labctx-sev-$(rand(1000:9999))")
    sS    = add_set!(projS; name="set-S")
    iS1   = add_image!(sS; name="s-1", meta=Dict{String,Any}("ori_path"=>"/tmp/s1.tif"))
    iS2   = add_image!(sS; name="s-2", meta=Dict{String,Any}("ori_path"=>"/tmp/s2.tif"))
    iS3   = add_image!(sS; name="s-3", meta=Dict{String,Any}("ori_path"=>"/tmp/s3.tif"))
    d = Date(2026, 7, 22)
    append_run_log!(iS1, "segment.measureLabels", "default", "failed"; at="2026-07-22T08:00:00")
    write_qc(iS2, "tracking.track_measures", "default",
             [Dict{String,Any}("level"=>"warn","code"=>"c","short"=>"s","long"=>"l")])
    append_run_log!(iS2, "tracking.track_measures", "default"; at="2026-07-22T09:00:00")
    # a fun run on 3 images with the SAME warn banked on 2 of them
    for im in (iS1, iS2, iS3); append_run_log!(im, "behaviour.hmm_states", "default"; at="2026-07-22T10:00:00"); end
    for im in (iS1, iS2)
        write_qc(im, "behaviour.hmm_states", "default",
                 [Dict{String,Any}("level"=>"warn","code"=>"hmm.collapsed","short"=>"Collapsed to one state","long"=>"l")])
    end

    sev = capture_context!(projS; date=d)
    @test sev !== nothing
    @test occursin("❌ Segment", sev)      # measureLabels failed → worst outcome for the module
    @test occursin("⚠️ Tracking", sev)     # track_measures produced a warn finding
    @test occursin("hmm_states on 3 images — 2 flagged", sev)
    @test !occursin("(3 images)", sev)     # redundant parenthetical dropped for >2 images
    # the SAME finding across 2 images collapses to ONE detail line (few images → listed by stable
    # uid, sorted; the panel resolves uid→name on demand)
    @test occursin("↳ Collapsed to one state ($(join(sort([iS1.uid, iS2.uid]), ", ")))", sev)
    @test count("Collapsed to one state", sev) == 1     # folded to one line, not repeated per image

    # per-CHANNEL collapse: 4 findings differing only by channel → ONE "ch 0-3" detail line
    iH = add_image!(sS; name="h-1", meta=Dict{String,Any}("ori_path"=>"/tmp/h.tif"))
    append_run_log!(iH, "segment.cellpose", "seg"; at="2026-07-22T11:00:00")
    write_qc(iH, "segment.cellpose", "seg",
             [Dict{String,Any}("level"=>"warn","code"=>"cellpose.channel_blank",
                               "short"=>"Channel $i is saturated","long"=>"l",
                               "detail"=>Dict{String,Any}("channel"=>i)) for i in 0:3])
    b2 = capture_context!(projS; date=d)
    @test occursin("↳ is saturated — ch 0-3 ($(iH.uid))", b2)

    rm(projS.root; recursive=true)
end

# a failure the user re-ran successfully is NOT a failure — the digest reports where the day left each
# image, not every attempt (2026-08-13: 4 of 6 images failed drift correction, all 4 succeeded on the
# re-run, and the module was still headed ❌ "4 failed")
@testset "Lab log context — a re-run supersedes an earlier failure" begin
    projR = create_project!(name="labctx-retry-$(rand(1000:9999))")
    sR    = add_set!(projR; name="set-R")
    iR1   = add_image!(sR; name="r-1", meta=Dict{String,Any}("ori_path"=>"/tmp/r1.tif"))
    iR2   = add_image!(sR; name="r-2", meta=Dict{String,Any}("ori_path"=>"/tmp/r2.tif"))
    d = Date(2026, 7, 26)
    for im in (iR1, iR2)
        append_run_log!(im, "cleanupImages.driftCorrect", "driftCorrected", "failed"; at="2026-07-26T09:00:00")
        append_run_log!(im, "cleanupImages.driftCorrect", "driftCorrected", "done";   at="2026-07-26T11:00:00")
    end
    ctx = capture_context!(projR; date=d)
    @test occursin("✅ Cleanup", ctx)                                  # recovered → not a failed module
    @test !occursin("failed", ctx)
    @test occursin("driftCorrect on 2 images", ctx)
    @test occursin("2 re-run after a failure", ctx)                   # the retry itself is still recorded

    # an image left failed still counts — and counts ONCE per image, not once per attempt
    iR3 = add_image!(sR; name="r-3", meta=Dict{String,Any}("ori_path"=>"/tmp/r3.tif"))
    for at in ("2026-07-26T12:00:00", "2026-07-26T12:30:00")
        append_run_log!(iR3, "cleanupImages.driftCorrect", "driftCorrected", "failed"; at=at)
    end
    ctx2 = capture_context!(projR; date=d)
    @test occursin("❌ Cleanup", ctx2)
    @test occursin("driftCorrect on 3 images — 2 re-run after a failure — 1 failed", ctx2)

    rm(projR.root; recursive=true)
end

# gating + exclusion deltas: NET over the day, diffed against a start-of-day baseline
@testset "Lab log context — gating & exclusions (daily net)" begin
    proj = create_project!(name="labctx-gate-$(rand(1000:9999))")
    s    = add_set!(proj; name="set-A")
    img1 = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
    img2 = add_image!(s; name="img-2", meta=Dict{String,Any}("ori_path"=>"/tmp/b.tif"))
    d1, d2 = Date(2026, 7, 23), Date(2026, 7, 24)

    # nothing yet
    @test capture_context!(proj; date=d1) === nothing

    # add a gated pop on d1 → "added: CD3" (net vs the day baseline)
    m = PopulationMap(; pop_type="flow", value_name="default")
    cd3 = add_pop!(m, "CD3"; gate=RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
    save_pop_map!(m, img1)
    bg = capture_context!(proj; date=d1)
    @test bg !== nothing && occursin("Gating — ", bg) && occursin("added: CD3", bg)

    # editing that gate the SAME day still nets to "added: CD3": the digest line is unchanged, so
    # the block is a no-op rewrite (returns nothing) and the log still reads "added", never "gate changed"
    set_gate!(m, cd3, RectangleGate("c1", "c2", 0.2, 1.0, 0.0, 1.0)); save_pop_map!(m, img1)
    @test capture_context!(proj; date=d1) === nothing
    ll = read_lab_log(proj)
    @test occursin("added: CD3", ll) && !occursin("gate changed", ll)

    # roll into d2: first capture seeds the new day's baseline (CD3 as-is) and reports nothing new
    @test capture_context!(proj; date=d2) === nothing
    # NOW change the gate on d2 → "gate changed: CD3" (CD3 is in the day baseline, so it's an edit)
    set_gate!(m, cd3, RectangleGate("c1", "c2", 0.3, 1.0, 0.0, 1.0)); save_pop_map!(m, img1)
    bg3 = capture_context!(proj; date=d2)
    @test bg3 !== nothing && occursin("gate changed: CD3", bg3) && !occursin("added: CD3", bg3)

    # filter/membership pop (cluster tracks): a DEFINITION change is caught generically, not just gates
    mt = PopulationMap(; pop_type="trackclust", value_name="default")
    add_pop!(mt, "clust_a"; filter_measure="clusters.x", filter_values=[0, 1])
    save_pop_map!(mt, img1)
    bd = capture_context!(proj; date=d2)
    @test bd !== nothing && occursin("added: clust_a", bd)

    # exclusions: net over the day. Images are referenced by stable uid, not name (the panel
    # resolves uid→name on demand), so the line reads "excluded <uid>".
    img2.included = false; save!(img2)
    be = capture_context!(proj; date=d2)
    @test be !== nothing && occursin("excluded $(img2.uid)", be) && !occursin("img-2", be)

    rm(proj.root; recursive=true)
end

@testset "Lab log context — first capture seeds silently" begin
    proj = create_project!(name="labctx-seed-$(rand(1000:9999))")
    s    = add_set!(proj; name="set-A")
    img  = add_image!(s; name="imgB", meta=Dict{String,Any}("ori_path"=>"/tmp/c.tif"))
    m    = PopulationMap(; pop_type="flow", value_name="default")
    add_pop!(m, "preexisting"; gate=RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
    save_pop_map!(m, img)

    # gating already present at first capture → baseline seeded, NOT reported (no retro dump)
    @test capture_context!(proj) === nothing
    @test isempty(parse_lab_log(read_lab_log(proj)))

    # a subsequent addition IS reported
    m2 = load_pop_map(img; value_name="default", pop_type="flow")
    add_pop!(m2, "newpop"; gate=RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
    save_pop_map!(m2, img)
    b = capture_context!(proj)
    @test b !== nothing && occursin("added: newpop", b)

    rm(proj.root; recursive=true)
end

# ── Lab log dismiss (hide a single entry — config sidecar, log stays append-only) ──
@testset "Lab log dismiss" begin
    proj = create_project!(name="dismiss-test-$(rand(1000:9999))")
    @test read_dismissed(proj) == String[]
    set_dismissed!(proj, "e1a2", true)
    set_dismissed!(proj, "b3c4", true)
    @test Set(read_dismissed(proj)) == Set(["e1a2", "b3c4"])
    set_dismissed!(proj, "e1a2", false)                                # un-hide
    @test read_dismissed(proj) == ["b3c4"]
    @test read_dismissed(load_project(proj.uid)) == ["b3c4"]           # persists
    @test_throws ErrorException set_dismissed!(proj, "  ", true)       # empty id rejected

    # hiding NEVER edits the log file (append-only): the entry text is still on disk
    append_lab_log!(proj, "Cecelia", ["a digest line to hide"])
    before = read_lab_log(proj)
    set_dismissed!(proj, "deadbeef", true)
    @test read_lab_log(proj) == before                                 # file untouched
    rm(proj.root; recursive=true)
end

# ── Lab log capture: the daily [Cecelia] digest groups activity by task category ──
@testset "Lab log capture — category digest" begin
    proj = create_project!(name="capture-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="set-A")
    img  = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
    d    = Date(2026, 7, 25)

    @test capture_context!(proj; date=d) === nothing    # no activity yet → nothing to digest

    # run-log activity across two categories → one bullet per category, in _CATEGORY_ORDER
    append_run_log!(img, "segment.cellpose", "default"; at="2026-07-25T09:00:00")
    append_run_log!(img, "behaviour.hmm", ""; at="2026-07-25T10:00:00")
    b = capture_context!(proj; date=d)
    @test b !== nothing && occursin("Segment — cellpose", b) && occursin("Behaviour — hmm", b)
    # Segment (earlier in _CATEGORY_ORDER) precedes Behaviour in the block
    @test findfirst("Segment", b).start < findfirst("Behaviour", b).start
    # images are referenced by stable uid, never name — the panel resolves uid→name on demand
    @test occursin(img.uid, b) && !occursin("img-1", b)
    rm(proj.root; recursive=true)
end

# ── Param validation ──────────────────────────────────────────────────────
# ONE test over the whole registry, instead of a hand-written testset per task.
#
# `validate_params` is a single generic function driven by the task's JSON spec, so asserting it
# per task tested one function N times, and only for the tasks somebody remembered to write a
# testset for — a new task whose spec declared no bounds, or a malformed spec, passed silently.
# This walks EVERY registered task and EVERY bounded param: ~80 range checks rather than 16, and
# it covers tasks added after this was written. Per-task testsets remain ONLY where they assert
# something task-specific (Branching's µm key rename, NeighbourStats/ClustRegions' moved keys).
#
# Two rules that only a whole-registry sweep can enforce, both of which caught a real defect when
# this landed: `params` must be a JSON ARRAY (testTasks.incremental_plot_task declared an object, so
# `validate_params` threw MethodError instead of validating), and every `type` must be one the
# validator knows (migrateLegacy said "string", which is not a case in `_validate_leaf`, so the
# param silently skipped validation).
@testset "Param validation — every registered task, from its spec" begin
    # Spec param types `_validate_leaf` understands. A type outside this set is a typo that
    # silently disables validation for that param, so the set is asserted, not assumed.
    known_types = Set(["int", "float", "bool", "select", "chipSelect", "text", "dirPath", "section", "group",
                       "channelSelection", "valueNameSelection", "valueNameInput", "popSelection",
                       "labelPropsColsSelection", "motionDimsSelection", "imagePicker"])

    # `field` values a `valueNameSelection` may name — the frontend's CciaImage fields, kept in step
    # with `VALUE_NAME_FIELDS` (frontend/src/tasks/paramValues.ts). Absent is legal and means image
    # versions. NOT the ccid.json spelling (`filepath`, singular) nor the R version's (`imFilepath`).
    # `labels` vs `labelPropsNames` is the choice that keeps going wrong — mask pixels vs a
    # measurement table, two independent registries — and is ratcheted separately; see
    # "a picker gates on `labels` only when the task needs the MASK".
    known_value_name_fields = Set(["filepaths", "labels", "labelPropsNames", "spatialGraphs"])

    # Namespaces a `valueNameInput` may write into — `VALUE_NAME_NAMESPACES`
    # (frontend/src/utils/taskOutput.ts). A superset of the fields above: several are not readable
    # from the image payload yet, so they offer no suggestions, but declaring one is still how the
    # param says what it names. See docs/todo/VALUE_NAME_INPUT_PLAN.md.
    known_value_name_namespaces = Set(["filepaths", "labels", "labelProps", "spatialGraphs", "tracks", "branches",
                                       "clusters", "regions", "stats", "models", "obsCols"])

    # A value the spec itself calls valid: the declared default, else something in range/options.
    function valid_value(p)
        t = string(get(p, "type", ""))
        d = get(p, "default", nothing)
        # A `required` param's default is UNUSABLE by definition — "pick at least one" ships with an
        # empty list — so the baseline must supply a stand-in. Without this, marking a param required
        # made its own task fail the "the spec's defaults satisfy the spec" check, and every
        # perturbation test in that task then threw for the required param instead of the one being
        # perturbed, hiding whatever the perturbation was meant to prove.
        empty_default = d isa Union{AbstractVector,AbstractDict} && isempty(d)
        if !isnothing(d) && d != "" && !(empty_default && get(p, "required", false) === true)
            return d
        end
        t == "int"    && return Int(get(p, "min", 1))
        t == "float"  && return Float64(get(p, "min", 1.0))
        t == "bool"   && return false
        t == "select" && return get(first(get(p, "options", [Dict("value" => "x")])), "value", "x")
        return "x"
    end

    # Every param at a value the spec accepts — so a rejection below is provably the ONE value we
    # perturbed, not a required param we forgot to supply. Group values live nested under the
    # group's key, keyed by index string, which is what `each_spec_param` hands back as `gk`.
    function baseline(spec_params)
        d = Dict{String,Any}()
        each_spec_param(spec_params) do p, gk
            key = String(something(spec_get(p, "key", ""), ""))
            t   = String(something(spec_get(p, "type", ""), ""))
            isempty(key) && return
            if t == "group"
                d[key] = Dict{String,Any}("0" => Dict{String,Any}())
            elseif t == "section"
                return                        # layout only, holds no value
            elseif isempty(gk)
                d[key] = valid_value(p)
            else
                get!(d, gk, Dict{String,Any}("0" => Dict{String,Any}()))["0"][key] = valid_value(p)
            end
        end
        d
    end

    # What a rejection may say instead of the raw key, per param. A `required` param rejects with the
    # sentence the SPEC declares (`requiredMessage`, else "<label> is required") — "Required param
    # 'basisPops' is missing" is a wire key, not something to show a user, and the nine tasks that
    # hand-rolled this check were already saying things like "select at least 2 populations".
    # Populated per task below, so the contract stays "the rejection names the offending PARAM" and
    # only widens on how it may name it.
    alt_names = Dict{String,Vector{String}}()

    # Assert the rejection names the offending param — otherwise a throw for an unrelated reason
    # (a missing required param) would let a broken bound pass as "validated".
    function rejects(task, params, key, why)
        err = nothing
        try
            validate_params(task, params)
        catch e
            err = e
        end
        if !(err isa ParamValidationError)
            @error "expected a ParamValidationError" task = typeof(task) param = key case = why got = err
        end
        @test err isa ParamValidationError
        names = err isa ParamValidationError &&
                (occursin(key, err.msg) || any(a -> occursin(a, err.msg), get(alt_names, key, String[])))
        if err isa ParamValidationError && !names
            @error "rejected, but for a different param" param = key case = why msg = err.msg
        end
        @test !(err isa ParamValidationError) || names
    end

    checked_bounds   = 0
    checked_selects  = 0
    checked_required = 0
    checked_tasks    = 0
    skipped_tasks    = 0     # registered, but ships no spec / no params (composites, testTasks)

    for (fun_name, registered) in sort(collect(Cecelia._fun_name_map()); by = first)
        @testset "$fun_name" begin
            # dispatch wiring: the registry name resolves back to the same task
            resolved = _task_from_fun_name(fun_name)
            @test typeof(resolved) === typeof(registered)
            @test task_scope(resolved) ∈ ("image", "set")

            spec = Cecelia._task_spec(resolved)
            spec_params = spec === nothing ? [] : get(spec, "params", [])
            # ARRAY, not object — an object makes validate_params throw MethodError
            @test spec_params isa AbstractVector
            # a task may legitimately ship no spec / no params; anything else is walked below
            if !(spec_params isa AbstractVector) || isempty(spec_params)
                skipped_tasks += 1
            end
            if spec_params isa AbstractVector && !isempty(spec_params)
            checked_tasks += 1

            empty!(alt_names)
            each_spec_param(spec_params) do p, _
                k = String(something(spec_get(p, "key", ""), ""))
                isempty(k) && return
                alt = filter(!isempty, strip.(String[string(something(spec_get(p, "requiredMessage", ""), "")),
                                                    string(something(spec_get(p, "label", ""), ""))]))
                isempty(alt) || (alt_names[k] = alt)
            end

            base = baseline(spec_params)
            # the spec's own defaults must satisfy the spec
            @test validate_params(resolved, deepcopy(base)) === nothing

            # every param, carrying the group it is nested in (containers filtered out below)
            flat = Tuple[]
            each_spec_param(spec_params) do p, gk
                push!(flat, (p, gk))
            end

            for (p, group_key) in flat
                key = String(something(spec_get(p, "key", ""), ""))
                t   = String(something(spec_get(p, "type", ""), ""))
                isempty(key) && continue
                t in ("section", "group") && continue   # containers hold no value of their own
                @test t ∈ known_types

                # A `valueNameSelection`'s `field` names a CciaImage field the FRONTEND reads
                # (`VALUE_NAME_FIELDS` in frontend/src/tasks/paramValues.ts). Same failure mode as an
                # unknown `type`: nothing errors, the widget just quietly degrades. Four tasks carried
                # the R version's `imFilepath`, which matched no branch, so they stopped preselecting
                # the image's ACTIVE version — and the form pointed at a version the viewer wasn't
                # showing while cellpose (field absent) pointed at the right one.
                if t == "valueNameSelection"
                    fld = spec_get(p, "field", nothing)
                    fld === nothing || @test String(fld) ∈ known_value_name_fields
                end

                # A `valueNameInput` declares the NAMESPACE it writes into, and it is REQUIRED —
                # unlike `field` above, there is no sensible default, and the whole point of the type
                # is that it is what makes "the name this task writes under" one concept across six
                # different key names (`outputValueName`, `valueNameSuffix`, `graphSuffix`, …). A
                # missing or misspelled one degrades exactly as quietly as an unknown `field` did:
                # no suggestions, no chain propagation, no param recall — and the form still looks
                # fine. Kept in step with `VALUE_NAME_NAMESPACES` (frontend/src/utils/taskOutput.ts).
                if t == "valueNameInput"
                    ns = spec_get(p, "namespace", nothing)
                    @test ns !== nothing
                    ns === nothing || @test String(ns) ∈ known_value_name_namespaces
                end

                # A `showIf` param is only validated when its condition holds — that is the whole
                # point of the mechanism, and it is what the frontend does too. So the sweep must
                # SATISFY the condition before perturbing the value, or it silently stops covering
                # every conditional param: `validate_params` skips it, nothing is rejected, and the
                # test reads as "this param has no constraints" rather than "this param was never
                # reached". Setting the condition keeps the coverage instead of exempting the param.
                #
                # First accepted value per key, which is all the condition asks for. Operator forms
                # (`{"notEndsWith": ...}`) are not expressible as a value, so those params fall back
                # to being probed as the defaults leave them.
                cond = spec_get(p, "showIf", nothing)
                shown = Dict{String,Any}()
                if cond isa AbstractDict
                    for (ck, want) in cond
                        want isa AbstractDict && continue
                        shown[String(ck)] = want isa AbstractVector ? first(want) : want
                    end
                end

                # perturb exactly one value, in place, inside its group entry if nested
                function with(bad)
                    d = deepcopy(base)
                    merge!(d, shown)
                    if isempty(group_key)
                        d[key] = bad
                    else
                        d[group_key]["0"][key] = bad
                    end
                    d
                end

                # a required param that goes missing must be rejected, whatever its type
                if get(p, "required", false) == true && isempty(group_key)
                    d = deepcopy(base)
                    merge!(d, shown)
                    delete!(d, key)
                    rejects(resolved, d, key, "required but missing")
                    checked_required += 1
                end

                if t in ("int", "float")
                    if haskey(p, "min")
                        rejects(resolved, with(p["min"] - 1), key, "below min")
                        checked_bounds += 1
                    end
                    if haskey(p, "max")
                        rejects(resolved, with(p["max"] + 1), key, "above max")
                        checked_bounds += 1
                    end
                    rejects(resolved, with("not-a-number"), key, "wrong type")
                elseif t == "select"
                    rejects(resolved, with("__not_a_valid_option__"), key, "unknown option")
                    checked_selects += 1
                elseif t == "bool"
                    rejects(resolved, with("yes"), key, "non-bool")
                end
            end
            end   # spec_params is a non-empty array
        end
    end

    # Guard against the sweep silently covering nothing. If the walk breaks — a renamed registry,
    # a spec shape this walker doesn't recognise — the loop runs zero times, every assertion in it
    # passes vacuously, and the suite reports green having tested nothing.
    #
    # The task count is EXACT and relative: every registered task is either walked or explicitly
    # counted as spec-less, so it needs no maintenance and catches a task that silently stopped
    # being visited. The rest are deliberately LOOSE floors — their job is "the walk still finds
    # params", not "the tree is currently this big". Pinning them near today's counts (82 bounds,
    # 22 selects) would turn deleting a task into a red build, which is how a guard becomes a
    # chore. Raise one only if it ever fails without the walk being broken.
    @test checked_tasks + skipped_tasks == length(Cecelia._fun_name_map())
    @test checked_bounds   >= 25
    @test checked_selects  >= 8
    @test checked_required >= 2
end

@testset "Param validation — run_task enforces it, not just validate_params" begin
    # The bounds themselves are swept for every task above; what is unique here is WHERE
    # validation happens — the scheduler entry point rejects bad params before `_run_task`
    # is ever reached, so a task body never runs with values its spec forbids.
    proj2 = create_project!(name="val-test-$(rand(1000:9999))")
    s2 = add_set!(proj2; name="s")
    img2 = add_image!(s2; name="img", meta=Dict{String,Any}("ori_path" => "/tmp/fake.tif"))
    @test_throws ParamValidationError run_task(
        ImportOmezarr(), img2, Dict{String,Any}("pyramidLevels" => 99))
    rm(proj2.root; recursive=true)
end
