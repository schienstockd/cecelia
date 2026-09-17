# ── Task-spec ratchets + copy-style testsets ──────────────────────────
# 11 sections pinning the task-spec surface: numeric param ranges are plausible, task
# spec tips stay short, a handler fallback never contradicts its spec default, every task
# spec field is declared and documented, optionsFrom fills a picker from a named source,
# showIf conditions name a param that exists, a param that says segmentation reads
# SEGMENTATIONS, a picker gates on `labels` only when the task needs the MASK, every task
# param carries a tip, task spec copy follows the house style, and run_stats. Extracted
# from suite.jl to keep it small enough to merge without EOF conflicts on every append.
# The extracted file loads inside this file's aggregating testset scope, so any helpers
# defined earlier in suite.jl are still in scope (lexical include).

@testset "numeric param ranges are plausible" begin
    RATIO_MAX = 50
    # A param whose range genuinely spans orders of magnitude. Empty on purpose: before adding one,
    # check that the DEFAULT isn't the thing that's wrong.
    ALLOWED_WIDE = String[]

    nums = Tuple[]
    each_spec() do f, spec
        each_spec_param(spec_get(spec, "params")) do p, _
            String(something(spec_get(p, "type"), "")) in ("int", "float") || return
            push!(nums, (f, String(something(spec_get(p, "key"), "?")),
                         spec_get(p, "min"), spec_get(p, "max"),
                         spec_get(p, "step"), spec_get(p, "default")))
        end
    end
    @test length(nums) > 20                      # the walk found the numeric params

    # structural sanity first — these are bugs, not judgement calls
    for (f, k, mn, mx, _, def) in nums
        mn === nothing && continue
        if mx !== nothing
            @test mn <= mx || "$f/$k: min $mn > max $mx" == ""
        end
        if def !== nothing && def isa Real
            @test def >= mn                                   || "$f/$k: default $def < min $mn" == ""
            @test mx === nothing || def <= mx                 || "$f/$k: default $def > max $mx" == ""
        end
    end

    # a step coarser than the whole range means the slider has one position
    coarse = ["$f/$k: step $st over range $mn..$mx" for (f, k, mn, mx, st, _) in nums
              if st !== nothing && mn !== nothing && mx !== nothing && st > (mx - mn)]
    @test isempty(coarse)

    # …then the judgement call, as a loose bound
    wide = ["$f/$k: max $mx is $(round(mx / def, digits = 1))× the default $def"
            for (f, k, _, mx, _, def) in nums
            if mx !== nothing && def isa Real && def > 0 && mx / def > RATIO_MAX &&
               !("$f/$k" in ALLOWED_WIDE)]
    @test isempty(wide)
end

@testset "task spec tips stay short" begin
    COPY_MAX = 90
    ALLOWED = String[]

    # `tip`s nest inside `section`/`group` params, so recurse.


    # A trailing dot here is an abbreviation, not a sentence end ("e.g. HMM state").
    ABBREV = r"(?:^|[\s(])(?:e\.g|i\.e|etc|vs|cf|approx|fig|no)\.$"i
    function multi_sentence(s)
        for m in eachmatch(r"\S*\.\s+(?=[A-Z(])", s)
            occursin(ABBREV, rstrip(m.match)) || return true
        end
        false
    end

    tips = Tuple{String,String}[]
    nspecs = 0
    each_spec() do f, spec
        nspecs += 1
        each_spec_param(spec_get(spec, "params")) do p, _
            t = spec_get(p, "tip")
            t isa AbstractString && push!(tips, (f, join(split(String(t)), " ")))
            # `tips: [{text, requires?}]` — image-dependent variants of `tip`. Each entry's text is a
            # tip in its own right and gets the same length + house-style check.
            ts = spec_get(p, "tips")
            if ts isa AbstractVector
                for entry in ts
                    entry isa AbstractDict || continue
                    et = spec_get(entry, "text")
                    et isa AbstractString && push!(tips, (f, join(split(String(et)), " ")))
                end
            end
        end
    end

    @test nspecs > 20                       # the walk found the specs
    @test length(tips) > 100                # ...and their tips

    too_long = ["$f: [$(length(t))] $t" for (f, t) in tips
                if length(t) > COPY_MAX && !(t in ALLOWED)]
    @test isempty(too_long)

    two_sentence = ["$f: $t" for (f, t) in tips if multi_sentence(t) && !(t in ALLOWED)]
    @test isempty(two_sentence)
end

# ── UI copy COVERAGE: every task param carries a `tip` ────────────────────────────────────────
#
# The testset above polices tips that EXIST. This one polices the ones that don't. `docs/UI.md`
# asks for CellProfiler-style tip DENSITY — every setting explains itself on hover — and until
# this existed nothing could see a gap: `branching.json` shipped **twelve** parameters with no
# tip at all, so the form read "Flatten Z" / "Pre-dilation" / "Anisotropy box size (px)" with no
# way to find out what any of them did short of reading the Python runner.
#
# Presence is the half a machine can decide; whether a tip is the RIGHT tip stays a review
# question, exactly as the length ratchet can't tell you a short line is a good line. The
# frontend half of this rule — settable controls with no `v-tooltip` — is `uncoveredControls`
# in `frontend/src/utils/uiCopy.ts`, checked in `uiCopy.test.ts`.
#
# SECTIONS AND GROUPS ARE EXEMPT. They are container headers ("Advanced", "Filters"), not inputs
# — a user can't set them to anything, and requiring one would buy 18 tips saying "advanced
# options". Their CHILDREN are checked like any other param.
@testset "a handler fallback never contradicts its spec default" begin
    # `run_task` now applies the spec's `default` before calling `_run_task`, so a handler's own
    # `get(params, "k", d)` fallback is unreachable for any declared param — dead weight, not a second
    # answer. It stops being harmless the moment the two DISAGREE, because then the form promises one
    # number and a REPL/chain/MCP run uses another. Five did, and each was a real production
    # divergence: clustTracks.minTracklength 1 vs 5, opticalFlow.trainRatio 1.0 vs 0.8,
    # coastal.labelSmoothing 0.0 vs 0.5, contactsMeshes.maxContactDist 10.0 vs 5,
    # track_measures.forceRecompute false vs true.
    #
    # Deliberately a TEXT scan of the handler sources: the alternative is running every task. Only
    # literal scalars are compared — a computed fallback is a different thing and is skipped.
    root = dirname(dirname(pathof(Cecelia)))
    norm(x) = x isa AbstractString ? strip(String(x), ['"']) :
              x isa Bool ? string(x) :
              x isa Number ? string(float(x)) : nothing
    bad = String[]
    for (rootdir, _, files) in walkdir(joinpath(root, "src", "tasks")), f in files
        endswith(f, ".json") || continue
        spec = try JSON3.read(read(joinpath(rootdir, f), String), Dict{String,Any}) catch; continue end
        haskey(spec, "params") || continue
        jl = joinpath(rootdir, replace(f, ".json" => ".jl"))
        isfile(jl) || continue
        src = read(jl, String)
        defaults = Dict{String,Any}()
        walk(ps) = ps isa AbstractVector && for q in ps
            q isa AbstractDict || continue
            # A per-param `requires.axes` gate can DROP the key from the effective run
            # (`_apply_param_requires`), so the handler's fallback here is deliberately the "off"
            # value rather than the visible spec default — the guard IS the reason they differ.
            # Same reason `showIf`-only params are already exempt (their fallback is a state the
            # form no longer names). Skip these keys from the equality check.
            if !haskey(q, "requires")
                haskey(q, "default") && (defaults[string(get(q, "key", ""))] = q["default"])
            end
            walk(get(q, "params", nothing))
        end
        walk(spec["params"])
        for (key, dflt) in defaults
            want = norm(dflt)
            isnothing(want) && continue                      # arrays/objects: not a literal fallback
            for m in eachmatch(Regex("get\\(params, \"$(key)\", ([^)]+)\\)"), src)
                got_raw = strip(m.captures[1])
                got = occursin(r"^[-0-9.]+$", got_raw) ? string(parse(Float64, got_raw)) :
                      got_raw in ("true", "false") ? got_raw :
                      startswith(got_raw, "\"") ? strip(got_raw, ['"']) : nothing
                isnothing(got) && continue                   # computed fallback — a different thing
                got == want || push!(bad, "$(basename(jl)): $key — handler $got_raw, spec $(dflt)")
            end
        end
    end
    @test isempty(bad) || (@info "handler fallback contradicts the spec default" bad; false)
end

@testset "every task spec field is declared and documented" begin
    # Spec fields drift in BOTH directions, so this checks both.
    #
    #   forward  — a field is added to `ParamDef` and rendered, and `docs/MODULES.md` never hears of
    #              it. The reference silently stops being the reference.
    #   backward — a spec declares a field no consumer reads. `clustPops/cluster.json` carried
    #              `"includeChannels": true` for a `labelPropsColsSelection`; nothing in the form
    #              read it, in Julia or in TS (the only match was `napariOverlays.ts`, an unrelated
    #              movie-overlay concept). A spec that declares something nobody reads is a lie about
    #              the form, and it reads as intent to whoever copies the spec next.
    #
    # "Read by a consumer" is either half of the contract: declared on the frontend's `ParamDef`, or
    # read by Julia — `hideInComposite` is server-only and legitimately absent from `ParamDef`.
    root      = dirname(dirname(dirname(pathof(Cecelia))))
    types_ts  = read(joinpath(root, "frontend", "src", "tasks", "types.ts"), String)
    modules   = read(joinpath(root, "docs", "MODULES.md"), String)
    # Both `tasks/task.jl` and `api/src/routes.jl` are small aggregators now — their structural
    # spec-field references (`"composite"`, `"steps"`, `"scope"`, …) live in split fragments under
    # `tasks/task/*.jl` and `api/src/routes/*.jl` respectively.
    task_dir   = joinpath(root, "app", "src", "tasks", "task")
    routes_dir = joinpath(root, "api", "src", "routes")
    julia_src = join([read(f, String) for f in vcat(
                        joinpath(root, "app", "src", "tasks", "task.jl"),
                        filter(f -> endswith(f, ".jl"), readdir(task_dir;   join=true)),
                        joinpath(root, "api", "src", "routes.jl"),
                        filter(f -> endswith(f, ".jl"), readdir(routes_dir; join=true)))], "\n")

    # Structural keys of the params array itself, not fields a spec author sets on a param.
    STRUCTURAL = Set(["key", "label", "type", "default", "\$include"])
    fields = Set{String}()
    each_spec() do _, spec
        walk(ps) = ps isa AbstractVector && for q in ps
            q isa AbstractDict || continue
            union!(fields, string.(keys(q)))
            walk(get(q, "params", nothing))
        end
        walk(get(spec, "params", nothing))
    end

    undeclared = String[]; undocumented = String[]
    for f in sort(collect(setdiff(fields, STRUCTURAL)))
        occursin(Regex("^\\s*$(f)\\??:", "m"), types_ts) ||
            occursin("\"$f\"", julia_src) || push!(undeclared, f)
        occursin("`$f`", modules) || push!(undocumented, f)
    end
    @test isempty(undeclared)   || (@info "spec field read by nothing" undeclared; false)
    @test isempty(undocumented) || (@info "spec field absent from docs/MODULES.md" undocumented; false)
end

@testset "optionsFrom fills a picker from a named source" begin
    # Three tasks each carried twenty lines of identical dict-walking to do this — cellpose, coastal
    # and opticalFlow.train — differing only in which lister they called. The point for plugins: a
    # plugin author ships JSON and a task .jl, so offering a model vault used to mean writing a Julia
    # hook. Resolved for every task now, before the dispatch hook.
    flat(spec) = begin
        out = Dict{String,Any}()
        go(ps) = ps isa AbstractVector && for q in ps
            q isa AbstractDict || continue
            out[string(get(q, "key", ""))] = q
            go(get(q, "params", nothing))
        end
        go(get(spec, "params", nothing)); out
    end

    cp = flat(Cecelia._task_spec(Cecelia._task_from_fun_name("segment.cellpose")))["model"]
    @test cp["optionsFrom"] == "cellposeModels"
    @test !isempty(cp["options"])                                    # the built-ins are always there
    @test Set(String(m.name) for m in Cecelia.list_cellpose_models()) ==
          Set(String(o["value"]) for o in cp["options"])

    # Coastal APPENDS the vault to the literal options its spec declares, so "None" stays first and
    # stays selectable. The vault is empty until the user trains something, and an empty state should
    # be a legible choice — not a select that rejects everything including its own default.
    co = flat(Cecelia._task_spec(Cecelia._task_from_fun_name("segment.coastal")))["model"]
    @test co["options"][1]["value"] == "" && co["options"][1]["label"] == "None"
    @test length(co["options"]) == 1 + length(Cecelia.list_coastal_models())

    # value == label here: the user types the stem, so the suggestion IS what goes in the field.
    tr = flat(Cecelia._task_spec(Cecelia._task_from_fun_name("opticalFlow.train")))["modelName"]
    @test all(o -> o["label"] == o["value"], tr["options"])

    # …and every picker filled this way lists each value ONCE. The append is what makes coastal's
    # "None" work, and it is also what duplicated cellpose's built-ins: the spec declared `cpsam_v2`
    # and `cpsam` as literals while `cellposeModels` enumerates the same tuple, so the Model select
    # showed both twice. Neither the `issubset` check above nor
    # a `Set ==` comparison can see a duplicate — both collapse them — which is why it shipped.
    for (fn, key) in (("segment.cellpose", "model"), ("segment.coastal", "model"),
                      ("opticalFlow.train", "modelName"))
        vals = [string(o["value"])
                for o in flat(Cecelia._task_spec(Cecelia._task_from_fun_name(fn)))[key]["options"]]
        @test length(vals) == length(unique(vals))
    end

    # A declared option that the source ALSO enumerates keeps the SPEC's wording and position: the
    # label is the author's, and order is what keeps a "None" first.
    dup = Dict{String,Any}("params" => Any[Dict{String,Any}(
        "key" => "k", "type" => "select", "optionsFrom" => "cellposeModels",
        "options" => Any[Dict{String,Any}("value" => "cpsam_v2", "label" => "Mine")])])
    Cecelia._apply_options_from!(dup)
    opts = dup["params"][1]["options"]
    @test count(o -> string(o["value"]) == "cpsam_v2", opts) == 1
    @test string(first(opts)["label"]) == "Mine"

    # An unregistered name leaves the declared options alone rather than emptying the picker — a
    # typo in a spec must not silently produce a control nobody can choose anything in.
    spec = Dict{String,Any}("params" => Any[Dict{String,Any}(
        "key" => "k", "type" => "select", "optionsFrom" => "nope",
        "options" => Any[Dict{String,Any}("value" => "a", "label" => "A")])])
    Cecelia._apply_options_from!(spec)
    @test [o["value"] for o in spec["params"][1]["options"]] == ["a"]
end

@testset "showIf conditions name a param that exists" begin
    # `showIf` is the DECLARATIVE half of "this param does not apply here": a condition on the form,
    # beside the param it is about, so a plugin author never writes Julia to make a field disappear.
    # (The other half — a condition needing a file read, like "this XML export has no columns" —
    # cannot be a spec field and stays a server hook setting `hidden`.)
    #
    # Its one silent failure mode: name a key that is not in the spec and the condition can never be
    # satisfied, so the param is hidden FOREVER with no error anywhere. A typo costs a whole control.
    bad = String[]
    each_spec() do label, spec
        present = Set{String}()
        conds   = Tuple{String,String}[]
        function walk(ps)
            ps isa AbstractVector || return
            for q in ps
                q isa AbstractDict || continue
                push!(present, string(get(q, "key", "")))
                cond = get(q, "showIf", nothing)
                cond isa AbstractDict &&
                    for k in keys(cond); push!(conds, (string(get(q, "key", "?")), string(k))); end
                walk(get(q, "params", nothing))
            end
        end
        walk(get(spec, "params", nothing))
        # Sub-params of a section are stored FLAT in the value dict, so a condition may cross that
        # boundary in either direction — which is why membership is checked against the whole spec.
        for (owner, k) in conds
            k ∈ present || push!(bad, "$label: $owner showIf → '$k', which no param declares")
        end
    end
    @test isempty(bad) || (@info "showIf names a param that does not exist" bad; false)
end

@testset "a param that says segmentation reads SEGMENTATIONS" begin
    # `valueNameSelection` defaults to `filepaths` — image VERSIONS — when `field` is omitted, and
    # that default is right for the seven built-ins that omit it ("Image to segment", "Images to
    # train on"). It is silent, though: the importer's "Segmentation" picker offered smoothed,
    # afCorrected, driftCorrected — image versions with segmentation's label on them, and nothing
    # anywhere said so. Three example specs had the same bug, in the files people copy.
    #
    # The rule is narrow on purpose: only a param whose own label or tip CALLS ITSELF a segmentation
    # or a label set must read `labels`. "Image to segment" is not a claim about the picker's
    # contents, so it stays exempt.
    bad = String[]
    each_spec() do label, spec
        function walk(ps)
            ps isa AbstractVector || return
            for q in ps
                q isa AbstractDict || continue
                if get(q, "type", "") == "valueNameSelection"
                    txt = lowercase(string(get(q, "label", ""), " ", get(q, "tip", "")))
                    claims = occursin("segmentation", txt) || occursin("label set", txt)
                    claims && get(q, "field", "") != "labels" &&
                        push!(bad, "$label → $(get(q, "key", "?")) (field=$(get(q, "field", "omitted")))")
                end
                walk(get(q, "params", nothing))
            end
        end
        walk(get(spec, "params", nothing))
    end
    @test isempty(bad) || (@info "valueNameSelection claims a segmentation but reads image versions" bad; false)
end

@testset "a picker gates on `labels` only when the task needs the MASK" begin
    # The mirror of the testset above, and the one that actually cost a workflow. `labels` and
    # `label_props` are two INDEPENDENT ccid.json registries: mask pixels vs a measurement table. A
    # directly-imported track set registers only the second — there are no mask pixels to register —
    # so `field: "labels"` silently drops exactly the sets `ccia-importTracks` creates.
    #
    # Every track-CONSUMING task reads the h5ad and nothing else, and all three gated on `labels`
    # anyway: `tracking.track_measures`, `tracking.correct`, and the plugin's
    # `trackTools.cumulativeChange` (which is where spotted it, from the word "Segmentation"
    # on a form that wanted tracks). You could import tracks and then not measure them, with nothing
    # saying why the set was missing from the picker.
    #
    # So the rule is what the HANDLER does, not what the label says: if neither the task's `.jl` nor
    # its `_run.py` reaches mask pixels, its picker must not gate on `labels`.
    MASK_ACCESS = r"img_labels_path|img\.labels|labelsPath|open_labels|labels_path|zarr"

    # Gates on `labels`, reads no mask, and that is DELIBERATE — with the reason, because a bare
    # exemption list is how a real hit hides.
    #
    # `tracking.bayesian_tracking` reads centroids out of the h5ad and writes lineage columns back,
    # so it touches no mask either. It stays on `labels` because it PRODUCES tracks rather than
    # consuming them: every trackable set today comes from a segmentation (the importer only writes
    # sets that are already tracked), so `labels` is not currently narrower than the truth. Revisit
    # if anything ever registers untracked detections without a mask.
    ALLOWED = Set(["bayesian_tracking.json"])

    bad = String[]
    for dir in spec_dirs(), (root, _, files) in walkdir(dir), fname in files
        endswith(fname, ".json") || continue
        basename(root) == "plotDefinitions" && continue
        fname ∈ ALLOWED && continue
        spec = try JSON3.read(read(joinpath(root, fname), String)) catch; continue end
        spec isa AbstractDict || continue
        base = splitext(fname)[1]
        src  = join([isfile(joinpath(root, base * e)) ? read(joinpath(root, base * e), String) : ""
                     for e in (".jl", "_run.py")], "\n")
        occursin(MASK_ACCESS, src) && continue      # genuinely needs the mask
        function walk(ps)
            ps isa AbstractVector || return
            for q in ps
                q isa AbstractDict || continue
                get(q, "type", "") == "valueNameSelection" && get(q, "field", "") == "labels" &&
                    push!(bad, "$(joinpath(basename(root), fname)) → $(get(q, "key", "?"))")
                walk(get(q, "params", nothing))
            end
        end
        walk(get(spec, "params", nothing))
    end
    @test isempty(bad) ||
        (@info "picker gates on `labels` but the task never reads a mask — an imported " *
               "points-only set can never be picked; use `labelPropsNames`" bad; false)
end

@testset "every task param carries a tip" begin
    CONTAINER = ("section", "group")
    # A param whose label genuinely IS the whole explanation. Empty on purpose — same reason as
    # the length ratchet's: an allow-list that starts populated never gets emptied. Before adding
    # one, try writing the tip; it is nearly always shorter than the argument for skipping it.
    ALLOWED_NO_TIP = String[]

    # Collects every SETTABLE param (container children included), flagged tipped or not, so the
    # guard below can assert the walk actually found something — a silently empty walk would
    # otherwise report perfect coverage, which is how the QC scraper once lost 40 strings.


    params = Tuple{String,String,Bool}[]
    each_spec() do f, spec
        each_spec_param(spec_get(spec, "params")) do p, _
            ptype = String(something(spec_get(p, "type"), ""))
            (haskey(p, :key) || haskey(p, "key")) && !(ptype in CONTAINER) || return
            tip = spec_get(p, "tip")
            tipped = !isempty(strip(tip isa AbstractString ? String(tip) : ""))
            # `tips: [{text, requires?}]` — image-dependent variants. Any entry with a non-empty
            # `text` covers the requirement; the renderer picks the first that matches, and a
            # deliberate no-match (a T-only tip on a still) is the honest empty state.
            if !tipped
                tips = spec_get(p, "tips")
                if tips isa AbstractVector
                    for entry in tips
                        entry isa AbstractDict || continue
                        txt = spec_get(entry, "text")
                        if txt isa AbstractString && !isempty(strip(String(txt)))
                            tipped = true; break
                        end
                    end
                end
            end
            push!(params, (f, String(something(spec_get(p, "key"), "?")), tipped))
        end
    end

    @test length(params) > 150              # the walk found the params it is meant to police

    missing_tips = ["$f: $k" for (f, k, tipped) in params
                    if !tipped && !("$f: $k" in ALLOWED_NO_TIP)]
    @test isempty(missing_tips)
end

# ── UI copy house style: task-spec `label` + `tip` ────────────────────────────────────────────
#
# The Julia half of `docs/UI.md` → *House style*, mirroring the frontend checks in
# `frontend/src/utils/uiCopy.test.ts`. Split for the same reason the `tip` budget is: task specs
# are backend files and the frontend never holds a copy of one.
#
# This is the surface that actually drifted. Nothing could see the whole corpus at once, so the
# two halves of the app diverged along the storage boundary — 14 task labels went Title Case
# ("Bayesian Tracking", "Drift Correction") while every frontend label stayed sentence case, and
# all 164 tips grew a trailing period that no tooltip in the frontend had. `pixi run ui-copy`
# found it; this keeps it found. Exact allow-lists, not counts.
@testset "task spec copy follows the house style" begin
    ALLOWED_TITLE_CASE = String[]      # a label that is really a proper name
    ALLOWED_TRAILING_PERIOD = String[] # a `tip` that is genuinely a sentence

    # `@testset` bodies are their own scope, so the collector above isn't visible here — this one
    # pulls both keys in a single walk rather than re-deriving two nearly identical recursions.


    # Mirrors `isTitleCase` in uiCopy.ts — see there for why the allowances exist. A capital is
    # only evidence of Title Case when the word isn't expected to carry one: acronyms, single
    # letters, known proper nouns, and the first word after a separator ("Spatial / Time").
    PROPER = r"^(?:Cellpose|Bayesian|Dask|Cecelia|Leiden|Python|Julia|ImageJ|Fiji|OME|Napari|Zarr|Pluto|Rscript)$"
    SEPARATOR = r"^[/+&–—|]+$"
    expected_cap(w) = occursin(r"^[A-Z0-9+&/–-]+$", w) || length(w) == 1 || occursin(PROPER, w)
    function title_case(text)
        words = [w for w in split(text) if occursin(r"^[A-Za-z]", w) || occursin(SEPARATOR, w)]
        length(words) < 2 && return false
        judged = [(w = words[i], after_sep = occursin(SEPARATOR, words[i - 1]))
                  for i in 2:length(words) if !occursin(SEPARATOR, words[i])]
        isempty(judged) && return false
        any(j -> occursin(r"^[A-Z]", j.w) && !j.after_sep && !expected_cap(j.w), judged) &&
            all(j -> occursin(r"^[A-Z]", j.w) || j.after_sep || expected_cap(j.w), judged)
    end

    labels, tips2 = Tuple{String,String}[], Tuple{String,String}[]
    each_spec() do f, spec
        l = get(spec, :label, nothing)
        l isa AbstractString && push!(labels, (f, join(split(String(l)), " ")))
        each_spec_param(spec_get(spec, "params")) do p, _
            l = spec_get(p, "label")
            l isa AbstractString && push!(labels, (f, join(split(String(l)), " ")))
            t = spec_get(p, "tip")
            t isa AbstractString && push!(tips2, (f, join(split(String(t)), " ")))
        end
    end

    @test length(labels) > 150              # the walk found task + param labels

    titled = ["$f: $l" for (f, l) in labels if title_case(l) && !(l in ALLOWED_TITLE_CASE)]
    @test isempty(titled)

    # `…`/`...` is a continuation, not a sentence end.
    dotted = ["$f: $t" for (f, t) in tips2
              if occursin(r"[^.]\.$", t) && !(t in ALLOWED_TRAILING_PERIOD)]
    @test isempty(dotted)

    # Only the words with a decided winner. Create/Add, Delete/Remove and Run/Start are NOT
    # synonyms (see the vocabulary table in docs/UI.md) and are deliberately absent.
    BANNED = ["Choose" => "Select", "Pick" => "Select", "Display" => "Show",
              "Execute" => "Run", "Modify" => "Edit", "Discard" => "Remove"]
    wrong_verb = ["$f: \"$s\" — use $good" for (f, s) in vcat(labels, tips2)
                  for (bad, good) in BANNED if occursin(Regex("\\b$bad\\b", "i"), s)]
    @test isempty(wrong_verb)
end

# ── Stats module (docs/todo/STATS_ANNOTATIONS_PLAN.md) ─────────────────────
#
# Pins the glue between `run_stats` and HypothesisTests.jl. We don't test the underlying
# test math (that's HypothesisTests' own suite) — we test that the API is wired correctly:
# test dispatch, insertion order preserved (via Vector{Pair}), pairwise Bonferroni-adjusted,
# ns/star ladder, error handling for empty and too-few groups.
@testset "run_stats" begin
    # Two clearly-different groups → mannwhitney by default; p is very small; significance
    # ladder is at least ** (matches STATS_ANNOTATIONS_PLAN.md → S0-1).
    @testset "2 groups auto → mannwhitney" begin
        r = Cecelia.run_stats(["WT" => [1.0,2,3,4,5], "KO" => [10.0,11,12,13,14]])
        @test r.test == :mannwhitney
        @test r.groups == ["WT", "KO"]
        @test r.n == [5, 5]
        @test r.means[1] ≈ 3.0 && r.means[2] ≈ 12.0
        @test r.medians[1] ≈ 3.0 && r.medians[2] ≈ 12.0
        @test r.p_value < 0.05
        @test r.significance in ("*", "**", "***", "****")
        @test occursin("Mann-Whitney", r.method_note)
        @test isempty(r.comparison_pairs)   # omnibus IS the pair for 2 groups
    end

    # `auto` also has to say WHY. The UI showed the resolved test name and nothing else, so a user
    # had no way to know the basis — and deriving the explanation in the frontend would fork the
    # rule (change `_auto_test` and the tooltip would quietly keep claiming the old basis).
    @testset "auto states its basis; a NAMED test states none" begin
        two = Cecelia.run_stats(["WT" => [1.0,2,3], "KO" => [9.0,10,11]])
        @test occursin("2 groups", two.auto_reason)
        @test occursin("Mann-Whitney", two.auto_reason)
        three = Cecelia.run_stats(["A" => [1.0,2,3], "B" => [9.0,10,11], "C" => [20.0,21,22]])
        @test occursin("3 groups", three.auto_reason)
        @test occursin("Kruskal-Wallis", three.auto_reason)
        # both auto choices are rank-based — that's the reassurance the note has to carry, since
        # `auto` never runs a normality check
        @test occursin("rank-based", two.auto_reason) && occursin("rank-based", three.auto_reason)
        # nothing was chosen for the user, so there is nothing to explain
        @test isempty(Cecelia.run_stats(["A" => [1.0,2,3], "B" => [9.0,10,11]]; test=:ttest).auto_reason)
        @test isempty(Cecelia.run_stats(["A" => [1.0,2,3], "B" => [9.0,10,11]]; test=:mannwhitney).auto_reason)
        # the reason must name the test that actually ran — one rule, not two
        for n in (2, 3, 7)
            @test occursin(n == 2 ? "Mann-Whitney" : "Kruskal-Wallis", Cecelia._auto_reason(n))
            @test occursin("$(n) groups", Cecelia._auto_reason(n))
        end
        # …and it reaches the wire under `autoReason`
        d = Cecelia._stats_result_dict(two)
        @test d["autoReason"] == two.auto_reason
        @test isempty(Cecelia._stats_result_dict(
            Cecelia.run_stats(["A" => [1.0,2,3], "B" => [9.0,10,11]]; test=:ttest))["autoReason"])
    end

    # Two identical groups → p ≈ 1, "ns".
    @testset "identical groups → ns" begin
        r = Cecelia.run_stats(["A" => [1.0,2,3,4,5], "B" => [1.0,2,3,4,5]])
        @test r.p_value > 0.9
        @test r.significance == "ns"
    end

    # Welch's t-test opt-in — different method note, still small p on separated data.
    @testset "ttest opt-in" begin
        r = Cecelia.run_stats(["A" => [1.0,2,3,4], "B" => [10.0,11,12,13]]; test=:ttest)
        @test r.test == :ttest
        @test occursin("t-test", r.method_note)
        @test r.p_value < 0.05
    end

    # Three groups → kruskal by default, pairs are populated with Bonferroni-adjusted values.
    @testset "3 groups → kruskal + pairwise" begin
        r = Cecelia.run_stats([
            "A" => [1.0,2,3,4,5], "B" => [10.0,11,12,13,14], "C" => [20.0,21,22,23,24]])
        @test r.test == :kruskal
        @test r.groups == ["A", "B", "C"]
        @test occursin("Kruskal-Wallis", r.method_note)
        @test length(r.comparison_pairs) == 3   # (A,B), (A,C), (B,C)
        for (a, b, p_adj, sig) in r.comparison_pairs
            @test p_adj >= 0.0 && p_adj <= 1.0
            @test sig in ("ns", "*", "**", "***", "****")
        end
        # A vs C is the widest gap → definitely significant post-Bonferroni.
        ac = only(p for (a, b, p, _) in r.comparison_pairs if a == "A" && b == "C")
        @test ac < 0.05
    end

    # ANOVA opt-in with 3 groups.
    @testset "anova opt-in (3 groups)" begin
        r = Cecelia.run_stats([
            "A" => [1.0,2,3,4,5], "B" => [5.0,6,7,8,9], "C" => [10.0,11,12,13,14]];
            test=:anova)
        @test r.test == :anova
        @test occursin("ANOVA", r.method_note)
        @test r.p_value < 0.05
    end

    # Insertion order preserved (Vector of Pairs guarantees it — this asserts we don't sort).
    @testset "group order preserved" begin
        r = Cecelia.run_stats(["Z" => [1.0,2,3], "A" => [4.0,5,6], "M" => [7.0,8,9]])
        @test r.groups == ["Z", "A", "M"]
    end

    # Error paths.
    @testset "errors" begin
        @test_throws ArgumentError Cecelia.run_stats(["only" => [1.0,2,3]])
        @test_throws ArgumentError Cecelia.run_stats(["A" => Float64[], "B" => [1.0,2]])
        # 2-group tests refuse when given ≠2 groups.
        three = ["A" => [1.0,2], "B" => [3.0,4], "C" => [5.0,6]]
        @test_throws ArgumentError Cecelia.run_stats(three; test=:ttest)
        @test_throws ArgumentError Cecelia.run_stats(three; test=:mannwhitney)
        @test_throws ArgumentError Cecelia.run_stats(["A"=>[1.0,2], "B"=>[3.0,4]];
                                                    test=:notarealtest)
    end
end
