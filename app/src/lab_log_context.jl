# Auto-generated LAB-LOG CONTEXT — the app's own account of what the user has done, written into the
# lab log as `[Cecelia]` entries so the notebook records the *what* (activity) alongside the human
# *why*. See docs/ai-assist/LAB-LOG.md.
#
# Design: **one ROLLING DAILY block, regenerated from source.** `capture_context!` maintains a single
# `[Cecelia]` block PER DAY, rewritten in place as the day's activity accrues (see
# `upsert_daily_context_block!` — the append-only guarantee is kept for the human record, relaxed only
# for this derived block). This is what tames the redundancy: N long-running tasks that finish hours
# apart on the same day fold into ONE block regardless of how often capture fires (it used to append a
# fresh block per capture → one block per task). Sources:
#   • tasks   — the per-image run logs (run_log.jl): every task whose `at` falls on the block's day
#               (grouped by module category, collapsed across images; per-image/per-channel repetition
#               of the same QC finding folds into one detail line).
#   • gating  — a per-pop fingerprint (name + a hash of the membership definition) diffed against the
#               START-OF-DAY snapshot → populations added/removed and which pops' definitions changed.
#               Reads the gating JSON files directly (source of truth).
#   • exclusions — the set of excluded image uids, diffed vs the start-of-day snapshot.
# The day baseline (`dayDate`/`dayGating`/`dayExcluded`) lives in `settings/lab-log-context.json` and
# resets when the day rolls over, so within a day the gating/exclusion change is cumulative-net and the
# first capture of a day seeds silently (no retro dump); tasks need no baseline (regenerated per day).
#
# Not captured / to tune as we go: gate-change *magnitude* (e.g. % of cells shifted) — only that a
# gate changed, by population. Threshold/geometry values themselves are intentionally out (that's the
# undo-list we don't want). A day with ZERO captures is not retro-summarised (activity stays in the run
# logs); with per-task auto-capture on, each active day gets its block.

import Dates

const CONTEXT_AUTHOR = "Cecelia"

_context_state_path(proj::CciaProject)::String =
    joinpath(proj.root, "settings", "lab-log-context.json")

# recursively convert JSON3 values to native String-keyed Dicts / Vectors (avoids the Symbol-key +
# JSON3.Object-isa-Dict gotchas when diffing a re-read snapshot).
_native(x) = x
_native(x::JSON3.Object) = Dict{String,Any}(String(k) => _native(v) for (k, v) in x)
_native(x::JSON3.Array)  = Any[_native(v) for v in x]

function _read_context_state(proj::CciaProject)::Dict{String,Any}
    p = _context_state_path(proj)
    isfile(p) || return Dict{String,Any}()
    try
        s = _native(JSON3.read(read(p, String)))
        s isa Dict{String,Any} ? s : Dict{String,Any}()
    catch
        Dict{String,Any}()
    end
end

function _write_context_state!(proj::CciaProject, state::AbstractDict)
    p = _context_state_path(proj)
    mkpath(dirname(p))
    open(p, "w") do io
        JSON3.write(io, state)
    end
end

# Human-readable image list, capped so a big cohort doesn't produce a wall of names.
function _summarise_images(names::Vector{String})::String
    u = unique(names)
    length(u) > 6 ? string(join(u[1:6], ", "), ", +", length(u) - 6, " more") : join(u, ", ")
end

# ── category taxonomy ──────────────────────────────────────────────────────────────
# The digest is CATEGORY-CENTRIC, using the SAME category tags the task manager shows (the task
# specs' `category` field — "Segment", "Tracking", "Clustering", …) so labelling is consistent and
# nothing has to be maintained here: a task's category is read from its spec, so new tasks/categories
# appear automatically. Non-task activity maps to the nearest tag: gate pops → "Gating", cluster pops
# → "Clustering" (matching the clustPops/clustTracks task category), exclusions → "Manage images".
# Digest order follows these categories; extras (custom modules) are appended alphabetically.
const CATEGORY_GATING = "Gating"
const CATEGORY_IMAGES = "Manage images"
# preferred display/mute order; categories not listed are appended alphabetically (stays general).
const _CATEGORY_ORDER = ("import", "Manage images", "Cleanup", "Edit", "Segment",
                         CATEGORY_GATING, "Tracking", "Clustering", "Behaviour")

# a task's category, straight from its spec (the task-manager tag). Falls back to the fun's namespace.
function _category_of_fun(fun::AbstractString)::String
    task = try _task_from_fun_name(String(fun)) catch; nothing end
    if task !== nothing
        spec = try _task_spec(task) catch; nothing end
        spec !== nothing && haskey(spec, "category") && return String(spec["category"])
    end
    uppercasefirst(String(split(fun, ".")[1]))
end

# `region` (spatial region-clustering pops) is cluster-family — grouped with Clustering in the digest.
_category_of_pop_type(pt::AbstractString)::String =
    String(pt) in ("clust", "trackclust", "region") ? "Clustering" : CATEGORY_GATING

# rank for digest ordering (index in _CATEGORY_ORDER; unknown categories sort last, then alphabetical)
_category_rank(c::AbstractString)::Int = (i = findfirst(==(String(c)), _CATEGORY_ORDER); i === nothing ? typemax(Int) : i)

# "N images" for a big cohort, else the uids — the collapse that turns per-image repetition into one.
# Images are referenced by stable uid (names change); the panel resolves uid→name on demand.
_where(uids)::String = (n = length(uids); n <= 2 ? join(sort(collect(uids)), ", ") : "$n images")

# ── tasks (run logs) → per-module items ───────────────────────────────────────────
# The digest is DAY-scoped: a run-log entry belongs to the block for the local date in its `at`
# timestamp. Both `at` (run_log.jl, `Dates.now()`) and the capture date (`Dates.today()`) are the
# server's LOCAL time, so grouping by the `yyyy-mm-dd` prefix is internally consistent — the trap to
# avoid is introducing a UTC clock on either side. The `category.` prefix is dropped from the fun (the
# module header carries it): `segment.cellpose` → "cellpose on 5 images".

# Each `warn` finding in an image's (fun, vn) QC doc as `(code, short, channel)` — the actual
# "what's wrong" for the digest's detail lines. `channel` is the finding's `detail.channel` (an Int)
# when it carries one (per-channel findings like the import hot-pixel/clip checks), else `nothing`;
# it drives the cross-channel collapse so N per-channel findings read as one "ch a-b" line rather
# than N near-identical lines. Empty when none.
function _warn_findings(img::CciaImage, fun::AbstractString, vn::AbstractString)::Vector{Tuple{String,String,Union{Int,Nothing}}}
    doc = read_qc(img, fun, isempty(vn) ? VERSIONED_DEFAULT_VAL : vn)
    doc === nothing && return Tuple{String,String,Union{Int,Nothing}}[]
    fs = get(doc, :findings, nothing)
    fs === nothing && return Tuple{String,String,Union{Int,Nothing}}[]
    out = Tuple{String,String,Union{Int,Nothing}}[]
    for f in fs
        String(get(f, :level, "")) == "warn" || continue
        short = String(get(f, :short, ""))
        isempty(short) && continue
        push!(out, (String(get(f, :code, "")), short, _finding_channel(f)))
    end
    out
end

# a finding's `detail.channel` as an Int, or nothing (robust to JSON3 Symbol keys + numeric coercion).
function _finding_channel(f)::Union{Int,Nothing}
    d = get(f, :detail, nothing)
    d === nothing && return nothing
    c = get(d, :channel, nothing)
    c === nothing && return nothing
    try Int(c) catch; nothing end
end

# strip a leading "Channel <n>" from a finding short so per-channel findings collapse by their shared
# phrase ("Channel 0 may have a hot pixel" → "may have a hot pixel"); falls back to the short if that
# would leave nothing.
function _strip_channel(short::AbstractString)::String
    base = strip(replace(String(short), r"^Channel\s+\d+\s*" => ""))
    isempty(base) ? String(short) : base
end

# compress sorted unique ints into a compact range string: [0,1,2,3] → "0-3", [0,2,3] → "0, 2-3".
function _int_ranges(xs::Vector{Int})::String
    isempty(xs) && return ""
    parts = String[]; s = xs[1]; p = xs[1]
    for x in xs[2:end]
        x == p + 1 ? (p = x) : (push!(parts, s == p ? "$s" : "$s-$p"); s = x; p = x)
    end
    push!(parts, s == p ? "$s" : "$s-$p")
    join(parts, ", ")
end

const _SEV_RANK = Dict("ok" => 0, "warn" => 1, "fail" => 2)
const _MAX_DIGEST_DETAILS = 8   # cap the per-module finding-detail lines so a big cohort can't flood the entry

# Per-module run summary for the runs whose `at` falls on `day` ("yyyy-mm-dd"). Returns
# `(items_by_module, severity_by_module, details_by_module)`: severity is the worst outcome across
# that module's runs that day — `fail` (a run failed), else `warn` (a run produced a warn QC finding),
# else `ok` — driving the digest line's ✅/⚠️/❌ symbol. Regenerating from the run logs each capture
# (rather than diffing since a cutoff) is what makes the block a stable DAILY aggregate: N tasks that
# finished hours apart all land in the one block regardless of when capture fired.
function _task_items_by_module(proj::CciaProject, day::AbstractString)::Tuple{Dict{String,Vector{String}},Dict{String,String},Dict{String,Vector{String}}}
    by_mod_fun = Dict{String,Dict{String,Set{String}}}()   # module => fun-display => image uids
    fail_count = Dict{Tuple{String,String},Int}()          # (module, fun-display) => # failed runs
    warn_imgs  = Dict{Tuple{String,String},Set{String}}()  # (module, fun-display) => images with a warn QC finding
    # module => (code, base-short) => (channels, image uids) — grouped so per-channel/per-image
    # repetition of the SAME finding collapses to one "base — ch a-b (N images)" detail line.
    warn_det   = Dict{String,Dict{Tuple{String,String},Tuple{Set{Int},Set{String}}}}()
    mod_sev    = Dict{String,String}()                     # module => "ok"|"warn"|"fail" (worst)
    bump(mod, sev) = (get(_SEV_RANK, sev, 0) > get(_SEV_RANK, get(mod_sev, mod, "ok"), 0)) &&
                     (mod_sev[mod] = sev)
    for img in images(proj)
        for e in read_run_log(img)
            startswith(String(get(e, "at", "")), day) || continue   # this day's runs only (local date)
            fun  = String(get(e, "fun", "?"))
            disp = occursin(".", fun) ? String(split(fun, "."; limit = 2)[2]) : fun
            mod  = _category_of_fun(fun)
            vn   = String(get(e, "valueName", ""))
            push!(get!(get!(by_mod_fun, mod, Dict{String,Set{String}}()), disp, Set{String}()), img.uid)
            get!(mod_sev, mod, "ok")
            if String(get(e, "status", "done")) == "failed"
                fail_count[(mod, disp)] = get(fail_count, (mod, disp), 0) + 1   # surface failures too
                bump(mod, "fail")
            else
                findings = _warn_findings(img, fun, vn)
                if !isempty(findings)
                    push!(get!(warn_imgs, (mod, disp), Set{String}()), img.uid)   # count the flagged images
                    g = get!(warn_det, mod, Dict{Tuple{String,String},Tuple{Set{Int},Set{String}}}())
                    for (code, short, chan) in findings
                        base = chan === nothing ? short : _strip_channel(short)
                        chans, imgs = get!(g, (code, base), (Set{Int}(), Set{String}()))
                        chan === nothing || push!(chans, chan)
                        push!(imgs, img.uid)
                    end
                    bump(mod, "warn")
                end
            end
        end
    end
    out = Dict{String,Vector{String}}()
    for (mod, funs) in by_mod_fun
        items = String[]
        for disp in sort(collect(keys(funs)))
            uids = funs[disp]
            n  = length(uids)
            item = "$disp on $n image$(n == 1 ? "" : "s")"
            n <= 2 && (item *= " ($(join(sort(collect(uids)), ", ")))")   # list uids when few; for more, the count says it
            w  = length(get(warn_imgs, (mod, disp), Set{String}()))
            fc = get(fail_count, (mod, disp), 0)
            w  > 0 && (item *= " — $w flagged")   # how many banked a warn QC finding (not just that one did)
            fc > 0 && (item *= " — $fc failed")
            push!(items, item)
        end
        out[mod] = items
    end
    # per-module finding-detail lines (the actual QC text, ↳-prefixed under the line), collapsed by
    # finding across channels + images, sorted by phrase, capped so a big cohort can't flood the entry.
    details = Dict{String,Vector{String}}()
    for (mod, g) in warn_det
        rendered = String[]
        for (code, base) in sort(collect(keys(g)); by = kb -> kb[2])
            chans, imgs = g[(code, base)]
            loc  = length(imgs) <= 2 ? join(sort(collect(imgs)), ", ") : "$(length(imgs)) images"
            chstr = isempty(chans) ? "" : " — ch " * _int_ranges(sort(collect(chans)))
            push!(rendered, "↳ $base$chstr ($loc)")
        end
        details[mod] = length(rendered) > _MAX_DIGEST_DETAILS ?
            vcat(rendered[1:_MAX_DIGEST_DETAILS], ["↳ +$(length(rendered) - _MAX_DIGEST_DETAILS) more"]) :
            rendered
    end
    (out, mod_sev, details)
end

# ── gating fingerprint + diff ─────────────────────────────────────────────────────
# Per image: { "value_name|pop_type|pop_path" => defn_hash }. `defn_hash` hashes the pop's whole
# MEMBERSHIP DEFINITION, not just its gate — so a change to a gate, a filter, cluster membership, or
# any FUTURE definition field is caught. This is deliberately GENERIC (reflect over the struct's
# fields, minus a small identity/cosmetic ignore-list) rather than enumerating fields, so new pop
# types (clust/trackclust are filter-defined, gateless) and new fields need no change here — and it
# needs no per-action trigger: the snapshot is read fresh at capture time.

# Fields that identify/decorate a pop rather than define its membership — excluded so cosmetic edits
# (colour, show) and identity (name/path/parent, in the key) don't read as "changed".
const _POP_IGNORE_FIELDS = (:name, :path, :parent, :pop_type, :value_name, :colour, :show, :transient)

# Stable hash of everything that DEFINES a pop's membership (gate via its canonical spec; all other
# non-ignored fields as-is). Reflection over fieldnames → new fields are covered automatically.
function _pop_defn_hash(pop)::String
    d = Dict{String,Any}()
    for f in fieldnames(typeof(pop))
        f in _POP_IGNORE_FIELDS && continue
        v = getfield(pop, f)
        d[String(f)] = (f === :gate) ? (v === nothing ? nothing : gate_spec(v)) : v
    end
    string(hash(JSON3.write(d)))
end

function _gating_fingerprint(proj::CciaProject)::Dict{String,Any}
    fp = Dict{String,Any}()
    for img in images(proj)
        gdir = gating_dir(img._dir)
        isdir(gdir) || continue
        m_all = Dict{String,String}()
        for f in readdir(gdir)
            endswith(f, ".json") || continue
            m = try
                from_tree(JSON3.read(read(joinpath(gdir, f), String), Dict{String,Any}))
            catch
                continue   # skip a corrupt/half-written map rather than fail the whole capture
            end
            for path in pop_paths(m)
                is_root(path) && continue
                m_all["$(m.value_name)|$(m.pop_type)|$path"] = _pop_defn_hash(pop_at(m, path))
            end
        end
        isempty(m_all) || (fp[img.uid] = m_all)
    end
    fp
end

_popname_of_key(k::AbstractString)::String = pop_name(String(split(k, "|")[end]))
_key_pop_type(k::AbstractString)::String   = String(split(k, "|")[2])

# Populations → per-category items. Aggregates changes ACROSS images by (category, verb, popname) →
# image-set, then collapses pops that share an image-set into one item — so a cohort-wide edit is one
# line ("redefined: Directed, Meandering, Scanning (8 images)"), not one line per image.
function _pop_items_by_module(prev::AbstractDict, cur::AbstractDict)::Dict{String,Vector{String}}
    acc = Dict{String,Dict{Symbol,Dict{String,Set{String}}}}()   # category => verb => pop => image uids
    add!(cat, verb, pop, img) = push!(
        get!(get!(get!(acc, cat, Dict{Symbol,Dict{String,Set{String}}}()), verb, Dict{String,Set{String}}()),
             pop, Set{String}()), img)
    for u in union(keys(prev), keys(cur))
        nm = String(u)   # reference images by stable uid; the panel resolves uid→name on demand
        p = get(prev, u, Dict{String,Any}())
        c = get(cur, u, Dict{String,Any}())
        for k in keys(c); haskey(p, k) || add!(_category_of_pop_type(_key_pop_type(k)), :added,   _popname_of_key(k), nm); end
        for k in keys(p); haskey(c, k) || add!(_category_of_pop_type(_key_pop_type(k)), :removed, _popname_of_key(k), nm); end
        for k in keys(c)
            (haskey(p, k) && string(p[k]) != string(c[k])) || continue
            verb = is_gating_pop_type(_key_pop_type(k)) ? :gate : :redef
            add!(_category_of_pop_type(_key_pop_type(k)), verb, _popname_of_key(k), nm)
        end
    end
    out = Dict{String,Vector{String}}()
    for (cat, byverb) in acc
        items = String[]
        for (verb, label) in ((:added, "added"), (:removed, "removed"), (:gate, "gate changed"), (:redef, "redefined"))
            haskey(byverb, verb) || continue
            byset = Dict{Vector{String},Vector{String}}()   # shared image-set → pops
            for (pop, imgs) in byverb[verb]
                push!(get!(byset, sort(collect(imgs)), String[]), pop)
            end
            for (imgs, pops) in byset
                push!(items, "$label: $(join(sort(pops), ", ")) ($(_where(imgs)))")
            end
        end
        out[cat] = items
    end
    out
end

# ── exclusions ─────────────────────────────────────────────────────────────────
_excluded_uids(proj::CciaProject)::Vector{String} =
    sort(String[img.uid for img in images(proj) if !image_included(img)])

function _exclusion_items(prev::Vector{String}, cur::Vector{String})::Vector{String}
    ps, cs = Set(prev), Set(cur)
    newly_ex = sort(String[u for u in cur if !(u in ps)])
    newly_in = sort(String[u for u in prev if !(u in cs)])
    items = String[]
    # reference images by stable uid; the panel resolves uid→name on demand
    isempty(newly_ex) || push!(items, "excluded $(_where(newly_ex))")
    isempty(newly_in) || push!(items, "re-included $(_where(newly_in))")
    items
end

"""
Upsert the ROLLING DAILY `[Cecelia]` digest for `date` — the day's activity grouped by module category
(the task-manager tags), collapsed across images. Regenerated from source each call and rewritten in
place (see `upsert_daily_context_block!`): tasks come from the run logs dated that day (so long tasks
finishing hours apart aggregate into the one block), gating/exclusion changes are the NET change since
the START of the day (a baseline that resets when the day rolls over). Returns the block on a real
change, or `nothing` when there's no (unmuted) activity or the block is unchanged from disk. The first
capture of a day seeds the gating/exclusion baseline silently (no retro dump).
"""
function capture_context!(proj::CciaProject; date::Dates.Date = Dates.today())::Union{String,Nothing}
    state    = _read_context_state(proj)
    day_str  = Dates.format(date, "yyyy-mm-dd")

    task_items, mod_sev, task_details = _task_items_by_module(proj, day_str)

    cur_gating = _gating_fingerprint(proj)
    cur_excl   = _excluded_uids(proj)

    # Day baseline: the gating/exclusion snapshot as of the START of `date`. On a new day (or the first
    # ever capture) it resets to the CURRENT world, so the day opens with no pop/exclusion deltas and
    # only genuine same-day changes accrue thereafter; within a day it is held fixed so the reported
    # change is cumulative-net over the whole day (matching how tasks aggregate).
    same_day   = String(get(state, "dayDate", "")) == day_str
    day_gating = same_day ? get(state, "dayGating", Dict{String,Any}()) : cur_gating
    day_excl   = same_day ? String[String(u) for u in get(state, "dayExcluded", String[])] : cur_excl

    pop_items  = _pop_items_by_module(day_gating, cur_gating)
    excl_items = _exclusion_items(day_excl, cur_excl)

    # merge every source into per-category item lists
    by_cat = Dict{String,Vector{String}}()
    for (cat, its) in task_items; append!(get!(by_cat, cat, String[]), its); end
    for (cat, its) in pop_items;  append!(get!(by_cat, cat, String[]), its); end
    isempty(excl_items) || append!(get!(by_cat, CATEGORY_IMAGES, String[]), excl_items)

    # persist the (possibly reset) day baseline — held across same-day captures, reset on rollover
    state["dayDate"]     = day_str
    state["dayGating"]   = day_gating
    state["dayExcluded"] = day_excl
    _write_context_state!(proj, state)

    # one bullet per category, in task-manager order. Each line leads with a traffic-light symbol
    # (✅/⚠️/❌) from the module's worst run outcome this day — so the reader sees at a glance which
    # entries need attention. Non-task categories (populations, exclusions) have no run severity ⇒ ✅
    # (informational).
    lines = String[]
    for cat in sort(collect(keys(by_cat)); by = c -> (_category_rank(c), c))
        isempty(by_cat[cat]) && continue
        sym = severity_symbol(get(mod_sev, cat, "ok"))
        push!(lines, "$sym $cat — " * join(by_cat[cat], "; "))
        # under a flagged category, spell out the actual QC findings (what's wrong) so the lab log
        # carries the real signal, not just a "N flagged" count the user has to go hover a tag for.
        append!(lines, get(task_details, cat, String[]))
    end

    isempty(lines) && return nothing
    upsert_daily_context_block!(proj, CONTEXT_AUTHOR, lines; date = date)
end
