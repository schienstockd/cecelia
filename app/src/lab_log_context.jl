# Auto-generated LAB-LOG CONTEXT — the app's own account of what the user has done, written into the
# lab log as `[Cecelia]` entries so the notebook records the *what* (activity) alongside the human
# *why*. See docs/ai-assist/LAB-LOG.md.
#
# Design: **snapshot-diff, not an edit log.** `capture_context!` reports the NET change since the last
# capture — never per-edit keystrokes — so a fiddly gating session with 50 polygon nudges that ends
# with one changed gate is ONE line, and nudges that cancel out are nothing. Sources:
#   • tasks   — the per-image run logs (run_log.jl): every task that ran (segment/track/cluster/…),
#               time-filtered by a stored cutoff.
#   • gating  — a per-pop fingerprint (name + a hash of the gate spec) diffed against the last snapshot
#               → populations added/removed and which pops' GATES changed (by name). Reads the gating
#               JSON files directly (source of truth), so it doesn't depend on the segmentation list.
#   • exclusions — the set of excluded image uids, diffed → newly excluded / re-included.
# Snapshots + cutoff live in `settings/lab-log-context.json`. The FIRST capture seeds the gating/
# exclusion baselines silently (no giant retro digest); only genuine deltas thereafter.
#
# Not captured / to tune as we go: gate-change *magnitude* (e.g. % of cells shifted) — only that a
# gate changed, by population. Threshold/geometry values themselves are intentionally out (that's the
# undo-list we don't want).

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
# Digest order + mute keys are these categories; muting is lenient (any category string).
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

_category_of_pop_type(pt::AbstractString)::String =
    String(pt) in ("clust", "trackclust") ? "Clustering" : CATEGORY_GATING

"""All digest categories the app can emit — task categories (from the registry, generic) plus the
non-task ones. Ordered by `_CATEGORY_ORDER`, extras appended. Backs the panel's per-category mutes."""
function lab_log_categories()::Vector{String}
    cats = Set{String}([CATEGORY_GATING, "Clustering", CATEGORY_IMAGES])
    # built-ins (static registry) PLUS loaded custom modules (runtime registry) — so a new task/page,
    # or a user's custom-module category (e.g. customExamples), appears automatically with no change here.
    for fun in vcat(collect(keys(_fun_name_map())), collect(_custom_task_keys()))
        push!(cats, _category_of_fun(fun))
    end
    ordered = String[c for c in _CATEGORY_ORDER if c in cats]
    vcat(ordered, sort(String[c for c in cats if !(c in _CATEGORY_ORDER)]))
end

# Categories that are OPERATIONS — an action, not a module page (crop lives in Edit; include/exclude in
# Manage images). Everything else the app emits is a module page. Small + explicit; the mute bar shows
# these as one general "Operations" group, separate from the per-module-page chips.
const _OPERATION_CATEGORIES = ("Manage images", "Edit")
_is_operation_category(c::AbstractString)::Bool = String(c) in _OPERATION_CATEGORIES

"""Module-page digest categories — everything `lab_log_categories` can emit that ISN'T an operation —
ordered by `_CATEGORY_ORDER`. Backs the mute bar's 'Module pages' group (all pages, always shown)."""
lab_log_page_categories()::Vector{String} =
    String[c for c in lab_log_categories() if !_is_operation_category(c)]

"""Operation digest categories (actions with no module page of their own — Edit, Manage images),
ordered by `_CATEGORY_ORDER`. Backs the mute bar's general 'Operations' group."""
lab_log_operation_categories()::Vector{String} =
    String[c for c in lab_log_categories() if _is_operation_category(c)]

# rank for digest ordering (index in _CATEGORY_ORDER; unknown categories sort last, then alphabetical)
_category_rank(c::AbstractString)::Int = (i = findfirst(==(String(c)), _CATEGORY_ORDER); i === nothing ? typemax(Int) : i)

# "N images" for a big cohort, else the names — the collapse that turns per-image repetition into one.
_where(names)::String = (n = length(names); n <= 2 ? join(sort(collect(names)), ", ") : "$n images")

# ── tasks (run logs) → per-module items ───────────────────────────────────────────
# Returns (module => [item…], max_at). Only run-log entries strictly after `cutoff` (ISO timestamps
# are fixed-width, so a lexical `>` is a correct "happened after"; second-granular + strict, so
# same-second-as-last-capture activity is skipped — negligible). The `category.` prefix is dropped
# from the fun (the module header carries it): `segment.cellpose` → "cellpose on 5 images".
# True when an image's (fun, value_name) QC doc carries a `warn` finding — the ⚠️ signal for a
# digest line. Cheap read of the sidecar the task already wrote this window.
function _has_warn_qc(img::CciaImage, fun::AbstractString, vn::AbstractString)::Bool
    doc = read_qc(img, fun, isempty(vn) ? VERSIONED_DEFAULT_VAL : vn)
    doc === nothing && return false
    fs = get(doc, :findings, nothing)
    fs === nothing && return false
    any(f -> String(get(f, :level, "")) == "warn", fs)
end

const _SEV_RANK = Dict("ok" => 0, "warn" => 1, "fail" => 2)

# Per-module run summary since `cutoff`. Returns `(items_by_module, severity_by_module, max_at)`:
# severity is the worst outcome across that module's runs this window — `fail` (a run failed), else
# `warn` (a run produced a warn QC finding), else `ok` — driving the digest line's ✅/⚠️/❌ symbol.
function _task_items_by_module(proj::CciaProject, cutoff::AbstractString)::Tuple{Dict{String,Vector{String}},Dict{String,String},String}
    by_mod_fun = Dict{String,Dict{String,Set{String}}}()   # module => fun-display => image names
    fail_count = Dict{Tuple{String,String},Int}()          # (module, fun-display) => # failed runs
    warn_imgs  = Dict{Tuple{String,String},Set{String}}()  # (module, fun-display) => images with a warn QC finding
    mod_sev    = Dict{String,String}()                     # module => "ok"|"warn"|"fail" (worst)
    bump(mod, sev) = (get(_SEV_RANK, sev, 0) > get(_SEV_RANK, get(mod_sev, mod, "ok"), 0)) &&
                     (mod_sev[mod] = sev)
    max_at = String(cutoff)
    for img in images(proj)
        for e in read_run_log(img)
            at = String(get(e, "at", ""))
            at > cutoff || continue
            fun  = String(get(e, "fun", "?"))
            disp = occursin(".", fun) ? String(split(fun, "."; limit = 2)[2]) : fun
            mod  = _category_of_fun(fun)
            vn   = String(get(e, "valueName", ""))
            push!(get!(get!(by_mod_fun, mod, Dict{String,Set{String}}()), disp, Set{String}()), img.name)
            get!(mod_sev, mod, "ok")
            if String(get(e, "status", "done")) == "failed"
                fail_count[(mod, disp)] = get(fail_count, (mod, disp), 0) + 1   # surface failures too
                bump(mod, "fail")
            elseif _has_warn_qc(img, fun, vn)
                push!(get!(warn_imgs, (mod, disp), Set{String}()), img.name)   # count the flagged images
                bump(mod, "warn")
            end
            at > max_at && (max_at = at)
        end
    end
    out = Dict{String,Vector{String}}()
    for (mod, funs) in by_mod_fun
        items = String[]
        for disp in sort(collect(keys(funs)))
            names = funs[disp]
            n  = length(names)
            item = "$disp on $n image$(n == 1 ? "" : "s")"
            n <= 2 && (item *= " ($(join(sort(collect(names)), ", ")))")   # name them when few; for more, the count says it
            w  = length(get(warn_imgs, (mod, disp), Set{String}()))
            fc = get(fail_count, (mod, disp), 0)
            w  > 0 && (item *= " — $w flagged")   # how many banked a warn QC finding (not just that one did)
            fc > 0 && (item *= " — $fc failed")
            push!(items, item)
        end
        out[mod] = items
    end
    (out, mod_sev, max_at)
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
function _pop_items_by_module(prev::AbstractDict, cur::AbstractDict, name_of)::Dict{String,Vector{String}}
    acc = Dict{String,Dict{Symbol,Dict{String,Set{String}}}}()   # category => verb => pop => image names
    add!(cat, verb, pop, img) = push!(
        get!(get!(get!(acc, cat, Dict{Symbol,Dict{String,Set{String}}}()), verb, Dict{String,Set{String}}()),
             pop, Set{String}()), img)
    for u in union(keys(prev), keys(cur))
        nm = name_of(u)
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

function _exclusion_items(prev::Vector{String}, cur::Vector{String}, name_of)::Vector{String}
    ps, cs = Set(prev), Set(cur)
    newly_ex = sort(String[u for u in cur if !(u in ps)])
    newly_in = sort(String[u for u in prev if !(u in cs)])
    items = String[]
    isempty(newly_ex) || push!(items, "excluded $(_where(String[name_of(u) for u in newly_ex]))")
    isempty(newly_in) || push!(items, "re-included $(_where(String[name_of(u) for u in newly_in]))")
    items
end

"""
Append a dated `[Cecelia]` digest of the NET change since the last capture, grouped by module
category (the task-manager tags), collapsed across images. Advances the stored snapshot. Returns the
appended block, or `nothing` when there is no (unmuted) change. First capture seeds baselines silently.
"""
function capture_context!(proj::CciaProject; date::Dates.Date = Dates.today())::Union{String,Nothing}
    state   = _read_context_state(proj)
    cutoff  = String(get(state, "lastCapturedAt", ""))
    name_by = Dict(img.uid => img.name for img in images(proj))
    name_of(u) = get(name_by, String(u), String(u))

    task_items, mod_sev, max_at = _task_items_by_module(proj, cutoff)

    cur_gating  = _gating_fingerprint(proj)
    prev_gating = get(state, "gating", nothing)          # absent → first capture → seed silently
    pop_items = prev_gating === nothing ? Dict{String,Vector{String}}() :
                _pop_items_by_module(prev_gating, cur_gating, name_of)

    cur_excl  = _excluded_uids(proj)
    prev_excl = get(state, "excluded", nothing)
    excl_items = prev_excl === nothing ? String[] :
                 _exclusion_items(String[String(u) for u in prev_excl], cur_excl, name_of)

    # merge every source into per-category item lists
    by_cat = Dict{String,Vector{String}}()
    for (cat, its) in task_items; append!(get!(by_cat, cat, String[]), its); end
    for (cat, its) in pop_items;  append!(get!(by_cat, cat, String[]), its); end
    isempty(excl_items) || append!(get!(by_cat, CATEGORY_IMAGES, String[]), excl_items)

    # always advance the snapshot (seeds baselines on first capture, absorbs muted-while changes)
    state["lastCapturedAt"] = max_at
    state["gating"]   = cur_gating
    state["excluded"] = cur_excl
    _write_context_state!(proj, state)

    # one bullet per category, in task-manager order, skipping muted categories. Each line leads with a
    # traffic-light symbol (✅/⚠️/❌) from the module's worst run outcome this window — so the reader
    # sees at a glance which entries need attention. Non-task categories (populations, exclusions) have
    # no run severity ⇒ ✅ (informational).
    muted = Set(read_mutes(proj))
    lines = String[]
    for cat in sort(collect(keys(by_cat)); by = c -> (_category_rank(c), c))
        (cat in muted || isempty(by_cat[cat])) && continue
        sym = severity_symbol(get(mod_sev, cat, "ok"))
        push!(lines, "$sym $cat — " * join(by_cat[cat], "; "))
    end

    isempty(lines) && return nothing
    append_lab_log!(proj, CONTEXT_AUTHOR, lines; date = date)
end
