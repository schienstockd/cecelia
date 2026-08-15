using JSON3

# Versioned path-dict access (active entry, list value names, …) goes through the shared
# `versioned_*` helpers in helpers.jl — one family for both the String→String path dicts here and
# the Any/JSON3 raw ccid.json dicts. Don't add a second variant.

# ── CciaImage ──────────────────────────────────────────────────────────────────

mutable struct CciaImage
    uid::String
    name::String
    status::String                    # pending | converting | done | failed
    filepath::Dict{String,String}     # versioned filenames (relative to zero dir)
    labels::Dict{String,Vector{String}}  # valueName → [filename, ...] (e.g. labels.zarr, labels_cyto.zarr)
    label_props::Dict{String,String}
    # Skeleton (branch) label sets — a separate registry from `labels`. Branch labels are a different
    # granularity (paths/edges, not cell regions), so they get their own field to keep the generic
    # `labels` picker (measure/track/segment dropdowns) unpolluted. See docs/todo/BRANCHING_PLAN.md
    # Decision 6. Files live at `{proj}/1/{uid}/branchLabels/{filename}.zarr` (mirrors `labels/`).
    branch_labels::Dict{String,Vector{String}}
    im_channel_names::Dict{String,Any} # versioned: {value_name => [names], _active => value_name}
    attr::Dict{String,String}         # user-defined metadata attributes
    meta::Dict{String,Any}
    # Include/exclude an image from further processing & analysis (the systematic successor to the
    # old R app's Include=Y/N metadata keyword). Excluded images are greyed (not hidden) in the GUI,
    # can't be checkbox-selected for a run, and are hard-skipped by the task/chain runners even if
    # somehow selected. `note` is a free-text reason the user can leave (why it was excluded).
    included::Bool
    note::String
    # A plain user bookmark — "I like this one". ANY number of images in a set can be starred, and
    # nothing downstream reads it: it does not affect selection, runs, or processing. It exists so a
    # long import can be narrowed to the handful worth looking at, via the Starred row filter that
    # sits beside Excluded/Imported. Deliberately not a nomination — the 8-bit import once derived a
    # set-wide intensity window from a single starred "reference", and that coupling is gone.
    starred::Bool
    _dir::String                      # {proj}/1/{uid}/ — runtime only
    # runtime-only pop_df result cache (keyed by request hash; values are DataFrames).
    # Mirrors R cciaImage `private$filteredPopDT`. Not serialised; cleared via
    # `pop_df(...; flush_cache=true)`. Typed loosely because DataFrames is imported after
    # this file in the module include order.
    _pop_df_cache::Dict{String,Any}
end

function CciaImage(; uid=gen_uid(), name="", status="pending", dir="")
    CciaImage(uid, name, status,
              Dict{String,String}(), Dict{String,Vector{String}}(), Dict{String,String}(),
              Dict{String,Vector{String}}(),      # branch_labels (Decision 6)
              Dict{String,Any}(),                 # im_channel_names (versioned)
              Dict{String,String}(), Dict{String,Any}(),
              true, "", false,                    # included (default), note, starred
              dir,
              Dict{String,Any}())                 # _pop_df_cache (runtime only)
end

"""Image data directory — {proj}/0/{uid}/"""
function img_zero_dir(img::CciaImage)::String
    joinpath(dirname(dirname(img._dir)), "0", img.uid)
end

"""Project directory this image lives in — `{projects_dir}/{proj_uid}/` (`img._dir` is `{proj}/1/{uid}/`)."""
img_project_dir(img::CciaImage)::String = dirname(dirname(img._dir))

"""
Project uid this image belongs to. The uid IS the project directory's name (see
`docs/OBJECTMODEL.md`), so it is read off the path rather than stored on the image — an image that
was copied into another project belongs to that project, whatever any baked-in value would say.
"""
img_project_uid(img::CciaImage)::String = basename(img_project_dir(img))

"""Absolute path to the active (or named) filepath version. Resolves into the 0 (image) dir."""
function img_filepath(img::CciaImage, name::Union{String,Nothing}=nothing)::Union{String,Nothing}
    filename = isnothing(name) ? versioned_get(img.filepath) : get(img.filepath, name, nothing)
    isnothing(filename) ? nothing : joinpath(img_zero_dir(img), filename)
end

"""
Whether the image is included in further processing & analysis (default `true`). Excluded images
(`included == false`) are advisory-greyed in the GUI and hard-skipped by the task/chain runners.
Image-owned accessor so run-dispatch code asks the model, not the raw field.
"""
image_included(img::CciaImage)::Bool = img.included

"""The image's labelProps directory — `{proj}/1/{uid}/labelProps`."""
img_label_props_dir(img::CciaImage)::String = joinpath(img._dir, "labelProps")

"""
Absolute path to a labelProps `.h5ad` for a value_name (mirrors R `imLabelPropsFilepath`).
Uses the registered filename when present, else the conventional `{value_name}.h5ad`. This is
the single owner of the labelProps path convention — readers (`label_props`) and tasks
(segmentation/tracking) resolve here rather than joining `"labelProps"` inline.
"""
function img_label_props_path(img::CciaImage, value_name::AbstractString="default")::String
    filename = get(img.label_props, String(value_name), "$(value_name).h5ad")
    joinpath(img_label_props_dir(img), filename)
end

"""The image's labels directory — `{proj}/1/{uid}/labels`. Cell-segmentation label zarrs live here
(the `branchLabels/` sibling below holds skeleton labels)."""
img_labels_dir(img::CciaImage)::String = joinpath(img._dir, "labels")

"""
Absolute path to a segmentation labels zarr for a value_name — resolves the registered filename
from `img.labels`, falling back to the conventional `{value_name}.zarr` when the value_name isn't
registered yet (registration happens only on success, so an unregistered name is normal).

This is where a FINISHED store lives. It is **not** where a run in progress is writing: that goes to
a staging sibling and is renamed here on completion (`zarr_utils.staged_store`), which is why
cancelling a re-run can no longer truncate a registered set. Use `staging_store_path` for the
in-progress path — `segment_live_outputs` is the caller that needs it.

Image-owned + pop_type-neutral, exactly like `img_label_props_path`; a `labels` value_name can carry
SEVERAL files (base + nuc), so use `img.labels[vn]` directly when you need all of them.
"""
function img_labels_path(img::CciaImage, value_name::AbstractString="default")::String
    filenames = get(img.labels, String(value_name), String[])
    filename = isempty(filenames) ? "$(value_name).zarr" : first(filenames)
    joinpath(img_labels_dir(img), filename)
end

"""
    img_spatial_graph_path(img, suffix) -> String
    img_spatial_graph_suffixes(img)     -> Vector{String}

The spatial neighbour graph `spatialAnalysis.cellNeighbours` persists per run:
`{img._dir}/spatialGraph/{suffix}.h5ad`. The graph is POP-AGNOSTIC and pools across segmentations, so —
unlike labelProps — it is keyed by RUN suffix, not by value_name (a `{vn}.spatial.h5ad` next to the cell
table could not represent a cross-segmentation graph, which is why it was replaced). Discovery is by
listing the directory, the same convention as the `spatialStats/{suffix}.json` sidecars — no ccid.json
registration needed. Image-owned + pop_type-neutral, like `img_label_props_path`.
"""
img_spatial_graph_dir(img::CciaImage)::String = joinpath(img._dir, "spatialGraph")
img_spatial_graph_path(img::CciaImage, suffix::AbstractString)::String =
    joinpath(img_spatial_graph_dir(img), "$(suffix).h5ad")
function img_spatial_graph_suffixes(img::CciaImage)::Vector{String}
    d = img_spatial_graph_dir(img)
    isdir(d) || return String[]
    sort!(String[f[1:prevind(f, end, 5)] for f in readdir(d) if endswith(f, ".h5ad")])
end

# Generic value_name checks over a versioned property field (default `label_props` = the segmentations).
# It's just "does this versioned field carry this value_name" — reusable wherever a feature must know
# whether an image has a given value_name before acting on it (e.g. copying gating across images).
"""
    img_value_names(img::CciaImage; field = :label_props) -> Vector{String}

The value_names on an image for a versioned field — by default the segmentation names (the keys of the
`label_props` field). These are the `value_name`s you pass to `pop_df` / `label_props` / `track_props`.
"""
img_value_names(img::CciaImage; field::Symbol = :label_props)::Vector{String} =
    versioned_keys(getfield(img, field))
img_has_value_name(img::CciaImage, value_name::AbstractString; field::Symbol = :label_props)::Bool =
    String(value_name) in img_value_names(img; field = field)

"""
    resolve_value_name(img, value_name = nothing) -> String

The segmentation to act on: the caller's `value_name` when given, else the image's **active** one.
Every accessor that takes an optional `value_name` resolves it here.

This is the `defaultOnly = TRUE` half of R's `valueNames(x, valueType, defaultOnly)` on `cciaImage`;
`img_value_names` is the list half.

R took the field as a parameter (`x`) because in R **every** registry was versioned with an active
key. This port is narrower, and by *type* rather than by decision: `filepath`, `label_props` and
`imChannelNames` map to `String`/`Any` values and can hold an `_active` entry, but `labels` and
`branch_labels` are `Dict{String,Vector{String}}` — a `String` active marker does not fit the value
type at all. So they carry no active pointer, and this resolver is `label_props`-only rather than
field-parameterised; a generic version would silently return `"default"` for them (or stringify a
vector). Today that works out because a segmentation's label store and props table share one
value_name, so `label_props`' pointer serves both, and branch labels — which can exist with no
`label_props` entry — are enumerated from disk by `img_branch_value_names` instead. Making `labels`
genuinely versioned would be a `ccid.json` shape change, not a refactor.

R's third parameter, `valueType`, has no counterpart on purpose. It regex-matched a `.cl`/`.branch`
**suffix** on the name (its own comment: *"TODO this should be better"*); the port replaced that with
separate registries — branch labels live in `branch_labels` and are enumerated from disk by
`img_branch_value_names` (see Decision 6 there for why that distinction is load-bearing).

Nine call sites used to spell this out inline as
`something(value_name, get(img.label_props, "_active", "default"))` — hardcoding both the `_active`
key and the `"default"` fallback that `VERSIONED_ACTIVE_KEY`/`VERSIONED_DEFAULT_VAL` exist to name,
and re-deriving `versioned_active` by hand. Image-owned and pop_type-neutral, so it lives here rather
than in gating, where most of the copies had accumulated.
"""
resolve_value_name(img::CciaImage, value_name = nothing)::String =
    something(value_name, versioned_active(img.label_props))

# Per-track table suffix. A tracked segmentation gets a companion `.h5ad` holding ONE row per
# track (track measures in X/var, lineage in obs) alongside the per-cell labelProps. The double
# underscore keeps it distinct from a segmentation literally named "{x}_tracks" and marks the
# name as reserved (a user segmentation may not end in `__tracks`).
const TRACK_PROPS_SUFFIX = "__tracks"

"""
Absolute path to the per-track labelProps `.h5ad` for a value_name:
`labelProps/{value_name}__tracks.h5ad`. Written by `tracking.track_measures` (one row per
`track_id`; measures in X/var so they are gateable, lineage in obs). Distinct from the per-cell
`img_label_props_path` so the cell table stays normalised (no broadcast track columns).
"""
img_track_props_path(img::CciaImage, value_name::AbstractString="default")::String =
    joinpath(img_label_props_dir(img), "$(value_name)$(TRACK_PROPS_SUFFIX).h5ad")

"""
    img_track_value_names(img) -> Vector{String}

The value_names that have a per-track table, from the `{vn}__tracks.h5ad` sidecars on disk. Sorted;
empty when tracking never measured. The exact twin of `img_branch_value_names` — same directory, same
reserved-suffix convention.

**The sidecar is the source, not the run log.** "Has this image been tracked" cannot be answered from
`runLog` (a project migrated from the R version, or tracked before the run log existed, carries no
`tracking.*` entry at all while its tracks sit right there on disk), and `label_props` holds only the
per-CELL tables so it looks identical whether or not tracking ran. This is also STRICTLY stronger than
`is_tracked` (which only asks whether `track_id` reached obs): the sidecar means the track measures
landed, which is what every track-grained consumer — `track_props`, `clustTracks.cluster`, the
behaviour HMM — actually needs. Cheap enough for a payload: one `readdir`, no HDF5 open.
"""
function img_track_value_names(img::CciaImage)::Vector{String}
    dir = img_label_props_dir(img)
    isdir(dir) || return String[]
    suffix = TRACK_PROPS_SUFFIX * ".h5ad"
    sort!(String[f[1:end-length(suffix)] for f in readdir(dir) if endswith(f, suffix)])
end

# Per-branch (skeleton) table suffix. A skeletonised segmentation gets a companion `.h5ad` holding
# ONE row per branch path (branch measures in X/var — length, tortuosity, branch-type; endpoints
# in obs). Lives beside the per-cell labelProps. Same double-underscore convention as tracks; the
# name `{x}__branch` is reserved. See docs/todo/BRANCHING_PLAN.md Decision 1.
const BRANCH_PROPS_SUFFIX = "__branch"

"""
Absolute path to the per-branch labelProps `.h5ad` for a value_name:
`labelProps/{value_name}__branch.h5ad`. Written by `segment.branching` (one row per skeleton
path). Distinct from the per-cell `img_label_props_path` so the cell table stays normalised (no
branch columns leak into cell tables), and from `img_track_props_path` so the branch pop type
routes to its own sidecar (docs/POPULATION.md).
"""
img_branch_props_path(img::CciaImage, value_name::AbstractString="default")::String =
    joinpath(img_label_props_dir(img), "$(value_name)$(BRANCH_PROPS_SUFFIX).h5ad")

"""
    img_branch_value_names(img) -> Vector{String}

The value_names that have a branch table, from the `{vn}__branch.h5ad` sidecars on disk. Sorted;
empty when branching never ran.

**These are NOT the `label_props` value_names**, and that difference is load-bearing. Branching
runs on a *segmentation* (`img.labels`), which need not have a per-cell measurement table — an SHG
collagen mask is skeletonised but never measured, so it appears in `labels` and `branch_labels`
while `label_props` holds only the measured cell segmentations. Enumerating branch populations
from `label_props` therefore finds nothing at all: it looks for `B__branch` / `T__branch` and
misses the `SHG__branch` that actually exists. A single image can carry several
(`SHG__branch` + `DCs__branch` — collagen and a dendritic-cell network, as in
`behaviourUbiTom3P.Rmd`), so this is the plural case, not an edge case.

The sidecar is the right source because it is exactly what `pop_df(img, "branch", …)` reads.
"""
function img_branch_value_names(img::CciaImage)::Vector{String}
    dir = img_label_props_dir(img)
    isdir(dir) || return String[]
    suffix = BRANCH_PROPS_SUFFIX * ".h5ad"
    sort!(String[f[1:end-length(suffix)] for f in readdir(dir) if endswith(f, suffix)])
end

"""The image's branch-labels directory — `{proj}/1/{uid}/branchLabels`. Skeleton label zarrs live
here (mirrors the `labels/` directory for cell segmentations). Separate on-disk directory so the
generic `labels` scan never picks up branch label sets. See BRANCHING_PLAN.md Decision 6."""
img_branch_labels_dir(img::CciaImage)::String = joinpath(img._dir, "branchLabels")

"""
Absolute path to a branch labels zarr for a value_name — resolves the registered filename from
`img.branch_labels` (mirrors `img_labels_path`/`img_label_props_path`). Falls back to
`{value_name}.zarr` if the value_name isn't registered yet (write path).
"""
function img_branch_labels_path(img::CciaImage, value_name::AbstractString="default")::String
    filenames = get(img.branch_labels, String(value_name), String[])
    filename = isempty(filenames) ? "$(value_name).zarr" : first(filenames)
    joinpath(img_branch_labels_dir(img), filename)
end

"""True if a value_name uses one of the reserved suffixes (`__tracks`, `__branch`) — not allowed
for a user-created segmentation (each names a companion sidecar for `{base}`)."""
is_reserved_value_name(value_name::AbstractString) =
    endswith(String(value_name), TRACK_PROPS_SUFFIX) ||
    endswith(String(value_name), BRANCH_PROPS_SUFFIX)

# ── Channel names ──────────────────────────────────────────────────────────────

"""
Set channel names, validating against SizeC from OME metadata.
Stored as a top-level versioned field (`imChannelNames`) — the same on-disk
convention used by tasks and the API — so `versioned_get_field`/`versioned_set_field!`
readers see them. value_name allows storing multiple sets (e.g. per population).
"""
function set_channel_names!(img::CciaImage, names::Vector{String};
                             value_name::String="default",
                             check_length::Bool=true)::CciaImage
    if check_length
        size_c = parse(Int, string(get(img.meta, "SizeC", "0")))
        if size_c > 0 && length(names) != size_c
            error("Expected $size_c channel names (SizeC), got $(length(names))")
        end
    end
    versioned_set!(img.im_channel_names, names, value_name)
    img
end

"""
Return channel names for a value_name (defaults to the active version). Channel names are
often stored only under a base version (e.g. `default`) while labelProps use other value_names
(e.g. `B`), so an explicit value_name with no own entry falls back to the active version.
"""
function channel_names(img::CciaImage; value_name=nothing)::Union{Vector{String},Nothing}
    isempty(img.im_channel_names) && return nothing
    v = versioned_get(img.im_channel_names, value_name)   # nothing → active
    isnothing(v) && !isnothing(value_name) && (v = versioned_get(img.im_channel_names, nothing))
    isnothing(v) ? nothing : collect(String, v)
end

# ── Channel name → 0-based index ──────────────────────────────────────────────
#
# THE ONE resolver for "a `channelSelection` param holds names; Python wants indices". Six task
# handlers had hand-rolled `findfirst(==(String(ch)), ch_names)` — `cellpose_correct`, `drift_correct`,
# `af_correct` (twice), `segment/cellpose`, `segment/branching` — and they had drifted into three
# different behaviours, every one of them silently wrong:
#
#   * an already-resolved INDEX crashed four of them (`String(::Int64)` → MethodError), because a REPL
#     caller, a test, or a re-translated chain dict hands back what the first pass produced;
#   * an unmatched NAME was silently DROPPED by five — so a stale name in a saved chain quietly
#     segmented on the wrong channels, or on none;
#   * and `drift_correct` silently fell back to index 0, which on a resonance-scanner movie means
#     registering the whole timelapse against SHG at 99.5% zeros. Measured on `zolIMa/2h06xA`: the
#     reference channel is worth ~2x in shift jitter (Y sd 1.86 px on CD169-Kat vs 0.93 on mem-TOM),
#     so picking it by accident is not a small error.
#
# So: idempotent on integers, and an unmatched name RAISES with the available names. That is a
# deliberate behaviour change from silent-drop — a channel the user named and we could not find is
# not a thing to guess about. The Python counterpart is `script_utils.channel_indices`, which catches
# the mirror-image failure (a NAME arriving where an index was due, i.e. this never ran).

"""
    channel_index(ch, ch_names; what="channel") -> Int

Resolve one channel to its **0-based** index. `ch` may be a name (looked up in `ch_names`) or an
already-resolved `Integer`, which passes through — so translating twice is a no-op.

Raises when a name is not among `ch_names`, naming what was available.
"""
function channel_index(ch, ch_names::AbstractVector{<:AbstractString};
                       what::AbstractString = "channel")::Int
    ch isa Integer && return Int(ch)
    name = String(ch)
    idx = findfirst(==(name), ch_names)
    if isnothing(idx)
        # Case-only differences are the common real cause and the match stays EXACT anyway: two images
        # from the same experiment shipped `mem-TOM` and `mem-Tom`, so a chain built on one would fail
        # on the other. Naming the near match makes that a five-second fix instead of a puzzle — but it
        # is a hint, not a silent coercion. Guessing which channel was meant is what this resolver exists
        # to stop doing.
        near = findfirst(n -> lowercase(n) == lowercase(name), ch_names)
        error("$what: no channel named '$name' in this image. Available: " *
              (isempty(ch_names) ? "(none registered)" : join(ch_names, ", ")) *
              (isnothing(near) ? "" : ". Did you mean '$(ch_names[near])'? (differs only in case)") *
              ". Channel names are per image version — a saved chain may name a channel this version " *
              "does not have.")
    end
    idx - 1
end

"""
    channel_indices(chs, ch_names; what="channels", unique_only=true) -> Vector{Int}

`channel_index` over a collection, preserving order. Deduplicates by default: a channel named twice
would otherwise be counted twice, which for the AF weight squares its term into the denominator a
second time. Pass `unique_only=false` where multiplicity is meaningful.

A `nothing` or empty input gives `Int[]` — "no channels selected" is a legitimate state that each
task judges for itself (branching only needs `fibreChannels` for `anisotropySource="channel"`).
"""
function channel_indices(chs, ch_names::AbstractVector{<:AbstractString};
                         what::AbstractString = "channels",
                         unique_only::Bool = true)::Vector{Int}
    isnothing(chs) && return Int[]
    seq = chs isa AbstractVector ? chs : [chs]
    out = Int[channel_index(c, ch_names; what = what) for c in seq]
    unique_only ? unique(out) : out
end

"""
    ccid_channel_names(raw, value_name=VERSIONED_DEFAULT_VAL) -> Vector{String}

Channel names straight off a `ccid.json` dict, for a task handler that has `raw` rather than a loaded
`CciaImage`. The same three lines were repeated in all six handlers; `nothing` for `value_name` means
the ACTIVE version (what `segment/branching` wants — a corrected image with renamed channels).
"""
function ccid_channel_names(raw::AbstractDict,
                            value_name = VERSIONED_DEFAULT_VAL)::Vector{String}
    v = versioned_get_field(raw, "imChannelNames", value_name)
    v isa AbstractVector ? collect(String, v) : String[]
end

# ── State file — the object owns its own path ─────────────────────────────────
# Ported from the old R `reactivePersistentObject.R`, where the state file was private to the object
# (`private$getStateFile()`) and everything — save, load, and the `.lock` — derived from it. That
# encapsulation was lost in the port: 20+ call sites each re-derived `joinpath(obj._dir, STATE_FILENAME)`
# and threaded the string around, and `_lock_path` invented its OWN name (`.cecelia.lock`) instead of
# deriving from the state file, which is why per-image locking had no mechanism to hang off.
#
# So: ask for the state file, never build the path. `state_file` is the single derivation — one generic
# with a method per form a caller actually has (loaded object / metadata dir / project dir + uid), so
# the API layer answering from raw uids uses the same helper rather than re-spelling `1/` + the
# filename. The lockfile is `state_file(obj) * ".lock"`, exactly as in R. `STATE_FILENAME` is exported
# only so an out-of-tree caller can recognise the file; in-tree, call `state_file`.
const STATE_FILENAME = "ccid.json"

"""
    obj_meta_dir(proj_dir, obj_uid) -> String

An object's metadata directory — `{proj_dir}/1/{obj_uid}/`. The `1/` (metadata) vs `0/` (image data)
split is layout knowledge that belongs here, next to its `0/` counterpart `img_zero_dir`, not spelled
out at each call site.
"""
obj_meta_dir(proj_dir::AbstractString, obj_uid::AbstractString)::String =
    joinpath(proj_dir, "1", obj_uid)

"""
    state_file(obj)                  -> String   # a loaded image / set / project
    state_file(meta_dir)             -> String   # a metadata dir already in hand
    state_file(proj_dir, obj_uid)    -> String   # by uid, without loading the object

Absolute path of an object's persisted state — `ccid.json` for an image/set, `project.json` for a
project. THE way to locate it, in whichever form the caller has: never join the filename or the `1/`
segment yourself.

The object form is preferred where an object is loaded (the R `getStateFile` encapsulation); the uid
form exists because the API layer routinely answers a request from raw uids without paying to load
the object.
"""
state_file(img::CciaImage)::String = joinpath(img._dir, STATE_FILENAME)
state_file(meta_dir::AbstractString)::String = joinpath(meta_dir, STATE_FILENAME)
state_file(proj_dir::AbstractString, obj_uid::AbstractString)::String =
    state_file(obj_meta_dir(proj_dir, obj_uid))

function save!(img::CciaImage)
    d = Dict{String,Any}(
        "class"          => "CciaImage",
        "uid"            => img.uid,
        "name"           => img.name,
        "status"         => img.status,
        "filepath"       => img.filepath,
        "labels"         => img.labels,
        "branch_labels"  => img.branch_labels,
        "label_props"    => img.label_props,
        "imChannelNames" => img.im_channel_names,
        "attr"           => img.attr,
        "meta"           => img.meta,
        "included"       => img.included,
        "note"           => img.note,
        "starred"        => img.starred,
    )
    write_json_atomic(state_file(img), d)
end

# ── Per-task param memory (funParams) ───────────────────────────────────────────
# Mirrors the old R `saveModuleFunParams`/`moduleFunParams`: the last-used params for a task are
# remembered in the object's ccid.json under `meta["funParams"][fun_name]`. On run they are saved
# to each processed IMAGE (a record of what params produced it) AND to the SET (the shared
# last-used default); the module-page form is populated image → set → task-defaults.
#
# This is a targeted read-modify-write of ccid.json (same idiom a task uses to register its output
# filepath) rather than load-object → save! — deliberately **dir-based** so remembering a param blob
# on the set never has to load all of the set's images (`save!(::CciaSet)` cascades to every child).
const FUN_PARAMS_META_KEY = "funParams"

# Params remembered PER OUTPUT NAME, alongside (never instead of) the flat last-run blob above.
#
# One blob per task is wrong the moment a task is run twice under different names: segmenting `Tcell`
# and then `Neutrophil` left the form showing the Neutrophil settings, so re-running Tcell meant
# re-entering every model parameter by hand. That is the problem this exists for.
#
# A SEPARATE meta key rather than nesting inside `funParams[fun]`, because the two answer different
# questions and mixing them would make the flat blob ambiguous — is a key a param, or a name? Old
# entries keep working untouched and there is no migration: a task/name pair with nothing banked falls
# back to the flat blob, which is exactly today's behaviour.
const FUN_PARAMS_BY_NAME_META_KEY = "funParamsByName"

"""
    read_module_fun_params(ccid_dir, fun; value_name = "") -> Dict | nothing

Last-used params for task `fun` in `<ccid_dir>/ccid.json`, or `nothing` if absent. `ccid_dir` is an
object metadata dir (`{proj}/1/{uid}/`) — image or set.

With a `value_name`, prefers what was last run UNDER THAT NAME (`meta["funParamsByName"][fun][name]`)
and falls back to the flat last-run blob (`meta["funParams"][fun]`). The fallback is the point: the
first time a name is used there is nothing banked for it, and starting from the last run is a better
default than the task's bare defaults — and is what the form did before this existed.
"""
function read_module_fun_params(ccid_dir::String, fun::String;
                                value_name::AbstractString = "")::Union{Dict{String,Any},Nothing}
    meta = _read_meta(ccid_dir)
    isnothing(meta) && return nothing
    if !isempty(value_name)
        hit = _fun_params_entry(_fun_params_entry(get(meta, FUN_PARAMS_BY_NAME_META_KEY, nothing), fun),
                                String(value_name))
        isnothing(hit) || return hit
    end
    _fun_params_entry(get(meta, FUN_PARAMS_META_KEY, nothing), fun)
end

"""
    read_module_fun_params_by_name(ccid_dir, fun, value_name) -> Dict | nothing

ONLY what was last run under `value_name` — no fallback to the flat blob.

Separate from the fallback-taking read above because the caller needs to tell the two apart. The form
must REPLACE what the user is looking at when a name has params banked for it, and leave it alone when
it does not: falling back there would quietly discard edits the user had just made, replacing them
with the previous run's. "Nothing banked for this name" and "here are the previous run's params" are
different answers and only one of them is safe to apply.
"""
function read_module_fun_params_by_name(ccid_dir::String, fun::String,
                                        value_name::AbstractString)::Union{Dict{String,Any},Nothing}
    isempty(value_name) && return nothing
    meta = _read_meta(ccid_dir)
    isnothing(meta) && return nothing
    _fun_params_entry(_fun_params_entry(get(meta, FUN_PARAMS_BY_NAME_META_KEY, nothing), fun),
                      String(value_name))
end

function _read_meta(ccid_dir::String)
    path = state_file(ccid_dir)
    isfile(path) || return nothing
    meta = get(JSON3.read(read(path, String), Dict{String,Any}), "meta", nothing)
    meta isa AbstractDict ? meta : nothing
end

# one level of `{key => Dict}` lookup, String-keyed (JSON3 hands back Symbols — CLAUDE.md)
function _fun_params_entry(bag, key)::Union{Dict{String,Any},Nothing}
    v = bag isa AbstractDict ? get(bag, key, nothing) : nothing
    v isa AbstractDict ? Dict{String,Any}(String(k) => vv for (k, vv) in v) : nothing
end

"""
    write_module_fun_params!(ccid_dir, fun, params; value_name = "")

Remember `params` as the last-used params for task `fun` in `<ccid_dir>/ccid.json`, preserving every
other field. No-op if the file is absent.

Writes the flat blob (`meta["funParams"][fun]`) ALWAYS, and additionally under the output name when
one is given (`meta["funParamsByName"][fun][name]`). Both, deliberately: the flat one is what a NEW
name falls back to, so it has to keep tracking the most recent run whatever it was called.
"""
function write_module_fun_params!(ccid_dir::String, fun::String, params::AbstractDict;
                                  value_name::AbstractString = "")
    path = state_file(ccid_dir)
    isfile(path) || return nothing
    raw  = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(path, String), Dict{String,Any}))
    meta = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
    clean = Dict{String,Any}(String(k) => v for (k, v) in params)

    fp = Dict{String,Any}(String(k) => v for (k, v) in get(meta, FUN_PARAMS_META_KEY, Dict{String,Any}()))
    fp[fun] = clean
    meta[FUN_PARAMS_META_KEY] = fp

    if !isempty(value_name)
        byn = Dict{String,Any}(String(k) => v
                               for (k, v) in get(meta, FUN_PARAMS_BY_NAME_META_KEY, Dict{String,Any}()))
        per_fun = Dict{String,Any}(String(k) => v
                                   for (k, v) in get(byn, fun, Dict{String,Any}()))
        per_fun[String(value_name)] = clean
        byn[fun] = per_fun
        meta[FUN_PARAMS_BY_NAME_META_KEY] = byn
    end

    raw["meta"] = meta
    write_json_atomic(path, raw)
    nothing
end

"""
Physical pixel sizes for the image, read from `img.meta`.
Returns `(pixel_res, time_step)` where `pixel_res` is a `Vector{Float64}` of µm/px per
spatial axis in skimage order (z, y, x for 3-D), and `time_step` is **minutes/frame**.
Mirrors `cciaImage\$omeXMLPixelRes()` / `omeXMLTimelapseInfo()` from the old R version — which
reports the interval in minutes (`TimeIncrement / 60`), so track speed comes out in **µm/min**
(the celltrackR / T-cell convention, ~10 µm/min) rather than µm/s.

The values are persisted into `meta` at import time (the omezarr import reads them from the
OME-XML via `ome_types`/`dim_utils`); this accessor is pure-Julia and does no Python/XML I/O.
Any axis (or the time step) absent from `meta` falls back to `1.0` (pixel-space), so measures
remain correct in pixel/frame units when physical metadata is unavailable.
"""
function img_physical_sizes(img::CciaImage)::Tuple{Vector{Float64}, Float64}
    m = img.meta
    getf(key) = begin
        v = get(m, key, nothing)
        (isnothing(v) || v == "") ? 1.0 : something(tryparse_f64(v), 1.0)
    end
    sx = getf("PhysicalSizeX")
    sy = getf("PhysicalSizeY")
    sz = getf("PhysicalSizeZ")
    # OME TimeIncrement is in SECONDS; report minutes/frame (÷60) so speed is µm/min, matching the
    # old R omeXMLTimelapseInfo. Absent/blank → 1.0 min/frame fallback (parity with R interval=1).
    tiv = get(m, "TimeIncrement", nothing)
    ts = (isnothing(tiv) || tiv == "") ? 1.0 : something(tryparse_f64(tiv), 60.0) / 60.0
    ([sz, sy, sx], ts)   # skimage order: z, y, x
end

"""
    img_is_calibrated(img) -> Bool

Whether the image carries a real physical pixel size (`PhysicalSizeX` **and** `PhysicalSizeY` present
and > 0). `PhysicalSizeZ` is not required — a 2D image legitimately has none.

The companion to `img_physical_sizes`, which defaults a missing axis to `1.0` so that measures stay
correct in pixel units. That default is deliberately indistinguishable from a genuine 1 µm/px, so any
consumer that *reports* µm (rather than just computing) needs this to tell "uncalibrated" from
"calibrated at 1.0" — `pop_df(…; centroids = :physical)` uses it to warn instead of relabelling pixels
as microns. (`api_images_meta_get` keeps the same distinction for the UI by reading `meta` raw.)
"""
function img_is_calibrated(img::CciaImage)::Bool
    ok(key) = begin
        v = get(img.meta, key, nothing)
        (isnothing(v) || v == "") && return false
        p = tryparse_f64(v)
        !isnothing(p) && p > 0
    end
    ok("PhysicalSizeX") && ok("PhysicalSizeY")
end

"""
    physical_size_for_axis(img_or_sizes, axis::Symbol) -> Float64

Physical pixel size (µm/px) for ONE spatial axis (`:x`/`:y`/`:z`). Explicit per-axis lookup so a
consumer maps a `centroid_{axis}` column to its resolution BY NAME, never by tail position — the fix
for the silent 2D mis-scaling (docs/todo/CENTROID_AXES_PLAN.md). `img_or_sizes` is a `CciaImage` or the
`[sz, sy, sx]` vector from `img_physical_sizes`. Absent metadata → 1.0 (pixel space)."""
function physical_size_for_axis(sizes::AbstractVector{<:Real}, axis::Symbol)::Float64
    axis === :z ? Float64(sizes[1]) :
    axis === :y ? Float64(sizes[2]) :
    axis === :x ? Float64(sizes[3]) :
        error("physical_size_for_axis: axis must be :x/:y/:z (got :$axis)")
end
physical_size_for_axis(img::CciaImage, axis::Symbol)::Float64 =
    physical_size_for_axis(first(img_physical_sizes(img)), axis)

"""
    img_axes(img) -> Set{Symbol}

The set of non-trivial axes the image carries — a subset of `{:T, :Z, :C, :Y, :X}`. Read from
`img.meta` (persisted at import from OME-XML via `Size*`), so no zarr/XML I/O. `:X`/`:Y` are always
present; `:T`/`:Z`/`:C` present iff the corresponding size is > 1.

Canonical predicate for task-level applicability gates (`requires.axes` in the task JSON,
`task_applies` in `tasks/task.jl`) — don't hand-roll `SizeT > 1` in a task. `TimeIncrement` is
honoured as a fallback signal for T on projects imported before `SizeT` was persisted.
"""
function img_axes(img::CciaImage)::Set{Symbol}
    m = img.meta
    axes = Set{Symbol}((:X, :Y))
    geti(key) = begin
        v = get(m, key, nothing)
        (isnothing(v) || v == "") ? 1 : something(tryparse_i(v), 1)
    end
    geti("SizeT") > 1 && push!(axes, :T)
    geti("SizeZ") > 1 && push!(axes, :Z)
    geti("SizeC") > 1 && push!(axes, :C)
    # Fallback: pre-SizeT imports still carry TimeIncrement when the source was a timelapse.
    if :T ∉ axes
        tiv = get(m, "TimeIncrement", nothing)
        (!isnothing(tiv) && tiv != "" && !isnothing(tryparse_f64(tiv))) && push!(axes, :T)
    end
    axes
end

"""Does the image have a T (time) axis? See `img_axes`."""
img_has_time(img::CciaImage)::Bool = :T ∈ img_axes(img)

tryparse_i(v::Integer) = Int(v)
tryparse_i(v::Real) = isfinite(v) ? Int(round(v)) : nothing
tryparse_i(v::AbstractString) = tryparse(Int, v)
tryparse_i(::Any) = nothing

tryparse_f64(v::Real) = Float64(v)
tryparse_f64(v::AbstractString) = tryparse(Float64, v)
tryparse_f64(::Any) = nothing

function _load_image(dir::String)::CciaImage
    d = read_state_json(state_file(dir); as = Dict{String,Any})
    to_spaths(key) = Dict{String,String}(
        string(k) => string(v) for (k, v) in get(d, key, Dict{String,Any}()))
    # labels: Dict{String, Vector{String}} — value can be a list or a bare string (legacy)
    to_labels(key) = Dict{String,Vector{String}}(
        string(k) => (v isa AbstractVector ? collect(String, v) : [string(v)])
        for (k, v) in get(d, key, Dict{String,Any}()))
    icn = Dict{String,Any}(string(k) => v for (k, v) in get(d, "imChannelNames", Dict{String,Any}()))
    # Legacy `kind` field silently ignored — project-wide static/live/flow distinction was dropped
    # in favour of per-image axis gating (see Cecelia.task_applies). Legacy ccid.jsons round-trip
    # into memory without kind; next save! strips it from disk.
    CciaImage(
        d["uid"], d["name"], get(d, "status", "pending"),
        to_spaths("filepath"), to_labels("labels"), to_spaths("label_props"),
        to_labels("branch_labels"),                              # legacy images: absent → empty
        icn,
        to_spaths("attr"),
        Dict{String,Any}(get(d, "meta", Dict{String,Any}())),
        # Legacy images (pre-inclusion) have none of these → included, no note, not starred.
        Bool(get(d, "included", true)), String(get(d, "note", "")), Bool(get(d, "starred", false)),
        dir,
        Dict{String,Any}(),                 # _pop_df_cache (runtime only)
    )
end
