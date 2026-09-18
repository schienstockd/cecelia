# ── MovieConfig — what produced this movie ──────────────────────────────────
#
# Banked into `settings/movies.json` once the bytes land (Phase 4 of
# docs/todo/MOVIE_MANAGEMENT_PLAN.md). Two disjoint flavours, one abstract type:
#
#   MovieRecordConfig — the viewer's Record button, or the animation page's render. Carries the
#                        image, the on-screen `look` (channels + overlays), the compare-grid axes,
#                        and (for animations) the keyframes payload.
#   MovieBatchConfig  — the batch page's "apply this authored config across N images" run.
#                        Carries the authored `config` dict once, plus the file-attr picks and
#                        the whole image selection.
#
# Assembled at the WS boundary (`sockets.jl`) rather than inside the recorder, so the recorder
# keeps knowing nothing about the request shape. Passed opaquely through `run_single_*_offline`
# and `run_batch_offline` (`movie_rail.jl`) to `register_movie!` (`movies_api.jl`), which JSON-
# serialises it under the movie's row.
#
# Persisted shape: `JSON3.write(config)` produces the same JSON keys as the pre-struct
# `Dict{String,Any}(...)` did, so existing `settings/movies.json` files continue to read back
# through `_entry_channels` / `_entry_image_uid` (`movies_api.jl`) unchanged — those readers walk
# a JSON3.Object after `_read_movies_registry`, not this struct, so on-disk backward compatibility
# is by field-name equality with the previous ad-hoc dict.
#
# Not typed here: `look`, `keyframes`, `keyframeMeta`, `titleCard`, and (batch's) `config` are
# frontend-shaped payloads that the recorder walks via `get(look, "colourBy", …)` etc.
# Typing them would either require a struct per snapshot version or lose the round-trip on
# unknown fields the frontend added — so they ride as `Any` and are accessed exactly as before.

import JSON3

# Not sealed — recorders always know which flavour they built and dispatch on the concrete type.
# The abstract parent exists so signatures that legitimately accept either flavour (`register_movie!`,
# a future audit tool) can spell `Union{Nothing,MovieConfig}` once instead of listing both.
abstract type MovieConfig end

# The record flavour. Field names are CamelCase because they are the on-wire keys frontend +
# `settings/movies.json` already use; StructTypes.Struct() below preserves them 1:1.
Base.@kwdef struct MovieRecordConfig <: MovieConfig
    imageUid::String
    # keyframeMeta is the timeline-editor sidecar (thumbnail, per-step seconds, title). Rides on
    # the record request as an opaque array/dict; the recorder never reads it — only Phase 6's
    # movieRestore does, on the way back into the editor.
    keyframeMeta::Any                       = nothing
    fps::Int                                = 15
    # `nothing` means "record at the browser viewer's canvas size" — the default and the pre-size-
    # field behaviour. Coerced by `_movie_size_params` (movie_helpers.jl) so a blank / non-positive
    # value never reaches here as anything other than nothing.
    sizeX::Union{Int,Nothing}               = nothing
    sizeY::Union{Int,Nothing}               = nothing
    suffix::String                          = ""
    # AbstractDict when the user enabled the title card, else nothing. Left as Any so a future
    # title-card schema change doesn't break the round-trip on movies recorded under the old shape.
    titleCard::Any                          = nothing
    valueNames::Vector{String}              = String[]
    # Absent (nothing) differs from empty ([]) — absent leaves the canvas' masks alone (plain
    # "record what's on screen"), empty is an explicit "no masks". Same three-valued contract for
    # branch (skeleton) value names.
    labelValueNames::Union{Vector{String},Nothing}  = nothing
    branchValueNames::Union{Vector{String},Nothing} = nothing
    labelContour::Int                       = 0
    show3D::Bool                            = false
    zSlice::Union{Int,Nothing}              = nothing
    tStart::Int                             = 1
    tEnd::Union{Int,Nothing}                = nothing
    compareLayout::String                   = "row"
    compareContrast::String                 = ""
    showTimestamp::Bool                     = true
    showScaleBar::Bool                      = true
    # The viewer's live channels + overlays snapshot. Opaque frontend-shaped dict; the recorder
    # reads specific keys (`colourBy`, `pointsSize`, `valueName`, …) via `get(look, "…", …)`.
    look::Any                               = nothing
    # An animation's render payload — `{viewState, steps}`. Nothing on a plain timelapse or
    # compare grid; recorded verbatim under the movie for reproducibility.
    keyframes::Any                          = nothing
end

# The batch flavour. `config` is the AUTHORED config the batch is applying (not a `look` snapshot);
# `imageUids` is the whole selection banked on EVERY movie in the batch so the edit side can reopen
# the authoring page — see `handle_movie_batch` for why the selection is stored on every row.
Base.@kwdef struct MovieBatchConfig <: MovieConfig
    # The authored config dict the batch applies to each image (versions + masks + layout + etc.).
    # Opaque to the recorder — passed through to `_render_grid_offline` / `_compare_grid` which
    # decompose it themselves.
    config::Any                             = nothing
    fileAttrs::Vector{String}               = String[]
    fps::Int                                = 15
    sizeX::Union{Int,Nothing}               = nothing
    sizeY::Union{Int,Nothing}               = nothing
    suffix::String                          = ""
    imageUids::Vector{String}               = String[]
end

# The declaration that keeps the on-disk shape a 1:1 rewrite of the pre-struct dict. Without it
# JSON3 falls back to `StructTypes.NoStructType()` and can silently drop fields the way an
# in-JSON3-dev-mode regression showed once. Pin both types so future struct changes are one
# recorded decision, not an implicit one.
# Reached through JSON3 to avoid adding StructTypes to api/Project.toml — JSON3 already depends on
# it, and this is the ONLY code in api/src that names a StructType.
JSON3.StructTypes.StructType(::Type{MovieRecordConfig}) = JSON3.StructTypes.Struct()
JSON3.StructTypes.StructType(::Type{MovieBatchConfig})  = JSON3.StructTypes.Struct()
