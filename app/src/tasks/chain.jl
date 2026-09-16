import SHA

# Chain module aggregator. The runtime is one namespace — nothing here `module`s the sub-files —
# they were split off by responsibility so the execution engine (~800 L) does not sit next to the
# type definitions, the disk I/O, and the public entry points. Order matters for structs and consts
# only; function bodies resolve at call time, so downstream files can freely reference forward.
#
#   types.jl       — enums, ChainNode/ChainEdge/ChainTemplate/ImageNodeState/ChainRun structs, and
#                    the two `parse_chain_*` string↔enum helpers.
#   validate.jl    — CHAIN_SCOPES constants, ChainTemplateError, root/reachability, pool-name and
#                    axis-shape checks, plus the template/params hashing used by the cache and by
#                    the runtime's staleness comparison.
#   persistence.jl — filesystem layout (`settings/chains/`), template serde + load/save, the
#                    content-addressed template cache (SHA-keyed), and `_save_run!` (the per-node
#                    JSON write after every state transition).
#   execute.jl     — the ~800-line execution engine: topo sort, per-image threads, barriers,
#                    resume/reset, and the three node runners (image-scope, set-scope, incremental).
#   api.jl         — the public entry points: `load_chain_run`, `run_chain` (two methods),
#                    `chain_node`, `make_chain`.
#
# ── Chain data model: template vs. run ────────────────────────────────────────
#
# Two distinct artifacts:
#
#   ChainTemplate  — reusable, no images baked in. Lives at
#                    <project>/settings/chains/<name>.json. Editing a template never
#                    retroactively changes a completed run.
#
#   ChainRun       — created when a template is applied to a set of images.
#                    Stores a FROZEN COPY of the template at run time (not a
#                    pointer to the template file). Per-image per-node state
#                    is persisted to <project>/settings/chains/runs/<run_id>/run.json
#                    after every node completion.
include(joinpath(@__DIR__, "chain", "types.jl"))
include(joinpath(@__DIR__, "chain", "validate.jl"))
include(joinpath(@__DIR__, "chain", "persistence.jl"))
include(joinpath(@__DIR__, "chain", "execute.jl"))
include(joinpath(@__DIR__, "chain", "api.jl"))
