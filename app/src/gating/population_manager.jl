using JSON3

# Population manager aggregator. The runtime is one namespace — nothing here `module`s the
# sub-files — they were split off by responsibility so 2529 L of tree structs, mutation API,
# persistence, boolean logic, cluster/palette resolution, pop_df, picker + allow-list and the
# mixed-type resolver do not sit on top of one another. Order matters for structs and consts
# only; function bodies resolve at call time, so downstream files can freely reference forward.
# See docs/POPULATION.md.
#
#   types.jl              — path helpers + PopType / BoolMembership / FilterFun / FilterCondition.
#   population.jl         — Population + PopulationMap (tree per value_name + traversal).
#   boolean.jl            — Boolean populations (Decision 16 combinators — filter, not stored).
#   mutations.jl          — the write API (add/rename/move/remove/gate/filter/colour swaps).
#   persistence.jl        — nested-tree (de)serialisation + `gating/{value_name}[__tracks].json` I/O.
#   clustering_colour.jl  — co-clustered segmentations + cluster-pop share + categorical palette.
#   pop_df.jl             — pop_df + Derived populations (the ONE population accessor).
#   picker.jl             — summary-canvas population picker + popScope (author-time selection).
#   allow_list.jl         — accepts allow-list (Decision 14) — one gate for pop-type mismatches.
#   mixed_resolution.jl   — mixed-type resolver for module-function pickers (any-pop-type row list).

include("popmanager/types.jl")
include("popmanager/population.jl")
include("popmanager/boolean.jl")
include("popmanager/mutations.jl")
include("popmanager/persistence.jl")
include("popmanager/clustering_colour.jl")
include("popmanager/pop_df.jl")
include("popmanager/picker.jl")
include("popmanager/allow_list.jl")
include("popmanager/mixed_resolution.jl")
