# RESULT-REVIEW: result-9e123c3f
# The gating-inputs stamp must change on ANY edit to a gating map, including a same-length edit to a map
# of 8192+ bytes. Julia's `hash` of a byte vector that long samples only some elements, so a stamp built
# on `hash(read(file))` missed e.g. a cluster pop repointed 17→18 in a ~40-pop map — and with the cache
# living across requests, the overlay then served the old membership indefinitely.

@testset "gating-inputs stamp sees a same-length edit to a large gating map" begin
    td = mktempdir(); mkpath(joinpath(td, "gating")); mkpath(joinpath(td, "labelProps"))
    img = CciaImage(uid="StmpLg", dir=td)
    # A pretty-printed 40-pop map (~11 KB) of varied text, like a real one — not padding: `hash` collapses
    # runs of equal bytes, which changes what it samples. Fixed seed, so the map is the same every run.
    # Deterministic varied text (no RNG): each pop's uid / colour / note differ from every other's.
    v(i, j) = string(mod(i * 7919 + j * 104729, 99991))
    function big(ids)
        pops = ["    {\n      \"name\": \"Cluster $(lpad(i, 2, '0'))\",\n      \"uid\": \"u$(v(i, 1))x$(v(i, 2))\",\n" *
                "      \"colour\": \"#$(lpad(string(mod(i * 40503, 16^6); base = 16), 6, '0'))\",\n      \"show\": true,\n" *
                "      \"clusters\": [$(ids[i])],\n      \"note\": \"$(join((v(i, j) for j in 3:30), "-"))\"\n    }"
                for i in eachindex(ids)]
        "{\n  \"pops\": [\n" * join(pops, ",\n") * "\n  ]\n}\n"
    end
    ids = collect(10:49)                                   # 40 two-digit ids, each edit keeps the length
    f = joinpath(td, "gating", "B.json")
    write(f, big(ids))
    @test filesize(f) >= 8192
    st = Cecelia._gating_inputs_stamp(img, "B")
    unchanged = 0
    for k in eachindex(ids)
        ids2 = copy(ids); ids2[k] = ids[k] == 99 ? 98 : ids[k] + 1   # same width, same file length
        write(f, big(ids2))
        Cecelia._gating_inputs_stamp(img, "B") == st && (unchanged += 1)
    end
    @test unchanged == 0
end
