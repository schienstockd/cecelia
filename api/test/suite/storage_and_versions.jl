# Storage layout + VN versioning pilot + versions inventory + _json_safe testsets —
# extracted from api/test/runtests.jl.
#
# Four testsets covering the /api/storage/* and /api/versions/* surface plus the
# handler-side JSON coercion helper:
#  - `API: store layout defaults` (ZARR_V3_PLAN D10 — format/separator pre-fill).
#  - `API: keep-previous-version toggle` (VN versioning pilot).
#  - `API: /api/versions unions vN across images` (VN P3 chain-designer picker).
#  - `API: _json_safe reaches into NamedTuples, not just Dicts`.
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: store layout defaults" begin
    # DEFAULTS the import form pre-fills, not a switch over what happens next: format and separator are
    # fixed per image at import (no converter) and derived stores inherit. ZARR_V3_PLAN D10.
    st, body = api_store_layout_get(HTTP.Request("GET", "/api/storage/layout"))
    @test st == 200
    d = JSON3.read(body)
    @test d.default == "nested"                     # flat was removed — see below
    @test d.current in [String(c.name) for c in d.choices]
    @test !isempty(String(d.measuredOn))

    # FLAT IS NOT OFFERED AT ALL (2026-08-14). It was the default, on ~14% less disk at identical read
    # time — but re-measured on a real 3.5 GB movie the saving is ~5%, and a flat store conforms to no
    # published NGFF version: nested storage is what 0.2 introduced, so flat keys are 0.1 storage under
    # the 0.4-shaped metadata we write beside them. bioformats2raw stamped 0.1 for `--no-nested` while
    # our own writers stamped 0.4 for the identical layout. Removing the choice makes the
    # unnameable store unrepresentable, and retires the old flat+0.5 conflict with it.
    @test length(d.choices) == 2
    @test !any(String(c.chunkSeparator) == "flat" for c in d.choices)
    # every row carries its measured numbers, since that is the whole reason this is a table
    for c in d.choices
        for k in (:label, :keys, :dirs, :size, :read, :detail)
            @test !isempty(String(getproperty(c, k)))
        end
    end

    # bad input is rejected rather than silently persisted — this writes custom.toml
    @test _post(api_store_layout_set, Dict("name" => "nope"))[1] == 400
    @test _post(api_store_layout_set, Dict("name" => ""))[1] == 400
end

@testset "API: keep-previous-version toggle (VN versioning pilot)" begin
    # The Settings toggle that flips `Cecelia.keep_previous_version()` — a re-run of ingest (the
    # pilot writer) mints the next `vN` instead of overwriting. Same knob future autonomous
    # execution flips programmatically. Runs hermetic against the throwaway CECELIA_DEV_DIR.
    prior = Cecelia.keep_previous_version()
    try
        Cecelia.set_keep_previous_version!(false)
        st, body = api_keep_previous_version_get(HTTP.Request("GET", "/api/storage/keep-previous-version"))
        @test st == 200
        d = JSON3.read(body)
        @test d.current === false
        @test d.default === false                          # global default is off

        @test _post(api_keep_previous_version_set, Dict("value" => true))[1] == 200
        @test Cecelia.keep_previous_version() === true

        @test _post(api_keep_previous_version_set, Dict("value" => false))[1] == 200
        @test Cecelia.keep_previous_version() === false

        # missing key + wrong type both 400 — no silent partial toggles
        @test _post(api_keep_previous_version_set, Dict{String,Any}())[1]      == 400
        @test _post(api_keep_previous_version_set, Dict("value" => "yes"))[1]  == 400
        @test _post(api_keep_previous_version_set, Dict("value" => 1))[1]      == 400
    finally
        Cecelia.set_keep_previous_version!(prior)
    end
end

@testset "API: /api/versions unions vN across images (VN P3 chain-designer picker)" begin
    # Feeds the chain-designer's per-node "Input version" picker. A chain runs against N images
    # that may carry different vN sets per value_name — the endpoint returns the UNION so the
    # picker offers every real vN, and per-image resolution happens at run time in the reader
    # (`versioned_get_field_at`). Legacy bare-scalar shape counts as implicit v1.
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        puid = "VNVER"; iuidA = "IMGA"; iuidB = "IMGB"; iuidC = "IMGC"
        mkpath(joinpath(tmp, puid, "1", iuidA))
        mkpath(joinpath(tmp, puid, "1", iuidB))
        mkpath(joinpath(tmp, puid, "1", iuidC))
        write(joinpath(tmp, puid, "project.json"),
              JSON3.write((; uid = puid, name = "T", set_uids = String[])))

        # Image A: versioned entry with v1, v2, v3 (_latest=v3) under `default`.
        write(state_file(joinpath(tmp, puid), iuidA), JSON3.write(Dict{String,Any}(
            "class" => "CciaImage",
            "filepath" => Dict{String,Any}(
                "default" => Dict{String,Any}(
                    "v1" => "a1.zarr", "v2" => "a2.zarr", "v3" => "a3.zarr", "_latest" => "v3"),
                "_active" => "default"))))
        # Image B: versioned entry with v1, v2 only (_latest=v2).
        write(state_file(joinpath(tmp, puid), iuidB), JSON3.write(Dict{String,Any}(
            "class" => "CciaImage",
            "filepath" => Dict{String,Any}(
                "default" => Dict{String,Any}("v1" => "b1.zarr", "v2" => "b2.zarr", "_latest" => "v2"),
                "_active" => "default"))))
        # Image C: legacy bare scalar (no version dict) — counts as implicit v1.
        write(state_file(joinpath(tmp, puid), iuidC), JSON3.write(Dict{String,Any}(
            "class" => "CciaImage",
            "filepath" => Dict{String,Any}("default" => "c.zarr", "_active" => "default"))))

        _get(url) = api_versions_list(HTTP.Request("GET", url))

        # Union across A + B, explicit valueName — v1, v2, v3 (numeric-sorted).
        st, body = _get("/api/versions?projectUid=$puid&imageUids=$iuidA,$iuidB&valueName=default")
        @test st == 200
        @test JSON3.read(body).versions == ["v1", "v2", "v3"]

        # valueName omitted ⇒ backend uses active (which is `default` here) — same answer.
        st, body = _get("/api/versions?projectUid=$puid&imageUids=$iuidA,$iuidB")
        @test st == 200
        @test JSON3.read(body).versions == ["v1", "v2", "v3"]

        # Just image B — only v1, v2 surface.
        st, body = _get("/api/versions?projectUid=$puid&imageUids=$iuidB&valueName=default")
        @test st == 200
        @test JSON3.read(body).versions == ["v1", "v2"]

        # Just image C (legacy bare scalar) — implicit v1.
        st, body = _get("/api/versions?projectUid=$puid&imageUids=$iuidC&valueName=default")
        @test st == 200
        @test JSON3.read(body).versions == ["v1"]

        # A value_name that doesn't exist on any image — empty list (not 404).
        st, body = _get("/api/versions?projectUid=$puid&imageUids=$iuidA&valueName=nope")
        @test st == 200
        @test JSON3.read(body).versions == []

        # An unknown image uid is skipped, not fatal — the endpoint fields whatever it can. A chain
        # can carry a run-set uid that was renamed since; refusing the whole request would blank the
        # picker for the sibling images that are still there.
        st, body = _get("/api/versions?projectUid=$puid&imageUids=NOPE,$iuidA&valueName=default")
        @test st == 200
        @test JSON3.read(body).versions == ["v1", "v2", "v3"]

        # Guards: missing projectUid / imageUids → 400; unknown project → 404.
        @test _get("/api/versions?imageUids=$iuidA")[1] == 400
        @test _get("/api/versions?projectUid=$puid")[1] == 400
        @test _get("/api/versions?projectUid=NOPE&imageUids=$iuidA")[1] == 404
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

# ── `_json_safe` covers the shape handlers actually return ────────────────────
#
# JSON has no NaN/Inf literal, so JSON3.write throws — and the analysis layer emits NaN deliberately
# for "not assessed" (a drift p-value with too few decorrelated steps, an sem at n=1, a step scale with
# nothing tracked). `_json_safe` maps non-finite → null so the client sees a gap instead of a 500.
#
# It claimed to be recursive and was not, for NamedTuples — which is how most handlers shape a
# response. `/api/tracking/diagnostics` 500-ed on a real image for exactly this. The method is pinned
# here because the failure is invisible until a specific dataset produces a NaN in a specific field.
@testset "API: _json_safe reaches into NamedTuples, not just Dicts" begin
    @test _json_safe(NaN) === nothing
    @test _json_safe(Inf) === nothing
    @test _json_safe(1.5) == 1.5

    nt = _json_safe((; p = NaN, n = 3, nested = (; sem = [1.0, NaN], ok = "x")))
    @test nt.p === nothing
    @test nt.n == 3
    @test nt.nested.sem == [1.0, nothing]
    @test nt.nested.ok == "x"
    # a NamedTuple stays a NamedTuple, so the response key ORDER survives the sanitising pass
    @test nt isa NamedTuple && keys(nt) == (:p, :n, :nested)
    # and the whole point: it now serialises
    @test occursin("\"p\":null", JSON3.write(nt))

    @test _json_safe(Dict("a" => NaN))["a"] === nothing
    @test _json_safe(Any[NaN, 2.0]) == Any[nothing, 2.0]
end
