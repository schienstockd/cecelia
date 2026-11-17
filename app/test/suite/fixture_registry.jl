# ── fixture registry — test-data/fixtures.toml says only what the data backs; NEEDS.toml stays honest ──
# The helper is app/test/fixture_registry.jl; docs/DEV.md → *Ask for properties, not a path*.

@testset "fixture registry: every checked property is a fact of the data" begin
    vocab = fixture_vocabulary()
    checked = Set(p for (p, v) in vocab if v.checked)
    for fx in materialise.(registered_fixtures())
        @test isdir(something(fx.dir, fx.image_dir))
        @test all(in(keys(vocab)), fx.properties)
        facts = fixture_facts(fx)
        @test Set(keys(facts)) == checked         # a new checked property needs its fact check
        for p in checked
            # both directions: a claim the data doesn't back, and a property it has but doesn't list
            # (an unlisted property makes fixture_for file a need that is already met)
            (p in fx.properties) == facts[p] ||
                @error "fixtures.toml → $(fx.uid): `$p` is listed=$(p in fx.properties) but the data says $(facts[p])"
            @test (p in fx.properties) == facts[p]
        end
    end
end

@testset "fixture registry: the fact checks fail on the stand-in that started this" begin
    # KDIeEm has no pixel size, so its µm are pixels: listing it as calibrated must fail the check
    # above. Validate the guard against the bug it exists for, not just against the honest file.
    # MAINT-EXEMPT: the registry's own tests name fixtures by uid on purpose
    kd = only(filter(fx -> fx.uid == "KDIeEm", registered_fixtures()))
    facts = fixture_facts(kd)
    @test facts[:calibrated] == false
    @test facts[:movie] && !facts[:still] && facts[Symbol("3d")]
    @test facts[Symbol("marker-names")] == false          # ch0..ch3
    zv = only(filter(fx -> fx.uid == "ZV2img", registered_fixtures()))
    @test fixture_facts(zv)[:calibrated] == true          # 0.5964 µm/px from the NGFF scale
end

@testset "fixture registry: fixture_for returns a match, never a stand-in" begin
    fx = fixture_for(:cells, :movie, :tracked)
    @test fx isa RegisteredFixture && fx.uid == "KDIeEm"
    @test isfile(fixture_table(fx))
    @test fixture_for(:calibrated, Symbol("image-store")) isa RegisteredFixture
    # MAINT-EXEMPT: the registry's own tests name fixtures by uid on purpose
    @test fixture_for(:cells; technology = "two-photon").uid == "KDIeEm"
    # the calibrated variant: the same real cells, a temp copy whose ccid.json carries a pixel size
    cal = fixture_for(:cells, :movie, :calibrated)
    @test cal.uid == "KDIeEm-cal" && startswith(cal.project, tempdir())
    sizes, _ = Cecelia.img_physical_sizes(Cecelia._load_image(cal.dir))
    @test sizes == [2.0, 0.5, 0.5]
    @test cal.project != fixture_for(:cells, :movie, :calibrated).project   # fresh copy per call
    # no fixture has these: with a need, nothing (and the test skips); without one, an error
    @test fixture_for(:cells, :calibrated, :still, :structure; need = "multiplex-structure-still") === nothing
    @test_throws ErrorException fixture_for(:cells, :calibrated, :still, :structure)
    @test_throws ErrorException fixture_for(:cells, :calibrated; need = "no-such-request")
    @test_throws ErrorException fixture_for(:calibratd)       # a typo is not an empty match
end

@testset "fixture registry: NEEDS.toml requests are well formed and still open" begin
    vocab = fixture_vocabulary()
    fixtures = registered_fixtures()
    for (id, n) in fixture_needs()
        @test all(p -> haskey(vocab, Symbol(p)), n["properties"])
        @test all(k -> haskey(n, k), ("why", "waiting", "budget", "opened", "status", "synthetic"))
        props = Symbol.(n["properties"])
        tech = get(n, "technology", nothing)
        meets = [fx.uid for fx in fixtures if all(in(fx.properties), props) &&
                 (tech === nothing || fx.technology == tech)]
        if n["status"] == "open"
            # a registered fixture meets it: mark it `met: <uid>` so the list doesn't go stale
            isempty(meets) || @error "NEEDS.toml → $id is met by $(meets); set status = \"met: $(first(meets))\""
            @test isempty(meets)
        else
            m = match(r"^met: (\S+)$", n["status"])
            @test m !== nothing && m[1] in meets
        end
    end
end

@testset "fixture registry: every need = \"…\" a test names is in NEEDS.toml" begin
    ids = Set(keys(fixture_needs()))
    repo = normpath(joinpath(_TEST_DATA_ROOT, ".."))
    named = Set{String}()
    for dir in ("app/test", "api/test", "python/cecelia/tests", "mcp/tests"), (d, _, files) in walkdir(joinpath(repo, dir))
        for f in files
            (endswith(f, ".jl") || endswith(f, ".py")) || continue
            f in ("fixture_registry.jl", "fixture_registry.py", "test_fixture_registry.py") && continue
            for m in eachmatch(r"need\s*=\s*\"([a-z0-9-]+)\"", read(joinpath(d, f), String))
                push!(named, m[1])
            end
        end
    end
    @test isempty(setdiff(named, ids))
end
