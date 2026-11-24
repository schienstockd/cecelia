# Where a set's images came from (app/src/data_source.jl, docs/todo/DATA_SOURCE_PLAN.md): the record
# and its shape rules, set vs image precedence, the import suggestions, and derived images keeping it.

@testset "Data source — record, precedence and image_set" begin
    # a source that says nothing is unknown, not an empty record
    @test data_source_from(Dict("citation" => "  ", "licence" => "CC-BY")) === nothing
    # a DOI given as a link or with a prefix is stored bare; a user's source is confirmed by default
    d = data_source_from(Dict("citation" => " HuBMAP HBM666.MCZF.973 ", "doi" => "https://doi.org/10.35079/HBM666.MCZF.973"))
    @test d.citation == "HuBMAP HBM666.MCZF.973" && d.doi == "10.35079/HBM666.MCZF.973"
    @test d.by == SOURCE_BY_USER && d.confirmed
    @test data_source_from(Dict("doi" => "doi: 10.1/x")).doi == "10.1/x"
    # an import suggestion is unconfirmed unless it says otherwise; an unknown `by` names the allowed ones
    @test !data_source_from(Dict("doi" => "10.1/x", "by" => "import")).confirmed
    @test_throws "expected one of: import, user" data_source_from(Dict("doi" => "10.1/x", "by" => "robot"))
    @test data_source_from(data_source_dict(d)) == d                    # wire form round-trips

    proj = create_project!(name = "ds-test-$(rand(1000:9999))")
    s  = add_set!(proj; name = "atlas")
    a  = add_image!(s; name = "a")
    b  = add_image!(s; name = "b")
    @test image_set(proj, a).uid == s.uid && image_set_index(proj, b.uid) == findfirst(==(s), proj._sets)
    @test image_set(proj, "no-such-uid") === nothing

    @test data_source(proj, a) === nothing                              # unknown
    set_data_source!(proj, s.uid, d)
    @test data_source(init_object(proj.uid, s.uid)) == d                # persisted on the set
    @test data_source(proj, a) == d && data_source_reply(proj, a)["from"] == "set"

    own = data_source_from(Dict("citation" => "Collaborator's movie", "url" => "https://example.org/m"))
    set_image_data_source!(a, own)
    a2 = init_object(proj.uid, a.uid)
    @test data_source(proj, a2) == own && data_source_reply(proj, a2)["from"] == "image"   # override wins
    @test data_source(proj, b) == d                                     # the others keep the set's
    set_image_data_source!(a2, nothing)                                 # clearing the override
    @test data_source(proj, init_object(proj.uid, a.uid)) == d
    set_data_source!(proj, s.uid, nothing)
    @test data_source(init_object(proj.uid, s.uid)) === nothing
end

@testset "Data source — import suggestions" begin
    # the real download path of the HuBMAP PhenoCycler dataset this was built for
    p = "/home/u/hubmap-downloads/HBM666.MCZF.973-8165df972cad95329f438dcb9a22d58d/raw/images/S16-B14-111223_Scan1.qptiff"
    s = suggest_data_source(p)
    @test s.repository == "hubmap" && s.id == "HBM666.MCZF.973"
    @test s.doi == "10.35079/HBM666.MCZF.973" && s.by == SOURCE_BY_IMPORT && !s.confirmed
    @test suggest_data_source("/data/lab/2026-01-12/mouse3.oir") === nothing
    @test suggest_data_source("/data/XHBM666.MCZF.9731/a.tif") === nothing   # not an ID on its own

    im = Dict{String,Any}("experimentId" => 7, "experimentName" => "LN T cells",
                          "experimentUrl" => "https://immunemap.org/e/7", "experimentDois" => Any[],
                          "dois" => Any["https://doi.org/10.1/abc"])
    m = immunemap_data_source(im)
    @test m.citation == "Immunemap: LN T cells" && m.doi == "10.1/abc" && m.id == "7" && !m.confirmed
    @test immunemap_data_source(Dict{String,Any}("experimentId" => 7)) === nothing
end

@testset "Data source — derived images keep it" begin
    # the image's own source rides SOURCE_META_KEYS with the raw file
    dst = Cecelia.copy_source_meta!(Dict{String,Any}(), Dict{String,Any}("ori_path" => "/x.tif",
                            "data_source" => Dict("citation" => "c")))
    @test dst["data_source"] == Dict("citation" => "c") && dst["ori_path"] == "/x.tif"

    # a source recorded on the SET reaches a derived image in ANOTHER set; an own one is not replaced
    proj = create_project!(name = "ds-derived-$(rand(1000:9999))")
    s = add_set!(proj; name = "atlas"); img = add_image!(s; name = "a")
    crops = add_set!(proj; name = "crop")
    d = data_source_from(Dict("citation" => "HuBMAP HBM666.MCZF.973"))
    set_data_source!(proj, s.uid, d)
    @test inherit_data_source!(Dict{String,Any}(), proj, img, crops)["data_source"] == data_source_dict(d)
    kept = Dict{String,Any}("data_source" => Dict("citation" => "own"))
    @test inherit_data_source!(kept, proj, img, crops)["data_source"] == Dict("citation" => "own")
    # staying in the source's set it gets no frozen copy: it resolves to the set's live, so a later
    # correction of the set's source reaches it
    @test !haskey(inherit_data_source!(Dict{String,Any}(), proj, img, s), "data_source")
    stay = add_image!(s; name = "a-crop", meta = inherit_data_source!(Dict{String,Any}(), proj, img, s))
    fixed = data_source_from(Dict("citation" => "HuBMAP HBM666.MCZF.973, Stanford TMC"))
    set_data_source!(proj, s.uid, fixed)
    @test data_source(proj, init_object(proj.uid, stay.uid)) == fixed
    @test !haskey(inherit_data_source!(Dict{String,Any}(), proj, add_image!(add_set!(proj; name = "z"); name = "q"), crops),
                  "data_source")
end

@testset "Data source — a moved image takes its new set's" begin
    proj = create_project!(name = "ds-move-$(rand(1000:9999))")
    s = add_set!(proj; name = "atlas"); other = add_set!(proj; name = "mine")
    a = add_image!(s; name = "a"); b = add_image!(s; name = "b")
    d = data_source_from(Dict("citation" => "HuBMAP HBM666.MCZF.973"))
    mine = data_source_from(Dict("citation" => "Collaborator"))
    set_data_source!(proj, s.uid, d); set_data_source!(proj, other.uid, mine)
    own = data_source_from(Dict("citation" => "Own"))
    set_image_data_source!(b, own)
    move_image!(proj, a.uid, s.uid, other.uid)
    move_image!(proj, b.uid, s.uid, other.uid)
    @test data_source(proj, init_object(proj.uid, a.uid)) == mine        # the set's source describes the set
    @test data_source(init_object(proj.uid, a.uid)) === nothing          # nothing frozen onto the image
    @test data_source(proj, init_object(proj.uid, b.uid)) == own         # its own is untouched
end

@testset "Data source — the Methods draft cites it" begin
    proj = create_project!(name = "ds-methods-$(rand(1000:9999))")
    s = add_set!(proj; name = "atlas")
    a = add_image!(s; name = "a"); b = add_image!(s; name = "b"); c = add_image!(s; name = "c")
    # no source anywhere: the lab's own data, no Data section
    @test !occursin("### Data", methods_report(proj; set_uid = s.uid)["markdown"])

    set_data_source!(proj, s.uid, data_source_from(Dict("citation" => "HuBMAP HBM666.MCZF.973",
        "doi" => "10.35079/HBM666.MCZF.973", "licence" => "CC BY 4.0")))
    set_image_data_source!(c, suggest_data_source("/d/HBM123.ABCD.456-x/raw/c.qptiff"))
    md = methods_report(proj; set_uid = s.uid)["markdown"]
    @test occursin("### Data\n\n- HuBMAP HBM123.ABCD.456 (doi:10.35079/HBM123.ABCD.456): 1 image. " *
                   "Suggested at import from the file path; not confirmed.\n" *
                   "- HuBMAP HBM666.MCZF.973 (doi:10.35079/HBM666.MCZF.973, CC BY 4.0): 2 images.\n", md)

    set_data_source!(proj, s.uid, nothing)                  # a and b now unknown
    @test occursin("- 2 images: data source not recorded.", methods_report(proj; set_uid = s.uid)["markdown"])
end

@testset "Data source — an exported figure's provenance carries it" begin
    proj = create_project!(name = "ds-prov-$(rand(1000:9999))")
    s = add_set!(proj; name = "atlas"); a = add_image!(s; name = "a")
    @test !haskey(only(board_export_provenance(proj, [a.uid])["images"]), "dataSource")
    set_data_source!(proj, s.uid, data_source_from(Dict("citation" => "HuBMAP HBM666.MCZF.973")))
    im = only(board_export_provenance(proj, [a.uid])["images"])
    @test im["dataSource"]["citation"] == "HuBMAP HBM666.MCZF.973" && im["dataSource"]["from"] == "set"
end
