# ── RO-Crate of a chain run (app/src/ai/ro_crate.jl; docs/ANALYSIS.md → *RO-Crate export*) ──
# A real chain run — the measure step on the committed label-store fixture — exported as a crate, then
# checked against the MUST list of the profiles it claims, read from the specifications, not the code:
#   RO-Crate 1.1            https://www.researchobject.org/ro-crate/1.1/ (metadata descriptor, root data
#                           entity: name/description/datePublished/license, data entities via hasPart)
#   Process Run Crate 0.5   https://w3id.org/ro/wfrun/process/0.5 (root conformsTo; CreateAction @id +
#                           instrument → SoftwareApplication | SoftwareSourceCode | ComputationalWorkflow)
#   Workflow Run Crate 0.5  https://w3id.org/ro/wfrun/workflow/0.5 (root conformsTo; ComputationalWorkflow
#                           mainEntity; at least one CreateAction with the mainEntity as instrument)
# The official validator (`rocrate-validator`, CRS4) is not in the pixi env; docs/ANALYSIS.md has the
# command that ran it on this crate.

# A stored ZIP's entries, read through its central directory (APPNOTE §4.3.12 / §4.3.16)
function _roc_unzip(bytes::Vector{UInt8})
    u16(i) = Int(bytes[i]) | Int(bytes[i + 1]) << 8
    u32(i) = u16(i) | u16(i + 2) << 16
    eocd = findlast(i -> u32(i) == 0x06054b50, 1:length(bytes) - 21)
    n, cd = u16(eocd + 10), u32(eocd + 16) + 1
    out = Pair{String,Vector{UInt8}}[]
    for _ in 1:n
        @assert u32(cd) == 0x02014b50
        crc, size, nlen, off = u32(cd + 16), u32(cd + 24), u16(cd + 28), u32(cd + 42) + 1
        name = String(bytes[cd + 46:cd + 45 + nlen])
        @assert u32(off) == 0x04034b50 && u16(off + 8) == 0          # local header, method 0 = stored
        start = off + 30 + u16(off + 26) + u16(off + 28)
        data = bytes[start:start + size - 1]
        @assert crc32_ieee(data) == crc
        push!(out, name => data)
        cd += 46 + nlen + u16(cd + 30) + u16(cd + 32)
    end
    out
end

@testset "RO-Crate — the ZIP's CRC-32 is the published one" begin
    # the CRC-32 check value: CRC of the ASCII digits "123456789" (APPNOTE §4.4.7 / IEEE 802.3)
    @test crc32_ieee(Vector{UInt8}(codeunits("123456789"))) == 0xCBF43926
    @test crc32_ieee(UInt8[]) == 0x00000000
    files = ["a.txt" => Vector{UInt8}(codeunits("hello")), "dir/b.json" => Vector{UInt8}(codeunits("{}"))]
    bytes = take!(write_stored_zip(IOBuffer(), files; modified = DateTime(2026, 11, 22)))
    @test _roc_unzip(bytes) == files
    @test take!(write_stored_zip(IOBuffer(), files; modified = DateTime(2026, 11, 22))) == bytes   # same input, same bytes
end

@testset "RO-Crate — a chain run is a Workflow Run Crate" begin
    fx = fixture_for(:cells, Symbol("image-store"), Symbol("label-store"))
    td = mktempdir()
    pu = basename(fx.project)
    cp(fx.project, joinpath(td, pu))
    write(joinpath(td, pu, "project.json"),
          JSON3.write(Dict("class" => "CciaProject", "uid" => pu, "name" => "crate", "set_uids" => [])))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = td   # in memory only — never set_projects_dir
        proj = load_project(pu)
        save_chain_template!(proj, ChainTemplate("crate-measure",
            [ChainNode(id = "m", fn = "segment.measureLabels", scope = "image",
                       params = Dict{String,Any}("outputValueName" => fx.value_name, "intensityValueName" => "default"))],
            ChainEdge[]))
        logs = String[]
        run = run_chain(proj, [fx.uid]; chain = "crate-measure", on_log = l -> push!(logs, l))
        @test run.image_states[fx.uid]["m"].status == NODE_DONE || error(join(logs, "\n"))
        # the step was Claude's: the stamp the scheduler writes for a Claude-started run
        img = init_object(pu, fx.uid)
        model = "claude-opus-4-1-20250805"
        log = read_run_log(img)
        for e in log
            e["taskId"] == run.image_states[fx.uid]["m"].task_id &&
                (e["author"] = Cecelia.run_author("default", Dict("via" => "claude", "model" => model)))
        end
        write(Cecelia.run_log_path(img._dir), JSON3.write(log))

        day = Date(2026, 11, 22)
        zip = joinpath(td, "crate.zip")
        @test chain_run_ro_crate(proj, run.id, zip; published = day) == zip
        files = _roc_unzip(read(zip))
        names = first.(files)
        # the four files, nothing else: no image bytes, no label store, no cell table
        @test sort(names) == sort(["ro-crate-metadata.json", "methods.md", "parameters.csv", "chain_template.json"])
        @test all(n -> !occursin(r"\.(zarr|h5ad|tiff?|ome)"i, n), names)
        @test sum(length ∘ last, files) < 64_000
        # the directory form holds the same bytes, and the same record + date gives the same crate
        dir = chain_run_ro_crate(proj, run.id, joinpath(td, "crate"); published = day)
        @test all(read(joinpath(dir, n)) == b for (n, b) in files)
        @test chain_run_ro_crate_files(proj, run.id; published = day) == files
        f = Dict(files)
        # the report's text and table, not a second assembly
        rep = methods_report(proj; run_id = run.id)
        @test String(f["methods.md"]) == rep["markdown"]
        @test String(f["parameters.csv"]) == rep["csv"]
        @test startswith(rep["csv"], "step,task,output,images,last_run,changed_from_default,differs_between_images,note\n")
        # the frozen template is the run's, by its content hash
        @test bytes2hex(Cecelia.SHA.sha256(f["chain_template.json"])) == run.template_hash

        doc = JSON3.read(f["ro-crate-metadata.json"])
        # read in order: the context, then the graph; each entity opens with its @id and @type
        @test collect(keys(doc)) == [Symbol("@context"), Symbol("@graph")]
        @test all(e -> collect(keys(e))[1:2] == [Symbol("@id"), Symbol("@type")], doc[Symbol("@graph")])
        graph = Dict(String(e[:id_]) => e for e in [merge(Dict(:id_ => x[Symbol("@id")]), x) for x in doc[Symbol("@graph")]])
        ids(v) = v === nothing ? String[] : v isa AbstractVector ? [String(x[Symbol("@id")]) for x in v] : [String(v[Symbol("@id")])]
        types(e) = (t = e[Symbol("@type")]; t isa AbstractString ? [String(t)] : String.(t))
        prop(e, k) = get(e, Symbol(k), nothing)

        # RO-Crate 1.1: the context and the metadata descriptor
        @test "https://w3id.org/ro/crate/1.1/context" in doc[Symbol("@context")]
        desc = graph["ro-crate-metadata.json"]
        @test "CreativeWork" in types(desc) && ids(prop(desc, "about")) == ["./"]
        @test "https://w3id.org/ro/crate/1.1" in ids(prop(desc, "conformsTo"))
        # … the root data entity and its required properties
        root = graph["./"]
        @test "Dataset" in types(root)
        @test all(k -> prop(root, k) !== nothing && !isempty(prop(root, k)), ("name", "description", "datePublished", "license"))
        @test prop(root, "datePublished") == "2026-11-22"
        @test occursin("not included", prop(root, "description"))
        # … every data entity reachable through hasPart, every file that is in the payload actually there
        parts = ids(prop(root, "hasPart"))
        for (id, e) in graph
            any(in(("File", "Dataset")), types(e)) && id != "./" && @test id in parts
            any(in(("File", "Dataset")), types(e)) && !startswith(id, "#") && id != "./" && @test id in names
        end
        # Process + Workflow Run Crate: the root conforms to both, each a CreativeWork in the graph
        for p in ("https://w3id.org/ro/wfrun/process/0.5", "https://w3id.org/ro/wfrun/workflow/0.5")
            @test p in ids(prop(root, "conformsTo")) && "CreativeWork" in types(graph[p])
        end
        # Workflow Run Crate: the mainEntity is the workflow, and a CreateAction ran it
        main = only(ids(prop(root, "mainEntity")))
        @test main == "chain_template.json" && "ComputationalWorkflow" in types(graph[main])
        actions = [e for e in values(graph) if "CreateAction" in types(e)]
        @test count(a -> ids(prop(a, "instrument")) == [main], actions) == 1
        # Process Run Crate: every action names its tool, which is a software entity, and is mentioned
        for a in actions
            tool = graph[only(ids(prop(a, "instrument")))]
            @test any(in(("SoftwareApplication", "SoftwareSourceCode", "ComputationalWorkflow")), types(tool))
            @test String(a[Symbol("@id")]) in ids(prop(root, "mentions"))
            # start, end and status on every action, the times ISO 8601 with an offset
            @test all(k -> occursin(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\+00:00$", string(prop(a, k))), ("startTime", "endTime"))
            @test prop(a, "actionStatus") == "http://schema.org/CompletedActionStatus"
        end
        # one action per node × image, besides the run's
        step = only(a for a in actions if ids(prop(a, "instrument")) != [main])
        # inputs and outputs by their path inside the project, not copied
        res = [graph[i] for i in ids(prop(step, "result"))]
        @test any(e -> prop(e, "alternateName") == "1/$(fx.uid)/labelProps/$(fx.value_name).h5ad", res)
        obj = [graph[i] for i in ids(prop(step, "object"))]
        @test any(e -> startswith(string(prop(e, "alternateName")), "0/$(fx.uid)/"), obj)
        @test all(e -> isfile(joinpath(proj.root, prop(e, "alternateName"))) || isdir(joinpath(proj.root, prop(e, "alternateName"))),
                  vcat(res, obj))
        # the person from the profile stamp, and Claude with the recorded model
        agents = [graph[i] for i in ids(prop(step, "agent"))]
        @test any(e -> "Person" in types(e) && prop(e, "name") == "default", agents)
        @test any(e -> prop(e, "name") == "Claude" && prop(e, "softwareVersion") == model, agents)
        # Cecelia, at the version the run log recorded, citing its paper
        tool = graph[only(ids(prop(step, "instrument")))]
        cec = graph[only(ids(prop(tool, "softwareRequirements")))]
        @test prop(cec, "softwareVersion") == Cecelia.cecelia_version()
        @test ids(prop(cec, "citation")) == ["https://doi.org/10.1038/s41467-025-57193-y"]
        @test_throws Exception chain_run_ro_crate_files(proj, "nope")
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
end
