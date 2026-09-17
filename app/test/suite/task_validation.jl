# ── Task validation + platform ratchets testsets ──────────────────────
# Five sections covering: channelSelection params resolve through channel_indices, zarr
# access routes through the canonical helpers (Julia-side ratchet), a process exit check
# also checks termsignal (libuv exitcode==0 on signal-kill), dirPath param validation, and
# units written into OME-XML are schema-valid symbols. Extracted from suite.jl to keep it
# small enough to merge without EOF conflicts on every append. The extracted file loads
# inside this file's aggregating testset scope, so any helpers defined earlier in
# suite.jl are still in scope (lexical include).
#
# Five `joinpath(@__DIR__, "..", "src", ...)` and `joinpath(@__DIR__, "..", "..", "api",
# "src", ...)` scans (channelSelection + zarr-access + process-exit walk both app/src and
# api/src) are rerouted through pathof(Cecelia) via `_app_src` / `_api_src` so they
# resolve identically whether the file sits at app/test/ or app/test/suite/.

_repo    = dirname(dirname(dirname(pathof(Cecelia))))
_app_src = joinpath(_repo, "app", "src")
_api_src = joinpath(_repo, "api", "src")

@testset "channelSelection params resolve through channel_indices" begin
    # A `channelSelection` param submits channel NAMES. `channel_indices` is the one resolver — it
    # takes names OR already-resolved indices, returns 0-based values, and errors by name on a miss
    # (with a case-difference hint). Its own comment records SIX handlers that had hand-rolled
    # `findfirst(==(String(ch)), ch_names)` and drifted into three different wrong behaviours: an
    # index crashed four of them, an unmatched name was silently DROPPED by five, and drift
    # correction silently fell back to channel 0 — registering a whole timelapse against SHG.
    #
    # It happened again on the OME-TIFF export (`Int("SHG")` → MethodError, straight out of the task),
    # which is why this is now a test rather than a comment.
    function _has_channel_param(ps)::Bool
        for p in ps
            p isa AbstractDict || continue
            String(get(p, "type", "")) == "channelSelection" && return true
            inner = get(p, "params", nothing)
            inner isa AbstractVector && _has_channel_param(inner) && return true
        end
        false
    end

    checked = String[]
    for (fun, task) in Cecelia._fun_name_map()
        spec = Cecelia._task_spec(task)
        isnothing(spec) && continue
        _has_channel_param(get(spec, "params", [])) || continue
        spec_path = Cecelia._spec_path(task)
        isnothing(spec_path) && continue
        jl = replace(spec_path, r"\.json$" => ".jl")
        isfile(jl) || continue        # a composite resolves nothing itself; its steps are checked
        push!(checked, fun)
        # A task file may have been split into a family (e.g. af_correct/{translate,qc,run}.jl next
        # to af_correct.jl) — check the aggregator PLUS any siblings under a same-named dir, since
        # the handler that actually calls `channel_indices` may live in a sub-file.
        family = String[jl]
        subdir = replace(jl, r"\.jl$" => "")
        if isdir(subdir)
            for f in readdir(subdir; join = true)
                endswith(f, ".jl") && push!(family, f)
            end
        end
        combined = join((read(f, String) for f in family), "\n")
        @test occursin("channel_indices", combined) ||
              error("$fun declares a channelSelection param but its handler never calls " *
                    "`channel_indices`. Resolve names with it (0-based, errors by name) rather " *
                    "than converting them by hand — see CLAUDE.md and channel_index's own comment.")
    end
    # The scan must actually find tasks; a rename that silently matched nothing would "pass".
    @test length(checked) >= 8
end

@testset "zarr access routes through the canonical helpers" begin
    # OME-ZARR is the same rule as `.h5ad`: one set of readers per language, and re-opening a store
    # by hand is how the two variants drift (CLAUDE.md → *Image / OME-ZARR access*). Julia has a
    # metadata tier in `app/src/tasks/importImages/omezarr.jl` (`series_base`, `ngff_attrs`,
    # `ngff_multiscales`, `zarr_array_meta`, `ngff_version` — exported through `Cecelia.jl`) and a
    # narrow display-only pixel tier in `api/src/image_geometry.jl` (`open_level` / `open_level0` /
    # `read_native`). Every other caller routes through those helpers.
    #
    # Two ways this rule is bypassed in practice — a bare `zopen(...)` / `Zarr.open(...)`, and a
    # `joinpath(..., ".zattrs" | ".zarray" | "zarr.json")` that reads the JSON itself instead of
    # going through `ngff_attrs`/`zarr_array_meta`. Both regressions have shipped before (the
    # `read_ngff_axes` note in `image_geometry.jl` records the second: reading `.zattrs` directly
    # made every v3 store answer EMPTY).
    #
    # Docstring mentions use backticks (`` `.zattrs` ``), which don't match the double-quoted literal.
    #
    # Sanctioned owners:
    #   * `importImages/omezarr.jl` (aggregator) and every `importImages/omezarr/*.jl` sub-file
    #     collectively define the metadata reader/writer tier — those are the files allowed to read
    #     the raw `.zattrs` / `.zarray` / `zarr.json` files.
    #   * `image_geometry.jl` defines `open_level`, so it is the one file allowed to call `zopen`.

    # ".zattrs", ".zarray", "zarr.json" as a code literal (a path join into a store) — but not the
    # docstring form `` `.zattrs` ``. `\\.` in raw-string form is the literal dot.
    literal_re = r"""\"\.zattrs\"|\"\.zarray\"|\"zarr\.json\""""
    # `zopen(...)` or `Zarr.open(...)` — the two ways a store gets opened via `Zarr.jl`.
    open_re    = r"\bzopen\s*\(|\bZarr\.open\s*\("

    # Path fragment (portable on Windows via joinpath). Any file under the omezarr/ family owns
    # the raw-JSON reads; the aggregator's own basename is the sentinel for the parent file itself.
    omezarr_family = joinpath("importImages", "omezarr")
    allowed_open   = Set(["image_geometry.jl"])

    literal_hits = String[]
    open_hits    = String[]
    for root in (_app_src, _api_src)
        isdir(root) || continue
        for (dir, _, files) in walkdir(root), f in files
            endswith(f, ".jl") || continue
            path = joinpath(dir, f)
            # rel key is enough to distinguish siblings — importImages/omezarr.jl vs the base name.
            in_omezarr_family = occursin(omezarr_family, path)
            rel  = in_omezarr_family ? joinpath(omezarr_family, f) : f
            for (i, ln) in enumerate(eachline(path))
                startswith(strip(ln), "#") && continue          # comments don't count
                if occursin(literal_re, ln) && !in_omezarr_family
                    push!(literal_hits, "$rel:$i  $(strip(ln))")
                end
                if occursin(open_re, ln) && !(rel in allowed_open)
                    push!(open_hits, "$rel:$i  $(strip(ln))")
                end
            end
        end
    end

    isempty(literal_hits) || error(
        "bare `.zattrs` / `.zarray` / `zarr.json` read outside `importImages/omezarr.jl`:\n  " *
        join(literal_hits, "\n  ") *
        "\nRoute through `ngff_attrs` / `ngff_multiscales` / `zarr_array_meta` / `ngff_version` — " *
        "reading `.zattrs` directly made every v3 store answer EMPTY once already " *
        "(see docs/todo/ZARR_V3_PLAN.md and CLAUDE.md → *Image / OME-ZARR access*).")

    isempty(open_hits) || error(
        "bare `zopen` / `Zarr.open` outside `api/src/image_geometry.jl`:\n  " *
        join(open_hits, "\n  ") *
        "\nOpen a store through `open_level` / `open_level0` and read pixels with `read_native` — " *
        "the display-only carve-out. Anything that PROCESSES data reads through Python `zarr_utils` " *
        "(CLAUDE.md → *Image / OME-ZARR access*).")

    @test isempty(literal_hits) && isempty(open_hits)

    # The scan must actually reach the sanctioned owners; a wrong root would let a real offender
    # slip through with an empty offender list.
    saw_literal_owner = false
    saw_open_owner    = false
    for root in (_app_src, _api_src),
        (dir, _, files) in walkdir(root), f in files
        endswith(f, ".jl") || continue
        path = joinpath(dir, f)
        occursin(omezarr_family, path) &&
            occursin(literal_re, read(path, String)) && (saw_literal_owner = true)
        f == "image_geometry.jl" &&
            occursin(open_re, read(path, String)) && (saw_open_owner = true)
    end
    @test saw_literal_owner
    @test saw_open_owner
end

@testset "a process exit check also checks termsignal" begin
    # libuv reports `exitcode = 0` for a SIGNAL-KILLED child, and `task:cancel` kills by design — so
    # `exitcode == 0` alone reads a cancelled or timed-out process as a clean success. That is how a
    # timed-out agent run had its TRUNCATED output handed to the result parser.
    offenders = String[]
    for root in (_app_src, _api_src)
        isdir(root) || continue
        for (dir, _, files) in walkdir(root), f in files
            endswith(f, ".jl") || continue
            path  = joinpath(dir, f)
            lines = readlines(path)
            for (i, ln) in enumerate(lines)
                occursin(".exitcode", ln) || continue
                # A window, not the same line: the check is often split over two lines, or guarded by
                # a `killed` flag derived from termsignal a few lines above.
                lo, hi = max(1, i - 6), min(length(lines), i + 6)
                any(occursin("termsignal", lines[j]) for j in lo:hi) && continue
                push!(offenders, "$(basename(path)):$i  $(strip(ln))")
            end
        end
    end
    isempty(offenders) && @test true
    isempty(offenders) || error("`.exitcode` used without a nearby `termsignal` check:\n  " *
                                join(offenders, "\n  ") *
                                "\nlibuv sets exitcode 0 for a signal-killed process — check " *
                                "`proc.exitcode == 0 && proc.termsignal == 0`.")
end

@testset "dirPath param validation" begin
    # A destination folder, typed by hand or picked with the FileBrowser. The failure this guards is
    # late and expensive: without it a bad destination is only discovered after the task has read,
    # converted and tried to write the whole output.
    spec = [Dict{String,Any}("key" => "outDir", "label" => "Destination", "type" => "dirPath")]

    # Empty is legal — every consumer falls back to its own default (default_export_dir()).
    Cecelia._validate_params_against_spec(Dict{String,Any}("outDir" => ""), spec)
    Cecelia._validate_params_against_spec(Dict{String,Any}(), spec)

    mktempdir() do dir
        # An existing folder is the normal case.
        Cecelia._validate_params_against_spec(Dict{String,Any}("outDir" => dir), spec)

        # One that does not exist yet is fine too — a destination is created on demand, so rejecting
        # it would stop someone naming a new subfolder, which is the obvious thing to want.
        Cecelia._validate_params_against_spec(
            Dict{String,Any}("outDir" => joinpath(dir, "new_subfolder")), spec)

        # An existing FILE is the one unambiguous mistake: nothing can write output into it.
        f = joinpath(dir, "not_a_dir.txt"); write(f, "x")
        @test_throws Cecelia.ParamValidationError Cecelia._validate_params_against_spec(
            Dict{String,Any}("outDir" => f), spec)
    end

    @test_throws Cecelia.ParamValidationError Cecelia._validate_params_against_spec(
        Dict{String,Any}("outDir" => 42), spec)

    # The export's destination actually uses the type — the point of adding it.
    ospec = JSON3.read(read(Cecelia._spec_path(ExportOmeTiff()), String))
    outdir = only(filter(p -> String(get(p, :key, "")) == "outDir", collect(ospec[:params])))
    @test String(get(outdir, :type, "")) == "dirPath"
end

@testset "units written into OME-XML are schema-valid symbols" begin
    # OME's UnitsLength / UnitsTime are ENUMERATIONS of symbols. A value outside them makes the
    # whole <Pixels> element schema-invalid, and Bio-Formats then discards the ENTIRE OME block and
    # falls back to counting IFDs — a 31x4x32 movie opened as 3968 timepoints, one channel, no
    # names, no voxel size. Verified against real Bio-Formats (bioformats2raw): "µm" round-trips in
    # full, "micrometer" yields nothing.
    #
    # The trap is that "micrometer" is CORRECT in the two places it comes from: NGFF `.zattrs` axes
    # use UDUNITS-2 names, and `ccid.json` mirrors them because the importer reads the unit from the
    # axes. Only the OME-XML boundary needs the symbol — which is what `ome_xml_unit_name` is for.
    valid_length = Set(["Ym","Zm","Em","Pm","Tm","Gm","Mm","km","hm","dam","m","dm","cm","mm",
                        "µm","nm","pm","fm","am","zm","ym","Å","thou","li","in","ft","yd","mi",
                        "ua","ly","pc","pt","pixel","reference frame"])
    valid_time   = Set(["Ys","Zs","Es","Ps","Ts","Gs","Ms","ks","hs","das","s","ds","cs","ms",
                        "µs","ns","ps","fs","as","zs","ys","min","h","d"])
    valid = union(valid_length, valid_time)

    # Every output of the converter is a member — including for inputs already in symbol form.
    for (ngff, sym) in Cecelia._OME_XML_UNIT
        @test sym in valid
        @test Cecelia.ome_xml_unit_name(ngff) == sym
        @test Cecelia.ome_xml_unit_name(sym) in valid    # idempotent: a symbol stays valid
    end
    # The vocabularies the importer actually stores in ccid.json must all convert.
    for ngff in ("micrometer", "nanometer", "millimeter", "second", "minute")
        @test Cecelia.ome_xml_unit_name(ngff) in valid
    end
    # An unknown unit passes through — we do not guess a conversion — so it is the CALLER's job not
    # to invent one, and the scan below is what keeps a caller from skipping the converter entirely.
    @test Cecelia.ome_xml_unit_name("furlong") == "furlong"

    # Anything that ASSIGNS an OME unit attribute must route through the converter. This is the
    # bypass that shipped: the OME-TIFF export copied ccid.json's "micrometer" straight into
    # PhysicalSizeXUnit, while every other writer converted.
    #
    # Compliance is per-DIRECTORY: a file that only READS these keys into an intermediate Dict (the
    # metadata reader) doesn't itself need to call `ome_xml_unit_name`, but the sibling file in the
    # same directory that WRITES OME-XML must — and the split of `importImages/omezarr.jl` into
    # `omezarr/reader.jl` + `omezarr/calibration.jl` separates the two halves, so a file-local check
    # would false-positive on reader.jl. Aggregate at directory level: the family passes iff SOMEONE
    # under the same dir calls the converter.
    dir_has_converter = Dict{String,Bool}()
    for root in (_app_src, _api_src)
        isdir(root) || continue
        for (dir, _, files) in walkdir(root), f in files
            endswith(f, ".jl") || continue
            occursin("ome_xml_unit_name", read(joinpath(dir, f), String)) || continue
            dir_has_converter[dir] = true
        end
    end
    offenders = String[]
    for root in (_app_src, _api_src)
        isdir(root) || continue
        for (dir, _, files) in walkdir(root), f in files
            endswith(f, ".jl") || continue
            path = joinpath(dir, f); src = read(path, String)
            occursin(r"\"(PhysicalSize[XYZ]Unit|TimeIncrementUnit)\"\s*(=>|\]\s*=)", src) || continue
            occursin("ome_xml_unit_name", src) && continue
            get(dir_has_converter, dir, false) && continue
            push!(offenders, basename(path))
        end
    end
    isempty(offenders) && @test true
    isempty(offenders) || error("these assign an OME-XML unit attribute without calling " *
                                "`ome_xml_unit_name`:\n  " * join(offenders, "\n  ") *
                                "\nccid.json/NGFF store UDUNITS names ('micrometer'); OME-XML " *
                                "needs the symbol ('µm'), and an invalid one voids the whole block.")
end
