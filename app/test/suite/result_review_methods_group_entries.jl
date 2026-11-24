# RESULT-REVIEW: result-957cf013
# A repeatable group (Cellpose's models): the template speaks for the first entry. A second model
# whose diameter differs between images must not lend that spread to the first model's sentence,
# and the second model is named, not dropped. (`_mr_two_image_log`: result_review_methods_partial_input.jl.)

@testset "Methods report — a second model's values stay with the second model" begin
    proj, s, imgs = _mr_two_image_log() do logs
        for (log, d) in zip(logs, (5, 8))
            seg = last(log)
            m1 = deepcopy(seg["params"]["models"]["0"])
            m1["model"] = "nuclei"; m1["cellChannels"] = ["OTI-CTV"]; m1["cellDiameter"] = d
            seg["params"]["models"]["1"] = m1
        end
    end
    md = methods_report(proj; set_uid = s.uid)["markdown"]
    @test occursin("diameter 10 µm;", md)
    @test !occursin("5 µm (1 image)", md) && !occursin("8 µm (1 image)", md)
    @test occursin("The run had 2 entries under \"Segmentation models\"", md)
    rm(proj.root; recursive = true)

    # one image ran a second model and the other did not: said for the set, in both orders
    for second_on in (1, 2)
        proj, s, imgs = _mr_two_image_log() do logs
            seg = last(logs[second_on])
            m1 = deepcopy(seg["params"]["models"]["0"])
            m1["model"] = "nuclei"; m1["cellChannels"] = ["OTI-CTV"]; m1["cellDiameter"] = 8
            seg["params"]["models"]["1"] = m1
        end
        md = methods_report(proj; set_uid = s.uid)["markdown"]
        @test occursin("diameter 10 µm;", md)
        @test occursin("The images differ in their entries under \"Segmentation models\"", md)
        @test !occursin("The run had", md)
        rm(proj.root; recursive = true)
    end

    # both images ran the same two models, one with no nucleus channels: an empty list is a value,
    # not a missing entry
    proj, s, imgs = _mr_two_image_log() do logs
        for (log, nuc) in zip(logs, (String[], ["OTI-CTV"]))
            seg = last(log)
            m1 = deepcopy(seg["params"]["models"]["0"])
            m1["model"] = "nuclei"; m1["nucChannels"] = nuc
            seg["params"]["models"]["1"] = m1
        end
    end
    md = methods_report(proj; set_uid = s.uid)["markdown"]
    @test occursin("The run had 2 entries under \"Segmentation models\"", md)
    rm(proj.root; recursive = true)
end
