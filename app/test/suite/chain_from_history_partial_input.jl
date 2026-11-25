# Chain from history on images that read different inputs (`_mr_two_image_log`): one image segmented
# on its drift-corrected version, the other (run later) on raw. Segmentation must wait for drift
# correction, whatever the newest run read — the `valueName` conflict lets the user pick driftCorrected.

@testset "Chain from history — a step waits for what any image read" begin
    proj, s, imgs = _mr_two_image_log(_ -> nothing)
    tpl = chain_from_history(proj, [i.uid for i in imgs])["template"]
    id(fn) = only(n["id"] for n in tpl["nodes"] if n["fn"] == fn)
    @test any(e -> e["from"] == id("cleanupImages.driftCorrect") && e["to"] == id("segment.cellpose"), tpl["edges"])
    rm(proj.root; recursive = true)
end
