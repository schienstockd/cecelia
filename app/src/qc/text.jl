# ── QC copy catalog ───────────────────────────────────────────────────────────────────────────────
#
# Every user-facing QC string, in one table. This is the worst-placed copy in the app — prose buried
# inside analysis functions, where nobody reviews it and `pixi run ui-copy` was the first thing that
# could even see it. Keeping it in one dedicated file means the wording can be read and revised as a
# set, and the analysis code stays logic rather than logic-plus-writing. Rendering (`qc_text`) and
# the read-time helpers live in `../qc.jl`; this file is the catalog only.
#
# Text rules (`docs/ui/COPY.md`, `docs/MODULES.md` → *QC*): `short` = the problem, terse and
# with no trailing period; `long` = what to DO about it, one imperative sentence. Numbers belong in
# the finding's `detail`, not in the prose — a `{}` placeholder is only for the cases where the
# number IS the message (a percentage, a channel index). The set of allowed placeholder names is
# ASSERTED in runtests, so adding one is a deliberate act, not a typo.
#
# KEYED SEPARATELY FROM `code`. Usually the key is the code, but they are deliberately not the same
# field: `metadata.pixel_size_no_unit` is emitted for the x, y AND z axes with different wording for
# z, and `output.canvas_expansion` is emitted by the drift task under `drift.canvas_expansion`. Codes
# are a stored contract — they sit in every banked `qc/*.json` on disk and the frontend filters on
# them (`isMetadataCode`) — so the catalog bends around them rather than the other way round.
const QC_TEXT = Dict{String,@NamedTuple{short::String, long::String}}(
    # calibration (metadata_qc_findings)
    "metadata.z_spacing_unknown" => (
        short = "Z spacing unknown",
        long  = "No Z step found — set the voxel depth (acquisition software, or Fiji ▸ Image ▸ Properties)."),
    "metadata.z_spacing_corrected" => (
        short = "Z spacing auto-corrected",
        long  = "Auto-corrected from the source ImageJ tag — confirm it in Fiji ▸ Image ▸ Properties before trusting it."),
    "metadata.z_spacing_unusual" => (
        short = "Z spacing looks unusual",
        long  = "Z step is far from the XY pixel size — likely a wrong calibration unit; check the original in Fiji and correct it."),
    "metadata.frame_interval_unknown" => (
        short = "Frame interval unknown",
        long  = "No frame interval found — enter it from your acquisition settings."),
    "metadata.frame_interval_no_unit" => (
        short = "Frame interval has no unit",
        long  = "A frame interval is recorded without a unit — re-enter it with seconds/minutes."),
    "metadata.pixel_size_unknown" => (
        short = "Pixel size unknown",
        long  = "No XY pixel size found — nothing that measures in microns can run until it is set (enter it from your acquisition settings)."),
    "metadata.pixel_size_no_unit" => (
        short = "Pixel size has no unit",
        long  = "A pixel size is recorded without a unit — re-enter it with a unit."),
    "metadata.voxel_depth_no_unit" => (      # emitted under code `metadata.pixel_size_no_unit`
        short = "Voxel depth has no unit",
        long  = "A Z step is recorded without a unit — re-enter it with a unit."),

    # pyramid depth (pyramid_qc_findings). The user picks `pyramidLevels` in the import form BEFORE
    # anything reads the source, so a big image imported at the default (2 levels) has no zoomed-out
    # view — every viewport pull at low zoom stays a per-tile fetch. This finding measures the store
    # after the fact and says how many more levels would collapse it to one tile.
    "import.pyramid_too_shallow" => (
        short = "Pyramid too shallow — deepest level is still {tiles} tiles",
        long  = "Re-import with {suggest} pyramid levels — zoomed-out views can't fetch the whole image in one request until the deepest level fits one tile."),

    # clipping at acquisition (import.channel_saturated)
    "import.channel_saturated" => (
        short = "Channel {channel} clipped at the detector",
        long  = "Lower the gain or exposure when acquiring — clipped values cannot be recovered."),

    # photon-limited channels (import.photon_limited) — ONE finding per image, not per channel:
    # photon-limitation is a scanning-mode property (laser/PMT settings shared across channels), so
    # a per-channel finding would be N copies of the same acquisition observation.
    "import.photon_limited" => (
        short = "{n} photon-limited channel{s} (up to {pct}% zero voxels)",
        long  = "Run Cleanup → Denoise before segmentation. Channels: {channels}."),


    # HMM (hmm_states_qc_findings / hmm_transitions_qc_findings)
    "hmm.no_states_decoded" => (
        short = "No cells decoded into a state",
        long  = "Tracks may be too short or measurements incomplete — check segmentation/tracking and re-run."),
    "hmm.single_state" => (
        short = "All cells sat in one state",
        long  = "This image didn't switch states — check it's the same acquisition and measurements, or reduce the state count."),
    "hmm.dominant_state" => (
        short = "One state holds {pct}% of cells",
        long  = "Check the behaviour is really this uniform, or the model may have too many states."),
    "hmm.no_transitions" => (
        short = "No state transitions found",
        long  = "Tracks may be too short or the model produced one state — check HMM states and track lengths."),

    # tracking (track_measures_qc_findings)
    "tracking.motion_dims_uncertain" => (
        short = "Motion dimensionality uncertain ({dims}D)",
        long  = "z couldn't be classified as migration vs jitter — review whether tracking should be 2D or 3D and re-run with dims set."),

    # celltrackR diagnostics battery (track_diagnostic_findings) — advisory, and every one of them
    # can be correct biology, which is why none is an error and each long names the check to make.
    "tracking.field_drift" => (
        short = "Whole field is drifting",
        long  = "Cells share a net direction (Hotelling p = {value}) — if that is stage drift rather than chemotaxis, run drift correction in Cleanup images and re-track."),
    "tracking.msd_confined" => (
        short = "Tracks barely displace",
        long  = "The log-log MSD slope is {value} where a random walk is 1.0 — check a few tracks in the viewer to rule out tracking something that is not moving."),
    "tracking.plane_artefact" => (
        short = "Steps flatten near the volume edge",
        long  = "Steps nearest the lower z boundary average {value}° to it against the 32.7° of unbiased 3D motion — crop the z range or exclude those tracks."),
    "tracking.duplicate_tracks" => (
        short = "{count} track pair(s) look like one cell",
        long  = "Two tracks moved in near-parallel within a few µm of each other for several frames — review them in the correction worklist."),

    # manual track correction (track_correction_qc_findings)
    "correction.large_share_edited" => (
        short = "{pct}% of cells hand-corrected",
        long  = "This much correction points at the tracking parameters — revisit those and re-track instead."),
    "correction.short_tracks" => (
        short = "{count} track(s) below {min} timepoints",
        long  = "Splitting left tracks shorter than tracking's own minimum — join them or remove them."),

    # manual label correction (label_correction_qc_findings)
    "correction.labels_large_share_edited" => (
        short = "{pct}% of labels hand-corrected",
        long  = "This much correction points at the segmentation parameters — revisit those and re-segment instead."),

    # downstream staleness after a correction (correction_staleness.jl / P3 Decision 5). ONE finding
    # per report; the affected artefact list rides in `detail.artefacts` so the cockpit can render it
    # inline. The `long` names the classes rather than each file — a per-artefact wording would balloon
    # to N lines for a big cohort and the useful information is "which tasks to re-run", not a path list.
    "correction.stale_artefacts" => (
        short = "{n} downstream artefact(s) now predate this correction",
        long  = "A {scope} correction changed the row set — re-run the affected tasks (tracking, clustering, gating rebuild, spatial graph) to refresh; the report file lists each artefact."),

    # clustering (cluster_qc_findings)
    "clustering.single_cluster" => (
        short = "Only one cluster found",
        long  = "Resolution too low or features don't separate populations — raise resolution or add features and re-run."),
    "clustering.image_one_cluster" => (
        short = "All {unit} fell into one cluster",
        long  = "This image separated from the cohort — check it's the same acquisition and normalisation, then re-run."),
    "clustering.dominant_cluster" => (
        short = "One cluster holds {pct}% of {unit}",
        long  = "Check the population is really this uniform, or raise resolution to split it."),

    # skeleton branching (segment/branching.jl). These predated the catalog and were inlined at the
    # call site with the four-argument form; moved here so the wording is reviewable with the rest.
    "branching.no_branches" => (
        short = "No branches found",
        long  = "Lower the pre-dilation or check the segmentation, then re-run."),
    "branching.aniso_grid_large" => (
        short = "Orientation grid is large",
        long  = "Raise the grid spacing — the stored field costs 1/box², so doubling it saves about 4x."),
    "branching.uncalibrated" => (
        short = "Image has no pixel size",
        long  = "Set the pixel size, or read the µm scale settings as pixels — 1 µm/px was assumed."),

    # AF correction (af_qc_findings). The only finding this task has: the correction itself has no free
    # parameter left to land behaviourally wrong, so the one objective signal is about the INPUT.
    "af.saturated_input" => (
        short = "Channel {channel} saturated",
        long  = "Input voxels are clipped at the top of the range, so their true value is already lost — lower the gain or laser power and reacquire."),
    "af.bleedthrough" => (
        short = "Channel {channel} carries {value} bleedthrough",
        long  = "The correction subtracted it, but a leak is a filter-set property — check the emission filters if it differs across a set acquired the same way."),

    # smoothing (_smooth_qc_findings). Both findings are about the STEP rather than
    # the input: it always "succeeds", it just may have overshot the dtype or bought nothing.
    "smooth.gain_clipped" => (
        short = "Dynamic-range gain clipped {value} voxels",
        long  = "Turn 'Restore dynamic range' off and re-run — the bright end of every smoothed channel is flat."),
    "smooth.no_effect" => (
        short = "Smoothing changed little",
        long  = "This input was not photon-limited — the extra store is likely redundant."),

    # denoise (SUPPORT) — the input is what matters, since a shot-noise denoiser can't help what it
    # can't see. When ALL selected channels are saturated the task errors before writing anything;
    # when only some are, the run continues on the rest and this finding records the drop.
    "denoise.channel_saturated" => (
        short = "{value} channel(s) skipped — saturated at import",
        long  = "SUPPORT is a shot-noise denoiser; on channels whose noise floor is tiny relative to signal there is nothing to remove. Uncheck them, or re-acquire at lower gain."),

    # denoise training (_support_train_qc_findings). One check — same as opticalFlow.train's
    # "loss did not decrease" — because until inference runs the loss is the only signal.
    "denoise.loss_flat" => (
        short = "Loss did not decrease",
        long  = "Check the channel is photon-limited (not saturated) and has real signal, then retrain."),

    # denoise collapse (_denoise_qc_findings, pooled runs only). Within-run cohort comparison: the
    # weakest channel's post-network dynamic range is <½ the median of the others. Signature seen
    # on x4E5HU CD169-Kat 2026-09-07 — a low-SNR channel treated as noise by the pooled prior.
    "denoise.channel_collapsed" => (
        short = "Channel {channel} range collapsed ({value}× vs cohort)",
        long  = "Retrain with Training mode = Per channel so this channel gets its own model."),

    # OME-TIFF export (_export_qc_findings). The write always "succeeds", so the only objective
    # signal is whether the CALIBRATION came out with it — which is the entire point of the task.
    "export.no_z_calibration" => (
        short = "Export has no Z pixel size",
        long  = "Set the Z spacing in the image metadata and export again — Imaris will otherwise ask for it or guess."),
    "export.no_xy_calibration" => (
        short = "Export has no XY pixel size",
        long  = "Set the pixel size in the image metadata and export again — the scale bar will be wrong without it."),

    # output geometry (qc_canvas_expansion)
    "output.canvas_expansion" => (
        short = "Output canvas grew +{pct}% in XY",
        long  = "Larger than a clean correction — check the output and re-run this step if it looks wrong."),

    # drift correction (_drift_qc_findings). `drift.unreliable` is the primary one: it is measured
    # from the registration disagreeing with ITSELF, so unlike the other two it does not depend on
    # what the drift happens to look like. See correction_utils.drift_residuals.
    "drift.unreliable" => (
        short = "Frames did not register ({value} px disagreement)",
        long  = "Pick a structural reference channel and re-run — nothing downstream of this store is aligned."),
    "drift.jump" => (
        short = "Drift jumped sharply at T={value}",
        long  = "The reference channel likely lost tracking — re-run drift with a clearer/structural channel."),
    "drift.unregistered_frames" => (
        short = "{value} frame(s) could not be registered",
        long  = "Their position was predicted from neighbours — check those timepoints before measuring on them."),
    "drift.rotation.capped" => (
        short = "{value} frame(s) rotated past the cap",
        long  = "The rigid estimator hit the per-frame rotation cap and predicted those frames' rotation from neighbours — check whether the stage really rotated that much, or a moving object dominates the reference channel."),

    # flow-based per-pixel registration (cleanupImages.flowRegister). Fires when the dense-flow
    # aligner is chronically saturating its per-pixel clamp — either the sample is deforming more
    # than dense flow can capture in the current configuration (raise the clamp or the pyramid
    # levels), or the reference channel isn't the structured one (pick a brighter one).
    "flow_register.high_shifts" => (
        short = "Flow saturated the clamp on {value} of frames",
        long  = "Many frames' flow field hit the maxShiftPx clamp — either raise the clamp or the pyramid levels, or pick a structured reference channel."),

    # cohort comparison (qc_cohort.jl `_cohort_finding`)
    "cohort.outlier" => (
        short = "{metric} is a cohort outlier",
        long  = "This image's {metric} ({value}) is far {dir} the set median ({median}) — check this image before trusting the run."),
)
