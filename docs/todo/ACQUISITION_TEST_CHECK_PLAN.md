# Test-acquisition check — plan

**Status:** **PARKED** (2026-11-09). Nothing built. Written from one set used as a stand-in (below).
Part of [`ACQUISITION_ADVISOR_PLAN.md`](ACQUISITION_ADVISOR_PLAN.md) — that plan's Decision 1/2 ("test
acquisition imported into the user's project") given a concrete shape. Reuses the mechanism of
[`IMAGE_ANALYSIS_RECITAL_PLAN.md`](IMAGE_ANALYSIS_RECITAL_PLAN.md).

## Goal

Before the user processes anything, and before they acquire the rest of the experiment:

> "Here is my image. I'm planning to track the uptake of beads by B cells. Is that OK, or should I
> change anything before acquiring the other images?"

The answer is a short list. Each item names a problem in this image that gets in the way of that
plan, the change to make at the microscope, and what the change costs. Changes for the next
acquisitions come first. What it means for this image comes second.

## Route — the recital mechanism, with a plan instead of a claim (the maintainer, 2026-11-09)

The plan sentence plays the role a claim plays in recital: it scopes what matters.

| Recital | Test-acquisition check |
|---|---|
| Claim: "T faster than B" | Plan: "track bead uptake by B cells" |
| Translator (parked there) → populations, measure, replicate | **Needed here**: plan → objects (B cells, beads), the channel for each, the event (uptake), context channels. The user confirms |
| Checks: record gaps, cohort consistency, replicate floors | Checks: the six below, read per channel against the plan |
| Review: one Kiwi turn, advisory, Good/Bad/Unsure | Review: one Kiwi turn reading `get_acquisition_knowledge`; "consistent with" until the cause is separated |
| A Blackboard entry, re-checked each session | Same: the plan is an entry, re-checked when the next test image lands |
| Popover: facts grouped by category, "what, where → what to do" | Same layout |

**Before analysis vs. after (the maintainer, 2026-11-09).** This plan covers *before*: a test image and a
plan, and advice for the next acquisitions. Asking which acquisition problems affect a claimed result is
*after*, and belongs to claim recital: a recital check that reads the import findings on the channels the
claim uses. Don't build a per-set "affects your analysis" report here.

**Not the import QC badge.** The badge reports everything on every image. Every m5uLPh image is flagged,
and the most common warning is on a channel the plan doesn't use.

## The checks (the maintainer, 2026-11-09)

| Check | What it measures | Exists today | When |
|---|---|---|---|
| **Noise** | Signal over noise *on the objects* | `noise.snr` (P4b-1), but it disagrees with the maintainer's eye (below) | import |
| **Dynamic range on the objects** | How much of the detector range the cells and beads use, against what sets the ceiling | not built; `range` and `hist` are over all non-zero voxels | import |
| **Bleed-through** | Signal from one channel in another | AF correction's `maxBleedthrough` (pilot); P4b-2 at import waits | pilot (import later) |
| **Signal fading** | Loss over the movie per channel | `import.signal_fades` (`byT`) | import |
| **Drift** | Sample movement between frames | `drift.unreliable`, `drift.unregistered_frames` | pilot |
| **Uneven z intensity** | Signal change with depth | `import.brighter_with_depth` / `darker_with_depth` / `clipped_deep` (`byZ`) | import |

Read every check per channel against the plan. Tracked and measured channels count. A context channel
(collagen here) gets one line: "not used by your plan".

**Pilot** is the project's own steps run on a crop (advisor P6, ~15 min for 256² × 41 t on zolIMa). Open
question: does the check run it itself (an app action; MCP can't start work), or ask the user to?

## Stand-in case — Immunemap XbeT2l, set m5uLPh (Obino 2018, Exp 63)

These are already-acquired public movies, treated as if one were the test image. Channels:
`Microspheres (Rhodamine)`, `B cells (GFP)`, `Collagen (Autofluorescent)`.

**The maintainer's diagnosis (2026-11-09), written before any check exists:** the main problems are
**noise**, **low dynamic range on the actual cells and beads**, and **bleed-through between channels**.
This is the expected answer the first slice is scored against (advisor P7).

### Settings (re-imported 2026-11-09 with the `.ims` reader, #2064: J1ha0v, JW7Qr0, JIThPI, niHekF)

- Zeiss LSM 510/710, two-photon, **one 890 nm laser for all three channels**. Emission filters are
  BP 612/69 (beads), BP 520/35 (GFP) and BP 472/30 (collagen). So the channels are separated only by
  emission filters, and excitation can't be done sequentially with one line.
- Not recorded: laser power, detector HV, gain, offset. Advice can give a direction, not a value.
- 9 planes at 4 µm, 30 s frames, 12-bit.
- **J1ha0v used a different objective** (Plan-Neofluar 10x/0.3, 1.8 µm pixels) from the other three
  (Achroplan 40x/0.8 W, 0.45 µm). For a test image this becomes a consistency check: acquire the rest
  with the same objective and zoom.

### Numbers from the import probe (`meta.saturation.channels`, the 3 Achroplan images)

| Channel | Background median / spread | p50 non-zero | Ceiling 4095 hit | Voxels in the bottom 6 % of range (0–255) | `snr` |
|---|---|---|---|---|---|
| Beads | 56 / 9–11 | 60–62 | yes, 0.1–2.7 % of signal | 74–79 % | 39–42 |
| B cells (GFP) | 117–118 / 11–12 | 133–159 | yes, 0.9–1.3 % | 57–62 % | 45–51 |
| Collagen | 142–143 / 11 | 147 | yes, 1.4–1.9 % | 72–75 % | 61–63 |

What this shows (reasoning):
- **Low dynamic range is visible in the numbers.** In every channel the median sits within ~45 grey
  levels of the background, while 1–3 % of voxels clip at 4095. A few bright objects set the ceiling,
  and the bulk of the signal uses a few percent of the range. That matches the diagnosis.
- **The noise number disagrees with the eye.** An `snr` of 39–75 would read as clean. Two likely
  reasons, neither checked yet: the estimator takes the median of voxels above background, which bright
  objects pull up; and it measures the spread of background voxels capped at the background level, which
  underestimates the noise. The noise the eye sees is on the dim cells, at signal level. First step on
  resume: measure noise on object voxels (or estimate gain and noise from the single image, Heintzmann 2016, per
  `ACQUISITION_ADVISOR_PRIOR_ART.md`), and keep the number only if it matches his call on these movies.
- **Bleed-through can't be read at import yet.** RSQxFI's AF correction (an earlier pilot-type run)
  found 41 % collagen and 10 % GFP signal in the bead channel.

### Next steps the check should be able to give (to confirm with the maintainer)

| Finding | Next acquisition | Cost / caveat |
|---|---|---|
| Low range on cells; a few bright objects clip | If the clipping objects are beads, raise power or HV for the cells and let the beads clip. A clipped bead still has a centre, so tracking and object counts survive | Uptake measured by bead intensity is lost; count bead objects |
| Noise on dim cells | Frame or line averaging, or more power | Longer frames (30 s now) or more bleaching; check fading after |
| Bleed-through, one excitation line | Narrower emission filter on the receiving channel, or a second excitation wavelength for one dye | Hardware-dependent; otherwise correct afterwards (AF correction) |
| Fading over the movie | If it's bleaching: less power on that dye, fewer planes or a longer interval | Bead counts per frame first: beads leaving or being taken up look the same in intensity |
| Drift | Stabilise the preparation; a larger z-range as margin | Correctable afterwards up to a point; unregistered frames break tracks |
| Uneven z intensity | Depth power ramp, if the scope has one | Beads collecting at a depth looks the same; "consistent with" only |
| Different objective in the set | Use the test image's objective and zoom for the rest | — |

## Open decisions

1. **Turning the plan sentence into structure.** Either a model parses it and the user confirms
   (recital's parked Translator), or a small GUI form (objects, channel per object, event). Not a config
   file.
2. **Pilot: run it or ask for it**, and on what crop.
3. **Rules vs. review.** "Let beads clip if you only track and count them" is a rule once the plan says
   beads are counted objects. "Brighter with depth is bead placement" is a judgement for the review.
4. **Where it lives.** A step in the import flow ("what will you do with this?"), or an action on an
   imported image. The metadata dialog's light path stays the echo-back.
5. **Finding-text fixes, separate from this plan:** `import.channel_saturated` gives gain advice on
   rescaled 8-bit exports (the Immunemap lymphatic set); `brighter_with_depth` / `clipped_deep` name
   Bright Z when no settings were read.

## Resume when

1. A noise measure that agrees with the maintainer's call on these movies (above).
2. A dynamic-range-on-objects measure (above-background voxels, clipped ones excluded, against the
   ceiling).
3. Then the smallest slice: plan entry + import-only checks + one review turn, scored on one Achroplan
   movie against the diagnosis above. Bleed-through and drift join when the pilot question is settled.
