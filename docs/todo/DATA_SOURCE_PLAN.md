# Data source — plan

**Status:** planning (2026-11-23). Nothing built.

## Why

Cecelia imports images other people acquired: public atlases, collaborators' data, demo sets. Nothing
records where they came from. For the PhenoCycler set `xxHqjQ` ("HuMAP tissues"), the only trace is
the import path, `…/hubmap-downloads/HBM666.MCZF.973-8165df972cad95329f438dcb9a22d58d/raw/images/
S16-B14-111223_Scan1.qptiff`. That is HuBMAP dataset HBM666.MCZF.973, doi:10.35079/HBM666.MCZF.973.
Neither the user nor an agent looked there when the README needed a citation for it.

A figure, a Methods draft or an exported crate made from someone else's images has to cite them.
Today that citation is typed by hand, if it is remembered at all.

## Decisions

1. **One field, on the set, with a per-image override.** `set.meta["data_source"]` holds the source of
   the whole set. `img.meta["data_source"]` overrides it for one image (a set mixing two datasets). One
   resolver, `data_source(proj, img)`, returns the image's own or else its set's. A `CciaImage` has no
   pointer to its set, so the resolver takes the project and finds the set through a new
   `image_set(proj, img)`; eight call sites in `app/src` do that lookup inline today (`findfirst(s ->
   img.uid in s.image_uids, proj._sets)`), and P1 moves them onto it. It is stored the way
   `meta["experiment_design"]` is (`app/src/experiment_design.jl`), written through `commit_state!`.

2. **Not called `source`, nor `dataSource`.** `source` already means the file format in acquisition
   metadata (`source: "qptiff"`) and the source project in the doc fixtures (`user-docs/shots/fixtures.ts`);
   `dataSource` is the plot definitions' data-source key (`app/src/plotDefinitions/*.json`,
   `app/src/analysis_board_spec.jl`). On disk and in Julia it is `data_source`, in TypeScript
   `imageSource`, and in the UI "Source".

3. **Shape.** `{citation, doi, url, licence, repository, id, by, confirmed}`:
   - `citation`: one line as the user wants it cited ("HuBMAP HBM666.MCZF.973, Stanford TMC").
   - `doi`, `url`, `licence`: each optional.
   - `repository` and `id`: set by import when it recognises one ("hubmap", "HBM666.MCZF.973").
   - `by`: `"import"` or `"user"`. `confirmed`: false for an import suggestion until the user accepts it.
   An empty record means unknown. It is never an error.

4. **Import pre-fills, offline.** Import never goes online for this: it also runs offline and on a
   cluster. It fills only what follows from the ID itself:
   - **HuBMAP**: a path component matching `HBM\d{3}\.[A-Z]{4}\.\d{3}` gives `repository`, `id`, and
     `doi` = `10.35079/<id>`. HuBMAP builds its DOIs from the ID. Checked on HBM666.MCZF.973: the DOI
     resolves to the dataset.
   - **Immunemap**: an image imported through Import from Immunemap already carries
     `meta["immunemap"]` (`app/src/tasks/importImages/immunemapFetch.jl`). The source is filled from it.
   - Other repositories (BioImage Archive, IDR) are added when a dataset from them arrives, with the
     pattern checked on that dataset. An unchecked pattern is not added.
   A pre-filled source shows as a suggestion to confirm, the way an inferred design does. It is never
   applied silently.

5. **Details on request.** For HuBMAP, the title, contributors and institution come from the public
   entity API (`https://entity.api.hubmapconsortium.org/entities/<uuid>`; the UUID is in the download
   folder name). That is a **Fetch details** button beside the pre-filled DOI, not an import step.

6. **Derived images inherit, through the existing canonical.** A derived image comes from the same
   acquisition, and `SOURCE_META_KEYS` + `copy_source_meta!` (`app/src/model/image.jl`) is already the
   one place that carries `ori_path` and `oriFingerprint` to it. Crop, splitTissues, zProject,
   tProject, resampleZ, bin and register call it, and copyImage splats the keys. `data_source` joins
   `SOURCE_META_KEYS`; no per-task helper names it. One difference: a derived image often lands in
   another set (a crop set), so it gets the source's *resolved* `data_source` (the image's own, else
   its set's) written on the image, not only the image's own key. That resolve happens once, in a new
   method `copy_source_meta!(dst, proj, src::CciaImage)` beside the dict one, which copies the other source
   keys and the resolved source. P2 moves the seven callers and copyImage (which today splats
   `SOURCE_META_KEYS` into its own key list) onto it, so no call site resolves by hand.

7. **Warn, never block.** Nothing refuses to run or export without a source. The Methods draft says
   "Data source not recorded" when an image has none and its path or set suggests outside data.

## Readers

| Reader | What it does with the source |
|---|---|
| Methods draft (`app/src/ai/methods_report.jl`, #2440) | A "Data" line per distinct source, with the DOI |
| RO-Crate export (`app/src/ai/ro_crate.jl`, #2443) | The dataset cited as what the run is based on |
| Figure and board provenance (`frontend/src/utils/boardProvenance.ts`) | The source in the exported provenance |
| MCP `get_image_info` | The resolved source, so an agent cites it rather than guessing |
| Doc shots (`user-docs/shots/`) | A measured fact for captions, `{{<project>.<image>.citation}}` |

## Build sequence

- **P1. Record.** Field, `image_set` (and its eight inline call sites moved onto it), resolver, set and image API, the Metadata page editor (set level, image
  override), MCP read. Tests: resolver precedence, empty record, round trip.
- **P2. Import pre-fill.** HuBMAP path match and the Immunemap mapping, both as unconfirmed
  suggestions. Derived images inherit. Tests on the real HBM666.MCZF.973 path and an Immunemap image.
- **P3. Readers.** Methods "Data" line, RO-Crate citation, figure and board provenance, doc-shot fact.
- **P4. Fetch details** (optional). The HuBMAP lookup behind a button.

About seven touchpoints in P1–P3: the record and resolver, the API, the Metadata UI, the import
pre-fill, and the three readers. The README hero shot of a HuBMAP section (after #2446) waits for P1
and the doc-shot fact in P3, so its caption reads the citation from the record instead of typing it.

## Open

- Licence: HuBMAP public data has its own terms; whether to pre-fill a licence per repository or leave
  it to the user.
- Whether a set made of several datasets should list them on the set, or only through image overrides.
