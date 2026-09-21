# Blackboard fingerprint extractors

Lookup tables used by `_infer_fingerprint` (in `mcp/cecelia_mcp/server.py`) to map raw project
context — a channel name, a filename, a profile-prose blurb — to the coarse **fingerprint** labels
banked with each Blackboard entry (`PROJECT_MEMORY_PLAN` Phase 5.1 / 5.2). The label vocabulary
here is what a retrieval pass matches on, so an extractor is a first-match regex over a small
canonical vocabulary — deliberately coarse, deliberately editable in one place.

Three extractors, in fingerprint order:

1. [Stain class](#stain-class-classifier) — `channelNames[i]` → coarse organelle/marker bucket
2. [Modality](#modality-classifier) — filename / oriPath → acquisition modality
3. [Tissue context](#tissue-context-parser) — profile-prose scan → tissue vocabulary

---

## Stain-class classifier

`_classify_stain(name)` in `mcp/cecelia_mcp/server.py` — used to derive `fingerprint.stain_classes`.
A project using `plasma-TOM` and a project using `mem-TOM` both hit "membrane" without needing a
per-lab dictionary.

**Ordered, first match wins.** Case-insensitive `re.search`.

| Pattern | Class | Notes |
|---|---|---|
| `^mem[-_]` | membrane | Membrane label (mem-TOM, mem-GFP, …) |
| `^nuc[-_]` | nucleus | Nuclear label prefix convention |
| `^cyto[-_]` | cytoplasm | Cytoplasmic label prefix convention |
| `^cd\d+[-_a-z]*` | macrophage_or_marker | CD marker family — CD169, CD8, CD4, … — a coarse "immune marker" bucket, not per-CD |
| `(^\|[-_])dapi` | nucleus | DNA dye |
| `(^\|[-_])hoechst` | nucleus | DNA dye |
| `(^\|[-_])tomato\b` | membrane | tdTomato as a bare fluorophore (mem-TOM already caught by `mem-`) |
| `(^\|[-_])gfp\b` | reporter | Generic GFP reporter (nuc-GFP already caught by `nuc-`) |
| `(^\|[-_])rfp\b` | reporter | Generic RFP reporter |
| `(^\|[-_])yfp\b` | reporter | Generic YFP reporter |
| `(^\|[-_])shg\b` | structural | Second-harmonic generation — collagen / fibre |
| `(^\|[-_])autofl` | autofluorescence | `autofluor`, `autoFL`, … |
| _no match_ | unknown | See [Why "unknown" is a signal](#why-unknown-is-a-signal-not-a-bug) |

### Class semantics

- **membrane** — cell surface / plasma membrane label
- **nucleus** — nuclear DNA or nuclear-marker label
- **cytoplasm** — cytoplasmic label
- **macrophage_or_marker** — CD-family immune marker (coarse; a per-CD split would need many more
  users than we have to be worth doing)
- **reporter** — bare fluorescent protein used as a lineage/transgene reporter (no organelle
  target implied)
- **structural** — SHG or similar extracellular structural signal
- **autofluorescence** — deliberately labeled auto-fluorescence channel (not a probe)
- **unknown** — the name didn't match any pattern

---

## Modality classifier

`_classify_modality(image_info)` in `mcp/cecelia_mcp/server.py` — used to derive
`fingerprint.modality`. Reads the image's `oriPath` (and `name` fallback), first-match wins.

**Ordered, first match wins.** Case-insensitive `re.search` against the filename basename.

| Pattern | Modality | Notes |
|---|---|---|
| `(^\|[-_])res(\|[-_.])` | 2p | Resonant-scan convention — Ailsa uses `-res_` as the intravital 2P marker; extend if another lab reserves the token |
| `2p\|two[-_]?photon\|multiphoton` | 2p | Direct 2P callouts |
| `confocal\|(^\|[-_])conf(\|[-_.])` | confocal | Includes point-scan confocal |
| `spinning[-_]?disk\|(^\|[-_])sdc(\|[-_.])\|(^\|[-_])csu(\|[-_.])` | spinning_disk | CSU-family spinning-disk confocal |
| `light[-_]?sheet\|(^\|[-_])lsfm(\|[-_.])\|(^\|[-_])spim(\|[-_.])` | lightsheet | Light-sheet / SPIM |
| `widefield\|(^\|[-_])wf(\|[-_.])` | widefield | Widefield epifluorescence |
| _no match_ | unknown | Same discipline as stain: signal, not bug |

### Modality semantics

- **2p** — two-photon acquisition (resonant or galvo). We don't split resonant vs galvo — the retrieval
  bucket doesn't need it and the info isn't reliably in the filename.
- **confocal** — classic point-scan confocal
- **spinning_disk** — Yokogawa CSU-family, other spinning-disk confocals
- **lightsheet** — light-sheet fluorescence microscopy (SPIM, DiSPIM, LSFM, …)
- **widefield** — widefield epifluorescence
- **unknown** — the filename doesn't say

Not preserved from OME `Instrument` (extraMeta comes back empty on import) — filename convention is
what we have. If another metadata source arrives (a reader change), this classifier grows a new
input, but the output vocabulary stays the same for backwards-compatible retrieval.

---

## Tissue-context parser

`_infer_tissue_context(profile_content)` in `mcp/cecelia_mcp/server.py` — used to derive
`fingerprint.tissue_context`. Scans the project profile's **Subject** section for a small
canonical vocabulary; the first hit wins.

**Ordered, first match wins.** Case-insensitive substring match against the Subject text.

| Vocabulary | Tissue | Notes |
|---|---|---|
| `germinal cent(re\|er)`, `\bgc\b` | germinal_centre | GC hits both spellings |
| `lymph node`, `\bln\b` | lymph_node |  |
| `spleen`, `splenic` | spleen |  |
| `liver`, `hepatic` | liver |  |
| `kidney`, `renal` | kidney |  |
| `gut`, `intestin`, `small bowel`, `colon` | gut |  |
| `skin`, `dermal`, `epiderm` | skin |  |
| `lung`, `pulmonary`, `alveol` | lung |  |
| `bone marrow`, `\bbm\b` | bone_marrow |  |
| `brain`, `cortex`, `cerebell` | brain |  |
| `thymus`, `thymic` | thymus |  |
| _no match_ | unknown | Or absent from the fingerprint entirely if the profile isn't authored yet |

### Why the parse is over the Subject only

The Blackboard **profile** entry has structured headings (`_BB_PROFILE_PLACEHOLDER_BODY`); Subject
is the "what this data is — tissue + preparation" section. Grepping the whole body would catch
mentions of tissue in an *unrelated* Blackboard entry copied into the profile, which would fire
the wrong retrieval bucket. Subject-only keeps the parse anchored to the project's own
declaration.

If the profile is unfilled (or only contains the seeded placeholder), the parser returns None and
the fingerprint field is absent — a legitimate "not enough context to say yet" signal.

---

## Why "unknown" is a signal, not a bug

The vocabularies here are deliberately coarse and Ailsa-shaped. Extractors default to `unknown`
rather than guess; a fingerprint that carries `["unknown", "unknown", "unknown"]` means "this
project's naming convention isn't in the map yet" — retrieval reads it as no-signal on that
dimension, not as an error. If another lab's convention starts landing repeatedly, extend the
table AND the regex list in the same change; the class/modality/tissue *vocabulary* itself is
what retrieval matches on, so adding a new regex that maps to an existing label is a compatible
extension.

## When to update

- **A new pattern for an existing label** (a new lab's channel prefix, a new filename convention)
   — add the pattern here and to the corresponding tuple in `server.py` in the same change. The
  schema version does NOT bump; the vocabulary is unchanged.
- **A new label** (e.g. `stain_classes` grows `t_cell_marker` split from `macrophage_or_marker`)
   — that IS a schema change: bump `_BB_FINGERPRINT_VERSION` on **both** sides (Julia constant +
  Python constant) so retrieval can dispatch on the version. Old-version entries stay valid but
  won't score against the new-vocabulary bucket.
