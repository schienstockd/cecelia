# Stain-class inventory

The lookup table used by `_classify_stain` (in `mcp/cecelia_mcp/server.py`) to map a raw channel
name to a coarse **stain-class** label. The class labels are what the Blackboard **entry
fingerprint** (`PROJECT_MEMORY_PLAN` Phase 5.1) records — not the raw channel name — so a project
using `plasma-TOM` and a project using `mem-TOM` both look the same to a retrieval pass without
needing a per-lab dictionary.

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
| _no match_ | unknown | See below — a signal, not a bug |

## Class semantics

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

## Why "unknown" is a signal, not a bug

The class list is deliberately coarse and the pattern list is deliberately Ailsa's convention
(`mem-`, `nuc-`, `CD169-…`). When a fingerprint carries `stain_classes: ["unknown", "unknown",
"unknown"]`, that means "this project's channel-naming convention isn't in the map yet" — which is
a useful thing for a retrieval pass to see, not something to hide. If another lab's convention
starts landing repeatedly, extend the table (in this doc AND in `_STAIN_CLASS_PATTERNS`) rather
than trying to auto-detect.

## When to update

- **A new channel-name convention starts showing up** (a new lab, a new project family) — add the
  pattern here and to `_STAIN_CLASS_PATTERNS` in the same change; update the fingerprint schema
  version in `_BB_FINGERPRINT_VERSION` **only** if the *class list itself* changes (adding a
  regex that maps to an existing class is a compatible extension).
- **A class gets split** (e.g. `macrophage_or_marker` breaks into `macrophage` vs `t_cell_marker`)
   — that IS a schema change; bump `_BB_FINGERPRINT_VERSION` on both sides (Julia constant +
  Python constant) so retrieval can dispatch.
