# Phase 2 Observer: Parameter Suggestions + Notebook Generation + Chat Entry

Opus planning pass. Read the codebase before producing a plan. Read `docs/ai-assist/OBSERVER.md`, `docs/todo/OBSERVER_DATA_ACCESS_PLAN.md` (Slices A-E all done), `docs/ai-assist/LAB-LOG.md`, and the MCP server at `mcp/cecelia_mcp/` before designing anything. This builds on a working foundation — don't redesign what's already there.

Context: the observer is already working well in practice. It detects outlier images after clustering, sees images that are off after cell tracking, and can detect behaviour differences from the pulled MCP data. The next step is to make those detections *actionable* and to give Claude a way to create visualizations users can verify.

**Core principle repeated:** Cecelia works without Claude. Everything in this document that requires Claude is clearly marked as Claude-optional. Cecelia's own QC and reporting functions independently.

---

## What gets built (in order)

### 1. Parameter suggestions (read-only, no mutations)

When the observer detects a QC outlier or an image that's consistently off — too few tracks, poor segmentation confidence, gate that's returning <5% of expected cells — Claude should be able to suggest specific parameter adjustments, not just flag the problem.

This is still read-only. Claude reads the QC data, the population data, the measure summaries, and the parameter history from the run log. It suggests. The user decides. Nothing runs automatically.

**What Claude needs access to (MCP tools, check what already exists first):**
- Parameter history per image/function: what params were used in prior runs → read from run log + `funParams` per task
- Current module param specs: the JSON input definitions (already served? check) → Claude needs to know the valid range for each param it suggests
- QC outcomes per param set: what happened when params X were used → correlate run log with qc.jl store

**What Claude produces:**
- "Image KDIeEm has 1043 tracks (cohort mean 187). The tracking search_radius was 8.0 — try reducing to 4.0-5.0, which in prior runs on similar high-density data produced cohort-consistent counts."
- "Gate CD4+ is at 2.3% of parent (below 5% threshold). Channel 2 intensity distribution for this image shows the population shifted right vs. the cohort median — try lowering the gate upper boundary from 0.85 to 0.72."

These suggestions go into the lab log as `[Claude]` entries with 🟡 severity and a clear "suggestion, not instruction" framing.

**Opus: design the minimal MCP additions needed.** The run log, qc store, and measure summaries are already accessible. What's missing? Is there a clean way to expose the JSON param specs already, or does a new route need adding? Check the allow-list in `mcp/cecelia_mcp/client.py` before proposing new routes.

---

### 2. Chat to Claude button — enhanced entry

Currently the "Chat to Claude" button launches an external Claude Code session. Enhance it so Claude starts the session already oriented:

On launch, the button passes a startup context payload that Claude reads first:
1. Recent lab log entries (last 7 days, or since last session)
2. List of available MCP tools (what Claude can access)
3. Current project name and image count
4. Any active QC flags (images currently at 🟡/🔴)

Then Claude's opening message is not a blank slate — it's:

```
I can see you're working on [project name] — [N] images, [M] flagged.

I have access to: analysis lineage, populations and gating, 
measure summaries, behaviour/cluster summaries, QC metrics, 
task history, and the lab log.

What do you want me to look at?
```

This makes Claude useful immediately rather than requiring the user to re-explain context each session.

**Implementation:** the button builds a temp context file (JSON) or passes it as a CLI arg to the Claude Code launch command. The MCP server reads it on session start. Check how `_build_claude_cmd` currently works in the observer and extend that pattern — do not build a parallel launch mechanism.

---

### 3. Pluto notebook generation

This is the write testing ground — the safest possible write feature because notebooks are exploration files, not data mutations. Notebooks can write figures (PNG/SVG/PDF) and CSV files. They cannot write to H5AD, qc.jl, lab log, ccid.json, or any Cecelia data store.

**The Pluto notebook path is the clean answer to real project-specific plot requests from collaborators and supervisors.** This is not AI for AI's sake — it directly solves the gap between a domain scientist's question and the code needed to answer it, without requiring the researcher to become a Julia developer. Here is a concrete example of exactly the kind of request Claude should be able to handle:

> "Plus, I want to try and demonstrate the stability of the prep by showing the instantaneous speed of cells over time. Can you give me data that shows the speed of cells at each timepoint. I'd like to do this per cell if possible. Maybe also try it averaged at each timepoint? If this data is pooled from all 7 movies, even though some movies are shorter than others, I hope it shows that cell speed is unchanged by a long period of imaging. Are there any other parameters that this could work for? Ie turning angle?"

Claude's response to this should be:
- Understand: per-cell instantaneous speed at each timepoint, plus mean ± SEM per timepoint, pooled across 7 images of variable length
- Pull from MCP: which images are in the project, which have track measures, what timepoints exist
- Suggest extension: yes, turning angle works the same way; also displacement, confinement ratio
- Generate a Pluto notebook that:
  - Loads track measures for all 7 images via `track_measures(img; value_name="default")`
  - Computes per-cell speed at each timepoint (from step-level speeds if available, or from the track measures)
  - Computes mean ± SEM across cells at each timepoint, handling variable movie lengths correctly (no padding, report N at each timepoint)
  - Produces two plots: per-cell speed traces (light lines) + mean ± SEM overlay, and mean ± SEM only
  - Saves both plots as PNG/SVG
  - Saves a CSV with columns: `image_uid, cell_id, timepoint, speed, turning_angle` — ready for Prism
  - Saves a second CSV: `timepoint, n_cells, mean_speed, sem_speed, mean_turning_angle, sem_turning_angle` for the summary plot

This is a direct replacement for "I'll export the data from Cecelia as a CSV and then write an R script." Claude handles the data access, the computation, the plot generation, and the Prism-ready export in one notebook, from one natural language description.

The notebook must use only `using Cecelia` — no manual path construction, no raw HDF5 access. This tests the REPL access documentation (Step 1 in the build order) because the notebook is exactly the kind of consumer that documentation is written for.



**Notebook versioning — reuse the existing project versioning system:**
The project already has a versioning system accessible through the GUI. Do NOT build a separate notebook versioning mechanism. Before implementing anything, check how the existing project versioning works (find it in the codebase — it manages project state versions and is accessible via the GUI). Notebooks should be versioned through that same system. If Claude creates a new version of a notebook, it does so by creating a new project version, not by appending `_v2` to a filename. If the existing versioning system does not cleanly accommodate notebooks, flag this and propose how to extend it — do not build a parallel system.

**What Claude needs to know about Cecelia.jl REPL access:**
Claude must know the canonical patterns for accessing Cecelia data from Julia. These should be documented in `docs/REPL.md` (or similar) and exposed to Claude as a resource the MCP server can return. The patterns are:

```julia
using Cecelia

# Load project
project = load_project("/path/to/cecelia-projects/my-project")

# Get images
img = get_image(project; uid="KDIeEm")
imgs = get_images(project; set_uid="PABXLt")

# Population data table (the pop_dt equivalent)
df = pop_dt(img, "live", ["tcells/tracked"]; uIDs=["KDIeEm", "PABXLt"])
df = pop_dt(img, "flow", ["cd4+"])
df = pop_dt(img, "clust", ["Immotile", "Directed"])

# Track measures
measures = track_measures(img; value_name="default")

# Label props (for raw cell data)
df = label_props(img; value_name="default") |> select_cols(["area", "mean_intensity_0"]) |> as_df
```

And the write rules for notebooks:
```julia
# ✅ Allowed in notebooks
save("/path/figures/my_plot.png", fig)
CSV.write("/path/output.csv", df)

# ❌ Never in notebooks
# No label_props |> add_obs |> save!
# No write_qc!
# No ccid.json changes
# (lab log writes happen via Chat to Claude, not from notebook code)
```

Opus: propose where this REPL access documentation lives and how Claude accesses it from the MCP server (as a static resource? a dedicated tool?). It should be readable by Claude and usable as a reference when generating notebooks.

**Claude helping debug notebooks:**
User can paste a notebook cell or ask "what am I doing wrong?" in the Chat session. Claude can read the error, understand the Cecelia.jl access patterns, and suggest corrections. Or if the user wants, Claude drops a corrected version as a new notebook version. Same versioning rule — never overwrite.

---

### 4. Available plot types exposed to Claude

Claude should know what plot types exist in the analysis canvas so it can suggest "you could visualise this as an HMM state frequency bar chart — here's how to add it to your canvas" or generate the equivalent in a Pluto notebook.

**What's needed:**
- An MCP tool `get_available_plots` that returns the list of plot types from the JSON plot specs (already exist in the codebase — find them before designing a new source)
- For each plot: name, module, what data it needs (population type, measure name), scope modes (per_image/summarised)

**GUI plot suggestion (deferred to Opus):**
Whether Claude can suggest and possibly create a plot directly in the analysis canvas GUI is left for Opus to evaluate separately. The canvas JSON format and the API for adding plots programmatically may or may not be straightforward — Opus should assess and report rather than assume it's doable. The Pluto notebook path is the safe fallback and is always doable.

---

### 5. In-app Claude overview panel

A panel in the Cecelia GUI (sidebar page or settings section) that explains what Claude can do in this context, with simple examples. Not a feature tour — a reference users can consult when they're not sure whether to ask Claude.

Organized by category:

```
What Claude can see
  • Your analysis lineage (what ran, in what order)
  • Population trees and gate definitions
  • QC flags and cohort outliers
  • Measure summaries (cell speed, intensity, morphology)
  • HMM state distributions and cluster sizes
  • Task history and parameter history
  • Lab log entries

What Claude can suggest
  • Which images may need parameter adjustments (and which knobs)
  • How to visualize a pattern it detected
  • Plot strategies for the analysis canvas

What Claude can create
  • Pluto notebooks for data access and visualization
  • CSV exports for further analysis in Prism or R

What Claude cannot do
  • Change your data
  • Run tasks (you approve before anything runs)
  • Access image pixels or raw microscopy files
  • Draw biological conclusions (that's your job)
  • Write to H5AD, QC store, or project config

What Claude can write (when asked)
  • Lab log entries — already working via append_lab_log MCP tool
  • Pluto notebooks — figures and CSV only, no data store writes

Ask Claude: best for "why is this image off?" or "how do I plot behaviour differences?"
Chat to Claude: best for longer exploration, notebook generation, or getting unstuck
```

Each category has one short concrete example. The panel is static — it doesn't update. It's a reference, not a dynamic dashboard.

---

### 6. Parameter and task discovery from JSON specs

The JSON input definitions are already served to the GUI for `DynamicWidget`. Claude needs access to the same source for parameter suggestions and notebook generation.

Check whether a `get_module_params` MCP tool already exists or whether the param specs are already accessible. If not, it's a simple read of the JSON files at `inputDefinitions/`. Claude needs:
- Valid range for each param (min, max, step)
- Default value
- Param type (slider, checkbox, channelSelection etc.)
- Which params affect which aspects of the output (not all are equally important — Opus should assess whether this is annotatable in the JSON spec or has to be inferred)

---

## What NOT to build in this phase

- Automatic parameter adjustment (Claude suggests, user approves — no auto-run)
- Interpretation of biological meaning ("this population is activated") — Claude describes patterns, user interprets
- Direct GUI plot creation (Opus to evaluate separately, not this phase)
- Cross-project learning (stay within the current project's history)
- Any writes outside notebook files and figures/CSV

---

## Build order

1. REPL access documentation + MCP resource → notebooks need this first
2. Parameter history MCP tool (if not already exposed) → suggestions need this
3. Chat to Claude enhanced entry → unblocks useful sessions immediately
4. Notebook generation → the most user-visible new capability
5. Available plot types MCP tool → enables plot suggestions in notebooks
6. In-app overview panel → last, after the features exist to describe
7. GUI plot suggestion → Opus evaluates separately

---

## Verification

- Chat to Claude opens with context already loaded — user doesn't re-explain
- Claude suggests specific param values for a flagged image (within spec range, citing prior run outcome)
- "Make a notebook for this" → versioned Pluto notebook created, existing version untouched
- Notebook runs cleanly with `using Cecelia`, produces a figure or CSV, touches no data store
- Claude helps correct a broken notebook cell without overwriting the working version
- `get_available_plots` returns the plot type list Claude uses to suggest a visualization
- In-app panel accurately describes what's actually available (verify against real capabilities)
