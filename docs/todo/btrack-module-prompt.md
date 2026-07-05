# Phase: Port btrack Module Function + Module Page

This is a straightforward porting phase. Read the existing files first, port what's there, follow established patterns. One step at a time, stop and report after each step, re-read this document before starting the next.

Reference CLAUDE.md and ARCHITECTURE.md. Follow the existing module patterns already established in the codebase — this is not a design phase, it's a port.

## Step 1 — Read the originals (before writing anything)

Read the following files in full:

- `old-R-shiny-version/inst/modules/sources/tracking/bayesianTracking.R` — the original module function: what params it takes, what it calls, what it writes back
- The corresponding input definitions JSON (find it at `old-R-shiny-version/inst/app/modules/inputDefinitions/tracking/` — look for the bayesianTracking JSON file)

Also re-read one or two existing ported module functions and their module pages in the new codebase to confirm the current pattern before writing anything.

Report back: what the module does, the full param list from the JSON, and what btrack writes to disk (track IDs, coordinates, H5AD columns, or any other output files). Do not proceed to Step 2 until confirmed.

## Step 2 — Port the Julia module function

Port `bayesianTracking.R` to Julia following the established module pattern exactly:

- Julia module function file in `src/modules/tracking/`
- Co-located JSON input definition ported directly from the original — do not redesign params
- `run(params, task)` function signature matching the existing module convention
- Calls btrack via PythonCall.jl — btrack is a Python library, do not reimplement it
- Writes output exactly as the original did — confirm from Step 1 and replicate
- Param validation against the JSON spec
- Logging via the standard task logger

No new patterns. Follow existing module functions as the template.

## Step 3 — Module page

Add a module page for btrack following the existing module page pattern in the Vue frontend:

- Standard module page: image table, param form via `DynamicWidget` reading the JSON spec, run button wired to the task manager
- No plots — the plot framework for module pages is a separate phase. Leave a clearly marked placeholder: `<!-- TODO: tracking module plots — see module page plot phase -->`
- Appears in the correct position in the processing workflow sidebar: after segmentation, before gating

No new UI patterns. Follow existing module pages as the template.

## Step 4 — Verify

- Run the btrack module function from the REPL on a real image — assert it completes and produces the expected output
- Open the module page — assert the param form renders from the JSON spec, image table loads, task submits to the task manager
- Confirm end-to-end: Vue → task manager → Julia → btrack Python → output on disk
