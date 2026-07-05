# Phase: Chain Execution — Per-Image Pipelining with Resource-Pool Concurrency

This is a complex phase — don't attempt all of it in one pass. Work through the steps below one at a time. After each step, stop, report what you built and any tradeoffs, and wait for confirmation before continuing. Re-read this document in full before starting each new step — don't rely on memory of it from earlier in the session, details from later steps may change how an earlier step should have been built.

Reference CLAUDE.md, ARCHITECTURE.md, and the package/API/GUI separation phase (this depends on it being in place). This phase has two parts that must be designed together, not sequentially: the chain data model, and the scheduler. The scheduler is not an optimization to bolt on later — it's the reason this phase exists.

## The problem this solves

The old version processed images in lockstep batches: import all 20, wait, manually click denoise for all 20, wait, manually click segment for all 20. Two failures: (1) no pipelining — image 1 sits idle after finishing import instead of immediately starting denoise, (2) no per-stage concurrency awareness — denoising is GPU-bound and should run one image at a time, segmentation can run several images at once, but the old workflow couldn't express that distinction at all.

## Core rule

> **Each image progresses through the chain independently. A stage's concurrency limit is a property of the stage (resource pool), not a property of the batch.**

Image A starting segmentation has nothing to do with whether image B has finished denoising yet. The only thing that gates a transition is: (a) did this image's previous node finish, and (b) is there a free slot in this node's resource pool.

The one deliberate exception to this rule is Step 3 (picnic nodes) — read that before assuming every node fits the per-image model.

## Step 1 — Design the scheduler primitives (report back before wiring into chains)

This is genuinely the hard part — get the design reviewed before building the chain executor on top of it.

**Resource pools**: named, globally configured concurrency limits — e.g. `gpu: 1`, `segmentation: 4`, `io: 8`. Config lives outside module code (TOML/JSON, per-environment — a workstation with one GPU vs. a multi-GPU node shouldn't require code changes). Each module function's JSON spec gets a `resource_pool` field (default to a generous/unbounded pool if unspecified, so existing modules don't break).

**Scheduler**: lives in `tasks.jl` alongside the existing task system. Sketch (use your judgment on the right Julia primitives — `Base.Semaphore`, `Channel`-per-pool, or something else):

```julia
# one semaphore/queue per named pool, sized from config
# a task requests a pool slot when its dependency (prior node, same image) is satisfied
# task blocks in the pool's queue, not the chain's queue — this is what gives pipelining
```

**Per-image state machine**: each image's progress through a chain is tracked independently — effectively N small state machines (one per image) walking the same chain DAG, each requesting pool slots as it goes. Don't model this as "run stage 1 for all images, then stage 2" — that's the lockstep bug we're removing.

**Queue visibility**: a task waiting for a pool slot and a task actively running are different states — both the whiteboard node and the module-page task display must distinguish "queued, waiting on pool X" from "running" so a saturated GPU pool doesn't just look like nothing is happening.

**Fault isolation**: one image erroring at any stage must not block or cancel other images' progress through the chain. Surface the error on that image's task, continue everything else.

**Barrier primitive**: build one shared "wait until every image in the set has completed stage X" mechanism in the event bus now — it's needed by both Step 3 (picnic nodes) and Step 6 (final diagnostic plots). One implementation, two consumers, not two separate barrier mechanisms.

Report back: pool model, where the scheduler lives, how it integrates with the existing task manager (does a chain task become a parent with per-image-per-node child tasks?), and any tradeoffs before proceeding to Step 2.

## Step 2 — Chain data model: template vs. run

Two distinct artifacts, not one — this matters for reproducibility:

- **Template** — `chains/<name>.json`, reusable, no specific images baked in. Reference, not a hard schema:

```
{
  nodes: [{ id, fn: "moduleName.functionName", resource_pool, scope, params }],
  edges: [{ from, to }]   // linear in practice, DAG-capable for branching
}
```

`scope` is `"image"` (default — one task instance per image) or `"set"` (one task instance over the whole image list — see Step 3).

- **Run record** — created per image (or per set, for `scope: "set"` nodes) when a template is applied. Stores a **frozen copy** of the exact nodes/params/chain version that ran, not a reference back to the template by name. If the template is edited later, already-completed runs still show exactly what actually produced their results — editing `chains/<name>.json` must never retroactively change what a completed run is understood to have done.

A template is never tied to one image set — it should be validated on a handful of images via the module page, then applied project-wide to any set, per Step 8's standalone-to-chain composition workflow.

REPL contract, same discipline as `run_task`/`run_tasks`:

```julia
run_chain(project, image_ids; chain="name", overrides=Dict())
```

Must work headless, no API, no Vue — the scheduler is package-owned, not API-owned, same boundary rule as everything else in this codebase.

## Step 3 — Picnic nodes: set-scope synchronization points

Not every function fits the per-image model. HMM training from tracked cells is the clear example — it needs every image's tracking output simultaneously to build one model across the whole set, not N independent per-image runs. Internally call these **picnic nodes** (formal term: barrier / set-scope node) — everyone has to arrive before anything happens.

This is the `scope: "set"` case from Step 2's schema: one task instance over the full list of images, gated by the barrier primitive from Step 1 — it only starts once every image has cleared its upstream dependency.

After a picnic node, the chain can branch back to `scope: "image"` (e.g. classifying each image's tracks against the HMM model the picnic node produced) — downstream per-image nodes depend on both the picnic node's output and their own per-image upstream state. Don't force everything downstream of a picnic node to also become set-scope; the picnic is a single synchronization point, not a mode switch for the rest of the chain.

## Step 4 — Resume and restart semantics

If a chain fails partway, or a node's params change after upstream stages already ran (expensive ones like denoise/segment especially), the correct behavior is to **restart the affected task in place** — not spawn a new one. This also fixes an existing bug: the current module-page rerun button spawns a brand-new task rather than restarting the failed/existing one. Fix that as part of this step, not just for chains.

Concretely:
- Per image, per node: if that node already completed successfully and its params haven't changed, skip it on a chain re-run — don't redo expensive work that's already valid.
- If a node failed, or its params changed, or an upstream node it depends on reran — restart that node's task: same task identity, not a new task record, status reset to running/queued, prior failed/stale result superseded in place.
- This applies to picnic nodes too: if even one image's upstream input changes, the picnic node's prior set-wide result is stale and must restart — same in-place restart logic, just gated on the full set instead of one image.
- This means a task needs a stable identity independent of "did this run succeed or fail" — re-running is a state transition on the same task, not a new entry in the task list.

## Step 5 — Verify (resume/restart)

- Run a chain on an image, force a failure at node 3: confirm restarting it resumes from node 3 (nodes 1-2 not redone) and reuses the same task record, not a new one
- Change node 4's params after a successful full run: confirm only node 4 (and anything downstream of it) reruns, nodes 1-3 are skipped as still valid
- Confirm the module-page single-function rerun button now restarts the existing task rather than creating a duplicate
- Change one image's upstream input feeding a picnic node: confirm the picnic node's task restarts in place rather than silently keeping a stale set-wide result

## Step 6 — Diagnostic/incremental plots

Two distinct plot-node behaviors, both needed:

- **Incremental/diagnostic**: subscribes to "any image completed stage X" — recomputes a lightweight aggregate (e.g. running cell-count histogram) as each image clears that stage. Debounce this — if several images finish within the same second, don't recompute on every single one.
- **Final**: uses the same barrier primitive as picnic nodes (Step 1/3) — "all images in the set completed stage X" — produces the final plot.

This needs the lightweight event bus from Step 1 (stage-completion events), not polling. The event bus is package-owned — REPL users should be able to subscribe to it directly if they want live feedback in a script; the API layer's WebSocket push to Vue is just one more subscriber, not a special case.

## Step 7 — Verify (pipelining, picnic nodes, and plots)

- Start a chain on 20 images where denoise=`gpu` pool (limit 1) and segmentation=`segmentation` pool (limit 4): confirm image 1 starts denoising immediately after its own import finishes, without waiting for the other 19 to finish importing
- Confirm segmentation runs up to 4 images concurrently once they individually clear denoise, not gated by the batch
- Force an error on one image mid-chain: confirm the other 19 continue unaffected
- Run a chain with a picnic node (e.g. HMM training) after a per-image tracking stage: confirm it does not start until all images have finished tracking, runs once over the full set, and downstream per-image nodes correctly resume per-image scope afterward
- Confirm the incremental diagnostic plot updates as images clear a stage, and the final plot only fires once all images have (same barrier as the picnic node)
- Confirm `run_chain` works identically with `api/` not running

## Step 8 — Chain authoring: REPL and whiteboard are equal citizens

A chain is just the template JSON from Step 2 — both the REPL and the Vue whiteboard are authoring tools that read and write the same file, neither is privileged:

- **REPL**: build/edit a chain by constructing the JSON directly (or via a thin Julia helper), call `run_chain` on it — no GUI required, same headless discipline as everything else in this codebase.
- **Whiteboard**: visual editor over the identical schema — drag nodes from the same module spec list, connect edges, edit params via the existing `DynamicWidget` form. Saves to the same `chains/<name>.json`. Picnic nodes should be visually distinct on the canvas (e.g. a different node shape/border) since they behave differently from the per-image default — the whiteboard shouldn't make them look like an ordinary node.

A chain built in the REPL should open correctly on the whiteboard and vice versa — there's exactly one chain format, two ways to produce it.

## Step 9 — One task system, not two

The module page task runner and the whiteboard are two views over the same task system — not a separate scheduler each. This is load-bearing, not cosmetic:

- **Every task is a task**, regardless of where it was launched from. Running a single function from a module page creates exactly the same kind of task object as a node within a chain run.
- **Whiteboard shows every task as a node**, live, not just saved chain definitions. A one-off run from a module page appears on the whiteboard as an unconnected node while running and in recent history. Edges only exist between tasks that share a `chain_id` — no edge, no chain membership, but it's still a first-class node on the canvas.
- **Standalone nodes are connectable after the fact.** A researcher testing function params on a handful of images via the module page should be able to go to the whiteboard, see those runs as unconnected nodes, and drag edges from them to compose a chain — taking the params they just validated and wiring them into a pipeline for the rest of the image set. Don't model standalone vs. chained as different task types; the only difference is whether a `chain_id`/edge exists.
- **Module pages show a per-image badge** when a task is chain-connected — small, not a redesign of the module page UI. One badge per image task, no aggregation, links back to the relevant point on the whiteboard.

Module pages aren't a legacy UI being superseded by the whiteboard — they're where module-specific visualization, plots, and previews will live going forward. Don't let the whiteboard work crowd them out or treat them as a thin task-launcher to be minimized; the badge is an addition to that page, not a replacement for its purpose.

Not every module-page visualization is meant to become a whiteboard node. The declarative plot-node mechanism from Step 6 fits simple, composable outputs (histograms, scatter plots, summary stats) — those should be flagged on the module page as whiteboard-compatible (reusing the same plot module both places). More complex, interactive visualizations — cell gating being the clear example — are module-page-only; they're not expected to work as a floating whiteboard element and shouldn't be forced into the plot-node shape just for consistency. Each module page should make this distinction visible (e.g. a small "available on whiteboard" indicator on the plots that qualify), rather than implying everything is portable.

**First reference example**: there are no plots on module pages yet, so build one concrete shared example to validate the whole mechanism end to end before generalizing — a before/after intensity histogram per channel for Cellpose denoising. One plot module, `run` takes the pre- and post-denoise image data for a given channel and returns a declarative histogram spec (per Step 9's chosen format). This same module should render identically on the Cellpose module page and as a whiteboard plot node — proving the shared-source principle with something simple before any other module gets a plot.

**Module page layout**: the existing image table becomes collapsible — collapsed by default once a function has run, expanded for selecting images. Plots (the histogram example above) render below the table, not beside it or in a separate tab.

**Image selection — same component, compact variant.** Module pages already have an image table for selecting which images in a set a function runs on. The whiteboard node for that same function must use the identical underlying component for image selection — not a new picker. A whiteboard node's image input is that same table, toggleable, just rendered compactly to fit a graph node (e.g. a collapsed summary with a count and an expand-to-toggle interaction, rather than the full-size table). Same selection state and logic, different layout density — not a second implementation that can drift out of sync with the module page's selection behavior.

This is the same principle as the param form (same `DynamicWidget`, same JSON spec) extended to image selection — anywhere the whiteboard needs an input the module pages already have a working UI for, reuse that component rather than rebuilding it. The whiteboard should not become a parallel UI layer with its own component library; it's the same building blocks at a different scale.

Practical implication for Step 1's scheduler design: the resource-pool/concurrency logic must apply identically whether a task arrived via a module-page single run or as a chain node — there's no "ad-hoc path" that bypasses the pool limits.

**Whiteboard implementation**: `@vue-flow/core` for the node-graph canvas (drag, connect, pan/zoom) — standard choice for this in Vue, no need to build a canvas from scratch. Node palette sourced from the same module/plot JSON specs already used elsewhere. Per-node param panel reuses the existing `DynamicWidget` component rather than a new form implementation. Plot nodes render their output via `vega-embed` (or `vue-plotly`/`vue-chartjs` if you'd rather standardize on a different declarative format than Vega-Lite) — pick one plotting library and keep plot specs in that single format so the same spec also renders in Quarto reports without translation.

## Step 10 — Verify (task-system unification)

- Run a single function from a module page on 3 images; confirm all 3 appear as unconnected nodes on the whiteboard, showing queued/running state correctly
- Drag edges from those 3 nodes to a new downstream function node; confirm this correctly forms a chain (gets a `chain_id`, subsequent runs on more images can reuse it as a template)
- Run a 20-image chain; confirm each image's module page view shows its own small chain badge, not one aggregated badge for the batch
- Confirm a standalone task launched from a module page still respects its resource pool's concurrency limit, identically to a chain node
- Confirm a picnic node renders visually distinct from per-image nodes on the whiteboard

## Out of scope

Branching/conditional chains (a node choosing different downstream paths based on output) — keep the DAG support from Step 2 but don't build conditional logic yet. No MCP — that's hype-driven scope creep for this stage, not a real functionality gap; drop it from consideration entirely, not just deferred.
