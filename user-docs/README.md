# Start here: what's in Cecelia

This page names every part of Cecelia once: what it is for and what it needs. In the app, the sidebar
is this map, and **Guides** (Help ▾ → Guides, top right) opens on your next step, where each guide shows
you the buttons.

```mermaid
flowchart LR
  subgraph core["The analysis"]
    D[Data<br/>import · metadata · segment] --> P[Populations<br/>gate · track · cluster]
    P --> E[Explore<br/>phenotype · behaviour · spatial]
  end
  E --> A[Analysis<br/>board · notebooks · movies]
  D -. same steps, every image .-> W[Chains]
  W -. optional .-> H[HPC]
  K[Kiwi + Claude<br/>optional] -. reads .-> core
  K -. reads .-> A
```

## Where to start

1. **Find your way around:** the tour runs by itself the first time you close the welcome dialog.
2. **Open Guides and select your recipe.** Where you start depends on your data: whether it moves, how
   many markers it has, and what you want to know. The recipe lights its pages in the sidebar, and
   Guides shows your next step, each with why it is there. The recipes are listed [below](#recipes).
3. **No recipe fits?** Click **Ask for a recipe** under the recipe list in Guides: tell us what you image and what
   you want to know.

Everything below "Scaling up" can wait.

## How your data is organised

| Term | Meaning |
|---|---|
| **Project** | One study. Everything below lives inside it. |
| **Set** | A group of images processed together (e.g. one mouse, one condition). |
| **Image** | One acquisition. Belongs to exactly one set. |
| **Image version** | Corrections (drift, autofluorescence, denoise) make a new version; the raw import stays. Pick which one you see in the viewer. |
| **Segmentation** | Each segmentation has a name and its own cell table, so you can keep several side by side. |
| **Population** | A named group of cells or tracks, from a gate or a cluster. Most plots are drawn per population. |

## Recipes

Open **Guides** and select the one that matches your data: its pages light up in the sidebar and its
steps are listed in order, each with why it is there. Steps marked optional depend on your images.
Every recipe ends the same way: once the settings work on a few images, run them over the whole set
as a chain.

| Recipe | When this is you | Steps |
|---|---|---|
| **Few-marker tissue** | A fixed section with a handful of markers, e.g. a confocal stack of spleen. | 9 (2 optional) |
| **Multiplex tissue** | Many markers on one section, e.g. IBEX or CODEX of a lymph node or spleen. | 9 (1 optional) |
| **Intravital timelapse** | A time-lapse of cells moving in tissue, e.g. T cells in a lymph node. | 13 (5 optional) |
| **Live cells and structures** | A time-lapse where cells move along a structure, e.g. T cells and lymphatics in skin. | 10 (1 optional) |
| **In vitro migration** | Cells migrating in a dish or chamber, e.g. a cancer cell line under two treatments. | 8 |

## The sidebar

The groups follow the analysis: **Populations** pages *define* who the cells are; **Explore** pages
*summarise* those populations across images. Every page except Tasks needs an open project. Each
page that defines a population names the page that summarises it. **Maturity** says how far to
trust what the page produces: *Experimental* — check every result by hand; *Beta* — checked by hand
on real data, spot-check results; *Tested* — a test fails when the result is wrong. **Help** →
**Feature maturity** shows each feature's evidence and what would move it up.

### Data: get images in and ready

| Page | What it's for | Summarised on | Guide | Maturity | Claude |
|---|---|---|---|---|---|
| Manage images | Add, organise, copy, move and export images |  | Import images | Beta |  |
| Preprocessing | Crop, project or resample into a new image |  | Preprocess an image | Experimental |  |
| Metadata | Pixel size, channel names, colours — fix before segmenting |  | Fix pixel size and channel names | Beta |  |
| Cleanup | Drift correction, bleedthrough and autofluorescence, denoise |  | Drift correct a time series | Beta |  |
| Model training | Train your own denoise or flow models |  | Train a flow model | Experimental |  |
| Segment | Find cells, with segmentation QC |  | Segment an image | Beta |  |

### Populations: define who the cells are

| Page | What it's for | Summarised on | Guide | Maturity | Claude |
|---|---|---|---|---|---|
| Gate | Draw gates on cell measures, like flow cytometry | Phenotype | Create and gate populations | Beta | Claude Code proposes a gate; keep or delete it |
| Track | Link cells over time; gate tracks | Behaviour | Track cells | Beta |  |
| Cluster cells | Group cells by phenotype across a set | Phenotype | Cluster cells into phenotypes | Beta |  |
| Cluster tracks | Group tracks by behaviour | Behaviour | Cluster tracks into behaviours | Beta |  |
| Cluster regions | Find tissue neighbourhoods | Spatial | Cluster spatial regions | Experimental |  |

### Explore: summarise those populations across images

| Page | What it's for | Summarised on | Guide | Maturity | Claude |
|---|---|---|---|---|---|
| Phenotype | Counts and proportions per population, across images |  |  | Experimental |  |
| Behaviour | Speed, HMM states and other track measures |  | Classify behaviour states | Experimental |  |
| Spatial | Neighbours, interactions, contacts — build the graph first |  | Score spatial interactions | Experimental |  |

### Analysis: your own figures and notes

| Page | What it's for | Summarised on | Guide | Maturity | Claude |
|---|---|---|---|---|---|
| Analysis board | Combine any plots across pages and images; export PDF or CSV |  | Build plots | Beta | Claude Code builds a board of plots for you |
| Blackboard | Notes for an open question, versioned, tagged good or bad |  | Use the blackboard |  | Kiwi replies you save land here; Claude Code writes entries |
| Notebooks | Pluto (Julia) notebooks for what the board can’t do |  | Create a notebook | Experimental | Claude Code writes a notebook for you |
| Animation | Keyframe a fly-through of one image into a movie |  | Build an animation | Beta |  |
| Batch movies | One movie setup, one timelapse movie per image |  | Record a movie | Beta |  |
| Movies & figures | Play the movies this project has rendered |  |  |  |  |

### Pipeline: run many images

| Page | What it's for | Summarised on | Guide | Maturity | Claude |
|---|---|---|---|---|---|
| Tasks | Everything running or finished; history of past runs |  |  |  |  |
| Chains | Wire the steps you settled on into a chain; run it over a set |  | Run a pipeline over a whole set | Beta | Claude Code designs the chain; you run it |

## Panels beside the pages

Opened from the icon row in the sidebar; they stay open while you change pages.

| Panel | What it's for |
|---|---|
| Viewer | Your image in 2D/3D with cells, tracks and populations. Opens in its own window; its controls panel stays in the main one |
| Correction cockpit | Fix masks and tracks by hand: merge, split, remove. *In progress* |
| Lab log | A dated diary per project, written by you, Claude and Cecelia's activity summary |
| Kiwi | Ask about your project and get answers you can check; connect your own Claude session |
| Console | Docked at the bottom: the app's messages. Look here when something fails |

## Working with Claude (optional)

**AI is off until you turn it on.** Turn it on in **Preferences → Kiwi → AI features**. A dialog
first lists what is sent and what you gain. Turn it off there at any time. With AI off, Kiwi's button
opens a short note, or you can hide it. Hand-offs to Kiwi and the AI parts of the Guides are hidden.
Claude sessions in your terminal are refused, and nothing is sent to Anthropic. What Claude wrote
earlier stays, marked ✦. On a shared workstation, peanut can turn AI off for everyone in
**Settings → Shared workstation → Allow AI features**.

Everything above works without Claude. Claude doesn't add steps to your analysis. It helps you
**check** one: a segmentation that looks off, a gate, a cluster you can't name, a result you want to
state carefully. There are two ways in, and either can come first; Kiwi's **?** and the guide
*Assist with Kiwi* show the same:

**Kiwi** gives answers you can check. Every claim in its reply links to the data it is about, and
Cecelia checks that each link exists and that Kiwi read it before you see the reply; whether the data
supports the claim is yours to judge. When it can't tell, it says so. It
can't change anything in your project, and it needs no terminal: log in once and ask. It points at
the evidence; you decide.

1. **Point at it.** Press **@ Add to Kiwi** on what puzzles you: an image row, a population, a plot,
   the viewer or a Blackboard entry. If it is a spot *in* the picture, **Share** a frame and draw on
   it; that's a *capture*.
2. **Ask Kiwi.** It points and counts, it doesn't judge. Every claim in its reply has a reference;
   click it to see what the claim rests on.
3. **Keep it.** **Save** the reply, then tag it good or bad on the Blackboard, with a why. Replies
   expire after 50 turns; the Blackboard keeps them, and bad tags are how Claude warns you when the
   same failure comes back.

**Claude Code** is the interactive route: you work the analysis through with Claude in a terminal.
Start it from Kiwi's Chat row (**Start in terminal**); you don't need to ask Kiwi first. It can mark
cells and add chains, boards and notebooks for you to review. To carry a Kiwi reply over, press
**Continue in terminal** on it: that saves the reply and starts the session from that entry. The
Blackboard is the bridge: a terminal session can't see Kiwi replies you didn't save.

| | Kiwi | Terminal | `pixi run chat` (no GUI, experimental) |
|---|:-:|:-:|:-:|
| Every claim links to data it read, checked before you see it | ✓ | – | – |
| Can't change anything, enforced | ✓ | – | – |
| Reads your project, captures, Blackboard and lab log | ✓ | ✓ | ✓ except captures (no viewer to share from) |
| Warns when a bad-tagged failure keeps coming back | ✓ | ✓ | ✓ |
| Marks cells and tracks on the viewer | – | ✓ | – (no viewer) |
| Adds a chain, board, notebook or lab-log line for you to review | – | ✓ | ✓ |
| Proposes a gate (marked ✦, undoable); moves or removes only its own | – | ✓ | ✓ |
| Writes a custom module for a step Cecelia lacks (you reload and run it) | – | ✓ | ✓ |
| Reads your LabArchives notebook, once linked | – | ✓ | – |
| Brings in data: new project, set, images | – | – | ✓ |
| Runs a task or chain | – | – | ✓ you approve each call |
| Deletes images or projects, or edits cell data | – | – | – |

**`pixi run chat`** is Cecelia without the GUI: Claude in your terminal is the front end, and it can
run steps, so it asks before each one. It is new and experimental. See
[`docs/INSTALL.md`](../docs/INSTALL.md) → *Without the GUI*.

Replies are provisional: they aren't checked against the biology. What Claude reads is sent to
Anthropic, so check your institute's rules for unpublished or patient data.

**Setup:** install Claude Code and log in from Kiwi; that's all Kiwi needs. For a terminal session,
also click Kiwi → Assistant → **Set up** once (it installs the Cecelia plugin).

### Checking a claim

Before you write a result down, state it from its summary plot: the claim button in the plot's
footer takes one sentence ("T cells move faster than B cells") and the replicate (e.g. Mouse). The
checks need no AI. They read what Cecelia recorded: are the images there, is each step's processing
saved, were the compared images and groups processed the same way, how many replicates, are units
calibrated. Each line is a fact, or a problem with what to do. With AI on, **Model review → Run** asks a model
for what the checks can't see; it reads the record, not your notes, and you mark its points Good,
Bad or Unsure. Everything lands on one Blackboard entry, and the checks run again whenever the plot
opens. Nothing is blocked.

<p align="center"><img src="../frontend/public/readme-claim.svg" alt="Claim recital: a claim from a plot, checked against the record. A helper, not a gate: nothing is blocked, and no p-value is computed. State a claim: one sentence from a summary plot, naming the replicate (e.g. Mouse). Checks, no model: images, processing record, same processing, replicates, units; each line is a fact, a problem with what to do, or not checked. Model review, optional: with AI on, about a minute; it reads what was imaged, run and measured, not your notes, knows what the checks found and adds only what they miss. Both go to one Blackboard entry, every point by category with what to do; you mark the review's points Good, Bad or Unsure; restating starts a new round and clears the old review. Re-check: when the plot opens; a check finding closes once the analysis no longer shows it." width="100%"></p>

## Where to write things down

| You want to record… | Use |
|---|---|
| Something about one image ("fold in this section") | The image's note, in the image table |
| What you did and why, dated | Lab log |
| An open question you are working through | Blackboard entry |
| What ran, with which parameters | Nothing: Cecelia records it. See task history |

## Scaling up

| Feature | What it's for | Needs |
|---|---|---|
| Chains | Run the steps you settled on over every image in a set | Steps that worked on one image first |
| HPC (optional) | Run segmentation, the heavy step, on a SLURM/PBS cluster. More tasks may follow | Cluster account + SSH key; your admin sets it up in **Settings → HPC**, you add your login in **Preferences → HPC**. Built for one lab's cluster so far |
| Task limits | How many tasks run at once, per CPU/GPU/disk | The sliders on the Tasks page |

## Extending Cecelia

| Feature | What it's for |
|---|---|
| Custom module | Your own analysis step, as a task with its own parameters |
| Plugin | One or more custom modules packaged to share, optionally with its own page |

Both run with full access to your machine. Install only code you trust.

## Settings, profiles and moving projects

| | What it's for |
|---|---|
| **Settings** (sidebar footer) | The machine: storage, updates, HPC, connected tools. Shared by everyone on this computer |
| **Preferences** (your avatar menu, top right) | You: your own settings, your HPC login, your Claude login |
| **User profile** | Who is using the app, when several people share a computer |
| **Sidebar layout** | Hide sidebar pages you don't use. Tidies the menu; doesn't restrict anything |
| **Export / import** | Move a whole project as one folder (`.ccbundle`), from the project panel |
