# Start here: what's in Cecelia

This page names every part of Cecelia once: what it is for, what it needs, and whether you need
it on day one. The buttons themselves are taught in the app: open **Guides** (compass icon, top
right) and pick the guide named in the last column.

```mermaid
flowchart LR
  subgraph core["Day one"]
    D[Data<br/>import · metadata · segment] --> P[Populations<br/>gate · track · cluster]
    P --> E[Explore<br/>phenotype · behaviour · spatial]
  end
  E --> A[Analysis<br/>board · notebooks · movies]
  D -. same steps, every image .-> W[Whiteboard<br/>chains]
  W -. optional .-> H[HPC]
  K[Kiwi + Claude<br/>optional] -. reads .-> core
  K -. reads .-> A
```

## Day one

1. **Find your way around:** the tour runs by itself the first time you close the welcome dialog.
2. **Import images** → **fix pixel size and channel names** → **segment** → **gate** → **phenotype**.
   For timelapses, add **track** → **behaviour**.
3. If you work with intravital timelapses, follow the recipe **Intravital timelapse** at the top
   of Guides. It chains those guides in order and says why each step is there.

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

## The sidebar

The groups follow the analysis: **Populations** pages *define* who the cells are; **Explore** pages
*summarise* those populations across images. Every page except Tasks needs an open project.

### Data: get images in and ready

| Page | What it's for | Day one | Guide |
|---|---|---|---|
| Manage images | Add, organise, copy, move and export images | Yes | Import images |
| Metadata | Pixel size, channel names, colours. Fix before segmenting | Yes | Fix pixel size and channel names |
| Preprocessing | Crop, project or resample into a new image | Optional | Preprocess an image |
| Cleanup | Drift correction, bleedthrough/autofluorescence, denoise | Optional | Drift correct · Correct bleedthrough |
| Model training | Train your own denoise or flow models | No | Train a denoise / flow model |
| Segment | Find cells (Cellpose and others), with segmentation QC | Yes | Segment an image · Segment a movie by motion |

### Populations: define who the cells are

| Page | What it's for | Needs | Guide |
|---|---|---|---|
| Gate | Draw gates on cell measures, like flow cytometry | Segmentation | Create and gate populations |
| Track | Link cells over time; gate tracks | Timelapse + segmentation | Track cells |
| Cluster cells | Group cells by phenotype across a set | Segmentation | Cluster cells into phenotypes |
| Cluster tracks | Group tracks by behaviour | Tracks | Cluster tracks into behaviours |
| Cluster regions | Find tissue neighbourhoods | Spatial's neighbour graph | Cluster spatial regions |

### Explore: summarise populations

| Page | Summarises | Paired with |
|---|---|---|
| Phenotype | Counts and proportions per population, across images | Gate, Cluster cells |
| Behaviour | Speed, HMM states and other track measures | Track, Cluster tracks |
| Spatial | Neighbours, interactions, contacts. Build the graph first | Cluster regions |

### Analysis: your own figures and notes

| Page | What it's for | Day one |
|---|---|---|
| Analysis board | Combine any plots across pages, images and segmentations; export PDF/CSV | Later |
| Blackboard | Notes and diagrams for an open question, versioned, marked open/resolved | Optional |
| Notebooks | Pluto (Julia) notebooks for analysis the board can't do | No |
| Animation | Keyframe a fly-through of one image into a movie | No |
| Batch movies | One movie setup → one timelapse movie per image | No |
| Movies | Play the movies this project has rendered | — |

A quick timelapse movie of the image you're viewing is the **record** button in the viewer.
Guides: Build plots · Record a movie · Build an animation · Create a notebook · Use the blackboard.

### Pipeline: run many images

| Page | What it's for |
|---|---|
| Tasks | Everything running or finished; **History** shows past runs; pop-out window |
| Whiteboard | Draw your pipeline as a chain of tasks and run it on a whole set |

Guide: Run a pipeline over a whole set.

## Panels beside the pages

Opened from the icon row in the sidebar; they stay open while you change pages.

| Panel | What it's for |
|---|---|
| Viewer | Your image in 2D/3D with cells, tracks and populations. Opens in its own window; its controls panel stays in the main one |
| Correction cockpit | Fix masks and tracks by hand: merge, split, remove. *In progress* |
| Lab log | A dated diary per project, written by you, Claude and Cecelia's activity summary |
| Kiwi | Ask Claude about your project; connect your own Claude session |
| Console | Docked at the bottom: the app's messages. Look here when something fails |

## Working with Claude (optional)

Everything above works without Claude. Claude doesn't add steps to your analysis. It helps you
**check** a step: a segmentation that looks off, a gate, a cluster you can't name, a result you want
to state carefully. All of it fits into one loop:

1. **Point at the evidence.** Press **@ Add to Kiwi** on what puzzles you: an image row, a
   population, a plot, the viewer, a task or a Blackboard entry. If it is something *in* the
   picture, **Share** from the viewer or a plot and draw on it; that's a *capture*.
2. **Ask Kiwi.** Kiwi is a rubber duck. It notices, counts and points, but doesn't judge: it won't
   tell you why something happened or what to exclude. Every claim in its reply has a reference.
   Click it and the app shows you what the claim rests on. Ask a follow-up to keep going.
3. **Decide, then keep it.** Once you've settled something (or ruled it out), **Save to Blackboard**
   and tag it good or bad, with a why. Kiwi replies expire after 50 turns; the Blackboard keeps
   them. Bad-tagged entries are how Claude warns you when the same failure comes back.
4. **Go to a terminal session when one question isn't enough.**

| You want to… | Use |
|---|---|
| Ask about something you're looking at | **@** it, then ask Kiwi |
| Show Claude a spot *in* an image or plot | **Share** and draw on it (a capture) |
| Keep a conclusion, or a dead end | **Save to Blackboard**, tag good/bad |
| Work through a problem over many steps | Claude in a terminal |
| Have Claude make something: a chain from what you ran, a board, a notebook, a lab-log line | Claude in a terminal (you review it, then you run it) |
| Have Claude mark cells or tracks on the viewer, or jump to a frame | Claude in a terminal |
| Pull in your LabArchives notes | Claude in a terminal |

Kiwi only reads: it never changes anything, and points back through its references. A terminal
session can also add things (the rows above) but never runs a task or edits your data. What Claude
reads is sent to Anthropic, so check your institute's rules for unpublished or patient data.

**Setup:** install Claude Code and log in from Kiwi. That's all you need for Kiwi. For a terminal
session, also click Kiwi → Assistant → **Set up** once (it installs the Cecelia plugin), then
**Start in terminal**. Guide: Assist with Kiwi.

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
| Whiteboard (chains) | Run the steps you settled on over every image in a set | Steps that worked on one image first |
| HPC | Send the heavy part of a task to a SLURM/PBS cluster | Cluster account + SSH key; your admin sets it up in **Settings → HPC**, you add your login in **Preferences → HPC**. Not yet run on a real cluster |
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
| **Preferences** (your profile chip) | You: your own settings, your HPC login, your Claude login |
| **User profile** | Who is using the app, when several people share a computer |
| **View profile** | Hide sidebar pages you don't use. Tidies the menu; doesn't restrict anything |
| **Export / import** | Move a whole project as one folder (`.ccbundle`), from the project panel |
