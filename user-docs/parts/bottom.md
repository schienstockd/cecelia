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
**check** one: a segmentation that looks off, a gate, a cluster you can't name, a result you want to
state carefully. One loop, the same one Kiwi's **?** and the guide *Assist with Kiwi* show:

1. **Point at it.** Press **@ Add to Kiwi** on what puzzles you: an image row, a population, a plot,
   the viewer or a Blackboard entry. If it is a spot *in* the picture, **Share** a frame and draw on
   it; that's a *capture*.
2. **Ask Kiwi.** Kiwi is a rubber duck: it points and counts, it doesn't judge. Every claim in its
   reply has a reference; click it to see what the claim rests on.
3. **Keep it.** **Save** the reply, then tag it good or bad on the Blackboard, with a why. Replies
   expire after 50 turns; the Blackboard keeps them, and bad tags are how Claude warns you when the
   same failure comes back.
4. **Go further.** **Continue in terminal** on a reply when one question isn't enough. It saves the
   reply and copies a command that starts Claude in a terminal, picking up from that entry. The
   Blackboard is the bridge: a terminal session can't see Kiwi replies you didn't save.

| | Kiwi | Terminal | `pixi run chat` (no GUI, experimental) |
|---|:-:|:-:|:-:|
| Reads your project, captures, Blackboard and lab log | ✓ | ✓ | ✓ except captures (no viewer to share from) |
| Warns when a bad-tagged failure keeps coming back | ✓ | ✓ | ✓ |
| Marks cells and tracks on the viewer | – | ✓ | – (no viewer) |
| Adds a chain, board, notebook or lab-log line for you to review | – | ✓ | ✓ |
| Writes a custom module for a step Cecelia lacks (you reload and run it) | – | ✓ | ✓ |
| Reads your LabArchives notebook, once linked | – | ✓ | – |
| Brings in data: new project, set, images | – | – | ✓ |
| Runs a task or chain; adds, changes or deletes a gate | – | – | ✓ you approve each call |
| Deletes images or projects, or edits cell data | – | – | – |

**`pixi run chat`** is Cecelia without the GUI: Claude in your terminal is the front end, and it can
run steps, so it asks before each one. It is new and experimental. See
[`docs/INSTALL.md`](../docs/INSTALL.md) → *Without the GUI*.

Replies are provisional: they aren't checked against the biology. What Claude reads is sent to
Anthropic, so check your institute's rules for unpublished or patient data.

**Setup:** install Claude Code and log in from Kiwi; that's all Kiwi needs. For a terminal session,
also click Kiwi → Assistant → **Set up** once (it installs the Cecelia plugin).

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
| **Preferences** (your profile chip) | You: your own settings, your HPC login, your Claude login |
| **User profile** | Who is using the app, when several people share a computer |
| **Sidebar layout** | Hide sidebar pages you don't use. Tidies the menu; doesn't restrict anything |
| **Export / import** | Move a whole project as one folder (`.ccbundle`), from the project panel |
