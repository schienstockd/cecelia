# GUI clarity audit, P0 step 2: journey walk ("where do I click now?")

Worktree: `cecelia-gui-clarity-audit-archive`. All paths are relative to `frontend/src/` unless they start with `app/` or `docs/`.
**Every verdict below comes from reading the code. None was observed in the running app.**
The 5-second test: can a first-time user, using only what the UI shows, tell what to do next?

Context that shapes the verdicts:
- The welcome page (`modules/WelcomeModule.vue:17-26`) is a watermark with no copy. By design, no page is "the start".
- The map (`components/GuideMap.vue:1-15`) lights nothing until a recipe is picked ("there is no default path").
- **No next-step mechanism exists outside `lib/guides/**`.** A grep for `nextStep|next step|Next:` across `src/` turns up only the guide bubble and the acquisition finding's "Next" line (`utils/acqCheck.ts:124`). So after any task finishes, the only things that can say what comes next are a guide, a recipe, or the task picker's "When to use" lines. "When to use" (`tasks/TaskRunner.vue:880-905`, open by default via `useStoredSectionOpen(...,true)`) is the strongest in-page signpost in the app. Many verdicts below lean on it.
- The task picker preselects the top row as displayed (`tasks/TaskRunner.vue:617-624`). Groups follow `GROUP_ORDER` (`utils/taskGroups.ts:8-15`, where **'Import' ranks first**), and labels sort alphabetically inside each group (`utils/taskGroups.ts:47`).

---

## Journey 1: Import a new image set

| # | Step | Click path (route · control · file:line) | Next step signposted? | Dead end / must already know | Verdict |
|---|---|---|---|---|---|
| 1.1 | First launch | `/` · blank watermark (`modules/WelcomeModule.vue:17-26`). Sidebar items are locked with the tooltip "Open or create a project first." (`components/AppSidebar.vue:98,169`). There is a "Manage projects…" button (`AppSidebar.vue:131-136`). The orientation tour starts on its own once What's New closes (`App.vue:78`). Its step 2 points at the project block (`lib/guides/tour.ts` "Your project"). | Partly: a locked sidebar, one button, and the tour. The page itself says nothing. | If the user skips the tour, the only clue is the "Manage projects…" button. Its label names a list, not "create". | WEAK |
| 1.2 | Create project | Project panel empty state: "No projects yet… A project holds all your images and analysis for one experiment." plus a "Create your first project" button (`components/ProjectPanel.vue:350-356`). Below it sits "Create project" (`ProjectPanel.vue:505-509`). | Yes: empty state with one action. | none | PASS |
| 1.3 | After create | `createProject` only closes the modal (`ProjectPanel.vue:44-45`). Nothing navigates. The user stays on the blank `/`, and the sidebar unlocks with Data open (`AppSidebar.vue:76`). | No toast, no route change, no prompt. "Manage images" is merely the first visible item. | The user must infer that Manage images is the start. | WEAK |
| 1.4 | Make a set | `/manage-images`. Action-bar hint "Create a set to get started." (`modules/ManageImagesModule.vue:272`), the SetBar "New set" button (`components/SetBar.vue:186-187`), and "Add images" disabled with the tip "Create or select a set first, then add images" (`ManageImagesModule.vue:276-285`). | Yes: hint, button, and a disabled-reason tooltip. | Other pages show "Select a set to get started." (`components/ModuleLayout.vue:100`) and have no New set button (`allowManage` defaults to false), so arriving there first is a soft dead end. | PASS |
| 1.5 | Add files | "Add images" → FileBrowser → SeriesPicker (`ManageImagesModule.vue:276-286, 26-33`). On success it writes only `log.info("Added N images to …")` to the console rail (`ManageImagesModule.vue:214-217`). | **No.** Nothing on the page says the rows still need converting. The eye button is disabled with the tip "Import this image first" (`components/ImageTable.vue:605-611`). The right-panel picker preselects "Convert to OME-ZARR" (the Import group, top row alphabetically). Its "When to use" reads "Images added but not converted yet" (`app/src/tasks/importImages/omezarr.json`). | The user has to connect "added" with "Import this image first" and the "Convert…" task without help. Only the guide spells it out ("Added, but not converted yet", `lib/guides/importImages.ts:87-93`). | WEAK |
| 1.6 | Convert, then done | Select rows → Run (disabled tip "Select at least one image from the list to enable run", `tasks/TaskRunner.vue:1056`). | When the task finishes, the eye button enables and the Acquisition badge refetches. That's it. "Ready to clean up or segment" exists only in the guide (`lib/guides/importImages.ts:141-142`). | The user must already know the pipeline continues at Metadata/Cleanup/Segment. | WEAK |

**J1: PASS 2 · WEAK 4 · FAIL 0**

## Journey 2: Check acquisition / image quality

| # | Step | Click path | Signposted? | Dead end / must know | Verdict |
|---|---|---|---|---|---|
| 2.1 | Find the check | The "Acquisition" ghost button in the table tools of every ModuleLayout page (`components/ModuleLayout.vue:326`, `components/AcqCheckButton.vue:31-35`). It renders only when a set has images (`ModuleLayout.vue:322`). A warn dot and count appear when there are findings. Tooltip: "Check these movies against what you plan to do with them" (`utils/acqCheck.ts:110`). | Badge with a count, but no prompt after import. | The label "Acquisition" is a noun, not "Check". It sits among the CSV and filter toggles. | WEAK |
| 2.2 | Read findings | AcqCheckDialog: plan sentence, then "Set a plan"/"Edit" (`components/AcqCheckDialog.vue:8-19`). Each finding carries a labelled **Next** line (`utils/acqCheck.ts:121-126`). | Yes: every finding says what to do. | none | PASS |
| 2.3 | Fix metadata | A per-row metadata flag sits in front of the name (`ImageTable.vue:60-63`). "Select every flagged image" helper (`ImageTable.vue:345-351`). The Metadata page is in the sidebar. | Yes: a flag on the row opens the dialog. | none | PASS |
| 2.4 | Per-image QC | The QC dot column (`ImageTable.vue:636-638`). With no findings the tooltip reads "No QC yet — nothing has been run" (`ImageTable.vue:83`). | It says what is missing, not which task produces QC. | The user must know which tasks emit QC. | WEAK |
| 2.5 | Acquisition settings card | Only inside ImageMetadataDialog → "Settings card" button, and only when `cardAvailable` (`components/ImageMetadataDialog.vue:312-320`). | No: buried in a per-image dialog. | It must be known to exist. | WEAK |
| 2.6 | Cohort consistency | "Check cohort" (`components/CohortCheckButton.vue`, rendered at `ModuleLayout.vue:324`) hides itself unless the module banks cohort metrics. | It appears on some pages only. | The user can't predict where. | WEAK |
| 2.7 | Image triage (Find ROIs) | Only in the viewer panel's "ROIs" section (`modules/ViewerWindow.vue:6920-6923`). | No page-level entry. | It must be known. | WEAK |

**J2: PASS 2 · WEAK 5 · FAIL 0**

## Journey 3: Correct (drift, denoise, versions)

| # | Step | Click path | Signposted? | Dead end / must know | Verdict |
|---|---|---|---|---|---|
| 3.1 | Find the page | The sidebar entry is **Cleanup**, tooltip "Correct and denoise images before segmentation." (`lib/navGroups.ts:35`). | Only the tooltip says "correct". | A user looking for "drift" or "correction" has to hover. The Correction cockpit (wrench) is a different thing (see J4). | WEAK |
| 3.2 | Pick the function | `/cleanup` TaskRunner (`modules/CleanupModule.vue:10-19`). Default is **AF correction**, because Autofluorescence ranks before Align (`utils/taskGroups.ts:12`). Every intravital recipe puts drift first (`lib/guides/recipes.ts:165-166, 235`). | "When to use" helps once the right task is picked. | The default steers away from the recipe order. | WEAK |
| 3.3 | Drift correction | Purpose, Use when / Not when (`app/src/tasks/cleanupImages/drift_correct.json`: "Remove whole-frame drift…", "Planes within one stack are offset: use Within-stack alignment"). | Yes: per-task forks are named. | none | PASS |
| 3.4 | Denoise | The label is "Denoising (SUPPORT)". The Model param tip reads "Trained on the Model Training page (Phase B)" (`app/src/tasks/cleanupImages/denoise.json:38`). Use when: "Needs a model from Train denoise model (SUPPORT)". | The page is named, but "SUPPORT" and "Phase B" are internal words. | The user must train a model on another page first. Nothing links there. | WEAK |
| 3.5 | Versions | The output is a new version (`driftCorrected`). Downstream image pickers preselect the active version (`tasks/paramValues.ts:609-624`) and show an advisory when the picked version differs from the active one (`tasks/paramAdvisors.ts:335-370`). The active version is shown only in ImageMetadataDialog → "Stored files" → "Active version" (`components/ImageMetadataDialog.vue:351-354`). | Implicit: the right version is picked for you. | The user can't see on the table which version each image is on. Not verified: whether a correction makes its output active. | WEAK |
| 3.6 | Then segment | When the task completes, nothing points onward. Cellpose's Not-when line "Noisy or drifting images: fix them in Cleanup before tuning this" points backwards only. | No. | Must know. | WEAK |

**J3: PASS 1 · WEAK 5 · FAIL 0**

## Journey 4: Segment, then cleanup

| # | Step | Click path | Signposted? | Dead end / must know | Verdict |
|---|---|---|---|---|---|
| 4.1 | Open Segment | `/segment` (`modules/SegmentModule.vue:11-20`). The picker holds 14 functions in 5 groups. **The default is "Import labels"**: `segment/import_labels.json` has group `Import`, which ranks first (`utils/taskGroups.ts:9`), and "Import labels" sorts before "Import labels + measure". The top row is preselected (`tasks/TaskRunner.vue:623`). | The picker opens on a task the user almost never wants. | A first-time user has to know to scroll to the "Segment" optgroup. | **FAIL** |
| 4.2 | Pick a model / composite | "Cellpose segment + measure" says in Use when: "The usual choice: labels alone leave nothing to gate, track or cluster" (`segment/cellpose_measure.json`). Plain "Cellpose segmentation" has **no** Not-when line pointing to "+ measure" (`segment/cellpose.json` notWhen: dim / noisy / fibres only). Tracking does have that line (`tracking/bayesian_tracking.json`: "Tracks without speed or angle: use Bayesian track + measures"). | Only if the composite happens to be selected. | A user who picks the plain task gets labels and every later page comes up empty. | WEAK |
| 4.3 | Check the result | Segment plots canvas "Select one or more images above to plot." (`components/canvas/SummaryCanvas.vue:434`), with QC plots (`SegmentModule.vue:23-27`). | Yes. | none | PASS |
| 4.4 | Fix labels (cleanup) | The **Correction cockpit** opens from an icon-only wrench in the sidebar launcher row, with a WIP hammer badge. Tooltip: "Correction cockpit — tracks + labels (in progress)" (`components/PanelLaunchers.vue:38-49`). It says "Open an image in the viewer to begin" (`components/correction/CorrectionCockpit.vue:610`), and edits end in "Apply N". The Segment page's own "Correct labels" tasks say "Merge, split or remove labels from the correction cockpit's edits" (`segment/correct.json`) but don't link to it. | No link from Segment. The only route is an unlabeled icon. | Must already know the wrench means "fix segmentation" and that the viewer must be open first. | **FAIL** |
| 4.5 | Debris cleanup by gating | The recipes say "gate out debris … before tracking" (`lib/guides/recipes.ts:200-201, 242, 269`). Segment's Use when / Not when lines don't mention gating. | Recipe only. | Must know. | WEAK |

**J4: PASS 1 · WEAK 2 · FAIL 2**

## Journey 5: Define populations / gate

| # | Step | Click path | Signposted? | Dead end / must know | Verdict |
|---|---|---|---|---|---|
| 5.1 | Open Gate | `/gate`, single-select. "Select one image above to gate." (`modules/gate/GatingPlots.vue:570`). | Yes. | none | PASS |
| 5.2 | Pick segmentation | A lowercase label "segmentation" with tooltip "Which segmentation (labelProps) to gate on" (`GatingPlots.vue:573-577`). | — | **No empty state when the image has no segmentation.** The dropdown is simply empty, and nothing says "segment first". The tooltip leaks "labelProps". | WEAK |
| 5.3 | Add plot and draw | The primary "+ Plot" button (`GatingPlots.vue:580-582`). Icon-only draw chips: Rectangle, and Polygon "click vertices, double-click to close" (`modules/gate/GatePlotPanel.vue:89-92`). A name prompt follows (`GatePlotPanel.vue:561`). | Yes: a primary action and tooltips. | The draw chips are icon-only. Polygon uses `pi-share-alt`, the same glyph as Track. | PASS |
| 5.4 | See the populations | An icon-only toggle, "Show the populations rail" (`GatingPlots.vue` ~612-615). | Tooltip only. | The rail may start hidden. | WEAK |
| 5.5 | Copy to other images | "Copy this gating to other images in the set" (`GatingPlots.vue` ~625). | Yes. | none | PASS |
| 5.6 | Use the populations | Nothing on Gate points to Phenotype, Track, or the board. Track's Population param reads "Track the whole segmentation (NONE) or only the cells of a gated flow population" (`app/src/tasks/tracking/bayesian_tracking.json`). | No. | Must know which pages consume gates. "flow population" isn't a term on the Gate page. | WEAK |

**J5: PASS 3 · WEAK 3 · FAIL 0**

## Journey 6: Track, then behaviour clusters

| # | Step | Click path | Signposted? | Dead end / must know | Verdict |
|---|---|---|---|---|---|
| 6.1 | Track | `/track` TaskRunner (`modules/TrackingModule.vue`). Default is "Bayesian track + measures" (it sorts first in Track). Plain tracking's Not-when line points to it. | Yes. | none | PASS |
| 6.2 | Track a gate | Population param: literal `NONE` / "gated flow population" (`tracking/bayesian_tracking.json`). | Partly. | The user must map "flow population" to the gates they drew. | WEAK |
| 6.3 | Check tracks | "Track gating" canvas. On an untracked image it shows "Not tracked yet — run tracking on this segmentation first." (`modules/gate/GatePlotPanel.vue:557-560`). | Yes: a real empty state with the action. | none | PASS |
| 6.4 | Behaviour states (HMM) | HMM tasks run on **Behaviour**, which sits in the *Explore* group **below** Cluster tracks (`lib/navGroups.ts:47` vs `:56`). Behaviour's tooltip is "Summary plots of cell/track measures (speed, HMM states, …)". It never says HMM is *run* there. The Cluster tracks Use-when line "After HMM states, to cluster on state and transition frequencies" (`app/src/tasks/clustTracks/cluster.json`) doesn't name the page. | No. Sidebar order contradicts the recipe order (`lib/guides/recipes.ts:214-220`: behaviour-states, then cluster-tracks). | Must already know HMM lives on an "Explore" page. | **FAIL** |
| 6.5 | Cluster tracks + define pops | `/clust-tracks`. "Select clustered image(s) above to explore clusters." (`modules/cluster/ClusterPlots.vue:309`). There's a "Select clustered images" fix button (`ClusterPlots.vue:356`). Populations are made by ticking cluster IDs. | Partly. | Not verified from code: the tick-into-population gesture on the rail. | WEAK |
| 6.6 | Read the behaviour clusters | The trackclust population summary lives on **Behaviour** (file header, `modules/ClusterTracksModule.vue:9-10`). Cluster tracks doesn't link there. | No. | The user goes back down to Explore → Behaviour without being told. | WEAK |

**J6: PASS 2 · WEAK 3 · FAIL 1**

## Journey 7: Build a figure / analysis board / report

| # | Step | Click path | Signposted? | Dead end / must know | Verdict |
|---|---|---|---|---|---|
| 7.1 | Open board | `/analysis`: "Select one or more images above to plot." (`components/canvas/LayoutCanvas.vue:679`). | Yes. | none | PASS |
| 7.2 | Add plots | Each empty slot has a select, "Add a plot to this slot", plus an "empty slot" label (`LayoutCanvas.vue` ~826-842). | Yes. | First paint is dense: uniform grids, grid popover, A4/Free, compare, pool, plates (`LayoutCanvas.vue` ~688-768). | PASS |
| 7.3 | Export | "⤓ Export" select: PDF / SVG / CSV / Parameters (`components/canvas/TabbedCanvas.vue` export select, ~`data-guide="board.export"`), with an info icon explaining the formats. | Yes. | none | PASS |
| 7.4 | Viewer figure | Viewer panel button, tooltip "Save this view as a figure → Movies" (`utils/figureSpec.ts:99`, `components/ViewerPanel.vue:1400`). The Movies & figures empty state lists the sources (`modules/MoviesModule.vue:631-632`). "Edit" is disabled until a figure is picked, with the tip "Edit a figure: select one". | Yes. | "→ Movies" names a page that is labelled "Movies & figures". | PASS |
| 7.5 | "Report" | No surface is called "report". The candidates are Blackboard (notes), "What made this board" (an icon-only `pi-history` button, `TabbedCanvas.vue` ~tab-record), and Notebooks. The Blackboard empty state reads "No entries yet. Claude can create them via MCP, or click **New entry**." (`modules/BlackboardModule.vue:681`). | Partly. | Must know that Blackboard is the write-up place. The lineage record is icon-only. | WEAK |

**J7: PASS 4 · WEAK 1 · FAIL 0**

## Journey 8: Build a chain and run it on a batch

| # | Step | Click path | Signposted? | Dead end / must know | Verdict |
|---|---|---|---|---|---|
| 8.1 | Find chains | Pipeline → Chains, tooltip "Wire tasks into a chain and run it over a set." (`lib/navGroups.ts:80`). | Yes. | none | PASS |
| 8.2 | Create one | "No chains yet" (`modules/ChainModule.vue:1619`). The canvas says "Select a chain or create one to start editing." (`ChainModule.vue:1872-1875`). Create is an **icon-only "+"**, tooltip "Create a new chain template" (`ChainModule.vue:1624-1629`). | Text yes; the control is an unlabeled icon in a 7-icon group. | — | WEAK |
| 8.3 | Add and connect steps | Palette "Module functions", collapsible (`ChainModule.vue:1740`). A new chain gets a start dot automatically (`ChainModule.vue:899`), so the canvas `#empty` slot never shows again and no "drag a function here" prompt appears. The canvas hints are only "Drag to pan · Scroll to zoom · Double-click edge to remove" (`ChainModule.vue:1879-1881`). No hint explains how to *connect* nodes. | No. | Drag-from-palette and drag-a-handle-to-connect must be known (the `run-a-chain` guide says "drag one onto the canvas"). | WEAK |
| 8.4 | Build from what I already ran | An icon-only `pi-history` button, "Create a chain from what ran on some images" (`ChainModule.vue:1631-1637`). Module pages don't link to it. | No. | This is the natural "done it on 2 images, now batch it" path, and it's buried. | WEAK |
| 8.5 | Configure, pick images, run | "Click a node to configure it." (`ChainModule.vue:2023-2026`). Run section: set select, image ticks, "Run chain" with disabled reasons "Select or create a chain first." / "Select at least one image." (`ChainModule.vue:1303-1307, 1836-1844`). | Yes. | none | PASS |
| 8.6 | Watch it | The preflight modal "Before this run" with Run anyway / Cancel (`ChainModule.vue:1458-1471`). After a run it switches to the Live tab automatically (`ChainModule.vue:1345`). | Yes. | none | PASS |

**J8: PASS 3 · WEAK 3 · FAIL 0**

## Journey 9: Ask Claude/Kiwi about a result

| # | Step | Click path | Signposted? | Dead end / must know | Verdict |
|---|---|---|---|---|---|
| 9.1 | (AI on) Find Kiwi | An icon-only `pi-comments` launcher, tooltip "Kiwi — ask about your project, get answers you can check" (`components/PanelLaunchers.vue:65-73`). Also the tour step (`lib/guides/tour.ts:111-118`). | Tooltip and tour. | "Kiwi" as a name tells a new user nothing. | WEAK |
| 9.2 | (AI on) Attach a plot | An icon-only `pi-at` "@" button on live plot panels, tooltip "Add to Kiwi" (`components/canvas/CanvasPanel.vue:227-228`, `components/kiwi/AddToKiwiButton.vue:30-33`). It opens Kiwi with the ref attached (`stores/kiwi.ts:55-60`). | The tooltip only. The "@" glyph isn't self-evident. | — | WEAK |
| 9.3 | (AI on) Reach the Ask box | The cockpit stacks Account, **Pairing**, **Chat** ("Start in terminal"), **Share**, and only then the **Ask** section (`components/kiwi/KiwiCockpit.vue:374, 391, ~420, 443`). | The Ask box sits below three Claude-Code rows. | The pairing and socket jargon comes first. | WEAK |
| 9.4 | (AI on) Ask | Placeholder "Ask about what's attached…", Ctrl+Enter, "Ask Kiwi" (`components/kiwi/KiwiAsk.vue:222-238`). The account row has a log-in button when logged out. | Yes. | none | PASS |
| 9.5 | (AI off) Launcher | Still shown unless hidden. It opens on "AI features are off", "They send project data to Anthropic", "Turn on…" / "Hide this button" (`KiwiCockpit.vue:325-335`, `utils/aiSwitch.ts:20-26`). | Yes: one clear way back in. | none | PASS |
| 9.6 | (AI off) First-run tour | The tour (`tour.ts:111-118`) presents "Kiwi — answers you can check / Ask about what's on screen" on first launch, when AI is **off by default**. | The tour promises a feature that is off. | Clicking the launcher then says it's off. | WEAK |
| 9.7 | (AI off) Claude mentions on non-AI pages | Blackboard empty state "Claude can create them via MCP" (`BlackboardModule.vue:681`) and nav tooltip "…iterable with Claude." (`lib/navGroups.ts:66`) are not gated by the AI toggle. Guide map and AddToKiwi *are* gated (`GuideMap.vue` `showAi`, `AddToKiwiButton.vue:30`). | — | "MCP" is jargon. Claude is advertised while switched off. | WEAK |
| 9.8 | (AI off) Map | The ✦ toggle and assist lines are hidden when AI is off (`components/GuideMap.vue` `showAi = settings.guidesShowClaude && observer.ai.enabled`). | Yes. | none | PASS |

**J9: PASS 3 · WEAK 5 · FAIL 0**

---

## Totals

| Journey | PASS | WEAK | FAIL |
|---|---|---|---|
| 1 Import | 2 | 4 | 0 |
| 2 Acquisition / quality | 2 | 5 | 0 |
| 3 Correct | 1 | 5 | 0 |
| 4 Segment → cleanup | 1 | 2 | 2 |
| 5 Gate | 3 | 3 | 0 |
| 6 Track → behaviour clusters | 2 | 3 | 1 |
| 7 Figure / board / report | 4 | 1 | 0 |
| 8 Chain on a batch | 3 | 3 | 0 |
| 9 Kiwi / Claude | 3 | 5 | 0 |
| **All** | **21** | **31** | **3** |

Cross-cutting pattern: **in-page next-step signposting stops at the end of a task.** Pages are good at explaining a disabled state (tooltips such as "Select at least one image…", "Create or select a set first") and at offering per-task forks ("When to use"). Nothing says "this finished — next is X on page Y". That only exists in guides and recipes (open question 3: a per-recipe spine).

---

## Terminology drift (same thing, different word)

| Concept | Words used (quoted) | Where |
|---|---|---|
| Bringing pixels in | "Add images" · "Import from…" · "Import your first image" / "Import image" · "Import this image first" · filter "Imported" / "not yet imported" · task "Convert to OME-ZARR" · guide "Added, but not converted yet" | `ManageImagesModule.vue:284,294`; `ImageTable.vue:552-555,608`; `utils/rowFilters.ts:59,67`; `app/src/tasks/importImages/omezarr.json`; `lib/guides/importImages.ts:87` |
| Image correction vs object correction | Page "Cleanup" (tooltip "Correct and denoise images") · task Not-when "fix them in Cleanup" · tasks "AF correction", "Drift correction" · **and** "Correction cockpit", "Correct labels", "Correct tracks". The journey brief itself calls label fixing "cleanup". | `lib/navGroups.ts:35`; `segment/cellpose.json`; `PanelLaunchers.vue:38-49`; `segment/correct.json`; `tracking/correct.json` |
| A segmentation | "segmentation" (Gate label) · "(labelProps)" · "Output label name" · "label set" · "labels" ("Measure labels") · "segmented populations" | `GatingPlots.vue:573-574`; `segment/cellpose.json`; `tracking/bayesian_tracking.json`; `lib/navGroups.ts:43` |
| A gate's output | Page "Gate" · "populations rail" · group "Gated" · param "gated flow population" · `NONE` | `GatingPlots.vue` rail toggle; `utils/popGroups.ts:22`; `tracking/bayesian_tracking.json` |
| "Flow" (three meanings) | "flow population" (FlowJo gating) · "Optical flow segmentation" / "optical-flow segmentation" · "Flow-based registration" | `bayesian_tracking.json`; `segment/coastal.json`; `lib/navGroups.ts:36`; `cleanupImages/flow_register.json` |
| A runnable step | "Function" (picker heading) · "analysis function" · "Run '<label>'" · page "Tasks" · filter "Task" / "Processed with… any function…" · chain palette "Module functions" · "Wire tasks into a chain" | `TaskRunner.vue:842,844`; `lib/navGroups.ts:79-80`; `ModuleLayout.vue:340-370`; `ChainModule.vue:1740` |
| A chain | "Chains" · "chain template" · guide "Run a pipeline over a whole set" · "pipeline drawn as a graph" | `lib/navGroups.ts:80`; `ChainModule.vue:1627`; `lib/guides/singleTopicGuides.ts:207,218` |
| Behaviour | Page "Behaviour" (runs HMM *and* shows summaries) · "behaviour states" (HMM) · "Group whole tracks into behaviours" (Cluster tracks) · guide "Cluster tracks into behaviours" | `lib/navGroups.ts:56`; `clustTracks/cluster.json`; `lib/guides/taskGuides.ts:555,599` |
| Figure | Board export "figure, data, or both" (= PDF/SVG of a board) · viewer "Save this view as a figure → Movies" (= a still) · page "Movies & figures" | `TabbedCanvas.vue` export tooltip; `utils/figureSpec.ts:99`; `lib/navGroups.ts:72` |
| The assistant | "Kiwi" · "your assistant" ("Open a project to pair with your assistant") · "Claude Code session" · "Claude … via MCP" | `PanelLaunchers.vue:65-73`; `KiwiCockpit.vue:367,391-400`; `BlackboardModule.vue:681` |
| Internal labels leaking | "Denoising (SUPPORT)" · "Train denoise model (SUPPORT)" · "(Phase B)" · "(labelProps)" | `cleanupImages/denoise.json:38`; `GatingPlots.vue:574` |

---

## What to OBSERVE in the running app to confirm the WEAK/FAIL verdicts

Use a fresh profile in a private window and an isolated `CECELIA_DEV_DIR`.

1. **1.1 / 1.3**: On first launch, skip the tour and check whether anything on screen besides "Manage projects…" says what to do. After "Create project", confirm the app stays on the blank watermark.
2. **1.5 / 1.6**: After "Add images", check whether anything outside the console says the rows still need converting, and which function the right panel shows. After conversion finishes, check for any toast or prompt pointing to the next page.
3. **2.1**: On a set with images, can a new user spot the "Acquisition" button among CSV and filters? Does the badge count draw the eye?
4. **2.4–2.7**: Hover a QC dot on an unprocessed image. Find the "Settings card" (needs an image with `cardAvailable`). Check which pages show "Check cohort".
5. **3.2 / 4.1** (highest value): On a clean profile with no `cc-fn:segment` saved, open `/segment` and `/cleanup` and read the preselected function. Expected from code: "Import labels" and "AF correction".
6. **3.5**: After drift correction, does the image's active version change? Is the version visible anywhere in the image table?
7. **4.2**: Select plain "Cellpose segmentation" and read "When to use". Does anything point to "+ measure"?
8. **4.4**: From `/segment`, look for any route to the Correction cockpit other than the wrench icon. Does the wrench read as "fix segmentation"?
9. **5.2**: Open `/gate` on an image with no segmentation. Is the segmentation dropdown empty with no message? What does "+ Plot" do then?
10. **5.4**: Does the populations rail start open or hidden on a first visit?
11. **6.4**: Starting from tracked images, can a new user find where HMM states are run? Watch whether they go to "Cluster tracks" first.
12. **6.5 / 6.6**: Can the cluster → population tick gesture be found without the guide? Is there any route from Cluster tracks to the Behaviour summary?
13. **7.5**: Ask "where would you write up results?" Is the board's history icon found?
14. **8.2–8.4**: Create a chain. Does the canvas give any hint after the start dot appears? Is the connect gesture discoverable? Is the history icon noticed?
15. **9.1–9.3** (AI on): Do the "@" icon and Kiwi launcher read as "ask"? How far down is the Ask box at a normal window height?
16. **9.6 / 9.7** (AI off, the default): Watch the first-run tour's Kiwi step, then click the launcher. Open an empty Blackboard and read the "Claude … via MCP" line.
