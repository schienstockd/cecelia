// THE icon glossary — what every glyph in this app means, and the reference to consult before
// choosing a new one.
//
// Two audiences, one list. A **user** opens it from the header key (`pi-key`, beside Guides) to find
// out what a symbol means. An **author** reads it before rendering an icon, so the app keeps saying one
// thing with one glyph. `docs/UI.md` → *Icons* points here.
//
// It cannot rot: `iconLegend.test.ts` scans every glyph actually rendered under `frontend/src` (with
// comments stripped) and fails when one is missing from this list — or listed here and used nowhere.
// A new icon therefore fails the suite until somebody says what it means, which is the whole point.
//
// Rules this list encodes, learned from the 2026-08-17 audit:
//   * ONE meaning per glyph. The audit found `pi-replay` doing both "run again" and "cancel", and
//     `pi-sliders-h` doing both "Settings" and "viewer controls" — 40px apart in the same sidebar.
//   * ONE glyph per meaning. Busy was split ~50/50 between a spinning cog and a spinning spinner;
//     "edit" was split between a pencil and a file-pencil.
//   * `pi-spin` is a MODIFIER, not a glyph: `pi-spin pi-spinner` is the one busy state.

export interface IconEntry {
  /** The class an author writes, without the `pi ` prefix (the names are PrimeIcons' — kept so no
   *  call site changed when the set moved to Lucide). */
  icon: string
  /** The Lucide glyph it renders (lucide.dev/icons). `scripts/vendor_icons.mjs` builds `src/icons.css`
   *  from these. */
  lucide: string
  /** Render the outline filled — Lucide is outline-only, and a few meanings need the solid shape. */
  fill?: true
  /** What it means here — one short line, the user's words not the developer's. */
  means: string
}

export interface IconFamily {
  title: string
  /** The rule that holds the family together — shown under the heading. */
  note?: string
  icons: IconEntry[]
}

export const ICON_LEGEND: IconFamily[] = [
  {
    title: 'Status',
    note: 'Severity is the traffic light: amber warns, red failed, green passed. Never decorative.',
    icons: [
      { icon: 'pi-spinner', lucide: 'loader-circle', means: 'Working' },
      { icon: 'pi-clock', lucide: 'clock', means: 'Queued, waiting its turn' },
      { icon: 'pi-hourglass', lucide: 'hourglass', means: 'Nothing has run here yet' },
      { icon: 'pi-check', lucide: 'check', means: 'Done, or confirm this' },
      { icon: 'pi-check-circle', lucide: 'circle-check', means: 'Passed, or include it again' },
      { icon: 'pi-times', lucide: 'x', means: 'Close, cancel or clear' },
      { icon: 'pi-times-circle', lucide: 'circle-x', means: 'Failed' },
      { icon: 'pi-exclamation-triangle', lucide: 'triangle-alert', means: 'Warning — and on a delete, "click again"' },
      { icon: 'pi-exclamation-circle', lucide: 'circle-alert', means: 'A prerequisite is missing' },
      { icon: 'pi-info-circle', lucide: 'info', means: 'Something worth knowing' },
      { icon: 'pi-question-circle', lucide: 'circle-question-mark', means: 'What is this panel?' },
      { icon: 'pi-ban', lucide: 'ban', means: 'Excluded from processing' },
      { icon: 'pi-flag', lucide: 'flag', means: 'QC findings on this image' },
      { icon: 'pi-minus', lucide: 'minus', means: 'Nothing to report, or zoom out' },
      { icon: 'pi-bell', lucide: 'bell', means: 'Cecelia logged something that needs a look' },
      { icon: 'pi-sparkles', lucide: 'sparkles', means: 'Claude wrote this' },
      { icon: 'pi-lightbulb', lucide: 'lightbulb', means: 'A suggestion' },
      { icon: 'pi-graduation-cap', lucide: 'graduation-cap', means: 'A schematic — what this control does' },
      { icon: 'pi-bolt', lucide: 'zap', means: 'Preview it live, before running' },
      { icon: 'pi-lock', lucide: 'lock', means: 'Pinned, or needs a project open first' },
      { icon: 'pi-lock-open', lucide: 'lock-open', means: 'Not pinned — follows what you do' },
    ],
  },
  {
    title: 'Choosing',
    note: 'A filled shape is on, an outline is off, and a dash means "some of them".',
    icons: [
      { icon: 'pi-check-square', lucide: 'square-check', means: 'All of them' },
      { icon: 'pi-minus-circle', lucide: 'circle-minus', means: 'Some of them' },
      { icon: 'pi-stop', lucide: 'square', means: 'An empty square — nothing selected, or the rectangle gate' },
      { icon: 'pi-stop-circle', lucide: 'circle-stop', means: 'Stop this service' },
      { icon: 'pi-circle', lucide: 'circle', means: 'Not running, or nothing to show' },
      { icon: 'pi-circle-fill', lucide: 'circle', fill: true, means: 'Points, coloured by density' },
      { icon: 'pi-chart-line', lucide: 'chart-line', means: 'Density contours only — fastest' },
      { icon: 'pi-asterisk', lucide: 'asterisk', means: 'Contours plus the sparse outliers' },
      { icon: 'pi-star', lucide: 'star', means: 'Not starred' },
      { icon: 'pi-star-fill', lucide: 'star', fill: true, means: 'Starred' },
      { icon: 'pi-filter', lucide: 'funnel', means: 'Filter what is listed' },
      { icon: 'pi-link', lucide: 'link', means: 'Combine populations — one defined by others' },
      { icon: 'pi-filter-slash', lucide: 'funnel-x', means: 'Clear the filter' },
      { icon: 'pi-search', lucide: 'search', means: 'Search' },
      { icon: 'pi-sort-alt', lucide: 'arrow-up-down', means: 'Sortable, not sorted yet' },
      { icon: 'pi-sort-amount-up-alt', lucide: 'arrow-up-narrow-wide', means: 'Sorted smallest first' },
      { icon: 'pi-sort-amount-down', lucide: 'arrow-down-wide-narrow', means: 'Sorted largest first' },
    ],
  },
  {
    title: 'Doing things',
    note: 'Anything destructive arms on the first click and fires on the second.',
    icons: [
      { icon: 'pi-play', lucide: 'play', means: 'Run' },
      { icon: 'pi-play-circle', lucide: 'circle-play', means: 'The movie player' },
      { icon: 'pi-forward', lucide: 'fast-forward', means: 'Playback speed' },
      { icon: 'pi-pause', lucide: 'columns-2', means: 'Side by side' },
      { icon: 'pi-replay', lucide: 'rotate-ccw', means: 'Run it again, or restore a snapshot' },
      { icon: 'pi-undo', lucide: 'undo-2', means: 'Undo — leave things as they were; mirrored, redo' },
      { icon: 'pi-refresh', lucide: 'refresh-cw', means: 'Reload, or restart a service' },
      { icon: 'pi-sync', lucide: 'refresh-ccw', means: 'Re-read from the file, or the model-training page' },
      { icon: 'pi-step-forward', lucide: 'skip-forward', means: 'Show another example — a different representative cell' },
      { icon: 'pi-trash', lucide: 'trash-2', means: 'Delete' },
      { icon: 'pi-eraser', lucide: 'eraser', means: 'Delete what was derived, keep the original' },
      { icon: 'pi-plus', lucide: 'plus', means: 'Add' },
      { icon: 'pi-user', lucide: 'user', means: 'Your active profile — click to open preferences' },
      { icon: 'pi-users', lucide: 'users', means: 'Show projects from every profile, not just yours' },
      { icon: 'pi-user-plus', lucide: 'user-plus', means: 'Add a Kiwi profile (a separate credential + MCP scope)' },
      { icon: 'pi-user-minus', lucide: 'user-minus', means: 'Retire a Kiwi profile — non-selectable after, data stays on disk' },
      { icon: 'pi-sign-in', lucide: 'log-in', means: 'Copy a login one-liner for this profile — paste in a terminal' },
      { icon: 'pi-pencil', lucide: 'pencil', means: 'Edit or rename' },
      { icon: 'pi-save', lucide: 'save', means: 'Save' },
      { icon: 'pi-copy', lucide: 'copy', means: 'Copy — to the clipboard, or a copy of this' },
      { icon: 'pi-download', lucide: 'download', means: 'Bring it in from a file, or set up the observer' },
      { icon: 'pi-upload', lucide: 'upload', means: 'Import' },
      { icon: 'pi-camera', lucide: 'camera', means: 'Freeze this version as a snapshot' },
      { icon: 'pi-video', lucide: 'video', means: 'Record a movie' },
      { icon: 'pi-share-alt', lucide: 'share-2', means: 'Apply to the others — and cell tracks; polygon draw tool' },
      { icon: 'pi-power-off', lucide: 'power', means: 'Quit Cecelia' },
      { icon: 'pi-reply', lucide: 'reply', means: 'Reply to what the assistant wrote — correct it, or follow it up' },
      { icon: 'pi-external-link', lucide: 'external-link', means: 'Opens outside the app' },
      { icon: 'pi-github', lucide: 'folder-git-2', means: 'Opens the repository' },
      { icon: 'pi-megaphone', lucide: 'megaphone', means: 'Call for Datasets — what we can build with your data' },
      { icon: 'pi-comments', lucide: 'messages-square', means: 'Chat to Claude' },
      { icon: 'pi-at', lucide: 'at-sign', means: 'Add to Kiwi — attach it to your next question' },
      { icon: 'pi-send', lucide: 'send', means: 'Ask Kiwi' },
      { icon: 'pi-question', lucide: 'message-circle-question-mark', means: 'A question Kiwi asks — a next look you can check' },
      { icon: 'pi-comment', lucide: 'message-square', means: 'A note on an image' },
      { icon: 'pi-thumbs-up', lucide: 'thumbs-up', means: 'A Blackboard thread that turned out right' },
      { icon: 'pi-thumbs-down', lucide: 'thumbs-down', means: 'A Blackboard thread that turned out wrong' },
    ],
  },
  {
    title: 'Showing and hiding',
    note: 'The eye is about what is on screen. It never means "allowed".',
    icons: [
      { icon: 'pi-eye', lucide: 'eye', means: 'Shown — or click to show' },
      { icon: 'pi-eye-slash', lucide: 'eye-off', means: 'Hidden' },
      { icon: 'pi-key', lucide: 'key-round', means: 'This glossary' },
      { icon: 'pi-compass', lucide: 'compass', means: 'Guides — walk through the basics' },
      { icon: 'pi-cog', lucide: 'settings', means: 'Settings and options' },
      { icon: 'pi-sliders-h', lucide: 'sliders-horizontal', means: 'Viewer controls, or how a canvas is laid out' },
      { icon: 'pi-thumbtack', lucide: 'pin', means: 'Keep these controls visible' },
      { icon: 'pi-bookmark', lucide: 'bookmark', means: 'Saved for later — a folder, or the viewer look' },
      { icon: 'pi-clipboard', lucide: 'clipboard-list', means: 'A written plan — the correction plan' },
      { icon: 'pi-search-plus', lucide: 'zoom-in', means: 'Zoom' },
      { icon: 'pi-tag', lucide: 'tag', means: 'Labels — channel names, or labels drawn on a plot' },
      { icon: 'pi-palette', lucide: 'palette', means: 'Colour — palettes, colour-by options, cluster hues' },
      { icon: 'pi-globe', lucide: 'globe', means: 'Applies to every plot' },
      { icon: 'pi-map-marker', lucide: 'map-pin', means: 'Just this one — the active plot, or a pick selection' },
    ],
  },
  {
    title: 'Getting around',
    note: 'A chevron points the way the thing will move. Doubled, it moves a whole panel.',
    icons: [
      { icon: 'pi-chevron-down', lucide: 'chevron-down', means: 'Moves down — expand, or drop the console away' },
      { icon: 'pi-chevron-up', lucide: 'chevron-up', means: 'Moves up — collapse, or raise the console' },
      { icon: 'pi-chevron-right', lucide: 'chevron-right', means: 'Moves right — expand sideways, or the next card' },
      { icon: 'pi-chevron-left', lucide: 'chevron-left', means: 'Moves left — collapse sideways, or the card before' },
      { icon: 'pi-angle-double-down', lucide: 'chevrons-down', means: 'Jump to the newest line' },
      { icon: 'pi-angle-double-left', lucide: 'chevrons-left', means: 'Show the side panel' },
      { icon: 'pi-angle-double-right', lucide: 'chevrons-right', means: 'Hide the side panel' },
      { icon: 'pi-arrow-up', lucide: 'arrow-up', means: 'The folder above' },
      { icon: 'pi-arrow-left', lucide: 'arrow-left', means: 'Back' },
      { icon: 'pi-arrow-down', lucide: 'arrow-down', means: 'Lay the pipeline out top to bottom' },
      { icon: 'pi-arrow-right', lucide: 'arrow-right', means: 'Lay the pipeline out left to right' },
      { icon: 'pi-arrow-down-left', lucide: 'arrow-down-left', means: 'Zoom to fit the selected population' },
      { icon: 'pi-arrow-circle-up', lucide: 'circle-arrow-up', means: 'An update is available' },
      { icon: 'pi-bars', lucide: 'menu', means: 'Show or hide the menu' },
      { icon: 'pi-ellipsis-h', lucide: 'ellipsis', means: 'More actions' },
      { icon: 'pi-ellipsis-v', lucide: 'grip-vertical', means: 'Drag to place' },
      { icon: 'pi-arrows-alt', lucide: 'move', means: 'Drag to move or swap' },
      { icon: 'pi-arrows-h', lucide: 'move-horizontal', means: 'Move it somewhere else — another set, another parent population' },
      { icon: 'pi-arrows-v', lucide: 'move-vertical', means: 'Fit the height, or stack vertically' },
      { icon: 'pi-arrow-right-arrow-left', lucide: 'arrow-right-left', means: 'Widen to the full range — an unclipped raw view' },
      { icon: 'pi-equals', lucide: 'equal', means: 'Stacked in a column' },
      { icon: 'pi-window-maximize', lucide: 'maximize-2', means: 'Fill the window' },
      { icon: 'pi-window-minimize', lucide: 'minimize-2', means: 'Back to its own size' },
      { icon: 'pi-directions', lucide: 'navigation', means: 'Direction of movement' },
    ],
  },
  {
    title: 'Your project',
    icons: [
      { icon: 'pi-folder', lucide: 'folder', means: 'The open project' },
      { icon: 'pi-folder-open', lucide: 'folder-open', means: 'Open or create a project' },
      { icon: 'pi-file', lucide: 'file', means: 'A file it can read' },
      { icon: 'pi-file-o', lucide: 'file-x', means: 'A file it cannot read' },
      { icon: 'pi-image', lucide: 'image', means: 'One image' },
      { icon: 'pi-images', lucide: 'images', means: 'Images — a set, or none yet' },
      { icon: 'pi-gauge', lucide: 'ruler', means: 'Voxel size and frame interval' },
      { icon: 'pi-history', lucide: 'history', means: 'Earlier — past runs, versions, recent projects' },
      { icon: 'pi-server', lucide: 'server', means: 'Which resource pool it runs in' },
      { icon: 'pi-box', lucide: 'package', means: 'Installed packages' },
      { icon: 'pi-desktop', lucide: 'square-terminal', means: 'The console' },
      { icon: 'pi-database', lucide: 'database', means: 'The model vault — a trained denoise or flow model' },
      { icon: 'pi-wrench', lucide: 'wrench', means: 'A tool you reach for — the correction cockpit, or a module you dropped in' },
      { icon: 'pi-hammer', lucide: 'hammer', means: 'A surface still being built — expect more verbs in a follow-up release' },
      { icon: 'pi-book', lucide: 'book-open', means: 'The lab log and notebooks' },
      { icon: 'pi-align-left', lucide: 'text-align-start', means: 'The blackboard — shared Markdown notes for an analysis' },
      { icon: 'pi-paperclip', lucide: 'paperclip', means: 'An attachment — a capture stapled to a blackboard entry' },
      { icon: 'pi-list-check', lucide: 'list-checks', means: 'Tasks' },
      { icon: 'pi-clone', lucide: 'layers', means: 'The analysis board, or cascade the plots' },
      { icon: 'pi-table', lucide: 'table', means: 'A heatmap of values' },
    ],
  },
  {
    title: 'What the pages do',
    note: 'A population keeps its glyph everywhere it appears — in the nav, the viewer and the plots.',
    icons: [
      { icon: 'pi-th-large', lucide: 'layout-grid', means: 'Segmentation — masks and label sets' },
      { icon: 'pi-chart-scatter', lucide: 'chart-scatter', means: 'Gating' },
      { icon: 'pi-sitemap', lucide: 'network', means: 'Track clusters' },
      { icon: 'pi-objects-column', lucide: 'land-plot', means: 'Spatial regions' },
      { icon: 'pi-map', lucide: 'map', means: 'Region populations' },
      { icon: 'pi-percentage', lucide: 'percent', means: 'Phenotype — how much of each population' },
      { icon: 'pi-chart-bar', lucide: 'chart-column', means: 'Behaviour and QC plots' },
      { icon: 'pi-wave-pulse', lucide: 'git-branch', means: 'Branch skeletons' },
    ],
  },
]

// ─── Adding a glyph ─────────────────────────────────────────────────────────────────────────────
// Look here first — reuse a glyph that already means the thing. A new one needs its own meaning: pick
// it from Lucide (https://lucide.dev/icons, ~2,000 glyphs), add `{ icon: 'pi-<name>', lucide:
// '<lucide-name>', means: '…' }` above, and run `pixi run icons` to vendor it into `src/icons.css`.
// The `pi-` class is the author-facing name (PrimeIcons' names, kept so no call site changed when the
// set moved to Lucide); a new glyph may take its Lucide name after the prefix.
//
// Modifiers, NOT glyphs — never document these as icons:
//   pi-spin (spin animation) — always paired with a glyph, e.g. `pi pi-spin pi-spinner`.

/** Every glyph the legend explains. */
export function legendGlyphs(): Set<string> {
  return new Set(ICON_LEGEND.flatMap(f => f.icons.map(i => i.icon)))
}

/** What one glyph means, or `undefined` — the lookup the ratchet and the dialog share. */
export function iconMeaning(glyph: string): IconEntry | undefined {
  for (const f of ICON_LEGEND) {
    const hit = f.icons.find(i => i.icon === glyph)
    if (hit) return hit
  }
  return undefined
}
