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
  /** The PrimeIcons class, without the `pi ` prefix. */
  icon: string
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
      { icon: 'pi-spinner', means: 'Working' },
      { icon: 'pi-clock', means: 'Queued, waiting its turn' },
      { icon: 'pi-hourglass', means: 'Nothing has run here yet' },
      { icon: 'pi-check', means: 'Done, or confirm this' },
      { icon: 'pi-check-circle', means: 'Passed, or include it again' },
      { icon: 'pi-times', means: 'Close, cancel or clear' },
      { icon: 'pi-times-circle', means: 'Failed' },
      { icon: 'pi-exclamation-triangle', means: 'Warning — and on a delete, "click again"' },
      { icon: 'pi-exclamation-circle', means: 'A prerequisite is missing' },
      { icon: 'pi-info-circle', means: 'Something worth knowing' },
      { icon: 'pi-question-circle', means: 'What is this panel?' },
      { icon: 'pi-ban', means: 'Excluded from processing' },
      { icon: 'pi-flag', means: 'QC findings on this image' },
      { icon: 'pi-minus', means: 'Nothing to report, or zoom out' },
      { icon: 'pi-bell', means: 'Cecelia logged something that needs a look' },
      { icon: 'pi-sparkles', means: 'Claude wrote this' },
      { icon: 'pi-lightbulb', means: 'A suggestion' },
      { icon: 'pi-bolt', means: 'Preview it live, before running' },
      { icon: 'pi-lock', means: 'Pinned, or needs a project open first' },
      { icon: 'pi-lock-open', means: 'Not pinned — follows what you do' },
    ],
  },
  {
    title: 'Choosing',
    note: 'A filled shape is on, an outline is off, and a dash means "some of them".',
    icons: [
      { icon: 'pi-check-square', means: 'All of them' },
      { icon: 'pi-minus-circle', means: 'Some of them' },
      { icon: 'pi-stop', means: 'An empty square — nothing selected, or the rectangle gate' },
      { icon: 'pi-stop-circle', means: 'Stop this service' },
      { icon: 'pi-circle', means: 'Not running, or nothing to show' },
      { icon: 'pi-circle-fill', means: 'Points, coloured by density' },
      { icon: 'pi-chart-line', means: 'Density contours only — fastest' },
      { icon: 'pi-asterisk', means: 'Contours plus the sparse outliers' },
      { icon: 'pi-star', means: 'Not starred' },
      { icon: 'pi-star-fill', means: 'Starred' },
      { icon: 'pi-filter', means: 'Filter what is listed' },
      { icon: 'pi-link', means: 'Combine populations — one defined by others' },
      { icon: 'pi-filter-slash', means: 'Clear the filter' },
      { icon: 'pi-search', means: 'Search' },
      { icon: 'pi-sort-alt', means: 'Sortable, not sorted yet' },
      { icon: 'pi-sort-amount-up-alt', means: 'Sorted smallest first' },
      { icon: 'pi-sort-amount-down', means: 'Sorted largest first' },
    ],
  },
  {
    title: 'Doing things',
    note: 'Anything destructive arms on the first click and fires on the second.',
    icons: [
      { icon: 'pi-play', means: 'Run' },
      { icon: 'pi-play-circle', means: 'The movie player' },
      { icon: 'pi-forward', means: 'Playback speed' },
      { icon: 'pi-pause', means: 'Side by side' },
      { icon: 'pi-replay', means: 'Run it again, or restore a snapshot' },
      { icon: 'pi-undo', means: 'Undo — leave things as they were; mirrored, redo' },
      { icon: 'pi-refresh', means: 'Reload, or restart a service' },
      { icon: 'pi-sync', means: 'Re-read from the file, or the optical-flow page' },
      { icon: 'pi-trash', means: 'Delete' },
      { icon: 'pi-eraser', means: 'Delete what was derived, keep the original' },
      { icon: 'pi-plus', means: 'Add' },
      { icon: 'pi-pencil', means: 'Edit or rename' },
      { icon: 'pi-save', means: 'Save' },
      { icon: 'pi-copy', means: 'Copy — to the clipboard, or a copy of this' },
      { icon: 'pi-download', means: 'Bring it in from a file, or set up the observer' },
      { icon: 'pi-upload', means: 'Import' },
      { icon: 'pi-camera', means: 'Freeze this version as a snapshot' },
      { icon: 'pi-video', means: 'Record a movie' },
      { icon: 'pi-share-alt', means: 'Apply to the others — and cell tracks' },
      { icon: 'pi-power-off', means: 'Quit Cecelia' },
      { icon: 'pi-reply', means: 'Correct what Claude wrote' },
      { icon: 'pi-external-link', means: 'Opens outside the app' },
      { icon: 'pi-github', means: 'Opens the repository' },
      { icon: 'pi-comments', means: 'Chat to Claude' },
      { icon: 'pi-comment', means: 'A note on an image' },
    ],
  },
  {
    title: 'Showing and hiding',
    note: 'The eye is about what is on screen. It never means "allowed".',
    icons: [
      { icon: 'pi-eye', means: 'Shown — or click to show' },
      { icon: 'pi-eye-slash', means: 'Hidden' },
      { icon: 'pi-key', means: 'This glossary' },
      { icon: 'pi-compass', means: 'Guides — walk through the basics' },
      { icon: 'pi-cog', means: 'Settings and options' },
      { icon: 'pi-sliders-h', means: 'Viewer controls, or how a canvas is laid out' },
      { icon: 'pi-thumbtack', means: 'Keep these controls visible' },
      { icon: 'pi-bookmark', means: 'Saved for later — a folder, or the viewer look' },
      { icon: 'pi-database', means: 'Load slices on demand' },
      { icon: 'pi-search-plus', means: 'Zoom' },
      { icon: 'pi-tag', means: 'Labels — channel names, or labels drawn on a plot' },
      { icon: 'pi-palette', means: 'Colour — palettes, colour-by options, cluster hues' },
      { icon: 'pi-globe', means: 'Applies to every plot' },
      { icon: 'pi-map-marker', means: 'Just this one — the active plot, or a napari selection' },
    ],
  },
  {
    title: 'Getting around',
    note: 'A chevron points the way the thing will move. Doubled, it moves a whole panel.',
    icons: [
      { icon: 'pi-chevron-down', means: 'Moves down — expand, or drop the console away' },
      { icon: 'pi-chevron-up', means: 'Moves up — collapse, or raise the console' },
      { icon: 'pi-chevron-right', means: 'Moves right — expand sideways, or the next card' },
      { icon: 'pi-chevron-left', means: 'Moves left — collapse sideways, or the card before' },
      { icon: 'pi-angle-double-down', means: 'Jump to the newest line' },
      { icon: 'pi-angle-double-left', means: 'Show the side panel' },
      { icon: 'pi-angle-double-right', means: 'Hide the side panel' },
      { icon: 'pi-arrow-up', means: 'The folder above' },
      { icon: 'pi-arrow-left', means: 'Back' },
      { icon: 'pi-arrow-down', means: 'Lay the pipeline out top to bottom' },
      { icon: 'pi-arrow-right', means: 'Lay the pipeline out left to right' },
      { icon: 'pi-arrow-down-left', means: 'Zoom to fit the selected population' },
      { icon: 'pi-arrow-circle-up', means: 'An update is available' },
      { icon: 'pi-bars', means: 'Show or hide the menu' },
      { icon: 'pi-ellipsis-h', means: 'More actions' },
      { icon: 'pi-ellipsis-v', means: 'Drag to place' },
      { icon: 'pi-arrows-alt', means: 'Drag to move or swap' },
      { icon: 'pi-arrows-h', means: 'Move it somewhere else — another set, another parent population' },
      { icon: 'pi-arrows-v', means: 'Fit the height, or stack vertically' },
      { icon: 'pi-equals', means: 'Stacked in a column' },
      { icon: 'pi-window-maximize', means: 'Fill the window' },
      { icon: 'pi-window-minimize', means: 'Back to its own size' },
      { icon: 'pi-directions', means: 'Direction of movement' },
    ],
  },
  {
    title: 'Your project',
    icons: [
      { icon: 'pi-folder', means: 'The open project' },
      { icon: 'pi-folder-open', means: 'Open or create a project' },
      { icon: 'pi-file', means: 'A file it can read' },
      { icon: 'pi-file-o', means: 'A file it cannot read' },
      { icon: 'pi-image', means: 'One image' },
      { icon: 'pi-images', means: 'Images — a set, or none yet' },
      { icon: 'pi-gauge', means: 'Voxel size and frame interval' },
      { icon: 'pi-history', means: 'Earlier — past runs, versions, recent projects' },
      { icon: 'pi-server', means: 'Which resource pool it runs in' },
      { icon: 'pi-box', means: 'Installed packages' },
      { icon: 'pi-desktop', means: 'The console' },
      { icon: 'pi-wrench', means: 'A module you dropped in yourself' },
      { icon: 'pi-book', means: 'The lab log and notebooks' },
      { icon: 'pi-list-check', means: 'Tasks' },
      { icon: 'pi-clone', means: 'The analysis board, or cascade the plots' },
      { icon: 'pi-table', means: 'A heatmap of values' },
    ],
  },
  {
    title: 'What the pages do',
    note: 'A population keeps its glyph everywhere it appears — in the nav, the viewer and the plots.',
    icons: [
      { icon: 'pi-th-large', means: 'Segmentation — masks and label sets' },
      { icon: 'pi-chart-scatter', means: 'Gating' },
      { icon: 'pi-sitemap', means: 'Track clusters' },
      { icon: 'pi-objects-column', means: 'Spatial regions' },
      { icon: 'pi-map', means: 'Region populations' },
      { icon: 'pi-percentage', means: 'Phenotype — how much of each population' },
      { icon: 'pi-chart-bar', means: 'Behaviour and QC plots' },
      { icon: 'pi-wave-pulse', means: 'Branch skeletons' },
    ],
  },
]

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
