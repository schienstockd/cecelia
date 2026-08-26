// "Record a movie" — the most-asked-for OUTPUT, and the one place the app has three routes to the same
// result, which is exactly why it needs a guide (Dominik, 2026-08-12).
//
// The three routes, as the /movies empty state itself lists them: the Viewer panel (records the current
// napari view — what you see is what you get), Batch movies (the same config across a whole selection),
// and Animation (a keyframed timeline). This guide teaches the first two: one movie to get the look
// right, then the same look across the set. Animation is a different job — a crafted camera move — and
// belongs in its own guide rather than as a coda to this one.
//
// Neither route is a TaskRunner function, so this is not a `moduleTaskGuide`: the Viewer records what
// napari is showing right now, and Batch movies has its own config panel with its own task list.

import type { GuideDef } from './types'
import { PREREQ } from './prereqs'

export const recordMovieGuide: GuideDef = {
  id: 'record-a-movie',
  title: 'Record a movie',
  group: 'Analysis',
  icon: 'pi-video',
  summary: 'Record what napari is showing as an mp4, then repeat the same look across a whole set.',
  prereqs: [PREREQ.projectOpen, PREREQ.timeSeries],

  steps: [
    {
      anchor: 'images.napariBtn',
      placement: 'right',
      title: 'Set the shot up first',
      text: 'A recording captures exactly what napari is showing — so get the view right before you record.',
      bullets: [
        'Open the image with the ↗ beside it.',
        'Channels, contrast, masks, tracks, populations: all of it is recorded as shown.',
      ],
      when: c => c.napariImageUid !== null,
    },
    {
      anchor: 'sidebar.viewerCta',
      placement: 'right',
      title: 'Open the Viewer panel',
      text: 'The overlay switches and the recorder both live here.',
      reveal: {
        needed: c => !c.viewerPanelOpen,
        anchor: 'sidebar.viewerCta',
        text: 'The Viewer panel is closed — open it here.',
        placement: 'right',
      },
      when: c => c.viewerPanelOpen,
    },
    {
      anchor: 'viewer.movieSection',
      placement: 'left',
      title: 'The Movie block',
      text: 'The gear holds fps, output size, filename, timestamp, scale bar and the title card.',
      bullets: [
        'Trim the time range here if you only want part of the timelapse.',
        'Pick two versions to record them side by side instead.',
      ],
      reveal: {
        needed: c => !c.viewerPanelOpen,
        anchor: 'sidebar.viewerCta',
        text: 'The Viewer panel is closed — open it to reach the Movie block.',
        placement: 'right',
      },
    },
    {
      anchor: 'viewer.record',
      placement: 'left',
      title: 'Record',
      text: 'The render runs as a task, so progress and Cancel are in the task list.',
      bullets: [
        'The mp4 lands in the project\'s movies/ folder.',
        'napari renders one frame at a time — leave it alone while it sweeps.',
      ],
      clickAnchor: true,
    },
    {
      anchor: 'nav:/movies',
      placement: 'right',
      title: 'Where they land',
      text: 'Every movie from any of the three routes shows up on this page, with a player.',
      clickAnchor: true,
    },
    {
      anchor: 'nav:/batch-movies',
      placement: 'right',
      title: 'Now do it for the whole set',
      text: 'Batch movies applies one config to every image you select, instead of one at a time.',
      bullets: ['This is the point of getting the single one right first.'],
      clickAnchor: true,
    },
    {
      anchor: 'images.table',
      route: '/batch-movies',
      placement: 'top-start',
      title: 'Pick the images',
      text: 'Tick every image you want a movie of — one mp4 comes out per image.',
      when: c => c.selection('batchMovies').length > 0,
    },
    {
      anchor: 'batchMovies.generate',
      route: '/batch-movies',
      placement: 'left',
      title: 'Generate',
      text: 'Configure channels, size and the title card in the panel, then record the lot.',
      bullets: [
        'The config is remembered per set, so the next batch starts where you left off.',
        'Each movie is its own task — watch them in the list below.',
      ],
      reveal: {
        needed: c => c.rightPanelCollapsed,
        anchor: 'layout.rightPanelHandle',
        text: 'The movie panel is folded away — open it with this handle.',
        placement: 'left',
      },
      clickAnchor: true,
    },
    {
      title: 'Two more things worth knowing',
      text: 'Movies are rendered by napari, so they cost real time on long timelapses.',
      bullets: [
        'Everything lands in movies/ and is playable on the Movies page.',
        'Want a camera move instead of a straight sweep? See the "Build an animation" guide.',
      ],
    },
  ],
}
