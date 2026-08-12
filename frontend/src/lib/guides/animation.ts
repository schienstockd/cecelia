// "Build an animation" — the third route to a movie, and the one that is genuinely a different job
// from the other two, which is why it was held back from the record-a-movie guide rather than tacked
// on as a coda (Dominik, 2026-08-12).
//
// The distinction that the guide has to land: the Viewer recorder and Batch movies SWEEP the time axis
// with a fixed look, whereas an animation TWEENS between views you captured by hand. A keyframe is a
// whole napari view — camera, channels, populations, and where you are in the timelapse — so an
// animation can move the camera, fade a channel in, and travel through time at once. It therefore does
// NOT need a time series: a keyframed move over a z-stack or a single frame is perfectly valid, which is
// why the prereqs stop at an imported image.
//
// Not a `moduleTaskGuide`: the captures read the live napari viewer, and rendering is the panel's own
// button, not a TaskRunner function.

import type { GuideDef } from './types'
import { PREREQ } from './prereqs'

export const animationGuide: GuideDef = {
  id: 'build-an-animation',
  title: 'Build an animation',
  group: 'Analysis',
  icon: 'pi-images',
  summary: 'Capture napari views as keyframes and tween between them — a camera move, not a time sweep.',
  prereqs: [PREREQ.projectOpen, PREREQ.imageImported],

  steps: [
    {
      anchor: 'nav:/animation',
      placement: 'right',
      title: 'Animation',
      text: 'A recording sweeps time with one fixed look; an animation moves between looks you choose.',
      bullets: [
        'A keyframe holds the camera, the channels, the populations and the timepoint.',
        'So it can pan, fade a channel in and travel through time at once.',
      ],
      clickAnchor: true,
    },
    {
      anchor: 'images.table',
      route: '/animation',
      placement: 'top-start',
      title: 'One image at a time',
      text: 'A timeline belongs to a single image — pick one and its keyframes appear below.',
      when: c => c.selection('animation').length === 1,
    },
    {
      anchor: 'animation.open',
      route: '/animation',
      placement: 'left',
      title: 'Open it in napari',
      text: 'Every capture reads the live viewer, so napari has to be showing the image.',
      bullets: ['This button disappears once it is open.'],
      reveal: {
        needed: c => c.rightPanelCollapsed,
        anchor: 'layout.rightPanelHandle',
        text: 'The animation panel is folded away — open it with this handle.',
        placement: 'left',
      },
      when: c => c.napariImageUid !== null,
    },
    {
      anchor: 'animation.capture',
      route: '/animation',
      placement: 'left',
      title: 'Set the view up, then capture',
      text: 'Arrange it in napari exactly as you want the shot to start — then take the keyframe.',
      bullets: [
        'Camera angle, zoom, channels, contrast, overlays, timepoint: all recorded.',
        'This first one is your base look.',
      ],
      // The timeline only renders once a keyframe exists, so its appearance is the signal.
      when: c => c.anchorExists('animation.timeline'),
    },
    {
      anchor: 'animation.timeline',
      route: '/animation',
      placement: 'top-start',
      title: 'The timeline',
      text: 'Columns are keyframes; rows are the channels, populations and camera read from each one.',
      bullets: [
        'The slider under a column is how many seconds it tweens FROM the previous one.',
        'Drag columns to reorder; an "edited" badge means it differs from what was captured.',
      ],
    },
    {
      anchor: 'animation.addKeyframe',
      route: '/animation',
      placement: 'left',
      title: 'Add the next one',
      text: 'Two ways: duplicate the last keyframe and vary it in the rows, or re-capture from napari.',
      bullets: [
        'Add keyframe — duplicates, for a small change you make in the timeline.',
        'Update selected — replaces a keyframe with the current napari view.',
      ],
      reveal: {
        needed: c => c.rightPanelCollapsed,
        anchor: 'layout.rightPanelHandle',
        text: 'The animation panel is folded away — open it with this handle.',
        placement: 'left',
      },
    },
    {
      anchor: 'animation.render',
      route: '/animation',
      placement: 'left',
      title: 'Render',
      text: 'Needs two keyframes at least — there is nothing to tween between with one.',
      bullets: [
        'fps, output size and the title card are the same controls as the other two routes.',
        'It renders as a task; progress is in the list below.',
      ],
      reveal: {
        needed: c => c.rightPanelCollapsed,
        anchor: 'layout.rightPanelHandle',
        text: 'The animation panel is folded away — open it with this handle.',
        placement: 'left',
      },
      clickAnchor: true,
    },
    {
      anchor: 'nav:/movies',
      placement: 'right',
      title: 'Where it lands',
      text: 'Animations join the recordings and batch movies on the Movies page.',
      bullets: ['Keyframes are saved with the project, so you can come back and adjust.'],
    },
  ],
}
