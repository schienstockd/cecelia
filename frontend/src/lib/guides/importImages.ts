// "Import images" — the cold-start guide. The only one whose sole prerequisite is an open project,
// so it is what the picker offers when nothing else can run yet (plan D6).
//
// It also teaches the two things every later guide assumes: that work is scoped to a SET, and that
// import converts to OME-Zarr in the background rather than instantly.

import type { GuideDef } from './types'
import { PREREQ } from './prereqs'

export const importImagesGuide: GuideDef = {
  id: 'import-images',
  title: 'Import images',
  group: 'Data',
  icon: 'pi-upload',
  summary: 'Get microscopy files into a project as OME-Zarr, ready to analyse.',
  prereqs: [PREREQ.projectOpen],

  steps: [
    {
      anchor: 'sidebar.projectBlock',
      placement: 'right',
      title: 'Everything lives in a project',
      text: 'A project holds your images and every result derived from them.',
      bullets: ['Use the ⋯ menu here to switch projects or make a new one.'],
      when: c => c.hasProject,
    },
    {
      anchor: 'nav:/manage-images',
      placement: 'right',
      title: 'Manage images',
      text: 'Adding, moving and deleting images all happen on this one page.',
      clickAnchor: true,
    },
    {
      anchor: 'set.select',
      route: '/manage-images',
      placement: 'bottom-start',
      title: 'Pick a set first',
      text: 'A set groups images you want to treat together — a condition, a day, an experiment.',
      bullets: ['Every function runs against the active set.'],
      reveal: {
        // With no sets there is nothing to select — a "pick a set" bubble pointing at an empty
        // dropdown is a dead end, so point at "New set" until one exists (Dominik, 2026-08-12).
        needed: c => c.setCount === 0,
        anchor: 'set.new',
        text: 'No sets yet — create one first. A set groups the images you treat together.',
        placement: 'bottom-start',
      },
      when: c => c.setUid !== null,
    },
    {
      anchor: 'manageImages.addImages',
      route: '/manage-images',
      placement: 'bottom-start',
      title: 'Add images',
      text: 'Browse to your microscopy files — CZI, LIF, ND2, OME-TIFF and friends.',
      clickAnchor: true,
    },
    {
      // The select-all checkbox, NOT the whole table: ringing a large scrolling container drags the
      // highlight around as you scroll it, and says "somewhere in here" rather than pointing at a
      // control (Dominik, 2026-08-12).
      anchor: 'filebrowser.selectAll',
      route: '/manage-images',
      placement: 'right',
      title: 'Choose the files',
      text: 'Tick the images you want; unsupported file types are greyed out.',
      bullets: ['This header checkbox takes every image in the folder at once.'],
    },
    {
      anchor: 'filebrowser.confirm',
      route: '/manage-images',
      placement: 'top-end',
      text: 'Add them to the set — conversion starts straight away.',
      clickAnchor: true,
    },
    {
      anchor: 'images.table',
      route: '/manage-images',
      placement: 'top-start',
      title: 'Conversion runs in the background',
      text: 'Each image is converted to OME-Zarr, which is what everything downstream reads.',
      bullets: [
        'A row appears immediately; it is only usable once it reads "done".',
        'Big time series take a while — you can keep working.',
      ],
      when: c => c.images.length > 0,
    },
    {
      anchor: 'images.viewerBtn',
      route: '/manage-images',
      placement: 'right',
      title: 'Check it opened correctly',
      text: 'The eye opens an image in the napari viewer — worth doing once per new file type.',
      bullets: [
        'Wrong channel count or pixel size means the metadata needs a look.',
        'Fix those on the Metadata page before segmenting.',
      ],
    },
    {
      text: 'That is import done — your set is ready to clean up or segment.',
      title: 'Ready',
      bullets: [
        'Odd pixel sizes or channel names? Metadata page.',
        'Drifting time series? Cleanup page.',
        'Otherwise go straight to Segment.',
      ],
    },
  ],
}
