// "Import images" — the cold-start guide. The only one whose sole prerequisite is an open project,
// so it is what the picker offers when nothing else can run yet (plan D6).
//
// It teaches the two things every later guide assumes: that work is scoped to a SET, and that importing
// is TWO steps — "Add images" registers rows (`POST /api/images/register`), and converting them to
// OME-Zarr is an ordinary task run (`importImages.omezarr`) you dispatch yourself. The first version of
// this guide claimed conversion "starts straight away" and then sent the user to open the image in
// napari, which cannot work: the eye is disabled until the image HAS a converted file (Dominik,
// 2026-08-12) — `isImported`, which is what the table itself uses.
// That convert phase is the shared `taskRunSteps` block, the same furniture every module page uses.

import type { GuideDef } from './types'
import { PREREQ } from './prereqs'
import { isImported } from '../../utils/inclusion'
import { taskRunSteps } from './moduleTask'

export const importImagesGuide: GuideDef = {
  id: 'import-images',
  title: 'Import images',
  group: 'Data',
  icon: 'pi-upload',
  summary: 'Register microscopy files and convert them to OME-Zarr, ready to analyse.',
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
      text: 'Add them to the set.',
      clickAnchor: true,
    },
    {
      anchor: 'images.table',
      route: '/manage-images',
      placement: 'top-start',
      title: 'Added, but not converted yet',
      text: 'Adding files registers a row per image — it does not read the pixels.',
      bullets: [
        'Nothing downstream can use an image until it is converted.',
        'The eye on the left of each row stays disabled until then.',
      ],
      when: c => c.images.length > 0,
    },

    // The convert phase IS a task run, through the same ModuleLayout + TaskRunner furniture as every
    // module page — so it is the shared block, not a seventh hand-written copy of those five steps.
    ...taskRunSteps({
      route: '/manage-images',
      taskKey: 'omezarr',
      funName: 'importImages.omezarr',
      funLabel: 'Convert to OME-ZARR',
      selectionModule: 'manageImages',
      waitLabel: 'Converting',
      withSet: false,                       // the set was chosen four steps ago
      selectTitle: 'Tick what to convert',
      selectText: 'Select the images you just added — one conversion is queued per image.',
      selectHint: ['The flag icon in the Name header selects everything that needs attention.'],
      params: [
        'Pyramid levels — downscaled copies for fast zoomed-out viewing; 2-3 suits a 512-1024 px frame.',
        'Copy to local scratch first — much faster when the source is on a network share.',
        'The defaults are right for most files; you can leave Advanced alone.',
      ],
    }),

    {
      anchor: 'images.table',
      route: '/manage-images',
      placement: 'top-start',
      title: 'Converted',
      text: 'The image now has an OME-Zarr, which is what everything downstream reads.',
      bullets: [
        'The eye is enabled once an image has one.',
        'Big time series take a while — you can keep working while they run.',
        'Nothing appeared? The run\'s log in the task rail says why.',
      ],
      when: c => c.images.some(isImported),
    },
    {
      anchor: 'images.viewerBtn',
      route: '/manage-images',
      placement: 'right',
      title: 'Check it opened correctly',
      text: 'The ↗ opens an image in napari — worth doing once per new file type.',
      bullets: [
        'It stays disabled until that image has converted.',
        'Wrong channel count or pixel size means the metadata needs a look.',
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
