// "Build plots" — the analysis board. Its own shape because a board is a LAYOUT of plot slots rather
// than a single output, and because the page is deliberately read-only: /analysis never mutates gates
// or population definitions (project_analysis_canvas_readonly). The guide has to say so, or a user
// will go looking for the gate tools here and conclude they're missing.

import type { GuideDef } from './types'
import { PREREQ } from './prereqs'

export const plotsGuide: GuideDef = {
  id: 'build-plots',
  title: 'Build plots',
  group: 'Analysis',
  icon: 'pi-chart-bar',
  summary: 'Lay out summary plots across images on a board, then export the figure and its data.',
  prereqs: [PREREQ.projectOpen, PREREQ.segmented],

  steps: [
    {
      anchor: 'nav:/analysis',
      placement: 'right',
      title: 'Analysis board',
      text: 'A board is a grid of plot slots you fill, arrange, and export as one figure.',
      clickAnchor: true,
    },
    {
      anchor: 'images.table',
      route: '/analysis',
      placement: 'top-start',
      title: 'Pick what to plot across',
      text: 'Tick the images the board should summarise — plots pool across your selection.',
      bullets: ['Attributes (treatment, mouse) are what you will group by later.'],
      when: c => c.selection('analysis').length > 0,
    },
    {
      anchor: 'board.addPlot',
      route: '/analysis',
      placement: 'right',
      title: 'Fill a slot',
      text: 'Each empty slot offers every plot your data supports — summary, interactive, clustering.',
      bullets: ['The list is derived from what has actually been run, so it never offers a dead plot.'],
    },
    {
      anchor: 'board.rail',
      route: '/analysis',
      placement: 'left',
      title: 'Choose the populations',
      text: 'The rail on the right picks which populations the active plot draws.',
      bullets: [
        'This page is READ-ONLY — you pick populations here, you do not edit them.',
        'To change a gate, go back to the Gate page.',
      ],
    },
    {
      anchor: 'board.options',
      route: '/analysis',
      placement: 'bottom-end',
      title: 'Plot options',
      text: 'The gear holds how the active plot is drawn — and how it groups your images.',
      bullets: [
        'Compare by an attribute to split the plot by treatment or genotype.',
        'Options are saved with the board.',
      ],
    },
    {
      anchor: 'board.newBoard',
      route: '/analysis',
      placement: 'bottom-start',
      title: 'Boards are cheap',
      text: 'One board per question — they are tabs, they autosave, and they can be duplicated.',
    },
    {
      anchor: 'board.export',
      route: '/analysis',
      placement: 'bottom-end',
      title: 'Export',
      text: 'One control for the figure and the numbers behind it.',
      bullets: [
        'SVG is true vector — editable in Illustrator or Inkscape.',
        'CSV gives you the tidy data, so a figure is never unreproducible.',
      ],
    },
  ],
}
