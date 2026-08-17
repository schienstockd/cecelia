// THE sidebar navigation catalogue — the one list of module pages, their groups and their order.
//
// Extracted from AppSidebar.vue because three surfaces must agree on it: the sidebar renders it, the
// view-profile editor offers it (you can only curate pages that exist), and the guide picker checks a
// guide's pages against it. A second copy of this list is how a page ends up offerable but unroutable.
// Route paths must match `frontend/src/main.ts`; `meta.label` there is the router's own copy.

import type { CustomCategory } from '../stores/customModules'

export interface NavItem {
  to: string
  label: string
  icon: string
  tip: string
  disabled?: boolean
  soon?: boolean
  requiresProject?: boolean
}

export interface NavGroup { heading: string; items: NavItem[] }

// Grouped by pipeline stage, not by "everything is analysis":
//   Data        — get images in and ready (import → segment)
//   Populations — the modules that DEFINE populations (gate / track / cluster cells / cluster tracks)
//   Explore     — modules that USE those populations to explore their properties (phenotype/behaviour/spatial)
//   Analysis    — the only free-form analysis surfaces (board + notebooks)
//   Pipeline    — orchestration (tasks + whiteboard); Settings lives in the footer, not here.
export const NAV_GROUPS: NavGroup[] = [
  {
    heading: 'Data',
    items: [
      { to: '/manage-images', label: 'Manage images', icon: 'pi-upload', tip: 'Add, organise and export images.', requiresProject: true },
      { to: '/metadata', label: 'Metadata', icon: 'pi-tag',      tip: 'Edit channel names, colours and other image metadata.', requiresProject: true },
      { to: '/cleanup',  label: 'Cleanup',  icon: 'pi-sparkles', tip: 'Correct and denoise images before segmentation.', requiresProject: true },
      { to: '/optical-flow', label: 'Optical flow', icon: 'pi-sync', tip: 'Train and manage optical-flow segmentation models.', requiresProject: true },
      { to: '/segment',  label: 'Segment',  icon: 'pi-th-large', tip: 'Run cell segmentation (Cellpose, StarDist, …).', requiresProject: true },
    ],
  },
  {
    heading: 'Populations',
    items: [
      { to: '/gate',    label: 'Gate',    icon: 'pi-chart-scatter', tip: 'FlowJo-style manual gating on segmented populations.', requiresProject: true },
      { to: '/track',   label: 'Track',   icon: 'pi-share-alt',     tip: 'Track segmented or gated cells over time (btrack).', requiresProject: true },
      { to: '/clust-cells',  label: 'Cluster cells',  icon: 'pi-palette', tip: 'Leiden cluster cells (intensities + morphology), then define populations from clusters.', requiresProject: true },
      { to: '/clust-tracks', label: 'Cluster tracks', icon: 'pi-sitemap', tip: 'Leiden cluster tracks (motility + HMM/behaviour), then define populations from clusters.', requiresProject: true },
      // Region clustering DEFINES populations (the `region` pop type) on the same ClusterPlots +
      // PopulationManager as its two siblings above, so it belongs here rather than under Explore.
      { to: '/regions',      label: 'Cluster regions', icon: 'pi-objects-column', tip: 'Cluster cells into spatial regions by neighbourhood composition, then define region populations.', requiresProject: true },
    ],
  },
  {
    heading: 'Explore',
    items: [
      { to: '/phenotype', label: 'Phenotype', icon: 'pi-percentage', tip: 'Summarise populations — counts / proportion of each population across images.', requiresProject: true },
      { to: '/behaviour', label: 'Behaviour', icon: 'pi-chart-bar',  tip: 'Summary plots of cell/track measures (speed, HMM states, …).', requiresProject: true },
      { to: '/spatial',   label: 'Spatial',   icon: 'pi-map',        tip: 'Neighbour graph, cell interactions, contacts and aggregates.', requiresProject: true },
    ],
  },
  {
    heading: 'Analysis',
    items: [
      { to: '/analysis',  label: 'Analysis board', icon: 'pi-clone', tip: 'Free-form canvas combining plots across modules, images and segmentations.', requiresProject: true },
      { to: '/notebooks', label: 'Notebooks',      icon: 'pi-book',  tip: 'Pure-Julia downstream analysis in Pluto notebooks (load objects, pop_df, plot, export).', requiresProject: true },
      { to: '/animation', label: 'Animation',      icon: 'pi-video', tip: 'Capture napari view snapshots and record them as movies (channels, populations, colour-by).', requiresProject: true },
      { to: '/batch-movies', label: 'Batch movies', icon: 'pi-images', tip: 'Author one config (channels, overlays, colour-by) and generate a timelapse mp4 for every selected image.', requiresProject: true },
      { to: '/movies',    label: 'Movies',         icon: 'pi-play-circle', tip: 'Play the movies rendered for this project — native player with adjustable speed and zoom.', requiresProject: true },
    ],
  },
  {
    heading: 'Pipeline',
    items: [
      { to: '/tasks',    label: 'Tasks',      icon: 'pi-list-check', tip: 'View and manage all running and completed analysis tasks.' },
      { to: '/chain',    label: 'Whiteboard', icon: 'pi-cog',        tip: 'Visual chain editor — drag tasks, connect nodes, build pipelines.', requiresProject: true },
    ],
  },
]

// User custom-module categories that have NO built-in page get their own generic page + nav entry
// (docs/CUSTOM_MODULES.md). Tasks in an existing category surface on that category's real page, so
// only `builtin === false` categories appear here. Group is hidden entirely when there are none.
function prettifyCategory(name: string): string {
  const spaced = name.replace(/([a-z0-9])([A-Z])/g, '$1 $2').replace(/[_-]+/g, ' ').trim()
  return spaced.charAt(0).toUpperCase() + spaced.slice(1)
}
export function customNavGroup(categories: CustomCategory[]): NavGroup | null {
  const items = categories
    .filter(c => !c.builtin)
    .map<NavItem>(c => ({
      to: `/custom/${c.name}`,
      label: prettifyCategory(c.name),
      icon: 'pi-wrench',
      tip: `Custom module: ${c.funNames.join(', ')}`,
      requiresProject: true,
    }))
  return items.length ? { heading: 'Custom', items } : null
}

/** The full menu: the static pipeline groups plus the custom-module group when the user has one. */
export function allNavGroups(categories: CustomCategory[]): NavGroup[] {
  const custom = customNavGroup(categories)
  return custom ? [...NAV_GROUPS, custom] : NAV_GROUPS
}

/** A page's menu label, for messages that must name a page rather than show a raw path. */
export function navLabelFor(groups: NavGroup[], path: string): string {
  for (const g of groups) {
    const hit = g.items.find(i => i.to === path)
    if (hit) return hit.label
  }
  return path
}
