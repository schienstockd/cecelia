<script setup lang="ts">
import { ref, computed, onMounted, watch } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import { useAppControlStore } from '../stores/appControl'
import { useCustomModulesStore } from '../stores/customModules'
import ProjectPanel from './ProjectPanel.vue'
import ConfirmButton from './ConfirmButton.vue'

const projectMeta = useProjectMetaStore()
const settings = useSettingsStore()
// lab-log badge: Cecelia digests colour by severity (⚠️/❌); Claude notes keep the accent tint.
const labLogBadgeStyle = computed(() =>
  settings.labLogUnseenLevel === 'fail' ? { color: 'var(--cc-sev-fail)' }
  : settings.labLogUnseenLevel === 'warn' ? { color: 'var(--cc-sev-warn)' }
  : {})
const appCtl = useAppControlStore()
const customModules = useCustomModulesStore()
const showPanel = ref(false)

// quick app controls in the footer: Quit (everyone) + Restart backend (dev only). Same shared store
// the Settings → System panel uses. Quit is destructive → two-click ConfirmButton (no native dialog).
onMounted(() => { appCtl.refreshDev(); customModules.ensureLoaded() })

// Re-scan custom modules when a project opens — the custom-module nav group needs a project anyway, and
// this way a module dropped after startup appears without a detour through Settings (the old symptom:
// `ensureLoaded` fetched once at boot and never retried). Also refreshes the per-category cohortFuns.
watch(() => projectMeta.current?.uid, uid => { if (uid) customModules.refresh() })

// Track which groups are collapsed (all open by default)
const collapsed = ref<Set<string>>(new Set())
function toggleGroup(key: string) {
  collapsed.value.has(key) ? collapsed.value.delete(key) : collapsed.value.add(key)
  collapsed.value = new Set(collapsed.value)
}
function isOpen(key: string) { return !collapsed.value.has(key) }

interface NavItem {
  to: string
  label: string
  icon: string
  tip: string
  disabled?: boolean
  soon?: boolean
  requiresProject?: boolean
}

// Grouped by pipeline stage, not by "everything is analysis":
//   Data        — get images in and ready (import → segment)
//   Populations — the modules that DEFINE populations (gate / track / cluster cells / cluster tracks)
//   Explore     — modules that USE those populations to explore their properties (phenotype/behaviour/spatial)
//   Analysis    — the only free-form analysis surfaces (board + notebooks)
//   Pipeline    — orchestration (tasks + whiteboard); Settings lives in the footer, not here.
const groups: { heading: string; items: NavItem[] }[] = [
  {
    heading: 'Data',
    items: [
      { to: '/import',   label: 'Import',   icon: 'pi-upload',   tip: 'Import microscopy images into your project.', requiresProject: true },
      { to: '/metadata', label: 'Metadata', icon: 'pi-tag',      tip: 'Edit channel names, colours and other image metadata.', requiresProject: true },
      { to: '/cleanup',  label: 'Cleanup',  icon: 'pi-sparkles', tip: 'Correct and denoise images before segmentation.', requiresProject: true },
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
    ],
  },
  {
    heading: 'Explore',
    items: [
      { to: '/phenotype', label: 'Phenotype', icon: 'pi-percentage', tip: 'Summarise populations — counts / proportion of each population across images.', requiresProject: true },
      { to: '/behaviour', label: 'Behaviour', icon: 'pi-chart-bar',  tip: 'Summary plots of cell/track measures (speed, HMM states, …).', requiresProject: true },
      { to: '/spatial',   label: 'Spatial',   icon: 'pi-map',        tip: 'Spatial neighbourhood, contact and aggregate analysis.', requiresProject: true },
      { to: '/regions',   label: 'Cluster regions', icon: 'pi-clone', tip: 'Cluster cells into spatial regions by neighbourhood composition (i-niches), then define region populations.', requiresProject: true },
    ],
  },
  {
    heading: 'Analysis',
    items: [
      { to: '/analysis',  label: 'Analysis board', icon: 'pi-clone', tip: 'Free-form canvas combining plots across modules, images and segmentations.', requiresProject: true },
      { to: '/notebooks', label: 'Notebooks',      icon: 'pi-book',  tip: 'Pure-Julia downstream analysis in Pluto notebooks (load objects, pop_df, plot, export).', requiresProject: true },
      { to: '/animation', label: 'Animation',      icon: 'pi-video', tip: 'Capture napari view snapshots and record them as movies (channels, populations, colour-by).', requiresProject: true },
      { to: '/batch-movies', label: 'Batch movies', icon: 'pi-images', tip: 'Author one config (channels, overlays, colour-by) and generate a timelapse mp4 for every selected image.', requiresProject: true },
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
const customGroup = computed<{ heading: string; items: NavItem[] } | null>(() => {
  const items = customModules.categories
    .filter(c => !c.builtin)
    .map<NavItem>(c => ({
      to: `/custom/${c.name}`,
      label: prettifyCategory(c.name),
      icon: 'pi-wrench',
      tip: `Custom module: ${c.funNames.join(', ')}`,
      requiresProject: true,
    }))
  return items.length ? { heading: 'Custom', items } : null
})

// static pipeline groups + the dynamic custom-module group (when any new-category modules exist)
const allGroups = computed(() => customGroup.value ? [...groups, customGroup.value] : groups)

function navTip(item: NavItem): string {
  if (item.disabled && item.soon) return `${item.tip} (coming soon)`
  if (item.requiresProject && !projectMeta.hasProject) return 'Open or create a project first.'
  return item.tip
}

function isNavDisabled(item: NavItem): boolean {
  return !!(item.disabled || (item.requiresProject && !projectMeta.hasProject))
}
</script>

<template>
  <nav class="sidebar" v-show="!settings.sidebarCollapsed">

    <!-- ── Project block ───────────────────────────────────────────────── -->
    <div class="project-block">
      <template v-if="projectMeta.current">
        <div class="proj-info">
          <i class="pi pi-folder proj-icon" />
          <div class="proj-text">
            <span class="proj-name" v-tooltip.right="`Project: ${projectMeta.current.name} (${projectMeta.current.type})`">
              {{ projectMeta.current.name }}
            </span>
            <span class="proj-type">{{ projectMeta.current.type }}</span>
          </div>
          <!-- no manual save: the /analysis boards autosave; everything else persists on edit -->
          <button class="proj-menu-btn" @click="showPanel = true"
            v-tooltip.right="'Switch project or create a new one.'">
            <i class="pi pi-ellipsis-h" />
          </button>
        </div>
      </template>
      <template v-else>
        <button class="open-project-btn" @click="showPanel = true"
          v-tooltip.right="'Open or create a project to get started.'">
          <i class="pi pi-folder-open" />
          Manage projects…
        </button>
      </template>
    </div>

    <!-- ── Navigation groups ───────────────────────────────────────────── -->
    <template v-for="group in allGroups" :key="group.heading">
      <button class="group-heading" @click="toggleGroup(group.heading)">
        <span>{{ group.heading }}</span>
        <i :class="['pi', isOpen(group.heading) ? 'pi-chevron-up' : 'pi-chevron-down', 'group-chevron']" />
      </button>

      <template v-if="isOpen(group.heading)">
        <RouterLink
          v-for="item in group.items"
          :key="item.to"
          :to="isNavDisabled(item) ? '' : item.to"
          class="nav-item"
          :class="{ disabled: isNavDisabled(item) }"
          v-tooltip.right="navTip(item)"
          :aria-disabled="isNavDisabled(item)"
        >
          <i :class="['pi', item.icon, 'nav-icon']" />
          <span class="nav-label">{{ item.label }}</span>
          <span v-if="item.soon" class="soon-badge">soon</span>
          <span v-else-if="item.requiresProject && !projectMeta.hasProject" class="lock-badge"
            v-tooltip.right="'Requires an open project.'">
            <i class="pi pi-lock" />
          </span>
        </RouterLink>
      </template>
    </template>

    <!-- ── Viewer ──────────────────────────────────────────────────────────
         The viewer controls are a floating dockable panel (see App.vue / FloatingPanel), not a
         sidebar section. This is a prominent call-to-action button (it drives most napari controls —
         populations, tracks, colour-by — so it must be noticeable), not a dim group heading. -->
    <button class="viewer-cta" :class="{ 'viewer-on': settings.viewerPanelOpen }"
            @click="settings.viewerPanelOpen = !settings.viewerPanelOpen"
            v-tooltip.right="'Napari viewer controls: populations, tracks, colour-by. Floating panel — drag it anywhere.'">
      <i class="pi pi-sliders-h viewer-cta-icon" />
      <span class="viewer-cta-title">Viewer controls</span>
      <i :class="['pi', settings.viewerPanelOpen ? 'pi-eye' : 'pi-eye-slash', 'viewer-cta-state']" />
    </button>

    <!-- ── Lab log ──────────────────────────────────────────────────────────
         Per-project append-only analysis memory (you + Claude). Like the viewer, a floating panel
         toggled here (see App.vue / LabLogPanel). -->
    <button class="viewer-cta lablog-cta" :class="{ 'viewer-on': settings.labLogPanelOpen, 'lablog-unseen': !!settings.labLogUnseen }"
            style="margin-top: 0.4rem"
            @click="settings.labLogPanelOpen = !settings.labLogPanelOpen"
            v-tooltip.right="settings.labLogUnseen
              ? ((settings.labLogUnseenKind === 'cecelia' ? 'Cecelia: ' : 'Claude noted: ') + settings.labLogUnseen)
              : 'Lab log: append-only analysis notes for this project (you + Claude). Floating panel — drag it anywhere.'">
      <i class="pi pi-book viewer-cta-icon" />
      <span class="viewer-cta-title">Lab log</span>
      <!-- badge: Claude (sparkles) or Cecelia (bell, coloured by severity) added something while the
           panel was closed (cleared on open) -->
      <i v-if="settings.labLogUnseen"
         :class="['pi', settings.labLogUnseenKind === 'cecelia' ? 'pi-bell' : 'pi-sparkles', 'lablog-badge']"
         :style="labLogBadgeStyle" />
      <i :class="['pi', settings.labLogPanelOpen ? 'pi-eye' : 'pi-eye-slash', 'viewer-cta-state']" />
    </button>

    <!-- ── Footer: Settings on the left; app controls (quit / restart) on the right ──────────
         Settings is an app preference, not a pipeline step, so it sits apart from the module nav
         and opposite the destructive/lifecycle controls. -->
    <div class="sidebar-footer">
      <RouterLink to="/settings" class="footer-btn"
                  v-tooltip.right="'Settings — project name, ID, and interface preferences'">
        <i class="pi pi-sliders-h" />
      </RouterLink>
      <div class="footer-ctl">
        <ConfirmButton @confirm="appCtl.quit()" v-slot="{ armed, arm, confirm, cancel }">
          <button v-if="!armed" class="footer-btn danger" :disabled="appCtl.busy" @click="arm"
                  v-tooltip.right="'Quit Cecelia — stop napari, notebooks and the backend'">
            <i class="pi pi-power-off" />
          </button>
          <template v-else>
            <button class="footer-btn danger" @click="confirm"
                    v-tooltip.right="'Confirm quit — stops napari, notebooks and the backend'"><i class="pi pi-check" /></button>
            <button class="footer-btn" @click="cancel" v-tooltip.right="'Cancel'"><i class="pi pi-times" /></button>
          </template>
        </ConfirmButton>
        <button v-if="appCtl.dev" class="footer-btn" :disabled="appCtl.busy" @click="appCtl.restartBackend()"
                v-tooltip.right="'Restart the backend server (dev) — reconnects when it is back'">
          <i :class="['pi', appCtl.busy ? 'pi-spin pi-cog' : 'pi-refresh']" />
        </button>
      </div>
    </div>

  </nav>

  <!-- Project panel modal -->
  <ProjectPanel v-if="showPanel" @close="showPanel = false" />
</template>

<style scoped>
.sidebar {
  width: var(--cc-sidebar-w);
  flex-shrink: 0;
  background: var(--cc-surface-1);
  border-right: 1px solid var(--cc-border);
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  padding-bottom: 0.5rem;
}

/* ── Project block ────────────────────────────────────────────────────────── */
.project-block {
  padding: 0.55rem 0.6rem;
  border-bottom: 1px solid var(--cc-border);
  flex-shrink: 0;
}

.proj-info {
  display: flex;
  align-items: center;
  gap: 0.4rem;
  min-width: 0;
}
.proj-icon { font-size: 0.85rem; color: var(--cc-accent); flex-shrink: 0; }
.proj-text { flex: 1; min-width: 0; display: flex; flex-direction: column; }
.proj-name {
  font-size: 0.8rem;
  font-weight: 600;
  color: var(--cc-text);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  cursor: default;
}
.proj-type {
  font-size: 0.65rem;
  color: var(--cc-text-dim);
  text-transform: uppercase;
  letter-spacing: 0.05em;
}
.proj-menu-btn {
  background: none;
  border: none;
  cursor: pointer;
  color: var(--cc-text-dim);
  padding: 0.15rem 0.3rem;
  border-radius: 0.2rem;
  font-size: 0.75rem;
  flex-shrink: 0;
}
.proj-menu-btn:hover { background: var(--cc-surface-2); color: var(--cc-text); }

.open-project-btn {
  width: 100%;
  display: flex;
  align-items: center;
  gap: 0.4rem;
  font-size: 0.78rem;
  font-weight: 500;
  color: var(--cc-accent);
  background: #a78bfa14;
  border: 1px dashed #a78bfa55;
  border-radius: 0.35rem;
  padding: 0.35rem 0.6rem;
  cursor: pointer;
  transition: background 0.12s;
}
.open-project-btn:hover { background: #a78bfa22; }

/* ── Group headings ───────────────────────────────────────────────────────── */
.group-heading {
  display: flex;
  align-items: center;
  justify-content: space-between;
  width: 100%;
  background: none;
  border: none;
  cursor: pointer;
  font-size: 0.65rem;
  font-weight: 600;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: var(--cc-text-dim);
  padding: 0.9rem 0.85rem 0.3rem;
  transition: color 0.1s;
}
.group-heading:hover { color: var(--cc-text); }
.group-chevron { font-size: 0.55rem; opacity: 0.6; }
/* Viewer controls: a prominent call-to-action (it drives most napari controls, so it must stand out
   from the dim nav headings — a bordered, filled button with a title + subtitle). */
.viewer-cta {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  width: calc(100% - 1rem);
  margin: 0.6rem 0.5rem 0.2rem;
  padding: 0.5rem 0.6rem;
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
  border-radius: 0.4rem;
  cursor: pointer;
  color: var(--cc-text);
  text-align: left;
  transition: background 0.1s, border-color 0.1s, color 0.1s;
}
/* GREEN accent (matches the viewer's floating-panel border, --cc-viewer) so the viewer controls read
   as their own distinct thing, apart from the purple form/accent chrome. */
.viewer-cta:hover { border-color: #16a34a; background: #14261a; }
.viewer-cta.viewer-on { background: #0f3d24; border-color: var(--cc-viewer); color: #bbf7d0; }
.viewer-cta-icon { font-size: 0.95rem; color: var(--cc-viewer); flex-shrink: 0; }
.viewer-cta-title { flex: 1; min-width: 0; font-size: 0.78rem; font-weight: 700; }
.viewer-cta-state { font-size: 0.8rem; opacity: 0.75; flex-shrink: 0; }
/* badge: Claude added a lab-log note while the panel was closed */
.lablog-badge { font-size: 0.8rem; color: var(--cc-accent); flex-shrink: 0; margin-left: 0.2rem; }
.lablog-cta.lablog-unseen { border-color: var(--cc-accent); }
/* Lab log CTA: a neutral/whiteish variant so it reads as its own thing, distinct from the coloured
   Viewer control. Overrides the .viewer-cta base (defined above → these win on equal specificity). */
.lablog-cta .viewer-cta-icon { color: #e6edf3; }
.lablog-cta:hover { border-color: rgba(255, 255, 255, 0.55); background: rgba(255, 255, 255, 0.06); }
.lablog-cta.viewer-on {
  background: rgba(255, 255, 255, 0.1);
  border-color: rgba(255, 255, 255, 0.6);
  color: #fff;
}

/* ── Nav items ────────────────────────────────────────────────────────────── */
.nav-item {
  display: flex;
  align-items: center;
  gap: 0.55rem;
  padding: 0.45rem 0.85rem;
  border-radius: 0.35rem;
  margin: 0 0.35rem;
  font-size: 0.82rem;
  font-weight: 500;
  color: var(--cc-text-dim);
  text-decoration: none;
  cursor: pointer;
  transition: background 0.12s, color 0.12s;
  position: relative;
}
.nav-item:hover:not(.disabled) { background: var(--cc-surface-2); color: var(--cc-text); }
.nav-item.router-link-active   { background: var(--cc-surface-2); color: var(--cc-text); }
.nav-item.router-link-active::before {
  content: '';
  position: absolute;
  left: -0.35rem; top: 20%; height: 60%; width: 3px;
  background: var(--cc-accent);
  border-radius: 0 2px 2px 0;
}
.nav-item.disabled { opacity: 0.4; cursor: not-allowed; }

.nav-icon  { font-size: 0.85rem; flex-shrink: 0; }
.nav-label { flex: 1; }

.soon-badge {
  font-size: 0.6rem;
  font-weight: 700;
  padding: 0.05rem 0.3rem;
  border-radius: 0.2rem;
  background: var(--cc-surface-2);
  color: var(--cc-text-dim);
  text-transform: uppercase;
  letter-spacing: 0.05em;
}
.lock-badge { font-size: 0.68rem; color: var(--cc-text-dim); opacity: 0.7; }

/* ── Footer: quick app controls, pinned to the bottom ──────────────────────── */
.sidebar-footer {
  margin-top: auto;                 /* push to the bottom of the flex column */
  display: flex;
  align-items: center;
  justify-content: space-between;   /* Settings on the left, quit/restart cluster on the right */
  gap: 0.4rem;
  padding: 0.5rem 0.6rem 0.2rem;
  border-top: 1px solid var(--cc-border);
}
.footer-ctl { display: flex; gap: 0.4rem; }   /* the right-hand quit + restart group */
/* Settings link active state (RouterLink) — mark it when on /settings, like the nav items */
.footer-btn.router-link-active { color: var(--cc-text); border-color: var(--cc-accent); }
.footer-btn {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
  color: var(--cc-text-dim);
  border-radius: 0.35rem;
  padding: 0.35rem 0.55rem;
  cursor: pointer;
  font-size: 0.85rem;
  text-decoration: none;            /* Settings is a RouterLink (<a>) — no underline */
  transition: background 0.12s, color 0.12s;
}
.footer-btn:hover:not(:disabled) { color: var(--cc-text); background: var(--cc-surface-1); }
.footer-btn.danger:hover:not(:disabled) { color: #fff; background: var(--cc-danger, #ef4444); border-color: var(--cc-danger, #ef4444); }
.footer-btn:disabled { opacity: 0.45; cursor: not-allowed; }

</style>
