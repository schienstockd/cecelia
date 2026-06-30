<script setup lang="ts">
import { ref } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import ProjectPanel from './ProjectPanel.vue'
import ViewerPanel from './ViewerPanel.vue'

const projectMeta = useProjectMetaStore()
const showPanel = ref(false)

// Track which groups are collapsed (all open by default, except Dev)
const collapsed = ref<Set<string>>(new Set(['Dev']))
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

const groups: { heading: string; items: NavItem[] }[] = [
  {
    heading: 'Data',
    items: [
      { to: '/import',   label: 'Import',   icon: 'pi-upload',   tip: 'Import microscopy images into your project.', requiresProject: true },
      { to: '/metadata', label: 'Metadata', icon: 'pi-tag',      tip: 'Edit channel names, colours and other image metadata.', requiresProject: true },
      { to: '/cleanup',  label: 'Cleanup',  icon: 'pi-sparkles', tip: 'Correct and denoise images before segmentation.', requiresProject: true },
    ],
  },
  {
    heading: 'Analysis',
    items: [
      { to: '/segment', label: 'Segment', icon: 'pi-th-large',    tip: 'Run cell segmentation (Cellpose, StarDist, …).', requiresProject: true },
      { to: '/gate',    label: 'Gate',    icon: 'pi-chart-scatter',tip: 'FlowJo-style manual gating on segmented populations.', requiresProject: true },
      { to: '/track',   label: 'Track',   icon: 'pi-share-alt',   tip: 'Track segmented or gated cells over time (btrack).', requiresProject: true },
      { to: '/behaviour', label: 'Behaviour', icon: 'pi-chart-bar', tip: 'Summary plots of cell/track measures (speed, HMM states, …).', requiresProject: true },
      { to: '/clust-cells',  label: 'Cluster cells',  icon: 'pi-palette', tip: 'Leiden cluster cells (intensities + morphology), then define populations from clusters.', requiresProject: true },
      { to: '/clust-tracks', label: 'Cluster tracks', icon: 'pi-sitemap',      tip: 'Leiden cluster tracks (motility + HMM/behaviour), then define populations from clusters.', requiresProject: true },
      { to: '/spatial', label: 'Spatial', icon: 'pi-map',          tip: 'Spatial neighbourhood and proximity analysis.', disabled: true, soon: true },
      { to: '/analysis', label: 'Analysis canvas', icon: 'pi-clone', tip: 'Free-form canvas combining plots across modules, images and segmentations.', disabled: true, soon: true },
    ],
  },
  {
    heading: 'Pipeline',
    items: [
      { to: '/tasks',    label: 'Tasks',      icon: 'pi-list-check', tip: 'View and manage all running and completed analysis tasks.' },
      { to: '/chain',    label: 'Whiteboard', icon: 'pi-cog',        tip: 'Visual chain editor — drag tasks, connect nodes, build pipelines.', requiresProject: true },
      { to: '/settings', label: 'Settings',   icon: 'pi-sliders-h',  tip: 'Project name, ID, and interface preferences.' },
    ],
  },
  {
    heading: 'Dev',
    items: [
      { to: '/test', label: 'Test Tasks', icon: 'pi-flask', tip: 'Run lightweight mock tasks to debug the task system.', requiresProject: true },
    ],
  },
]

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
  <nav class="sidebar">

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
          <button class="proj-menu-btn" @click="projectMeta.saveProject()"
            v-tooltip.right="'Save the current project state to disk.'">
            <i class="pi pi-save" />
          </button>
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
    <template v-for="group in groups" :key="group.heading">
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

    <!-- ── Viewer ──────────────────────────────────────────────────────── -->
    <button class="group-heading" @click="toggleGroup('Viewer')">
      <span>Viewer</span>
      <i :class="['pi', isOpen('Viewer') ? 'pi-chevron-up' : 'pi-chevron-down', 'group-chevron']" />
    </button>
    <ViewerPanel v-if="isOpen('Viewer')" />

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

</style>
