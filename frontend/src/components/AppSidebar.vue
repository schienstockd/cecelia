<script setup lang="ts">
import { ref, computed, onMounted, watch } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import { useAppControlStore } from '../stores/appControl'
import { useCustomModulesStore } from '../stores/customModules'
import { useViewProfilesStore } from '../stores/viewProfiles'
import { allNavGroups, type NavItem } from '../lib/navGroups'
import { applyProfile } from '../utils/viewProfiles'
import { runningTaskCount } from '../utils/runningTasks'
import { quitConfirmTooltip } from '../utils/quitWarning'
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
// Quit says what it will kill: shutdown exits the backend without waiting for in-flight work, so a
// long segmentation run would otherwise vanish on a click the user thought was harmless. The count is
// read from the backend when the button is ARMED (not on hover) — one request per intent to quit.
const quitTasks   = ref(0)
const quitConfirm = computed(() => quitConfirmTooltip(quitTasks.value))
async function armQuit(arm: () => void) {
  arm()                                        // arm first: the confirm must appear immediately
  quitTasks.value = await runningTaskCount()   // then fill in what it will cost
}
const customModules = useCustomModulesStore()
const viewProfiles = useViewProfilesStore()
const showPanel = ref(false)

// quick app controls in the footer: Quit (everyone) + Restart backend (dev only). Same shared store
// the Settings → System panel uses. Quit is destructive → two-click ConfirmButton (no native dialog).
onMounted(() => { appCtl.refreshDev(); customModules.ensureLoaded(); viewProfiles.ensureLoaded() })

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

// static pipeline groups + the dynamic custom-module group (when any new-category modules exist).
// The catalogue itself lives in lib/navGroups.ts — the view-profile editor and the guide picker read
// the SAME list, so nothing can offer a page this sidebar doesn't have.
const allGroups = computed(() => allNavGroups(customModules.categories))

// …then curated by the active VIEW PROFILE: an ordered subset of the above, so a user doing narrow
// work isn't navigating 20 items they never touch. No profile ⇒ the implicit "All" ⇒ untouched.
// Live-reactive on purpose (`allGroups` is already a computed, and hiding an entry unmounts nothing).
// A hidden page stays reachable by URL — this is decluttering, NOT access control. Filtering logic +
// tests: utils/viewProfiles.ts. See docs/todo/VIEW_PROFILES_PLAN.md.
const shownGroups = computed(() => applyProfile(allGroups.value, viewProfiles.activeItems))

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
    <div class="project-block" data-guide="sidebar.projectBlock">
      <template v-if="projectMeta.current">
        <div class="proj-info">
          <i class="pi pi-folder proj-icon" />
          <div class="proj-text">
            <span class="proj-name" v-tooltip.right="`Project: ${projectMeta.current.name}`">
              {{ projectMeta.current.name }}
            </span>
          </div>
          <!-- no manual save: the /analysis boards autosave; everything else persists on edit -->
          <button class="proj-menu-btn cc-btn cc-btn-bare cc-btn-icon" @click="showPanel = true"
            v-tooltip.right="'Switch project or create a new one'">
            <i class="pi pi-ellipsis-h" />
          </button>
        </div>
        <!-- Only when a profile is actually curating the menu: "All pages" is the default, and a badge
             for the default state is noise on every screen forever. Its own row BELOW `.proj-info`,
             never a second line inside it — that row centres the folder icon and the ⋯ button against
             the name, so growing it pushed the name up and both controls down (Dominik, 2026-08-17). -->
        <span v-if="viewProfiles.active" class="profile-badge"
              v-tooltip.right="'View profile — change in Settings → Interface'">
          <i class="pi pi-eye" />{{ viewProfiles.active.label }}
        </span>
      </template>
      <template v-else>
        <button class="open-project-btn" @click="showPanel = true"
          v-tooltip.right="'Open or create a project to get started'">
          <i class="pi pi-folder-open" />
          Manage projects…
        </button>
      </template>
    </div>

    <!-- ── Navigation groups ───────────────────────────────────────────────
         ONLY this region scrolls (flex:1 + overflow). The project block above and the viewer/lab-log
         CTAs + footer below stay pinned, so a long menu never pushes them out of reach. -->
    <div class="nav-scroll">
      <template v-for="group in shownGroups" :key="group.heading">
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
            <!-- no tip of its own: the row's navTip already says "Open or create a project first."
                 in exactly this state, and a second one inside it fired on top of it -->
            <span v-else-if="item.requiresProject && !projectMeta.hasProject" class="lock-badge cc-muted cc-fs-xs">
              <i class="pi pi-lock" />
            </span>
          </RouterLink>
        </template>
      </template>
    </div>

    <!-- ── Viewer ──────────────────────────────────────────────────────────
         The viewer controls are a floating dockable panel (see App.vue / FloatingPanel), not a
         sidebar section. This is a prominent call-to-action button (it drives most napari controls —
         populations, tracks, colour-by — so it must be noticeable), not a dim group heading. -->
    <button class="viewer-cta" data-guide="sidebar.viewerCta" :class="{ 'viewer-on': settings.viewerPanelOpen }"
            @click="settings.viewerPanelOpen = !settings.viewerPanelOpen"
            v-tooltip.right="'Napari viewer controls: populations, tracks, colour-by'">
      <i class="pi pi-sliders-h viewer-cta-icon" />
      <span class="viewer-cta-title">Viewer controls</span>
      <i :class="['pi', settings.viewerPanelOpen ? 'pi-eye' : 'pi-eye-slash', 'viewer-cta-state']" />
    </button>

    <!-- ── Lab log ──────────────────────────────────────────────────────────
         Per-project append-only analysis memory (you + Claude). Like the viewer, a floating panel
         toggled here (see App.vue / LabLogPanel). -->
    <button class="viewer-cta lablog-cta" data-guide="sidebar.labLogCta"
            :class="{ 'viewer-on': settings.labLogPanelOpen, 'lablog-unseen': !!settings.labLogUnseen }"
            style="margin-top: 0.4rem"
            @click="settings.labLogPanelOpen = !settings.labLogPanelOpen"
            v-tooltip.right="settings.labLogUnseen
              ? ((settings.labLogUnseenKind === 'cecelia' ? 'Cecelia: ' : 'Claude noted: ') + settings.labLogUnseen)
              : 'Lab log — analysis notes for this project (you + Claude)'">
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
      <!-- An explicit `data-guide`, NOT the `nav:/settings` scheme. That scheme exists because the nav
           groups above are data-driven and so carry no attributes of their own; this is hand-written
           markup and can just say what it is. Addressing it by href made the one guide that points here
           the only one resolving a `nav:` anchor outside `.nav-scroll`. -->
      <RouterLink to="/settings" class="footer-btn cc-btn cc-btn-ghost cc-btn-icon cc-btn-lg"
                  data-guide="sidebar.settings"
                  v-tooltip.right="'Settings — project name, ID, and interface preferences'">
        <i class="pi pi-cog" />
      </RouterLink>
      <div class="footer-ctl">
        <ConfirmButton @confirm="appCtl.quit()" v-slot="{ armed, arm, confirm, cancel }">
          <!-- Anchored for the orientation tour, which points here to say "close the tab and the
               backend keeps running" — the fact the `cc.hint.shutdown` callout used to carry. Only the
               UNARMED button takes the anchor: arming swaps the node, and a tour step must never
               invite this particular click. -->
          <button v-if="!armed" class="footer-btn cc-btn cc-btn-ghost cc-btn-icon cc-btn-lg danger" data-guide="sidebar.quit"
                  :disabled="appCtl.busy" @click="armQuit(arm)"
                  v-tooltip.right="'Quit Cecelia — stop napari, notebooks and the backend'">
            <i class="pi pi-power-off" />
          </button>
          <template v-else>
            <button class="footer-btn cc-btn cc-btn-ghost cc-btn-icon cc-btn-lg danger" @click="confirm"
                    v-tooltip.right="quitConfirm"><i class="pi pi-check" /></button>
            <button class="footer-btn cc-btn cc-btn-ghost cc-btn-icon cc-btn-lg" @click="cancel" v-tooltip.right="'Cancel'"><i class="pi pi-times" /></button>
          </template>
        </ConfirmButton>
        <button v-if="appCtl.dev" class="footer-btn cc-btn cc-btn-ghost cc-btn-icon cc-btn-lg" :disabled="appCtl.busy" @click="appCtl.restartBackend()"
                v-tooltip.right="'Restart the backend server (dev) — reconnects when it is back'">
          <i :class="['pi', appCtl.busy ? 'pi-spin pi-spinner' : 'pi-refresh']" />
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
  overflow: hidden;               /* only .nav-scroll scrolls — the rest stays pinned */
  display: flex;
  flex-direction: column;
  padding-bottom: 0.5rem;
}

/* the scrollable middle: absorbs all spare height so the viewer/lab-log CTAs + footer sit at the
   bottom, and scrolls on its own when the menu is long (project block above stays pinned too) */
.nav-scroll {
  flex: 1 1 auto;
  min-height: 0;
  overflow-y: auto;
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
.proj-icon { font-size: var(--cc-fs-md); color: var(--cc-accent); flex-shrink: 0; }
.proj-text { flex: 1; min-width: 0; display: flex; flex-direction: column; }
/* The third badge in this file (`.soon-badge`, `.lock-badge` below), same shape. Badges are
   deliberately NOT unified app-wide — a badge, a chip and a card are all surface + border + radius, so
   the class name is the only thing carrying intent (docs/todo/UX_PRIMITIVES_PLAN.md, principle 7).
   Not uppercased, unlike `.soon-badge`: this shows a name the user typed. */
.profile-badge {
  /* Indented to start under the project NAME rather than under the folder icon — the icon's own width
     plus `.proj-info`'s gap. Tied to `.proj-icon`'s font-size; change one, change both. */
  margin: 0.25rem 0 0 calc(var(--cc-fs-md) + 0.4rem);
  max-width: calc(100% - var(--cc-fs-md) - 0.4rem);
  display: inline-flex;
  align-items: center;
  gap: 0.25rem;
  font-size: var(--cc-fs-2xs);
  padding: 0.05rem 0.3rem;
  border-radius: var(--cc-radius-xs);
  background: var(--cc-surface-2);
  color: var(--cc-text-dim);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  cursor: default;
}
.proj-name {
  font-size: var(--cc-fs-md);
  font-weight: 600;
  color: var(--cc-text);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  cursor: default;
}

/* .proj-menu-btn → cc-btn cc-btn-bare cc-btn-icon */
.proj-menu-btn:hover { background: var(--cc-surface-2); color: var(--cc-text); }

.open-project-btn {
  width: 100%;
  display: flex;
  align-items: center;
  gap: 0.4rem;
  font-size: var(--cc-fs-sm);
  font-weight: 500;
  color: var(--cc-accent);
  background: #a78bfa14;
  border: 1px dashed #a78bfa55;
  border-radius: var(--cc-radius-sm);
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
  font-size: var(--cc-fs-2xs);
  font-weight: 600;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: var(--cc-text-dim);
  padding: 0.9rem 0.85rem 0.3rem;
  transition: color 0.1s;
}
.group-heading:hover { color: var(--cc-text); }
.group-chevron { font-size: var(--cc-fs-3xs); opacity: 0.6; }
/* Viewer controls: a prominent call-to-action (it drives most napari controls, so it must stand out
   from the dim nav headings — a bordered, filled button with a title + subtitle). */
.viewer-cta {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  flex-shrink: 0;                 /* pinned below the scroll region — never squeezed */
  width: calc(100% - 1rem);
  margin: 0.6rem 0.5rem 0.2rem;
  padding: 0.5rem 0.6rem;
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-md);
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
.viewer-cta-title { flex: 1; min-width: 0; font-size: var(--cc-fs-sm); font-weight: 700; }
.viewer-cta-state { font-size: var(--cc-fs-md); opacity: 0.75; flex-shrink: 0; }
/* badge: Claude added a lab-log note while the panel was closed */
.lablog-badge { font-size: var(--cc-fs-md); color: var(--cc-accent); flex-shrink: 0; margin-left: 0.2rem; }
.lablog-cta.lablog-unseen { border-color: var(--cc-accent); }
/* Lab log CTA: a neutral/whiteish variant so it reads as its own thing, distinct from the coloured
   Viewer control. Overrides the .viewer-cta base (defined above → these win on equal specificity). */
.lablog-cta .viewer-cta-icon { color: var(--cc-text); }
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
  border-radius: var(--cc-radius-sm);
  margin: 0 0.35rem;
  font-size: var(--cc-fs-md);
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

.nav-icon  { font-size: var(--cc-fs-md); flex-shrink: 0; }
.nav-label { flex: 1; }

.soon-badge {
  font-size: var(--cc-fs-2xs);
  font-weight: 700;
  padding: 0.05rem 0.3rem;
  border-radius: var(--cc-radius-xs);
  background: var(--cc-surface-2);
  color: var(--cc-text-dim);
  text-transform: uppercase;
  letter-spacing: 0.05em;
}
.lock-badge { opacity: 0.7; }

/* ── Footer: quick app controls, pinned to the bottom ──────────────────────── */
.sidebar-footer {
  margin-top: auto;                 /* push to the bottom of the flex column */
  flex-shrink: 0;                   /* pinned below the scroll region — never squeezed */
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
.footer-btn { text-decoration: none; /* Settings is a RouterLink (<a>) — no underline */
  transition: background 0.12s, color 0.12s; }   /* + cc-btn cc-btn-ghost cc-btn-icon cc-btn-lg */
.footer-btn:hover:not(:disabled) { color: var(--cc-text); background: var(--cc-surface-1); }
.footer-btn.danger:hover:not(:disabled) { color: #fff; background: var(--cc-danger); border-color: var(--cc-danger); }
.footer-btn:disabled { opacity: 0.45; cursor: not-allowed; }

</style>
