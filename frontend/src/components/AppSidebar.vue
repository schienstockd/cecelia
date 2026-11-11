<script setup lang="ts">
import { computed, onMounted, watch } from 'vue'
import { useRoute } from 'vue-router'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import { useAppControlStore } from '../stores/appControl'
import { useCustomModulesStore } from '../stores/customModules'
import { useViewProfilesStore } from '../stores/viewProfiles'
import { allNavGroups, navGroupFor, stageTint, type NavItem } from '../lib/navGroups'
import { applyProfile } from '../utils/viewProfiles'
import { useSingleOpenSection } from '../composables/useSingleOpenSection'
import { useGuideStore } from '../stores/guide'
import { NAV_PREFIX } from '../utils/guideAnchor'
import { pageMaturity } from '../utils/maturity'
import MaturityIndicator from './MaturityIndicator.vue'
import ComingSoonBadge from './ComingSoonBadge.vue'
import { useProjectStore } from '../stores/project'
import { litRoutes } from '../utils/setRecipe'

const projectMeta = useProjectMetaStore()
const settings = useSettingsStore()
const appCtl = useAppControlStore()
const customModules = useCustomModulesStore()
const viewProfiles = useViewProfilesStore()

// `refreshDev` feeds the header's avatar menu (Restart backend is dev only); Quit / Log out live there too.
onMounted(() => { appCtl.refreshDev(); customModules.ensureLoaded(); viewProfiles.ensureLoaded() })

// Re-scan custom modules when a project opens — the custom-module nav group needs a project anyway, and
// this way a module dropped after startup appears without a detour through Settings (the old symptom:
// `ensureLoaded` fetched once at boot and never retried). Also refreshes the per-category cohortFuns.
watch(() => projectMeta.current?.uid, uid => { if (uid) customModules.refresh() })

// Accordion: ONE nav group open at a time (maintainer, 2026-11-11: accordions show one entry by
// default). Same rule + composable as the viewer window's control accordion and Settings. On
// first-ever load, pick the first shown group so the sidebar isn't empty; after that profileStorage
// takes over (including an intentional "all closed" state). The headings are plain buttons, not
// CollapsibleSections, so this script drives the group directly.
const navGroup = useSingleOpenSection('cc.sidebar.openGroup', '')
const { open: openGroup, isOpen, toggle: toggleGroup } = navGroup

// static pipeline groups + the dynamic custom-module group (when any new-category modules exist).
// The catalogue itself lives in lib/navGroups.ts — the view-profile editor and the guide picker read
// the SAME list, so nothing can offer a page this sidebar doesn't have.
const allGroups = computed(() => allNavGroups(customModules.categories))

// The first visible group is the DEFAULT until the user picks one — offered, not written, so it never
// overrides a choice (an explicit "all closed" included) and a later default reaches users who never
// chose. A blank sidebar reads as broken.
watch(() => allGroups.value, gs => { if (gs.length) navGroup.offerDefault(gs[0].heading) }, { immediate: true })

// A guide step pointing at a sidebar link (`nav:/gate`, or the page an off-route step belongs to)
// needs that link on screen: open its group, as arriving on the page would.
const guide = useGuideStore()
watch(() => {
  const s = guide.currentStep
  const nav = s?.anchor?.startsWith(NAV_PREFIX) ? s.anchor.slice(NAV_PREFIX.length) : null
  return guide.offRoute && s?.route ? s.route : nav
}, path => {
  const g = path ? navGroupFor(allGroups.value, path) : null
  if (g && !isOpen(g)) openGroup.value = g
})

// Follow navigation: arriving on a page — by a link elsewhere, a guide, Kiwi pointing at a plot — opens
// its group, else the accordion keeps showing the old one and the menu reads as if nothing changed.
// Only on a route CHANGE, so a group the user closes on the page they're on stays closed.
const route = useRoute()
watch(() => route.path, p => {
  const g = navGroupFor(allGroups.value, p)
  if (g && !isOpen(g)) openGroup.value = g
})

// The page's maturity: the open page carries its full indicator (dot → map row, and "Something
// wrong?"), moved here from the header; other rows show a dot only when results need checking by hand
// (experimental) — a dot on every row would be noise (GUI_CLARITY_PLAN Decision 8 keeps dots, not words).
const handCheck = (to: string) => pageMaturity(to)?.tier === 'experimental'

// …then curated by the active VIEW PROFILE: an ordered subset of the above, so a user doing narrow
// work isn't navigating 20 items they never touch. No profile ⇒ the implicit "All" ⇒ untouched.
// Live-reactive on purpose (`allGroups` is already a computed, and hiding an entry unmounts nothing).
// A hidden page stays reachable by URL — this is decluttering, NOT access control. Filtering logic +
// tests: utils/viewProfiles.ts. See docs/todo/VIEW_PROFILES_PLAN.md.
const shownGroups = computed(() => applyProfile(allGroups.value, viewProfiles.activeItems))
// `--stage` for a group's heading and rows (Decision 15); none for a group with no stage.
const stageStyle = (heading: string): Record<string, string> | undefined => {
  const t = stageTint(heading)
  return t ? { '--stage': t } : undefined
}
// The active set's recipe lights its pages; the rest dim but stay clickable (GUI_CLARITY_PLAN D16).
const project = useProjectStore()
const lit = computed(() => litRoutes(project.activeSet()))

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

    <!-- ── Navigation groups ───────────────────────────────────────────────
         ONLY this region scrolls (flex:1 + overflow); the footer below stays pinned. The project moved
         to the header crumb (ProjectCrumb.vue). Rows are text only (mock frame 1). -->
    <div class="nav-scroll">
      <template v-for="group in shownGroups" :key="group.heading">
        <button class="group-heading" :class="{ staged: stageTint(group.heading) }"
          :style="stageStyle(group.heading)"
          @click="toggleGroup(group.heading)">
          <span>{{ group.heading }}</span>
          <i :class="['pi', isOpen(group.heading) ? 'pi-chevron-up' : 'pi-chevron-down', 'group-chevron']" />
        </button>

        <template v-if="isOpen(group.heading)">
          <RouterLink
            v-for="item in group.items"
            :key="item.to"
            :to="isNavDisabled(item) ? '' : item.to"
            class="nav-item"
            :style="stageStyle(group.heading)"
            :class="{ disabled: isNavDisabled(item), 'off-recipe': lit && !lit.has(item.to) }"
            v-tooltip.right="navTip(item)"
            :aria-disabled="isNavDisabled(item)"
          >
            <span class="nav-label">{{ item.label }}</span>
            <MaturityIndicator v-if="route.path === item.to" class="nav-maturity" :maturity="pageMaturity(item.to)"
                               :page="item.to" @click.stop.prevent />
            <ComingSoonBadge v-else-if="handCheck(item.to)" tier="experimental" dot class="nav-maturity" />
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

    <!-- ── Footer: Settings. An app preference, not a pipeline step, so it sits apart from the module
         nav. Quit / Log out / Restart moved to the header's avatar menu. -->
    <div class="sidebar-footer">
      <!-- An explicit `data-guide`, NOT the `nav:/settings` scheme. That scheme exists because the nav
           groups above are data-driven and so carry no attributes of their own; this is hand-written
           markup and can just say what it is. Addressing it by href made the one guide that points here
           the only one resolving a `nav:` anchor outside `.nav-scroll`. -->
      <RouterLink to="/settings" class="nav-item footer-link"
                  data-guide="sidebar.settings"
                  v-tooltip.right="'Project name, ID, and interface preferences'">
        <span class="nav-label">Settings</span>
      </RouterLink>
    </div>

  </nav>

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

.nav-item.off-recipe:not(.router-link-active) .nav-label { opacity: 0.5; }

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
  padding: 0.75rem 0.85rem 0.25rem;
  transition: color 0.1s;
}
.group-heading:hover { color: var(--cc-text); }
/* a stage's heading carries its colour, softened so the labels stay quiet (Decision 15) */
.group-heading.staged { color: color-mix(in srgb, var(--stage) 75%, var(--cc-text-dim)); }
.group-chevron { font-size: var(--cc-fs-3xs); opacity: 0.6; }

/* ── Nav items ────────────────────────────────────────────────────────────── */
.nav-item {
  display: flex;
  align-items: center;
  gap: 0.55rem;
  padding: 0.22rem 0.85rem;
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
  background: var(--stage, var(--cc-accent));   /* the page's stage colour; lavender off-stage */
  border-radius: 0 2px 2px 0;
}
.nav-item.disabled { opacity: 0.4; cursor: not-allowed; }

.nav-label { flex: 1; }
.nav-maturity { flex-shrink: 0; }

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
  gap: 0.4rem;
  padding: 0.35rem 0.35rem 0;
}
.footer-link { flex: 1; margin: 0; }
</style>
