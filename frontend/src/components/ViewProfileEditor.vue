<script setup lang="ts">
/*
  The view-profile builder (docs/todo/VIEW_PROFILES_PLAN.md → Decisions 3-4).

  A profile is a curated sidebar: which pages show, in which order. The file under
  <config_dir>/profiles/ is the STORAGE format, not the authoring path — nobody should write JSON to
  get a smaller menu.

  Assembled from canonical primitives, not hand-rolled: `BaseModal` for the shell, and ONE
  `ChipSelect` per sidebar group (`multiple` + `reorderable`), where the selection IS that group's
  pages and the chip order IS their order — so include/exclude and reorder are the same control and
  there is no bespoke drag-and-drop list. Options come from `lib/navGroups.ts`, the same catalogue the
  sidebar renders, so a page can never be offered that does not exist.
*/
import { computed, ref, watch } from 'vue'
import BaseModal from './BaseModal.vue'
import ChipSelect from './ChipSelect.vue'
import ConfirmDeleteButton from './ConfirmDeleteButton.vue'
import { useCustomModulesStore } from '../stores/customModules'
import { useViewProfilesStore, ALL_PROFILE_ID } from '../stores/viewProfiles'
import { allNavGroups } from '../lib/navGroups'
import { availablePaths, unknownPaths, type ViewProfile } from '../utils/viewProfiles'

const emit = defineEmits<{ (e: 'close'): void }>()

const profiles      = useViewProfilesStore()
const customModules = useCustomModulesStore()

const groups = computed(() => allNavGroups(customModules.categories))

// ── the profile being edited ────────────────────────────────────────────────
// `editing` is null for a new profile (no id yet); `picked` holds one path list for the whole profile,
// while each group's ChipSelect reads and writes only its own slice.
const editing = ref<ViewProfile | null>(null)
const label   = ref('')
const picked  = ref<string[]>([])
const error   = ref('')

function load(p: ViewProfile | null) {
  editing.value = p
  label.value   = p?.label ?? ''
  picked.value  = [...(p?.items ?? [])]
  error.value   = ''
}
load(profiles.active)

// Keep the form pointed at a profile that still exists after a save/delete refresh.
watch(() => profiles.profiles, list => {
  if (editing.value && !list.some(p => p.id === editing.value!.id)) load(null)
})

const dirty = computed(() =>
  label.value.trim() !== (editing.value?.label ?? '') ||
  picked.value.join(' ') !== (editing.value?.items ?? []).join(' '))

const canSave = computed(() => !!label.value.trim() && picked.value.length > 0 && !profiles.saving)

// Pages listed by the loaded profile that the app no longer has — dropped on save unless re-picked,
// so say so rather than letting the profile quietly shrink.
const missing = computed(() => unknownPaths(editing.value?.items, availablePaths(groups.value)))

// Deliberately NO per-option `tip`. The nav item's own tip describes what that PAGE does, which is not
// what someone picking pages needs — and ChipSelect renders it as a tooltip on the chip, so it fired on
// top of this control's own tooltip (two tooltips at once, ). One tooltip per control,
// on the control, explaining the interaction: docs/UI.md → Tooltips.
function optionsFor(heading: string) {
  const g = groups.value.find(x => x.heading === heading)
  return (g?.items ?? []).map(i => ({ value: i.to, label: i.label, icon: i.icon }))
}

/** This group's pages, in the profile's order — what its ChipSelect shows as selected. */
function selectedIn(heading: string): string[] {
  const own = new Set(optionsFor(heading).map(o => o.value))
  return picked.value.filter(p => own.has(p))
}

/**
 * Write one group's selection back into the single ordered list. The group's paths are replaced in
 * place — at the position of its first previous entry, or appended — so editing Populations never
 * reshuffles Data.
 */
function setGroup(heading: string, next: string[]) {
  const own = new Set(optionsFor(heading).map(o => o.value))
  const at  = picked.value.findIndex(p => own.has(p))
  const rest = picked.value.filter(p => !own.has(p))
  const insert = at < 0 ? rest.length : rest.filter((_, i) => i < at).length
  picked.value = [...rest.slice(0, insert), ...next, ...rest.slice(insert)]
}

async function save() {
  error.value = ''
  const res = await profiles.save(label.value.trim(), picked.value, editing.value?.id)
  if (res.error) { error.value = res.error; return }
  if (res.profile) {
    load(res.profile)
    profiles.select(res.profile.id)   // editing a profile means you want to be on it
  }
}

async function remove(p: ViewProfile) {
  error.value = ''
  const res = await profiles.remove(p.id)
  if (!res.ok) { error.value = res.error ?? 'Could not delete the profile.'; return }
  if (editing.value?.id === p.id) load(null)
}

function duplicate() {
  // A new id comes from a new label — so "Duplicate" is "same pages, name it yourself".
  editing.value = null
  label.value = label.value ? `${label.value} copy` : ''
}
</script>

<template>
  <BaseModal title="View profiles" icon="pi-eye" width="620px" @close="emit('close')">
    <div class="vp-body" data-guide="viewProfile.editor">

      <!-- existing profiles + the implicit All -->
      <div class="vp-row">
        <span class="vp-lbl cc-eyebrow cc-fs-2xs">profiles</span>
        <div class="vp-list cc-row">
          <button class="cc-btn" :class="{ 'cc-btn-on': editing === null && !label }"
                  @click="load(null); profiles.select(ALL_PROFILE_ID)"
                  v-tooltip.top="'Show every page (the default)'">All pages</button>
          <button v-for="p in profiles.profiles" :key="p.id"
                  class="cc-btn" :class="{ 'cc-btn-on': editing?.id === p.id }"
                  @click="load(p)"
                  v-tooltip.top="`${p.items.length} pages`">{{ p.label }}</button>
        </div>
      </div>

      <!-- name -->
      <div class="vp-row">
        <span class="vp-lbl cc-eyebrow cc-fs-2xs">name</span>
        <input class="vp-input cc-input-xs" v-model="label" placeholder="Gating + behaviour"
               v-tooltip.top="'Shown in the profile picker'" />
        <!-- icons, like every other row action in the app; ConfirmDeleteButton is THE delete
             affordance (arms on the first click, fires on the second) — docs/UI.md → confirm-delete -->
        <button v-if="editing" type="button" class="cc-btn cc-btn-bare cc-btn-icon" @click="duplicate"
                aria-label="Duplicate this profile"
                v-tooltip.top="'Copy these pages into a new profile'">
          <i class="pi pi-copy" />
        </button>
        <ConfirmDeleteButton v-if="editing" title="Delete this profile"
                             armed-title="Click again to delete this profile"
                             @confirm="remove(editing!)" />
      </div>

      <!-- pages, one reorderable ChipSelect per sidebar group -->
      <div class="vp-row vp-row-top">
        <span class="vp-lbl cc-eyebrow cc-fs-2xs">pages</span>
        <div class="vp-groups">
          <div v-for="g in groups" :key="g.heading" class="vp-group">
            <span class="vp-group-name cc-muted cc-fs-xs">{{ g.heading }}</span>
            <ChipSelect :options="optionsFor(g.heading)" :model-value="selectedIn(g.heading)"
                        multiple reorderable select-all
                        :aria-label="`${g.heading} pages in this profile`"
                        v-tooltip.top="'Click to show or hide a page; drag to reorder'"
                        @update:model-value="setGroup(g.heading, $event as string[])" />
          </div>
          <div class="vp-bulk cc-row">
            <span class="cc-muted cc-fs-xs">{{ picked.length }} selected · drag chips to reorder</span>
          </div>
        </div>
      </div>

      <p v-if="missing.length" class="vp-warn cc-fs-xs">
        Not in this app any more: {{ missing.join(', ') }}
      </p>
      <p v-if="error" class="vp-err cc-fs-xs">{{ error }}</p>
    </div>

    <template #footer>
      <span class="cc-uid vp-dir" v-tooltip.top="'Profiles are files you can copy between machines'">
        {{ profiles.dir }}
      </span>
      <button class="cc-btn" @click="emit('close')">Close</button>
      <button class="cc-btn cc-btn-primary" :disabled="!canSave" @click="save"
              v-tooltip.top="'Write this profile and switch to it'">
        {{ profiles.saving ? 'Saving…' : editing ? (dirty ? 'Save' : 'Saved') : 'Create' }}
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
.vp-body { display: flex; flex-direction: column; gap: 0.75rem; }
.vp-row { display: grid; grid-template-columns: 4rem 1fr auto auto; gap: 0.5rem; align-items: center; }
.vp-row-top { align-items: start; }
.vp-lbl { grid-column: 1; padding-top: 0.2rem; }
.vp-list { grid-column: 2 / -1; }
.vp-input { grid-column: 2; }
.vp-groups { grid-column: 2 / -1; display: flex; flex-direction: column; gap: 0.5rem; }
.vp-group { display: flex; flex-direction: column; gap: 0.2rem; }
.vp-bulk { /* geometry only; chrome from .cc-row */ }
.vp-warn { color: var(--cc-sev-warn); }
.vp-err { color: var(--cc-sev-fail); }
.vp-dir { margin-right: auto; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 22rem; }
</style>
