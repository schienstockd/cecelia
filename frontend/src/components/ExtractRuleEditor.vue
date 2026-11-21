<script setup lang="ts">
/*
  How one attribute's value is read off an image's file name or original path: a regex, built without
  writing regex — split by a separator and take a field, or the part after / before a marker — with a
  live preview on a sample image. The regex is the stored result (`utils/regexBuilder.ts`); the builder
  settings ride alongside (`spec`) so a rule reopens as the builder.

  Two sizes. Full (the Metadata panel): the regex field with the caller's Apply in `#action`, the
  preview, the source, and the builder behind a toggle. Compact (a rule row in the experiment design
  dialog): source and builder on one line with the value it gives; the regex behind a toggle.
*/
import { computed, ref } from 'vue'
import CcToggle from './CcToggle.vue'
import {
  buildRegex, defaultSpec, extractWith, regexSampleFor,
  type BuilderSpec, type CtxClass, type ExtractKind, type FieldPos, type RegexSource, type SepChoice,
} from '../utils/regexBuilder'

const props = defineProps<{
  regex: string
  source: RegexSource
  spec?: BuilderSpec | null
  /** the image the preview runs on */
  sample?: { name: string; oriPath?: string | null } | null
  compact?: boolean
  disabled?: boolean
}>()
const emit = defineEmits<{
  (e: 'update:regex', v: string): void
  (e: 'update:source', v: RegexSource): void
  (e: 'update:spec', v: BuilderSpec): void
}>()

// the builder's settings: the caller's when it keeps them, else this editor's own
const ownSpec = ref<BuilderSpec>(defaultSpec())
const s = computed<BuilderSpec>(() => props.spec ?? ownSpec.value)
function setSpec(next: BuilderSpec) {
  ownSpec.value = next
  emit('update:spec', next)
  const r = buildRegex(next)
  if (r) emit('update:regex', r)
}
// one field of the current spec changed (only the fields of its own mode are ever offered)
function patch(p: Record<string, unknown>) { setSpec({ ...s.value, ...p } as BuilderSpec) }
function setMode(mode: BuilderSpec['mode']) { if (mode !== s.value.mode) setSpec(defaultSpec(mode)) }

const builderOpen = ref(false)
const regexOpen = ref(false)

const sampleText = computed(() => (props.sample ? regexSampleFor(props.sample, props.source) : ''))
const preview = computed(() => extractWith(props.regex, sampleText.value))

const SEPS: { value: SepChoice; label: string }[] = [
  { value: '-', label: '- dash' }, { value: '_', label: '_ underscore' }, { value: '.', label: '. dot' },
  { value: 'space', label: '␣ space' }, { value: 'folder', label: '/ folder' }, { value: 'custom', label: 'custom…' },
]
const POSITIONS: { value: FieldPos; label: string }[] = [
  { value: 'first', label: '1st field' }, { value: 'second', label: '2nd field' }, { value: 'third', label: '3rd field' },
  { value: 'thirdLast', label: '3rd-last field' }, { value: 'secondLast', label: '2nd-last field' }, { value: 'last', label: 'last field' },
]
const CLASSES: { value: CtxClass; label: string }[] = [
  { value: 'none', label: '—' }, { value: 'digits', label: '+ number' }, { value: 'letters', label: '+ letters' },
  { value: 'lower', label: '+ lowercase' }, { value: 'upper', label: '+ uppercase' },
]
const KINDS: { value: ExtractKind; label: string }[] = [
  { value: 'digits', label: 'a number' }, { value: 'letters', label: 'letters' }, { value: 'lower', label: 'lowercase' },
  { value: 'upper', label: 'uppercase' }, { value: 'word', label: 'word' }, { value: 'custom', label: 'custom…' },
]
const val = (e: Event) => (e.target as HTMLInputElement | HTMLSelectElement).value
</script>

<template>
  <!-- ── compact: one line ───────────────────────────────────────────────────────── -->
  <div v-if="compact" class="xr-compact" :class="{ 'xr-disabled': disabled }">
    <div class="cc-row">
      <select :value="source" :disabled="disabled" v-tooltip.bottom="'What the value is read from'"
              @change="emit('update:source', val($event) as RegexSource)">
        <option value="name">File name</option>
        <option value="path">Original path</option>
      </select>
      <select :value="s.mode" :disabled="disabled" v-tooltip.bottom="'Split into fields, or next to a marker'"
              @change="setMode(val($event) as BuilderSpec['mode'])">
        <option value="field">split by</option>
        <option value="around">after</option>
      </select>
      <template v-if="s.mode === 'field'">
        <select :value="s.sep" :disabled="disabled" v-tooltip.bottom="'Character it is split on'"
                @change="patch({ sep: val($event) })">
          <option v-for="o in SEPS" :key="o.value" :value="o.value">{{ o.label }}</option>
        </select>
        <input v-if="s.sep === 'custom'" class="xr-sep" :value="s.customSep" maxlength="4" placeholder="sep"
               :disabled="disabled" v-tooltip.bottom="'Your own separator'" @input="patch({ customSep: val($event) })" />
        <select :value="s.pos" :disabled="disabled" v-tooltip.bottom="'Which field to keep'"
                @change="patch({ pos: val($event) })">
          <option v-for="o in POSITIONS" :key="o.value" :value="o.value">{{ o.label }}</option>
        </select>
      </template>
      <template v-else>
        <input class="xr-text" :value="s.before.text" placeholder="e.g. M" :disabled="disabled"
               v-tooltip.bottom="'Text just before the value'"
               @input="patch({ before: { ...s.before, text: val($event) } })" />
        <select :value="s.extract.kind" :disabled="disabled" v-tooltip.bottom="'Kind of value to take'"
                @change="patch({ extract: { ...s.extract, kind: val($event) } })">
          <option v-for="o in KINDS.filter(k => k.value !== 'custom')" :key="o.value" :value="o.value">{{ o.label }}</option>
        </select>
      </template>
      <span class="xr-arrow cc-muted">→</span>
      <span class="xr-value" v-tooltip.bottom="sampleText || 'No sample'">{{ preview || 'no match' }}</span>
      <button class="cc-btn cc-btn-bare cc-btn-icon" :class="{ 'cc-btn-on': regexOpen }" :disabled="disabled"
              v-tooltip.bottom="'Show the regex'" @click="regexOpen = !regexOpen">
        <i class="pi pi-code" />
      </button>
    </div>
    <input v-if="regexOpen" class="xr-regex" :value="regex" :disabled="disabled" placeholder="Regex"
           v-tooltip.bottom="'First (group), else the whole match'" @input="emit('update:regex', val($event))" />
  </div>

  <!-- ── full: the Metadata panel ────────────────────────────────────────────────── -->
  <div v-else class="xr-full" :class="{ 'xr-disabled': disabled }">
    <div class="cc-row">
      <input class="xr-regex xr-grow" :value="regex" placeholder="Regular expression…" :disabled="disabled"
             v-tooltip.bottom="'Regex over the name; a (group) if present, e.g. M(\\d+)→4'"
             @input="emit('update:regex', val($event))" />
      <slot name="action" />
    </div>

    <div v-if="sampleText && regex" class="xr-preview">
      <span class="xr-src cc-muted" v-tooltip.bottom="sampleText">{{ sampleText }}</span>
      <span class="cc-muted">→</span>
      <span class="xr-value">{{ preview || '(no match)' }}</span>
    </div>

    <div class="cc-row">
      <label class="xr-radio cc-muted">
        <input type="radio" :checked="source === 'name'" :disabled="disabled"
               v-tooltip.bottom="'Match against the file name only'" @change="emit('update:source', 'name')" />
        Filename
      </label>
      <label class="xr-radio cc-muted">
        <input type="radio" :checked="source === 'path'" :disabled="disabled"
               v-tooltip.bottom="'Match against the full path of the imported file'" @change="emit('update:source', 'path')" />
        Original path
      </label>
    </div>

    <button class="xr-toggle cc-section-toggle" :disabled="disabled" @click="builderOpen = !builderOpen"
            v-tooltip.bottom="'Build the pattern above without writing regex — select a separator and field'">
      <i :class="['pi', builderOpen ? 'pi-chevron-down' : 'pi-chevron-right']" /> Builder
    </button>
    <div v-if="builderOpen" class="xr-builder">
      <div class="cc-row">
        <span class="xr-label cc-muted">Mode</span>
        <select class="xr-grow" :value="s.mode" :disabled="disabled"
                v-tooltip.bottom="'Split the name into fields, or extract around a marker'"
                @change="setMode(val($event) as BuilderSpec['mode'])">
          <option value="field">Split into fields</option>
          <option value="around">Around a marker</option>
        </select>
      </div>
      <template v-if="s.mode === 'field'">
        <div class="cc-row">
          <span class="xr-label cc-muted">Split by</span>
          <select class="xr-grow" :value="s.sep" :disabled="disabled" v-tooltip.bottom="'Character the name is split on'"
                  @change="patch({ sep: val($event) })">
            <option v-for="o in SEPS" :key="o.value" :value="o.value">{{ o.label }}</option>
          </select>
          <input v-if="s.sep === 'custom'" class="xr-sep" :value="s.customSep" maxlength="4" placeholder="sep"
                 :disabled="disabled" v-tooltip.bottom="'Your own separator character'" @input="patch({ customSep: val($event) })" />
        </div>
        <div class="cc-row">
          <span class="xr-label cc-muted">Take</span>
          <select class="xr-grow" :value="s.pos" :disabled="disabled" v-tooltip.bottom="'Which field of the split name to keep'"
                  @change="patch({ pos: val($event) })">
            <option v-for="o in POSITIONS" :key="o.value" :value="o.value">{{ o.label }}</option>
          </select>
          <!-- only the last field can carry the file extension — see buildFieldRegex -->
          <CcToggle v-if="s.pos === 'last'" label="no ext" :disabled="disabled" :model-value="s.stripExt"
                    v-tooltip.bottom="'Drop a trailing .extension from the captured value'"
                    @update:model-value="patch({ stripExt: $event })" />
        </div>
      </template>
      <template v-else>
        <div class="cc-row">
          <span class="xr-label cc-muted">Preceded by</span>
          <input class="xr-grow" :value="s.before.text" placeholder="text, e.g. M" :disabled="disabled"
                 v-tooltip.bottom="'Text that comes just before the part you want'"
                 @input="patch({ before: { ...s.before, text: val($event) } })" />
          <select class="xr-grow" :value="s.before.cls" :disabled="disabled" v-tooltip.bottom="'…then a varying part (a number, letters, …)'"
                  @change="patch({ before: { ...s.before, cls: val($event) } })">
            <option v-for="o in CLASSES" :key="o.value" :value="o.value">{{ o.label }}</option>
          </select>
        </div>
        <div class="cc-row">
          <span class="xr-label cc-muted">Extract</span>
          <select class="xr-grow" :value="s.extract.kind" :disabled="disabled" v-tooltip.bottom="'Kind of value to capture'"
                  @change="patch({ extract: { ...s.extract, kind: val($event) } })">
            <option v-for="o in KINDS" :key="o.value" :value="o.value">{{ o.label }}</option>
          </select>
          <input v-if="s.extract.kind === 'custom'" class="xr-grow" :value="s.extract.text" placeholder="e.g. [a-d]"
                 :disabled="disabled" v-tooltip.bottom="'Your own character set to capture'"
                 @input="patch({ extract: { ...s.extract, text: val($event) } })" />
        </div>
        <div class="cc-row">
          <span class="xr-label cc-muted">Followed by</span>
          <input class="xr-grow" :value="s.after.text" placeholder="text, e.g. -" :disabled="disabled"
                 v-tooltip.bottom="'Text that comes just after the part you want'"
                 @input="patch({ after: { ...s.after, text: val($event) } })" />
          <select class="xr-grow" :value="s.after.cls" :disabled="disabled" v-tooltip.bottom="'…then a varying part (a number, letters, …)'"
                  @change="patch({ after: { ...s.after, cls: val($event) } })">
            <option v-for="o in CLASSES" :key="o.value" :value="o.value">{{ o.label }}</option>
          </select>
        </div>
        <p class="xr-hint cc-muted">Each side: fixed text + a part that varies (<em>+ number</em>, <em>+ letters</em>).</p>
      </template>
      <p class="xr-hint cc-muted">Adjusts the pattern above — watch the preview, then apply.</p>
    </div>
  </div>
</template>

<style scoped>
.xr-full, .xr-compact { display: flex; flex-direction: column; gap: 0.35rem; min-width: 0; }
.xr-disabled { opacity: 0.5; pointer-events: none; }
.xr-grow { flex: 1 1 0; min-width: 0; }
.xr-regex { font-family: var(--cc-mono); }
.xr-compact .xr-regex { width: 100%; }
.xr-sep { width: 3.5rem; }
.xr-text { width: 5rem; }
.xr-value { font-family: var(--cc-mono); font-weight: 600; color: var(--cc-accent); white-space: nowrap; }
.xr-preview { display: flex; align-items: center; gap: 0.3rem; font-size: var(--cc-fs-sm); min-width: 0; }
.xr-src { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; flex: 1; min-width: 0; }
.xr-radio { display: flex; align-items: center; gap: 0.3rem; cursor: pointer; }
.xr-toggle { display: inline-flex; width: auto; font-size: var(--cc-fs-sm); padding: 0.1rem 0; }
.xr-toggle .pi { font-size: var(--cc-fs-2xs); }
.xr-builder {
  display: flex; flex-direction: column; gap: 0.4rem; padding: 0.5rem;
  border: 1px solid var(--cc-border); border-radius: var(--cc-radius-sm); background: var(--cc-surface-1);
}
.xr-label { flex: 0 0 5rem; white-space: nowrap; }
.xr-hint { margin: 0; line-height: 1.4; }
</style>
