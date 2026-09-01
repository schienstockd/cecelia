<script setup lang="ts">
/**
 * A schematic FRAME — a small SVG that draws whatever a producer resolved: dots, ribbons, mask
 * hints, timestamp corner, scale-bar corner, and an optional "TITLE CARD" chip above the frame.
 *
 * Deliberately not about batch movies, not about any one panel. It takes a `SceneAidRender` and
 * draws it; whatever produced that is the caller's business
 * (`modules/batchmovies/overlayPreview.ts` is the first producer; more will follow — see the note
 * in `lib/sceneAid.ts`). The reason to have it at all: a config panel is a form of numbers, and
 * "what will this produce" is a picture. `VisualAid` is the sibling that draws numbers as shapes,
 * `SceneAid` is the sibling that draws a schematic frame.
 *
 * **Coordinates are normalised 0..1**, so a producer never has to know the on-screen box. Scale to
 * the frame's viewBox here.
 *
 * **One svg viewport,** deliberately small (default 200×200). Big enough for a dozen dots to read
 * as a scattered field, small enough to stay a glyph next to controls.
 */
import { computed } from 'vue'
import type { SceneAidRender } from '../lib/sceneAid'
import InlineNote from './InlineNote.vue'

const props = withDefaults(defineProps<{
  /** The render, already computed by the producer. */
  render: SceneAidRender
  /** SVG side in CSS px. Producers pass the same for every render so cases stay comparable. */
  size?: number
  /** Radius of a drawn point, in SVG viewport units (the viewBox is 0..100). */
  pointR?: number
  /** Stroke width for ribbons and mask rings, in viewBox units. */
  strokeW?: number
  /** Show the frame border. Turned OFF when the producer supplies its own container. */
  bordered?: boolean
}>(), {
  size: 200,
  pointR: 1.5,
  strokeW: 0.8,
  bordered: true,
})

/** The viewBox is always 0..100 — so a producer can hand normalised 0..1 coords and this scales
 *  them without a second knob. */
const VB = 100
/** Size on the FRAME, not the outer wrapper: the outer is a flex column (chip / frame / caption)
 *  whose width doesn't stretch to children by default, so pinning size on it left the SVG at 0×0
 *  intrinsic (empty preview). */
const frameStyle = computed(() => ({ width: `${props.size}px`, height: `${props.size}px` }))

/** Turn a producer's ribbon into an SVG polyline `points` string. */
function polyPoints(pts: Array<{ x: number; y: number }>): string {
  return pts.map(p => `${p.x * VB},${p.y * VB}`).join(' ')
}
</script>

<template>
  <div class="scene-aid">
    <div v-if="render.corners.showTitleChip" class="sa-chip cc-fs-3xs">TITLE CARD</div>
    <div class="sa-frame" :class="{ 'is-bordered': bordered }" :style="frameStyle">
      <svg width="100%" height="100%" :viewBox="`0 0 ${VB} ${VB}`" preserveAspectRatio="xMidYMid meet"
           role="img" aria-label="Preview of the overlay">
        <!-- Ribbons FIRST, so a point sits ON its own ribbon rather than under it. -->
        <polyline v-for="(rib, i) in render.ribbons" :key="`rib-${i}`"
          :points="polyPoints(rib.points)"
          :stroke="rib.colour" :stroke-width="strokeW"
          fill="none" stroke-linejoin="round" stroke-linecap="round" />
        <!-- Points. `ringed` gets a thin outline in the point's own colour — the mask-outline hint
             from the producer, which the movie draws as a labels contour around the same cell.
             `mode: 'ring-only'` skips the filled centre for a mask-only view (no dots requested). -->
        <template v-for="(pt, i) in render.points" :key="`pt-${i}`">
          <circle v-if="pt.ringed || pt.mode === 'ring-only'" :cx="pt.x * VB" :cy="pt.y * VB"
            :r="pointR * 1.9" :stroke="pt.colour" :stroke-width="strokeW * 0.5"
            fill="none" :opacity="pt.mode === 'ring-only' ? 0.75 : 0.55" />
          <circle v-if="pt.mode !== 'ring-only'" :cx="pt.x * VB" :cy="pt.y * VB"
            :r="pointR" :fill="pt.colour" />
        </template>
      </svg>
      <span v-if="render.corners.showTimestamp" class="sa-ts cc-fs-3xs">
        {{ render.corners.timestampText ?? '0:00:00' }}
      </span>
      <span v-if="render.corners.showScaleBar" class="sa-sb cc-fs-3xs">
        <span class="sa-sb-bar" />
        {{ render.corners.scaleBarText ?? '25 µm' }}
      </span>
    </div>
    <!-- The empty-state hint uses the canonical `InlineNote` primitive (docs/ui/PRIMITIVES.md) so
         a producer's "cell-track ribbons need populations on" reads with the same icon + text
         shape as every other advisory in the app, at full size — a smaller variant here would be
         a fourth spelling of the primitive `InlineNote` exists to unify. -->
    <InlineNote v-if="render.caption" class="sa-cap" :short="render.caption" />
  </div>
</template>

<style scoped>
.scene-aid { display: flex; flex-direction: column; align-items: center; gap: 3px; }
/* The chip that stands in for "the batch will prepend a title card". Sits ABOVE the frame so it
   doesn't cover the schematic, and picks up the accent tint so it reads as "this is metadata". */
.sa-chip {
  background: color-mix(in srgb, var(--cc-accent) 22%, transparent);
  border: 1px solid var(--cc-accent);
  color: var(--cc-text); padding: 1px 6px; border-radius: var(--cc-radius-sm);
  letter-spacing: 0.06em;
}
.sa-frame { position: relative; background: #000; border-radius: var(--cc-radius-sm); overflow: hidden; }
.sa-frame.is-bordered { border: 1px solid var(--cc-border); }
.sa-frame > svg { display: block; }
/* Corner overlays sit on top of the SVG in tabular numerals so a tiny reference stays legible on
   top of a black frame, without a second glyph to read (which is what the movie shows too). */
.sa-ts, .sa-sb {
  position: absolute; color: #fff; text-shadow: 0 0 2px #000; font-variant-numeric: tabular-nums;
  padding: 1px 3px;
}
.sa-ts { top: 2px; left: 3px; }
.sa-sb { bottom: 2px; right: 3px; display: inline-flex; align-items: center; gap: 3px; }
.sa-sb-bar { display: inline-block; width: 18px; height: 2px; background: #fff; }
.sa-cap { align-self: center; }
</style>
