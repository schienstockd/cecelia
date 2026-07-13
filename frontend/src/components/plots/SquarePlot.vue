<!--
  Generalised square plot area — the ONE way to render a coord-fixed plot (UMAP, gating scatter) as a
  1:1 square, centred in whatever (possibly rectangular) space it's given. The inner square is sized to
  the SMALLER of the container's width/height via container-query units, so it stays square on any
  resize and never overflows. Slot content fills the square (position it `absolute; inset:0`).

  Why a component: D wants the same square enforced across UMAP + gating modules, not re-derived per
  plot (the montage single-cell already used this container-query trick — this promotes it to a shared
  primitive). A square plot area also removes the letterbox ambiguity that desynced HTML overlays
  (centroid labels / facet titles) from the canvas dots.
-->
<template>
  <div class="sq-outer"><div class="sq-inner"><slot /></div></div>
</template>

<style scoped>
/* container-query context; centre the square in the leftover space */
.sq-outer { flex: 1; min-width: 0; min-height: 0; container-type: size;
  display: flex; align-items: center; justify-content: center; }
/* the square: as large as possible while fitting BOTH dimensions (min of the two container sides) */
.sq-inner { position: relative; aspect-ratio: 1; width: min(100cqw, 100cqh); height: min(100cqw, 100cqh); }
</style>
