// v-range-fill — keeps a native `<input type="range">`'s filled part in step with its value.
//
// The base style (style.css → *Sliders*) draws one look for every single-value slider: a thin rail,
// accent fill up to the thumb, a small round thumb — the shape `RangeSlider` draws for two thumbs.
// Firefox can paint the fill from CSS alone (`::-moz-range-progress`); WebKit/Blink cannot, so the
// track's gradient stops at `--cc-range-pct`, which this directive writes. It is never written by
// hand: `vite.config.ts` adds it to every range input at compile time (`utils/rangeFillTransform.ts`), so a
// new slider is covered without anyone remembering to.
//
// `updated` catches a value set from code (a playing timepoint, a reset); the `input` listener
// catches a drag on a slider whose value no template reads back.
import type { Directive } from 'vue'
import { posFromValue } from '../utils/rangeScale'

function paint(el: HTMLInputElement) {
  const min = el.min === '' ? 0 : Number(el.min)
  const max = el.max === '' ? 100 : Number(el.max)
  // a range input's value is always a valid number (the browser sanitises it), so no NaN guard
  el.style.setProperty('--cc-range-pct', `${posFromValue(Number(el.value), min, max, 'linear') * 100}%`)
}

const onInput = (e: Event) => paint(e.target as HTMLInputElement)

export const rangeFill: Directive<HTMLInputElement> = {
  mounted(el) { paint(el); el.addEventListener('input', onInput) },
  updated(el) { paint(el) },
  beforeUnmount(el) { el.removeEventListener('input', onInput) },
}
