// Pure logic for the Analysis-board custom plate builder (PlateBuilder.vue). A "plate" is a CSS-grid
// template (cols × rows) with a grid-area per slot; the builder lets the user drag across cells to MERGE
// them into rectangular spans, the rest staying 1×1. Kept out of the SFC so it's unit-tested
// (docs/DEV.md → Tests). Spans are 0-indexed, INCLUSIVE of both corners; slot grid-areas are the CSS
// 1-indexed, end-EXCLUSIVE form ("rowStart / colStart / rowEnd / colEnd").
import type { LayoutTemplate } from '../plots/layoutTemplates'

export interface PlateSpan { r0: number; c0: number; r1: number; c1: number }
export interface Cell { r: number; c: number }

// order two dragged corners into a normalised span (top-left → bottom-right)
export function normSpan(a: Cell, b: Cell): PlateSpan {
  return { r0: Math.min(a.r, b.r), c0: Math.min(a.c, b.c), r1: Math.max(a.r, b.r), c1: Math.max(a.c, b.c) }
}
export function spanArea(s: PlateSpan): number { return (s.r1 - s.r0 + 1) * (s.c1 - s.c0 + 1) }
export function spansOverlap(a: PlateSpan, b: PlateSpan): boolean {
  return a.r0 <= b.r1 && b.r0 <= a.r1 && a.c0 <= b.c1 && b.c0 <= a.c1
}
export function cellInSpan(s: PlateSpan, r: number, c: number): boolean {
  return r >= s.r0 && r <= s.r1 && c >= s.c0 && c <= s.c1
}

// apply a dragged rectangle: a SINGLE cell that lands inside an existing span un-merges it (split back
// to 1×1s); a MULTI-cell rectangle merges (dropping any spans it overlaps, so merges never overlap).
export function applyDrag(spans: PlateSpan[], rect: PlateSpan): PlateSpan[] {
  if (spanArea(rect) <= 1) {
    const hit = spans.find(s => cellInSpan(s, rect.r0, rect.c0))
    return hit ? spans.filter(s => s !== hit) : spans
  }
  return [...spans.filter(s => !spansOverlap(s, rect)), rect]
}

// drop spans that fall outside a resized grid (so shrinking cols/rows can't leave dangling spans)
export function clampSpans(spans: PlateSpan[], cols: number, rows: number): PlateSpan[] {
  return spans.filter(s => s.r0 >= 0 && s.c0 >= 0 && s.r1 < rows && s.c1 < cols)
}

// row-major slot list: each span emitted at its top-left, remaining cells as 1×1s. Row-major keeps slot
// indices stable-ish so applyTemplate can preserve existing slot CONTENTS by index.
export function spansToSlots(cols: number, rows: number, spans: PlateSpan[]): string[] {
  const used: boolean[][] = Array.from({ length: rows }, () => Array(cols).fill(false))
  const slots: string[] = []
  for (let r = 0; r < rows; r++) for (let c = 0; c < cols; c++) {
    if (used[r][c]) continue
    const s = spans.find(sp => sp.r0 === r && sp.c0 === c)
    if (s) {
      for (let rr = s.r0; rr <= s.r1; rr++) for (let cc = s.c0; cc <= s.c1; cc++) used[rr][cc] = true
      slots.push(`${s.r0 + 1} / ${s.c0 + 1} / ${s.r1 + 2} / ${s.c1 + 2}`)
    } else {
      used[r][c] = true
      slots.push(`${r + 1} / ${c + 1} / ${r + 2} / ${c + 2}`)
    }
  }
  return slots
}

// parse an existing plate's slot areas back into multi-cell spans (so the builder opens seeded from the
// current layout). 1×1 slots are implicit and dropped.
export function slotsToSpans(slotAreas: string[]): PlateSpan[] {
  const spans: PlateSpan[] = []
  for (const a of slotAreas) {
    const p = a.split('/').map(x => parseInt(x.trim(), 10))
    if (p.length !== 4 || p.some(n => Number.isNaN(n))) continue
    const [rs, cs, re, ce] = p                       // 1-indexed, end-exclusive
    const s: PlateSpan = { r0: rs - 1, c0: cs - 1, r1: re - 2, c1: ce - 2 }
    if (s.r1 > s.r0 || s.c1 > s.c0) spans.push(s)     // keep multi-cell spans only
  }
  return spans
}

export function buildPlate(cols: number, rows: number, spans: PlateSpan[]): LayoutTemplate {
  return { id: 'custom', label: 'Custom', cols, rows, orient: 'any',
           slots: spansToSlots(cols, rows, clampSpans(spans, cols, rows)) }
}
