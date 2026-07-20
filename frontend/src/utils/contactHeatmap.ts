// Map the /api/plots/contact_matrix response to the shared PlotDataResponse matrix shape, so the
// CODEX log-odds contact heatmap draws through the generic PlotChart (chartType 'heatmap') — no
// bespoke renderer (SPATIAL_REGIONS_PLAN Decision 16). Logic lives here (not the SFC) so it's tested.

import type { PlotDataResponse } from '../plots/types'

export interface ContactMatrixResponse {
  suffixes: string[]
  suffix: string
  basis: string[]
  nCells: number
  nEdges: number
  cells: { x: string; y: string; value: number }[]
}

/** Pop × pop log-odds matrix → PlotDataResponse (square: xLabels = yLabels = basis). */
export function contactMatrixToPlotData(r: ContactMatrixResponse): PlotDataResponse {
  return {
    chartType: 'matrix',
    matrixMode: 'profile',
    measure: '',
    granularity: 'cell',
    series: [],
    xLabels: r.basis,
    yLabels: r.basis,
    cells: r.cells.map(c => ({ x: c.x, y: c.y, value: c.value })),
    valueLabel: 'log-odds',
  }
}

/** True when there's a matrix to show (a neighbourStats run exists with ≥1 pair). */
export function hasContactMatrix(r: ContactMatrixResponse | null): boolean {
  return !!r && r.cells.length > 0
}
