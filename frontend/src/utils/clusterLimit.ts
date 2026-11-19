// Most clusters a plot or the population manager draws one by one (a centroid label, a legend row, a
// chip). Above it the page would lay out thousands of labels and freeze — a run like that is not a
// result to read, so the views show the count instead. Matches the heatmap's limit on the server
// (`MATRIX_MAX_LEVELS`, app/src/plotting/plot_data.jl) and the region task's (`MAX_REGIONS`).
export const MAX_DRAWN_CLUSTERS = 200

export const tooManyClustersToDraw = (n: number): boolean => n > MAX_DRAWN_CLUSTERS

export const tooManyClustersText = (n: number): string =>
  `${n.toLocaleString()} clusters — too many to label`
