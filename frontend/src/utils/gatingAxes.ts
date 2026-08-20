// Axis-classification helpers for the gate axis pickers. Pure (name-based) so they hold regardless of
// whether the .h5ad lists centroids in `uns/spatial_cols` (legacy/partially-migrated data surfaces
// them as ordinary features). Kept out of the store SFC so they are unit-testable.

// A centroid coordinate column: centroid_x / centroid_y / centroid_z / centroid_t (and any centroid_*).
// These are raw positions → they should default to a LINEAR transform, never logicle.
export const isCentroidAxis = (col: string): boolean => /^centroid_/i.test(col)

// Display names for the centroid columns. `centroid_x` / `centroid_t` are the on-disk obs/obsm names
// (written by `centroid_migrate.py`) and every picker showed them verbatim — so the axis a biologist
// picks to gate on position read as `centroid_x`, and the one that splits a movie into timepoints read
// as `centroid_t`. Neither says "spatial X" or "time" to anyone who did not write the pipeline.
//
// DISPLAY ONLY. The stored column, the CSV export and the REPL keep the raw name — this is the same
// split the gating store's `colLabel` already makes for intensity columns → channel names, and the
// reason a rename here cannot desynchronise anything downstream.
//
// An unmapped `centroid_*` falls through unchanged rather than being guessed at.
const CENTROID_LABELS: Record<string, string> = {
  centroid_x: 'X position',
  centroid_y: 'Y position',
  centroid_z: 'Z position',
  centroid_t: 'Time',
}
export const centroidLabel = (col: string): string =>
  CENTROID_LABELS[col.toLowerCase()] ?? col

// Axis name for display, with the unit the server says the values are in — `centroid_x (µm)` vs
// `centroid_x (px)`. Position gates used to read as a bare `centroid_x` with the numbers silently in
// pixels, which is unreadable across images with different pixel sizes. The unit comes from the
// plotdata response (`xUnit`/`yUnit`), never guessed here, so the label cannot claim µm while the
// numbers are pixels. Empty unit (any non-spatial axis, e.g. an intensity channel) → the name alone.
export const axisLabelWithUnit = (name: string, unit?: string | null): string =>
  unit ? `${name} (${unit})` : name

/**
 * Is this axis an IMAGE ROW coordinate — i.e. does it grow downward on screen?
 *
 * Only `centroid_y`. An image is indexed from the top-left, so row 0 is the top of the frame: that is
 * what napari draws, what every pixel index in the pipeline means, and what the segmentation wrote.
 * A chart's default y axis grows upward, which is right for an intensity and a MIRROR for a position —
 * a cell drifting down-screen appears to drift up, and comparing a plot with the viewer silently means
 * comparing a shape with its reflection.
 *
 * NOT `centroid_z` and NOT `centroid_t`. z is depth into the stack and t is time; neither is a screen
 * axis in the viewer, so "which way is up" for them is a plotting convention rather than a
 * disagreement with the image. Flipping those would invent a mismatch instead of removing one.
 *
 * Name-based, like `isCentroidAxis` above and for the same reason: it must hold for data that does not
 * list its centroids in `uns/spatial_cols`.
 */
export const isImageYAxis = (col: string): boolean => /^centroid_y$/i.test(col ?? '')
