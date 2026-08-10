// Axis-classification helpers for the gate axis pickers. Pure (name-based) so they hold regardless of
// whether the .h5ad lists centroids in `uns/spatial_cols` (legacy/partially-migrated data surfaces
// them as ordinary features). Kept out of the store SFC so they are unit-testable.

// A centroid coordinate column: centroid_x / centroid_y / centroid_z / centroid_t (and any centroid_*).
// These are raw positions → they should default to a LINEAR transform, never logicle.
export const isCentroidAxis = (col: string): boolean => /^centroid_/i.test(col)

// Axis name for display, with the unit the server says the values are in — `centroid_x (µm)` vs
// `centroid_x (px)`. Position gates used to read as a bare `centroid_x` with the numbers silently in
// pixels, which is unreadable across images with different pixel sizes. The unit comes from the
// plotdata response (`xUnit`/`yUnit`), never guessed here, so the label cannot claim µm while the
// numbers are pixels. Empty unit (any non-spatial axis, e.g. an intensity channel) → the name alone.
export const axisLabelWithUnit = (name: string, unit?: string | null): string =>
  unit ? `${name} (${unit})` : name
