// Axis-classification helpers for the gate axis pickers. Pure (name-based) so they hold regardless of
// whether the .h5ad lists centroids in `uns/spatial_cols` (legacy/partially-migrated data surfaces
// them as ordinary features). Kept out of the store SFC so they are unit-testable.

// A centroid coordinate column: centroid_x / centroid_y / centroid_z / centroid_t (and any centroid_*).
// These are raw positions → they should default to a LINEAR transform, never logicle.
export const isCentroidAxis = (col: string): boolean => /^centroid_/i.test(col)
