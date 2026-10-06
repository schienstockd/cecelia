// Which IMAGE version a `task:result` frame says it wrote — or none.
//
// `meta.valueName` is overloaded across tasks: a pixel writer (smooth, denoise, drift_correct, flip,
// dtype, af_correct, stack_align, flow_register, a composite, the OME-ZARR import) returns the image
// version it WROTE, but tracking, segment/track correction, track measures, contacts, aggregates,
// clustering and the OME-TIFF export return the LABEL / input vn they worked on. Only the pixel
// writers also return `filename` (the new store's path), so the pair is the one reliable mark of a
// new image version.
//
// `ws.ts` folds the same pair into the image's `filepaths`; `ViewerPanel.onTaskResult` follows it.
export function writtenImageVersion(meta: Record<string, unknown> | null | undefined): string | null {
  const vn = meta?.valueName
  const filename = meta?.filename
  return typeof vn === 'string' && vn && typeof filename === 'string' && filename ? vn : null
}
