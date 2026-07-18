// "This image's physical metadata looks missing/suspect" — read from the qc.jl store (single source
// of truth, computed by `metadata_qc_findings` in qc.jl and banked under `importImages.omezarr` on
// import / resync / metadata edit). The old frontend re-derivation (`fieldIssues`) has been retired
// so the image table, the physical-size dialog, the whiteboard, the lab log and MCP never disagree.
// Used by the ImageTable row icon (short) and PhysicalSizeDialog (long hint + per-field highlight).
import type { CciaImage } from '../stores/project'
import { qcFindings, isMetadataCode, type QcFinding } from './qc'

export interface MetadataWarning {
  short: string
  long: string
}

export type PhysField = 'x' | 'y' | 'z' | 't'

// The image's calibration findings (`metadata.*` codes), in the backend's emit order.
function metadataFindings(img: CciaImage): QcFinding[] {
  return qcFindings(img).filter(f => isMetadataCode(f.code))
}

// The single most relevant issue — drives the ImageTable row icon and the dialog's warning line.
export function metadataWarning(img: CciaImage): MetadataWarning | null {
  const fs = metadataFindings(img)
  return fs.length ? { short: fs[0].short, long: fs[0].long } : null
}

// Which specific fields are implicated (from each finding's `detail.field`) — drives per-field
// highlighting in PhysicalSizeDialog so the user isn't left guessing which of X/Y/Z/Δt is meant.
export function flaggedFields(img: CciaImage): Set<PhysField> {
  const out = new Set<PhysField>()
  for (const f of metadataFindings(img)) {
    const field = f.detail?.field
    if (field === 'x' || field === 'y' || field === 'z' || field === 't') out.add(field)
  }
  return out
}

// Processed versions (drift/AF/cellpose) and segmentations bake in the calibration they were built
// with — corrections and measurements read pixel size from the image zarr, not this dialog — so a
// calibration change (or one that was wrong from the start) doesn't reach them until they're re-run.
export function downstreamArtifactsNote(img: CciaImage): MetadataWarning | null {
  const hasVariants = Object.keys(img.filepaths ?? {}).some(k => k !== 'default')
  const hasSegs = Object.keys(img.labels ?? {}).length > 0
  if (!hasVariants && !hasSegs) return null
  return {
    short: 'Has processed versions/segmentations — re-run them after correcting',
    long: 'Existing corrections, segmentations and measurements were built with the current ' +
      'calibration and won’t update on their own. Re-run them so analysis uses the new values.',
  }
}
