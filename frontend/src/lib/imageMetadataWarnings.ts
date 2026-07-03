// Single source of truth for "this image's physical metadata looks missing/suspect" — used by
// the ImageTable row icon (short tooltip), and PhysicalSizeDialog (long hint + per-field
// highlight), so all three surfaces never disagree about the same image.
import type { CciaImage } from '../stores/project'

export interface MetadataWarning {
  short: string
  long: string
}

export type PhysField = 'x' | 'y' | 'z' | 't'

interface FieldIssue extends MetadataWarning {
  field: PhysField
}

// Generous sane band for Z-step vs XY pixel size. Real acquisitions vary widely, but a ratio
// outside this range is far more likely to be a unit-conversion bug (e.g. an ImageJ TIFF saved
// with a non-micron calibration unit) than a real voxel geometry.
const Z_RATIO_MIN = 0.02
const Z_RATIO_MAX = 50

function fieldIssues(img: CciaImage): FieldIssue[] {
  const issues: FieldIssue[] = []
  const sizeZ = img.sizeZ ?? 1
  const sizeT = img.sizeT ?? 1

  if (sizeZ > 1 && (img.physicalSizeZ === null || img.physicalSizeZ === undefined)) {
    issues.push({
      field: 'z',
      short: 'Z spacing unknown — click to fill in',
      long: 'No Z step size was found for this image. Check your acquisition software’s ' +
        'metadata or open the file in Fiji (Image ▸ Properties) to find the voxel depth, ' +
        'then enter it below.',
    })
  } else if (img.physicalSizeZCorrected) {
    // Auto-corrected at import from the source file's own ImageJ tag (bioformats2raw's raw value
    // was even more obviously wrong) — but that tag isn't independently verifiable from the data
    // alone, so this ALWAYS stays flagged for a human to confirm, even when the corrected ratio
    // now looks plausible.
    issues.push({
      field: 'z',
      short: 'Z spacing auto-corrected — click to verify',
      long: 'This Z step was auto-corrected from the source file’s own ImageJ tag ' +
        '(the file’s calibration unit wasn’t micrometers). That tag may not be a real ' +
        'per-slice measurement, so please confirm it against Fiji (Image ▸ Properties) or ' +
        'your acquisition settings before trusting it.',
    })
  } else if (img.physicalSizeZ != null && img.physicalSizeX != null && img.physicalSizeX > 0) {
    const ratio = img.physicalSizeZ / img.physicalSizeX
    if (ratio < Z_RATIO_MIN || ratio > Z_RATIO_MAX) {
      issues.push({
        field: 'z',
        short: 'Z spacing looks unusual — click to review',
        long: 'The Z step looks unusually small or large compared to the pixel size in X/Y. ' +
          'This can happen when the original file’s calibration unit wasn’t ' +
          'micrometers — e.g. an ImageJ TIFF saved with unit = inch instead of micron. ' +
          'Open the original file in Fiji (Image ▸ Properties) to check the real voxel ' +
          'depth, then correct it below.',
      })
    }
  }

  if (sizeT > 1 && (img.timeIncrement === null || img.timeIncrement === undefined)) {
    issues.push({
      field: 't',
      short: 'Frame interval unknown — click to fill in',
      long: 'No frame interval was found in this file’s metadata. This is common for plain ' +
        'TIFF stacks without hyperstack timing info. If you know the interval from your ' +
        'acquisition settings, enter it below.',
    })
  } else if (img.timeIncrement != null && !img.timeIncrementUnit) {
    // A number with no known unit is exactly as untrustworthy as no number — don't let it pass
    // silently just because a value is present.
    issues.push({
      field: 't',
      short: 'Frame interval has no unit — click to review',
      long: 'A frame interval is recorded but its unit (seconds/minutes) is unknown, so the ' +
        'number can\'t be trusted as-is. Confirm it against your acquisition settings and ' +
        're-enter it with a unit below.',
    })
  }

  // Same idea for the spatial unit: a size without a known unit isn't usable either. One shared
  // PhysicalSizeUnit covers X/Y/Z, so flag whichever axes actually have a value recorded.
  if (!img.physicalSizeUnit) {
    if (img.physicalSizeX != null) issues.push({
      field: 'x',
      short: 'Pixel size has no unit — click to review',
      long: 'A pixel size is recorded but its unit is unknown, so the number can\'t be trusted ' +
        'as-is. Confirm it against your acquisition settings and re-enter it with a unit below.',
    })
    if (img.physicalSizeY != null) issues.push({
      field: 'y',
      short: 'Pixel size has no unit — click to review',
      long: 'A pixel size is recorded but its unit is unknown, so the number can\'t be trusted ' +
        'as-is. Confirm it against your acquisition settings and re-enter it with a unit below.',
    })
    if (img.physicalSizeZ != null && !issues.some(i => i.field === 'z')) issues.push({
      field: 'z',
      short: 'Voxel depth has no unit — click to review',
      long: 'A Z step is recorded but its unit is unknown, so the number can\'t be trusted ' +
        'as-is. Confirm it against your acquisition settings and re-enter it with a unit below.',
    })
  }

  return issues
}

// The single most relevant issue — drives the ImageTable row icon and the dialog's warning line.
export function metadataWarning(img: CciaImage): MetadataWarning | null {
  const issues = fieldIssues(img)
  return issues.length ? { short: issues[0].short, long: issues[0].long } : null
}

// Which specific fields are implicated — drives per-field highlighting in PhysicalSizeDialog so
// the user isn't left guessing which of X/Y/Z/Δt the banner text is actually about.
export function flaggedFields(img: CciaImage): Set<PhysField> {
  return new Set(fieldIssues(img).map(i => i.field))
}
