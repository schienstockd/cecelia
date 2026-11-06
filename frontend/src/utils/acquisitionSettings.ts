/**
 * The source file's acquisition settings (`image.acquisition`, read at import by
 * `python/cecelia/utils/oir_meta.py`, `czi_meta.py`) turned into label/value rows for the image metadata dialog.
 * Values are shown as recorded; nothing here judges them.
 */

import { fmtNum } from './physicalSize'

export interface AcquisitionLaser { name: string; wavelengthNm?: number | null; transmissionPct?: number | null }
export interface AcquisitionChannel {
  order: number; name: string | null; dyePreset: string | null; laser: string | null
  detector: string | null; hv: number | null; gain: number | null; offset: number | null
  bitDepth?: number | null
}
export interface AcquisitionSettings {
  /** Which reader produced it: `oir_meta` or `czi_meta`. */
  source?: 'oir' | 'czi' | null
  system?: string | null
  softwareVersion?: string | null
  objective?: { name?: string | null; magnification?: number | null; na?: number | null; immersion?: string | null } | null
  lasers?: AcquisitionLaser[]
  brightZ?: { enabled: boolean | null; points: { zFromStartUm: number | null; transmissionPct: Record<string, number | null> }[] } | null
  scanner?: {
    type?: string | null; zoom?: number | null; pixelDwellUs?: number | null
    integration?: { type?: string | null; count?: number | null; average?: boolean | null } | null
  } | null
  autoHV?: boolean | null
  pinholeUm?: number | null
  channels?: AcquisitionChannel[]
  phaseCount?: number | null
  /** `acquired` is set when the store has fewer frames than the file planned (stopped early, or cropped). */
  timelapse?: { intervalS?: number | null; count?: number | null; acquired?: number | null } | null
}

/** Why an image has no settings (`ACQUISITION_MISSING` in omezarr/acquisition.jl). */
export interface AcquisitionMissing { reason: 'format' | 'source' | 'unread' | 'other-stack' | 'registered' | 'no-source'; ext?: string }

/** The vendor's name for the depth power ramp: Evident's "Bright Z", Zeiss's "Auto Z brightness". */
export function depthRampName(acq: AcquisitionSettings | null | undefined): string {
  return acq?.source === 'czi' ? 'Auto Z brightness' : 'Bright Z'
}

/** "FVMPE-RS · software 2.6.1.243", or null when the file does not name the system. */
export function microscopeName(acq: AcquisitionSettings | null | undefined): string | null {
  return acq?.system ? join([acq.system, acq.softwareVersion && `software ${acq.softwareVersion}`]) : null
}

/** "21 of 45 planned frames" when a time-lapse stopped early (or was cropped), else null. */
export function framesShortOf(acq: AcquisitionSettings | null | undefined): string | null {
  const t = acq?.timelapse
  return t?.acquired != null && t.count != null && t.acquired < t.count ? `${t.acquired} of ${n(t.count)} planned frames` : null
}

export interface AcquisitionNotice { text: string; action?: { label: string; url: string } }

/**
 * What the Acquisition section says instead of (or under) the settings, so it never silently vanishes:
 * why there are no settings, or why there is no diagram. `requestUrl(ext)` builds the "support this
 * microscope" issue link (lib/links.ts).
 */
export function acquisitionNotice(
  acq: AcquisitionSettings | null | undefined, missing: AcquisitionMissing | null | undefined,
  hasDiagram: boolean, requestUrl: (ext: string) => string,
): AcquisitionNotice | null {
  if (acq) return hasDiagram ? null : { text: 'Light-path diagram: two-photon only so far' }
  switch (missing?.reason) {
    case 'format': {
      const ext = missing.ext || 'this file type'
      return { text: `Settings are not read from ${ext} files yet`, action: { label: 'Request your microscope', url: requestUrl(ext) } }
    }
    case 'source': return { text: 'Source file not reachable: refresh metadata when it is' }
    case 'unread': return { text: 'No readable settings in the source file' }
    case 'other-stack': return { text: 'Source settings describe another stack (a projection, crop or resample)' }
    case 'registered': return { text: 'Registered from several cycles: no single source file' }
    case 'no-source': return { text: 'No original file recorded' }
    default: return { text: 'Not read yet: refresh metadata' }
  }
}

export interface AcquisitionRow { k: string; v: string }

const n = (x: number | null | undefined): string => (x == null ? '—' : fmtNum(x))

const join = (parts: (string | null | undefined | false)[]) => parts.filter(Boolean).join(' · ')

/** "MaiTai 14.4 → 21 %" per laser over "0 → 60 µm" — every stored point, so a ramp that peaks
 *  mid-stack reads as one ("10 → 40 → 20 %"), not as its two ends. */
function brightZRamp(bz: NonNullable<AcquisitionSettings['brightZ']>): string {
  const pts = bz.points ?? []
  if (pts.length < 2) return ''
  const lasers = Object.keys(pts[0].transmissionPct ?? {})
  const depths = pts.map(p => n(p.zFromStartUm)).join(' → ')
  return lasers.map(l => `${l} ${pts.map(p => n(p.transmissionPct[l])).join(' → ')} %`)
    .join('; ') + ` over ${depths} µm`
}

export function acquisitionRows(acq: AcquisitionSettings | null | undefined): AcquisitionRow[] {
  if (!acq) return []
  const rows: AcquisitionRow[] = []
  const scope = microscopeName(acq)
  if (scope) rows.push({ k: 'Microscope', v: scope })
  const short = framesShortOf(acq)
  if (short) rows.push({ k: 'Time-lapse', v: `${short} (stopped early or cropped)` })
  if ((acq.phaseCount ?? 1) > 1) rows.push({ k: 'Phases', v: `${acq.phaseCount} · first shown` })
  const o = acq.objective
  if (o) rows.push({ k: 'Objective', v: join([o.name, o.magnification != null && `${n(o.magnification)}×`, o.na != null && `NA ${n(o.na)}`, o.immersion]) })
  for (const l of acq.lasers ?? []) {
    rows.push({ k: 'Laser', v: join([l.name, l.wavelengthNm != null && `${n(l.wavelengthNm)} nm`, l.transmissionPct != null && `${n(l.transmissionPct)} %`]) })
  }
  const bz = acq.brightZ
  if (bz && bz.enabled != null) {
    rows.push({ k: depthRampName(acq), v: bz.enabled ? join(['on', brightZRamp(bz)]) : 'off' })
  }
  const s = acq.scanner
  if (s) {
    rows.push({ k: 'Scanner', v: join([s.type?.toLowerCase(), s.zoom != null && `zoom ${n(s.zoom)}×`, s.pixelDwellUs != null && `dwell ${n(s.pixelDwellUs)} µs`]) })
    const integ = s.integration
    if (integ?.type) {
      const how = integ.average === false ? 'sum' : integ.average === true ? 'average' : null
      rows.push({ k: 'Integration', v: integ.type === 'None' ? 'none' : join([`${integ.type.toLowerCase()} ×${n(integ.count)}`, how]) })
    }
  }
  if (acq.autoHV != null) rows.push({ k: 'Auto HV', v: acq.autoHV ? 'on' : 'off' })
  if (acq.pinholeUm != null) rows.push({ k: 'Pinhole', v: `${n(acq.pinholeUm)} µm` })
  for (const c of acq.channels ?? []) {
    rows.push({ k: c.name ?? `Channel ${c.order}`, v: join([c.dyePreset, c.laser, c.detector, c.hv != null && `HV ${n(c.hv)}`]) })
  }
  return rows
}
