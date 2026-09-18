// Filename generator for the viewer's "Save PNG" button (`ViewerPanel.vue` Screenshot section).
// Pure — the composite itself lives in `modules/ViewerWindow.vue` behind
// `window.__cceceliaViewerScreenshot` because it needs live DOM (the WebGPU canvas element and the
// SVG overlays layered over it). Kept out here so at least the filename has a testable contract.
//
// Napari's parity: it saves as `<image_name>-<yyyy-mm-dd_HH-mm-ss>.png`. We follow the same shape
// so a user who screenshots several timepoints ends up with files that sort lexicographically in
// timestamp order. Colons and slashes forbidden on Windows filenames, hence the dashes.

// One `.` in a name is a real extension we don't want to duplicate; a `.` after the first char is
// treated as such. A trailing `.png` in the source name is stripped so the output is not
// `xxx.png-2026-09-18_14-45-12.png`.
export function stripExt(name: string): string {
  const i = name.lastIndexOf('.')
  return i > 0 && i > name.length - 6 ? name.slice(0, i) : name
}

// Windows-safe timestamp — no colons in `HH:mm:ss` (illegal in NTFS filenames), no timezone offset
// (bloats the name and the user's LOCAL clock is what they think of the shot as taken at).
export function timestampForFilename(now: Date = new Date()): string {
  const p = (n: number) => String(n).padStart(2, '0')
  return `${now.getFullYear()}-${p(now.getMonth() + 1)}-${p(now.getDate())}` +
         `_${p(now.getHours())}-${p(now.getMinutes())}-${p(now.getSeconds())}`
}

// Empty base falls back to `viewer` — a screenshot pane with no image name (very early boot, or a
// missing metadata field) still writes a legible filename.
export function screenshotFilename(imageName: string | null | undefined, now?: Date): string {
  const base = stripExt((imageName ?? '').trim()) || 'viewer'
  return `${base}-${timestampForFilename(now)}.png`
}
