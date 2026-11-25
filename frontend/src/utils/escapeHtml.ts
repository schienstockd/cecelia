/**
 * Escape text for HTML or XML — element text and attribute values alike (`& < > " '`). The one copy:
 * every hand-built markup string (an HTML tooltip, an SVG export, the Blackboard's Markdown fallback)
 * escapes each interpolation through this.
 */
const ESCAPES: Record<string, string> = {
  '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;',
}

export function escapeHtml(s: string): string {
  return s.replace(/[&<>"']/g, c => ESCAPES[c])
}
