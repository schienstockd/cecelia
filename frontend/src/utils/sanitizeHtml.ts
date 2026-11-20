// The ONE sanitiser for HTML that reaches `v-html` from text this build did not write: a blackboard
// entry (imported bundle, a steered Claude), release notes (GitHub release body). Script running in
// this origin carries the API cookie, and the API can install a plugin or enable the debug REPL —
// security audit F2 (docs/todo/SECURITY_AUDIT.md).
//
// DOMPurify needs a DOM: its Node build hands the input back untouched, so the Node unit tests see
// the raw HTML. The sanitising is proven in `sanitizeHtml.browser.test.ts` (real Chromium).
import DOMPurify from 'dompurify'

// DOMPurify keeps forms by default; a form in the text could submit to the API with the cookie.
// `input` stays: GFM task lists render `<input type="checkbox" disabled>`.
const _PURIFY = {
  FORBID_TAGS: ['form', 'button', 'textarea', 'select'],
  FORBID_ATTR: ['formaction', 'form'],
}

export function sanitizeHtml(html: string): string {
  return DOMPurify.isSupported ? DOMPurify.sanitize(html, _PURIFY) : html
}
