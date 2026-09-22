// Per-tab stable identifier (BIDIR PR #8). One module = one value for the process, used by
// `stores/ws.ts` (sent on `viewer:hello`) AND `stores/plotRegistry.ts` (stamped on every
// register / deregister POST). Kept in its own file so a Vitest importer can pull the constant
// without dragging the WS store's transitive `window`-reading dependencies (`log.ts` binds a
// `storage` listener at setup time — a node/jsdom-free unit test would blow up on the import).
//
// `crypto.randomUUID` is available on every modern browser; the Math.random fallback keeps this
// safe under a Node-only Vitest run where `crypto` is imported but not the DOM.
export const wsClientId: string =
  (globalThis.crypto?.randomUUID?.() ?? String(Math.random()).slice(2))
