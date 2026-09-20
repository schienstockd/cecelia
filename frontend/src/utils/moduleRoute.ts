// Map a `module` tag (as stored on a plot capture's `address.plotSpec.params.module` and set on
// `ModuleLayout module="..."` / `SummaryCanvas module="..."`) to the router path that mounts
// its module page. Used by Kiwi's refocus on a plot capture: navigate to the page that owns the
// canvas the capture was made on, then hand it the envelope via the reshow store.
//
// New module pages: add a row. If a module has NO canvas Share flow (or no plot canvas at all),
// leave it out — the refocus path silently degrades (Kiwi's row keeps the copy-id fallback).

/** module tag → router hash path, or `null` when the module has no reshow-capable page. */
export function moduleRouteFor(module: string): string | null {
  switch (module) {
    case 'behaviourAnalysis': return '/behaviour'
    case 'phenotype':         return '/phenotype'
    case 'clustTracks':       return '/clust-tracks'
    case 'clustPops':         return '/clust-cells'
    case 'analysis':          return '/analysis'
    default:                  return null
  }
}
