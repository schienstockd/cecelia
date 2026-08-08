// The persisted /analysis boards document (`settings/analysisBoards.json`) as it travels over the
// wire, plus the two decisions the autosave has to make about it. The server owns the file format and
// normalises both shapes on read (app/src/analysis_boards.jl), so the client only ever sees this one.
//
// `version` is optimistic concurrency. The autosave is a debounced overwrite of the WHOLE document, so
// two browser tabs open on one project used to clobber each other with no error — whichever debounce
// fired last simply won, and the other tab's boards were gone. Each write now echoes the version it
// last read; the server rejects a stale one with 409.

export interface BoardTab { id: number; name: string }
export interface BoardTabGroup { tabs: BoardTab[]; activeId: number; nextId: number }

export interface BoardsDoc {
  version: number
  tabs: BoardTab[]
  activeId: number
  nextId: number
  layouts: Record<string, unknown>
}

/** The document to POST. `version` is added by the caller, which owns the last-read value. */
export function boardsPayload(
  group: BoardTabGroup | null | undefined,
  layouts: Record<string, unknown>,
): Omit<BoardsDoc, 'version'> {
  return {
    tabs: group?.tabs ?? [],
    activeId: group?.activeId ?? 0,
    nextId: group?.nextId ?? 0,
    layouts,
  }
}

/** Split a document back into what the two stores each own. */
export function tabGroupOf(doc: Partial<BoardsDoc> | null | undefined): BoardTabGroup {
  return { tabs: doc?.tabs ?? [], activeId: doc?.activeId ?? 0, nextId: doc?.nextId ?? 0 }
}

/**
 * Should a `boards:changed` broadcast make THIS client reload?
 *
 * No for another project, and no for the echo of our OWN write, which is identified by `clientId` —
 * NOT by comparing versions. The server broadcasts before it returns the response, so a writer still
 * holds the pre-write version when its own frame arrives: a version test says "newer, reload" and the
 * writer reloads its own write, replacing every board entry and re-rendering the whole canvas on every
 * autosave. That is a timing race, so it cannot be fixed with a timing-based guard.
 *
 * A version at or below ours is still ignored — broadcasts can arrive out of order — but that is the
 * secondary check now, not the identity one.
 *
 * A frame with NO `clientId` is someone else by construction (the MCP add-a-board route, or an older
 * client), so it is honoured.
 */
export function shouldReloadBoards(
  frame: { projectUid?: unknown; version?: unknown; clientId?: unknown } | null | undefined,
  uid: string | null | undefined,
  ourVersion: number,
  ourClientId?: string,
): boolean {
  if (!uid || !frame || String(frame.projectUid ?? '') !== uid) return false
  const from = String(frame.clientId ?? '')
  if (from && ourClientId && from === ourClientId) return false      // our own write coming back
  const v = Number(frame.version)
  return Number.isFinite(v) && v > ourVersion
}
