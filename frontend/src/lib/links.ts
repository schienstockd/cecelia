// The project's outward links, in one place.
//
// There were three hardcoded `github.com/schienstockd/cecelia/…` literals across the frontend before
// this file (the What's New "Report a problem" default, its "View on GitHub" footer link, and the
// releases page) and the header was about to add two more. A repo move is one rename here, not a
// grep — and the rename is coming: `docs/SHIPPING.md` → *Repo swap* still has the legacy repo living
// at the old name.
//
// Kept deliberately dumb — string constants only, no fetching. `stores/appControl.ts` owns anything
// that ASKS GitHub something (the update check reads the releases API); this module only says where
// things are.

export const CECELIA_REPO_URL = 'https://github.com/schienstockd/cecelia'

/** The issues LIST — "here is where problems get reported", for a browse-first entry point. */
export const CECELIA_ISSUES_URL = `${CECELIA_REPO_URL}/issues`
/** Straight to the form — for a "report this" action, where the user already knows what they hit. */
export const CECELIA_NEW_ISSUE_URL = `${CECELIA_REPO_URL}/issues/new`
export const CECELIA_RELEASES_URL = `${CECELIA_REPO_URL}/releases`

// Zulip, not a GitHub Discussion: the lab already runs one, and a question people ask in chat is a
// question they would not have opened an issue for.
export const CECELIA_CHAT_URL = 'https://cecelia.zulipchat.com/'
