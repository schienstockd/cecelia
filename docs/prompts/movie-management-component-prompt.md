# Movie management component

## Context

PR #505 rebuilt the Movies page's file list on shared parts (`SelectionTable`
with opt-in sort/sizing, `CollapsiblePanel`, `useColumnResize`, `sortRows.ts`)
rather than as a bespoke list. That's the foundation to build on, not
something to re-architect.

The set of recorded movies is going to keep growing in kind, not just count:
with/without segmentation, tracking, image processing steps, animations —
and more categories over time. The current page is a flat, one-shot list of
files. It needs to become an interface for organising and managing that
growing collection, not just displaying it.

## Requirements

Check `ImageTable` for each of these before implementing — several likely
already exist there and should be generalised rather than duplicated (see
"Duplication with ImageTable" below).

1. **Rename.** Edit a movie's filename (or a display name distinct from the
   filename, if the codebase already separates the two — check how `note`
   currently works before deciding).
2. **Delete.** Remove a movie, with confirmation given it's a destructive
   filesystem action.
3. **Favourite/star.** Mark movies, independent of category.
4. **Categorise.** Movies need to be groupable — check whether "category"
   should be a fixed enum tied to how the movie was produced (segmentation,
   tracking, animation, raw/processing-step output) or a free-form tag the
   user assigns, or both. The taxonomy will keep growing, so whatever's
   chosen must not require a code change to add a new kind.
5. **Filter/organise by the above** — star, category/tag, and existing
   sortable columns should compose, not replace each other.
6. **Save generation configuration.** When a movie is generated (animation
   page, batch movie page), persist the configuration used to produce it —
   whatever set of parameters drives that generation (colours/channel
   mapping, whether segmentation is overlaid, tracking on/off, etc. —
   confirm the actual parameter set from those pages' generation code
   rather than assuming this list).
7. **Edit/recreate.** An "edit movie" action on a saved movie that reopens
   the animation or batch movie page pre-filled with that movie's saved
   configuration, so a small change (e.g. swap cell colour green→red, or
   add the segmentation overlay to an existing movie) means adjusting one
   field and regenerating, not rebuilding the configuration from scratch.

## Configuration versioning

Saved configuration will outlive the code that produced it — a config
saved today needs to still be loadable and meaningfully editable after the
generation parameters themselves change (new option added, old one
renamed/removed, default changed). Cecelia already has a version-sensitive
wire protocol for the napari bridge; treat saved movie configuration the
same way rather than as a plain unversioned parameter dump.

- Tag each saved configuration with a protocol/schema version at save time.
- Decide how "edit movie" handles a config saved under an older version
  than the current generation code expects — best-effort field mapping,
  explicit migration step, or surfacing to the user what couldn't be
  restored — rather than silently dropping or misapplying fields.
- Check whether the napari bridge's existing version-handling pattern
  (or any other existing protocol-versioning code in the repo) is
  something this should reuse directly, rather than inventing a second
  versioning scheme.

## Duplication with ImageTable and the analysis board

Some of these — rename, delete, and possibly others — already exist on
`ImageTable`. The same save-configuration-and-recreate principle already
exists on the analysis board's image strip too. Before building anything
new for Movies, check both:
- what `ImageTable` already does for each requirement above, and whether
  it's implemented as something reusable or inline/table-specific.
- how the analysis board's image strip saves and reapplies its own
  generation configuration — same shape of problem as requirements 6/7
  above (save config, recreate/edit later) — and whether it already solved
  the versioning question raised below, or has the same gap.

**Open question for Opus, wants your actual opinion, not just an audit:**
should `ImageTable` itself move onto the same shared table component being
built out for Movies (i.e. one generalised table component with rename/
delete/star/category used by both ImageTable and Movies), rather than
Movies growing its own version of what ImageTable already has? Weigh it
against the alternative of keeping them separate consumers of shared
pieces (as #505 already does via `SelectionTable`/`sortRows`/
`useColumnResize`) without merging the tables themselves. State a
recommendation and the reasoning, including migration risk to
`ImageTable` (an existing, presumably load-bearing page) versus the
duplication cost of not doing it. Extend the same question to the image
strip's config-save/recreate mechanism: is there already a shared piece to
build on, or would Movies and the image strip end up as two separate
implementations of the same idea?

## Constraints

- Reuse `SelectionTable`, `sortRows.ts`, `CollapsiblePanel`,
  `useColumnResize` as they exist post-#505. If a requirement doesn't fit
  what those already support, extend them the same way #505 did — pull the
  new behaviour out as a shared piece if another page could plausibly want
  it, rather than growing something Movies-page-specific.
- Metadata that doesn't exist today (star, category, tag, rename target)
  needs a storage decision: check how the existing `note` field persists
  (PR #505 treats it as an existing per-movie field) and extend that
  mechanism rather than introducing a second one, unless there's a reason
  the existing mechanism can't hold this.
- Renaming and deleting touch the filesystem — check what else references a
  movie by filename/path (e.g. anything in Julia that lists or loads movies
  by name) so a rename/delete from the UI can't silently break another
  access path.
- Don't assume the current data model for a "movie" — read how movies are
  currently represented (row shape, backing fields) before deciding where
  star/category/rename fit into it.

## Ask

- Give the ImageTable-vs-shared-table opinion above, with reasoning.
- Audit `ImageTable` for existing rename/delete/star/category-like
  behaviour and report what's already generalisable vs. table-specific.
- Audit the analysis board's image strip config-save/recreate mechanism
  and report whether it should be generalised into the same mechanism
  Movies uses, extended, or left separate — with reasoning.
- Propose where category/tag and star should live in the data model, and
  whether category is fixed-enum or free-form, based on what's easiest to
  extend without code changes as new movie kinds appear.
- Identify every place a movie is referenced by filename/path outside the
  Movies page, so rename/delete can update or safely invalidate those.
- Sketch the component breakdown (what's new vs. extended from #505's
  shared parts) before writing implementation.
- Flag anything that should be a separate, smaller PR rather than one large
  change — #505's own reservations note `ModuleLayout`-style refactors carry
  wide blast radius; a management component touching data model, filesystem
  ops, and UI at once is a candidate for splitting.
- Determine the actual configuration parameter set and versioning approach
  by reading the animation and batch movie pages' generation code and the
  napari bridge's existing version-handling — don't assume either.
