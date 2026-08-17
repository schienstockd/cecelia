# Example view profile

Copy `gating-behaviour.json` into `<config_dir>/profiles/` (`~/.cecelia/profiles/`, or
`$CECELIA_DEV_DIR/profiles/` in dev) and pick it in **Settings → Interface → View profile**.

You do not have to write one by hand — that panel's **Edit** button builds them. The file is the
storage format (so a profile can be copied between machines), not the authoring path.

- `label` — what the picker shows. Defaults to the filename stem.
- `items` — the route paths to show, **in order**. Group headings stay as they are; a group with no
  listed page disappears. Paths must match `frontend/src/main.ts`.

A profile only hides menu clutter: every hidden page still opens by URL, and guides that walk a hidden
page are flagged in the guide picker rather than removed.

See `docs/todo/VIEW_PROFILES_PLAN.md` and `docs/UI.md` → *View profiles*.
