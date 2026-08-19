# ccia-importTracks

A [Cecelia](https://github.com/schienstockd/cecelia) **plugin**: import tracks produced in another
tool — ImageJ Manual Tracking, TrackMate, Imaris — and attach them to a Cecelia segmentation.

## Install

```bash
mkdir -p ~/.cecelia/modules/plugins
git clone https://github.com/schienstockd/ccia-importTracks ~/.cecelia/modules/plugins/ccia-importTracks
```

Then restart the server, or Settings → Custom modules → **Reload**. (A *new* plugin loads on reload;
editing an already-loaded `.jl` needs a restart — Julia cannot redefine a struct in place.)

## What it does

An external tracker knows nothing about Cecelia's labels, so there is no id to join on. Each cell
takes the track of the nearest spot **in its own frame**, within a distance cutoff. Cells with nothing
inside the cutoff stay `-1` — untracked is a real answer, since the two tools segmented and tracked
independently.

Nothing about a file format is hard-coded. The importer needs four things — track id, frame, X and Y
(Z if 3D) — supplied as a **column mapping**. `templates/` ships ready-made mappings and anything you
set on the form overrides them, so an unlisted tool works by mapping its columns once and a
nearly-matching one works by fixing a single field. Supporting a new source is a new template file,
not new code.

> The shipped templates are inferred from each tool's documented output and are **not yet verified
> against a real export**. `Track n°` carries a non-ASCII degree sign and ImageJ does not reliably
> write UTF-8; Imaris's preamble length has varied between versions. Check one against a real file.

## Not sandboxed

A plugin's Julia is `Base.include`d into the `Cecelia` module with full access to your machine,
exactly like a module you drop in yourself. Installing one is trusting whoever wrote it.

## Licence

GPL-3.0-or-later, matching Cecelia.
