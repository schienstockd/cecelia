> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. Current design lives in `docs/todo/DESKTOP_ICON_PLAN.md`.
>
> One premise the plan resolved differently from the brief:
> - The Linux "current state" says `Icon=` points at `favicon.svg`, which it does — but `favicon.svg`
>   is a stale purple lightning mark, not the feijoa (`feijoa.svg` is the brand mark used by
>   `index.html`, `AppHeader`, `SetupModule`, `WelcomeModule`). Linux is *wrong*, not just *generic*.
>
> The brief also asks how to handle the fact there is no macOS host on the maintainer's desk. The
> plan validates the `.app` bundle on a `macos-latest` CI job (free-tier runners, `ci.yml:11`) with
> structural checks + a Finder `screencapture` artefact, keeping Gatekeeper first-launch as a
> one-off human check at release time. No macOS runner in the release workflow itself.

# Investigate: feijoa icon for Cecelia desktop shortcuts

## Context

`install.sh` (Linux/macOS) and `install.ps1` (Windows) each build a launcher entry
at the end of provisioning. No Tauri — these are plain shell/PowerShell scripts
writing `.desktop` files, `.command` files, and `.lnk` shortcuts directly. This is
an investigation pass only — produce a plan, don't implement yet.

## Current state (as of this file)

- **Linux** (`install.sh`, both scopes): writes a `.desktop` file with an `Icon=`
  key already pointing at `$INSTALL_DIR/frontend/dist/favicon.svg`.
- **macOS** (`install.sh`, both scopes): writes a bare `Cecelia.command` shell
  script. No bundle, no `Info.plist`, no icon mechanism at all — Finder shows the
  generic script icon.
- **Windows** (`install.ps1`, both scopes): creates a `.lnk` via
  `WScript.Shell.CreateShortcut`. `$Shortcut.IconLocation` is never set — no icon
  mechanism currently used either.

## Task

For each platform, determine the concrete, minimal path to a feijoa icon on the
shortcut, and flag anything that complicates it:

### Linux
- Confirm `.desktop` `Icon=` accepts a raw file path vs. requiring install into an
  icon theme dir (`~/.local/share/icons/hicolor/...`) for taskbar/alt-tab
  resolution, not just the launcher entry itself.
- Decide source asset: repo-shipped PNG at a few standard sizes vs. a single SVG.
- Where does this asset live in the repo / release tarball so both `install.sh`
  scopes (user `~/.local/share/cecelia`, system `/opt/cecelia` or
  `/Applications/cecelia`) can find it after `tar -xzf`?

### macOS
- `Cecelia.command` is not an app bundle — a bare script can't carry a custom
  icon via any `Icon=`-style key. Two options to evaluate:
  1. Generate a minimal `Cecelia.app` bundle (`Contents/MacOS/cecelia`,
     `Contents/Info.plist`, `Contents/Resources/feijoa.icns`) in place of the
     `.command` file — needs an `.icns` built from source art (`iconutil` /
     `sips`), and the installer writing multi-line `Info.plist` + `chmod +x`
     scaffolding.
  2. Set the icon on the existing `.command` file directly (`SetFile -a C` /
     Finder's resource-fork-style icon, or `osascript` to set the icon) —
     check whether this is still reliable on current macOS, and whether it
     survives re-running the installer (file gets recreated each time).
  - Recommend one; note the system-scope case too (`/Applications/Cecelia.command`
    → `/Applications/Cecelia.app`, root-owned, world-executable).

### Windows
- `.lnk` shortcuts support `$Shortcut.IconLocation = "path\to\file.ico[,index]"`.
  Confirm this is enough — no bundle/manifest needed.
  - Note: an `.ico` can point at the launcher `.exe`/`.cmd` itself (falls back to
    a generic icon) or at a **separate** `.ico` file — prefer the latter so the
    icon doesn't depend on `pixi.exe`'s own resources.
- Same asset-location question as Linux: where does the `.ico` live under
  `%LOCALAPPDATA%\cecelia` / `%ProgramFiles%\cecelia` after extraction, for both
  scopes and both the direct-launch (user) and wrapper-launch (system) cases?

### Cross-cutting
- One source image (feijoa artwork) → three derived formats (SVG/PNG for Linux,
  `.icns` for macOS, `.ico` for Windows). Where should generation happen: checked
  into the repo pre-built, or generated at build/release time? Note any tooling
  needed (`iconutil`, `ImageMagick`, `png2icns`, etc.) and whether it's available
  in CI.
- Confirm none of this affects the `dev` channel's local frontend build step.

## Output

A short plan per platform (file paths, exact keys/commands to set, source-asset
requirements) plus a recommendation on where the icon asset(s) should live in the
repo. No code changes yet.
