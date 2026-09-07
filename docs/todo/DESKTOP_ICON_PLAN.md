# Desktop-shortcut feijoa icon

**Status:** planning (2026-09-08). Brief: `docs/archive/feijoa-icon-investigation.md`.

## Goal

`install.sh` and `install.ps1` each write a launcher entry at the end of provisioning. Give that
entry a feijoa icon on all three platforms, from one source asset, with the smallest possible
change to the release pipeline. Installation is a plain shell/PowerShell script — no Tauri, no
Electron.

## How macOS gets validated without a Mac on the desk

There is no macOS host in the maintainer's workspace, but `ci.yml` already runs `macos-latest`
alongside ubuntu/windows in three matrix jobs, and `ci.yml:11` records that GitHub-hosted runners
are free on all OSes for this public repo. macOS validation moves into a **new CI job** (Decision
6) that untars a release bundle, runs `install.sh` in a scratch dir, then checks bundle structure
(`plutil -lint`, `sips -g` on the `.icns`, `lsregister -dump`) and uploads a `screencapture` of a
Finder window as an artefact. Merge gated on green CI + one human eyeball on the screenshot per
PR that touches the macOS path.

One thing that still needs a real human on a real Mac, once ever, at release time: **Gatekeeper's
first-launch dialog** for an unsigned `.app` (wording, right-click-to-Open bypass). That is
independent of icons and would be true for any first `.app` we ship.

## Current state (2026-09-08)

- **Source of truth.** `frontend/public/feijoa.svg` is the brand mark used by `index.html`,
  `AppHeader.vue`, `SetupModule.vue`, `WelcomeModule.vue`. `favicon.svg` (purple lightning) is a
  stale artefact — nothing in the app references it, only `install.sh` does.
- **Linux** (`install.sh:232`). Writes a `.desktop` with `Icon=$INSTALL_DIR/frontend/dist/favicon.svg`
  — points at the **wrong** file (stale mark). Bug, not just a gap.
- **macOS** (`install.sh:267-301`). Writes a bare `Cecelia.command` shell script. No bundle, no
  `Info.plist`, no icon slot. Finder shows the generic script icon.
- **Windows** (`install.ps1:225-240`). `WScript.Shell.CreateShortcut(...)`. `$Shortcut.IconLocation`
  never set — Start Menu falls back to pixi.exe's own icon (or a generic one for the wrapper `.cmd`).
- **Release pipeline** (`.github/workflows/release.yml`). Single `runs-on: ubuntu-latest` job. No
  macOS runner. `iconutil` and `sips` are unavailable. The tarball uses an ALLOW-list — anything not
  named is silently absent (comment in `release.yml` cites #540 for exactly this failure mode).
  `frontend/dist` IS in the allow-list; Vite copies `frontend/public/**` into `dist/` verbatim.

## Locked decisions (2026-09-08)

1. **One SVG source of truth: `frontend/public/feijoa.svg`.** Everything else is derived. When the
   mark changes, only this file changes.

2. **Derived rasters live under `frontend/public/icons/` and are committed.** Since Vite copies
   `public/**` into `dist/`, they ride the existing `frontend/dist` line in the tarball allow-list
   — no `release.yml` edit needed. Committing them (rather than generating in CI) means:
   - the release job stays on `ubuntu-latest` with no new dep;
   - the `dev` channel (a full branch archive) has them too, without a build step;
   - regeneration is a local, on-demand script — the SVG changes rarely.

   Trade-off surfaced: this adds ~10-30 KB of binary blobs to the repo. Acceptable given the
   pipeline is single-job and the assets change ~never.

3. **Regeneration script: `scripts/regen-icons.sh`.** One command, reproducible, documented in
   `docs/DEV.md`. Uses ImageMagick (`convert`) + Pillow (both already available in the pixi env /
   on any dev machine — Pillow 10+ writes both `.ico` and `.icns` natively; no `iconutil`, no
   `png2icns`, no macOS host required). Committed derived assets have a header comment in the
   script naming the source SHA to make drift detectable.

4. **Linux .desktop: raw path, no hicolor install.** `Icon=` accepts a filesystem path; that's
   sufficient for the launcher-menu entry itself. The *taskbar* icon comes from the running
   process's `_NET_WM_ICON`, which is the **browser**'s icon (Cecelia launches the user's default
   browser — see [`docs/SHIPPING.md`](../SHIPPING.md)). Installing into
   `~/.local/share/icons/hicolor/` would not change that; skip it.

5. **macOS: minimal `Cecelia.app` bundle, not `SetFile -a C` on the `.command`.**
   `Cecelia.command` cannot carry an icon that survives Finder cache flushes without a fragile
   resource-fork dance, and the runtime process is still Terminal. A minimal `.app`
   (`Contents/MacOS/cecelia`, `Contents/Info.plist`, `Contents/Resources/cecelia.icns`) gets us the
   Finder icon *and* the Dock icon while the launcher is running. The `MacOS/cecelia` executable is
   the same shell payload we currently put in `Cecelia.command` — the bundle is a wrapper, not a
   rewrite. See Phase 4.

6. **macOS verification runs on `macos-latest` in CI, not by hand.** New workflow (or new job on an
   existing one) that untars a release bundle, runs `install.sh` in a scratch dir, structurally
   validates the `.app`, and uploads a Finder screenshot as an artefact. Merge of Phase 4 gated on
   green CI + one human eyeball on the screenshot per PR. See Phase 4 for the exact checks. Cost
   is zero minutes (public repo, `ci.yml:11`).

6. **Windows: standalone `.ico`, not `pixi.exe`'s embedded icon.** `$Shortcut.IconLocation` points
   at the shipped `cecelia.ico` so the shortcut doesn't depend on pixi's own resources (which
   change on pixi upgrades). Multi-resolution `.ico` (16/32/48/64/128/256).

7. **No change to the `dev` channel local frontend build.** `npm run build` still runs on the
   author's box; `frontend/public/icons/**` is already there and gets copied like any other public
   asset. No new script, no new build step.

## Source-asset generation

`scripts/regen-icons.sh` (new, ~30 lines):

```
in=frontend/public/feijoa.svg
out=frontend/public/icons

# Rasterise the SVG at the largest size we need (once), then downsample.
convert -background none -density 1024 -resize 1024x1024 "$in" "$out/cecelia-1024.png"

# Individual sizes (kept as loose PNGs for the Linux .desktop).
for s in 16 32 48 64 128 256 512; do
  convert "$out/cecelia-1024.png" -resize ${s}x${s} "$out/cecelia-${s}.png"
done

# .ico (multi-res, Windows).
python -c "
from PIL import Image
img = Image.open('$out/cecelia-1024.png')
img.save('$out/cecelia.ico', sizes=[(16,16),(32,32),(48,48),(64,64),(128,128),(256,256)])
"

# .icns (multi-res, macOS). Pillow 10+ writes .icns natively.
python -c "
from PIL import Image
img = Image.open('$out/cecelia-1024.png')
img.save('$out/cecelia.icns')
"
```

Committed outputs, all under `frontend/public/icons/`:
`cecelia-16.png`, `-32.png`, `-48.png`, `-64.png`, `-128.png`, `-256.png`, `-512.png`,
`cecelia.ico`, `cecelia.icns`. The `-1024.png` intermediate is not committed.

After Vite build, these appear at `frontend/dist/icons/*` and travel in the tarball via the
existing `frontend/dist` entry in `release.yml`.

## Per-platform plan

### Linux (`install.sh`, both scopes)

Change `install.sh:232` from `ICON="$INSTALL_DIR/frontend/dist/favicon.svg"` to:

```
ICON="$INSTALL_DIR/frontend/dist/icons/cecelia-256.png"
```

Rationale: (a) fixes the bug (stale mark); (b) prefers a rasterised size over the SVG because a
handful of older desktop environments still don't parse SVG icons in `.desktop` entries reliably.
The 256-px PNG renders fine at every menu size. No other change to either scope's `.desktop`
writer.

### macOS (`install.sh`, both scopes)

Replace both `.command` writers with a minimal `.app` bundle. Structure:

```
Cecelia.app/
  Contents/
    Info.plist
    MacOS/
      cecelia          # the current .command payload, chmod 755
    Resources/
      cecelia.icns
```

`Info.plist` (minimum viable — 12 lines):

```
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>CFBundleName</key><string>Cecelia</string>
  <key>CFBundleDisplayName</key><string>Cecelia</string>
  <key>CFBundleIdentifier</key><string>org.cecelia.launcher</string>
  <key>CFBundleVersion</key><string>1</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleExecutable</key><string>cecelia</string>
  <key>CFBundleIconFile</key><string>cecelia.icns</string>
</dict></plist>
```

- System scope: `/Applications/Cecelia.app`, root-owned, world-executable. Same shell payload as
  today (`exec "$LAUNCH"`), just placed at `Contents/MacOS/cecelia`.
- User scope: `~/Applications/Cecelia.app`, same shape.
- `cecelia.icns` copied in from `$INSTALL_DIR/frontend/dist/icons/cecelia.icns`.
- Both scopes: `touch "$APP"` on the bundle after writing so LaunchServices notices the change on
  re-install (stale icon cache is the classic gotcha).
- Migration: the installer is re-runnable and idempotent, so a v-next `install.sh` also `rm -f`s
  the old `Cecelia.command` at both scopes before writing the `.app`, otherwise both entries
  appear side by side.

### Windows (`install.ps1`, both scopes)

Add one line to each of the two `CreateShortcut` blocks (`install.ps1:230` and `:239`), before the
`.Save()` call:

```
$Shortcut.IconLocation = (Join-Path $InstallDir 'frontend\dist\icons\cecelia.ico')
```

Nothing else changes — `.lnk` supports absolute `IconLocation` paths; no manifest, no bundle.
Windows caches shortcut icons aggressively, so document (in `docs/INSTALL.md`) that a Start-Menu
re-index (`ie4uinit.exe -show`) may be needed for pre-existing installs; new installs are fine.

## Phases

- **P1 — Assets in the tree.** Write `scripts/regen-icons.sh`, run it, commit the outputs under
  `frontend/public/icons/`. Add a `docs/DEV.md` line under *Repository layout* pointing at the
  script. Verify `npm run build` copies them into `frontend/dist/icons/`. **No installer change
  yet** — this phase alone ships zero user-visible change and is safe to merge on its own.

- **P2 — Linux fix.** One-line change to `install.sh:232`. Manually re-run the installer against
  a scratch `$INSTALL_DIR` on a Linux dev box, confirm the app-menu entry shows the feijoa.

- **P3 — Windows.** One-line change per scope in `install.ps1`. Cannot be verified from a Linux dev
  box directly; needs a screenshot from someone who already runs a Windows install (or the
  existing `windows-latest` matrix in `ci.yml` growing a screencapture step, parallel to P4's
  macOS job — same pattern, cheap if we already build the macOS one). Flag as **needs Windows
  sanity check before release** in the PR.

- **P4 — macOS bundle + CI verification.** Two parts, land together (installer change alone would
  ship unverified, verification job alone would have nothing to verify).
  - *Installer:* replace both `.command` writers with `.app`-bundle writers per the *macOS*
    section above; add the `rm -f` of any pre-existing `Cecelia.command` at both scopes.
  - *CI:* new job on `macos-latest` (either a new step in `ci.yml`'s existing macOS matrix or a
    new short workflow triggered on paths `install.sh`, `frontend/public/icons/**`,
    `.github/workflows/verify-macos.yml`). Steps:
    1. Untar a locally built bundle (`npm run build` + `tar` of the same file list as
       `release.yml`), OR skip the tar and run `install.sh` directly against the checked-out tree
       — whichever is simpler; both are the same script from the installer's point of view.
    2. `install.sh` into `$RUNNER_TEMP/cecelia`, user scope.
    3. `test -d ~/Applications/Cecelia.app/Contents/{MacOS,Resources}` and
       `test -x ~/Applications/Cecelia.app/Contents/MacOS/cecelia`.
    4. `plutil -lint ~/Applications/Cecelia.app/Contents/Info.plist`.
    5. `sips -g pixelWidth -g pixelHeight ~/Applications/Cecelia.app/Contents/Resources/cecelia.icns`
       (asserts the icns is well-formed and multi-resolution).
    6. `/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister
       -f ~/Applications/Cecelia.app` then `lsregister -dump | grep -A2 org.cecelia.launcher` —
       confirms LaunchServices actually picked it up.
    7. `open -R ~/Applications/Cecelia.app` (reveals in Finder), give it 2 s to render, then
       `screencapture -x $RUNNER_TEMP/cecelia-finder.png`, and
       `actions/upload-artifact` the PNG.
  - *Merge bar:* CI green + one human eyeball on the uploaded PNG showing the feijoa on the
    bundle. Automate what can be automated; keep the visual check for what can't.
  - *One-off human check at release time, not per PR:* Gatekeeper's first-launch dialog on a real
    unsigned `.app`. Independent of icons — would be needed the first time we ship a `.app` at
    all. Document the observed wording (and right-click-to-Open bypass) in `docs/INSTALL.md` once
    checked.

Order matters: P1 must land before P2/P3/P4 (they depend on the assets being present in
`frontend/dist/`). P2, P3, P4 are independent and can be one PR each or bundled.

## Open questions

- **Do we care about the browser tab's runtime taskbar icon?** No — see Decision 4. The browser
  owns that, and we already ship `feijoa.svg` as the `<link rel="icon">`. Called out here so we
  don't get re-asked.
- **Should the Windows shortcut's icon point at `%LOCALAPPDATA%\cecelia\frontend\dist\icons\cecelia.ico`
  (an absolute path) or a copy at a stable location like `%LOCALAPPDATA%\cecelia\cecelia.ico`?**
  Absolute path into the install tree is fine and matches how Linux does it; the shortcut breaks if
  the install tree is moved, which is already true of `TargetPath`. Not worth a separate copy.
- **Should P3 grow a `windows-latest` screencapture job mirroring P4's?** Cheap once the pattern
  exists (same free-runner story). Deferred to a follow-up rather than blocking P3 — a real
  Windows user is the current baseline check.
