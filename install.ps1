# Cecelia installer — Windows (PowerShell 5+).
#
# Installs Pixi and Julia (juliaup) if missing, downloads Cecelia, provisions the environment, and
# adds a Start Menu shortcut. Re-runnable.
#
# Two channels ($env:CECELIA_CHANNEL):
#   stable (default) — the latest tagged GitHub Release; ships a prebuilt frontend (no Node needed).
#   dev              — the current GitHub state (the `main` branch tarball); the frontend is built
#                      locally, so Node.js (npm) must be on PATH. Tracks HEAD without waiting for a
#                      tagged release — just re-run to update. See docs/SHIPPING.md.
#
#   powershell -ExecutionPolicy Bypass -c "irm https://raw.githubusercontent.com/schienstockd/cecelia/main/install.ps1 | iex"
#   powershell -ExecutionPolicy Bypass -c "$env:CECELIA_CHANNEL='dev'; irm https://raw.githubusercontent.com/schienstockd/cecelia/main/install.ps1 | iex"
#
# NOTE: authored on Linux and not yet verified on Windows hardware — first real test is a
# Windows CI runner. See docs/SHIPPING.md.
#
# Env overrides:  $env:CECELIA_CHANNEL = 'stable'|'dev' ;  $env:CECELIA_VERSION = 'v0.1.0' ;
#                 $env:CECELIA_BRANCH = 'main' ;  $env:CECELIA_HOME = 'C:\path'
$ErrorActionPreference = 'Stop'

$Repo       = 'schienstockd/cecelia'
$InstallDir = if ($env:CECELIA_HOME) { $env:CECELIA_HOME } else { Join-Path $env:LOCALAPPDATA 'cecelia' }
$Channel    = if ($env:CECELIA_CHANNEL) { $env:CECELIA_CHANNEL } else { 'stable' }
$Version    = if ($env:CECELIA_VERSION) { $env:CECELIA_VERSION } else { 'latest' }

function Say($m) { Write-Host "[cecelia] $m" -ForegroundColor Cyan }

# ── Pixi ──────────────────────────────────────────────────────────────────────
$Pixi = (Get-Command pixi -ErrorAction SilentlyContinue).Source
if (-not $Pixi) {
  $Pixi = Join-Path $env:USERPROFILE '.pixi\bin\pixi.exe'
  if (-not (Test-Path $Pixi)) {
    Say 'Installing Pixi...'
    powershell -ExecutionPolicy Bypass -c "irm -useb https://pixi.sh/install.ps1 | iex"
  }
}

# ── Julia (juliaup via winget / MS Store) ──────────────────────────────────────
$Julia = (Get-Command julia -ErrorAction SilentlyContinue).Source
if (-not $Julia -and -not (Test-Path (Join-Path $env:USERPROFILE '.juliaup\bin\julia.exe'))) {
  Say 'Installing Julia (juliaup)...'
  winget install --id 9NJNWW8PVKMN -e --source msstore --accept-package-agreements --accept-source-agreements
}
if (-not $Julia) { $Julia = Join-Path $env:USERPROFILE '.juliaup\bin\julia.exe' }

# ── Fetch Cecelia (release bundle, or branch source for the dev channel) ───────
if ($Channel -eq 'dev') {
  # Current GitHub state: a branch archive (source only — the frontend is built below). GitHub serves
  # any branch as a tarball at archive/refs/heads/<branch>.tar.gz, so no tag/release is needed.
  if (-not (Get-Command npm -ErrorAction SilentlyContinue)) {
    throw "The dev channel builds the frontend from source and needs Node.js (npm) on PATH. Install Node >= 20 and re-run, or use the default stable channel."
  }
  $Branch = if ($env:CECELIA_BRANCH) { $env:CECELIA_BRANCH } else { 'main' }
  $Url = "https://github.com/$Repo/archive/refs/heads/$Branch.tar.gz"
  Say "Resolving current $Branch commit..."
  $commit = Invoke-RestMethod -Uri "https://api.github.com/repos/$Repo/commits/$Branch" -Headers @{ 'User-Agent' = 'cecelia-installer' }
  $Sha = $commit.sha
  $Provenance = "dev @ $Branch $Sha"
} else {
  # GitHub's `releases/latest` endpoint only ever resolves to a NON-prerelease release, so while the
  # project is still on release candidates (v*-rcN, all marked prerelease) it 404s. Resolve the newest
  # published release ourselves via the API — it lists prereleases too, newest first.
  if ($Version -eq 'latest') {
    Say 'Resolving the latest release...'
    $rel = Invoke-RestMethod -Uri "https://api.github.com/repos/$Repo/releases" -Headers @{ 'User-Agent' = 'cecelia-installer' }
    $Version = $rel[0].tag_name
    if (-not $Version) { throw 'Could not resolve the latest release from the GitHub API.' }
    Say "Latest release is $Version"
  }
  $Url = "https://github.com/$Repo/releases/download/$Version/cecelia.tar.gz"
  $Provenance = $Version
}

$Tmp = New-TemporaryFile
Say "Downloading $Url"
Invoke-WebRequest -Uri $Url -OutFile $Tmp

Say "Installing to $InstallDir"
if (Test-Path $InstallDir) { Remove-Item -Recurse -Force $InstallDir }
New-Item -ItemType Directory -Force -Path $InstallDir | Out-Null
# A release bundle extracts flat (api\, app\, …); a branch archive wraps everything in one
# `<repo>-<branch>\` dir, so strip that leading component for the dev channel only. bsdtar ships
# with Windows 10+.
if ($Channel -eq 'dev') {
  tar -xzf $Tmp -C $InstallDir --strip-components=1
} else {
  tar -xzf $Tmp -C $InstallDir
}
Remove-Item $Tmp

# ── bioformats2raw (image import) ───────────────────────────────────────────────
# ~190 MB, so fetched here rather than shipped in the bundle. The app resolves it at
# <install>\bioformats2raw\bin (bioformats2raw_bin() in config.jl); Java comes from the Pixi env.
# Skipped if a system bioformats2raw is already on PATH (the app falls back to PATH).
if (Get-Command bioformats2raw -ErrorAction SilentlyContinue) {
  Say 'Using bioformats2raw already on PATH.'
} else {
  # Pinned version (reproducible installs — not their `latest`, so our import engine can't change
  # under us on an upstream release). Override with $env:CECELIA_BIOFORMATS2RAW_VERSION.
  $B2rVersion = if ($env:CECELIA_BIOFORMATS2RAW_VERSION) { $env:CECELIA_BIOFORMATS2RAW_VERSION } else { '0.12.1' }
  $B2rUrl = "https://github.com/glencoesoftware/bioformats2raw/releases/download/v$B2rVersion/bioformats2raw-$B2rVersion.zip"
  Say "Fetching bioformats2raw $B2rVersion (image import; ~190 MB)..."
  $b2rZip = Join-Path ([System.IO.Path]::GetTempPath()) ('b2r-' + [System.IO.Path]::GetRandomFileName() + '.zip')
  $b2rTmp = Join-Path ([System.IO.Path]::GetTempPath()) ('b2r-' + [System.IO.Path]::GetRandomFileName())
  Invoke-WebRequest -Uri $B2rUrl -OutFile $b2rZip
  Expand-Archive -Path $b2rZip -DestinationPath $b2rTmp -Force
  $b2rSrc = Get-ChildItem -Path $b2rTmp -Directory | Where-Object { $_.Name -like 'bioformats2raw-*' } | Select-Object -First 1
  Move-Item -Path $b2rSrc.FullName -Destination (Join-Path $InstallDir 'bioformats2raw')
  Remove-Item $b2rZip; Remove-Item -Recurse -Force $b2rTmp
  Say 'Installed bioformats2raw.'
}

# ── Provision ───────────────────────────────────────────────────────────────────
Push-Location $InstallDir
Say 'Installing the Python environment (downloads a few GB on first run)...'
& $Pixi install
Say 'Precompiling Julia (a few minutes on first run)...'
& $Julia --project=api -e 'using Pkg; Pkg.instantiate()'
# The dev channel ships source only — build the frontend the server serves (stable already has it).
if ($Channel -eq 'dev') {
  Say 'Building the frontend (dev channel)...'
  Push-Location (Join-Path $InstallDir 'frontend')
  # `npm install`, not `npm ci`: npm silently skips a platform-specific optional native dep
  # (@rolldown/binding-win32-x64-msvc, vite 8's bundler) when the lockfile was made on another OS
  # (npm/cli#4828) — `npm ci` would leave the Windows build without its native binding. See ci.yml.
  npm install
  npm run build
  Pop-Location
}
Pop-Location

# Record what was installed (channel + tag/commit) for provenance and bug reports.
Set-Content -Path (Join-Path $InstallDir '.cecelia-version') -Value $Provenance
Say "Installed: $Provenance"

# ── Start Menu shortcut ───────────────────────────────────────────────────────
$Lnk = Join-Path ([Environment]::GetFolderPath('Programs')) 'Cecelia.lnk'
$Shell = New-Object -ComObject WScript.Shell
$Shortcut = $Shell.CreateShortcut($Lnk)
$Shortcut.TargetPath       = $Pixi
$Shortcut.Arguments        = 'run app'
$Shortcut.WorkingDirectory = $InstallDir
$Shortcut.Save()
Say "Installed a 'Cecelia' Start Menu shortcut."

Say "Done. Launch Cecelia from the Start Menu, or run:  cd `"$InstallDir`"; & `"$Pixi`" run app"
