# Cecelia installer — Windows (PowerShell 5+).
#
# Installs Pixi and Julia (juliaup) if missing, downloads the latest Cecelia release (ships a
# prebuilt frontend, so no Node needed), provisions the environment, and adds a Start Menu
# shortcut. Re-runnable.
#
#   powershell -ExecutionPolicy Bypass -c "irm https://raw.githubusercontent.com/schienstockd/cecelia/main/install.ps1 | iex"
#
# NOTE: authored on Linux and not yet verified on Windows hardware — first real test is a
# Windows CI runner. See docs/SHIPPING.md.
#
# Env overrides:  $env:CECELIA_VERSION = 'v0.1.0' ;  $env:CECELIA_HOME = 'C:\path'
$ErrorActionPreference = 'Stop'

$Repo       = 'schienstockd/cecelia'
$InstallDir = if ($env:CECELIA_HOME) { $env:CECELIA_HOME } else { Join-Path $env:LOCALAPPDATA 'cecelia' }
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

# ── Download the release bundle ────────────────────────────────────────────────
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
$Tmp = New-TemporaryFile
Say "Downloading $Url"
Invoke-WebRequest -Uri $Url -OutFile $Tmp

Say "Installing to $InstallDir"
if (Test-Path $InstallDir) { Remove-Item -Recurse -Force $InstallDir }
New-Item -ItemType Directory -Force -Path $InstallDir | Out-Null
tar -xzf $Tmp -C $InstallDir    # bsdtar ships with Windows 10+
Remove-Item $Tmp

# ── Provision ───────────────────────────────────────────────────────────────────
Push-Location $InstallDir
Say 'Installing the Python environment (downloads a few GB on first run)...'
& $Pixi install
Say 'Precompiling Julia (a few minutes on first run)...'
& $Julia --project=api -e 'using Pkg; Pkg.instantiate()'
Pop-Location

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
