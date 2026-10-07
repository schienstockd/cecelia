# Cecelia uninstaller - Windows. The counterpart of uninstall.sh; same rules, same defaults.
#
# Removes the install (app, Python env and, for a system install, its shared Pixi + Julia), the Start
# Menu shortcut and the Claude observer registration. Your settings (%USERPROFILE%\.cecelia) and your
# projects are KEPT unless you ask for them to go - it asks in the console.
#
#   powershell -ExecutionPolicy Bypass -File "$env:LOCALAPPDATA\cecelia\uninstall.ps1"
#   powershell -ExecutionPolicy Bypass -c "irm https://raw.githubusercontent.com/schienstockd/cecelia/main/uninstall.ps1 | iex"
#
# Options - arguments with -File, or the env var for irm | iex:
#   -WipeSettings  [CECELIA_WIPE_SETTINGS=1]  also delete %USERPROFILE%\.cecelia (settings, profiles
#                                            incl. Claude logins, models, custom modules)
#   -WipeProjects  [CECELIA_WIPE_PROJECTS=1]  also delete your projects - each <projects>\<uid>\ with a
#                                            project.json; anything else in that folder is left alone
#   -DataOnly                                 keep the install, remove only your own data
#   -Yes           [CECELIA_YES=1]            don't ask; do exactly what the flags say
#   $env:CECELIA_HOME / $env:CECELIA_INSTALL_SCOPE='system' pick the install, as for install.ps1.
#
# Never removed: Pixi (~\.pixi), Julia (~\.juliaup, ~\.julia), caches and Claude (~\.claude*) - other
# software uses them. A system install (Program Files, needs an elevated shell) is removed for every
# account; other accounts' settings and projects are never touched.
# Design: docs/todo/INSTALL_OWNER_UNINSTALL_PLAN.md (D6).
$ErrorActionPreference = 'Stop'

function Say($m) { Write-Host "[cecelia] $m" -ForegroundColor Cyan }
function Ask($q) {
  if ($Yes) { return $false }
  $a = Read-Host "[cecelia] $q [y/N]"
  return ($a -match '^(y|yes)$')
}
function SizeOf($p) {
  $b = (Get-ChildItem -LiteralPath $p -Recurse -Force -File -ErrorAction SilentlyContinue | Measure-Object Length -Sum).Sum
  if (-not $b) { return '0 MB' }
  if ($b -ge 1GB) { return ('{0:N1} GB' -f ($b / 1GB)) }
  return ('{0:N0} MB' -f ($b / 1MB))
}
function Remove-Path($p) {
  if (Test-Path -LiteralPath $p) { Remove-Item -LiteralPath $p -Recurse -Force; Write-Host "    removed $p" }
}

$WipeSettings = $env:CECELIA_WIPE_SETTINGS -eq '1'
$WipeProjects = $env:CECELIA_WIPE_PROJECTS -eq '1'
$Yes          = $env:CECELIA_YES -eq '1'
$DataOnly     = $false
foreach ($a in $args) {
  switch ($a) {
    '-WipeSettings' { $WipeSettings = $true }
    '-WipeProjects' { $WipeProjects = $true }
    '-DataOnly'     { $DataOnly = $true }
    '-Yes'          { $Yes = $true }
    default         { throw "Unknown option: $a" }
  }
}

$ConfigDir = Join-Path $env:USERPROFILE '.cecelia'
function Expand-Tilde($p) {
  if ($p -eq '~' -or $p.StartsWith('~/') -or $p.StartsWith('~\')) {
    return (Join-Path $env:USERPROFILE ($p.Substring(1).TrimStart('/', '\')))
  }
  return $p
}
function Test-Install($d) {
  ($d) -and (Test-Path -LiteralPath (Join-Path $d '.cecelia-version'))
}

# ── Find the install ──────────────────────────────────────────────────────────
$UserDefault   = Join-Path $env:LOCALAPPDATA 'cecelia'
$SystemDefault = Join-Path $env:ProgramFiles 'cecelia'
$ScriptDir     = if ($PSScriptRoot) { $PSScriptRoot } else { $null }
$InstallDir = $null
if ($env:CECELIA_HOME)                        { $InstallDir = Expand-Tilde $env:CECELIA_HOME }
elseif (Test-Install $ScriptDir)              { $InstallDir = $ScriptDir }
elseif ($env:CECELIA_INSTALL_SCOPE -eq 'system') { $InstallDir = $SystemDefault }
elseif ($env:CECELIA_INSTALL_SCOPE -eq 'user')   { $InstallDir = $UserDefault }
elseif (Test-Install $UserDefault)            { $InstallDir = $UserDefault }
elseif (Test-Install $SystemDefault)          { $InstallDir = $SystemDefault }

if ($DataOnly) {
  $InstallDir = $null
} elseif (-not $InstallDir -or -not (Test-Path -LiteralPath $InstallDir)) {
  Say "No Cecelia install found - only your own data will be considered."
  $InstallDir = $null
} else {
  if (Test-Path -LiteralPath (Join-Path $InstallDir '.git')) { throw "$InstallDir is a git checkout, not an install - not removing it." }
  if (-not (Test-Install $InstallDir)) { throw "$InstallDir does not look like a Cecelia install (no .cecelia-version) - not removing it." }
}
$Scope = 'user'
if ($InstallDir) {
  $sf = Join-Path $InstallDir '.cecelia-scope'
  if ((Test-Path -LiteralPath $sf) -and ((Get-Content -LiteralPath $sf -Raw).Trim() -eq 'system')) { $Scope = 'system' }
}
if ($Scope -eq 'system') {
  $admin = ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
  if (-not $admin) { throw "$InstallDir is a system-wide install - run this from an elevated (Administrator) PowerShell." }
}

# ── Refuse while it runs ──────────────────────────────────────────────────────
# Deleting a live env corrupts whatever the server is writing. Its julia/python/pixi binaries live
# under the install (system scope) or its .pixi env (both scopes).
if ($InstallDir) {
  $prefix = $InstallDir.TrimEnd('\') + '\'
  $running = Get-Process -ErrorAction SilentlyContinue |
    Where-Object { $_.Path -and $_.Path.StartsWith($prefix, [StringComparison]::OrdinalIgnoreCase) }
  if ($running) {
    throw "Cecelia is still running from $InstallDir - quit it (Settings -> Shut down) and run this again. ($(($running | ForEach-Object { "$($_.Id) $($_.ProcessName)" }) -join ', '))"
  }
}

# ── Projects dir (read before settings can go) ────────────────────────────────
# `[dirs] projects = "..."` in custom.toml; "/path/to/projects" = the setup wizard never ran.
$ProjectsDir = $null
$toml = Join-Path $ConfigDir 'custom.toml'
if (Test-Path -LiteralPath $toml) {
  $section = ''
  foreach ($line in Get-Content -LiteralPath $toml -Encoding UTF8) {
    if ($line -match '^\s*\[\s*([^\]]+?)\s*\]') { $section = $Matches[1]; continue }
    if ($section -eq 'dirs' -and $line -match '^\s*projects\s*=\s*["''](.*?)["'']\s*(#.*)?$') {
      $ProjectsDir = Expand-Tilde ($Matches[1] -replace '\\\\', '\'); break
    }
  }
  if ($ProjectsDir -eq '/path/to/projects') { $ProjectsDir = $null }
}
$Projects = @()
if ($ProjectsDir -and (Test-Path -LiteralPath $ProjectsDir)) {
  $Projects = @(Get-ChildItem -LiteralPath $ProjectsDir -Directory -Force |
    Where-Object { Test-Path -LiteralPath (Join-Path $_.FullName 'project.json') })
}

# ── What's here ──────────────────────────────────────────────────────────────
Say 'Found:'
if ($InstallDir) { Write-Host "    install    $InstallDir ($Scope scope, $(SizeOf $InstallDir))" }
if (Test-Path -LiteralPath $ConfigDir) { Write-Host "    settings   $ConfigDir ($(SizeOf $ConfigDir))" }
if ($ProjectsDir) { Write-Host "    projects   $($Projects.Count) in $ProjectsDir ($(SizeOf $ProjectsDir))" }
if (-not $InstallDir -and -not (Test-Path -LiteralPath $ConfigDir) -and -not $ProjectsDir) { Say 'Nothing to remove.'; return }

# ── Decide ───────────────────────────────────────────────────────────────────
if ($InstallDir -and -not $Yes) {
  if (-not (Ask "Remove Cecelia from ${InstallDir}?")) { Say 'Nothing removed.'; return }
}
if (-not $WipeSettings -and (Test-Path -LiteralPath $ConfigDir) -and -not $Yes) {
  $WipeSettings = Ask "Also delete your settings, profiles and models in ${ConfigDir}? (No keeps them for a reinstall)"
}
if ($Projects.Count -gt 0 -and -not $Yes) {
  if ($WipeProjects -or (Ask "Also delete your $($Projects.Count) project(s) in ${ProjectsDir}? (No keeps them)")) {
    $c = Read-Host "[cecelia] This permanently deletes $($Projects.Count) project(s) - analysis results, notebooks, lab logs. Type delete to confirm"
    $WipeProjects = ($c -eq 'delete')
    if (-not $WipeProjects) { Say 'Projects kept.' }
  }
}

# ── Remove the install ───────────────────────────────────────────────────────
if ($InstallDir) {
  # The observer MCP registration - only one pointing into THIS install, via the CLI as the app did.
  $claudeJson = Join-Path $env:USERPROFILE '.claude.json'
  if ((Get-Command claude -ErrorAction SilentlyContinue) -and (Test-Path -LiteralPath $claudeJson) -and
      (Select-String -LiteralPath $claudeJson -Pattern 'cecelia-observer' -SimpleMatch -Quiet)) {
    $info = (& claude mcp get cecelia-observer 2>$null) -join "`n"
    if ($info -and $info.Contains($InstallDir)) {
      & claude mcp remove cecelia-observer -s user *> $null
      Write-Host '    removed the cecelia-observer registration from Claude'
    }
  }
  Say "Removing $InstallDir..."
  Remove-Path $InstallDir
  $programs = if ($Scope -eq 'system') { [Environment]::GetFolderPath('CommonPrograms') } else { [Environment]::GetFolderPath('Programs') }
  if ($programs) { Remove-Path (Join-Path $programs 'Cecelia.lnk') }
}

# ── Your data ─────────────────────────────────────────────────────────────────
Remove-Path (Join-Path $ConfigDir 'julia-depot')   # per-user Julia cache for a shared install - always
if ($WipeProjects -and $Projects.Count -gt 0) {
  Say "Deleting $($Projects.Count) project(s)..."
  foreach ($p in $Projects) { Remove-Path $p.FullName }
  if (-not (Get-ChildItem -LiteralPath $ProjectsDir -Force)) { Remove-Path $ProjectsDir }
  else { Say "Kept $ProjectsDir - it holds other files besides Cecelia projects." }
}
if ($WipeSettings -and (Test-Path -LiteralPath $ConfigDir)) { Say 'Deleting settings...'; Remove-Path $ConfigDir }

# ── Summary ──────────────────────────────────────────────────────────────────
Say 'Done.'
if (Test-Path -LiteralPath $ConfigDir) { Write-Host "    kept settings  $ConfigDir   (remove: -DataOnly -WipeSettings)" }
if (-not $WipeProjects -and $Projects.Count -gt 0) { Write-Host "    kept projects  $ProjectsDir   (remove: -DataOnly -WipeProjects)" }
$shared = @('.pixi', '.juliaup', '.julia') | ForEach-Object { Join-Path $env:USERPROFILE $_ } |
  Where-Object { Test-Path -LiteralPath $_ }
if ($shared) {
  Write-Host '    Left in place - shared with other software, delete by hand if nothing else needs them:'
  foreach ($s in $shared) { Write-Host "      $s ($(SizeOf $s))" }
}
if ($Scope -eq 'system') { Write-Host "    Other accounts' settings and projects are untouched; each can run this with -DataOnly." }
