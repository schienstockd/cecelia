@echo off
rem Windows counterpart of activate_juliaup.sh, run by pixi on every `pixi run` / `pixi shell` (see
rem [target.win.activation] in pixi.toml). Same order as `_find_julia` in app.py: the install's own
rem juliaup, then the Julia the installer recorded, then PATH. Without it, `prod`, `doctor`,
rem `julia-instantiate` from a terminal ran whichever `julia` PATH offered, not the one `Pkg.instantiate`
rem ran under, and system scope's terminal tasks missed its shared juliaup and depot.
rem
rem No parenthesised block may expand %PATH%: a "C:\Program Files (x86)" entry ends the block early.
rem Hence the gotos. Variables set here are the env pixi captures, so no setlocal.
rem Paths are compared in their 8.3 short form, so a long and a short spelling of one file (C:\Users\
rem runneradmin vs C:\Users\RUNNER~1) still match.

set "_cecelia_first="
for /f "delims=" %%J in ('where julia 2^>nul') do if not defined _cecelia_first set "_cecelia_first=%%~sJ"

rem A juliaup inside the install (`<root>\juliaup`, always there for system scope) wins, with juliaup
rem pointed at its own state. Its shared depot is read-only to every account but the owner: a per-user
rem writable depot goes in front, and the trailing empty entry keeps Julia's bundled stdlib depot.
rem Same shape as cecelia-launch.cmd, which install.ps1 writes.
set "_cecelia_juliaup=%PIXI_PROJECT_ROOT%\juliaup"
if not exist "%_cecelia_juliaup%\bin\julia.exe" goto :cecelia_recorded
set "JULIAUP_DEPOT_PATH=%_cecelia_juliaup%"
if exist "%_cecelia_juliaup%\depot\" set "JULIA_DEPOT_PATH=%USERPROFILE%\.cecelia\julia-depot;%_cecelia_juliaup%\depot;"
set "_cecelia_dir=%_cecelia_juliaup%\bin"
for %%F in ("%_cecelia_dir%\julia.exe") do if /i "%%~sF"=="%_cecelia_first%" goto :cecelia_done
set "PATH=%_cecelia_dir%;%PATH%"
goto :cecelia_done

rem Otherwise the Julia the installer recorded beats PATH's. install.ps1 records the `julia` PATH offered
rem at install time, so usually they match and nothing changes. When they don't, its dir can't go first
rem on PATH (WindowsApps also holds the Store's python.exe, which would shadow the pixi env's), so a
rem per-user dir holding only a `julia.cmd` that runs %CECELIA_JULIA% goes first. If that dir can't be
rem made, the recorded dir is appended, and only when no `julia` is on PATH at all.
:cecelia_recorded
if not exist "%PIXI_PROJECT_ROOT%\.cecelia-julia" goto :cecelia_done
set "_cecelia_julia="
set /p _cecelia_julia=<"%PIXI_PROJECT_ROOT%\.cecelia-julia"
if not defined _cecelia_julia goto :cecelia_done
if not exist "%_cecelia_julia%" goto :cecelia_done
for %%F in ("%_cecelia_julia%") do if /i "%%~sF"=="%_cecelia_first%" goto :cecelia_done
set "CECELIA_JULIA=%_cecelia_julia%"
set "_cecelia_dir=%USERPROFILE%\.cecelia\julia-shim"
rem sourced again (nested `pixi shell`)
for %%F in ("%_cecelia_dir%\julia.cmd") do if /i "%%~sF"=="%_cecelia_first%" goto :cecelia_done
if exist "%_cecelia_dir%\julia.cmd" goto :cecelia_shim
mkdir "%_cecelia_dir%" 2>nul
rem Written aside and moved in, so a concurrent `pixi run` never runs a half-written shim.
set "_cecelia_tmp=%_cecelia_dir%\julia.cmd.%RANDOM%"
2>nul >"%_cecelia_tmp%" echo @"%%CECELIA_JULIA%%" %%*
2>nul >>"%_cecelia_tmp%" echo @exit /b %%ERRORLEVEL%%
move /y "%_cecelia_tmp%" "%_cecelia_dir%\julia.cmd" >nul 2>nul
if exist "%_cecelia_tmp%" del "%_cecelia_tmp%" 2>nul
if exist "%_cecelia_dir%\julia.cmd" goto :cecelia_shim
if defined _cecelia_first goto :cecelia_done
for %%D in ("%_cecelia_julia%") do set "PATH=%PATH%;%%~dpD"
goto :cecelia_done

:cecelia_shim
set "PATH=%_cecelia_dir%;%PATH%"

:cecelia_done
set "_cecelia_first="
set "_cecelia_juliaup="
set "_cecelia_julia="
set "_cecelia_dir="
set "_cecelia_tmp="
