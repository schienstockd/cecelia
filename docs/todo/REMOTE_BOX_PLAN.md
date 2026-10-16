# Remote access — the lab GPU box

**Status:** P0 done 2026-10-16 (GNOME Remote Login chosen) · P1 script built + run on the box · no branch

## Goal

A physical Ubuntu GPU box sits in the basement. Users can walk down to it, but mostly would rather
reach it from their desk, the way they already remote into the Windows analysis machines. Remote use
must feel like that: open a client they already have (or install one with no terminal), connect, and
see Cecelia running on the **box's** GPU.

The sibling plan [`REMOTE_ACCESS_PLAN.md`](REMOTE_ACCESS_PLAN.md) covers a different machine: a cloud
VM reached by an SSH tunnel, with the browser and WebGPU on the laptop. Both are wanted. They differ on
one requirement, which is why they are two plans:

| | Cloud VM (`REMOTE_ACCESS_PLAN`) | Lab box (this plan) |
|---|---|---|
| Where WebGPU renders | the laptop's GPU | the box's GPU |
| What crosses the network | the app's HTTP traffic, slabs included | an encoded video stream only |
| Client | the per-OS tunnel launcher | a remote-desktop client |
| Physical console | none | real monitor + keyboard, people walk up to it |

The brief behind this plan is [`docs/archive/remote-access-prompt.md`](../archive/remote-access-prompt.md).
This plan corrects it in several places (marked **brief said**).

## Requirements (from the brief, still standing)

1. WebGPU runs on the box GPU. Chrome on the box loads `http://localhost:<port>` (a secure context).
2. Imaging data never leaves the box: no file transfer, no clipboard sync, no tunnelling the UI out.
3. Clients are Windows, Mac or Linux, and client setup needs no terminal.

The brief's "pin driver and CUDA versions" is **dropped** (Dominik, 2026-10-16).
On this box Ubuntu's unattended security upgrades already moved the driver from 595.91.07 to 595.99.02
on 2026-10-10, and P0 ran on the result. The driver libraries and the kernel's pre-built NVIDIA modules
update together, so holding only the driver would split them, which is the failure pinning is meant to
prevent. Holding both would block NVIDIA security fixes on a box IT scans. The remaining risk: after an
automatic driver update, new GPU programs fail with a driver/library version mismatch until the next
reboot. `remote-check` reports that as "reboot the box".

## Decisions

1. **Cecelia is started by hand from the menu, inside the user's desktop session. No autostart, and
   no systemd service** (Dominik, 2026-10-16). **Brief said** "backend and frontend as systemd
   services". Production has no separate frontend process: `pixi run app` serves the built Vue app
   from the Julia server (`app.py:5-6`). And `app.py` already does what a unit would add:
   - it relaunches the server on Settings → Restart (exit 42)
   - it restarts the server after a crash
   - on first launch it opens the browser through the sign-in link (`app.py:427`), which sets the
     cookie every request needs

   A system service would run outside the GPU desktop session, could not open that browser, and would
   need the token handled some other way. The user launches Cecelia from the `cecelia.desktop` menu
   entry that `install.sh` already writes, as on any desktop, and in-app Quit stops it.

2. **The server stays bound to loopback.** No change. `CECELIA_HOST` stays unset; every Cecelia port
   (`app/src/ports.jl`: 8080, 5173, 7656, 7657, 7660 in slot 0, +10 per slot) answers on `127.0.0.1`
   only. Nothing about Cecelia is reachable from the network, the tailnet included. **Brief said**
   "not reachable from outside localhost/tailnet", which would allow tailnet exposure.

3. **No Cecelia code changes** beyond scripts, a pixi task, docs and a connection-file template.

4. **No dummy X display.** **Brief said** "dummy Xorg config if headless". The box has a real monitor.
   And the `xserver-xorg-video-dummy` driver would move the X session off the NVIDIA GPU, so Chrome
   would fall back to software rendering, which defeats requirement 1. If the box ever goes headless,
   use an HDMI dummy plug or the NVIDIA driver's own headless options, not the dummy driver.

## The box (surveyed 2026-10-16)

Ubuntu 26.04.1, GNOME 50, **Wayland only**: no X11 session is installed, so the brief's "X11, not
Wayland" is not an option. NVIDIA RTX A4500 (20 GB), driver 595 (updated by unattended upgrades), one monitor on DP-1. Two
OS accounts already exist (`cecelia`, `peanut`). It is IT-managed: Rapid7 and Cortex XDR agents are
installed. The network dropped every inbound TCP port until SSH was opened, and once RDP was enabled
3389 was reachable from the institute Wi-Fi. Cecelia is a per-user install started by hand in a
terminal, on `127.0.0.1:8080`. Browsers are Brave and Firefox; no Chrome.

## P0 result (2026-10-16) — GNOME Remote Login is the answer

Both GNOME Remote Desktop modes were tried on the box from Remmina on the institute Wi-Fi.

**Desktop Sharing** (per-user `grdctl rdp`) works but mirrors the console. `brave://gpu` in the remote
session: **WebGPU: Hardware accelerated**, GPU0 = NVIDIA RTX A4500 (driver 595.99.02, `*ACTIVE*`),
Brave 154 (Chrome/154) on `--ozone-platform=wayland`, **no WebGPU flags**. Vulkan shows Disabled;
WebGPU runs regardless. The daemon logs `[HWAccel.CUDA] Initialization of CUDA was successful`, and the
viewer on a timelapse "feels fine". But the basement monitor showed everything the remote user did.
Now switched off.

**Remote Login** (system `grdctl --system rdp`, enabled from Settings → System → Remote Desktop) is
what we use. The client connects, GNOME's login screen appears, and the user logs in with an Ubuntu
account (`peanut` in the test). The session has no seat, so nothing reaches the basement monitor. The software-rendering
report for this mode (Launchpad #2077538) does not hold on 26.04. `nvidia-smi` listed the remote
user's `gnome-shell`, their `gnome-remote-desktop-daemon` (encoding) and their Brave GPU process all
on the A4500, and `brave://gpu` in that session looked the same as above.

Gotchas hit on the way, each one a line in the P3 admin notes:
- **Remote Login has its own RDP credentials**, separate from Desktop Sharing's. Setting them in
  Settings did not stick: every connection logged `[RDP] Credentials are not set, denying client`.
  `sudo grdctl --system rdp set-credentials <user> <password>` plus a service restart fixed it. These
  are one shared gateway login for the box; the user then signs in at the GNOME login screen
  with the Ubuntu account (`peanut`).
- `Init TPM credentials failed … using GKeyFile as fallback` is harmless: the box has no usable TPM,
  so the credentials are kept in a root-only file.
- **Keyring prompt on login.** If an account's password was reset, Brave asks for the old keyring
  password. Cancel works; the fix is the old password once, or deleting
  `~/.local/share/keyrings/login.keyring`.
- **VRAM is shared.** The console session's Brave held 9.9 GB of the card's 20 GB with the viewer
  open. Two users with heavy viewers at once can exhaust it. Not a blocker; note it in the handout and
  watch for it.

## Decisions from P0 (2026-10-16)

5. **GNOME Remote Desktop, Remote Login mode.** Built into Ubuntu, so nothing to install on the box.
   Users connect with the client they already know: Windows Remote Desktop (`mstsc`), Microsoft's
   Windows App on Mac, Remmina on Linux. NoMachine, xrdp and Desktop Sharing are not used.
6. **Everyone uses the one `peanut` account; people are told apart inside Cecelia by user profiles**
   (Dominik, 2026-10-16). That machinery has shipped (`USER_PROFILE_PLAN.md`): a launch-time profile
   picker, per-profile settings, project ownership and a load-screen filter, and author stamps on
   blackboard, notebooks, chains and tasks. `cecelia` is Dominik's admin account. What follows:
   - **One person at a time.** One OS account means one Cecelia (the single-instance lock is per
     config dir) and one install-wide active profile (`docs/audit/persistence-audit.md`, gap B, "fine
     for one person per machine"). A second Remote Login as `peanut` is expected to take over the
     existing session, not open a second one. That still needs testing; see P4.
   - **No OS-level privacy between users.** Profiles filter projects; they do not protect them. Anyone
     on `peanut` can open anyone's files. Acceptable for a lab-shared analysis box.
   - No auto-login. Remote and walk-up users both start at the GNOME login screen.
7. **The console behaves like any desktop.** Remote sessions never show on it, so the brief's
   "sleep/lock disabled" goes. Lock the screen on idle, as on any machine in a room people walk into.
8. **Brave is the browser.** It is installed, and WebGPU is on by default for this GPU on Wayland. No
   Chrome install and no flags.
9. **SSH is for Dominik only** (2026-10-16): key login only, password login off, `AllowUsers
   cecelia`. `peanut` users have no shell access, which also closes the `scp` path around
   requirement 2.

## Open questions — need Dominik

- **Q3. Reachability from outside the building.** From the institute Wi-Fi, 3389 is reachable now. SSH
  was not until something was changed on 2026-10-15/16; record what that was. Does it work over the
  institute VPN from home? Tailscale on this IT-managed box (Rapid7 + Cortex XDR) needs IT's
  agreement, so only pursue it if the VPN does not reach the box.
- **Q7. `peanut`'s keyring.** Its login keyring still has an old password, so Brave prompts for it on
  every login (see the gotchas above). Reset it (delete `~/.local/share/keyrings/login.keyring`,
  losing anything stored in it), or does someone know the old password?

## Phases

- **P0 — spike on the box. DONE 2026-10-16**, see above. Tooling built: `pixi run remote-check` →
  `scripts/remote-box/box-check.sh`. It is read-only and needs no sudo. Run inside a session, it reports
  the GPU (FAIL with "reboot the box" on a driver/library mismatch), Vulkan devices, session type and the session's own renderer, the browser (Brave/Chrome/Chromium), which
  remote-desktop service is active, and the bind address of every listener the user's julia/node/python
  processes own (matched by process, so no port numbers are hardcoded). It exits non-zero on a FAIL.
  It passed on the box over SSH on 2026-10-16. The session probe uses `eglinfo`'s Wayland platform on Wayland: `glxinfo` goes through
  XWayland, which reported `llvmpipe` on a hybrid Intel+NVIDIA laptop whose session ran on the Intel
  GPU. The X11 path still FAILs on a real `llvmpipe` reading.

- **P1 — box setup script. Built:** `scripts/remote-box/setup-box.sh`. It is general, for anyone
  setting up an Ubuntu GNOME box with an NVIDIA GPU, not only this one:
  `sudo bash scripts/remote-box/setup-box.sh [--shared-user NAME] [--ssh-admin NAME] [--dry-run]`.
  It is idempotent, prints what it changed, and asks before creating an account or locking SSH down.
  - **Remote Login:** a self-signed certificate under `/var/lib/gnome-remote-desktop`, the shared RDP
    login through `grdctl --system rdp set-credentials` (it prompts, so the password never appears on
    a command line), and the service enabled. It restarts the service only when something changed,
    because a restart drops everyone connected.
  - **Console lock:** a system dconf default, so the screen locks after 5 idle minutes. Users can still
    override it.
  - **`--shared-user`:** creates the account if missing, and makes a Chromium-based browser its
    default, because `app.py` opens the sign-in link in the default browser. It reminds the admin to
    run `install.sh` as that user. It does not install Cecelia itself.
  - **`--ssh-admin`:** a drop-in under `/etc/ssh/sshd_config.d/` (`AllowUsers`, passwords off). It
    refuses unless the admin already has an authorized key, and it runs `sshd -t` before reloading.
  - **Never touched:** the firewall (often IT's), the NVIDIA driver, and Cecelia's config.

  **First real run on this box, 2026-10-16** (`--shared-user peanut --ssh-admin cecelia`):
  - 4 changes: certificate, dconf profile, console-lock default, SSH drop-in
  - the existing RDP login was detected and kept
  - `peanut` already had Brave as default and a Cecelia install, so no change there

  Verified from outside: key login works, password login is no longer offered, `peanut` is refused
  over SSH, and Remote Login is back on 3389. **Gap:** `cecelia` has `lock-enabled=false` set in its
  own dconf, which overrides the new default, so the long-running console session still never locks.
  Fix pending: lock the setting system-wide, or reset it per account. The Q7 keyring stays a manual
  step.

  **Brief said** a `cecelia-ui` pixi task; it would have to replicate the token step.

- **P2 — harden the check.** FAIL when the session renderer is not the NVIDIA GPU (single-GPU box), or
  when Remote Login is not the active mode. Drop the NoMachine/xrdp probes.

- **P3 — handout.** A *Remote access to a lab GPU box* section in `docs/INSTALL.md`, next to *Running
  on a remote server*, plus an `.rdp` connection-file template.
  - User part: open the `.rdp` file, or type the address into Remote Desktop / Windows App / Remmina;
    enter the shared RDP login, then the `peanut` login, then pick your profile in Cecelia. One person at
    a time: connecting as `peanut` takes over anyone already connected.
  - Admin part: rebuild, add a person (a Cecelia profile, not an Ubuntu account), rotate the RDP
    credentials, the gotchas above, and driver updates:
    they arrive automatically; if `remote-check` says "reboot the box", reboot.

  **Brief said** `docs/remote-access.md`; docs live in the existing area files.

- **P4 — reboot test on the box.** Cold boot with nobody at the console: a remote user connects, logs
  in as `peanut`, starts Cecelia from the menu, and `remote-check` passes in that session.
  Then test sharing the one account: while one person is connected as `peanut`, a second connects as
  `peanut`. Expect the first to be disconnected. Do the same with someone logging in as `peanut` at the
  console.
  Repeat the connect from **Windows Remote Desktop** and from **Windows App on a Mac**. P0 used Remmina
  only; Remote Login hands the client over to the user's session with an RDP server redirection, and
  each client has to follow it.

## Not in scope

- Sunshine + Moonlight: the brief lists it as a documented fallback only. It is game-streaming
  oriented and needs its own client everywhere, so it is less familiar than RDP.
- Scheduling GPU memory between concurrent users (see *VRAM is shared*). Revisit if it bites.
- The cloud VM path: [`REMOTE_ACCESS_PLAN.md`](REMOTE_ACCESS_PLAN.md).

## References (checked 2026-10-15)

- gpuweb implementation status (Chrome on Linux by vendor/session):
  https://github.com/gpuweb/gpuweb/wiki/Implementation-Status
- GNOME Remote Desktop headless sessions in software on Ubuntu — Ubuntu Discourse, Launchpad #2077538:
  https://discourse.ubuntu.com/t/vnc-server-stopped-working-after-ubuntu-22-24-upgrade/50953
- GNOME Remote Desktop hardware acceleration requirements:
  https://discourse.gnome.org/t/minimum-requirements-for-hardware-accelerated-rdp-in-mutter-gnome-remote-desktop-daemon/19043
- xrdp NVIDIA acceleration and the rootless-X modesetting failure — Arch wiki, forum:
  https://wiki.archlinux.org/title/Xrdp , https://bbs.archlinux.org/viewtopic.php?id=305022
- Ubuntu Remote Desktop test plan (Vulkan hardware-acceleration log line from 26.04):
  https://ubuntu.com/project/docs/maintainers/test-plans/desktop-team/RemoteDesktop/
