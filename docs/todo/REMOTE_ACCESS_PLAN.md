# Remote access — cross-OS SSH-tunnel launcher

**Status:** planning (2026-08-27)

## Goal

A Cecelia VM install (any cloud, any provider) is reached from a laptop by **double-clicking a
per-OS launcher icon**. The launcher opens an SSH tunnel to the VM in the background and points the
user's default browser at `http://localhost:8080`. **No SSH command typed by the user, no
third-party service, no rate limits, no cloud-vendor lock, no code changes to Cecelia's server.**

## Non-goals

- **No token auth or per-user identity at the app layer.** Cecelia is single-tenant by its data
  model; whoever has SSH access to the VM has app access. That's the correct authorization scope for
  this tool.
- **No HTTPS in the app.** SSH provides real end-to-end encryption between laptop and VM; the app
  stays HTTP on `127.0.0.1:8080` because nothing but the tunnel can reach it.
- **No cloud-provider SDK dependency.** `gcloud`, `awscli`, `az` — none of them. Just OpenSSH,
  which ships in-box on every supported OS.
- **No code-signed native binaries.** Ship shell scripts + OS-native shortcuts to avoid the $99/yr
  Apple + Windows signing certs. Cost: one "downloaded from internet" click on first launch per OS.
- **No exposed public port for Cecelia.** Server keeps binding `127.0.0.1:8080` in all scenarios.
  The tunnel is the only reach path.
- **No shared running instance across users.** Two people looking at the same VM run two launchers
  and get two tunnels. Cecelia doesn't reshape to multi-tenant.

## Motivation — path we considered and why we're here

The earlier iteration of this plan proposed URL-based token auth so users could click a shared link
in a browser. That path collapses on the confidentiality question, which has only two free answers:

- **Plain HTTP + shared link** — the token and every subsequent binary slab travel cleartext on the
  wire. Fine for a single trusted network, not for the cross-institution "click a link a
  collaborator can open" story.
- **HTTPS via Caddy + nip.io + Let's Encrypt** — free, but rate-limited to **50 certs per 7 days**
  on the entire `nip.io` bucket globally (nip.io is not on the Public Suffix List, so every user of
  `*.nip.io` shares one bucket with every other user of `*.nip.io`). Not acceptable for an in-box
  shipping tool.

The SSH tunnel launcher pattern is the only free, universal, rate-limit-free path that gives real
confidentiality **and** the "click an icon" UX. It's the same pattern VS Code Remote-SSH, JetBrains
Gateway, and remote-Jupyter workflows have used for years — well-understood, well-supported.

## Prior art / verified before designing

- **Cecelia already binds loopback by default** (`api/src/server.jl:640`, `HOST = get(ENV,
  "CECELIA_HOST", "127.0.0.1")`). This plan keeps that unchanged, which means no new auth surface,
  no new middleware, no new API routes.
- **OpenSSH is shipped in-box everywhere we support:**
  - Linux: standard package, universally installed on any real distro image.
  - macOS: shipped since Mac OS X.
  - Windows 10 September 2018 Update onwards: OpenSSH client is a Windows Feature, enabled by
    default. `ssh.exe` is on `PATH` out of the box.
- **The tunnel command is identical across all three:** `ssh -N -L 8080:localhost:8080 user@host`.
  Backgrounding differs (`-f` on Linux/macOS, `Start-Process -WindowStyle Hidden` on Windows), but
  the connect syntax is the same.
- **Every cloud VM user has already placed a pubkey.** GCP / AWS / Azure / DigitalOcean / Hetzner /
  on-prem — VM creation always includes an "SSH key" step. That means by the time Cecelia's install
  script runs on the VM, the user's laptop already has a working SSH path to it. The launcher
  doesn't have to solve key management.

## Decisions (2026-08-27)

1. **Server bind: unchanged, forever loopback.** No `CECELIA_HOST` override in the remote install
   path. No fail-closed guard needed for a non-loopback bind (there won't be one). Loopback-only
   means no auth needed at the app layer AND no accidental exposure via config drift.

2. **Transport: OpenSSH tunnel.** `ssh -N -L <localPort>:localhost:<remotePort> user@host` on the
   laptop. Zero cloud-vendor dependencies. `ServerAliveInterval=30` for keepalive.

3. **Auth: whatever the user's cloud-provider VM-creation step already gave them.** Their pubkey is
   on the VM; the launcher uses the private key from their default SSH config. Cecelia never sees a
   key.

4. **Connection profile: `~/.cecelia/connection.json` on the laptop.**
   ```json
   { "host": "34.56.78.90", "user": "ubuntu", "localPort": 8080, "remotePort": 8080,
     "label": "cecelia-lab-vm" }
   ```
   Written by the wizard from what the user pastes; consumed by the launcher on every subsequent
   run.

5. **VM-side handoff: `install.sh` prints connection.json to stdout** at end of install AND writes it
   to `~/cecelia-connection.json` in the SSHed-in user's home. Fields resolved:
   - `host` — cloud metadata endpoint if reachable (see Risks for URLs), else the value of
     `CECELIA_PUBLIC_HOST` env, else empty (user fills in).
   - `user` — `$USER`.
   - `localPort`/`remotePort` — both 8080.
   - `label` — hostname of the VM.

6. **Per-OS delivery: scripts, not binaries.**
   - **Linux:** `cecelia-connect.sh` + `.desktop` file with a Cecelia icon. Placed in
     `~/.local/share/applications/` on install.
   - **macOS:** `cecelia-connect.command` inside a stub `.app` bundle (a folder with
     `Contents/Info.plist` + `Contents/MacOS/cecelia-connect`). No signing → one Gatekeeper prompt
     on first launch (right-click → Open), then clean.
   - **Windows:** `cecelia-connect.ps1` + a `.lnk` shortcut pointing at
     `powershell.exe -ExecutionPolicy Bypass -File cecelia-connect.ps1`. No signing → one
     SmartScreen prompt on first launch, then clean.

7. **Wizard: shared logic, per-OS wrapper.** First-run flow:
   - Prompt for connection.json (paste or file picker).
   - Probe `ssh -o BatchMode=yes -o ConnectTimeout=5 user@host echo ok`. On failure, print a
     copyable diagnostic including the exact ssh command, the return code, and the standard-error
     tail. No handholding of the user's SSH config beyond that.
   - Save validated JSON to `~/.cecelia/connection.json`.

8. **Launch loop: SSH backgrounded, poll for port, open browser.**
   - Start `ssh -N -L …` as a detached child process (with a PID file so the launcher can offer to
     kill it later).
   - Poll `localhost:<localPort>` with a TCP connect until it accepts or 15 s elapses.
   - On success: open default browser (`xdg-open` / `open` / `Start-Process`) at the URL.
   - On timeout: print the SSH log tail from `~/.cecelia/last-connect.log`.

9. **Local-port collision.** If `<localPort>` is already bound, launcher walks upward until it
   finds a free port and uses that. Browser is opened at whatever port won. This makes the
   "developer with a local Cecelia running on 8080" case just work.

10. **Reconnect: manual, no `autossh`.** Adds a cross-OS install dependency we don't need. If the
    tunnel drops, the user clicks the launcher again. Diagnostic log at `~/.cecelia/last-connect.log`
    is enough to debug.

11. **Uninstall: delete the shortcut + `~/.cecelia/connection.json`.** No system services, no
    launchd items, no systemd units — everything is user-owned files. Documented in the doc, not
    scripted.

## Phases

- **P1 — VM-side connection.json.** Modify `install.sh` (Linux) / `install.ps1` (Windows-native
  installs, which is a separate audience) to write and print connection.json at end of install. Env
  hook `CECELIA_PUBLIC_HOST=` overrides autodetected IP for edge cases. **Testable on a Linux dev
  box against `localhost` as the pretend-VM.**

- **P2 — Cross-OS launcher scripts.** All three scripts + shared wizard flow. **Linux path is
  end-to-end testable locally** (SSH from your machine to itself via `sshd` on `:22`). macOS + Windows
  need real machines of those OSes for integration — expected to work by mechanical translation of
  the Linux flow, but land verified on those OSes before the download page announces them.

- **P3 — Per-OS packaging.** `.desktop`, `.app` stub, `.lnk` — small; done in one pass once P2's
  scripts are solid.

- **P4 — Docs rewrite.** `docs/INSTALL.md` → *Running on a remote server* section (PR #677)
  rewritten to describe: create VM on your cloud (add SSH key at creation), run `install.sh` on the
  VM, copy the printed connection.json, install the launcher for your laptop's OS, click the icon.
  Retires the raw `ssh -L` tutorial as an "advanced / manual" appendix.

## Test plan

**Locally testable (Linux dev box, no VM required):**
- `install.sh` writes a connection.json with expected schema. Round-trip test: parse, re-serialize,
  compare.
- `CECELIA_PUBLIC_HOST=1.2.3.4 install.sh` overrides the host field.
- Wizard imports a hand-authored connection.json, probes `ssh localhost echo ok` (against dev
  box's own sshd), succeeds, saves.
- Wizard's failure path: bad host, `BatchMode=yes` refuses password, wizard prints diagnostic.
- Launcher runs `ssh -N -L 8080:localhost:8080 $USER@localhost`, polls port, opens browser.
- Port-collision: bind `:8080` with `python3 -m http.server 8080`, launcher walks to `:8081`.

**Requires a real VM (deferred until access):**
- End-to-end on a fresh Ubuntu GCE VM.
- Cross-cloud: same on AWS EC2 (metadata endpoint differs), DigitalOcean.
- Cross-OS: launcher on a macOS laptop + Windows laptop against the same VM.

## Risks / open questions

- **Windows PowerShell execution policy** defaults to `Restricted`. The `.lnk` shortcut passes
  `-ExecutionPolicy Bypass -File …`, which sidesteps the policy for this one script without changing
  system state. Verified pattern; documented gotcha.
- **macOS Gatekeeper quarantine** on downloaded `.app` bundles: first launch requires Cmd-click →
  Open, then persists. One-time UX cost.
- **Cloud metadata endpoints** for autodetected `host`:
  - GCP: `curl -H 'Metadata-Flavor: Google' http://metadata.google.internal/computeMetadata/v1/instance/network-interfaces/0/access-configs/0/external-ip`
  - AWS: `curl http://169.254.169.254/latest/meta-data/public-ipv4`
  - Azure: `curl -H 'Metadata: true' 'http://169.254.169.254/metadata/instance/network/interface/0/ipv4/ipAddress/0/publicIpAddress?api-version=2021-02-01&format=text'`
  - Timeout each probe at 1 s, fall through to `CECELIA_PUBLIC_HOST` env, then empty string. Never
    hang the installer waiting for metadata.
- **VM IP change on ephemeral IPs.** If the VM's public IP changes on stop/start, the connection.json
  is stale. Options: re-run `install.sh` to reprint (no reinstall — should be a `pixi run
  print-connection` command). Doc note + P4 CLI helper.
- **SSH known_hosts prompt** on first connection. Wizard offers
  `-o StrictHostKeyChecking=accept-new` as opt-in (safer than blindly `no`, less scary than the
  default prompt).
- **Port 22 outbound from user's laptop** blocked by some corporate networks. `Port 443` in VM's
  sshd config + `ssh -p 443` in launcher is the escape hatch. Doc note only, not default.
- **Wizard needs a GUI or a terminal.** Options:
  - Pure-terminal (opens a terminal window and asks). Simplest. Ugly.
  - Zenity/osascript/PowerShell GUI dialog per OS. Ships in-box on all three (`zenity` on modern
    distros, `osascript` universally on macOS, PowerShell forms on Windows). Ugly-but-native.
  - Bikeshed at P2 kickoff. Default-lean toward native dialogs.
- **No auto-reconnect** on VPN flake / network hiccup. Documented decision; if it becomes a real
  complaint, `autossh` is an install-time flag away.
- **Concurrent launcher runs.** If user double-clicks twice, second run detects the first tunnel
  still up, opens browser without re-tunneling. PID-file check.
- **Preserving the token design** — if a future requirement wants direct-browser access without a
  tunnel (e.g. Windows-only user who refuses SSH), the earlier token-based design is preserved in
  this file's git history. Reviving it doesn't break anything in this plan.

## References

- Previous token-auth iteration: see `git log docs/todo/REMOTE_ACCESS_PLAN.md`. Superseded
  2026-08-27 by the LE-rate-limit finding + the observation that a tunnel makes the token moot.
- Server bind default: `api/src/server.jl:640`.
- Existing SSH-tunnel workaround doc: `docs/INSTALL.md` → *Running on a remote server* (PR #677;
  rewritten in P4).
- Cross-check on the LE rate-limit posture: `letsencrypt.org/docs/rate-limits/` (50 certs / 7 days /
  registered domain; nip.io shares one bucket globally per PSL lookup).
