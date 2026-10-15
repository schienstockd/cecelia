> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in [`docs/todo/REMOTE_BOX_PLAN.md`](../todo/REMOTE_BOX_PLAN.md).
>
> **Outcome:** not run as written. It covers a physical GPU box in the lab that users can walk to but
> would rather reach remotely, which complements the cloud-VM SSH tunnel in
> [`docs/todo/REMOTE_ACCESS_PLAN.md`](../todo/REMOTE_ACCESS_PLAN.md). A spike on the real box
> (2026-10-16) replaced several premises:
> - the box is Ubuntu 26.04, which is **Wayland only**, so "X11, not Wayland" cannot be met
> - **GNOME Remote Desktop in Remote Login mode** (plain RDP, built in) gets the GPU, so NoMachine is
>   not used
> - production has no separate frontend service to put under systemd
> - a dummy Xorg driver would take the session off the GPU
> - "lock disabled" is unsafe on a console people walk up to

# Task: Remote-access setup for the Cecelia GPU box

## Context
Cecelia (Julia backend + Vue frontend, WebGPU rendering) runs on a single Ubuntu GPU box. Users are used to remoting into Windows analysis machines, so the connection experience must be simple.

## Hard requirements
1. **WebGPU must run on the box GPU.** Chrome runs on the box against `http://localhost:<port>` (secure context). The user's local GPU is not used.
2. **Imaging data never leaves the box.** Only encoded video goes to the client. No file transfer, no clipboard sync, no SSH-tunnelling of the UI to a local browser.
3. **One concurrent user.** No multi-user session handling needed.
4. **Mostly Cecelia UI use.** No need for a full-desktop toolset beyond Chrome.
5. **Clients are Windows/Mac/Linux.** Client setup must need no terminal.

## Chosen stack
- Ubuntu LTS, NVIDIA proprietary driver, X11 (not Wayland), auto-login to a real GPU-backed session. Dummy Xorg config if headless.
- **NoMachine** (console/physical-session mode, NVENC hardware encoding, file transfer and clipboard disabled).
- **Tailscale** for reachability. `ufw` allows only the `tailscale0` interface plus SSH.
- Cecelia backend and frontend as `systemd` services, bound to `localhost`, auto-start, restart on failure.
- Fallback (document only, do not implement unless asked): Sunshine + Moonlight.

## Deliverables
1. `scripts/setup-box.sh`: idempotent, run once with sudo. Covers driver/Vulkan checks, X11 auto-login, dummy display config, sleep/lock disabled, NoMachine install and config, Tailscale, `ufw`, systemd units for Cecelia. Print what it changed. Safe to re-run.
2. `scripts/remote-check.sh`: no sudo. Verifies `nvidia-smi`, `vulkaninfo`, NoMachine service status, Cecelia ports bound to localhost only, and ports not exposed on other interfaces. Exit non-zero on failure.
3. `pixi.toml` tasks: `remote-check` and `cecelia-ui` (launch Chrome with the WebGPU flags against localhost). No `sudo` inside tasks.
4. `docs/remote-access.md`: a short user handout (install Tailscale, install NoMachine client, open the provided connection file) and an admin section (rebuild steps, how to approve devices, how to update drivers safely with `apt-mark hold`).
5. A template NoMachine connection file (`.nxs`) with host, resolution and display settings as placeholders.

## Constraints
- Do not change the Cecelia application code.
- Do not touch existing user data or mounts.
- Ask before any destructive or hard-to-reverse system change (display manager swap, driver reinstall, firewall reset).
- Pin driver and CUDA versions; do not auto-upgrade them.
- Keep scripts readable: small functions, comments only where the reason isn't obvious.

## Verification (report results, don't assume)
- `chrome://gpu` shows WebGPU as hardware accelerated **inside the NoMachine session**. If it can't be checked from the script, say so and list the manual step.
- Headless reboot test: auto-login, NoMachine, Cecelia services and the display all come back without intervention.
- Confirm no Cecelia port is reachable from outside `localhost`/tailnet.

## Output
Summarise what was created, what was run, what was verified, and what still needs a manual check. Keep it brief.
