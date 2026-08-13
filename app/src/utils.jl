# SHA is also imported in tasks/chain.jl; repeat it here rather than depend on include order or
# on an unrelated file keeping its import.
import SHA
import JSON3

const UID_LENGTH = 6
const UID_CHARS  = collect("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

gen_uid(n::Int=UID_LENGTH) = String(rand(UID_CHARS, n))

# ── The websocket frame cap on both resident-Python legs — ONE number ─────────
# Every message to/from the napari bridge and the preview worker is ONE JSON frame carrying a whole
# payload: a label block, a set of AF-corrected channels, a contact sheet of PNGs. Both ends cap the
# size of a frame they will accept, and the two caps are independent — which is how they drifted.
#
# The Python side was raised to 64 MiB long ago (`WS_MAX_SIZE`, napari_bridge.py) with a comment saying
# the 1 MiB default "is not a graceful degradation — the server rejects the frame and closes the
# connection, so the preview would fail on big images only". Every word of that is true of the JULIA
# side too, and nobody set it: HTTP.jl's client default is 16 MiB, so the backend was quietly the
# narrow leg. Measured on `zolIMa/VJy1Nx` driftCorrected (1044x1102): a whole-frame flow-metrics sheet
# is 16 colour-mapped PNGs, 36.3 MB of JSON in ONE frame, and the read died with
# `websocket closed with status 1009: message too large`. On the 418x434 image it had been developed
# against the same reply is ~1.5 MB — so it worked on exactly one image, and looked broken everywhere.
#
# So: one constant, both directions, asserted equal to the Python one by the
# "resident python legs agree on their frame cap" testset. This is a TRANSPORT cap, not a budget —
# a producer that can emit tens of MB should still bound what it sends (see
# `FLOW_INSPECT_MAX_PX` in `api/src/optical_flow_api.jl`); the cap is what turns a payload nobody
# bounded into a clean failure instead of a silent one.
const WS_MAX_FRAME_SIZE = 64 * 1024 * 1024

# ── Durable state writes — ONE mechanism ──────────────────────────────────────
# `write_atomic` is THE way to write any durable state file — ccid.json, project.json, the
# gating/QC/spatial/board sidecars, chain templates + run.json, custom.toml, the lab log, a Pluto
# notebook. Every content type goes through it (`write_json_atomic` is the same mechanism with the
# JSON serialiser bound, not a second one), so there is nothing per-file-type to look up. Do NOT
# hand-roll `open(path, "w")` for state: the `durable state writes are atomic` testset fails on a
# new bare-open site.
#
# Why it exists: `open(path, "w")` TRUNCATES before the new bytes land. Anything that ends the
# process in that window — the Quit button (`_kill_tree` SIGKILLs), a task cancel, a crash — leaves
# a half-written file. That was unrecoverable for `ccid.json`, and not only for the image concerned:
# `_load_set` has no per-image guard, so ONE truncated image ccid.json made the WHOLE project fail
# to open with an opaque `invalid JSON at byte position N` — every other image intact on disk but
# unreachable, and export is the only backup. ~30 call sites had hand-rolled the truncating form;
# the gating sidecar was the single place that got it right, and this is that pattern promoted to
# the one shared helper.
#
# Deliberate scope: this defends against the PROCESS dying, which is the trigger we actually have.
# It is NOT an fsync — power loss can still lose a rename the OS hasn't flushed. Julia exposes no
# portable fsync and process death is the realistic case, so rename-level is the honest depth here
# rather than a durability claim we can't back.

# ── Staged store writes — the same idea, one directory up ─────────────────────
# A STORE (a label zarr, an image version) has the same failure mode as a state file, only stretched
# over minutes: a writer that opens its final path in write mode destroys the previous store up front
# and then fills it frame by frame, so a cancelled re-run leaves `ccid.json` advertising a store that
# is now partial — and on a single-level store the missing frames read as ZEROS, with no error at all.
#
# The mechanism lives on the Python side, where the writers are: `zarr_utils.staged_store` streams
# into a staging sibling and renames it into place only once the store is complete. Julia needs the
# two suffixes for its own reasons — declaring which store a live preview should watch
# (`segment_live_outputs`) and sweeping debris left by a killed run (`maintenance.jl`) — so they are
# mirrored here. Keep them in step with `STAGING_SUFFIX`/`SUPERSEDED_SUFFIX` in
# python/cecelia/utils/zarr_utils.py; there is no shared constant across the two languages.
const STORE_STAGING_SUFFIX = ".partial"
const STORE_SUPERSEDED_SUFFIX = ".superseded"
const STORE_TMP_SUFFIXES = (STORE_STAGING_SUFFIX, STORE_SUPERSEDED_SUFFIX)

"""
    staging_store_path(path) -> String

The staging sibling `zarr_utils.staged_store` writes `path` through while a task is filling it. The
one place Julia spells this out — a live preview has to name the in-progress store *before* the run
starts, so the name has to be derived, not discovered.
"""
staging_store_path(path::AbstractString) = string(path, STORE_STAGING_SUFFIX)

"""
    write_atomic(f, path) -> path

Write `path` by streaming through `f(io)` into a sibling temp file, then atomically renaming it into
place. A reader never observes a partial file, and an interrupted write leaves the **previous**
content intact instead of a truncated one.

The temp file is a sibling (same directory ⇒ same filesystem, so the rename is atomic rather than a
copy) and carries a unique suffix, so two concurrent writers of the same path never clobber each
other's temp. The suffix deliberately does **not** end in `.json`: sidecar discovery in several
places is `readdir` + `endswith(f, ".json")`, so a leftover temp from a killed process must not
register as a population/QC/stats sidecar.

If `f` throws, the temp file is removed and the original `path` is left untouched.

```julia
write_atomic(path) do io          # any content type
    TOML.print(io, cfg)
end
write_json_atomic(path, data)     # the JSON case
```
"""
function write_atomic(f::Function, path::AbstractString)
    dir = dirname(path)
    isempty(dir) || isdir(dir) || mkpath(dir)
    # gen_uid (not tempname) so a leftover from a killed process is recognisably ours, and adjacent
    # to the file it was replacing
    tmp = string(path, ".tmp.", gen_uid())
    try
        open(tmp, "w") do io
            f(io)
        end
        mv(tmp, path; force = true)
    catch
        rm(tmp; force = true)
        rethrow()
    end
    path
end

"""
    write_json_atomic(path, data) -> path

`write_atomic` with the JSON serialiser bound — the form ~90% of state writes use. Always pretty:
this is state a human reads when something has gone wrong, and one writer means one on-disk format.
(Before this, `save!` wrote pretty while every task-commit site wrote compact JSON to the *same*
ccid.json, so a file's formatting depended on who touched it last.)
"""
write_json_atomic(path::AbstractString, data) = write_atomic(io -> JSON3.pretty(io, data), path)

function _dir_bytes(path::String)::Int
    if Sys.isunix()
        # `du -sk` (KiB), NOT `-sb`: `-b` is a GNU coreutils extension that BSD/macOS `du` does not
        # have, so on macOS the command failed, the `catch` swallowed it, and this returned **0 for
        # every directory** — storage reclaim silently reported nothing to free. `-k` is in POSIX and
        # works on both. It reports disk BLOCKS rather than apparent size (so slightly larger than
        # `-sb` on Linux), which is the more honest number for "how much space would I get back".
        try; parse(Int, split(readchomp(`du -sk $path`))[1]) * 1024; catch; 0; end
    else
        try
            total = 0
            for (root, _, files) in walkdir(path)
                for f in files
                    try; total += filesize(joinpath(root, f)); catch; end
                end
            end
            total
        catch; 0; end
    end
end

"""
    _path_bytes(path) -> Int

On-disk size of one stored thing, whether it is a directory (walked with `_dir_bytes`) or a plain
file, and 0 when it is neither. Every "how big is this on disk" caller needs exactly this ternary —
an image version is a zarr DIRECTORY while a label set can be a single file — so it lives here once
instead of being re-spelled per call site (storage accounting, version removal, the metadata modal).

Note it is a directory WALK, not a stat: ~10k chunk files for a 4 GB image version. Cheap enough to
answer on demand (~0.05–0.3 s warm, ~2 s cold per store here), too expensive to fold into listing.
"""
_path_bytes(path::AbstractString) =
    isdir(path) ? _dir_bytes(String(path)) : (isfile(path) ? Int(filesize(path)) : 0)

# ── Release-bundle integrity ──────────────────────────────────────────────────────────────────────

"""
    _file_sha256(path) -> String

Lowercase hex SHA-256 of a file, streamed rather than slurped — the release bundle is small today but
this must not depend on that.

Used by `/api/update/apply` to check a downloaded bundle against the `.sha256` published beside it.
HTTPS already covers the transport; this covers a truncated or swapped asset, which transport
security says nothing about.
"""
_file_sha256(path::AbstractString)::String = open(io -> bytes2hex(SHA.sha256(io)), path)

"""
    _sha256_matches(path, digest_file_contents) -> Bool

Whether `path` hashes to the digest recorded in a `sha256sum`-style line:

    2f0a…9c  cecelia.tar.gz

Only the first whitespace-delimited token is read, so both the GNU (`hash  name`) and bare-hash forms
work. Comparison is case-insensitive and whitespace-tolerant; a malformed or empty digest file is
`false` rather than an error, so the CALLER decides whether a missing/broken digest is fatal.
"""
function _sha256_matches(path::AbstractString, digest_contents::AbstractString)::Bool
    tok = first(split(strip(digest_contents)), 1)
    isempty(tok) && return false
    expected = lowercase(strip(first(tok)))
    occursin(r"^[0-9a-f]{64}$", expected) || return false
    _file_sha256(path) == expected
end

"""
    safe_name_part(raw) -> String

One filename-safe fragment: keep `[A-Za-z0-9._-]`, collapse every run of anything else to `_`, and
drop the separators the collapse leaves at the EDGES.

The edge strip is the whole point. An image called `"… -res (cropped)"` ends in `)`, so sanitising
alone gives `"…-res_cropped_"` — a name ending in a separator — and a suffixed variant compounds it
to `"…-res_cropped__animation"`. Two call sites once had the strip and the sanitise split between
them while claiming to share "the same character rule"; they are one function so a name cannot be
sanitised two ways.

Lives in the package (not the API layer) because tasks name their own output files — the OME-TIFF
export and the napari movie recorders must agree on what a safe name is.
"""
function safe_name_part(raw)::String
    s = replace(strip(String(raw === nothing ? "" : raw)), r"[^A-Za-z0-9._-]+" => "_")
    String(strip(s, ['_', '.']))
end

"""
    git_probe(args...; dir = pwd()) -> String

Best-effort `git`: stdout stripped, `""` on any failure, and **stderr discarded**.

For dev *diagnostics* only — the REPL panel's commit line and the worktree switcher. Every caller
treats "" as "not a git checkout" and carries on, which is the normal case for an installed app:
there is no `.git` under `~/.local/share/cecelia`, so each probe made git print
`fatal: not a git repository (or any of the parent directories): .git` into the user's launch
output. The failures were already caught and harmless; only the leaked text reached the console,
where it reads like a broken install (reported in #540).

Same reason `_kill_tree`/`_dir_bytes` live here rather than inline: a shell-out gets one spelling.
Not for anything whose *result* matters — a caller that needs to know WHY git failed should run it
itself and read stderr.
"""
function git_probe(args::AbstractString...; dir::AbstractString = pwd())::String
    try
        cmd = Cmd(String["git", "-C", String(dir), String.(args)...])
        String(strip(read(pipeline(cmd; stderr = devnull), String)))
    catch
        ""      # git absent, not a repo, or a non-zero exit — all "no answer"
    end
end
