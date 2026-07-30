# SHA is also imported in tasks/chain.jl; repeat it here rather than depend on include order or
# on an unrelated file keeping its import.
import SHA
import JSON3

const UID_LENGTH = 6
const UID_CHARS  = collect("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

gen_uid(n::Int=UID_LENGTH) = String(rand(UID_CHARS, n))

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
