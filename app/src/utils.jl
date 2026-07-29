# SHA is also imported in tasks/chain.jl; repeat it here rather than depend on include order or
# on an unrelated file keeping its import.
import SHA

const UID_LENGTH = 6
const UID_CHARS  = collect("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

gen_uid(n::Int=UID_LENGTH) = String(rand(UID_CHARS, n))

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
