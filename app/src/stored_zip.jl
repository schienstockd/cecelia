# ── stored_zip.jl — a minimal ZIP writer (no compression) ─────────────────────────────────────────────
#
# For small text exports that a consumer expects as one `.zip` (the RO-Crate of a chain run,
# `ai/ro_crate.jl`): a few KB of JSON / Markdown / CSV, so compression buys nothing and the writer can
# be the plain "stored" method. Julia's deps carry no zip library and the project bundle uses the
# system `tar` (project_io.jl), which cannot write a zip on Linux; this is ~60 lines instead of a dep.
#
# Format: PKWARE APPNOTE.TXT 6.3.10 (https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT) —
# local file header §4.3.7, central directory header §4.3.12, end of central directory §4.3.16,
# method 0 = stored §4.4.5, general-purpose flag bit 11 = UTF-8 names §4.4.4, MS-DOS date/time §4.4.6.
# CRC-32 is the IEEE 802.3 polynomial, reflected (0xEDB88320), init and final XOR 0xFFFFFFFF (§4.4.7);
# its published check value — CRC-32 of "123456789" = 0xCBF43926 — is asserted in the test suite.
# No ZIP64: every entry and the whole archive must stay under 4 GiB, which this writer checks.

const _CRC32_TABLE = let t = Vector{UInt32}(undef, 256)
    for i in 0:255
        c = UInt32(i)
        for _ in 1:8
            c = (c & 0x1) != 0 ? (0xEDB88320 ⊻ (c >> 1)) : (c >> 1)
        end
        t[i + 1] = c
    end
    t
end

"""
    crc32_ieee(bytes) -> UInt32

CRC-32 as ZIP, gzip and PNG use it (IEEE 802.3, reflected). Not CRC-32C (the `CRC32c` stdlib).
"""
function crc32_ieee(data::AbstractVector{UInt8})::UInt32
    c = 0xFFFFFFFF
    for b in data
        c = _CRC32_TABLE[((c ⊻ b) & 0xFF) + 1] ⊻ (c >> 8)
    end
    c ⊻ 0xFFFFFFFF
end

# MS-DOS time and date words (APPNOTE §4.4.6): 2-second resolution, years from 1980
function _dos_time_date(t::DateTime)::Tuple{UInt16,UInt16}
    y = clamp(Dates.year(t), 1980, 2107)
    time = UInt16(Dates.hour(t) << 11 | Dates.minute(t) << 5 | div(Dates.second(t), 2))
    date = UInt16((y - 1980) << 9 | Dates.month(t) << 5 | Dates.day(t))
    time, date
end

"""
    write_stored_zip(io, files; modified = DateTime(1980, 1, 1)) -> io

Write `files` (`name => bytes`, in the given order; names use `/`) as a ZIP archive to `io`, every
entry stored uncompressed. `modified` is the timestamp every entry carries — pass a fixed one and the
same files give the same bytes.
"""
function write_stored_zip(io::IO, files::AbstractVector{<:Pair{<:AbstractString,<:AbstractVector{UInt8}}};
                          modified::DateTime = DateTime(1980, 1, 1))
    time, date = _dos_time_date(modified)
    flags = UInt16(1 << 11)                     # names are UTF-8
    central = IOBuffer()
    offset = 0
    for (name, data) in files
        nm = Vector{UInt8}(codeunits(String(name)))
        (length(data) < typemax(UInt32) && offset < typemax(UInt32)) || error("zip entry too large for a non-ZIP64 archive: $name")
        crc = crc32_ieee(data); n = UInt32(length(data))
        hdr = IOBuffer()
        write(hdr, UInt32(0x04034b50), UInt16(20), flags, UInt16(0), time, date, crc, n, n,
              UInt16(length(nm)), UInt16(0), nm)
        h = take!(hdr)
        write(io, h); write(io, data)
        write(central, UInt32(0x02014b50), UInt16(20), UInt16(20), flags, UInt16(0), time, date, crc, n, n,
              UInt16(length(nm)), UInt16(0), UInt16(0), UInt16(0), UInt16(0), UInt32(0), UInt32(offset), nm)
        offset += length(h) + length(data)
    end
    cd = take!(central)
    write(io, cd)
    write(io, UInt32(0x06054b50), UInt16(0), UInt16(0), UInt16(length(files)), UInt16(length(files)),
          UInt32(length(cd)), UInt32(offset), UInt16(0))
    io
end
