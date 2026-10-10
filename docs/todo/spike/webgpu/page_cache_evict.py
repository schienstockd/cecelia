"""Evict one store's files from the Linux page cache without root, and report how much is resident.

    python3 page_cache_evict.py <dir> [--check-only]

The no-sudo stand-in for `echo 3 > /proc/sys/vm/drop_caches` when the cold run only needs ONE store
cold: posix_fadvise(POSIX_FADV_DONTNEED) drops a file's clean pages for any caller that can open it.
Residency is measured with mincore(2) before and after, so the eviction is verified rather than assumed.
Linux only.
"""
import ctypes
import mmap
import os
import sys

libc = ctypes.CDLL("libc.so.6", use_errno=True)
libc.mincore.argtypes = [ctypes.c_void_p, ctypes.c_size_t, ctypes.c_char_p]
libc.mmap.restype = ctypes.c_void_p
libc.mmap.argtypes = [ctypes.c_void_p, ctypes.c_size_t, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_long]
libc.munmap.argtypes = [ctypes.c_void_p, ctypes.c_size_t]
PAGE = mmap.PAGESIZE


def resident(path):
    """(resident_pages, total_pages) for one file."""
    size = os.path.getsize(path)
    if size == 0:
        return 0, 0
    n = (size + PAGE - 1) // PAGE
    fd = os.open(path, os.O_RDONLY)
    try:
        addr = libc.mmap(None, size, mmap.PROT_READ, mmap.MAP_SHARED, fd, 0)
        if addr in (None, ctypes.c_void_p(-1).value):
            raise OSError(ctypes.get_errno(), "mmap failed", path)
        vec = ctypes.create_string_buffer(n)
        if libc.mincore(addr, size, vec) != 0:
            raise OSError(ctypes.get_errno(), "mincore failed", path)
        libc.munmap(addr, size)
        return sum(b & 1 for b in vec.raw), n
    finally:
        os.close(fd)


def files(root):
    for d, _, fs in os.walk(root):
        for f in fs:
            yield os.path.join(d, f)


def report(root, tag):
    r = t = 0
    for p in files(root):
        a, b = resident(p)
        r += a
        t += b
    print(f"{tag}: {r * PAGE / 1e6:.1f} MB of {t * PAGE / 1e6:.1f} MB resident ({100 * r / max(t, 1):.1f}%)")


def main():
    root = sys.argv[1]
    report(root, "before")
    if "--check-only" in sys.argv:
        return
    for p in files(root):
        fd = os.open(p, os.O_RDONLY)
        try:
            os.posix_fadvise(fd, 0, 0, os.POSIX_FADV_DONTNEED)
        finally:
            os.close(fd)
    report(root, "after")


if __name__ == "__main__":
    main()
