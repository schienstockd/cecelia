"""
A per-image cache for whole-movie normalisation percentiles — the one statistic that legitimately
has to see every pixel, kept so it is paid once per image rather than once per run.

**Why this exists.** Flow training normalises each (plane × channel) by percentiles taken over the
WHOLE plane sequence, because that is the statistic inference reproduces; normalising a crop or a
frame window by its own range would scale the same structure differently depending on where the
window landed. Measured on `zolIMa/VJy1Nx` (181 × 4 × 38 × 1046 × 1104 uint16), one plane-channel
costs ~1.1 s to read and ~4.9 s for the two percentiles, against 0.13 s to read the [60, 256, 256]
block training actually keeps. At 6 movies × 10 planes × 2 channels that is the difference between a
30-minute prelude and seconds — and it repeats on every rerun that changes only the epoch count or a
loss weight, which is most of them.

**Staleness is a KEY problem, not a warning problem.** Everything that changes the number is part of
the key or the fingerprint, so a changed setting is a MISS and a recompute — never a stale hit that
needs a warning the user has to understand and act on. Specifically:

  * the percentile, the channel, the plane and the zero-handling policy are in the `key`, so two
    policies or two percentiles coexist in one file instead of invalidating each other;
  * the store's shape, dtype and metadata mtime are the `fingerprint`, so pixels changing under an
    unchanged path drop the whole file. Cecelia writes stores staged-then-renamed, so re-running the
    smoothing that produced the source gives its metadata a new mtime — the case a path-only key
    would get silently wrong.

The frame window and the crop are deliberately NOT in the key: the percentile spans the whole movie
regardless of either, which is exactly what makes it reusable across reruns that change them.

**Not `.json`.** Sidecar discovery in several places is `readdir` + `endswith(".json")`, so a file
that is not a population/QC/stats sidecar must not look like one — the same reasoning as
`write_atomic`'s temp suffix (`app/src/utils.jl`). The content is JSON; the name is not.
"""
import json
import os

import numpy as np

from cecelia.utils import atomic_io

#: Bumped when the FILE layout changes. A reader that does not recognise a version treats the file as
#: absent rather than guessing at its shape — a cache is regenerable, so refusing to read a format
#: you do not know costs one recompute and risks nothing.
VERSION = 1

#: Deliberately not `.json` — see the module docstring.
SUFFIX = '.normstats'

#: Zero-handling, as it goes into a key. Flow training percentiles the plane INCLUDING background
#: zeros; segmentation's `_compute_norm_params` drops them (`hist[0] = 0`). The two disagree by 5-20%
#: on real data and the difference is under review, so the policy is named in every key rather than
#: assumed: whichever way that decision goes, entries written under the other policy are ignored
#: instead of silently reused.
ZEROS_INCLUDED = 'zin'
ZEROS_EXCLUDED = 'zex'


def path_for(im_path):
    """The cache file for an image store: a SIBLING of the store, not a file inside it.

    Outside, because the store belongs to whichever task produced it and a consumer writing into
    another task's store is the kind of thing nobody expects to have happened. The cost of being
    outside is that a rewritten store leaves the old sidecar behind — which is what `fingerprint`
    is for.
    """
    return os.path.join(os.path.dirname(os.path.abspath(im_path)),
                        os.path.basename(os.path.abspath(im_path)) + SUFFIX)


def fingerprint(im_path, shape, dtype):
    """An identity for the store's PIXELS, cheap enough to compute on every open.

    Shape and dtype catch a store rebuilt at a different size; the level-0 metadata mtime catches one
    rebuilt at the same size, which is the common case — re-running smoothing or AF correction writes
    a new store to the same path. No checksum: hashing gigabytes to save seconds would cost more than
    the statistic it protects.

    Returns `None` when no metadata file can be found, and `None` never matches anything — an
    unrecognisable store is treated as uncacheable rather than assumed unchanged.
    """
    stamp = None
    for name in ('0/.zarray', '0/zarr.json', '.zmetadata', 'zarr.json'):
        p = os.path.join(im_path, name)
        if os.path.exists(p):
            stamp = int(os.stat(p).st_mtime_ns)
            break
    if stamp is None:
        return None
    dims = 'x'.join(str(int(s)) for s in shape)
    return f'{dims}/{np.dtype(dtype).str}/{stamp}'


def key(channel, plane, percentile, zeros=ZEROS_INCLUDED, variant=None):
    """The key for one cached `(lo, hi)` pair.

    `plane` may be `None` for a whole-image statistic or a 2D movie, which is a distinct case from
    plane 0 and must not collide with it.

    `variant` is for anything ELSE about how the number was derived that changes it. Segmentation
    needs it: `_compute_norm_params` reads a pyramided store's lowest-res level but streams a
    histogram over a single-level one, and those are two different answers to the same question — so
    a range derived one way must never be served to a call that would have taken the other. Nothing
    in the fingerprint can catch that, because it depends on how many levels the CALLER opened.
    """
    z = 'flat' if plane is None else f'z{int(plane)}'
    tail = '' if variant is None else f'/{variant}'
    return f'c{int(channel)}/{z}/p{float(percentile):g}/{zeros}{tail}'


def read(im_path, fp):
    """Cached entries for a store whose fingerprint is `fp`, or `{}`.

    `{}` for every unhappy path — absent, unreadable, wrong version, stale fingerprint, malformed.
    A cache miss is always a correct outcome, so there is nothing here worth raising over and
    nothing worth warning about: the caller recomputes.
    """
    if not fp:
        return {}
    try:
        with open(path_for(im_path), 'r', encoding='utf-8') as fh:
            doc = json.load(fh)
    except (OSError, ValueError):
        return {}
    if not isinstance(doc, dict) or doc.get('version') != VERSION:
        return {}
    if doc.get('fingerprint') != fp:
        return {}
    entries = doc.get('entries')
    if not isinstance(entries, dict):
        return {}
    out = {}
    for k, v in entries.items():
        # A pair of finite numbers or nothing. A truncated or hand-edited entry must not reach the
        # normalisation as a None or a string, where it would fail deep inside the arithmetic.
        try:
            lo, hi = float(v[0]), float(v[1])
        except (TypeError, ValueError, IndexError, KeyError):
            continue
        if np.isfinite(lo) and np.isfinite(hi):
            out[str(k)] = (lo, hi)
    return out


def write(im_path, fp, entries):
    """Replace the cache file with `entries` under fingerprint `fp`. Returns whether it wrote.

    Through `atomic_io.write_json_atomic`, so a reader never sees a partial document and an
    interrupted write leaves the previous one intact — the same reason every other durable write in
    the Python tier goes through it, and its temp suffix already sits after the real extension so a
    discovery listing cannot pick one up.

    Never raises. The source store may sit on a read-only mount or a full disk, and a run that has
    already done the work must not fail at the point of saving a convenience — a cache that could not
    be written costs the next run its recompute and nothing else.
    """
    if not fp or not entries:
        return False
    doc = {'version': VERSION, 'fingerprint': fp,
           'entries': {str(k): [float(v[0]), float(v[1])] for k, v in entries.items()}}
    try:
        atomic_io.write_json_atomic(path_for(im_path), doc, indent=1, sort_keys=True)
        return True
    except (OSError, ValueError, TypeError):
        return False
