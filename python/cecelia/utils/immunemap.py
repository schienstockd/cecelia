"""Immunemap client — list an experiment's videos and download one, with nothing but the stdlib.

Immunemap (https://immunemap.org; Pizzagalli et al., EMBO J 2025, doi:10.1038/s44318-025-00629-4)
is an open atlas of intravital movies. Its backend at `api.immunemap.org` has no published spec;
the shapes below were read off live responses, and the test fixtures are copies of those responses.

What the importer relies on:

- `/experiment/{id}` → `experimental_groups[].acquisition[]` (acquisition ids). The experiment id is
  the `id=` of `app.immunemap.org/experiment-public-view?id=N`. Group ids are a DIFFERENT number
  space from experiment ids, and groups have no endpoint of their own.
- `/acquisition/{id}` → one video's metadata: `video_details` (size, spacing, fps, checksums, curated
  track ids), `cell_stainings` (which channel shows what), `links` (DOI). A video can be private
  inside a public experiment — that acquisition answers 401, and the manifest says so per video.
- `/video/{uid}/ims` → the ORIGINAL uploaded file (Imaris HDF5), byte-identical to
  `original_file` (size, md5, sha1), so a download is verified against Immunemap's own checksums.

Public data needs no login. Nothing here writes to Immunemap.
"""
import hashlib
import json
import os
import re
import time
import urllib.error
import urllib.request
from datetime import datetime

from cecelia.utils.ims_meta import summarise_gaps

API_BASE = 'https://api.immunemap.org'
APP_BASE = 'https://app.immunemap.org'
USER_AGENT = 'Cecelia-Immunemap-importer'


class ImmunemapError(RuntimeError):
    """A request Immunemap answered with an error — `status` carries the HTTP code (None = network)."""

    def __init__(self, msg, status=None):
        super().__init__(msg)
        self.status = status


def parse_experiment_ref(ref):
    """The experiment id from an int, a digit string, or an Immunemap URL carrying `id=N`."""
    if isinstance(ref, int):
        return ref
    s = str(ref).strip()
    if s.isdigit():
        return int(s)
    m = re.search(r'[?&]id=(\d+)', s) or re.search(r'/experiment/(\d+)', s)
    if not m:
        raise ValueError(f'Not an Immunemap experiment id or URL: {ref!r}')
    return int(m.group(1))


def experiment_url(exp_id):
    return f'{APP_BASE}/experiment-public-view?id={exp_id}'


def video_url(video_uid):
    return f'{API_BASE}/video/{video_uid}/ims'


def _request(url, headers=None, timeout=60):
    req = urllib.request.Request(url, headers={'User-Agent': USER_AGENT, **(headers or {})})
    return urllib.request.urlopen(req, timeout=timeout)


def get_json(path, retries=3, backoff=2.0, timeout=60):
    """GET `API_BASE + path` as JSON. Retries network errors and 5xx; a 4xx is final (401 = private)."""
    url = API_BASE + path
    for attempt in range(retries):
        try:
            with _request(url, timeout=timeout) as r:
                return json.loads(r.read().decode('utf-8'))
        except urllib.error.HTTPError as e:
            if e.code < 500 or attempt == retries - 1:
                raise ImmunemapError(f'{url}: HTTP {e.code}', status=e.code) from e
        except (urllib.error.URLError, TimeoutError, ConnectionError) as e:
            if attempt == retries - 1:
                raise ImmunemapError(f'{url}: {e}') from e
        time.sleep(backoff * (2 ** attempt))


# ── metadata mapping ─────────────────────────────────────────────────────────────────────────────

def channel_names(cell_stainings, n_channels):
    """Channel labels from `cell_stainings`, one per REAL channel.

    Each staining marks the channel that carries it with `selected: true`; its label is the stained
    `class` plus the fluorophore, e.g. `B cells (GFP)`. Immunemap's own channel names are generic or
    null, and `channel_configuration` lists virtual channels beyond `number_of_channels`, so neither
    is used. A channel no staining selects keeps a positional `Channel i`; one that several stainings
    select joins them with ` + `; a staining that selects several channels labels each of them. Each
    entry says which channel it is (`channel_number`); list position is only the fallback, because the
    list is not guaranteed to be in channel order.
    """
    labels = [[] for _ in range(n_channels)]
    for st in cell_stainings or []:
        cls = (st.get('class') or '').strip()
        fl = (st.get('fluorophore') or '').strip()
        if not cls and not fl:
            continue
        label = f'{cls} ({fl})' if cls and fl else (cls or fl)
        for pos, ch in enumerate(st.get('channels') or []):
            i = ch.get('channel_number')
            i = i if isinstance(i, int) else pos
            if ch.get('selected') and 0 <= i < n_channels and label not in labels[i]:
                labels[i].append(label)
    return [' + '.join(ls) if ls else f'Channel {i}' for i, ls in enumerate(labels)]


def time_step_interval(time_steps):
    """`(interval, irregular)` from the ISO `time_steps` stamps.

    The same rule as the file readers (`ims_meta.summarise_gaps`): a series whose gaps aren't uniform
    yields NO interval, not its median — a paused acquisition stamped with the typical gap would
    make every speed across the pause wrong. `irregular` says the stamps exist but disagree, so the
    caller doesn't fall back to the nominal frame rate either. `(None, False)` when there are fewer
    than two readable stamps.
    """
    stamps = []
    for t in time_steps or []:
        try:
            stamps.append(datetime.strptime(t, '%Y-%m-%dT%H:%M:%S%z'))
        except (TypeError, ValueError):
            return None, False
    if len(stamps) < 2:
        return None, False
    interval, uniform, _ = summarise_gaps([(b - a).total_seconds() for a, b in zip(stamps, stamps[1:])])
    if interval is None:
        return None, False
    return (interval, False) if uniform else (None, True)


def interval_from_time_steps(time_steps):
    """The uniform interval in seconds from `time_steps`, or None (too few, unreadable, irregular)."""
    return time_step_interval(time_steps)[0]


# Relative difference above which a file and Immunemap disagree about a voxel size.
SPACING_TOL = 0.02


def spacing_mismatch(file_xyz, immunemap_xyz, tol=SPACING_TOL):
    """Axes (`x`/`y`/`z`) where two voxel sizes differ by more than `tol`, relative. Missing = no claim."""
    out = []
    for axis, a, b in zip('xyz', file_xyz or [], immunemap_xyz or []):
        if a and b and abs(a - b) / max(abs(a), abs(b)) > tol:
            out.append(axis)
    return out


def _dois(links):
    return [l['reference'] for l in links or [] if l.get('doi') and l.get('reference')]


def video_record(acq, group):
    """One video's import record from its `/acquisition/{id}` payload and its group."""
    v = acq.get('video_details') or {}
    size = v.get('size') or {}
    orig = v.get('original_file') or {}
    n_ch = int(size.get('number_of_channels') or 0)
    fps = size.get('fps') or 0
    spacing = size.get('spacing') or []
    access = acq.get('access') or {}
    status = v.get('upload_status')
    step_interval, steps_irregular = time_step_interval(v.get('time_steps'))
    reasons = []
    if v.get('deleted'):
        reasons.append('deleted on Immunemap')
    if not v.get('uid'):
        reasons.append('no video file')
    return {
        'acquisitionId': acq['id'],
        'groupId': group['id'],
        'condition': group['name'],
        'name': os.path.splitext(acq.get('name') or str(acq['id']))[0],
        'fileName': orig.get('name') or acq.get('name'),
        'videoUid': v.get('uid'),
        'sizeBytes': orig.get('size'),
        'md5': orig.get('md5'),
        'sha1': orig.get('sha1'),
        'width': size.get('width'), 'height': size.get('height'),
        'slices': size.get('slices'), 'frames': size.get('frames'),
        'nChannels': n_ch,
        'spacingXYZ': spacing,
        'intervalSec': (1.0 / fps) if fps else None,
        'intervalFromTimeStepsSec': step_interval,
        'timeStepsIrregular': steps_irregular,
        'channelNames': channel_names(acq.get('cell_stainings'), n_ch),
        'nCuratedTracks': len(v.get('tracks') or []),
        'uploadStatus': status,
        'public': bool(access.get('public')),
        'dois': _dois(acq.get('links')),
        'notes': acq.get('notes') or '',
        'tags': [t.get('value') for t in acq.get('tags') or [] if t.get('value')],
        'recorded': ((v.get('recorded') or {}).get('date') or '')[:19],
        'importable': not reasons,
        'reason': '; '.join(reasons),
    }


def experiment_manifest(ref, fetch=get_json, pause=0.3):
    """Everything a preview needs about an experiment, one `/acquisition` request per video.

    `fetch(path) -> dict` is injectable so tests replay captured responses. Requests are sequential
    with a short pause — this is a public service run by a lab. A private video (HTTP 401/403) comes
    back as a non-importable row naming the reason rather than failing the whole experiment.
    """
    exp_id = parse_experiment_ref(ref)
    e = fetch(f'/experiment/{exp_id}')
    videos = []
    for g in e.get('experimental_groups') or []:
        for acq_id in g.get('acquisition') or []:
            try:
                acq = fetch(f'/acquisition/{acq_id}')
            except ImmunemapError as err:
                why = 'not public' if err.status in (401, 403) else f'unavailable ({err})'
                videos.append({'acquisitionId': acq_id, 'groupId': g['id'], 'condition': g['name'],
                               'name': str(acq_id), 'importable': False, 'reason': why})
                continue
            videos.append(video_record(acq, g))
            pause and time.sleep(pause)
    ok = [v for v in videos if v['importable']]
    return {
        'experiment': {
            'id': exp_id,
            'name': e.get('name') or '',
            'url': experiment_url(exp_id),
            'dois': _dois(e.get('links')),
            'microscope': (e.get('microscope') or {}).get('name') or '',
            'authors': [a.get('fullName') for a in e.get('authors') or [] if a.get('fullName')],
        },
        'groups': [{'id': g['id'], 'name': g['name'], 'description': g.get('description') or '',
                    'nVideos': len(g.get('acquisition') or [])}
                   for g in e.get('experimental_groups') or []],
        'videos': videos,
        'totals': {'videos': len(videos), 'importable': len(ok),
                   'bytes': sum(v.get('sizeBytes') or 0 for v in ok)},
        'citation': ('Data from Immunemap (Pizzagalli et al., EMBO J 2025, '
                     'doi:10.1038/s44318-025-00629-4) and the contributing lab — cite the '
                     "dataset's own DOI and check its terms before publishing."),
    }


# ── download ─────────────────────────────────────────────────────────────────────────────────────

def md5_of(path, chunk=8 << 20):
    h = hashlib.md5()
    with open(path, 'rb') as f:
        for block in iter(lambda: f.read(chunk), b''):
            h.update(block)
    return h.hexdigest()


def download(url, dest, size=None, md5=None, on_progress=None, chunk=8 << 20, retries=5,
             backoff=2.0, opener=_request):
    """Stream `url` to `dest`, resumable, verified. Returns the md5 of the finished file.

    Writes to `dest + '.partial'` and renames only after the size and md5 match, so `dest` existing
    means a verified file. The `.partial` is deliberately KEPT on failure (not `atomic_io`, whose
    context deletes it): the next attempt resumes from its length with an HTTP Range request. An
    existing `dest` whose md5 already matches is reused without a request. Never holds more than one
    `chunk` in memory — these files are 0.3–1 GB.
    """
    if os.path.isfile(dest):
        have_md5 = md5_of(dest)
        if md5 is None or have_md5 == md5:
            return have_md5
    part = dest + '.partial'
    os.makedirs(os.path.dirname(dest) or '.', exist_ok=True)
    for attempt in range(retries):
        have = os.path.getsize(part) if os.path.isfile(part) else 0
        if size is not None and have > size:   # a stale partial from some other file
            os.remove(part)
            have = 0
        try:
            headers = {'Range': f'bytes={have}-'} if have else {}
            with opener(url, headers=headers, timeout=120) as r:
                status = getattr(r, 'status', 200)
                if have and status != 206:     # server ignored the Range: start over
                    have = 0
                with (open(part, 'ab') if have else open(part, 'wb')) as f:
                    done = have
                    for block in iter(lambda: r.read(chunk), b''):
                        f.write(block)
                        done += len(block)
                        on_progress and on_progress(done, size or 0)
            break
        except urllib.error.HTTPError as e:
            if e.code == 416 and size is not None and have == size:
                break                          # already complete
            if e.code < 500 or attempt == retries - 1:
                raise ImmunemapError(f'{url}: HTTP {e.code}', status=e.code) from e
        except (urllib.error.URLError, TimeoutError, ConnectionError, OSError) as e:
            if attempt == retries - 1:
                raise ImmunemapError(f'{url}: {e} — the partial file is kept; re-run to resume') from e
        time.sleep(backoff * (2 ** attempt))

    got_size = os.path.getsize(part)
    if size is not None and got_size != size:
        raise ImmunemapError(f'{url}: got {got_size} bytes, expected {size} — re-run to resume')
    got_md5 = md5_of(part)
    if md5 is not None and got_md5 != md5:
        os.remove(part)                        # wrong bytes: resuming would only extend them
        raise ImmunemapError(f'{url}: md5 {got_md5} does not match Immunemap ({md5})')
    os.replace(part, dest)
    return got_md5
