"""Engine for migrating a legacy R/Shiny cecelia project into the new Pineapple layout — **images,
segmentation, and tracking only**. Clustering, gating, and HMM are intentionally NOT migrated (they
are trivial to re-run in the new app, and their old formats diverged).

Kept dependency-light on purpose (this is a niche path for a handful of users):
  * ``ccid.rds`` is read via an external base-R helper (``read_ccid_rds.R`` — no R packages);
  * OME-XML is parsed with the Python stdlib;
  * the ``.h5ad`` rewrite uses ``anndata`` (already an app dep); zarr is file-copied (no ``zarr``).

Two entry points wrap this: ``scan_legacy_run.py`` (read-only preview manifest) and
``migrate_legacy_run.py`` (one image). See ``docs/todo/LEGACY_MIGRATION_PLAN.md``.
"""
from __future__ import annotations

import os
import shutil
import subprocess
import xml.etree.ElementTree as ET
from pathlib import Path

# Excluded, re-derivable obs columns (substring match): HMM states/transitions + any clustering col.
EXCLUDE_PATTERNS = (".hmm.", "clust")
_IGNORE = shutil.ignore_patterns("._*", ".DS_Store")


def _is_excluded(col: str) -> bool:
    return any(p in col for p in EXCLUDE_PATTERNS)


# ── ccid.rds (via base-R helper) → {uid: {...}} ─────────────────────────────────
def read_rds(rds_paths, rscript: str = "Rscript") -> dict:
    """Batch-read many ``ccid.rds`` in one R process. Returns
    ``{uid: {meta, attr{}, active{}, channels{vn:[..]}, filepath{vn:fn}, labels{vn:[fn]}, labelprops{vn:fn}}}``."""
    helper = Path(__file__).with_name("read_ccid_rds.R")
    paths = [str(p) for p in rds_paths]
    if not paths:
        return {}                       # nothing to read → don't invoke R with no args
    proc = subprocess.run([rscript, str(helper), *paths], capture_output=True, text=True)
    if proc.returncode != 0:
        raise RuntimeError(
            f"Rscript failed (exit {proc.returncode}). Is '{rscript}' a valid R? "
            f"stderr: {proc.stderr.strip()[:400]}")
    out = proc.stdout
    res: dict = {}
    ch_tmp: dict = {}
    for line in out.splitlines():
        f = line.rstrip("\n").split("\t")
        if len(f) < 2:
            continue
        uid, tag = f[0], f[1]
        d = res.setdefault(uid, {"meta": {}, "attr": {}, "active": {}, "channels": {},
                                 "filepath": {}, "labels": {}, "labelprops": {}})
        if tag == "META":
            d["meta"][f[2]] = f[3] if len(f) > 3 else ""
        elif tag == "ATTR":
            d["attr"][f[2]] = f[3] if len(f) > 3 else ""
        elif tag == "ACTIVE":
            d["active"][f[2]] = f[3]
        elif tag == "CHANNEL":
            ch_tmp.setdefault((uid, f[2]), []).append((int(f[3]), f[4]))
        elif tag == "FILEPATH":
            d["filepath"][f[2]] = f[3]
        elif tag == "LABELS":
            d["labels"].setdefault(f[2], []).append(f[3])
        elif tag == "LABELPROPS":
            d["labelprops"][f[2]] = f[3]
    for (uid, vn), pairs in ch_tmp.items():
        res[uid]["channels"][vn] = [n for _, n in sorted(pairs)]
    return res


# ── OME-XML calibration (stdlib) ────────────────────────────────────────────────
def read_ome_meta(ome_xml: Path) -> dict:
    meta: dict = {}
    if not Path(ome_xml).is_file():
        return meta
    root = ET.parse(ome_xml).getroot()
    pixels = next((e for e in root.iter() if e.tag.rsplit("}", 1)[-1] == "Pixels"), None)
    if pixels is None:
        return meta
    a = pixels.attrib
    for k in ("SizeC", "SizeT", "SizeZ", "SizeX", "SizeY"):
        if k in a:
            meta[k] = int(a[k])
    for k in ("PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "TimeIncrement"):
        if k in a:
            meta[k] = float(a[k])
    if "PhysicalSizeXUnit" in a:
        meta["PhysicalSizeUnit"] = a["PhysicalSizeXUnit"]
    if "TimeIncrementUnit" in a:
        meta["TimeIncrementUnit"] = a["TimeIncrementUnit"]
    return meta


# ── h5ad peek (scan) + rewrite (migrate) ────────────────────────────────────────
def _h5ad_obs_cols(path: Path):
    import anndata as ad
    a = ad.read_h5ad(str(path), backed="r")
    return a, list(map(str, a.obs.columns))


def migrate_h5ad(src: str, dst: str) -> dict:
    """Rewrite one labelProps h5ad onto the new schema. Idempotent. See the plan doc §4."""
    import numpy as np
    import pandas as pd
    import anndata as ad

    a = ad.read_h5ad(src)
    summary: dict = {"n_obs": int(a.n_obs)}

    # 1. index <- label (old label is 1-based; index is 0-based positional → mandatory)
    if "label" in a.obs.columns:
        a.obs_names = pd.Index(np.asarray(a.obs["label"].to_numpy()).astype(np.int64).astype(str))
        a.obs = a.obs.drop(columns=["label"])
        summary["index_set_from_label"] = True
    else:
        summary["index_set_from_label"] = False

    # 2. centroids -> obsm, only if not already there
    if "spatial" not in a.obsm:
        var_names = list(map(str, a.var_names))
        spatial_src = [c for c in ("centroid_z", "centroid_y", "centroid_x") if c in var_names]
        temporal_src = [c for c in ("centroid_t", "t") if c in var_names]
        if spatial_src:
            X = a.to_df()
            a.obsm["spatial"] = X[spatial_src].to_numpy(dtype=np.float32)
            a.uns["spatial_cols"] = np.array([f"centroid-{i}" for i in range(len(spatial_src))], dtype=object)
            if temporal_src:
                a.obsm["temporal"] = X[temporal_src[:1]].to_numpy(dtype=np.float32)
                a.uns["temporal_cols"] = np.array(["t"], dtype=object)
            a = a[:, [c for c in var_names if c not in spatial_src + temporal_src]].copy()
            summary["centroids_lifted"] = spatial_src + temporal_src
        else:
            summary["centroids_lifted"] = None
    else:
        summary["centroids_lifted"] = "already_obsm"

    # 3/4. drop excluded (HMM / clustering) obs columns; keep track_id + measures
    dropped = [c for c in a.obs.columns if _is_excluded(str(c))]
    if dropped:
        a.obs = a.obs.drop(columns=dropped)
    summary["dropped_obs_cols"] = dropped

    a.write_h5ad(dst)
    return summary


# ── SCAN: what will / won't transfer, per image ─────────────────────────────────
def scan_project(src_proj: str, rscript: str = "Rscript", uids=None) -> dict:
    """Read-only manifest for the preview table. Reports, per CciaImage: image variant + dims, the
    segmentations, per-segmentation track counts, and what is present but EXCLUDED (clustering /
    gating / HMM) so the UI can be explicit about what is left behind."""
    proj = Path(src_proj)
    ana_root = proj / "ANALYSIS"
    if not ana_root.is_dir():
        return {"error": f"Not a legacy cecelia project (missing {ana_root}).", "images": []}
    ana0 = ana_root / "0"
    ver = _meta_version(ana_root)
    if ver is None:
        return {"error": f"No populated analysis version under {ana_root} "
                         f"(expected ANALYSIS/<n>/<uid>/ccid.type).", "images": []}
    ana1 = ana_root / ver

    # discover CciaImage uids
    all_uids = []
    for p in sorted(ana1.iterdir()):
        if p.name.startswith((".", "._")) or not (p / "ccid.type").is_file():
            continue
        if (p / "ccid.type").read_text().strip() == "CciaImage":
            all_uids.append(p.name)
    want = [u for u in all_uids if (uids is None or u in uids)]

    rds = read_rds([ana1 / u / "ccid.rds" for u in want], rscript)

    def _scan_one(uid):
        r = rds.get(uid, {})
        lp_dir = ana1 / uid / "labelProps"
        segs = list(r.get("labelprops", {}).keys())
        tracking: dict = {}
        hmm = False
        for vn, fn in r.get("labelprops", {}).items():
            h5 = lp_dir / fn
            if not h5.is_file():
                continue
            try:
                a, cols = _h5ad_obs_cols(h5)
                if "track_id" in cols:
                    tid = a.obs["track_id"].to_numpy()
                    tracking[vn] = int(len({t for t in tid if t and t == t and t != 0}))
                if any(".hmm." in c for c in cols):
                    hmm = True
            except Exception:
                pass
        clustering = False
        if lp_dir.is_dir():
            for f in lp_dir.iterdir():
                n = f.name
                if n.endswith(".sc.h5ad") or n.startswith("tracks.clusters") or n.startswith("clusters."):
                    clustering = True
                    break
        pops = ana1 / uid / "populations"
        gating = pops.is_dir() and any(x.suffix == ".json" for x in pops.iterdir()) if pops.is_dir() else False

        fp = r.get("filepath", {})
        active_fp = r.get("active", {}).get("filepath", "default")
        img_file = fp.get(active_fp) or fp.get("default")
        if not img_file or not (ana0 / uid / img_file).exists():
            img_file = next((c for c in ("ccidImage.ome.zarr", "ccidImage.zarr")
                             if (ana0 / uid / c).exists()), img_file)
        default_img = fp.get("default") or img_file
        meta = read_ome_meta(ana0 / uid / (default_img or "") / "OME" / "METADATA.ome.xml") if default_img else {}
        kind = "live" if meta.get("SizeT", 1) > 1 else "static"

        warnings = []
        if not img_file or not (ana0 / uid / img_file).exists():
            warnings.append("image data not found on disk")
        if kind == "live" and "TimeIncrement" not in meta:
            warnings.append("no frame interval in metadata (set it in the app)")
        if not meta:
            warnings.append("no calibration metadata found")

        return {
            "uid": uid,
            "name": r.get("meta", {}).get("name", uid),
            "kind": kind,
            "size": {k: meta[k] for k in ("SizeC", "SizeT", "SizeZ", "SizeX", "SizeY") if k in meta},
            "image_variant": img_file,
            "segmentation": segs,
            "tracking": tracking,
            "excluded": {"clustering": clustering, "gating": gating, "hmm": hmm},
            "warnings": warnings,
        }

    # one bad image must not abort the whole scan
    images = []
    for uid in want:
        try:
            images.append(_scan_one(uid))
        except Exception as e:
            images.append({
                "uid": uid, "name": uid, "kind": "static", "size": {}, "image_variant": None,
                "segmentation": [], "tracking": {},
                "excluded": {"clustering": False, "gating": False, "hmm": False},
                "warnings": [f"could not read this image: {type(e).__name__}: {e}"],
            })

    out = {
        "project_uid": proj.name,
        "version": ver,
        "n_images": len(images),
        "images": images,
        # what the migrator carries vs drops — surfaced so the UI can state it plainly
        "transfers": ["image", "segmentation", "tracking"],
        "not_transferred": ["clustering", "gating", "HMM"],
    }
    if not images:
        n_entries = sum(1 for p in ana1.iterdir() if not p.name.startswith((".", "._")))
        out["note"] = (
            f"No CciaImage found under {ana1} ({n_entries} entries scanned). Point at the "
            f"project folder that directly contains ANALYSIS/1/<uid>/ccid.type — not its parent."
        )
    return out


# ── MIGRATE one image → returns the ccid field dict for Julia to apply ───────────
def _copy_or_link(src: Path, dst: Path, mode: str) -> None:
    dst.parent.mkdir(parents=True, exist_ok=True)
    if dst.is_symlink():
        dst.unlink()
    elif dst.is_dir():
        shutil.rmtree(dst)
    elif dst.exists():
        dst.unlink()
    if mode == "symlink":
        os.symlink(src, dst)
    else:
        shutil.copytree(src, dst, ignore=_IGNORE)


def _meta_version(ana_root: Path):
    """The analysis-version subdir that holds metadata (`<uid>/ccid.type`). Old cecelia keeps image
    data in `ANALYSIS/0` and metadata in `ANALYSIS/<versionID>` — and the active version varies per
    project (e.g. `1` for one project, `11` for another; earlier versions may be left empty). Pick
    the numeric subdir != '0' with the most CciaImage entries (tie → highest number). None if there
    is no populated version."""
    best, best_n, best_num = None, 0, -1
    if not ana_root.is_dir():
        return None
    for p in sorted(ana_root.iterdir()):
        if not p.is_dir() or not p.name.isdigit() or p.name == "0":
            continue
        n = sum(1 for c in p.iterdir()
                if not c.name.startswith((".", "._")) and (c / "ccid.type").is_file())
        num = int(p.name)
        if n > best_n or (n == best_n and n > 0 and num > best_num):
            best, best_n, best_num = p.name, n, num
    return best


# array order by ndim: images/labels drop the channel axis for labels (4D = TZYX)
_AXES_BY_NDIM = {5: ["t", "c", "z", "y", "x"], 4: ["t", "z", "y", "x"],
                 3: ["z", "y", "x"], 2: ["y", "x"]}


def _store_ndim(zarr_path: str):
    """ndim of a store's level-0 array (series-nested `0/0` or flat `0`), or None."""
    import json
    for cand in (os.path.join(zarr_path, "0", "0", ".zarray"),
                 os.path.join(zarr_path, "0", ".zarray")):
        if os.path.isfile(cand):
            with open(cand) as f:
                return len(json.load(f).get("shape", []))
    return None


def _upgrade_label_axes(path: Path, meta: dict, log) -> None:
    """Upgrade a copied legacy label store: write NGFF `axes` + `datasets` (from the legacy `labels`
    key) so napari's label loader finds `datasets` and places the mask (labels are TZYX — no channel)."""
    try:
        from cecelia.utils import zarr_utils
        names = _AXES_BY_NDIM.get(_store_ndim(str(path)))
        if not names or "c" in names:      # labels shouldn't carry a channel axis
            log(f"! label {path.name}: unexpected ndim, skipped axis upgrade")
            return
        sp = meta.get("PhysicalSizeUnit")
        ok = zarr_utils.set_ngff_axes(
            str(path), names,
            scale={"t": meta.get("TimeIncrement", 1.0), "z": meta.get("PhysicalSizeZ", 1.0),
                   "y": meta.get("PhysicalSizeY", 1.0), "x": meta.get("PhysicalSizeX", 1.0)},
            units={"z": sp, "y": sp, "x": sp, "t": meta.get("TimeIncrementUnit")})
        log(f"upgraded NGFF axes ({''.join(names)}) on {path.name}"
            if ok else f"! label {path.name}: axis upgrade skipped")
    except Exception as e:
        log(f"! label axis upgrade failed on {path.name}: {e}")


def _versioned(values: dict, active, fallback=None) -> dict:
    out = dict(values)
    act = active if active in values else (fallback if fallback in values else next(iter(values), None))
    if act is not None:
        out["_active"] = act
    return out


def migrate_image(src_proj: str, uid: str, dst_zero_dir: str, dst_meta_dir: str,
                  mode: str = "copy", rscript: str = "Rscript", version=None, log=None) -> dict:
    """Migrate one image's data into the given new dirs and return its ccid field dict.
    ``dst_zero_dir`` = new ``{proj}/0/{uid}``; ``dst_meta_dir`` = new ``{proj}/1/{uid}``. Image data
    is read from ``ANALYSIS/0``; metadata from ``ANALYSIS/<version>`` (auto-discovered if not given —
    old cecelia's active analysis version varies per project)."""
    _log = log or (lambda m: None)
    proj = Path(src_proj).resolve()
    ana_root = proj / "ANALYSIS"
    ver = str(version) if version else _meta_version(ana_root)
    if ver is None:
        raise ValueError(f"no populated analysis version under {ana_root}")
    old0, old1 = ana_root / "0" / uid, ana_root / ver / uid
    new0, new1 = Path(dst_zero_dir).resolve(), Path(dst_meta_dir).resolve()

    # SAFETY: migration is strictly read-only on the source. The only deletions anywhere are on the
    # destination (idempotent re-run). Refuse if the destination lands inside the source tree, so we
    # can never overwrite or remove old data. (_copy_or_link only ever rmtrees `dst`, never `src`.)
    for nd in (new0, new1):
        if proj == nd or proj in nd.parents:
            raise ValueError(f"refusing to migrate: destination {nd} is inside the source project {proj}")
    r = read_rds([old1 / "ccid.rds"], rscript).get(uid, {})
    if r.get("meta", {}).get("class") != "CciaImage":
        raise ValueError(f"{uid}: not a CciaImage")

    fp = r.get("filepath", {})
    active_fp = r.get("active", {}).get("filepath", "default")
    img_file = fp.get(active_fp) or fp.get("default")
    if not img_file or not (old0 / img_file).exists():
        img_file = next((c for c in ("ccidImage.ome.zarr", "ccidImage.zarr")
                         if (old0 / c).exists()), img_file)
    active_fp = next((vn for vn, f in fp.items() if f == img_file), active_fp)

    default_img = fp.get("default") or img_file
    meta = read_ome_meta(old0 / default_img / "OME" / "METADATA.ome.xml")
    if "orifilepath" in r.get("meta", {}):
        meta["ori_path"] = r["meta"]["orifilepath"]
    kind = "live" if meta.get("SizeT", 1) > 1 else "static"

    new1.mkdir(parents=True, exist_ok=True)
    (new1 / "labelProps").mkdir(exist_ok=True)
    (new1 / "labels").mkdir(exist_ok=True)

    _log(f"[PROGRESS] 0/3")
    _log(f"copying image {img_file} ({mode})")
    _copy_or_link(old0 / img_file, new0 / img_file, mode)

    # Upgrade the copied zarr's NGFF axes so the new stack recognises the dimensions (esp. the
    # channel axis — the legacy bioformats2raw store carries a v0.2 multiscales stub with no `axes`,
    # so napari would otherwise show one layer with C as a plain slider). COPY mode only: in symlink
    # mode the "copy" is the source, and we never modify old data.
    if mode == "copy":
        try:
            from cecelia.utils import zarr_utils
            sp = meta.get("PhysicalSizeUnit")
            okx = zarr_utils.set_ngff_axes(              # bioformats2raw is 5D TCZYX
                str(new0 / img_file), ["t", "c", "z", "y", "x"],
                scale={"t": meta.get("TimeIncrement", 1.0), "c": 1.0,
                       "z": meta.get("PhysicalSizeZ", 1.0),
                       "y": meta.get("PhysicalSizeY", 1.0),
                       "x": meta.get("PhysicalSizeX", 1.0)},
                units={"z": sp, "y": sp, "x": sp, "t": meta.get("TimeIncrementUnit")},
                channels=r.get("channels", {}).get(active_fp) or r.get("channels", {}).get("default"))
            _log(f"upgraded NGFF axes (TCZYX) on {img_file}" if okx
                 else f"! NGFF axis upgrade skipped (shape mismatch) on {img_file}")
        except Exception as e:
            _log(f"! NGFF axis upgrade failed: {e}")
    else:
        _log("! symlink mode: skipped NGFF axis upgrade (would modify the source); "
             "use copy mode for napari channel recognition")

    _log(f"[PROGRESS] 1/3")
    labels_out: dict = {}
    for vn, files in r.get("labels", {}).items():
        if vn not in r.get("labelprops", {}):
            continue
        kept = []
        for fn in files:
            if (old1 / "labels" / fn).exists():
                _copy_or_link(old1 / "labels" / fn, new1 / "labels" / fn, mode)
                kept.append(fn)
        if kept:
            labels_out[vn] = kept

    _log(f"[PROGRESS] 2/3")
    lp_out: dict = {}
    for vn, fn in r.get("labelprops", {}).items():
        src_h5 = old1 / "labelProps" / fn
        if not src_h5.exists():
            _log(f"! labelProps missing, skipping: {fn}")
            continue
        s = migrate_h5ad(str(src_h5), str(new1 / "labelProps" / fn))
        lp_out[vn] = fn
        _log(f"h5ad {vn}: idx<-label={s['index_set_from_label']} "
             f"centroids={s['centroids_lifted']} dropped={len(s['dropped_obs_cols'])}")

    _log(f"[PROGRESS] 3/3")
    # user attributes carry over verbatim, except the legacy `Include=Y/N` keyword → the new
    # top-level `included` bool (its documented successor; see image.jl). Keep every other attr.
    attr = dict(r.get("attr", {}))
    inc = attr.pop("Include", None)
    included = True if inc is None else (str(inc).strip().upper() not in ("N", "NO", "FALSE", "0"))
    return {
        "uid": uid,
        "name": r.get("meta", {}).get("name", uid),
        "kind": kind,
        "status": "done",
        "filepath": {active_fp: img_file, "_active": active_fp},
        "labels": labels_out,
        "label_props": _versioned(lp_out, r.get("active", {}).get("labelprops"), next(iter(lp_out), None)),
        "imChannelNames": _versioned(r.get("channels", {}), r.get("active", {}).get("channels"), "default"),
        "attr": attr,
        "included": included,
        "meta": meta,
    }
