"""
Python mirror of the Julia ``versioned_*`` / ``version_*`` composer in ``app/src/helpers.jl``.

Two orthogonal axes on the same ``ccid.json`` shape:

* **Outer axis — value_name variant** (``default``, ``dtype``, ``cropped``, …). The active variant
  is named by the ``_active`` key alongside the value_names.
* **Inner axis — version per value_name** (``v1``, ``v2``, …). The latest version is named by the
  ``_latest`` key alongside the version keys.

Full shape example (``ccid.json`` fragment)::

    { "filepath": { "default": { "v1": "image.zarr", "v2": "image.zarr", "_latest": "v2" },
                    "_active": "default" } }

Legacy — a bare scalar at the value_name key — is treated as implicit ``v1``, so old projects
load unchanged and every existing reader keeps working.

Full design: ``docs/todo/VN_VERSIONING_PLAN.md``. Julia composer this mirrors:
``app/src/helpers.jl``.
"""
from typing import Any, Mapping, Optional

VERSIONED_ACTIVE_KEY = "_active"
VERSIONED_DEFAULT_VAL = "default"
LATEST_ACTIVE_KEY = "_latest"
LATEST_DEFAULT_VAL = "v1"


# ── Outer axis (value_name variant) ─────────────────────────────────────────

def versioned_active(d: Mapping) -> str:
    """The active value_name of a versioned dict (``d[_active]``, or ``default``)."""
    return str(d.get(VERSIONED_ACTIVE_KEY, VERSIONED_DEFAULT_VAL))


def versioned_get(d: Mapping, value_name: Optional[str] = None) -> Any:
    """The entry under ``value_name`` (or under the active entry when ``None``)."""
    name = versioned_active(d) if value_name is None else str(value_name)
    return d.get(name)


def versioned_get_field(d: Mapping, field: str, value_name: Optional[str] = None) -> Any:
    """The entry for ``value_name`` under ``d[field]``. Returns the raw inner value regardless of
    shape (versioned entry OR legacy bare scalar). ``None`` when the field or value_name is absent.
    Non-dict field values pass through unchanged (legacy compat, matches Julia)."""
    inner = d.get(field)
    if inner is None:
        return None
    if not isinstance(inner, Mapping):
        return inner
    return versioned_get(inner, value_name)


def versioned_keys(d: Mapping) -> list[str]:
    """All user-facing value_names in a versioned dict (excludes ``_active``)."""
    return [str(k) for k in d.keys() if str(k) != VERSIONED_ACTIVE_KEY]


def resolve_value_name(d: Mapping, value_name: Optional[str] = None) -> str:
    """The value_name to act on: the caller's when given, else the versioned dict's active one.
    Mirror of the Julia ``resolve_value_name(img, value_name=nothing)`` — same fallback logic,
    lifted to a raw dict since Python side has no CciaImage struct."""
    if value_name is not None:
        return str(value_name)
    return versioned_active(d)


# ── Inner axis (version per value_name) ─────────────────────────────────────

def is_versioned_entry(x: Any) -> bool:
    """True when ``x`` is an inner versioned entry (has a ``_latest`` pointer)."""
    return isinstance(x, Mapping) and LATEST_ACTIVE_KEY in x


def version_latest(d: Mapping) -> str:
    """The latest version of an inner versioned entry (``d[_latest]``, or ``v1``)."""
    return str(d.get(LATEST_ACTIVE_KEY, LATEST_DEFAULT_VAL))


def version_get(d: Mapping, version: Optional[str] = None) -> Any:
    """The value stored under ``version`` (or under the latest entry when ``None``)."""
    ver = version_latest(d) if version is None else str(version)
    return d.get(ver)


def version_keys(d: Mapping) -> list[str]:
    """All user-facing versions in an inner versioned entry (excludes ``_latest``)."""
    return [str(k) for k in d.keys() if str(k) != LATEST_ACTIVE_KEY]


# ── Composers (walk both axes) ──────────────────────────────────────────────

def versioned_get_field_at(d: Mapping, field: str, value_name: Optional[str] = None,
                           version: Optional[str] = None) -> Any:
    """The leaf value at ``(field, value_name, version)``. Returns the entry unchanged on legacy
    bare-scalar shape (implicit ``v1``), the leaf value on new versioned-entry shape, and ``None``
    when the field/value_name is absent. Mirror of the Julia composer."""
    inner = versioned_get_field(d, field, value_name)
    if inner is None:
        return None
    if not is_versioned_entry(inner):
        return inner
    return version_get(inner, version)


def unversion_value(value: Any, version: Optional[str] = None) -> Any:
    """The one-argument composer: unwrap ``value`` on the version axis alone.

    Legacy (bare scalar / list) — returned unchanged.
    New shape (versioned entry) — returned as ``version_get(value, version)``.

    Entry point for callers that already hold the resolved-on-value_name entry (e.g. from
    ``versioned_get_field``) and want the leaf. Mirror of the Julia ``unversion_value``."""
    if value is None:
        return None
    if not is_versioned_entry(value):
        return value
    return version_get(value, version)


def resolve_version(entry: Any, version: Optional[str] = None) -> str:
    """The version string an ``entry`` resolves to. New-shape entries return their explicit
    ``version`` or ``_latest``; legacy (bare scalar / list / ``None``) entries return
    ``LATEST_DEFAULT_VAL`` (``v1``) — the implicit-v1 fallback that keeps old projects working.

    Companion to ``resolve_value_name`` for callers that need the version string for pinning /
    logging (chain-planner surface, task run logs). Mirror of the Julia ``resolve_version``."""
    if is_versioned_entry(entry):
        return version_latest(entry) if version is None else str(version)
    return LATEST_DEFAULT_VAL
