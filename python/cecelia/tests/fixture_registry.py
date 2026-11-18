"""Fixtures by property — the Python twin of `app/test/fixture_registry.jl`.

    from cecelia.tests.fixture_registry import fixture_for
    fx = fixture_for("cells", "calibrated", need="<id>")
    if fx is None:
        self.skipTest("waiting on test-data/NEEDS.toml → <id>")

`test-data/fixtures.toml` says what each committed fixture can prove; `test-data/NEEDS.toml` lists what the
tests need and no fixture has. A test that depends on a property of the data gets a fixture that has it,
or None — never the nearest stand-in. The facts behind each property are re-derived from the files by the
Julia `fixture registry` testsets (one place); this side reads the same two files.
Docs: docs/DEV.md → *Ask for properties, not a path*.
"""
from __future__ import annotations

import json
import os
import shutil
import tempfile
import tomllib
import warnings
from dataclasses import dataclass, field

TEST_DATA = os.path.normpath(os.path.join(os.path.dirname(__file__), "..", "..", "..", "test-data"))


def _projects_root() -> str:
    return os.environ.get("CECELIA_TEST_DATA") or os.path.join(TEST_DATA, "projects")


def _load(name: str) -> dict:
    with open(os.path.join(TEST_DATA, name), "rb") as f:
        return tomllib.load(f)


@dataclass(frozen=True)
class RegisteredFixture:
    uid: str
    project: str                 # project dir (absolute)
    dir: str | None              # where ccid.json lives; None for an image-only fixture
    image_dir: str | None
    value_name: str | None
    technology: str
    properties: frozenset[str]
    meta: dict = field(default_factory=dict)   # a derived fixture's ccid.json meta overrides

    @property
    def table(self) -> str:
        """The cell table of a fixture with `cells` (absolute path)."""
        return os.path.join(self.dir, "labelProps", f"{self.value_name}.h5ad")


def vocabulary() -> dict[str, tuple[bool, str]]:
    return {k: (bool(v[0]), str(v[1])) for k, v in _load("fixtures.toml")["vocabulary"].items()}


def needs() -> dict[str, dict]:
    return _load("NEEDS.toml").get("needs", {})


def registered_fixtures() -> list[RegisteredFixture]:
    raw = _load("fixtures.toml")["fixtures"]
    root = _projects_root()

    def absp(p):
        return None if p is None else os.path.join(root, *p.split("/"))

    out = []
    for uid in sorted(raw):     # the same stable pick as the Julia side
        e = raw[uid]
        if "derive" in e:
            e = {**raw[e["derive"]], **e}
        rel = e.get("dir") or e["image_dir"]
        out.append(RegisteredFixture(
            uid, os.path.join(root, rel.split("/")[0]), absp(e.get("dir")), absp(e.get("image_dir")),
            e.get("value_name"), e["technology"], frozenset(e["properties"]),
            {k: str(v) for k, v in e.get("meta", {}).items()} if "derive" in e else {}))
    return out


def materialise(fx: RegisteredFixture) -> RegisteredFixture:
    """A derived fixture as a fresh temp copy of its base's project, `meta` written into ccid.json."""
    if not fx.meta:
        return fx
    proj = os.path.join(tempfile.mkdtemp(), os.path.basename(fx.project))
    shutil.copytree(fx.project, proj)

    def re(p):
        return None if p is None else os.path.join(proj, os.path.relpath(p, fx.project))

    ccid_path = os.path.join(re(fx.dir), "ccid.json")
    with open(ccid_path, encoding="utf-8") as f:
        ccid = json.load(f)
    ccid["meta"] = {**ccid.get("meta", {}), **fx.meta}
    with open(ccid_path, "w", encoding="utf-8") as f:
        json.dump(ccid, f)
    return RegisteredFixture(fx.uid, proj, re(fx.dir), re(fx.image_dir), fx.value_name, fx.technology,
                             fx.properties, fx.meta)


_warned: set[str] = set()


def fixture_for(*props: str, technology: str | None = None, need: str | None = None) -> RegisteredFixture | None:
    """A registered fixture with every property in `props`, or None (then `need` must name a request in
    NEEDS.toml). No match and no `need` raises: file the request, don't use a fixture that lacks them."""
    vocab = vocabulary()
    bad = [p for p in props if p not in vocab]
    if bad:
        raise ValueError(f"fixture_for: unknown properties {bad} — see test-data/fixtures.toml → [vocabulary]")
    if need is not None and need not in needs():
        raise ValueError(f'fixture_for: need "{need}" is not in test-data/NEEDS.toml — add it there')
    for fx in registered_fixtures():
        if set(props) <= fx.properties and (technology is None or fx.technology == technology) \
                and os.path.isdir(fx.dir or fx.image_dir):
            return materialise(fx)
    if need is None:
        raise LookupError(f"fixture_for{props}: no registered fixture has these properties. Don't use one "
                          f'that lacks them: add a request to test-data/NEEDS.toml and pass need="<id>".')
    if need not in _warned:
        _warned.add(need)
        warnings.warn(f"FIXTURE NEEDED — tests waiting on test-data/NEEDS.toml → {need} are SKIPPED", stacklevel=2)
    return None
