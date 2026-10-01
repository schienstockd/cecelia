"""The viewer's WGSL for the movie renderer: read, expand, and pack uniforms for the same shaders.

The shaders live in ``frontend/src/lib/webgpu/shaders/`` and the browser viewer runs them
(``shaderSource.ts``). This module is that file's twin — same two expansion rules, same uniform
layouts — so a movie frame runs the text the viewer runs, not a port of it
(``docs/todo/SHARED_RENDERER_PLAN.md`` Decisions 1 and 3).

- ``#include "x.wgsl" [NAME=VALUE ...]`` on a line of its own → x.wgsl, expanded. VALUE is a variable
  name (resolved in the includer's scope) or a literal; the included file sees the includer's
  variables plus these.
- ``${NAME}`` → a key of ``constants.json`` or a variable the caller passes. Unknown → ``KeyError``.

Numbers print the way JavaScript's ``String(n)`` does for the values the shaders use (``32``,
``0.45``, and an integral float without its ``.0``). Both sides are tested against
``shaders/golden.json``.
"""
import json
import pathlib
import re
from typing import Callable, Dict, List, Mapping, Optional, Union

import numpy as np

#: ``python/cecelia/utils/wgsl_utils.py`` → repo root (parents[3]) → the viewer's shader directory.
SHADER_DIR = pathlib.Path(__file__).resolve().parents[3] / "frontend" / "src" / "lib" / "webgpu" / "shaders"

Var = Union[str, int, float]

_INCLUDE = re.compile(r'^[ \t]*#include[ \t]+"([\w.-]+)"((?:[ \t]+\w+=[\w.-]+)*)[ \t]*$', re.M)
_TOKEN = re.compile(r"\$\{([A-Za-z_]\w*)\}")
_LANE = re.compile(r"^(\w+)(?:\[(\d+)\])?\.(\w+)$")

_constants: Optional[Dict[str, Var]] = None
_uniforms: Optional[dict] = None


def shader_constants() -> Dict[str, Var]:
    """``constants.json`` — MAX_CHANNELS, LUT_STOPS, VIEW_HALF_ANGLE, the pick bindings."""
    global _constants
    if _constants is None:
        with open(SHADER_DIR / "constants.json", encoding="utf-8") as f:
            _constants = json.load(f)
    return _constants


def wgsl_file(name: str) -> str:
    """The raw text of ``shaders/<name>``, before includes and substitution."""
    with open(SHADER_DIR / name, encoding="utf-8") as f:
        return f.read()


def format_var(v: Var) -> str:
    """A variable as the browser prints it: ``String(v)`` in JavaScript."""
    if isinstance(v, bool):
        raise TypeError("wgsl: a boolean is not a shader value")
    if isinstance(v, float):
        return str(int(v)) if v.is_integer() else repr(v)
    return str(v)


def expand_source(name: str, variables: Mapping[str, Var], read: Callable[[str], str],
                  stack: Optional[List[str]] = None) -> str:
    """The expansion rules over any reader: ``variables`` is the whole scope, no constants added."""
    stack = stack or []
    if name in stack:
        raise ValueError(f"wgsl: include cycle {' → '.join(stack + [name])}")

    def include(m: "re.Match[str]") -> str:
        scope = dict(variables)
        for kv in m.group(2).split():
            k, v = kv.split("=", 1)
            scope[k] = variables[v] if v in variables else v
        # Drop the included text's trailing newline: the include line's own newline ends the block.
        out = expand_source(m.group(1), scope, read, stack + [name])
        return out[:-1] if out.endswith("\n") else out

    def token(m: "re.Match[str]") -> str:
        key = m.group(1)
        if key not in variables:
            raise KeyError(f"wgsl: {name} uses ${{{key}}} but it is not defined")
        return format_var(variables[key])

    return _TOKEN.sub(token, _INCLUDE.sub(include, read(name)))


def expand_wgsl(name: str, **variables: Var) -> str:
    """``shaders/<name>`` with includes inlined and every ``${NAME}`` substituted."""
    return expand_source(name, {**shader_constants(), **variables}, wgsl_file)


class UniformLayout:
    """One uniform block of ``uniforms.json``: every field a ``vec4<f32>`` (or an array of them)."""

    def __init__(self, name: str):
        global _uniforms
        if _uniforms is None:
            with open(SHADER_DIR / "uniforms.json", encoding="utf-8") as f:
                _uniforms = json.load(f)
        if name not in _uniforms:
            raise KeyError(f"wgsl: no uniform block '{name}'")
        spec = _uniforms[name]
        self.name, self.struct, self.file = name, spec["struct"], spec["file"]
        self.fields = spec["fields"]
        #: field → (first f32 slot, element count, {lane: offset 0..3}, is an array)
        self._at: Dict[str, tuple] = {}
        slot = 0
        for f in self.fields:
            if len(f["lanes"]) != 4:
                raise ValueError(f"wgsl: {name}.{f['name']} must name 4 lanes")
            count = self._count(f)
            self._at[f["name"]] = (slot, count, {lane: i for i, lane in enumerate(f["lanes"])}, "count" in f)
            slot += 4 * count
        self.floats = slot
        self.bytes = slot * 4

    @staticmethod
    def _count(f: dict) -> int:
        if "count" not in f:
            return 1
        c = f["count"]
        n = shader_constants().get(c, c)
        n = int(n)
        if n < 1:
            raise ValueError(f"wgsl: field {f['name']} has bad count '{c}'")
        return n

    def slot(self, lane: str) -> int:
        """f32 slot of ``'cam.dist'`` or ``'ch[3].hi'``."""
        m = _LANE.match(lane)
        if not m or m.group(1) not in self._at:
            raise KeyError(f"wgsl: no lane '{lane}' in {self.struct}")
        base, count, lanes, is_array = self._at[m.group(1)]
        if m.group(3) not in lanes:
            raise KeyError(f"wgsl: no lane '{lane}' in {self.struct}")
        idx = m.group(2)
        if idx is not None and not is_array:
            raise KeyError(f"wgsl: '{lane}' indexes a field that is not an array")
        i = int(idx or 0)
        if i >= count:
            raise IndexError(f"wgsl: lane '{lane}' is out of range")
        return base + i * 4 + lanes[m.group(3)]

    def pack(self, values: Mapping[str, float]) -> np.ndarray:
        """``{'cam.dist': 120, 'ch[2].hi': 4000}`` → the block as float32; lanes not given are 0."""
        u = np.zeros(self.floats, dtype=np.float32)
        for lane, v in values.items():
            u[self.slot(lane)] = v
        return u
