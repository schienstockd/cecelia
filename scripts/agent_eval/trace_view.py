"""Read an agent run's stream-json trace as a transcript: its text, each tool call + a slice of the
result (errors marked), and the final result line. Safe on a trace still being written.

    python scripts/agent_eval/trace_view.py ~/.cecelia-effectiveness/app-runs/<run>/trace.jsonl [--width 300] [--tail 40]
"""
from __future__ import annotations

import argparse
import json
import sys

from cecelia.effectiveness.claude_cli import rate_limit


def _result_text(content) -> str:
    return content if isinstance(content, str) else " ".join(
        x.get("text", "") for x in content or [] if isinstance(x, dict))


def events(path: str) -> list[dict]:
    """The trace in order, one dict per item: `init` {model, sessionId, tools, mcp}, `text` {text},
    `call` {id, name, input}, `result` {id, text, isError, images: [base64 PNG]}, `final` {subtype,
    cost, turns, text, rateLimited (the CLI's message when the run ended on the usage limit, else
    None)}. Tool names lose their `mcp__<server>__` prefix."""
    with open(path, encoding="utf-8", errors="replace") as f:
        raws = f.readlines()
    out = []
    for raw in raws:
        try:
            ev = json.loads(raw)
        except json.JSONDecodeError:
            continue
        if ev.get("type") == "system" and ev.get("subtype") == "init":
            out.append({"kind": "init", "model": ev.get("model"), "sessionId": ev.get("session_id"),
                        "tools": len(ev.get("tools") or []),
                        "mcp": [(s.get("name"), s.get("status")) for s in ev.get("mcp_servers") or []]})
        for b in (ev.get("message") or {}).get("content") or []:
            if not isinstance(b, dict):
                continue
            if b.get("type") == "text":
                out.append({"kind": "text", "text": b["text"]})
            elif b.get("type") == "tool_use":
                out.append({"kind": "call", "id": b.get("id"), "name": b.get("name", "?").split("__")[-1],
                            "input": b.get("input") or {}})
            elif b.get("type") == "tool_result":
                c = b.get("content")
                images = [x["source"]["data"] for x in c if isinstance(x, dict) and x.get("type") == "image"
                          and (x.get("source") or {}).get("data")] if isinstance(c, list) else []
                out.append({"kind": "result", "id": b.get("tool_use_id"), "text": _result_text(c),
                            "isError": bool(b.get("is_error")), "images": images})
        if ev.get("type") == "result":
            out.append({"kind": "final", "subtype": ev.get("subtype"), "cost": ev.get("total_cost_usd"),
                        "turns": ev.get("num_turns"), "text": ev.get("result") or "",
                        "rateLimited": rate_limit(ev)})
    return out


def lines(path: str, width: int) -> list[str]:
    out = []
    for e in events(path):
        if e["kind"] == "init":
            out.append(f"INIT model={e['model']} tools={e['tools']} mcp={e['mcp']}")
        elif e["kind"] == "text":
            out.append("TEXT " + e["text"][: width * 3])
        elif e["kind"] == "call":
            out.append(f"CALL {e['name']} {json.dumps(e['input'])[:width]}")
        elif e["kind"] == "result":
            out.append(f"  {'ERR ' if e['isError'] else ''}→ {e['text'][:width]}")
        elif e["kind"] == "final":
            limited = " RATE-LIMITED" if e["rateLimited"] else ""
            out.append(f"RESULT {e['subtype']}{limited} cost=${e['cost']} turns={e['turns']}\n{e['text']}")
    return out


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("trace")
    ap.add_argument("--width", type=int, default=300)
    ap.add_argument("--tail", type=int, default=0)
    a = ap.parse_args(argv)
    out = lines(a.trace, a.width)
    print("\n".join(out[-a.tail:] if a.tail else out))
    return 0


if __name__ == "__main__":
    sys.exit(main())
