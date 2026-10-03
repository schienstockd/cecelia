"""Read an agent run's stream-json trace as a transcript: its text, each tool call + a slice of the
result (errors marked), and the final result line. Safe on a trace still being written.

    python scripts/agent_eval/trace_view.py /tmp/cecelia-agent-app/<run>/trace.jsonl [--width 300] [--tail 40]
"""
from __future__ import annotations

import argparse
import json
import sys


def lines(path: str, width: int) -> list[str]:
    out = []
    for raw in open(path, encoding="utf-8", errors="replace"):
        try:
            ev = json.loads(raw)
        except json.JSONDecodeError:
            continue
        if ev.get("type") == "system" and ev.get("subtype") == "init":
            out.append(f"INIT model={ev.get('model')} tools={len(ev.get('tools') or [])} "
                       f"mcp={[(s.get('name'), s.get('status')) for s in ev.get('mcp_servers') or []]}")
        for b in (ev.get("message") or {}).get("content") or []:
            if not isinstance(b, dict):
                continue
            if b.get("type") == "text":
                out.append("TEXT " + b["text"][: width * 3])
            elif b.get("type") == "tool_use":
                name = b.get("name", "?").split("__")[-1]
                out.append(f"CALL {name} {json.dumps(b.get('input'))[:width]}")
            elif b.get("type") == "tool_result":
                c = b.get("content")
                text = c if isinstance(c, str) else " ".join(
                    x.get("text", "") for x in c or [] if isinstance(x, dict))
                out.append(f"  {'ERR ' if b.get('is_error') else ''}→ {text[:width]}")
        if ev.get("type") == "result":
            out.append(f"RESULT {ev.get('subtype')} cost=${ev.get('total_cost_usd')} "
                       f"turns={ev.get('num_turns')}\n{ev.get('result') or ''}")
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
