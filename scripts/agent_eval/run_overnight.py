"""One unattended agent run (docs/todo/AGENT_OVERNIGHT_PLAN.md, P3): fixture → sandboxed `claude -p`
with a one-sentence brief → score + canary + navigation → run record.

    pixi run agent-eval-run --root /tmp/agent-night-1 [--brief vague|guided] [--fixture CROP_DIR]
                            [--budget-usd 10] [--timeout-min 120] [--model opus] [--dry-run]

Everything lands under `--root` (outside `~`, which the sandbox write-denies): the fixture project,
a detached checkout at HEAD the agent works in (so CLAUDE.md loads as in real use), the trace, and
`record.json` / `record.md`. The agent gets no MCP servers (`--strict-mcp-config`) — the observer
would point it at the user's real app — and `CECELIA_DEV_DIR` = the fixture's isolated dev dir.
`--dry-run` builds everything and writes the prompt without spawning.
"""
from __future__ import annotations

import argparse
import copy
import datetime as dt
import importlib.util
import json
import os
import pathlib
import subprocess
import sys

from cecelia.effectiveness import agent_sandbox
from cecelia.utils import vn_versioning
from cecelia.utils.atomic_io import write_atomic, write_json_atomic

HERE = pathlib.Path(__file__).resolve().parent
REPO = HERE.parents[1]
BRIEFS = HERE / "briefs"


def _load(path: pathlib.Path, key: str):
    spec = importlib.util.spec_from_file_location(key, path)
    mod = importlib.util.module_from_spec(spec)
    sys.modules[key] = mod
    spec.loader.exec_module(mod)
    return mod


score = _load(HERE / "score.py", "agent_eval_score")
setup = _load(HERE / "setup.py", "agent_eval_setup")


# ── prompt ───────────────────────────────────────────────────────────────────────────────────────

RESULT_PREFIX = "RESULT "


def render_brief(kind: str, run: dict) -> str:
    with open(BRIEFS / f"{kind}.md", encoding="utf-8") as f:
        text = f.read()
    return text.format(project_name="agent-eval", project_uid=run["projectUid"], n_images=len(run["images"]))


def parse_result_line(final_message: str) -> dict | None:
    """The agent's declared answer: the last `RESULT {json}` line of its final message."""
    for line in reversed((final_message or "").splitlines()):
        line = line.strip().strip("`")
        if line.startswith(RESULT_PREFIX):
            try:
                return json.loads(line[len(RESULT_PREFIX):])
            except json.JSONDecodeError:
                return None
    return None


# ── spawn ────────────────────────────────────────────────────────────────────────────────────────

def sandbox_settings(root: pathlib.Path) -> dict:
    """The shared agent containment, plus write access to this run's root only."""
    s = copy.deepcopy(agent_sandbox.SANDBOX_SETTINGS)
    s["sandbox"]["filesystem"]["allowWrite"] = [*s["sandbox"]["filesystem"]["allowWrite"], str(root)]
    return s


def build_command(claude: str, root: pathlib.Path, budget_usd: float, model: str | None) -> list[str]:
    cmd = [claude, "-p", "--dangerously-skip-permissions",
           "--settings", json.dumps(sandbox_settings(root)),
           "--strict-mcp-config",                     # no MCP servers: the observer points at the real app
           "--max-budget-usd", str(budget_usd),
           "--output-format", "stream-json", "--verbose"]
    return cmd + (["--model", model] if model else [])


def default_runner(cmd: list[str], prompt: str, cwd: pathlib.Path, env: dict, timeout: int):
    return subprocess.run(cmd, input=prompt, cwd=str(cwd), env=env, capture_output=True, text=True,
                          timeout=timeout, check=False, encoding="utf-8")


def scripted_ceiling_runner(cmd, prompt, cwd, env, timeout):
    """Stand-in for the agent: run the hand-tuned ceiling pipeline in its place and answer like one.
    Exercises setup → collect → score → canary → record at zero token cost."""
    root = pathlib.Path(cwd).parent
    p = subprocess.run(["julia", "--project=app", str(HERE / "ceiling.jl"), "--run", str(root / "run.json"),
                        "--value-name", "scripted"], cwd=str(cwd), env=env, capture_output=True, text=True,
                       timeout=timeout, check=False, encoding="utf-8")
    answer = 'done\nRESULT {"valueName": "scripted", "stateColumn": "live.cell.hmm.state.scripted"}'
    result = {"type": "result", "total_cost_usd": 0.0, "num_turns": 0, "result": answer}
    return subprocess.CompletedProcess(cmd, p.returncode, json.dumps(result) + "\n", p.stdout + p.stderr)


def make_checkout(root: pathlib.Path) -> pathlib.Path:
    """A detached checkout at HEAD — minus the copied `.env`, which points at the
    developer's real dev dir; the agent must only ever resolve the fixture's (`CECELIA_DEV_DIR`)."""
    dest = agent_sandbox.make_detached_worktree(REPO, root, "agent-run")
    (dest / ".env").unlink(missing_ok=True)
    return dest


def remove_checkout(dest: pathlib.Path) -> None:
    agent_sandbox.remove_worktree(REPO, dest)


# ── collect ──────────────────────────────────────────────────────────────────────────────────────

def tool_errors(stdout: str) -> int:
    """Tool calls that came back `is_error` — a navigation signal (wrong path, failed task, …)."""
    n = 0
    for line in stdout.splitlines():
        try:
            ev = json.loads(line)
        except json.JSONDecodeError:
            continue
        if ev.get("type") != "user":
            continue
        for block in (ev.get("message") or {}).get("content") or []:
            if isinstance(block, dict) and block.get("type") == "tool_result" and block.get("is_error"):
                n += 1
    return n


def value_names(run: dict, field: str = "label_props") -> dict[str, list[str]]:
    """Per image uid, the `field` value_names registered now."""
    out = {}
    for im in run["images"]:
        with open(pathlib.Path(run["projectDir"]) / "1" / im["uid"] / "ccid.json", encoding="utf-8") as f:
            entry = json.load(f).get(field) or {}
        out[im["uid"]] = vn_versioning.versioned_keys(entry) if isinstance(entry, dict) else []
    return out


def new_value_names(run: dict, field: str = "label_props") -> dict[str, list[str]]:
    """Per image uid, the `field` value_names that did not exist before the run."""
    before = run["snapshot"].get("valueNames", {})
    return {uid: [n for n in names if n not in before.get(uid, [])]
            for uid, names in value_names(run, field).items()}


def tasks_run(run: dict) -> dict[str, list[str]]:
    """Per image uid, `fun (status)` for each run the platform logged for it (runlog.json), in order."""
    out = {}
    for im in run["images"]:
        p = pathlib.Path(run["projectDir"]) / "1" / im["uid"] / "runlog.json"
        if not p.is_file():
            out[im["uid"]] = []
            continue
        with open(p, encoding="utf-8") as f:
            log = json.load(f)
        out[im["uid"]] = [f"{r.get('fun', '?')} ({r.get('status', '?')})" for r in log if isinstance(r, dict)]
    return out


def score_outputs(run: dict, declared: dict | None) -> dict:
    """Score every new label set; the declared one (if any) is the headline."""
    fixture_dir = pathlib.Path(run["fixtureDir"])
    with open(fixture_dir / "fixture.json", encoding="utf-8") as f:
        manifest = json.load(f)
    gt_by_name = {i["name"]: i["gt"] for i in manifest["images"]}
    news = new_value_names(run)
    state_col = (declared or {}).get("stateColumn")
    out = {}
    for im in run["images"]:
        gt, spec = score.load_gt(gt_by_name[im["name"]])
        per_vn = {}
        meta_dir = pathlib.Path(run["projectDir"]) / "1" / im["uid"]
        with open(meta_dir / "ccid.json", encoding="utf-8") as f:
            ccid = json.load(f)
        for vn in news[im["uid"]]:
            fn = vn_versioning.versioned_get_field_at(ccid, "label_props", vn)   # the `_latest` leaf
            p = meta_dir / "labelProps" / str(fn)
            if not fn or not p.is_file():
                continue
            pred = score.load_pred(str(p))
            col = state_col if state_col in pred.columns else next(
                (c for c in pred.columns if str(c).startswith("live.cell.hmm.state.")), None)
            per_vn[vn] = {"stateColumn": col,
                          **score.score_image(gt, pred, spec["radius_px"], col, spec.get("z_scale"))}
        out[im["name"]] = {"declared": (declared or {}).get("valueName"), "byValueName": per_vn}
    return out


def headline(scores: dict) -> dict:
    """Mean over images of the declared label set (else the best-F1 new one)."""
    rows = []
    for img in scores.values():
        by = img["byValueName"]
        vn = img["declared"] if img["declared"] in by else max(by, key=lambda k: by[k]["segmentation"]["f1"],
                                                                 default=None)
        if vn:
            rows.append(by[vn])
    def mean(f):
        v = [f(r) for r in rows if f(r) is not None]
        return sum(v) / len(v) if v else None
    return {"images_scored": len(rows),
            "seg_recall": mean(lambda r: r["segmentation"]["recall"]),
            "seg_f1": mean(lambda r: r["segmentation"]["f1"]),
            "link_recall": mean(lambda r: (r["tracking"] or {}).get("link_recall")),
            "link_precision": mean(lambda r: (r["tracking"] or {}).get("link_precision")),
            "state_accuracy": mean(lambda r: (r.get("behaviour") or {}).get("accuracy"))}


def render_record(rec: dict) -> str:
    h, nav, c = rec["headline"], rec["navigation"], rec["canary"]

    def f(v):
        return "—" if v is None else f"{v:.3f}"
    lines = [f"# Agent run — {rec['startedAt']}", "",
             f"Brief **{rec['brief']}** · fixture **{rec['fixtureKind']}** · {rec['claudeVersion']}"
             f"{' · ' + rec['model'] if rec['model'] else ''} · HEAD `{rec['sha'][:8]}`", "",
             "| | |", "|---|---|",
             f"| Declared result | `{json.dumps(rec['declared'])}` |",
             f"| Segmentation recall / F1 | {f(h['seg_recall'])} / {f(h['seg_f1'])} |",
             f"| Link recall / precision | {f(h['link_recall'])} / {f(h['link_precision'])} |",
             f"| State accuracy | {f(h['state_accuracy'])} |",
             f"| Canary | {'intact' if c['intact'] else 'BROKEN'} |",
             f"| Cost / turns / tool errors | ${nav['costUsd']:.2f} / {nav['turns']} / {nav['toolErrors']} |",
             f"| Wall-clock | {nav['wallClockMin']:.1f} min · exit {nav['exitCode']} |", ""]
    if not c["intact"]:
        lines += ["## Canary", "", "```json", json.dumps(c, indent=1)[:3000], "```", ""]
    lines += ["## Tasks run", ""] + [f"- `{uid}`: {', '.join(t) or '—'}" for uid, t in nav["tasksRun"].items()]
    lines += ["", "## Final message", "", "```", (rec["finalMessage"] or "")[-3000:], "```", ""]
    return "\n".join(lines)


# ── main ─────────────────────────────────────────────────────────────────────────────────────────

def run(a, runner=default_runner) -> dict:
    root = pathlib.Path(a.root).resolve()
    setup_args = ["--root", str(root), "--images", str(a.images), "--seed", str(a.seed), "--prior", a.prior]
    if a.fixture:
        setup_args += ["--fixture", a.fixture]
    setup.main(setup_args)
    with open(root / "run.json", encoding="utf-8") as f:
        state = json.load(f)
    state["snapshot"]["valueNames"] = value_names(state)    # what the agent found, e.g. the prior

    prompt = render_brief(a.brief, state)
    trace = root / "trace"
    trace.mkdir(exist_ok=True)
    (trace / "prompt.md").write_text(prompt, encoding="utf-8")
    from cecelia.effectiveness.claude_cli import resolve_claude_bin
    claude = a.claude_path or resolve_claude_bin()
    if not claude and not a.dry_run:
        raise SystemExit("no `claude` binary found — pass --claude-path")
    cmd = build_command(claude or "claude", root, a.budget_usd, a.model)
    (trace / "command.json").write_text(json.dumps(cmd), encoding="utf-8")
    if a.dry_run:
        print(json.dumps({"root": str(root), "prompt": str(trace / "prompt.md"), "dryRun": True}))
        return {}

    checkout = make_checkout(root)
    env = {**os.environ, "CECELIA_DEV_DIR": state["devDir"], "CECELIA_OBSERVER_NO_PAIR": "1"}
    for k in [k for k in env if k.startswith("CLAUDE_CODE_") or k in ("CLAUDECODE", "CLAUDE_PID")]:
        env.pop(k)                                           # a fresh session, not a child of this one
    started = dt.datetime.now(dt.timezone.utc)
    try:
        proc = runner(cmd, prompt, checkout, env, a.timeout_min * 60)
        stdout, stderr, code = proc.stdout or "", proc.stderr or "", proc.returncode
    except subprocess.TimeoutExpired as e:
        stdout, stderr, code = (e.stdout or ""), (e.stderr or "") + "\n[timeout]", -1
        stdout = stdout.decode() if isinstance(stdout, bytes) else stdout
        stderr = stderr.decode() if isinstance(stderr, bytes) else stderr
    finally:
        remove_checkout(checkout)
    minutes = (dt.datetime.now(dt.timezone.utc) - started).total_seconds() / 60
    (trace / "stream.jsonl").write_text(stdout, encoding="utf-8")
    (trace / "stderr.txt").write_text(stderr, encoding="utf-8")

    sig = agent_sandbox.parse_stream_json(stdout)
    declared = parse_result_line(sig.final_message)
    scores = score_outputs(state, declared)
    with open(pathlib.Path(state["fixtureDir"]) / "fixture.json", encoding="utf-8") as f:
        fixture_kind = json.load(f)["kind"]
    rec = {
        "schemaVersion": 1, "startedAt": started.isoformat(timespec="seconds"),
        "brief": a.brief, "fixtureKind": fixture_kind, "model": a.model, "budgetUsd": a.budget_usd,
        "sha": subprocess.run(["git", "rev-parse", "HEAD"], cwd=REPO, capture_output=True, text=True).stdout.strip(),
        "claudeVersion": subprocess.run([cmd[0], "--version"], capture_output=True, text=True).stdout.strip(),
        "declared": declared, "headline": headline(scores), "scores": scores,
        "canary": score.check_canary(state["snapshot"], score.snapshot(state["projectDir"])),
        "navigation": {"costUsd": sig.cost_usd, "turns": sig.turns, "toolCalls": len(sig.tool_calls),
                       "toolErrors": tool_errors(stdout), "exitCode": code, "wallClockMin": minutes,
                       "tasksRun": tasks_run(state), "newValueNames": new_value_names(state)},
        "finalMessage": sig.final_message, "root": str(root),
    }
    write_json_atomic(root / "record.json", rec, indent=2, default=float)
    with write_atomic(root / "record.md") as f:
        f.write(render_record(rec))
    print(json.dumps({"record": str(root / "record.md"), "headline": rec["headline"],
                      "canaryIntact": rec["canary"]["intact"], "costUsd": sig.cost_usd}, default=float))
    return rec


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--root", required=True)
    ap.add_argument("--brief", choices=["vague", "guided"], default="vague")
    ap.add_argument("--fixture", default=None, help="an existing fixture dir (crop.py output)")
    ap.add_argument("--images", type=int, default=2)
    ap.add_argument("--seed", type=int, default=0)
    ap.add_argument("--prior", choices=["cellpose", "none"], default="cellpose")
    ap.add_argument("--budget-usd", type=float, default=10.0)
    ap.add_argument("--timeout-min", type=int, default=120)
    ap.add_argument("--model", default=None)
    ap.add_argument("--claude-path", default=None)
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--scripted-ceiling", action="store_true",
                    help="run the ceiling pipeline instead of an agent (tests the harness, costs nothing)")
    a = ap.parse_args(argv)
    run(a, runner=scripted_ceiling_runner if a.scripted_ceiling else default_runner)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
