# Cecelia MCP observer server (Phase 1 — read-only)

A [Model Context Protocol](https://modelcontextprotocol.io) server that gives Claude **read-only**
access to a running Cecelia project — project state, images, task logs, QC, and the lab log — plus a
single **append-only** write to the lab log. This is Phase 1 ("Observer") of the arc in
[`docs/ai-assist/OBSERVER.md`](../docs/ai-assist/OBSERVER.md); the write phases (`submit_task`,
`adjust_params`, `acknowledge_flag`) are deliberately **not** wired here.

It's a **standalone stdio process** (Python + [FastMCP](https://github.com/modelcontextprotocol/python-sdk))
that talks to the Julia API over HTTP. It is separate infra, not part of the `cecelia` Python package
(one language per top-level dir).

## Layout

```
mcp/
  cecelia_mcp/
    client.py    # read-only HTTP client + the ALLOW-LIST (stdlib only; the no-mutation guarantee)
    server.py    # FastMCP server — wires the client into 8 read tools + append_lab_log
  tests/
    test_client.py   # stdlib unittest, HTTP mocked
```

## Tools

| Tool | Backing route | Returns |
|---|---|---|
| `get_project_info(project_uid)` | `GET /api/images` | name, kind, image count, sets, per-status breakdown |
| `list_images(project_uid)` | `GET /api/images` | every image: uid, name, status, set |
| `get_image_info(project_uid, image_uid)` | `GET /api/images/meta` | channels, dims, physical sizes, labels, QC, run log, note |
| `get_image_notes(project_uid, image_uid)` | `GET /api/images/meta` | the user's note for the image |
| `get_qc_metrics(project_uid, image_uid)` | `GET /api/images/meta` | per-image QC flags/metrics |
| `get_task_log(project_uid, image_uid, fun)` | `GET /api/images/tasklog` | raw log text for one task fn on one image |
| `get_task_history(project_uid, limit=100)` | `GET /api/tasks/history` | recent runs across all images, newest first |
| `read_lab_log(project_uid)` | `GET /api/lablog` | the full lab-log markdown |
| `append_lab_log(project_uid, lines)` | `POST /api/lablog/append` | **the only write** — appends a dated `[Claude]` entry, append-only |

## The no-mutation guarantee

Every request goes through `ALLOWED_ROUTES` in `client.py`. The only non-GET route on it is
`POST /api/lablog/append` (append-only, itself server-guarded). A call to any other route raises
`DisallowedRoute` — so if a future tool ever wires in a mutating route, the test suite fails loudly
rather than a project being silently mutated. `test_client.py` asserts append is the sole write.

## Running it

1. Start the Cecelia backend: `pixi run dev` (serves the API on `:8080`).
2. Start this server: `pixi run mcp` (stdio). Override the API with `CECELIA_API_URL`.

### Wire into Claude Code

Add to your Claude Code MCP config (adjust the absolute path to this checkout):

```json
{
  "mcpServers": {
    "cecelia-observer": {
      "command": "pixi",
      "args": ["run", "mcp"],
      "cwd": "/home/dominik/cc-workspace/cecelia/cecelia-pineapple"
    }
  }
}
```

Then, with a project open, ask e.g. *"describe my project"*, *"what failed on image 7?"*, or
*"note in the lab log that I switched cellpose diameter to 30 because the nuclei were undersegmented."*

## Tests

`pixi run test-mcp` — stdlib `unittest`, HTTP mocked, no server required.
