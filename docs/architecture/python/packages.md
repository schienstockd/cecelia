# Python — the parts and who imports whom

> **Generated** by `scripts/architecture/python_extract.py` — do not edit; run `pixi run arch-diagrams`.


Python has three jobs here: **task runners** (`app/src/tasks/<category>/*_run.py`, run by path through
`run_py`, never imported), the **`cecelia` IO library** they import (`python/cecelia/utils` +
`writers`), and side processes (the MCP server Claude talks to, the preview worker). Dev tooling
(recital, judge) lives in `cecelia.effectiveness` and is not part of the app.

`A → B (n)`: n files in A import something from B. Runners are grouped by task category.

```mermaid
flowchart LR
  subgraph RUN["task runners — app/src/tasks (run by run_py)"]
    runners__behaviour["runners: behaviour<br/><small>1 files · 0.4k lines</small>"]
    runners__cleanupImages["runners: cleanupImages<br/><small>6 files · 1.5k lines</small>"]
    runners__clustPops["runners: clustPops<br/><small>1 files · 0.1k lines</small>"]
    runners__clustRegions["runners: clustRegions<br/><small>1 files · 0.2k lines</small>"]
    runners__clustTracks["runners: clustTracks<br/><small>1 files · 0.1k lines</small>"]
    runners__editImages["runners: editImages<br/><small>9 files · 1.6k lines</small>"]
    runners__exportImages["runners: exportImages<br/><small>1 files · 0.2k lines</small>"]
    runners__importImages["runners: importImages<br/><small>12 files · 0.9k lines</small>"]
    runners__opticalFlow["runners: opticalFlow<br/><small>2 files · 1.7k lines</small>"]
    runners__segment["runners: segment<br/><small>9 files · 2.0k lines</small>"]
    runners__spatialAnalysis["runners: spatialAnalysis<br/><small>6 files · 0.7k lines</small>"]
    runners__tracking["runners: tracking<br/><small>3 files · 0.1k lines</small>"]
  end
  MCP_server__cecelia_mcp_["MCP server (cecelia_mcp)<br/><small>19 files · 6.9k lines</small>"]
  cecelia_utils["cecelia.utils<br/><small>61 files · 22.6k lines</small>"]
  cecelia_writers["cecelia.writers<br/><small>4 files · 1.2k lines</small>"]
  cecelia_client["cecelia_client<br/><small>1 files · 0.1k lines</small>"]
  dev_tooling__effectiveness_["dev tooling (effectiveness)<br/><small>19 files · 6.1k lines</small>"]
  preview_worker["preview worker<br/><small>1 files · 1.7k lines</small>"]
  runners__importImages -->|12| cecelia_utils
  runners__editImages -->|9| cecelia_utils
  runners__segment -->|9| cecelia_utils
  MCP_server__cecelia_mcp_ -->|6| cecelia_utils
  runners__cleanupImages -->|6| cecelia_utils
  runners__spatialAnalysis -->|6| cecelia_utils
  cecelia_writers -->|4| cecelia_utils
  dev_tooling__effectiveness_ -->|4| cecelia_utils
  runners__tracking -->|3| cecelia_utils
  runners__opticalFlow -->|2| cecelia_utils
  cecelia_client -->|1| cecelia_utils
  preview_worker -->|1| cecelia_utils
  preview_worker -->|1| cecelia_writers
  runners__behaviour -->|1| cecelia_utils
  runners__clustPops -->|1| cecelia_utils
  runners__clustRegions -->|1| cecelia_utils
  runners__clustTracks -->|1| cecelia_utils
  runners__exportImages -->|1| cecelia_utils
```

Runners and side processes never import each other; everything shared goes through `cecelia.utils`. An arrow between two runner groups or into a runner would be a violation.

Arrows into a runner group today: **0**
