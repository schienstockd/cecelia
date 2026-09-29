# Retired eval prompts

Prompts here **do not run in the weekly cron** — the runner enumerates `prompts/*.md`
non-recursively (`run_suite.py:_list_prompt_ids`), so files in this subdirectory are
skipped without any filter code.

## Why prompts land here

Under the diagnostic frame (see
[`docs/todo/CLAUDE_MD_EVAL_REFRESH_ROUTINE.md`](../../../../docs/todo/CLAUDE_MD_EVAL_REFRESH_ROUTINE.md)
→ *TL;DR*), every prompt is a hypothesis about a weakness. A prompt retires when its
result is delivered — either because the weakness is closed (stable-compliant with a
ratchet backstop), or because the layer that could fix it isn't the prompt itself
(rule not teachable → escalate + retire).

## What's currently here (2026-09-29)

Seven backend I/O prompts that scored stable 3/3 and are already enforced by ratchets:

| Prompt | Ratchet that already enforces the rule |
|---|---|
| `h5ad-read.md`, `h5ad-write.md` | `python/cecelia/tests/test_h5ad_access_convention.py` |
| `zarr-read.md`, `zarr-write.md`, `crop-failure.md` | `python/cecelia/tests/test_zarr_access_convention.py` + `zarr-access ratchet` in `app/test/suite.jl` |
| `utf-8-json-write.md` | `python/cecelia/tests/test_utf8_encoding_convention.py` |
| `spawn-python.md` | `python spawn ratchet` in `app/test/suite/ratchets.jl` |

Retirement decision recorded in
[`docs/todo/CLAUDE_MD_EVAL_PUNCHLIST.md`](../../../../docs/todo/CLAUDE_MD_EVAL_PUNCHLIST.md)
→ *P4 — Retire the stable-3/3 backend anchors*.

## Un-retiring a prompt

If a ratchet is later removed or weakened, `git mv` the corresponding prompt back to
`prompts/` and note the reason in the frontmatter. A retired prompt is not deleted —
history lives in git, and un-retirement should be one-step.
