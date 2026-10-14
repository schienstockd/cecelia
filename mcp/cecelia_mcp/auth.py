"""The API token every Cecelia request needs (the server's gate: app/src/api_token.jl).

Loopback is shared by every account on the machine, so the server only answers a request carrying
`Authorization: Bearer <token>`, where the token is `<config_dir>/api-token` — a file only the user
who launched Cecelia can read. The in-app observer registration passes its path as
`CECELIA_API_TOKEN_FILE` (the path, not the secret, so the secret never lands in Claude's config);
`CECELIA_API_TOKEN` carries the value itself. Without either, the default config dir is tried.
"""
from __future__ import annotations

from cecelia.utils.loopback import api_token  # the one lookup: env value, env file, `<config_dir>/api-token`


def auth_headers() -> dict[str, str]:
    """`{"Authorization": "Bearer <token>"}`, or {} when there is no token."""
    tok = api_token()
    return {"Authorization": f"Bearer {tok}"} if tok else {}
