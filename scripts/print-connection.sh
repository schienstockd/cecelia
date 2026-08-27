#!/usr/bin/env sh
# Emit a Cecelia remote-access connection profile as JSON to stdout.
#
# Probes cloud metadata for a public IP (GCP → AWS → Azure), falls through to
# CECELIA_PUBLIC_HOST env, then prints nothing (exit 0) if no host resolves —
# a local install has nothing to hand off. See docs/todo/REMOTE_ACCESS_PLAN.md.
#
# Each probe is capped at 1s so the installer never hangs on an unreachable
# metadata endpoint (e.g. running this on a laptop).
set -eu

_probe_gcp_ip() {
  curl -fsS --max-time 1 -H "Metadata-Flavor: Google" \
    "http://metadata.google.internal/computeMetadata/v1/instance/network-interfaces/0/access-configs/0/external-ip" \
    2>/dev/null || true
}

_probe_aws_ip() {
  # IMDSv2 (v1 disabled by default on new instances). If token fetch fails we skip.
  _TOKEN=$(curl -fsS --max-time 1 -X PUT \
    -H "X-aws-ec2-metadata-token-ttl-seconds: 60" \
    "http://169.254.169.254/latest/api/token" 2>/dev/null || true)
  [ -z "${_TOKEN:-}" ] && return 0
  curl -fsS --max-time 1 -H "X-aws-ec2-metadata-token: $_TOKEN" \
    "http://169.254.169.254/latest/meta-data/public-ipv4" 2>/dev/null || true
}

_probe_azure_ip() {
  curl -fsS --max-time 1 -H "Metadata: true" \
    "http://169.254.169.254/metadata/instance/network/interface/0/ipv4/ipAddress/0/publicIpAddress?api-version=2021-02-01&format=text" \
    2>/dev/null || true
}

HOST="${CECELIA_PUBLIC_HOST:-}"
[ -z "$HOST" ] && HOST=$(_probe_gcp_ip)
[ -z "$HOST" ] && HOST=$(_probe_aws_ip)
[ -z "$HOST" ] && HOST=$(_probe_azure_ip)

if [ -z "$HOST" ]; then
  exit 0
fi

CONN_USER="${USER:-$(whoami 2>/dev/null || echo cecelia)}"
LABEL=$(uname -n 2>/dev/null || echo cecelia-vm)

cat <<EOF
{
  "host": "$HOST",
  "user": "$CONN_USER",
  "localPort": 8080,
  "remotePort": 8080,
  "label": "$LABEL"
}
EOF
