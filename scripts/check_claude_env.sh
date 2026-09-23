#!/usr/bin/env bash
# Pre-rollout check: any ambient credential env var will silently override CLAUDE_CONFIG_DIR
# and cause Kiwi turns to authenticate as whoever set the var, not as the selected Cecelia
# profile. Failure is silent (no error surfaced to the user). See
# docs/todo/LOGIN_CREDENTIAL_ISOLATION_PLAN.md P1.
set -u

VARS='ANTHROPIC_API_KEY|ANTHROPIC_AUTH_TOKEN|CLAUDE_CODE_OAUTH_TOKEN|CLAUDE_CODE_USE_BEDROCK|CLAUDE_CODE_USE_VERTEX|CLAUDE_CODE_USE_FOUNDRY|AWS_BEARER_TOKEN_BEDROCK'
found=0

hit() { printf '  %s\n' "$1"; found=1; }

echo '== current shell environment =='
env_hits=$(env | grep -E "^(${VARS})=" | sed 's/=.*/=<REDACTED>/')
if [ -n "$env_hits" ]; then echo "$env_hits" | while read -r l; do hit "$l"; done; found=1; else echo '  (clean)'; fi

echo '== user rc/profile files =='
for f in "$HOME/.bashrc" "$HOME/.profile" "$HOME/.bash_profile" "$HOME/.zshrc" "$HOME/.zprofile" "$HOME/.bash_aliases" "$HOME/.config/fish/config.fish"; do
  [ -f "$f" ] || continue
  hits=$(grep -nE "^[[:space:]]*(export[[:space:]]+)?(${VARS})=" "$f" || true)
  if [ -n "$hits" ]; then echo "-- $f"; echo "$hits" | while read -r l; do hit "$l"; done; fi
done
[ "$found" = 0 ] && echo '  (clean)'

echo '== /etc profile files (may require sudo to read) =='
etc_files='/etc/environment /etc/profile'
for d in /etc/profile.d/*.sh; do [ -e "$d" ] && etc_files="$etc_files $d"; done
for f in $etc_files; do
  [ -f "$f" ] || continue
  if [ ! -r "$f" ]; then echo "-- $f (unreadable — re-run with sudo to check)"; continue; fi
  hits=$(grep -nE "^[[:space:]]*(export[[:space:]]+)?(${VARS})=" "$f" || true)
  if [ -n "$hits" ]; then echo "-- $f"; echo "$hits" | while read -r l; do hit "$l"; done; fi
done

echo
if [ "$found" = 0 ]; then
  echo 'OK — no ambient Claude credential env vars found.'
  exit 0
else
  echo 'FAIL — one or more ambient credential env vars present. These will silently override'
  echo 'CLAUDE_CONFIG_DIR and make every Kiwi turn authenticate as whoever set the var. Remove'
  echo 'them (or arrange for Cecelia to scrub them at spawn — see LOGIN_CREDENTIAL_ISOLATION_PLAN P2)'
  echo 'before enabling multi-profile.'
  exit 1
fi
