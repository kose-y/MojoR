#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: check_mirror_parity.sh [--allow-mismatch] [--strict] [--help]

Migration-complete guard:
- verifies tracked files do not contain legacy mirror path tokens.

Options:
  --allow-mismatch  Report violations but do not fail.
  --strict          Backward-compatible no-op (strict is default).
  --help            Show this help text.
USAGE
}

ALLOW_MISMATCH=0
while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
    --allow-mismatch) ALLOW_MISMATCH=1; shift ;;
    --strict) shift ;;
    --help|-h) usage; exit 0 ;;
    *) echo "Unknown argument: $1" >&2; usage; exit 1 ;;
  esac
done

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

legacy_token="$(printf 'prototype%s' '/')"

hits="$(git grep -n "$legacy_token" || true)"
if [[ -n "$hits" ]]; then
  echo "Legacy mirror token violations found in tracked files:"
  printf '%s\n' "$hits"
  if [[ "$ALLOW_MISMATCH" -eq 1 ]]; then
    echo "Continuing because --allow-mismatch was provided."
    exit 0
  fi
  exit 1
fi

echo "Mirror parity guard: clean (no legacy mirror path tokens)."
exit 0
