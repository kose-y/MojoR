#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: bootstrap_local_ci.sh [options]

Installs managed local git hooks for MojoR CI gates.

Options:
  --pre-commit-mode <mode>   one of: gates, gates-tests, off (default: gates)
  --pre-push-mode <mode>     one of: off, gates, rc (default: off)
  --allow-artifacts          pass --allow-artifacts to gate scripts in hooks
  --verbose                  print details
  --help, -h                 show this help

Exit codes:
  0 success
  2 usage error
USAGE
}

PRE_COMMIT_MODE="gates"
PRE_PUSH_MODE="off"
ALLOW_ARTIFACTS=0
VERBOSE=0

while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
    --pre-commit-mode)
      PRE_COMMIT_MODE="${2:-}"
      shift 2
      ;;
    --pre-push-mode)
      PRE_PUSH_MODE="${2:-}"
      shift 2
      ;;
    --allow-artifacts)
      ALLOW_ARTIFACTS=1
      shift
      ;;
    --verbose)
      VERBOSE=1
      shift
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 2
      ;;
  esac
done

case "$PRE_COMMIT_MODE" in
  gates|gates-tests|off) ;;
  *) echo "Invalid --pre-commit-mode: $PRE_COMMIT_MODE" >&2; exit 2 ;;
esac

case "$PRE_PUSH_MODE" in
  off|gates|rc) ;;
  *) echo "Invalid --pre-push-mode: $PRE_PUSH_MODE" >&2; exit 2 ;;
esac

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
HOOK_DIR="$REPO_ROOT/.git/hooks"
mkdir -p "$HOOK_DIR"

write_managed_hook() {
  local hook_path="$1"
  local body="$2"

  if [ -f "$hook_path" ] && ! grep -q "MOJOR_MANAGED_HOOK" "$hook_path"; then
    local backup="$hook_path.mojor_backup_$(date -u +%Y%m%dT%H%M%SZ)"
    cp "$hook_path" "$backup"
    [ "$VERBOSE" -eq 1 ] && echo "Backed up existing hook: $backup"
  fi

  cat > "$hook_path" <<EOF
#!/usr/bin/env bash
set -euo pipefail
# MOJOR_MANAGED_HOOK

REPO_ROOT="\$(cd "\$(dirname "\$0")/../.." && pwd)"
cd "\$REPO_ROOT"

$body
EOF
  chmod +x "$hook_path"
  [ "$VERBOSE" -eq 1 ] && echo "Installed hook: $hook_path"
}

make_gate_cmd() {
  local base_cmd="$1"
  local add_tests="$2"
  local cmd="$base_cmd"
  if [ "$ALLOW_ARTIFACTS" -eq 1 ]; then
    cmd="$cmd --allow-artifacts"
  fi
  if [ "$add_tests" = "1" ]; then
    cmd="$cmd --with-tests"
  fi
  printf '%s' "$cmd"
}

PRE_COMMIT_HOOK="$HOOK_DIR/pre-commit"
PRE_PUSH_HOOK="$HOOK_DIR/pre-push"

if [ "$PRE_COMMIT_MODE" = "off" ]; then
  [ "$VERBOSE" -eq 1 ] && echo "Skipping pre-commit installation (--pre-commit-mode off)."
else
  if [ "$PRE_COMMIT_MODE" = "gates-tests" ]; then
    cmd="$(make_gate_cmd 'bash "$REPO_ROOT/scripts/check_continuous_verification.sh"' 1)"
  else
    cmd="$(make_gate_cmd 'bash "$REPO_ROOT/scripts/check_continuous_verification.sh"' 0)"
  fi
  write_managed_hook "$PRE_COMMIT_HOOK" "$cmd"
fi

if [ "$PRE_PUSH_MODE" = "off" ]; then
  [ "$VERBOSE" -eq 1 ] && echo "Skipping pre-push installation (--pre-push-mode off)."
else
  if [ "$PRE_PUSH_MODE" = "gates" ]; then
    cmd="$(make_gate_cmd 'bash "$REPO_ROOT/scripts/check_continuous_verification.sh"' 1)"
  else
    cmd='bash "$REPO_ROOT/scripts/check_release_candidate.sh" --with-tests'
    if [ "$ALLOW_ARTIFACTS" -eq 1 ]; then
      cmd="$cmd --allow-artifacts"
    fi
  fi
  write_managed_hook "$PRE_PUSH_HOOK" "$cmd"
fi

echo "Local CI hook bootstrap: complete"
exit 0
