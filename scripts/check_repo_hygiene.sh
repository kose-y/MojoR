#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: check_repo_hygiene.sh [--verbose] [--allow-artifacts] [--help]

Checks repository hygiene for:
  1) backup files (*.bak*) in active source/test trees
  2) compiled artifacts (*.o, *.so, *.dylib, *.dll) in source dirs

Exit codes:
  0 clean
  1 hygiene violation
  2 usage error
USAGE
}

VERBOSE=0
ALLOW_ARTIFACTS=0

while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
    --verbose) VERBOSE=1; shift ;;
    --allow-artifacts) ALLOW_ARTIFACTS=1; shift ;;
    --help|-h) usage; exit 0 ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 2
      ;;
  esac
done

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

BACKUP_DIRS=(
  "$REPO_ROOT/packages/mojor/R"
  "$REPO_ROOT/packages/mojor/tests/testthat"
)

ARTIFACT_DIRS=(
  "$REPO_ROOT/packages/mojor/src"
)

ARTIFACT_PATTERNS=(
  "*.o"
  "*.so"
  "*.dylib"
  "*.dll"
)

IGNORED_ARTIFACT_BASENAMES=(
  "mojor.so"
  "mojor.dll"
  "libmojor_backend.so"
  "libmojor_backend_rng.so"
  "libmojor_backend_gamma.so"
  "libmojor_backend.dylib"
  "libmojor_backend_rng.dylib"
  "libmojor_backend_gamma.dylib"
  "libmojor_backend.dll"
  "libmojor_backend_rng.dll"
  "libmojor_backend_gamma.dll"
)

artifact_is_ignored() {
  local path="$1"
  local base
  base="$(basename "$path")"
  local keep
  for keep in "${IGNORED_ARTIFACT_BASENAMES[@]}"; do
    if [[ "$base" == "$keep" ]]; then
      return 0
    fi
  done
  return 1
}

if [ "$VERBOSE" -eq 1 ]; then
  echo "Repository root: $REPO_ROOT"
  echo "Checking backup directories:"
  printf '  - %s\n' "${BACKUP_DIRS[@]}"
  echo "Checking artifact directories:"
  printf '  - %s\n' "${ARTIFACT_DIRS[@]}"
fi

backup_hits=()
for dir in "${BACKUP_DIRS[@]}"; do
  [ -d "$dir" ] || continue
  while IFS= read -r -d '' f; do
    backup_hits+=("$f")
  done < <(find "$dir" -type f -name '*.bak*' -print0)
done

artifact_hits=()
if [ "$ALLOW_ARTIFACTS" -eq 0 ]; then
  for dir in "${ARTIFACT_DIRS[@]}"; do
    [ -d "$dir" ] || continue
    for pat in "${ARTIFACT_PATTERNS[@]}"; do
      while IFS= read -r -d '' f; do
        artifact_hits+=("$f")
      done < <(find "$dir" -maxdepth 1 -type f -name "$pat" -print0)
    done
  done
fi

violation=0

if [ "${#backup_hits[@]}" -gt 0 ]; then
  violation=1
  echo "Hygiene violation: backup files found"
  printf '  - %s\n' "${backup_hits[@]}"
fi

if [ "$ALLOW_ARTIFACTS" -eq 1 ]; then
  [ "$VERBOSE" -eq 1 ] && echo "Skipping compiled artifact checks (--allow-artifacts)."
elif [ "${#artifact_hits[@]}" -gt 0 ]; then
  filtered_hits=()
  for hit in "${artifact_hits[@]}"; do
    if artifact_is_ignored "$hit"; then
      continue
    fi
    filtered_hits+=("$hit")
  done
  if [ "${#filtered_hits[@]}" -eq 0 ]; then
    artifact_hits=()
  else
    artifact_hits=("${filtered_hits[@]}")
  fi
fi

if [ "$ALLOW_ARTIFACTS" -eq 1 ]; then
  :
elif [ "${#artifact_hits[@]}" -gt 0 ]; then
  violation=1
  echo "Hygiene violation: compiled artifacts found in source directories"
  printf '  - %s\n' "${artifact_hits[@]}"
fi

legacy_token="$(printf 'prototype%s' '/')"
legacy_hits="$(git grep -n "$legacy_token" || true)"
if [ -n "$legacy_hits" ]; then
  violation=1
  echo "Hygiene violation: legacy mirror path tokens found in tracked files"
  printf '%s\n' "$legacy_hits"
fi

if [ "$violation" -ne 0 ]; then
  exit 1
fi

echo "Repository hygiene check: clean"
exit 0
