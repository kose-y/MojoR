#!/usr/bin/env bash
set -euo pipefail

DRY_RUN=0
INCLUDE_BACKUPS=0

usage() {
  cat <<'USAGE'
Usage: clean_build_artifacts.sh [--dry-run] [--include-backups] [--help]

Removes build artifacts from source/build trees.
With --include-backups, also removes *.bak* files in active source/test trees.
USAGE
}

while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
    --dry-run) DRY_RUN=1; shift ;;
    --include-backups) INCLUDE_BACKUPS=1; shift ;;
    --help|-h) usage; exit 0 ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 1
      ;;
  esac
done

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

TARGET_ROOTS=(
  "$REPO_ROOT"
  "$REPO_ROOT/packages/mojor"
)

TARGET_PATTERNS=(
  "*.o"
  "*.so"
  "*.dylib"
  "*.dll"
  "*.a"
  "*.lo"
  "*.la"
)

BACKUP_DIRS=(
  "$REPO_ROOT/packages/mojor/R"
  "$REPO_ROOT/packages/mojor/tests/testthat"
)

remove_file_if_exists() {
  local p="$1"
  if [ -e "$p" ]; then
    if [ "$DRY_RUN" -eq 1 ]; then
      echo "[dry-run] would remove: $p"
    else
      rm -f "$p"
      echo "removed: $p"
    fi
  fi
}

remove_dir_if_exists() {
  local d="$1"
  if [ -d "$d" ]; then
    if [ "$DRY_RUN" -eq 1 ]; then
      echo "[dry-run] would remove: $d"
    else
      rm -rf "$d"
      echo "removed: $d"
    fi
  fi
}

for root in "${TARGET_ROOTS[@]}"; do
  if [ -d "$root/src" ]; then
    for patt in "${TARGET_PATTERNS[@]}"; do
      while IFS= read -r -d '' f; do
        if [ "$DRY_RUN" -eq 1 ]; then
          echo "[dry-run] would remove: $f"
        else
          rm -f "$f"
          echo "removed: $f"
        fi
      done < <(find "$root/src" -maxdepth 1 -type f -name "$patt" -print0)
    done
  fi

  remove_dir_if_exists "$root/build"
  remove_file_if_exists "$root/testthat-problems.rds"
  remove_file_if_exists "$root/packages/mojor/src/symbols.rds"

  while IFS= read -r -d '' f; do
    if [ "$DRY_RUN" -eq 1 ]; then
      echo "[dry-run] would remove: $f"
    else
      rm -f "$f"
      echo "removed: $f"
    fi
  done < <(find "$root" -maxdepth 2 \( -name "mojor_*.tar.gz" -o -name "mojor_*.tgz" \) -print0)

done

if [ "$INCLUDE_BACKUPS" -eq 1 ]; then
  for dir in "${BACKUP_DIRS[@]}"; do
    [ -d "$dir" ] || continue
    while IFS= read -r -d '' f; do
      if [ "$DRY_RUN" -eq 1 ]; then
        echo "[dry-run] would remove: $f"
      else
        rm -f "$f"
        echo "removed: $f"
      fi
    done < <(find "$dir" -type f -name '*.bak*' -print0)
  done
fi

while IFS= read -r -d '' f; do
  if [ "$DRY_RUN" -eq 1 ]; then
    echo "[dry-run] would remove: $f"
  else
    rm -f "$f"
    echo "removed: $f"
  fi

done < <(find "$REPO_ROOT" -maxdepth 2 -type f \( -name "tmp_*" -o -name "*testresult*" -o -name "*testrun*" -o -name "files*.zip" \) -print0)
