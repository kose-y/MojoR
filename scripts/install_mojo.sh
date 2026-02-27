#!/usr/bin/env bash
set -euo pipefail

REQUIRED_PREFIX="${MOJOR_REQUIRED_MOJO_PREFIX:-0.26}"
PINNED_VERSION="${MOJOR_REQUIRED_MOJO_VERSION:-0.26.1}"
MODE="${1:-}"

extract_version() {
  local raw="${1:-}"
  local v
  v="$(printf '%s\n' "$raw" | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+(\.[0-9]+)?' | head -n 1 || true)"
  if [ -z "$v" ]; then
    v="$(printf '%s\n' "$raw" | grep -Eo '[0-9]+\.[0-9]+' | head -n 1 || true)"
  fi
  printf '%s' "$v"
}

has_required_mojo() {
  if ! command -v mojo >/dev/null 2>&1; then
    return 1
  fi
  local raw ver
  raw="$(mojo --version 2>/dev/null || true)"
  ver="$(extract_version "$raw")"
  case "$ver" in
    "$REQUIRED_PREFIX"|"$REQUIRED_PREFIX".*)
      return 0
      ;;
  esac
  return 1
}

print_help() {
  cat <<EOF
Usage: bash scripts/install_mojo.sh [--check]

Purpose:
  Check Mojo availability/version for MojoR and print install commands when missing.

Environment overrides:
  MOJOR_REQUIRED_MOJO_PREFIX   default: $REQUIRED_PREFIX
  MOJOR_REQUIRED_MOJO_VERSION  default: $PINNED_VERSION
EOF
}

print_install_instructions() {
  cat <<EOF
MojoR requires Mojo ${REQUIRED_PREFIX}.x.

Recommended install path (pixi + Modular channel):
  curl -fsSL https://pixi.sh/install.sh | sh
  pixi init mojor-env -c https://conda.modular.com/max/ -c conda-forge
  cd mojor-env
  pixi add "mojo==${PINNED_VERSION}"
  pixi shell
  mojo --version

Alternative (uv):
  curl -LsSf https://astral.sh/uv/install.sh | sh
  uv venv && source .venv/bin/activate
  uv pip install "mojo==${PINNED_VERSION}" --extra-index-url https://modular.gateway.scarf.sh/simple/
  mojo --version

After installation, continue with MojoR:
  bash packages/mojor/build.sh
  R CMD build packages/mojor
  R CMD INSTALL mojor_*.tar.gz
EOF
}

case "$MODE" in
  -h|--help)
    print_help
    exit 0
    ;;
  --check)
    if has_required_mojo; then
      echo "Mojo check: OK ($(mojo --version | head -n 1))"
      exit 0
    fi
    echo "Mojo check: missing or unsupported version (need ${REQUIRED_PREFIX}.x)" >&2
    exit 1
    ;;
  "")
    if has_required_mojo; then
      echo "Mojo already satisfies requirement: $(mojo --version | head -n 1)"
      exit 0
    fi
    print_install_instructions
    exit 1
    ;;
  *)
    echo "Unknown argument: $MODE" >&2
    print_help
    exit 2
    ;;
esac
