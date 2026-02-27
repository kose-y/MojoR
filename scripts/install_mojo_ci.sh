#!/usr/bin/env bash
set -euo pipefail

REQUIRED_PREFIX="${MOJOR_REQUIRED_MOJO_PREFIX:-0.26}"
PINNED_VERSION="${MOJOR_REQUIRED_MOJO_VERSION:-0.26.1}"
CI_TAG="[install_mojo_ci]"

extract_version() {
  local raw="${1:-}"
  local v
  v="$(printf '%s\n' "$raw" | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+(\.[0-9]+)?' | head -n 1 || true)"
  if [ -z "$v" ]; then
    v="$(printf '%s\n' "$raw" | grep -Eo '[0-9]+\.[0-9]+' | head -n 1 || true)"
  fi
  printf '%s' "$v"
}

add_path_if_dir() {
  local dir="${1:-}"
  [ -n "$dir" ] || return 0
  [ -d "$dir" ] || return 0
  export PATH="$dir:$PATH"
  if [ -n "${GITHUB_PATH:-}" ]; then
    printf '%s\n' "$dir" >> "$GITHUB_PATH"
  fi
}

mojo_ready() {
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
    *)
      return 1
      ;;
  esac
}

refresh_modular_paths() {
  add_path_if_dir "$HOME/.modular/bin"
  add_path_if_dir "$HOME/.modular/pkg/packages.modular.com_modular/bin"
  add_path_if_dir "$HOME/.modular/pkg/packages.modular.com_mojo/bin"
  add_path_if_dir "$HOME/.modular/pkg/packages.modular.com_max/bin"

  if [ -d "$HOME/.modular" ]; then
    while IFS= read -r mod_bin; do
      add_path_if_dir "$(dirname "$mod_bin")"
    done < <(find "$HOME/.modular" -type f -name modular -perm -u+x 2>/dev/null || true)
    while IFS= read -r mojo_bin; do
      add_path_if_dir "$(dirname "$mojo_bin")"
    done < <(find "$HOME/.modular" -type f -name mojo -perm -u+x 2>/dev/null || true)
  fi
}

install_via_modular_bootstrap() {
  local attempt
  for attempt in 1 2 3; do
    echo "$CI_TAG modular bootstrap attempt $attempt/3"
    set +e
    curl -fsSL https://get.modular.com | sh -
    local curl_rc=$?
    set -e
    refresh_modular_paths
    if [ "$curl_rc" -ne 0 ]; then
      echo "$CI_TAG bootstrap script failed (attempt $attempt)"
      continue
    fi

    if command -v modular >/dev/null 2>&1; then
      set +e
      modular install mojo
      local mod_rc=$?
      set -e
      refresh_modular_paths
      if [ "$mod_rc" -eq 0 ] && mojo_ready; then
        return 0
      fi
      echo "$CI_TAG modular install mojo failed (attempt $attempt)"
    else
      echo "$CI_TAG modular command not found after bootstrap (attempt $attempt)"
    fi
  done
  return 1
}

install_via_uv_fallback() {
  local venv_dir="$HOME/.mojor-mojo-ci-venv"
  echo "$CI_TAG trying uv fallback for mojo==$PINNED_VERSION"

  if ! command -v uv >/dev/null 2>&1; then
    curl -LsSf https://astral.sh/uv/install.sh | sh
  fi
  add_path_if_dir "$HOME/.local/bin"
  if ! command -v uv >/dev/null 2>&1; then
    echo "$CI_TAG uv not available after install"
    return 1
  fi

  rm -rf "$venv_dir"
  uv venv "$venv_dir"
  uv pip install \
    --python "$venv_dir/bin/python" \
    "mojo==${PINNED_VERSION}" \
    --extra-index-url "https://modular.gateway.scarf.sh/simple/"
  add_path_if_dir "$venv_dir/bin"
  mojo_ready
}

refresh_modular_paths
if mojo_ready; then
  echo "$CI_TAG already ready: $(mojo --version | head -n 1)"
  exit 0
fi

if install_via_modular_bootstrap || install_via_uv_fallback; then
  echo "$CI_TAG installed: $(mojo --version | head -n 1)"
  exit 0
fi

echo "$CI_TAG failed to install a compatible Mojo (${REQUIRED_PREFIX}.x required)" >&2
exit 1
