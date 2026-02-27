#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
if [ -f "$SCRIPT_DIR/src/backend.mojo" ]; then
  ROOT="$SCRIPT_DIR"
elif [ -f "$SCRIPT_DIR/../src/backend.mojo" ]; then
  ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
else
  ROOT="$SCRIPT_DIR"
fi
BUILD="$ROOT/build"
F64_REDUCE_FASTPATH_RAW="${MOJOR_GPU_F64_REDUCE_FASTPATH:-auto}"
COPY_LIBS_TO_SRC="${MOJOR_COPY_LIBS_TO_SRC:-0}"
UPDATE_BACKEND_BUNDLE="${MOJOR_UPDATE_BACKEND_BUNDLE:-1}"
OS="$(uname)"
ARCH="$(uname -m)"
BACKEND_LIBS=(libmojor_backend libmojor_backend_gamma libmojor_backend_rng)
BUNDLE_ROOT="$ROOT/inst/backend"
BACKEND_MAIN_SRC_TMP=""
BACKEND_MAIN_SRC="$ROOT/src/backend.mojo"
R_BIN=""

resolve_r_bin() {
  if [ -n "${R_HOME:-}" ] && [ -x "${R_HOME}/bin/R" ]; then
    echo "${R_HOME}/bin/R"
    return
  fi
  if command -v R >/dev/null 2>&1; then
    command -v R
    return
  fi
  echo ""
}

default_gpu_api() {
  case "$OS" in
    Darwin) echo "metal" ;;
    Linux)
      if command -v nvidia-smi >/dev/null 2>&1; then
        echo "cuda"
        return
      fi
      if command -v rocm-smi >/dev/null 2>&1 || [ -n "${ROCM_PATH:-}" ] || [ -n "${ROCM_HOME:-}" ] || [ -e /dev/kfd ]; then
        echo "amd"
        return
      fi
      if command -v lspci >/dev/null 2>&1; then
        local pci
        pci="$(lspci 2>/dev/null || true)"
        if printf '%s\n' "$pci" | grep -Eqi 'NVIDIA'; then
          echo "cuda"
          return
        fi
        if printf '%s\n' "$pci" | grep -Eqi 'AMD|Advanced Micro Devices|Radeon|ATI'; then
          echo "amd"
          return
        fi
      fi
      echo "cuda"
      ;;
    *) echo "metal" ;;
  esac
}

GPU_API="${MOJOR_GPU_API:-$(default_gpu_api)}"
MOJO_BUILD_TARGET="${MOJOR_MOJO_BUILD_TARGET:-}"
RESOLVED_MOJO_BUILD_TARGET="$MOJO_BUILD_TARGET"
case "$(printf '%s' "$MOJO_BUILD_TARGET" | tr '[:upper:]' '[:lower:]')" in
  no_gpu)
    # stdlib GPU kernel instantiation still needs a concrete known target at
    # compile time; treat NO_GPU as an explicit non-host-safe alias.
    RESOLVED_MOJO_BUILD_TARGET="sm_80"
    ;;
esac
MOJO_ACCEL_TARGET=""
if [ "$OS" = "Linux" ] && [ -n "$RESOLVED_MOJO_BUILD_TARGET" ]; then
  case "$RESOLVED_MOJO_BUILD_TARGET" in
    nvidia:*|amdgpu:*|metal:*)
      MOJO_ACCEL_TARGET="$RESOLVED_MOJO_BUILD_TARGET"
      ;;
    sm_*)
      MOJO_ACCEL_TARGET="nvidia:$RESOLVED_MOJO_BUILD_TARGET"
      ;;
    gfx*|mi*)
      MOJO_ACCEL_TARGET="amdgpu:$RESOLVED_MOJO_BUILD_TARGET"
      ;;
  esac
fi
MOJO_BUILD_TARGET_ARGS=()
if [ -n "$RESOLVED_MOJO_BUILD_TARGET" ]; then
  MOJO_BUILD_TARGET_ARGS=(-D "target=$RESOLVED_MOJO_BUILD_TARGET")
fi
if [ -n "$MOJO_ACCEL_TARGET" ]; then
  MOJO_BUILD_TARGET_ARGS+=(--target-accelerator "$MOJO_ACCEL_TARGET")
fi

if [ "$OS" = "Darwin" ]; then
  BACKEND_EXT="dylib"
else
  BACKEND_EXT="so"
fi

mkdir -p "$BUILD"

backend_bundle_platform_key() {
  local os="$1"
  local arch="$2"
  local arch_key="$arch"
  case "$arch" in
    aarch64) arch_key="arm64" ;;
    amd64) arch_key="x86_64" ;;
  esac
  case "$os" in
    Darwin) echo "darwin-$arch_key" ;;
    Linux) echo "linux-$arch_key" ;;
    *) echo "unknown-$arch_key" ;;
  esac
}

sha256_file() {
  local path="$1"
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$path" | awk '{print $1}'
    return
  fi
  shasum -a 256 "$path" | awk '{print $1}'
}

run_with_timeout() {
  local seconds="$1"
  shift
  if command -v timeout >/dev/null 2>&1; then
    timeout "$seconds" "$@"
    return
  fi
  if command -v gtimeout >/dev/null 2>&1; then
    gtimeout "$seconds" "$@"
    return
  fi
  "$@"
}

resolve_f64_reduce_fastpath() {
  case "$F64_REDUCE_FASTPATH_RAW" in
    auto|AUTO|"")
      echo "False"
      ;;
    1|true|TRUE|on|ON|yes|YES)
      echo "True"
      ;;
    0|false|FALSE|off|OFF|no|NO)
      echo "False"
      ;;
    *)
      echo "Unsupported MOJOR_GPU_F64_REDUCE_FASTPATH: $F64_REDUCE_FASTPATH_RAW (expected auto|1|0|true|false)" >&2
      exit 1
      ;;
  esac
}

prepare_backend_source() {
  local src="$ROOT/src/backend.mojo"
  BACKEND_MAIN_SRC="$src"
  local gpu_api_value="metal"
  local need_patch=0

  case "$GPU_API" in
    metal|cuda|amd) ;;
    *) echo "Unsupported MOJOR_GPU_API: $GPU_API (expected metal|cuda|amd)" >&2; exit 1 ;;
  esac

  if [ "$GPU_API" != "metal" ] || [ -n "${MOJOR_GPU_API:-}" ]; then
    gpu_api_value="$GPU_API"
    need_patch=1
  fi

  if [ "$F64_REDUCE_FASTPATH_BOOL" != "False" ]; then
    need_patch=1
  fi

  if [ "$need_patch" -eq 1 ]; then
    BACKEND_MAIN_SRC="$ROOT/src/backend_build_generated.mojo"
    sed \
      -e "s/comptime MOJOR_GPU_API: String = \"metal\"/comptime MOJOR_GPU_API: String = \"$gpu_api_value\"/" \
      -e "s/comptime MOJOR_GPU_F64_REDUCE_FASTPATH: Bool = False/comptime MOJOR_GPU_F64_REDUCE_FASTPATH: Bool = $F64_REDUCE_FASTPATH_BOOL/" \
      "$src" > "$BACKEND_MAIN_SRC"
    BACKEND_MAIN_SRC_TMP="$BACKEND_MAIN_SRC"
  fi
}

cleanup_backend_source_tmp() {
  if [ -n "$BACKEND_MAIN_SRC_TMP" ] && [ -f "$BACKEND_MAIN_SRC_TMP" ]; then
    rm -f "$BACKEND_MAIN_SRC_TMP"
  fi
}

F64_REDUCE_FASTPATH_BOOL="$(resolve_f64_reduce_fastpath)"
R_BIN="$(resolve_r_bin)"
trap cleanup_backend_source_tmp EXIT

has_main_backend() {
  local root="$1"
  [ -f "$root/libmojor_backend.$BACKEND_EXT" ] || [ -f "$root/libmojor_backend.dylib" ] || [ -f "$root/libmojor_backend.so" ] || [ -f "$root/libmojor_backend.dll" ]
}

has_backend_lib() {
  local root="$1"
  local lib="$2"
  [ -f "$root/$lib.$BACKEND_EXT" ] || [ -f "$root/$lib.dylib" ] || [ -f "$root/$lib.so" ] || [ -f "$root/$lib.dll" ]
}

has_required_backends() {
  local root="$1"
  has_backend_lib "$root" "libmojor_backend" &&
    has_backend_lib "$root" "libmojor_backend_rng" &&
    has_backend_lib "$root" "libmojor_backend_gamma"
}

copy_backend_libs_to_src() {
  [ "$COPY_LIBS_TO_SRC" = "1" ] || return 0

  local dst_dir="$ROOT/src"
  mkdir -p "$dst_dir"

  local copied=0
  for lib in "${BACKEND_LIBS[@]}"; do
    for ext in "$BACKEND_EXT" dylib so dll; do
      local src_path="$BUILD/$lib.$ext"
      if [ -f "$src_path" ]; then
        cp "$src_path" "$dst_dir/"
        copied=1
        break
      fi
    done
  done

  if [ "$copied" -eq 1 ]; then
    echo "Copied backend libraries to $dst_dir (MOJOR_COPY_LIBS_TO_SRC=1)."
  else
    echo "warning: MOJOR_COPY_LIBS_TO_SRC=1 requested, but no backend libraries were found in $BUILD." >&2
  fi
}

update_backend_bundle() {
  [ "$UPDATE_BACKEND_BUNDLE" = "1" ] || return 0

  local platform
  platform="$(backend_bundle_platform_key "$OS" "$ARCH")"
  if [[ "$platform" == unknown-* ]]; then
    echo "Skipping backend bundle update: unsupported platform '$OS/$ARCH'." >&2
    return 0
  fi

  local dst="$BUNDLE_ROOT/$platform"
  mkdir -p "$dst"
  for lib in "${BACKEND_LIBS[@]}"; do
    local src_path="$BUILD/$lib.$BACKEND_EXT"
    if [ ! -f "$src_path" ]; then
      echo "warning: skipping bundle update, missing backend artifact: $src_path" >&2
      return 0
    fi
    cp "$src_path" "$dst/"
  done

  local manifest="$BUNDLE_ROOT/MANIFEST.tsv"
  local tmp_manifest="$manifest.tmp"
  {
    echo -e "platform\tfile\tsha256"
    if [ -f "$manifest" ]; then
      awk -F'\t' -v p="$platform" 'NR==1{next} $1!=p {print $1 "\t" $2 "\t" $3}' "$manifest"
    fi
    for lib in "${BACKEND_LIBS[@]}"; do
      local filename="$lib.$BACKEND_EXT"
      local hash
      hash="$(sha256_file "$dst/$filename")"
      echo -e "$platform\t$filename\t$hash"
    done
  } > "$tmp_manifest"
  mv "$tmp_manifest" "$manifest"
  echo "Updated backend bundle: $dst"
  echo "Updated backend manifest: $manifest"
}

cleanup_bridge_object() {
  rm -f "$ROOT/src/bridge.o"
}

require_backend_lib() {
  local lib_path="$1"
  local label="$2"
  if [ ! -f "$lib_path" ]; then
    echo "ERROR: missing $label ($lib_path)." >&2
    echo "C backend fallbacks have been removed; build the Mojo backend successfully." >&2
    exit 1
  fi
}

# Build code generators if mojo is available
if command -v mojo >/dev/null 2>&1; then
  mojo "$ROOT/../tools/gen_reduce_exports.mojo" "$ROOT/src/backend.mojo" 2>/dev/null || true
  mojo "$ROOT/../tools/gen_gpu_sigmoid_exports.mojo" "$ROOT/src/backend.mojo" 2>/dev/null || true
  mojo "$ROOT/../tools/gen_gpu_buf_exports.mojo" "$ROOT/src/backend.mojo" 2>/dev/null || true
fi

if ! has_required_backends "$BUILD"; then
  echo "warning: required backend artifacts not found locally; attempting direct Mojo build." >&2
fi

if [ "$OS" = "Darwin" ]; then
  # Build strategy:
  # 1. Build main backend.mojo
  # 2. Build separate RNG/Gamma modules
  # 3. Require all backend artifacts (no C fallback)

  fix_bridge_install_names() {
    local bridge="$1"
    [ -f "$bridge" ] || return 0
    if ! command -v install_name_tool >/dev/null 2>&1; then
      return 0
    fi
    local abs_build
    abs_build="$(cd "$BUILD" && pwd)"
    for lib in "${BACKEND_LIBS[@]}"; do
      local dylib="$lib.$BACKEND_EXT"
      local new_ref="@loader_path/$dylib"
      for old_ref in \
        "build/$dylib" \
        "$BUILD/$dylib" \
        "$ROOT/build/$dylib" \
        "$abs_build/$dylib"; do
        install_name_tool -change "$old_ref" "$new_ref" "$bridge" 2>/dev/null || true
      done
    done
  }

  if command -v mojo >/dev/null 2>&1; then
    echo "Building Mojo backend modules..."
    echo "  - f64 reduce fastpath: $F64_REDUCE_FASTPATH_BOOL (MOJOR_GPU_F64_REDUCE_FASTPATH=${F64_REDUCE_FASTPATH_RAW})"

    # Build main backend
    prepare_backend_source

    run_with_timeout 300 mojo build "${MOJO_BUILD_TARGET_ARGS[@]}" --emit shared-lib -o "$BUILD/libmojor_backend.$BACKEND_EXT" \
      "$BACKEND_MAIN_SRC"

    # Build RNG modules separately (faster compilation)
    if [ -f "$ROOT/src/backend_rng.mojo" ]; then
      echo "  - Building RNG module..."
      mojo build "${MOJO_BUILD_TARGET_ARGS[@]}" --emit shared-lib -o "$BUILD/libmojor_backend_rng.$BACKEND_EXT" \
        "$ROOT/src/backend_rng.mojo"
    fi

    if [ -f "$ROOT/src/backend_gamma.mojo" ]; then
      echo "  - Building Gamma module..."
      mojo build "${MOJO_BUILD_TARGET_ARGS[@]}" --emit shared-lib -o "$BUILD/libmojor_backend_gamma.$BACKEND_EXT" \
        "$ROOT/src/backend_gamma.mojo"
    fi
  elif ! has_required_backends "$BUILD"; then
    echo "ERROR: mojo binary not found and required backend artifacts are missing." >&2
    echo "C backend fallbacks have been removed." >&2
    exit 1
  fi

  require_backend_lib "$BUILD/libmojor_backend.$BACKEND_EXT" "main backend library"
  require_backend_lib "$BUILD/libmojor_backend_rng.$BACKEND_EXT" "RNG backend library"
  require_backend_lib "$BUILD/libmojor_backend_gamma.$BACKEND_EXT" "Gamma backend library"

  # Build R bridge - link against all available backends
  if [ -n "$R_BIN" ]; then
    echo "Building R bridge..."
    R_LDFLAGS="-L\"$BUILD\" -lmojor_backend"
    # Link all required backend modules.
    for lib in rng gamma; do
      R_LDFLAGS="$R_LDFLAGS -lmojor_backend_$lib"
    done

    "$R_BIN" CMD SHLIB -o "$BUILD/mojor_bridge.so" \
      "$ROOT/src/bridge.c" \
      $R_LDFLAGS \
      -Wl,-rpath,@loader_path
    fix_bridge_install_names "$BUILD/mojor_bridge.so"
    cleanup_bridge_object
  else
    echo "R not found; built backend only" >&2
  fi
else
  # Linux build (similar structure)
  if command -v mojo >/dev/null 2>&1; then
    echo "Building Mojo backend modules..."
    echo "  - f64 reduce fastpath: $F64_REDUCE_FASTPATH_BOOL (MOJOR_GPU_F64_REDUCE_FASTPATH=${F64_REDUCE_FASTPATH_RAW})"

    prepare_backend_source

    run_with_timeout 300 mojo build "${MOJO_BUILD_TARGET_ARGS[@]}" --emit shared-lib -o "$BUILD/libmojor_backend.$BACKEND_EXT" \
      "$BACKEND_MAIN_SRC"

    # Build RNG modules separately
    for mod in rng gamma gibbs; do
      if [ -f "$ROOT/src/backend_$mod.mojo" ]; then
        echo "  - Building $mod module..."
        mojo build "${MOJO_BUILD_TARGET_ARGS[@]}" --emit shared-lib -o "$BUILD/libmojor_backend_$mod.$BACKEND_EXT" \
          "$ROOT/src/backend_$mod.mojo"
      fi
    done
  elif ! has_required_backends "$BUILD"; then
    echo "ERROR: mojo binary not found and required backend artifacts are missing." >&2
    echo "C backend fallbacks have been removed." >&2
    exit 1
  fi

  require_backend_lib "$BUILD/libmojor_backend.$BACKEND_EXT" "main backend library"
  require_backend_lib "$BUILD/libmojor_backend_rng.$BACKEND_EXT" "RNG backend library"
  require_backend_lib "$BUILD/libmojor_backend_gamma.$BACKEND_EXT" "Gamma backend library"

  if [ -n "$R_BIN" ]; then
    echo "Building R bridge..."
    R_LDFLAGS="-L\"$BUILD\" -lmojor_backend"
    for lib in rng gamma; do
      R_LDFLAGS="$R_LDFLAGS -lmojor_backend_$lib"
    done

    "$R_BIN" CMD SHLIB -o "$BUILD/mojor_bridge.so" \
      "$ROOT/src/bridge.c" \
      $R_LDFLAGS \
      -Wl,-rpath,"$BUILD"
    cleanup_bridge_object
  else
    echo "R not found; built backend only" >&2
  fi
fi

copy_backend_libs_to_src
update_backend_bundle
cleanup_bridge_object

echo ""
echo "Build complete: $BUILD"
echo "Libraries:"
ls -la "$BUILD"/*.dylib "$BUILD"/*.so 2>/dev/null | awk '{print "  " $9 " (" $5 " bytes)"}'
