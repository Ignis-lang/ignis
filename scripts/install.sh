#!/usr/bin/env bash

set -euo pipefail

REPO_URL="https://github.com/Ignis-lang/ignis"
DEFAULT_PREFIX="/usr/local"

SCRIPT_PATH="${BASH_SOURCE[0]:-}"
if [[ -n "$SCRIPT_PATH" ]] && [[ -f "$SCRIPT_PATH" ]] && [[ "$SCRIPT_PATH" != /dev/fd/* ]] && [[ "$SCRIPT_PATH" != /proc/self/fd/* ]]; then
  SCRIPT_DIR="$(cd "$(dirname "$SCRIPT_PATH")" && pwd)"
  PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
  REMOTE_MODE=false
else
  SCRIPT_DIR=""
  PROJECT_ROOT=""
  REMOTE_MODE=true
fi

if [[ -t 1 ]]; then
  RED='\033[0;31m'
  GREEN='\033[0;32m'
  YELLOW='\033[1;33m'
  BLUE='\033[0;34m'
  NC='\033[0m'
else
  RED=''
  GREEN=''
  YELLOW=''
  BLUE=''
  NC=''
fi

DRY_RUN=false
PREFIX="$DEFAULT_PREFIX"
VERSION="latest"
BUILD_FROM_SOURCE=false

info() { echo -e "${GREEN}[INFO]${NC} $1" >&2; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1" >&2; }
error() { echo -e "${RED}[ERROR]${NC} $1" >&2; }
step() { echo -e "${BLUE}==>${NC} $1" >&2; }

usage() {
  cat << EOF
Usage: $0 [OPTIONS]

Install Ignis on Linux.

OPTIONS:
  --prefix PATH       Installation prefix (default: $DEFAULT_PREFIX)
  --version VERSION   Install a specific version tag (example: v0.3.0)
  --build             Build from source instead of downloading release artifact
  --dry-run           Show what would be done without changing files
  --help              Show this help message

EXAMPLES:
  # Install latest release to /usr/local
  curl -fsSL $REPO_URL/raw/main/scripts/install.sh | sudo bash

  # Install latest release to ~/.local
  curl -fsSL $REPO_URL/raw/main/scripts/install.sh | bash -s -- --prefix ~/.local

  # Build from source
  curl -fsSL $REPO_URL/raw/main/scripts/install.sh | bash -s -- --build
EOF
  exit 1
}

while [[ $# -gt 0 ]]; do
  case $1 in
    --prefix)
      PREFIX="$2"
      shift 2
      ;;
    --version)
      VERSION="$2"
      shift 2
      ;;
    --build)
      BUILD_FROM_SOURCE=true
      shift
      ;;
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    --help|-h)
      usage
      ;;
    *)
      error "Unknown option: $1"
      usage
      ;;
  esac
done

normalize_version() {
  local input="$1"
  if [[ "$input" == "latest" ]]; then
    echo "latest"
  elif [[ "$input" == v* ]]; then
    echo "$input"
  else
    echo "v$input"
  fi
}

detect_arch() {
  local arch
  arch="$(uname -m)"

  case "$arch" in
    x86_64|amd64)
      echo "linux-amd64"
      ;;
    *)
      error "Unsupported architecture: $arch"
      error "This installer currently supports Linux amd64 artifacts only."
      exit 1
      ;;
  esac
}

detect_os() {
  local os
  os="$(uname -s)"

  case "$os" in
    Linux)
      return 0
      ;;
    *)
      error "Unsupported operating system: $os"
      exit 1
      ;;
  esac
}

download() {
  local url="$1"
  local output="$2"

  if command -v curl >/dev/null 2>&1; then
    curl -fsSL "$url" -o "$output"
  elif command -v wget >/dev/null 2>&1; then
    wget -q "$url" -O "$output"
  else
    error "Neither curl nor wget is installed"
    exit 1
  fi
}

check_requirements() {
  local missing=()

  if ! command -v gcc >/dev/null 2>&1; then
    missing+=("gcc")
  fi
  if ! command -v ar >/dev/null 2>&1; then
    missing+=("ar")
  fi
  if ! command -v make >/dev/null 2>&1; then
    missing+=("make")
  fi

  if [[ "$BUILD_FROM_SOURCE" == "true" ]]; then
    if ! command -v cargo >/dev/null 2>&1; then
      missing+=("cargo")
    fi
    if ! command -v git >/dev/null 2>&1; then
      missing+=("git")
    fi
  else
    if ! command -v tar >/dev/null 2>&1; then
      missing+=("tar")
    fi
    if ! command -v sha256sum >/dev/null 2>&1; then
      missing+=("sha256sum")
    fi
    if ! command -v curl >/dev/null 2>&1 && ! command -v wget >/dev/null 2>&1; then
      missing+=("curl-or-wget")
    fi
  fi

  if [[ ${#missing[@]} -gt 0 ]]; then
    error "Missing required tools: ${missing[*]}"
    exit 1
  fi
}

check_prefix_writable() {
  if [[ "$DRY_RUN" == "true" ]]; then
    return 0
  fi

  local test_dir="$PREFIX/.ignis_write_test_$$"

  if mkdir -p "$test_dir" 2>/dev/null; then
    rm -rf "$test_dir" 2>/dev/null || true
    return 0
  fi

  rm -rf "$test_dir" 2>/dev/null || true

  if [[ $EUID -ne 0 ]]; then
    error "Installation prefix '$PREFIX' is not writable"
    echo "Try: sudo $0 --prefix $PREFIX"
    echo "Or : $0 --prefix ~/.local"
    exit 1
  fi
}

mkdir_safe() {
  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[DRY-RUN] mkdir -p $1"
  else
    mkdir -p "$1"
  fi
}

cp_safe() {
  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[DRY-RUN] cp $1 $2"
  else
    cp "$1" "$2"
  fi
}

get_latest_version() {
  local api_url="https://api.github.com/repos/Ignis-lang/ignis/releases/latest"
  local tag=""

  if command -v curl >/dev/null 2>&1; then
    tag="$(curl -fsSL "$api_url" | grep '"tag_name"' | sed -E 's/.*"([^"]+)".*/\1/')"
  elif command -v wget >/dev/null 2>&1; then
    tag="$(wget -qO- "$api_url" | grep '"tag_name"' | sed -E 's/.*"([^"]+)".*/\1/')"
  fi

  if [[ -z "$tag" ]]; then
    error "Failed to fetch latest release version"
    exit 1
  fi

  echo "$tag"
}

download_release() {
  local arch="$1"
  local version_tag="$2"
  local tmp_dir="$3"

  local tar_name="ignis-${arch}.tar.gz"
  local checksum_name="${tar_name}.sha256"
  local release_base="$REPO_URL/releases/download/$version_tag"

  local tar_url="$release_base/$tar_name"
  local checksum_url="$release_base/$checksum_name"

  local tar_path="$tmp_dir/$tar_name"
  local checksum_path="$tmp_dir/$checksum_name"

  step "Downloading Ignis $version_tag ($arch)..."
  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[DRY-RUN] download $tar_url"
    echo "[DRY-RUN] download $checksum_url"
    return 0
  fi

  download "$tar_url" "$tar_path"
  download "$checksum_url" "$checksum_path"

  step "Verifying checksum..."
  (
    cd "$tmp_dir"
    sha256sum -c "$checksum_name" >/dev/null
  )
  info "Checksum OK"

  mkdir -p "$tmp_dir/pkg"
  tar -xzf "$tar_path" -C "$tmp_dir/pkg"
}

build_from_source() {
  local tmp_dir="$1"
  local version_tag="$2"
  local repo_dir="$tmp_dir/ignis"

  if [[ -n "$PROJECT_ROOT" ]] && [[ -f "$PROJECT_ROOT/Cargo.toml" ]]; then
    repo_dir="$PROJECT_ROOT"
    info "Using local repository at $repo_dir"
  else
    step "Cloning repository..."
    if [[ "$DRY_RUN" == "true" ]]; then
      echo "[DRY-RUN] git clone $REPO_URL.git $repo_dir"
      return 0
    fi

    git clone --depth 1 "$REPO_URL.git" "$repo_dir"

    if [[ "$version_tag" != "latest" ]]; then
      (
        cd "$repo_dir"
        git fetch --depth 1 origin "tag" "$version_tag"
        git checkout "$version_tag"
      )
    fi
  fi

  step "Building Ignis from source..."
  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[DRY-RUN] cargo build --release -p ignis --locked"
    return 0
  fi

  (
    cd "$repo_dir"
    cargo build --release -p ignis --locked
  )

  mkdir -p "$tmp_dir/pkg"
  cp "$repo_dir/target/release/ignis" "$tmp_dir/pkg/ignis"
  cp -r "$repo_dir/std" "$tmp_dir/pkg/std"
}

install_files() {
  local source_dir="$1"

  if [[ "$DRY_RUN" == "true" ]]; then
    step "Installing to $PREFIX"
    echo "[DRY-RUN] cp $source_dir/ignis $PREFIX/lib/ignis/ignis-bin"
    echo "[DRY-RUN] cp -r $source_dir/std $PREFIX/share/ignis/std"
    echo "[DRY-RUN] write wrapper to $PREFIX/bin/ignis"
    return 0
  fi

  if [[ ! -f "$source_dir/ignis" ]]; then
    error "Binary not found: $source_dir/ignis"
    exit 1
  fi

  if [[ ! -d "$source_dir/std" ]]; then
    error "Standard library not found: $source_dir/std"
    exit 1
  fi

  step "Installing to $PREFIX"

  mkdir_safe "$PREFIX/bin"
  mkdir_safe "$PREFIX/lib/ignis"
  mkdir_safe "$PREFIX/share/ignis"

  cp_safe "$source_dir/ignis" "$PREFIX/lib/ignis/ignis-bin"

  chmod 755 "$PREFIX/lib/ignis/ignis-bin"
  rm -rf "$PREFIX/share/ignis/std"
  cp -r "$source_dir/std" "$PREFIX/share/ignis/std"

  cat > "$PREFIX/bin/ignis" << EOF
#!/usr/bin/env bash
set -euo pipefail

DEFAULT_STD_PATH="$PREFIX/share/ignis/std"
export IGNIS_STD_PATH="\${IGNIS_STD_PATH:-\$DEFAULT_STD_PATH}"
export IGNIS_HOME="\${IGNIS_HOME:-$PREFIX/share/ignis}"

exec "$PREFIX/lib/ignis/ignis-bin" "\$@"
EOF
  chmod 755 "$PREFIX/bin/ignis"
}

check_existing_installation() {
  if [[ -x "$PREFIX/bin/ignis" ]] && [[ -t 0 ]]; then
    warn "Ignis already exists at $PREFIX/bin/ignis"
    read -r -p "Overwrite existing installation? [y/N] " reply
    if [[ ! "$reply" =~ ^[Yy]$ ]]; then
      info "Installation cancelled"
      exit 0
    fi
  fi
}

print_success() {
  echo
  info "Ignis installed successfully"
  echo

  local bin_dir="$PREFIX/bin"
  if [[ ":$PATH:" != *":$bin_dir:"* ]]; then
    warn "'$bin_dir' is not currently in PATH"
    echo "Add it with:"
    echo "  export PATH=\"$bin_dir:\$PATH\""
    echo
  fi

  echo "Run: ignis --help"
  echo
  echo "To uninstall:"
  if [[ "$REMOTE_MODE" == "true" ]]; then
    echo "  curl -fsSL $REPO_URL/raw/main/scripts/uninstall.sh | bash -s -- --prefix $PREFIX"
  else
    echo "  $SCRIPT_DIR/uninstall.sh --prefix $PREFIX"
  fi
}

main() {
  echo
  echo "  ======================================"
  echo "        Ignis Linux Installer"
  echo "  ======================================"
  echo

  detect_os
  local arch
  arch="$(detect_arch)"
  info "Detected artifact: $arch"

  check_requirements
  check_prefix_writable

  VERSION="$(normalize_version "$VERSION")"
  if [[ "$VERSION" == "latest" ]] && [[ "$BUILD_FROM_SOURCE" == "false" ]]; then
    step "Resolving latest release..."
    VERSION="$(get_latest_version)"
  fi
  info "Version: $VERSION"

  check_existing_installation

  local tmp_dir
  tmp_dir="$(mktemp -d)"
  trap 'if [[ -n "${tmp_dir:-}" ]]; then rm -rf "$tmp_dir"; fi' EXIT

  if [[ "$BUILD_FROM_SOURCE" == "true" ]]; then
    build_from_source "$tmp_dir" "$VERSION"
    install_files "$tmp_dir/pkg"
  elif [[ "$REMOTE_MODE" == "false" ]] && [[ -f "$PROJECT_ROOT/ignis" ]] && [[ -d "$PROJECT_ROOT/std" ]]; then
    info "Using local extracted release package"
    install_files "$PROJECT_ROOT"
  elif [[ "$REMOTE_MODE" == "false" ]] && [[ -f "$PROJECT_ROOT/target/release/ignis" ]] && [[ -d "$PROJECT_ROOT/std" ]]; then
    info "Using local repository build output"
    mkdir -p "$tmp_dir/pkg"
    cp "$PROJECT_ROOT/target/release/ignis" "$tmp_dir/pkg/ignis"
    cp -r "$PROJECT_ROOT/std" "$tmp_dir/pkg/std"
    install_files "$tmp_dir/pkg"
  elif [[ "$REMOTE_MODE" == "false" ]] && [[ -f "$PROJECT_ROOT/Cargo.toml" ]]; then
    info "No local release artifact found, building from source"
    build_from_source "$tmp_dir" "$VERSION"
    install_files "$tmp_dir/pkg"
  else
    download_release "$arch" "$VERSION" "$tmp_dir"
    install_files "$tmp_dir/pkg"
  fi

  if [[ "$DRY_RUN" == "true" ]]; then
    echo
    info "Dry-run completed. No files were changed."
  else
    print_success
  fi
}

main "$@"
