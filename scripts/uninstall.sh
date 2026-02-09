#!/usr/bin/env bash

set -euo pipefail

DEFAULT_PREFIX="/usr/local"
APP_NAME="ignis"

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
FORCE=false
PREFIX="$DEFAULT_PREFIX"

info() { echo -e "${GREEN}[INFO]${NC} $1" >&2; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1" >&2; }
error() { echo -e "${RED}[ERROR]${NC} $1" >&2; }
step() { echo -e "${BLUE}==>${NC} $1" >&2; }

usage() {
  cat << EOF
Usage: $0 [OPTIONS]

Uninstall Ignis from your Linux system.

OPTIONS:
  --prefix PATH   Installation prefix (default: $DEFAULT_PREFIX)
  --force         Skip confirmation prompt
  --dry-run       Show what would be removed without changing files
  --help          Show this help message
EOF
  exit 1
}

while [[ $# -gt 0 ]]; do
  case $1 in
    --prefix)
      PREFIX="$2"
      shift 2
      ;;
    --force|-f)
      FORCE=true
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
    error "Prefix '$PREFIX' is not writable"
    echo "Try: sudo $0 --prefix $PREFIX"
    exit 1
  fi
}

rm_safe() {
  local path="$1"

  if [[ ! -e "$path" ]]; then
    return
  fi

  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[DRY-RUN] rm -f $path"
  else
    rm -f "$path"
    info "Removed: $path"
  fi
}

rm_rf_safe() {
  local path="$1"

  if [[ ! -e "$path" ]]; then
    return
  fi

  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[DRY-RUN] rm -rf $path"
  else
    rm -rf "$path"
    info "Removed: $path"
  fi
}

rmdir_safe() {
  local path="$1"

  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[DRY-RUN] rmdir $path (if empty)"
  else
    rmdir "$path" 2>/dev/null || true
  fi
}

main() {
  echo
  echo "  ======================================"
  echo "       Ignis Linux Uninstaller"
  echo "  ======================================"
  echo

  check_prefix_writable

  local installed=()
  [[ -f "$PREFIX/bin/$APP_NAME" ]] && installed+=("$PREFIX/bin/$APP_NAME")
  [[ -f "$PREFIX/lib/ignis/ignis-bin" ]] && installed+=("$PREFIX/lib/ignis/ignis-bin")
  [[ -d "$PREFIX/share/ignis/std" ]] && installed+=("$PREFIX/share/ignis/std")

  if [[ ${#installed[@]} -eq 0 ]]; then
    error "No Ignis installation found under '$PREFIX'"
    exit 1
  fi

  info "Found installation files:"
  for item in "${installed[@]}"; do
    echo "  - $item"
  done
  echo

  if [[ "$FORCE" == "false" ]] && [[ -t 0 ]]; then
    read -r -p "Proceed with uninstall? [y/N] " reply
    if [[ ! "$reply" =~ ^[Yy]$ ]]; then
      info "Uninstall cancelled"
      exit 0
    fi
  fi

  step "Removing files"
  rm_safe "$PREFIX/bin/$APP_NAME"
  rm_safe "$PREFIX/lib/ignis/ignis-bin"
  rm_rf_safe "$PREFIX/share/ignis/std"

  rmdir_safe "$PREFIX/lib/ignis"
  rmdir_safe "$PREFIX/share/ignis"
  rmdir_safe "$PREFIX/lib"
  rmdir_safe "$PREFIX/share"

  echo
  if [[ "$DRY_RUN" == "true" ]]; then
    info "Dry-run completed. No files were changed."
  else
    info "Ignis uninstalled successfully"
  fi
}

main "$@"
