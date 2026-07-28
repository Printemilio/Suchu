#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="${SCRIPT_DIR}"
PREFIX="${PREFIX:-/usr/local}"
PROFILE="${DUNE_PROFILE:-release}"
OPAM_SWITCH="${OPAM_SWITCH:-}"
DUNE_TARGET="src/suchu_cli.exe"
TARGET_PATH="_build/default/src/suchu_cli.exe"

log() {
  printf '[suchu] %s\n' "$*"
}

err() {
  printf '[suchu] ERROR: %s\n' "$*" >&2
}

usage() {
  cat <<'EOF'
Usage: ./install.sh [--prefix PATH] [--profile PROFILE] [--switch NAME]

Options:
  --prefix PATH    Install suchu into PATH/bin (default: $PREFIX or /usr/local).
  --profile NAME   Dune build profile to use (default: release).
  --switch NAME    Use `opam exec --switch NAME -- dune ...` for the build.
  -h, --help       Show this help text.

Environment:
  PREFIX           Same as --prefix.
  DUNE_PROFILE     Same as --profile.
  OPAM_SWITCH      Same as --switch.
EOF
}

parse_args() {
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --prefix)
        shift || { err "Missing value for --prefix"; exit 1; }
        PREFIX="$1"
        ;;
      --prefix=*)
        PREFIX="${1#*=}"
        ;;
      --profile)
        shift || { err "Missing value for --profile"; exit 1; }
        PROFILE="$1"
        ;;
      --profile=*)
        PROFILE="${1#*=}"
        ;;
      --switch)
        shift || { err "Missing value for --switch"; exit 1; }
        OPAM_SWITCH="$1"
        ;;
      --switch=*)
        OPAM_SWITCH="${1#*=}"
        ;;
      -h|--help)
        usage
        exit 0
        ;;
      *)
        err "Unknown option: $1"
        usage
        exit 1
        ;;
    esac
    shift
  done
}

resolve_bin_dir() {
  if [[ "${PREFIX}" == "/" ]]; then
    BIN_DIR="/bin"
  else
    BIN_DIR="${PREFIX%/}/bin"
  fi
}

resolve_dune() {
  if [[ -n "${OPAM_SWITCH}" ]]; then
    if ! command -v opam >/dev/null 2>&1; then
      err "Option --switch requires opam to be installed."
      exit 1
    fi
    DUNE_CMD=(opam exec --switch "${OPAM_SWITCH}" -- dune)
    return
  fi

  if command -v dune >/dev/null 2>&1; then
    DUNE_CMD=(dune)
    return
  fi

  if command -v opam >/dev/null 2>&1; then
    DUNE_CMD=(opam exec -- dune)
    return
  fi

  err "dune is not available. Install dune or run install_deps.sh first."
  exit 1
}

main() {
  parse_args "$@"
  resolve_bin_dir
  resolve_dune

  log "Using prefix: ${PREFIX}"
  log "Using build profile: ${PROFILE}"
  if [[ -n "${OPAM_SWITCH}" ]]; then
    log "Using opam switch: ${OPAM_SWITCH}"
  fi

  log "Building suchu CLI..."
  (
    cd "${REPO_ROOT}"
    "${DUNE_CMD[@]}" build --profile "${PROFILE}" "${DUNE_TARGET}"
  )

  ARTIFACT="${REPO_ROOT}/${TARGET_PATH}"
  if [[ ! -f "${ARTIFACT}" ]]; then
    err "Build artifact not found at ${ARTIFACT}"
    exit 1
  fi

  log "Installing binary to ${BIN_DIR}..."
  install -d "${BIN_DIR}"
  install "${ARTIFACT}" "${BIN_DIR}/suchu"

  log "Installation complete: ${BIN_DIR}/suchu"
  log "If ${BIN_DIR} is not on your PATH, add it before running 'suchu'."
}

main "$@"
