#!/usr/bin/env bash

set -euo pipefail

log() {
  printf '[suchu deps] %s\n' "$*"
}

err() {
  printf '[suchu deps] ERROR: %s\n' "$*" >&2
}

require_root_tools() {
  if [[ "${EUID:-$(id -u)}" -ne 0 ]]; then
    if command -v sudo >/dev/null 2>&1; then
      SUDO="sudo"
    else
      err "Privileges élevées requises pour installer les paquets système (sudo absent)."
      exit 1
    fi
  else
    SUDO=""
  fi
}

install_with_apt() {
  require_root_tools
  local packages=(
    build-essential
    git
    curl
    m4
    pkg-config
    opam
    bubblewrap
    libsdl2-dev
    libsdl2-image-dev
    libsdl2-ttf-dev
  )

  log "Installation des dépendances système via apt-get..."
  ${SUDO} apt-get update
  ${SUDO} apt-get install -y "${packages[@]}"
}

install_with_pacman() {
  require_root_tools
  local packages=(
    base-devel
    git
    curl
    m4
    pkgconf
    opam
    bubblewrap
    sdl2
    sdl2_image
    sdl2_ttf
  )

  log "Installation des dépendances système via pacman..."
  ${SUDO} pacman -Syu --needed --noconfirm "${packages[@]}"
}

install_with_brew() {
  log "Installation des dépendances système via Homebrew..."
  brew update
  brew install opam ocaml dune pkg-config sdl2 sdl2_image sdl2_ttf
}

ensure_opam_ready() {
  if ! command -v opam >/dev/null 2>&1; then
    err "opam est introuvable même après l'installation système."
    exit 1
  fi

  if ! opam var root >/dev/null 2>&1; then
    log "Initialisation de l'environnement opam (non interactif)..."
    local init_cmd=(opam init -y --bare)
    if ! command -v bubblewrap >/dev/null 2>&1; then
      init_cmd+=(--disable-sandboxing)
    fi
    "${init_cmd[@]}"
  fi
}

setup_ocaml_toolchain() {
  local switch_name="suchu-5.1.1"
  local compiler="ocaml-base-compiler.5.1.1"

  log "Configuration du switch opam « ${switch_name} »..."
  if ! opam switch list --short | grep -Fxq "${switch_name}"; then
    opam switch create "${switch_name}" "${compiler}" -y
  fi

  eval "$(opam env --switch "${switch_name}" --set-switch)"
  opam update -y
  opam install -y dune bogue

  log "La toolchain OCaml/Dune/Bogue est prête dans le switch « ${switch_name} »."
  log "Pour l'activer dans votre shell courant :"
  printf '  eval "$(opam env --switch %s)"\n' "${switch_name}"
}

main() {
  log "Détection du gestionnaire de paquets..."
  if command -v apt-get >/dev/null 2>&1; then
    install_with_apt
  elif command -v pacman >/dev/null 2>&1; then
    install_with_pacman
  elif command -v brew >/dev/null 2>&1; then
    install_with_brew
  else
    err "Gestionnaire de paquets non supporté. Installez OCaml, opam, dune et les bibliothèques SDL2 manuellement."
    exit 1
  fi

  ensure_opam_ready
  setup_ocaml_toolchain

  log "Installation des dépendances terminée."
}

main "$@"
