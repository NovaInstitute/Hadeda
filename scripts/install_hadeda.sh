#!/usr/bin/env bash
set -euo pipefail

log() {
  printf '==> %s\n' "$*"
}

die() {
  printf '!! %s\n' "$*" >&2
  exit 1
}

need_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    die "Missing dependency: $1 (please install it and retry)"
  fi
}

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd "$script_dir/.." && pwd)"
cd "$project_root"

log "Running from $project_root"

need_cmd "curl"
need_cmd "unzip"
need_cmd "tar"
need_cmd "R"

ensure_pkg_config() {
  if command -v pkg-config >/dev/null 2>&1; then
    return
  fi

  if command -v brew >/dev/null 2>&1; then
    log "Installing pkg-config via Homebrew"
    brew install pkg-config
  elif command -v apt-get >/dev/null 2>&1; then
    log "Installing pkg-config via apt-get"
    sudo apt-get update
    sudo apt-get install -y pkg-config
  else
    die "pkg-config not found and automatic installation is unsupported on this platform; install it manually and retry."
  fi

  command -v pkg-config >/dev/null 2>&1 || die "pkg-config installation failed; ensure it is on PATH."
}

ensure_grpc_native() {
  if pkg-config --exists grpc grpc++; then
    log "gRPC development libraries detected via pkg-config"
    return
  fi

  if command -v brew >/dev/null 2>&1; then
    log "Installing gRPC via Homebrew"
    brew install grpc
  elif command -v apt-get >/dev/null 2>&1; then
    log "Installing gRPC via apt-get"
    sudo apt-get update
    sudo apt-get install -y libgrpc-dev protobuf-compiler-grpc
  else
    die "gRPC development libraries not found and automatic installation is unsupported on this platform; install gRPC (including pkg-config metadata) and retry."
  fi

  if ! pkg-config --exists grpc grpc++; then
    die "pkg-config still cannot find gRPC after installation; check that grpc.pc is on PKG_CONFIG_PATH."
  fi
}

ensure_pandoc() {
  if command -v pandoc >/dev/null 2>&1; then
    log "System pandoc detected at $(command -v pandoc)"
    return 0
  fi

  pandoc_version="${PANDOC_VERSION:-3.8.2.1}"
  machine="$(uname -s)"
  arch="$(uname -m)"

  case "$machine" in
    Darwin)
      case "$arch" in
        arm64) pandoc_asset="pandoc-${pandoc_version}-arm64-macOS.zip" ;;
        x86_64) pandoc_asset="pandoc-${pandoc_version}-x86_64-macOS.zip" ;;
        *)
          die "Unsupported macOS architecture: $arch"
          ;;
      esac
      ;;
    Linux)
      case "$arch" in
        aarch64|arm64) pandoc_asset="pandoc-${pandoc_version}-linux-arm64.tar.gz" ;;
        x86_64) pandoc_asset="pandoc-${pandoc_version}-linux-amd64.tar.gz" ;;
        *)
          die "Unsupported Linux architecture: $arch"
          ;;
      esac
      ;;
    *)
      die "Unsupported platform: $machine"
      ;;
  esac

  pandoc_url="https://github.com/jgm/pandoc/releases/download/${pandoc_version}/${pandoc_asset}"
  pandoc_dir="$project_root/tools/pandoc"
  pandoc_bin="$pandoc_dir/bin"
  pandoc_share="$pandoc_dir/share"

  if [[ -x "$pandoc_bin/pandoc" ]]; then
    log "Using existing bundled pandoc at $pandoc_bin/pandoc"
  else
    log "Downloading pandoc ${pandoc_version} from $pandoc_url"
    tmpfile="$(mktemp)"
    curl -fsSL "$pandoc_url" -o "$tmpfile"

    rm -rf "$pandoc_dir"
    mkdir -p "$pandoc_dir"

    case "$pandoc_asset" in
      *.zip)
        unzip -q "$tmpfile" -d "$pandoc_dir/tmp"
        ;;
      *.tar.gz)
        mkdir -p "$pandoc_dir/tmp"
        tar -xzf "$tmpfile" -C "$pandoc_dir/tmp"
        ;;
    esac

    extracted_dir="$(find "$pandoc_dir/tmp" -maxdepth 1 -mindepth 1 -type d | head -n 1)"
    [[ -n "$extracted_dir" ]] || die "Failed to extract pandoc payload"

    mkdir -p "$pandoc_bin"
    mkdir -p "$pandoc_share"
    cp "$extracted_dir"/bin/pandoc "$pandoc_bin/"
    chmod 0755 "$pandoc_bin/pandoc"

    if [[ -d "$extracted_dir/share" ]]; then
      cp -R "$extracted_dir/share/"* "$pandoc_share/"
    fi

    rm -rf "$pandoc_dir/tmp"
    rm -f "$tmpfile"

    log "Bundled pandoc installed to $pandoc_bin/pandoc"
  fi

  export PATH="$pandoc_bin:$PATH"
  log "PATH updated to include bundled pandoc"
}

ensure_pandoc
ensure_pkg_config
ensure_grpc_native

export RENV_PATHS_LIBRARY="$project_root/renv/library"

log "Restoring R package dependencies with renv and ensuring gRPC package is available"
R --vanilla <<'RS'
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cran.rstudio.com")
}
renv::consent(provided = TRUE)
renv::restore(prompt = FALSE)
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cran.rstudio.com")
}
if (!requireNamespace("grpc", quietly = TRUE)) {
  tryCatch(
    remotes::install_github("christiaanpauw/grpc"),
    error = function(e) {
      stop("Failed to install grpc from GitHub (system gRPC libraries required): ", conditionMessage(e))
    }
  )
}
RS

log "Building hadeda package"
R CMD build "$project_root"

pkg_tarball="$(ls -t hadeda_*.tar.gz | head -n 1)"
[[ -n "$pkg_tarball" ]] || die "Package tarball not found after build"

log "Installing package from $pkg_tarball"
R CMD INSTALL "$pkg_tarball"

log "Installation completed successfully"
