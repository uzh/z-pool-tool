
#!/bin/bash
#
# watchexec Installation Script
# 
# This script installs watchexec (a file watcher utility) by compiling it from source
# using Rust/Cargo. This approach ensures compatibility with the target system's GLIBC
# version, avoiding the compatibility issues that can occur with pre-built binaries.
#
# Usage:
#   ./watchexec-install.sh [OPTIONS]
#
# Options:
#   -f, --force       Force reinstallation even if watchexec is already installed
#   -v, --version VER Install a specific version (default: latest)
#   -h, --help        Show this help message
#
# Examples:
#   ./watchexec-install.sh                    # Interactive installation
#   ./watchexec-install.sh --force            # Force reinstallation
#   ./watchexec-install.sh --version 2.3.2    # Install specific version
#
# Requirements:
#   - curl (for downloading Rust installer)
#   - Internet connection
#   - Build tools (usually pre-installed or installed with Rust)
#

set -euo pipefail

# Configuration
DEFAULT_FALLBACK_VERSION="2.3.2"
CARGO_ENV_FILE="$HOME/.cargo/env"

# Script arguments
FORCE_INSTALL=false
SPECIFIC_VERSION=""
SHOW_HELP=false

# Function to show help
show_help() {
  cat << 'EOF'
watchexec Installation Script

USAGE:
    ./watchexec-install.sh [OPTIONS]

OPTIONS:
    -f, --force              Force reinstallation even if watchexec is already installed
    -v, --version VERSION    Install a specific version (default: latest)
    -h, --help              Show this help message

EXAMPLES:
    ./watchexec-install.sh                    # Interactive installation
    ./watchexec-install.sh --force            # Force reinstallation  
    ./watchexec-install.sh --version 2.3.2    # Install specific version
    ./watchexec-install.sh -f -v 1.25.1       # Force install specific version

DESCRIPTION:
    This script installs watchexec by compiling it from source using Rust/Cargo.
    This approach ensures compatibility with the target system's GLIBC version,
    avoiding compatibility issues that can occur with pre-built binaries.

EOF
}

# Parse command line arguments
parse_arguments() {
  while [[ $# -gt 0 ]]; do
    case $1 in
      -f|--force)
        FORCE_INSTALL=true
        shift
        ;;
      -v|--version)
        if [[ -n "${2:-}" ]]; then
          SPECIFIC_VERSION="$2"
          shift 2
        else
          echo "Error: --version requires a version argument" >&2
          exit 1
        fi
        ;;
      -h|--help)
        SHOW_HELP=true
        shift
        ;;
      *)
        echo "Error: Unknown option $1" >&2
        echo "Use --help for usage information." >&2
        exit 1
        ;;
    esac
  done
}

# Function to check if watchexec is already installed and up to date
check_existing_installation() {
  if command -v watchexec >/dev/null 2>&1; then
    local current_version
    current_version=$(watchexec --version 2>/dev/null | head -n1 | cut -d' ' -f2 || echo "unknown")
    echo "Found existing watchexec version: $current_version"
    
    # Check if force install is requested
    if [[ "$FORCE_INSTALL" == "true" ]]; then
      echo "Force installation requested, proceeding with reinstallation..."
      return 0
    fi
    
    # Only ask if running interactively
    if [[ -t 0 ]]; then
      echo "Do you want to reinstall/update watchexec? (y/N)"
      read -r response
      case "$response" in
        [yY]|[yY][eE][sS]) 
          echo "Proceeding with reinstallation..."
          return 0 ;;
        *) 
          echo "Keeping existing installation."
          exit 0 ;;
      esac
    else
      echo "Existing installation found. Use --force to reinstall."
      exit 0
    fi
  fi
  return 0
}

# Function to install Rust if not present
install_rust() {
  echo "Rust/Cargo not found. Installing Rust..."
  
  # Install Rust using rustup
  if ! curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable; then
    echo "Error: Failed to download and install Rust" >&2
    exit 1
  fi
  
  # Source the cargo environment
  if [[ -f "$CARGO_ENV_FILE" ]]; then
    source "$CARGO_ENV_FILE"
  else
    echo "Error: Cargo environment file not found at $CARGO_ENV_FILE" >&2
    exit 1
  fi
  
  # Verify installation
  if ! command -v cargo >/dev/null 2>&1; then
    echo "Error: Rust/Cargo installation verification failed" >&2
    exit 1
  fi
  
  echo "✓ Rust/Cargo installed successfully."
}

# Function to install watchexec
install_watchexec() {
  echo "Installing watchexec from source using cargo..."
  
  # Ensure cargo is available (source environment if needed)
  if ! command -v cargo >/dev/null 2>&1 && [[ -f "$CARGO_ENV_FILE" ]]; then
    source "$CARGO_ENV_FILE"
  fi
  
  # Determine which version to install
  if [[ -n "$SPECIFIC_VERSION" ]]; then
    echo "Installing watchexec version $SPECIFIC_VERSION..."
    if cargo install watchexec-cli --version "$SPECIFIC_VERSION" 2>/dev/null; then
      echo "✓ Watchexec version $SPECIFIC_VERSION installed successfully."
    else
      echo "Error: Failed to install watchexec version $SPECIFIC_VERSION" >&2
      exit 1
    fi
  else
    # Try to install the latest version first, fall back to specific version if needed
    if cargo install watchexec-cli 2>/dev/null; then
      echo "✓ Latest watchexec version installed successfully."
    elif cargo install watchexec-cli --version "$DEFAULT_FALLBACK_VERSION" 2>/dev/null; then
      echo "✓ Watchexec version $DEFAULT_FALLBACK_VERSION installed successfully."
    else
      echo "Error: Failed to install watchexec" >&2
      exit 1
    fi
  fi
}

# Parse command line arguments
parse_arguments "$@"

# Show help if requested
if [[ "$SHOW_HELP" == "true" ]]; then
  show_help
  exit 0
fi

echo "Installing watchexec using Rust/Cargo..."

# Check for existing installation
check_existing_installation

# Check if Rust is already installed
if ! command -v cargo >/dev/null 2>&1; then
  install_rust
else
  echo "✓ Rust/Cargo already installed."
fi

# Install watchexec
install_watchexec

# Add cargo bin to PATH for current session
export PATH="$HOME/.cargo/bin:$PATH"

# Function to create system-wide symlink
setup_system_access() {
  local watchexec_bin="$HOME/.cargo/bin/watchexec"
  local system_bin="/usr/local/bin/watchexec"
  
  if [[ ! -f "$watchexec_bin" ]]; then
    echo "Error: watchexec binary not found at $watchexec_bin" >&2
    return 1
  fi
  
  if [[ "$(id -u)" -eq 0 ]]; then
    ln -sf "$watchexec_bin" "$system_bin"
    echo "✓ Created system-wide symlink at $system_bin"
  elif sudo -n true 2>/dev/null; then
    sudo ln -sf "$watchexec_bin" "$system_bin"
    echo "✓ Created system-wide symlink at $system_bin (with sudo)"
  else
    echo "ℹ Note: watchexec installed in $watchexec_bin"
    echo "ℹ Add \$HOME/.cargo/bin to your PATH to use it globally"
    echo "ℹ Or run: sudo ln -sf $watchexec_bin $system_bin"
    return 1
  fi
}

# Function to verify installation
verify_installation() {
  local watchexec_path=""
  
  # Find watchexec binary
  if command -v watchexec >/dev/null 2>&1; then
    watchexec_path=$(command -v watchexec)
  elif [[ -f "$HOME/.cargo/bin/watchexec" ]]; then
    watchexec_path="$HOME/.cargo/bin/watchexec"
  fi
  
  # Verify it works
  if [[ -n "$watchexec_path" ]] && "$watchexec_path" --version >/dev/null 2>&1; then
    echo "✅ watchexec installed successfully at: $watchexec_path"
    "$watchexec_path" --version
    return 0
  else
    echo "❌ Error: watchexec installation verification failed" >&2
    return 1
  fi
}

# Setup system access and verify
setup_system_access || true  # Don't fail if system access setup fails
verify_installation
