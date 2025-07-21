#!/bin/bash
#
# inotify-tools Installation Script
# 
# This script installs inotify-tools (a file monitoring utility) by downloading
# the source code from GitHub and compiling it. This approach ensures compatibility
# with the target system and provides the latest features.
#
# Usage:
#   ./inotify-install.sh [OPTIONS]
#
# Options:
#   -f, --force       Force reinstallation even if inotify-tools is already installed
#   -v, --version VER Install a specific version (default: latest)
#   -h, --help        Show this help message
#
# Examples:
#   ./inotify-install.sh                      # Interactive installation
#   ./inotify-install.sh --force              # Force reinstallation
#   ./inotify-install.sh --version 4.23.9.0   # Install specific version
#
# Requirements:
#   - curl or wget (for downloading source)
#   - Internet connection
#   - Build tools (gcc, make, autotools)
#

set -euo pipefail

# Configuration
GITHUB_REPO="inotify-tools/inotify-tools"
DEFAULT_FALLBACK_VERSION="4.23.9.0"
BUILD_DIR="/tmp/inotify-tools-build"
PREFIX="/usr/local"

# Script arguments
FORCE_INSTALL=false
SPECIFIC_VERSION=""
SHOW_HELP=false

# Function to show help
show_help() {
  cat << 'EOF'
inotify-tools Installation Script

USAGE:
    ./inotify-install.sh [OPTIONS]

OPTIONS:
    -f, --force              Force reinstallation even if inotify-tools is already installed
    -v, --version VERSION    Install a specific version (default: latest)
    -h, --help              Show this help message

EXAMPLES:
    ./inotify-install.sh                      # Interactive installation
    ./inotify-install.sh --force              # Force reinstallation  
    ./inotify-install.sh --version 4.23.9.0   # Install specific version
    ./inotify-install.sh -f -v 3.22.6.0       # Force install specific version

DESCRIPTION:
    This script installs inotify-tools by downloading the source code from GitHub
    and compiling it. This approach ensures compatibility with the target system
    and provides access to the latest features and bug fixes.

    The inotify-tools package provides utilities like inotifywait and inotifywatch
    for monitoring filesystem events using Linux's inotify interface.

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

# Function to check system requirements
check_system_requirements() {
  echo "Checking system requirements..."
  
  # Check for required download tools
  if ! command -v curl >/dev/null 2>&1 && ! command -v wget >/dev/null 2>&1; then
    echo "Error: Neither curl nor wget found. Please install one of them." >&2
    exit 1
  fi
  
  echo "✓ System requirements check passed."
}

# Function to check if inotify-tools is already installed
check_existing_installation() {
  if command -v inotifywait >/dev/null 2>&1; then
    local current_version
    current_version=$(inotifywait --version 2>&1 | head -n1 | grep -o '[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+' || echo "unknown")
    echo "Found existing inotify-tools version: $current_version"
    
    # Check if force install is requested
    if [[ "$FORCE_INSTALL" == "true" ]]; then
      echo "Force installation requested, proceeding with reinstallation..."
      return 0
    else
      echo "inotify-tools is already installed. Use --force to reinstall."
      exit 0
    fi
  fi
  return 0
}

# Function to execute command with proper privileges
execute_with_privileges() {
  local cmd="$1"
  shift
  
  if [[ "$(id -u)" -eq 0 ]]; then
    "$cmd" "$@"
  else
    sudo "$cmd" "$@"
  fi
}

# Function to execute command with error handling
execute_or_exit() {
  local description="$1"
  shift
  
  echo "$description..."
  "$@" || {
    echo "Error: $description failed" >&2
    exit 1
  }
}

# Function to get the latest version from GitHub API
get_latest_version() {
  local api_url="https://api.github.com/repos/$GITHUB_REPO/releases/latest"
  
  if command -v curl >/dev/null 2>&1; then
    curl -s "$api_url" | grep '"tag_name":' | sed -E 's/.*"([^"]+)".*/\1/' 2>/dev/null || echo ""
  elif command -v wget >/dev/null 2>&1; then
    wget -qO- "$api_url" | grep '"tag_name":' | sed -E 's/.*"([^"]+)".*/\1/' 2>/dev/null || echo ""
  else
    echo ""
  fi
}

# Function to download and build inotify-tools
install_inotify_tools() {
  echo "Installing inotify-tools from source..."
  
  # Determine which version to install
  local version_to_install=""
  if [[ -n "$SPECIFIC_VERSION" ]]; then
    version_to_install="$SPECIFIC_VERSION"
    echo "Installing inotify-tools version $version_to_install..."
  else
    echo "Fetching latest version information..."
    version_to_install=$(get_latest_version)
    if [[ -z "$version_to_install" ]]; then
      version_to_install="$DEFAULT_FALLBACK_VERSION"
      echo "Could not fetch latest version, using fallback version $version_to_install"
    else
      echo "Installing latest inotify-tools version $version_to_install..."
    fi
  fi
  
  # Clean up any existing build directory
  rm -rf "$BUILD_DIR"
  mkdir -p "$BUILD_DIR"
  cd "$BUILD_DIR"
  
  # Download source code
  local download_url="https://github.com/$GITHUB_REPO/archive/refs/tags/$version_to_install.tar.gz"
  local filename="inotify-tools-$version_to_install.tar.gz"
  
  if command -v curl >/dev/null 2>&1; then
    execute_or_exit "Downloading source code" curl -L -o "$filename" "$download_url"
  elif command -v wget >/dev/null 2>&1; then
    execute_or_exit "Downloading source code" wget -O "$filename" "$download_url"
  else
    echo "Error: Neither curl nor wget found" >&2
    exit 1
  fi
  
  # Verify download
  if [[ ! -f "$filename" ]] || [[ $(stat -c%s "$filename" 2>/dev/null || echo 0) -lt 1000 ]]; then
    echo "Error: Download failed or file is too small" >&2
    exit 1
  fi
  
  # Extract source code
  execute_or_exit "Extracting source code" tar -xzf "$filename"
  
  # Find the extracted directory (handle different naming patterns)
  local src_dir=""
  for dir in "inotify-tools-$version_to_install" "inotify-tools-${version_to_install#v}"; do
    if [[ -d "$dir" ]]; then
      src_dir="$dir"
      break
    fi
  done
  
  if [[ -z "$src_dir" ]]; then
    echo "Error: Could not find extracted source directory" >&2
    ls -la
    exit 1
  fi
  
  cd "$src_dir"
  
  # Build and install
  if [[ -f "autogen.sh" ]]; then
    execute_or_exit "Running autogen.sh" ./autogen.sh
  elif [[ ! -f "configure" ]]; then
    execute_or_exit "Running autoreconf to generate configure script" autoreconf -fiv
  fi
  
  execute_or_exit "Running configure" ./configure --prefix="$PREFIX"
  
  local num_cores
  num_cores=$(nproc 2>/dev/null || echo 2)
  execute_or_exit "Building inotify-tools" make -j"$num_cores"
  
  execute_or_exit "Installing inotify-tools" execute_with_privileges make install
  
  # Update library cache
  if command -v ldconfig >/dev/null 2>&1; then
    execute_with_privileges ldconfig 2>/dev/null || true
  fi
  
  echo "✓ inotify-tools installed successfully."
}

# Parse command line arguments
parse_arguments "$@"

# Show help if requested
if [[ "$SHOW_HELP" == "true" ]]; then
  show_help
  exit 0
fi

echo "Installing inotify-tools from GitHub source..."

# Check system requirements first
check_system_requirements

# Check for existing installation
check_existing_installation

# Install inotify-tools
install_inotify_tools

# Function to verify installation
verify_installation() {
  echo "Verifying installation..."
  
  # Check if tools are available
  if ! command -v inotifywait >/dev/null 2>&1; then
    echo "❌ Error: inotifywait not found in PATH" >&2
    return 1
  fi
  
  if ! command -v inotifywatch >/dev/null 2>&1; then
    echo "❌ Error: inotifywatch not found in PATH" >&2
    return 1
  fi
  
  local version
  version=$(inotifywait --version 2>&1 | head -n1 || echo "unknown")
  
  echo "✅ inotify-tools installed successfully!"
  echo "Version: $version"
  echo "Location: $(command -v inotifywait)"
  
  return 0
}

# Clean up build directory
cleanup() {
  if [[ -d "$BUILD_DIR" ]]; then
    echo "Cleaning up build directory..."
    rm -rf "$BUILD_DIR"
  fi
}

# Set up cleanup trap
trap cleanup EXIT

# Verify installation
verify_installation
