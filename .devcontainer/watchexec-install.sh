
set -euo pipefail

ARCH=$(uname -m)
# Map architecture to watchexec naming convention
case "$ARCH" in
  x86_64) TARGET="x86_64-unknown-linux-gnu" ;;
  aarch64|arm64) TARGET="aarch64-unknown-linux-gnu" ;;
  i686) TARGET="i686-unknown-linux-gnu" ;;
  *) echo "Unsupported architecture: $ARCH" >&2; exit 1 ;;
esac

# Use version that works with glibc 2.28 (RHEL 8)
VERSION="1.25.1"
FILENAME="watchexec-${VERSION}-${TARGET}.tar.xz"
URL="https://github.com/watchexec/watchexec/releases/download/v${VERSION}/${FILENAME}"

echo "Downloading watchexec ${VERSION} for ${ARCH}..."
curl -fL -O "$URL"
tar -xf "$FILENAME"

# Use sudo only if not running as root
if [ "$(id -u)" -eq 0 ]; then
  mv watchexec-*/watchexec /usr/local/bin/watchexec
  chmod +x /usr/local/bin/watchexec
else
  sudo mv watchexec-*/watchexec /usr/local/bin/watchexec
  sudo chmod +x /usr/local/bin/watchexec
fi
rm -rf watchexec-*/ "$FILENAME"

# Verify installation
if /usr/local/bin/watchexec --version; then
  echo "watchexec installed successfully."
else
  echo "Error: watchexec installation failed." >&2
  exit 1
fi
