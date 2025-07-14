#!/bin/sh

# Script to install custom OpenSSL 1.1.1k FIPS and MariaDB Connector/C
# This script is designed to run in a Debian-based container

# immediately exit when a command fails and print each command
set -ex

printTitle() {
  echo -e "\e[1;33m~~~ $1 ~~~\e[0m"
}

# =====================================================
# OpenSSL 1.1.1k FIPS installation
# =====================================================

install_openssl() {
  printTitle "Installing OpenSSL 1.1.1k FIPS from source"

  local OPENSSL_VERSION=1.1.1k
  local TEMP_DIR=/tmp/openssl-build

  mkdir -p "$TEMP_DIR"
  cd "$TEMP_DIR"

  # Download and extract OpenSSL
  wget --progress=dot:giga "https://www.openssl.org/source/openssl-${OPENSSL_VERSION}.tar.gz"
  tar -xzf "openssl-${OPENSSL_VERSION}.tar.gz"
  cd "openssl-${OPENSSL_VERSION}"

  # Configure based on architecture
  if [ "$(uname -m)" = "aarch64" ]; then
    ./config --prefix=/usr/local/openssl --openssldir=/usr/local/openssl
  else
    ./config --prefix=/usr/local/openssl --openssldir=/usr/local/openssl enable-fips
  fi

  # Build and install
  make -j"$(nproc)"
  make install

  # Configure library paths
  echo "/usr/local/openssl/lib" > /etc/ld.so.conf.d/01-openssl-custom.conf
  ldconfig

  # Create system symlink
  ln -sf /usr/local/openssl/bin/openssl /usr/local/bin/openssl

  # Cleanup
  cd /
  rm -rf "$TEMP_DIR"

  printTitle "OpenSSL installation completed"
}

# =====================================================
# MariaDB Connector/C installation
# =====================================================

install_mariadb_connector() {
  printTitle "Installing MariaDB Connector/C from source"

  local MARIADB_CONNECTOR_VERSION=3.3.10
  local TEMP_DIR=/tmp/mariadb-build

  mkdir -p "$TEMP_DIR"
  cd "$TEMP_DIR"

  # Download and extract MariaDB Connector/C
  wget --progress=dot:giga "https://github.com/mariadb-corporation/mariadb-connector-c/archive/refs/tags/v${MARIADB_CONNECTOR_VERSION}.tar.gz"
  tar -xzf "v${MARIADB_CONNECTOR_VERSION}.tar.gz"
  cd "mariadb-connector-c-${MARIADB_CONNECTOR_VERSION}"

  # Configure with custom OpenSSL
  cmake -DCMAKE_INSTALL_PREFIX=/usr/local/mariadb \
    -DOPENSSL_ROOT_DIR=/usr/local/openssl \
    -DOPENSSL_LIBRARIES="/usr/local/openssl/lib/libssl.so;/usr/local/openssl/lib/libcrypto.so" \
    -DOPENSSL_INCLUDE_DIR=/usr/local/openssl/include \
    -DWITH_SSL=OPENSSL \
    .

  # Build and install
  make -j"$(nproc)"
  make install

  # Configure library paths
  echo "/usr/local/mariadb/lib/mariadb" > /etc/ld.so.conf.d/01-mariadb-custom.conf
  ldconfig

  # Create pkg-config file
  mkdir -p /usr/local/mariadb/lib/mariadb/pkgconfig
  cat > /usr/local/mariadb/lib/mariadb/pkgconfig/libmariadb.pc << 'EOF'
prefix=/usr/local/mariadb
exec_prefix=${prefix}
libdir=${exec_prefix}/lib/mariadb
includedir=${prefix}/include/mariadb

Name: libmariadb
Description: MariaDB Connector/C
Version: 3.3.10
Libs: -L${libdir} -lmariadb
Cflags: -I${includedir}
EOF

  # Create mariadb.pc symlink
  ln -s /usr/local/mariadb/lib/mariadb/pkgconfig/libmariadb.pc /usr/local/mariadb/lib/mariadb/pkgconfig/mariadb.pc

  # Cleanup
  cd /
  rm -rf "$TEMP_DIR"

  printTitle "MariaDB Connector/C installation completed"
}

# =====================================================
# System cleanup and configuration
# =====================================================

cleanup_system_packages() {
  printTitle "Removing system MariaDB packages"

  apt-get update -q
  apt-get remove -y \
    libmariadb-dev \
    libmariadb3 \
    default-libmysqlclient-dev \
    libmysqlclient21 \
    libmysqlclient-dev \
    || true

  apt-get autoremove -y
  apt-get clean all
  rm -rf /var/lib/apt/lists/*

  # Remove any remaining system MariaDB/MySQL libraries and headers
  rm -f /usr/lib/*/libmariadb.so* /usr/lib/*/libmysqlclient.so* || true
  rm -f /lib/*/libmariadb.so* /lib/*/libmysqlclient.so* || true
  rm -rf /usr/include/mariadb /usr/include/mysql || true
  rm -rf /usr/lib/*/pkgconfig/libmariadb.pc /usr/lib/*/pkgconfig/mariadb.pc || true

  printTitle "System package cleanup completed"
}

create_compatibility_symlinks() {
  printTitle "Creating compatibility symlinks"

  mkdir -p /usr/local/lib/pkgconfig /usr/share/pkgconfig

  # MariaDB symlinks
  ln -sf /usr/local/mariadb/lib/mariadb/libmariadb.so /usr/local/lib/libmariadb.so
  ln -sf /usr/local/mariadb/lib/mariadb/libmariadb.so.3 /usr/local/lib/libmariadb.so.3 || true
  ln -sf /usr/local/mariadb/include/mariadb /usr/local/include/mariadb
  ln -sf /usr/local/mariadb/lib/mariadb/pkgconfig/libmariadb.pc /usr/share/pkgconfig/libmariadb.pc
  ln -sf /usr/local/mariadb/lib/mariadb/pkgconfig/mariadb.pc /usr/share/pkgconfig/mariadb.pc
  ln -sf /usr/local/mariadb/lib/mariadb/pkgconfig/libmariadb.pc /usr/local/lib/pkgconfig/libmariadb.pc

  # OpenSSL symlinks
  ln -sf /usr/local/openssl/lib/pkgconfig/openssl.pc /usr/local/lib/pkgconfig/openssl.pc

  printTitle "Compatibility symlinks created"
}

create_environment_setup() {
  printTitle "Creating environment setup script"

  cat > /tmp/env_setup.sh << 'EOF'
export PATH="/usr/local/openssl/bin:/usr/local/mariadb/bin:$PATH"
export LD_LIBRARY_PATH="/usr/local/mariadb/lib/mariadb:/usr/local/openssl/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/mariadb/lib/mariadb/pkgconfig:/usr/local/openssl/lib/pkgconfig:$PKG_CONFIG_PATH"
export OPENSSL_DIR="/usr/local/openssl"
export OPENSSL_INCLUDE_DIR="/usr/local/openssl/include"
export OPENSSL_LIB_DIR="/usr/local/openssl/lib"
export MARIADB_CONFIG="/usr/local/mariadb/bin/mariadb_config"
EOF

  chmod +x /tmp/env_setup.sh

  printTitle "Environment setup script created at /tmp/env_setup.sh"
}

# =====================================================
# Main installation process
# =====================================================

main() {
  printTitle "Starting OpenSSL and MariaDB installation"

  # Install in order
  install_openssl
  install_mariadb_connector
  cleanup_system_packages
  create_compatibility_symlinks
  create_environment_setup

  printTitle "All installations completed successfully"

  # Verify installations
  echo "OpenSSL version: $(/usr/local/openssl/bin/openssl version)"
  echo "MariaDB connector installed at: /usr/local/mariadb"
  echo "Library paths configured and updated"
}

# Run main function
main "$@"
