FROM ocaml/opam:debian-12-ocaml-5.2

USER root

# copy node from node container and link commands
COPY --from=node:lts /usr/local/lib/node_modules /usr/local/lib/node_modules
COPY --from=node:lts /usr/local/bin/node /usr/local/bin/node
COPY --from=node:lts /opt /opt
RUN ln -s /usr/local/lib/node_modules/npm/bin/npm-cli.js /usr/local/bin/npm \
  && ln -s /usr/local/lib/node_modules/npm/bin/npx-cli.js /usr/local/bin/npx \
  && ln -s /usr/local/bin/node /usr/local/bin/nodejs \
  && ln -s /opt/yarn-v*/bin/yarn /usr/local/bin/yarn \
  && ln -s /opt/yarn-v*/bin/yarnpkg /usr/local/bin/yarnpkg

# copy hadolint
COPY --from=hadolint/hadolint:latest-alpine /bin/hadolint /bin/hadolint

# Avoid warnings by switching to noninteractive
ENV DEBIAN_FRONTEND=noninteractive
ENV SIHL_ENV=development

# install packages and build dependencies
# hadolint ignore=DL3008
RUN apt-get update -q && apt-get install -yqq --no-install-recommends \
  # development dependencies
  inotify-tools \
  zsh \
  m4 \
  wget \
  curl \
  build-essential \
  python-is-python3 \
  #
  # build dependencies for OpenSSL and other packages
  gcc \
  libev-dev \
  libffi-dev \
  libgmp-dev \
  pkg-config \
  make \
  perl \
  cmake \
  git \
  #
  # cleanup installations
  && apt-get autoremove -y \
  && apt-get clean all \
  && rm -rf /var/lib/apt/lists/*

# Copy and run installation script for OpenSSL and MariaDB
COPY --chmod=0755 scripts/install.sh /tmp/install.sh
RUN /tmp/install.sh && rm /tmp/install.sh

# Set OpenSSL and MariaDB environment variables
ENV PATH="/usr/local/openssl/bin:/usr/local/mariadb/bin:$PATH"
ENV LD_LIBRARY_PATH="/usr/local/mariadb/lib/mariadb:/usr/local/openssl/lib:$LD_LIBRARY_PATH"
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:/usr/local/mariadb/lib/mariadb/pkgconfig:/usr/local/openssl/lib/pkgconfig:$PKG_CONFIG_PATH"
ENV OPENSSL_DIR="/usr/local/openssl"
ENV OPENSSL_INCLUDE_DIR="/usr/local/openssl/include"
ENV OPENSSL_LIB_DIR="/usr/local/openssl/lib"
ENV MARIADB_CONFIG="/usr/local/mariadb/bin/mariadb_config"

# add timezone
RUN ln -fs /usr/share/zoneinfo/Europe/Zurich /etc/localtime

# WTF: https://github.com/mirage/ocaml-cohttp/issues/675
RUN bash -c 'echo "http		80/tcp	www		# WorldWideWeb HTTP" >> /etc/services' \
  && bash -c 'echo "https		443/tcp	www		# WorldWideWeb HTTPS" >> /etc/services'

# link opam version
RUN ln -fs /usr/bin/opam-2.2 /usr/bin/opam

USER opam

# Install and configure zsh with oh-my-zsh
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
# hadolint ignore=SC2016
RUN wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -q -O - | zsh \
  && cp ~/.oh-my-zsh/templates/zshrc.zsh-template ~/.zshrc \
  && sed -i "/^plugins=/c\plugins=(git dotenv)" ~/.zshrc \
  && echo 'alias make="make -f /workspace/.devcontainer/Makefile"' >> ~/.zshrc

# Apply environment setup script to shell profiles (created by install.sh)
RUN cat /tmp/env_setup.sh >> ~/.bashrc \
  && cat /tmp/env_setup.sh >> ~/.zshrc \
  && rm /tmp/env_setup.sh

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialog
