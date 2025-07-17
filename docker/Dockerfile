FROM registry.access.redhat.com/ubi8/ubi:8.10-1752567140

LABEL org.opencontainers.image.title="OCaml Development Environment"
LABEL org.opencontainers.image.description="OCaml 5.3.0 development environment with opam 2.3.0 installed, based on Red Hat Universal Base Image 8.10 (RHEL 8)"
LABEL org.opencontainers.image.source="https://github.com/uzh/z-pool-tool"
LABEL org.opencontainers.image.url="https://github.com/uzh/z-pool-tool"
LABEL org.opencontainers.image.documentation="https://github.com/uzh/z-pool-tool/blob/main/README.md"
LABEL org.opencontainers.image.base.name="registry.access.redhat.com/ubi8/ubi:8.10-1752567140"
LABEL org.opencontainers.image.base.licenses="GPL-2.0-or-later"

USER root

# Set up environment
ENV OPAM_VERSION=2.3.0
ENV OCAML_VERSION=5.3.0

# Enable RHEL repositories and install packages
# hadolint ignore=DL3041,DL3033
RUN dnf update -y && dnf install -y \
  bzip2 \
  curl \
  diffutils \
  gcc \
  gcc-c++ \
  git \
  gmp-devel \
  m4 \
  make \
  mariadb-connector-c-devel \
  openssl \
  openssl-devel \
  patch \
  python3 \
  python3-devel \
  rsync \
  sudo \
  unzip \
  wget \
  which \
  xz \
  zsh \
  && dnf clean all

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# add timezone
RUN ln -fs /usr/share/zoneinfo/Europe/Zurich /etc/localtime

# WTF: https://github.com/mirage/ocaml-cohttp/issues/675
RUN bash -c 'echo "http		80/tcp	www		# WorldWideWeb HTTP" >> /etc/services' \
  && bash -c 'echo "https		443/tcp	www		# WorldWideWeb HTTPS" >> /etc/services'

# Create opam user
RUN groupadd -r opam && useradd -r -g opam -m -s /bin/bash opam \
  && echo 'opam ALL=(ALL) NOPASSWD: ALL' | tee /etc/sudoers.d/opam > /dev/null

# Install opam
COPY ./scripts/opam-install.sh /tmp/opam-install.sh
RUN /tmp/opam-install.sh --version "${OPAM_VERSION}" \
  && rm /tmp/opam-install.sh

# Switch to opam user and set working directory
USER opam
WORKDIR /app

# Initialize opam and install OCaml
RUN opam init --bare --disable-sandboxing -y \
  && opam switch create "${OCAML_VERSION}"

# Default command
CMD ["/bin/bash"]
