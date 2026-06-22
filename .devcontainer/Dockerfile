FROM ghcr.io/uzh/z-pool-tool/ocaml:9-5.3-2.3

# Set up environment
ENV SIHL_ENV=development

USER root

# NOTE: Pre-create volume folders for the correct ownership.
RUN mkdir -p /workspace/_build /workspace/node_modules \
  && chown -R opam:opam /workspace/_build /workspace/node_modules

# Install Node.js LTS
RUN wget -qO- https://rpm.nodesource.com/setup_lts.x | bash - \
  && dnf install -y nodejs \
  && dnf clean all \
  && npm install -g corepack \
  && corepack prepare yarn@3.8.7 --activate

# copy hadolint
COPY --from=hadolint/hadolint:latest-alpine /bin/hadolint /bin/hadolint

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# Install file watcher
COPY ./watcher-install.sh /tmp/watcher-install.sh
RUN /tmp/watcher-install.sh && rm /tmp/watcher-install.sh \
  && echo 'opam ALL=(ALL) NOPASSWD: ALL' | tee /etc/sudoers.d/opam > /dev/null

USER opam

# Install and configure zsh with oh-my-zsh
# hadolint ignore=SC2016
RUN wget --progress=dot:giga https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -q -O - | zsh \
  && cp ~/.oh-my-zsh/templates/zshrc.zsh-template ~/.zshrc \
  && sed -i "/^plugins=/c\plugins=(git dotenv)" ~/.zshrc \
  && echo 'alias make="make -f /workspace/.devcontainer/Makefile"' >> ~/.zshrc
