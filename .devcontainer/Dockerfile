FROM ghcr.io/uzh/z-pool-tool/ocaml:5.3-2.3

# Set up environment
ENV SIHL_ENV=development

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

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# Install file watcher
COPY ./watcher-install.sh /tmp/watcher-install.sh
RUN /tmp/watcher-install.sh && rm /tmp/watcher-install.sh

USER opam

# Install and configure zsh with oh-my-zsh
# hadolint ignore=SC2016
RUN wget --progress=dot:giga https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -q -O - | zsh \
  && cp ~/.oh-my-zsh/templates/zshrc.zsh-template ~/.zshrc \
  && sed -i "/^plugins=/c\plugins=(git dotenv)" ~/.zshrc \
  && echo 'alias make="make -f /workspace/.devcontainer/Makefile"' >> ~/.zshrc
