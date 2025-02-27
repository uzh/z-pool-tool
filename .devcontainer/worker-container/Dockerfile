FROM cr.gitlab.uzh.ch/econ/it/docker/python:3-slim

COPY --from=hadolint/hadolint:latest-alpine /bin/hadolint /bin/hadolint

ENV EDITOR="code -nw"
ENV PATH=~/.local/bin:$PATH

# Avoid warnings by switching to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# Set time zone
RUN ln -sf /usr/share/zoneinfo/Europe/Zurich /etc/localtime

# Configure apt and install packages
RUN apt-get update \
  && apt-get -y install --no-install-recommends \
  #
  # Install dev tools
  apt-utils dialog wget zsh ssh git \
  #
  # Install dependencies
  gcc libmariadb-dev pkg-config libxml2-dev libxmlsec1-dev libxmlsec1-openssl libxslt1-dev xmlsec1 \
  #
  # Clean up
  && apt-get autoremove -y \
  && apt-get clean -y \
  && rm -rf /var/lib/apt/lists/*

# Install oh-my-zsh
RUN wget --progress=dot:giga https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh \
  && cp ~/.oh-my-zsh/templates/zshrc.zsh-template ~/.zshrc \
  && sed -i "/^plugins=/c\plugins=(git dotenv)" ~/.zshrc

# enable Poetry shell integration
RUN mkdir ~/.oh-my-zsh/custom/plugins/poetry \
  && poetry completions zsh > ~/.oh-my-zsh/custom/plugins/poetry/_poetry

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialog
