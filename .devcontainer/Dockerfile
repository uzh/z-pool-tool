FROM hadolint/hadolint:latest-alpine AS hadolint
FROM ocaml/opam:alpine-ocaml-4.12

USER root

# copy hadolint
COPY --from=hadolint /bin/hadolint /bin/hadolint

# Avoid warnings by switching to noninteractive
ENV DEBIAN_FRONTEND noninteractive
ENV SIHL_ENV development

# install packages
RUN apk update && apk add \
  # development dependencies
  inotify-tools \
  zsh \
  nodejs \
  yarn \
  # 
  # build dependencies
  openssl-libs-static \
  mariadb-static \
  sqlite-static \
  zlib-static

# add timezone
RUN ln -fs /usr/share/zoneinfo/Europe/Zurich /etc/localtime

# WTF: https://github.com/mirage/ocaml-cohttp/issues/675
RUN bash -c 'echo "http		80/tcp	www		# WorldWideWeb HTTP" >> /etc/services' \
  && bash -c 'echo "https		443/tcp	www		# WorldWideWeb HTTPS" >> /etc/services'

USER opam

# install oh-my-zsh
RUN wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -q -O - | zsh \
  && cp ~/.oh-my-zsh/templates/zshrc.zsh-template ~/.zshrc \
  && sed -i "/^plugins=/c\plugins=(git dotenv)" ~/.zshrc \
  #
  # link make to devcontainer makefile
  && echo 'alias make="make -f /workspace/.devcontainer/Makefile"' >> ~/.zshrc

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialog
