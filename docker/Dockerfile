FROM debian:11
WORKDIR /app

COPY _build/default/pool/run/run.exe run.exe
COPY public pool/public public/

  # install dependencies
RUN apt-get update \
  && apt-get -y install --no-install-recommends \
    ca-certificates \
    libev-dev \
    libgmp-dev \
    libmariadb-dev \
    libssl-dev \
    m4 \
    perl \
    pkg-config \
    zlib1g-dev \
  #
  # Clean up
  && apt-get autoremove -y \
  && apt-get clean -y \
  && rm -rf /var/lib/apt/lists/* \
  #
  # set timezone
  && ln -fs /usr/share/zoneinfo/Europe/Zurich /etc/localtime

# TODO: check if still requred, Shil requires this only for sihl-email through sendgrid
# WTF: https://github.com/mirage/ocaml-cohttp/issues/675
# This is also important for `canary` to run properly
RUN sudo bash -c 'echo "http		80/tcp	www		# WorldWideWeb HTTP" >> /etc/services' && \
  sudo bash -c 'echo "https		443/tcp	www		# WorldWideWeb HTTPS" >> /etc/services'

ENV SIHL_ENV production
ENV LOG_LEVEL info
ENV ROOT_PATH /app

COPY ./docker/entrypoint.sh /docker-entrypoint.sh
RUN chmod +x /docker-entrypoint.sh

ENTRYPOINT ["/docker-entrypoint.sh"]
CMD /app/run.exe server
