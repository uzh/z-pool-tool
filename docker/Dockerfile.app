FROM registry.access.redhat.com/ubi8/ubi-minimal:8.10

# Install runtime dependencies
RUN microdnf install -y \
    ca-certificates \
    curl \
    gmp \
    mariadb-connector-c \
    openssl-libs \
    shadow-utils \
    && microdnf clean all

RUN useradd -m -u 1000 -s /bin/bash app

WORKDIR /app

COPY artifacts/run.exe/run.exe /app/run.exe
COPY artifacts/public /app/public
COPY docker/entrypoint.sh /app/entrypoint.sh

# WORKAROUND: https://github.com/mirage/ocaml-cohttp/issues/675
RUN bash -c 'echo "http		80/tcp	www		# WorldWideWeb HTTP" >> /etc/services' && \
    bash -c 'echo "https		443/tcp	www		# WorldWideWeb HTTPS" >> /etc/services'

RUN chown -R app:app /app && \
    chmod +x /app/run.exe /app/entrypoint.sh

USER app

ENV SIHL_ENV=production
ENV LOG_LEVEL=info
ENV ROOT_PATH=/app
ENV PUBLIC_DIR=/app/public

EXPOSE 3000

HEALTHCHECK --interval=60s --timeout=3s --start-period=5s --retries=10 \
  CMD curl -f http://localhost:3000/root/api/v1/status || exit 1

ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["/app/run.exe", "server"]
