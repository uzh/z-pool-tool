FROM ghcr.io/uzh/z-pool-tool/ocaml:9-5.5-2.6
COPY --from=hadolint/hadolint:latest-alpine /bin/hadolint /bin/hadolint

USER root

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# NOTE: Pre-create volume folders for the correct ownership.
RUN mkdir -p /workspace/_build /workspace/node_modules /home/opam/.cache \
  && chown -R opam:opam /workspace/_build /workspace/node_modules /home/opam/.cache

USER opam

# GitButler CLI (`but`), installed to ~/.local/bin (the installer verifies the binary's signature)
RUN curl -fsSL https://gitbutler.com/install.sh | sh > /dev/null
